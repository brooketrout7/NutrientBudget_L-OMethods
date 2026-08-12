# ==============================================================================
# Estimate wet deposition phosphorus flux to Lake McDonald
#
# Purpose:
#   Calculate weekly total phosphorus (TP) concentration and deposition flux
#   from FLBS wet-deposition samples, then scale deposition from the collection
#   bucket to the surface area of Lake McDonald.
#
# Notes:
#   - TP concentration is reported in ug-P/L.
#   - Sample volume is converted from mL to L.
#   - TP deposition is first calculated per unit bucket area (kg-P/m2),
#     then scaled to the full surface area of Lake McDonald.
# ==============================================================================




# 1. Define constants ----------------------------------------------------------

# Lake McDonald surface area (m2)
lake_area_m2 <- 27810670.1791

# Wet-deposition collection bucket area:
# 629.02 cm2 / 10,000 = 0.062902 m2
bucket_area_m2 <- 0.062902


# 2. Read FLBS wet-deposition data --------------------------------------------

flbs <- read.csv(
  here(
    "0_data",
    "Wet_Deposition.csv"
  ),
  header = TRUE,
  na.strings = ""
)

flbs_add <- read.csv(
  here(
    "0_data",
    "FLBSPublicData.csv"
  ),
  header = TRUE,
  na.strings = ""
)


# 3. Clean and format dates ----------------------------------------------------

# Convert collection dates to POSIXct in UTC

flbs$Date <- as.POSIXct(
  flbs$CollectDate,
  format = "%m/%d/%Y",
  tz = "UTC"
)

flbs_add$Date <- as.POSIXct(
  flbs_add$Date,
  format = "%m/%d/%Y",
  tz = "UTC"
)

# Supplemental dataset is used only for dates prior to June 26, 2008

flbs_add <- flbs_add %>%
  filter(
    Date < as.POSIXct(
      "2008-06-26",
      tz = "UTC"
    )
  )


# 4. Select relevant variables -------------------------------------------------

flbs <- flbs %>%
  select(
    Date,
    Volume,
    Param,
    CorrectedReportedResult
  ) %>%
  rename(
    Parameter = Param,
    Value = CorrectedReportedResult
  )

# Supplemental dataset contains TP observations used to extend the record

flbs_add <- flbs_add %>%
  filter(
    Parameter == "TP"
  ) %>%
  select(
    Date,
    Volume,
    Parameter,
    Value
  )


# 5. Convert volume and concentration variables -------------------------------

flbs <- flbs %>%
  mutate(
    Volume_mL = as.numeric(Volume),
    Volume_L = Volume_mL / 1000,
    Result_ugL = as.numeric(Value)
  )

flbs_add <- flbs_add %>%
  mutate(
    Volume_mL = as.numeric(Volume),
    Volume_L = Volume_mL / 1000,
    Result_ugL = as.numeric(Value)
  )


# 6. Combine FLBS datasets -----------------------------------------------------

flbs <- rbind(
  flbs_add,
  flbs
)


# 7. Check and combine duplicate collection dates -----------------------------

# Identify dates with multiple records.

duplicated_dates <- duplicated(
  flbs$Date
)

duplicated_values <- flbs$Date[
  duplicated_dates
]

print(
  duplicated_values
)

# Previously identified duplicate dates:
#
# 2007-11-14
# 2008-05-02
# 2008-05-13
# 2014-05-02
#
# The first three dates have reported sample volumes of zero.
#
# Sum records occurring on the same collection date.

flbs <- flbs %>%
  group_by(
    Date
  ) %>%
  summarise(
    Volume = sum(
      Volume,
      na.rm = TRUE
    ),
    Volume_mL = sum(
      Volume_mL,
      na.rm = TRUE
    ),
    Result_ugL = sum(
      Result_ugL,
      na.rm = TRUE
    ),
    Volume_L = sum(
      Volume_L,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# 8. Calculate TP deposition flux ---------------------------------------------

# Calculate the mass of TP deposited per square meter of collection area:
#
#   TP flux (kg-P/m2) =
#     concentration (ug-P/L)
#     * sample volume (L)
#     * 1 kg / 1e9 ug
#     / bucket area (m2)

flbs <- flbs %>%
  mutate(
    TP_kg_m2 =
      (
        Result_ugL *
          (1 / 1000000000) *
          Volume_L
      ) /
      bucket_area_m2
  )


# 9. Scale TP deposition to Lake McDonald -------------------------------------

# Scale deposition per unit area to the full lake surface area.

flbs <- flbs %>%
  mutate(
    TP_wet_kg =
      TP_kg_m2 *
      lake_area_m2
  )


# 10. Plot TP concentration ----------------------------------------------------

ggplot(
  flbs,
  aes(
    x = Date,
    y = Result_ugL
  )
) +
  geom_point(
    size = 1,
    color = "salmon1"
  ) +
  labs(
    title = "A",
    x = "",
    y = expression(
      C[W] * " (" * mu * g * L^-1 * ")"
    )
  ) +
  scale_x_datetime(
    limits = as.POSIXct(
      c(
        "2007-10-01",
        "2023-10-01"
      ),
      tz = "UTC"
    ),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  scale_y_log10(
    labels = scales::comma
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 12)
  )


# 11. Plot TP deposition per unit area ----------------------------------------

ggplot(
  flbs,
  aes(
    x = Date,
    y = TP_kg_m2
  )
) +
  geom_point(
    size = 1,
    color = "salmon1"
  ) +
  labs(
    title = "B",
    x = "",
    y = expression(
      F[W] * " (" * kg * "-" * P ~ m^-2 ~ wk^-1 * ")"
    )
  ) +
  scale_x_datetime(
    limits = as.POSIXct(
      c(
        "2007-10-01",
        "2023-10-01"
      ),
      tz = "UTC"
    ),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(
      0,
      0.0000025
    ),
    labels = scales::comma
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 12)
  )


# 12. Plot TP deposition to Lake McDonald -------------------------------------

ggplot(
  flbs,
  aes(
    x = Date,
    y = TP_wet_kg
  )
) +
  geom_point(
    size = 1,
    color = "salmon1"
  ) +
  labs(
    title = "C",
    x = "Date",
    y = expression(
      F[W] * " (" * kg * "-" * P ~ wk^-1 * ")"
    )
  ) +
  scale_x_datetime(
    limits = as.POSIXct(
      c(
        "2007-10-01",
        "2023-10-01"
      ),
      tz = "UTC"
    ),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(
      0,
      100
    ),
    labels = scales::comma
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(
      size = 8,
      angle = 90,
      hjust = 1
    ),
    plot.title = element_text(size = 12)
  )

# 13. Save processed wet-deposition dataset -----------------------------------

write.csv(
  flbs,
  here(
    "2_incremental",
    "Fw_P.csv"
  ),
  row.names = FALSE
)
