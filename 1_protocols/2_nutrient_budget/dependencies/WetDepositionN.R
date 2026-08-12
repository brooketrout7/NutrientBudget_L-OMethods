# ==============================================================================
# Estimate wet nitrogen deposition to Lake McDonald
#
# Purpose:
#   Calculate weekly wet inorganic nitrogen deposition from NADP measurements
#   of nitrate (NO3) and ammonium (NH4), then scale deposition from the NADP
#   collection bucket to the surface area of Lake McDonald.
#
#
# Notes:
#   - NADP NO3 and NH4 concentrations are reported in mg/L.
#   - Concentrations are converted to mass of N using molecular-weight ratios.
#   - The original bucket-based calculation is retained.
#   - A simplified calculation is also retained as a check; bucket area
#     cancels when calculating deposition per unit area.
# ==============================================================================



# 1. Define constants ----------------------------------------------------------

# Lake McDonald surface area (m2)
lake_area_m2 <- 27810670.1791

# NADP collection bucket area
# 678.9 cm2 = 0.06789 m2
bucket_area_cm2 <- 678.9
bucket_area_m2 <- 0.06789

# Atomic mass of nitrogen (mg N/mol)
N_molar_mass <- 14.00674

# Molecular masses
NO3_molar_mass <- 62.00494
NH4_molar_mass <- 18.03834


# 2. Read NADP data ------------------------------------------------------------

nadp <- read.csv(
  here(
    "0_data",
    "NTN-mt05-W-s.csv"
  ),
  header = TRUE,
  na.strings = ""
)


# 3. Format NADP collection dates ---------------------------------------------

# NADP collection dates are reported in Greenwich Mean Time (GMT).
# Convert dateOn and dateOff to POSIXct and express timestamps in UTC.

nadp$dateOn <- as.POSIXct(
  nadp$dateOn,
  format = "%m/%d/%Y %H:%M",
  tz = "GMT"
)

nadp$dateOn <- with_tz(
  nadp$dateOn,
  tzone = "UTC"
)

nadp$dateOff <- as.POSIXct(
  nadp$dateOff,
  format = "%m/%d/%Y %H:%M",
  tz = "GMT"
)

nadp$dateOff <- with_tz(
  nadp$dateOff,
  tzone = "UTC"
)

# 4. Round collection times and clean values ----------------------------------

# Round collection dates to the nearest hour.

nadp <- nadp %>%
  mutate(
    dateOn = round_date(
      dateOn,
      unit = "hour"
    ),
    dateOff = round_date(
      dateOff,
      unit = "hour"
    )
  )


# Replace NADP invalid-value code (-9) with NA.

nadp[nadp == -9] <- NA


# Convert relevant variables to numeric.

nadp$NH4 <- as.numeric(
  nadp$NH4
)

nadp$NO3 <- as.numeric(
  nadp$NO3
)

nadp$subppt <- as.numeric(
  nadp$subppt
)


# 5. Calculate precipitation volume collected --------------------------------

# Calculate the volume of precipitation collected by the NADP bucket.
#
#   Volume (L) =
#     precipitation depth (mm)
#     * 1 cm / 10 mm
#     * bucket area (cm2)
#     * 1 L / 1000 cm3

nadp$v_L <-
  (nadp$subppt / 10) *
  bucket_area_cm2 *
  (1 / 1000)


# 6. Calculate nitrogen mass in collected precipitation -----------------------

# Convert NO3 concentration to mass of N.
#
#   NO3-N mass (kg) =
#     NO3 concentration (mg/L)
#     * 14.00674 mg N / 62.00494 mg NO3
#     * sample volume (L)
#     * 1 kg / 1,000,000 mg

nadp$no3_kg <-
  (
    (
      nadp$NO3 *
        N_molar_mass /
        NO3_molar_mass
    ) *
      nadp$v_L *
      (1 / 1000000)
  )


# Convert NH4 concentration to mass of N.
#
#   NH4-N mass (kg) =
#     NH4 concentration (mg/L)
#     * 14.00674 mg N / 18.03834 mg NH4
#     * sample volume (L)
#     * 1 kg / 1,000,000 mg

nadp$nh4_kg <-
  (
    (
      nadp$NH4 *
        N_molar_mass /
        NH4_molar_mass
    ) *
      nadp$v_L *
      (1 / 1000000)
  )


# Total inorganic nitrogen deposited in the collection bucket.

nadp$tn_kg <-
  nadp$no3_kg +
  nadp$nh4_kg


# 7. Calculate nitrogen deposition per unit area ------------------------------

# Calculate deposition rate per square meter of bucket area.
#
#   deposition (kg-N/m2) =
#     N mass (kg) / bucket area (m2)

nadp$tn_m2 <-
  nadp$tn_kg /
  bucket_area_m2


# 8. Verify using simplified deposition equation ------------------------------

# The bucket area appears in both:
#
#   1. the precipitation-volume calculation, and
#   2. the denominator of the deposition-area calculation.
#
# Therefore, bucket area cancels and the calculation can be simplified.
#
# This alternative calculation is retained as a check against the
# bucket-based calculation above.

nadp$tn_kg_m2_alt <-
  (
    (
      nadp$NO3 *
        N_molar_mass /
        NO3_molar_mass
    ) *
      nadp$subppt *
      0.000001
  ) +
  (
    (
      nadp$NH4 *
        N_molar_mass /
        NH4_molar_mass
    ) *
      nadp$subppt *
      0.000001
  )


# 9. Scale wet nitrogen deposition to Lake McDonald ---------------------------

# Scale deposition per unit area to the surface area of Lake McDonald:
#
#   TN wet deposition (kg-N) =
#     deposition (kg-N/m2) * lake area (m2)

nadp <- nadp %>%
  mutate(
    TN_wet_kg =
      tn_kg_m2_alt *
      lake_area_m2
  )


# 10. Restrict record to study period -----------------------------------------

nadp <- nadp %>%
  filter(
    dateOff >= as.POSIXct(
      "2007-10-02 15:00:00",
      tz = "UTC"
    ) &
      dateOff <= as.POSIXct(
        "2023-10-03 19:00:00",
        tz = "UTC"
      )
  )


# 11. Plot wet nitrogen concentration -----------------------------------------

ggplot(
  data = nadp,
  aes(
    x = dateOff,
    y = (NO3 + NH4 * 1000)
  )
) +
  geom_point(
    size = 1,
    color = "mediumpurple4"
  ) +
  theme_classic() +
  labs(
    title = "A",
    x = "",
    y = expression(
      C[W] * " (" * mu * g * L^-1 * ")"
    )
  ) +
  scale_x_datetime(
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 20)
  ) +
  scale_y_continuous(
    limits = c(
      0,
      5000
    ),
    labels = scales::comma
  ) +
  scale_y_log10()


# 12. Plot wet nitrogen deposition per unit area ------------------------------

ggplot(
  data = nadp,
  aes(
    x = dateOff,
    y = tn_m2
  )
) +
  geom_point(
    size = 1,
    color = "mediumpurple4"
  ) +
  theme_classic() +
  labs(
    title = "B",
    x = "",
    y = expression(
      F[W] * " (" * kg * "-" * N ~ m^-2 ~ wk^-1 * ")"
    )
  ) +
  scale_x_datetime(
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 20)
  ) +
  scale_y_continuous(
    limits = c(
      0,
      0.000015
    ),
    labels = scales::comma
  ) +
  ylim(
    0,
    0.00002
  )



# 13. Plot wet nitrogen deposition to Lake McDonald ---------------------------

ggplot(
  data = nadp,
  aes(
    x = dateOff,
    y = TN_wet_kg
  )
) +
  geom_point(
    size = 1,
    color = "mediumpurple4"
  ) +
  theme_classic() +
  labs(
    title = "C",
    x = "Date",
    y = expression(
      F[W] * " (" * kg * "-" * N ~ wk^-1 * ")"
    )
  ) +
  scale_x_datetime(
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(
      size = 8,
      angle = 90,
      hjust = 1
    ),
    plot.title = element_text(size = 20)
  ) +
  scale_y_continuous(
    limits = c(
      0,
      500
    ),
    labels = scales::comma
  )

# 14. Retain variables used in nutrient budget --------------------------------

nadp <- nadp %>%
  select(
    dateOn,
    dateOff,
    TN_wet_kg
  )


# 15. Save processed wet nitrogen deposition data -----------------------------

write.csv(
  nadp,
  here(
    "2_incremental",
    "Fw_N.csv"
  ),
  row.names = FALSE
)
