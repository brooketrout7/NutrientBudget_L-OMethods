# ==============================================================================
# Estimate dry nitrogen and phosphorus deposition to Lake McDonald
#
# Purpose:
#   Calculate weekly total nitrogen (TN) and total phosphorus (TP) dry
#   deposition from FLBS deposition samples and scale deposition from the
#   collection bucket to the surface area of Lake McDonald.
#
# Inputs:
#   0_data/Dry_Deposition.csv
#   0_data/FLBSPublicData.csv
#
# Outputs:
#   2_incremental/FD_for_fire_sim.csv
#   2_incremental/FD.csv
#
# Notes:
#   - TN and TP concentrations are reported in ug/L.
#   - Sample volume is converted from mL to L.
#   - Nutrient deposition is first calculated per unit bucket area (kg/m2)
#     and then scaled to the full surface area of Lake McDonald.
#   - TN and TP are processed separately because they have separate
#     concentration records.
# ==============================================================================


# 0. Load packages -------------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(lubridate)
library(patchwork)
library(scales)
library(dplyr)
library(tidyr)
library(here)

options(scipen = 999)


# 1. Define constants ----------------------------------------------------------

# Lake McDonald surface area (m2)
lake_area_m2 <- 27810670.1791

# Dry-deposition collection bucket area:
# 629.02 cm2 / 10,000 = 0.062902 m2
bucket_area_m2 <- 0.062902


# 2. Read FLBS dry-deposition data --------------------------------------------

flbs <- read.csv(
  here(
    "0_data",
    "Dry_Deposition.csv"
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


# 3. Format collection dates --------------------------------------------------

# Convert collection dates to POSIXct in UTC.

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

# Supplemental public data are used only before June 13, 2008.

flbs_add <- flbs_add %>%
  filter(
    Date < "2008-06-13"
  )


# 4. Select and standardize relevant variables --------------------------------

flbs <- flbs %>%
  select(
    Date,
    Volume,
    Test,
    CorrectedReportedResult
  ) %>%
  rename(
    Parameter = Test,
    Value = CorrectedReportedResult
  )


# Standardize nutrient names.

flbs <- flbs %>%
  mutate(
    Parameter = recode(
      Parameter,
      "Total Nitrogen" = "TN",
      "Total Phosphorus" = "TP"
    )
  )


# Retain matching variables from the supplemental dataset.

flbs_add <- flbs_add %>%
  select(
    Date,
    Volume,
    Parameter,
    Value
  )


# 5. Convert volume and concentration variables -------------------------------

flbs$Volume_mL <- as.numeric(
  flbs$Volume
)

flbs$Volume_L <-
  flbs$Volume_mL / 1000

flbs$Result_ugL <- as.numeric(
  flbs$Value
)


flbs_add$Volume_mL <- as.numeric(
  flbs_add$Volume
)

flbs_add$Volume_L <-
  flbs_add$Volume_mL / 1000

flbs_add$Result_ugL <- as.numeric(
  flbs_add$Value
)


# 6. Combine FLBS datasets -----------------------------------------------------

flbs <- rbind(
  flbs_add,
  flbs
)


# 7. Separate and clean TN records --------------------------------------------

# Isolate total nitrogen observations.

N <- subset(
  flbs,
  Parameter == "TN"
)


# Identify dates with duplicate TN observations.

duplicated_dates <- duplicated(
  N$Date
)

duplicated_values <- N$Date[
  duplicated_dates
]

print(
  duplicated_values
)


# Sum duplicate TN observations occurring on the same date.

N <- N %>%
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


# 8. Separate and clean TP records --------------------------------------------

# Isolate total phosphorus observations.

P <- subset(
  flbs,
  Parameter == "TP"
)


# Identify dates with duplicate TP observations.

duplicated_dates <- duplicated(
  P$Date
)

duplicated_values <- P$Date[
  duplicated_dates
]

print(
  duplicated_values
)


# Sum duplicate TP observations occurring on the same date.

P <- P %>%
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


# 9. Save dry-deposition data for fire simulations ----------------------------

# Save the combined FLBS dataset before separating TN and TP because this
# dataset is used later in the wildfire simulations.

write.csv(
  flbs,
  here(
    "2_incremental",
    "FD_for_fire_sim.csv"
  ),
  row.names = FALSE
)


# 10. Calculate TN and TP deposition per unit area ----------------------------

# Nutrient deposition per square meter:
#
#   deposition (kg/m2) =
#     concentration (ug/L)
#     * 1 kg / 1,000,000,000 ug
#     * sample volume (L)
#     / bucket area (m2)


# Total nitrogen

N$TN_kg_m2 <-
  (
    N$Result_ugL *
      (1 / 1000000000) *
      N$Volume_L
  ) /
  bucket_area_m2


# Total phosphorus

P$TP_kg_m2 <-
  (
    P$Result_ugL *
      (1 / 1000000000) *
      P$Volume_L
  ) /
  bucket_area_m2


# 11. Combine TN and TP deposition estimates ----------------------------------

# Join TN and TP deposition estimates by collection date.

flbs <- N %>%
  left_join(
    P,
    by = "Date"
  ) %>%
  select(
    Date,
    TN_kg_m2,
    TP_kg_m2
  )


# 12. Scale dry deposition to Lake McDonald -----------------------------------

# Scale deposition per unit area to the full surface area of Lake McDonald.

flbs <- flbs %>%
  mutate(
    TN_dry_kg =
      TN_kg_m2 *
      lake_area_m2,
    
    TP_dry_kg =
      TP_kg_m2 *
      lake_area_m2
  )


# 13. Format data for plotting -------------------------------------------------

N$Date <- as.POSIXct(
  N$Date,
  format = "%m/%d/%Y",
  tz = "UTC"
)

N$Result_ugL <- as.numeric(
  N$Result_ugL
)


P$Date <- as.POSIXct(
  P$Date,
  format = "%m/%d/%Y",
  tz = "UTC"
)

P$Result_ugL <- as.numeric(
  P$Result_ugL
)


# 14. Plot collection volume --------------------------------------------------

# Plot the volume of dry-deposition sample collected.
#
# Volume_L / 1000 converts L to m3.

ggplot(
  data = N,
  aes(
    x = Date,
    y = Volume_L / 1000
  )
) +
  geom_point(
    size = 1,
    color = "black"
  ) +
  theme_classic() +
  labs(
    title = "A",
    x = "",
    y = expression(
      L[D]^3 * " (" * m^3 ~ wk^-1 * ")"
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
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(
      size = 8,
      angle = 90,
      hjust = 1
    ),
    plot.title = element_text(size = 12)
  ) +
  scale_y_continuous(
    labels = scales::comma
  )




# 15. Plot dry nitrogen concentration -----------------------------------------

ggplot(
  data = N,
  aes(
    x = Date,
    y = Result_ugL
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
      C[D] * " (" * mu * g * L^-1 * ")"
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
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 12)
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  scale_y_log10()


# 16. Plot dry nitrogen deposition per unit area ------------------------------

ggplot(
  data = flbs,
  aes(
    x = Date,
    y = TN_kg_m2
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
      F[D] * " (" * kg * "-" * N ~ m^-2 ~ wk^-1 * ")"
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
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 12)
  )



# 17. Plot dry nitrogen deposition to Lake McDonald ---------------------------

ggplot(
  data = flbs,
  aes(
    x = Date,
    y = TN_dry_kg
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
      F[D] * " (" * kg * "-" * N ~ wk^-1 * ")"
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
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(
      size = 8,
      angle = 90,
      hjust = 1
    ),
    plot.title = element_text(size = 12)
  ) +
  scale_y_continuous(
    limits = c(
      0,
      1500
    ),
    labels = scales::comma
  )

# 18. Plot dry phosphorus concentration ---------------------------------------

ggplot(
  data = P,
  aes(
    x = Date,
    y = Result_ugL
  )
) +
  geom_point(
    size = 1,
    color = "salmon1"
  ) +
  theme_classic() +
  labs(
    title = "A",
    x = "",
    y = expression(
      C[D] * " (" * mu * g * L^-1 * ")"
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
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 12)
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  scale_y_log10()



# 19. Plot dry phosphorus deposition per unit area ----------------------------

ggplot(
  data = flbs,
  aes(
    x = Date,
    y = TP_kg_m2
  )
) +
  geom_point(
    size = 1,
    color = "salmon1"
  ) +
  theme_classic() +
  labs(
    title = "B",
    x = "",
    y = expression(
      F[D] * " (" * kg * "-" * P ~ m^-2 ~ wk^-1 * ")"
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
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 12)
  ) +
  scale_y_continuous(
    limits = c(
      0,
      0.000015
    ),
    labels = scales::comma
  )


# 20. Plot dry phosphorus deposition to Lake McDonald -------------------------

ggplot(
  data = flbs,
  aes(
    x = Date,
    y = TP_dry_kg
  )
) +
  geom_point(
    size = 1,
    color = "salmon1"
  ) +
  theme_classic() +
  labs(
    title = "C",
    x = "Date",
    y = expression(
      F[D] * " (" * kg * "-" * P ~ wk^-1 * ")"
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
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(
      size = 8,
      angle = 90,
      hjust = 1
    ),
    plot.title = element_text(size = 12)
  ) +
  scale_y_continuous(
    limits = c(
      0,
      500
    ),
    labels = scales::comma
  )


# 21. Save processed dry-deposition dataset -----------------------------------

write.csv(
  flbs,
  here(
    "2_incremental",
    "FD.csv"
  ),
  row.names = FALSE
)
