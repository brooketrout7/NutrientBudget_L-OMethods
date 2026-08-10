# ==============================================================================
# Estimate wet deposition water input to Lake McDonald
#
# Purpose:
#   Calculate weekly precipitation volume falling directly on Lake McDonald
#   using precipitation measurements from the National Atmospheric Deposition
#   Program (NADP) MT05 station.
#
# Input:
#   0_data/NADP_data/NTN-mt05-W-s.csv
#
# Outputs:
#   2_incremental/WetDeposition.csv
#   3_products/LW_L3W.png
#
# Notes:
#   - NADP precipitation is reported in mm.
#   - Weekly precipitation volume is calculated as:
#
#       precipitation depth (m) * lake surface area (m2)
#
#   - Lake McDonald surface area = 27,810,670.1791 m2.
# ==============================================================================


# 0. Load packages -------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(patchwork)

options(scipen = 999)


# 1. Define constants ----------------------------------------------------------

lake_area_m2 <- 27810670.1791


# 2. Read NADP data ------------------------------------------------------------

nadp <- read.csv(
  here(
    "0_data",
    "NTN-mt05-W-s.csv"
  ),
  header = TRUE,
  na.strings = ""
)


# 3. Clean NADP data -----------------------------------------------------------

# Convert sampling dates to UTC.
# NADP dates are reported as month/day/year hour:minute.

nadp <- nadp %>%
  mutate(
    dateOn = as.POSIXct(
      dateOn,
      format = "%m/%d/%Y %H:%M",
      tz = "GMT"
    ),
    dateOff = as.POSIXct(
      dateOff,
      format = "%m/%d/%Y %H:%M",
      tz = "GMT"
    ),
    dateOn = with_tz(dateOn, tzone = "UTC"),
    dateOff = with_tz(dateOff, tzone = "UTC")
  ) %>%
  select(
    dateOn,
    dateOff,
    NH4,
    NO3,
    subppt
  ) %>%
  mutate(
    # NADP missing-value code
    subppt = ifelse(subppt == -9.99, NA, subppt),
    
    # Round sampling dates to nearest hour
    dateOn = round_date(dateOn, unit = "hour"),
    dateOff = round_date(dateOff, unit = "hour")
  )


# 4. Calculate precipitation volume -------------------------------------------

# subppt = precipitation depth (mm)
#
# Convert mm to m and multiply by lake surface area:
#
#   Lw (m3/week) = subppt (mm/week) * 0.001 (m/mm) * lake area (m2)

nadp <- nadp %>%
  mutate(
    L3w = (subppt / 1000) * lake_area_m2
  )


# 5. Plot weekly precipitation -------------------------------------------------

ggplot(
  nadp,
  aes(x = as.Date(dateOff), y = subppt)
) +
  geom_point(
    color = "cadetblue",
    size = 1
  ) +
  labs(
    title = "A",
    x = "",
    y = expression(
      L[W] * " (" * mm ~ wk^-1 * ")"
    )
  ) +
  scale_x_date(
    limits = as.Date(c("2007-10-01", "2023-10-01")),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  ylim(0, 150) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(
      size = 8,
      angle = 90,
      hjust = 1
    ),
    plot.title = element_text(size = 20)
  )


# 6. Plot weekly precipitation volume -----------------------------------------

ggplot(
  nadp,
  aes(x = as.Date(dateOff), y = L3w)
) +
  geom_point(
    color = "cadetblue",
    size = 1
  ) +
  labs(
    title = "B",
    x = "Date",
    y = expression(
      L[W]^3 * " (" * m^3 ~ wk^-1 * ")"
    )
  ) +
  scale_x_date(
    limits = as.Date(c("2007-10-01", "2023-10-01")),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  scale_y_log10(
    breaks = c(
      1,
      1000,
      10000,
      1000000,
      4000000
    ),
    labels = scales::comma
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(
      size = 8,
      angle = 90,
      hjust = 1
    ),
    plot.title = element_text(size = 20)
  )

# 7. Save processed NADP dataset ----------------------------------------------

write.csv(
  nadp,
  here(
    "2_incremental",
    "Lw.csv"
  ),
  row.names = FALSE
)
