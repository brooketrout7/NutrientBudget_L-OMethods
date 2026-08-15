# ==============================================================================
# Estimate nitrogen and phosphorus burial in Lake McDonald sediments
#
# Purpose:
#   Estimate weekly N and P burial using sedimentation rate, dry bulk density,
#   nutrient mass proportions, and the estimated area of sediment accumulation.
#
#
# Notes:
#   - ugP_ugsed and ugN_ugsed are nutrient mass proportions and are
#     numerically equivalent to g or kg nutrient / g or kg sediment.
#   - Burial estimates are calculated using minimum, median, and maximum
#     nutrient proportions observed in the sediment core.
#   - A dynamic-ratio approach is used to estimate the fraction of the lake
#     dominated by sediment accumulation and to adjust permanent burial calculations. 
# ==============================================================================


# 1. Read burial dataset -------------------------------------------------------

burial <- read.csv(
  here(
    "0_data",
    "burial.csv"
  )
)


# 2. Define constants ----------------------------------------------------------

# Linear sedimentation rate (cm/yr)

SR_cm_yr <- 0.20


# Dry bulk density (g/cm3)

DBD_g_cm3 <- 0.2843


# Lake McDonald surface area (m2)

SA_m2 <- 27810670.1791


# Convert lake surface area to cm2
#
# 1 m2 = 10,000 cm2

SA_cm2 <- SA_m2 * 10000


# Convert lake surface area to km2
#
# 1 km2 = 1,000,000 m2

SA_km2 <- SA_m2 / 1000000


# Mean depth of Lake McDonald (m)

z <- 45.7


# Water depth at sediment-core location (m)

zcore <- 143


# 3. Prepare sedimentation-rate and density data for plotting -----------------

# Retain dated-core observations with sedimentation-rate estimates.
#
# Dry bulk density values correspond to the same sediment intervals.

mcd <- burial %>%
  filter(
    !is.na(sed.rate)
  ) %>%
  mutate(
    density = c(
      0.2843,
      0.5184,
      0.5537,
      0.6090,
      0.9537,
      1.0806,
      0.5755
    )
  )


# 4. Plot sedimentation rate and dry bulk density -----------------------------

ggplot(
  mcd,
  aes(y = actual_depth)
) +
  geom_path(
    aes(
      x = sed.rate,
      color = "Accrual (cm/yr)"
    ),
    linewidth = 0.4
  ) +
  geom_point(
    aes(
      x = sed.rate,
      color = "Accrual (cm/yr)"
    ),
    size = 2
  ) +
  geom_path(
    aes(
      x = density,
      color = "Dry Density (g/cm3)"
    ),
    linewidth = 0.4
  ) +
  geom_point(
    aes(
      x = density,
      color = "Dry Density (g/cm3)"
    ),
    size = 2
  ) +
  geom_errorbar(
    aes(
      xmin = sed.rate - sed.rate.error,
      xmax = sed.rate + sed.rate.error
    ),
    orientation = "y",
    width = 0.4,
    linewidth = 0.3
  ) +
  scale_x_continuous(
    position = "top",
    name = "",
    breaks = seq(
      0,
      2,
      by = 0.1
    )
  ) +
  scale_color_manual(
    values = c(
      "Accrual (cm/yr)" = "black",
      "Dry Density (g/cm3)" = "grey40"
    )
  ) +
  theme_classic() +
  theme(
    axis.title.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) + scale_y_reverse()



ggplot(
  mcd,
  aes(y = age)
) +
  geom_path(
    aes(
      x = sed.rate,
      color = "Accrual (cm/yr)"
    ),
    linewidth = 0.4
  ) +
  geom_point(
    aes(
      x = sed.rate,
      color = "Accrual (cm/yr)"
    ),
    size = 2
  ) +
  geom_path(
    aes(
      x = density,
      color = "Dry Density (g/cm3)"
    ),
    linewidth = 0.4
  ) +
  geom_point(
    aes(
      x = density,
      color = "Dry Density (g/cm3)"
    ),
    size = 2
  ) +
  geom_errorbar(
    aes(
      xmin = sed.rate - sed.rate.error,
      xmax = sed.rate + sed.rate.error
    ),
    orientation = "y",
    width = 0.4,
    linewidth = 0.3
  ) +
  scale_x_continuous(
    position = "top",
    name = "",
    breaks = seq(
      0,
      2,
      by = 0.1
    )
  ) +
  scale_y_continuous(
    name = "Year (CE)",
    limits = c(
      1900,
      NA
    ),
    breaks = seq(
      1900,
      2025,
      by = 5
    )
  ) +
  scale_color_manual(
    values = c(
      "Accrual (cm/yr)" = "black",
      "Dry Density (g/cm3)" = "grey40"
    )
  ) +
  theme_classic() +
  theme(
    axis.title.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )


# 5. Plot phosphorus mass proportion through time -----------------------------

p <- burial %>%
  filter(
    !is.na(ugP_ugsed)
  )


ggplot(
  p,
  aes(y = actual_depth)
) +
  geom_path(
    aes(x = ugP_ugsed),
    linewidth = 0.4, color = "salmon1"
  ) +
  geom_point(
    aes(x = ugP_ugsed),
    size = 2, color = "salmon1"
  ) +
  scale_x_continuous(
    position = "top",
    name = "P (g-P/g-sediment)",
    breaks = seq(
      0,
      0.003,
      by = 0.0005
    )
  ) +
  labs(
    y = "Depth (cm)"
  ) +
  theme_classic() +
  theme(
    axis.title.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank()
  ) + scale_y_reverse()


# 6. Plot nitrogen mass proportion through time ------------------------------

n <- burial %>%
  filter(
    !is.na(ugN_ugsed)
  )


ggplot(
  n,
  aes(y = actual_depth)
) +
  geom_path(
    aes(x = ugN_ugsed),
    linewidth = 0.4, color ="mediumpurple4"
  ) +
  geom_point(
    aes(x = ugN_ugsed),
    size = 2, color = "mediumpurple4"
  ) +
  scale_x_continuous(
    position = "top",
    name = "N (g-N/g-sediment)",
    breaks = seq(
      0,
      0.004,
      by = 0.0005
    )
  ) +
  labs(
    y = "Depth (cm)"
  ) +
  theme_classic() +
  theme(
    axis.title.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank()
  ) + scale_y_reverse()


# 7. Plot distribution of phosphorus mass proportion -------------------------

burial <- burial %>%
  filter(actual_depth >= 10)
  
ggplot(
  burial,
  aes(x = ugP_ugsed)
) +
  geom_histogram(
    bins = 20,
    color = "black",
    fill = "salmon1"
  ) +
  geom_vline(
    xintercept = min(
      burial$ugP_ugsed,
      na.rm = TRUE
    ),
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = median(
      burial$ugP_ugsed,
      na.rm = TRUE
    ),
    linewidth = 1
  ) +
  geom_vline(
    xintercept = max(
      burial$ugP_ugsed,
      na.rm = TRUE
    ),
    linetype = "dashed"
  ) +
  labs(
    x = "P (g-P/g-sediment)",
    y = "Count"
  ) +
  theme_classic()


# 8. Plot distribution of nitrogen mass proportion --------------------------

ggplot(
  burial,
  aes(x = ugN_ugsed)
) +
  geom_histogram(
    bins = 20,
    color = "black",
    fill = "mediumpurple4"
  ) +
  geom_vline(
    xintercept = min(
      burial$ugN_ugsed,
      na.rm = TRUE
    ),
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = median(
      burial$ugN_ugsed,
      na.rm = TRUE
    ),
    linewidth = 1
  ) +
  geom_vline(
    xintercept = max(
      burial$ugN_ugsed,
      na.rm = TRUE
    ),
    linetype = "dashed"
  ) +
  labs(
    x = "N (g-N/g-sediment)",
    y = "Count"
  ) +
  theme_classic()


# 9. Calculate burial using depth-scaled whole-lake area ----------------------

# Calculate sediment mass accumulation rate (MAR):
#
#   MAR =
#     sedimentation rate
#     * dry bulk density
#     * mean-depth / core-depth scaling factor
#     * lake area
#
# Lake area is expressed in cm2 so that units are compatible with
# sedimentation rate (cm/yr) and dry bulk density (g/cm3).
#
# The final factor converts g sediment/yr to kg sediment/yr.

MAR <-
  SR_cm_yr *
  DBD_g_cm3 *
  (z / zcore) *
  SA_cm2 *
  (1 / 1000)


# Weekly phosphorus burial estimates

Sedimentation_P_kg_wk_min <-
  MAR *
  min(
    burial$ugP_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)

Sedimentation_P_kg_wk_best <-
  MAR *
  median(
    burial$ugP_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)

Sedimentation_P_kg_wk_max <-
  MAR *
  max(
    burial$ugP_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)


# Weekly nitrogen burial estimates

Sedimentation_N_kg_wk_min <-
  MAR *
  min(
    burial$ugN_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)

Sedimentation_N_kg_wk_best <-
  MAR *
  median(
    burial$ugN_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)

Sedimentation_N_kg_wk_max <-
  MAR *
  max(
    burial$ugN_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)


# 10. Estimate fraction of lake dominated by accumulation ----------------------

# Dynamic ratio:
#
#   DR = sqrt(Area / mean depth)
#
# where:
#   Area = lake surface area (km2)
#   mean depth = m

DR <- sqrt(
  SA_km2 / z
)


# Estimate fraction of lake dominated by erosion and transport (ET):
#
#   ET = 0.25 * DR * 41^(0.061 / DR)

ET <-
  0.25 *
  DR *
  (41^(0.061 / DR))


# Fraction of lake dominated by sediment accumulation

A <- 1 - ET


# 11. Calculate burial using sediment accumulation area ------------------------

# Recalculate sediment mass accumulation rate using only the estimated
# fraction of lake area dominated by accumulation.

MAR <-
  SR_cm_yr *
  DBD_g_cm3 *
  (z / zcore) *
  (SA_cm2 * A) *
  (1 / 1000)


# Weekly phosphorus burial estimates

Sedimentation_P_kg_wk_min <-
  MAR *
  min(
    burial$ugP_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)

Sedimentation_P_kg_wk_best <-
  MAR *
  median(
    burial$ugP_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)

Sedimentation_P_kg_wk_max <-
  MAR *
  max(
    burial$ugP_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)


# Weekly nitrogen burial estimates

Sedimentation_N_kg_wk_min <-
  MAR *
  min(
    burial$ugN_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)

Sedimentation_N_kg_wk_best <-
  MAR *
  median(
    burial$ugN_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)

Sedimentation_N_kg_wk_max <-
  MAR *
  max(
    burial$ugN_ugsed,
    na.rm = TRUE
  ) *
  (1 / 52)


