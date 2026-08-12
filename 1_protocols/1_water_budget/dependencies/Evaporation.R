# ==============================================================================
# Estimate evaporation from Lake McDonald
#
# Purpose:
#   Estimate daily evaporation depth and water-volume loss from Lake McDonald
#   using NASA POWER solar radiation and air temperature data and the
#   Simple Abtew Method.
#
#
# Notes:
#   - Daily mean air temperature is used for the best approximation.
#   - Daily minimum and maximum air temperatures are used to characterize
#     sensitivity of the evaporation estimate to temperature.
# ==============================================================================


# 1. Define constants ----------------------------------------------------------

# Lake McDonald coordinates

lon <- -114.0102
lat <- 48.49552


# Study period

start_date <- "2007-10-01"
end_date <- "2023-09-30"


# Lake McDonald surface area (m2)

lake_area_m2 <- 27810670.1791


# Simple Abtew coefficient

k_abtew <- 0.53


# 2. Download NASA POWER data --------------------------------------------------

# NASA POWER variables:
#
# ALLSKY_SFC_SW_DWN = all-sky surface shortwave downward radiation
#                     (MJ m-2 day-1)
#
# T2M_MAX = maximum air temperature at 2 m (degrees C)
# T2M_MIN = minimum air temperature at 2 m (degrees C)
# T2M     = mean air temperature at 2 m (degrees C)

power_data <- nasapower::get_power(
  community = "AG",
  pars = c(
    "ALLSKY_SFC_SW_DWN",
    "T2M_MAX",
    "T2M_MIN",
    "T2M"
  ),
  lonlat = c(
    lon,
    lat
  ),
  dates = c(
    start_date,
    end_date
  ),
  temporal_api = "DAILY"
) %>%
  as.data.frame() %>%
  select(
    YYYYMMDD,
    ALLSKY_SFC_SW_DWN,
    T2M_MAX,
    T2M_MIN,
    T2M
  ) %>%
  rename(
    date = YYYYMMDD,
    solar_mj_m2_d = ALLSKY_SFC_SW_DWN,
    tmax = T2M_MAX,
    tmin = T2M_MIN,
    tavg = T2M
  ) %>%
  mutate(
    date = as.Date(date)
  )


# Save original downloaded NASA POWER data

write.csv(
  power_data,
  here(
    "0_data",
    "NASA_POWER_daily.csv"
  ),
  row.names = FALSE
)


# 3. Estimate daily evaporation ------------------------------------------------

# Simple Abtew Method:
#
#   E = K * Rs / lambda
#
# where:
#   E      = evaporation depth (mm/day)
#   K      = dimensionless coefficient (0.53)
#   Rs     = solar radiation (MJ/m2/day)
#   lambda = latent heat of vaporization (MJ/kg)
#
# Latent heat of vaporization:
#
#   lambda = 2.501 - 0.002361 * T
#
# Daily mean air temperature is used for the best approximation.
# Daily minimum and maximum air temperatures are used to characterize
# sensitivity of the evaporation estimate to temperature.

evap <- power_data %>%
  mutate(
    
    # Latent heat of vaporization calculated using daily air temperature.
    # Because lambda decreases as temperature increases, names refer to
    # the temperature input rather than the resulting lambda magnitude.
    
    lambda_tmax =
      2.501 -
      0.002361 *
      tmax,
    
    lambda_tmin =
      2.501 -
      0.002361 *
      tmin,
    
    lambda =
      2.501 -
      0.002361 *
      tavg,
    
    
    # Daily evaporation depth (mm/day)
    
    LE_mm_d_max =
      k_abtew *
      (
        solar_mj_m2_d /
          lambda_tmax
      ),
    
    LE_mm_d_min =
      k_abtew *
      (
        solar_mj_m2_d /
          lambda_tmin
      ),
    
    LE_mm_d =
      k_abtew *
      (
        solar_mj_m2_d /
          lambda
      ),
    
    
    # Convert evaporation depth (mm/day) to water-volume loss (m3/day):
    #
    #   evaporation depth (mm/day)
    #   * 1 m / 1000 mm
    #   * lake surface area (m2)
    
    L3E_m3_d =
      (LE_mm_d / 1000) *
      lake_area_m2,
    
    L3E_m3_d_min =
      (LE_mm_d_min / 1000) *
      lake_area_m2,
    
    L3E_m3_d_max =
      (LE_mm_d_max / 1000) *
      lake_area_m2
  ) %>%
  select(
    date,
    LE_mm_d,
    LE_mm_d_min,
    LE_mm_d_max,
    L3E_m3_d,
    L3E_m3_d_min,
    L3E_m3_d_max
  )


# 4. Save evaporation estimates ------------------------------------------------

write.csv(
  evap,
  here(
    "2_incremental",
    "Le.csv"
  ),
  row.names = FALSE
)


# 5. Plot daily evaporation depth ---------------------------------------------

ggplot(
  evap,
  aes(x = date)
) +
  geom_ribbon(
    aes(
      ymin = LE_mm_d_min,
      ymax = LE_mm_d_max
    ),
    fill = "cadetblue",
    alpha = 0.4
  ) +
  geom_point(
    aes(
      y = LE_mm_d
    ),
    color = "cadetblue",
    size = 0.5
  ) +
  labs(
    x = "",
    y = expression(
      L[E] * " (" * mm ~ d^-1 * ")"
    )
  ) +
  scale_x_date(
    limits = as.Date(
      c(
        "2007-10-01",
        "2023-10-01"
      )
    ),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(
      0,
      0
    )
  ) +
  scale_y_continuous(
    limits = c(
      0,
      8
    )
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(
      size = 12
    ),
    axis.text.y = element_text(
      size = 12
    ),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )


# 6. Plot daily evaporation volume --------------------------------------------

ggplot(
  evap,
  aes(x = date)
) +
  geom_ribbon(
    aes(
      ymin = L3E_m3_d_min,
      ymax = L3E_m3_d_max
    ),
    fill = "cadetblue",
    alpha = 0.4
  ) +
  geom_point(
    aes(
      y = L3E_m3_d
    ),
    color = "cadetblue",
    size = 0.5
  ) +
  labs(
    x = "Date",
    y = expression(
      L[E] * " (" * m^3 ~ d^-1 * ")"
    )
  ) +
  scale_x_date(
    limits = as.Date(
      c(
        "2007-10-01",
        "2023-10-01"
      )
    ),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(
      0,
      0
    )
  ) +
  scale_y_log10(
    breaks = c(
      1,
      10,
      1000,
      10000,
      200000
    ),
    labels = scales::comma
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(
      size = 12
    ),
    axis.text.y = element_text(
      size = 12
    ),
    axis.text.x = element_text(
      size = 8,
      angle = 90,
      hjust = 1
    )
  )
