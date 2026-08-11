# ==============================================================================
# Reconstruct Lake McDonald water level and lake volume
#
# Purpose:
#   1. Relate water level measured at Lake McDonald Lodge to water level
#      measured at Lower McDonald Creek (LMC).
#   2. Relate LMC water level to the Middle Fork Flathead River donor gage.
#   3. Use the donor gage record to reconstruct Lake McDonald water level.
#   4. Convert reconstructed changes in lake level to changes in lake volume.
# ==============================================================================


# 0. Load packages -------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(brms)
library(patchwork)
library(scales)
library(here)

options(scipen = 999)


# 1. Define constants ----------------------------------------------------------

lake_area_m2 <- 27810670.1791

# Initial Lake McDonald volume (m3)
initial_lake_volume_m3 <- 1491191000

# Time zone used by lake-level loggers
logger_tz <- "America/Denver"


# 2. Read Lake McDonald Lodge water-level data --------------------------------

lodge <- read.csv(
  here(
    "0_data",
    "Ll_lodge.csv"
  ),
  header = FALSE
) %>%
  select(
    date_time = V2,
    lodge = V6
  ) %>%
  mutate(
    date_time = as.POSIXct(
      date_time,
      format = "%m/%d/%y %I:%M:%S %p",
      tz = logger_tz
    ),
    lodge = as.numeric(lodge)
  ) %>%
  filter(
    date_time >= as.POSIXct(
      "2024-04-24 14:00:00",
      tz = logger_tz
    ),
    date_time <= as.POSIXct(
      "2024-09-14 12:00:00",
      tz = logger_tz
    )
  )


# 3. Read Lower McDonald Creek water-level data -------------------------------

lmc <- read.csv(
  here(
    "0_data",
    "Lo.csv"
  ),
  header = FALSE
) %>%
  select(
    date_time = V2,
    lmc = V5
  ) %>%
  mutate(
    date_time = as.POSIXct(
      date_time,
      format = "%m/%d/%y %I:%M:%S %p",
      tz = logger_tz
    ),
    lmc = as.numeric(lmc)
  )


# 4. Merge logger records ------------------------------------------------------

lmc_lake <- lodge %>%
  left_join(
    lmc,
    by = "date_time"
  )

# Identify unmatched observations
na_rows <- lmc_lake %>%
  filter(
    is.na(lodge) |
      is.na(lmc)
  )

print(na_rows)

# Remove unmatched records
lmc_lake <- lmc_lake %>%
  drop_na(
    lodge,
    lmc
  )


# 5. Correct Lake McDonald Lodge logger record --------------------------------

# Logger retrieval/redeployment produced offsets in the observed water-level
# record. Apply correction factors determined from field notes and adjacent measurements.
# Records from 2024-07-18 05:00 through 09:00 are removed because the logger was disturbed.

lmc_lake <- lmc_lake %>%
  filter(
    !between(
      date_time,
      as.POSIXct(
        "2024-07-18 05:00:00",
        tz = logger_tz
      ),
      as.POSIXct(
        "2024-07-18 09:00:00",
        tz = logger_tz
      )
    )
  ) %>%
  mutate(
    lodge = case_when(
      
      date_time <= as.POSIXct(
        "2024-05-27 15:00:00",
        tz = logger_tz
      ) ~ lodge + 0.358,
      
      date_time <= as.POSIXct(
        "2024-06-28 15:00:00",
        tz = logger_tz
      ) ~ lodge + 0.716,
      
      date_time <= as.POSIXct(
        "2024-07-18 04:00:00",
        tz = logger_tz
      ) ~ lodge + 0.619,
      
      date_time <= as.POSIXct(
        "2024-08-19 16:00:00",
        tz = logger_tz
      ) ~ lodge - 0.985,
      
      TRUE ~ lodge - 0.495
    )
  )


# 6. Correct Lower McDonald Creek logger record -------------------------------

# The logger was removed at 2024-05-27 16:00; remove that observation.
#
# Correction factors account for changes associated with logger
# retrieval and redeployment.

lmc_lake <- lmc_lake %>%
  filter(
    date_time != as.POSIXct(
      "2024-05-27 16:00:00",
      tz = logger_tz
    )
  ) %>%
  mutate(
    lmc = case_when(
      
      date_time <= as.POSIXct(
        "2024-05-27 15:00:00",
        tz = logger_tz
      ) ~ lmc,
      
      date_time <= as.POSIXct(
        "2024-06-28 09:00:00",
        tz = logger_tz
      ) ~ lmc + 0.064,
      
      TRUE ~ lmc + 0.051
    )
  )


# 7. Check corrected logger records -------------------------------------------

ggplot(
  lmc_lake,
  aes(x = date_time, y = lodge)
) +
  geom_point(size = 0.25) +
  labs(
    x = "Hourly Record",
    y = "Water Level at Lake McDonald Lodge (m)"
  ) +
  theme_classic()


ggplot(
  lmc_lake,
  aes(x = date_time, y = lmc)
) +
  geom_point(size = 0.25) +
  labs(
    x = "Hourly Record",
    y = "Lower McDonald Creek Water Level (m)"
  ) +
  theme_classic()

# 8. Plot water levels over time -----------------------------------------------
ggplot(
  lmc_lake,
  aes(x = date_time)
) +
  geom_point(
    aes(
      y = lmc,
      color = "Lower McDonald Creek"
    ),
    size = 0.5
  ) +
  geom_point(
    aes(
      y = lodge,
      color = "Lake McDonald"
    ),
    size = 0.5
  ) +
  labs(
    x = "",
    y = "Water Level (m)",
    color = ""
  ) +
  scale_color_manual(
    values = c(
      "Lower McDonald Creek" = "cadetblue",
      "Lake McDonald" = "midnightblue"
    )
  ) +
  coord_cartesian(
    ylim = c(0, 5)
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 3)
    )
  )



# 9. Relate LMC water level to Lake McDonald ----------------------------------

# Bayesian linear regression:
#
#   Lake McDonald water level ~ Lower McDonald Creek water level

bayes_lake <- brm(
  lodge ~ lmc,
  data = lmc_lake,
  family = gaussian(),
  seed = 123
)

summary(bayes_lake)
plot(bayes_lake)
pp_check(bayes_lake)


# Expected fitted lake levels and 95% credible intervals

lake_fit_pred <- fitted(
  bayes_lake,
  newdata = lmc_lake,
  probs = c(0.025, 0.975)
)

lmc_lake <- lmc_lake %>%
  mutate(
    lake_fit = lake_fit_pred[, "Estimate"],
    lake_fit_lwr = lake_fit_pred[, "Q2.5"],
    lake_fit_upr = lake_fit_pred[, "Q97.5"]
  )

# Plot relationship

ggplot(
  lmc_lake,
  aes(x = lmc, y = lodge)
) +
  geom_ribbon(
    aes(
      ymin = lake_fit_lwr,
      ymax = lake_fit_upr
    ),
    fill = "cadetblue",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = lake_fit),
    color = "cadetblue",
    linewidth = 1
  ) +
  geom_point(
    color = "black",
    size = 0.5
  ) +
  labs(
    x = "Lower McDonald Creek Water Level (m)",
    y = "Lake Water Level (m)"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )



# 10. Read Middle Fork Flathead River donor-gage data --------------------------

MF <- readr::read_csv(
  here(
    "0_data",
    "Q_donor_gage.csv"
  ),
  col_types = cols(
    dateTime = col_datetime()
  )
) %>%
  rename(
    date_time = dateTime
  ) %>%
  mutate(
    # USGS gage height: ft -> m
    height_m = height * 0.3048
  )


# 11. Match donor gage to LMC logger record -----------------------------------

# Convert logger timestamps explicitly to UTC before matching the USGS record.

lmc_lake <- lmc_lake %>%
  mutate(
    date_time = with_tz(
      date_time,
      "UTC"
    )
  ) %>%
  inner_join(
    MF,
    by = "date_time"
  )


# 12. Relate donor-gage height to LMC water level -----------------------------

# Bayesian linear regression:
#
#   Lower McDonald Creek water level ~ Middle Fork Flathead River gage height

bayes_gage <- brm(
  lmc ~ height_m,
  data = lmc_lake,
  family = gaussian(),
  seed = 123
)

summary(bayes_gage)
plot(bayes_gage)
pp_check(bayes_gage)


# Expected fitted values and 95% credible intervals

lmc_fit_preds <- fitted(
  bayes_gage,
  newdata = lmc_lake,
  probs = c(0.025, 0.975)
)

lmc_lake <- lmc_lake %>%
  mutate(
    lmc_fit = lmc_fit_preds[, "Estimate"],
    lmc_fit_lwr = lmc_fit_preds[, "Q2.5"],
    lmc_fit_upr = lmc_fit_preds[, "Q97.5"]
  )


# Plot relationship

ggplot(
  lmc_lake,
  aes(x = height_m, y = lmc)
) +
  geom_ribbon(
    aes(
      ymin = lmc_fit_lwr,
      ymax = lmc_fit_upr
    ),
    fill = "cadetblue",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = lmc_fit),
    color = "cadetblue",
    linewidth = 1
  ) +
  geom_point(
    color = "black",
    size = 0.5
  ) +
  labs(
    x = "Middle Fork Flathead River Gage Height (m)",
    y = "Lower McDonald Creek Water Level (m)"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )


# 13. Reconstruct Lake McDonald water level -----------------------------------

preds <- MF %>%
  select(
    date_time,
    height_m
  )


# Predict Lower McDonald Creek water level from donor gage

lmc_bayes_pred <- predict(
  bayes_gage,
  newdata = preds,
  probs = c(0.025, 0.975)
)

preds <- preds %>%
  mutate(
    lmc = lmc_bayes_pred[, "Estimate"],
    lmc_lwr = lmc_bayes_pred[, "Q2.5"],
    lmc_upr = lmc_bayes_pred[, "Q97.5"]
  )


# Predict Lake McDonald water level from reconstructed LMC level

lake_bayes_pred <- predict(
  bayes_lake,
  newdata = preds,
  probs = c(0.025, 0.975)
)

preds <- preds %>%
  mutate(
    lodge = lake_bayes_pred[, "Estimate"],
    lodge_lwr = lake_bayes_pred[, "Q2.5"],
    lodge_upr = lake_bayes_pred[, "Q97.5"]
  )


# 14. Reconstruct lake volume --------------------------------------------------

# Change in lake volume:
#
#   Delta V = Delta water level * lake surface area
#
# Lake volume is reconstructed relative to an initial volume of
# 1,491,191,000 m3.

lake <- preds %>%
  arrange(date_time) %>%
  mutate(
    volume_change =
      (lodge - lag(lodge)) * lake_area_m2,
    
    volume_change_lwr =
      (lodge_lwr - lag(lodge_lwr)) * lake_area_m2,
    
    volume_change_upr =
      (lodge_upr - lag(lodge_upr)) * lake_area_m2,
    
    # First time step has no preceding observation
    volume_change = replace_na(
      volume_change,
      0
    ),
    
    volume_change_lwr = replace_na(
      volume_change_lwr,
      0
    ),
    
    volume_change_upr = replace_na(
      volume_change_upr,
      0
    ),
    
    lake_volume =
      initial_lake_volume_m3 +
      cumsum(volume_change),
    
    lake_volume_lwr =
      initial_lake_volume_m3 +
      cumsum(volume_change_lwr),
    
    lake_volume_upr =
      initial_lake_volume_m3 +
      cumsum(volume_change_upr)
  ) %>%
  filter(
    date_time <= as.POSIXct(
      "2023-09-30 23:00:00",
      tz = "UTC"
    )
  )


# 15. Save reconstructed datasets ---------------------------------------------

lake_volume <- lake %>%
  select(
    -height_m,
    -lmc,
    -lmc_lwr,
    -lmc_upr
  )


write.csv(
  lake_volume,
  here(
    "2_incremental",
    "Vl.csv"
  ),
  row.names = FALSE
)


# 16. Plot reconstructed water levels -----------------------------------------

ggplot(
  lake,
  aes(x = date_time)
) +
  geom_line(
    aes(
      y = height_m,
      color = "Middle Fork Flathead River"
    ),
    linewidth = 0.25
  ) +
  geom_line(
    aes(
      y = lmc,
      color = "Lower McDonald Creek"
    ),
    linewidth = 0.25
  ) +
  geom_line(
    aes(
      y = lodge,
      color = "Lake McDonald"
    ),
    linewidth = 0.25
  ) +
  labs(
    x = "",
    y = "Gage Height (m)",
    color = ""
  ) +
  scale_x_datetime(
    limits = as.POSIXct(
      c(
        "2007-10-01 00:00:00",
        "2023-10-01 00:00:00"
      ),
      tz = "UTC"
    ),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  scale_color_manual(
    values = c(
      "Middle Fork Flathead River" = "midnightblue",
      "Lower McDonald Creek" = "cyan2",
      "Lake McDonald" = "cadetblue"
    )
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 6),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 1.5)
    )
  )


# 17. Plot reconstructed lake volume ------------------------------------------

ggplot(
  lake_volume,
  aes(x = date_time)
) +
  geom_ribbon(
    aes(
      ymin = lake_volume_lwr / 1e9,
      ymax = lake_volume_upr / 1e9
    ),
    fill = "cadetblue",
    alpha = 0.4
  ) +
  geom_line(
    aes(
      y = lake_volume / 1e9
    ),
    linewidth = 0.25,
    color = "cadetblue"
  ) +
  labs(
    x = "Date",
    y = expression("Volume (km"^3 * ")")
  ) +
  scale_x_datetime(
    limits = as.POSIXct(
      c(
        "2007-10-01 00:00:00",
        "2023-10-01 00:00:00"
      ),
      tz = "UTC"
    ),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(
      size = 8,
      angle = 90,
      hjust = 1,
      vjust = 0.25
    )
  )
