# ============================================================
# Reconstruct Lake McDonald tributary and outlet discharge
# ============================================================
#
# Purpose:
# Reconstruct instantaneous discharge for major Lake McDonald tributaries
# and the lake outlet using the Middle Fork Flathead River
# (USGS 12358500) as a donor gage.
#
# Sites:
#   UMC  = Upper McDonald Creek
#   LMC  = Lower McDonald Creek (Lake McDonald outlet)
#   SNY  = Snyder Creek
#   SPR  = Sprague Creek
#   FISH = Fish Creek
#
# Bayesian log-log regressions are fit between observed discharge
# at each site and concurrent discharge at the Middle Fork Flathead
# River donor gage. These relationships are then used to reconstruct
# hourly discharge for water years 2008–2023.
#
# Donor-gage method:
#   https://doi.org/10.5194/hess-28-545-2024
#
# ============================================================


# 0. Load packages ----

library(tidyverse)
library(lubridate)
library(brms)
library(patchwork)
library(here)

options(scipen = 999)


# 1. Read discharge data ----

# Upper McDonald Creek observed discharge
umc <- read.csv(
  here("0_data", "Qumc.csv")
)

umc <- umc %>%
  select(date_time, discharge_m3_s) %>%
  mutate(
    date_time = as.POSIXct(
      date_time,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    discharge_m3_s = as.numeric(discharge_m3_s)
  ) %>%
  filter(
    date_time >= as.POSIXct(
      "2022-04-28 19:00:00",
      tz = "America/Denver"
    ),
    date_time <= as.POSIXct(
      "2022-09-20 15:00:00",
      tz = "America/Denver"
    )
  ) %>%
  arrange(date_time)


# Lower McDonald Creek observed discharge
lmc <- read.csv(
  here("0_data", "Qlmc.csv")
)

lmc <- lmc %>%
  select(date_time, discharge_m3_s) %>%
  mutate(
    date_time = as.POSIXct(
      date_time,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    discharge_m3_s = as.numeric(discharge_m3_s)
  ) %>%
  filter(
    date_time >= as.POSIXct(
      "2022-04-28 19:00:00",
      tz = "America/Denver"
    ),
    date_time <= as.POSIXct(
      "2022-09-20 15:00:00",
      tz = "America/Denver"
    )
  ) %>%
  arrange(date_time)


# Middle Fork Flathead River donor-gage discharge
# downloaded from Q_donorgage.R

MF <- readr::read_csv(
  here("0_data", "Q_donor_gage.csv"),
  col_types = cols(
    dateTime = col_datetime(format = "")
  )
)


# 2. Prepare concurrent 2022 donor-gage data ----

# Isolate Middle Fork Flathead River discharge during the
# period when Lake McDonald tributary/outlet measurements
# were collected.

MF_2022 <- MF %>%
  filter(
    dateTime >= as.POSIXct(
      "2022-04-28 19:00:00",
      tz = "America/Denver"
    ),
    dateTime <= as.POSIXct(
      "2022-09-20 15:00:00",
      tz = "America/Denver"
    )
  ) %>%
  rename(date_time = dateTime) %>%
  mutate(
    date_time = as.POSIXct(
      date_time,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  )


# Join concurrent donor-gage discharge to UMC and LMC
umc <- umc %>%
  inner_join(MF_2022, by = "date_time")

lmc <- lmc %>%
  inner_join(MF_2022, by = "date_time")


# Create dataframe containing the complete donor-gage record
# used to reconstruct discharge for WY 2008–2023.

preds <- MF %>%
  select(
    dateTime,
    MF_Q_m3_s
  )


# 3. Upper McDonald Creek model ----

# Fit Bayesian log-log relationship between UMC discharge
# and Middle Fork Flathead River discharge.

bayes_umc <- brm(
  log(discharge_m3_s) ~ log(MF_Q_m3_s),
  data = umc,
  family = gaussian()
)

# Model diagnostics
summary(bayes_umc)
plot(bayes_umc)
pp_check(bayes_umc)


# Calculate fitted relationship and 95% credible interval.
# fitted() describes uncertainty in the expected relationship
# and does not include observation-level residual error.

umc_fit <- fitted(
  bayes_umc,
  newdata = umc,
  probs = c(0.025, 0.975)
)

# Back-transform fitted values from log scale
umc$umc_fit <- exp(umc_fit[, "Estimate"])
umc$umc_fit_lwr <- exp(umc_fit[, "Q2.5"])
umc$umc_fit_upr <- exp(umc_fit[, "Q97.5"])


# Generate posterior predictions for the complete donor-gage record

umc_bayes_pred <- predict(
  bayes_umc,
  newdata = preds,
  probs = c(0.025, 0.975)
)

preds$umc_discharge_m3_s <-
  exp(umc_bayes_pred[, "Estimate"])

preds$umc_discharge_m3_s_lwr <-
  exp(umc_bayes_pred[, "Q2.5"])

preds$umc_discharge_m3_s_upr <-
  exp(umc_bayes_pred[, "Q97.5"])


# 4. Lower McDonald Creek model ----

# Fit Bayesian log-log relationship between LMC discharge
# and Middle Fork Flathead River discharge.

bayes_lmc <- brm(
  log(discharge_m3_s) ~ log(MF_Q_m3_s),
  data = lmc,
  family = gaussian()
)

# Model diagnostics
summary(bayes_lmc)
plot(bayes_lmc)
pp_check(bayes_lmc)


# Calculate fitted relationship and 95% credible interval

lmc_fit <- fitted(
  bayes_lmc,
  newdata = lmc,
  probs = c(0.025, 0.975)
)

# Back-transform fitted values
lmc$lmc_fit <- exp(lmc_fit[, "Estimate"])
lmc$lmc_fit_lwr <- exp(lmc_fit[, "Q2.5"])
lmc$lmc_fit_upr <- exp(lmc_fit[, "Q97.5"])


# Generate posterior predictions for complete donor-gage record

lmc_bayes_pred <- predict(
  bayes_lmc,
  newdata = preds,
  probs = c(0.025, 0.975)
)

preds$lmc_discharge_m3_s <-
  exp(lmc_bayes_pred[, "Estimate"])

preds$lmc_discharge_m3_s_lwr <-
  exp(lmc_bayes_pred[, "Q2.5"])

preds$lmc_discharge_m3_s_upr <-
  exp(lmc_bayes_pred[, "Q97.5"])


# 5. Snyder Creek model ----

# Dates of field discharge measurements
sny_date <- as.POSIXct(
  c(
    "2022-04-28 19:00",
    "2022-05-10 10:00",
    "2022-05-24 15:00",
    "2022-06-07 14:00",
    "2022-06-29 08:00",
    "2022-07-19 16:00",
    "2022-08-03 13:00",
    "2022-09-06 16:00",
    "2022-09-20 14:00"
  ),
  format = "%Y-%m-%d %H:%M",
  tz = "America/Denver"
)


# Corresponding Snyder Creek discharge measurements (m3/s)

discharge_sny_m3_s <- c(
  0.4132,
  0.7673,
  0.8451,
  1.8525,
  1.7300,
  0.6452,
  0.2524,
  0.0949,
  0.0286
)


# Create Snyder Creek field dataset
sny <- data.frame(
  date = sny_date,
  discharge_sny_m3_s
)

# Convert measurement timestamps to UTC
sny$date <- with_tz(
  sny$date,
  tz = "UTC"
)


# Extract concurrent Middle Fork discharge

filtered_MF <- MF %>%
  filter(dateTime %in% sny$date)

sny <- sny %>%
  mutate(
    MF_Q_m3_s = filtered_MF$MF_Q_m3_s
  )


# Fit Bayesian donor-gage model

bayes_sny <- brm(
  log(discharge_sny_m3_s) ~ log(MF_Q_m3_s),
  data = sny,
  family = gaussian()
)

# Model diagnostics
summary(bayes_sny)
plot(bayes_sny)
pp_check(bayes_sny)


# Calculate fitted relationship

sny_fit <- fitted(
  bayes_sny,
  newdata = sny,
  probs = c(0.025, 0.975)
)

# Back-transform fitted values
sny$sny_fit <- exp(sny_fit[, "Estimate"])
sny$sny_fit_lwr <- exp(sny_fit[, "Q2.5"])
sny$sny_fit_upr <- exp(sny_fit[, "Q97.5"])


# Generate posterior predictions

sny_bayes_pred <- predict(
  bayes_sny,
  newdata = preds,
  probs = c(0.025, 0.975)
)

preds$sny_discharge_m3_s <-
  exp(sny_bayes_pred[, "Estimate"])

preds$sny_discharge_m3_s_lwr <-
  exp(sny_bayes_pred[, "Q2.5"])

preds$sny_discharge_m3_s_upr <-
  exp(sny_bayes_pred[, "Q97.5"])


# 6. Sprague Creek model ----

# Dates of field discharge measurements
spr_date <- as.POSIXct(
  c(
    "2022-04-28 19:00",
    "2022-05-10 10:00",
    "2022-05-24 16:00",
    "2022-06-07 14:00",
    "2022-06-29 12:00",
    "2022-07-19 16:00"
  ),
  format = "%Y-%m-%d %H:%M",
  tz = "America/Denver"
)


# Corresponding Sprague Creek discharge measurements (m3/s)

discharge_spr_m3_s <- c(
  0.2906,
  0.4278,
  0.2926,
  0.1413,
  0.5429,
  0.0357
)


# Create Sprague Creek field dataset
spr <- data.frame(
  date = spr_date,
  discharge_spr_m3_s
)

# Convert timestamps to UTC
spr$date <- with_tz(
  spr$date,
  tz = "UTC"
)


# Extract concurrent Middle Fork discharge

filtered_MF <- MF %>%
  filter(dateTime %in% spr$date)

spr <- spr %>%
  mutate(
    MF_Q_m3_s = filtered_MF$MF_Q_m3_s
  )


# Fit Bayesian donor-gage model

bayes_spr <- brm(
  log(discharge_spr_m3_s) ~ log(MF_Q_m3_s),
  data = spr,
  family = gaussian()
)

# Model diagnostics
summary(bayes_spr)
plot(bayes_spr)
pp_check(bayes_spr)


# Calculate fitted relationship

spr_fit <- fitted(
  bayes_spr,
  newdata = spr,
  probs = c(0.025, 0.975)
)

# Back-transform fitted values
spr$spr_fit <- exp(spr_fit[, "Estimate"])
spr$spr_fit_lwr <- exp(spr_fit[, "Q2.5"])
spr$spr_fit_upr <- exp(spr_fit[, "Q97.5"])


# Generate posterior predictions

spr_bayes_pred <- predict(
  bayes_spr,
  newdata = preds,
  probs = c(0.025, 0.975)
)

preds$spr_discharge_m3_s <-
  exp(spr_bayes_pred[, "Estimate"])

preds$spr_discharge_m3_s_lwr <-
  exp(spr_bayes_pred[, "Q2.5"])

preds$spr_discharge_m3_s_upr <-
  exp(spr_bayes_pred[, "Q97.5"])


# 7. Fish Creek model ----

# Dates of field discharge measurements
fish_date <- as.POSIXct(
  c(
    "2022-04-28 19:00",
    "2022-05-10 14:00",
    "2022-05-24 08:00",
    "2022-06-07 16:00",
    "2022-06-29 14:00",
    "2022-07-19 18:00",
    "2022-08-03 16:00",
    "2022-09-06 16:00",
    "2022-09-20 15:00"
  ),
  format = "%Y-%m-%d %H:%M",
  tz = "America/Denver"
)


# Corresponding Fish Creek discharge measurements (m3/s)

discharge_fish_m3_s <- c(
  2.5184,
  2.5273,
  1.5801,
  1.5958,
  1.3031,
  0.3028,
  0.1990,
  0.1123,
  0.0658
)


# Create Fish Creek field dataset
fish <- data.frame(
  date = fish_date,
  discharge_fish_m3_s
)

# Convert timestamps to UTC
fish$date <- with_tz(
  fish$date,
  tz = "UTC"
)


# Extract concurrent Middle Fork discharge

filtered_MF <- MF %>%
  filter(dateTime %in% fish$date)

fish <- fish %>%
  mutate(
    MF_Q_m3_s = filtered_MF$MF_Q_m3_s
  )


# Fit Bayesian donor-gage model

bayes_fish <- brm(
  log(discharge_fish_m3_s) ~ log(MF_Q_m3_s),
  data = fish,
  family = gaussian()
)

# Model diagnostics
summary(bayes_fish)
plot(bayes_fish)
pp_check(
  bayes_fish,
  ndraws = 100
)


# Calculate fitted relationship

fish_fit <- fitted(
  bayes_fish,
  newdata = fish,
  probs = c(0.025, 0.975)
)

# Back-transform fitted values
fish$fish_fit <- exp(fish_fit[, "Estimate"])
fish$fish_fit_lwr <- exp(fish_fit[, "Q2.5"])
fish$fish_fit_upr <- exp(fish_fit[, "Q97.5"])


# Generate posterior predictions

fish_bayes_pred <- predict(
  bayes_fish,
  newdata = preds,
  probs = c(0.025, 0.975)
)

preds$fish_discharge_m3_s <-
  exp(fish_bayes_pred[, "Estimate"])

preds$fish_discharge_m3_s_lwr <-
  exp(fish_bayes_pred[, "Q2.5"])

preds$fish_discharge_m3_s_upr <-
  exp(fish_bayes_pred[, "Q97.5"])


# 8. Prepare final reconstructed discharge dataset ----

# Restrict reconstruction to water years 2008–2023.

preds <- preds %>%
  filter(
    dateTime <= as.POSIXct(
      "2023-09-30 23:00:00",
      tz = "UTC"
    )
  )

# Apply upper limit to Sprague Creek prediction interval.
# The upper prediction bound for Sprague Creek is constrained
# using the maximum Snyder Creek upper prediction bound.

preds <- preds %>%
  mutate(
    spr_discharge_m3_s_upr = pmin(
      spr_discharge_m3_s_upr,
      max(
        sny_discharge_m3_s_upr,
        na.rm = TRUE
      )
    )
  )

# 9. Save reconstructed discharge ----

write.csv(
  preds,
  here(
    "2_incremental",
    "QtQo.csv"
  ),
  row.names = FALSE
)

# 10. Plot donor-gage relationships ----

# Upper McDonald Creek

ggplot(
  umc,
  aes(
    x = MF_Q_m3_s,
    y = discharge_m3_s
  )
) +
  geom_ribbon(
    aes(
      ymin = umc_fit_lwr,
      ymax = umc_fit_upr
    ),
    fill = "cadetblue",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = umc_fit),
    color = "cadetblue",
    linewidth = 1
  ) +
  geom_point(
    color = "black",
    size = 0.5
  ) +
  labs(
    x = expression(
      "Middle Fork Flathead River (" *
        m^3 * " s"^-1 * ")"
    ),
    y = expression(
      "Upper McDonald Creek (" *
        m^3 * " s"^-1 * ")"
    ),
    title = "A"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 12),
    legend.position = "none"
  )


# Lower McDonald Creek

ggplot(
  lmc,
  aes(
    x = MF_Q_m3_s,
    y = discharge_m3_s
  )
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
    x = expression(
      "Middle Fork Flathead River (" *
        m^3 * " s"^-1 * ")"
    ),
    y = expression(
      "Lower McDonald Creek (" *
        m^3 * " s"^-1 * ")"
    ),
    title = "B"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 12),
    legend.position = "none"
  )


# Snyder Creek

ggplot(
  sny,
  aes(
    x = MF_Q_m3_s,
    y = discharge_sny_m3_s
  )
) +
  geom_ribbon(
    aes(
      ymin = sny_fit_lwr,
      ymax = sny_fit_upr
    ),
    fill = "cadetblue",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = sny_fit),
    color = "cadetblue",
    linewidth = 1
  ) +
  geom_point(
    color = "black",
    size = 0.5
  ) +
  labs(
    x = expression(
      "Middle Fork Flathead River (" *
        m^3 * " s"^-1 * ")"
    ),
    y = expression(
      "Snyder Creek (" *
        m^3 * " s"^-1 * ")"
    ),
    title = "C"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 12),
    legend.position = "none"
  )


# Sprague Creek

ggplot(
  spr,
  aes(
    x = MF_Q_m3_s,
    y = discharge_spr_m3_s
  )
) +
  geom_ribbon(
    aes(
      ymin = spr_fit_lwr,
      ymax = spr_fit_upr
    ),
    fill = "cadetblue",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = spr_fit),
    color = "cadetblue",
    linewidth = 1
  ) +
  geom_point(
    color = "black",
    size = 0.5
  ) +
  labs(
    x = expression(
      "Middle Fork Flathead River (" *
        m^3 * " s"^-1 * ")"
    ),
    y = expression(
      "Sprague Creek (" *
        m^3 * " s"^-1 * ")"
    ),
    title = "D"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 12),
    legend.position = "none"
  )


# Fish Creek

ggplot(
  fish,
  aes(
    x = MF_Q_m3_s,
    y = discharge_fish_m3_s
  )
) +
  geom_ribbon(
    aes(
      ymin = fish_fit_lwr,
      ymax = fish_fit_upr
    ),
    fill = "cadetblue",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = fish_fit),
    color = "cadetblue",
    linewidth = 1
  ) +
  geom_point(
    color = "black",
    size = 0.5
  ) +
  labs(
    x = expression(
      "Middle Fork Flathead River (" *
        m^3 * " s"^-1 * ")"
    ),
    y = expression(
      "Fish Creek (" *
        m^3 * " s"^-1 * ")"
    ),
    title = "E"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 12),
    legend.position = "none"
  )



