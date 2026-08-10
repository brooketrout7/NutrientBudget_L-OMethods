

# 0. Load packages----
#install.packages(c("tibble", "future.apply", "data.table"))
#install.packages("ggbreak")

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(patchwork)
library(purrr)
library(viridis)
library(tidyr)
library(truncdist)
library(tibble)
library(future)
library(future.apply)
library(data.table)
library(stringr)
library(ggbreak) 
library(matrixStats)
library(xtable)
library(grid)


# Disable scientific notation
options(scipen = 999)


# 1. Read in data----

# Nutrient budget----
nuts <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\NutrientBudget.csv", header = TRUE, sep=",")

# Convert to date 
nuts$start_date <- as.Date(nuts$start_date, format = "%Y-%m-%d")
nuts$end_date <- as.Date(nuts$end_date, format = "%Y-%m-%d")

# Stream chemistry----
chemdata_2022 <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\chemdata_2022.csv', header = TRUE, sep = ",")

chemdata_2022$date <- as.POSIXct(chemdata_2022$date, format = "%Y-%m-%d")

chemdata_2023 <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\chemdata_2023.csv', header = TRUE, sep = ",")

chemdata_2023$date <- as.POSIXct(chemdata_2023$date, format = "%Y-%m-%d")

chemdata_2023 <- chemdata_2023 %>%
  mutate(date = update(date, year = 2023))

# Group by 'date' and 'site' and calculate the mean for 'tn' and 'tp'
chemdata <- rbind(chemdata_2022, chemdata_2023)

mean_values <- chemdata %>%
  group_by(date, site) %>%
  filter(site %in% c("UMC", "Snyder", "Sprague", "Fish")) %>%
  summarise(
    mean_tn = mean(tn, na.rm = TRUE),
    mean_tp = mean(tp, na.rm = TRUE)
  )

# Ensure columns are numeric
mean_values$mean_tn <- as.numeric(mean_values$mean_tn)
mean_values$mean_tp <- as.numeric(mean_values$mean_tp)

# Replace NaNs for P with detection level = 1.5 ppb
mean_values <- mean_values %>%
  mutate(
    mean_tn = ifelse(is.nan(mean_tn), NA, mean_tn),
    mean_tp = ifelse(is.nan(mean_tp), 1.5, mean_tp)
  )

# Calculate means
UMC_mean_tp <- mean(subset(mean_values, site == "UMC")$mean_tp)
UMC_mean_tn <- mean(subset(mean_values, site == "UMC")$mean_tn)
SNY_mean_tp <- mean(subset(mean_values, site == "Snyder")$mean_tp)
SNY_mean_tn <- mean(subset(mean_values, site == "Snyder")$mean_tn)
SPR_mean_tp <- mean(subset(mean_values, site == "Sprague")$mean_tp)
SPR_mean_tn <- mean(subset(mean_values, site == "Sprague")$mean_tn)
FISH_mean_tp <- mean(subset(mean_values, site == "Fish")$mean_tp)
FISH_mean_tn <- mean(subset(mean_values, site == "Fish")$mean_tn)

# Dry deposition----
dry <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\dry_for_fire_sim.csv', header = TRUE, sep = ",")

dry$Date <- as.Date(dry$Date, format = "%Y-%m-%d")

# Separate for TP and TN
D_mean_P <- mean(subset(dry, Parameter == "TP")$Value, na.rm = TRUE)
D_mean_N <- mean(subset(dry, Parameter == "TN")$Value, na.rm = TRUE)

# Boulder fire
dry_dep_boulder <- dry %>%
  filter(Date >= as.Date("2021-07-31") & Date <= as.Date("2021-08-20"))

# Elmo and Garceau fire
dry_dep_2022 <- dry %>%
  filter(Date >= as.Date("2022-07-30") & Date <= as.Date("2022-09-01"))

# Niarada fire
dry_dep_2023 <- dry %>%
  filter(Date >= as.Date("2023-07-30") & Date <= as.Date("2023-08-02"))

fires_dry <- bind_rows(dry_dep_boulder, dry_dep_2022, dry_dep_2023)

P_min <- (min(subset(fires_dry, Parameter == "TP")$Value))/D_mean_P
P_mean <- (mean(subset(fires_dry, Parameter == "TP")$Value))/D_mean_P
P_max <- (max(subset(fires_dry, Parameter == "TP")$Value))/D_mean_P

N_min <- (min(subset(fires_dry, Parameter == "TN")$Value))/D_mean_N
N_mean <- (mean(subset(fires_dry, Parameter == "TN")$Value))/D_mean_N
N_max <- (max(subset(fires_dry, Parameter == "TN")$Value))/D_mean_N

# Discharge----
tribs_Q <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\tribs_out_WY_2008_2023.csv', header = TRUE, sep = ",")

# Convert 'dateTime' in tribs_Q to POSIXct
tribs_Q <- tribs_Q %>%
  mutate(dateTime = ymd_hms(dateTime))

tribs_Q <- tribs_Q %>%
  select(dateTime, umc_discharge_m3_s, umc_discharge_m3_s_lwr, umc_discharge_m3_s_upr, sny_discharge_m3_s, sny_discharge_m3_s_lwr, sny_discharge_m3_s_upr, spr_discharge_m3_s, spr_discharge_m3_s_lwr, spr_discharge_m3_s_upr, fish_discharge_m3_s, fish_discharge_m3_s_lwr, fish_discharge_m3_s_upr) %>%
  filter(
    dateTime >= as.POSIXct("2018-04-10 00:00:00") &
      dateTime <= as.POSIXct("2018-07-10 23:59:59")
  )


#1. Possible distribution of values for simulations----

# Set seed for reproducibility
set.seed(123)

# Simulate 10000 draws from each truncated gamma distribution
UMC_fire_P <- rtrunc(10000, spec = "gamma", a = UMC_mean_tp*5, b = UMC_mean_tp*10000, shape = 2, scale = (40*UMC_mean_tp)/2)

UMC_fire_N <- rtrunc(10000, spec = "gamma", a = UMC_mean_tn, b = UMC_mean_tn*10000, shape = 2, scale = (4*UMC_mean_tn)/2)

SNY_fire_P <- rtrunc(10000, spec = "gamma", a = SNY_mean_tp*5, b = SNY_mean_tp*10000, shape = 2, scale = (40*SNY_mean_tp)/2)

SNY_fire_N <- rtrunc(10000, spec = "gamma", a = SNY_mean_tn, b = SNY_mean_tn*10000, shape = 2, scale = (4*SNY_mean_tn)/2)

SPR_fire_P <- rtrunc(10000, spec = "gamma", a = SPR_mean_tp*5, b = SPR_mean_tp*10000, shape = 2, scale = (40*SPR_mean_tp)/2)

SPR_fire_N <- rtrunc(10000, spec = "gamma", a = SPR_mean_tn, b = SPR_mean_tn*10000, shape = 2, scale = (4*SPR_mean_tn)/2)

FISH_fire_P <- rtrunc(10000, spec = "gamma", a = FISH_mean_tp*5, b = FISH_mean_tp*10000, shape = 2, scale = (40*FISH_mean_tp)/2)

FISH_fire_N <- rtrunc(10000, spec = "gamma", a = FISH_mean_tn, b = FISH_mean_tn*10000, shape = 2, scale = (4*FISH_mean_tn)/2)

D_fire_P  <- rtrunc(10000, spec = "gamma", a = D_mean_P, b = D_mean_P*860, shape = 2, scale = (P_mean*D_mean_P)/2)

D_fire_N  <- rtrunc(10000, spec = "gamma", a = D_mean_N, b = D_mean_N*860, shape = 2, scale = (N_mean*D_mean_N)/2)

UMC_snow_P <- rtrunc(10000, spec = "gamma", a = UMC_mean_tp, b = UMC_mean_tp*10000, shape = 2, scale = (8*UMC_mean_tp)/2)

UMC_snow_N <- rtrunc(10000, spec = "gamma", a = UMC_mean_tn, b = UMC_mean_tn*100, shape = 2, scale = (6*UMC_mean_tn)/2)

SNY_snow_P <- rtrunc(10000, spec = "gamma", a = SNY_mean_tp, b = SNY_mean_tp*10000, shape = 2, scale = (8*SNY_mean_tp)/2)

SNY_snow_N <- rtrunc(10000, spec = "gamma", a = SNY_mean_tn, b = SNY_mean_tn*100, shape = 2, scale = (6*SNY_mean_tn)/2)

SPR_snow_P <- rtrunc(10000, spec = "gamma", a = SPR_mean_tp, b = SPR_mean_tp*10000, shape = 2, scale = (8*SPR_mean_tp)/2)

SPR_snow_N <- rtrunc(10000, spec = "gamma", a = SPR_mean_tn, b = SPR_mean_tn*100, shape = 2, scale = (6*SPR_mean_tn)/2)


# Number of simulations
n_sims <- 10000
n_weeks <- nrow(tribs_Q)

# Draw 10,000 random scalars
Q_scalar <- rtrunc(n_sims, spec = "gamma", a = 1, b = 100, shape = 2, scale = 1)

# Initialize arrays for results [weeks × sims]
UMC_Q_snow <- matrix(NA, nrow = n_weeks, ncol = n_sims)
SNY_Q_snow <- matrix(NA, nrow = n_weeks, ncol = n_sims)
SPR_Q_snow <- matrix(NA, nrow = n_weeks, ncol = n_sims)

# Loop over simulations
for (i in seq_len(n_sims)) {
  UMC_Q_snow[, i] <- tribs_Q$umc_discharge_m3_s * Q_scalar[i]
  SNY_Q_snow[, i] <- tribs_Q$sny_discharge_m3_s * Q_scalar[i]
  SPR_Q_snow[, i] <- tribs_Q$spr_discharge_m3_s * Q_scalar[i]
}

UMC_df <- cbind(dateTime = tribs_Q$dateTime, as.data.frame(UMC_Q_snow))
SNY_df <- cbind(dateTime = tribs_Q$dateTime, as.data.frame(SNY_Q_snow))
SPR_df <- cbind(dateTime = tribs_Q$dateTime, as.data.frame(SPR_Q_snow))

Q_snow_umc <- list(UMC = UMC_df)
Q_snow_umc <- as.data.frame(Q_snow_umc)

Q_snow_sny <- list(SNY = SNY_df)
Q_snow_sny <- as.data.frame(Q_snow_sny)

Q_snow_spr <- list(SPR = SPR_df)
Q_snow_spr <- as.data.frame(Q_snow_spr)

Q_snow_umc_stats <- Q_snow_umc %>%
  mutate(
    Q_min  = rowMins(as.matrix(.[, 2:10001]), na.rm = TRUE),
    Q_mean = rowMeans(as.matrix(.[, 2:10001]), na.rm = TRUE),
    Q_max  = rowMaxs(as.matrix(.[, 2:10001]), na.rm = TRUE)
  ) %>%
  select(UMC.dateTime, Q_min, Q_mean, Q_max) %>%
  rename(dateTime = "UMC.dateTime") %>%
  mutate(site = "Upper McDonald Creek")


Q_snow_sny_stats <- Q_snow_sny %>%
  mutate(
    Q_min  = rowMins(as.matrix(.[, 2:10001]), na.rm = TRUE),
    Q_mean = rowMeans(as.matrix(.[, 2:10001]), na.rm = TRUE),
    Q_max  = rowMaxs(as.matrix(.[, 2:10001]), na.rm = TRUE)
  ) %>%
  select(SNY.dateTime, Q_min, Q_mean, Q_max) %>%
  rename(dateTime = "SNY.dateTime") %>%
  mutate(site = "Snyder Creek")

Q_snow_spr_stats <- Q_snow_spr %>%
  mutate(
    Q_min  = rowMins(as.matrix(.[, 2:10001]), na.rm = TRUE),
    Q_mean = rowMeans(as.matrix(.[, 2:10001]), na.rm = TRUE),
    Q_max  = rowMaxs(as.matrix(.[, 2:10001]), na.rm = TRUE)
  ) %>%
  select(SPR.dateTime, Q_min, Q_mean, Q_max) %>%
  rename(dateTime = "SPR.dateTime") %>%
  mutate(site = "Sprague Creek")

# Construct dataframe using tidyverse tibble

param_df <- bind_rows(
  tibble(value = UMC_snow_P, site = "UMC", period = "Snowmelt", parameter = "P"),
  tibble(value = UMC_snow_N/10, site = "UMC", period = "Snowmelt", parameter = "N"),
  tibble(value = UMC_fire_P, site = "UMC", period = "Fire", parameter = "P"),
  tibble(value = UMC_fire_N, site = "UMC", period = "Fire", parameter = "N"),
  
  tibble(value = SNY_snow_P, site = "SNY", period = "Snowmelt", parameter = "P"),
  tibble(value = SNY_snow_N/10, site = "SNY", period = "Snowmelt", parameter = "N"),
  tibble(value = SNY_fire_P, site = "SNY", period = "Fire", parameter = "P"),
  tibble(value = SNY_fire_N, site = "SNY", period = "Fire", parameter = "N"),
  
  tibble(value = SPR_snow_P, site = "SPR", period = "Snowmelt", parameter = "P"),
  tibble(value = SPR_snow_N/10, site = "SPR", period = "Snowmelt", parameter = "N"),
  tibble(value = SPR_fire_P, site = "SPR", period = "Fire", parameter = "P"),
  tibble(value = SPR_fire_N, site = "SPR", period = "Fire", parameter = "N"),
  
  tibble(value = FISH_fire_P, site = "FISH", period = "Fire", parameter = "P"),
  tibble(value = FISH_fire_N, site = "FISH", period = "Fire", parameter = "N"),
  
  tibble(value = D_fire_P, site = "Dry Deposition", period = "Fire", parameter = "P"),
  tibble(value = D_fire_N, site = "Dry Deposition", period = "Fire", parameter = "N")
)

# Plot distributions

# Subset during fire and during Snowmelt
param_fire <- param_df %>% filter(period == "Fire")
param_snow <- param_df %>% filter(period == "Snowmelt")


# Set the ordering of the plot
param_fire <- param_fire %>%
  mutate(site = factor(site,
                       levels = c("UMC", "SPR", "SNY", "FISH", "Dry Deposition"),
                       labels = c("Upper McDonald Creek", "Sprague Creek", "Snyder Creek", "Fish Creek", "Dry Deposition")), 
         parameter = fct_recode(parameter,
                                "N (/10)" = "N",
                                "P" = "P")  # this is optional if P is already labeled correctly
  )


param_snow <- param_snow %>%
  mutate(
    site = factor(site,
                  levels = c("UMC", "SPR", "SNY"),
                  labels = c("Upper McDonald Creek", "Sprague Creek", "Snyder Creek")),
    parameter = fct_recode(parameter,
                           "N (/10)" = "N",
                           "P" = "P")  # this is optional if P is already labeled correctly
  ) 

# Plot Fire figure
Duringfire <- ggplot(param_fire, aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~site, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("P" = "salmon1", "N (/10)" = "mediumpurple4")) +
  theme_classic() +
  labs(
    title = "During Wildfire",
    x = bquote("Peak Nutrient Concentration ("*mu*"g L"^{-1}*")"),
    y = "Density",
    fill = "Nutrient"
  ) +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(size = 6),
    legend.position = "bottom",
    legend.box = "horizontal") +   scale_x_continuous(labels = scales::comma)

Duringfire

UMC_fire_P <- as.data.frame(UMC_fire_P)

umc_fire <- ggplot(UMC_fire_P, aes(x = UMC_fire_P)) +
  geom_density(fill = "salmon1", alpha = 0.7) +
  theme_classic() +
  labs(
    title = "Upper McDonald Creek",
    x = expression("TP concentration ("*mu*"g L"^{-1}*")"),
    y = "Density"
  ) +
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(size = 6),
    legend.position = "none"   # remove legend since it's not needed
  ) +
  scale_x_continuous(labels = scales::comma)

umc_fire

dry_fire_P <- as.data.frame(D_fire_P)


dry_fire <- ggplot(dry_fire_P, aes(x = D_fire_P)) +
  geom_density(fill = "salmon1", alpha = 0.7) +
  theme_classic() +
  labs(
    title = "Dry Deposition",
    x = expression("TP concentration ("*mu*"g L"^{-1}*")"),
    y = "Density"
  ) +
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(size = 6),
    legend.position = "none"   # remove legend since it's not needed
  ) +
  scale_x_continuous(labels = scales::comma)

dry_fire

ggsave(filename = "Duringfire.png",   # file name (can be .png, .pdf, .jpg, .tiff, .svg)
       plot = Duringfire,             # the ggplot object
       width = 5,                        # width in chosen units
       height = 4,                       # height in chosen units
       units = "in",                     # "in", "cm", or "mm"
       dpi = 300                         # resolution (good for publications)
)

# Nutrients plot
nutrient_plot <- ggplot(param_snow, aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~site, scales = "free", ncol = 3)  +
  theme(aspect.ratio = 1) +  # square facets
  theme(aspect.ratio = 1) +  # square facets
  scale_fill_manual(values = c("P" = "salmon1", "N (/10)" = "mediumpurple4")) + 
  theme_classic() +  labs(
    title = "During Snowmelt",
    x = bquote("Peak Nutrient Concentration ("*mu*"g L"^{-1}*")"),
    y = "Density",
    fill = "Nutrient"
  )  +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(size = 6),
    legend.position = "none",
    legend.box = "horizontal"
  ) +   scale_x_continuous(labels = scales::comma)

nutrient_plot

# Discharge plot
discharge_umc_plot <- ggplot(Q_snow_umc_stats, aes(x = dateTime)) +
  geom_ribbon(aes(ymin=Q_min, ymax =Q_max), fill = "cadetblue") +
  facet_wrap(~site, scales = "free", ncol = 3) +
  theme(aspect.ratio = 1) + # square facets
  theme_classic() +
  labs(
    title = "",
    x = "",
    y = expression("Discharge (m"^3~s^-1*")")
  ) +
  scale_x_datetime(date_breaks = "2 week", date_labels = "%b %d") +  
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
    strip.text = element_text(size = 6),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

discharge_sny_plot <- ggplot(Q_snow_sny_stats, aes(x = dateTime)) +
  geom_ribbon(aes(ymin=Q_min, ymax =Q_max), fill = "cadetblue") +
  facet_wrap(~site, scales = "free", ncol = 3) +
  theme(aspect.ratio = 1) +  # square facets
  theme_classic() +
  labs(
    title = "",
    x = "",
    y = expression("Discharge (m"^3~s^-1*")")
  ) +
  scale_x_datetime(date_breaks = "2 week", date_labels = "%b %d") +  
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 6),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

discharge_spr_plot <- ggplot(Q_snow_spr_stats, aes(x = dateTime)) +
  geom_ribbon(aes(ymin=Q_min, ymax =Q_max), fill = "cadetblue") +
  facet_wrap(~site, scales = "free", ncol = 3) +
  theme(aspect.ratio = 1) +  # square facets
  theme_classic() +
  labs(
    title = "",
    x = "",
    y = expression("Discharge (m"^3~s^-1*")")
  ) +
  scale_x_datetime(date_breaks = "2 week", date_labels = "%b %d") +  
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 6),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "bottom",
    legend.box = "horizontal"
  )


combine <- (discharge_umc_plot | discharge_spr_plot | discharge_sny_plot)

combine <- nutrient_plot/combine

ggsave(filename = "Dischargesnow.png",   # file name (can be .png, .pdf, .jpg, .tiff, .svg)
       plot = combine,             # the ggplot object
       width = 5,                        # width in chosen units
       height = 5,                       # height in chosen units
       units = "in",                     # "in", "cm", or "mm"
       dpi = 300                         # resolution (good for publications)
)



rm(list = setdiff(ls(), c("nuts", "tribs_Q", "UMC_mean_tp", "UMC_mean_tn", 
                          "SNY_mean_tp", "SNY_mean_tn",
                          "SPR_mean_tp", "SPR_mean_tn", 
                          "FISH_mean_tp", "FISH_mean_tn", 
                          "P_mean", "N_mean")))



# 2. Simulate Wildfire----
####Phosphorus Simulation
# Dataframes----

dates <- nuts %>%
  filter(end_date >= as.Date("2017-08-22") & end_date <= as.Date("2018-09-25")) %>%
  select(start_date, end_date, 
         umc_P, umc_P_lwr, umc_P_upr, umc_N, umc_N_lwr, umc_N_upr, umc_m3, umc_m3_lwr, umc_m3_upr,
         sny_P, sny_P_lwr, sny_P_upr, sny_N, sny_N_lwr, sny_N_upr, sny_m3, sny_m3_lwr, sny_m3_upr,
         spr_P, spr_P_lwr, spr_P_upr, spr_N, spr_N_lwr, spr_N_upr, spr_m3, spr_m3_lwr, spr_m3_upr,     
         fish_P, fish_P_lwr, fish_P_upr, fish_N, fish_N_lwr, fish_N_upr, fish_m3, fish_m3_lwr, fish_m3_upr,  
         TN_wet_kg, 
         TP_dry_kg, TN_dry_kg, 
         H_P, H_N, S_P, S_N, 
         lmc_m3, lmc_m3_lwr, lmc_m3_upr,
         kg_P_est, kg_P_est_lwr, kg_P_est_upr, 
         kg_N_est, kg_N_est_lwr, kg_N_est_upr, 
         lake_volume) %>%
  mutate(umc_P_conc = (umc_P*10^9)/(umc_m3*1000), 
         umc_P_conc_lwr = (umc_P_lwr*10^9)/(umc_m3_lwr*1000),
         umc_P_conc_upr = (umc_P_upr*10^9)/(umc_m3_upr*1000),
         sny_P_conc = (sny_P*10^9)/(sny_m3*1000), 
         sny_P_conc_lwr = (sny_P_lwr*10^9)/(sny_m3_lwr*1000),
         sny_P_conc_upr = (sny_P_upr*10^9)/(sny_m3_upr*1000),
         spr_P_conc = (spr_P*10^9)/(spr_m3*1000), 
         spr_P_conc_lwr = (spr_P_lwr*10^9)/(spr_m3_lwr*1000),
         spr_P_conc_upr = (spr_P_upr*10^9)/(spr_m3_upr*1000),
         fish_P_conc = (fish_P*10^9)/(fish_m3*1000), 
         fish_P_conc_lwr = (fish_P_lwr*10^9)/(fish_m3_lwr*1000),
         fish_P_conc_upr = (fish_P_upr*10^9)/(fish_m3_upr*1000),
         umc_N_conc = (umc_N*10^9)/(umc_m3*1000), 
         umc_N_conc_lwr = (umc_N_lwr*10^9)/(umc_m3_lwr*1000),
         umc_N_conc_upr = (umc_N_upr*10^9)/(umc_m3_upr*1000),
         sny_N_conc = (sny_N*10^9)/(sny_m3*1000), 
         sny_N_conc_lwr = (sny_N_lwr*10^9)/(sny_m3_lwr*1000),
         sny_N_conc_upr = (sny_N_upr*10^9)/(sny_m3_upr*1000),
         spr_N_conc = (spr_N*10^9)/(spr_m3*1000), 
         spr_N_conc_lwr = (spr_N_lwr*10^9)/(spr_m3_lwr*1000),
         spr_N_conc_upr = (spr_N_upr*10^9)/(spr_m3_upr*1000),
         fish_N_conc = (fish_N*10^9)/(fish_m3*1000), 
         fish_N_conc_lwr = (fish_N_lwr*10^9)/(fish_m3_lwr*1000),
         fish_N_conc_upr = (fish_N_upr*10^9)/(fish_m3_upr*1000))

sprague <- dates %>%
  filter(end_date >= as.Date("2017-08-22") & end_date <= as.Date("2017-10-31"))

null_winter <-  dates %>% 
  filter(end_date >= as.Date("2017-11-07") & end_date <= as.Date("2018-04-10"))  
  
snow <-  dates %>% 
  filter(end_date >= as.Date("2018-04-17") & end_date <= as.Date("2018-07-10"))

null_summer <-  dates %>% 
  filter(end_date >= as.Date("2018-07-17") & end_date <= as.Date("2018-08-14")) 

howe <- dates %>% 
  filter(end_date >= as.Date("2018-08-21") & end_date <= as.Date("2018-09-25"))


sim <- rbind(sprague, null_winter, snow, null_summer, howe)

# Initial values---- 

set.seed(123)

n_iter <- 100000 # Number of iterations
plan(multisession, workers = parallel::detectCores() - 1) # Parallel processing

# Set output
out_dir <- "C:/Users/brook/Documents/PhD/Dissertation/Chp_1/LakeMcDonaldNutrientBudget/2_incremental/" # output directory
all_mass <- vector("list", n_iter)
all_concentrations <- vector("list", n_iter)
all_discharges <- vector("list", n_iter)
all_deposition <- vector("list", n_iter)


# Define the decay function to save memory in the function; final = initial*e^(-k*t) 
decay <- function(init, end, steps) {
  rate <- log(end / init) / (steps - 1)
  init * exp(rate * (0:(steps - 1)))
} 


M0 <- sprague$kg_P_est[1] # initial starting mass

# Deposition
D_mean_P <- mean(nuts$TP_dry_kg, na.rm = TRUE) # Set deposition to load
D_snow <- snow$TP_dry_kg

# Volume of the lake (converted to liters from m3) and discharge from lmc needed to calculate outflow
V_sprague <- sprague$lake_volume * 1000
Q_sprague <- sprague$lmc_m3 * 1000

V_snow <- snow$lake_volume * 1000
Q_snow <- snow$lmc_m3 * 1000

V_howe <- howe$lake_volume * 1000
Q_howe <- howe$lmc_m3 * 1000


# Discharge to calculate loading from the tributaries given the newly simulated concentrations
UMC_Q_sprague <- sprague$umc_m3 * 1000
SNY_Q_sprague <- sprague$sny_m3 * 1000
SPR_Q_sprague <- sprague$spr_m3 * 1000
FISH_Q_sprague <- sprague$fish_m3 * 1000


# define general shape of the hydrograph for each trib
get_discharge_means <- function(start, end, df) {
  df %>%
    filter(dateTime >= start & dateTime < end) %>%
    summarise(
      umc_discharge_mean = mean(umc_discharge_m3_s, na.rm = TRUE),
      sny_discharge_mean = mean(sny_discharge_m3_s, na.rm = TRUE),
      spr_discharge_mean = mean(spr_discharge_m3_s, na.rm = TRUE)
    )
}

# Apply the function across each row in `snow` to get discharge means
discharge_means <- map2_dfr(snow$start_date, snow$end_date,
                            ~ get_discharge_means(.x, .y, tribs_Q))

# normalize by max so just capturing the shape 

UMC_Q_hydro <- discharge_means$umc_discharge_mean/max(discharge_means$umc_discharge_mean)
SNY_Q_hydro <- discharge_means$sny_discharge_mean/max(discharge_means$sny_discharge_mean)
SPR_Q_hydro <- discharge_means$spr_discharge_mean/max(discharge_means$spr_discharge_mean)

UMC_Q_howe <- howe$umc_m3 * 1000
SNY_Q_howe <- howe$sny_m3 * 1000
SPR_Q_howe <- howe$spr_m3 * 1000
FISH_Q_howe <- howe$fish_m3 * 1000

#Constants
H_sprague <- sprague$H_P
S_sprague <- sprague$S_P

H_snow <- snow$H_P
S_snow <- snow$S_P

H_howe <- howe$H_P
S_howe <- howe$S_P

FISH_snow <- snow$fish_P

t_snow <- nrow(snow) # number of timesteps


all_outputs <- future_lapply(1:n_iter, function(i) {
  set.seed(123 + i)
  
  
  #### PHASE 1: Sprague decay----
  
  # Repeats picking from a gamma distribution until the criteria below is met
  repeat {
    UMC1_sprague <- rtrunc(1, spec = "gamma", a = UMC_mean_tp*5, b = UMC_mean_tp*10000, shape = 2, scale = (40*UMC_mean_tp)/2)
    SNY1_sprague <- rtrunc(1, spec = "gamma", a = SNY_mean_tp*5, b = SNY_mean_tp*10000, shape = 2, scale = (40*SNY_mean_tp)/2)
    SPR1_sprague <- rtrunc(1, spec = "gamma", a = SPR_mean_tp*5, b = SPR_mean_tp*10000, shape = 2, scale = (40*SPR_mean_tp)/2)
    FISH1_sprague <- rtrunc(1, spec = "gamma", a = FISH_mean_tp*5, b = FISH_mean_tp*10000, shape = 2, scale = (40*FISH_mean_tp)/2)
    
    # specifies that SNY and SPR concentrations need to be larger than UMC and FISH because of proximity to the fire
    if (SNY1_sprague > UMC1_sprague && SNY1_sprague > FISH1_sprague && SPR1_sprague > UMC1_sprague && SPR1_sprague > FISH1_sprague) break
  }
  
  # Picks from a gamma distribution to modify dry deposition
  D1_sprague <- rtrunc(1, spec = "gamma", a = D_mean_P, b = D_mean_P*860, shape = 2, scale = (P_mean*D_mean_P)/2)
  
  # Runs the decay for the Sprague fire over the number of time steps (defined by number of rows) in Sprague dataframe
  timesteps <- nrow(sprague)
  UMC_sprague  <- decay(UMC1_sprague, UMC_end_sprague <- sprague$umc_P_conc[timesteps], timesteps)
  SNY_sprague  <- decay(SNY1_sprague, SNY_end_sprague <- sprague$sny_P_conc[timesteps], timesteps)
  SPR_sprague  <- decay(SPR1_sprague, SPR_end_sprague <- sprague$spr_P_conc[timesteps], timesteps)
  FISH_sprague <- decay(FISH1_sprague, FISH_end_sprague <- sprague$fish_P_conc[timesteps], timesteps)
  D_sprague    <- decay(D1_sprague,  D_end_sprague <- sprague$TP_dry_kg[timesteps], timesteps)
  
  # Number of masses to predict +1 because of the initial value from Sprague 
  M <- numeric(timesteps + 1)
  M[1] <- M0
  
  # Predict mass except for the first time step (that is M0)
  for (t in 2:(timesteps + 1)) {
    M[t] <- M[t-1] +
      (UMC_sprague[t-1] * UMC_Q_sprague[t-1] + 
         SNY_sprague[t-1] * SNY_Q_sprague[t-1] + 
         SPR_sprague[t-1] * SPR_Q_sprague[t-1] +
         FISH_sprague[t-1] * FISH_Q_sprague[t-1]) * 1e-9 +
      D_sprague[t-1] + H_sprague[t-1] - S_sprague[t-1] -
      (M[t-1] / V_sprague[t-1]) * Q_sprague[t-1]
  }
  
  M_sprague <- M[-1]
  
  #### PHASE 2: Winter - null model----
  
  M_winter <- numeric(nrow(null_winter) + 1) # number of rows plus pull the initial value from the Sprague simulation
  M_winter[1] <- tail(M, 1) 
  
  for (w in 2:length(M_winter)) {
    M_winter[w] <- M_winter[w - 1] +
      null_winter$umc_P[w - 1] +
      null_winter$sny_P[w - 1] +
      null_winter$spr_P[w - 1] +
      null_winter$fish_P[w - 1] +
      null_winter$TP_dry_kg[w - 1] +
      null_winter$H_P[w - 1] -
      null_winter$S_P[w - 1] - 
      ((M_winter[w-1]/(null_winter$lake_volume[w-1]*1000))*(null_winter$lmc_m3[w-1]*1000))
  }
  
  #### PHASE 3: Snowmelt----
  
  # Simulate concentrations
  UMC_peak <- rtrunc(1, "gamma", a=UMC_mean_tp, b=UMC_mean_tp*10000, shape=2, scale=(8*UMC_mean_tp)/2)
  SNY_peak <- rtrunc(1, "gamma", a=SNY_mean_tp, b=SNY_mean_tp*10000, shape=2, scale=(8*SNY_mean_tp)/2)
  SPR_peak <- rtrunc(1, "gamma", a=SPR_mean_tp, b=SPR_mean_tp*10000, shape=2, scale=(8*SPR_mean_tp)/2)
  
  # Create the concentration time series given the hydrograph shape
  UMC_snow <- UMC_Q_hydro * UMC_peak
  SNY_snow <- SNY_Q_hydro * SNY_peak
  SPR_snow <- SPR_Q_hydro * SPR_peak
  
  # Simulate discharge 
  Q_scalar <- rtrunc(1, "gamma", a=1, b=100, shape=2, scale=1)
  UMC_Q_snow <- snow$umc_m3 * 1000 * Q_scalar
  SNY_Q_snow <- snow$sny_m3 * 1000 * Q_scalar
  SPR_Q_snow <- snow$spr_m3 * 1000 * Q_scalar
  
  # Predict mass except for the first time step, which comes from the Sprague simulation
  M_snow <- numeric(t_snow + 1)
  M_snow[1] <- tail(M_winter, 1)
  for (t in 2:(t_snow + 1)) {
    M_snow[t] <- M_snow[t - 1] +
      UMC_snow[t - 1] * UMC_Q_snow[t - 1] * 1e-9 +
      SNY_snow[t - 1] * SNY_Q_snow[t - 1] * 1e-9 +
      SPR_snow[t - 1] * SPR_Q_snow[t - 1] * 1e-9 +
      FISH_snow[t - 1] + D_snow[t - 1] + H_snow[t - 1] - S_snow[t - 1] -
      (M_snow[t - 1] / V_snow[t - 1]) * Q_snow[t - 1]
  }
  
  #### PHASE 4: Summer - null model----
  
  M_summer <- numeric(nrow(null_summer) + 1) #number of rows plus pull the initial value from the snowmelt simulation
  M_summer[1] <- tail(M_snow, 1)
  
  for (s in 2:length(M_summer)) {
    M_summer[s] <- M_summer[s - 1] +
      null_summer$umc_P[s - 1] +
      null_summer$sny_P[s - 1] +
      null_summer$spr_P[s - 1] +
      null_summer$fish_P[s - 1] +
      null_summer$TP_dry_kg[s - 1] +
      null_summer$H_P[s - 1] -
      null_summer$S_P[s - 1] -
      ((M_summer[s - 1]/(null_summer$lake_volume[s-1]*1000)) * (null_summer$lmc_m3[s-1]*1000))
    
  }
  
  #### PHASE 5: Howe decay; same process as Sprague----
  
  n_howe <- nrow(howe)
  UMC1_howe <- rtrunc(1, spec = "gamma", a = UMC_mean_tp*5, b = UMC_mean_tp*10000, shape = 2, scale = (40*UMC_mean_tp)/2)
  SNY1_howe <- rtrunc(1, spec = "gamma", a = SNY_mean_tp*5, b = SNY_mean_tp*10000, shape = 2, scale = (40*SNY_mean_tp)/2)
  SPR1_howe <- rtrunc(1, spec = "gamma", a = SPR_mean_tp*5, b = SPR_mean_tp*10000, shape = 2, scale = (40*SPR_mean_tp)/2)
  FISH1_howe <- rtrunc(1, spec = "gamma", a = FISH_mean_tp*5, b = FISH_mean_tp*10000, shape = 2, scale = (40*FISH_mean_tp)/2)
  D1_howe <- rtrunc(1, spec = "gamma", a = D_mean_P, b = D_mean_P*860, shape = 2, scale = (P_mean*D_mean_P)/2)
  
  # Run the decay
  UMC_howe <- decay(UMC1_howe, howe$umc_P_conc[n_howe], n_howe)
  SNY_howe <- decay(SNY1_howe, howe$sny_P_conc[n_howe], n_howe)
  SPR_howe <- decay(SPR1_howe, howe$spr_P_conc[n_howe], n_howe)
  FISH_howe <- decay(FISH1_howe, howe$fish_P_conc[n_howe], n_howe)
  D_howe <- decay(D1_howe, howe$TP_dry_kg[n_howe], n_howe)
  
  
  M_howe <- numeric(n_howe + 1) #number of rows plus pull the initial value from the snowmelt simulation
  M_howe[1] <- tail(M_summer, 1)
  
  for (t in 2:(n_howe + 1)) {
    M_howe[t] <- M_howe[t - 1] +
      (UMC_howe[t - 1] * UMC_Q_howe[t - 1] +
         SNY_howe[t - 1] * SNY_Q_howe[t - 1] +
         SPR_howe[t - 1] * SPR_Q_howe[t - 1] +
         FISH_howe[t - 1] * FISH_Q_howe[t - 1]) * 1e-9 +
      D_howe[t - 1] + H_howe[t - 1] - S_howe[t - 1] -
      (M_howe[t - 1] / V_howe[t - 1]) * Q_howe[t - 1]
  }
  
  # Save iterations
  # Then collect outputs into one list to return
  full_mass <- c(M_sprague, M_winter[-1], M_snow[-1], M_summer[-1], M_howe[-1])
  full_dates <- c(sprague$end_date, null_winter$end_date, snow$end_date, null_summer$end_date, howe$end_date)
  
  mass_out <- data.table(
    iteration = i,
    date = full_dates,
    mass_kg = full_mass
  )
  
  deposition_out <- rbind(
    data.table(iteration = i, date = sprague$end_date, D = D_sprague, phase = "Sprague"),
    data.table(iteration = i, date = howe$end_date, D = D_howe, phase = "Howe")
  )
  
  discharge_out <- data.table(
    iteration = i,
    date = rep(snow$end_date, 3),
    Q = c(UMC_Q_snow, SNY_Q_snow, SPR_Q_snow),
    site = rep(c("UMC", "SNY", "SPR"), each = nrow(snow)),
    phase = "Snowmelt"
  )
  
  conc_out <- data.table(
    iteration = i,
    date = c(rep(sprague$end_date, 4), rep(snow$end_date, 3), rep(howe$end_date, 4)),
    conc = c(UMC_sprague, SNY_sprague, SPR_sprague, FISH_sprague,
             UMC_snow, SNY_snow, SPR_snow,
             UMC_howe, SNY_howe, SPR_howe, FISH_howe),
    site = rep(c("UMC", "SNY", "SPR", "FISH", "UMC", "SNY", "SPR", "UMC", "SNY", "SPR", "FISH"),
               times = c(nrow(sprague), nrow(sprague), nrow(sprague), nrow(sprague),
                         nrow(snow), nrow(snow), nrow(snow),
                         nrow(howe), nrow(howe), nrow(howe), nrow(howe))),
    phase = c(rep("Sprague", nrow(sprague)*4), rep("Snowmelt", nrow(snow)*3), rep("Howe", nrow(howe)*4))
  )
  
  return(list(
    mass = mass_out,
    concentration = conc_out,
    discharge = discharge_out,
    deposition = deposition_out
  ))
})


# Extract each type of output from the results list
all_mass <- rbindlist(lapply(all_outputs, function(x) x[["mass"]]))
all_concentrations <- rbindlist(lapply(all_outputs, function(x) x[["concentration"]]))
all_discharges <- rbindlist(lapply(all_outputs, function(x) x[["discharge"]]))
all_deposition <- rbindlist(lapply(all_outputs, function(x) x[["deposition"]]))


fwrite(all_mass,           paste0(out_dir, "wildfire_simulation_mass.csv"))
fwrite(all_concentrations, paste0(out_dir, "wildfire_simulation_conc.csv"))
fwrite(all_discharges,     paste0(out_dir, "wildfire_simulation_discharge.csv"))
fwrite(all_deposition,     paste0(out_dir, "wildfire_simulation_deposition.csv"))

# Lake concentration----

mass_P <- read.csv("F:\\B_trout_files\\toobigforgit\\Chp_1\\wildfire_simulation_mass.csv", header = TRUE, sep=",")

mass_P$date <- as.Date(mass_P$date)

vol <- sim %>%
  select(end_date, lake_volume)%>%
  mutate(end_date = as.Date(end_date))

mass_P <- mass_P %>%
  left_join(vol, by = c("date" = "end_date"))%>%
  mutate(TP_conc = (mass_kg*10^9)/(lake_volume*1000))

TP_summary <- mass_P %>%
  group_by(date) %>%
  summarise(
    TP_conc_mean    = mean(TP_conc, na.rm = TRUE),
    TP_conc_min     = min(TP_conc, na.rm = TRUE),
    TP_conc_2.5  = quantile(TP_conc, probs = 0.025, na.rm = TRUE),
    TP_conc_97.5  = quantile(TP_conc, probs = 0.975, na.rm = TRUE),
    TP_conc_max     = max(TP_conc, na.rm = TRUE)
  )

write.csv(TP_summary, "C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\fire_sim_TP_lake_for_plotting.csv" )

P_160 <- subset(mass_P, date == "2018-09-18")
P_160 <- subset(P_160, TP_conc >=160)

rm(list = setdiff(ls(), c("nuts", "tribs_Q", "UMC_mean_tp", "UMC_mean_tn", 
                          "SNY_mean_tp", "SNY_mean_tn",
                          "SPR_mean_tp", "SPR_mean_tn", 
                          "FISH_mean_tp", "FISH_mean_tn", 
                          "P_mean", "N_mean", "dates", "P_sim_plot", "TP_summary")))

####Nitrogen Simulation----
# Dataframes & initial values----

sprague <- dates %>%
  filter(end_date >= as.Date("2017-08-22") & end_date <= as.Date("2017-10-31"))

null_winter <-  dates %>% 
  filter(end_date >= as.Date("2017-11-07") & end_date <= as.Date("2018-04-10"))  

snow <-  dates %>% 
  filter(end_date >= as.Date("2018-04-17") & end_date <= as.Date("2018-07-10"))

null_summer <-  dates %>% 
  filter(end_date >= as.Date("2018-07-17") & end_date <= as.Date("2018-08-14")) 

howe <- dates %>% 
  filter(end_date >= as.Date("2018-08-21") & end_date <= as.Date("2018-09-25"))

sim <- rbind(sprague, null_winter, snow, null_summer, howe)

#Simulation---- 

set.seed(123)

n_iter <- 100000 # Number of iterations
plan(multisession, workers = parallel::detectCores() - 1) # Parallel processing

# Set output
out_dir <- "C:/Users/brook/Documents/PhD/Dissertation/Chp_1/LakeMcDonaldNutrientBudget/2_incremental/" # output directory
all_mass <- vector("list", n_iter)
all_concentrations <- vector("list", n_iter)
all_discharges <- vector("list", n_iter)
all_deposition <- vector("list", n_iter)


# Define the decay function to save memory in the function; final = initial*e^(-k*t) 
decay <- function(init, end, steps) {
  rate <- log(end / init) / (steps - 1)
  init * exp(rate * (0:(steps - 1)))
} 


M0 <- sprague$kg_N_est[1] # initial starting mass

# Deposition
D_mean_N <- mean(nuts$TN_dry_kg, na.rm= TRUE) # Set deposition to load
D_snow <- snow$TN_dry_kg

W_sprague <- sprague$TN_wet_kg
W_snow <- snow$TN_wet_kg
W_howe <- howe$TN_wet_kg


# Volume of the lake (converted to liters from m3) and discharge from lmc needed to calculate outflow
V_sprague <- sprague$lake_volume * 1000
Q_sprague <- sprague$lmc_m3 * 1000

V_snow <- snow$lake_volume * 1000
Q_snow <- snow$lmc_m3 * 1000

V_howe <- howe$lake_volume * 1000
Q_howe <- howe$lmc_m3 * 1000


# Discharge to calculate loading from the tributaries given the newly simulated concentrations
UMC_Q_sprague <- sprague$umc_m3 * 1000
SNY_Q_sprague <- sprague$sny_m3 * 1000
SPR_Q_sprague <- sprague$spr_m3 * 1000
FISH_Q_sprague <- sprague$fish_m3 * 1000

# define general shape of the hydrograph for each trib
get_discharge_means <- function(start, end, df) {
  df %>%
    filter(dateTime >= start & dateTime < end) %>%
    summarise(
      umc_discharge_mean = mean(umc_discharge_m3_s, na.rm = TRUE),
      sny_discharge_mean = mean(sny_discharge_m3_s, na.rm = TRUE),
      spr_discharge_mean = mean(spr_discharge_m3_s, na.rm = TRUE)
    )
}

# Apply the function across each row in `snow` to get discharge means
discharge_means <- map2_dfr(snow$start_date, snow$end_date,
                            ~ get_discharge_means(.x, .y, tribs_Q))

# normalize by max so just capturing the shape 

UMC_Q_hydro <- discharge_means$umc_discharge_mean/max(discharge_means$umc_discharge_mean)
SNY_Q_hydro <- discharge_means$sny_discharge_mean/max(discharge_means$sny_discharge_mean)
SPR_Q_hydro <- discharge_means$spr_discharge_mean/max(discharge_means$spr_discharge_mean)


UMC_Q_howe <- howe$umc_m3 * 1000
SNY_Q_howe <- howe$sny_m3 * 1000
SPR_Q_howe <- howe$spr_m3 * 1000
FISH_Q_howe <- howe$fish_m3 * 1000

#Constants
H_sprague <- sprague$H_N
S_sprague <- sprague$S_N

H_snow <- snow$H_N
S_snow <- snow$S_N

H_howe <- howe$H_N
S_howe <- howe$S_N

FISH_snow <- snow$fish_N

# Hydrograph shape
t_snow <- nrow(snow) # number of timesteps



all_outputs <- future_lapply(1:n_iter, function(i) {
  set.seed(123 + i)
  
  
  #### PHASE 1: Sprague decay----
  
  # Repeats picking from a gamma distribution until the criteria below is met
  repeat {
    UMC1_sprague <- rtrunc(1, spec = "gamma", a = UMC_mean_tn*5, b = UMC_mean_tn*10000, shape = 2, scale = (40*UMC_mean_tn)/2)
    SNY1_sprague <- rtrunc(1, spec = "gamma", a = SNY_mean_tn*5, b = SNY_mean_tn*10000, shape = 2, scale = (40*SNY_mean_tn)/2)
    SPR1_sprague <- rtrunc(1, spec = "gamma", a = SPR_mean_tn*5, b = SPR_mean_tn*10000, shape = 2, scale = (40*SPR_mean_tn)/2)
    FISH1_sprague <- rtrunc(1, spec = "gamma", a = FISH_mean_tn*5, b = FISH_mean_tn*10000, shape = 2, scale = (40*FISH_mean_tn)/2)
    
    # specifies that SNY and SPR concentrations need to be larger than UMC and FISH because of proximity to the fire
    if (SNY1_sprague > UMC1_sprague && SNY1_sprague > FISH1_sprague && SPR1_sprague > UMC1_sprague && SPR1_sprague > FISH1_sprague) break
  }
  
  # Picks from a gamma distribution to modify dry deposition
  D1_sprague <- rtrunc(1, spec = "gamma", a = D_mean_N, b = D_mean_N*860, shape = 2, scale = (N_mean*D_mean_N)/2)
  
  # Runs the decay for the Sprague fire over the number of time steps (defined by number of rows) in Sprague dataframe
  timesteps <- nrow(sprague)
  UMC_sprague  <- decay(UMC1_sprague, UMC_end_sprague <- sprague$umc_N_conc[timesteps], timesteps)
  SNY_sprague  <- decay(SNY1_sprague, SNY_end_sprague <- sprague$sny_N_conc[timesteps], timesteps)
  SPR_sprague  <- decay(SPR1_sprague, SPR_end_sprague <- sprague$spr_N_conc[timesteps], timesteps)
  FISH_sprague <- decay(FISH1_sprague, FISH_end_sprague <- sprague$fish_N_conc[timesteps], timesteps)
  D_sprague    <- decay(D1_sprague,  D_end_sprague <- sprague$TN_dry_kg[timesteps], timesteps)
  
  # Number of masses to predict +1 because of the initial value from Sprague 
  M <- numeric(timesteps + 1)
  M[1] <- M0
  
  # Predict mass except for the first time step (that is M0)
  for (t in 2:(timesteps + 1)) {
    M[t] <- M[t-1] +
      (UMC_sprague[t-1] * UMC_Q_sprague[t-1] + 
         SNY_sprague[t-1] * SNY_Q_sprague[t-1] + 
         SPR_sprague[t-1] * SPR_Q_sprague[t-1] +
         FISH_sprague[t-1] * FISH_Q_sprague[t-1]) * 1e-9 + 
      W_sprague[t-1] + D_sprague[t-1] + H_sprague[t-1] - S_sprague[t-1] -
      (M[t-1] / V_sprague[t-1]) * Q_sprague[t-1]
  }
  
  M_sprague <- M[-1]
  
  #### PHASE 2: Winter - null model----
  
  M_winter <- numeric(nrow(null_winter) + 1) # number of rows plus pull the initial value from the Sprague simulation
  M_winter[1] <- tail(M, 1) 
  
  for (w in 2:length(M_winter)) {
    M_winter[w] <- M_winter[w - 1] +
      null_winter$umc_N[w - 1] +
      null_winter$sny_N[w - 1] +
      null_winter$spr_N[w - 1] +
      null_winter$fish_N[w - 1] +
      null_winter$TN_dry_kg[w - 1] +
      null_winter$TN_wet_kg[w - 1] +
      null_winter$H_P[w - 1] -
      null_winter$S_P[w - 1] - 
      ((M_winter[w-1]/(null_winter$lake_volume[w-1]*1000))*(null_winter$lmc_m3[w-1]*1000))
  }
  
  #### PHASE 3: Snowmelt----
  
  # Simulate concentrations
  UMC_peak <- rtrunc(1, "gamma", a=UMC_mean_tn, b=UMC_mean_tn*10000, shape=2, scale=(8*UMC_mean_tn)/2)
  SNY_peak <- rtrunc(1, "gamma", a=SNY_mean_tn, b=SNY_mean_tn*10000, shape=2, scale=(8*SNY_mean_tn)/2)
  SPR_peak <- rtrunc(1, "gamma", a=SPR_mean_tn, b=SPR_mean_tn*10000, shape=2, scale=(8*SPR_mean_tn)/2)
  
  # Create the concentration time series given the hydrograph shape
  UMC_snow <- UMC_Q_hydro * UMC_peak
  SNY_snow <- SNY_Q_hydro * SNY_peak
  SPR_snow <- SPR_Q_hydro * SPR_peak
  
  # Simulate discharge 
  Q_scalar <- rtrunc(1, "gamma", a=1, b=100, shape=2, scale=1)
  UMC_Q_snow <- snow$umc_m3 * 1000 * Q_scalar
  SNY_Q_snow <- snow$sny_m3 * 1000 * Q_scalar
  SPR_Q_snow <- snow$spr_m3 * 1000 * Q_scalar
  
  # Predict mass except for the first time step, which comes from the Sprague simulation
  M_snow <- numeric(t_snow + 1)
  M_snow[1] <- tail(M_winter, 1)
  for (t in 2:(t_snow + 1)) {
    M_snow[t] <- M_snow[t - 1] +
      UMC_snow[t - 1] * UMC_Q_snow[t - 1] * 1e-9 +
      SNY_snow[t - 1] * SNY_Q_snow[t - 1] * 1e-9 +
      SPR_snow[t - 1] * SPR_Q_snow[t - 1] * 1e-9 +
      FISH_snow[t - 1] + W_snow[t - 1] + D_snow[t - 1] + H_snow[t - 1] - S_snow[t - 1] -
      (M_snow[t - 1] / V_snow[t - 1]) * Q_snow[t - 1]
  }
  
  #### PHASE 4: Summer - null model----
  
  M_summer <- numeric(nrow(null_summer) + 1) #number of rows plus pull the initial value from the snowmelt simulation
  M_summer[1] <- tail(M_snow, 1)
  
  for (s in 2:length(M_summer)) {
    M_summer[s] <- M_summer[s - 1] +
      null_summer$umc_N[s - 1] +
      null_summer$sny_N[s - 1] +
      null_summer$spr_N[s - 1] +
      null_summer$fish_N[s - 1] +
      null_summer$TN_dry_kg[s - 1] +
      null_summer$TN_wet_kg[s - 1] +
      null_summer$H_N[s - 1] -
      null_summer$S_N[s - 1] -
      ((M_summer[s - 1]/(null_summer$lake_volume[s-1]*1000)) * (null_summer$lmc_m3[s-1]*1000))
    
  }
  
  #### PHASE 5: Howe decay; same process as Sprague----
  
  n_howe <- nrow(howe)
  UMC1_howe <- rtrunc(1, spec = "gamma", a = UMC_mean_tn*5, b = UMC_mean_tn*10000, shape = 2, scale = (40*UMC_mean_tn)/2)
  SNY1_howe <- rtrunc(1, spec = "gamma", a = SNY_mean_tn*5, b = SNY_mean_tn*10000, shape = 2, scale = (40*SNY_mean_tn)/2)
  SPR1_howe <- rtrunc(1, spec = "gamma", a = SPR_mean_tn*5, b = SPR_mean_tn*10000, shape = 2, scale = (40*SPR_mean_tn)/2)
  FISH1_howe <- rtrunc(1, spec = "gamma", a = FISH_mean_tn*5, b = FISH_mean_tn*10000, shape = 2, scale = (40*FISH_mean_tn)/2)
  D1_howe <- rtrunc(1, spec = "gamma", a = D_mean_N, b = D_mean_N*860, shape = 2, scale = (N_mean*D_mean_N)/2)
  
  # Run the decay
  UMC_howe <- decay(UMC1_howe, howe$umc_N_conc[n_howe], n_howe)
  SNY_howe <- decay(SNY1_howe, howe$sny_N_conc[n_howe], n_howe)
  SPR_howe <- decay(SPR1_howe, howe$spr_N_conc[n_howe], n_howe)
  FISH_howe <- decay(FISH1_howe, howe$fish_N_conc[n_howe], n_howe)
  D_howe <- decay(D1_howe, howe$TN_dry_kg[n_howe], n_howe)
  
  
  M_howe <- numeric(n_howe + 1) #number of rows plus pull the initial value from the snowmelt simulation
  M_howe[1] <- tail(M_summer, 1)
  
  for (t in 2:(n_howe + 1)) {
    M_howe[t] <- M_howe[t - 1] +
      (UMC_howe[t - 1] * UMC_Q_howe[t - 1] +
         SNY_howe[t - 1] * SNY_Q_howe[t - 1] +
         SPR_howe[t - 1] * SPR_Q_howe[t - 1] +
         FISH_howe[t - 1] * FISH_Q_howe[t - 1]) * 1e-9 +
      W_howe[t - 1] + D_howe[t - 1] + H_howe[t - 1] - S_howe[t - 1] -
      (M_howe[t - 1] / V_howe[t - 1]) * Q_howe[t - 1]
  }
  
  # Save iterations
  # Then collect outputs into one list to return
  full_mass <- c(M_sprague, M_winter[-1], M_snow[-1], M_summer[-1], M_howe[-1])
  full_dates <- c(sprague$end_date, null_winter$end_date, snow$end_date, null_summer$end_date, howe$end_date)
  
  mass_out <- data.table(
    iteration = i,
    date = full_dates,
    mass_kg = full_mass
  )
  
  deposition_out <- rbind(
    data.table(iteration = i, date = sprague$end_date, D = D_sprague, phase = "Sprague"),
    data.table(iteration = i, date = howe$end_date, D = D_howe, phase = "Howe")
  )
  
  discharge_out <- data.table(
    iteration = i,
    date = rep(snow$end_date, 3),
    Q = c(UMC_Q_snow, SNY_Q_snow, SPR_Q_snow),
    site = rep(c("UMC", "SNY", "SPR"), each = nrow(snow)),
    phase = "Snowmelt"
  )
  
  conc_out <- data.table(
    iteration = i,
    date = c(rep(sprague$end_date, 4), rep(snow$end_date, 3), rep(howe$end_date, 4)),
    conc = c(UMC_sprague, SNY_sprague, SPR_sprague, FISH_sprague,
             UMC_snow, SNY_snow, SPR_snow,
             UMC_howe, SNY_howe, SPR_howe, FISH_howe),
    site = rep(c("UMC", "SNY", "SPR", "FISH", "UMC", "SNY", "SPR", "UMC", "SNY", "SPR", "FISH"),
               times = c(nrow(sprague), nrow(sprague), nrow(sprague), nrow(sprague),
                         nrow(snow), nrow(snow), nrow(snow),
                         nrow(howe), nrow(howe), nrow(howe), nrow(howe))),
    phase = c(rep("Sprague", nrow(sprague)*4), rep("Snowmelt", nrow(snow)*3), rep("Howe", nrow(howe)*4))
  )
  
  return(list(
    mass = mass_out,
    concentration = conc_out,
    discharge = discharge_out,
    deposition = deposition_out
  ))
})


# Extract each type of output from the results list
all_mass <- rbindlist(lapply(all_outputs, function(x) x[["mass"]]))
all_concentrations <- rbindlist(lapply(all_outputs, function(x) x[["concentration"]]))
all_discharges <- rbindlist(lapply(all_outputs, function(x) x[["discharge"]]))
all_deposition <- rbindlist(lapply(all_outputs, function(x) x[["deposition"]]))


fwrite(all_mass,           paste0(out_dir, "wildfire_simulation_mass_N.csv"))
fwrite(all_concentrations, paste0(out_dir, "wildfire_simulation_conc_N.csv"))
fwrite(all_discharges,     paste0(out_dir, "wildfire_simulation_discharge_N.csv"))
fwrite(all_deposition,     paste0(out_dir, "wildfire_simulation_deposition_N.csv"))


# Lake concentration----

mass_N <- read.csv("F:\\B_trout_files\\toobigforgit\\Chp_1\\wildfire_simulation_mass_N.csv", header = TRUE, sep=",")

mass_N$date <- as.Date(mass_N$date)

vol <- sim %>%
  select(end_date, lake_volume)%>%
  mutate(end_date = as.Date(end_date))

mass_N <- mass_N %>%
  left_join(vol, by = c("date" = "end_date"))%>%
  mutate(TN_conc = (mass_kg*10^9)/(lake_volume*1000))

TN_summary <- mass_N %>%
  group_by(date) %>%
  summarise(
    TN_conc_mean = mean(TN_conc, na.rm = TRUE),
    TN_conc_min  = min(TN_conc, na.rm = TRUE),
    TN_conc_2.5  = quantile(TN_conc, probs = 0.025, na.rm = TRUE),
    TN_conc_97.5  = quantile(TN_conc, probs = 0.975, na.rm = TRUE),
    TN_conc_max  = max(TN_conc, na.rm = TRUE)
  )


N_1445 <- subset(mass_N, date == "2018-09-18")
N_1445 <- subset(N_1445, TN_conc >=1445)

write.csv(TN_summary, "C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\fire_sim_TN_lake_for_plotting.csv" )


rm(list = setdiff(ls(), c("nuts", "tribs_Q", "UMC_mean_tp", "UMC_mean_tn", 
                          "SNY_mean_tp", "SNY_mean_tn",
                          "SPR_mean_tp", "SPR_mean_tn", 
                          "FISH_mean_tp", "FISH_mean_tn", 
                          "P_mean", "N_mean", "dates", "TP_summary", "TN_summary")))

# 3. Calculate Lake Concentration RMSE----


TN_summary <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\fire_sim_TN_lake_for_plotting.csv", header = TRUE, sep=",")

TN_summary$date <- as.Date(TN_summary$date, format = "%Y-%m-%d")

TP_summary <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\fire_sim_TP_lake_for_plotting.csv", header = TRUE, sep=",")

TP_summary$date <- as.Date(TP_summary$date, format = "%Y-%m-%d")

obs <- subset(nuts, !is.na(mean_tp))

obs <- obs %>%
  select(end_date, mean_tp, mean_tn) %>%
  filter(end_date %in% as.Date(c("2018-07-10", "2018-09-18"))) %>%
  left_join(TP_summary %>% select(date, TP_conc_mean), 
            by = c("end_date" = "date")) %>%
  left_join(TN_summary %>% select(date, TN_conc_mean), 
            by = c("end_date" = "date"))

rmse_tp <- sqrt(sum((obs$TP_conc_mean - obs$mean_tp)^2)/nrow(obs))

rmse_tn <- sqrt(sum((obs$TN_conc_mean - obs$mean_tn)^2)/nrow(obs))

# 4. Tributary Concentration plots---- 

#P concentration----

conc_P <- read.csv("F:\\B_trout_files\\toobigforgit\\Chp_1\\wildfire_simulation_conc.csv", header = TRUE, sep=",")

conc_P$date <- as.Date(conc_P$date)

# Calculate summary stats
concentration_P_summary <- conc_P %>%
  filter(site %in% c("UMC", "SPR", "SNY", "FISH")) %>%
  group_by(site, date) %>%
  summarise(
    conc_mean = mean(conc, na.rm = TRUE),
    conc_med  = median(conc, na.rm = TRUE),
    conc_2.5  = quantile(conc, 0.025, na.rm = TRUE),
    conc_25   = quantile(conc, 0.25, na.rm = TRUE),
    conc_75   = quantile(conc, 0.75, na.rm = TRUE),
    conc_97.5 = quantile(conc, 0.975, na.rm = TRUE),
    conc_min  = min(conc, na.rm = TRUE),
    conc_max  = max(conc, na.rm = TRUE),
    .groups = "drop"
  )

# Reduce to mean, range, and CI interval
concentration_P_summary <- concentration_P_summary %>%
  select(date, conc_mean, conc_min, conc_max, site, conc_2.5, conc_97.5)

# Add null model where necessary

null_winter <-  dates %>% 
  filter(end_date >= as.Date("2017-11-07") & end_date <= as.Date("2018-04-10"))  

snow <-  dates %>% 
  filter(end_date >= as.Date("2018-04-17") & end_date <= as.Date("2018-07-10")) %>%
  mutate(across(c(umc_N, umc_N_conc, sny_N, sny_N_conc, spr_N, spr_N_conc), ~ NA)) 

null_summer <-  dates %>% 
  filter(end_date >= as.Date("2018-07-17") & end_date <= as.Date("2018-08-14")) 

howe <- dates %>% 
  filter(end_date >= as.Date("2018-08-21") & end_date <= as.Date("2018-09-25")) %>%
  mutate(across(
    c(umc_N, sny_N, spr_N, fish_N, TN_dry_kg,
      umc_N_conc, sny_N_conc, spr_N_conc, fish_N_conc),
    ~ if_else(row_number() == n(), ., as.numeric(NA))
  ))

winter_umc <- null_winter %>%
  select(end_date, umc_P_conc, umc_P_conc_lwr, umc_P_conc_upr) %>%
  rename(date = "end_date", conc_mean = "umc_P_conc", conc_min = "umc_P_conc_lwr", conc_max = "umc_P_conc_upr") %>%
  mutate(site = "UMC", conc_2.5 = NA, conc_97.5 = NA)

winter_sny <- null_winter %>%
  select(end_date, sny_P_conc, sny_P_conc_lwr, sny_P_conc_upr)%>%
  rename(date = "end_date", conc_mean = "sny_P_conc", conc_min = "sny_P_conc_lwr", conc_max = "sny_P_conc_upr") %>%
  mutate(site = "SNY", conc_2.5 = NA, conc_97.5 = NA)

winter_spr <- null_winter %>%
  select(end_date, spr_P_conc, spr_P_conc_lwr, spr_P_conc_upr)%>%
  rename(date = "end_date", conc_mean = "spr_P_conc", conc_min = "spr_P_conc_lwr", conc_max = "spr_P_conc_upr") %>%
  mutate(site = "SPR", conc_2.5 = NA, conc_97.5 = NA)

winter_fish <- null_winter %>%
  select(end_date, fish_P_conc, fish_P_conc_lwr, fish_P_conc_upr)%>%
  rename(date = "end_date", conc_mean = "fish_P_conc", conc_min = "fish_P_conc_lwr", conc_max = "fish_P_conc_upr") %>%
  mutate(site = "FISH", conc_2.5 = NA, conc_97.5 = NA)

snow_fish <- snow %>%
  select(end_date, fish_P_conc, fish_P_conc_lwr, fish_P_conc_upr)%>%
  rename(date = "end_date", conc_mean = "fish_P_conc", conc_min = "fish_P_conc_lwr", conc_max = "fish_P_conc_upr") %>%
  mutate(site = "FISH", conc_2.5 = NA, conc_97.5 = NA)

summer_umc <- null_summer %>%
  select(end_date, umc_P_conc, umc_P_conc_lwr, umc_P_conc_upr) %>%
  rename(date = "end_date", conc_mean = "umc_P_conc", conc_min = "umc_P_conc_lwr", conc_max = "umc_P_conc_upr") %>%
  mutate(site = "UMC", conc_2.5 = NA, conc_97.5 = NA)

summer_sny <- null_summer %>%
  select(end_date, sny_P_conc, sny_P_conc_lwr, sny_P_conc_upr)%>%
  rename(date = "end_date", conc_mean = "sny_P_conc", conc_min = "sny_P_conc_lwr", conc_max = "sny_P_conc_upr") %>%
  mutate(site = "SNY", conc_2.5 = NA, conc_97.5 = NA)

summer_spr <- null_summer %>%
  select(end_date, spr_P_conc, spr_P_conc_lwr, spr_P_conc_upr)%>%
  rename(date = "end_date", conc_mean = "spr_P_conc", conc_min = "spr_P_conc_lwr", conc_max = "spr_P_conc_upr") %>%
  mutate(site = "SPR", conc_2.5 = NA, conc_97.5 = NA)

summer_fish <- null_summer %>%
  select(end_date, fish_P_conc, fish_P_conc_lwr, fish_P_conc_upr)%>%
  rename(date = "end_date", conc_mean = "fish_P_conc", conc_min = "fish_P_conc_lwr", conc_max = "fish_P_conc_upr") %>%
  mutate(site = "FISH", conc_2.5 = NA, conc_97.5 = NA)

nulls <- rbind(winter_umc, winter_sny, winter_spr, winter_fish, snow_fish, summer_umc, summer_sny, summer_spr, summer_fish)

all_P_conc <- rbind(concentration_P_summary, nulls)

# Set the ordering of the plot
all_P_conc <- all_P_conc %>%
  mutate(site = factor(site,
                       levels = c("UMC", "SPR", "SNY", "FISH"),
                       labels = c("Upper McDonald Creek", "Sprague Creek", "Snyder Creek", "Fish Creek"))) 

umc <- subset(all_P_conc, site == 'Upper McDonald Creek')


P_conc_fire <- ggplot(all_P_conc, aes(x = date, y = conc_mean)) +
  geom_ribbon(aes(ymin = conc_min, ymax = conc_max), fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(ymin = conc_2.5, ymax = conc_97.5), fill = "darkred", alpha = 0.5) +
  geom_line(color = "darkred", linewidth = 0.5) +
  facet_wrap(~site, scales = "free_y", nrow = 3, ncol = 2) +
  theme_classic() +
  labs(
    title = "",
    x = "Date",
    y = expression(
      paste("TP Concentration (",~ mu, "g-P ", L^{-1}, ")")
    )
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # Linetype and shape for events
  scale_linetype_manual(name = "", values = c("Wildfires" = "dotted", "Snowmelt" = "dotted")) +
  # Guide order: Model first, Events second
  guides(
    color = guide_legend(order = 1, override.aes = list(linetype = "solid")),
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 2)
  ) +  
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal"
  )  + scale_y_log10(breaks = c(1, 50, 100, 1000, 2000), labels = comma)


P_conc_fire

P_conc_umc <- ggplot(umc, aes(x = date, y = conc_mean)) +
  geom_ribbon(aes(ymin = conc_min, ymax = conc_max), fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(ymin = conc_2.5, ymax = conc_97.5), fill = "darkred", alpha = 0.5) +
  geom_line(color = "darkred", linewidth = 0.5) +
  theme_classic() +
  labs(
    title = "Upper McDonald Creek",
    x = "",
    y = expression(
      paste("TP Concentration (",~ mu, "g-P ", L^{-1}, ")")
    )
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # Linetype and shape for events
  scale_linetype_manual(name = "", values = c("Wildfires" = "dotted", "Snowmelt" = "dotted")) +
  # Guide order: Model first, Events second
  guides(
    color = guide_legend(order = 1, override.aes = list(linetype = "solid")),
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 2)
  ) +  
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal"
  )  + scale_y_log10(breaks = c(1, 50, 100, 1000, 2000), labels = comma)


P_conc_umc

ggsave(
  filename = "Fire_simulation_tribtuary_P_conc.png",   # file name (can be .png, .pdf, .jpg, .tiff, .svg)
  plot = P_conc_fire,             # the ggplot object
  width = 5,                        # width in chosen units
  height = 4,                       # height in chosen units
  units = "in",                     # "in", "cm", or "mm"
  dpi = 300                         # resolution (good for publications)
)


# N concentration----
conc_N <- read.csv("F:\\B_trout_files\\toobigforgit\\Chp_1\\wildfire_simulation_conc_N.csv", header = TRUE, sep=",")

conc_N$date <- as.Date(conc_N$date)

# Calculate summary stats
concentration_N_summary <- conc_N %>%
  filter(site %in% c("UMC", "SPR", "SNY", "FISH")) %>%
  group_by(site, date) %>%
  summarise(
    conc_mean = mean(conc, na.rm = TRUE),
    conc_med  = median(conc, na.rm = TRUE),
    conc_2.5  = quantile(conc, 0.025, na.rm = TRUE),
    conc_25   = quantile(conc, 0.25, na.rm = TRUE),
    conc_75   = quantile(conc, 0.75, na.rm = TRUE),
    conc_97.5 = quantile(conc, 0.975, na.rm = TRUE),
    conc_min  = min(conc, na.rm = TRUE),
    conc_max  = max(conc, na.rm = TRUE),
    .groups = "drop"
  )

# Reduce to mean and CI interval
concentration_N_summary <- concentration_N_summary %>%
  select(date, conc_mean, conc_min, conc_max, site, conc_2.5, conc_97.5)


# Add null model where necessary
winter_umc <- null_winter %>%
  select(end_date, umc_N_conc, umc_N_conc_lwr, umc_N_conc_upr) %>%
  rename(date = "end_date", conc_mean = "umc_N_conc", conc_min = "umc_N_conc_lwr", conc_max = "umc_N_conc_upr") %>%
  mutate(site = "UMC", conc_2.5 = NA, conc_97.5 = NA)

winter_sny <- null_winter %>%
  select(end_date, sny_N_conc, sny_N_conc_lwr, sny_N_conc_upr)%>%
  rename(date = "end_date", conc_mean = "sny_N_conc", conc_min = "sny_N_conc_lwr", conc_max = "sny_N_conc_upr") %>%
  mutate(site = "SNY", conc_2.5 = NA, conc_97.5 = NA)

winter_spr <- null_winter %>%
  select(end_date, spr_N_conc, spr_N_conc_lwr, spr_N_conc_upr)%>%
  rename(date = "end_date", conc_mean = "spr_N_conc", conc_min = "spr_N_conc_lwr", conc_max = "spr_N_conc_upr") %>%
  mutate(site = "SPR", conc_2.5 = NA, conc_97.5 = NA)

winter_fish <- null_winter %>%
  select(end_date, fish_N_conc, fish_N_conc_lwr, fish_N_conc_upr)%>%
  rename(date = "end_date", conc_mean = "fish_N_conc", conc_min = "fish_N_conc_lwr", conc_max = "fish_N_conc_upr") %>%
  mutate(site = "FISH", conc_2.5 = NA, conc_97.5 = NA)

snow_fish <- snow %>%
  select(end_date, fish_N_conc, fish_N_conc_lwr, fish_N_conc_upr)%>%
  rename(date = "end_date", conc_mean = "fish_N_conc", conc_min = "fish_N_conc_lwr", conc_max = "fish_N_conc_upr") %>%
  mutate(site = "FISH", conc_2.5 = NA, conc_97.5 = NA)

summer_umc <- null_summer %>%
  select(end_date, umc_N_conc, umc_N_conc_lwr, umc_N_conc_upr) %>%
  rename(date = "end_date", conc_mean = "umc_N_conc", conc_min = "umc_N_conc_lwr", conc_max = "umc_N_conc_upr") %>%
  mutate(site = "UMC", conc_2.5 = NA, conc_97.5 = NA)

summer_sny <- null_summer %>%
  select(end_date, sny_N_conc, sny_N_conc_lwr, sny_N_conc_upr)%>%
  rename(date = "end_date", conc_mean = "sny_N_conc", conc_min = "sny_N_conc_lwr", conc_max = "sny_N_conc_upr") %>%
  mutate(site = "SNY", conc_2.5 = NA, conc_97.5 = NA)

summer_spr <- null_summer %>%
  select(end_date, spr_N_conc, spr_N_conc_lwr, spr_N_conc_upr)%>%
  rename(date = "end_date", conc_mean = "spr_N_conc", conc_min = "spr_N_conc_lwr", conc_max = "spr_N_conc_upr") %>%
  mutate(site = "SPR", conc_2.5 = NA, conc_97.5 = NA)

summer_fish <- null_summer %>%
  select(end_date, fish_N_conc, fish_N_conc_lwr, fish_N_conc_upr)%>%
  rename(date = "end_date", conc_mean = "fish_N_conc", conc_min = "fish_N_conc_lwr", conc_max = "fish_N_conc_upr") %>%
  mutate(site = "FISH", conc_2.5 = NA, conc_97.5 = NA)


nulls <- rbind(winter_umc, winter_sny, winter_spr, winter_fish, snow_fish, summer_umc, summer_sny, summer_spr, summer_fish)


all_N_conc <- rbind(concentration_N_summary, nulls)

# Set the ordering of the plot
all_N_conc <- all_N_conc %>%
  mutate(site = factor(site,
                       levels = c("UMC", "SPR", "SNY", "FISH"),
                       labels = c("Upper McDonald Creek", "Sprague Creek", "Snyder Creek", "Fish Creek"))) 


N_conc_fire <- ggplot(all_N_conc, aes(x = date, y = conc_mean)) +
  geom_ribbon(aes(ymin = conc_min, ymax = conc_max), fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(ymin = conc_2.5, ymax = conc_97.5), fill = "darkred", alpha = 0.5) +
  geom_line(color = "darkred", linewidth = 0.5) +
  facet_wrap(~site, scales = "free_y", nrow = 3, ncol = 2) +
  theme_classic() +
  labs(
    title = "",
    x = "Week",
    y = expression(
      paste("TN Concentration (",~ mu, "g-N ", L^{-1}, ")")
    )
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # Linetype and shape for events
  scale_linetype_manual(name = "", values = c("Wildfires" = "dotted", "Snowmelt" = "dotted")) +
  # Guide order: Model first, Events second
  guides(
    color = guide_legend(order = 1, override.aes = list(linetype = "solid")),
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 2)
  ) +  
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) + scale_y_log10(breaks = c(1, 50, 100, 1000, 5000, 10000, 20000), labels = comma)
 
N_conc_fire

ggsave(
  filename = "N_conc_fire.png",   # file name (can be .png, .pdf, .jpg, .tiff, .svg)
  plot = N_conc_fire,             # the ggplot object
  width = 5,                        # width in chosen units
  height = 4,                       # height in chosen units
  units = "in",                     # "in", "cm", or "mm"
  dpi = 300                         # resolution (good for publications)
)

# 4. Plot Deposition ----
# Needed to calculate proper deposition intervals after simulation----

wet <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\WetDepositionN.csv', header = TRUE, sep = ",")

# Convert to POSIX
wet$dateOn <- as.Date(wet$dateOn)
wet$dateOff <- as.Date(wet$dateOff)

# Get NADP pulls
precip_dates_times <- wet %>%
  select(dateOn, dateOff)

# Dry deposition volume----
dry <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\dry_for_fire_sim.csv', header = TRUE, sep = ",")

dry$Date <- as.Date(dry$Date, format = "%Y-%m-%d")

# Get deposition volume

dry <- subset(dry, Parameter == "TP")

dry <- dry %>%
  dplyr::mutate(date_diff = as.integer(Date - lag(Date)), 
                vol_L_d = Volume_L/date_diff,
                start_date = lag(Date),
                end_date = Date) %>%
  select(start_date, end_date, date_diff, vol_L_d)


calculate_vol_totals <- function(dry, ranges) {
  results <- list()  # To store results for each range
  
  # Ensure dates are properly formatted
  dry$start_date <- as.Date(dry$start_date)
  dry$end_date <- as.Date(dry$end_date)
  ranges$dateOn <- as.Date(ranges$dateOn)
  ranges$dateOff <- as.Date(ranges$dateOff)
  
  # Loop through each date range in precip_dates_times
  for (i in seq_len(nrow(ranges))) {
    current_start <- ranges$dateOn[i]
    current_end <- ranges$dateOff[i]
    
    # Filter dry for records that overlap with the current range
    filtered <- dry %>%
      filter(start_date <= current_end & end_date >= current_start) %>%
      # Calculate overlapping days
      mutate(
        overlap_start = pmax(start_date, current_start),
        overlap_end = pmin(end_date, current_end),
        overlapping_days = as.integer(overlap_end - overlap_start + 1)
      )
    
    # Check if 'overlapping_days' exists
    if ("overlapping_days" %in% names(filtered)) {
      # Compute total TN and TP depositions
      vol_L <- sum(filtered$vol_L_d * filtered$overlapping_days, na.rm = TRUE)
      
      # Store the result
      results[[i]] <- data.frame(
        range_start = current_start,
        range_end = current_end,
        vol_L = vol_L
      )
    } else {
      # Handle cases where overlapping_days could not be computed (e.g., no overlap)
      results[[i]] <- data.frame(
        range_start = current_start,
        range_end = current_end,
        vol_L = 0,
      )
    }
  }
  
  # Combine all results into a single dataframe
  result_df <- do.call(rbind, results)
  return(result_df)
}

# Call relevant dataframes
result <- calculate_vol_totals(dry, precip_dates_times)
print(result)

# Upload simulated P deposition----

dep_P <- read.csv("F:\\B_trout_files\\toobigforgit\\Chp_1\\wildfire_simulation_deposition.csv", header = TRUE, sep=",")

dep_P$date <- as.Date(dep_P$date)

# Calculate summary stats
dep_P_summary <- dep_P %>%
  group_by(date) %>%
  summarise(
    load_mean = mean(D, na.rm = TRUE),
    load_med  = median(D, na.rm = TRUE),
    load_2.5  = quantile(D, 0.025, na.rm = TRUE),
    load_25   = quantile(D, 0.25, na.rm = TRUE),
    load_75   = quantile(D, 0.75, na.rm = TRUE),
    load_97.5 = quantile(D, 0.975, na.rm = TRUE),
    load_min  = min(D, na.rm = TRUE),
    load_max  = max(D, na.rm = TRUE),
    type = "Dry Deposition",
    .groups = "drop"
  )

# Reduce to mean and CI interval
dep_P_summary <- dep_P_summary %>%
  select(date, load_mean, load_2.5, load_97.5, type, load_min, load_max)

winter_dep <- null_winter %>%
  select(end_date, TP_dry_kg)%>%
  rename(date = "end_date", load_mean = "TP_dry_kg") %>%
  mutate(load_2.5 = "NA", load_97.5 = "NA", type = "Dry Deposition", load_min = NA, load_max = NA)

snow_dep <- snow %>%
  select(end_date, TP_dry_kg)%>%
  rename(date = "end_date", load_mean = "TP_dry_kg") %>%
  mutate(load_2.5 = "NA", load_97.5 = "NA", type = "Dry Deposition", load_min = NA, load_max = NA)

summer_dep <- null_summer %>%
  select(end_date, TP_dry_kg)%>%
  rename(date = "end_date", load_mean = "TP_dry_kg") %>%
  mutate(load_2.5 = "NA", load_97.5 = "NA", type = "Dry Deposition", load_min = NA, load_max = NA)

# Combine all P dep values
dep_P_all <- rbind(dep_P_summary, winter_dep, snow_dep, summer_dep)

# Make sure conc columns are numeric
dep_P_all <- dep_P_all %>%
  mutate(across(c(load_mean, load_2.5, load_97.5, load_min, load_max), as.numeric)) %>%
  left_join(result, by = c("date" = "range_end"))

dep_P_all <- dep_P_all %>%
  mutate(conc_mean = (load_mean*(10^9)*(0.062902/27810670.1791)*(1/vol_L)), 
         conc_min = (load_min*(10^9)*(0.062902/27810670.1791)*(1/vol_L)),
         conc_2.5 = (load_2.5*(10^9)*(0.062902/27810670.1791)*(1/vol_L)), 
         conc_97.5 = (load_97.5*(10^9)*(0.062902/27810670.1791)*(1/vol_L)), 
         conc_max = (load_max*(10^9)*(0.062902/27810670.1791)*(1/vol_L)))


dep_P_plot <- ggplot(dep_P_all, aes(x = date, y = conc_mean)) +
  geom_ribbon(aes(ymin = conc_min, ymax = conc_max), fill = "darkred", alpha =0.2) +
  geom_ribbon(aes(ymin = conc_2.5, ymax = conc_97.5), fill = "darkred", alpha = 0.5) +
  geom_line(color = "darkred", linewidth = 0.5) +
  theme_classic() +
  labs(
    title = "Dry Deposition",
    x = "",
    y = expression(paste("TP Concentration (",~ mu, "g-P ", L^{-1}, ")")
    )
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # Linetype and shape for events
  scale_linetype_manual(name = "", values = c("Wildfires" = "dotted", "Snowmelt" = "dotted")) +
  # Guide order: Model first, Events second
  guides(
    color = guide_legend(order = 1, override.aes = list(linetype = "solid")),
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 2)
  ) +  
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) + theme(legend.position = "none") + scale_y_log10(breaks = c(1, 10, 100, 1000), labels = comma)

dep_P_plot

# Upload simulated N deposition----
dep_N <- read.csv("F:\\B_trout_files\\toobigforgit\\Chp_1\\wildfire_simulation_deposition_N.csv", header = TRUE, sep=",")

dep_N$date <- as.Date(dep_N$date)

# Calculate summary stats
dep_N_summary <- dep_N %>%
  group_by(date) %>%
  summarise(
    load_mean = mean(D, na.rm = TRUE),
    load_med  = median(D, na.rm = TRUE),
    load_2.5  = quantile(D, 0.025, na.rm = TRUE),
    load_25   = quantile(D, 0.25, na.rm = TRUE),
    load_75   = quantile(D, 0.75, na.rm = TRUE),
    load_97.5 = quantile(D, 0.975, na.rm = TRUE),
    load_min  = min(D, na.rm = TRUE),
    load_max  = max(D, na.rm = TRUE),
    type = "Dry Deposition",
    .groups = "drop"
  )

# Reduce to mean and CI interval
dep_N_summary <- dep_N_summary %>%
  select(date, load_mean, load_2.5, load_97.5, type, load_min, load_max)

winter_dep <- null_winter %>%
  select(end_date, TN_dry_kg)%>%
  rename(date = "end_date", load_mean = "TN_dry_kg") %>%
  mutate(load_2.5 = "NA", load_97.5 = "NA", type = "Dry Deposition", load_min = NA, load_max = NA)

snow_dep <- snow %>%
  select(end_date, TN_dry_kg)%>%
  rename(date = "end_date", load_mean = "TN_dry_kg") %>%
  mutate(load_2.5 = "NA", load_97.5 = "NA", type = "Dry Deposition", load_min = NA, load_max = NA)

summer_dep <- null_summer %>%
  select(end_date, TN_dry_kg)%>%
  rename(date = "end_date", load_mean = "TN_dry_kg") %>%
  mutate(load_2.5 = "NA", load_97.5 = "NA", type = "Dry Deposition", load_min = NA, load_max = NA)

# Combine all N dep values
dep_N_all <- rbind(dep_N_summary, winter_dep, snow_dep, summer_dep)

# Make sure conc columns are numeric
dep_N_all <- dep_N_all %>%
  mutate(across(c(load_mean, load_2.5, load_97.5, load_min, load_max), as.numeric)) %>%
  left_join(result, by = c("date" = "range_end"))

dep_N_all <- dep_N_all %>%
  mutate(conc_mean = (load_mean*(10^9)*(0.062902/27810670.1791)*(1/vol_L)), 
         conc_min = (load_min*(10^9)*(0.062902/27810670.1791)*(1/vol_L)), 
         conc_2.5 = (load_2.5*(10^9)*(0.062902/27810670.1791)*(1/vol_L)), 
         conc_97.5 = (load_97.5*(10^9)*(0.062902/27810670.1791)*(1/vol_L)), 
         conc_max = (load_max*(10^9)*(0.062902/27810670.1791)*(1/vol_L)))


dep_N_plot <- ggplot(dep_N_all, aes(x = date, y = conc_mean)) +
  geom_ribbon(aes(ymin = conc_min, ymax = conc_max), fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(ymin = conc_2.5, ymax = conc_97.5), fill = "darkred", alpha = 0.5) +
  geom_line(color = "darkred", linewidth = 0.5) +
  facet_wrap(~type, scales = "free_y") +
  theme_classic() +
  labs(
    title = "",
    x = "Week",
    y = expression(paste("TN Concentration (",~ mu, "g-N ", L^{-1}, ")")
    )
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # Linetype and shape for events
  scale_linetype_manual(name = "", values = c("Wildfires" = "dotted", "Snowmelt" = "dotted")) +
  # Guide order: Model first, Events second
  guides(
    color = guide_legend(order = 1, override.aes = list(linetype = "solid")),
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 2)
  ) +  
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) + scale_y_log10(breaks = c(1, 50, 100, 1000, 2000), labels = comma)

dep_N_plot

fire_dep <- dep_P_plot/dep_N_plot

ggsave(
  filename = "deposition.png",   # file name (can be .png, .pdf, .jpg, .tiff, .svg)
  plot = fire_dep,             # the ggplot object
  width = 3,                        # width in chosen units
  height = 4.5,                       # height in chosen units
  units = "in",                     # "in", "cm", or "mm"
  dpi = 300                         # resolution (good for publications)
)


#5. Plot Discharge----

#null model discharge 

tribs_Q <- readr::read_csv("F:\\B_trout_files\\toobigforgit\\Chp_1\\tribs_nuts_10_01_07_9_30_23.csv",col_types = cols(date_time = col_datetime(format = "")))

tribs_Q <- tribs_Q %>%
  select(date_time, umc_discharge_m3_s, umc_discharge_m3_s_lwr, umc_discharge_m3_s_upr, sny_discharge_m3_s, sny_discharge_m3_s_lwr, sny_discharge_m3_s_upr, spr_discharge_m3_s, spr_discharge_m3_s_lwr, spr_discharge_m3_s_upr, fish_discharge_m3_s, fish_discharge_m3_s_lwr, fish_discharge_m3_s_upr) %>%
  filter(
    date_time >= as.POSIXct("2017-08-22 00:00:00") &
      date_time <= as.POSIXct("2018-09-25 23:00:00")
  )


dis <- read.csv("F:\\B_trout_files\\toobigforgit\\Chp_1\\wildfire_simulation_discharge.csv", header = TRUE, sep=",")

dis$date <- as.Date(dis$date)

# Calculate summary stats
dis_summary <- dis %>%
  group_by(date, site) %>%
  summarise(
    Q_mean = mean(Q, na.rm = TRUE),
    Q_med  = median(Q, na.rm = TRUE),
    Q_2.5  = quantile(Q, 0.025, na.rm = TRUE),
    Q_97.5 = quantile(Q, 0.975, na.rm = TRUE),
    Q_min  = min(Q, na.rm = TRUE),
    Q_max  = max(Q, na.rm = TRUE),
    type = "Discharge",
    .groups = "drop"
  )

# Reduce to mean and CI interval
dis_summary <- dis_summary %>%
  select(date, site, Q_mean, Q_2.5, Q_97.5, Q_min, Q_max)

# Get data from pre-snowmelt and post-snowmelt
dis_null <- nuts %>%
  select(end_date, 
         umc_m3,
         sny_m3,
         spr_m3, 
         fish_m3)

# Check multipliers to apply to instantaneous discharge----
dis_null_long <- dis_null %>%
  filter(
    (end_date >= as.Date("2017-08-22") & end_date <= as.Date("2018-09-25"))
  ) %>%
  pivot_longer(
    cols = -end_date,
    names_to = "name",
    values_to = "value"
  ) %>%
  mutate(
    site = str_extract(name, "umc|sny|spr|fish"),
    stat = case_when(
      TRUE ~ "Q_null"
    ),
    site = toupper(site)  # Convert site to uppercase
  ) %>%
  select(-name) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  rename(date = end_date)

# Combine dataframes

mult <- dis_null_long %>%
  left_join(dis_summary, by = c("date", "site"))

#need to convert back to m3/s because simulation spits out L/s
mult <- mult %>% 
  mutate(mult.max = (Q_max/1000)/(Q_null),
         mult.97.5 = (Q_97.5/1000)/(Q_null),
         mult.min = (Q_min/1000)/(Q_null), 
         mult.2.5 = (Q_2.5/1000)/(Q_null),
         mult.mean = (Q_mean/1000)/(Q_null))


# Apply multipliers to instantaneous discharge records used for the null model

umc_dis_fire_sim <- tribs_Q %>%
  select(date_time, umc_discharge_m3_s) %>%
  filter(
    (date_time >= as.Date("2018-04-10") & date_time <= as.Date("2018-07-10"))
  ) %>%
  mutate(Q_min = umc_discharge_m3_s*1, 
         Q_2.5 = umc_discharge_m3_s*1.05, 
         Q_mean  = umc_discharge_m3_s*2.5, 
         Q_97.5 = umc_discharge_m3_s*5.97,
         Q_max = umc_discharge_m3_s*16.49) %>%
  mutate(site = "Upper McDonald Creek")
           
           
sny_dis_fire_sim <- tribs_Q %>%
  select(date_time, sny_discharge_m3_s) %>%
  filter(
    (date_time >= as.Date("2018-04-10") & date_time <= as.Date("2018-07-10"))
  ) %>%
  mutate(Q_min = sny_discharge_m3_s*1, 
         Q_2.5 = sny_discharge_m3_s*1.05, 
         Q_mean  = sny_discharge_m3_s*2.5, 
         Q_97.5 = sny_discharge_m3_s*5.97,
         Q_max = sny_discharge_m3_s*16.49) %>%
  mutate(site = "Snyder Creek")

spr_dis_fire_sim <- tribs_Q %>%
  select(date_time, spr_discharge_m3_s) %>%
  filter(
    (date_time >= as.Date("2018-04-10") & date_time <= as.Date("2018-07-10"))
  ) %>%
  mutate(Q_min = spr_discharge_m3_s*1, 
         Q_2.5 = spr_discharge_m3_s*1.05, 
         Q_mean  = spr_discharge_m3_s*2.5, 
         Q_97.5 = spr_discharge_m3_s*5.97,
         Q_max = spr_discharge_m3_s*16.49) %>%
  mutate(site = "Sprague Creek")


# Plot

pre_snow <- tribs_Q %>%
  select(date_time, umc_discharge_m3_s, sny_discharge_m3_s, spr_discharge_m3_s, umc_discharge_m3_s_lwr, sny_discharge_m3_s_lwr, spr_discharge_m3_s_lwr, umc_discharge_m3_s_upr, sny_discharge_m3_s_upr, spr_discharge_m3_s_upr) %>%
  filter(
    date_time >= as.POSIXct("2017-08-22 00:00:00") &
      date_time <= as.POSIXct("2018-04-09 23:00:00")
  )

post_snow <- tribs_Q %>%
  select(date_time, umc_discharge_m3_s, sny_discharge_m3_s, spr_discharge_m3_s, umc_discharge_m3_s_lwr, sny_discharge_m3_s_lwr, spr_discharge_m3_s_lwr, umc_discharge_m3_s_upr, sny_discharge_m3_s_upr, spr_discharge_m3_s_upr) %>%
  filter(
    date_time >= as.POSIXct("2018-07-11 00:00:00") &
      date_time <= as.POSIXct("2018-09-18 23:00:00")
  )


dis_umc_plot <- ggplot(umc_dis_fire_sim) +
  geom_ribbon(aes(x = date_time, ymin = Q_min, ymax = Q_max), fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(x=date_time, ymin = Q_2.5, ymax = Q_97.5), fill = "darkred", alpha = 0.5) +
  geom_line(aes(x = date_time, y = Q_mean), color = "darkred", linewidth = 0.5) +
  
  geom_ribbon(data = pre_snow, aes(x = date_time, ymin = umc_discharge_m3_s_lwr, ymax = umc_discharge_m3_s_upr), fill = "darkred", alpha = 0.2) +
  geom_line(data = pre_snow, aes(x = date_time, y = umc_discharge_m3_s), color = "darkred", linewidth = 0.5) +
  
  geom_ribbon(data = post_snow, aes(x = date_time, ymin = umc_discharge_m3_s_lwr, ymax = umc_discharge_m3_s_upr), fill = "darkred", alpha = 0.2) +
  geom_line(data = post_snow, aes(x = date_time, y = umc_discharge_m3_s), color = "darkred", linewidth = 0.5) +
  theme_classic() +
  labs(
    title = "Upper McDonald Creek",
    x = "",
    y = expression(paste("Discharge (", m^{3}, s^{-1}, ")")
    )
  ) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5)
  ) + theme(legend.position = "none")+ scale_y_log10(breaks = c(1, 50, 100, 1000, 1500), labels = comma)


dis_umc_plot


dis_sny_plot <- ggplot(sny_dis_fire_sim) +
  geom_ribbon(aes(x = date_time, ymin = Q_min, ymax = Q_max), fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(x=date_time, ymin = Q_2.5, ymax = Q_97.5), fill = "darkred", alpha = 0.5) +
  geom_line(aes(x = date_time, y = Q_mean), color = "darkred", linewidth = 0.5) +
  
  geom_ribbon(data = pre_snow, aes(x = date_time, ymin = sny_discharge_m3_s_lwr, ymax = sny_discharge_m3_s_upr), fill = "darkred", alpha = 0.2) +
  geom_line(data = pre_snow, aes(x = date_time, y = sny_discharge_m3_s), color = "darkred", linewidth = 0.5) +
  
  geom_ribbon(data = post_snow, aes(x = date_time, ymin = sny_discharge_m3_s_lwr, ymax = sny_discharge_m3_s_upr), fill = "darkred", alpha = 0.2) +
  geom_line(data = post_snow, aes(x = date_time, y = sny_discharge_m3_s), color = "darkred", linewidth = 0.5) +
  facet_wrap(~site, scales = "free_y", nrow=3) +
  theme_classic() +
  labs(
    title = "",
    x = "",
    y = expression(paste("Discharge (", m^{3}, s^{-1}, ")")
    )
  ) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5)
  ) + theme(legend.position = "none")+ scale_y_log10(breaks = c(1, 5, 10, 20, 50), labels = comma)


dis_sny_plot


dis_spr_plot <- ggplot(spr_dis_fire_sim) +
  geom_ribbon(aes(x = date_time, ymin = Q_min, ymax = Q_max), fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(x=date_time, ymin = Q_2.5, ymax = Q_97.5), fill = "darkred", alpha = 0.5) +
  geom_line(aes(x = date_time, y = Q_mean), color = "darkred", linewidth = 0.5) +
  
  geom_ribbon(data = pre_snow, aes(x = date_time, ymin = spr_discharge_m3_s_lwr, ymax = spr_discharge_m3_s_upr), fill = "darkred", alpha = 0.2) +
  geom_line(data = pre_snow, aes(x = date_time, y = spr_discharge_m3_s), color = "darkred", linewidth = 0.5) +
  
  geom_ribbon(data = post_snow, aes(x = date_time, ymin = spr_discharge_m3_s_lwr, ymax = spr_discharge_m3_s_upr), fill = "darkred", alpha = 0.2) +
  geom_line(data = post_snow, aes(x = date_time, y = spr_discharge_m3_s), color = "darkred", linewidth = 0.5) +
  facet_wrap(~site, scales = "free_y", nrow=3) +
  theme_classic() +
  labs(
    title = "",
    x = "",
    y = expression(paste("Discharge (", m^{3}, s^{-1}, ")")
    )
  ) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5)
  )+ 
  scale_y_continuous(limits = c(0,80),
                     labels = scales::comma
  ) + theme(legend.position = "none") + scale_y_log10(breaks = c(1, 5, 10, 20, 50), labels = comma) 


dis_spr_plot

dis_all <- dis_umc_plot/dis_sny_plot/dis_spr_plot


ggsave(
  filename = "Discharge.png",   # file name (can be .png, .pdf, .jpg, .tiff, .svg)
  plot = dis_all,             # the ggplot object
  width = 5,                        # width in chosen units
  height = 6,                       # height in chosen units
  units = "in",                     # "in", "cm", or "mm"
  dpi = 300                         # resolution (good for publications)
)


#6. Calculate difference in loading during simulation periods----

#P----
# Lake mass

TP_summary <- TP_summary %>%
  left_join(nuts %>% select(end_date, kg_P_est, kg_P_est_lwr, kg_P_est_upr, TP_conc_est, TP_conc_est_lwr, TP_conc_est_upr), by = c("date" = "end_date"))

#summarize final mass and concentration predictions

after_sprague_P <- subset(TP_summary, date == "2017-10-31")
after_snow_P <- subset(TP_summary, date == "2018-07-10")
after_howe_P <- subset(TP_summary, date == "2018-09-18")

# deposition

#fire
sprague_dep_P <-  dep_P_summary %>% 
  filter(date >= as.Date("2017-08-22") & date <= as.Date("2017-10-31"))

#null
sprague <-  dates %>% 
  filter(end_date >= as.Date("2017-08-22") & end_date <= as.Date("2017-10-31"))

#fire
howe_dep_P <-  dep_P_summary %>% 
  filter(date >= as.Date("2018-08-21") & date <= as.Date("2018-09-25"))

#null
howe <-  dates %>% 
  filter(end_date >= as.Date("2018-08-21") & end_date <= as.Date("2018-09-25"))


# Isolate trib concentrations
dis_null_long <- dis_null_long %>%
  mutate(site = recode(site,
                       "UMC"  = "Upper McDonald Creek",
                       "SNY"  = "Snyder Creek",
                       "SPR"  = "Sprague Creek",
                       "FISH" = "Fish Creek"))

dis_summary <- dis_summary %>%
  mutate(site = recode(site,
                       "UMC"  = "Upper McDonald Creek",
                       "SNY"  = "Snyder Creek",
                       "SPR"  = "Sprague Creek",
                       "FISH" = "Fish Creek"))

all_P_conc <- all_P_conc %>% 
  left_join(dis_null_long, by = c("date", "site")) %>%
  left_join(dis_summary, by = c("date", "site"))




umc_P <- subset(all_P_conc, site == "Upper McDonald Creek")

umc_P_sprague <- umc_P %>% 
  filter(date >= as.Date("2017-08-22") & date <= as.Date("2017-10-31"))

umc_P_snow <- umc_P %>% 
  filter(date >= as.Date("2018-04-17") & date <= as.Date("2018-07-10"))

umc_P_howe <- umc_P %>% 
  filter(date >= as.Date("2018-08-21") & date <= as.Date("2018-09-25"))


sny_P <- subset(all_P_conc, site == "Snyder Creek")

sny_P_sprague <- sny_P %>% 
  filter(date >= as.Date("2017-08-22") & date <= as.Date("2017-10-31"))

sny_P_snow <- sny_P %>% 
  filter(date >= as.Date("2018-04-17") & date <= as.Date("2018-07-10"))

sny_P_howe <- sny_P %>% 
  filter(date >= as.Date("2018-08-21") & date <= as.Date("2018-09-25"))


spr_P <- subset(all_P_conc, site == "Sprague Creek")

spr_P_sprague <- spr_P %>% 
  filter(date >= as.Date("2017-08-22") & date <= as.Date("2017-10-31"))

spr_P_snow <- spr_P %>% 
  filter(date >= as.Date("2018-04-17") & date <= as.Date("2018-07-10"))

spr_P_howe <- spr_P %>% 
  filter(date >= as.Date("2018-08-21") & date <= as.Date("2018-09-25"))


fish_P <- subset(all_P_conc, site == "Fish Creek")

fish_P_sprague <- fish_P %>% 
  filter(date >= as.Date("2017-08-22") & date <= as.Date("2017-10-31"))

fish_P_snow <- fish_P %>% 
  filter(date >= as.Date("2018-04-17") & date <= as.Date("2018-07-10"))

fish_P_howe <- fish_P %>% 
  filter(date >= as.Date("2018-08-21") & date <= as.Date("2018-09-25"))



P_table <- as.data.frame(x = c("Upper McDonald Creek", "Snyder Creek", "Sprague Creek", "Fish Creek", "Dry Deposition", "Concentration"))
colnames(P_table) <- "Parameter"

P_table <- P_table %>% 
  mutate(Null_mean_sprague = c(sum(sprague$umc_P), 
                               sum(sprague$sny_P), 
                               sum(sprague$spr_P), 
                               sum(sprague$fish_P), 
                               sum(sprague$TP_dry_kg), 
                               after_sprague_P$TP_conc_est),
                               
         Null_min_sprague = c(sum(sprague$umc_P_lwr), 
                              sum(sprague$sny_P_lwr), 
                              sum(sprague$spr_P_lwr), 
                              sum(sprague$fish_P_lwr),
                              NA, 
                            after_sprague_P$TP_conc_est_lwr), 
                            
         Null_max_sprague = c(sum(sprague$umc_P_upr), 
                              sum(sprague$sny_P_upr), 
                              sum(sprague$spr_P_upr), 
                              sum(sprague$fish_P_upr),
                              NA, 
                              after_sprague_P$TP_conc_est_upr),
                              
         Simulation_mean_sprague = c(sum((umc_P_sprague$conc_mean*10^-9)*(umc_P_sprague$Q_null*1000)), 
                                     sum((sny_P_sprague$conc_mean*10^-9)*(sny_P_sprague$Q_null*1000)), 
                                     sum((spr_P_sprague$conc_mean*10^-9)*(spr_P_sprague$Q_null*1000)), 
                                     sum((fish_P_sprague$conc_mean*10^-9)*(fish_P_sprague$Q_null*1000)), 
                                     sum(sprague_dep_P$load_mean),  
                                     after_sprague_P$TP_conc_mean),
         
         Simulation_min_sprague = c(sum((umc_P_sprague$conc_min*10^-9)*(umc_P_sprague$Q_null*1000)), 
                              sum((sny_P_sprague$conc_min*10^-9)*(sny_P_sprague$Q_null*1000)), 
                              sum((spr_P_sprague$conc_min*10^-9)*(spr_P_sprague$Q_null*1000)), 
                              sum((fish_P_sprague$conc_min*10^-9)*(fish_P_sprague$Q_null*1000)), 
                              sum(sprague_dep_P$load_min),  
                              after_sprague_P$TP_conc_min),
         
         Simulation_max_sprague = c(sum((umc_P_sprague$conc_max*10^-9)*(umc_P_sprague$Q_null*1000)), 
                              sum((sny_P_sprague$conc_max*10^-9)*(sny_P_sprague$Q_null*1000)), 
                              sum((spr_P_sprague$conc_max*10^-9)*(spr_P_sprague$Q_null*1000)), 
                              sum((fish_P_sprague$conc_max*10^-9)*(fish_P_sprague$Q_null*1000)), 
                              sum(sprague_dep_P$load_max),  
                              after_sprague_P$TP_conc_max), 
         Null_mean_snow = c(sum(snow$umc_P), 
                            sum(snow$sny_P), 
                            sum(snow$spr_P), 
                            sum(snow$fish_P), 
                            sum(snow$TP_dry_kg), 
                            after_snow_P$TP_conc_est),
         
         Null_min_snow = c(sum(snow$umc_P_lwr), 
                           sum(snow$sny_P_lwr), 
                           sum(snow$spr_P_lwr), 
                           sum(snow$fish_P_lwr),
                           NA, 
                           after_snow_P$TP_conc_est_lwr), 
         
         Null_max_snow = c(sum(snow$umc_P_upr), 
                           sum(snow$sny_P_upr), 
                           sum(snow$spr_P_upr), 
                           sum(snow$fish_P_upr),
                           NA, 
                           after_snow_P$TP_conc_est_upr),
         
         Simulation_mean_snow = c(sum((umc_P_snow$conc_mean*10^-9)*(umc_P_snow$Q_mean)), 
                                  sum((sny_P_snow$conc_mean*10^-9)*(sny_P_snow$Q_mean)), 
                                  sum((spr_P_snow$conc_mean*10^-9)*(spr_P_snow$Q_mean)), 
                                  sum(snow$fish_P),
                                  sum(snow$TP_dry_kg),
                                  after_snow_P$TP_conc_mean),
         
         Simulation_min_snow = c(sum((umc_P_snow$conc_min*10^-9)*(umc_P_snow$Q_min)), 
                                 sum((sny_P_snow$conc_min*10^-9)*(sny_P_snow$Q_min)), 
                                 sum((spr_P_snow$conc_min*10^-9)*(spr_P_snow$Q_min)), 
                                 sum(snow$fish_P_lwr),
                                 NA,
                                 after_snow_P$TP_conc_min),
         
         Simulation_max_snow = c(sum((umc_P_snow$conc_max*10^-9)*(umc_P_snow$Q_max)), 
                                 sum((sny_P_snow$conc_max*10^-9)*(sny_P_snow$Q_max)), 
                                 sum((spr_P_snow$conc_max*10^-9)*(spr_P_snow$Q_max)), 
                                 sum(snow$fish_P_upr),
                                 NA, 
                                 after_snow_P$TP_conc_max),
         Null_mean_howe = c(sum(howe$umc_P), 
                            sum(howe$sny_P), 
                            sum(howe$spr_P), 
                            sum(howe$fish_P), 
                            sum(howe$TP_dry_kg), 
                            after_howe_P$TP_conc_est),
         
         Null_min_howe = c(sum(howe$umc_P_lwr), 
                           sum(howe$sny_P_lwr), 
                           sum(howe$spr_P_lwr), 
                           sum(howe$fish_P_lwr),
                           NA, 
                           after_howe_P$TP_conc_est_lwr), 
         
         Null_max_howe = c(sum(howe$umc_P_upr), 
                           sum(howe$sny_P_upr), 
                           sum(howe$spr_P_upr), 
                           sum(howe$fish_P_upr),
                           NA, 
                           after_howe_P$TP_conc_est_upr),
         
         Simulation_mean_howe = c(sum((umc_P_howe$conc_mean*10^-9)*(umc_P_howe$Q_null*1000)), 
                                  sum((sny_P_howe$conc_mean*10^-9)*(sny_P_howe$Q_null*1000)), 
                                  sum((spr_P_howe$conc_mean*10^-9)*(spr_P_howe$Q_null*1000)), 
                                  sum((fish_P_howe$conc_mean*10^-9)*(fish_P_howe$Q_null*1000)), 
                                  sum(howe_dep_P$load_mean),  
                                  after_howe_P$TP_conc_mean),
         
         Simulation_min_howe = c(sum((umc_P_howe$conc_min*10^-9)*(umc_P_howe$Q_null*1000)), 
                                 sum((sny_P_howe$conc_min*10^-9)*(sny_P_howe$Q_null*1000)), 
                                 sum((spr_P_howe$conc_min*10^-9)*(spr_P_howe$Q_null*1000)), 
                                 sum((fish_P_howe$conc_min*10^-9)*(fish_P_howe$Q_null*1000)), 
                                 sum(howe_dep_P$load_min), 
                                 after_howe_P$TP_conc_min),
         
         Simulation_max_howe = c(sum((umc_P_howe$conc_max*10^-9)*(umc_P_howe$Q_null*1000)), 
                                 sum((sny_P_howe$conc_max*10^-9)*(sny_P_howe$Q_null*1000)), 
                                 sum((spr_P_howe$conc_max*10^-9)*(spr_P_howe$Q_null*1000)), 
                                 sum((fish_P_howe$conc_max*10^-9)*(fish_P_howe$Q_null*1000)), 
                                 sum(howe_dep_P$load_max), 
                                 after_howe_P$TP_conc_max)
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )

# create xtable
tab_P <- xtable(P_table,
              digits = 3)  # set decimals

# print LaTeX tabular code
print(tab_P, include.rownames = FALSE, booktabs = TRUE)


Trib_totals <- P_table[1:4,]

# Sum across columns, ignoring NA and column 1
trib_sums <- colSums(Trib_totals[ , -1], na.rm = TRUE)

trib_sums_df <- as.data.frame(t(trib_sums))

print(xtable(trib_sums_df), include.rownames = FALSE)



#repeat for N----

TN_summary <- TN_summary %>%
  left_join(nuts %>% select(end_date, kg_N_est, kg_N_est_lwr, kg_N_est_upr, TN_conc_est, TN_conc_est_lwr, TN_conc_est_upr), by = c("date" = "end_date"))

#summarize final mass and concentration predictions

after_sprague_N <- subset(TN_summary, date == "2017-10-31")
after_snow_N <- subset(TN_summary, date == "2018-07-10")
after_howe_N <- subset(TN_summary, date == "2018-09-18")

# deposition

#fire
sprague_dep_N <-  dep_N_summary %>% 
  filter(date >= as.Date("2017-08-22") & date <= as.Date("2017-10-31"))

#null
sprague <-  dates %>% 
  filter(end_date >= as.Date("2017-08-22") & end_date <= as.Date("2017-10-31"))

#fire
howe_dep_N <-  dep_N_summary %>% 
  filter(date >= as.Date("2018-08-21") & date <= as.Date("2018-09-25"))

#null
howe <-  dates %>% 
  filter(end_date >= as.Date("2018-08-21") & end_date <= as.Date("2018-09-25"))


# Isolate trib concentrations
dis_null_long <- dis_null_long %>%
  mutate(site = recode(site,
                       "UMC"  = "Upper McDonald Creek",
                       "SNY"  = "Snyder Creek",
                       "SPR"  = "Sprague Creek",
                       "FISH" = "Fish Creek"))

dis_summary <- dis_summary %>%
  mutate(site = recode(site,
                       "UMC"  = "Upper McDonald Creek",
                       "SNY"  = "Snyder Creek",
                       "SPR"  = "Sprague Creek",
                       "FISH" = "Fish Creek"))

all_N_conc <- all_N_conc %>% 
  left_join(dis_null_long, by = c("date", "site")) %>%
  left_join(dis_summary, by = c("date", "site"))




umc_N <- subset(all_N_conc, site == "Upper McDonald Creek")

umc_N_sprague <- umc_N %>% 
  filter(date >= as.Date("2017-08-22") & date <= as.Date("2017-10-31"))

umc_N_snow <- umc_N %>% 
  filter(date >= as.Date("2018-04-17") & date <= as.Date("2018-07-10"))

umc_N_howe <- umc_N %>% 
  filter(date >= as.Date("2018-08-21") & date <= as.Date("2018-09-25"))


sny_N <- subset(all_N_conc, site == "Snyder Creek")

sny_N_sprague <- sny_N %>% 
  filter(date >= as.Date("2017-08-22") & date <= as.Date("2017-10-31"))

sny_N_snow <- sny_N %>% 
  filter(date >= as.Date("2018-04-17") & date <= as.Date("2018-07-10"))

sny_N_howe <- sny_N %>% 
  filter(date >= as.Date("2018-08-21") & date <= as.Date("2018-09-25"))


spr_N <- subset(all_N_conc, site == "Sprague Creek")

spr_N_sprague <- spr_N %>% 
  filter(date >= as.Date("2017-08-22") & date <= as.Date("2017-10-31"))

spr_N_snow <- spr_N %>% 
  filter(date >= as.Date("2018-04-17") & date <= as.Date("2018-07-10"))

spr_N_howe <- spr_N %>% 
  filter(date >= as.Date("2018-08-21") & date <= as.Date("2018-09-25"))


fish_N <- subset(all_N_conc, site == "Fish Creek")

fish_N_sprague <- fish_N %>% 
  filter(date >= as.Date("2017-08-22") & date <= as.Date("2017-10-31"))

fish_N_snow <- fish_N %>% 
  filter(date >= as.Date("2018-04-17") & date <= as.Date("2018-07-10"))

fish_N_howe <- fish_N %>% 
  filter(date >= as.Date("2018-08-21") & date <= as.Date("2018-09-25"))



N_table <- as.data.frame(x = c("Upper McDonald Creek", "Snyder Creek", "Sprague Creek", "Fish Creek", "Dry Deposition", "Concentration"))
colnames(N_table) <- "Parameter"

N_table <- N_table %>% 
  mutate(Null_mean_sprague = c(sum(sprague$umc_N), 
                               sum(sprague$sny_N), 
                               sum(sprague$spr_N), 
                               sum(sprague$fish_N), 
                               sum(sprague$TN_dry_kg), 
                               after_sprague_N$TN_conc_est),
         
         Null_min_sprague = c(sum(sprague$umc_N_lwr), 
                              sum(sprague$sny_N_lwr), 
                              sum(sprague$spr_N_lwr), 
                              sum(sprague$fish_N_lwr),
                              NA, 
                              after_sprague_N$TN_conc_est_lwr), 
         
         Null_max_sprague = c(sum(sprague$umc_N_upr), 
                              sum(sprague$sny_N_upr), 
                              sum(sprague$spr_N_upr), 
                              sum(sprague$fish_N_upr),
                              NA, 
                              after_sprague_N$TN_conc_est_upr),
         
         Simulation_mean_sprague = c(sum((umc_N_sprague$conc_mean*10^-9)*(umc_N_sprague$Q_null*1000)), 
                                     sum((sny_N_sprague$conc_mean*10^-9)*(sny_N_sprague$Q_null*1000)), 
                                     sum((spr_N_sprague$conc_mean*10^-9)*(spr_N_sprague$Q_null*1000)), 
                                     sum((fish_N_sprague$conc_mean*10^-9)*(fish_N_sprague$Q_null*1000)), 
                                     sum(sprague_dep_N$load_mean),  
                                     after_sprague_N$TN_conc_mean),
         
         Simulation_min_sprague = c(sum((umc_N_sprague$conc_min*10^-9)*(umc_N_sprague$Q_null*1000)), 
                                    sum((sny_N_sprague$conc_min*10^-9)*(sny_N_sprague$Q_null*1000)), 
                                    sum((spr_N_sprague$conc_min*10^-9)*(spr_N_sprague$Q_null*1000)), 
                                    sum((fish_N_sprague$conc_min*10^-9)*(fish_N_sprague$Q_null*1000)), 
                                    sum(sprague_dep_N$load_min),  
                                    after_sprague_N$TN_conc_min),
         
         Simulation_max_sprague = c(sum((umc_N_sprague$conc_max*10^-9)*(umc_N_sprague$Q_null*1000)), 
                                    sum((sny_N_sprague$conc_max*10^-9)*(sny_N_sprague$Q_null*1000)), 
                                    sum((spr_N_sprague$conc_max*10^-9)*(spr_N_sprague$Q_null*1000)), 
                                    sum((fish_N_sprague$conc_max*10^-9)*(fish_N_sprague$Q_null*1000)), 
                                    sum(sprague_dep_N$load_max),  
                                    after_sprague_N$TN_conc_max), 
         
         Null_mean_snow = c(sum(snow$umc_N), 
                            sum(snow$sny_N), 
                            sum(snow$spr_N), 
                            sum(snow$fish_N), 
                            sum(snow$TN_dry_kg), 
                            after_snow_N$TN_conc_est),
         
         Null_min_snow = c(sum(snow$umc_N_lwr), 
                           sum(snow$sny_N_lwr), 
                           sum(snow$spr_N_lwr), 
                           sum(snow$fish_N_lwr),
                           NA, 
                           after_snow_N$TN_conc_est_lwr), 
         
         Null_max_snow = c(sum(snow$umc_N_upr), 
                           sum(snow$sny_N_upr), 
                           sum(snow$spr_N_upr), 
                           sum(snow$fish_N_upr),
                           NA, 
                           after_snow_N$TN_conc_est_upr),
         
         Simulation_mean_snow = c(sum((umc_N_snow$conc_mean*10^-9)*(umc_N_snow$Q_mean)), 
                                  sum((sny_N_snow$conc_mean*10^-9)*(sny_N_snow$Q_mean)), 
                                  sum((spr_N_snow$conc_mean*10^-9)*(spr_N_snow$Q_mean)), 
                                  sum(snow$fish_N),
                                  sum(snow$TN_dry_kg),
                                  after_snow_N$TN_conc_mean),
         
         Simulation_min_snow = c(sum((umc_N_snow$conc_min*10^-9)*(umc_N_snow$Q_min)), 
                                 sum((sny_N_snow$conc_min*10^-9)*(sny_N_snow$Q_min)), 
                                 sum((spr_N_snow$conc_min*10^-9)*(spr_N_snow$Q_min)), 
                                 sum(snow$fish_N_lwr),
                                 NA,
                                 after_snow_N$TN_conc_min),
         
         Simulation_max_snow = c(sum((umc_N_snow$conc_max*10^-9)*(umc_N_snow$Q_max)), 
                                 sum((sny_N_snow$conc_max*10^-9)*(sny_N_snow$Q_max)), 
                                 sum((spr_N_snow$conc_max*10^-9)*(spr_N_snow$Q_max)), 
                                 sum(snow$fish_N_upr),
                                 NA, 
                                 after_snow_N$TN_conc_max),
         Null_mean_howe = c(sum(howe$umc_N), 
                            sum(howe$sny_N), 
                            sum(howe$spr_N), 
                            sum(howe$fish_N), 
                            sum(howe$TN_dry_kg), 
                            after_howe_N$TN_conc_est),
         
         Null_min_howe = c(sum(howe$umc_N_lwr), 
                           sum(howe$sny_N_lwr), 
                           sum(howe$spr_N_lwr), 
                           sum(howe$fish_N_lwr),
                           NA, 
                           after_howe_N$TN_conc_est_lwr), 
         
         Null_max_howe = c(sum(howe$umc_N_upr), 
                           sum(howe$sny_N_upr), 
                           sum(howe$spr_N_upr), 
                           sum(howe$fish_N_upr),
                           NA, 
                           after_howe_N$TN_conc_est_upr),
         
         Simulation_mean_howe = c(sum((umc_N_howe$conc_mean*10^-9)*(umc_N_howe$Q_null*1000)), 
                                  sum((sny_N_howe$conc_mean*10^-9)*(sny_N_howe$Q_null*1000)), 
                                  sum((spr_N_howe$conc_mean*10^-9)*(spr_N_howe$Q_null*1000)), 
                                  sum((fish_N_howe$conc_mean*10^-9)*(fish_N_howe$Q_null*1000)), 
                                  sum(howe_dep_N$load_mean),  
                                  after_howe_N$TN_conc_mean),
         
         Simulation_min_howe = c(sum((umc_N_howe$conc_min*10^-9)*(umc_N_howe$Q_null*1000)), 
                                 sum((sny_N_howe$conc_min*10^-9)*(sny_N_howe$Q_null*1000)), 
                                 sum((spr_N_howe$conc_min*10^-9)*(spr_N_howe$Q_null*1000)), 
                                 sum((fish_N_howe$conc_min*10^-9)*(fish_N_howe$Q_null*1000)), 
                                 sum(howe_dep_N$load_min), 
                                 after_howe_N$TN_conc_min),
         
         Simulation_max_howe = c(sum((umc_N_howe$conc_max*10^-9)*(umc_N_howe$Q_null*1000)), 
                                 sum((sny_N_howe$conc_max*10^-9)*(sny_N_howe$Q_null*1000)), 
                                 sum((spr_N_howe$conc_max*10^-9)*(spr_N_howe$Q_null*1000)), 
                                 sum((fish_N_howe$conc_max*10^-9)*(fish_N_howe$Q_null*1000)), 
                                 sum(howe_dep_N$load_max), 
                                 after_howe_N$TN_conc_max)
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )

# create xtable
tab_N <- xtable(N_table,
                digits = 3)  # set decimals

# print LaTeX tabular code
print(tab_N, include.rownames = FALSE, booktabs = TRUE)

Trib_totals <- N_table[1:4,]

# Sum across columns, ignoring NA and column 1
trib_sums <- colSums(Trib_totals[ , -1], na.rm = TRUE)

trib_sums_df <- as.data.frame(t(trib_sums))

print(xtable(trib_sums_df), include.rownames = FALSE)

