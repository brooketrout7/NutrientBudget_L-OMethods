

#0. Load packages----

library(lubridate)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(tidyverse)
library(scales)
library(xtable)
library(ggbreak) 


# 1. Plots----

# Read in nutrient budget
nuts <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\NutrientBudget.csv", header = TRUE, sep=",")

nuts$end_date <- as.Date(nuts$end_date, format = "%Y-%m-%d")

# Read in fire simulation

TN_summary <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\fire_sim_TN_lake_for_plotting.csv", header = TRUE, sep=",")

TN_summary$date <- as.Date(TN_summary$date, format = "%Y-%m-%d")

TP_summary <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\fire_sim_TP_lake_for_plotting.csv", header = TRUE, sep=",")

TP_summary$date <- as.Date(TP_summary$date, format = "%Y-%m-%d")

# Concentration time series -----

# Predict concentrations from 1975 budgets----

# TP
vol <- 1491191000
surplus <- 4465

#1975
tp_mean_1975 <- 6
tp_lwr_1975 <- 3
tp_upr_1975 <- 10
kg_P_1975 <- tp_mean_1975*(10^(-9))*(vol*1000)
kg_P_1975_lwr <- tp_lwr_1975*(10^(-9))*(vol*1000)
kg_P_1975_upr <- tp_upr_1975*(10^(-9))*(vol*1000)

#2007
tp_mean_2007 <- ((kg_P_1975+(32*surplus))*(10^9))/(vol*1000)
tp_lwr_2007 <- ((kg_P_1975_lwr+(32*surplus))*(10^9))/(vol*1000)
tp_upr_2007 <- ((kg_P_1975_upr+(32*surplus))*(10^9))/(vol*1000)
kg_P_2007 <- tp_mean_2007*(10^(-9))*(vol*1000)
kg_P_2007_lwr <- tp_lwr_2007*(10^(-9))*(vol*1000)
kg_P_2007_upr <- tp_upr_2007*(10^(-9))*(vol*1000)

preds_tp_1975 <- data.frame(
  end_date = as.Date(c("1975-10-01", "2007-10-09")),
  mean_tp = c(tp_mean_1975, tp_mean_2007),
  min_tp = c(tp_lwr_1975, tp_lwr_2007),
  max_tp = c(tp_upr_1975, tp_upr_2007), 
  kg_P_est = c(kg_P_1975, kg_P_2007), 
  kg_P_est_lwr =  c(kg_P_1975_lwr, kg_P_2007_lwr),
  kg_P_est_upr =  c(kg_P_1975_upr, kg_P_2007_upr))
  
# Add the dates needed to plot
end_dates <- nuts[-c(1), 1]

new_rows <- tibble::tibble(end_date = end_dates)

preds_tp_1975 <- dplyr::bind_rows(preds_tp_1975, new_rows)

# Predict mass and concentration at each

for(i in 3:nrow(preds_tp_1975)){
  preds_tp_1975$kg_P_est[i] = preds_tp_1975$kg_P_est[i-1] + (surplus/52)
  
  preds_tp_1975$kg_P_est_lwr[i] = preds_tp_1975$kg_P_est_lwr[i-1] + (surplus/52)
  
  preds_tp_1975$kg_P_est_upr[i] = preds_tp_1975$kg_P_est_upr[i-1] + (surplus/52)
  
}

vol_col1 <- data.frame(vol = rep(vol, times = 1))

vol_col  <- rbind(vol_col1,
                  data.frame(vol = nuts[[75]]))



preds_tp_1975 <- preds_tp_1975 %>%
  mutate(vol_col) %>%
  mutate(mean_tp = (kg_P_est*(10^9))/(vol*1000), 
         min_tp = (kg_P_est_lwr*(10^9))/(vol*1000), 
         max_tp = (kg_P_est_upr*(10^9))/(vol*1000))


# TN
surplus <- 75680

#1975
tn_mean_1975 <- 369.5
tn_lwr_1975 <- 300
tn_upr_1975 <- 460
kg_N_1975 <- tn_mean_1975*(10^(-9))*(vol*1000)
kg_N_1975_lwr <- tn_lwr_1975*(10^(-9))*(vol*1000)
kg_N_1975_upr <- tn_upr_1975*(10^(-9))*(vol*1000)

#2007
tn_mean_2007 <- ((kg_N_1975+(32*surplus))*(10^9))/(vol*1000)
tn_lwr_2007 <- ((kg_N_1975_lwr+(32*surplus))*(10^9))/(vol*1000)
tn_upr_2007 <- ((kg_N_1975_upr+(32*surplus))*(10^9))/(vol*1000)
kg_N_2007 <- tn_mean_2007*(10^(-9))*(vol*1000)
kg_N_2007_lwr <- tn_lwr_2007*(10^(-9))*(vol*1000)
kg_N_2007_upr <- tn_upr_2007*(10^(-9))*(vol*1000)

preds_tn_1975 <- data.frame(
  end_date = as.Date(c("1975-10-01", "2007-10-09")),
  mean_tn = c(tn_mean_1975, tn_mean_2007),
  min_tn = c(tn_lwr_1975, tn_lwr_2007),
  max_tn = c(tn_upr_1975, tn_upr_2007), 
  kg_N_est = c(kg_N_1975, kg_N_2007), 
  kg_N_est_lwr =  c(kg_N_1975_lwr, kg_N_2007_lwr),
  kg_N_est_upr =  c(kg_N_1975_upr, kg_N_2007_upr))

# Add the dates needed to plot

preds_tn_1975 <- dplyr::bind_rows(preds_tn_1975, new_rows)

# Predict mass and concentration at each

for(i in 3:nrow(preds_tn_1975)){
  preds_tn_1975$kg_N_est[i] = preds_tn_1975$kg_N_est[i-1] + (surplus/52)
  
  preds_tn_1975$kg_N_est_lwr[i] = preds_tn_1975$kg_N_est_lwr[i-1] + (surplus/52)
  
  preds_tn_1975$kg_N_est_upr[i] = preds_tn_1975$kg_N_est_upr[i-1] + (surplus/52)
  
}

preds_tn_1975 <- preds_tn_1975 %>%
  mutate(vol_col) %>%
  mutate(mean_tn = (kg_N_est*(10^9))/(vol*1000), 
         min_tn = (kg_N_est_lwr*(10^9))/(vol*1000), 
         max_tn = (kg_N_est_upr*(10^9))/(vol*1000))


# Generate plot----

x_breaks <- c(
  as.Date("1975-10-01"),
  seq(as.Date("2007-10-01"), as.Date("2024-10-01"), by = "1 year")
)

x_labels <- c(
  "Oct-1975",
  format(seq(as.Date("2007-10-01"), as.Date("2024-10-01"), by = "1 year"), "%b-%Y")
)

panel_theme <- theme_classic() +
  theme(
    plot.title   = element_text(size = 12),
    axis.title.y = element_text(size = 7),
    axis.text.y  = element_text(size = 6),
    axis.title.x =  element_text(size = 7),
    axis.text.x.top  = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.line.x.top  = element_blank()
  )


# Plot predictions + run the fire simulation Lake concentration summary in fires.R to plot the data from the TP_summary or TN_summary dataframe

nuts[1, "mean_tp"] <- NA
nuts[1, "mean_tn"] <- NA


P_conc_plot <- ggplot(data = preds_tp_1975, aes(x = end_date)) +
  geom_ribbon(data = preds_tp_1975, aes(ymin = min_tp, ymax = max_tp), fill = "gray70", alpha = 0.7) + 
  geom_line(data = preds_tp_1975, aes(y = mean_tp), color = "gray30", linewidth =  0.25) +
  
  geom_ribbon(data = nuts, aes(ymin = TP_conc_est_lwr, ymax = TP_conc_est_upr), fill = "peachpuff1", alpha = 0.7) +
  geom_line(data = nuts, aes(y = TP_conc_est), color = "salmon1", linewidth = 0.5) +
  
  geom_ribbon(data = TP_summary, aes(x = date, ymin = TP_conc_2.5, ymax = TP_conc_97.5), fill = "darkred", alpha = 0.5) +
  geom_ribbon(data = TP_summary, aes(x = date, ymin = TP_conc_min, ymax = TP_conc_max), fill = "darkred", alpha = 0.2) + 
  geom_line(data = TP_summary, aes(x = date, y = TP_conc_mean), color = "darkred", linewidth = 0.5)  +
  
  geom_errorbar(data = nuts, aes(ymin = min_tp, ymax = max_tp), width = 0.1, color = "black") +   
  geom_point(data = nuts, aes(y = mean_tp), color = "black", size = 1) +
  
  labs(title = "A", x = "", y = expression("TP Concentration ("*mu * "g-P L"^{-1} * ")")) +
  scale_x_date(limits = as.Date(c("1975-10-01", "2023-10-15")), breaks = x_breaks, labels = x_labels, expand = c(0, 0)) +  
  scale_x_break(breaks = as.Date(c("2005-10-01", "2007-10-16")), scales = 30) + 
  scale_y_log10(limits = c(0.5, 250), breaks = c(1, 10, 20, 50, 100, 250), labels = scales::label_number()) + 
  panel_theme +
  theme(axis.text.x  = element_blank()) +   theme(legend.position = "horizontal")


P_conc_plot


P_conc_plot <- ggplot(data = preds_tp_1975, aes(x = end_date)) +
  
  # 1975
  geom_ribbon(
    data = preds_tp_1975,
    aes(ymin = min_tp, ymax = max_tp, fill = "Range (1975)"),
    alpha = 0.7
  ) +
  geom_line(
    data = preds_tp_1975,
    aes(y = mean_tp, color = "Best Approximation (1975)"),
    linewidth = 0.35,
    lineend = "round"
  ) +
  
  # Updated
  geom_ribbon(
    data = nuts,
    aes(x = end_date, ymin = TP_conc_est_lwr, ymax = TP_conc_est_upr, fill = "Range"),
    alpha = 0.7
  ) +
  geom_line(
    data = nuts,
    aes(x = end_date, y = TP_conc_est, color = "Best Approximation"),
    linewidth = 0.6,
    lineend = "round"
  ) +
  
  # Posterior (two ribbons but ONE legend entry)
  geom_ribbon(
    data = TP_summary,
    aes(x = date, ymin = TP_conc_min, ymax = TP_conc_max, fill = "Range (fire)"),
    alpha = 0.5
  ) +
  geom_line(
    data = TP_summary,
    aes(x = date, y = TP_conc_mean, color = "Mean (fire)"),
    linewidth = 0.7,
    lineend = "round"
  ) +
  
  # Observations (no legend)
  geom_errorbar(
    data = nuts,
    aes(x = end_date, ymin = min_tp, ymax = max_tp),
    width = 0.1,
    color = "black",
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = nuts,
    aes(x = end_date, y = mean_tp),
    color = "black",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  labs(
    title = "A",
    x = "",
    y = expression("TP Concentration ("*mu * "g-P L"^{-1} * ")")
  ) +
  scale_x_date(
    limits = as.Date(c("1975-10-01", "2023-10-15")),
    breaks = x_breaks,
    labels = x_labels,
    expand = c(0, 0)
  ) +
  scale_x_break(breaks = as.Date(c("2005-10-01", "2007-10-16")), scales = 30) +
  scale_y_log10(
    limits = c(0.5, 250),
    breaks = c(1, 10, 20, 50, 100, 250),
    labels = scales::label_number()
  ) +
  # Ribbons legend (fill)
  scale_fill_manual(
    name = "Ribbons",
    values = c(
      "Range (1975)" = "gray70",
      "Range" = "peachpuff1",
      "Range (fire)" = "darkred"
    ),
    breaks = c("Range (1975)", "Range", "Range (fire)")
  ) +
  # Lines legend (color)
  scale_color_manual(
    name = "Lines",
    values = c(
      "Best Approximation (1975)" = "gray30",
      "Best Approximation" = "salmon1",
      "Mean (fire)" = "darkred"
    ),
    breaks = c("Best Approximation (1975)", "Best Approximation", "Mean (fire)")
  ) +
  guides(
    fill  = guide_legend(order = 1),
    color = guide_legend(order = 2)
  )+
  panel_theme +
  theme(
    axis.text.x = element_blank(),
    legend.text      = element_text(size = 4),
    legend.title     = element_blank(),
    legend.key.size  = unit(0.5, "cm")
  ) + theme(
    legend.position = "bottom",
    #legend.box = "vertical",   # stack fill + color legends
    legend.direction = "horizontal"
  ) + theme(
    axis.title.y = element_text(
      hjust = 0.75   # increase to move UP
    )
  )

P_conc_plot





N_conc_plot <- ggplot(preds_tn_1975, aes(x = end_date)) +
  geom_ribbon(data = preds_tn_1975, aes(ymin = min_tn, ymax = max_tn), fill = "gray70", alpha = 0.7) + 
  geom_line(data = preds_tn_1975, aes(y = mean_tn), color = "gray30", linewidth = 0.25) +
  
  geom_ribbon(data = nuts, aes(ymin = TN_conc_est_lwr, ymax = TN_conc_est_upr), fill = "#cbc9e2") +
  geom_line(data = nuts, aes(y = TN_conc_est), color = "mediumpurple4", linewidth = 0.5) +
  
  geom_ribbon(data = TN_summary, aes(x = date, ymin = TN_conc_2.5, ymax = TN_conc_97.5), fill = "darkred", alpha = 0.5) +
  geom_ribbon(data = TN_summary, aes(x = date, ymin = TN_conc_min, ymax = TN_conc_max), fill = "darkred", alpha = 0.2) + 
  geom_line(data = TN_summary, aes(x = date, y = TN_conc_mean), color = "darkred", linewidth = 0.5)  +
  
  geom_errorbar(data = nuts, aes(ymin = min_tn, ymax = max_tn), width = 0.1, color = "black") +   
  geom_point(data = nuts, aes(y = mean_tn), color = "black", size = 1) +
  
  labs(title = "B", x = "Date", y = expression("TN Concentration ("*mu * "g-N L"^{-1} * ")")) +
  
  scale_x_date(limits = as.Date(c("1975-10-01", "2023-10-15")), breaks = x_breaks, labels = x_labels, expand = c(0, 0)) +
  scale_y_log10(limits = c(100, 9000), breaks = c(100, 300, 1000, 3000, 9000), labels = scales::label_comma()) +
  scale_x_break(breaks = as.Date(c("2005-10-01", "2007-10-16")), scales = 30) +
  panel_theme +
  theme(
    axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.25), axis.text.y = element_text(size = 7)) + theme(legend.position = "none") 


N_conc_plot

N_conc_plot <- ggplot(data = preds_tn_1975, aes(x = end_date)) +
  
  # 1975
  geom_ribbon(
    data = preds_tn_1975,
    aes(ymin = min_tn, ymax = max_tn, fill = "Range (1975)"),
    alpha = 0.7
  ) +
  geom_line(
    data = preds_tn_1975,
    aes(y = mean_tn, color = "Best Approximation (1975)"),
    linewidth = 0.35,
    lineend = "round"
  ) +
  
  # Updated
  geom_ribbon(
    data = nuts,
    aes(x = end_date, ymin = TN_conc_est_lwr, ymax = TN_conc_est_upr, fill = "Range"),
    alpha = 0.7
  ) +
  geom_line(
    data = nuts,
    aes(x = end_date, y = TN_conc_est, color = "Best Approximation"),
    linewidth = 0.6,
    lineend = "round"
  ) +
  
  # Posterior (two ribbons but ONE legend entry)
  geom_ribbon(
    data = TN_summary,
    aes(x = date, ymin = TN_conc_min, ymax = TN_conc_max, fill = "Range (fire)"),
    alpha = 0.5
  ) +
  geom_line(
    data = TN_summary,
    aes(x = date, y = TN_conc_mean, color = "Mean (fire)"),
    linewidth = 0.7,
    lineend = "round"
  ) +
  
  # Observations (no legend)
  geom_errorbar(
    data = nuts,
    aes(x = end_date, ymin = min_tn, ymax = max_tn),
    width = 0.1,
    color = "black",
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = nuts,
    aes(x = end_date, y = mean_tn),
    color = "black",
    size = 1,
    inherit.aes = FALSE
  ) +
  
  labs(
    title = "B",
    x = "Date",
    y = expression("TN Concentration ("*mu * "g-N L"^{-1} * ")")
  ) +
  scale_x_date(
    limits = as.Date(c("1975-10-01", "2023-10-15")),
    breaks = x_breaks,
    labels = x_labels,
    expand = c(0, 0)
  ) +
  scale_x_break(breaks = as.Date(c("2005-10-01", "2007-10-16")), scales = 30) +
  scale_y_log10(limits = c(100, 9000), breaks = c(100, 300, 1000, 3000, 9000), labels = scales::label_comma()) +
  # Ribbons legend (fill)
  scale_fill_manual(
    name = "Ribbons",
    values = c(
      "Range (1975)" = "gray70",
      "Range" = "#cbc9e2",
      "Range (fire)" = "darkred"
    ),
    breaks = c("Range (1975)", "Range", "Range (fire)")
  ) +
  # Lines legend (color)
  scale_color_manual(
    name = "Lines",
    values = c(
      "Best Approximation (1975)" = "gray30",
      "Best Approximation" = "mediumpurple4",
      "Mean (fire)" = "darkred"
    ),
    breaks = c("Best Approximation (1975)", "Best Approximation", "Mean (fire)")
  ) +
  guides(
    fill  = guide_legend(order = 1),
    color = guide_legend(order = 2)
  )+
  panel_theme +
  theme(
    legend.text      = element_text(size = 12),
    legend.title     = element_blank(),
    legend.key.size  = unit(0.5, "cm")
  ) + theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.25), axis.text.y = element_text(size = 7)) + theme(
    legend.position = "bottom",
 #   legend.box = "vertical",   # stack fill + color legends
    legend.direction = "horizontal"
  ) + theme(
    axis.title.y = element_text(
      hjust = 0.85   # increase to move UP
    )
  )

N_conc_plot




both <- P_conc_plot/N_conc_plot



ggsave("Predictions_Fire_Simulations.png", both, "png", width = 5, height = 6)


# without 1975

P_conc_plot <- ggplot(data = nuts, aes(x = end_date)) +
  geom_ribbon(data = nuts, aes(ymin = TP_conc_est_lwr, ymax = TP_conc_est_upr), fill = "peachpuff1", alpha = 0.7) +
  geom_line(data = nuts, aes(y = TP_conc_est), color = "salmon1", linewidth = 1) +
  
  geom_ribbon(data = TP_summary, aes(x = date, ymin = TP_conc_2.5, ymax = TP_conc_97.5), fill = "darkred", alpha = 0.5) +
  geom_ribbon(data = TP_summary, aes(x = date, ymin = TP_conc_min, ymax = TP_conc_max), fill = "darkred", alpha = 0.2) + 
  geom_line(data = TP_summary, aes(x = date, y = TP_conc_mean), color = "darkred", linewidth = 0.5)  +
  
  geom_errorbar(data = nuts, aes(ymin = min_tp, ymax = max_tp), width = 1, color = "black") +   
  geom_point(data = nuts, aes(y = mean_tp), color = "black", size = 3) +
  
  labs(title = "A", x = "", y = expression("TP Concentration ("*mu * "g-P L"^{-1} * ")")) +
  scale_y_log10(limits = c(0.5, 250), breaks = c(1, 10, 20, 50, 100, 250), labels = scales::label_number()) + 
  theme(axis.text.x  = element_blank()) +   theme(legend.position = "horizontal") + theme_classic()  + scale_x_datetime(
    date_breaks = "1 year",
    date_labels = "%Y"
  )


P_conc_plot


N_conc_plot <- ggplot(nuts, aes(x = end_date)) +
  geom_ribbon(data = nuts, aes(ymin = TN_conc_est_lwr, ymax = TN_conc_est_upr), fill = "#cbc9e2") +
  geom_line(data = nuts, aes(y = TN_conc_est), color = "mediumpurple4", linewidth = 1) +
  
  geom_ribbon(data = TN_summary, aes(x = date, ymin = TN_conc_2.5, ymax = TN_conc_97.5), fill = "darkred", alpha = 0.5) +
  geom_ribbon(data = TN_summary, aes(x = date, ymin = TN_conc_min, ymax = TN_conc_max), fill = "darkred", alpha = 0.2) + 
  geom_line(data = TN_summary, aes(x = date, y = TN_conc_mean), color = "darkred", linewidth = 0.5)  +
  
  geom_errorbar(data = nuts, aes(ymin = min_tn, ymax = max_tn), width = 1, color = "black") +   
  geom_point(data = nuts, aes(y = mean_tn), color = "black", size = 3) +
  
  labs(title = "B", x = "Date", y = expression("TN Concentration ("*mu * "g-N L"^{-1} * ")")) +
    scale_y_log10(limits = c(100, 9000), breaks = c(100, 300, 1000, 3000, 9000), labels = scales::label_comma()) +
  theme(
    axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.25), axis.text.y = element_text(size = 7)) + theme(legend.position = "none") + theme_classic()  + scale_x_datetime(
      date_breaks = "1 year",
      date_labels = "%Y"
    )


N_conc_plot



both <- P_conc_plot/N_conc_plot

ggsave("Predictions.png", both, "png", width = 7, height = 6)


# 2. RMSE calculations----

tp_sub <- subset(nuts, !is.na(mean_tp)  & end_date > "2008-10-01")

tp_sub <- tp_sub %>%
  select(end_date, mean_tp, TP_conc_est, TP_conc_est_lwr, TP_conc_est_upr) %>%
  rename(Date = end_date,
         Observed_TP = mean_tp,
         Predicted_TP_lwr = TP_conc_est_lwr,
         Predicted_TP   = TP_conc_est,
         Predicted_TP_upr = TP_conc_est_upr) %>%
  mutate(
    Observed_TP      = round(Observed_TP, 2),
    Predicted_TP_lwr = round(Predicted_TP_lwr, 2),
    Predicted_TP     = round(Predicted_TP, 2),
    Predicted_TP_upr = round(Predicted_TP_upr, 2)
  )

tp_sub$Date <- as.character(tp_sub$Date)   # make Date a character

tp_obs_mean <- mean(tp_sub$Observed_TP, na.rm = TRUE)
tp_preds_mean <- mean(tp_sub$Predicted_TP, na.rm = TRUE)
tp_preds_lwr <- mean(tp_sub$Predicted_TP_lwr, na.rm = TRUE)
tp_preds_upr <- mean(tp_sub$Predicted_TP_upr, na.rm = TRUE)


col1 <- "Mean"
col2 <- round(mean(tp_sub$Observed_TP, na.rm = TRUE), 2)
col3 <- NA_real_
col4 <- NA_real_
col5 <- NA_real_

mean.row <- data.frame(
  Date = col1,
  Observed_TP = col2,
  Predicted_TP = col3,
  Predicted_TP_lwr = col4,
  Predicted_TP_upr = col5,
  stringsAsFactors = FALSE
)

col1 <- "RMSE"
col2 <- NA_real_
col3 <- round(sqrt(mean((tp_sub$Predicted_TP - tp_sub$Observed_TP)^2, na.rm = TRUE)), 2)
col4 <- NA_real_
col5 <- NA_real_

rmse.row <- data.frame(
  Date = col1,
  Observed_TP = col2,
  Predicted_TP = col3,
  Predicted_TP_lwr = col4,
  Predicted_TP_upr = col5,
  stringsAsFactors = FALSE
)

  
TP_RMSE_table <- rbind(tp_sub, mean.row, rmse.row)
  
tn_sub <- subset(nuts, !is.na(mean_tn) & end_date > "2008-10-01")

tn_sub <- tn_sub %>%
  select(end_date, mean_tn, TN_conc_est, TN_conc_est_lwr,  TN_conc_est_upr)%>%
  rename(Date = "end_date", Observed_TN = "mean_tn", Predicted_TN_lwr = "TN_conc_est_lwr", Predicted_TN = "TN_conc_est",  Predicted_TN_upr = "TN_conc_est_upr")%>%
  mutate(
    Observed_TN      = round(Observed_TN, 2),
    Predicted_TN_lwr = round(Predicted_TN_lwr, 2),
    Predicted_TN     = round(Predicted_TN, 2),
    Predicted_TN_upr = round(Predicted_TN_upr, 2)
  )
         

tn_sub$Date <- as.character(tn_sub$Date)   # make Date a character

tn_obs_mean <- mean(tn_sub$Observed_TN, na.rm = TRUE)
tn_preds_mean <- mean(tn_sub$Predicted_TN, na.rm = TRUE)
tn_preds_lwr <- mean(tn_sub$Predicted_TN_lwr, na.rm = TRUE)
tn_preds_upr <- mean(tn_sub$Predicted_TN_upr, na.rm = TRUE)

col1 <- "Mean"
col2 <- round(mean(tn_sub$Observed_TN, na.rm = TRUE), 2)
col3 <- NA_real_
col4 <- NA_real_
col5 <- NA_real_

mean.row <- data.frame(
  Date = col1,
  Observed_TN = col2,
  Predicted_TN = col3,
  Predicted_TN_lwr = col4,
  Predicted_TN_upr = col5,
  stringsAsFactors = FALSE
)

col1 <- "RMSE"
col2 <- NA_real_
col3 <- round(sqrt(mean((tn_sub$Predicted_TN     - tn_sub$Observed_TN)^2, na.rm = TRUE)), 2)
col4 <- NA_real_
col5 <- NA_real_

rmse.row <- data.frame(
  Date = col1,
  Observed_TN = col2,
  Predicted_TN = col3,
  Predicted_TN_lwr = col4,
  Predicted_TN_upr = col5,
  stringsAsFactors = FALSE
)


TN_RMSE_table <- rbind(tn_sub, mean.row, rmse.row)

colnames(TP_RMSE_table) <- c(
  "Date",
  "Observed", "Predicted", "Predicted Minimum", "Predicted Maximum")


colnames(TN_RMSE_table) <- c(
  "Date",
  "Observed", "Predicted", "Predicted Minimum", "Predicted Maximum")


# Create LaTeX table
xtable_table_tp <- xtable(TP_RMSE_table)
xtable_table_tn <- xtable(TN_RMSE_table)


# Print to .tex file
print(xtable_table_tp, type = "latex", file = "C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\3_products\\TP_RMSE_table.tex", include.rownames = FALSE)

print(xtable_table_tn, type = "latex", file = "C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\3_products\\TN_RMSE_table.tex", include.rownames = FALSE)

# For 1975----

tp_sub_preds <- preds_tp_1975 %>%
  inner_join(nuts, by = "end_date")

tp_sub_preds <- subset(tp_sub_preds, !is.na(mean_tp.y)  & end_date > "2008-10-01")

tp_sub_preds <- tp_sub_preds  %>%
  select(end_date, mean_tp.x, mean_tp.y) %>%
  rename(Date = end_date,
         Observed_TP = mean_tp.y,
         Predicted_TP   = mean_tp.x) %>%
  mutate(
    Observed_TP      = round(Observed_TP, 2),
    Predicted_TP     = round(Predicted_TP, 2))

mean(tp_sub_preds$Observed_TP, na.rm = TRUE)
mean(tp_sub_preds$Predicted_TP, na.rm = TRUE)

round(sqrt(mean((tp_sub_preds$Predicted_TP     - tp_sub_preds$Observed_TP)^2, na.rm = TRUE)), 2)

tn_sub_preds <- preds_tn_1975 %>%
  inner_join(nuts, by = "end_date")

tn_sub_preds <- subset(tn_sub_preds, !is.na(mean_tn.y)  & end_date > "2008-10-01")

tn_sub_preds <- tn_sub_preds  %>%
  select(end_date, mean_tn.x, mean_tn.y) %>%
  rename(Date = end_date,
         Observed_TN = mean_tn.y,
         Predicted_TN   = mean_tn.x) %>%
  mutate(
    Observed_TN      = round(Observed_TN, 2),
    Predicted_TN     = round(Predicted_TN, 2))

mean(tn_sub_preds$Observed_TN, na.rm = TRUE)
mean(tn_sub_preds$Predicted_TN, na.rm = TRUE)

round(sqrt(mean((tn_sub_preds$Predicted_TN     - tn_sub_preds$Observed_TN)^2, na.rm = TRUE)), 2)

#3. Annual summary----

# Assign water year based on end_date
water_year <- nuts %>%
  mutate(
    end_date = as.Date(end_date),  # Ensure end_date is a Date
    water_year = year(end_date) + if_else(month(end_date) >= 10, 1, 0)
  ) %>%
  filter(water_year <= 2023)%>%
  mutate(lmc_P = (lag(TP_conc_est)*10^(-9))*(lmc_m3*1000), 
         lmc_N = (lag(TN_conc_est)*10^(-9))*(lmc_m3*1000), 
         lmc_P_lwr = (lag(TP_conc_est_lwr)*10^(-9))*(lmc_m3_lwr*1000), 
         lmc_N_lwr = (lag(TN_conc_est_lwr)*10^(-9))*(lmc_m3_lwr*1000),          
         lmc_P_upr = (lag(TP_conc_est_upr)*10^(-9))*(lmc_m3_upr*1000), 
         lmc_N_upr = (lag(TN_conc_est_upr)*10^(-9))*(lmc_m3_upr*1000))

# Summarize by water year for P----

summary_water_year_P <- water_year %>%
  group_by(water_year) %>%
  summarize(
    across(c(
      umc_P_lwr, sny_P_lwr, spr_P_lwr, fish_P_lwr,
      umc_P, sny_P, spr_P, fish_P,
      umc_P_upr, sny_P_upr, spr_P_upr, fish_P_upr, 
      TP_dry_kg, TP_wet_kg, H_P_lwr, H_P, H_P_upr, S_P_lwr, S_P, S_P_upr,
      lmc_P_lwr, lmc_P, lmc_P_upr
    ), ~ sum(.x, na.rm = TRUE))
  ) %>%
  mutate(
    T_mean = rowSums(across(c(umc_P, sny_P, spr_P, fish_P)), na.rm = TRUE),
    T_lwr  = rowSums(across(c(umc_P_lwr, sny_P_lwr, spr_P_lwr, fish_P_lwr)), na.rm = TRUE),
    T_upr  = rowSums(across(c(umc_P_upr, sny_P_upr, spr_P_upr, fish_P_upr)), na.rm = TRUE),
    
    H_lwr = H_P_lwr,
    H  = H_P,
    H_upr  = H_P_upr,
    
    D = TP_dry_kg,
    W = TP_wet_kg,
    S_lwr = -S_P_lwr,
    S  = -S_P,
    S_upr  = -S_P_upr,
    
    O_mean = -lmc_P,
    O_lwr  = -lmc_P_lwr,
    O_upr  = -lmc_P_upr
  ) %>%
  select(water_year, T_mean, T_lwr, T_upr,
         O_mean, O_lwr, O_upr,
         H_lwr, H, H_upr, S_lwr, S, S_upr, 
         D)


new_row_P <- data.frame(
  water_year = 1975,
  T_mean = 8265,
  T_lwr = NA,
  T_upr = NA,
  O_mean = -5035,
  O_lwr = NA,
  O_upr = NA,
  H_lwr = NA,
  H = 70,
  H_upr = NA,
  S_lwr = NA,
  S = NA,
  S_upr = NA,
  D = 570
)

summary_water_year_P <- rbind(summary_water_year_P, new_row_P)

fill_colors <- c(
  "Deposition" = "#DD571C",
  "Septic" = "brown4",
  "Burial" = "tan",
  "Outflow" = "darkblue",
  "Tributaries" = "turquoise4"
)

border_colors <- c(
  "Deposition" = "#DD571C",
  "Septic" = "brown4",
  "Burial" = "tan",
  "Outflow" = "darkblue",
  "Tributaries" = "turquoise4"
)

errorbar_data_P <- summary_water_year_P %>%
  transmute(
    water_year = as.factor(water_year),
    
    # Mean, lower, upper for tributaries
    Category = "Tributaries",
    Mean = T_mean,
    Min = T_lwr,
    Max = T_upr
  ) %>%
  bind_rows(
    summary_water_year_P%>%
      transmute(water_year = as.factor(water_year),
                Category = "Outflow", Mean = O_mean, Min = O_lwr, Max = O_upr),
    summary_water_year_P %>%
      transmute(water_year = as.factor(water_year),
                Category = "Septic", Mean = H, Min = H_lwr, Max = H_upr),
    summary_water_year_P %>%
      transmute(water_year = as.factor(water_year),
                Category = "Burial", Mean = S, Min = S_lwr, Max = S_upr),
    summary_water_year_P %>%
      transmute(water_year = as.factor(water_year),
                Category = "Dry Deposition", Mean = D, Min = NA, Max = NA))

P_annual_plot <- ggplot(errorbar_data_P, aes(x = water_year, y = Mean, fill = Category, color = Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +  # no hardcoded color here!
  geom_errorbar(aes(ymin = Min, ymax = Max),
                position = position_dodge(width = 0.75),
                width = 0.25,
                na.rm = TRUE,
                color = "grey20") +
  scale_fill_manual(values = fill_colors) +
  scale_color_manual(values = border_colors) +
  guides(fill = guide_legend(title = NULL, nrow = 1),
         color = guide_legend(title = NULL, nrow = 1)) +  # <-- removes both legend titles 
  labs(title = "A", x = "", y = bquote("Flux ("*kg-P~yr^-1*")")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8), 
    legend.position = "bottom") + 
  scale_y_continuous(
    labels = scales::comma,
    limits = c(-10000, 14000),
    breaks = seq(-10000, 14000, by = 2000)
  )

P_annual_plot

# Summarize by water year for N----
summary_water_year_N <- water_year %>%
  group_by(water_year) %>%
  summarize(
    across(c(
      umc_N_lwr, sny_N_lwr, spr_N_lwr, fish_N_lwr,
      umc_N, sny_N, spr_N, fish_N,
      umc_N_upr, sny_N_upr, spr_N_upr, fish_N_upr,
      TN_wet_kg, TN_dry_kg, H_N_lwr, H_N, H_N_upr, S_N_lwr, S_N, S_N_upr,
      lmc_N_lwr, lmc_N, lmc_N_upr
    ), ~ sum(.x, na.rm = TRUE))
  ) %>%
  mutate(
    T_mean = rowSums(across(c(umc_N, sny_N, spr_N, fish_N)), na.rm = TRUE),
    T_lwr  = rowSums(across(c(umc_N_lwr, sny_N_lwr, spr_N_lwr, fish_N_lwr)), na.rm = TRUE),
    T_upr  = rowSums(across(c(umc_N_upr, sny_N_upr, spr_N_upr, fish_N_upr)), na.rm = TRUE),
    
    H_lwr = H_N_lwr,
    H  = H_N,
    H_upr  = H_N_upr,
    
    D = TN_dry_kg,
    W = TN_wet_kg,
    
    S_lwr = -S_N_lwr,
    S  = -S_N,
    S_upr  = -S_N_upr,
    
    O_mean = -lmc_N,
    O_lwr  = -lmc_N_lwr,
    O_upr  = -lmc_N_upr
  ) %>%
  select(water_year, T_mean, T_lwr, T_upr,
         O_mean, O_lwr, O_upr,
         H_lwr, H, H_upr, S_lwr, S, S_upr, 
         D, W)


new_row_N <- data.frame(
  water_year = 1975,
  T_mean = 381285,
  T_lwr = NA,
  T_upr = NA,
  O_mean = -364395,
  O_lwr = NA,
  O_upr = NA,
  H_lwr = NA,
  H = 2635,
  H_upr = NA,
  S_lwr = NA,
  S = NA,
  S_upr = NA,
  D = NA, 
  W = 35225
)

summary_water_year_N <- rbind(summary_water_year_N, new_row_N)

fill_colors <- c(
  "Dry Deposition" = "#DD571C",
  "Septic" = "brown4",
  "Burial" = "tan",
  "Outflow" = "darkblue",
  "Tributaries" = "turquoise4",
  "Wet Deposition" = "black"
)

border_colors <- c(
  "Dry Deposition" = "#DD571C",
  "Septic" = "brown4",
  "Burial" = "tan",
  "Outflow" = "darkblue",
  "Tributaries" = "turquoise4",
  "Wet Deposition" = "black"
)

# Reshape data to get one row per category and year
errorbar_data_N <- summary_water_year_N %>%
  transmute(
    water_year = as.factor(water_year),
    
    # Mean, lower, upper for tributaries
    Category = "Tributaries",
    Mean = T_mean,
    Min = T_lwr,
    Max = T_upr
  ) %>%
  bind_rows(
    summary_water_year_N %>%
      transmute(water_year = as.factor(water_year),
                Category = "Outflow", Mean = O_mean, Min = O_lwr, Max = O_upr),
    summary_water_year_N %>%
      transmute(water_year = as.factor(water_year),
                Category = "Septic", Mean = H, Min = H_lwr, Max = H_upr),
    summary_water_year_N %>%
      transmute(water_year = as.factor(water_year),
                Category = "Burial", Mean = S, Min = S_lwr, Max = S_upr),
    summary_water_year_N %>%
      transmute(water_year = as.factor(water_year),
                Category = "Dry Deposition", Mean = D, Min = NA, Max = NA),
    summary_water_year_N %>%
      transmute(water_year = as.factor(water_year),
                Category = "Wet Deposition", Mean = W, Min = NA, Max = NA)
  )

N_annual_plot <- ggplot(errorbar_data_N, aes(x = water_year, y = Mean, fill = Category, color = Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  geom_errorbar(aes(ymin = Min, ymax = Max),
                position = position_dodge(width = 0.75),
                width = 0.25,
                na.rm = TRUE,
                color = "grey20") +  # This line controls errorbar color globally
  scale_fill_manual(values = fill_colors) +
  scale_color_manual(values = border_colors) +
  guides(fill = guide_legend(title = NULL, nrow = 1),
         color = guide_legend(title = NULL, nrow = 1)) +  # <-- removes both legend titles
  labs(title = "B", x = "", y = bquote("Flux ("*kg-N~yr^-1*")")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "bottom") +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(-500000, 403000),
    breaks = seq(-500000, 403000, by = 100000)) + 
  theme(axis.line.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank())

N_annual_plot

loading_summary <- P_annual_plot/N_annual_plot

ggsave("loading_summary.png", loading_summary, "png", width = 5, height = 6)

# Compare to 1975----

summary_table_P <- water_year %>%
  group_by(water_year) %>%
  summarize(
    across(c(
      umc_P_lwr, sny_P_lwr, spr_P_lwr, fish_P_lwr,
      umc_P, sny_P, spr_P, fish_P,
      umc_P_upr, sny_P_upr, spr_P_upr, fish_P_upr, 
      TP_dry_kg, TP_wet_kg, H_P_lwr, H_P, H_P_upr, S_P_lwr, S_P, S_P_upr,
      lmc_P_lwr, lmc_P, lmc_P_upr), ~ sum(.x, na.rm = TRUE)))


summary_table_P <- data.frame(
  umc_P       = round(mean(summary_table_P$umc_P, na.rm = TRUE)),
  umc_P_lwr   = round(mean(summary_table_P$umc_P_lwr, na.rm = TRUE)),
  umc_P_upr   = round(mean(summary_table_P$umc_P_upr, na.rm = TRUE)),
  
  
  sny_P       = round(mean(summary_table_P$sny_P, na.rm = TRUE)),
  sny_P_lwr   = round(mean(summary_table_P$sny_P_lwr, na.rm = TRUE)),
  sny_P_upr   = round(mean(summary_table_P$sny_P_upr, na.rm = TRUE)),
  
  spr_P       = round(mean(summary_table_P$spr_P, na.rm = TRUE)),
  spr_P_lwr   = round(mean(summary_table_P$spr_P_lwr, na.rm = TRUE)),
  spr_P_upr   = round(mean(summary_table_P$spr_P_upr, na.rm = TRUE)),
  
  fish_P      = round(mean(summary_table_P$fish_P, na.rm = TRUE)),
  fish_P_lwr  = round(mean(summary_table_P$fish_P_lwr, na.rm = TRUE)),
  fish_P_upr  = round(mean(summary_table_P$fish_P_upr, na.rm = TRUE)),
  
  H_P_lwr     = round(mean(summary_table_P$H_P_lwr, na.rm = TRUE)),
  H_P         = round(mean(summary_table_P$H_P, na.rm = TRUE)),
  H_P_upr     = round(mean(summary_table_P$H_P_upr, na.rm = TRUE)),

  W_P         = round(mean(summary_table_P$TP_wet_kg, na.rm = TRUE)),
  D_P         = round(mean(summary_table_P$TP_dry_kg, na.rm = TRUE)),

  
  lmc_P       = round(mean(summary_table_P$lmc_P, na.rm = TRUE)),
  lmc_P_lwr   = round(mean(summary_table_P$lmc_P_lwr, na.rm = TRUE)),
  lmc_P_upr   = round(mean(summary_table_P$lmc_P_upr, na.rm = TRUE)),
  
  S_P   = round(mean(summary_table_P$S_P, na.rm = TRUE)), 
  S_P_lwr   = round(mean(summary_table_P$S_P_lwr, na.rm = TRUE)), 
  S_P_upr   = round(mean(summary_table_P$S_P_upr, na.rm = TRUE)), 
  E_P = NA)


# Now pivot it to long format
summary_table_P_long <- summary_table_P %>%
  pivot_longer(
    cols = matches("_(P|P_lwr|P_upr)$"),
    names_to = c("Variable", ".value"),
    names_pattern = "^(.*)_(P|P_lwr|P_upr)$"
  ) %>%
  rename(
      `2008-2023 Mean` = P,
      `2008-2023 Minimum` = P_lwr,
      `2008-2023 Maximum` = P_upr
    )

summary_table_P_long <- as.data.frame(summary_table_P_long)

tribs  <- summary_table_P_long[1:4,]

sum(tribs$'2008-2023 Mean')
sum(tribs$'2008-2023 Minimum')
sum(tribs$'2008-2023 Maximum')

dep  <- summary_table_P_long[6:7,]

sum(dep$'2008-2023 Mean')



summary_table_N <- water_year %>%
  group_by(water_year) %>%
  summarize(
    across(c(
      umc_N_lwr, sny_N_lwr, spr_N_lwr, fish_N_lwr,
      umc_N, sny_N, spr_N, fish_N,
      umc_N_upr, sny_N_upr, spr_N_upr, fish_N_upr, 
      TN_dry_kg, TN_wet_kg, H_N_lwr, H_N, H_N_upr, S_N_lwr, S_N, S_N_upr,
      lmc_N_lwr, lmc_N, lmc_N_upr), ~ sum(.x, na.rm = TRUE)))


summary_table_N <- data.frame(
  umc_N       = round(mean(summary_table_N$umc_N, na.rm = TRUE)),
  umc_N_lwr   = round(mean(summary_table_N$umc_N_lwr, na.rm = TRUE)),
  umc_N_upr   = round(mean(summary_table_N$umc_N_upr, na.rm = TRUE)),
  
  sny_N       = round(mean(summary_table_N$sny_N, na.rm = TRUE)),
  sny_N_lwr   = round(mean(summary_table_N$sny_N_lwr, na.rm = TRUE)),
  sny_N_upr   = round(mean(summary_table_N$sny_N_upr, na.rm = TRUE)),
  
  spr_N       = round(mean(summary_table_N$spr_N, na.rm = TRUE)),
  spr_N_lwr   = round(mean(summary_table_N$spr_N_lwr, na.rm = TRUE)),
  spr_N_upr   = round(mean(summary_table_N$spr_N_upr, na.rm = TRUE)),
  
  fish_N      = round(mean(summary_table_N$fish_N, na.rm = TRUE)),
  fish_N_lwr  = round(mean(summary_table_N$fish_N_lwr, na.rm = TRUE)),
  fish_N_upr  = round(mean(summary_table_N$fish_N_upr, na.rm = TRUE)),
  
  H_N_lwr     = round(mean(summary_table_N$H_N_lwr, na.rm = TRUE)),
  H_N         = round(mean(summary_table_N$H_N, na.rm = TRUE)),
  H_N_upr     = round(mean(summary_table_N$H_N_upr, na.rm = TRUE)),
  
  W_N         = round(mean(summary_table_N$TN_wet_kg, na.rm = TRUE)),
  D_N         = round(mean(summary_table_N$TN_dry_kg, na.rm = TRUE)),

  lmc_N       = round(mean(summary_table_N$lmc_N, na.rm = TRUE)),
  lmc_N_lwr   = round(mean(summary_table_N$lmc_N_lwr, na.rm = TRUE)),
  lmc_N_upr   = round(mean(summary_table_N$lmc_N_upr, na.rm = TRUE)),

  S_N         = round(mean(summary_table_N$S_N, na.rm = TRUE)),
  S_N_lwr     = round(mean(summary_table_N$S_N_lwr, na.rm = TRUE)),
  S_N_upr     = round(mean(summary_table_N$S_N_upr, na.rm = TRUE)), 
  E_N = NA)

# Now pivot it to long format
summary_table_N_long <- summary_table_N %>%
  pivot_longer(
    cols = matches("_(N|N_lwr|N_upr)$"),
    names_to = c("Variable", ".value"),
    names_pattern = "^(.*)_(N|N_lwr|N_upr)$"
  ) %>%
  rename(
    `2008-2023 Mean` = N,
    `2008-2023 Minimum` = N_lwr,
    `2008-2023 Maximum` = N_upr
  )

summary_table_N_long <- as.data.frame(summary_table_N_long)

tribs  <- summary_table_N_long[1:4,]

sum(tribs$'2008-2023 Mean')
sum(tribs$'2008-2023 Minimum')
sum(tribs$'2008-2023 Maximum')

dep  <- summary_table_N_long[6:7,]

sum(dep$'2008-2023 Mean')

sum(summary_table_N_long$`2008-2023 Mean`[1:7], na.rm = TRUE) -sum(summary_table_N_long$`2008-2023 Mean`[8:9], na.rm = TRUE)
