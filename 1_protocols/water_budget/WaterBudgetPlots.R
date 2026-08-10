

#0. Load packages----
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(patchwork)
library(ggbreak)

options(scipen = 999)


#1. Read in data----

working_budget <- read.csv(
  file = "C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\WaterBudget.csv",
  header = TRUE,
  sep = ","
)

# 815 lines start date 10-2-2008 through 2023-09-26

# Convert end_date column to Date format
working_budget$end_date <- as.Date(working_budget$end_date)

#2. Residual time series---- 

res_plot <- ggplot(working_budget, aes(x = end_date)) +
  geom_ribbon(aes(ymin = (resid_lwr), ymax = (resid_upr)), fill = "cadetblue", alpha = 0.4) +
  geom_line(data = working_budget, aes(y = (resid)), linewidth = 0.7, color = "cadetblue") +
  labs(x = "Date", title = "") +
  theme_classic() + 
  scale_x_date(
    limits = as.Date(c("2007-10-01", "2023-10-01")),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  )  +
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.25),
    plot.title = element_text(size = 12)
  ) +
  scale_y_continuous(
   limits = c(-1e8, 4e7),
  breaks = pretty_breaks(n = 7), labels = comma) +
  labs(y = expression(epsilon ~ "(" * m^3 * ")")) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) + theme(legend.position = "none")

res_plot

res_plot <- ggplot(working_budget, aes(x = end_date)) +
  
  geom_ribbon(
    aes(ymin = resid_lwr,
        ymax = resid_upr,
        fill = "Range"),
    alpha = 0.4
  ) +
  
  geom_line(
    aes(y = resid,
        color = "Best Approximation"),
    linewidth = 0.7
  ) +
  
  scale_color_manual(
    values = c("Best Approximation" = "cadetblue")
  ) +
  
  scale_fill_manual(
    values = c("Range" = "cadetblue")
  ) +
  
  labs(
    x = "Date",
    y = expression(epsilon ~ "(" * m^3 * ")"),
    color = "",
    fill  = ""
  ) +
  
  theme_classic() +
  labs(x = "Date", title = "") +
  theme_classic() + 
  scale_x_date(
    limits = as.Date(c("2007-10-01", "2023-10-01")),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  )  +
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.25),
    plot.title = element_text(size = 12)
  ) +
  scale_y_continuous(
    limits = c(-1e8, 4e7),
    breaks = pretty_breaks(n = 7), labels = comma) +
  labs(y = expression(epsilon ~ "(" * m^3 * ")")) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) + theme(legend.position="bottom")



res_plot
ggsave("res_plot.png", res_plot, "png", width = 5, height = 3)


#2. Statistics about residuals
working_budget <- working_budget %>%
  mutate(resid_prop = (resid/lake_volume)*100, 
         resid_upr_prop = (resid_upr/lake_volume_upr)*100, 
         resid_lwr_prop = (resid_lwr/lake_volume_lwr)*100)


# number of entries where the proportion is greater than 2.5% of lake volume
sum(working_budget$resid_lwr_prop > -3, na.rm = TRUE)

# Number of weeks where residuals are less than zero 
sum(working_budget$resid_lwr < 0 & working_budget$resid < 0 & working_budget$resid_upr < 0, na.rm = TRUE)

# Weeks with all negative residuals
neg_weeks <- working_budget %>%
  filter(resid_lwr < 0 & resid < 0 & resid_upr < 0) %>%
  mutate(month = month(end_date, label = TRUE, abbr = TRUE),
         year  = year(end_date))

# Count by year-month
neg_counts <- neg_weeks %>%
  count(year, month) %>%
  arrange(desc(n))

neg_counts

neg_month_counts <- neg_weeks %>%
  count(month) %>%
  arrange(desc(n))

neg_month_counts

# Number of weeks where residuals are greater than zero 

sum(working_budget$resid_lwr > 0 & working_budget$resid > 0 & working_budget$resid_upr > 0, na.rm = TRUE)

pos_weeks <- working_budget %>%
  filter(resid_lwr > 0 & resid > 0 & resid_upr > 0) %>%
  mutate(month = month(end_date, label = TRUE, abbr = TRUE),
         year  = year(end_date))

pos_counts <- pos_weeks %>%
  count(year, month) %>%
  arrange(desc(n))

pos_counts

# Check the math, the remaining weeks residuals span zero

sum(working_budget$resid_lwr < 0 & working_budget$resid_upr > 0, na.rm = TRUE)


#3. Tabular summary----

# Assign water year based on end_date
water_budget <- working_budget %>%
  mutate(
    end_date = as.Date(end_date),  # Ensure end_date is a Date
    water_year = year(end_date) + if_else(month(end_date) >= 10, 1, 0)
  ) %>%
  filter(water_year <= 2023)  # Ensure no unexpected water years

# Summarize by water year
water_year <- water_budget %>%
  group_by(water_year) %>%
  summarize(
    across(c(
      umc_m3, sny_m3, spr_m3, fish_m3,
      umc_m3_lwr, sny_m3_lwr, spr_m3_lwr, fish_m3_lwr,
      umc_m3_upr, sny_m3_upr, spr_m3_upr, fish_m3_upr,
      L3W_m3, L3E_m3, L3E_m3_lwr, L3E_m3_upr,
      lmc_m3, lmc_m3_lwr, lmc_m3_upr
    ), ~ sum(.x, na.rm = TRUE))
  ) 


summary_water_year <-  water_year %>%
  mutate(
    T_mean = rowSums(across(c(umc_m3, sny_m3, spr_m3, fish_m3)), na.rm = TRUE),
    T_lwr  = rowSums(across(c(umc_m3_lwr, sny_m3_lwr, spr_m3_lwr, fish_m3_lwr)), na.rm = TRUE),
    T_upr  = rowSums(across(c(umc_m3_upr, sny_m3_upr, spr_m3_upr, fish_m3_upr)), na.rm = TRUE),
    
    O_mean = -lmc_m3,
    O_lwr  = -lmc_m3_lwr,
    O_upr  = -lmc_m3_upr,
    
    E_mean = -L3E_m3,
    E_lwr  = -L3E_m3_lwr,
    E_upr  = -L3E_m3_upr,
    
    W = L3W_m3
  ) %>%
  select(water_year, T_mean, T_lwr, T_upr,
         O_mean, O_lwr, O_upr,
         E_mean, E_lwr, E_upr,
         W)

# Add 1975 values
normalized_flows <- data.frame(
  Tributary = c("lmc", "umc", "fish", "sny", "spr"),
  Area_km2 = c(443.9, 283.9, 39.6, 13.8, 10.5),
  Jan = c(4.47, 2.55, 0.31, 0.071, 0.113),
  Feb = c(3.51, 2.12, 0.25, 0.071, 0.142),
  Mar = c(4.16, 2.41, 0.23, 0.065, 0.198),
  Apr = c(14.16, 7.36, 0.34, 0.283, 0.283),
  May = c(33.98, 42.48, 3.11, 2.265, 0.623),
  Jun = c(56.63, 53.80, 1.70, 1.274, 0.311),
  Jul = c(29.73, 24.07, 0.57, 0.566, 0.031),
  Aug = c(7.06, 3.96, 0.42, 0.425, 0.031),
  Sep = c(5.24, 2.55, 0.28, 0.142, 0.028),
  Oct = c(6.80, 3.68, 0.40, 0.198, 0.023),
  Nov = c(6.23, 3.68, 0.42, 0.170, 0.057),
  Dec = c(4.93, 2.69, 0.31, 0.099, 0.142),
  Mean = c(14.77, 12.65, 0.70, 0.472, 0.165)
)

# Seconds per month (approximate using days/month)
days_in_month <- c(Jan=31, Feb=28, Mar=31, Apr=30, May=31, Jun=30, 
                   Jul=31, Aug=31, Sep=30, Oct=31, Nov=30, Dec=31)

# Multiply each month's CMS by the number of seconds in that month
monthly_seconds <- days_in_month * 60 * 60 * 24  # seconds per month

# Select only the monthly columns
monthly_flows <- normalized_flows[, c("Tributary", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")]

# Convert to long format
flows_long <- pivot_longer(monthly_flows, -Tributary, names_to = "Month", values_to = "CMS")

# Add seconds per month
flows_long$Seconds <- monthly_seconds[flows_long$Month]

# Calculate monthly volume in m³
flows_long$Volume_m3 <- flows_long$CMS * flows_long$Seconds

# Summarize by tributary
annual_flows <- flows_long %>%
  filter(Tributary %in% c("umc", "fish", "sny", "spr")) %>%
  group_by(Tributary) %>%
  summarise(Annual_Volume_m3 = sum(Volume_m3))

# Tributaries
T_total_1975 <- sum(annual_flows$Annual_Volume_m3)

# Outlet
lmc_total_1975 <- flows_long %>%
  filter(Tributary == "lmc") %>%
  summarise(Total = sum(CMS * Seconds)) %>%
  pull(Total)

lmc_total_1975

#Add to summary_water_year
new_row_water <- data.frame(
  water_year = 1975,
  T_mean = T_total_1975,
  T_lwr = NA,
  T_upr = NA,
  O_mean = -lmc_total_1975,
  O_lwr = NA,
  O_upr = NA,
  E_mean = NA,
  E_lwr = NA,
  E_upr = NA,
  W = 20941435
)

summary_water_year <- rbind(summary_water_year, new_row_water)


fill_colors <- c(
  "Evaporation" = "#DD571C",
  "Outflow" = "darkblue",
  "Tributaries" = "turquoise4",
  "Wet Deposition" = "black"
)

border_colors <- c(
  "Evaporation" = "#DD571C",
  "Outflow" = "darkblue",
  "Tributaries" = "turquoise4",
  "Wet Deposition" = "black"
)

errorbar_data <- summary_water_year %>%
  transmute(
    water_year = as.factor(water_year),
    
    # Mean, lower, upper for tributaries
    Category = "Tributaries",
    Mean = T_mean,
    Min = T_lwr,
    Max = T_upr
  ) %>%
  bind_rows(
    summary_water_year%>%
      transmute(water_year = as.factor(water_year),
                Category = "Outflow", Mean = O_mean, Min = O_lwr, Max = O_upr),
    summary_water_year %>%
      transmute(water_year = as.factor(water_year),
                Category = "Evaporation", Mean = E_mean, Min = E_lwr, Max = E_upr),
    summary_water_year %>%
      transmute(water_year = as.factor(water_year),
                Category = "Wet Deposition", Mean = W, Min = NA, Max = NA))

water_annual_plot <- ggplot(errorbar_data, aes(x = water_year, y = Mean, fill = Category, color = Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  geom_errorbar(aes(ymin = Min, ymax = Max),
                position = position_dodge(width = 0.75),
                width = 0.25,
                na.rm = TRUE,
                color = "grey20") +
  scale_fill_manual(values = fill_colors) +
  scale_color_manual(values = border_colors) +
  guides(fill = guide_legend(title = NULL),
         color = guide_legend(title = NULL)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) +
  scale_y_continuous(
    labels = function(x) comma(x / 1e6),   # commas + millions
    breaks = pretty_breaks(n = 7)
  ) +
  labs(x = "Water Year", y = bquote("Flux ("*m^3~yr^-1*")"), title = "C")  

water_annual_plot

ggsave("water_annual_plot.png", water_annual_plot, "png", width = 5, height = 6)

# For table summary

summary_means <- water_year %>%
  summarise(across(-water_year, ~mean(.x, na.rm = TRUE)))

summary_means

# Calculate the minors contribution for 1975
minors <- data.frame(
  Tributary = "3008ZZ",
  Area_km2 = 99.2,
  Jan = 0.34,
  Feb = 0.28,
  Mar = 0.28,
  Apr = 0.42,
  May = 4.25,
  Jun = 2.83,
  Jul = 0.85,
  Aug = 0.57,
  Sep = 0.42,
  Oct = 0.42,
  Nov = 0.51,
  Dec = 0.34,
  Mean = 0.97
)


# Select only the monthly columns
monthly_flows_minors <- minors[, c("Tributary", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")]

# Convert to long format
flows_long <- pivot_longer(monthly_flows_minors, -Tributary, names_to = "Month", values_to = "CMS")

# Add seconds per month
flows_long$Seconds <- monthly_seconds[flows_long$Month]

# Calculate monthly volume in m³
flows_long$Volume_m3 <- flows_long$CMS * flows_long$Seconds

# Summarize by tributary
annual_flows_minor <- flows_long %>%
  filter(Tributary %in% c("3008ZZ")) %>%
  group_by(Tributary) %>%
  summarise(Annual_Volume_m3 = sum(Volume_m3))


# Flip to long format

summary_means_long <- summary_means %>%
  pivot_longer(
    cols = matches("_(m3|m3_lwr|m3_upr)$"),
    names_to = c("Variable", ".value"),
    names_pattern = "^(.*)_(m3|m3_lwr|m3_upr)$"
  ) %>%
  rename(
    `2008-2023` = m3,
    `2008-2023 minimum` = m3_lwr,
    `2008-2023 maximum` = m3_upr
  )

tribs <- summary_means_long[c(1:4),]
str(tribs)
sum(tribs$'2008-2023')/(10^9)
sum(tribs$'2008-2023 minimum')/(10^9)
sum(tribs$'2008-2023 maximum')/(10^9)
