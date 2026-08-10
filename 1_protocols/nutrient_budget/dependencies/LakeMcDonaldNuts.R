
#0. Load packages----

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(scales)
library(knitr)
library(kableExtra)

#1. Calculate M----

# Read in data chemistry

chemdata_2022 <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\chemdata_2022.csv', header = TRUE, sep = ",")

chemdata_2023 <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\chemdata_2023.csv', header = TRUE, sep = ",")

# Convert date to POSIXct

chemdata_2022$date <- as.POSIXct(chemdata_2022$date, format = "%Y-%m-%d")
chemdata_2023$date <- as.POSIXct(chemdata_2023$date, format = "%Y-%m-%d")

# Read in Volume data

lake <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\lake_WY_2008_2023.csv', header = TRUE, sep = ",")

# Convert date_time to POSIXct

lake$date_time <- as.POSIXct(lake$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Calculate mean TN and TP in Lake McDonald 

# Subset 2022 for 10 m
LM_2022 <- subset(chemdata_2022, site =="LM10")

# Group by 'date' and 'site' and calculate the mean for 'tn' and 'tp'

mean_values_2022 <- LM_2022 %>%
  group_by(date) %>%
  summarise(
    mean_tn = mean(tn, na.rm = TRUE),
    mean_tp = mean(tp, na.rm = TRUE), 
    min_tn = min(tn, na.rm=TRUE),
    max_tn = max(tn, na.rm=TRUE),
    min_tp = min(tp, na.rm=TRUE),
    max_tp = max(tp, na.rm=TRUE)
  )

# Subset 2023 for 10 m 
LM_2023 <- subset(chemdata_2023, site =="LM10")

# Group by 'date' and 'site' and calculate the mean for 'tn' and 'tp'
mean_values_2023 <- LM_2023 %>%
  group_by(date) %>%
  summarise(
    mean_tn = mean(tn, na.rm = TRUE),
    mean_tp = mean(tp, na.rm = TRUE), 
    min_tn = min(tn, na.rm=TRUE),
    max_tn = max(tn, na.rm=TRUE),
    min_tp = min(tp, na.rm=TRUE),
    max_tp = max(tp, na.rm=TRUE)
  )


# Print the result
print(mean_values_2022)
print(mean_values_2023)


#isolate dates_times when lake was sampled: 4/28 @ 10 AM, 5/24 @ 10 AM, 6/7 @ 10 AM, 6:30 @ 10 AM, 7/19 @ 10 AM, 8/4 @ 8 AM, 9/6 @ 12 PM, 9/20 @ 10 AM/ modify time to be aligned with NADP pulls 
nadp_pulls <- read.csv(file = "C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\WetDeposition.csv", header=TRUE, sep=",")

# Define the specific dates to filter

# Add 2018

date <- as.POSIXct(c("2018-07-10 17:00:00", "2018-09-18 19:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
mean_tn <- as.numeric(c("605","321"))
min_tn <- as.numeric(c(390,140))
max_tn <- as.numeric(c(820,1445))

mean_tp <- as.numeric(c("18","41.8"))
min_tp <- as.numeric(c(16, 15))
max_tp <- as.numeric(c(20,160))

mean_values_2018 <- data.frame(date, mean_tn, min_tn, max_tn, mean_tp, min_tp, max_tp)

mean_values_2022$date[c(1, 2, 3, 4, 5, 6, 7, 8)] <- as.POSIXct(c("2022-04-26 15:00:00", "2022-05-24 17:00:00", "2022-06-07 15:00:00", "2022-06-28 16:00:00", "2022-07-19 17:00:00", "2022-08-02 17:00:00", "2022-09-06 18:00:00", "2022-09-20 16:00:00"), tz = "UTC")

print(mean_values_2022)

mean_values_2023$date[c(1, 2, 3)] <- as.POSIXct(c("2023-05-09 20:00:00", "2023-07-11 14:00:00", "2023-09-19 18:00:00"), tz = "UTC")

print(mean_values_2023)

selected_dates <- as.POSIXct(c("2018-07-10 17:00:00", "2018-09-18 19:00:00", "2022-04-26 15:00:00", "2022-05-24 17:00:00", "2022-06-07 15:00:00", "2022-06-28 16:00:00", "2022-07-19 17:00:00", "2022-08-02 17:00:00", "2022-09-06 18:00:00", "2022-09-20 16:00:00", "2023-05-09 20:00:00", "2023-07-11 14:00:00", "2023-09-19 18:00:00"), tz = "UTC")

# Filter the data frame for the selected dates

lake_filtered <- lake %>%
  filter(date_time %in% selected_dates)

# Print the filtered data frame

print(lake_filtered)

# Calculate mass and use 10 m as the "mean"

# Merge data_frames based on date_time/date

mean_values <- rbind(mean_values_2018, mean_values_2022, mean_values_2023)

lake_filtered <- lake_filtered %>%
  left_join(mean_values, by = c("date_time" = "date"))

# Print the updated data frame

print(lake_filtered)

# Calculate M; lake volume is in m3 so must convert to L and make mass kg. 

lake_nuts <- lake_filtered %>%
  mutate(kg_N = mean_tn*(lake_volume*1000)*(1/1000000000), 
         kg_N_lwr = (min_tn)*(lake_volume*1000)*(1/1000000000),
         kg_N_upr = (max_tn)*(lake_volume*1000)*(1/1000000000),
         kg_P = mean_tp*(lake_volume*1000)*(1/1000000000), 
         kg_P_lwr = (min_tp)*(lake_volume*1000)*(1/1000000000),
         kg_P_upr = (max_tp)*(lake_volume*1000)*(1/1000000000)
         )

print(lake_nuts)

# Write csv
write.csv(lake_nuts, file = "C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\lake_nuts.csv", row.names = FALSE)


#2. For supplemental section
# Previous data----

p <- data.frame(
  dates = c("1975", "1984-1990", "1996-2007", "2018", "2022", "2023"),
  tp = c(6, 2.8, 5.1, 37.8, 3.35, 4.3),
  tp_lower = c(3, 0.5, 2.5, 15, 1.5, 1.7),
  tp_upper = c(10, 10.5, 10, 160, 9.1, 6.8)
)

n <- data.frame(
  dates = c("1975", "1984-1990", "1996-2007", "2018", "2022", "2023"),
  tn = c(369.5, 211.8, 274, 369, 133.05, 146.8), 
  tn_lower = c(300, 182, 226, 140, 97.4, 119),
  tn_upper = c(460, 244, 314, 1445, 181, 160)
)


phyto <- data.frame(
  dates = c("1975", "1984-1990", "1996-2007", "2018", "2022", "2023"),
  chl = c(0.5, 0.612, 0.6, 0.621, 0.85, 0.435),            
  chl_lower = c(0.4, 0.24, 0.2, NA, 0.18, 0.22),         
  chl_upper = c(0.6, 0.956, 1.4, NA, 1.48, 0.67)
)

secchi <- data.frame(
  dates = c("1975", "1984-1990", "1996-2007", "2018", "2022", "2023"),
  sd = c(7.9, 14.8, 12.9, NA, 11.6, 14.7),            
  sd_lower = c(7, 7.5, 3.2, NA, 6.5, 13.5),         
  sd_upper = c(8.7, 20.5, 19.8, NA, 16.2, 16.5)
)


#TP plot 

p_plot <- ggplot(data = p, aes(x = dates, y = tp)) +
  geom_bar(stat = "identity", fill = "salmon1") +
  geom_errorbar(aes(ymin = tp_lower, ymax = tp_upper), 
                width = 0.2, position = position_dodge(width = 0.9), color = "black") +
  labs(title = "A", x = "", y = expression(paste("TP (",~ mu, "g-P ", L^{-1}, ")"))) +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 20), labels = scales::comma) +
  geom_hline(yintercept = 17.7, color = "grey", linetype = "dashed", size = 1.0) + 
  geom_hline(yintercept = 95.6, color = "grey", linetype = "dashed", size = 1.0) + 
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
  ) + 
  theme(legend.position = "none") +
  annotate("text", x = 1.5, y = 35, size = 2, label = "Mesotrophic") +
  annotate("text", x = 1.5, y = 110, size = 2, label = "Eutrophic")

print(p_plot)

#TN

n_plot <- ggplot(data = n, aes(x = dates, y = tn)) +
  geom_bar(stat = "identity", fill = "mediumpurple4") +
  geom_errorbar(aes(ymin = tn_lower, ymax = tn_upper), width = 0.2, position = position_dodge(width = 0.9), color = "black") +
  labs(title = "B", x = "", y = expression(paste("TN (",~ mu, "g-N ", L^{-1}, ")"))) +
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 200), labels = scales::comma) +
  geom_hline(yintercept = 1630, color = "grey", linetype = "dashed", size = 1.0) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
  ) + 
  theme(legend.position = "none") +
  annotate("text", x = 1.5, y = 1800, size = 2, label = "Mesotrophic")


print(n_plot)



#chl
chl_plot <- ggplot(data = phyto, aes(x = dates, y = chl)) +
  geom_bar(stat = "identity", fill = "#487A45") +
  geom_errorbar(aes(ymin = chl_lower, ymax = chl_upper), width = 0.2, position = position_dodge(width = 0.9), color = "black") +
  labs(title = "C", x = "", y = expression(paste("Chlorophyll-a (",~ mu, "g ", L^{-1}, ")"))) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1), labels = scales::comma) +
  geom_hline(yintercept = 4.5, color = "grey", linetype = "dashed", size = 1.0) + 
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 90)
  ) + 
  theme(legend.position = "none") +
  annotate("text", x = 1.5, y = 4.9, size = 2, label = "Mesotrophic")


print(chl_plot)


#sd
sd_plot <- ggplot(data = secchi, aes(x = dates, y = sd)) +
  geom_bar(stat = "identity", fill = "turquoise4") +  # Use stat = "identity" to supply y-values directly
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper), width = 0.2, position = position_dodge(width = 0.9), color = "black") +
  labs(title = "D", y = "Secchi disc depth (m)", x = "") +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 2.5), labels = scales::comma) +
  geom_hline(yintercept = 5.4, color = "grey", linetype = "dashed", size = 1.0) + 
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 90)
  ) + 
  theme(legend.position = "none") +   
  annotate("text", x = 1.5, y = 22.5, size = 2, label = "Mesotrophic")


print(sd_plot)


lmdata <- (p_plot|n_plot)/(chl_plot|sd_plot)

ggsave("lmdata.png", lmdata, "png", width = 5, height = 4)


#LM data

chemdata_2018 <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\0_data\\historic\\forpub.csv', header = TRUE, sep = ",")

chemdata_2022 <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\chemdata_2022.csv', header = TRUE, sep = ",")

chemdata_2023 <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\chemdata_2023.csv', header = TRUE, sep = ",")

# Convert date to POSIXct

chemdata_2022$date <- as.POSIXct(chemdata_2022$date, format = "%Y-%m-%d")
chemdata_2023$date <- as.POSIXct(chemdata_2023$date, format = "%Y-%m-%d")

# Replace "2019" with 2023
chemdata_2023$date <- update(chemdata_2023$date, year = 2023)

# Remove unnecessary columns

clean_2018 <- chemdata_2018 %>%
  select(Month, Param, Value, Station.Name)

# Make two separate columns for TN or TP

clean_2018_wide <- clean_2018 %>%
  pivot_wider(
    names_from = Param,
    values_from = Value
  )

# Add year column

clean_2018_wide <- clean_2018_wide %>%
mutate(Year = 2018) %>%
  select(Year, Month, Station.Name, TP, TN)%>%
  rename(Location = "Station.Name")

# Rename stations

clean_2018_wide <- clean_2018_wide %>%
  mutate(
    Location = if_else(
      Location %in% c("Mid-Lake, South", "Mid-Lake, North"),
      Location,
      "Nearshore"
    )
  )

# Add type of sample

clean_2018_wide <- clean_2018_wide %>%
  mutate("Sample Type" = "10-m")%>%
  select(Year, Month, Location, "Sample Type", TP, TN)


# Add 2022 data

# Keep LM data
chemdata_2022 <- chemdata_2022 %>%
  select(date, id, tp, tn) %>%
  filter(str_detect(id, "(?i)^(LM10_|LM5i_|LMHYPO_)[ABC]$"))

# Put sample type
chemdata_2022 <- chemdata_2022 %>%
  mutate(
    `Sample Type` = case_when(
      str_detect(id, "(?i)^LM10_") ~ "10-m",
      str_detect(id, "(?i)^LM5i_") ~ "5-integrated",
      str_detect(id, "(?i)^LMHYPO_") ~ "50-m",
      TRUE ~ NA_character_
    )
  )

# Add Year and Month

chemdata_2022 <- chemdata_2022 %>%
  mutate(
    Year  = year(date),
    Month = lubridate::month(date, label = TRUE, abbr = FALSE),
    Location = "Mid-Lake, South"
  )

# Organize like 2018

chemdata_2022 <- chemdata_2022 %>%
  select(Year, Month, Location, "Sample Type", tp, tn)%>%
  rename(TP = "tp", TN = "tn")


# Add 2023 data

# Keep LM data
chemdata_2023 <- chemdata_2023 %>%
  select(date, id, tp, tn)

# Keep only lake data
chemdata_2023 <- chemdata_2023 %>%
  filter(
  !str_detect(id, "(?i)^(UMC_|LMC_)"))


# Put sample type and location
chemdata_2023 <- chemdata_2023 %>%
  mutate(
    Year  = year(date),
    Month = lubridate::month(date, label = TRUE, abbr = FALSE),
    Location = case_when(
      str_detect(id, "(?i)^(LM10_|LM5i_)") ~ "Mid-Lake, South",
      str_detect(id, "(?i)^(MLN_)") ~ "Mid-Lake, North",
      TRUE ~ "Nearshore"
    )
  )

# Put sample type
chemdata_2023 <- chemdata_2023 %>%
  mutate(
    `Sample Type` = case_when(
      str_detect(id, "(?i)^LM10_") ~ "10-m",
      str_detect(id, "(?i)^LM5i_") ~ "5-integrated",
      TRUE ~ NA_character_
    )
  )

# Organize like 2018
chemdata_2023 <- chemdata_2023 %>%
  select(Year, Month, Location, "Sample Type", tp, tn)%>%
  rename(TP = "tp", TN = "tn")


chemdata_2023 <- chemdata_2023 %>%
  mutate(
    `Sample Type` = case_when(
      is.na(`Sample Type`) & Location %in% c("Nearshore", "Mid-Lake, North") ~ "10-m",
      TRUE ~ `Sample Type`
    )
  )
# Merge all together and replace "NA's with below detection"

all_data <- rbind(clean_2018_wide, chemdata_2022, chemdata_2023)

# Export as latex table

df_display <- all_data %>%
  mutate(
    Year  = as.integer(Year),                                    # no .00
    TPfmt = ifelse(is.na(TP), "below detection", number(TP, 0.1)),
    TNfmt = ifelse(is.na(TN), "below detection", number(TN, 0.1))
  ) %>%
  select(
    Year, Month, Location, `Sample Type`,
    `TP (\\mu g\\, L^{-1})` = TPfmt,
    `TN (\\mu g\\, L^{-1})` = TNfmt
  )

kbl(
  df_display,
  format  = "latex",
  booktabs = TRUE,
  escape = FALSE,                     # keep LaTeX in headers
  longtable = TRUE,
  caption = "Lake McDonald nutrient observations by year, month, location, and sample type."
) |>
  kable_styling(latex_options = c("repeat_header"), font_size = 9) |>
  save_kable("lake_mcdonald_nutrients_long.tex")

