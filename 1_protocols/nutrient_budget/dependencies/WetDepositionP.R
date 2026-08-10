
#0. Load packages----

library(ggplot2)
library(gridExtra)
library(lubridate)
options(scipen = 999)
library(patchwork)
library(scales)
library(dplyr)
library(tidyr)


#1. Flux of TN and TP in dry deposition to Lake McDonald----

# Read in and clean up data

flbs <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\0_data\\2022\\FLBS\\Wet_Deposition_for_Brooke.csv", header = T, na.strings = (""))

flbs_add <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\0_data\\2022\\FLBS\\FLBSPublicData.csv", header = T, na.strings = (""))

# Convert date to POSIXct and time zone UTC
flbs$Date <- as.POSIXct(flbs$CollectDate, format = "%m/%d/%Y", tz="UTC")

flbs_add$Date <- as.POSIXct(flbs_add$Date, format = "%m/%d/%Y", tz="UTC")

flbs_add <- flbs_add %>%
  filter(Date < "2008-06-26")

# Remove unnecessary columns
flbs <- flbs %>%
  select(Date, Volume, Param, CorrectedReportedResult) %>%
  rename(Parameter = "Param", Value = "CorrectedReportedResult")

flbs_add <- flbs_add %>%
  filter(Parameter == "TP") %>%
  select(Date, Volume, Parameter, Value)

# Convert relevant columns to numeric
flbs$Volume_mL <- as.numeric(flbs$Volume)
flbs$Volume_L <- flbs$Volume_mL/1000
flbs$Result_ugL <-as.numeric(flbs$Value)

flbs_add$Volume_mL <- as.numeric(flbs_add$Volume)
flbs_add$Volume_L <- flbs_add$Volume_mL/1000
flbs_add$Result_ugL <-as.numeric(flbs_add$Value)

# Merge dataframes
flbs <- rbind(flbs_add, flbs)

# TP that fell into the bucket
duplicated_dates <- duplicated(flbs$Date)
duplicated_values <- flbs$Date[duplicated(flbs$Date)]
print(duplicated_values)

#"2007-11-14 UTC" "2008-05-02 UTC" "2008-05-13 UTC" -> these all have a reported volume of  0; "2014-05-02 UTC"

#sum duplicated values
flbs <- flbs %>%
  group_by(Date) %>%
  summarise(
    Volume  = sum(Volume, na.rm = TRUE),
    Volume_mL = sum(Volume_mL, na.rm = TRUE),
    Result_ugL  = sum(Result_ugL, na.rm = TRUE),
    Volume_L = sum(Volume_L, na.rm = TRUE),
    .groups = "drop"
  )


# Kg TP/m2 = C (ug/L) x (1 kg/1000000000 ug) x Volume of deposition (L)/A (bucket; m2 = 629.02 cm2/10,000 cm2) 

flbs$TP_kg_m2 <- ((flbs$Result_ugL)*(1/1000000000)*(flbs$Volume_L))/0.062902

# Scale to the area of Lake McDonald
flbs <- flbs %>%
  mutate(TP_wet_kg = TP_kg_m2*27810670.1791)

# Plots 

P_wet_conc <- ggplot(data = flbs, aes(x = Date, y = Result_ugL)) + 
  geom_point(size = 1, color = "salmon1") + 
  theme_classic() + 
  labs(title = "A", x = "", y  = expression(C[W] * " (" * mu * g * L^-1 * ")")) +
  scale_x_datetime(
    limits = as.POSIXct(c("2007-10-01", "2023-10-01"), tz = "UTC"),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  )+
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 12)
  ) + 
  scale_y_continuous(
    limits = c(0, 800),
    labels = scales::comma
  )+ scale_y_log10()


print(P_wet_conc)

P_wet_flux <- ggplot(data = flbs, aes(x = Date, y = TP_kg_m2)) + 
  geom_point(size = 1, color = "salmon1") + 
  theme_classic() + 
  labs(
    title = "B",
    x = "",
    y = expression(F[W] * " (" * kg * "-" * P ~ m^-2 ~ wk^-1 * ")")
  ) +
  scale_x_datetime(
    limits = as.POSIXct(c("2007-10-01", "2023-10-01"), tz = "UTC"),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 12)
  )  + scale_y_continuous(
    limits = c(0, 0.0000025),
    labels = scales::comma
  )


print(P_wet_flux)

P_wet <- ggplot(data = flbs, aes(x = Date, y = TP_wet_kg)) + 
  geom_point(size = 1, color = "salmon1") + 
  theme_classic() + 
  labs(title = "C", x = "Date", y = expression(F[W] * " (" * kg * "-" * P ~ wk^-1 * ")")) +
  scale_x_datetime(
    limits = as.POSIXct(c("2007-10-01", "2023-10-01"), tz = "UTC"),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  ) +
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
    plot.title = element_text(size = 12)
  )  + scale_y_continuous(
    limits = c(0, 100),
    labels = scales::comma
  )


print(P_wet)


wet_tp <- P_wet_conc/P_wet_flux/P_wet

ggsave(filename = "CW_L3W_P.png",   # file name (can be .png, .pdf, .jpg, .tiff, .svg)
       plot = wet_tp,             # the ggplot object
       width = 5,                        # width in chosen units
       height = 6,                       # height in chosen units
       units = "in",                     # "in", "cm", or "mm"
       dpi = 300                         # resolution (good for publications)
)

# Write datafile

write.csv(flbs, "C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\WetdepositionP.csv", row.names = FALSE)


