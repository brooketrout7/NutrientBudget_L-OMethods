
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

flbs <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\0_data\\2022\\FLBS\\Dry_Deposition_for_Brooke.csv", header = T, na.strings = (""))

flbs_add <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\0_data\\2022\\FLBS\\FLBSPublicData.csv", header = T, na.strings = (""))

# Convert date to POSIXct and time zone UTC
flbs$Date <- as.POSIXct(flbs$CollectDate, format = "%m/%d/%Y", tz="UTC")

flbs_add$Date <- as.POSIXct(flbs_add$Date, format = "%m/%d/%Y", tz="UTC")

flbs_add <- flbs_add %>%
  filter(Date < "2008-06-13")

# Remove unnecessary columns
flbs <- flbs %>%
  select(Date, Volume, Test, CorrectedReportedResult) %>%
  rename(
    Parameter = Test,
    Value = CorrectedReportedResult)

flbs <- flbs %>%
  dplyr::mutate(
    Parameter = dplyr::recode(
      Parameter,
      "Total Nitrogen"   = "TN",
      "Total Phosphorus" = "TP"
    )
  )

flbs_add <- flbs_add %>%
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

# TN and TP that fell into the bucket
N <- subset(flbs, Parameter == "TN")

duplicated_dates <- duplicated(N$Date)
duplicated_values <- N$Date[duplicated(N$Date)]
print(duplicated_values)

#sum duplicated values
N <- N %>%
  group_by(Date) %>%
  summarise(
    Volume  = sum(Volume, na.rm = TRUE),
    Volume_mL = sum(Volume_mL, na.rm = TRUE),
    Result_ugL  = sum(Result_ugL, na.rm = TRUE),
    Volume_L = sum(Volume_L, na.rm = TRUE),
    .groups = "drop"
  )



P <- subset(flbs, Parameter == "TP")
duplicated_dates <- duplicated(P$Date)
duplicated_values <- P$Date[duplicated(P$Date)]
print(duplicated_values)

P <- P %>%
  group_by(Date) %>%
  summarise(
    Volume  = sum(Volume, na.rm = TRUE),
    Volume_mL = sum(Volume_mL, na.rm = TRUE),
    Result_ugL  = sum(Result_ugL, na.rm = TRUE),
    Volume_L = sum(Volume_L, na.rm = TRUE),
    .groups = "drop"
  )

# File for fire simulations
write.csv(flbs, "C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\dry_for_fire_sim.csv", row.names = FALSE)

# Kg TN or TP/m2 = C (ug/L) x (1 kg/1000000000 ug) x Volume of deposition (L)/A (bucket; m2 = 629.02 cm2/10,000 cm2) 

N$TN_kg_m2 <- ((N$Result_ugL)*(1/1000000000)*(N$Volume_L))/0.062902

P$TP_kg_m2 <- ((P$Result_ugL)*(1/1000000000)*(P$Volume_L))/0.062902

flbs <- N %>%
  left_join(P, by = "Date") %>%
  select(Date = Date, 
         TN_kg_m2,
         TP_kg_m2)

# Scale to the area of Lake McDonald
flbs <- flbs %>%
  mutate(TN_dry_kg = TN_kg_m2*27810670.1791, TP_dry_kg = TP_kg_m2*27810670.1791)

# Plot
N$Date <- as.POSIXct(N$Date, format = "%m/%d/%Y", tz="UTC")
N$Result_ugL <- as.numeric(N$Result_ugL)
P$Date <- as.POSIXct(P$Date, format = "%m/%d/%Y", tz="UTC")
P$Result_ugL <- as.numeric(P$Result_ugL)


# Plots 

vol_plot <- ggplot(data = N, aes(x = Date, y = Volume_L/1000)) + 
  geom_point(size = 1, color = "black") + 
  theme_classic() + 
  labs(
    title = "A",
    x = "",
    y = expression(L[D]^3 * " (" * m^3 ~ wk^-1 * ")")
  )  + 
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
  ) + 
  scale_y_continuous(
    labels = scales::comma)

vol_plot

N_dry_conc <- ggplot(data = N, aes(x = Date, y = Result_ugL)) + 
  geom_point(size = 1, color = "mediumpurple4") + 
  theme_classic() + 
  labs(title = "A", x = "", y  = expression(C[D] * " (" * mu * g * L^-1 * ")")) +
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
  ) + 
  scale_y_continuous(
    labels = scales::comma
  )+ scale_y_log10()

print(N_dry_conc)

N_dry_flux <- ggplot(data = flbs, aes(x = Date, y = TN_kg_m2)) + 
  geom_point(size = 1, color = "mediumpurple4") + 
  theme_classic() + 
  labs(
    title = "B",
    x = "",
    y = expression(F[D] * " (" * kg * "-" * N ~ m^-2 ~ wk^-1 * ")")
  ) +
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
  ) 


print(N_dry_flux)

N_dry <- ggplot(data = flbs, aes(x = Date, y = TN_dry_kg)) + 
  geom_point(size = 1, color = "mediumpurple4") + 
  theme_classic() + 
  labs(title = "C", x = "Date", y = expression(F[D] * " (" * kg * "-" * N ~ wk^-1 * ")")) +
  scale_x_datetime(
    limits = as.POSIXct(c("2007-10-01", "2023-10-01"), tz = "UTC"),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  )+
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
    plot.title = element_text(size = 12)
  )  + scale_y_continuous(
    limits = c(0, 1500),
    labels = scales::comma
  )

print(N_dry)

dry_tn <- N_dry_conc/N_dry_flux/N_dry

ggsave(filename = "CD_L3D_N.png",   # file name (can be .png, .pdf, .jpg, .tiff, .svg)
       plot = dry_tn,             # the ggplot object
       width = 5,                        # width in chosen units
       height = 6,                       # height in chosen units
       units = "in",                     # "in", "cm", or "mm"
       dpi = 300                         # resolution (good for publications)
)

P_dry_conc <- ggplot(data = P, aes(x = Date, y = Result_ugL)) + 
  geom_point(size = 1, color = "salmon1") + 
  theme_classic() + 
  labs(title = "A", x = "", y  = expression(C[D] * " (" * mu * g * L^-1 * ")")) +
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
  ) + 
  scale_y_continuous(
    labels = scales::comma
  )+ scale_y_log10()


print(P_dry_conc)

P_dry_flux <- ggplot(data = flbs, aes(x = Date, y = TP_kg_m2)) + 
  geom_point(size = 1, color = "salmon1") + 
  theme_classic() + 
  labs(
    title = "B",
    x = "",
    y = expression(F[D] * " (" * kg * "-" * P ~ m^-2 ~ wk^-1 * ")")
  ) +
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
  )  + scale_y_continuous(
    limits = c(0, 0.000015),
    labels = scales::comma
  )

P_dry <- ggplot(data = flbs, aes(x = Date, y = TP_dry_kg)) + 
  geom_point(size = 1, color = "salmon1") + 
  theme_classic() + 
  labs(title = "C", x = "Date", y = expression(F[D] * " (" * kg * "-" * P ~ wk^-1 * ")")) +
  scale_x_datetime(
    limits = as.POSIXct(c("2007-10-01", "2023-10-01"), tz = "UTC"),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  )+
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
    plot.title = element_text(size = 12)
  )  + scale_y_continuous(
    limits = c(0, 500),
    labels = scales::comma
  )

print(P_dry)

dry_tp <- P_dry_conc/P_dry_flux/P_dry

ggsave(filename = "CD_L3D_P.png",   # file name (can be .png, .pdf, .jpg, .tiff, .svg)
       plot = dry_tp,             # the ggplot object
       width = 5,                        # width in chosen units
       height = 6,                       # height in chosen units
       units = "in",                     # "in", "cm", or "mm"
       dpi = 300                         # resolution (good for publications)
)

# Write datafile

write.csv(flbs, "C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\drydepositionNuts.csv", row.names = FALSE)


