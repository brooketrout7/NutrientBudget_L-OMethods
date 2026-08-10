
#0. Load packages----

library(ggplot2)
library(gridExtra)
library(dplyr)
library(patchwork)

#1. Calculate wet (N) deposition----

# read in and clean up data
nadp <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\0_data\\2022\\NADP_data\\NTN-mt05-W-s.csv", header = T, na.strings = (""))

# convert dates to POSIXct; data reported in Greenwich Mean Time (GMT)YYYY-MM-DD hh:mm format --> convert to UTC
nadp$dateOn <- as.POSIXct(nadp$dateOn, format = "%m/%d/%Y %H:%M", tz = "GMT")
nadp$dateOn <- with_tz(nadp$dateOn, tzone = "UTC")
nadp$dateOff <- as.POSIXct(nadp$dateOff, format = "%m/%d/%Y %H:%M", tz = "GMT")
nadp$dateOff <- with_tz(nadp$dateOff, tzone = "UTC")

summary(nadp)

# Round measurement to the nearest hour
nadp <- nadp %>%
  mutate(dateOn = round_date(dateOn, unit = "hour"), dateOff = round_date(dateOff, unit = "hour"))

# Replace -9 (invalid values) with NA

nadp[nadp == -9] <- NA

# Convert columns to numeric

nadp$NH4 <- as.numeric(nadp$NH4)
nadp$NO3 <- as.numeric(nadp$NO3)
nadp$subppt <- as.numeric(nadp$subppt)

# Calculate amount of N (wet) that fell into the bucket in kg 

#Volume = L = (subppt mm * 1 cm/10 mm) * bucket (678.9 cm2) * 1 L/1000 cm3

nadp$v_L <- (nadp$subppt/10)*678.9*(1/1000)

# Mass (kg) = C * V * (1 kg/1000000 mg)

# [no3 mg/L] x (14.00674 mg N/62.00494 mg NO3) * v_L * (1 kg /1000000 mg) = kg N
nadp$no3_kg <- ((nadp$NO3*14.00674/62.00494)*nadp$v_L*(1/1000000))

# [nh4 mg/L] x (14.00674 mg N/18.03834 mg NH4) * v_L * (1 kg /1000000 mg) = kg N
nadp$nh4_kg <- ((nadp$NH4*14.00674/18.03834)*nadp$v_L*(1/1000000))

nadp$tn_kg <- nadp$no3_kg + nadp$nh4_kg

# Deposition rate = M (kg)/Area (bucket area = 678.9 cm 2 = 0.06789 m2)

nadp$tn_m2 <- (nadp$tn_kg)/0.06789

# Note that area of the bucket cancels out (numerator for volume calculation and denominator for area calculation); so simplified equation is: 
#(C (mg/L) * P (subppt/10000) * (1/1000000)/(1m/100cm) = C * P * (1 kg/ 1000000)

# Testing this new equation; BOOM WORKED!

nadp$tn_kg_m2_alt <- (((nadp$NO3*14.00674/62.00494)*nadp$subppt*0.000001) + ((nadp$NH4*14.00674/18.03834)*nadp$subppt*0.000001))

# Scale to the area of Lake McDonald----

# deposition (kg/m2) * 27810670.1791

nadp <- nadp %>%
  mutate(TN_wet_kg = tn_kg_m2_alt*27810670.1791)

# Plot

nadp <- nadp %>%
  filter(dateOff >= as.POSIXct("2007-10-02 15:00:00") &
           dateOff <= as.POSIXct("2023-10-03 19:00:00"))


N_wet_conc <- ggplot(data = nadp, aes(x = dateOff, y = (NO3+NH4*1000))) + 
  geom_point(size = 1, color = "mediumpurple4") + 
  theme_classic() + 
  labs(title = "A", x = "", y  = expression(C[W] * " (" * mu * g * L^-1 * ")")) +
  scale_x_datetime(
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)  # <-- force axis to use only your limits
  ) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 20)
  ) + 
  scale_y_continuous(
    limits = c(0, 5000),
    labels = scales::comma
  ) + scale_y_log10()

print(N_wet_conc)

N_wet_flux <- ggplot(data = nadp, aes(x = dateOff, y = tn_m2)) + 
  geom_point(size = 1, color = "mediumpurple4") + 
  theme_classic() + 
  labs(
    title = "B",
    x = "",
    y = expression(F[W] * " (" * kg * "-" * N ~ m^-2 ~ wk^-1 * ")")
  ) + 
    scale_x_datetime(
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)  # <-- force axis to use only your limits
  ) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 20)
  )  + scale_y_continuous(
    limits = c(0, 0.000015),
    labels = scales::comma
  )+ 
  ylim(0,0.00002)

print(N_wet_flux)

N_wet <- ggplot(data = nadp, aes(x = dateOff, y = TN_wet_kg)) + 
  geom_point(size = 1, color = "mediumpurple4") + 
  theme_classic() + 
  labs(title = "C", x = "Date", y = expression(F[W] * " (" * kg * "-" * N ~ wk^-1 * ")")) +
  scale_x_datetime(
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)  # <-- force axis to use only your limits
  ) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
    plot.title = element_text(size = 20)
  )  + scale_y_continuous(
    limits = c(0, 500),
    labels = scales::comma
  )

print(N_wet)

wetN <- N_wet_conc/N_wet_flux/N_wet

ggsave(filename = "Cw_L3w.png",   # file name (can be .png, .pdf, .jpg, .tiff, .svg)
       plot = wetN,             # the ggplot object
       width = 5,                        # width in chosen units
       height = 6,                       # height in chosen units
       units = "in",                     # "in", "cm", or "mm"
       dpi = 300                         # resolution (good for publications)
)

# Remove unnecessary columns

nadp <- nadp %>%
  select(dateOn, dateOff, TN_wet_kg)

# write new file

write.csv(nadp, "C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\WetDepositionN.csv", row.names = FALSE)
