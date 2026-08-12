
# ==============================================================================
# Simulate wildfire and snowmelt effects on the Lake McDonald nutrient budget
#
# Purpose:
#   Simulate changes in tributary nutrient concentrations, tributary discharge,
#   and atmospheric deposition during wildfire and snowmelt periods and
#   propagate these changes through the Lake McDonald nutrient mass-balance
#   model.
#
# Simulation periods:
#   - Sprague Fire:       August-October 2017
#   - Winter null period: November 2017-April 2018
#   - Snowmelt:           April-July 2018
#   - Summer null period: July-August 2018
#   - Howe Ridge Fire:    August-September 2018
#
#
# Notes:
#   - Large Monte Carlo outputs may be excluded from GitHub using .gitignore.
#   - Summary products required to reproduce manuscript figures should remain
#     in 3_products/.
# ==============================================================================

# 1. Read input data -----------------------------------------------------------

# 1.1 Lake McDonald nutrient budget ------------------------------------------
# Read the completed weekly nutrient-budget time series used as the baseline
# state for the event simulations. Dates are converted to Date objects below.

nuts <- read.csv(
  here(
    "3_products",
    "NutrientBudget.csv"
  )
)

nuts <- nuts %>%
  mutate(
    start_date = as.Date(start_date),
    end_date = as.Date(end_date)
  )

# 1.2 Tributary chemistry ----------------------------------------------------
# Read observed tributary chemistry used to estimate representative baseline
# TN and TP concentrations for each tributary.

chemdata_2022 <- read.csv(
  here(
    "0_data",
    "chemdata_2022.csv"
  )
)

chemdata_2022$date <- as.POSIXct(
  chemdata_2022$date,
  format = "%Y-%m-%d",
  tz = "UTC"
)


chemdata_2023 <- read.csv(
  here(
    "0_data",
    "chemdata_2023.csv"
  )
)

chemdata_2023$date <- as.POSIXct(
  chemdata_2023$date,
  format = "%Y-%m-%d",
  tz = "UTC"
)

chemdata_2023 <- chemdata_2023 %>%
  mutate(
    date = update(
      date,
      year = 2023
    )
  )
# 1.3 Tributary discharge ----------------------------------------------------
# Read modeled tributary discharge produced earlier in the workflow. The file
# is an incremental product and is restricted to the 2018 snowmelt period used
# to define hydrograph shape in the simulation.
tribs_Q <- readr::read_csv(
  here(
    "2_incremental",
    "QtQo.csv"
  ),
  col_types = cols(
    dateTime = col_datetime(format = "")
  )
)

tribs_Q$dateTime <- as.POSIXct(
  tribs_Q$dateTime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)


tribs_Q <- tribs_Q %>%
  select(dateTime, umc_discharge_m3_s, umc_discharge_m3_s_lwr, umc_discharge_m3_s_upr, sny_discharge_m3_s, sny_discharge_m3_s_lwr, sny_discharge_m3_s_upr, spr_discharge_m3_s, spr_discharge_m3_s_lwr, spr_discharge_m3_s_upr, fish_discharge_m3_s, fish_discharge_m3_s_lwr, fish_discharge_m3_s_upr) %>%
  filter(
    dateTime >= as.POSIXct("2018-04-10 00:00:00") &
      dateTime <= as.POSIXct("2018-07-10 23:59:59")
  )
# 1.4 Dry deposition -------------------------------------------------
# Read the dry-deposition data prepared for the wildfire simulation and convert
# sampling dates to Date objects before identifying wildfire-associated periods.

dry <- read.csv(
  here(
    "2_incremental",
    "FD_for_fire_sim.csv"
  )
)

dry$Date <- as.Date(
  dry$Date,
  format = "%Y-%m-%d"
)


# 2. Estimate baseline nutrient concentrations -------------------------------
# Combine tributary chemistry observations and estimate mean TN and TP
# concentrations for each tributary. These means parameterize the probability
# distributions used in the Monte Carlo simulations.
# Tributary chemistry ---------------------------------------------------------
chemdata <- bind_rows(
  chemdata_2022,
  chemdata_2023
)

mean_values <- chemdata %>%
  filter(
    site %in% c(
      "UMC",
      "Snyder",
      "Sprague",
      "Fish"
    )
  ) %>%
  group_by(
    date,
    site
  ) %>%
  summarise(
    mean_tn = mean(
      tn,
      na.rm = TRUE
    ),
    mean_tp = mean(
      tp,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    mean_tn = ifelse(
      is.nan(mean_tn),
      NA,
      mean_tn
    ),
    mean_tp = ifelse(
      is.nan(mean_tp),
      1.5,
      mean_tp
    )
  )


# Calculate site-specific mean tributary concentrations. These values provide
# the baseline concentrations from which wildfire and snowmelt perturbations
# are simulated.

UMC_mean_tp <- mean(
  mean_values$mean_tp[mean_values$site == "UMC"],
  na.rm = TRUE
)

UMC_mean_tn <- mean(
  mean_values$mean_tn[mean_values$site == "UMC"],
  na.rm = TRUE
)

SNY_mean_tp <- mean(
  mean_values$mean_tp[mean_values$site == "Snyder"],
  na.rm = TRUE
)

SNY_mean_tn <- mean(
  mean_values$mean_tn[mean_values$site == "Snyder"],
  na.rm = TRUE
)

SPR_mean_tp <- mean(
  mean_values$mean_tp[mean_values$site == "Sprague"],
  na.rm = TRUE
)

SPR_mean_tn <- mean(
  mean_values$mean_tn[mean_values$site == "Sprague"],
  na.rm = TRUE
)

FISH_mean_tp <- mean(
  mean_values$mean_tp[mean_values$site == "Fish"],
  na.rm = TRUE
)

FISH_mean_tn <- mean(
  mean_values$mean_tn[mean_values$site == "Fish"],
  na.rm = TRUE
)

# Dry deposition --------------------------------------------------------------
# Estimate a TP dry-deposition multiplier from observations collected during
# wildfire-associated periods relative to the full dry-deposition record.
# dry deposition

# Calculate mean TP dry deposition across the complete observational record.
# This serves as the baseline dry-deposition value for the wildfire comparison.
# Mean TP concentration in dry deposition

D_mean_P <- mean(
  dry$Value[
    dry$Parameter == "TP"
  ],
  na.rm = TRUE
)

D_mean_N <- mean(
  dry$Value[
    dry$Parameter == "TN"
  ],
  na.rm = TRUE
)


# Isolate observations from three wildfire-associated sampling periods.
# Date windows are retained explicitly so the observations contributing to the
# multiplier are transparent and reproducible.
# Isolate dry-deposition observations associated with wildfire periods

dry_dep_boulder <- dry %>%
  filter(
    Date >= as.Date("2021-07-31"),
    Date <= as.Date("2021-08-20")
  )

dry_dep_2022 <- dry %>%
  filter(
    Date >= as.Date("2022-07-30"),
    Date <= as.Date("2022-09-01")
  )

dry_dep_2023 <- dry %>%
  filter(
    Date >= as.Date("2023-07-30"),
    Date <= as.Date("2023-08-02")
  )


# Combine observations from all wildfire-associated periods into one object.
# Combine wildfire-associated observations

fires_dry <- bind_rows(
  dry_dep_boulder,
  dry_dep_2022,
  dry_dep_2023
)


# Calculate the wildfire TP dry-deposition multiplier. P_mean is dimensionless:
# values > 1 indicate higher mean TP deposition during wildfire-associated
# periods relative to the full observational record.
# Calculate wildfire TP deposition multiplier relative to mean dry deposition

P_mean <- mean(
  fires_dry$Value[
    fires_dry$Parameter == "TP"
  ],
  na.rm = TRUE
) / D_mean_P

N_mean <- mean(
  fires_dry$Value[
    fires_dry$Parameter == "TN"
  ],
  na.rm = TRUE
) / D_mean_N

# 3. Prepare event periods for simulation ------------------------------------
# Restrict the nutrient-budget record to the interval spanning the Sprague Fire,
# winter null period, 2018 snowmelt, summer null period, and Howe Ridge Fire.
# 4. Simulate Wildfire----

# Phosphorus simulation -------------------------------------------------------
# Convert weekly tributary P and N loads to concentrations where needed for the
# event simulations. Concentrations are expressed from mass and water volume.
####Phosphorus Simulation


dates <- nuts %>%
  filter(end_date >= as.Date("2017-08-22") & end_date <= as.Date("2018-09-25")) %>%
  select(start_date, end_date, 
         umc_P, umc_P_lwr, umc_P_upr, umc_N, umc_N_lwr, umc_N_upr, umc_m3, umc_m3_lwr, umc_m3_upr,
         sny_P, sny_P_lwr, sny_P_upr, sny_N, sny_N_lwr, sny_N_upr, sny_m3, sny_m3_lwr, sny_m3_upr,
         spr_P, spr_P_lwr, spr_P_upr, spr_N, spr_N_lwr, spr_N_upr, spr_m3, spr_m3_lwr, spr_m3_upr,     
         fish_P, fish_P_lwr, fish_P_upr, fish_N, fish_N_lwr, fish_N_upr, fish_m3, fish_m3_lwr, fish_m3_upr,  
         TN_wet_kg, 
         TP_dry_kg, TN_dry_kg, 
         H_P, H_N, S_P, S_N, DN_N, 
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


# 5. Initialize Monte Carlo simulation ---------------------------------------
# Set the random-number seed, number of iterations, parallel-processing plan,
# output location, and containers used to collect simulation results.
# Initial values---- 

set.seed(123)

n_iter <- 10 # Number of iterations
plan(multisession, workers = parallel::detectCores() - 1) # Parallel processing

# Define the repository directory for intermediate Monte Carlo outputs.
# Set output
out_dir <- here(
  "2_incremental"
)

all_mass <- vector("list", n_iter)
all_concentrations <- vector("list", n_iter)
all_discharges <- vector("list", n_iter)
all_deposition <- vector("list", n_iter)


# Define an exponential decay function used to transition simulated wildfire
# concentrations and deposition from an initial elevated value back toward the
# observed endpoint over each event period.
# Define the decay function to save memory in the function; final = initial*e^(-k*t) 
decay <- function(init, end, steps) {
  rate <- log(end / init) / (steps - 1)
  init * exp(rate * (0:(steps - 1)))
} 


# Initialize lake P mass using the modeled nutrient-budget mass at the beginning
# of the Sprague Fire simulation.

M0 <- sprague$kg_P_est[1] # initial starting mass

# Store deposition terms needed by the simulation.
# Deposition
D_mean_P <- mean(nuts$TP_dry_kg, na.rm = TRUE) # Set deposition to load
D_snow <- snow$TP_dry_kg

# Convert lake volume and Lower McDonald Creek discharge to liters so that
# outflow losses are dimensionally consistent with nutrient concentrations.
# Volume of the lake (converted to liters from m3) and discharge from lmc needed to calculate outflow
V_sprague <- sprague$lake_volume * 1000
Q_sprague <- sprague$lmc_m3 * 1000

V_snow <- snow$lake_volume * 1000
Q_snow <- snow$lmc_m3 * 1000

V_howe <- howe$lake_volume * 1000
Q_howe <- howe$lmc_m3 * 1000


# Convert tributary discharge from m3 to liters for calculation of simulated
# tributary nutrient loads during wildfire periods.
# Discharge to calculate loading from the tributaries given the newly simulated concentrations
UMC_Q_sprague <- sprague$umc_m3 * 1000
SNY_Q_sprague <- sprague$sny_m3 * 1000
SPR_Q_sprague <- sprague$spr_m3 * 1000
FISH_Q_sprague <- sprague$fish_m3 * 1000


# Derive the mean weekly hydrograph shape during snowmelt from modeled tributary
# discharge. The normalized hydrograph is later scaled by simulated peak values.
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

# Calculate mean discharge for each snowmelt time step.
# Apply the function across each row in `snow` to get discharge means
discharge_means <- map2_dfr(snow$start_date, snow$end_date,
                            ~ get_discharge_means(.x, .y, tribs_Q))

# Normalize each tributary hydrograph by its maximum so values represent only
# the relative temporal shape of discharge through snowmelt.
# normalize by max so just capturing the shape 

UMC_Q_hydro <- discharge_means$umc_discharge_mean/max(discharge_means$umc_discharge_mean)
SNY_Q_hydro <- discharge_means$sny_discharge_mean/max(discharge_means$sny_discharge_mean)
SPR_Q_hydro <- discharge_means$spr_discharge_mean/max(discharge_means$spr_discharge_mean)

UMC_Q_howe <- howe$umc_m3 * 1000
SNY_Q_howe <- howe$sny_m3 * 1000
SPR_Q_howe <- howe$spr_m3 * 1000
FISH_Q_howe <- howe$fish_m3 * 1000

# Store external P inputs/losses and event-specific constants used repeatedly
# inside the simulation loop.
#Constants
H_sprague <- sprague$H_P
S_sprague <- sprague$S_P

H_snow <- snow$H_P
S_snow <- snow$S_P

H_howe <- howe$H_P
S_howe <- howe$S_P

FISH_snow <- snow$fish_P

t_snow <- nrow(snow) # number of timesteps


# Run each Monte Carlo iteration in parallel. Each iteration independently
# simulates wildfire concentrations/deposition and snowmelt concentration and
# discharge effects, then propagates those changes through the mass balance.
all_outputs <- future_lapply(1:n_iter, function(i) {
  set.seed(123 + i)
  
  
  # PHASE 1: Sprague Fire -----------------------------------------------------
  # Draw elevated tributary TP concentrations and dry deposition, constrain the
  # spatial pattern of tributary responses, then decay values toward observed
  # concentrations over the Sprague Fire period.
  
  # Draw from truncated gamma distributions until Snyder and Sprague simulated
  # concentrations exceed those of UMC and Fish, reflecting greater assumed
  # wildfire influence at the tributaries closest to the fire.
  repeat {
    UMC1_sprague <- rtrunc(1, spec = "gamma", a = UMC_mean_tp*5, b = UMC_mean_tp*10000, shape = 2, scale = (40*UMC_mean_tp)/2)
    SNY1_sprague <- rtrunc(1, spec = "gamma", a = SNY_mean_tp*5, b = SNY_mean_tp*10000, shape = 2, scale = (40*SNY_mean_tp)/2)
    SPR1_sprague <- rtrunc(1, spec = "gamma", a = SPR_mean_tp*5, b = SPR_mean_tp*10000, shape = 2, scale = (40*SPR_mean_tp)/2)
    FISH1_sprague <- rtrunc(1, spec = "gamma", a = FISH_mean_tp*5, b = FISH_mean_tp*10000, shape = 2, scale = (40*FISH_mean_tp)/2)
    
    # Require Snyder and Sprague concentrations to exceed UMC and Fish because
    # of their assumed greater proximity/exposure to the Sprague Fire.
    if (SNY1_sprague > UMC1_sprague && SNY1_sprague > FISH1_sprague && SPR1_sprague > UMC1_sprague && SPR1_sprague > FISH1_sprague) break
  }
  
  # Draw an elevated dry-deposition value from a truncated gamma distribution.
  D1_sprague <- rtrunc(1, spec = "gamma", a = D_mean_P, b = D_mean_P*860, shape = 2, scale = (P_mean*D_mean_P)/2)
  
  # Decay simulated Sprague Fire concentrations and deposition toward the
  # observed values at the end of the event period.
  timesteps <- nrow(sprague)
  UMC_sprague  <- decay(UMC1_sprague, UMC_end_sprague <- sprague$umc_P_conc[timesteps], timesteps)
  SNY_sprague  <- decay(SNY1_sprague, SNY_end_sprague <- sprague$sny_P_conc[timesteps], timesteps)
  SPR_sprague  <- decay(SPR1_sprague, SPR_end_sprague <- sprague$spr_P_conc[timesteps], timesteps)
  FISH_sprague <- decay(FISH1_sprague, FISH_end_sprague <- sprague$fish_P_conc[timesteps], timesteps)
  D_sprague    <- decay(D1_sprague,  D_end_sprague <- sprague$TP_dry_kg[timesteps], timesteps)
  
  # Allocate lake-mass storage for the event. The extra element stores the
  # initial lake P mass before the first simulated weekly update.
  M <- numeric(timesteps + 1)
  M[1] <- M0
  
  # Propagate simulated tributary inputs, deposition, hydrologic inputs/losses,
  # sedimentation, and outflow through the weekly lake P mass balance.
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
  
  # PHASE 2: Winter null period -----------------------------------------------
  # Continue the nutrient budget through winter using observed/model-derived
  # baseline inputs and losses rather than an imposed event perturbation.
  
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
  
  # PHASE 3: Snowmelt ---------------------------------------------------------
  # Simulate snowmelt-related increases in tributary nutrient concentrations and
  # discharge, then propagate those changes through the lake P mass balance.
  
  # Draw peak snowmelt TP concentrations from truncated gamma distributions.
  UMC_peak <- rtrunc(1, "gamma", a=UMC_mean_tp, b=UMC_mean_tp*10000, shape=2, scale=(8*UMC_mean_tp)/2)
  SNY_peak <- rtrunc(1, "gamma", a=SNY_mean_tp, b=SNY_mean_tp*10000, shape=2, scale=(8*SNY_mean_tp)/2)
  SPR_peak <- rtrunc(1, "gamma", a=SPR_mean_tp, b=SPR_mean_tp*10000, shape=2, scale=(8*SPR_mean_tp)/2)
  
  # Scale the normalized hydrograph by each simulated peak concentration to
  # generate a time-varying snowmelt concentration series.
  UMC_snow <- UMC_Q_hydro * UMC_peak
  SNY_snow <- SNY_Q_hydro * SNY_peak
  SPR_snow <- SPR_Q_hydro * SPR_peak
  
  # Draw a discharge scalar and apply it to baseline tributary discharge to
  # represent uncertainty in snowmelt flow magnitude.
  Q_scalar <- rtrunc(1, "gamma", a=1, b=100, shape=2, scale=1)
  UMC_Q_snow <- snow$umc_m3 * 1000 * Q_scalar
  SNY_Q_snow <- snow$sny_m3 * 1000 * Q_scalar
  SPR_Q_snow <- snow$spr_m3 * 1000 * Q_scalar
  
  # Propagate simulated snowmelt inputs through the weekly P mass balance. The
  # initial mass is inherited from the end of the winter null period.
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
  
  # PHASE 4: Summer null period -----------------------------------------------
  # Continue the nutrient budget using baseline inputs and losses between the
  # snowmelt simulation and the Howe Ridge Fire period.
  
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
  
  # PHASE 5: Howe Ridge Fire --------------------------------------------------
  # Repeat the wildfire simulation structure used for the Sprague Fire: draw
  # elevated tributary concentrations and dry deposition, decay toward observed
  # endpoints, and propagate the changes through the lake P mass balance.
  
  n_howe <- nrow(howe)
  UMC1_howe <- rtrunc(1, spec = "gamma", a = UMC_mean_tp*5, b = UMC_mean_tp*10000, shape = 2, scale = (40*UMC_mean_tp)/2)
  SNY1_howe <- rtrunc(1, spec = "gamma", a = SNY_mean_tp*5, b = SNY_mean_tp*10000, shape = 2, scale = (40*SNY_mean_tp)/2)
  SPR1_howe <- rtrunc(1, spec = "gamma", a = SPR_mean_tp*5, b = SPR_mean_tp*10000, shape = 2, scale = (40*SPR_mean_tp)/2)
  FISH1_howe <- rtrunc(1, spec = "gamma", a = FISH_mean_tp*5, b = FISH_mean_tp*10000, shape = 2, scale = (40*FISH_mean_tp)/2)
  D1_howe <- rtrunc(1, spec = "gamma", a = D_mean_P, b = D_mean_P*860, shape = 2, scale = (P_mean*D_mean_P)/2)
  
  # Decay simulated concentrations and deposition toward values observed at the
  # end of the Howe Ridge Fire period.
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
  
  # Assemble outputs for the current Monte Carlo iteration.
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


# 6. Combine and save Monte Carlo outputs ------------------------------------
# Bind iteration-level results into four long-format output tables for lake P
# mass, tributary concentration, snowmelt discharge, and wildfire deposition.
# Extract each type of output from the results list

all_mass <- rbindlist(lapply(all_outputs, function(x) x[["mass"]]))
all_concentrations <- rbindlist(lapply(all_outputs, function(x) x[["concentration"]]))
all_discharges <- rbindlist(lapply(all_outputs, function(x) x[["discharge"]]))
all_deposition <- rbindlist(lapply(all_outputs, function(x) x[["deposition"]]))

# Write large simulation outputs to 2_incremental

fwrite(
  all_mass,
  here::here("2_incremental", "wildfire_simulation_mass.csv")
)

fwrite(
  all_concentrations,
  here::here("2_incremental", "wildfire_simulation_conc.csv")
)

fwrite(
  all_discharges,
  here::here("2_incremental", "wildfire_simulation_discharge.csv")
)

fwrite(
  all_deposition,
  here::here("2_incremental", "wildfire_simulation_deposition.csv")
)

# Plot----

mass_P <- all_mass
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


vol_col <- c(
  vol,
  nuts$lake_volume
)


preds_tp_1975 <- preds_tp_1975 %>%
  mutate(vol_col) %>%
  mutate(mean_tp = (kg_P_est*(10^9))/(vol*1000), 
         min_tp = (kg_P_est_lwr*(10^9))/(vol*1000), 
         max_tp = (kg_P_est_upr*(10^9))/(vol*1000))

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


ggplot(data = preds_tp_1975, aes(x = end_date)) +
  
  geom_ribbon(
    data = preds_tp_1975,
    aes(ymin = min_tp, ymax = max_tp),
    fill = "gray70",
    alpha = 0.7
  ) +
  
  geom_line(
    data = preds_tp_1975,
    aes(y = mean_tp),
    color = "gray30",
    linewidth = 0.25
  ) +
  
  geom_ribbon(
    data = nuts,
    aes(ymin = TP_conc_est_lwr, ymax = TP_conc_est_upr),
    fill = "peachpuff1",
    alpha = 0.7
  ) +
  
  geom_line(
    data = nuts,
    aes(y = TP_conc_est),
    color = "salmon1",
    linewidth = 0.5
  ) +
  
  geom_ribbon(
    data = TP_summary,
    aes(
      x = date,
      ymin = TP_conc_2.5,
      ymax = TP_conc_97.5
    ),
    fill = "darkred",
    alpha = 0.5
  ) +
  
  geom_ribbon(
    data = TP_summary,
    aes(
      x = date,
      ymin = TP_conc_min,
      ymax = TP_conc_max
    ),
    fill = "darkred",
    alpha = 0.2
  ) +
  
  geom_line(
    data = TP_summary,
    aes(
      x = date,
      y = TP_conc_mean
    ),
    color = "darkred",
    linewidth = 0.5
  ) +
  
  geom_errorbar(
    data = nuts,
    aes(
      ymin = min_tp,
      ymax = max_tp
    ),
    width = 0.1,
    color = "black"
  ) +
  
  geom_point(
    data = nuts,
    aes(y = mean_tp),
    color = "black",
    size = 1
  ) +
  
  labs(
    title = "A",
    x = "",
    y = expression(
      "TP Concentration (" * mu * "g-P L"^{-1} * ")"
    )
  ) +
  
  scale_x_date(
    limits = as.Date(c("1975-10-01", "2023-10-15")),
    breaks = x_breaks,
    labels = x_labels,
    expand = c(0, 0)
  ) +
  
  scale_x_break(
    breaks = as.Date(c("2005-10-01", "2007-10-16")),
    scales = 30
  ) +
  
  scale_y_log10(
    breaks = c(
      1, 10, 20, 50, 100, 250
    ),
    labels = scales::label_number()
  ) +
  
  coord_cartesian(
    ylim = c(0.5, 250)
  ) +
  
  panel_theme +
  
  theme(
    axis.text.x = element_blank(),
    legend.position = "horizontal"
  )



####Nitrogen Simulation----------------------

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

#Simulation

set.seed(123)

n_iter <- 10 # Number of iterations
plan(multisession, workers = parallel::detectCores() - 1) # Parallel processing

# Set output
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
DN_sprague <- sprague$DN_N


H_snow <- snow$H_N
S_snow <- snow$S_N
DN_snow <- snow$DN_N


H_howe <- howe$H_N
S_howe <- howe$S_N
DN_howe <- howe$DN_N


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
      W_sprague[t-1] + D_sprague[t-1] + H_sprague[t-1] - S_sprague[t-1] - DN_sprague[t-1] -
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
      null_winter$H_N[w - 1] -
      null_winter$S_N[w - 1] - 
      null_winter$DN_N[w - 1] - 
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
      FISH_snow[t - 1] + W_snow[t - 1] + D_snow[t - 1] + H_snow[t - 1] - S_snow[t - 1] - DN_snow[t - 1] - 
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
      null_summer$DN_N[s - 1] -
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
      DN_howe[t - 1] -
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

all_mass <- rbindlist(
  lapply(all_outputs, function(x) x[["mass"]])
)

all_concentrations <- rbindlist(
  lapply(all_outputs, function(x) x[["concentration"]])
)

all_discharges <- rbindlist(
  lapply(all_outputs, function(x) x[["discharge"]])
)

all_deposition <- rbindlist(
  lapply(all_outputs, function(x) x[["deposition"]])
)


# Save nitrogen simulation outputs to 2_incremental

fwrite(
  all_mass,
  here::here(
    "2_incremental",
    "wildfire_simulation_mass_N.csv"
  )
)

fwrite(
  all_concentrations,
  here::here(
    "2_incremental",
    "wildfire_simulation_conc_N.csv"
  )
)

fwrite(
  all_discharges,
  here::here(
    "2_incremental",
    "wildfire_simulation_discharge_N.csv"
  )
)

fwrite(
  all_deposition,
  here::here(
    "2_incremental",
    "wildfire_simulation_deposition_N.csv"
  )
)


# Lake concentration----

mass_N <- all_mass
mass_N$date <- as.Date(mass_N$date)

vol <- sim %>%
  select(end_date, lake_volume)%>%
  mutate(end_date = as.Date(end_date))

mass_N <- mass_N %>%
  left_join(vol, by = c("date" = "end_date")) %>%
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


# Predict concentrations from 1975 budgets----

# TN
vol <- 1491191000
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

nuts[1, "mean_tn"] <- NA

ggplot(
  nuts,
  aes(x = end_date)
) +
  
  # Modeled TN concentration
  geom_ribbon(
    aes(
      ymin = TN_conc_est_lwr,
      ymax = TN_conc_est_upr
    ),
    fill = "#cbc9e2"
  ) +
  
  geom_line(
    aes(y = TN_conc_est),
    color = "mediumpurple4",
    linewidth = 1
  ) +
  
  # Observed TN concentration
  geom_errorbar(
    aes(
      ymin = min_tn,
      ymax = max_tn
    ),
    width = 1,
    color = "black"
  ) +
  
  geom_point(
    aes(y = mean_tn),
    color = "black",
    size = 3
  ) +
  
  # Simulated TN concentration
  geom_ribbon(
    data = TN_summary,
    aes(
      x = date,
      ymin = TN_conc_2.5,
      ymax = TN_conc_97.5
    ),
    fill = "darkred",
    alpha = 0.5
  ) +
  
  geom_ribbon(
    data = TN_summary,
    aes(
      x = date,
      ymin = TN_conc_min,
      ymax = TN_conc_max
    ),
    fill = "darkred",
    alpha = 0.2
  ) +
  
  geom_line(
    data = TN_summary,
    aes(
      x = date,
      y = TN_conc_mean
    ),
    color = "darkred",
    linewidth = 0.5
  ) +
  
  # Labels
  labs(
    title = "B",
    x = "Date",
    y = expression(
      "TN Concentration (" * mu * "g-N L"^{-1} * ")"
    )
  ) +
  
  # Y-axis
  scale_y_log10(
    breaks = c(
      100,
      300,
      1000,
      3000,
      9000
    ),
    labels = scales::label_comma()
  ) +
  
  coord_cartesian(
    ylim = c(
      25,
      9000
    )
  ) +
  
  # X-axis
  scale_x_datetime(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  # Theme
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      size = 7,
      angle = 90,
      hjust = 1,
      vjust = 0.25
    ),
    axis.text.y = element_text(
      size = 7
    ),
    legend.position = "none"
  )

