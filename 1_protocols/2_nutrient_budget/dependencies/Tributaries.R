# ==============================================================================
# Estimate tributary nutrient loads to Lake McDonald
#
# Purpose:
#   1. Combine tributary discharge estimates with measured TN and TP chemistry.
#   2. Fit LOADEST models separately for each tributary.
#   3. Predict hourly TN and TP loads for water year 2008-2023.
#   4. Generate diagnostic and concentration time-series plots.
#   5. Compare reconstructed UMC nutrient concentrations with observations
#      using RMSE.
#
# Tributaries:
#   - Upper McDonald Creek (UMC)
#   - Snyder Creek (SNY)
#   - Fish Creek (FISH)
#   - Sprague Creek (SPR)
#
# ==============================================================================




# 1. Merge tributary discharge and chemistry ----------------------------------

# 1.1 Read tributary discharge estimates --------------------------------------

QtQo <- readr::read_csv(
  here(
    "2_incremental",
    "QtQo.csv"
  ),
  col_types = cols(
    dateTime = col_datetime(format = "")
  )
)

QtQo$dateTime <- as.POSIXct(
  QtQo$dateTime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

QtQo <- QtQo %>%
  rename(
    date_time = dateTime
  )

# 1.2 Read tributary chemistry data -------------------------------------------

chemdata_2022 <- read.csv(
  here(
    "0_data",
    "chemdata_2022.csv"
  ),
  header = TRUE,
  sep = ","
)

chemdata_2022$date <- as.POSIXct(
  chemdata_2022$date,
  format = "%Y-%m-%d"
)


chemdata_2023 <- read.csv(
  here(
    "0_data",
    "chemdata_2023.csv"
  ),
  header = TRUE,
  sep = ","
)

chemdata_2023$date <- as.POSIXct(
  chemdata_2023$date,
  format = "%Y-%m-%d"
)


# 1.3 Calculate mean TN and TP by date and site -------------------------------

mean_values <- chemdata_2022 %>%
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
    )
  )


# Ensure nutrient concentrations are numeric.

mean_values$mean_tn <- as.numeric(
  mean_values$mean_tn
)

mean_values$mean_tp <- as.numeric(
  mean_values$mean_tp
)


# Replace NaN values.
#
# TN NaN values are converted to NA.
# TP NaN values are assigned the detection limit of 1.5 ug/L.

mean_values <- mean_values %>%
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


# Remove Lake McDonald sampling sites; retain tributaries and outlet.

mean_values <- mean_values %>%
  filter(
    !site %in% c(
      "LM10",
      "LM5i",
      "LMHYPO"
    )
  )


# Define site ordering.

site_order <- c(
  "UMC",
  "Snyder",
  "Sprague",
  "LMC",
  "Fish"
)

mean_values$site <- factor(
  mean_values$site,
  levels = site_order
)


# Arrange observations by date and site.

mean_values <- mean_values %>%
  arrange(
    date,
    site
  )

mean_values <- data.frame(
  mean_values
)


# 1.4 Assign sampling times ----------------------------------------------------

# Sampling dates did not contain collection times.
# Add field-recorded sampling times so chemistry measurements can be matched
# to hourly discharge estimates.

new_times <- c(
  "19:00:00", "19:00:00", "19:00:00", "19:00:00", "20:00:00",
  "10:00:00", "10:00:00", "12:00:00", "13:00:00", "14:00:00",
  "14:00:00", "15:00:00", "16:00:00", "12:00:00", "08:00:00",
  "13:00:00", "14:00:00", "14:00:00", "15:00:00", "16:00:00",
  "11:00:00", "08:00:00", "12:00:00", "13:00:00", "14:00:00",
  "15:00:00", "16:00:00", "16:00:00", "17:00:00", "18:00:00",
  "12:00:00", "13:00:00", "13:00:00", "14:00:00", "16:00:00",
  "15:00:00", "16:00:00", "16:00:00", "17:00:00", "16:00:00",
  "12:00:00", "14:00:00", "12:00:00", "14:00:00", "15:00:00"
)


# Combine dates and collection times in Mountain Time, then convert to UTC.

mean_values <- mean_values %>%
  mutate(
    datetime_str = paste(
      format(
        date,
        "%Y-%m-%d"
      ),
      new_times
    ),
    date = as.POSIXct(
      datetime_str,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "America/Denver"
    )
  ) %>%
  select(
    -datetime_str
  ) %>%
  mutate(
    date = with_tz(
      date,
      tzone = "UTC"
    )
  )

print(
  mean_values
)

mean_values <- mean_values %>%
  rename(
    date_time = date
  )


# 1.5 Add chemistry to discharge record ---------------------------------------

# Upper McDonald Creek

UMC <- subset(
  mean_values,
  site == "UMC"
)

QtQo <- QtQo %>%
  left_join(
    UMC,
    by = "date_time"
  ) %>%
  rename(
    UMC_mean_tn = mean_tn,
    UMC_mean_tp = mean_tp
  )


# Snyder Creek

SNY <- subset(
  mean_values,
  site == "Snyder"
)

QtQo <- QtQo %>%
  left_join(
    SNY,
    by = "date_time"
  ) %>%
  rename(
    SNY_mean_tn = mean_tn,
    SNY_mean_tp = mean_tp
  )


# Sprague Creek

SPR <- subset(
  mean_values,
  site == "Sprague"
)

QtQo <- QtQo %>%
  left_join(
    SPR,
    by = "date_time"
  ) %>%
  rename(
    SPR_mean_tn = mean_tn,
    SPR_mean_tp = mean_tp
  )


# Lower McDonald Creek

LMC <- subset(
  mean_values,
  site == "LMC"
)

QtQo <- QtQo %>%
  left_join(
    LMC,
    by = "date_time"
  ) %>%
  rename(
    LMC_mean_tn = mean_tn,
    LMC_mean_tp = mean_tp
  )


# Fish Creek

FISH <- subset(
  mean_values,
  site == "Fish"
)

QtQo <- QtQo %>%
  left_join(
    FISH,
    by = "date_time"
  ) %>%
  rename(
    FISH_mean_tn = mean_tn,
    FISH_mean_tp = mean_tp
  )


# Remove duplicate site-identification columns created during joins.

QtQo <- QtQo %>%
  select(
    -site.x,
    -site.y,
    -site.x.x,
    -site.y.y,
    -site
  )


# Remove all objects except QtQo before running LOADEST models.

rm(
  list = setdiff(
    ls(),
    "QtQo"
  )
)


# 2. Estimate nutrient loads with LOADEST --------------------------------------

# LOADEST models are run separately for each tributary and nutrient.
#
# For each tributary:
#   1. Fit TN using mean discharge.
#   2. Fit TN using lower discharge.
#   3. Fit TN using upper discharge.
#   4. Fit TP using mean discharge.
#   5. Fit TP using lower discharge.
#   6. Fit TP using upper discharge.
#
# Predicted daily flux from predictSolute() is divided by 24 to obtain
# hourly nutrient load (kg/hr).


# 2.1 Upper McDonald Creek -----------------------------------------------------

# Metadata


meta <- metadata(
  constituent = "UMC_mean_tn",
  flow = "umc_discharge_m3_s",
  dates = "date_time",
  conc.units = "ug L^-1",
  flow.units = "cms",
  load.units = "kg",
  load.rate.units = "kg s^-1",
  site.name = "UMC",
  consti.name = "Total Nitrogen",
  site.id = "UMC"
)


# Separate calibration and prediction datasets.

intdat <- QtQo %>%
  select(
    date_time,
    umc_discharge_m3_s,
    umc_discharge_m3_s_lwr,
    umc_discharge_m3_s_upr,
    UMC_mean_tn,
    UMC_mean_tp
  )


# TN calibration data

regdat <- intdat %>%
  filter(
    !is.na(UMC_mean_tn)
  )


# Prediction datasets

estdat_mean <- intdat %>%
  select(
    date_time,
    umc_discharge_m3_s
  )

estdat_lwr <- intdat %>%
  select(
    date_time,
    umc_discharge_m3_s_lwr
  )

estdat_upr <- intdat %>%
  select(
    date_time,
    umc_discharge_m3_s_upr
  )


# Manually define the mMatrix class if it is missing.

setClass(
  "mMatrix",
  contains = "matrix"
)

# Reload smwrQW after defining mMatrix.

library(smwrQW)

# Check lcens class.

showClass(
  "lcens"
)


# ----- UMC TN: mean discharge -------------------------------------------------

tn_lr <- loadReg2(
  loadReg(
    UMC_mean_tn ~ model(1),
    data = regdat,
    flow = "umc_discharge_m3_s",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr <- predictSolute(
  tn_lr,
  "flux",
  estdat_mean,
  se.pred = TRUE
)

preds_tn_lr <- preds_tn_lr %>%
  mutate(
    umc_kg_N_hr = flux / 24
  )


# ----- UMC TN: lower discharge ------------------------------------------------

tn_lr_lwr <- loadReg2(
  loadReg(
    UMC_mean_tn ~ model(1),
    data = regdat,
    flow = "umc_discharge_m3_s_lwr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr_lwr <- predictSolute(
  tn_lr_lwr,
  "flux",
  estdat_lwr,
  se.pred = TRUE
)

preds_tn_lr_lwr <- preds_tn_lr_lwr %>%
  mutate(
    umc_kg_N_hr_lwr = flux / 24
  )


# ----- UMC TN: upper discharge ------------------------------------------------

tn_lr_upr <- loadReg2(
  loadReg(
    UMC_mean_tn ~ model(1),
    data = regdat,
    flow = "umc_discharge_m3_s_upr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr_upr <- predictSolute(
  tn_lr_upr,
  "flux",
  estdat_upr,
  se.pred = TRUE
)

preds_tn_lr_upr <- preds_tn_lr_upr %>%
  mutate(
    umc_kg_N_hr_upr = flux / 24
  )


# Combine UMC TN estimates.

tn_umc <- preds_tn_lr %>%
  inner_join(
    preds_tn_lr_lwr,
    by = "date"
  ) %>%
  inner_join(
    preds_tn_lr_upr,
    by = "date"
  )


# Check for duplicate dates.

preds_tn_lr %>%
  dplyr::count(date) %>%
  dplyr::filter(n > 1) %>%
  dplyr::arrange(dplyr::desc(n))

preds_tn_lr_lwr %>%
  dplyr::count(date) %>%
  dplyr::filter(n > 1) %>%
  dplyr::arrange(dplyr::desc(n))

preds_tn_lr_upr %>%
  dplyr::count(date) %>%
  dplyr::filter(n > 1) %>%
  dplyr::arrange(dplyr::desc(n))


# ----- UMC TP -----------------------------------------------------------------

# Use observations with measured TP.

regdat <- intdat %>%
  filter(
    !is.na(UMC_mean_tp)
  )


# UMC TP: mean discharge

tp_lr <- loadReg2(
  loadReg(
    UMC_mean_tp ~ model(1),
    data = regdat,
    flow = "umc_discharge_m3_s",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr <- predictSolute(
  tp_lr,
  "flux",
  estdat_mean,
  se.pred = TRUE
)

preds_tp_lr <- preds_tp_lr %>%
  mutate(
    umc_kg_P_hr = flux / 24
  )


# UMC TP: lower discharge

tp_lr_lwr <- loadReg2(
  loadReg(
    UMC_mean_tp ~ model(1),
    data = regdat,
    flow = "umc_discharge_m3_s_lwr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr_lwr <- predictSolute(
  tp_lr_lwr,
  "flux",
  estdat_lwr,
  se.pred = TRUE
)

preds_tp_lr_lwr <- preds_tp_lr_lwr %>%
  mutate(
    umc_kg_P_hr_lwr = flux / 24
  )


# UMC TP: upper discharge

tp_lr_upr <- loadReg2(
  loadReg(
    UMC_mean_tp ~ model(1),
    data = regdat,
    flow = "umc_discharge_m3_s_upr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr_upr <- predictSolute(
  tp_lr_upr,
  "flux",
  estdat_upr,
  se.pred = TRUE
)

preds_tp_lr_upr <- preds_tp_lr_upr %>%
  mutate(
    umc_kg_P_hr_upr = flux / 24
  )


# Combine UMC TP estimates.

tp_umc <- preds_tp_lr %>%
  inner_join(
    preds_tp_lr_lwr,
    by = "date"
  ) %>%
  inner_join(
    preds_tp_lr_upr,
    by = "date"
  )


# Combine UMC TN and TP estimates.

tribs_nut <- tn_umc %>%
  inner_join(
    tp_umc,
    by = "date"
  ) %>%
  select(
    date,
    umc_kg_N_hr,
    umc_kg_N_hr_lwr,
    umc_kg_N_hr_upr,
    umc_kg_P_hr,
    umc_kg_P_hr_lwr,
    umc_kg_P_hr_upr
  )


# Remove temporary UMC model objects.

rm(meta)
rm(intdat)
rm(estdat_mean)
rm(estdat_lwr)
rm(estdat_upr)
rm(regdat)

rm(tn_lr)
rm(preds_tn_lr)
rm(tn_lr_lwr)
rm(preds_tn_lr_lwr)
rm(tn_lr_upr)
rm(preds_tn_lr_upr)

rm(tp_lr)
rm(preds_tp_lr)
rm(tp_lr_lwr)
rm(preds_tp_lr_lwr)
rm(tp_lr_upr)
rm(preds_tp_lr_upr)

rm(tn_umc)
rm(tp_umc)

# 2.2 Snyder Creek -------------------------------------------------------------

# Manually define mMatrix class if needed

setClass(
  "mMatrix",
  contains = "matrix"
)

library(smwrQW)

showClass(
  "lcens"
)


# Metadata

meta <- metadata(
  constituent = "SNY_mean_tn",
  flow = "sny_discharge_m3_s",
  dates = "date_time",
  conc.units = "ug L^-1",
  flow.units = "cms",
  load.units = "kg",
  load.rate.units = "kg s^-1",
  site.name = "Snyder",
  consti.name = "Total Nitrogen, Total Phosphorus",
  site.id = "SNY"
)


# Separate calibration and prediction datasets

intdat <- QtQo %>%
  select(
    date_time,
    sny_discharge_m3_s,
    sny_discharge_m3_s_lwr,
    sny_discharge_m3_s_upr,
    SNY_mean_tn,
    SNY_mean_tp
  )


# TN calibration data

regdat <- intdat %>%
  filter(
    !is.na(SNY_mean_tn)
  )


# Prediction datasets

estdat_mean <- intdat %>%
  select(
    date_time,
    sny_discharge_m3_s
  )

estdat_lwr <- intdat %>%
  select(
    date_time,
    sny_discharge_m3_s_lwr
  )

estdat_upr <- intdat %>%
  select(
    date_time,
    sny_discharge_m3_s_upr
  )


# ----- TN: mean ---------------------------------------------------------------

tn_lr <- loadReg2(
  loadReg(
    SNY_mean_tn ~ model(1),
    data = regdat,
    flow = "sny_discharge_m3_s",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr <- predictSolute(
  tn_lr,
  "flux",
  estdat_mean,
  se.pred = TRUE
)

preds_tn_lr <- preds_tn_lr %>%
  mutate(
    sny_kg_N_hr = flux / 24
  )


# ----- TN: lower --------------------------------------------------------------

tn_lr_lwr <- loadReg2(
  loadReg(
    SNY_mean_tn ~ model(1),
    data = regdat,
    flow = "sny_discharge_m3_s_lwr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr_lwr <- predictSolute(
  tn_lr_lwr,
  "flux",
  estdat_lwr,
  se.pred = TRUE
)

preds_tn_lr_lwr <- preds_tn_lr_lwr %>%
  mutate(
    sny_kg_N_hr_lwr = flux / 24
  )


# ----- TN: upper --------------------------------------------------------------

tn_lr_upr <- loadReg2(
  loadReg(
    SNY_mean_tn ~ model(1),
    data = regdat,
    flow = "sny_discharge_m3_s_upr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr_upr <- predictSolute(
  tn_lr_upr,
  "flux",
  estdat_upr,
  se.pred = TRUE
)

preds_tn_lr_upr <- preds_tn_lr_upr %>%
  mutate(
    sny_kg_N_hr_upr = flux / 24
  )


# Combine TN estimates

tn_sny <- preds_tn_lr %>%
  inner_join(
    preds_tn_lr_lwr,
    by = "date"
  ) %>%
  inner_join(
    preds_tn_lr_upr,
    by = "date"
  )


# ----- TP: mean ---------------------------------------------------------------

tp_lr <- loadReg2(
  loadReg(
    SNY_mean_tp ~ model(1),
    data = regdat,
    flow = "sny_discharge_m3_s",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr <- predictSolute(
  tp_lr,
  "flux",
  estdat_mean,
  se.pred = TRUE
)

preds_tp_lr <- preds_tp_lr %>%
  mutate(
    sny_kg_P_hr = flux / 24
  )


# ----- TP: lower --------------------------------------------------------------

tp_lr_lwr <- loadReg2(
  loadReg(
    SNY_mean_tp ~ model(1),
    data = regdat,
    flow = "sny_discharge_m3_s_lwr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr_lwr <- predictSolute(
  tp_lr_lwr,
  "flux",
  estdat_lwr,
  se.pred = TRUE
)

preds_tp_lr_lwr <- preds_tp_lr_lwr %>%
  mutate(
    sny_kg_P_hr_lwr = flux / 24
  )


# ----- TP: upper --------------------------------------------------------------

tp_lr_upr <- loadReg2(
  loadReg(
    SNY_mean_tp ~ model(1),
    data = regdat,
    flow = "sny_discharge_m3_s_upr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr_upr <- predictSolute(
  tp_lr_upr,
  "flux",
  estdat_upr,
  se.pred = TRUE
)

preds_tp_lr_upr <- preds_tp_lr_upr %>%
  mutate(
    sny_kg_P_hr_upr = flux / 24
  )


# Combine TP estimates

tp_sny <- preds_tp_lr %>%
  inner_join(
    preds_tp_lr_lwr,
    by = "date"
  ) %>%
  inner_join(
    preds_tp_lr_upr,
    by = "date"
  )


# Add Snyder TN and TP to tributary nutrient dataset

tribs_nut <- tribs_nut %>%
  inner_join(
    tn_sny,
    by = "date"
  ) %>%
  inner_join(
    tp_sny,
    by = "date"
  ) %>%
  select(
    date,
    
    umc_kg_N_hr,
    umc_kg_N_hr_lwr,
    umc_kg_N_hr_upr,
    umc_kg_P_hr,
    umc_kg_P_hr_lwr,
    umc_kg_P_hr_upr,
    
    sny_kg_N_hr,
    sny_kg_N_hr_lwr,
    sny_kg_N_hr_upr,
    sny_kg_P_hr,
    sny_kg_P_hr_lwr,
    sny_kg_P_hr_upr
  )


# Remove temporary objects

rm(meta)
rm(intdat)
rm(estdat_mean)
rm(estdat_lwr)
rm(estdat_upr)
rm(regdat)

rm(tn_lr)
rm(preds_tn_lr)
rm(tn_lr_lwr)
rm(preds_tn_lr_lwr)
rm(tn_lr_upr)
rm(preds_tn_lr_upr)

rm(tp_lr)
rm(preds_tp_lr)
rm(tp_lr_lwr)
rm(preds_tp_lr_lwr)
rm(tp_lr_upr)
rm(preds_tp_lr_upr)

rm(tn_sny)
rm(tp_sny)

# 2.3 Fish Creek ---------------------------------------------------------------

# Manually define mMatrix class if needed

setClass(
  "mMatrix",
  contains = "matrix"
)

library(smwrQW)

showClass(
  "lcens"
)


# Metadata

meta <- metadata(
  constituent = "FISH_mean_tn",
  flow = "fish_discharge_m3_s",
  dates = "date_time",
  conc.units = "ug L^-1",
  flow.units = "cms",
  load.units = "kg",
  load.rate.units = "kg s^-1",
  site.name = "Sprague",
  consti.name = "Total Nitrogen, Total Phosphorus",
  site.id = "FISH"
)


# Separate calibration and prediction datasets

intdat <- QtQo %>%
  select(
    date_time,
    fish_discharge_m3_s,
    fish_discharge_m3_s_lwr,
    fish_discharge_m3_s_upr,
    FISH_mean_tn,
    FISH_mean_tp
  )


# TN calibration data

regdat <- intdat %>%
  filter(
    !is.na(FISH_mean_tn)
  )


# Prediction datasets

estdat_mean <- intdat %>%
  select(
    date_time,
    fish_discharge_m3_s
  )

estdat_lwr <- intdat %>%
  select(
    date_time,
    fish_discharge_m3_s_lwr
  )

estdat_upr <- intdat %>%
  select(
    date_time,
    fish_discharge_m3_s_upr
  )


# ----- TN: mean ---------------------------------------------------------------

tn_lr <- loadReg2(
  loadReg(
    FISH_mean_tn ~ model(1),
    data = regdat,
    flow = "fish_discharge_m3_s",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr <- predictSolute(
  tn_lr,
  "flux",
  estdat_mean,
  se.pred = TRUE
)

preds_tn_lr <- preds_tn_lr %>%
  mutate(
    fish_kg_N_hr = flux / 24
  )


# ----- TN: lower --------------------------------------------------------------

tn_lr_lwr <- loadReg2(
  loadReg(
    FISH_mean_tn ~ model(1),
    data = regdat,
    flow = "fish_discharge_m3_s_lwr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr_lwr <- predictSolute(
  tn_lr_lwr,
  "flux",
  estdat_lwr,
  se.pred = TRUE
)

preds_tn_lr_lwr <- preds_tn_lr_lwr %>%
  mutate(
    fish_kg_N_hr_lwr = flux / 24
  )


# ----- TN: upper --------------------------------------------------------------

tn_lr_upr <- loadReg2(
  loadReg(
    FISH_mean_tn ~ model(1),
    data = regdat,
    flow = "fish_discharge_m3_s_upr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr_upr <- predictSolute(
  tn_lr_upr,
  "flux",
  estdat_upr,
  se.pred = TRUE
)

preds_tn_lr_upr <- preds_tn_lr_upr %>%
  mutate(
    fish_kg_N_hr_upr = flux / 24
  )


# Combine TN estimates

tn_fish <- preds_tn_lr %>%
  inner_join(
    preds_tn_lr_lwr,
    by = "date"
  ) %>%
  inner_join(
    preds_tn_lr_upr,
    by = "date"
  )


# ----- TP: mean ---------------------------------------------------------------

tp_lr <- loadReg2(
  loadReg(
    FISH_mean_tp ~ model(1),
    data = regdat,
    flow = "fish_discharge_m3_s",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr <- predictSolute(
  tp_lr,
  "flux",
  estdat_mean,
  se.pred = TRUE
)

preds_tp_lr <- preds_tp_lr %>%
  mutate(
    fish_kg_P_hr = flux / 24
  )


# ----- TP: lower --------------------------------------------------------------

tp_lr_lwr <- loadReg2(
  loadReg(
    FISH_mean_tp ~ model(1),
    data = regdat,
    flow = "fish_discharge_m3_s_lwr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr_lwr <- predictSolute(
  tp_lr_lwr,
  "flux",
  estdat_lwr,
  se.pred = TRUE
)

preds_tp_lr_lwr <- preds_tp_lr_lwr %>%
  mutate(
    fish_kg_P_hr_lwr = flux / 24
  )


# ----- TP: upper --------------------------------------------------------------

tp_lr_upr <- loadReg2(
  loadReg(
    FISH_mean_tp ~ model(1),
    data = regdat,
    flow = "fish_discharge_m3_s_upr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr_upr <- predictSolute(
  tp_lr_upr,
  "flux",
  estdat_upr,
  se.pred = TRUE
)

preds_tp_lr_upr <- preds_tp_lr_upr %>%
  mutate(
    fish_kg_P_hr_upr = flux / 24
  )


# Combine TP estimates

tp_fish <- preds_tp_lr %>%
  inner_join(
    preds_tp_lr_lwr,
    by = "date"
  ) %>%
  inner_join(
    preds_tp_lr_upr,
    by = "date"
  )


# Add Fish Creek TN and TP to tributary nutrient dataset

tribs_nut <- tribs_nut %>%
  inner_join(
    tn_fish,
    by = "date"
  ) %>%
  inner_join(
    tp_fish,
    by = "date"
  ) %>%
  select(
    date,
    
    umc_kg_N_hr,
    umc_kg_N_hr_lwr,
    umc_kg_N_hr_upr,
    umc_kg_P_hr,
    umc_kg_P_hr_lwr,
    umc_kg_P_hr_upr,
    
    sny_kg_N_hr,
    sny_kg_N_hr_lwr,
    sny_kg_N_hr_upr,
    sny_kg_P_hr,
    sny_kg_P_hr_lwr,
    sny_kg_P_hr_upr,
    
    fish_kg_N_hr,
    fish_kg_N_hr_lwr,
    fish_kg_N_hr_upr,
    fish_kg_P_hr,
    fish_kg_P_hr_lwr,
    fish_kg_P_hr_upr
  )


# Remove temporary objects

rm(meta)
rm(intdat)
rm(estdat_mean)
rm(estdat_lwr)
rm(estdat_upr)
rm(regdat)

rm(tn_lr)
rm(preds_tn_lr)
rm(tn_lr_lwr)
rm(preds_tn_lr_lwr)
rm(tn_lr_upr)
rm(preds_tn_lr_upr)

rm(tp_lr)
rm(preds_tp_lr)
rm(tp_lr_lwr)
rm(preds_tp_lr_lwr)
rm(tp_lr_upr)
rm(preds_tp_lr_upr)

rm(tn_fish)
rm(tp_fish)


# 2.4 Sprague Creek ------------------------------------------------------------

# Manually define mMatrix class if needed

setClass(
  "mMatrix",
  contains = "matrix"
)

library(smwrQW)

showClass(
  "lcens"
)


# Metadata

meta <- metadata(
  constituent = "SPR_mean_tn",
  flow = "spr_discharge_m3_s",
  dates = "date_time",
  conc.units = "ug L^-1",
  flow.units = "cms",
  load.units = "kg",
  load.rate.units = "kg s^-1",
  site.name = "Sprague",
  consti.name = "Total Nitrogen, Total Phosphorus",
  site.id = "SPR"
)


# Separate calibration and prediction datasets

intdat <- QtQo %>%
  select(
    date_time,
    spr_discharge_m3_s,
    spr_discharge_m3_s_lwr,
    spr_discharge_m3_s_upr,
    SPR_mean_tn,
    SPR_mean_tp
  )


# TN calibration data

regdat <- intdat %>%
  filter(
    !is.na(SPR_mean_tn)
  )


# Prediction datasets

estdat_mean <- intdat %>%
  select(
    date_time,
    spr_discharge_m3_s
  )

estdat_lwr <- intdat %>%
  select(
    date_time,
    spr_discharge_m3_s_lwr
  )

estdat_upr <- intdat %>%
  select(
    date_time,
    spr_discharge_m3_s_upr
  )


# ----- TN: mean ---------------------------------------------------------------

tn_lr <- loadReg2(
  loadReg(
    SPR_mean_tn ~ model(1),
    data = regdat,
    flow = "spr_discharge_m3_s",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr <- predictSolute(
  tn_lr,
  "flux",
  estdat_mean,
  se.pred = TRUE
)

preds_tn_lr <- preds_tn_lr %>%
  mutate(
    spr_kg_N_hr = flux / 24
  )


# ----- TN: lower --------------------------------------------------------------

tn_lr_lwr <- loadReg2(
  loadReg(
    SPR_mean_tn ~ model(1),
    data = regdat,
    flow = "spr_discharge_m3_s_lwr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr_lwr <- predictSolute(
  tn_lr_lwr,
  "flux",
  estdat_lwr,
  se.pred = TRUE
)

preds_tn_lr_lwr <- preds_tn_lr_lwr %>%
  mutate(
    spr_kg_N_hr_lwr = flux / 24
  )


# ----- TN: upper --------------------------------------------------------------

tn_lr_upr <- loadReg2(
  loadReg(
    SPR_mean_tn ~ model(1),
    data = regdat,
    flow = "spr_discharge_m3_s_upr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tn_lr_upr <- predictSolute(
  tn_lr_upr,
  "flux",
  estdat_upr,
  se.pred = TRUE
)

preds_tn_lr_upr <- preds_tn_lr_upr %>%
  mutate(
    spr_kg_N_hr_upr = flux / 24
  )


# Combine TN estimates

tn_spr <- preds_tn_lr %>%
  inner_join(
    preds_tn_lr_lwr,
    by = "date"
  ) %>%
  inner_join(
    preds_tn_lr_upr,
    by = "date"
  )


# ----- TP: mean ---------------------------------------------------------------

tp_lr <- loadReg2(
  loadReg(
    SPR_mean_tp ~ model(1),
    data = regdat,
    flow = "spr_discharge_m3_s",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr <- predictSolute(
  tp_lr,
  "flux",
  estdat_mean,
  se.pred = TRUE
)

preds_tp_lr <- preds_tp_lr %>%
  mutate(
    spr_kg_P_hr = flux / 24
  )


# ----- TP: lower --------------------------------------------------------------

tp_lr_lwr <- loadReg2(
  loadReg(
    SPR_mean_tp ~ model(1),
    data = regdat,
    flow = "spr_discharge_m3_s_lwr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr_lwr <- predictSolute(
  tp_lr_lwr,
  "flux",
  estdat_lwr,
  se.pred = TRUE
)

preds_tp_lr_lwr <- preds_tp_lr_lwr %>%
  mutate(
    spr_kg_P_hr_lwr = flux / 24
  )


# ----- TP: upper --------------------------------------------------------------

tp_lr_upr <- loadReg2(
  loadReg(
    SPR_mean_tp ~ model(1),
    data = regdat,
    flow = "spr_discharge_m3_s_upr",
    dates = "date_time",
    flow.units = "cms",
    conc.units = "ug/L",
    time.step = "instantaneous"
  )
)

preds_tp_lr_upr <- predictSolute(
  tp_lr_upr,
  "flux",
  estdat_upr,
  se.pred = TRUE
)

preds_tp_lr_upr <- preds_tp_lr_upr %>%
  mutate(
    spr_kg_P_hr_upr = flux / 24
  )


# Combine TP estimates

tp_spr <- preds_tp_lr %>%
  inner_join(
    preds_tp_lr_lwr,
    by = "date"
  ) %>%
  inner_join(
    preds_tp_lr_upr,
    by = "date"
  )


# Add Sprague Creek TN and TP to tributary nutrient dataset

tribs_nut <- tribs_nut %>%
  inner_join(
    tn_spr,
    by = "date"
  ) %>%
  inner_join(
    tp_spr,
    by = "date"
  ) %>%
  select(
    date,
    
    umc_kg_N_hr,
    umc_kg_N_hr_lwr,
    umc_kg_N_hr_upr,
    umc_kg_P_hr,
    umc_kg_P_hr_lwr,
    umc_kg_P_hr_upr,
    
    sny_kg_N_hr,
    sny_kg_N_hr_lwr,
    sny_kg_N_hr_upr,
    sny_kg_P_hr,
    sny_kg_P_hr_lwr,
    sny_kg_P_hr_upr,
    
    fish_kg_N_hr,
    fish_kg_N_hr_lwr,
    fish_kg_N_hr_upr,
    fish_kg_P_hr,
    fish_kg_P_hr_lwr,
    fish_kg_P_hr_upr,
    
    spr_kg_N_hr,
    spr_kg_N_hr_lwr,
    spr_kg_N_hr_upr,
    spr_kg_P_hr,
    spr_kg_P_hr_lwr,
    spr_kg_P_hr_upr
  ) %>%
  rename(
    date_time = date
  )


# Add discharge variables back to nutrient-load dataset

tribs_nut <- tribs_nut %>%
  inner_join(
    QtQo,
    by = "date_time"
  )


# Save hourly tributary nutrient loads -----------------------------------------

write.csv(
  tribs_nut,
  here(
    "2_incremental",
    "Ft.csv"
  ),
  row.names = FALSE
)

# Plots---------------------------- 

# Calibration data-----
ggplot(data = QtQo, aes(x = umc_discharge_m3_s)) + 
  geom_point(aes(y = UMC_mean_tp, color = "[Total Phosphorus]"), size = 1) +
  geom_point(aes(y = UMC_mean_tn, color = "[Total Nitrogen]"), size = 1) +
  theme_classic() +
  labs(
    title = "A",
    x = expression("Discharge (" * m^3 * " s"^-1 * ")"),
    y = expression("Concentration (" * mu * "g " * L^-1 * ")")
  ) + scale_color_manual(name = "Parameter", values = c("[Total Phosphorus]" = "salmon1", "[Total Nitrogen]" = "mediumpurple4")) + 
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 20)
  ) + theme(legend.position = "none") + scale_x_log10() + scale_y_log10() 


ggplot(data = QtQo, aes(x = sny_discharge_m3_s)) + 
  geom_point(aes(y = SNY_mean_tp, color = "[Total Phosphorus]"), size = 1) +
  geom_point(aes(y = SNY_mean_tn, color = "[Total Nitrogen]"), size = 1) +
  theme_classic() +
  labs(
    title = "B",
    x = expression("Discharge (" * m^3 * " s"^-1 * ")"),
    y = expression("Concentration (" * mu * "g " * L^-1 * ")")
  ) + scale_color_manual(name = "Parameter", values = c("[Total Phosphorus]" = "salmon1", "[Total Nitrogen]" = "mediumpurple4")) + 
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 20)
  ) + theme(legend.position = "none") + scale_x_log10() + scale_y_log10() 

ggplot(data = QtQo, aes(x = spr_discharge_m3_s)) + 
  geom_point(aes(y = SPR_mean_tp, color = "[Total Phosphorus]"), size = 1) +
  geom_point(aes(y = SPR_mean_tn, color = "[Total Nitrogen]"), size = 1) +
  theme_classic() +
  labs(
    title = "C",
    x = expression("Discharge (" * m^3 * " s"^-1 * ")"),
    y = expression("Concentration (" * mu * "g " * L^-1 * ")")
  ) + scale_color_manual(name = "Parameter", values = c("[Total Phosphorus]" = "salmon1", "[Total Nitrogen]" = "mediumpurple4")) + 
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 20)
  ) + theme(legend.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal") + scale_x_log10() + scale_y_log10() 


ggplot(data = QtQo, aes(x = fish_discharge_m3_s)) + 
  geom_point(aes(y = FISH_mean_tp, color = "[Total Phosphorus]"), size = 1) +
  geom_point(aes(y = FISH_mean_tn, color = "[Total Nitrogen]"), size = 1) +
  theme_classic() +
  labs(
    title = "D",
    x = expression("Discharge (" * m^3 * " s"^-1 * ")"),
    y = expression("Concentration (" * mu * "g " * L^-1 * ")")
  ) + scale_color_manual(name = "Parameter", values = c("[Total Phosphorus]" = "salmon1", "[Total Nitrogen]" = "mediumpurple4")) + 
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 20)
  ) + theme(legend.position = "none") +  scale_x_log10() + scale_y_log10()

# Simulated data----

ggplot(data = tribs_nut, aes(x = date_time)) + 
  geom_ribbon(
    aes(
      ymin = (umc_kg_N_hr_lwr * 1e9) / (umc_discharge_m3_s_lwr * 1000 * 3600),
      ymax = (umc_kg_N_hr_upr * 1e9) / (umc_discharge_m3_s_upr * 1000 * 3600)
    ),
    fill = "mediumpurple4",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = (umc_kg_N_hr * 1e9) / (umc_discharge_m3_s * 1000 * 3600)),
    color = "mediumpurple4", linewidth = 0.25
  ) +
  geom_ribbon(
    aes(
      ymin = (umc_kg_P_hr_lwr * 1e9) / (umc_discharge_m3_s_lwr * 1000 * 3600),
      ymax = (umc_kg_P_hr_upr * 1e9) / (umc_discharge_m3_s_upr * 1000 * 3600)
    ),
    fill = "salmon1",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = (umc_kg_P_hr * 1e9) / (umc_discharge_m3_s * 1000 * 3600)),
    color = "salmon1", linewidth = 0.25
  ) +
  labs(
    title = "",
    y = bquote(Concentration~"(" * mu * "g L"^{-1} * ")"),
    x = "Date"
  ) +
  theme_classic() +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%Y") +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
    axis.ticks.x = element_line(),
    plot.title = element_text(size = 20)
  ) + scale_y_log10(breaks = c(1, 10, 100, 200))


ggplot(data = tribs_nut, aes(x = date_time)) + 
  geom_ribbon(
    aes(
      ymin = (sny_kg_N_hr_lwr * 1e9) / (sny_discharge_m3_s_lwr * 1000 * 3600),
      ymax = (sny_kg_N_hr_upr * 1e9) / (sny_discharge_m3_s_upr * 1000 * 3600)
    ),
    fill = "mediumpurple4",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = (sny_kg_N_hr * 1e9) / (sny_discharge_m3_s * 1000 * 3600)),
    color = "mediumpurple4", linewidth = 0.25
  ) +
  geom_ribbon(
    aes(
      ymin = (sny_kg_P_hr_lwr * 1e9) / (sny_discharge_m3_s_lwr * 1000 * 3600),
      ymax = (sny_kg_P_hr_upr * 1e9) / (sny_discharge_m3_s_upr * 1000 * 3600)
    ),
    fill = "salmon1",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = (sny_kg_P_hr * 1e9) / (sny_discharge_m3_s * 1000 * 3600)),
    color = "salmon1", linewidth = 0.25
  ) +
  labs(
    title = "",
    y = bquote(Concentration~"(" * mu * "g L"^{-1} * ")"),
    x = "Date"
  ) +
  theme_classic() +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%Y") +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
    axis.ticks.x = element_line(),
    plot.title = element_text(size = 20)
  ) + scale_y_log10(breaks = c(1, 10, 100, 200, 400))


ggplot(data = tribs_nut, aes(x = date_time)) + 
  geom_ribbon(
    aes(
      ymin = (spr_kg_N_hr_lwr * 1e9) / (spr_discharge_m3_s_lwr * 1000 * 3600),
      ymax = (spr_kg_N_hr_upr * 1e9) / (spr_discharge_m3_s_upr * 1000 * 3600)
    ),
    fill = "mediumpurple4",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = (spr_kg_N_hr * 1e9) / (spr_discharge_m3_s * 1000 * 3600)),
    color = "mediumpurple4", linewidth = 0.25
  ) +
  geom_ribbon(
    aes(
      ymin = (spr_kg_P_hr_lwr * 1e9) / (spr_discharge_m3_s_lwr * 1000 * 3600),
      ymax = (spr_kg_P_hr_upr * 1e9) / (spr_discharge_m3_s_upr * 1000 * 3600)
    ),
    fill = "salmon1",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = (spr_kg_P_hr * 1e9) / (spr_discharge_m3_s * 1000 * 3600)),
    color = "salmon1", linewidth = 0.25
  ) +
  labs(
    title = "",
    y = bquote(Concentration~"(" * mu * "g L"^{-1} * ")"),
    x = "Date"
  ) +
  theme_classic() +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%Y") +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
    axis.ticks.x = element_line(),
    plot.title = element_text(size = 20)
  ) + scale_y_log10(breaks = c(1, 10, 100, 200))


ggplot(data = tribs_nut, aes(x = date_time)) + 
  geom_ribbon(
    aes(
      ymin = (fish_kg_N_hr_lwr * 1e9) / (fish_discharge_m3_s_lwr * 1000 * 3600),
      ymax = (fish_kg_N_hr_upr * 1e9) / (fish_discharge_m3_s_upr * 1000 * 3600)
    ),
    fill = "mediumpurple4",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = (fish_kg_N_hr * 1e9) / (fish_discharge_m3_s * 1000 * 3600)),
    color = "mediumpurple4", linewidth = 0.25
  ) +
  geom_ribbon(
    aes(
      ymin = (fish_kg_P_hr_lwr * 1e9) / (fish_discharge_m3_s_lwr * 1000 * 3600),
      ymax = (fish_kg_P_hr_upr * 1e9) / (fish_discharge_m3_s_upr * 1000 * 3600)
    ),
    fill = "salmon1",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = (fish_kg_P_hr * 1e9) / (fish_discharge_m3_s * 1000 * 3600)),
    color = "salmon1", linewidth = 0.25
  ) +
  labs(
    title = "",
    y = bquote(Concentration~"(" * mu * "g L"^{-1} * ")"),
    x = "Date"
  ) +
  theme_classic() +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%Y") +
  theme(
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
    axis.ticks.x = element_line(),
    plot.title = element_text(size = 20)
  ) + scale_y_log10(breaks = c(1, 10, 100, 200))
