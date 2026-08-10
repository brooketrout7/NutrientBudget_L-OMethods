
#0. Load packages----

# Install packages if they are not already installed
library(remotes)
remotes::install_gitlab("water/analysis-tools/smwrData", host = "code.usgs.gov")
remotes::install_gitlab("water/analysis-tools/smwrBase", host = "code.usgs.gov")
remotes::install_gitlab("water/analysis-tools/smwrGraphs", host = "code.usgs.gov")
remotes::install_gitlab("water/analysis-tools/smwrStats", host = "code.usgs.gov") # needs compilation
remotes::install_gitlab("water/analysis-tools/smwrQW", host = "code.usgs.gov")    # needs compilation
remotes::install_github("appling/unitted")
remotes::install_github("DOI-USGS/EGRET")
remotes::install_github("USGS-R/rloadest")
remotes::install_github("USGS-R/loadflex") # soon to be "DOI-USGS/loadflex"

#install.packages("smwrStats")
library(methods)
library(NADA)
#library(smwrQW)
#library(smwrStats)
library(loadflex)
library(rloadest)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(rlang)
library(patchwork)
library(tidyr)
library(knitr)
options(scipen = 999)


#1. Merge discharge and chemistry ----

# Read in data, create POSIXct object, and rename columns

tribs_out_WY_2008_2023 <- readr::read_csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\tribs_out_WY_2008_2023.csv",col_types = cols(dateTime = col_datetime(format = "")))

tribs_out_WY_2008_2023$dateTime <- as.POSIXct(tribs_out_WY_2008_2023$dateTime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
summary(tribs_out_WY_2008_2023)

tribs_out <- tribs_out_WY_2008_2023 %>%
  rename(date_time = dateTime)

summary(tribs_out)

chemdata_2022 <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\chemdata_2022.csv', header = TRUE, sep = ",")

chemdata_2022$date <- as.POSIXct(chemdata_2022$date, format = "%Y-%m-%d")

chemdata_2023 <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\chemdata_2023.csv', header = TRUE, sep = ",")

chemdata_2023$date <- as.POSIXct(chemdata_2023$date, format = "%Y-%m-%d")

# Group by 'date' and 'site' and calculate the mean for 'tn' and 'tp'

mean_values <- chemdata_2022 %>%
  group_by(date, site) %>%
  summarise(
    mean_tn = mean(tn, na.rm = TRUE),
    mean_tp = mean(tp, na.rm = TRUE)
  )

# Ensure columns are numeric

mean_values$mean_tn <- as.numeric(mean_values$mean_tn)
mean_values$mean_tp <- as.numeric(mean_values$mean_tp)

# Replace NaNs with detection level 1.5 ppb

mean_values <- mean_values %>%
  mutate(
    mean_tn = ifelse(is.nan(mean_tn), NA, mean_tn),
    mean_tp = ifelse(is.nan(mean_tp), 1.5, mean_tp)
  )

# Remove LM10, LM5i, and LMHYPO

mean_values <- mean_values %>%
  filter(!site %in% c("LM10", "LM5i", "LMHYPO"))

# Define the custom order for the site column

site_order <- c("UMC", "Snyder", "Sprague", "LMC", "Fish")

# Convert the site column to a factor with the specified order

mean_values$site <- factor(mean_values$site, levels = site_order)

# Arrange the dataframe by date and then by the custom order of site

mean_values <- mean_values %>%
  arrange(date, site)

mean_values <- data.frame(mean_values)

# Correct the times

new_times <- c("19:00:00", "19:00:00", "19:00:00", "19:00:00", "20:00:00",
               "10:00:00", "10:00:00", "12:00:00", "13:00:00", "14:00:00",
               "14:00:00", "15:00:00", "16:00:00", "12:00:00", "08:00:00", 
               "13:00:00", "14:00:00", "14:00:00", "15:00:00", "16:00:00", 
               "11:00:00", "08:00:00", "12:00:00", "13:00:00", "14:00:00", 
               "15:00:00", "16:00:00", "16:00:00", "17:00:00", "18:00:00",
               "12:00:00", "13:00:00", "13:00:00", "14:00:00", "16:00:00",
               "15:00:00", "16:00:00", "16:00:00", "17:00:00", "16:00:00",
               "12:00:00", "14:00:00", "12:00:00", "14:00:00", "15:00:00")

# Create new date-time strings by combining the date with new times
mean_values <- mean_values %>%
  mutate(
    datetime_str = paste(format(date, "%Y-%m-%d"), new_times),
    date = as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")
  ) %>%
  select(-datetime_str) %>%
  mutate(date = with_tz(date, tzone = "UTC"))

print(mean_values)

mean_values <- mean_values %>%
  rename(date_time = date)

# Subset each site and add to discharge data

#UMC

UMC <- subset(mean_values, site== "UMC")

tribs_out <- tribs_out %>%
  left_join(UMC, by = "date_time") %>%
  rename(UMC_mean_tn = mean_tn,
         UMC_mean_tp = mean_tp)

#SNY

SNY <- subset(mean_values, site== "Snyder")

tribs_out <- tribs_out %>%
  left_join(SNY, by = "date_time") %>%
  rename(SNY_mean_tn = mean_tn,
         SNY_mean_tp = mean_tp)

#SPR

SPR <- subset(mean_values, site== "Sprague")

tribs_out <- tribs_out %>%
  left_join(SPR, by = "date_time") %>%
  rename(SPR_mean_tn = mean_tn,
         SPR_mean_tp = mean_tp)

#LMC

LMC <- subset(mean_values, site== "LMC")

tribs_out <- tribs_out %>%
  left_join(LMC, by = "date_time") %>%
  rename(LMC_mean_tn = mean_tn,
         LMC_mean_tp = mean_tp)

#Fish

FISH <- subset(mean_values, site== "Fish")

tribs_out <- tribs_out %>%
  left_join(FISH, by = "date_time") %>%
  rename(FISH_mean_tn = mean_tn,
         FISH_mean_tp = mean_tp)

# Remove additional site columns

tribs_out <- tribs_out %>%
  select(-site.x , -site.y, -site.x.x, -site.y.y, -site)

# Write new file

write.csv(tribs_out, 'C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\tribs_out_chemistry.csv', row.names=F)

rm(list = setdiff(ls(), "tribs_out"))

#2. Loadflex----

# UMC---- 

# Metadata

meta <- metadata(
  constituent = "UMC_mean_tn",
  flow = "umc_discharge_m3_s",
  dates = "date_time",
  conc.units = "ug L^-1",  # Change units if necessary
  flow.units = "cms",  # Assuming flow units are in cubic meters per second
  load.units = "kg",
  load.rate.units = "kg s^-1",
  site.name = "UMC",
  consti.name = "Total Nitrogen",
  site.id = 'UMC'
)

# Separate into calibration dataset and prediction dataset

intdat <- tribs_out %>%
  select(date_time, umc_discharge_m3_s, umc_discharge_m3_s_lwr, umc_discharge_m3_s_upr, UMC_mean_tn, UMC_mean_tp)

#calibration data for UMC

regdat <- intdat %>%
  filter(!is.na(UMC_mean_tn))  # Filter out rows with NA in UMC_mean_tn

#estimation data; isolate discharge

estdat_mean <- intdat %>%
  select(date_time, umc_discharge_m3_s)

estdat_lwr <- intdat %>%
  select(date_time, umc_discharge_m3_s_lwr)

estdat_upr <- intdat %>%
  select(date_time, umc_discharge_m3_s_upr)

# Manually define the mMatrix class if it's missing
setClass("mMatrix", contains = "matrix")

# Reload the smwrQW package to ensure it picks up the new class definition
library(smwrQW)

# Check if the lcens class is defined correctly now
showClass("lcens")

# Run loadest model

#mean

tn_lr <- loadReg2(loadReg(UMC_mean_tn ~ model(1), data=regdat,
                              flow="umc_discharge_m3_s", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr <- predictSolute(tn_lr, "flux", estdat_mean, se.pred=TRUE)

preds_tn_lr <- preds_tn_lr %>% mutate(umc_kg_N_hr = flux/24)


#lwr

tn_lr_lwr <- loadReg2(loadReg(UMC_mean_tn ~ model(1), data=regdat,
                          flow="umc_discharge_m3_s_lwr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr_lwr <- predictSolute(tn_lr_lwr, "flux", estdat_lwr, se.pred=TRUE)

preds_tn_lr_lwr <- preds_tn_lr_lwr %>% mutate(umc_kg_N_hr_lwr = flux/24)

#upr

tn_lr_upr <- loadReg2(loadReg(UMC_mean_tn ~ model(1), data=regdat,
                              flow="umc_discharge_m3_s_upr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr_upr <- predictSolute(tn_lr_upr, "flux", estdat_upr, se.pred=TRUE)

preds_tn_lr_upr <- preds_tn_lr_upr %>% mutate(umc_kg_N_hr_upr = flux/24)

# Combine dataframes
tn_umc <- preds_tn_lr %>%
  inner_join(preds_tn_lr_lwr,  by = "date") %>%
  inner_join(preds_tn_lr_upr, by = "date")

preds_tn_lr %>% count(date) %>% filter(n > 1) %>% arrange(desc(n)) %>% print(n = 20)
preds_tn_lr_lwr %>% count(date) %>% filter(n > 1) %>% arrange(desc(n)) %>% print(n = 20)
preds_tn_lr_upr %>% count(date) %>% filter(n > 1) %>% arrange(desc(n)) %>% print(n = 20)


#TP

regdat <- intdat %>%
  filter(!is.na(UMC_mean_tp))


#mean
tp_lr <- loadReg2(loadReg(UMC_mean_tp ~ model(1), data=regdat,
                          flow="umc_discharge_m3_s", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr <- predictSolute(tp_lr, "flux", estdat_mean, se.pred=TRUE)

preds_tp_lr <- preds_tp_lr %>% mutate(umc_kg_P_hr = flux/24)

#lwr

tp_lr_lwr <- loadReg2(loadReg(UMC_mean_tp ~ model(1), data=regdat,
                              flow="umc_discharge_m3_s_lwr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr_lwr <- predictSolute(tp_lr_lwr, "flux", estdat_lwr, se.pred=TRUE)

preds_tp_lr_lwr <- preds_tp_lr_lwr %>% mutate(umc_kg_P_hr_lwr = flux/24)

#upr

tp_lr_upr <- loadReg2(loadReg(UMC_mean_tp ~ model(1), data=regdat,
                              flow="umc_discharge_m3_s_upr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr_upr <- predictSolute(tp_lr_upr, "flux", estdat_upr, se.pred=TRUE)

preds_tp_lr_upr <- preds_tp_lr_upr %>% mutate(umc_kg_P_hr_upr = flux/24)


tp_umc <- preds_tp_lr %>%
  inner_join(preds_tp_lr_lwr,  by = "date") %>%
  inner_join(preds_tp_lr_upr, by = "date")

# Combine dataframes

tribs_nut <- tn_umc %>% inner_join(tp_umc, by="date") %>%
  select(date, umc_kg_N_hr, umc_kg_N_hr_lwr, umc_kg_N_hr_upr, umc_kg_P_hr, umc_kg_P_hr_lwr, umc_kg_P_hr_upr)

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

#SNY----

# Manually define the mMatrix class if it's missing
setClass("mMatrix", contains = "matrix")

# Reload the smwrQW package to ensure it picks up the new class definition
library(smwrQW)

# Check if the lcens class is defined correctly now
showClass("lcens")

#Metadata

meta <- metadata(
  constituent = "SNY_mean_tn",
  flow = "sny_discharge_m3_s",
  dates = "date_time",
  conc.units = "ug L^-1",  # Change units if necessary
  flow.units = "cms",  # Assuming flow units are in cubic meters per second
  load.units = "kg",
  load.rate.units = "kg s^-1",
  site.name = "Snyder",
  consti.name = "Total Nitrogen, Total Phosphorus",
  site.id = 'SNY'
)

# Separate into calibration dataset and prediction dataset

intdat <- tribs_out %>%
  select(date_time, sny_discharge_m3_s, sny_discharge_m3_s_lwr, sny_discharge_m3_s_upr, SNY_mean_tn, SNY_mean_tp)

#calibration data for SNY

regdat <- intdat %>%
  filter(!is.na(SNY_mean_tn))  # Filter out rows with NA in SNY_mean_tn

#estimation data; isolate discharge

estdat_mean <- intdat %>%
  select(date_time, sny_discharge_m3_s)

estdat_lwr <- intdat %>%
  select(date_time, sny_discharge_m3_s_lwr)

estdat_upr <- intdat %>%
  select(date_time, sny_discharge_m3_s_upr)

# Run loadest model

#mean
tn_lr <- loadReg2(loadReg(SNY_mean_tn ~ model(1), data=regdat,
                          flow="sny_discharge_m3_s", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr <- predictSolute(tn_lr, "flux", estdat_mean, se.pred=TRUE)

preds_tn_lr <- preds_tn_lr %>% mutate(sny_kg_N_hr = flux/24)

#lwr

tn_lr_lwr <- loadReg2(loadReg(SNY_mean_tn ~ model(1), data=regdat,
                              flow="sny_discharge_m3_s_lwr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr_lwr <- predictSolute(tn_lr_lwr, "flux", estdat_lwr, se.pred=TRUE)

preds_tn_lr_lwr <- preds_tn_lr_lwr %>% mutate(sny_kg_N_hr_lwr = flux/24)

#upr

tn_lr_upr <- loadReg2(loadReg(SNY_mean_tn ~ model(1), data=regdat,
                              flow="sny_discharge_m3_s_upr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr_upr <- predictSolute(tn_lr_upr, "flux", estdat_upr, se.pred=TRUE)

preds_tn_lr_upr <- preds_tn_lr_upr %>% mutate(sny_kg_N_hr_upr = flux/24)

# Combine dataframes
tn_sny <- preds_tn_lr %>%
  inner_join(preds_tn_lr_lwr, by = "date") %>%
  inner_join(preds_tn_lr_upr, by = "date")

#mean
tp_lr <- loadReg2(loadReg(SNY_mean_tp ~ model(1), data=regdat,
                          flow="sny_discharge_m3_s", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr <- predictSolute(tp_lr, "flux", estdat_mean, se.pred=TRUE)

preds_tp_lr <- preds_tp_lr %>% mutate(sny_kg_P_hr = flux/24)

#lwr

tp_lr_lwr <- loadReg2(loadReg(SNY_mean_tp ~ model(1), data=regdat,
                              flow="sny_discharge_m3_s_lwr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr_lwr <- predictSolute(tp_lr_lwr, "flux", estdat_lwr, se.pred=TRUE)

preds_tp_lr_lwr <- preds_tp_lr_lwr %>% mutate(sny_kg_P_hr_lwr = flux/24)

#upr

tp_lr_upr <- loadReg2(loadReg(SNY_mean_tp ~ model(1), data=regdat,
                              flow="sny_discharge_m3_s_upr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr_upr <- predictSolute(tp_lr_upr, "flux", estdat_upr, se.pred=TRUE)

preds_tp_lr_upr <- preds_tp_lr_upr %>% mutate(sny_kg_P_hr_upr = flux/24)


tp_sny <- preds_tp_lr %>%
  inner_join(preds_tp_lr_lwr, by = "date") %>%
  inner_join(preds_tp_lr_upr, by = "date")

# Combine dataframes

tribs_nut <- tribs_nut %>% inner_join(tn_sny, by="date") %>%
  inner_join(tp_sny, by="date") %>%
  select(date, umc_kg_N_hr, umc_kg_N_hr_lwr, umc_kg_N_hr_upr, umc_kg_P_hr, umc_kg_P_hr_lwr, umc_kg_P_hr_upr,
         sny_kg_N_hr, sny_kg_N_hr_lwr, sny_kg_N_hr_upr, sny_kg_P_hr, sny_kg_P_hr_lwr, sny_kg_P_hr_upr)

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


#FISH----

# Manually define the mMatrix class if it's missing
setClass("mMatrix", contains = "matrix")

# Reload the smwrQW package to ensure it picks up the new class definition
library(smwrQW)

# Check if the lcens class is defined correctly now
showClass("lcens")

#Metadata

meta <- metadata(
  constituent = "FISH_mean_tn",
  flow = "fish_discharge_m3_s",
  dates = "date_time",
  conc.units = "ug L^-1",  # Change units if necessary
  flow.units = "cms",  # Assuming flow units are in cubic meters per second
  load.units = "kg",
  load.rate.units = "kg s^-1",
  site.name = "Sprague",
  consti.name = "Total Nitrogen, Total Phosphorus",
  site.id = 'FISH'
)

# Separate into calibration dataset and prediction dataset

intdat <- tribs_out %>%
  select(date_time, fish_discharge_m3_s, fish_discharge_m3_s_lwr, fish_discharge_m3_s_upr, FISH_mean_tn, FISH_mean_tp)

#calibration data for FISH

regdat <- intdat %>%
  filter(!is.na(FISH_mean_tn))  # Filter out rows with NA in FISH_mean_tn

#estimation data; isolate discharge

estdat_mean <- intdat %>%
  select(date_time, fish_discharge_m3_s)

estdat_lwr <- intdat %>%
  select(date_time, fish_discharge_m3_s_lwr)

estdat_upr <- intdat %>%
  select(date_time, fish_discharge_m3_s_upr)

# Run loadest model

#mean
tn_lr <- loadReg2(loadReg(FISH_mean_tn ~ model(1), data=regdat,
                          flow="fish_discharge_m3_s", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr <- predictSolute(tn_lr, "flux", estdat_mean, se.pred=TRUE)

preds_tn_lr <- preds_tn_lr %>% mutate(fish_kg_N_hr = flux/24)

#lwr

tn_lr_lwr <- loadReg2(loadReg(FISH_mean_tn ~ model(1), data=regdat,
                              flow="fish_discharge_m3_s_lwr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr_lwr <- predictSolute(tn_lr_lwr, "flux", estdat_lwr, se.pred=TRUE)

preds_tn_lr_lwr <- preds_tn_lr_lwr %>% mutate(fish_kg_N_hr_lwr = flux/24)

#upr

tn_lr_upr <- loadReg2(loadReg(FISH_mean_tn ~ model(1), data=regdat,
                              flow="fish_discharge_m3_s_upr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr_upr <- predictSolute(tn_lr_upr, "flux", estdat_upr, se.pred=TRUE)

preds_tn_lr_upr <- preds_tn_lr_upr %>% mutate(fish_kg_N_hr_upr = flux/24)

# Combine dataframes
tn_fish <- preds_tn_lr %>%
  inner_join(preds_tn_lr_lwr, by = "date") %>%
  inner_join(preds_tn_lr_upr, by = "date")

#mean
tp_lr <- loadReg2(loadReg(FISH_mean_tp ~ model(1), data=regdat,
                          flow="fish_discharge_m3_s", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr <- predictSolute(tp_lr, "flux", estdat_mean, se.pred=TRUE)

preds_tp_lr <- preds_tp_lr %>% mutate(fish_kg_P_hr = flux/24)

#lwr

tp_lr_lwr <- loadReg2(loadReg(FISH_mean_tp ~ model(1), data=regdat,
                              flow="fish_discharge_m3_s_lwr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr_lwr <- predictSolute(tp_lr_lwr, "flux", estdat_lwr, se.pred=TRUE)

preds_tp_lr_lwr <- preds_tp_lr_lwr %>% mutate(fish_kg_P_hr_lwr = flux/24)

#upr

tp_lr_upr <- loadReg2(loadReg(FISH_mean_tp ~ model(1), data=regdat,
                              flow="fish_discharge_m3_s_upr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr_upr <- predictSolute(tp_lr_upr, "flux", estdat_upr, se.pred=TRUE)

preds_tp_lr_upr <- preds_tp_lr_upr %>% mutate(fish_kg_P_hr_upr = flux/24)


tp_fish <- preds_tp_lr %>%
  inner_join(preds_tp_lr_lwr, by = "date") %>%
  inner_join(preds_tp_lr_upr, by = "date")

#combine dataframes
tribs_nut <- tribs_nut %>% inner_join(tn_fish, by="date") %>%
  inner_join(tp_fish, by="date") %>%
  select(date, umc_kg_N_hr, umc_kg_N_hr_lwr, umc_kg_N_hr_upr, umc_kg_P_hr, umc_kg_P_hr_lwr, umc_kg_P_hr_upr,
         sny_kg_N_hr, sny_kg_N_hr_lwr, sny_kg_N_hr_upr, sny_kg_P_hr, sny_kg_P_hr_lwr, sny_kg_P_hr_upr,
         fish_kg_N_hr, fish_kg_N_hr_lwr, fish_kg_N_hr_upr, fish_kg_P_hr, fish_kg_P_hr_lwr, fish_kg_P_hr_upr)

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

#SPR----

# Manually define the mMatrix class if it's missing
setClass("mMatrix", contains = "matrix")

# Reload the smwrQW package to ensure it picks up the new class definition
library(smwrQW)

# Check if the lcens class is defined correctly now
showClass("lcens")

#Metadata

meta <- metadata(
  constituent = "SPR_mean_tn",
  flow = "spr_discharge_m3_s",
  dates = "date_time",
  conc.units = "ug L^-1",  # Change units if necessary
  flow.units = "cms",  # Assuming flow units are in cubic meters per second
  load.units = "kg",
  load.rate.units = "kg s^-1",
  site.name = "Sprague",
  consti.name = "Total Nitrogen, Total Phosphorus",
  site.id = 'SPR'
)

# Separate into calibration dataset and prediction dataset

intdat <- tribs_out %>%
  select(date_time, spr_discharge_m3_s, spr_discharge_m3_s_lwr, spr_discharge_m3_s_upr, SPR_mean_tn, SPR_mean_tp)

#calibration data for SPR

regdat <- intdat %>%
  filter(!is.na(SPR_mean_tn))  # Filter out rows with NA in SPR_mean_tn

#estimation data; isolate discharge

estdat_mean <- intdat %>%
  select(date_time, spr_discharge_m3_s)

estdat_lwr <- intdat %>%
  select(date_time, spr_discharge_m3_s_lwr)

estdat_upr <- intdat %>%
  select(date_time, spr_discharge_m3_s_upr)

# Run loadest model

#mean
tn_lr <- loadReg2(loadReg(SPR_mean_tn ~ model(1), data=regdat,
                          flow="spr_discharge_m3_s", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr <- predictSolute(tn_lr, "flux", estdat_mean, se.pred=TRUE)

preds_tn_lr <- preds_tn_lr %>% mutate(spr_kg_N_hr = flux/24)

#lwr

tn_lr_lwr <- loadReg2(loadReg(SPR_mean_tn ~ model(1), data=regdat,
                              flow="spr_discharge_m3_s_lwr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr_lwr <- predictSolute(tn_lr_lwr, "flux", estdat_lwr, se.pred=TRUE)

preds_tn_lr_lwr <- preds_tn_lr_lwr %>% mutate(spr_kg_N_hr_lwr = flux/24)

#upr

tn_lr_upr <- loadReg2(loadReg(SPR_mean_tn ~ model(1), data=regdat,
                              flow="spr_discharge_m3_s_upr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tn_lr_upr <- predictSolute(tn_lr_upr, "flux", estdat_upr, se.pred=TRUE)

preds_tn_lr_upr <- preds_tn_lr_upr %>% mutate(spr_kg_N_hr_upr = flux/24)

# Combine dataframes
tn_spr <- preds_tn_lr %>%
  inner_join(preds_tn_lr_lwr, by = "date") %>%
  inner_join(preds_tn_lr_upr, by = "date")

#mean
tp_lr <- loadReg2(loadReg(SPR_mean_tp ~ model(1), data=regdat,
                          flow="spr_discharge_m3_s", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr <- predictSolute(tp_lr, "flux", estdat_mean, se.pred=TRUE)

preds_tp_lr <- preds_tp_lr %>% mutate(spr_kg_P_hr = flux/24)

#lwr

tp_lr_lwr <- loadReg2(loadReg(SPR_mean_tp ~ model(1), data=regdat,
                              flow="spr_discharge_m3_s_lwr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr_lwr <- predictSolute(tp_lr_lwr, "flux", estdat_lwr, se.pred=TRUE)

preds_tp_lr_lwr <- preds_tp_lr_lwr %>% mutate(spr_kg_P_hr_lwr = flux/24)

#upr

tp_lr_upr <- loadReg2(loadReg(SPR_mean_tp ~ model(1), data=regdat,
                              flow="spr_discharge_m3_s_upr", dates="date_time", flow.units = "cms", conc.units="ug/L", time.step="instantaneous"))

preds_tp_lr_upr <- predictSolute(tp_lr_upr, "flux", estdat_upr, se.pred=TRUE)

preds_tp_lr_upr <- preds_tp_lr_upr %>% mutate(spr_kg_P_hr_upr = flux/24)


tp_spr <- preds_tp_lr %>%
  inner_join(preds_tp_lr_lwr, by = "date") %>%
  inner_join(preds_tp_lr_upr, by = "date")

#combine dataframes
tribs_nut <- tribs_nut %>% inner_join(tn_spr, by="date") %>%
  inner_join(tp_spr, by="date") %>%
  select(date, umc_kg_N_hr, umc_kg_N_hr_lwr, umc_kg_N_hr_upr, umc_kg_P_hr, umc_kg_P_hr_lwr, umc_kg_P_hr_upr,
         sny_kg_N_hr, sny_kg_N_hr_lwr, sny_kg_N_hr_upr, sny_kg_P_hr, sny_kg_P_hr_lwr, sny_kg_P_hr_upr,
         fish_kg_N_hr, fish_kg_N_hr_lwr, fish_kg_N_hr_upr, fish_kg_P_hr, fish_kg_P_hr_lwr, fish_kg_P_hr_upr,
         spr_kg_N_hr, spr_kg_N_hr_lwr, spr_kg_N_hr_upr, spr_kg_P_hr, spr_kg_P_hr_lwr, spr_kg_P_hr_upr) %>%
  rename(date_time = "date")

tribs_nut <- tribs_nut %>%
  inner_join(tribs_out, by = "date_time")

##write file
write.csv(tribs_nut, 'F:\\B_trout_files\\toobigforgit\\Chp_1\\tribs_nuts_10_01_07_9_30_23.csv', row.names = FALSE) 

rm(list = ls())

#3. Plot----
#Tribs chemistry 2022----

tribs_out <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\tribs_out_chemistry.csv')

tribs_out  <- tribs_out  %>%
  mutate(date_time = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

umc_chem <- ggplot(data = tribs_out, aes(x = umc_discharge_m3_s)) + 
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
  
umc_chem

sny_chem <- ggplot(data = tribs_out, aes(x = sny_discharge_m3_s)) + 
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

sny_chem

spr_chem <- ggplot(data = tribs_out, aes(x = spr_discharge_m3_s)) + 
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

spr_chem

fish_chem <- ggplot(data = tribs_out, aes(x = fish_discharge_m3_s)) + 
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

fish_chem


L3_chem_combined <- (umc_chem|sny_chem)/(spr_chem|fish_chem)

ggsave("in_situ_L3T_chem.png", L3_chem_combined, "png", width = 5, height = 5)

rm(list = ls())


#Time series of conc water years 2008-2023----

tribs_nut <- readr::read_csv("F:\\B_trout_files\\toobigforgit\\Chp_1\\tribs_nuts_10_01_07_9_30_23.csv",col_types = cols(date_time = col_datetime(format = "")))

summary(tribs_nut)
str(tribs_nut)

umc <- ggplot(data = tribs_nut, aes(x = date_time)) + 
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


ggsave("umc_C.png", plot = umc, width = 5, height = 4, dpi = 300)


sny <- ggplot(data = tribs_nut, aes(x = date_time)) + 
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



ggsave("sny_C.png", plot = sny, width = 5, height = 4, dpi = 300) 

spr <- ggplot(data = tribs_nut, aes(x = date_time)) + 
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


ggsave("spr_C.png", plot = spr, width = 5, height = 4, dpi = 300) 


fish <- ggplot(data = tribs_nut, aes(x = date_time)) + 
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


ggsave("fish_C.png", plot = fish, width = 5, height = 4, dpi = 300) 


#4. Calculate RMSE for UMC----


## 1) Sampling dates and observed values
obs <- tibble(
  Date   = as.Date(c("2023-05-05","2023-06-12","2023-07-12","2023-08-20","2023-09-17")),
  TP_obs = c(16.50, 2.13, 2.10, 1.50, 2.10),
  TN_obs = c(399.00,113.75,133.00,149.50,163.75)
)

## 2) Daily means of hourly loads (kg/hr) from tribs_nuts
loads_daily <- tribs_nut %>%
  mutate(Date = as.Date(date_time)) %>%
  filter(Date %in% obs$Date) %>%
  group_by(Date) %>%
  summarise(
    P_kg_hr_lwr = mean(umc_kg_P_hr_lwr, na.rm = TRUE),
    P_kg_hr     = mean(umc_kg_P_hr,     na.rm = TRUE),
    P_kg_hr_upr = mean(umc_kg_P_hr_upr, na.rm = TRUE),
    N_kg_hr_lwr = mean(umc_kg_N_hr_lwr, na.rm = TRUE),
    N_kg_hr     = mean(umc_kg_N_hr,     na.rm = TRUE),
    N_kg_hr_upr = mean(umc_kg_N_hr_upr, na.rm = TRUE),
    .groups = "drop"
  )

## 3) Daily mean discharge (m^3/s) → L/s from tribs_out
Q_daily <- tribs_nut %>%
  mutate(Date = as.Date(date_time)) %>%
  filter(Date %in% obs$Date) %>%
  group_by(Date) %>%
  summarise(Q_L_s = mean(umc_discharge_m3_s * 1000, na.rm = TRUE), .groups = "drop")

## 4) Join and convert to concentration (µg/L)
preds <- loads_daily %>%
  inner_join(Q_daily, by = "Date") %>%
  mutate(
    TP_pred_lwr = (P_kg_hr_lwr * 1e9) / (Q_L_s * 3600),
    TP_pred     = (P_kg_hr     * 1e9) / (Q_L_s * 3600),
    TP_pred_upr = (P_kg_hr_upr * 1e9) / (Q_L_s * 3600),
    TN_pred_lwr = (N_kg_hr_lwr * 1e9) / (Q_L_s * 3600),
    TN_pred     = (N_kg_hr     * 1e9) / (Q_L_s * 3600),
    TN_pred_upr = (N_kg_hr_upr * 1e9) / (Q_L_s * 3600)
  )

## 5) Build TP table
TP_tbl <- obs %>%
  select(Date, TP_obs) %>%
  left_join(preds %>% select(Date, TP_pred, TP_pred_lwr, TP_pred_upr), by = "Date") %>%
  mutate(Difference = TP_obs - TP_pred)

## 6) Build TN table
TN_tbl <- obs %>%
  select(Date, TN_obs) %>%
  left_join(preds %>% select(Date, TN_pred, TN_pred_lwr, TN_pred_upr), by = "Date") %>%
  mutate(Difference = TN_obs - TN_pred)

TP_tbl_disp <- TP_tbl %>%
  transmute(
    Date,
    Observed = TP_obs,
    `Predicted (Lower Limit)` = TP_pred_lwr,
    Predicted = TP_pred,
    `Predicted (Upper Limit)` = TP_pred_upr
  )

# Add mean, RMSE, and normalized RMSE
tp_mean_obs <- mean(TP_tbl_disp$Observed, na.rm = TRUE)


tp_rmse_lwr <- sqrt(mean((TP_tbl_disp$Observed - TP_tbl_disp$`Predicted (Lower Limit)`)^2, na.rm = TRUE))
tp_rmse_mid <- sqrt(mean((TP_tbl_disp$Observed - TP_tbl_disp$Predicted)^2, na.rm = TRUE))
tp_rmse_upr <- sqrt(mean((TP_tbl_disp$Observed - TP_tbl_disp$`Predicted (Upper Limit)`)^2, na.rm = TRUE))

tp_rmse_lwr_norm <- sqrt(mean((TP_tbl_disp$Observed - TP_tbl_disp$`Predicted (Lower Limit)`)^2, na.rm = TRUE))/tp_mean_obs
tp_rmse_mid_norm <- sqrt(mean((TP_tbl_disp$Observed - TP_tbl_disp$Predicted)^2, na.rm = TRUE))/tp_mean_obs
tp_rmse_upr_norm <- sqrt(mean((TP_tbl_disp$Observed - TP_tbl_disp$`Predicted (Upper Limit)`)^2, na.rm = TRUE))/tp_mean_obs

mean.row <- tibble::tibble(
  Date = "Mean",
  Observed = tp_mean_obs,
  `Predicted (Lower Limit)` = NA_real_,
  `Predicted` = NA_real_,
  `Predicted (Upper Limit)` = NA_real_
)

rmse.row <- tibble::tibble(
  Date = "RMSE",
  Observed = NA_real_,
  `Predicted (Lower Limit)` = tp_rmse_lwr,
  Predicted = tp_rmse_mid, 
  `Predicted (Upper Limit)` = tp_rmse_upr
)

rmse.norm <- tibble::tibble(
  Date = "RMSE Normalized",
  Observed = NA_real_,
  `Predicted (Lower Limit)` = tp_rmse_lwr_norm,
  Predicted = tp_rmse_mid_norm, 
  `Predicted (Upper Limit)` = tp_rmse_upr_norm
)

TP_tbl_disp$Date <- as.character(TP_tbl_disp$Date)

TP_tbl_final <- rbind(TP_tbl_disp, mean.row, rmse.row, rmse.norm)

kable(
  TP_tbl_final,
  format = "latex", booktabs = TRUE, digits = 2,
  caption = "Predicted total phosphorus (TP) concentrations in McDonald Creek (\\si{\\micro g\\,L^{-1}}) compared to measurements."
)


## 6) Build TN table
TN_tbl <- obs %>%
  select(Date, TN_obs) %>%
  left_join(preds %>% select(Date, TN_pred, TN_pred_lwr, TN_pred_upr), by = "Date") %>%
  mutate(Difference = TN_obs - TN_pred)

TN_tbl_disp <- TN_tbl %>%
  transmute(
    Date,
    Observed = TN_obs,
    `Predicted (Lower Limit)` = TN_pred_lwr,
    Predicted = TN_pred,
    `Predicted (Upper Limit)` = TN_pred_upr
  )

# Add mean, RMSE, and normalized RMSE
tn_mean_obs <- mean(TN_tbl_disp$Observed, na.rm = TRUE)

tn_rmse_lwr <- sqrt(mean((TN_tbl_disp$Observed - TN_tbl_disp$`Predicted (Lower Limit)`)^2, na.rm = TRUE))
tn_rmse_mid <- sqrt(mean((TN_tbl_disp$Observed - TN_tbl_disp$Predicted)^2, na.rm = TRUE))
tn_rmse_upr <- sqrt(mean((TN_tbl_disp$Observed - TN_tbl_disp$`Predicted (Upper Limit)`)^2, na.rm = TRUE))

tn_rmse_lwr_norm <- sqrt(mean((TN_tbl_disp$Observed - TN_tbl_disp$`Predicted (Lower Limit)`)^2, na.rm = TRUE))/tn_mean_obs
tn_rmse_mid_norm <- sqrt(mean((TN_tbl_disp$Observed - TN_tbl_disp$Predicted)^2, na.rm = TRUE))/tn_mean_obs
tn_rmse_upr_norm <- sqrt(mean((TN_tbl_disp$Observed - TN_tbl_disp$`Predicted (Upper Limit)`)^2, na.rm = TRUE))/tn_mean_obs

mean.row <- tibble::tibble(
  Date = "Mean",
  Observed = tn_mean_obs,
  `Predicted (Lower Limit)` = NA_real_,
  `Predicted` = NA_real_,
  `Predicted (Upper Limit)` = NA_real_
)

rmse.row <- tibble::tibble(
  Date = "RMSE",
  Observed = NA_real_,
  `Predicted (Lower Limit)` = tn_rmse_lwr,
  Predicted = tn_rmse_mid, 
  `Predicted (Upper Limit)` = tn_rmse_upr
)

rmse.norm <- tibble::tibble(
  Date = "RMSE Normalized",
  Observed = NA_real_,
  `Predicted (Lower Limit)` = tn_rmse_lwr_norm,
  Predicted = tn_rmse_mid_norm, 
  `Predicted (Upper Limit)` = tn_rmse_upr_norm
)

TN_tbl_disp$Date <- as.character(TN_tbl_disp$Date)

TN_tbl_final <- rbind(TN_tbl_disp, mean.row, rmse.row, rmse.norm)

kable(
  TN_tbl_final,
  format = "latex", booktabs = TRUE, digits = 2,
  caption = "Predicted total nitrogen (TN) concentrations in McDonald Creek (\\si{\\micro g\\,L^{-1}}) compared to measurements."
)

