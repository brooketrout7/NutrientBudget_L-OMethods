
#0. load packages----

library(ggplot2)
library(gridExtra)
library(lubridate)
library(tidyverse)
library(patchwork)
library(stringr)
library(dplyr)

#1. Calculate Burial of N and P----

# Linear sedimentation rate (cm/yr) * bulk density (g/cm3) * N/P concentration (%; weight/weight)

# Sedimentation rate 

SR_cm_yr <- 0.107

#dry bulk density:

DBD_g_cm3 <- 0.5184

# SA m2 to cm2 (1 m2 = 10000 cm2)
SA_m2 <- 27810670.1791
SA_cm2 <- 27810670.1791*(100*100)
SA_km2 <- 27810670.1791*(1/(1000*1000))

# Use variation throughout the core as a proxy for possible variability throughout the lake; limit burial to area 20% of lake area

N_P <- read.csv("C:/Users/brook/Documents/PhD/Dissertation/Chp_2/paleo/2_incremental/lm_paleo.csv", header = TRUE)

N_P <- N_P %>%
  dplyr::select(age, n_prop, p_prop) %>%
  mutate(
    Sedimentation_N_kg_yr = SR_cm_yr * DBD_g_cm3 * 10000 * n_prop * (SA_m2*0.2) * (1 / 1000),
    Sedimentation_N_kg_wk = Sedimentation_N_kg_yr * (1 / 52), 
    Sedimentation_P_kg_yr = SR_cm_yr * DBD_g_cm3 * 10000 * p_prop * (SA_m2 * 0.20) * (1 / 1000),
    Sedimentation_P_kg_wk = Sedimentation_P_kg_yr * (1 / 52)
  )

summary(N_P)

Sedimentation_N_kg_wk <- median(N_P$Sedimentation_N_kg_wk, na.rm = TRUE)
Sedimentation_P_kg_wk <- median(N_P$Sedimentation_P_kg_wk, na.rm = TRUE)


# The Moyle and Boyle way

# Mean depth for LM: 

z <- 45.7

# Depth of coring location
zcore <- 143

# MAR; # they used z/zcore as a scaling factor, we decided on an area; note they said you can adjust the area in which fine sediments would accumulate (i.e., Asaa/Aacc = AL), so we could adjust this to the larger extent of the lake, but not the whole surface area, especially given the substrate of the lake. "Given the bathymetry of the lake". Note converting to kg here.  
MAR <- SR_cm_yr*DBD_g_cm3*(z/zcore)*SA_cm2*(1/1000)

Sedimentation_P_kg_wkmoylemin <- MAR * (min(N_P$p_prop, na.rm = TRUE)) * (1 / 52)
Sedimentation_P_kg_wkmoylemed <- MAR * (median(N_P$p_prop, na.rm = TRUE)) * (1 / 52)
Sedimentation_P_kg_wkmoylemax <- MAR * (max(N_P$p_prop, na.rm = TRUE)) * (1 / 52)

Sedimentation_N_kg_wkmoylemin <- MAR * (min(N_P$n_prop, na.rm = TRUE)) * (1 / 52)
Sedimentation_N_kg_wkmoylemed <- MAR * (median(N_P$n_prop, na.rm = TRUE)) * (1 / 52)
Sedimentation_N_kg_wkmoylemax <- MAR * (max(N_P$n_prop, na.rm = TRUE)) * (1 / 52)


#ET = 0.25 · DR · 41(0.061/DR); dynamic ratio (DR = √Area/Dm; Area in km2; mean depth, Dm in m)

DR <- sqrt((SA_km2/z))

# fraction of lake dominated by ET processes
ET <- 0.25*DR*(41^(0.061/DR))

# fraction of the lake dominated by accumulation 
A <- 1-ET

MAR <- SR_cm_yr*DBD_g_cm3*(z/zcore)*(SA_cm2*A)*(1/1000)

Sedimentation_P_kg_wkmoylemin <- MAR * (min(N_P$p_prop, na.rm = TRUE)) * (1 / 52)
Sedimentation_P_kg_wkmoylemed <- MAR * (median(N_P$p_prop, na.rm = TRUE)) * (1 / 52)
Sedimentation_P_kg_wkmoylemax <- MAR * (max(N_P$p_prop, na.rm = TRUE)) * (1 / 52)

Sedimentation_N_kg_wkmoylemin <- MAR * (min(N_P$n_prop, na.rm = TRUE)) * (1 / 52)
Sedimentation_N_kg_wkmoylemed <- MAR * (median(N_P$n_prop, na.rm = TRUE)) * (1 / 52)
Sedimentation_N_kg_wkmoylemax <- MAR * (max(N_P$n_prop, na.rm = TRUE)) * (1 / 52)

