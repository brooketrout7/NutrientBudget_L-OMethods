
#0. Load packages----

library(ggplot2)
library(ggpubr)
library(patchwork)
library(tidyverse)
library(scales)
library(xtable)
library(ggbreak) 
library(lubridate)
library(dplyr)
library(tidyr)

# Set the option to remove scientific notation
options(scipen = 999)

#1. Nutrient Budget----

# Read in data, create POSIXct, and select relevant columns (rename if needed)

tribs <- readr::read_csv("F:\\B_trout_files\\toobigforgit\\Chp_1\\tribs_nuts_10_01_07_9_30_23.csv",col_types = cols(date_time = col_datetime(format = "")))

#remove the "hr" component

tribs <- tribs %>%
  rename(umc_kg_N = "umc_kg_N_hr", umc_kg_P = "umc_kg_P_hr", sny_kg_N = "sny_kg_N_hr", sny_kg_P = "sny_kg_P_hr", spr_kg_N = "spr_kg_N_hr", spr_kg_P = "spr_kg_P_hr", fish_kg_N = "fish_kg_N_hr", fish_kg_P = "fish_kg_P_hr", 
         umc_kg_N_lwr = "umc_kg_N_hr_lwr", umc_kg_P_lwr = "umc_kg_P_hr_lwr", sny_kg_N_lwr = "sny_kg_N_hr_lwr", sny_kg_P_lwr = "sny_kg_P_hr_lwr", spr_kg_N_lwr = "spr_kg_N_hr_lwr", spr_kg_P_lwr = "spr_kg_P_hr_lwr", fish_kg_N_lwr = "fish_kg_N_hr_lwr", fish_kg_P_lwr = "fish_kg_P_hr_lwr", 
         umc_kg_N_upr = "umc_kg_N_hr_upr", umc_kg_P_upr = "umc_kg_P_hr_upr", sny_kg_N_upr = "sny_kg_N_hr_upr", sny_kg_P_upr = "sny_kg_P_hr_upr", spr_kg_N_upr = "spr_kg_N_hr_upr", spr_kg_P_upr = "spr_kg_P_hr_upr", fish_kg_N_upr = "fish_kg_N_hr_upr", fish_kg_P_upr = "fish_kg_P_hr_upr")%>%
  mutate(date_time  = as.POSIXct(date_time,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

# Read in data
lake <- readr::read_csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\lake_nuts.csv",col_types = cols(date_time = col_datetime(format = "")))

# Clean
lake <- lake %>%
  dplyr::select(date_time, mean_tn, mean_tp, min_tn, max_tn, min_tp, max_tp, kg_N, kg_N_lwr, kg_N_upr, kg_P, kg_P_lwr, kg_P_upr)

lake <- lake %>%
  mutate(date_time  = as.POSIXct(date_time,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

# Read in data

wetN <- readr::read_csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\wetdepositionN.csv",col_types = cols(dateOn = col_datetime(format = ""), dateOff = col_datetime(format = "")))

wetN <- wetN %>%
  mutate(
    dateOn  = as.POSIXct(dateOn,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    dateOff = as.POSIXct(dateOff, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

summary(wetN)

wetP <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\wetdepositionP.csv', header = TRUE, sep = ",")

# Convert to POSIX
wetP$Date <- as.Date(wetP$Date, format = "%Y-%m-%d", tz = "UTC")

# Read in data
dry <- read.csv('C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\drydepositionNuts.csv', header = TRUE, sep = ",")

# Convert to POSIXct
dry$Date <- as.Date(dry$Date, format = "%Y-%m-%d", tz = "UTC")

# Calculate the difference in dates between consecutive rows for dry and wetP

dry <- dry %>%
  dplyr::mutate(date_diff = as.integer(Date - lag(Date)), 
         TN_dry_kg_d = TN_dry_kg/date_diff, TP_dry_kg_d = TP_dry_kg/date_diff, 
         start_date = lag(Date),
         end_date = Date) %>%
  select(start_date, end_date, date_diff, TN_dry_kg_d, TP_dry_kg_d)

wetP <- wetP %>%
  arrange(Date) %>%  # make sure it's sorted
  dplyr::mutate(
    date_diff  = as.integer(Date - dplyr::lag(Date)),
    TP_wet_kg_d = TP_wet_kg / date_diff,
    start_date = dplyr::lag(Date),
    end_date   = Date
  ) %>%
  dplyr::select(start_date, end_date, date_diff, TP_wet_kg_d)

# Get NADP pulls
precip_dates_times <- wetN %>%
  select(dateOn, dateOff)

# Tributaries and Outflow

# Mean
sum_tribs<- function(tribs, precip_dates_times) {
  
  # Initialize the results dataframe
  results <- data.frame(start_date = character(), end_date = character(),
                        umc_N_sum = numeric(), umc_P_sum = numeric(), sny_N_sum = numeric(),
                        sny_P_sum = numeric(), spr_N_sum = numeric(), spr_P_sum = numeric(),
                        fish_N_sum = numeric(), fish_P_sum = numeric(), stringsAsFactors = FALSE)
  
  # Include the range before the first date specified in dateOff
  first_date <- precip_dates_times$dateOff[1]
  pre_first_date_data <- tribs %>% filter(date_time < first_date)
  
  if (nrow(pre_first_date_data) > 0) {
    pre_first_date_sums <- pre_first_date_data %>%
      summarize(
        umc_N_sum = sum(umc_kg_N, na.rm = TRUE),
        umc_P_sum = sum(umc_kg_P, na.rm = TRUE),
        sny_N_sum = sum(sny_kg_N, na.rm = TRUE),
        sny_P_sum = sum(sny_kg_P, na.rm = TRUE),
        spr_N_sum = sum(spr_kg_N, na.rm = TRUE),
        spr_P_sum = sum(spr_kg_P, na.rm = TRUE),
        fish_N_sum = sum(fish_kg_N, na.rm = TRUE),
        fish_P_sum = sum(fish_kg_P, na.rm = TRUE)
        )
    results <- rbind(results, cbind(start_date = NA, end_date = as.POSIXct(first_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), pre_first_date_sums))
  }
  
  # Iterate over each date range in precip_dates_times dataframe
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    start_date <- precip_dates_times$dateOff[i]
    end_date <- precip_dates_times$dateOff[i + 1]
    
    range_data <- tribs %>% filter(date_time >= start_date & date_time < end_date)
    
    if (nrow(range_data) > 0) {
      range_sums <- range_data %>%
        summarize(
          umc_N_sum = sum(umc_kg_N, na.rm = TRUE),
          umc_P_sum = sum(umc_kg_P, na.rm = TRUE),
          sny_N_sum = sum(sny_kg_N, na.rm = TRUE),
          sny_P_sum = sum(sny_kg_P, na.rm = TRUE),
          spr_N_sum = sum(spr_kg_N, na.rm = TRUE),
          spr_P_sum = sum(spr_kg_P, na.rm = TRUE),
          fish_N_sum = sum(fish_kg_N, na.rm = TRUE),
          fish_P_sum = sum(fish_kg_P, na.rm = TRUE)
        )
      results <- rbind(results, cbind(start_date = as.POSIXct(start_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), end_date = as.POSIXct(end_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), range_sums))
    }
  }
  
  # Handle the range after the last date specified in date_time
  last_date <- precip_dates_times$dateOff[nrow(precip_dates_times)]
  post_last_date_data <- tribs %>% filter(date_time >= last_date)
  
  if (nrow(post_last_date_data) > 0) {
    post_last_date_sums <- post_last_date_data %>%
      summarize(
        umc_N_sum = sum(umc_kg_N, na.rm = TRUE),
        umc_P_sum = sum(umc_kg_P, na.rm = TRUE),
        sny_N_sum = sum(sny_kg_N, na.rm = TRUE),
        sny_P_sum = sum(sny_kg_P, na.rm = TRUE),
        spr_N_sum = sum(spr_kg_N, na.rm = TRUE),
        spr_P_sum = sum(spr_kg_P, na.rm = TRUE),
        fish_N_sum = sum(fish_kg_N, na.rm = TRUE),
        fish_P_sum = sum(fish_kg_P, na.rm = TRUE)
      )
    results <- rbind(results, cbind(start_date = as.POSIXct(last_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), end_date = NA, post_last_date_sums))
  }
  
  return(results)
}

# Call the function with the dataframes
results <- sum_tribs(tribs, precip_dates_times)

working_budget <- data.frame(results)

working_budget <- working_budget %>%
  mutate(
    start_date = as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC"))

# LWR

sum_tribs<- function(tribs, precip_dates_times) {
  
  # Initialize the results dataframe
  results <- data.frame(start_date = character(), end_date = character(),
                        umc_N_lwr_sum = numeric(), umc_P_lwr_sum = numeric(), sny_N_lwr_sum = numeric(),
                        sny_P_lwr_sum = numeric(), spr_N_lwr_sum = numeric(), spr_P_lwr_sum = numeric(),
                        fish_N_lwr_sum = numeric(), fish_P_lwr_sum = numeric(), stringsAsFactors = FALSE)
  
  # Include the range before the first date specified in dateOff
  first_date <- precip_dates_times$dateOff[1]
  pre_first_date_data <- tribs %>% filter(date_time < first_date)
  
  if (nrow(pre_first_date_data) > 0) {
    pre_first_date_sums <- pre_first_date_data %>%
      summarize(
        umc_N_lwr_sum = sum(umc_kg_N_lwr, na.rm = TRUE),
        umc_P_lwr_sum = sum(umc_kg_P_lwr, na.rm = TRUE),
        sny_N_lwr_sum = sum(sny_kg_N_lwr, na.rm = TRUE),
        sny_P_lwr_sum = sum(sny_kg_P_lwr, na.rm = TRUE),
        spr_N_lwr_sum = sum(spr_kg_N_lwr, na.rm = TRUE),
        spr_P_lwr_sum = sum(spr_kg_P_lwr, na.rm = TRUE),
        fish_N_lwr_sum = sum(fish_kg_N_lwr, na.rm = TRUE),
        fish_P_lwr_sum = sum(fish_kg_P_lwr, na.rm = TRUE)
      )
    results <- rbind(results, cbind(start_date = NA, end_date = as.POSIXct(first_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), pre_first_date_sums))
  }
  
  # Iterate over each date range in precip_dates_times dataframe
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    start_date <- precip_dates_times$dateOff[i]
    end_date <- precip_dates_times$dateOff[i + 1]
    
    range_data <- tribs %>% filter(date_time >= start_date & date_time < end_date)
    
    if (nrow(range_data) > 0) {
      range_sums <- range_data %>%
        summarize(
          umc_N_lwr_sum = sum(umc_kg_N_lwr, na.rm = TRUE),
          umc_P_lwr_sum = sum(umc_kg_P_lwr, na.rm = TRUE),
          sny_N_lwr_sum = sum(sny_kg_N_lwr, na.rm = TRUE),
          sny_P_lwr_sum = sum(sny_kg_P_lwr, na.rm = TRUE),
          spr_N_lwr_sum = sum(spr_kg_N_lwr, na.rm = TRUE),
          spr_P_lwr_sum = sum(spr_kg_P_lwr, na.rm = TRUE),
          fish_N_lwr_sum = sum(fish_kg_N_lwr, na.rm = TRUE),
          fish_P_lwr_sum = sum(fish_kg_P_lwr, na.rm = TRUE)
        )
      results <- rbind(results, cbind(start_date = as.POSIXct(start_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), end_date = as.POSIXct(end_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), range_sums))
    }
  }
  
  # Handle the range after the last date specified in date_time
  last_date <- precip_dates_times$dateOff[nrow(precip_dates_times)]
  post_last_date_data <- tribs %>% filter(date_time >= last_date)
  
  if (nrow(post_last_date_data) > 0) {
    post_last_date_sums <- post_last_date_data %>%
      summarize(
        umc_N_lwr_sum = sum(umc_kg_N_lwr, na.rm = TRUE),
        umc_P_lwr_sum = sum(umc_kg_P_lwr, na.rm = TRUE),
        sny_N_lwr_sum = sum(sny_kg_N_lwr, na.rm = TRUE),
        sny_P_lwr_sum = sum(sny_kg_P_lwr, na.rm = TRUE),
        spr_N_lwr_sum = sum(spr_kg_N_lwr, na.rm = TRUE),
        spr_P_lwr_sum = sum(spr_kg_P_lwr, na.rm = TRUE),
        fish_N_lwr_sum = sum(fish_kg_N_lwr, na.rm = TRUE),
        fish_P_lwr_sum = sum(fish_kg_P_lwr, na.rm = TRUE)
      )
    results <- rbind(results, cbind(start_date = as.POSIXct(last_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), end_date = NA, post_last_date_sums))
  }
  
  return(results)
}

# Call the function with the dataframes
results <- sum_tribs(tribs, precip_dates_times)


# Merge with working_budget

results <- results %>%
  mutate(start_date = as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC"))

working_budget <- working_budget %>%
  inner_join(results, by = "end_date")

#UPR

sum_tribs<- function(tribs, precip_dates_times) {
  
  # Initialize the results dataframe
  results <- data.frame(start_date = character(), end_date = character(),
                        umc_N_upr_sum = numeric(), umc_P_upr_sum = numeric(), sny_N_upr_sum = numeric(),
                        sny_P_upr_sum = numeric(), spr_N_upr_sum = numeric(), spr_P_upr_sum = numeric(),
                        fish_N_upr_sum = numeric(), fish_P_upr_sum = numeric(), stringsAsFactors = FALSE)
  
  # Include the range before the first date specified in dateOff
  first_date <- precip_dates_times$dateOff[1]
  pre_first_date_data <- tribs %>% filter(date_time < first_date)
  
  if (nrow(pre_first_date_data) > 0) {
    pre_first_date_sums <- pre_first_date_data %>%
      summarize(
        umc_N_upr_sum = sum(umc_kg_N_upr, na.rm = TRUE),
        umc_P_upr_sum = sum(umc_kg_P_upr, na.rm = TRUE),
        sny_N_upr_sum = sum(sny_kg_N_upr, na.rm = TRUE),
        sny_P_upr_sum = sum(sny_kg_P_upr, na.rm = TRUE),
        spr_N_upr_sum = sum(spr_kg_N_upr, na.rm = TRUE),
        spr_P_upr_sum = sum(spr_kg_P_upr, na.rm = TRUE),
        fish_N_upr_sum = sum(fish_kg_N_upr, na.rm = TRUE),
        fish_P_upr_sum = sum(fish_kg_P_upr, na.rm = TRUE)
      )
    results <- rbind(results, cbind(start_date = NA, end_date = as.POSIXct(first_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), pre_first_date_sums))
  }
  
  # Iterate over each date range in precip_dates_times dataframe
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    start_date <- precip_dates_times$dateOff[i]
    end_date <- precip_dates_times$dateOff[i + 1]
    
    range_data <- tribs %>% filter(date_time >= start_date & date_time < end_date)
    
    if (nrow(range_data) > 0) {
      range_sums <- range_data %>%
        summarize(
          umc_N_upr_sum = sum(umc_kg_N_upr, na.rm = TRUE),
          umc_P_upr_sum = sum(umc_kg_P_upr, na.rm = TRUE),
          sny_N_upr_sum = sum(sny_kg_N_upr, na.rm = TRUE),
          sny_P_upr_sum = sum(sny_kg_P_upr, na.rm = TRUE),
          spr_N_upr_sum = sum(spr_kg_N_upr, na.rm = TRUE),
          spr_P_upr_sum = sum(spr_kg_P_upr, na.rm = TRUE),
          fish_N_upr_sum = sum(fish_kg_N_upr, na.rm = TRUE),
          fish_P_upr_sum = sum(fish_kg_P_upr, na.rm = TRUE)
        )
      results <- rbind(results, cbind(start_date = as.POSIXct(start_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), end_date = as.POSIXct(end_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), range_sums))
    }
  }
  
  # Handle the range after the last date specified in date_time
  last_date <- precip_dates_times$dateOff[nrow(precip_dates_times)]
  post_last_date_data <- tribs %>% filter(date_time >= last_date)
  
  if (nrow(post_last_date_data) > 0) {
    post_last_date_sums <- post_last_date_data %>%
      summarize(
        umc_N_upr_sum = sum(umc_kg_N_upr, na.rm = TRUE),
        umc_P_upr_sum = sum(umc_kg_P_upr, na.rm = TRUE),
        sny_N_upr_sum = sum(sny_kg_N_upr, na.rm = TRUE),
        sny_P_upr_sum = sum(sny_kg_P_upr, na.rm = TRUE),
        spr_N_upr_sum = sum(spr_kg_N_upr, na.rm = TRUE),
        spr_P_upr_sum = sum(spr_kg_P_upr, na.rm = TRUE),
        fish_N_upr_sum = sum(fish_kg_N_upr, na.rm = TRUE),
        fish_P_upr_sum = sum(fish_kg_P_upr, na.rm = TRUE)
      )
    results <- rbind(results, cbind(start_date = as.POSIXct(last_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), end_date = NA, post_last_date_sums))
  }
  
  return(results)
}

# Call the function with the dataframes
results <- sum_tribs(tribs, precip_dates_times)

# Merge with working_budget

results <- results %>%
  mutate(
    start_date = as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC"))


working_budget <- working_budget %>%
  inner_join(results, by = "end_date")

# Wet deposition

#N

working_budget <- working_budget %>%
  inner_join(wetN, by = c("end_date" = "dateOff"))

#P
#Determine Overlapping Days: For each range in precip_dates_times, calculate the number of days each record from dry overlaps within the range.
#Compute Deposition Totals: Multiply these overlapping days by the daily rates to get the total deposition for each part of the range, then sum these totals.

calculate_totals <- function(wetP, ranges) {
  results <- list()  # To store results for each range
  
  # Ensure dates are properly formatted
  wetP$start_date <- as.Date(wetP$start_date)
  wetP$end_date <- as.Date(wetP$end_date)
  ranges$dateOn <- as.Date(ranges$dateOn)
  ranges$dateOff <- as.Date(ranges$dateOff)
  
  # Loop through each date range in precip_dates_times
  for (i in seq_len(nrow(ranges))) {
    current_start <- ranges$dateOn[i]
    current_end <- ranges$dateOff[i]
    
    # Filter wetP for records that overlap with the current range
    filtered <- wetP %>%
      filter(start_date <= current_end & end_date >= current_start) %>%
      # Calculate overlapping days
      mutate(
        overlap_start = pmax(start_date, current_start),
        overlap_end = pmin(end_date, current_end),
        overlapping_days = as.integer(overlap_end - overlap_start + 1)
      )
    
    # Check if 'overlapping_days' exists
    if ("overlapping_days" %in% names(filtered)) {
      # Compute total TN and TP depositions
      total_TP <- sum(filtered$TP_wet_kg_d * filtered$overlapping_days, na.rm = TRUE)
      
      # Store the result
      results[[i]] <- data.frame(
        range_start = current_start,
        range_end = current_end,
        total_TP = total_TP
      )
    } else {
      # Handle cases where overlapping_days could not be computed (e.g., no overlap)
      results[[i]] <- data.frame(
        range_start = current_start,
        range_end = current_end,
        total_TP = 0
      )
    }
  }
  
  # Combine all results into a single dataframe
  result_df <- do.call(rbind, results)
  return(result_df)
}

# Call relevant dataframes
result <- calculate_totals(wetP, precip_dates_times)

result <- result %>%
  mutate(start_date = precip_dates_times$dateOn, end_date = precip_dates_times$dateOff) %>%
  select(start_date, end_date, total_TP) %>%
  rename(TP_wet_kg = total_TP)

working_budget <- working_budget %>%
  inner_join(result, by = "end_date")

# Dry Deposition

#Determine Overlapping Days: For each range in precip_dates_times, calculate the number of days each record from dry overlaps within the range.
#Compute Deposition Totals: Multiply these overlapping days by the daily rates to get the total deposition for each part of the range, then sum these totals.

calculate_totals <- function(dry, ranges) {
  results <- list()  # To store results for each range
  
  # Ensure dates are properly formatted
  dry$start_date <- as.Date(dry$start_date)
  dry$end_date <- as.Date(dry$end_date)
  ranges$dateOn <- as.Date(ranges$dateOn)
  ranges$dateOff <- as.Date(ranges$dateOff)
  
  # Loop through each date range in precip_dates_times
  for (i in seq_len(nrow(ranges))) {
    current_start <- ranges$dateOn[i]
    current_end <- ranges$dateOff[i]
    
    # Filter dry for records that overlap with the current range
    filtered <- dry %>%
      filter(start_date <= current_end & end_date >= current_start) %>%
      # Calculate overlapping days
      mutate(
        overlap_start = pmax(start_date, current_start),
        overlap_end = pmin(end_date, current_end),
        overlapping_days = as.integer(overlap_end - overlap_start + 1)
      )
    
    # Check if 'overlapping_days' exists
    if ("overlapping_days" %in% names(filtered)) {
      # Compute total TN and TP depositions
      total_TN <- sum(filtered$TN_dry_kg_d * filtered$overlapping_days, na.rm = TRUE)
      total_TP <- sum(filtered$TP_dry_kg_d * filtered$overlapping_days, na.rm = TRUE)
      
      # Store the result
      results[[i]] <- data.frame(
        range_start = current_start,
        range_end = current_end,
        total_TN = total_TN,
        total_TP = total_TP
      )
    } else {
      # Handle cases where overlapping_days could not be computed (e.g., no overlap)
      results[[i]] <- data.frame(
        range_start = current_start,
        range_end = current_end,
        total_TN = 0,
        total_TP = 0
      )
    }
  }
  
  # Combine all results into a single dataframe
  result_df <- do.call(rbind, results)
  return(result_df)
}

# Call relevant dataframes
result <- calculate_totals(dry, precip_dates_times)

# Merge with working budget

result <- result %>%
  mutate(start_date = precip_dates_times$dateOn, end_date = precip_dates_times$dateOff) %>%
  select(start_date, end_date, total_TN, total_TP) %>%
  rename(TN_dry_kg = "total_TN", TP_dry_kg = "total_TP")

working_budget <- working_budget %>%
  inner_join(result, by = "end_date")

# Septic (g/cap/d)

working_budget$H_N_lwr <- 1.1*(2.5*115)*7*(1/1000)
working_budget$H_N <- 10.1*(2.5*115)*7*(1/1000)
working_budget$H_N_upr <- 71.6*(2.5*115)*7*(1/1000)

working_budget$H_P_lwr <- 0*(2.5*115)*7*(1/1000)
working_budget$H_P <- 1.4*(2.5*115)*7*(1/1000)
working_budget$H_P_upr <- 15.9*(2.5*115)*7*(1/1000)

# Lake

# Merge with working_budget

working_budget <- working_budget %>%
  left_join(lake, by=c("end_date" = "date_time"))

# Add burial

working_budget$S_N_lwr <- 41
working_budget$S_N <- 115
working_budget$S_N_upr <- 174

working_budget$S_P_lwr <- 32
working_budget$S_P <- 47
working_budget$S_P_upr <- 123

# Add water budget

water_budget <- read.csv("C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\WaterBudget.csv", header = TRUE, sep = ",")

water_budget$end_date <- as.POSIXct(water_budget$end_date,  format = "%Y-%m-%d", tz = "UTC")

water <- water_budget %>%
  select(end_date, lake_volume, lake_volume_lwr, lake_volume_upr, resid, resid_lwr, resid_upr, lmc_m3, lmc_m3_lwr, lmc_m3_upr)

working_budget$end_date <- as.Date(working_budget$end_date,  format = "%Y-%m-%d", tz = "UTC")

working_budget <- working_budget %>%
  inner_join(water_budget, by = "end_date")

working_budget <- working_budget %>%
  select(-start_date.x, -start_date.y, -start_date.x.x, -start_date.y.y, -start_date.y.y.y)%>%
  rename(start_date = "start_date.x.x.x")


##### Create the model----

# Apply transformation to all columns except those in exclude_cols
# Define columns to exclude
exclude_cols <- c("start_date", "end_date", "mean_tn", "mean_tp", "kg_N", "kg_P")

working_budget <- working_budget %>%
  mutate(across(-all_of(exclude_cols), ~ ifelse(is.na(.) | . == -Inf | . == Inf, 0, .)))

# Calculate M for subsequent rows using 10/1 as the start M (mean)

working_budget$mean_tn[1] <- 274
working_budget$kg_N[1] <- working_budget$mean_tn[1]*10^(-9)*working_budget$lake_volume[1]*1000

working_budget$mean_tp[1] <- 5.1
working_budget$kg_P[1] <- working_budget$mean_tp[1]*10^(-9)*working_budget$lake_volume[1]*1000

working_budget$kg_N_est <- NA
working_budget$kg_N_est[1] <- working_budget$kg_N[1]
working_budget$TN_conc_est <- NA
working_budget$TN_conc_est[1] <- working_budget$mean_tn[1]

working_budget$kg_P_est <- NA
working_budget$kg_P_est[1] <- working_budget$kg_P[1]
working_budget$TP_conc_est <- NA
working_budget$TP_conc_est[1] <- working_budget$mean_tp[1]


for (i in 2:nrow(working_budget)) {
  working_budget$kg_N_est[i] <- working_budget$kg_N_est[i-1] + working_budget$umc_N_sum[i] + working_budget$sny_N_sum[i] + working_budget$spr_N_sum[i] + working_budget$fish_N_sum[i] + working_budget$TN_wet_kg[i] + working_budget$TN_dry_kg[i] + working_budget$H_N[i] - working_budget$S_N[i] - ((working_budget$TN_conc_est[i-1]*10^(-9))*(working_budget$lmc_m3[i]*1000))
  
  working_budget$TN_conc_est[i] <- (working_budget$kg_N_est[i]*10^(9))/(working_budget$lake_volume[i]*1000)
  
  working_budget$kg_P_est[i] <- working_budget$kg_P_est[i-1] + working_budget$umc_P_sum[i] + working_budget$sny_P_sum[i] + working_budget$spr_P_sum[i] + working_budget$fish_P_sum[i] + working_budget$TP_wet_kg[i]  + working_budget$TP_dry_kg[i] + working_budget$H_P[i] - working_budget$S_P[i] - ((working_budget$TP_conc_est[i-1]*10^(-9))*(working_budget$lmc_m3[i]*1000))
  
  working_budget$TP_conc_est[i] <- (working_budget$kg_P_est[i]*10^(9))/(working_budget$lake_volume[i]*1000)
  
}

# LWR

working_budget$mean_tn_lwr[1] <- 226
working_budget$kg_N_lwr[1] <- working_budget$mean_tn_lwr[1]*10^(-9)*working_budget$lake_volume_lwr[1]*1000

working_budget$mean_tp_lwr[1] <- 2.5
working_budget$kg_P_lwr[1] <- working_budget$mean_tp_lwr[1]*10^(-9)*working_budget$lake_volume_lwr[1]*1000

working_budget$kg_N_est_lwr <- NA
working_budget$kg_N_est_lwr[1] <- working_budget$kg_N_lwr[1]

working_budget$TN_conc_est_lwr <- NA
working_budget$TN_conc_est_lwr[1] <- working_budget$mean_tn_lwr[1]

working_budget$kg_P_est_lwr <- NA
working_budget$kg_P_est_lwr[1] <- working_budget$kg_P_lwr[1]

working_budget$TP_conc_est_lwr <- NA
working_budget$TP_conc_est_lwr[1] <- working_budget$mean_tp_lwr[1]


for (i in 2:nrow(working_budget)) {
  working_budget$kg_N_est_lwr[i] <- working_budget$kg_N_est_lwr[i-1] + working_budget$umc_N_lwr_sum[i] + working_budget$sny_N_lwr_sum[i] + working_budget$spr_N_lwr_sum[i] + working_budget$fish_N_lwr_sum[i] + working_budget$TN_wet_kg[i] + working_budget$TN_dry_kg[i] + working_budget$H_N_lwr[i] - working_budget$S_N_lwr[i] - ((working_budget$TN_conc_est_lwr[i-1]*10^(-9))*(working_budget$lmc_m3_lwr[i]*1000))
  
  working_budget$TN_conc_est_lwr[i] <- (working_budget$kg_N_est_lwr[i]*10^(9))/(working_budget$lake_volume_upr[i]*1000)
  
  working_budget$kg_P_est_lwr[i] <- working_budget$kg_P_est_lwr[i-1] + working_budget$umc_P_lwr_sum[i] + working_budget$sny_P_lwr_sum[i] + working_budget$spr_P_lwr_sum[i] + working_budget$fish_P_lwr_sum[i] + working_budget$TP_wet_kg[i] + working_budget$TP_dry_kg[i] + working_budget$H_P_lwr[i] - working_budget$S_P_lwr[i] - ((working_budget$TP_conc_est_lwr[i-1]*10^(-9))*(working_budget$lmc_m3_lwr[i]*1000))
  
  working_budget$TP_conc_est_lwr[i] <- (working_budget$kg_P_est_lwr[i]*10^(9))/(working_budget$lake_volume_upr[i]*1000)
  
}

# UPR

working_budget$mean_tn_upr[1] <- 314
working_budget$kg_N_upr[1] <- working_budget$mean_tn_upr[1]*10^(-9)*working_budget$lake_volume_upr[1]*1000

working_budget$mean_tp_upr[1] <- 10
working_budget$kg_P_upr[1] <- working_budget$mean_tp_upr[1]*10^(-9)*working_budget$lake_volume_upr[1]*1000

working_budget$kg_N_est_upr <- NA
working_budget$kg_N_est_upr[1] <- working_budget$kg_N_upr[1]

working_budget$TN_conc_est_upr <- NA
working_budget$TN_conc_est_upr[1] <- working_budget$mean_tn_upr[1]

working_budget$kg_P_est_upr <- NA
working_budget$kg_P_est_upr[1] <- working_budget$kg_P_upr[1]

working_budget$TP_conc_est_upr <- NA
working_budget$TP_conc_est_upr[1] <- working_budget$mean_tp_upr[1]


for (i in 2:nrow(working_budget)) {
  working_budget$kg_N_est_upr[i] <- working_budget$kg_N_est_upr[i-1] + working_budget$umc_N_upr_sum[i] + working_budget$sny_N_upr_sum[i] + working_budget$spr_N_upr_sum[i] + working_budget$fish_N_upr_sum[i] + working_budget$TN_wet_kg[i] + working_budget$TN_dry_kg[i] + working_budget$H_N_upr[i] - working_budget$S_N_upr[i] - ((working_budget$TN_conc_est_upr[i-1]*10^(-9))*(working_budget$lmc_m3_upr[i]*1000))
  
  working_budget$TN_conc_est_upr[i] <- (working_budget$kg_N_est_upr[i]*10^(9))/(working_budget$lake_volume_lwr[i]*1000)
  
  working_budget$kg_P_est_upr[i] <- working_budget$kg_P_est_upr[i-1] + working_budget$umc_P_upr_sum[i] + working_budget$sny_P_upr_sum[i] + working_budget$spr_P_upr_sum[i] + working_budget$fish_P_upr_sum[i] + working_budget$TP_wet_kg[i] + working_budget$TP_dry_kg[i] + working_budget$H_P_upr[i] - working_budget$S_P_upr[i] - ((working_budget$TP_conc_est_upr[i-1]*10^(-9))*(working_budget$lmc_m3_upr[i]*1000))
  
  working_budget$TP_conc_est_upr[i] <- (working_budget$kg_P_est_upr[i]*10^(9))/(working_budget$lake_volume_lwr[i]*1000)
  
}

working_budget <- working_budget %>%
  rename(umc_P_lwr = umc_P_lwr_sum, sny_P_lwr = sny_P_lwr_sum, spr_P_lwr = spr_P_lwr_sum, fish_P_lwr = fish_P_lwr_sum,
         umc_P = umc_P_sum, sny_P =  sny_P_sum, spr_P = spr_P_sum, fish_P = fish_P_sum,
         umc_P_upr = umc_P_upr_sum, sny_P_upr = sny_P_upr_sum, spr_P_upr = spr_P_upr_sum, fish_P_upr = fish_P_upr_sum, 
         umc_N_lwr = umc_N_lwr_sum, sny_N_lwr = sny_N_lwr_sum, spr_N_lwr = spr_N_lwr_sum, fish_N_lwr = fish_N_lwr_sum,
         umc_N = umc_N_sum, sny_N =  sny_N_sum, spr_N = spr_N_sum, fish_N = fish_N_sum,
         umc_N_upr = umc_N_upr_sum, sny_N_upr = sny_N_upr_sum, spr_N_upr = spr_N_upr_sum, fish_N_upr = fish_N_upr_sum,
  )


write.csv(working_budget, file="C:\\Users\\brook\\Documents\\PhD\\Dissertation\\Chp_1\\LakeMcDonaldNutrientBudget\\2_incremental\\NutrientBudget.csv", row.names=FALSE)

