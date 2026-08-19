# ==============================================================================
# Construct Lake McDonald nutrient budget
#
# Purpose:
#   Combine tributary nutrient loads, atmospheric deposition, septic inputs,
#   sediment burial, lake nutrient concentrations, lake volume, and outflow
#   into a weekly nutrient mass-balance model for Lake McDonald.
#
# Notes:
#   - Nutrient-budget intervals follow NADP precipitation sampling periods.
#   - Tributary nutrient loads are summed separately for best approximation and lower and upper estimates.
#   - Wet P and dry N/P deposition records from FLBS are allocated to NADP intervals
#     according to the number of overlapping days.
#   - Best approximation and lower and upper nutrient-budget models are calculated separately.
# ==============================================================================

 
# 1. Read nutrient-budget inputs ----------------------------------------------

# 1.1 Tributary nutrient loads -------------------------------------------------

# Read hourly reconstructed N and P loads for the four tributaries.

tribs <- readr::read_csv(
  here(
    "2_incremental",
    "Ft.csv"
  ),
  col_types = cols(
    date_time = col_datetime(format = ""),
    .default = col_double()
  )
)

# Remove "_hr" from variable names because values will subsequently be
# aggregated over NADP sampling intervals.

tribs <- tribs %>%
  select(
    date_time,
    
    # Mean nutrient loads
    umc_kg_N_hr,
    umc_kg_P_hr,
    sny_kg_N_hr,
    sny_kg_P_hr,
    spr_kg_N_hr,
    spr_kg_P_hr,
    fish_kg_N_hr,
    fish_kg_P_hr,
    
    # Lower nutrient loads
    umc_kg_N_hr_lwr,
    umc_kg_P_hr_lwr,
    sny_kg_N_hr_lwr,
    sny_kg_P_hr_lwr,
    spr_kg_N_hr_lwr,
    spr_kg_P_hr_lwr,
    fish_kg_N_hr_lwr,
    fish_kg_P_hr_lwr,
    
    # Upper nutrient loads
    umc_kg_N_hr_upr,
    umc_kg_P_hr_upr,
    sny_kg_N_hr_upr,
    sny_kg_P_hr_upr,
    spr_kg_N_hr_upr,
    spr_kg_P_hr_upr,
    fish_kg_N_hr_upr,
    fish_kg_P_hr_upr
  ) %>%
  
  # Rename hourly nutrient loads
  rename(
    umc_kg_N = umc_kg_N_hr,
    umc_kg_P = umc_kg_P_hr,
    sny_kg_N = sny_kg_N_hr,
    sny_kg_P = sny_kg_P_hr,
    spr_kg_N = spr_kg_N_hr,
    spr_kg_P = spr_kg_P_hr,
    fish_kg_N = fish_kg_N_hr,
    fish_kg_P = fish_kg_P_hr,
    
    umc_kg_N_lwr = umc_kg_N_hr_lwr,
    umc_kg_P_lwr = umc_kg_P_hr_lwr,
    sny_kg_N_lwr = sny_kg_N_hr_lwr,
    sny_kg_P_lwr = sny_kg_P_hr_lwr,
    spr_kg_N_lwr = spr_kg_N_hr_lwr,
    spr_kg_P_lwr = spr_kg_P_hr_lwr,
    fish_kg_N_lwr = fish_kg_N_hr_lwr,
    fish_kg_P_lwr = fish_kg_P_hr_lwr,
    
    umc_kg_N_upr = umc_kg_N_hr_upr,
    umc_kg_P_upr = umc_kg_P_hr_upr,
    sny_kg_N_upr = sny_kg_N_hr_upr,
    sny_kg_P_upr = sny_kg_P_hr_upr,
    spr_kg_N_upr = spr_kg_N_hr_upr,
    spr_kg_P_upr = spr_kg_P_hr_upr,
    fish_kg_N_upr = fish_kg_N_hr_upr,
    fish_kg_P_upr = fish_kg_P_hr_upr
  ) %>%
  
  # Ensure date/time is formatted as UTC
  mutate(
    date_time = as.POSIXct(
      date_time,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  )


# 1.2 Lake nutrient concentrations and masses ---------------------------------

lake <- readr::read_csv(
  here(
    "0_data",
    "LM_chemistry.csv"
  ),
  col_types = cols(
    date_time = col_datetime(format = "")
  )
)


# Retain measured lake concentrations and corresponding nutrient masses.

lake <- lake %>%
  dplyr::select(
    date_time,
    mean_tn,
    mean_tp,
    min_tn,
    max_tn,
    min_tp,
    max_tp,
    kg_N,
    kg_N_lwr,
    kg_N_upr,
    kg_P,
    kg_P_lwr,
    kg_P_upr
  )


lake <- lake %>%
  mutate(
    date_time = as.POSIXct(
      date_time,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  )


# 1.3 Wet nitrogen deposition -------------------------------------------------

wetN <- readr::read_csv(
  here(
    "2_incremental",
    "Fw_N.csv"
  ),
  col_types = cols(
    dateOn = col_datetime(format = ""),
    dateOff = col_datetime(format = "")
  )
)


wetN <- wetN %>%
  mutate(
    dateOn = as.POSIXct(
      dateOn,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    dateOff = as.POSIXct(
      dateOff,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  )

# 1.4 Wet phosphorus deposition -----------------------------------------------

wetP <- read.csv(
  here(
    "2_incremental",
    "Fw_P.csv"
  ),
  header = TRUE,
  sep = ","
)


# Convert wet-P collection date.

wetP$Date <- as.Date(
  wetP$Date,
  format = "%Y-%m-%d",
  tz = "UTC"
)


# 1.5 Dry nitrogen and phosphorus deposition ----------------------------------

dry <- read.csv(
  here(
    "2_incremental",
    "FD.csv"
  ),
  header = TRUE,
  sep = ","
)


# Convert dry-deposition collection date.

dry$Date <- as.Date(
  dry$Date,
  format = "%Y-%m-%d",
  tz = "UTC"
)


# 2. Convert deposition records to daily rates --------------------------------

# Calculate the number of days represented by each dry-deposition sample,
# then divide total deposited mass by interval length to obtain kg/day.

dry <- dry %>%
  dplyr::mutate(
    date_diff = as.integer(
      Date - lag(Date)
    ),
    TN_dry_kg_d = TN_dry_kg / date_diff,
    TP_dry_kg_d = TP_dry_kg / date_diff,
    start_date = lag(Date),
    end_date = Date
  ) %>%
  select(
    start_date,
    end_date,
    date_diff,
    TN_dry_kg_d,
    TP_dry_kg_d
  )


# Repeat for wet phosphorus deposition.

wetP <- wetP %>%
  arrange(Date) %>%
  dplyr::mutate(
    date_diff = as.integer(
      Date - dplyr::lag(Date)
    ),
    TP_wet_kg_d = TP_wet_kg / date_diff,
    start_date = dplyr::lag(Date),
    end_date = Date
  ) %>%
  dplyr::select(
    start_date,
    end_date,
    date_diff,
    TP_wet_kg_d
  )


# 3. Define nutrient-budget intervals -----------------------------------------

# NADP dateOn/dateOff pairs define the temporal intervals used throughout
# the nutrient budget.

precip_dates_times <- wetN %>%
  select(
    dateOn,
    dateOff
  )


# 4. Sum tributary nutrient loads: best approximation -----------------------------

# Sum hourly best approximations of TN and TP loads from each tributary within each NADP
# sampling interval.
#
# The function also retains:
#   1. observations before the first NADP interval
#   2. observations after the final NADP interval

sum_tribs <- function(
    tribs,
    precip_dates_times
) {
  
  # Initialize results dataframe
  
  results <- data.frame(
    start_date = character(),
    end_date = character(),
    umc_N_sum = numeric(),
    umc_P_sum = numeric(),
    sny_N_sum = numeric(),
    sny_P_sum = numeric(),
    spr_N_sum = numeric(),
    spr_P_sum = numeric(),
    fish_N_sum = numeric(),
    fish_P_sum = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  # Include range before first NADP dateOff
  
  first_date <- precip_dates_times$dateOff[1]
  
  pre_first_date_data <- tribs %>%
    filter(
      date_time < first_date
    )
  
  
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
    
    results <- rbind(
      results,
      cbind(
        start_date = NA,
        end_date = as.POSIXct(
          first_date,
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        ),
        pre_first_date_sums
      )
    )
  }
  
  
  # Sum nutrient loads within each NADP interval
  
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    
    start_date <- precip_dates_times$dateOff[i]
    end_date <- precip_dates_times$dateOff[i + 1]
    
    range_data <- tribs %>%
      filter(
        date_time >= start_date &
          date_time < end_date
      )
    
    
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
      
      results <- rbind(
        results,
        cbind(
          start_date = as.POSIXct(
            start_date,
            format = "%Y-%m-%d %H:%M:%S",
            tz = "UTC"
          ),
          end_date = as.POSIXct(
            end_date,
            format = "%Y-%m-%d %H:%M:%S",
            tz = "UTC"
          ),
          range_sums
        )
      )
    }
  }
  
  
  # Include range after final NADP dateOff
  
  last_date <- precip_dates_times$dateOff[
    nrow(precip_dates_times)
  ]
  
  post_last_date_data <- tribs %>%
    filter(
      date_time >= last_date
    )
  
  
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
    
    results <- rbind(
      results,
      cbind(
        start_date = as.POSIXct(
          last_date,
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        ),
        end_date = NA,
        post_last_date_sums
      )
    )
  }
  
  return(results)
}


results <- sum_tribs(
  tribs,
  precip_dates_times
)


# Initialize working nutrient budget

working_budget <- data.frame(
  results
)


working_budget <- working_budget %>%
  mutate(
    start_date = as.POSIXct(
      start_date,
      origin = "1970-01-01",
      tz = "UTC"
    )
  )


# 5. Sum tributary nutrient loads: lower estimates ----------------------------

# Repeat the interval aggregation using lower-bound tributary TN and TP loads.

sum_tribs <- function(
    tribs,
    precip_dates_times
) {
  
  # Initialize results dataframe
  
  results <- data.frame(
    start_date = character(),
    end_date = character(),
    umc_N_lwr_sum = numeric(),
    umc_P_lwr_sum = numeric(),
    sny_N_lwr_sum = numeric(),
    sny_P_lwr_sum = numeric(),
    spr_N_lwr_sum = numeric(),
    spr_P_lwr_sum = numeric(),
    fish_N_lwr_sum = numeric(),
    fish_P_lwr_sum = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  # Include range before first NADP dateOff
  
  first_date <- precip_dates_times$dateOff[1]
  
  pre_first_date_data <- tribs %>%
    filter(
      date_time < first_date
    )
  
  
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
    
    results <- rbind(
      results,
      cbind(
        start_date = NA,
        end_date = as.POSIXct(
          first_date,
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        ),
        pre_first_date_sums
      )
    )
  }
  
  
  # Sum lower-bound nutrient loads within each NADP interval
  
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    
    start_date <- precip_dates_times$dateOff[i]
    end_date <- precip_dates_times$dateOff[i + 1]
    
    range_data <- tribs %>%
      filter(
        date_time >= start_date &
          date_time < end_date
      )
    
    
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
      
      results <- rbind(
        results,
        cbind(
          start_date = as.POSIXct(
            start_date,
            format = "%Y-%m-%d %H:%M:%S",
            tz = "UTC"
          ),
          end_date = as.POSIXct(
            end_date,
            format = "%Y-%m-%d %H:%M:%S",
            tz = "UTC"
          ),
          range_sums
        )
      )
    }
  }
  
  
  # Include range after final NADP dateOff
  
  last_date <- precip_dates_times$dateOff[
    nrow(precip_dates_times)
  ]
  
  post_last_date_data <- tribs %>%
    filter(
      date_time >= last_date
    )
  
  
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
    
    results <- rbind(
      results,
      cbind(
        start_date = as.POSIXct(
          last_date,
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        ),
        end_date = NA,
        post_last_date_sums
      )
    )
  }
  
  return(results)
}


results <- sum_tribs(
  tribs,
  precip_dates_times
)


# Format start date before joining to working budget

results <- results %>%
  mutate(
    start_date = as.POSIXct(
      start_date,
      origin = "1970-01-01",
      tz = "UTC"
    )
  )


# Add lower-bound tributary loads

working_budget <- working_budget %>%
  inner_join(
    results,
    by = "end_date"
  )


# 6. Sum tributary nutrient loads: upper estimates ----------------------------

# Repeat the interval aggregation using upper-bound tributary TN and TP loads.

sum_tribs <- function(
    tribs,
    precip_dates_times
) {
  
  # Initialize results dataframe
  
  results <- data.frame(
    start_date = character(),
    end_date = character(),
    umc_N_upr_sum = numeric(),
    umc_P_upr_sum = numeric(),
    sny_N_upr_sum = numeric(),
    sny_P_upr_sum = numeric(),
    spr_N_upr_sum = numeric(),
    spr_P_upr_sum = numeric(),
    fish_N_upr_sum = numeric(),
    fish_P_upr_sum = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  # Include range before first NADP dateOff
  
  first_date <- precip_dates_times$dateOff[1]
  
  pre_first_date_data <- tribs %>%
    filter(
      date_time < first_date
    )
  
  
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
    
    results <- rbind(
      results,
      cbind(
        start_date = NA,
        end_date = as.POSIXct(
          first_date,
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        ),
        pre_first_date_sums
      )
    )
  }
  
  
  # Sum upper-bound nutrient loads within each NADP interval
  
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    
    start_date <- precip_dates_times$dateOff[i]
    end_date <- precip_dates_times$dateOff[i + 1]
    
    range_data <- tribs %>%
      filter(
        date_time >= start_date &
          date_time < end_date
      )
    
    
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
      
      results <- rbind(
        results,
        cbind(
          start_date = as.POSIXct(
            start_date,
            format = "%Y-%m-%d %H:%M:%S",
            tz = "UTC"
          ),
          end_date = as.POSIXct(
            end_date,
            format = "%Y-%m-%d %H:%M:%S",
            tz = "UTC"
          ),
          range_sums
        )
      )
    }
  }
  
  
  # Include range after final NADP dateOff
  
  last_date <- precip_dates_times$dateOff[
    nrow(precip_dates_times)
  ]
  
  post_last_date_data <- tribs %>%
    filter(
      date_time >= last_date
    )
  
  
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
    
    results <- rbind(
      results,
      cbind(
        start_date = as.POSIXct(
          last_date,
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        ),
        end_date = NA,
        post_last_date_sums
      )
    )
  }
  
  return(results)
}


# Apply upper-bound function

results <- sum_tribs(
  tribs,
  precip_dates_times
)


# Format start date before joining to working budget

results <- results %>%
  mutate(
    start_date = as.POSIXct(
      start_date,
      origin = "1970-01-01",
      tz = "UTC"
    )
  )


# Add upper-bound tributary loads

working_budget <- working_budget %>%
  inner_join(
    results,
    by = "end_date"
  )


# 7. Add wet nitrogen deposition ----------------------------------------------

# Wet N is already reported over the same NADP intervals, so match records
# directly using the NADP dateOff/end_date.

working_budget <- working_budget %>%
  inner_join(
    wetN,
    by = c(
      "end_date" = "dateOff"
    )
  )


# 8. Add wet phosphorus deposition --------------------------------------------

# Wet P sampling intervals do not necessarily align with NADP intervals.
#
# For each NADP interval:
#   1. identify wet-P records that overlap the interval
#   2. calculate the number of overlapping days
#   3. multiply daily P deposition by overlapping days
#   4. sum all contributions within that NADP interval

calculate_totals <- function(
    wetP,
    ranges
) {
  
  # Store results for each NADP interval
  
  results <- list()
  
  
  # Ensure dates are formatted consistently
  
  wetP$start_date <- as.Date(
    wetP$start_date
  )
  
  wetP$end_date <- as.Date(
    wetP$end_date
  )
  
  ranges$dateOn <- as.Date(
    ranges$dateOn
  )
  
  ranges$dateOff <- as.Date(
    ranges$dateOff
  )
  
  
  # Loop through each NADP date range
  
  for (i in seq_len(nrow(ranges))) {
    
    current_start <- ranges$dateOn[i]
    current_end <- ranges$dateOff[i]
    
    
    # Identify wet-P records overlapping the current NADP interval
    
    filtered <- wetP %>%
      filter(
        start_date <= current_end &
          end_date >= current_start
      ) %>%
      
      # Calculate number of overlapping days
      
      mutate(
        overlap_start = pmax(
          start_date,
          current_start
        ),
        overlap_end = pmin(
          end_date,
          current_end
        ),
        overlapping_days = as.integer(
          overlap_end -
            overlap_start +
            1
        )
      )
    
    
    # Calculate total wet P deposition within overlapping period
    
    if ("overlapping_days" %in% names(filtered)) {
      
      total_TP <- sum(
        filtered$TP_wet_kg_d *
          filtered$overlapping_days,
        na.rm = TRUE
      )
      
      
      # Store result
      
      results[[i]] <- data.frame(
        range_start = current_start,
        range_end = current_end,
        total_TP = total_TP
      )
      
    } else {
      
      # Return zero if no overlap can be calculated
      
      results[[i]] <- data.frame(
        range_start = current_start,
        range_end = current_end,
        total_TP = 0
      )
    }
  }
  
  
  # Combine all NADP intervals into one dataframe
  
  result_df <- do.call(
    rbind,
    results
  )
  
  return(result_df)
}


# Apply wet-P overlap function

result <- calculate_totals(
  wetP,
  precip_dates_times
)


# Match calculated totals to original NADP timestamps

result <- result %>%
  mutate(
    start_date = precip_dates_times$dateOn,
    end_date = precip_dates_times$dateOff
  ) %>%
  select(
    start_date,
    end_date,
    total_TP
  ) %>%
  rename(
    TP_wet_kg = total_TP
  )


# Add wet P to working nutrient budget

working_budget <- working_budget %>%
  inner_join(
    result,
    by = "end_date"
  )


# 9. Add dry nitrogen and phosphorus deposition -------------------------------

# Dry-deposition intervals also do not necessarily align with NADP intervals.
#
# For each NADP interval:
#   1. identify overlapping dry-deposition records
#   2. determine number of overlapping days
#   3. multiply daily N and P deposition by overlapping days
#   4. sum total dry N and P deposition

calculate_totals <- function(
    dry,
    ranges
) {
  
  # Store results for each NADP interval
  
  results <- list()
  
  
  # Ensure dates are formatted consistently
  
  dry$start_date <- as.Date(
    dry$start_date
  )
  
  dry$end_date <- as.Date(
    dry$end_date
  )
  
  ranges$dateOn <- as.Date(
    ranges$dateOn
  )
  
  ranges$dateOff <- as.Date(
    ranges$dateOff
  )
  
  
  # Loop through each NADP date range
  
  for (i in seq_len(nrow(ranges))) {
    
    current_start <- ranges$dateOn[i]
    current_end <- ranges$dateOff[i]
    
    
    # Identify dry-deposition records overlapping current NADP interval
    
    filtered <- dry %>%
      filter(
        start_date <= current_end &
          end_date >= current_start
      ) %>%
      
      # Calculate overlapping days
      
      mutate(
        overlap_start = pmax(
          start_date,
          current_start
        ),
        overlap_end = pmin(
          end_date,
          current_end
        ),
        overlapping_days = as.integer(
          overlap_end -
            overlap_start +
            1
        )
      )
    
    
    # Calculate total dry TN and TP deposition
    
    if ("overlapping_days" %in% names(filtered)) {
      
      total_TN <- sum(
        filtered$TN_dry_kg_d *
          filtered$overlapping_days,
        na.rm = TRUE
      )
      
      total_TP <- sum(
        filtered$TP_dry_kg_d *
          filtered$overlapping_days,
        na.rm = TRUE
      )
      
      
      # Store result
      
      results[[i]] <- data.frame(
        range_start = current_start,
        range_end = current_end,
        total_TN = total_TN,
        total_TP = total_TP
      )
      
    } else {
      
      # Return zero if no overlap can be calculated
      
      results[[i]] <- data.frame(
        range_start = current_start,
        range_end = current_end,
        total_TN = 0,
        total_TP = 0
      )
    }
  }
  
  
  # Combine all NADP intervals into one dataframe
  
  result_df <- do.call(
    rbind,
    results
  )
  
  return(result_df)
}


# Apply dry-deposition overlap function

result <- calculate_totals(
  dry,
  precip_dates_times
)


# Match calculated totals to original NADP timestamps

result <- result %>%
  mutate(
    start_date = precip_dates_times$dateOn,
    end_date = precip_dates_times$dateOff
  ) %>%
  select(
    start_date,
    end_date,
    total_TN,
    total_TP
  ) %>%
  rename(
    TN_dry_kg = "total_TN",
    TP_dry_kg = "total_TP"
  )


# Add dry deposition to working nutrient budget

working_budget <- working_budget %>%
  inner_join(
    result,
    by = "end_date"
  )


# 10. Add septic nutrient inputs ----------------------------------------------

# Septic nutrient loading:
#
#   loading rate (g nutrient / capita / day)
#   * 2.5 people per dwelling
#   * 115 dwellings
#   * 7 days/week
#   * 1 kg / 1000 g
#
# Lower, mean, and upper estimates are retained separately.


# Nitrogen

working_budget$H_N_lwr <-
  1.1 *
  (2.5 * 115) *
  7 *
  (1 / 1000)

working_budget$H_N <-
  10.1 *
  (2.5 * 115) *
  7 *
  (1 / 1000)

working_budget$H_N_upr <-
  71.6 *
  (2.5 * 115) *
  7 *
  (1 / 1000)


# Phosphorus

working_budget$H_P_lwr <-
  0 *
  (2.5 * 115) *
  7 *
  (1 / 1000)

working_budget$H_P <-
  1.4 *
  (2.5 * 115) *
  7 *
  (1 / 1000)

working_budget$H_P_upr <-
  15.9 *
  (2.5 * 115) *
  7 *
  (1 / 1000)


# 11. Add measured lake nutrient concentrations -------------------------------

# Add measured TN and TP concentrations and corresponding lake nutrient mass
# to matching nutrient-budget dates.

working_budget <- working_budget %>%
  left_join(
    lake,
    by = c(
      "end_date" = "date_time"
    )
  )


# 12. Add sediment burial ------------------------------------------------------

# Weekly N and P sediment burial losses.
#
# Lower, best, and upper estimates are retained separately.

working_budget$S_N_lwr <- 49
working_budget$S_N <- 140
working_budget$S_N_upr <- 209

working_budget$S_P_lwr <- 35
working_budget$S_P <- 54
working_budget$S_P_upr <- 129


# 13. Add denitrification---------------------------------

working_budget$DN_N_lwr <- 5
working_budget$DN_N <- 53
working_budget$DN_N_upr <- 535

# 13. Add water-budget components ---------------------------------------------

water_budget <- read.csv(
  here(
    "3_products",
    "WaterBudget.csv"
  ),
  header = TRUE,
  sep = ","
)


# Convert water-budget interval end dates.

water_budget$end_date <- as.POSIXct(
  water_budget$end_date,
  format = "%Y-%m-%d",
  tz = "UTC"
)

# Retain relevant water-budget variables.

water <- water_budget %>%
  select(
    end_date,
    lake_volume,
    lake_volume_lwr,
    lake_volume_upr,
    resid,
    resid_lwr,
    resid_upr,
    lmc_m3,
    lmc_m3_lwr,
    lmc_m3_upr
  )


# Convert nutrient-budget end date to Date before joining.

working_budget$end_date <- as.Date(
  working_budget$end_date,
  format = "%Y-%m-%d",
  tz = "UTC"
)


# Merge water budget with nutrient budget.

working_budget <- working_budget %>%
  inner_join(
    water_budget,
    by = "end_date"
  )


# Remove duplicate start-date columns generated by previous joins and retain
# the original nutrient-budget interval start date.

working_budget <- working_budget %>%
  select(
    -start_date.x,
    -start_date.y,
    -start_date.x.x,
    -start_date.y.y,
    -start_date.y.y.y
  ) %>%
  rename(
    start_date = "start_date.x.x.x"
  )


# 14. Prepare nutrient-budget model -------------------------------------------

# Replace missing or infinite values with zero for all model variables except:
#
#   start_date
#   end_date
#   observed mean TN
#   observed mean TP
#   observed lake N mass
#   observed lake P mass

exclude_cols <- c(
  "start_date",
  "end_date",
  "mean_tn",
  "mean_tp",
  "kg_N",
  "kg_P"
)


working_budget <- working_budget %>%
  mutate(
    across(
      -all_of(exclude_cols),
      ~ ifelse(
        is.na(.) |
          . == -Inf |
          . == Inf,
        0,
        .
      )
    )
  )


# 15. Run best approximation nutrient-budget model ------------------------------------------

# Initialize the nutrient model on October 1 using measured lake TN and TP.
#
# Convert concentration (ug/L) to lake nutrient mass (kg):
#
#   concentration (ug/L)
#   * 1e-9 kg/ug
#   * lake volume (m3)
#   * 1000 L/m3


# Initial TN concentration and mass

working_budget$mean_tn[1] <- 274

working_budget$kg_N[1] <-
  working_budget$mean_tn[1] *
  10^(-9) *
  working_budget$lake_volume[1] *
  1000


# Initial TP concentration and mass

working_budget$mean_tp[1] <- 5.1

working_budget$kg_P[1] <-
  working_budget$mean_tp[1] *
  10^(-9) *
  working_budget$lake_volume[1] *
  1000


# Initialize modeled TN mass and concentration

working_budget$kg_N_est <- NA
working_budget$kg_N_est[1] <- working_budget$kg_N[1]

working_budget$TN_conc_est <- NA
working_budget$TN_conc_est[1] <- working_budget$mean_tn[1]


# Initialize modeled TP mass and concentration

working_budget$kg_P_est <- NA
working_budget$kg_P_est[1] <- working_budget$kg_P[1]

working_budget$TP_conc_est <- NA
working_budget$TP_conc_est[1] <- working_budget$mean_tp[1]


# Propagate nutrient mass through subsequent intervals.
#
# Nutrient mass at time i =
#
#   previous lake nutrient mass
#   + tributary inputs
#   + wet atmospheric deposition
#   + dry atmospheric deposition
#   + septic inputs
#   - sediment burial
#   - denitrification (for N only)
#   - nutrient export through Lower McDonald Creek
#
# Outflow nutrient loss is estimated from the previous modeled lake
# concentration multiplied by the water volume exported during the interval.

for (i in 2:nrow(working_budget)) {
  
  working_budget$kg_N_est[i] <-
    working_budget$kg_N_est[i - 1] +
    working_budget$umc_N_sum[i] +
    working_budget$sny_N_sum[i] +
    working_budget$spr_N_sum[i] +
    working_budget$fish_N_sum[i] +
    working_budget$TN_wet_kg[i] +
    working_budget$TN_dry_kg[i] +
    working_budget$H_N[i] -
    working_budget$S_N[i] -
    working_budget$DN_N[i] -
    
    (
      (working_budget$TN_conc_est[i - 1] * 10^(-9)) *
        (working_budget$lmc_m3[i] * 1000)
    )
  
  
  # Convert modeled lake TN mass back to concentration (ug/L)
  
  working_budget$TN_conc_est[i] <-
    (
      working_budget$kg_N_est[i] *
        10^(9)
    ) /
    (
      working_budget$lake_volume[i] *
        1000
    )
  
  
  working_budget$kg_P_est[i] <-
    working_budget$kg_P_est[i - 1] +
    working_budget$umc_P_sum[i] +
    working_budget$sny_P_sum[i] +
    working_budget$spr_P_sum[i] +
    working_budget$fish_P_sum[i] +
    working_budget$TP_wet_kg[i] +
    working_budget$TP_dry_kg[i] +
    working_budget$H_P[i] -
    working_budget$S_P[i] -
    (
      (working_budget$TP_conc_est[i - 1] * 10^(-9)) *
        (working_budget$lmc_m3[i] * 1000)
    )
  
  
  # Convert modeled lake TP mass back to concentration (ug/L)
  
  working_budget$TP_conc_est[i] <-
    (
      working_budget$kg_P_est[i] *
        10^(9)
    ) /
    (
      working_budget$lake_volume[i] *
        1000
    )
}


# 16. Run lower nutrient-budget model -----------------------------------------

# Initialize lower TN estimate.

working_budget$mean_tn_lwr[1] <- 226

working_budget$kg_N_lwr[1] <-
  working_budget$mean_tn_lwr[1] *
  10^(-9) *
  working_budget$lake_volume_lwr[1] *
  1000


# Initialize lower TP estimate.

working_budget$mean_tp_lwr[1] <- 2.5

working_budget$kg_P_lwr[1] <-
  working_budget$mean_tp_lwr[1] *
  10^(-9) *
  working_budget$lake_volume_lwr[1] *
  1000


# Initialize lower modeled TN mass and concentration.

working_budget$kg_N_est_lwr <- NA
working_budget$kg_N_est_lwr[1] <- working_budget$kg_N_lwr[1]

working_budget$TN_conc_est_lwr <- NA
working_budget$TN_conc_est_lwr[1] <- working_budget$mean_tn_lwr[1]


# Initialize lower modeled TP mass and concentration.

working_budget$kg_P_est_lwr <- NA
working_budget$kg_P_est_lwr[1] <- working_budget$kg_P_lwr[1]

working_budget$TP_conc_est_lwr <- NA
working_budget$TP_conc_est_lwr[1] <- working_budget$mean_tp_lwr[1]


# Propagate lower nutrient-budget estimate.

for (i in 2:nrow(working_budget)) {
  
  working_budget$kg_N_est_lwr[i] <-
    working_budget$kg_N_est_lwr[i - 1] +
    working_budget$umc_N_lwr_sum[i] +
    working_budget$sny_N_lwr_sum[i] +
    working_budget$spr_N_lwr_sum[i] +
    working_budget$fish_N_lwr_sum[i] +
    working_budget$TN_wet_kg[i] +
    working_budget$TN_dry_kg[i] +
    working_budget$H_N_lwr[i] -
    working_budget$S_N_lwr[i] -
    working_budget$DN_N_lwr[i] -
    (
      (working_budget$TN_conc_est_lwr[i - 1] * 10^(-9)) *
        (working_budget$lmc_m3_lwr[i] * 1000)
    )
  
  
  working_budget$TN_conc_est_lwr[i] <-
    (
      working_budget$kg_N_est_lwr[i] *
        10^(9)
    ) /
    (
      working_budget$lake_volume_upr[i] *
        1000
    )
  
  
  working_budget$kg_P_est_lwr[i] <-
    working_budget$kg_P_est_lwr[i - 1] +
    working_budget$umc_P_lwr_sum[i] +
    working_budget$sny_P_lwr_sum[i] +
    working_budget$spr_P_lwr_sum[i] +
    working_budget$fish_P_lwr_sum[i] +
    working_budget$TP_wet_kg[i] +
    working_budget$TP_dry_kg[i] +
    working_budget$H_P_lwr[i] -
    working_budget$S_P_lwr[i] -
    (
      (working_budget$TP_conc_est_lwr[i - 1] * 10^(-9)) *
        (working_budget$lmc_m3_lwr[i] * 1000)
    )
  
  
  working_budget$TP_conc_est_lwr[i] <-
    (
      working_budget$kg_P_est_lwr[i] *
        10^(9)
    ) /
    (
      working_budget$lake_volume_upr[i] *
        1000
    )
}


# 17. Run upper nutrient-budget model -----------------------------------------

# Initialize upper TN estimate.

working_budget$mean_tn_upr[1] <- 314

working_budget$kg_N_upr[1] <-
  working_budget$mean_tn_upr[1] *
  10^(-9) *
  working_budget$lake_volume_upr[1] *
  1000


# Initialize upper TP estimate.

working_budget$mean_tp_upr[1] <- 10

working_budget$kg_P_upr[1] <-
  working_budget$mean_tp_upr[1] *
  10^(-9) *
  working_budget$lake_volume_upr[1] *
  1000


# Initialize upper modeled TN mass and concentration.

working_budget$kg_N_est_upr <- NA
working_budget$kg_N_est_upr[1] <- working_budget$kg_N_upr[1]

working_budget$TN_conc_est_upr <- NA
working_budget$TN_conc_est_upr[1] <- working_budget$mean_tn_upr[1]


# Initialize upper modeled TP mass and concentration.

working_budget$kg_P_est_upr <- NA
working_budget$kg_P_est_upr[1] <- working_budget$kg_P_upr[1]

working_budget$TP_conc_est_upr <- NA
working_budget$TP_conc_est_upr[1] <- working_budget$mean_tp_upr[1]


# Propagate upper nutrient-budget estimate.

for (i in 2:nrow(working_budget)) {
  
  working_budget$kg_N_est_upr[i] <-
    working_budget$kg_N_est_upr[i - 1] +
    working_budget$umc_N_upr_sum[i] +
    working_budget$sny_N_upr_sum[i] +
    working_budget$spr_N_upr_sum[i] +
    working_budget$fish_N_upr_sum[i] +
    working_budget$TN_wet_kg[i] +
    working_budget$TN_dry_kg[i] +
    working_budget$H_N_upr[i] -
    working_budget$S_N_upr[i] -
    working_budget$DN_N_upr[i] -
    (
      (working_budget$TN_conc_est_upr[i - 1] * 10^(-9)) *
        (working_budget$lmc_m3_upr[i] * 1000)
    )
  
  
  working_budget$TN_conc_est_upr[i] <-
    (
      working_budget$kg_N_est_upr[i] *
        10^(9)
    ) /
    (
      working_budget$lake_volume_lwr[i] *
        1000
    )
  
  
  working_budget$kg_P_est_upr[i] <-
    working_budget$kg_P_est_upr[i - 1] +
    working_budget$umc_P_upr_sum[i] +
    working_budget$sny_P_upr_sum[i] +
    working_budget$spr_P_upr_sum[i] +
    working_budget$fish_P_upr_sum[i] +
    working_budget$TP_wet_kg[i] +
    working_budget$TP_dry_kg[i] +
    working_budget$H_P_upr[i] -
    working_budget$S_P_upr[i] -
    (
      (working_budget$TP_conc_est_upr[i - 1] * 10^(-9)) *
        (working_budget$lmc_m3_upr[i] * 1000)
    )
  
  
  working_budget$TP_conc_est_upr[i] <-
    (
      working_budget$kg_P_est_upr[i] *
        10^(9)
    ) /
    (
      working_budget$lake_volume_lwr[i] *
        1000
    )
}


# 18. Rename tributary nutrient-load columns ----------------------------------

# Remove "_sum" suffix after interval aggregation.

working_budget <- working_budget %>%
  rename(
    umc_P_lwr = umc_P_lwr_sum,
    sny_P_lwr = sny_P_lwr_sum,
    spr_P_lwr = spr_P_lwr_sum,
    fish_P_lwr = fish_P_lwr_sum,
    
    umc_P = umc_P_sum,
    sny_P = sny_P_sum,
    spr_P = spr_P_sum,
    fish_P = fish_P_sum,
    
    umc_P_upr = umc_P_upr_sum,
    sny_P_upr = sny_P_upr_sum,
    spr_P_upr = spr_P_upr_sum,
    fish_P_upr = fish_P_upr_sum,
    
    umc_N_lwr = umc_N_lwr_sum,
    sny_N_lwr = sny_N_lwr_sum,
    spr_N_lwr = spr_N_lwr_sum,
    fish_N_lwr = fish_N_lwr_sum,
    
    umc_N = umc_N_sum,
    sny_N = sny_N_sum,
    spr_N = spr_N_sum,
    fish_N = fish_N_sum,
    
    umc_N_upr = umc_N_upr_sum,
    sny_N_upr = sny_N_upr_sum,
    spr_N_upr = spr_N_upr_sum,
    fish_N_upr = fish_N_upr_sum
  )


# 19. Save nutrient budget -----------------------------------------------------

write.csv(
  working_budget,
  here(
    "3_products",
    "NutrientBudget.csv"
  ),
  row.names = FALSE
)

# 20. Plots-----------------------------------------------------------------

ggplot(
  working_budget,
  aes(x = end_date)
) +
  geom_ribbon(
    aes(
      ymin = TP_conc_est_lwr,
      ymax = TP_conc_est_upr
    ),
    fill = "peachpuff1",
    alpha = 0.7
  ) +
  geom_line(
    aes(y = TP_conc_est),
    color = "salmon1",
    linewidth = 1
  ) +
  geom_errorbar(
    aes(
      ymin = min_tp,
      ymax = max_tp
    ),
    width = 1,
    color = "black"
  ) +
  geom_point(
    aes(y = mean_tp),
    color = "black",
    size = 3
  ) +
  labs(
    title = "A",
    x = "",
    y = expression(
      "TP Concentration (" * mu * "g-P L"^{-1} * ")"
    )
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
  scale_x_datetime(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank()
  )


ggplot(
  working_budget,
  aes(x = end_date)
) +
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
  labs(
    title = "B",
    x = "Date",
    y = expression(
      "TN Concentration (" * mu * "g-N L"^{-1} * ")"
    )
  ) +
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
  scale_x_datetime(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
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


# RMSE calculations --------------------------------------------------------

# This function creates a table of observed and predicted nutrient
# concentrations and calculates the mean observed concentration and RMSE.
#
# Arguments:
#   data         = nutrient-budget dataframe
#   nutrient     = "TP" or "TN"
#   exclude_2018 = whether observations from 2018 should be excluded

create_rmse_table <- function(
    data,
    nutrient = c("TP", "TN"),
    exclude_2018 = FALSE
) {
  
  nutrient <- match.arg(nutrient)
  
  # Define nutrient-specific column names.
  if (nutrient == "TP") {
    
    observed_col <- "mean_tp"
    predicted_col <- "TP_conc_est"
    lower_col <- "TP_conc_est_lwr"
    upper_col <- "TP_conc_est_upr"
    
  } else {
    
    observed_col <- "mean_tn"
    predicted_col <- "TN_conc_est"
    lower_col <- "TN_conc_est_lwr"
    upper_col <- "TN_conc_est_upr"
  }
  
  
  # 1. Select observations used to evaluate model performance ---------------
  
  rmse_data <- data %>%
    filter(
      !is.na(.data[[observed_col]]),
      end_date > as.Date("2008-10-01")
    )
  
  
  # Optionally remove observations collected during 2018.
  if (exclude_2018) {
    
    rmse_data <- rmse_data %>%
      filter(
        lubridate::year(end_date) != 2018
      )
  }
  
  
  # 2. Retain observed and predicted concentrations -------------------------
  
  rmse_data <- rmse_data %>%
    transmute(
      Date = end_date,
      Observed = .data[[observed_col]],
      Predicted = .data[[predicted_col]],
      `Predicted Minimum` = .data[[lower_col]],
      `Predicted Maximum` = .data[[upper_col]]
    ) %>%
    mutate(
      Observed = round(Observed, 2),
      Predicted = round(Predicted, 2),
      `Predicted Minimum` = round(`Predicted Minimum`, 2),
      `Predicted Maximum` = round(`Predicted Maximum`, 2)
    )
  
  
  # 3. Calculate model summary statistics -----------------------------------
  
  observed_mean <- round(
    mean(
      rmse_data$Observed,
      na.rm = TRUE
    ),
    2
  )
  
  rmse <- round(
    sqrt(
      mean(
        (rmse_data$Predicted - rmse_data$Observed)^2,
        na.rm = TRUE
      )
    ),
    2
  )
  
  
  # 4. Convert date to character so summary rows can be appended ------------
  
  rmse_data <- rmse_data %>%
    mutate(
      Date = as.character(Date)
    )
  
  
  # 5. Add mean and RMSE rows ------------------------------------------------
  
  mean_row <- data.frame(
    Date = "Mean",
    Observed = observed_mean,
    Predicted = NA_real_,
    `Predicted Minimum` = NA_real_,
    `Predicted Maximum` = NA_real_,
    check.names = FALSE
  )
  
  rmse_row <- data.frame(
    Date = "RMSE",
    Observed = NA_real_,
    Predicted = rmse,
    `Predicted Minimum` = NA_real_,
    `Predicted Maximum` = NA_real_,
    check.names = FALSE
  )
  
  
  # 6. Return final table ----------------------------------------------------
  
  bind_rows(
    rmse_data,
    mean_row,
    rmse_row
  )
}


# TP model performance -----------------------------------------------------

# Include all observations, including 2018.
TP_RMSE_table <- create_rmse_table(
  data = working_budget,
  nutrient = "TP",
  exclude_2018 = FALSE
)

# Exclude observations from 2018.
TP_RMSE_table_no2018 <- create_rmse_table(
  data = working_budget,
  nutrient = "TP",
  exclude_2018 = TRUE
)


# TN model performance -----------------------------------------------------

# Include all observations, including 2018.
TN_RMSE_table <- create_rmse_table(
  data = working_budget,
  nutrient = "TN",
  exclude_2018 = FALSE
)

# Exclude observations from 2018.
TN_RMSE_table_no2018 <- create_rmse_table(
  data = working_budget,
  nutrient = "TN",
  exclude_2018 = TRUE
)


# Create LaTeX tables ------------------------------------------------------

xtable_table_tp <- xtable::xtable(
  TP_RMSE_table
)

xtable_table_tp_no2018 <- xtable::xtable(
  TP_RMSE_table_no2018
)

xtable_table_tn <- xtable::xtable(
  TN_RMSE_table
)

xtable_table_tn_no2018 <- xtable::xtable(
  TN_RMSE_table_no2018
)


# Save LaTeX tables --------------------------------------------------------

# TP, including 2018.
print(
  xtable_table_tp,
  type = "latex",
  file = here::here(
    "3_products",
    "TP_RMSE_table.tex"
  ),
  include.rownames = FALSE
)

# TP, excluding 2018.
print(
  xtable_table_tp_no2018,
  type = "latex",
  file = here::here(
    "3_products",
    "TP_RMSE_table_no2018.tex"
  ),
  include.rownames = FALSE
)

# TN, including 2018.
print(
  xtable_table_tn,
  type = "latex",
  file = here::here(
    "3_products",
    "TN_RMSE_table.tex"
  ),
  include.rownames = FALSE
)

# TN, excluding 2018.
print(
  xtable_table_tn_no2018,
  type = "latex",
  file = here::here(
    "3_products",
    "TN_RMSE_table_no2018.tex"
  ),
  include.rownames = FALSE
)

# Annual summaries----------------------------------

nuts <- read.csv(
  here::here(
    "3_products",
    "NutrientBudget.csv"
  )
)


# Assign water year based on end_date
water_year <- nuts %>%
  mutate(
    end_date = as.Date(end_date),  # Ensure end_date is a Date
    water_year = year(end_date) + if_else(month(end_date) >= 10, 1, 0)
  ) %>%
  filter(water_year <= 2023)%>%
  mutate(lmc_P = (lag(TP_conc_est)*10^(-9))*(lmc_m3*1000), 
         lmc_N = (lag(TN_conc_est)*10^(-9))*(lmc_m3*1000), 
         lmc_P_lwr = (lag(TP_conc_est_lwr)*10^(-9))*(lmc_m3_lwr*1000), 
         lmc_N_lwr = (lag(TN_conc_est_lwr)*10^(-9))*(lmc_m3_lwr*1000),          
         lmc_P_upr = (lag(TP_conc_est_upr)*10^(-9))*(lmc_m3_upr*1000), 
         lmc_N_upr = (lag(TN_conc_est_upr)*10^(-9))*(lmc_m3_upr*1000))


# Summarize by water year ------------------------------------------------------

water_year_summary <- water_year %>%
  group_by(water_year) %>%
  summarize(
    across(
      c(
        umc_m3, sny_m3, spr_m3, fish_m3,
        umc_m3_lwr, sny_m3_lwr, spr_m3_lwr, fish_m3_lwr,
        umc_m3_upr, sny_m3_upr, spr_m3_upr, fish_m3_upr,
        Lw_m3,
        Le_m3, Le_m3_lwr, Le_m3_upr,
        lmc_m3, lmc_m3_lwr, lmc_m3_upr
      ),
      ~ sum(.x, na.rm = TRUE)
    )
  ) %>%
  mutate(
    # Tributaries
    T_mean = rowSums(
      across(c(umc_m3, sny_m3, spr_m3, fish_m3)),
      na.rm = TRUE
    ),
    T_lwr = rowSums(
      across(c(umc_m3_lwr, sny_m3_lwr, spr_m3_lwr, fish_m3_lwr)),
      na.rm = TRUE
    ),
    T_upr = rowSums(
      across(c(umc_m3_upr, sny_m3_upr, spr_m3_upr, fish_m3_upr)),
      na.rm = TRUE
    ),
    
    # Outflow as a loss
    O_mean = -lmc_m3,
    O_lwr = -lmc_m3_lwr,
    O_upr = -lmc_m3_upr,
    
    # Evaporation as a loss
    E_mean = -Le_m3,
    E_lwr = -Le_m3_lwr,
    E_upr = -Le_m3_upr,
    
    # Wet deposition as an input
    W = Lw_m3
  ) %>%
  select(
    water_year,
    T_mean, T_lwr, T_upr,
    O_mean, O_lwr, O_upr,
    E_mean, E_lwr, E_upr,
    W
  )


# 1975 values -----------------------------------------------------------------

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

days_in_month <- c(
  Jan = 31, Feb = 28, Mar = 31, Apr = 30,
  May = 31, Jun = 30, Jul = 31, Aug = 31,
  Sep = 30, Oct = 31, Nov = 30, Dec = 31
)

monthly_seconds <- days_in_month * 60 * 60 * 24


monthly_flows <- normalized_flows %>%
  select(
    Tributary,
    Jan, Feb, Mar, Apr, May, Jun,
    Jul, Aug, Sep, Oct, Nov, Dec
  ) %>%
  pivot_longer(
    -Tributary,
    names_to = "Month",
    values_to = "CMS"
  ) %>%
  mutate(
    Seconds = monthly_seconds[Month],
    Volume_m3 = CMS * Seconds
  )


# 1975 tributary volume
T_total_1975 <- monthly_flows %>%
  filter(Tributary %in% c("umc", "fish", "sny", "spr")) %>%
  summarise(Total = sum(Volume_m3)) %>%
  pull(Total)


# 1975 outlet volume
lmc_total_1975 <- monthly_flows %>%
  filter(Tributary == "lmc") %>%
  summarise(Total = sum(Volume_m3)) %>%
  pull(Total)


# Add 1975 row ---------------------------------------------------------------

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

summary_water_year_water <- rbind(
  water_year_summary,
  new_row_water
)


# Check values ---------------------------------------------------------------

summary_water_year_water %>%
  select(water_year, T_mean, O_mean, E_mean, W)


# Colors ----------------------------------------------------------------------

fill_colors_water <- c(
  "Wet Deposition" = "black",
  "Evaporation" = "peachpuff3",
  "Outflow" = "midnightblue",
  "Tributaries" = "lightskyblue2"
)

border_colors_water <- c(
  "Wet Deposition" = "black",
  "Evaporation" = "peachpuff3",
  "Outflow" = "midnightblue",
  "Tributaries" = "lightskyblue2"
)


# Convert to long format ------------------------------------------------------

errorbar_data_water <- summary_water_year_water %>%
  transmute(
    water_year = as.factor(water_year),
    Category = "Tributaries",
    Mean = T_mean,
    Min = T_lwr,
    Max = T_upr
  ) %>%
  bind_rows(
    
    summary_water_year_water %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Outflow",
        Mean = O_mean,
        Min = O_lwr,
        Max = O_upr
      ),
    
    summary_water_year_water %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Evaporation",
        Mean = E_mean,
        Min = E_lwr,
        Max = E_upr
      ),
    
    summary_water_year_water %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Wet Deposition",
        Mean = W,
        Min = NA_real_,
        Max = NA_real_
      )
  )


# Check categories ------------------------------------------------------------

unique(errorbar_data_water$Category)


# Plot ------------------------------------------------------------------------

ggplot(
  errorbar_data_water,
  aes(
    x = water_year,
    y = Mean,
    fill = Category,
    color = Category
  )
) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    width = 0.75
  ) +
  geom_errorbar(
    aes(
      ymin = Min,
      ymax = Max
    ),
    position = position_dodge(width = 0.75),
    width = 0.25,
    na.rm = TRUE,
    color = "grey20"
  ) +
  scale_fill_manual(
    values = fill_colors_water
  ) +
  scale_color_manual(
    values = border_colors_water
  ) +
  guides(
    fill = guide_legend(
      title = NULL,
      nrow = 1
    ),
    color = guide_legend(
      title = NULL,
      nrow = 1
    )
  ) +
  labs(
    title = "C",
    x = "Water Year",
    y = bquote("Flux (" * m^3 ~ yr^-1 * ")")
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )  +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    labels = scales::comma,
    breaks = c(
      -1e9,
      -1e8,
      -1e7,
      -1e6,
      0,
      1e6,
      1e7,
      1e8,
      1e9
    )
  )



# Summarize by water year for P -----------------------------------------------

summary_water_year_P <- water_year %>%
  group_by(water_year) %>%
  summarize(
    across(
      c(
        umc_P_lwr, sny_P_lwr, spr_P_lwr, fish_P_lwr,
        umc_P, sny_P, spr_P, fish_P,
        umc_P_upr, sny_P_upr, spr_P_upr, fish_P_upr,
        TP_dry_kg, TP_wet_kg,
        H_P_lwr, H_P, H_P_upr,
        S_P_lwr, S_P, S_P_upr,
        lmc_P_lwr, lmc_P, lmc_P_upr
      ),
      ~ sum(.x, na.rm = TRUE)
    )
  ) %>%
  mutate(
    # Tributaries
    T_mean = rowSums(
      across(c(umc_P, sny_P, spr_P, fish_P)),
      na.rm = TRUE
    ),
    T_lwr = rowSums(
      across(c(umc_P_lwr, sny_P_lwr, spr_P_lwr, fish_P_lwr)),
      na.rm = TRUE
    ),
    T_upr = rowSums(
      across(c(umc_P_upr, sny_P_upr, spr_P_upr, fish_P_upr)),
      na.rm = TRUE
    ),
    
    # Septic
    H_lwr = H_P_lwr,
    H = H_P,
    H_upr = H_P_upr,
    
    # Total atmospheric deposition
    D = TP_dry_kg + TP_wet_kg,
    
    # Burial as a loss
    S_lwr = -S_P_lwr,
    S = -S_P,
    S_upr = -S_P_upr,
    
    # Outflow as a loss
    O_mean = -lmc_P,
    O_lwr = -lmc_P_lwr,
    O_upr = -lmc_P_upr
  ) %>%
  select(
    water_year,
    T_mean, T_lwr, T_upr,
    O_mean, O_lwr, O_upr,
    H_lwr, H, H_upr,
    S_lwr, S, S_upr,
    D
  )


# Add 1975 values -------------------------------------------------------------

new_row_P <- data.frame(
  water_year = 1975,
  T_mean = 8265,
  T_lwr = NA,
  T_upr = NA,
  O_mean = -5035,
  O_lwr = NA,
  O_upr = NA,
  H_lwr = NA,
  H = 70,
  H_upr = NA,
  S_lwr = NA,
  S = NA,
  S_upr = NA,
  D = 570
)

summary_water_year_P <- rbind(
  summary_water_year_P,
  new_row_P
)


# Check deposition values -----------------------------------------------------

summary_water_year_P %>%
  select(water_year, D)


# Colors ----------------------------------------------------------------------

fill_colors_P <- c(
  "Deposition" = "black",
  "Septic" = "peachpuff3",
  "Burial" = "grey",
  "Outflow" = "midnightblue",
  "Tributaries" = "lightskyblue2"
)

border_colors_P <- c(
  "Deposition" = "black",
  "Septic" = "peachpuff3",
  "Burial" = "grey",
  "Outflow" = "midnightblue",
  "Tributaries" = "lightskyblue2"
)


# Convert to long format for plotting ----------------------------------------

errorbar_data_P <- summary_water_year_P %>%
  transmute(
    water_year = as.factor(water_year),
    Category = "Tributaries",
    Mean = T_mean,
    Min = T_lwr,
    Max = T_upr
  ) %>%
  bind_rows(
    
    summary_water_year_P %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Outflow",
        Mean = O_mean,
        Min = O_lwr,
        Max = O_upr
      ),
    
    summary_water_year_P %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Septic",
        Mean = H,
        Min = H_lwr,
        Max = H_upr
      ),
    
    summary_water_year_P %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Burial",
        Mean = S,
        Min = S_lwr,
        Max = S_upr
      ),
    
    summary_water_year_P %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Deposition",
        Mean = D,
        Min = NA_real_,
        Max = NA_real_
      )
  )


# Check categories ------------------------------------------------------------

unique(errorbar_data_P$Category)


# Plot ------------------------------------------------------------------------

ggplot(
  errorbar_data_P,
  aes(
    x = water_year,
    y = Mean,
    fill = Category,
    color = Category
  )
) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    width = 0.75
  ) +
  geom_errorbar(
    aes(
      ymin = Min,
      ymax = Max
    ),
    position = position_dodge(width = 0.75),
    width = 0.25,
    na.rm = TRUE,
    color = "grey20"
  ) +
  scale_fill_manual(
    values = fill_colors_P
  ) +
  scale_color_manual(
    values = border_colors_P
  ) +
  guides(
    fill = guide_legend(
      title = NULL,
      nrow = 1
    ),
    color = guide_legend(
      title = NULL,
      nrow = 1
    )
  ) +
  labs(
    title = "A",
    x = "",
    y = bquote("Flux (" * kg-P ~ yr^-1 * ")")
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_blank(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 12),
    legend.position = "none"
  )  +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    labels = scales::comma,
    breaks = c(
      -10000,
      -1000,
      -100,
      0,
      100,
      1000,
      10000
    )
  )


# Summarize by water year for N -----------------------------------------------

summary_water_year_N <- water_year %>%
  group_by(water_year) %>%
  summarize(
    across(
      c(
        umc_N_lwr, sny_N_lwr, spr_N_lwr, fish_N_lwr,
        umc_N, sny_N, spr_N, fish_N,
        umc_N_upr, sny_N_upr, spr_N_upr, fish_N_upr,
        TN_wet_kg, TN_dry_kg,
        H_N_lwr, H_N, H_N_upr,
        S_N_lwr, S_N, S_N_upr,
        DN_N_lwr, DN_N, DN_N_upr,
        lmc_N_lwr, lmc_N, lmc_N_upr
      ),
      ~ sum(.x, na.rm = TRUE)
    )
  ) %>%
  mutate(
    # Tributaries
    T_mean = rowSums(
      across(c(umc_N, sny_N, spr_N, fish_N)),
      na.rm = TRUE
    ),
    T_lwr = rowSums(
      across(c(umc_N_lwr, sny_N_lwr, spr_N_lwr, fish_N_lwr)),
      na.rm = TRUE
    ),
    T_upr = rowSums(
      across(c(umc_N_upr, sny_N_upr, spr_N_upr, fish_N_upr)),
      na.rm = TRUE
    ),
    
    # Septic
    H_lwr = H_N_lwr,
    H = H_N,
    H_upr = H_N_upr,
    
    # Total atmospheric deposition
    D = TN_dry_kg + TN_wet_kg,
    
    # Burial as a loss
    S_lwr = -S_N_lwr,
    S = -S_N,
    S_upr = -S_N_upr,
    
    # Denitrification as a loss
    DN_lwr = -DN_N_lwr,
    DN = -DN_N,
    DN_upr = -DN_N_upr,
    
    # Outflow as a loss
    O_mean = -lmc_N,
    O_lwr = -lmc_N_lwr,
    O_upr = -lmc_N_upr
  ) %>%
  select(
    water_year,
    T_mean, T_lwr, T_upr,
    O_mean, O_lwr, O_upr,
    H_lwr, H, H_upr,
    S_lwr, S, S_upr,
    DN_lwr, DN, DN_upr,
    D
  )


# Add 1975 values -------------------------------------------------------------

new_row_N <- data.frame(
  water_year = 1975,
  T_mean = 381285,
  T_lwr = NA,
  T_upr = NA,
  O_mean = -364395,
  O_lwr = NA,
  O_upr = NA,
  H_lwr = NA,
  H = 2635,
  H_upr = NA,
  S_lwr = NA,
  S = NA,
  S_upr = NA,
  DN_lwr = NA,
  DN = NA,
  DN_upr = NA,
  D = 35225
)

summary_water_year_N <- rbind(
  summary_water_year_N,
  new_row_N
)


# Check values ----------------------------------------------------------------

summary_water_year_N %>%
  select(water_year, D, DN_lwr, DN, DN_upr)


# Colors ----------------------------------------------------------------------

fill_colors_N <- c(
  "Deposition" = "black",
  "Septic" = "peachpuff3",
  "Burial" = "grey",
  "Denitrification" = "darkred",
  "Outflow" = "midnightblue",
  "Tributaries" = "lightskyblue2"
)

border_colors_N <- fill_colors_N


# Convert to long format for plotting ----------------------------------------

errorbar_data_N <- summary_water_year_N %>%
  transmute(
    water_year = as.factor(water_year),
    Category = "Tributaries",
    Mean = T_mean,
    Min = T_lwr,
    Max = T_upr
  ) %>%
  bind_rows(
    
    summary_water_year_N %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Outflow",
        Mean = O_mean,
        Min = O_lwr,
        Max = O_upr
      ),
    
    summary_water_year_N %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Septic",
        Mean = H,
        Min = H_lwr,
        Max = H_upr
      ),
    
    summary_water_year_N %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Burial",
        Mean = S,
        Min = S_lwr,
        Max = S_upr
      ),
    
    summary_water_year_N %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Denitrification",
        Mean = DN,
        Min = DN_lwr,
        Max = DN_upr
      ),
    
    summary_water_year_N %>%
      transmute(
        water_year = as.factor(water_year),
        Category = "Deposition",
        Mean = D,
        Min = NA_real_,
        Max = NA_real_
      )
  )


# Plot ------------------------------------------------------------------------

ggplot(
  errorbar_data_N,
  aes(
    x = water_year,
    y = Mean,
    fill = Category,
    color = Category
  )
) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    width = 0.75
  ) +
  geom_errorbar(
    aes(
      ymin = Min,
      ymax = Max
    ),
    position = position_dodge(width = 0.75),
    width = 0.25,
    na.rm = TRUE,
    color = "grey20"
  ) +
  scale_fill_manual(
    values = fill_colors_N
  ) +
  scale_color_manual(
    values = border_colors_N
  ) +
  guides(
    fill = guide_legend(
      title = NULL,
      nrow = 1
    ),
    color = guide_legend(
      title = NULL,
      nrow = 1
    )
  ) +
  labs(
    title = "B",
    x = "",
    y = bquote("Flux (" * kg-N ~ yr^-1 * ")")
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_blank(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    labels = scales::comma,
    breaks = c(
      -500000,
      -100000,
      -10000,
      -1000,
      -100,
      0,
      1000,
      10000,
      100000,
      500000
    )
  )




