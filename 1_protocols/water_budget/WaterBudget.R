# ==============================================================================
# Construct Lake McDonald water budget
#
# Purpose:
#   Combine tributary inflow, lake outflow, wet deposition, evaporation,
#   and reconstructed lake volume into a common water-budget dataset.
#
# Inputs:
#   2_incremental/QtQo.csv
#   2_incremental/Vl.csv
#   2_incremental/Le.csv
#   2_incremental/Lw.csv
#
# Output:
#   2_incremental/WaterBudget.csv
#
# Notes:
#   - Water-budget intervals are defined by NADP precipitation collection dates.
#   - Tributary and outlet discharge are converted from m3/s to m3/hour.
#   - Separate functions are retained for best approximation and lower and upper estimates because each calculation uses a different set of input variables.
# ==============================================================================


# 0. Load packages -------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(scales)
library(gridExtra)
library(patchwork)
library(here)
library(ggplot2)
library(ggbreak)

# Remove scientific notation
options(scipen = 999)


# 1. Read water-budget inputs --------------------------------------------------

# Tributary inflow and lake outflow

QtQo <- readr::read_csv(
  here(
    "2_incremental",
    "QtQo.csv"
  ),
  col_types = cols(
    dateTime = col_datetime(format = "")
  )
)

# Set timestamps to UTC

QtQo$dateTime <- lubridate::force_tz(
  QtQo$dateTime,
  "UTC"
)


# Reconstructed lake volume

lake <- readr::read_csv(
  here(
    "2_incremental",
    "Vl.csv"
  ),
  col_types = cols(
    date_time = col_datetime(format = "")
  )
)

lake$date_time <- lubridate::force_tz(
  lake$date_time,
  "UTC"
)


# Daily evaporation estimates

Le <- read.csv(
  here(
    "2_incremental",
    "Le.csv"
  ),
  header = TRUE,
  sep = ","
)

Le$date <- as.POSIXct(
  Le$date,
  format = "%Y-%m-%d",
  tz = "UTC"
)

# Wet deposition

Lw <- readr::read_csv(
  here(
    "2_incremental",
    "Lw.csv"
  ),
  col_types = cols(
    dateOn = col_datetime(format = ""),
    dateOff = col_datetime(format = "")
  )
)

Lw <- Lw %>%
  select(
    dateOn,
    dateOff,
    L3w
  )


# 2. Convert discharge to hourly water volume ---------------------------------

# Tributary and outlet discharge are reported as instantaneous discharge in m3/s.
# Convert discharge to hourly water volume: water volume (m3/hour) = discharge (m3/s) * 3600 s/hour

QtQo <- QtQo %>%
  rename(
    date_time = dateTime
  ) %>%
  mutate(
    across(
      c(
        umc_discharge_m3_s,
        umc_discharge_m3_s_lwr,
        umc_discharge_m3_s_upr,
        sny_discharge_m3_s,
        sny_discharge_m3_s_lwr,
        sny_discharge_m3_s_upr,
        spr_discharge_m3_s,
        spr_discharge_m3_s_lwr,
        spr_discharge_m3_s_upr,
        fish_discharge_m3_s,
        fish_discharge_m3_s_lwr,
        fish_discharge_m3_s_upr,
        lmc_discharge_m3_s,
        lmc_discharge_m3_s_lwr,
        lmc_discharge_m3_s_upr
      )
    )
  ) %>%
  mutate(
    umc_m3 = umc_discharge_m3_s * 3600,
    umc_m3_lwr = umc_discharge_m3_s_lwr * 3600,
    umc_m3_upr = umc_discharge_m3_s_upr * 3600,
    
    sny_m3 = sny_discharge_m3_s * 3600,
    sny_m3_lwr = sny_discharge_m3_s_lwr * 3600,
    sny_m3_upr = sny_discharge_m3_s_upr * 3600,
    
    spr_m3 = spr_discharge_m3_s * 3600,
    spr_m3_lwr = spr_discharge_m3_s_lwr * 3600,
    spr_m3_upr = spr_discharge_m3_s_upr * 3600,
    
    fish_m3 = fish_discharge_m3_s * 3600,
    fish_m3_lwr = fish_discharge_m3_s_lwr * 3600,
    fish_m3_upr = fish_discharge_m3_s_upr * 3600,
    
    lmc_m3 = lmc_discharge_m3_s * 3600,
    lmc_m3_lwr = lmc_discharge_m3_s_lwr * 3600,
    lmc_m3_upr = lmc_discharge_m3_s_upr * 3600
  )


# Keep relevant lake-volume columns

lake <- lake %>%
  select(
    date_time,
    lake_volume,
    lake_volume_lwr,
    lake_volume_upr
  )


# Assign daily evaporation estimates to 23:00 UTC

Le <- Le %>%
  mutate(
    date = as.POSIXct(
      paste(date, "23:00:00")
    )
  ) %>%
  rename(
    date_time = date
  )


# 3. Define water-budget intervals --------------------------------------------

# Water-budget intervals are based on NADP precipitation collection dates.
# Each interval ends on a unique NADP dateOff timestamp.

precip_dates_times <- data.frame(
  unique(Lw$dateOff)
)

precip_dates_times <- precip_dates_times %>%
  rename(
    date_time = "unique.Lw.dateOff."
  )

precip_dates_times$date_time <- as.POSIXct(
  precip_dates_times$date_time,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)


# 4. Sum tributary and outlet volume: mean estimates --------------------------

# Sum hourly tributary inflow and outlet volume within each NADP sampling
# interval.
#
# This function also retains:
#   1. data before the first NADP collection date
#   2. data after the final NADP collection date

sum_tribs_out <- function(
    QtQo,
    precip_dates_times
) {
  
  # Initialize results dataframe
  
  results <- data.frame(
    start_date = character(),
    end_date = character(),
    umc_m3_sum = numeric(),
    sny_m3_sum = numeric(),
    spr_m3_sum = numeric(),
    fish_m3_sum = numeric(),
    lmc_m3_sum = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  # Include observations before the first NADP collection date
  
  first_date <- precip_dates_times$date_time[1]
  
  pre_first_date_data <- QtQo %>%
    filter(
      date_time < first_date
    )
  
  if (nrow(pre_first_date_data) > 0) {
    
    pre_first_date_sums <- pre_first_date_data %>%
      summarize(
        umc_m3_sum = sum(umc_m3, na.rm = TRUE),
        sny_m3_sum = sum(sny_m3, na.rm = TRUE),
        spr_m3_sum = sum(spr_m3, na.rm = TRUE),
        fish_m3_sum = sum(fish_m3, na.rm = TRUE),
        lmc_m3_sum = sum(lmc_m3, na.rm = TRUE)
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
  
  
  # Sum observations within each NADP sampling interval
  
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    
    start_date <- precip_dates_times$date_time[i]
    end_date <- precip_dates_times$date_time[i + 1]
    
    range_data <- QtQo %>%
      filter(
        date_time >= start_date &
          date_time < end_date
      )
    
    if (nrow(range_data) > 0) {
      
      range_sums <- range_data %>%
        summarize(
          umc_m3_sum = sum(umc_m3, na.rm = TRUE),
          sny_m3_sum = sum(sny_m3, na.rm = TRUE),
          spr_m3_sum = sum(spr_m3, na.rm = TRUE),
          fish_m3_sum = sum(fish_m3, na.rm = TRUE),
          lmc_m3_sum = sum(lmc_m3, na.rm = TRUE)
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
  
  
  # Include observations after the final NADP collection date
  
  last_date <- precip_dates_times$date_time[
    nrow(precip_dates_times)
  ]
  
  post_last_date_data <- QtQo %>%
    filter(
      date_time >= last_date
    )
  
  if (nrow(post_last_date_data) > 0) {
    
    post_last_date_sums <- post_last_date_data %>%
      summarize(
        umc_m3_sum = sum(umc_m3, na.rm = TRUE),
        sny_m3_sum = sum(sny_m3, na.rm = TRUE),
        spr_m3_sum = sum(spr_m3, na.rm = TRUE),
        fish_m3_sum = sum(fish_m3, na.rm = TRUE),
        lmc_m3_sum = sum(lmc_m3, na.rm = TRUE)
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


# Apply function

results <- sum_tribs_out(
  QtQo,
  precip_dates_times
)

# Initialize working water budget

working_budget <- results


# 5. Sum tributary and outlet volume: lower estimates -------------------------

# Repeat interval summation using lower-bound discharge estimates.

sum_tribs_out_lwr <- function(
    QtQo,
    precip_dates_times
) {
  
  # Initialize results dataframe
  
  results <- data.frame(
    start_date = character(),
    end_date = character(),
    umc_m3_lwr_sum = numeric(),
    sny_m3_lwr_sum = numeric(),
    spr_m3_lwr_sum = numeric(),
    fish_m3_lwr_sum = numeric(),
    lmc_m3_lwr_sum = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  # Include observations before first NADP collection date
  
  first_date <- precip_dates_times$date_time[1]
  
  pre_first_date_data <- QtQo %>%
    filter(
      date_time < first_date
    )
  
  if (nrow(pre_first_date_data) > 0) {
    
    pre_first_date_sums <- pre_first_date_data %>%
      summarize(
        umc_m3_lwr_sum = sum(umc_m3_lwr, na.rm = TRUE),
        sny_m3_lwr_sum = sum(sny_m3_lwr, na.rm = TRUE),
        spr_m3_lwr_sum = sum(spr_m3_lwr, na.rm = TRUE),
        fish_m3_lwr_sum = sum(fish_m3_lwr, na.rm = TRUE),
        lmc_m3_lwr_sum = sum(lmc_m3_lwr, na.rm = TRUE)
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
  
  
  # Sum observations within each NADP sampling interval
  
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    
    start_date <- precip_dates_times$date_time[i]
    end_date <- precip_dates_times$date_time[i + 1]
    
    range_data <- QtQo %>%
      filter(
        date_time >= start_date &
          date_time < end_date
      )
    
    if (nrow(range_data) > 0) {
      
      range_sums <- range_data %>%
        summarize(
          umc_m3_lwr_sum = sum(umc_m3_lwr, na.rm = TRUE),
          sny_m3_lwr_sum = sum(sny_m3_lwr, na.rm = TRUE),
          spr_m3_lwr_sum = sum(spr_m3_lwr, na.rm = TRUE),
          fish_m3_lwr_sum = sum(fish_m3_lwr, na.rm = TRUE),
          lmc_m3_lwr_sum = sum(lmc_m3_lwr, na.rm = TRUE)
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
  
  
  # Include observations after final NADP collection date
  
  last_date <- precip_dates_times$date_time[
    nrow(precip_dates_times)
  ]
  
  post_last_date_data <- QtQo %>%
    filter(
      date_time >= last_date
    )
  
  if (nrow(post_last_date_data) > 0) {
    
    post_last_date_sums <- post_last_date_data %>%
      summarize(
        umc_m3_lwr_sum = sum(umc_m3_lwr, na.rm = TRUE),
        sny_m3_lwr_sum = sum(sny_m3_lwr, na.rm = TRUE),
        spr_m3_lwr_sum = sum(spr_m3_lwr, na.rm = TRUE),
        fish_m3_lwr_sum = sum(fish_m3_lwr, na.rm = TRUE),
        lmc_m3_lwr_sum = sum(lmc_m3_lwr, na.rm = TRUE)
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


# Apply lower-bound function

results_lwr <- sum_tribs_out_lwr(
  QtQo,
  precip_dates_times
)

# Add lower-bound results to working budget

working_budget <- working_budget %>%
  inner_join(
    results_lwr,
    by = "end_date"
  )


# 6. Sum tributary and outlet volume: upper estimates -------------------------

# Repeat interval summation using upper-bound discharge estimates.

sum_tribs_out_upr <- function(
    QtQo,
    precip_dates_times
) {
  
  # Initialize results dataframe
  
  results <- data.frame(
    start_date = character(),
    end_date = character(),
    umc_m3_upr_sum = numeric(),
    sny_m3_upr_sum = numeric(),
    spr_m3_upr_sum = numeric(),
    fish_m3_upr_sum = numeric(),
    lmc_m3_upr_sum = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  # Include observations before first NADP collection date
  
  first_date <- precip_dates_times$date_time[1]
  
  pre_first_date_data <- QtQo %>%
    filter(
      date_time < first_date
    )
  
  if (nrow(pre_first_date_data) > 0) {
    
    pre_first_date_sums <- pre_first_date_data %>%
      summarize(
        umc_m3_upr_sum = sum(umc_m3_upr, na.rm = TRUE),
        sny_m3_upr_sum = sum(sny_m3_upr, na.rm = TRUE),
        spr_m3_upr_sum = sum(spr_m3_upr, na.rm = TRUE),
        fish_m3_upr_sum = sum(fish_m3_upr, na.rm = TRUE),
        lmc_m3_upr_sum = sum(lmc_m3_upr, na.rm = TRUE)
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
  
  
  # Sum observations within each NADP sampling interval
  
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    
    start_date <- precip_dates_times$date_time[i]
    end_date <- precip_dates_times$date_time[i + 1]
    
    range_data <- QtQo %>%
      filter(
        date_time >= start_date &
          date_time < end_date
      )
    
    if (nrow(range_data) > 0) {
      
      range_sums <- range_data %>%
        summarize(
          umc_m3_upr_sum = sum(umc_m3_upr, na.rm = TRUE),
          sny_m3_upr_sum = sum(sny_m3_upr, na.rm = TRUE),
          spr_m3_upr_sum = sum(spr_m3_upr, na.rm = TRUE),
          fish_m3_upr_sum = sum(fish_m3_upr, na.rm = TRUE),
          lmc_m3_upr_sum = sum(lmc_m3_upr, na.rm = TRUE)
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
  
  
  # Include observations after final NADP collection date
  
  last_date <- precip_dates_times$date_time[
    nrow(precip_dates_times)
  ]
  
  post_last_date_data <- QtQo %>%
    filter(
      date_time >= last_date
    )
  
  if (nrow(post_last_date_data) > 0) {
    
    post_last_date_sums <- post_last_date_data %>%
      summarize(
        umc_m3_upr_sum = sum(umc_m3_upr, na.rm = TRUE),
        sny_m3_upr_sum = sum(sny_m3_upr, na.rm = TRUE),
        spr_m3_upr_sum = sum(spr_m3_upr, na.rm = TRUE),
        fish_m3_upr_sum = sum(fish_m3_upr, na.rm = TRUE),
        lmc_m3_upr_sum = sum(lmc_m3_upr, na.rm = TRUE)
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

results_upr <- sum_tribs_out_upr(
  QtQo,
  precip_dates_times
)

# Add upper-bound results to working budget

working_budget <- working_budget %>%
  inner_join(
    results_upr,
    by = "end_date"
  )


# 7. Add wet deposition --------------------------------------------------------

# Match precipitation volume to the corresponding NADP interval end date.

working_budget <- working_budget %>%
  inner_join(
    Lw,
    by = c(
      "end_date" = "dateOff"
    )
  )

# Replace missing precipitation volumes with zero

working_budget <- working_budget %>%
  mutate(
    L3w = if_else(
      is.na(L3w),
      0,
      L3w
    )
  )


# 8. Add reconstructed lake volume --------------------------------------------

# Match reconstructed lake volume to the end of each water-budget interval.

working_budget <- working_budget %>%
  inner_join(
    lake,
    by = c(
      "end_date" = "date_time"
    )
  )


# 9. Sum evaporation: mean estimates ------------------------------------------

# Sum daily mean evaporation within each NADP sampling interval.

summarize_evap <- function(
    Le,
    precip_dates_times
) {
  
  # Convert timestamps to Date for daily evaporation aggregation
  
  Le <- Le %>%
    mutate(
      date = as.Date(
        date_time,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      )
    )
  
  precip_dates_times <- precip_dates_times %>%
    mutate(
      date = as.Date(
        date_time,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      )
    )
  
  
  # Initialize results dataframe
  
  results <- data.frame(
    start_date = character(),
    end_date = character(),
    evap_m3 = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  # Include evaporation before first NADP collection date
  
  first_date <- precip_dates_times$date[1]
  
  range_data_evap <- Le %>%
    filter(
      date < first_date
    )
  
  if (nrow(range_data_evap) > 0) {
    
    pre_first_date_sum <- range_data_evap %>%
      summarize(
        total_sum = sum(
          L3E_m3_d,
          na.rm = TRUE
        )
      ) %>%
      pull(total_sum)
    
    results <- rbind(
      results,
      data.frame(
        start_date = NA,
        end_date = as.character(first_date),
        evap_m3 = pre_first_date_sum,
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  # Sum evaporation within each NADP sampling interval
  
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    
    start_date <- precip_dates_times$date[i]
    end_date <- precip_dates_times$date[i + 1]
    
    range_data_evap <- Le %>%
      filter(
        date >= start_date &
          date < end_date
      )
    
    evap_m3 <- if (
      nrow(range_data_evap) == 0
    ) {
      0
    } else {
      range_data_evap %>%
        summarize(
          total_sum = sum(
            L3E_m3_d,
            na.rm = TRUE
          )
        ) %>%
        pull(total_sum)
    }
    
    results <- rbind(
      results,
      data.frame(
        start_date = as.character(start_date),
        end_date = as.character(end_date),
        evap_m3 = evap_m3,
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  # Include evaporation after final NADP collection date
  
  last_date <- precip_dates_times$date[
    nrow(precip_dates_times)
  ]
  
  range_data_evap <- Le %>%
    filter(
      date >= last_date
    )
  
  if (nrow(range_data_evap) > 0) {
    
    post_last_date_sum <- range_data_evap %>%
      summarize(
        total_sum = sum(
          L3E_m3_d,
          na.rm = TRUE
        )
      ) %>%
      pull(total_sum)
    
    results <- rbind(
      results,
      data.frame(
        start_date = as.character(last_date),
        end_date = NA,
        evap_m3 = post_last_date_sum,
        stringsAsFactors = FALSE
      )
    )
  }
  
  return(results)
}


# Apply mean evaporation function

results <- summarize_evap(
  Le,
  precip_dates_times
)


# Convert interval end dates to Date before joining

working_budget$end_date <- as.Date(
  working_budget$end_date,
  "%Y-%m-%d",
  tz = "UTC"
)

results$end_date <- as.Date(
  results$end_date,
  "%Y-%m-%d",
  tz = "UTC"
)

# Add mean evaporation to working budget

working_budget <- working_budget %>%
  inner_join(
    results,
    by = "end_date"
  )


# 10. Sum evaporation: upper estimates ----------------------------------------

# Repeat interval summation using maximum daily evaporation estimates.

summarize_evap_upr <- function(
    Le,
    precip_dates_times
) {
  
  # Convert timestamps to Date
  
  Le <- Le %>%
    mutate(
      date = as.Date(
        date_time,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      )
    )
  
  precip_dates_times <- precip_dates_times %>%
    mutate(
      date = as.Date(
        date_time,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      )
    )
  
  
  # Initialize results dataframe
  
  results <- data.frame(
    start_date = character(),
    end_date = character(),
    evap_max = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  # Include evaporation before first NADP collection date
  
  first_date <- precip_dates_times$date[1]
  
  range_data_evap <- Le %>%
    filter(
      date < first_date
    )
  
  if (nrow(range_data_evap) > 0) {
    
    pre_first_date_sum <- range_data_evap %>%
      summarize(
        total_sum = sum(
          L3E_m3_d_max,
          na.rm = TRUE
        )
      ) %>%
      pull(total_sum)
    
    results <- rbind(
      results,
      data.frame(
        start_date = NA,
        end_date = as.character(first_date),
        evap_max = pre_first_date_sum,
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  # Sum evaporation within each NADP sampling interval
  
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    
    start_date <- precip_dates_times$date[i]
    end_date <- precip_dates_times$date[i + 1]
    
    range_data_evap <- Le %>%
      filter(
        date >= start_date &
          date < end_date
      )
    
    evap_max <- if (
      nrow(range_data_evap) == 0
    ) {
      0
    } else {
      range_data_evap %>%
        summarize(
          total_sum = sum(
            L3E_m3_d_max,
            na.rm = TRUE
          )
        ) %>%
        pull(total_sum)
    }
    
    results <- rbind(
      results,
      data.frame(
        start_date = as.character(start_date),
        end_date = as.character(end_date),
        evap_max = evap_max,
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  # Include evaporation after final NADP collection date
  
  last_date <- precip_dates_times$date[
    nrow(precip_dates_times)
  ]
  
  range_data_evap <- Le %>%
    filter(
      date >= last_date
    )
  
  if (nrow(range_data_evap) > 0) {
    
    post_last_date_sum <- range_data_evap %>%
      summarize(
        total_sum = sum(
          L3E_m3_d_max,
          na.rm = TRUE
        )
      ) %>%
      pull(total_sum)
    
    results <- rbind(
      results,
      data.frame(
        start_date = as.character(last_date),
        end_date = NA,
        evap_max = post_last_date_sum,
        stringsAsFactors = FALSE
      )
    )
  }
  
  return(results)
}


# Apply upper evaporation function

results_upr <- summarize_evap_upr(
  Le,
  precip_dates_times
)

results_upr$end_date <- as.Date(
  results_upr$end_date,
  "%Y-%m-%d",
  tz = "UTC"
)


# Add upper evaporation estimates to working budget

working_budget <- working_budget %>%
  inner_join(
    results_upr,
    by = "end_date"
  )


# 11. Sum evaporation: lower estimates ----------------------------------------

# Repeat interval summation using minimum daily evaporation estimates.

summarize_evap_lwr <- function(
    Le,
    precip_dates_times
) {
  
  # Convert timestamps to Date
  
  Le <- Le %>%
    mutate(
      date = as.Date(
        date_time,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      )
    )
  
  precip_dates_times <- precip_dates_times %>%
    mutate(
      date = as.Date(
        date_time,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      )
    )
  
  
  # Initialize results dataframe
  
  results <- data.frame(
    start_date = character(),
    end_date = character(),
    evap_min = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  # Include evaporation before first NADP collection date
  
  first_date <- precip_dates_times$date[1]
  
  range_data_evap <- Le %>%
    filter(
      date < first_date
    )
  
  if (nrow(range_data_evap) > 0) {
    
    pre_first_date_sum <- range_data_evap %>%
      summarize(
        total_sum = sum(
          L3E_m3_d_min,
          na.rm = TRUE
        )
      ) %>%
      pull(total_sum)
    
    results <- rbind(
      results,
      data.frame(
        start_date = NA,
        end_date = as.character(first_date),
        evap_min = pre_first_date_sum,
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  # Sum evaporation within each NADP sampling interval
  
  for (i in 1:(nrow(precip_dates_times) - 1)) {
    
    start_date <- precip_dates_times$date[i]
    end_date <- precip_dates_times$date[i + 1]
    
    range_data_evap <- Le %>%
      filter(
        date >= start_date &
          date < end_date
      )
    
    evap_min <- if (
      nrow(range_data_evap) == 0
    ) {
      0
    } else {
      range_data_evap %>%
        summarize(
          total_sum = sum(
            L3E_m3_d_min,
            na.rm = TRUE
          )
        ) %>%
        pull(total_sum)
    }
    
    results <- rbind(
      results,
      data.frame(
        start_date = as.character(start_date),
        end_date = as.character(end_date),
        evap_min = evap_min,
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  # Include evaporation after final NADP collection date
  
  last_date <- precip_dates_times$date[
    nrow(precip_dates_times)
  ]
  
  range_data_evap <- Le %>%
    filter(
      date >= last_date
    )
  
  if (nrow(range_data_evap) > 0) {
    
    post_last_date_sum <- range_data_evap %>%
      summarize(
        total_sum = sum(
          L3E_m3_d_min,
          na.rm = TRUE
        )
      ) %>%
      pull(total_sum)
    
    results <- rbind(
      results,
      data.frame(
        start_date = as.character(last_date),
        end_date = NA,
        evap_min = post_last_date_sum,
        stringsAsFactors = FALSE
      )
    )
  }
  
  return(results)
}


# Apply lower evaporation function

results_lwr <- summarize_evap_lwr(
  le,
  precip_dates_times
)

results_lwr$end_date <- as.Date(
  results_lwr$end_date,
  "%Y-%m-%d",
  tz = "UTC"
)


# Add lower evaporation estimates to working budget

working_budget <- working_budget %>%
  inner_join(
    results_lwr,
    by = "end_date"
  )


# 12. Clean water-budget columns ----------------------------------------------

# Retain relevant variables and simplify column names.

working_budget <- working_budget %>%
  select(
    start_date.x,
    end_date,
    
    umc_m3_sum,
    umc_m3_lwr_sum,
    umc_m3_upr_sum,
    
    sny_m3_sum,
    sny_m3_lwr_sum,
    sny_m3_upr_sum,
    
    spr_m3_sum,
    spr_m3_lwr_sum,
    spr_m3_upr_sum,
    
    fish_m3_sum,
    fish_m3_lwr_sum,
    fish_m3_upr_sum,
    
    lmc_m3_sum,
    lmc_m3_lwr_sum,
    lmc_m3_upr_sum,
    
    evap_m3,
    evap_min,
    evap_max,
    
    L3w,
    
    lake_volume,
    lake_volume_lwr,
    lake_volume_upr
  ) %>%
  rename(
    start_date = start_date.x,
    
    umc_m3 = umc_m3_sum,
    umc_m3_lwr = umc_m3_lwr_sum,
    umc_m3_upr = umc_m3_upr_sum,
    
    sny_m3 = sny_m3_sum,
    sny_m3_lwr = sny_m3_lwr_sum,
    sny_m3_upr = sny_m3_upr_sum,
    
    spr_m3 = spr_m3_sum,
    spr_m3_lwr = spr_m3_lwr_sum,
    spr_m3_upr = spr_m3_upr_sum,
    
    fish_m3 = fish_m3_sum,
    fish_m3_lwr = fish_m3_lwr_sum,
    fish_m3_upr = fish_m3_upr_sum,
    
    lmc_m3 = lmc_m3_sum,
    lmc_m3_lwr = lmc_m3_lwr_sum,
    lmc_m3_upr = lmc_m3_upr_sum,
    
    Le_m3 = evap_m3,
    Le_m3_lwr = evap_min,
    Le_m3_upr = evap_max,
    
    Lw_m3 = L3w
  )


# 13. Calculate water-budget residuals ----------------------------------------

# Water-budget residual:
#
#   residual =
#     change in lake volume
#     - tributary inflows
#     - wet deposition
#     + lake outflow
#     + evaporation
#
# Separate residual bounds are calculated using corresponding lower and
# upper estimates from the water-budget components.

working_budget <- working_budget %>%
  mutate(
    
    resid =
      (lead(lake_volume) - lake_volume) -
      rowSums(
        cbind(
          umc_m3,
          sny_m3,
          spr_m3,
          fish_m3,
          Lw_m3
        ),
        na.rm = TRUE
      ) +
      lmc_m3 +
      Le_m3,
    
    resid_upr =
      (lead(lake_volume_upr) - lake_volume_upr) -
      rowSums(
        cbind(
          umc_m3_lwr,
          sny_m3_lwr,
          spr_m3_lwr,
          fish_m3_lwr,
          Lw_m3
        ),
        na.rm = TRUE
      ) +
      lmc_m3_lwr +
      Le_m3_lwr,
    
    resid_lwr =
      (lead(lake_volume_lwr) - lake_volume_lwr) -
      rowSums(
        cbind(
          umc_m3_upr,
          sny_m3_upr,
          spr_m3_upr,
          fish_m3_upr,
          Lw_m3
        ),
        na.rm = TRUE
      ) +
      lmc_m3_upr +
      Le_m3_upr
  )


# Remove first row because precipitation and evaporation are incomplete
# for the initial interval.

working_budget <- working_budget[-1, ]


# 14. Check residual uncertainty bounds ---------------------------------------

# Check whether all mean residual estimates fall between their lower and
# upper uncertainty bounds.

all_within_bounds <- function(
    working_budget
) {
  
  all(
    working_budget$resid >
      working_budget$resid_lwr &
      working_budget$resid <
      working_budget$resid_upr
  )
}

all_within_bounds(
  working_budget
)


# Identify rows where the mean residual does not fall within its bounds.

which_fail <- function(
    working_budget
) {
  
  which(
    !(
      working_budget$resid >
        working_budget$resid_lwr &
        working_budget$resid <
        working_budget$resid_upr
    )
  )
}

which_fail(
  working_budget
)


# 15. Save water budget --------------------------------------------------------

write.csv(
  working_budget,
  here(
    "2_incremental",
    "WaterBudget.csv"
  ),
  row.names = FALSE
)

#16. Plot residuals ------------------------------------------

ggplot(working_budget, aes(x = end_date)) +
  geom_ribbon(aes(ymin = (resid_lwr), ymax = (resid_upr)), fill = "cadetblue", alpha = 0.4) +
  geom_line(data = working_budget, aes(y = (resid)), linewidth = 0.7, color = "cadetblue") +
  labs(x = "Date", title = "") +
  theme_classic() + 
  scale_x_date(
    limits = as.Date(c("2007-10-01", "2023-10-01")),
    date_breaks = "6 months",
    date_labels = "%b-%Y",
    expand = c(0, 0)
  )  +
  theme(
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.25),
    plot.title = element_text(size = 12)
  ) +
  scale_y_continuous(
    limits = c(-1e8, 4e7),
    breaks = pretty_breaks(n = 7), labels = comma) +
  labs(y = expression(epsilon ~ "(" * m^3 * ")")) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) + theme(legend.position = "none")



