# ============================================================
# Middle Fork Flathead River donor gage
# USGS site 12358500
# ============================================================

# 0. Load packages ----

library(dataRetrieval)
library(dplyr)
library(lubridate)
library(zoo)
library(here)


# 1. Download donor-gage data from USGS ----

site_number <- "12358500"

# USGS parameter codes:
# 00060 = discharge (ft3/s)
# 00065 = gage height (ft)

parameter_cd <- c("00060", "00065")

MF <- readNWISuv(
  siteNumbers = site_number,
  parameterCd = parameter_cd,
  startDate   = "2007-10-01T00:00Z",
  endDate     = "2024-09-30T23:00Z"
)


# Save raw USGS download ----

write.csv(
  MF,
  here("0_data", "MF_USGS_12358500.csv"),
  row.names = FALSE
)


# 2. Standardize timestamps ----

MF <- MF %>%
  mutate(
    dateTime = as.POSIXct(dateTime, tz = "UTC"),
    minute = minute(dateTime)
  )


# Retain observations at minute 00 when available;
# otherwise retain minute 01

filtered_MF <- MF %>%
  group_by(hour = floor_date(dateTime, unit = "hour")) %>%
  filter(minute %in% c(0, 1)) %>%
  slice_min(minute, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(dateTime = round_date(dateTime, unit = "hour")) %>%
  select(-minute, -hour)


# 3. Create complete hourly time series ----

complete_timestamps <- data.frame(
  dateTime = seq(
    from = as.POSIXct(
      "2007-10-01 00:00:00",
      tz = "UTC"
    ),
    to = as.POSIXct(
      "2024-09-30 23:00:00",
      tz = "UTC"
    ),
    by = "hour"
  )
)


MF_WY_2008_2024 <- complete_timestamps %>%
  left_join(
    filtered_MF,
    by = "dateTime"
  ) %>%
  select(
    dateTime,
    X_00060_00000,
    X_00065_00000
  ) %>%
  rename(
    MF_Q_cfs = X_00060_00000,
    height   = X_00065_00000
  )


# 4. Convert discharge from ft3/s to m3/s ----

MF_WY_2008_2024 <- MF_WY_2008_2024 %>%
  mutate(
    MF_Q_m3_s = MF_Q_cfs * 0.0283168466
  )


# 5. Fill missing hourly observations ----
#
# Missing values are first estimated using the mean of a
# centered 5-observation window. Any remaining missing values
# are filled using forward fill followed by backward fill.

replace_na_with_mean <- function(data, column_name) {
  
  data %>%
    mutate(
      !!sym(column_name) := ifelse(
        is.na(!!sym(column_name)),
        rollapplyr(
          !!sym(column_name),
          width = 5,
          FUN = function(x) {
            if (all(is.na(x))) {
              NA
            } else {
              mean(x, na.rm = TRUE)
            }
          },
          fill = NA,
          align = "center"
        ),
        !!sym(column_name)
      )
    ) %>%
    mutate(
      !!sym(column_name) :=
        zoo::na.locf(
          !!sym(column_name),
          na.rm = FALSE
        )
    ) %>%
    mutate(
      !!sym(column_name) :=
        zoo::na.locf(
          !!sym(column_name),
          fromLast = TRUE,
          na.rm = FALSE
        )
    )
}


MF_WY_2008_2024 <- MF_WY_2008_2024 %>%
  replace_na_with_mean("MF_Q_m3_s") %>%
  replace_na_with_mean("height") %>%
  replace_na_with_mean("MF_Q_cfs")


# 6. Save processed donor-gage dataset ----

write.csv(
  MF_WY_2008_2024,
  here("2_incremental", "Q_donor_gage.csv"),
  row.names = FALSE
)
