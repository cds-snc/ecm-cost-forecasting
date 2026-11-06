# This script queries the number of active users per department and uses that
# as training data to forecast future number of active users. It then writes
# it to a cut-and-pasteable file for use in the 2026-27 forecasting spreadsheet.

# In order to access the VendorData database, you must be connected to the 
# ESDC VPN and you must have the credentials saved in Windows Credential Manager

library(tidyverse)
library(fable)
library(feasts)
library(DBI)
library(tsibble)
library(lubridate)
source("00-setup.R")

# Query the database ------------------------------------------------------

historical_active_users <- 
  tbl(vendor_data, "Monthly_Creds_by_DEPT") |>
  select(
    acronym_bi = DepartmentName,
    date,
    active_users = Creds
  ) |>
  collect() |>
  mutate(date = as_date(date))

# Forecasts ---------------------------------------------------------------

# Convert to a time series table for easier forecasting
hau_ts <- historical_active_users |>
  filter(!is.na(active_users)) |>
  mutate(ym = yearmonth(date)) |>
  as_tsibble(index = ym, key = acronym_bi)

# Calculate fallback statistics from each acronym's full history
hau_fallback_stats <- historical_active_users |>
  filter(!is.na(active_users)) |>
  group_by(acronym_bi) |>
  summarise(
    historical_mean = mean(active_users, na.rm = TRUE),
    historical_sd = sd(active_users, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    # sd() returns NA when an acronym has fewer than two observations.
    # In that case, use zero-width intervals around the historical mean.
    historical_sd = replace_na(historical_sd, 0)
  )

# A straightforward ARIMA, using the default parameters for model selection
hau_model <- model(
  hau_ts,
  ARIMA(active_users)
)

# Forecast ahead a full year
hau_forecast <- forecast(hau_model, h = 12)

# Critical values for the fallback prediction intervals
z_89 <- qnorm(1 - (1 - 0.89) / 2)
z_95 <- qnorm(1 - (1 - 0.95) / 2)

# Convert back to a regular tibble and apply the fallback forecast
hau_forecast_clean <- as_tibble(hau_forecast) |>
  left_join(
    hau_fallback_stats,
    by = "acronym_bi"
  ) |>
  mutate(
    date = as_date(ym),
    
    # Identify forecasts for which ARIMA did not produce a point estimate
    used_fallback = is.na(.mean),
    
    # ARIMA intervals
    arima_lower_89 = hilo(active_users, 89)$lower,
    arima_upper_89 = hilo(active_users, 89)$upper,
    arima_lower_95 = hilo(active_users, 95)$lower,
    arima_upper_95 = hilo(active_users, 95)$upper,
    
    # Use the historical mean only when the ARIMA forecast is missing
    active_users = if_else(
      used_fallback,
      historical_mean,
      .mean
    ),
    
    # Use mean +/- z * historical SD for fallback intervals.
    # pmax() prevents negative user counts.
    lower_89 = if_else(
      used_fallback,
      pmax(0, historical_mean - z_89 * historical_sd),
      arima_lower_89
    ),
    upper_89 = if_else(
      used_fallback,
      historical_mean + z_89 * historical_sd,
      arima_upper_89
    ),
    lower_95 = if_else(
      used_fallback,
      pmax(0, historical_mean - z_95 * historical_sd),
      arima_lower_95
    ),
    upper_95 = if_else(
      used_fallback,
      historical_mean + z_95 * historical_sd,
      arima_upper_95
    ),
    
    # Round forecasts and intervals to whole users
    across(
      c(active_users, lower_89, upper_89, lower_95, upper_95),
      ~ round(.x, digits = 0)
    ),
    
    is_forecast = TRUE
  ) |>
  select(
    acronym_bi,
    date,
    active_users,
    upper_89,
    lower_89,
    upper_95,
    lower_95,
    is_forecast,
    used_fallback
  )
