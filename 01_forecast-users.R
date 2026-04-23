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
  mutate(ym = yearmonth(date)) |>
  as_tsibble(index = ym, key = acronym_bi)

# A straightforward ARIMA, using the default parameters for model selection
hau_model <- model(hau_ts, ARIMA(active_users))

# Using that model, forecast ahead a full year
# We will eventually drop the points beyond the end of the current fiscal year
hau_forecast <- forecast(hau_model, h = 12)


# Convert back to a regular tibble so it behaves as we expect on joins and binds
hau_forecast_clean <- as_tibble(hau_forecast) |>
  mutate(
    date = as_date(ym),
    lower_95 = hilo(active_users, 95)$lower,
    lower_80 = hilo(active_users, 80)$lower,
    upper_80 = hilo(active_users, 80)$upper,
    upper_95 = hilo(active_users, 95)$upper,
    is_forecast = TRUE,
    .mean = round(.mean, digits = 0)
  ) |>
  select(
    acronym_bi,
    date,
    active_users = .mean, 
    starts_with("upper"),
    starts_with("lower")
  )
