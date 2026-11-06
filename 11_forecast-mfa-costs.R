# This script queries the MFA costs of a given department and creates forecasts
# of the MFA costs into the future

library(tidyverse)
library(fable)
library(feasts)
library(DBI)
library(tsibble)
library(lubridate)
source("00-setup.R")


# Query the database ------------------------------------------------------

costs <- left_join(
  tbl(vendor_data, "service_costs"),
  tbl(vendor_data, "service_costs_type"),
  by = c("sct_fk" = "id")
) |>
  left_join(
    tbl(vendor_data, "gc_departments"),
    by = c("gcid_fk" = "id")
  ) |>
  filter(
    description_en == "Twilio Variable Cost",
    !(gcid_fk %in% ignored_gcids)
    ) |>
  mutate(acronym_bi = paste0(acronym_en, " / ", acronym_fr)) |>
  select(
    date = effective_date,
    acronym_bi,
    amount = ammount
  ) |>
  collect() |>
  mutate(date = as_date(date))


# Forecasts ---------------------------------------------------------------

# Convert to a time series table for easier forecasting
costs_ts <- costs |>
  mutate(ym = yearmonth(date)) |>
  as_tsibble(index = ym, key = acronym_bi)

# A straightforward ARIMA, using the default parameters for model selection
costs_model <- model(costs_ts, ARIMA(amount))

# Using that model, forecast ahead a full year
# We will eventually drop the points beyond the end of the current fiscal year
costs_forecast <- forecast(costs_model, h = 12)


# Convert back to a regular tibble so it behaves as we expect on joins and binds
costs_forecast_clean <- as_tibble(costs_forecast) |>
  mutate(
    date = as_date(ym),
    lower_95 = hilo(amount, 95)$lower,
    lower_80 = hilo(amount, 80)$lower,
    upper_80 = hilo(amount, 80)$upper,
    upper_95 = hilo(amount, 95)$upper,
    is_forecast = TRUE,
    .mean = round(.mean, digits = 2)
  ) |>
  select(
    acronym_bi,
    date,
    amount = .mean, 
    starts_with("upper"),
    starts_with("lower")
  )
