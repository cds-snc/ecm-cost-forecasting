# This script forecasts future monthly costs to ECM partner departments. It 
# reads  historical costs from the ECM `VendorData` database, from the table
# `costs_historical`. It then performs simple ARIMA model selection and
# forecasts 12 months into the future.

# After forecasting, the resulting forecasts are written to the `costs_forecast`
# table in the VendorData database. These forecasts are used in the Financial
# Report.

# In order to access the VendorData database, you must be connected to the 
# ESDC VPN.

library(tidyverse)
library(fable)
library(feasts)
library(DBI)
source("useful-functions.R")


# Database parameters -----------------------------------------------------

# This is the path to the ECM reporting database. In order to connect to this
# database, you must be connected to the ESDC VPN. You must also set the
# username/password secrets ahead of time. Manage your secrets with:

# keyring::key_set("ECM-DB-WRITE-USER")  # Enter the DB username
# keyring::key_set("ECM-DB-WRITE-PASS")  # Enter the DB password

con <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "MLCNESD4150.hrdc-drhc.net\\MLDB16SA01",
  Database = "VendorData",
  UID = keyring::key_get("ECM-DB-WRITE-USER"),
  PWD = keyring::key_get("ECM-DB-WRITE-PASS"),
)

# Read in data ------------------------------------------------------------

# This is the historical cost data, directly from the database
# We write these data earlier in the script `01-get_and_write_costs.R`
historical_costs <- dbReadTable(con, "costs_historical")


# Forecasts ---------------------------------------------------------------

# Convert to a time series table for easier forecasting
hc_tsb <- historical_costs %>%
  mutate(ym = yearmonth(date)) %>%
  as_tsibble(index = ym, key = gcid_fk)

# A straightforward ARIMA, using the default parameters for model selection
hc_model <- model(hc_tsb, ARIMA(cost))

# Using that model, forecast ahead a full year
# We will eventually drop the points beyond the end of the current fiscal year
hc_forecast <- forecast(hc_model, h = 12)


# Convert back to a regular tibble so it behaves as we expect on joins and binds
hc_forecast_clean <- as_tibble(hc_forecast) %>%
  mutate(
    date = as_date(ym),
    fy = to_fy(date),
    lower_95 = hilo(cost, 95)$lower,
    lower_80 = hilo(cost, 80)$lower,
    upper_80 = hilo(cost, 80)$upper,
    upper_95 = hilo(cost, 95)$upper,
    is_forecast = TRUE
  ) %>%
  select(
    gcid_fk,
    date,
    fy,
    cost = .mean, 
    starts_with("upper"),
    starts_with("lower")
  ) 


# Write to the database ---------------------------------------------------

if (!DBI::dbExistsTable(con, "costs_forecast")) {
  # To create tables, you must be logged in as the admin user
  # The read/write user does not have sufficient privileges
  DBI::dbExecute(con, 
                 "               
    CREATE TABLE dbo.costs_forecast (
        id                          INT IDENTITY(1,1) NOT NULL,
        gcid_fk                     INT               NOT NULL,
        forecasted_on               DATE              NOT NULL,
        forecast_date               DATE              NOT NULL,
        forecasted_cost             DECIMAL(19,4)     NOT NULL,
        forecasted_cost_lower_95    DECIMAL(19,4)     NOT NULL,
        forecasted_cost_lower_80    DECIMAL(19,4)     NOT NULL,
        forecasted_cost_upper_80    DECIMAL(19,4)     NOT NULL,
        forecasted_cost_upper_95    DECIMAL(19,4)     NOT NULL,
        CONSTRAINT PK_costs_forecast PRIMARY KEY CLUSTERED (id),
        CONSTRAINT FK_costs_forecast_gc_departments
        FOREIGN KEY (gcid_fk)
        REFERENCES dbo.gc_departments (id)
    );
    "         
  )
}

# We want to upsert the new forecasts - that means overwriting old forecasts
# if we've already made forecasts for this month

# Get the most recent value in the table - we only want to add new rows
# We don't want to modify old rows
most_recent_forecasted_on <- tbl(con, "costs_forecast") %>%
  filter(forecasted_on == max(forecasted_on, na.rm = TRUE)) %>%
  select(forecasted_on) %>%
  collect() %>%
  pull(1) %>%
  unique() 

# If there is no recent data in the database, use a date far in the past
most_recent_forecasted_on <- max(c(
  as_date(most_recent_forecasted_on),
  as_date("1970-01-01")
))


to_append <- hc_forecast_clean %>%
  mutate(forecasted_on = min(date) - months(1)) %>%
  filter(forecasted_on > most_recent_forecasted_on) %>%
  select(
    gcid_fk,
    forecasted_on,
    forecast_date = date,
    forecasted_cost = cost,
    forecasted_cost_lower_95 = lower_95,
    forecasted_cost_lower_80 = lower_80,
    forecasted_cost_upper_80 = upper_80,
    forecasted_cost_upper_95 = upper_95
  ) %>%
  # Replace unforecast-able scenarios with zeros and Infs with very high values
  mutate(
    across(starts_with("forecasted_cost"), ~ ifelse(is.na(.), 0, .)),  # Replace unforecastable values with zero
    # If we can't calculate confidence intervals, replace with the estimate
    across(starts_with("forecasted_cost_upper"), ~ ifelse(. == +Inf, forecasted_cost, .)),
    across(starts_with("forecasted_cost_lower"), ~ ifelse(. == -Inf, forecasted_cost, .))
    )

# Do some sanity checks here
stopifnot(nrow(to_append) > 0)

# All of the forecast costs should be numeric and finite
stopifnot(all(is.finite(c(
  to_append$forecasted_cost,
  to_append$forecasted_cost_lower_95,
  to_append$forecasted_cost_lower_80,
  to_append$forecasted_cost_upper_80,
  to_append$forecasted_cost_upper_95
))))

# We shouldn't be writing data older than 65 days
stopifnot(abs(as.numeric(today() - most_recent_forecasted_on)) < 65)  


# If we pass all of the sanity checks, write to the table
DBI::dbAppendTable(con, "costs_forecast", value = to_append)

message("All done")
