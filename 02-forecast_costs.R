library(tidyverse)
library(fable)
library(feasts)
source("useful-functions.R")

# Read in data ------------------------------------------------------------

# These datasets are created by the script `01-get_data.R`, which should exist
# in the same folder.

historical_costs <- read_csv("data/historical_costs.csv", show_col_types = FALSE)
mou_values <- read_csv("data/mou_values.csv", show_col_types = FALSE)

# Let's keep track of the values that are ACTUALS, not FORECASTS
historical_costs$is_forecast <- FALSE

# The fiscal year of the most recent date in the historical data
max_fy <- historical_costs %>%
  filter(date == max(date)) %>%
  pull(fy) %>%
  first()

# Forecasts ---------------------------------------------------------------

# Convert to a time series table for easier forecasting
hc_tsb <- historical_costs %>%
  mutate(ym = yearmonth(date)) %>%
  as_tsibble(index = ym, key = department)

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
    department, 
    date,
    fy,
    cost = .mean, 
    starts_with("upper"),
    starts_with("lower"),
    is_forecast
    )

# We also write the forecasts in a convenient, long format
# This is used in the Quarto document (.Qmd) file that helps
# visualize the forecasts.

write_excel_csv(hc_forecast_clean, "data/forecasts_long.csv")

# Estimate when each department surpasses the MOU estimate ----------------

costs_forecastacutal <- bind_rows(
  historical_costs,
  hc_forecast_clean
) %>%
  mutate(
    fy = to_fy(date),
    month = month(date, label = TRUE)
  ) %>%
  filter(fy == max_fy) %>%
  select(department, date, month, cost)


# We use simple cumulative sums to estimate when
mou_exceed <- costs_forecastacutal %>%
  left_join(mou_values, by = "department") %>% 
  arrange(department, date) %>% 
  group_by(department) %>%
  mutate(
    cum_cost = cumsum(cost),
    over_mou = cum_cost > mou
  ) %>%
  filter(over_mou) %>%
  summarize(mou_over_month = format(first(date), "%b %Y"))


# Create output table -----------------------------------------------------

# This takes the historical data and the forecast data for the fiscal year
# and combines them together in a WIDE format that plays nicely in Excel.
# The fill written at the end of the chunk is what gets cut and paste into
# the hidden `forecasts_raw` tab in the forecasting spreadsheet.

costs_forecastacutal %>%
  pivot_wider(
    id_cols = department,
    names_from = month,
    values_from = cost
  ) %>%
  left_join(mou_exceed, by = "department") %>% 
  write_excel_csv("output/forecasts_wide.csv", na = "")
