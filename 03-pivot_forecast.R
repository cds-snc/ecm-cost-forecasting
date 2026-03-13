# This script takes forecast and historical data and turns them into the
# pleasant, human-readable table that people like to see in Excel sheets.
# The format of the output as:

# DEPARTMENT | April | May | June | ... | March | mou_over_month
# where `mou_over_month` is the month the forecast exceeds the department's 
# MOU estimate for costs

library(tidyverse)
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

# First, we read in the names of GC Departments so we can merge correctly
gc_departments <- DBI::dbReadTable(con, "gc_departments") %>%
  mutate(department = paste0(acronym_en, " / ", acronym_fr)) %>%
  select(gcid_fk = id, department)


historical_costs_raw <- dbReadTable(con, "costs_historical")
forecasted_costs_raw <- dbReadTable(con, "costs_forecast")
mou_values_raw <- read_csv("data/mou_values.csv", show_col_types = FALSE)


# Combine data ------------------------------------------------------------

mou_values <- mou_values_raw %>%
  left_join(gc_departments, by = "department")

historical_costs <- historical_costs_raw %>%
  select(gcid_fk, date, cost)

forecasted_costs <- forecasted_costs_raw %>%
  filter(forecasted_on == max(forecasted_on)) %>%
  select(gcid_fk, date = forecast_date, cost = forecasted_cost)

combined_costs <- bind_rows(
  historical_costs,
  forecasted_costs
) %>%
  mutate(date = as_date(date)) %>%
  left_join(gc_departments, by = "gcid_fk") %>%
  filter(to_fy(date) == to_fy(max(historical_costs$date)))  # Only the current fiscal year



# We use simple cumulative sums to estimate when the forecast exceeds the MOU estimate
mou_exceed <- combined_costs %>%
  left_join(mou_values, by = c("gcid_fk", "department")) %>% 
  arrange(department, date) %>% 
  group_by(department) %>%
  mutate(
    cum_cost = cumsum(cost),
    over_mou = cum_cost > mou
  ) %>% 
  filter(over_mou) %>% 
  summarize(mou_over_month = format(first(date), "%b %d %Y"))


# Create output table -----------------------------------------------------

# This takes the historical data and the forecast data for the fiscal year
# and combines them together in a WIDE format that plays nicely in Excel.
# The fill written at the end of the chunk is what gets cut and paste into
# the hidden `forecasts_raw` tab in the forecasting spreadsheet.

output <- combined_costs %>%
  left_join(mou_exceed, by = "department") %>%
  mutate(month = format(date, "%B")) %>%
  pivot_wider(
    id_cols = department,
    names_from = month,
    values_from = cost
  ) %>%
  left_join(mou_exceed, by = "department") %>%
  arrange(-April)


write_excel_csv(output, format(Sys.Date(), "output/%Y%m-forecasts_wide.csv"), na = "")
write_excel_csv(output, "output/forecasts_wide.csv", na = "")

