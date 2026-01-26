library(readxl)
library(tidyverse)
library(tsibble)
source("useful-functions.R")

# Parameters --------------------------------------------------------------

# The path to the file that contains historical cost info
filepath <- file.path(
  Sys.getenv("OneDrive"),
  "Digital Credentials - 803 - ECM Operations",
  "administration",
  "reports",
  "forecasting",
  "2025-26 ECM Monthly Costs Forecast.xlsx"
  )

stopifnot(file.exists(filepath))

# This is the Excel range of the data on the "Historical" tab of the spread
# that contains the cost charged to each department every month. It assumes
# that the name of the department is in the first column, and every additional
# column is a month (i.e., the table is WIDE)

# This range is intentionally too big in order to capture any additional
# data added to the range. Later in the script, we drop fully empty rows
# or columns.
historical_range <- "A44:AZ999"

# Read in the spreadsheet -------------------------------------------------

historical_costs_raw <- 
  read_excel(
    filepath, 
    sheet = "Historical",
    range = historical_range
  ) 

# The string "Total Costs" should be in the first cell in the range. If this
# cell is not "Total Costs", chances are something is wrong with the
# `historical_range` variable or the spreadsheet, and an analyst should confirm 
# that it is set correctly.
stopifnot(names(historical_costs_raw)[[1]] == "Total Costs")

# Drop empty rows
historical_costs_raw <- historical_costs_raw[
  rowSums(is.na(historical_costs_raw)) != ncol(historical_costs_raw), 
  ]

# Drop empty cols
historical_costs_raw <- historical_costs_raw[, 
  colSums(is.na(historical_costs_raw)) != nrow(historical_costs_raw) 
]

# Pivot the dataframe into a longer format
# Add some date parameters that make filtering and forecasting easier
# And convert Excel date ints into proper date format
historical_costs <- historical_costs_raw %>%
  rename(department = 1) %>%
  pivot_longer(
    cols = c(everything(), -department),
    names_to = "date",
    values_to = "cost"
  ) %>%
  mutate(
    date = as.Date(as.numeric(date), origin = "1899-12-30"),
    month = month(date, label = TRUE),
    year = year(date),
    fy = to_fy(date)
  ) %>%
  filter(date >= as_date("2023-04-01"))


# Read in the costs data, we will extract the MOU values
mou_values_raw <- read_excel(filepath, sheet = "Financials")

# Which column contains the "MOUs" header - this is generalized
# in case someone decides to rename the column
mou_column <- which(str_detect(names(mou_values_raw), "MOU"))

# There should only be one MOU column - if there isn't, something's wrong
# and this process requires manually intervention.

stopifnot(length(mou_column) == 1)

# Pull the columns out and clean up the dataset
mou_values <- mou_values_raw %>%
  select(
    department = 1,
    mou = all_of(mou_column)
  ) %>%
  filter(
    !is.na(department),
    department != "Totals"
  )

# Confirm that there are MOUs for every department in the 'historical' tab
stopifnot(nrow(mou_values) == length(unique(historical_costs$department)))


# Write the data ----------------------------------------------------------

dir.create("data", showWarnings = FALSE, recursive = TRUE)

write_excel_csv(historical_costs, file = format(Sys.Date(), "data/%Y%m-historical_costs.csv"))
write_excel_csv(mou_values,       file = format(Sys.Date(), "data/%Y%m-mou_values.csv"))
write_excel_csv(historical_costs, file = "data/historical_costs.csv") 
write_excel_csv(mou_values,       file = "data/mou_values.csv")
