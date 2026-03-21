# This script reads cost data from the ECM CyberAuth cost reports on the ECM
# Sharepoint site. You can find these reports here:
# https://014gc.sharepoint.com/:f:/r/sites/CDS-Migration-Take3/Shared%20Documents/803%20-%20ECM%20Operations/administration/reports/monthly%20reports/ECM%20Financial%20Report%20-%20CyberAuth%20%26%20MFA/Primary%20Copy?csf=1&web=1&e=8tG3w0

# The Excel sheets are meant to be human-readable, but that means that they
# are tricky to read programmatically. That means this script is _very fragile_
# and assumes that the report files don't change their format (i.e., the columns
# aren't re-ordered or renamed). This is probably not a good assumption! If
# you're running this script, you should confirm that the table 
# `historical_costs_final` looks roughly correctly before you write it to the DB.

# After some cleanup, this script will write the resulting data to the ECM
# reporting database, in `VendorData`, as the table `costs_historical`. This
# operation only appends data - it will not overwrite previously written data
# You cannot access this database without being connected to the ESDC VPN

library(readxl)
library(tidyverse)
library(tsibble)
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


# File path parameters ----------------------------------------------------

# The path to the folder that contains monthly cost reports for ECM
# You must have this folder synced via OneDrive
filepath <- file.path(
  Sys.getenv("OneDrive"),
  "Digital Credentials - 803 - ECM Operations",
  "administration",
  "reports",
  "monthly reports",
  "ECM Financial Report - CyberAuth & MFA",
  "Primary Copy"
)

# Get a list of all of the files that match the reporting name pattern.
# For example: 2025-12 - ECM Financial Report - Rapport Financier GJE - CyberAuth - MFA.xlsx
report_paths <- list.files(
  path = filepath,
  pattern = "\\d{4}-\\d{2}.*(ECM Financial Report).*\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

# We assume that sorting them alphabetically will put the most recent report
# at the end. We can't use the file's mtime, it will fail if someone edits
# a past file.
most_recent_report_path <- last(sort(report_paths))
stopifnot(file.exists(most_recent_report_path))

# We have to infer the year for the dates in the spreadsheet, because they
# are not explicitly recorded. We will do that based on the filename
# This is the FIRST YEAR in the fiscal year span (i.e., 2025 for FY25-26)
spreadsheet_date <- str_extract(basename(most_recent_report_path), "^\\d{4}\\-\\d{2}")
spreadsheet_month <- as.numeric(str_extract(spreadsheet_date, "\\d{2}$"))
spreadsheet_year <- as.numeric(str_extract(spreadsheet_date, "^\\d{4}")) - (spreadsheet_month <= 3)  # Account for FY


# This is the Excel range of the data on the "Historical" tab of the spread
# that contains the cost charged to each department every month. It assumes
# that the name of the department is in the first column, and every additional
# column is a month (i.e., the table is WIDE)

# This range is intentionally too big in order to capture any additional
# data added to the range. Later in the script, we drop fully empty rows
# or columns.
historical_range <- "A11:AZ999"


# Read in the spreadsheet -------------------------------------------------

# There are always two sheets in each report file:
#  * One called "Service Costs" (we don't want it)
#  * One that is the reports date (e.g., Dec-2025)
# We can't trust that people will name the dated sheet correctly, so we will
# instead select the sheet that _isn't_ named "Service Cost"

historical_costs_sheet <- setdiff(
    excel_sheets(most_recent_report_path),
    "Service Cost"
)

historical_costs_raw <- 
  read_excel(
    most_recent_report_path, 
    sheet = historical_costs_sheet,
    range = historical_range
  ) 


# Extract historical costs from the spreadsheet ---------------------------

historical_costs <- historical_costs_raw

# Drop empty rows
historical_costs <- historical_costs[
  rowSums(is.na(historical_costs)) != ncol(historical_costs), 
]

# Drop empty cols
historical_costs <- historical_costs[, 
  colSums(is.na(historical_costs) | historical_costs == "-") != nrow(historical_costs) 
]

# Fix the columns that are named using bilingual month format
new_colnames <- colnames(historical_costs) %>%
  str_extract("^[0-9A-Za-z ]+")  # Extract characters and stop at non-space whitespace

new_colnames[is.na(new_colnames)] <- colnames(historical_costs)[is.na(new_colnames)]
colnames(historical_costs) <- new_colnames


# Pivot the dataframe into a longer format
# Add some date parameters that make filtering and forecasting easier
# And convert Excel date ints into proper date format
historical_costs_final <- historical_costs %>%
  rename(department = 1) %>%
  select(
    department,
    any_of(c(
      "April", "May", "June", "July", "August", "September", "October", 
      "November", "December", "January", "February", "March"))
  ) %>%
  pivot_longer(
    cols = c(everything(), -department),
    names_to = "month",
    values_to = "cost"
  ) %>%
  mutate(
    year = ifelse(month %in% c("January", "February", "March"), spreadsheet_year + 1, spreadsheet_year),
    date = as_date(paste0(year, "-", month, "-01")),
    fy = to_fy(date)
  ) %>%
  filter(department != "Totals")


# Extract MOU amounts from the spreadsheet --------------------------------

mou_values <- historical_costs_raw %>%
  select(
    department = 2,
    mou = 18
  ) %>%
  filter(
    !is.na(department),
    department != "Totals"
  )

# Confirm that there are MOUs for every department in the 'historical' tab
stopifnot(nrow(mou_values) == length(unique(historical_costs_final$department)))



# Write the data locally --------------------------------------------------

dir.create("data", showWarnings = FALSE, recursive = TRUE)

write_excel_csv(historical_costs_final, file = format(Sys.Date(), "data/%Y%m-historical_costs.csv"))
write_excel_csv(mou_values,             file = format(Sys.Date(), "data/%Y%m-mou_values.csv"))
write_excel_csv(historical_costs_final, file = "data/historical_costs.csv") 
write_excel_csv(mou_values,             file = "data/mou_values.csv")


# Write the data to the database ------------------------------------------

# We need the table of GC organisations so we can link the foreign keys correctly
# In most places, they are referenced as "en_acronym / fr_acronym", we have to
# create that so we can join correctly
gc_departments <- DBI::dbReadTable(con, "gc_departments") %>%
  mutate(acronym = paste0(acronym_en, " / ", acronym_fr)) %>%
  select(id, acronym)

# We should be able to find a gcid for every acronym
stopifnot(sum(is.na(gc_departments$id)) == 0)

# Check if the table exists - if it doesn't, create it
# This is written in SQL Server dialect - may have to change if DB changes

if (!DBI::dbExistsTable(con, "costs_historical")) {
  # To create tables, you must be logged in as the admin user
  # The read/write user does not have sufficient privileges
  
  DBI::dbExecute(con, 
    "
    CREATE TABLE dbo.costs_historical (
        id       INT IDENTITY(1,1) NOT NULL,
        gcid_fk  INT               NOT NULL,
        [date]   DATE              NOT NULL,
        cost     DECIMAL(19,4)     NOT NULL,
        CONSTRAINT PK_costs_historical PRIMARY KEY CLUSTERED (id),
        CONSTRAINT FK_costs_historical_gc_departments
            FOREIGN KEY (gcid_fk)
            REFERENCES dbo.gc_departments (id)
    );
    "
  )

} 

# Get the most recent value in the table - we only want to add new rows
# We don't want to modify old rows
most_recent <- tbl(con, "costs_historical") %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  select(date) %>%
  collect() %>%
  pull(1) %>%
  unique() 

# If there is no recent data in the database, use a date far in the past
most_recent <- max(c(
  as_date(most_recent),
  as_date("1970-01-01")
  ))

# Create the rows that we are going to append to the end of the table
to_append <- historical_costs_final %>%
  left_join(gc_departments, by = c("department" = "acronym")) %>%
  select(
    gcid_fk = id,
    date,
    cost
  ) %>%
  arrange(date, gcid_fk) %>%
  replace_na(list(costs = 0))

to_append <- filter(to_append, date > most_recent)

# Do some sanity checks here
stopifnot(nrow(to_append) > 0)
stopifnot(all(is.numeric(to_append$cost)))
stopifnot(abs(as.numeric(today() - most_recent)) < 95)  # We shouldn't be writing data older than 95 days
stopifnot(max(to_append$date) < today())  # Shouldn't be writing future data

# If we pass all of the sanity checks, write to the table
DBI::dbAppendTable(con, "costs_historical", value = to_append)
