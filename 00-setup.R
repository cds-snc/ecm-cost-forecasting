library(lubridate)

# Constants ---------------------------------------------------------------

# Some departments are "internal" and we don't use them in calculations
ignored_gcids <- c(
  -1,   # Government of Canada Credential Federation Consolidator
  0    # Others 
)


# Set up the database -----------------------------------------------------

# This is the path to the ECM reporting database. In order to connect to this
# database, you must be connected to the ESDC VPN. You must also set the
# username/password secrets ahead of time. Manage your secrets with:

#keyring::key_set("ECM-DB-READ-USER", prompt = "Enter ECM Reporting Database read-only username:")  # Enter the DB username
#keyring::key_set("ECM-DB-READ-PASS", prompt = "Enter ECM Reporting Database read-only password:")  # Enter the DB password

if (!exists("vendor_data")) {
  
  vendor_data <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "MLCNESD4150.hrdc-drhc.net\\MLDB16SA01",
    Database = "VendorData",
    UID = keyring::key_get("ECM-DB-READ-USER"),
    PWD = keyring::key_get("ECM-DB-READ-PASS"),
  )
  
}



# Figure out some key timeframes ------------------------------------------

# The last FY is the last COMPLETE fiscal year
last_fy <- list()
last_fy[["start"]] <- make_date(
  year = year(today()) - ifelse(month(today()) >= 4, 1, 2), 
  month = 4, 
  day = 1)
last_fy[["end"]] <- last_fy[[1]] + years(1) - days(1)
last_fy_str <- paste0(format(last_fy[["start"]], "%Y"), "-", format(last_fy[["end"]], "%y"))

current_fy <- purrr::map(last_fy, ~ . + years(1))
current_fy_str <- paste0(format(current_fy[["start"]], "%Y"), "-", format(current_fy[["end"]], "%y"))