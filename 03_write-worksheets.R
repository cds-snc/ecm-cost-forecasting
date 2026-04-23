library(openxlsx)


forecasting_workbook_path <- file.path(
  Sys.getenv("OneDrive"),
  "Digital Credentials - 803 - ECM Operations",
  "administration",
  "reports",
  "forecasting",
  "2026-27 ECM Monthly Costs Forecast.xlsx"
)

if (!file.exists(forecasting_workbook_path)) {
  stop("Can't find the forecasting workbook!\nExpected path:\n", forecasting_workbook_path)
}

# widen_timeseries --------------------------------------------------------

# This function takes a timeseries dataframe in the typical "long" format and
# widens it into a pretty format that is typical in ECM spreadsheets. It
# tries to keep the right rows and columns, even if there are no values to
# fill in for a given month or department

widen_timeseries <- function(df, index, key, value, range) {
  
  stopifnot(length(range) == 2)
  stopifnot(range[[1]] <= range[[2]])
  stopifnot("Date" %in% class(range[[1]]))
  stopifnot("Date" %in% class(range[[2]]))
  
  # Every month label
  month_levels <- format(seq.Date(range[[1]], range[[2]], by = "month"), "%b %Y")
  
  
  df |>
    arrange({{ index }}) |>
    mutate({{ key }} := as.factor({{ key }})) |>
    filter(between({{ index }}, range[[1]], range[[2]])) |>
    mutate(
      {{ index }} := factor(
        format({{ index }}, "%b %Y"),
        levels = month_levels
      )
    ) |>
    pivot_wider(
      id_cols = {{ key }},
      id_expand = TRUE,
      names_from = {{ index }},
      values_from = {{ value }},
      names_expand = TRUE
    )
}

# Set up the styles and texts of our workbook -----------------------------

wb <- openxlsx::loadWorkbook(forecasting_workbook_path)

# Create styles and headers

title_style <- createStyle(
  fontSize = 20,
  textDecoration = c("bold")
)

subheader_style <- createStyle(
  textDecoration = c("bold"),
)

warning_style <- createStyle(
  textDecoration = c("bold"),
  fontColour = "#960000"
)

warning_text_l1 <- "Do NOT edit this worksheet by hand! It is managed by an external script. // Ne modifiez PAS cette feuille de calcul manuellement ! Elle est gérée par un script externe."
warning_text_l2 <- "Any changes you make will be overwritten when this sheet is updated. // Toute modification que vous apporterez sera écrasée lors de la mise à jour de cette feuille de calcul."


# Write last year's actuals -----------------------------------------------

last_fy_sheet_name <- "Active Users (Last Year)"

last_year_actuals <- historical_active_users %>%
  widen_timeseries(
    index = date,
    key = acronym_bi,
    value = active_users,
    range = last_fy
  )

# Clear the sheet
deleteData(
  wb, 
  sheet = last_fy_sheet_name, 
  cols = 1:1024, 
  rows = 1:1024, 
  gridExpand = TRUE
)

# Write and format the sheet title
writeData(
  wb, 
  sheet = last_fy_sheet_name,
  c(paste0("FY", last_fy_str, " Active Users (Actual)"),
    "",
    warning_text_l1,
    warning_text_l2),
  startRow = 1, startCol = 1
)

addStyle(
  wb, last_fy_sheet_name,
  title_style,
  rows = 1, cols = 1
)


# Write and format warning at the top of the spreadsheet
writeData(
  wb, last_fy_sheet_name,
  last_year_actuals,
  startRow = 6, startCol = 1
)

addStyle(
  wb, last_fy_sheet_name,
  warning_style,
  rows = 3:4, cols = 1
)


# Write this year's actuals -----------------------------------------------

current_fy_sheet_name <- "Active Users (This Year)"

current_year_actuals <- historical_active_users %>%
  widen_timeseries(
    index = date,
    key = acronym_bi,
    value = active_users,
    range = current_fy
  )


# Clear the sheet
deleteData(
  wb, 
  sheet = current_fy_sheet_name, 
  cols = 1:1024, 
  rows = 1:1024, 
  gridExpand = TRUE
)

# Write and format the sheet title

writeData(
  wb, current_fy_sheet_name,
  c(paste0("FY", current_fy_str, " Active Users (Actual)"),
    "",
    warning_text_l1,
    warning_text_l2),
  startRow = 1, startCol = 1
)

addStyle(
  wb, current_fy_sheet_name,
  title_style,
  rows = 1, cols = 1
)


# Write and format warning at the top of the spreadsheet
writeData(
  wb, current_fy_sheet_name,
  current_year_actuals,
  startRow = 6, startCol = 1
)

addStyle(
  wb, current_fy_sheet_name,
  warning_style,
  rows = 3:4, cols = 1
)


# Write this year's forecast ----------------------------------------------

current_fy_forecast_sheet_name <- "Active Users (Forecasts)"

current_year_forecast <- hau_forecast_clean %>%
  widen_timeseries(
    index = date,
    key = acronym_bi,
    value = active_users,
    range = current_fy
  )


# Clear the sheet
deleteData(
  wb, 
  sheet = current_fy_forecast_sheet_name, 
  cols = 1:1024, 
  rows = 1:1024, 
  gridExpand = TRUE
)

# Write and format the sheet title

writeData(
  wb, current_fy_forecast_sheet_name,
  c(paste0("FY", current_fy_str, " Active Users (Forecast)"),
    "",
    warning_text_l1,
    warning_text_l2),
  startRow = 1, startCol = 1
)

addStyle(
  wb, current_fy_forecast_sheet_name,
  title_style,
  rows = 1, cols = 1
)


# Write and format warning at the top of the spreadsheet
writeData(
  wb, current_fy_forecast_sheet_name,
  current_year_forecast,
  startRow = 6, startCol = 1
)

addStyle(
  wb, current_fy_forecast_sheet_name,
  warning_style,
  rows = 3:4, cols = 1
)


# Write MFA variable costs (actuals and forecasts) ------------------------

mfa_sheet_name <- "MFA Variable Costs"

mfa_costs_actual <- costs |>
  widen_timeseries(
    index = date,
    key = acronym_bi,
    value = amount,
    range = list(last_fy[["start"]], current_fy[["end"]])
  )

mfa_costs_forecast <- costs_forecast_clean |>
  widen_timeseries(
    index = date,
    key = acronym_bi,
    value = amount,
    range = list(last_fy[["start"]], current_fy[["end"]])
  )


# Clear the sheet
deleteData(
  wb, 
  sheet = mfa_sheet_name, 
  cols = 1:1024, 
  rows = 1:1024, 
  gridExpand = TRUE
)


# Write the header of the actuals table
writeData(
  wb, 
  sheet = mfa_sheet_name,
  c("MFA Costs",
    "",
    warning_text_l1,
    warning_text_l2,
    "",
    "MFA Costs (Actuals)"
    ),
  startRow = 1, startCol = 1
)

actuals_start_row <- 7

# Write the MFA actuals data
writeData(
  wb, 
  sheet = mfa_sheet_name,
  mfa_costs_actual,
  startRow = actuals_start_row, startCol = 1
)

# The range of the actuals table so we can format them with dollar signs
actuals_data_rows <- (actuals_start_row + 1):(actuals_start_row + nrow(mfa_costs_actual))
actuals_data_cols <- 2:ncol(mfa_costs_actual)



forecast_start_row <- (nrow(mfa_costs_actual) + 1) + actuals_start_row + 1 

writeData(
  wb, 
  sheet = mfa_sheet_name,
  c("MFA Costs (Forecast)"),
  startRow = forecast_start_row , startCol = 1
)

writeData(
  wb, 
  sheet = mfa_sheet_name,
  mfa_costs_forecast,
  startRow = forecast_start_row + 1, startCol = 1
)

# The range of the forecast table so we can format them with dollar signs
forecasts_data_rows <- (forecast_start_row + 2):(forecast_start_row + nrow(mfa_costs_forecast) + 1)
forecasts_data_cols <- 2:ncol(mfa_costs_forecast)




# Format the title
addStyle(
  wb, mfa_sheet_name,
  title_style,
  rows = 1, cols = 1
)

# Format the warning and the sub-headings
addStyle(
  wb, mfa_sheet_name,
  warning_style,
  rows = 3:4, cols = 1
)

addStyle(
  wb, mfa_sheet_name,
  subheader_style,
  rows = c(actuals_start_row-1, forecast_start_row), cols = 1
)

# Format the values into dollars
addStyle(
  wb,
  sheet = mfa_sheet_name,
  style = createStyle(numFmt = "$#,##0.00"),
  rows = actuals_data_rows,
  cols = actuals_data_cols,
  gridExpand = TRUE
)

addStyle(
  wb,
  sheet = mfa_sheet_name,
  style = createStyle(numFmt = "$#,##0.00"),
  rows = forecasts_data_rows,
  cols = forecasts_data_cols,
  gridExpand = TRUE
)


saveWorkbook(wb, forecasting_workbook_path, overwrite = TRUE)