library(dplyr)
library(tidyr)
library(openxlsx)


# Workbook path -----------------------------------------------------------

forecasting_workbook_path <- file.path(
  Sys.getenv("OneDrive"),
  "Digital Credentials - 803 - ECM Operations",
  "administration",
  "reports",
  "forecasting",
  "2026-27 ECM Monthly Costs Forecast.xlsx"
)

if (!file.exists(forecasting_workbook_path)) {
  stop(
    "Can't find the forecasting workbook!\nExpected path:\n",
    forecasting_workbook_path
  )
}


# widen_timeseries --------------------------------------------------------

# This function takes a time-series dataframe in the typical long format and
# widens it into the format used in ECM spreadsheets.
#
# Departments in the_correct_order are shown first in the specified order.
# Other departments are appended alphabetically.
# Departments in to_omit are excluded entirely.

widen_timeseries <- function(df, index, key, value, range) {
  
  the_correct_order <- c(
    "CRA / ARC", "IRCC / IRCC", "ESDC / EDSC", "RCMP / GRC", "PC / PC",
    "ISED / ISDE", "CBSA / ASFC", "TC / TC", "VAC / ACC", "TBS / SCT",
    "NRCan / RNCan", "DND / MDN", "PCH / PCH", "DFO / MPO", "PSPC / SPAC",
    "CFIA / ACIA", "GAC / AMC", "ECCC / ECCC", "PS / SP", "HC / SC",
    "IRB / CISR", "AAFC / AAC", "CDC / CCL", "LAC / BAC", "CSC / SCC",
    "CRTC / CRTC", "ACOA / APECA", "SSC / SPC", "PSC / CFP", "CER / REC",
    "IAAC / AEIC", "StatCan / StatCan", "WD / DEO", "CNSC / CCSN",
    "JUS / JUS", "CGC / CCG", "ATSSC / SCDATA", "ISC / SAC", "WAGE / FEGC"
  )
  
  to_omit <- c(
    "Telefilm / Telefilm",
    "GCCFC / CFJGC"
  )
  
  stopifnot(
    length(range) == 2,
    range[[1]] <= range[[2]],
    inherits(range[[1]], "Date"),
    inherits(range[[2]], "Date")
  )
  
  month_levels <- format(
    seq.Date(range[[1]], range[[2]], by = "month"),
    "%b %Y"
  )
  
  result <- df |>
    filter(
      between({{ index }}, range[[1]], range[[2]]),
      !({{ key }} %in% to_omit)
    ) |>
    arrange({{ index }}) |>
    mutate(
      # Rebuild the factor to remove unused and omitted levels.
      {{ key }} := factor(as.character({{ key }})),
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
  
  key_name <- names(result)[1]
  
  present_values <- unique(as.character(result[[key_name]]))
  
  final_order <- c(
    the_correct_order[the_correct_order %in% present_values],
    sort(setdiff(present_values, the_correct_order))
  )
  
  result |>
    mutate(
      !!key_name := factor(
        .data[[key_name]],
        levels = final_order
      )
    ) |>
    arrange(.data[[key_name]])
}


# Load workbook -----------------------------------------------------------

wb <- loadWorkbook(forecasting_workbook_path)


# Styles and reusable text ------------------------------------------------

title_style <- createStyle(
  fontSize = 20,
  textDecoration = "bold"
)

description_style <- createStyle(
  textDecoration = "bold",
  fontColour = "#000000"
)

subheader_style <- createStyle(
  textDecoration = "bold"
)

warning_style <- createStyle(
  textDecoration = "bold",
  fontColour = "#960000"
)

currency_style <- createStyle(
  numFmt = "$#,##0.00"
)

warning_text_l1 <- paste0(
  "Do NOT edit this worksheet by hand! It is managed by an external script. ",
  "// Ne modifiez PAS cette feuille de calcul manuellement ! ",
  "Elle est gérée par un script externe."
)

warning_text_l2 <- paste0(
  "Any changes you make will be overwritten when this sheet is updated. ",
  "// Toute modification que vous apporterez sera écrasée lors de la mise ",
  "à jour de cette feuille de calcul."
)


# Workbook helper functions -----------------------------------------------

clear_sheet <- function(wb, sheet_name) {
  
  deleteData(
    wb,
    sheet = sheet_name,
    cols = 1:1024,
    rows = 1:1024,
    gridExpand = TRUE
  )
}


write_sheet_header <- function(
    wb,
    sheet_name,
    title,
    description = "placeholder text description, someone forgot to fill this in"
) {
  
  writeData(
    wb,
    sheet = sheet_name,
    x = title,
    startRow = 1,
    startCol = 1
  )
  
  writeData(
    wb,
    sheet = sheet_name,
    x = description,
    startRow = 2,
    startCol = 1
  )
  
  writeData(
    wb,
    sheet = sheet_name,
    x = c(warning_text_l1, warning_text_l2),
    startRow = 3,
    startCol = 1
  )
  
  addStyle(
    wb,
    sheet = sheet_name,
    style = title_style,
    rows = 1,
    cols = 1
  )
  
  addStyle(
    wb,
    sheet = sheet_name,
    style = description_style,
    rows = 2,
    cols = 1
  )
  
  addStyle(
    wb,
    sheet = sheet_name,
    style = warning_style,
    rows = 3:4,
    cols = 1
  )
}


# Write last year's actuals -----------------------------------------------

last_fy_sheet_name <- "Active Users (Last Year)"

last_year_actuals <- historical_active_users |>
  widen_timeseries(
    index = date,
    key = acronym_bi,
    value = active_users,
    range = last_fy
  )

clear_sheet(
  wb,
  last_fy_sheet_name
)

write_sheet_header(
  wb = wb,
  sheet_name = last_fy_sheet_name,
  title = paste0(
    "FY",
    last_fy_str,
    " Active Users (Actual)"
  ),
  description = paste0("This table shows the number of active users for each department for the PREVIOUS COMPLETE fiscal year (", last_fy_str, ").")
)

writeData(
  wb,
  sheet = last_fy_sheet_name,
  x = last_year_actuals,
  startRow = 6,
  startCol = 1
)


# Write this year's actuals -----------------------------------------------

current_fy_sheet_name <- "Active Users (This Year)"

current_year_actuals <- historical_active_users |>
  widen_timeseries(
    index = date,
    key = acronym_bi,
    value = active_users,
    range = current_fy
  )

clear_sheet(
  wb,
  current_fy_sheet_name
)

write_sheet_header(
  wb = wb,
  sheet_name = current_fy_sheet_name,
  title = paste0(
    "FY",
    current_fy_str,
    " Active Users (Actual)"
  ),
  description = paste0("This table shows the number of ACTUAL active users for each department in the current fiscal year (", current_fy_str, "). It only includes months that are concluded and for which reporting is complete.")
)

writeData(
  wb,
  sheet = current_fy_sheet_name,
  x = current_year_actuals,
  startRow = 6,
  startCol = 1
)


# Write this year's forecast ----------------------------------------------

current_fy_forecast_sheet_name <- "Active Users (Forecasts)"

current_year_forecast <- hau_forecast_clean |>
  widen_timeseries(
    index = date,
    key = acronym_bi,
    value = active_users,
    range = current_fy
  )

clear_sheet(
  wb,
  current_fy_forecast_sheet_name
)

write_sheet_header(
  wb = wb,
  sheet_name = current_fy_forecast_sheet_name,
  title = paste0(
    "FY",
    current_fy_str,
    " Active Users (Forecast)"
  ),
  description = paste0("This table shows the number of FORECASTED active users for each department for the remainder of the fiscal year (", current_fy_str, "). Completed months are not shown.")
)

writeData(
  wb,
  sheet = current_fy_forecast_sheet_name,
  x = current_year_forecast,
  startRow = 6,
  startCol = 1
)


# Write MFA variable costs ------------------------------------------------

mfa_sheet_name <- "MFA Variable Costs"

mfa_range <- list(
  last_fy[["start"]],
  current_fy[["end"]]
)

mfa_costs_actual <- costs |>
  widen_timeseries(
    index = date,
    key = acronym_bi,
    value = amount,
    range = mfa_range
  )

mfa_costs_forecast <- costs_forecast_clean |>
  widen_timeseries(
    index = date,
    key = acronym_bi,
    value = amount,
    range = mfa_range
  )

clear_sheet(
  wb,
  mfa_sheet_name
)

write_sheet_header(
  wb = wb,
  sheet_name = mfa_sheet_name,
  title = "MFA Costs",
  description = paste0("The sheet shows the historical MFA variable costs (top, left) and a forecast of future MFA costs (bottom, right).")
)


# MFA actual costs table --------------------------------------------------

actuals_subheader_row <- 6
actuals_start_row <- 7

writeData(
  wb,
  sheet = mfa_sheet_name,
  x = "MFA Costs (Actuals)",
  startRow = actuals_subheader_row,
  startCol = 1
)

writeData(
  wb,
  sheet = mfa_sheet_name,
  x = mfa_costs_actual,
  startRow = actuals_start_row,
  startCol = 1
)

actuals_data_rows <- seq.int(
  from = actuals_start_row + 1,
  length.out = nrow(mfa_costs_actual)
)

actuals_data_cols <- seq.int(
  from = 2,
  to = ncol(mfa_costs_actual)
)


# MFA forecast costs table ------------------------------------------------

forecast_subheader_row <- (
  actuals_start_row +
    nrow(mfa_costs_actual) +
    2
)

forecast_start_row <- forecast_subheader_row + 1

writeData(
  wb,
  sheet = mfa_sheet_name,
  x = "MFA Costs (Forecast)",
  startRow = forecast_subheader_row,
  startCol = 1
)

writeData(
  wb,
  sheet = mfa_sheet_name,
  x = mfa_costs_forecast,
  startRow = forecast_start_row,
  startCol = 1
)

forecasts_data_rows <- seq.int(
  from = forecast_start_row + 1,
  length.out = nrow(mfa_costs_forecast)
)

forecasts_data_cols <- seq.int(
  from = 2,
  to = ncol(mfa_costs_forecast)
)


# Format MFA subheaders and currency values -------------------------------

addStyle(
  wb,
  sheet = mfa_sheet_name,
  style = subheader_style,
  rows = c(
    actuals_subheader_row,
    forecast_subheader_row
  ),
  cols = 1
)

addStyle(
  wb,
  sheet = mfa_sheet_name,
  style = currency_style,
  rows = actuals_data_rows,
  cols = actuals_data_cols,
  gridExpand = TRUE
)

addStyle(
  wb,
  sheet = mfa_sheet_name,
  style = currency_style,
  rows = forecasts_data_rows,
  cols = forecasts_data_cols,
  gridExpand = TRUE
)


# Save the completed workbook --------------------------------------------

saveWorkbook(
  wb,
  forecasting_workbook_path,
  overwrite = TRUE
)