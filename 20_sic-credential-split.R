# Monthly active credentials for former Sign-in Canada partners.
# Run independently of steps 10-12; this uses CanadaLogin data in Athena,
# not the ECM VendorData database.

library(dplyr)
library(tidyr)
library(dbplyr)
library(DBI)
library(lubridate)
library(openxlsx)


# Reporting period ----------------------------------------------------------

as_of <- floor_date(today(tzone = "America/Toronto"), "month") - days(1)
launch_date <- as.Date("2026-04-22")
first_month <- as.Date("2026-04-01")
last_month <- floor_date(as_of, "month")
output_dir <- "output"

reporting_months <- tibble(
  month = seq(first_month, last_month, by = "month"),
  window_start = month %m-% months(11)
)


# Partner applications ------------------------------------------------------

app_groups <- tribble(
  ~group,      ~application_name,
  "CGC",       "MyCGC - CGC - Power Pages - Prod",
  "CGC",       "MyCGC - CGC - .NET - Prod",
  "FCAC",      "FCAC Secure Portal - Prod",
  "Prairies",  "IRIS - PRC - Prod",
  "ATIP",      "ATIP Online Management Tool - Prod",
  "ATIP",      "ATIP Online Request Service - Prod",
  "Digital T", "GC Digital Talent - TBS - Prod",
  "Digital T", "GC Digital Talent - TBS - Pilot - Prod",
  "DEC",       "DEC-CED - Prod",
  "VAC",       "VAC Healthshare - VAC - Prod"
)
group_order <- unique(app_groups$group)


# Connect to Athena ---------------------------------------------------------

if (file.exists(".env")) {
  dotenv::load_dot_env()
}

noctua::noctua_options(verbose = FALSE)
athena <- dbConnect(
  noctua::athena(),
  profile_name = Sys.getenv("AWS_PROFILE"),
  region_name = Sys.getenv("AWS_REGION", "ca-central-1"),
  s3_staging_dir = Sys.getenv("ATHENA_S3_STAGING_DIR"),
  bigint = "numeric"
)


# Query active credentials --------------------------------------------------

# Compare zero-padded partition strings without casts so Athena prunes them.
on_or_after <- function(date) {
  y <- format(date, "%Y")
  m <- format(date, "%m")
  d <- format(date, "%d")
  rlang::expr(
    year > !!y | (year == !!y & month > !!m) |
      (year == !!y & month == !!m & day >= !!d)
  )
}

on_or_before <- function(date) {
  y <- format(date, "%Y")
  m <- format(date, "%m")
  d <- format(date, "%d")
  rlang::expr(
    year < !!y | (year == !!y & month < !!m) |
      (year == !!y & month == !!m & day <= !!d)
  )
}

query_start <- max(launch_date, min(reporting_months$window_start))
apps <- copy_inline(athena, app_groups)
windows <- copy_inline(athena, reporting_months)

# The next UTC partition holds the last ET evening. Deduplicate within each
# partner before joining the trailing windows; only counts leave Athena.
credentials <- tbl(
  athena, in_schema("ibm_verify_events_raw", "application_usage")
) |>
  filter(!!on_or_after(query_start), !!on_or_before(as_of + 1)) |>
  filter(
    result == "success",
    !is.na(userid), !userid %in% c("", "UNKNOWN")
  ) |>
  select(time, userid, application_name = applicationname) |>
  inner_join(apps, by = "application_name") |>
  mutate(et_month = sql(
    "date_trunc('month', date(with_timezone(CAST(substr(time, 1, 19) AS timestamp), 'UTC') AT TIME ZONE 'America/Toronto'))"
  )) |>
  filter(et_month <= as.Date(!!format(last_month))) |>
  distinct(userid, group, et_month) |>
  inner_join(windows, by = join_by(between(et_month, window_start, month))) |>
  group_by(group, month, window_start) |>
  summarise(active_credentials = n_distinct(userid), .groups = "drop") |>
  collect() |>
  mutate(month = as.Date(month), window_start = as.Date(window_start)) |>
  arrange(month, group)

dbDisconnect(athena)


# Arrange monthly counts ----------------------------------------------------

counts <- expand_grid(group = group_order, month = reporting_months$month) |>
  left_join(credentials, by = c("group", "month")) |>
  mutate(active_credentials = coalesce(active_credentials, 0))

wide <- counts |>
  mutate(month = factor(
    format(month, "%b %Y"),
    levels = format(reporting_months$month, "%b %Y")
  )) |>
  pivot_wider(
    id_cols = group, names_from = month, values_from = active_credentials
  ) |>
  rename(Partner = group)


# Write workbook ------------------------------------------------------------

notes <- c(
  "Active credentials for former Sign-in Canada partners",
  "",
  paste("Data as of:", format(as_of)),
  paste("Billing month:", format(last_month, "%B %Y")),
  "",
  paste(
    "Active credential: a CanadaLogin credential with at least one successful",
    "sign-in to one of a partner's applications in the 12 calendar months",
    "ending with the month shown. Months follow Eastern Time."
  ),
  paste(
    "Applications that belong together are grouped, so a credential counts",
    "once per partner. A credential used with two partners counts once for",
    "each."
  ),
  paste(
    "CanadaLogin launched on April 22, 2026 so windows",
    "ending before March 2027 hold less than 12 months of sign-ins."
  ),
  "",
  "Source: IBM Verify SSO events (ibm_verify_events_raw.application_usage).",
  "",
  "Applications in each partner group:"
)

wb <- createWorkbook()
bold <- createStyle(textDecoration = "bold")

addWorksheet(wb, "Notes")
writeData(wb, "Notes", notes)
writeData(
  wb, "Notes",
  rename(app_groups, Partner = group,
         `IBM Verify application` = application_name),
  startRow = length(notes) + 1, headerStyle = bold
)
setColWidths(wb, "Notes", cols = 1:2, widths = c(12, 45))

addWorksheet(wb, "Credentials")
writeData(wb, "Credentials", wide, headerStyle = bold)
addStyle(
  wb, "Credentials", createStyle(numFmt = "#,##0"),
  rows = seq_len(nrow(wide)) + 1, cols = 2:ncol(wide),
  gridExpand = TRUE
)
setColWidths(wb, "Credentials", cols = seq_len(ncol(wide)), widths = "auto")

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
workbook_path <- file.path(
  output_dir,
  paste0("sic-active-credentials_", format(last_month, "%Y-%m"), ".xlsx")
)
saveWorkbook(wb, workbook_path, overwrite = TRUE)
message("Wrote ", workbook_path)
