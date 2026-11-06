# ECM Partner Cost Forecasting

This repository contains scripts to clean and analyze data about historical
costs billed to External Credential Management partners. It uses those
historical costs to create forecasts of future costs, that can be compared
against MOU estimates to gauge whether a partner is at risk of exceeding their
budget.

It works in conjunction with [this spreadsheet](https://014gc.sharepoint.com/:x:/s/CDS-Migration-Take3/IQBpYMgrvABhSIWAEzHzUOIOARrWvxhZqq_l9yfcYhplkz8?e=H0xPnN).

## Setup

Run scripts from the repository root. Restore R packages with
`renv::restore()`.

## Partner cost forecasts

Run these scripts in order in the same R session:

1. [00-setup.R](00-setup.R): VendorData connection and fiscal-year dates
   (also sourced by steps 10 and 11). Requires the ESDC VPN and the database
   credentials described in the script.
2. [10_forecast-users.R](10_forecast-users.R): active-user forecasts.
3. [11_forecast-mfa-costs.R](11_forecast-mfa-costs.R): MFA cost forecasts.
4. [12_write-worksheets.R](12_write-worksheets.R): updates the shared
   forecasting workbook in OneDrive.

## Former SIC partners: active credentials

[20_sic-credential-split.R](20_sic-credential-split.R) independently queries
Athena for each former SIC partner's active credentials over trailing
12-calendar-month windows, through the last completed month. Set
`AWS_PROFILE` and `ATHENA_S3_STAGING_DIR` in your environment or a local
`.env`, sign in with AWS SSO, then run `Rscript 20_sic-credential-split.R`.
It writes only `output/sic-active-credentials_<YYYY-MM>.xlsx`, with monthly
counts and method notes; it does not modify the OneDrive workbook.
