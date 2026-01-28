# Converts a date to a fiscal year, in the format "FY20xx-yy"

to_fy <- function(ds) {
  
  dates <- as_date(ds)
  years <- year(dates)
  offsets <- ifelse(month(dates) <= 3, -1, 0)
  
  fy_start <- years + offsets
  fy_end <- fy_start + 1 - 2000
  
  paste0("FY", fy_start, "-", fy_end)
  
}
