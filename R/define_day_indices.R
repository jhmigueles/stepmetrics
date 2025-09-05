#' Define day indices from ISO 8601 timestamps
#'
#' @param ts Character vector of ISO 8601 timestamps (e.g., "2024-06-26T23:45:00+0100")
#' @return Numeric vector with day indices
#' @export
define_day_indices <- function(ts) {
  # Convert to Date from ISO 8601 (safe even with time zones)
  ts = as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%S%z")
  dates = as.Date(ts, tz = "")

  # Assign a unique day index for each date
  day_indices = as.integer(factor(dates))

  return(day_indices)
}
