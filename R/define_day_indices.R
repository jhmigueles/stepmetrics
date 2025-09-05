#' Generate sequential day indices from ISO 8601 timestamps
#'
#' @description
#' Converts a vector of ISO 8601 timestamps into sequential day indices
#' (1, 2, 3, …), where each unique calendar date corresponds to a unique
#' integer. This is useful for looping over or summarizing data by day
#' when working with minute-level time series from wearables.
#'
#' @param ts Character vector of ISO 8601 timestamps
#'   (e.g., `"2024-06-26T23:45:00+0100"`). Time zone offsets are handled
#'   correctly when converting to `Date`.
#'
#' @return An integer vector of the same length as `ts`, where each element
#'   gives the index of the day the timestamp belongs to. The first unique
#'   date encountered is assigned index `1`, the second `2`, and so on.
#'
#' @examples
#' ts <- c("2024-06-26T23:45:00+0100",
#'         "2024-06-27T00:15:00+0100",
#'         "2024-06-27T14:30:00+0100")
#' define_day_indices(ts)
#' # Returns: c(1, 2, 2)
#'
#' @seealso [as.POSIXct()], [as.Date()]
#' @export
define_day_indices <- function(ts) {
  # Convert to Date from ISO 8601 (safe even with time zones)
  ts = as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%S%z")
  dates = as.Date(ts, tz = "")

  # Assign a unique day index for each date
  day_indices = as.integer(factor(dates))

  return(day_indices)
}
