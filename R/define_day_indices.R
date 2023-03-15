#' Define day indices from timestamp
#'
#' @param ts Character (no default) with timestamps from data.
#'
#' @return Numeric vector with day indices
#' @export
#'
#' @examples
#' \dontrun{
#' data = readFile("mydata/s01_fitbit.csv")
#' define_day_indices(data$timestamp)
#' }
define_day_indices = function(ts) {
  mnightsi = grep("00:00:00", ts, fixed = T)
  day = rep(NA, times = length(ts))
  d = 1
  # Loop through the rest of the days
  for (j in 1:length(mnightsi)) {
    if (j < length(mnightsi)) {
      if (j == 1 & mnightsi[j] > 1) {
        day[1:mnightsi[j]] = d
      } else if (j == 1 & mnightsi[j] == 1) {
        next()
      } else {
        day[mnightsi[j - 1]:mnightsi[j]] = d
      }
      d = d + 1
    } else {
      day[mnightsi[j - 1]:mnightsi[j]] = d; d = d + 1
      day[mnightsi[j]:length(day)] = d
    }
  }
  return(day)
}
