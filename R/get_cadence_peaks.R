#' Calculate cadence peak metrics
#'
#' @description
#' Cadence peaks represent the mean steps-per-minute (spm) achieved during
#' the highest-activity minutes of a day. For example, the 30-minute cadence
#' peak is the average cadence across the 30 highest cadence minutes of that
#' day, regardless of whether they occur consecutively.
#'
#' This function calculates cadence peaks for user-specified intervals and
#' also reports how many of the minutes within each interval contain zero
#' steps (useful for quality checks).
#'
#' @param x Numeric vector of cadence values (steps per minute) for the day
#'   of interest. Each element should represent one minute.
#' @param peaks Numeric vector of integers specifying which cadence peaks to
#'   compute. For example, `c(1, 30, 60)` (default) produces the 1-minute,
#'   30-minute, and 60-minute cadence peaks.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{values}{Numeric vector with cadence peak values (spm) followed by
#'   the corresponding counts of zero-minute values within each peak window.}
#'   \item{names}{Character vector of variable names corresponding to the
#'   `values`, in the format:
#'   \itemize{
#'     \item `"CAD_pk<peak>_spm"` for the cadence peak value
#'     \item `"CAD_nZeroes_pk<peak>"` for the number of zero minutes
#'   }}
#' }
#'
#' @details
#' Cadence peaks are calculated by:
#' \enumerate{
#'   \item Sorting all minute-level cadence values in descending order.
#'   \item Selecting the top *n* minutes, where *n* = peak length.
#'   \item Averaging those values to compute the cadence peak.
#'   \item Counting how many of those top *n* minutes contain zero steps.
#' }
#'
#' @examples
#' # Simulate one day of cadence values (1440 minutes)
#' set.seed(123)
#' cad <- sample(0:150, size = 1440, replace = TRUE)
#'
#' # Calculate 1-min, 30-min, and 60-min cadence peaks
#' get_cadence_peaks(cad, peaks = c(1, 30, 60))
#'
#' @seealso [get_cadence_bands()]
#' @export
get_cadence_peaks = function(x, peaks = c(1, 30, 60)) {
  cadence_peaks = c()
  nZeroes = c()
  names = c()
  for (i in 1:length(peaks)) {
    peak = peaks[i]
    peak_interval = sort(x, decreasing = TRUE)[1:peak]
    cadence_peaks[i] = mean(peak_interval)
    nZeroes[i] = length(which(peak_interval == 0))
  }
  names = c(paste0("CAD_pk", peaks, "_spm"), paste0("CAD_nZeroes_pk", peaks))

  # return
  return(list(values = c(cadence_peaks, nZeroes), names = names))
}
