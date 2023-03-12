#' Function to calculate cadence peaks
#'
#' @param x Numeric vector (no default) with cadence data for the day of interest.
#' @param peaks Numeric vector (default = c(1, 30, 60)) with the peaks of interest to calculate.
#'
#' @return Cadence peaks, number of zeroes in each peak, names of variables.
#' @export
#'
#' @examples
#' \dontrun{
#' get_cadence_peaks(x, peaks = c(1, 30, 60))
#' }
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
