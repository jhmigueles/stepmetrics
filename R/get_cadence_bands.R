#' Function to calculate cadence bands
#'
#' @param x Numeric vector (no default) with cadence data for the day of interest.
#' @param bands Numeric vector (default = c(0, 1, 20, 40, 60, 80, 100, 120, Inf)) with the bands of interest to calculate.
#'
#' @return Cadence bands and names of variables.
#' @export
#'
#' @examples
#' \dontrun{
#' get_cadence_bands(x, peaks = c(1, 30, 60))
#' }
get_cadence_bands = function(x, bands = c(0, 1, 20, 40, 60, 80, 100, 120, Inf)) {

  cadence_bands = table(cut(x, breaks = bands, right = FALSE))
  band_names = c()
  for (i in 1:(length(bands) - 1)) {
    band_names[i] = paste(bands[i], bands[i + 1] - 1, sep = "_")
    if (bands[i] == bands[i + 1] - 1) band_names[i] = as.character(bands[i])
  }
  names = c(paste0("CAD_band_", band_names, "_spm"))

  # return
  return(list(values = cadence_bands, names = names))
}
