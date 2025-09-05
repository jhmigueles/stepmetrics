#' Calculate minutes and steps spent in cadence bands
#'
#' @description
#' Splits a minute-based vector of cadence values (steps per minute) into
#' predefined bands and reports both:
#' \itemize{
#'   \item the number of minutes spent in each band
#'   \item the number of steps accumulated in each band
#' }
#'
#' @param x Numeric vector of cadence values (steps per minute), where each
#'   element represents one minute of the day.
#' @param bands Numeric vector of break points that define the cadence bands.
#'   Defaults to \code{c(0, 1, 20, 40, 60, 80, 100, 120, Inf)}, which produces
#'   the bands 0, 1–19, 20–39, 40–59, 60–79, 80–99, 100–119, and \eqn{\ge}120 spm.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{minutes}{Named numeric vector with minutes spent in each band.}
#'   \item{steps}{Named numeric vector with steps accumulated in each band.}
#'   \item{names}{Character vector of variable names in the format
#'   \code{"CAD_band_<lower>_<upper>_spm"}.}
#' }
#'
#' @examples
#' # Simulate 1 day of cadence values (1440 minutes)
#' set.seed(123)
#' cad <- sample(0:150, size = 1440, replace = TRUE)
#'
#' out <- get_cadence_bands(cad)
#' out$minutes  # minutes in each band
#' out$steps    # steps in each band
#'
#' @export
get_cadence_bands <- function(x,
                              bands = c(0, 1, 20, 40, 60, 80, 100, 120, Inf)) {
  # Bin each minute into a band
  band_factor <- cut(x, breaks = bands, right = FALSE)

  # Minutes per band = frequency counts
  minutes_per_band <- table(band_factor)

  # Steps per band = sum of cadence values in each band
  steps_per_band <- tapply(x, band_factor, sum, default = 0)

  # Build band names
  band_names <- character(length(bands) - 1)
  for (i in seq_along(band_names)) {
    band_names[i] <- paste(bands[i], bands[i + 1] - 1, sep = "_")
    if (bands[i] == bands[i + 1] - 1) {
      band_names[i] <- as.character(bands[i])
    }
  }
  names <- paste0("CAD_band_", band_names, "_spm")

  # Ensure consistent naming across outputs
  names(minutes_per_band) <- names
  names(steps_per_band) <- names

  list(
    minutes = as.numeric(minutes_per_band),
    steps   = as.numeric(steps_per_band),
    names   = names
  )
}
