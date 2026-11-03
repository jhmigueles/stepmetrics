#' Use a step-detection method inside GGIR
#'
#' @description
#' Builds the \code{myfun} object that \pkg{GGIR} accepts as an external
#' function, so that a method registered in \pkg{stepmetrics} runs during
#' GGIR part 1 on the data GGIR has already read and calibrated. GGIR then
#' stores the step counts per epoch next to its other metrics and can use
#' them in parts 2 and 5.
#'
#' @details
#' Limitations of this route, all coming from how GGIR passes data to
#' external functions (checked in GGIR 3.3):
#' \itemize{
#'   \item Only the acceleration (\code{x}, \code{y}, \code{z}) is passed, so
#'     methods that need the gyroscope cannot run in GGIR; use
#'     \code{\link{detect_steps}} instead.
#'   \item GGIR returns counts per epoch, not the time of each step.
#'   \item GGIR processes the file in consecutive blocks without overlap, so
#'     a step that falls on the border between two blocks can be missed or
#'     counted twice. With blocks of several hours the effect is negligible
#'     for daily totals, but \code{\link{detect_steps}} avoids it.
#'   \item Only one \code{myfun} object can be passed per GGIR run.
#' }
#'
#' @param method Character. Name of a registered method (acceleration only).
#' @param params Named list. Parameters overriding the method defaults.
#' @param expected_sample_rate Numeric. Rate (Hz) GGIR resamples the data to
#'   before calling the method. Defaults to the method's recommended rate.
#' @param outputres Numeric (default = 1). Resolution in seconds of the
#'   counts returned to GGIR; GGIR sums them to its short epoch.
#' @param colname Character (default = \code{"step_count"}). Name of the
#'   column in the GGIR output.
#'
#' @return A list with the elements GGIR expects in \code{myfun}
#'   (\code{FUN}, \code{parameters}, \code{expected_sample_rate},
#'   \code{expected_unit}, \code{colnames}, \code{minlength},
#'   \code{outputres}, \code{outputtype}, \code{aggfunction},
#'   \code{timestamp}, \code{reporttype}).
#' @seealso \code{\link{detect_steps}}
#' @examples
#' \dontrun{
#' GGIR::GGIR(datadir = "raw", outputdir = "out", mode = 1:2,
#'            myfun = myfun_steps("verisense"))
#' }
#' @export
myfun_steps = function(method, params = list(), expected_sample_rate = NULL,
                       outputres = 1, colname = "step_count") {
  m = .get_method(method)
  if ("gyro" %in% m$requires) {
    stop("Method '", m$name, "' needs the gyroscope, which GGIR does not ",
         "pass to external functions. Use detect_steps() instead.",
         call. = FALSE)
  }
  p = .merge_params(m, params)
  if (is.null(expected_sample_rate)) expected_sample_rate = m$sf
  list(FUN = .myfun_core,
       parameters = list(method = m, params = p, sf = expected_sample_rate,
                         outputres = outputres),
       expected_sample_rate = expected_sample_rate,
       expected_unit = "g",
       colnames = colname,
       minlength = 1,
       outputres = outputres,
       outputtype = "numeric",
       aggfunction = sum,
       timestamp = TRUE,
       reporttype = "event")
}

# Function GGIR calls: data is a matrix with the timestamp (numeric) in the
# first column and x, y, z in the next three, at the expected sample rate.
.myfun_core = function(data, parameters) {
  sf = parameters$sf
  res = parameters$outputres
  data = as.matrix(data)
  n_out = max(1, round(nrow(data) / (sf * res)))
  if (ncol(data) >= 4) {
    tm = as.numeric(data[, 1])
    acc = data[, 2:4, drop = FALSE]
  } else { # no timestamp: build one from the sample index
    tm = (seq_len(nrow(data)) - 1) / sf
    acc = data[, 1:3, drop = FALSE]
  }
  d = data.frame(time = tm, x = as.numeric(acc[, 1]), y = as.numeric(acc[, 2]),
                 z = as.numeric(acc[, 3]))
  steps = .run_core(parameters$method, d, sf, parameters$params)
  idx = floor((steps$time - tm[1]) / res) + 1
  idx = idx[idx >= 1 & idx <= n_out]
  matrix(tabulate(idx, nbins = n_out), ncol = 1)
}
