#' Detect steps in a raw recording
#'
#' @description
#' Runs a registered step-detection method (see \code{\link{step_methods}})
#' on a raw recording and returns one row per step with its exact time. The
#' whole recording is read at once (see \code{\link{read_raw}}), which suits
#' laboratory protocols of a few hours; for recordings of several days, run
#' the method inside GGIR with \code{\link{myfun_steps}}. The gyroscope is
#' read directly from the file when the method needs it.
#'
#' To aggregate the steps per epoch and use the cadence metrics of the
#' package, pass the result to \code{\link{steps_to_epochs}}.
#'
#' @param x Character path to a raw file (see \code{\link{read_raw}}), a
#'   \code{stepmetrics_raw} object, or a \code{data.frame} of raw samples
#'   (then \code{sf} is required; see \code{\link{as_raw}}).
#' @param method Character. Name of a registered method.
#' @param params Named list. Parameters overriding the method defaults.
#' @param sf Numeric. Sampling frequency, only when \code{x} is a
#'   \code{data.frame}.
#' @param side Character (default = \code{NA}). Label stored in column
#'   \code{side} (e.g. \code{"left"} or \code{"right"} for one sensor per
#'   foot).
#' @param tz Character (default = \code{""}). Time zone of the device clock;
#'   see \code{\link{read_raw}}.
#'
#' @return A \code{data.frame} with one row per step and columns \code{time}
#'   (numeric Unix seconds, attribute \code{tz}), \code{side}, \code{type},
#'   any extra columns returned by the method, \code{method} and
#'   \code{version}. Attributes: \code{params} (parameters used),
#'   \code{header} (file header) and \code{recording} (start and end of the
#'   recording, Unix time), used by \code{\link{steps_to_epochs}} to include
#'   epochs without steps.
#' @seealso \code{\link{steps_to_epochs}}, \code{\link{benchmark_steps}},
#'   \code{\link{myfun_steps}}
#' @examples
#' \dontrun{
#' steps = detect_steps("path/to/recording.cwa", method = "shoestep",
#'                      side = "right", tz = "Europe/Madrid")
#' epochs = steps_to_epochs(steps, epoch = 60)
#' }
#' @export
detect_steps = function(x, method, params = list(), sf = NULL, side = NA,
                        tz = "") {
  m = .get_method(method)
  p = .merge_params(m, params)
  if (is.data.frame(x)) {
    if (is.null(sf)) stop("'sf' is required when 'x' is a data.frame.", call. = FALSE)
    x = as_raw(x, sf = sf, tz = tz)
  }
  needs_gyro = "gyro" %in% m$requires
  if (!inherits(x, "stepmetrics_raw")) {
    header = raw_header(x, tz = tz)
    if (needs_gyro && isFALSE(header$has_gyro)) {
      stop("Method '", m$name, "' needs the gyroscope, but ", basename(x),
           " has none.", call. = FALSE)
    }
    x = read_raw(x, tz = tz, gyro = needs_gyro)
  }
  header = x$header
  tz = header$tz
  if (header$sf < m$min_sf) {
    warning("Sampling frequency (", header$sf, " Hz) is below the minimum ",
            "recommended for '", m$name, "' (", m$min_sf, " Hz).", call. = FALSE)
  }
  data = x$data
  if (needs_gyro && !all(c("gx", "gy", "gz") %in% names(data))) {
    stop("Method '", m$name, "' needs the gyroscope, but ",
         if (is.na(header$file)) "the data" else basename(header$file),
         " has none.", call. = FALSE)
  }
  steps = if (nrow(data) > 0) .run_core(m, data, header$sf, p) else NULL
  steps = if (is.null(steps) || NROW(steps) == 0) .empty_steps() else as.data.frame(steps)
  steps = steps[order(steps$time), , drop = FALSE]
  extra = setdiff(names(steps), c("time", "type"))
  steps = data.frame(time = steps$time, side = rep(side, nrow(steps)),
                     type = steps$type, steps[, extra, drop = FALSE],
                     method = rep(m$name, nrow(steps)),
                     version = rep(m$version, nrow(steps)))
  rownames(steps) = NULL
  attr(steps$time, "tz") = tz
  attr(steps, "params") = p
  attr(steps, "header") = header
  attr(steps, "recording") = c(
    start = header$start,
    end = if (nrow(data)) max(data$time) + 1 / header$sf else header$start)
  steps
}
