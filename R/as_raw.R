#' Convert a data frame of raw samples into a \code{stepmetrics_raw} object
#'
#' @description
#' Wraps raw data already in memory (for example exported to CSV by another
#' tool) so that it can be passed to \code{\link{detect_steps}}.
#'
#' @param data \code{data.frame} with \code{time} (numeric Unix seconds or
#'   POSIXct), \code{x}, \code{y}, \code{z} (g) and optionally \code{gx},
#'   \code{gy}, \code{gz} (degrees/s).
#' @param sf Numeric. Sampling frequency in Hz.
#' @param tz Character. Time zone attached to the time column.
#' @param device Character. Optional device name.
#' @return An object of class \code{stepmetrics_raw}.
#' @export
as_raw = function(data, sf, tz = "", device = NA_character_) {
  data = as.data.frame(data)
  miss = setdiff(c("time", "x", "y", "z"), names(data))
  if (length(miss)) {
    stop("'data' lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(sf) || length(sf) != 1 || sf <= 0) {
    stop("'sf' must be a positive number.", call. = FALSE)
  }
  data$time = as.numeric(data$time)
  data = data[order(data$time), , drop = FALSE]
  attr(data$time, "tz") = tz
  header = list(file = NA_character_, format = "data.frame", tz = tz,
                device = device, serial = NA_character_, sf = sf,
                start = if (nrow(data)) data$time[1] else NA_real_,
                acc_range = NA_real_,
                has_gyro = all(c("gx", "gy", "gz") %in% names(data)),
                duration = if (nrow(data)) diff(range(data$time)) + 1 / sf else 0)
  structure(list(data = data, header = header), class = "stepmetrics_raw")
}
