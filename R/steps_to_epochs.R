#' Aggregate detected steps into epochs
#'
#' @description
#' Counts the steps returned by \code{\link{detect_steps}} (or any vector of
#' step times) per epoch. The result has the same layout as the output of
#' \code{\link{readFile}} (\code{timestamp}, \code{steps}), so it can be
#' written to CSV and processed with \code{\link{step.metrics}} like the step
#' counts of any other device.
#'
#' @param steps A \code{data.frame} with a numeric column \code{time} (Unix
#'   seconds) as returned by \code{\link{detect_steps}}, or a numeric vector
#'   of step times. Several data frames (e.g. left and right foot) can be
#'   combined with \code{rbind} first.
#' @param epoch Numeric (default = 60). Epoch length in seconds; must divide
#'   60 or be a multiple of 60.
#' @param start,end Optional numeric (Unix seconds) or POSIXct. Range of the
#'   output. By default the recording range stored by
#'   \code{\link{detect_steps}}, or the range of the steps, rounded to whole
#'   epochs; epochs without steps are included with 0.
#' @param tz Character. Time zone of the output timestamps. By default the
#'   \code{tz} attribute of the time column.
#' @param by_type Logical (default = FALSE). Add one column per step type
#'   (\code{steps_<type>}).
#' @param file Character (optional). If given, the result is also written to
#'   this CSV file, with the timestamp as local clock time
#'   (\code{"YYYY-MM-DD HH:MM:SS"} in \code{tz}), the layout that
#'   \code{\link{readFile}} and \code{\link{step.metrics}} read. Read it back
#'   with the same \code{tz}.
#'
#' @return A \code{data.frame} with \code{timestamp} (character, ISO 8601
#'   with offset, \code{"YYYY-MM-DDTHH:MM:SS+hhmm"}) and \code{steps}, plus
#'   one column per step type if \code{by_type = TRUE}.
#' @seealso \code{\link{detect_steps}}, \code{\link{step.metrics}}
#' @examples
#' t0 = as.numeric(as.POSIXct("2026-11-03 10:00:00", tz = "UTC"))
#' steps_to_epochs(t0 + c(1, 2, 3, 65, 66), epoch = 60, tz = "UTC")
#' @export
steps_to_epochs = function(steps, epoch = 60, start = NULL, end = NULL,
                           tz = NULL, by_type = FALSE, file = NULL) {
  if (!is.numeric(epoch) || length(epoch) != 1 || epoch <= 0 ||
      (60 %% epoch != 0 && epoch %% 60 != 0)) {
    stop("'epoch' must divide 60 or be a multiple of 60.", call. = FALSE)
  }
  rec = attr(steps, "recording")
  if (is.numeric(steps) && is.null(dim(steps))) {
    time_attr = steps
    steps = data.frame(time = as.numeric(steps))
  } else {
    steps = as.data.frame(steps)
    if (!"time" %in% names(steps)) stop("'steps' needs a 'time' column.", call. = FALSE)
    time_attr = steps$time
  }
  if (is.null(tz)) tz = attr(time_attr, "tz")
  if (is.null(tz)) tz = ""
  tm = as.numeric(steps$time)
  if (is.null(start)) start = if (!is.null(rec)) rec[["start"]] else suppressWarnings(min(tm))
  if (is.null(end)) end = if (!is.null(rec)) rec[["end"]] else suppressWarnings(max(tm))
  start = as.numeric(start)
  end = as.numeric(end)
  if (!is.finite(start) || !is.finite(end)) {
    stop("No steps and no recording range: give 'start' and 'end'.", call. = FALSE)
  }
  # align epochs to the clock in the output time zone
  offset = .utc_offset(start, tz)
  e0 = floor((start + offset) / epoch) * epoch - offset
  e1 = ceiling((end + offset) / epoch) * epoch - offset
  if (e1 <= e0) e1 = e0 + epoch
  n = round((e1 - e0) / epoch)
  idx = floor((tm - e0) / epoch) + 1
  ok = idx >= 1 & idx <= n
  out = data.frame(
    timestamp = format(as.POSIXct(e0 + (seq_len(n) - 1) * epoch,
                                  origin = "1970-01-01", tz = tz),
                       "%Y-%m-%dT%H:%M:%S%z"),
    steps = tabulate(idx[ok], nbins = n))
  if (by_type && "type" %in% names(steps)) {
    types = sort(unique(stats::na.omit(steps$type)))
    for (ty in types) {
      sel = ok & !is.na(steps$type) & steps$type == ty
      out[[paste0("steps_", ty)]] = tabulate(idx[sel], nbins = n)
    }
  }
  if (!is.null(file)) {
    # readFile() does not parse ISO 8601 with offset: write the local clock
    csv = out
    csv$timestamp = format(as.POSIXct(e0 + (seq_len(n) - 1) * epoch,
                                      origin = "1970-01-01", tz = tz),
                           "%Y-%m-%d %H:%M:%S")
    utils::write.csv(csv, file, row.names = FALSE)
  }
  out
}

.utc_offset = function(t, tz) {
  p = as.POSIXlt(as.POSIXct(t, origin = "1970-01-01", tz = tz))
  off = p$gmtoff
  if (is.null(off) || is.na(off)) 0 else off
}
