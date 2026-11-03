#' Compare detected steps with a reference
#'
#' @description
#' Matches each detected step with at most one reference step (for example,
#' steps annotated on video) within a time tolerance, and reports how many
#' steps were found, missed and added. Used to validate every method with
#' the same criterion.
#'
#' @details
#' Matching is one-to-one: all pairs closer than \code{tolerance} are sorted
#' by their time difference and accepted from the closest, skipping steps
#' already matched. Unmatched detected steps are false positives and
#' unmatched reference steps are false negatives.
#'
#' When \code{segments} is given (for example the phases of a laboratory
#' protocol), steps are matched within each segment separately and one row
#' is returned per segment plus a row \code{"all"} with all the steps inside
#' any segment.
#'
#' @param detected,reference Numeric vectors of step times (Unix seconds),
#'   or data frames with a numeric column \code{time} (e.g. the output of
#'   \code{\link{detect_steps}}).
#' @param tolerance Numeric (default = 0.25). Maximum time difference in
#'   seconds for a detected step to match a reference step.
#' @param segments Optional \code{data.frame} with columns \code{label},
#'   \code{start} and \code{end} (numeric Unix seconds or POSIXct); each
#'   segment covers \code{[start, end)}.
#'
#' @return A list of class \code{stepmetrics_benchmark} with
#' \describe{
#'   \item{\code{summary}}{\code{data.frame} with one row per segment:
#'     \code{n_reference}, \code{n_detected}, \code{matched},
#'     \code{false_positives}, \code{false_negatives}, \code{sensitivity},
#'     \code{precision}, \code{f1}, \code{count_error} (detected minus
#'     reference), \code{abs_pct_error} (absolute count error as a
#'     percentage of the reference), \code{mean_offset_s} and
#'     \code{mean_abs_offset_s} (detected minus reference time of the
#'     matched pairs).}
#'   \item{\code{matches}}{\code{data.frame} with the matched pairs:
#'     \code{segment}, \code{reference}, \code{detected} and \code{offset}.}
#' }
#' @examples
#' ref = c(1, 2, 3, 4, 5)
#' det = c(1.05, 2.1, 3.4, 4.0, 6)
#' benchmark_steps(det, ref, tolerance = 0.25)$summary
#' @export
benchmark_steps = function(detected, reference, tolerance = 0.25,
                           segments = NULL) {
  det = .as_times(detected, "detected")
  ref = .as_times(reference, "reference")
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance <= 0) {
    stop("'tolerance' must be a positive number.", call. = FALSE)
  }
  if (is.null(segments)) {
    segs = data.frame(label = "all", start = -Inf, end = Inf)
  } else {
    segs = as.data.frame(segments)
    miss = setdiff(c("label", "start", "end"), names(segs))
    if (length(miss)) {
      stop("'segments' lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
    }
    segs$start = as.numeric(segs$start)
    segs$end = as.numeric(segs$end)
  }
  rows = list()
  pairs = list()
  in_any_det = rep(FALSE, length(det))
  in_any_ref = rep(FALSE, length(ref))
  for (i in seq_len(nrow(segs))) {
    sd = det >= segs$start[i] & det < segs$end[i]
    sr = ref >= segs$start[i] & ref < segs$end[i]
    in_any_det = in_any_det | sd
    in_any_ref = in_any_ref | sr
    mt = .match_events(det[sd], ref[sr], tolerance)
    rows[[i]] = .bench_row(as.character(segs$label[i]), sum(sr), sum(sd), mt)
    if (nrow(mt)) pairs[[i]] = data.frame(segment = as.character(segs$label[i]), mt)
  }
  summary = do.call(rbind, rows)
  if (!is.null(segments)) {
    all = list(matched = sum(summary$matched),
               offset = unlist(lapply(pairs, function(p) p$offset)))
    summary = rbind(summary, .bench_row("all", sum(in_any_ref), sum(in_any_det),
                                        data.frame(offset = all$offset)))
  }
  matches = if (length(pairs)) do.call(rbind, pairs) else
    data.frame(segment = character(0), reference = numeric(0),
               detected = numeric(0), offset = numeric(0))
  rownames(summary) = NULL
  rownames(matches) = NULL
  structure(list(summary = summary, matches = matches, tolerance = tolerance),
            class = "stepmetrics_benchmark")
}

#' @export
print.stepmetrics_benchmark = function(x, ...) {
  cat("Step detection benchmark (tolerance ", x$tolerance, " s)\n", sep = "")
  print(x$summary, row.names = FALSE, digits = 3)
  invisible(x)
}

.as_times = function(x, what) {
  if (is.data.frame(x)) {
    if (!"time" %in% names(x)) stop("'", what, "' needs a 'time' column.", call. = FALSE)
    x = x$time
  }
  x = as.numeric(x)
  if (anyNA(x)) stop("'", what, "' contains missing times.", call. = FALSE)
  sort(x)
}

.match_events = function(det, ref, tolerance) {
  empty = data.frame(reference = numeric(0), detected = numeric(0),
                     offset = numeric(0))
  if (!length(det) || !length(ref)) return(empty)
  # candidate pairs: for each detected step, the reference steps within tolerance
  lo = findInterval(det - tolerance, ref, left.open = TRUE) + 1
  hi = findInterval(det + tolerance, ref)
  n = pmax(0, hi - lo + 1)
  if (sum(n) == 0) return(empty)
  di = rep(seq_along(det), n)
  ri = unlist(mapply(function(a, b) if (b >= a) a:b else integer(0), lo, hi,
                     SIMPLIFY = FALSE))
  dt = det[di] - ref[ri]
  o = order(abs(dt), di)
  used_d = rep(FALSE, length(det))
  used_r = rep(FALSE, length(ref))
  keep = logical(length(o))
  for (k in seq_along(o)) {
    j = o[k]
    if (!used_d[di[j]] && !used_r[ri[j]]) {
      used_d[di[j]] = TRUE
      used_r[ri[j]] = TRUE
      keep[k] = TRUE
    }
  }
  sel = o[keep]
  out = data.frame(reference = ref[ri[sel]], detected = det[di[sel]],
                   offset = dt[sel])
  out[order(out$reference), , drop = FALSE]
}

.bench_row = function(label, n_ref, n_det, mt) {
  tp = nrow(mt)
  sens = if (n_ref > 0) tp / n_ref else NA_real_
  prec = if (n_det > 0) tp / n_det else NA_real_
  f1 = if (n_ref + n_det > 0) 2 * tp / (n_ref + n_det) else NA_real_
  data.frame(segment = label, n_reference = n_ref, n_detected = n_det,
             matched = tp, false_positives = n_det - tp,
             false_negatives = n_ref - tp, sensitivity = sens,
             precision = prec, f1 = f1, count_error = n_det - n_ref,
             abs_pct_error = if (n_ref > 0) 100 * abs(n_det - n_ref) / n_ref else NA_real_,
             mean_offset_s = if (tp) mean(mt$offset) else NA_real_,
             mean_abs_offset_s = if (tp) mean(abs(mt$offset)) else NA_real_)
}
