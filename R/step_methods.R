.step_registry = new.env(parent = emptyenv())

#' Register a step-detection method
#'
#' @description
#' Adds a step-detection algorithm to the registry used by
#' \code{\link{detect_steps}} and \code{\link{myfun_steps}}. Each algorithm
#' shipped with \pkg{stepmetrics} registers itself when the package is
#' loaded; this function also lets users plug in their own algorithm and
#' compare it with the same tools.
#'
#' @section Contract of the core function:
#' \code{fun(data, sf, params)} receives
#' \itemize{
#'   \item \code{data}: a \code{data.frame} with \code{time} (numeric,
#'     seconds since 1970-01-01 UTC), \code{x}, \code{y}, \code{z} (g) and,
#'     if the method requires them, \code{gx}, \code{gy}, \code{gz}
#'     (degrees/s), sampled at a constant rate;
#'   \item \code{sf}: the sampling frequency in Hz;
#'   \item \code{params}: a named list with the method's parameters (the
#'     defaults declared here, overridden by the user's values).
#' }
#' It must return a \code{data.frame} with one row per step and at least a
#' numeric column \code{time} (time of the step event, in the same units as
#' \code{data$time}). An optional column \code{type} classifies the step
#' (e.g. \code{"displacement"} or \code{"in_place"}); other columns are kept
#' as they are.
#'
#' @param name Character. Short identifier of the method (e.g.
#'   \code{"shoestep"}).
#' @param fun Function implementing the core algorithm (see section
#'   \emph{Contract of the core function}).
#' @param version Character. Version of the implementation.
#' @param requires Character vector. Channels the method needs:
#'   \code{"acc"} and optionally \code{"gyro"}.
#' @param sf Numeric. Recommended sampling frequency in Hz (used by
#'   \code{\link{myfun_steps}} as \code{expected_sample_rate}).
#' @param min_sf Numeric. Lowest sampling frequency at which the method is
#'   expected to work; lower rates trigger a warning.
#' @param locations Character vector. Body locations the method was designed
#'   for (e.g. \code{"shoe"}, \code{"ankle"}, \code{"wrist"}).
#' @param defaults Named list. Default parameters.
#' @param citation Character. Reference of the original method.
#' @param license Character. Licence of the original code the implementation
#'   is based on, or \code{"reimplemented from the publication"}.
#' @param description Character. One-line description.
#'
#' @return Invisibly, the registry entry.
#' @seealso \code{\link{step_methods}}, \code{\link{detect_steps}}
#' @examples
#' # a toy method: one step at every peak of the acceleration magnitude
#' # above 1.5 g, at least 0.3 s apart
#' toy = function(data, sf, params) {
#'   a = sqrt(data$x^2 + data$y^2 + data$z^2)
#'   i = which(a > params$threshold &
#'             a >= c(-Inf, a[-length(a)]) & a > c(a[-1], -Inf))
#'   keep = i[c(TRUE, diff(data$time[i]) > params$refractory)]
#'   data.frame(time = data$time[keep])
#' }
#' register_step_method("toy", toy, version = "0.0.1", sf = 50, min_sf = 25,
#'                      locations = "shoe",
#'                      defaults = list(threshold = 1.5, refractory = 0.3),
#'                      description = "Toy example")
#' step_methods()
#' unregister_step_method("toy")
#' @export
register_step_method = function(name, fun, version, requires = "acc",
                                sf = 100, min_sf = sf, locations = NA_character_,
                                defaults = list(),
                                citation = NA_character_,
                                license = NA_character_,
                                description = NA_character_) {
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    stop("'name' must be a single non-empty string.", call. = FALSE)
  }
  if (!is.function(fun)) stop("'fun' must be a function.", call. = FALSE)
  if (!all(names(formals(fun))[1:3] == c("data", "sf", "params"))) {
    stop("'fun' must have arguments (data, sf, params).", call. = FALSE)
  }
  requires = match.arg(requires, c("acc", "gyro"), several.ok = TRUE)
  if (!is.list(defaults)) stop("'defaults' must be a list.", call. = FALSE)
  entry = list(name = name, fun = fun, version = as.character(version),
               requires = union("acc", requires), sf = sf, min_sf = min_sf,
               locations = locations, defaults = defaults,
               citation = citation, license = license,
               description = description)
  assign(name, entry, envir = .step_registry)
  invisible(entry)
}

#' @rdname register_step_method
#' @export
unregister_step_method = function(name) {
  if (exists(name, envir = .step_registry, inherits = FALSE)) {
    rm(list = name, envir = .step_registry)
  }
  invisible(NULL)
}

#' List the available step-detection methods
#'
#' @return A \code{data.frame} with one row per registered method and its
#'   metadata: name, version, channels required, recommended and minimum
#'   sampling frequency, locations, citation, licence and
#'   description.
#' @seealso \code{\link{register_step_method}}
#' @export
step_methods = function() {
  nm = sort(ls(envir = .step_registry))
  if (length(nm) == 0) {
    return(data.frame(name = character(0), version = character(0),
                      requires = character(0), sf = numeric(0),
                      min_sf = numeric(0), locations = character(0),
                      citation = character(0),
                      license = character(0), description = character(0)))
  }
  do.call(rbind, lapply(nm, function(n) {
    m = get(n, envir = .step_registry)
    data.frame(name = m$name, version = m$version,
               requires = paste(m$requires, collapse = "+"),
               sf = m$sf, min_sf = m$min_sf,
               locations = paste(m$locations, collapse = ", "),
               citation = m$citation,
               license = m$license, description = m$description)
  }))
}

.get_method = function(method) {
  if (is.list(method) && !is.null(method$fun)) return(method)
  if (!is.character(method) || length(method) != 1 ||
      !exists(method, envir = .step_registry, inherits = FALSE)) {
    available = ls(envir = .step_registry)
    stop("Unknown step-detection method '", paste(method, collapse = ", "),
         "'. Available: ",
         if (length(available)) paste(sort(available), collapse = ", ") else "none",
         ".", call. = FALSE)
  }
  get(method, envir = .step_registry)
}

.merge_params = function(m, params) {
  if (!is.list(params)) stop("'params' must be a list.", call. = FALSE)
  unknown = setdiff(names(params), names(m$defaults))
  if (length(unknown) > 0) {
    stop("Unknown parameter(s) for method '", m$name, "': ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  utils::modifyList(m$defaults, params)
}

.run_core = function(m, data, sf, params) {
  out = m$fun(data, sf, params)
  if (is.null(out) || NROW(out) == 0) return(.empty_steps())
  out = as.data.frame(out)
  if (!"time" %in% names(out) || !is.numeric(out$time)) {
    stop("Method '", m$name, "' did not return a numeric 'time' column.",
         call. = FALSE)
  }
  if (!"type" %in% names(out)) out$type = NA_character_
  out
}

.empty_steps = function() {
  data.frame(time = numeric(0), type = character(0))
}
