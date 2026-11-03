#' Read raw accelerometer (and gyroscope) data in a common format
#'
#' @description
#' Thin layer over the readers that \pkg{GGIR} uses (\pkg{GGIRread},
#' \pkg{read.gt3x}, \pkg{unisensR}) that returns raw data from different
#' devices in one common format, keeps the gyroscope when the device recorded
#' it and gives every sample its own timestamp.
#'
#' The whole file is read at once. \pkg{stepmetrics} is meant for laboratory
#' protocols of a few hours; to run a step-detection method on recordings of
#' several days, use GGIR with \code{\link{myfun_steps}}, which reads the file
#' in blocks.
#'
#' Supported formats (detected from the file extension and content), the same
#' raw formats GGIR reads:
#' \itemize{
#'   \item \strong{Axivity} \code{.cwa} (AX3 and AX6) via
#'     \code{\link[GGIRread]{readAxivity}}. Gyroscope is returned for AX6.
#'   \item \strong{Axivity} \code{.csv} exported by OmGui (time, x, y, z and,
#'     with seven columns, gyroscope), resampled to the nominal frequency as
#'     GGIR does.
#'   \item \strong{GENEActiv} \code{.bin} via
#'     \code{\link[GGIRread]{readGENEActiv}}.
#'   \item \strong{Parmay Matrix} \code{.bin}/\code{.BIN} via
#'     \code{\link[GGIRread]{readParmayMatrix}} (with \code{read_gyro = TRUE}).
#'   \item \strong{ActiGraph} \code{.gt3x} via
#'     \code{\link[read.gt3x]{read.gt3x}} (package \pkg{read.gt3x} must be
#'     installed). Idle sleep periods are filled with the last sample, as
#'     ActiLife does.
#'   \item \strong{ActiGraph} \code{.csv} exported by ActiLife (raw data, with
#'     or without timestamp column), including Verisense files in that format.
#'   \item \strong{Movisens} \code{acc.bin} (or its folder) in Unisens format
#'     via \pkg{unisensR} (must be installed).
#' }
#' \code{.bin} files are told apart by their header (GENEActiv), by a
#' \code{unisens.xml} file in the same folder (Movisens) or otherwise read as
#' Parmay Matrix. Other csv layouts (the "ad-hoc" csv of GGIR) can be read
#' with any csv reader and passed through \code{\link{as_raw}}.
#'
#' @param file Character. Path to the raw data file.
#' @param tz Character (default = \code{""}, the session time zone). Time zone
#'   in which the device clock was configured. It is used to convert the
#'   device clock to Unix time and is stored as attribute \code{tz} of the
#'   \code{time} column.
#' @param gyro Logical (default = TRUE). Whether to return the gyroscope when
#'   the device recorded it.
#'
#' @details
#' Device clocks store local time without a time zone. As in GGIR, the clock
#' time is interpreted in \code{tz}.
#'
#' @return A list of class \code{stepmetrics_raw} with:
#' \describe{
#'   \item{\code{data}}{\code{data.frame} with \code{time} (numeric, seconds
#'     since 1970-01-01 UTC, with attribute \code{tz}), \code{x}, \code{y},
#'     \code{z} (g) and, when available, \code{gx}, \code{gy}, \code{gz}
#'     (degrees/s), \code{temperature} and \code{light}.}
#'   \item{\code{header}}{List with \code{file}, \code{format},
#'     \code{device}, \code{serial}, \code{sf} (Hz), \code{start} (numeric
#'     Unix time of the recording start), \code{duration} (seconds),
#'     \code{acc_range} (g, \code{NA} when not in the file), \code{has_gyro},
#'     \code{tz} and
#'     the original header of the device reader in \code{device_header}.}
#' }
#'
#' @examples
#' \dontrun{
#' raw = read_raw("path/to/recording.cwa", tz = "Europe/Madrid")
#' head(raw$data)
#' raw$header$sf
#' }
#' @seealso \code{\link{detect_steps}}, \code{\link{as_raw}}
#' @export
read_raw = function(file, tz = "", gyro = TRUE) {
  header = raw_header(file, tz = tz)
  data = switch(header$format,
                axivity = .read_axivity(file, tz, header),
                axivity_csv = .read_axivity_csv(file, tz, header),
                geneactiv = .read_geneactiv(file, tz),
                matrix = .read_matrix(file, tz, gyro),
                actigraph_gt3x = .read_gt3x(file, tz, header),
                actigraph_csv = .read_actigraph_csv(file, header),
                movisens = .read_movisens(header),
                stop("Unsupported format: ", header$format, call. = FALSE))
  data = .standardise_raw(data, header$format)
  if (header$format == "matrix" && gyro) {
    header$has_gyro = all(c("gx", "gy", "gz") %in% names(data))
  }
  header$duration = if (nrow(data)) max(data$time) - header$start + 1 / header$sf else 0
  if (!gyro) data = data[, setdiff(names(data), c("gx", "gy", "gz")), drop = FALSE]
  rownames(data) = NULL
  attr(data$time, "tz") = tz
  structure(list(data = data, header = header), class = "stepmetrics_raw")
}

#' Read the header of a raw data file
#'
#' @description
#' Returns the header of a raw accelerometer file in the common format used
#' by \code{\link{read_raw}}, without reading the data.
#'
#' @inheritParams read_raw
#' @return A list; see the \code{header} element in \code{\link{read_raw}}.
#' @export
raw_header = function(file, tz = "") {
  if (!is.character(file) || length(file) != 1 || !file.exists(file)) {
    stop("File not found: ", paste(file, collapse = ", "), call. = FALSE)
  }
  format = .raw_format(file)
  out = list(file = normalizePath(file, mustWork = FALSE), format = format,
             tz = tz)
  if (format == "axivity") {
    h = .try_reader(file, GGIRread::readAxivity(file, start = 0, end = 0,
                                                desiredtz = tz)$header)
    out$device = paste("Axivity", h$hardwareType)
    out$serial = as.character(h$uniqueSerialCode)
    out$sf = as.numeric(h$frequency)
    out$start = as.numeric(h$start)
    out$acc_range = as.numeric(h$accrange)
    out$has_gyro = identical(h$hardwareType, "AX6")
    out$duration = as.numeric(h$blocks) * as.numeric(h$blockLength) / out$sf
  } else if (format == "geneactiv") {
    h = .try_reader(file, GGIRread::readGENEActiv(file, start = 1, end = 1,
                                                  desiredtz = tz)$header)
    out$device = "GENEActiv"
    out$serial = as.character(h$serial_number)
    out$sf = as.numeric(h$SampleRate)
    first = GGIRread::readGENEActiv(file, start = 1, end = 1, desiredtz = tz)$data
    out$start = first$time[1]
    out$acc_range = 8
    out$has_gyro = FALSE
    out$duration = NA_real_ # page count in the header is unreliable
  } else if (format == "matrix") {
    h = .try_reader(file, GGIRread::readParmayMatrix(file, start = 1, end = 1,
                                                     desiredtz = tz,
                                                     read_gyro = TRUE)$header)
    out$device = "Parmay Matrix"
    out$serial = NA_character_
    out$sf = as.numeric(h$sf)
    out$start = as.numeric(h$starttime)
    out$acc_range = as.numeric(h$acc_dynrange)
    out$has_gyro = NA # only known after reading the data
    out$duration = NA_real_
  } else if (format == "actigraph_gt3x") {
    .need_package("read.gt3x", file)
    h = .try_reader(file, read.gt3x::parse_gt3x_info(file, tz = "GMT"))
    h = h[lengths(h) != 0]
    out$device = paste("ActiGraph", h[["Device Type"]])
    out$serial = as.character(h[["Serial Number"]])
    out$sf = as.numeric(h[["Sample Rate"]])
    # read.gt3x labels the device clock as GMT; it is local time in tz
    out$start = .clock_to_unix(as.numeric(h[["Start Date"]]), tz)
    last = as.numeric(h[["Last Sample Time"]])
    out$duration = if (length(last) && is.finite(last)) {
      last - as.numeric(h[["Start Date"]])
    } else NA_real_
    out$acc_range = suppressWarnings(as.numeric(h[["Acceleration Max"]]))
    out$has_gyro = FALSE
    h = lapply(h, function(v) if (inherits(v, "POSIXct")) format(v) else v)
  } else if (format == "actigraph_csv") {
    h = .actigraph_csv_header(file, tz)
    out$device = h$device
    out$serial = h$serial
    out$sf = h$sf
    out$start = h$start
    out$acc_range = NA_real_
    out$has_gyro = FALSE
    out$duration = NA_real_ # would need counting every line
  } else if (format == "axivity_csv") {
    h = .axivity_csv_header(file, tz)
    out$device = "Axivity"
    out$serial = NA_character_
    out$sf = h$sf
    out$start = h$start
    out$acc_range = NA_real_
    out$has_gyro = h$ncol >= 7
    out$duration = NA_real_
  } else if (format == "movisens") {
    .need_package("unisensR", file)
    h = .movisens_header(file, tz)
    out$device = "Movisens"
    out$serial = h$serial
    out$sf = h$sf
    out$start = h$start
    out$acc_range = NA_real_
    out$has_gyro = FALSE
    out$duration = h$n / h$sf
  }
  out$device_header = h
  out
}

# ---- internal helpers -------------------------------------------------------

.raw_format = function(file) {
  if (dir.exists(file)) {
    if (file.exists(file.path(file, "unisens.xml"))) return("movisens")
    stop("'", basename(file), "' is a folder without unisens.xml.", call. = FALSE)
  }
  ext = tolower(tools::file_ext(file))
  if (ext == "cwa") return("axivity")
  if (ext == "gt3x") {
    if (tools::file_ext(file) != "gt3x") {
      stop("read.gt3x cannot read files with extension '.",
           tools::file_ext(file), "'; rename ", basename(file),
           " to lower case '.gt3x'.", call. = FALSE)
    }
    return("actigraph_gt3x")
  }
  if (ext == "csv") {
    first = readLines(file, n = 1, warn = FALSE)
    if (length(first) == 1 && grepl("ActiGraph", first, fixed = TRUE)) {
      return("actigraph_csv")
    }
    row = strsplit(first, ",", fixed = TRUE)[[1]]
    if (length(row) >= 4 &&
        all(!is.na(suppressWarnings(as.numeric(row[2:4]))))) {
      return("axivity_csv")
    }
    stop("Unrecognised csv layout in ", basename(file), ". Read it with a ",
         "csv reader and pass the data frame through as_raw().", call. = FALSE)
  }
  if (ext == "bin") {
    if (file.exists(file.path(dirname(file), "unisens.xml"))) return("movisens")
    first = readLines(file, n = 1, warn = FALSE, encoding = "latin1")
    if (length(first) == 1 && grepl("Device Identity", first, useBytes = TRUE)) {
      return("geneactiv")
    }
    return("matrix")
  }
  stop("Unsupported file format '.", ext, "' (", basename(file), "). ",
       "Supported: Axivity .cwa/.csv, GENEActiv .bin, Parmay Matrix .bin, ",
       "ActiGraph .gt3x/.csv and Movisens .bin.", call. = FALSE)
}

.try_reader = function(file, expr) {
  tryCatch(expr, error = function(e) {
    stop("Could not read ", basename(file), ": ", conditionMessage(e),
         call. = FALSE)
  })
}

.need_package = function(pkg, file) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Reading ", basename(file), " needs package '", pkg,
         "': install.packages(\"", pkg, "\")", call. = FALSE)
  }
}

# Clock times stored without time zone and read as UTC (numeric seconds) are
# converted to Unix time in 'tz'. The UTC offset is looked up once per hour.
.clock_to_unix = function(x, tz) {
  if (!length(x)) return(x)
  hour = floor(x / 3600) * 3600
  u = unique(hour)
  clock = format(as.POSIXct(u, origin = "1970-01-01", tz = "UTC"),
                 "%Y-%m-%d %H:%M:%S")
  local = as.numeric(as.POSIXct(clock, tz = tz, format = "%Y-%m-%d %H:%M:%S"))
  x + (local - u)[match(hour, u)]
}

.read_axivity = function(file, tz, header) {
  n = header$device_header$blocks
  d = .try_reader(file, suppressWarnings(
    GGIRread::readAxivity(file, start = 0, end = n, desiredtz = tz,
                          header = header$device_header)))
  d$data
}

.read_geneactiv = function(file, tz) {
  d = .try_reader(file, GGIRread::readGENEActiv(file, start = 1, end = 0,
                                                desiredtz = tz))
  d$data
}

.read_matrix = function(file, tz, gyro) {
  d = .try_reader(file, GGIRread::readParmayMatrix(file, desiredtz = tz,
                                                   read_gyro = gyro))
  d$data
}

# ---- ActiGraph gt3x ---------------------------------------------------------

.read_gt3x = function(file, tz, header) {
  .need_package("read.gt3x", file)
  d = .try_reader(file, suppressMessages(
    read.gt3x::read.gt3x(file, asDataFrame = TRUE)))
  d = as.data.frame(d)
  if (!nrow(d)) return(NULL)
  # time is the device clock labelled as GMT
  d = data.frame(time = .clock_to_unix(as.numeric(d$time), tz),
                 x = d$X, y = d$Y, z = d$Z)
  .fill_gaps(d, header$start, header$sf)
}

# Places samples on the grid t0 + k / sf and fills missing grid points (idle
# sleep) with the last sample, as ActiLife does.
.fill_gaps = function(d, t0, sf) {
  k = round((d$time - t0) * sf)
  keep = !duplicated(k)
  d = d[keep, , drop = FALSE]
  k = k[keep]
  if (length(k) < 2 || all(diff(k) == 1)) {
    d$time = t0 + k / sf
    return(d)
  }
  kk = seq(k[1], k[length(k)])
  idx = findInterval(kk, k)
  out = d[idx, , drop = FALSE]
  out$time = t0 + kk / sf
  rownames(out) = NULL
  out
}

# ---- ActiGraph csv (ActiLife export) ----------------------------------------

.actigraph_csv_header = function(file, tz) {
  lines = readLines(file, n = 11, warn = FALSE)
  if (length(lines) < 10) stop("Header not found in ", basename(file), call. = FALSE)
  first = lines[1]
  get = function(pattern) {
    hit = regmatches(lines, regexpr(pattern, lines, perl = TRUE))
    if (length(hit)) sub(pattern, "\\1", hit[1], perl = TRUE) else NA_character_
  }
  sf = as.numeric(sub(",", ".", get("at ([0-9.,]+) Hz"), fixed = TRUE))
  date_format = get("date format ([^ ]+)")
  start_time = trimws(get("^Start Time\\s*(.*)$"))
  start_date = trimws(get("^Start Date\\s*(.*)$"))
  if (is.na(sf) || is.na(start_time) || is.na(start_date)) {
    stop("Sample rate, start date or start time not found in the header of ",
         basename(file), call. = FALSE)
  }
  fmt = .actilife_date_format(date_format, start_date)
  start = as.POSIXct(paste(start_date, start_time), tz = tz,
                     format = paste(fmt, "%H:%M:%OS"))
  if (is.na(start)) {
    stop("Could not parse start '", start_date, " ", start_time, "' in ",
         basename(file), call. = FALSE)
  }
  # line 11 holds column names in most exports
  names_line = if (length(lines) >= 11) lines[11] else ""
  fields = strsplit(names_line, ",", fixed = TRUE)[[1]]
  has_names = length(fields) > 0 && is.na(suppressWarnings(as.numeric(fields[1])))
  has_time = has_names && grepl("time", fields[1], ignore.case = TRUE)
  if (!has_names) {
    has_time = length(fields) >= 4 # no names: a fourth column means timestamp
  }
  list(sf = sf, start = as.numeric(start), date_format = date_format,
       skip = if (has_names) 11L else 10L, has_time = has_time,
       serial = trimws(get("^Serial Number:\\s*(.*)$")),
       device = if (grepl("Verisense", first, fixed = TRUE)) "ActiGraph Verisense" else "ActiGraph",
       first_line = first)
}

# ActiLife date formats (e.g. "M/d/yyyy", "dd-MM-yyyy") to strptime.
.actilife_date_format = function(x, example) {
  if (is.na(x)) {
    stop("Date format not found in the ActiGraph header.", call. = FALSE)
  }
  sep = regmatches(x, regexpr("[^A-Za-z]", x))
  parts = if (length(sep)) strsplit(x, sep, fixed = TRUE)[[1]] else x
  conv = vapply(parts, function(p) {
    if (grepl("^y{4}$", p)) "%Y" else if (grepl("^y{1,2}$", p)) "%y" else
      if (grepl("^M{3,}$", p)) "%b" else if (grepl("^M{1,2}$", p)) "%m" else
        if (grepl("^d{1,2}$", p)) "%d" else
          stop("Unknown date format '", x, "'.", call. = FALSE)
  }, character(1))
  paste(conv, collapse = if (length(sep)) sep else "")
}

.read_actigraph_csv = function(file, header) {
  h = header$device_header
  d = .fread_rows(file, skip = h$skip, nrows = Inf)
  if (is.null(d)) return(NULL)
  acc = if (h$has_time || ncol(d) >= 4) d[, 2:4] else d[, 1:3]
  data.frame(time = header$start + (seq_len(nrow(d)) - 1) / header$sf,
             x = as.numeric(acc[[1]]), y = as.numeric(acc[[2]]),
             z = as.numeric(acc[[3]]))
}

.fread_rows = function(file, skip, nrows, colClasses = NULL) {
  d = tryCatch(
    suppressWarnings(data.table::fread(file, skip = skip, nrows = nrows,
                                       header = FALSE, data.table = FALSE,
                                       showProgress = FALSE,
                                       colClasses = colClasses)),
    error = function(e) NULL)
  if (is.null(d) || !NROW(d)) return(NULL)
  d
}

# ---- Axivity csv (OmGui export) ---------------------------------------------

.axivity_csv_header = function(file, tz) {
  d = .fread_rows(file, skip = 0, nrows = 100000, colClasses = list(character = 1))
  if (is.null(d)) stop("No data in ", basename(file), call. = FALSE)
  tm = .axivity_csv_time(d[[1]], tz)
  sf = length(tm) / (tm[length(tm)] - tm[1])
  sf = floor(sf / 5) * 5 # as GGIR: never assume more frequency than there is
  if (!is.finite(sf) || sf <= 0) {
    stop("Sampling frequency could not be estimated from ", basename(file),
         call. = FALSE)
  }
  list(sf = sf, start = tm[1], ncol = ncol(d))
}

# OmGui writes either Unix time computed as if the device clock were UTC, or
# "YYYY-MM-DD hh:mm:ss.fff" clock time; both are clock time in tz.
.axivity_csv_time = function(x, tz) {
  num = suppressWarnings(as.numeric(x))
  if (!anyNA(num)) return(.clock_to_unix(num, tz))
  tm = as.numeric(as.POSIXct(x, tz = tz, format = "%Y-%m-%d %H:%M:%OS"))
  if (anyNA(tm)) {
    tm = as.numeric(as.POSIXct(x, tz = tz))
  }
  if (anyNA(tm)) stop("Unrecognised timestamps in the Axivity csv file.", call. = FALSE)
  tm
}

.read_axivity_csv = function(file, tz, header) {
  sf = header$sf
  t0 = header$start
  d = .fread_rows(file, skip = 0, nrows = Inf, colClasses = list(character = 1))
  if (is.null(d) || nrow(d) < 2) return(NULL)
  raw_time = .axivity_csv_time(d[[1]], tz)
  cols = if (ncol(d) >= 7) 2:7 else 2:4
  raw = as.matrix(d[, cols])
  storage.mode(raw) = "double"
  # resample onto the nominal frequency, as GGIR does
  grid = t0 + (0:floor((raw_time[length(raw_time)] - t0) * sf + 1e-6)) / sf
  res = GGIRread::resample(raw, raw_time, grid, nrow(raw), 1L)
  out = data.frame(time = grid[seq_len(nrow(res))], res)
  names(out) = c("time", c("x", "y", "z", "gx", "gy", "gz")[seq_along(cols)])
  out
}

# ---- Movisens (Unisens) -----------------------------------------------------

.movisens_header = function(file, tz) {
  folder = if (dir.exists(file)) file else dirname(file)
  acc_file = file.path(folder, "acc.bin")
  if (!file.exists(acc_file)) stop("acc.bin not found in ", folder, call. = FALSE)
  xml = paste(readLines(file.path(folder, "unisens.xml"), warn = FALSE), collapse = " ")
  attr_of = function(text, name) {
    m = regmatches(text, regexpr(paste0(name, "=\"[^\"]*\""), text))
    if (length(m)) sub(paste0(name, "=\"([^\"]*)\""), "\\1", m) else NA_character_
  }
  entry = regmatches(xml, regexpr("<signalEntry[^>]*id=\"acc.bin\"[^>]*>", xml))
  sf = as.numeric(attr_of(entry, "sampleRate"))
  start = as.POSIXct(attr_of(xml, "timestampStart"), tz = tz,
                     format = "%Y-%m-%dT%H:%M:%OS")
  serial_tag = regmatches(xml, regexpr("sensorSerialNumber\"[^>]*", xml))
  serial = if (length(serial_tag)) attr_of(serial_tag, "value") else NA_character_
  if (is.na(sf) || is.na(start)) {
    stop("Sample rate or start time not found in unisens.xml.", call. = FALSE)
  }
  list(folder = folder, acc_file = acc_file, sf = sf, start = as.numeric(start),
       n = unisensR::getUnisensSignalSampleCount(folder, "acc.bin"),
       serial = serial)
}

.read_movisens = function(header) {
  h = header$device_header
  d = .try_reader(h$acc_file, unisensR::readUnisensSignalEntry(
    h$folder, "acc.bin", startIndex = 1, endIndex = h$n))
  data.frame(time = header$start + (seq_len(nrow(d)) - 1) / header$sf,
             x = d[[1]], y = d[[2]], z = d[[3]])
}

.standardise_raw = function(data, format) {
  empty = data.frame(time = numeric(0), x = numeric(0), y = numeric(0),
                     z = numeric(0))
  if (is.null(data) || NROW(data) == 0) return(empty)
  data = as.data.frame(data)
  rename = c(acc_x = "x", acc_y = "y", acc_z = "z",
             gyro_x = "gx", gyro_y = "gy", gyro_z = "gz",
             temp = "temperature", bodySurface_temp = "temperature")
  hit = names(data) %in% names(rename)
  names(data)[hit] = rename[names(data)[hit]]
  cols = intersect(c("time", "x", "y", "z", "gx", "gy", "gz",
                     "temperature", "light"), names(data))
  data = data[, cols, drop = FALSE]
  data$time = as.numeric(data$time)
  data
}
