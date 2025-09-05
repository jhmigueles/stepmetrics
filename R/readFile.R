#' Read and standardize minute-level step data for one participant
#'
#' @description
#' Reads one or more files for a single participant and returns a clean,
#' minute-level data frame with two columns: `timestamp` and `steps`.
#' The function auto-detects common file formats and timestamp layouts,
#' fixes ActiGraph CSV headers/metadata when present, and aggregates to a
#' 60-second epoch if input data are recorded at sub-minute resolution.
#'
#' **Supported input formats**
#' \itemize{
#'   \item \strong{CSV}: generic CSVs and ActiGraph exports (header lines
#'         and delimiters auto-detected; handles date/time split columns).
#'   \item \strong{AGD}: ActiGraph binary files via \pkg{PhysicalActivity}.
#'   \item \strong{RData}: GGIR output (\code{IMP$metashort}).
#' }
#'
#' @param path Character vector. Path(s) to the file(s) containing timestamp
#'   and step data for one participant. When multiple files are provided,
#'   they are concatenated in the order given.
#' @param time_format Character (optional). Explicit timestamp format string
#'   (as used by \code{\link[base]{strptime}}) to override auto-detection
#'   for CSV inputs. If \code{NULL}, common formats are tried automatically.
#'
#' @return A \code{data.frame} with two columns:
#' \describe{
#'   \item{\code{timestamp}}{Character vector of ISO-8601 datetimes
#'     (\code{"YYYY-MM-DDTHH:MM:SS±ZZZZ"}) for CSV/AGD inputs. For GGIR
#'     \code{RData} inputs, timestamps are carried through as present in
#'     \code{IMP$metashort}.}
#'   \item{\code{steps}}{Numeric vector of steps per minute. If the source
#'     data have sub-minute epochs, values are summed to 60-second bins.
#'     Epochs longer than 60 seconds are not supported and trigger an error.}
#' }
#'
#' @details
#' \itemize{
#'   \item \strong{CSV handling:} The function detects and skips ActiGraph
#'         header lines (typically 10), infers the field separator
#'         (comma/semicolon), and reconstructs a single timestamp when date
#'         and time are stored in separate columns. If no explicit timestamp
#'         column exists (rare ActiGraph cases), it reconstructs one from the
#'         file metadata (start time + epoch).
#'   \item \strong{AGD handling:} AGD files are read with
#'         \code{\link[PhysicalActivity]{readActigraph}}; the recording
#'         start and epoch length are obtained from the embedded database and
#'         used to build a regular timestamp sequence.
#'   \item \strong{Step column detection:} The column containing step counts
#'         is inferred by matching names containing \emph{"step"} or
#'         \emph{"value"}; if multiple candidates are present, the column with
#'         higher variability is chosen.
#'   \item \strong{Epoch standardization:} If the input epoch is shorter than
#'         60 seconds, rows are aggregated by summing steps to 1-minute bins.
#'         Epochs longer than 60 seconds are currently unsupported.
#' }
#'
#' @section Time zones:
#' For CSV/AGD inputs, timestamps are returned in ISO-8601 with an explicit
#' offset. If \code{time_format} is provided, it is passed directly to
#' \code{\link[base]{strptime}} for parsing; otherwise a set of common formats
#' is attempted.
#'
#' @examples
#' \dontrun{
#' # Single CSV
#' df <- readFile("data/participant_001.csv")
#'
#' # Multiple CSVs for one participant (concatenated)
#' df <- readFile(c("data/part001_day1.csv", "data/part001_day2.csv"))
#'
#' # ActiGraph AGD
#' df <- readFile("data/participant_001.agd")
#'
#' # GGIR output (RData containing IMP$metashort)
#' df <- readFile("output_subject001.RData")
#'
#' # Specify a custom timestamp format (CSV)
#' df <- readFile("data/custom.csv", time_format = "%d/%m/%Y %H:%M:%S")
#' }
#'
#' @seealso
#' \code{\link{step.metrics}}, \code{\link{get_cadence_bands}}
#'
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom stats aggregate
#' @import PhysicalActivity
#' @export
readFile = function(path, time_format = c()) {

  # function to handle timestamp format
  chartime2iso8601 = function(x,tz = "", time_format = c()){
    # try formats if not provided
    tryFormats = c("%Y-%m-%d %I:%M:%S %p", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
                   "%m/%d/%Y %I:%M:%S %p", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M",
                   "%d/%m/%Y %I:%M:%S %p", "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M",
                   "%Y-%m-%d %H:%M:%OS", "%d-%m-%Y %H:%M:%OS")

    if (is.null(time_format)) {
      POStime = as.POSIXlt(as.numeric(as.POSIXlt(x, tz, tryFormats = tryFormats)), origin = "1970-01-01", tz)
    } else if (!is.null(time_format)) {
      POStime = as.POSIXlt(as.numeric(as.POSIXlt(x, tz, format = time_format)), origin = "1970-01-01", tz)
    }
    # POStime = lubridate::as_datetime(x, format = tryFormats)
    POStimeISO = strftime(POStime, format = "%Y-%m-%dT%H:%M:%S%z")
    return(POStimeISO)
  }

  # handle multiple files per participant
  AllFiles = path
  file = path[1]

  # identify file extension
  format = tools::file_ext(file)

  # check separator for csv
  if (format == "csv") {
    # check separator of csv
    skip = tryCatch(
      {utils::read.csv(file, nrows = 2)
       skip = 0
      }, error = function(cond) {
        utils::read.csv(file, nrows = 2, skip = 1)
        skip = 1
      })

    test = utils::read.csv(file, nrows = 2, skip = skip)

    # check if it is an ActiGraph file, then it would need to skip 10 rows
    isActiGraph = FALSE
    if (skip == 0) {
      isActiGraph = any(grepl("ActiGraph", colnames(test)))
    } else if (skip == 1) {
      expected_columns = c("date", "epoch", "axis1")
      isActiGraph = all(expected_columns %in% colnames(test))
    }

    # define skip and header for specific formats of actigraph
    sep = ","; header = TRUE
    column_names = NULL; startdate = NULL; starttime = NULL

    if (isActiGraph) {
      # identify startdate and starttime
      test = utils::read.csv(file, nrows = 9, skip = skip)
      d0 = grep("start date", test[,1], ignore.case = TRUE)
      t0 = grep("start time", test[,1], ignore.case = TRUE)
      startdate = gsub("start date ", "", test[d0,1], ignore.case = TRUE)
      starttime = gsub("start time ", "", test[t0,1], ignore.case = TRUE)

      # does it have a header to skip?
      if (length(startdate) > 0) {
        skip = 10
      } else {
        startdate = test[1, "date"]
        starttime = test[1, "epoch"]
      }

      # redefine skip
      test = utils::read.csv(file, nrows = 2, skip = skip, header = header)

      # check separator for csv file
      if (ncol(test) < 2) {
        #check semicolon
        test = utils::read.csv(file, nrows = 2, skip = skip, header = header, sep = ";")
        if (ncol(test) >= 2) {
          sep = ";"
        }
      }

      # header?
      if (tolower(colnames(test)[1]) != "date") { # no header
        header = FALSE
        test = utils::read.csv(file, nrows = 2, sep = sep, skip = skip, header = header)
        column_names = c("Axis1", "Axis2", "Axis3", "Steps",
                         "Lux", "Inclinometer Off", "Inclinometer Standing",
                         "Inclinometer Sitting", "Inclinometer Lying")

      }
    }

    # check separator for csv file
    test = utils::read.csv(file, skip = skip, nrows = 2)
    if (ncol(test) >= 2) {
      sep = ","
    } else {
      #check semicolon
      test = utils::read.csv(file, nrows = 2, sep = ";", header = header)
      if (ncol(test) >= 2) {
        sep = ";"
      }
    }

    # read csv data (this will merge files if multiple per participant)
    data = c()
    for (i in 1:length(AllFiles)) {
      data_i = utils::read.csv(AllFiles[i], sep = sep, skip = skip, header = header)
      data = rbind(data, data_i)
    }
    if (!is.null(column_names)) {
      colnames(data) = column_names
    }
  } else if (format == "agd") {
    if (!requireNamespace("RSQLite", quietly = TRUE)) {
      stop("Reading .agd requires the 'RSQLite' package. Please install RSQLite.", call. = FALSE)
    }
    data = c()
    for (i in 1:length(AllFiles)) {
      data_i = PhysicalActivity::readActigraph(AllFiles[i])
      data = rbind(data, data_i)
    }
    # fix timestamp -------
    # Next lines of code are adapted from cran/PhysicalActivity
    # Here I hardcode the format of the timestamp to ease handling later on
    agdStartTime = function(datfile) {
      # extra function for time format
      timeFromYear1 = function(x, base=62135596800, tz = "UTC") {
        as.POSIXct(x %/% 1e7 - base, origin = "1970-01-01", tz = tz)
      }
      # code start time
      qry = "SELECT settingID, settingName, settingValue FROM settings"
      res = queryActigraph(datfile, qry)
      sqlFields = c('deviceserial', 'startdatetime', 'epochlength',
                    'downloaddatetime', 'batteryvoltage', 'modenumber',
                    'addresspointer')
      sqlValues = res[match(sqlFields, res[,'settingName']), 'settingValue']
      starttime = timeFromYear1(as.numeric(sqlValues[2]), tz = "UTC")
      downtime = timeFromYear1(as.numeric(sqlValues[4]))
      epoch = as.POSIXct("1970-01-01", tz = "") + as.numeric(sqlValues[3])
      metaKeys = c('Serial TimeStamp', 'Epoch Period (hh:mm:ss)')
      metaVals = character(length(metaKeys))
      names(metaVals) = metaKeys
      metaVals[1] = format(starttime, "%Y-%m-%d %H:%M:%S")
      metaVals[2] = format(epoch, "%H:%M:%S")
      return(metaVals)
    }
    meta_agd = agdStartTime(AllFiles[i])
    starttime = as.POSIXlt(meta_agd[1], format = "%Y-%m-%d %H:%M:%S", tz = "")
    epoch_tmp = as.numeric(unlist(strsplit(meta_agd[2], ":")))
    epoch = epoch_tmp[1]*(60^2) + epoch_tmp[2]*60 + epoch_tmp[3]
    data$TimeStamp = seq(starttime, by = epoch, length.out = nrow(data))
    time_format = "%Y-%m-%d %H:%M:%S"
  } else if (format == "RData") { # then GGIR output
    IMP = c()
    load(file)
    data = IMP$metashort
  }

  # set up object to return ----
  cleanData = data.frame(timestamp = rep(NA, nrow(data)),
                         steps = rep(NA, nrow(data)))

  # find timestamp column/s -----
  colnames(data) = tolower(colnames(data))
  timestamp_tmp = grep("date|time|epoch", colnames(data), value = TRUE)
  if (length(timestamp_tmp) == 1) {
    ts = data[, timestamp_tmp]
  } else if (length(timestamp_tmp) == 2) {
    # date and time separated: colon split should return a vector of
    # length 1 for date and length 3 for time (or 2 if seconds are not stored)
    colonSplit1 = length(unlist(strsplit(data[1, timestamp_tmp[1]], split = ":")))
    colonSplit2 = length(unlist(strsplit(data[1, timestamp_tmp[2]], split = ":")))
    date_column = timestamp_tmp[which(c(colonSplit1, colonSplit2) == 1)]
    time_column = timestamp_tmp[which(c(colonSplit1, colonSplit2) > 1)]
    # define timestamp
    ts = paste(data[, date_column], data[, time_column])
  } else if (length(timestamp_tmp) == 0) {
    ts0 = as.POSIXlt(paste(startdate, starttime), tryFormats = c("%Y-%m-%d %H:%M:%S",
                                                                 "%Y/%m/%d %H:%M:%S",
                                                                 "%d/%m/%Y %H:%M:%S",
                                                                 "%Y-%m-%d %H:%M",
                                                                 "%Y/%m/%d %H:%M",
                                                                 "%m/%d/%Y %H:%M",
                                                                 "%m/%d/%Y %H:%M:%S"))
    ts = seq(from = ts0, by = 30, length.out = nrow(cleanData))
    time_format = "%Y-%m-%d %H:%M:%S"
  }

  if (format != "RData") { # then, no GGIR, we need to reformat timestamp
    cleanData$timestamp = chartime2iso8601(as.character(ts), tz = "", time_format = time_format)
  } else {
    cleanData$timestamp = ts
  }

  # find steps column -------
  steps_tmp = grep("step|value", colnames(data), value = TRUE)
  if (length(steps_tmp) == 1) {
    cleanData$steps = data[, steps_tmp]
  } else {
    # look for steps per minute
    uniqueSteps1 = length(unique(data[, steps_tmp[1]]))
    uniqueSteps2 = length(unique(data[, steps_tmp[2]]))
    steps_tmp = steps_tmp[which.max(c(uniqueSteps1, uniqueSteps2))]
    cleanData$steps = data[, steps_tmp]
  }

  # aggregate to 1-min epoch if needed -----
  ts = as.POSIXlt(cleanData$timestamp[1:2], format = "%Y-%m-%dT%H:%M:%S%z")
  epoch = as.numeric(difftime(ts[2], ts[1], units = "secs"))
  if (epoch < 60) {
    # aggregate data
    nEpochs = floor(nrow(data) / (60/epoch))
    cleanData = cleanData[1:(nEpochs*(60/epoch)),]
    cleanData$agg = rep(1:nEpochs, each = 60/epoch)
    steps = stats::aggregate(steps ~ agg, data = cleanData, FUN = sum)
    # select timestamps and remove agg column
    cleanData = cleanData[seq(1, nrow(cleanData), by = 60/epoch), 1:2]
    # update steps
    cleanData$steps = steps$steps
  }
  if (epoch > 60) stop("This package cannot work with epoch lengths longer than 60 seconds for now.")

  # return
  return(cleanData)
}
