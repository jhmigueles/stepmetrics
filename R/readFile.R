#' Reads and formats the timestamp and step data for one participant.
#'
#' @param path Character (no default). Path to file or files with the timestamp and step data for one participant.
#' @param time_format Character (no default, optional). String indicating the format of the timestamp in data (as in strptime)
#'
#' @return Data frame containing timestamp and steps in a clean format for one participant.
#' @export
#'
#' @examples
#' \dontrun{
#' readFile(path)
#' }
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom stats aggregate
#' @import PhysicalActivity
#' @import RSQLite
#'
readFile = function(path, time_format = c()) {

  # function to handle timestamp format
  chartime2iso8601 = function(x,tz = "", time_format = c()){
    # try formats if not provided
    tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
                   "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M",
                   "%Y-%m-%d %H:%M:%OS",
                   "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M")

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
    test = utils::read.csv(file, nrows = 2)

    # check if it is an ActiGraph file, then it would need to skip 10 rows
    isActiGraph = any(grepl("ActiGraph", colnames(test)))

    # define skip and header for specific formats of actigraph
    skip = 0; header = TRUE
    column_names = NULL; startdate = NULL; starttime = NULL

    if (isActiGraph) {
      # identify startdate and starttime
      test = utils::read.csv(file, nrows = 9)
      d0 = grep("start date", test[,1], ignore.case = TRUE)
      t0 = grep("start time", test[,1], ignore.case = TRUE)
      startdate = gsub("start date ", "", test[d0,1], ignore.case = TRUE)
      starttime = gsub("start time ", "", test[t0,1], ignore.case = TRUE)

      # redefine skip
      skip = 10
      test = utils::read.csv(file, nrows = 2, skip = skip, header = header)

      # header?
      if (colnames(test)[1] != "Date") { # no header
        header = FALSE
        test = utils::read.csv(file, nrows = 2, skip = skip, header = header)
        if (ncol(test) == 9) {
          column_names = c("Axis1", "Axis2", "Axis3", "Steps",
                           "Lux", "Inclinometer Off", "Inclinometer Standing",
                           "Inclinometer Sitting", "Inclinometer Lying")
        } else if (ncol(test) == 12) {
          column_names = c("Date", "Time", "Axis1", "Axis2", "Axis3", "Steps",
                           "Lux", "Inclinometer Off", "Inclinometer Standing",
                           "Inclinometer Sitting", "Inclinometer Lying", "Vector Magnitude")
        }
      }
    }

    # check separator for csv file
    if (ncol(test) >= 2) {
      sep = ","
    } else {
      #check semicolon
      test = utils::read.csv(file, nrows = 2, sep = ";", header = header)
      if (ncol(test) >= 2) {
        sep = ";"
      } else {
        sep = "\t"
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
  }

  # set up object to return ----
  cleanData = data.frame(timestamp = rep(NA, nrow(data)),
                         steps = rep(NA, nrow(data)))

  # find timestamp column/s -----
  colnames(data) = tolower(colnames(data))
  timestamp_tmp = grep("date|time", colnames(data), value = TRUE)
  if (length(timestamp_tmp) == 1) {
    ts = data[, timestamp_tmp]
  } else if (length(timestamp_tmp) == 2) {
    # date and time separated: colon split should return a vector of
    # length 1 for date and length 3 for time
    colonSplit1 = length(unlist(strsplit(data[1, timestamp_tmp[1]], split = ":")))
    colonSplit2 = length(unlist(strsplit(data[1, timestamp_tmp[2]], split = ":")))
    date_column = timestamp_tmp[which(c(colonSplit1, colonSplit2) == 1)]
    time_column = timestamp_tmp[which(c(colonSplit1, colonSplit2) == 3)]
    # define timestamp
    ts = paste(data[, date_column], data[, time_column])
  } else if (length(timestamp_tmp) == 0) {
    ts0 = as.POSIXlt(paste(startdate, starttime), tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                                                 "%Y/%m/%d %H:%M:%OS",
                                                                 "%Y-%m-%d %H:%M",
                                                                 "%Y/%m/%d %H:%M",
                                                                 "%m/%d/%Y %H:%M",
                                                                 "%d/%m/%Y %H:%M:%S",
                                                                 "%Y-%m-%d",
                                                                 "%Y/%m/%d"))
    ts = seq(from = ts0, by = 30, length.out = nrow(cleanData))
  }
  cleanData$timestamp = chartime2iso8601(as.character(ts), tz = "", time_format = time_format)

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
