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
    tryFormats = c("%Y-%m-%d %H:%M:%S",
                   "%Y/%m/%d %H:%M:%S",
                   "%Y-%m-%d %H:%M:%OS",
                   "%Y/%m/%d %H:%M:%OS",
                   "%Y-%m-%d %H:%M",
                   "%Y/%m/%d %H:%M",
                   "%m/%d/%Y %H:%M",
                   "%d/%m/%Y %H:%M:%S",
                   "%Y-%m-%d",
                   "%Y/%m/%d")

    if (is.null(time_format)) {
      POStime = as.POSIXlt(as.numeric(as.POSIXlt(x,tz, tryFormats = tryFormats)), origin = "1970-01-01", tz)
    } else if (!is.null(time_format)) {
      POStime = as.POSIXlt(as.numeric(as.POSIXlt(x,tz, format = time_format)), origin = "1970-01-01", tz)
    }
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
  }

  # set up object to return ----
  cleanData = data.frame(timestamp = rep(NA, nrow(data)),
                         steps = rep(NA, nrow(data)))

  # find timestamp column/s -----
  colnames(data) = tolower(colnames(data))
  timestamp_tmp = grep("date|time", colnames(data), value = TRUE)
  if (length(timestamp_tmp) == 1) {
    ts = data[, timestamp_tmp]
    print(paste("read from agd", ts[1:2]))
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
  print(paste("as character", as.character(ts)[1:2]))
  cleanData$timestamp = chartime2iso8601(as.character(ts), tz = "", time_format = time_format)
  print(paste("conv2ISO", cleanData$timestamp[1:2]))

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
    print(cleanData$timestamp[1:2])
    print(ts[1:2])
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
