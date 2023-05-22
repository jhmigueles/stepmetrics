#' Function to calculate and export step metrics.
#' @description This function loads the minute-based files containing the step counts,
#'                calculate the step and cadence metrics and store them in day-level and person-level csv files.
#'
#' @param datadir Character (no default). Directory where the files containing the steps data are stored, e.g., "C:/mydata/". If you are trying to process GGIR output, then this should be the output directory from GGIR (name starts with "output_").
#' @param outputdir Character (no default). Directory where the output needs to be stored. Note that this function will attempt to create folders in this directory and uses those folder to keep output.
#' @param cadence_MOD Numeric (default = 100)
#' @param cadence_VIG Numeric (default = 130)
#' @param includedaycrit Numeric (default = 10)
#' @param exclude_pk30_0 Logical (default = TRUE)
#' @param exclude_pk60_0 Logical (default = TRUE)
#' @param time_format Character (default = NULL)
#' @param idloc Character (default = "_"). The ID is expected to be before the character(s) specified by \code{idloc} in the filename
#' @param cadence_bands Numeric (default = c(0, 1, 20, 40, 60, 80, 100, 120, Inf)). Cadence bands to calculate the time accumulated into.
#' @param cadence_peaks Numeric (default = c(1, 30, 60)). Cadence peaks to calculate.
#' @param verbose logical (default = TRUE). Whether to print progress messages in the console.
#'
#' @return This function does not return any object. It stores csv files with the
#'          day-level and person-level data in the output directory.
#' @export
#'
#' @examples
#' \dontrun{
#' step.metrics(datadir = "C:/mydata/", outputdir = "D:/myoutput/")
#' }
#' @importFrom utils write.csv
step.metrics = function(datadir, outputdir="./",
                        idloc = "_",
                        cadence_bands = c(0, 1, 20, 40, 60, 80, 100, 120, Inf),
                        cadence_peaks = c(1, 30, 60),
                        cadence_MOD = 100,
                        cadence_VIG = 130,
                        includedaycrit = 10,
                        exclude_pk30_0 = TRUE,
                        exclude_pk60_0 = TRUE,
                        time_format = NULL,
                        verbose = TRUE) {

  # is GGIR output? -----
  isGGIR = isGGIRoutput(datadir)
  if (isGGIR == TRUE) datadir = file.path(datadir, "meta/ms2.out/")

  # Files to analyse ----
  files_fn = dir(datadir, pattern = "*.csv|*.agd|*.RData", full.names = TRUE)
  files = dir(datadir, pattern = "*.csv|*.agd|*.RData")

  # Get IDs -----
  ids = c()
  for (i in 1:length(files)) {
    ids[i] = unlist(strsplit(files[i], split = idloc))[1]
  }
  ids = unique(ids)

  if (verbose == TRUE) {
    cat('\n')
    cat(paste0(rep('_', options()$width), collapse = ''))
    cat("\nCalculating features per day...\n")
  }

  #Loop through the files
  if (verbose == TRUE) cat("Processing participant: ")
  for (i in 1:length(ids)) {
    if (verbose == TRUE) cat(paste0(ids[i], " "))
    # read data ----
    files2read = grep(ids[i], files_fn, value = TRUE)
    data = readFile(files2read)

    # create vector with day indices -----
    day = define_day_indices(data$timestamp)

    #Loop through days to calculate variables
    for (di in 1:length(unique(day))) {

      # ID
      id = ids[i]

      #Date
      if (di == 1) date = wday = wday_num = c()
      date[di] = strsplit(data$timestamp[which(day == di)[1]], "T")[[1]][1]
      wday[di] = format(as.POSIXct(data[which(day == di)[1],1]),"%a")
      wday_num[di] = format(as.POSIXct(data[which(day == di)[1],1]),"%u")

      #Duration of the day (number of minutes recorded)
      if (di == 1) record.min = c()
      record.min[di] = length(data[which(day == di),"steps"])

      #Steps/day
      if (di == 1) stepsperday = c()
      stepsperday[di] = sum(data[which(day == di),"steps"])

      #Peaks cadence
      if (di == 1) CAD_peaks = list()
      if (di == 1) CAD_peaks_spm = c()
      CAD_peaks[[di]] = get_cadence_peaks(x = data[which(day == di),"steps"], peaks = cadence_peaks)
      CAD_peaks_spm = rbind(CAD_peaks_spm, CAD_peaks[[di]]$values)

      #Cadence band levels
      if (di == 1) CAD_bands = list()
      if (di == 1) CAD_bands_spm = c()
      CAD_bands[[di]] = get_cadence_bands(x = data[which(day == di),"steps"], bands = cadence_bands)
      CAD_bands_spm = rbind(CAD_bands_spm, CAD_bands[[di]]$values)

      #MPA, VPA, MVPA
      if (di == 1) MPA = VPA = MVPA = c()
      MPA[di] = length(which(data[which(day == di),"steps"] < cadence_VIG & data[which(day == di),"steps"] >= cadence_MOD))
      VPA[di] = length(which(data[which(day == di),"steps"] >= cadence_VIG))
      MVPA[di] = length(which(data[which(day == di),"steps"] >= cadence_MOD))
    }

    ##OUTPUT PER DAY
    names.out = c("ID", "date", "weekday", "weekday_num", "dur_day_min", "threshold_MOD_spm", "threshold_VIG_spm",
                  "stepsperday", CAD_peaks[[1]]$names, CAD_bands[[1]]$names,
                  "MPA_min","VPA_min","MVPA_min")

    daily.out = data.frame(matrix(NA, length(unique(day[which(is.na(day) == FALSE)])), length(names.out)))
    colnames(daily.out) = names.out

    fi = 1
    daily.out[,fi] = id; fi = fi + 1
    daily.out[,fi:(fi + 2)] = cbind(date, wday, as.numeric(wday_num)); fi = fi + 3
    daily.out[,fi] = record.min; fi = fi + 1
    daily.out[,fi:(fi + 1)] = cbind(rep(cadence_MOD, times = nrow(daily.out)), rep(cadence_VIG, times = nrow(daily.out))); fi = fi + 2
    daily.out[,fi:ncol(daily.out)] = cbind(stepsperday, CAD_peaks_spm, CAD_bands_spm, MPA, VPA, MVPA)
    # Create output directory
    if (dir.exists(outputdir) == FALSE) {
      dir.create(outputdir)
    }
    if (dir.exists(paste0(outputdir,"/daySummary")) == FALSE) {
      dir.create(paste0(outputdir,"/daySummary/"))
    }
    utils::write.csv(daily.out, file = paste0(outputdir,"/daySummary/", id, "_DaySum",".csv"), row.names = F)
  }

  ################################################################################################################################
  #Calculate means per week plain and weighted

  if (verbose == TRUE) {
    cat('\n')
    cat(paste0(rep('_', options()$width), collapse = ''))
    cat("\nCalculating means per recording\n")
  }

  files = dir(paste0(outputdir, "/daySummary"))

  names.out.2 = c("ID","start_date", "valid_days","valid_days_WD", "valid_days_WE", "threshold_MOD_spm","threshold_VIG_spm",
                  paste0(c("stepsperday", CAD_peaks[[1]]$names, CAD_bands[[1]]$names,
                           "MPA_min", "VPA_min", "MVPA_min"), "_pla"),
                  paste0(c("stepsperday", CAD_peaks[[1]]$names, CAD_bands[[1]]$names,
                           "MPA_min", "VPA_min", "MVPA_min"), "_wei"),
                  paste0(c("stepsperday", CAD_peaks[[1]]$names, CAD_bands[[1]]$names,
                           "MPA_min", "VPA_min", "MVPA_min"), "_WD"),
                  paste0(c("stepsperday", CAD_peaks[[1]]$names, CAD_bands[[1]]$names,
                           "MPA_min", "VPA_min", "MVPA_min"), "_WE"))

  output = data.frame(matrix(NA, length(files), length(names.out.2)))
  colnames(output) = names.out.2
  #Loop through files to calculate mean variables
  for (i in 1:length(files)) {
    if (verbose == TRUE) cat("Processing participant: ", gsub("_DaySum.csv", "", files[i]), " ")
    D = read.csv(paste0(outputdir,"/daySummary/", files[i]))
    exclude = sum(D$dur_day_min < includedaycrit * 60)
    if (exclude > 0) D = D[-which(D$dur_day_min < includedaycrit * 60),]
    if (exclude_pk30_0 == TRUE) {
      zeroes = sum(D$CAD_nZeroes_pk30 > 0)
      if (zeroes > 0) D = D[-which(D$CAD_nZeroes_pk30 > 0),]
    }
    if (exclude_pk60_0 == TRUE) {
      zeroes = sum(D$CAD_nZeroes_pk60 > 0)
      if (zeroes > 0) D = D[-which(D$CAD_nZeroes_pk60 > 0),]
    }
    fi = 1                                                  #fi is the column of the new output data frame
    output[i,fi] = D[1, 1]; fi = fi + 1
    output[i,fi] = D[1,"date"]; fi = fi + 1
    output[i,fi] = nrow(D); fi = fi + 1
    output[i,fi] = sum(D$weekday_num < 6); fi = fi + 1
    output[i,fi] = sum(D$weekday_num >= 6); fi = fi + 1
    output[i,fi:(fi + 1)] = c(cadence_MOD, cadence_VIG); fi = fi + 2

    # averages
    for (mi in 8:ncol(D)) {
      columns = grep(colnames(D)[mi], colnames(output), value = TRUE)
      # plain
      fi = grep("_pla", columns, value = TRUE)
      output[i,fi] = mean(D[,mi])
      # weighted
      fi = grep("_wei", columns, value = TRUE)
      output[i,fi] = ((mean(D[which(D$weekday_num < 6), mi]) * 5) + (mean(D[which(D$weekday_num >= 6), mi]) * 2)) / 7
      # weekdays
      fi = grep("_WD", columns, value = TRUE)
      output[i,fi] = mean(D[which(D$weekday_num < 6), mi])
      # weekend days
      fi = grep("_WE", columns, value = TRUE)
      output[i,fi] = mean(D[which(D$weekday_num >= 6), mi])
    }
  }

  if (verbose == TRUE) {
    cat('\n')
    cat(paste0(rep('_', options()$width), collapse = ''))
    cat("\nStoring output...\n")
  }

  utils::write.csv(output, file = paste0(outputdir,"/personSummary.csv"), row.names = FALSE)

  if (verbose == TRUE) {
    cat('\n')
    cat("\nAnalyses complete!\n
        Your output is stored here: ", outputdir, "\n")
  }
}
