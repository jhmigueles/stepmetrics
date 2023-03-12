#' Function to calculate and export step metrics.
#' @description This function loads the minute-based files containing the step counts,
#'                calculate the step and cadence metrics and store them in day-level and person-level csv files.
#'
#' @param datadir Character (no default). Directory where the files containing the steps data are stored, e.g., "C:/mydata/".
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
#'
#' @return This function does not return any object. It stores csv files with the
#'          day-level and person-level data in the output directory.
#' @export
#'
#' @examples
#' \dontrun{
#' step.metrics(datadir = "C:/mydata/", outputdir = "D:/myoutput/")
#' }
step.metrics = function(datadir, outputdir="./",
                        idloc = "_",
                        cadence_bands = c(0, 1, 20, 40, 60, 80, 100, 120, Inf),
                        cadence_peaks = c(1, 30, 60),
                        cadence_MOD = 100,
                        cadence_VIG = 130,
                        includedaycrit = 10,
                        exclude_pk30_0 = TRUE,
                        exclude_pk60_0 = TRUE,
                        time_format = NULL){

  # Files to analyse ----
  files_fn = dir(datadir, pattern = "*.csv|*.agd", full.names = TRUE)
  files = dir(datadir, pattern = "*.csv|*.agd")

  # Get IDs -----
  ids = c()
  for (i in 1:length(files)) {
    ids[i] = unlist(strsplit(files[i], split = idloc))[1]
  }
  ids = unique(ids)

  print("Calculating features per day")

  #Loop through the files
  for (i in 1:length(ids)) {
    # read data ----
    files2read = grep(ids[i], files_fn, value = TRUE)
    data = readFile(files2read)

    # create vector with day indices -----
    day = define_day_indices(data$timestamp)

    #Loop through days to calculate variables
    for (di in 1:length(unique(day))){

      # ID
      id = ids[i]

      #Date
      if (di == 1) date = wday = wday_num = c()
      date[di] = strsplit(data$timestamp[which(day==di)[1]], "T")[[1]][1]
      wday[di] = format(as.POSIXct(data[which(day==di)[1],1]),"%a")
      wday_num[di] = format(as.POSIXct(data[which(day==di)[1],1]),"%u")

      #Duration of the day (number of minutes recorded)
      if (di == 1) record.min = c()
      record.min[di] = length(data[which(day==di),"steps"])

      #Steps/day
      if (di == 1) stepsperday = c()
      stepsperday[di] = sum(data[which(day==di),"steps"])

      #Peaks cadence
      if (di == 1) CAD_peaks = list()
      if (di == 1) CAD_peaks_spm = c()
      CAD_peaks[[di]] = get_cadence_peaks(x = data[which(day==di),"steps"], peaks = cadence_peaks)
      CAD_peaks_spm = rbind(CAD_peaks_spm, CAD_peaks[[di]]$values)

      #Cadence band levels
      if (di == 1) CAD_bands = list()
      if (di == 1) CAD_bands_spm = c()
      CAD_bands[[di]] = get_cadence_bands(x = data[which(day==di),"steps"], bands = cadence_bands)
      CAD_bands_spm = rbind(CAD_bands_spm, CAD_bands[[di]]$values)

      #MPA, VPA, MVPA
      if (di == 1) MPA = VPA = MVPA = c()
      MPA[di] = length(which(data[which(day==di),"steps"] < cadence_VIG & data[which(day==di),"steps"] >= cadence_MOD))
      VPA[di] = length(which(data[which(day==di),"steps"] >= cadence_VIG))
      MVPA[di] = length(which(data[which(day==di),"steps"] >= cadence_MOD))
    }

    ##OUTPUT PER DAY
    names.out = c("id", "date", "weekday", "weekday_num", "dur_day_min", "threshold_MOD_spm", "threshold_VIG_spm",
                  "stepsperday", CAD_peaks[[1]]$names, CAD_bands[[1]]$names,
                  "MPA_min","VPA_min","MVPA_min")

    daily.out = data.frame(matrix(NA, length(unique(day[which(is.na(day)==FALSE)])), length(names.out)))
    colnames(daily.out) = names.out

    fi=1
    daily.out[,fi] = id; fi=fi+1
    daily.out[,fi:(fi+2)] = cbind(date, wday, as.numeric(wday_num)); fi=fi+3
    daily.out[,fi] = record.min; fi=fi+1
    daily.out[,fi:(fi+1)] = cbind(rep(cadence_MOD, times=nrow(daily.out)),rep(cadence_VIG, times=nrow(daily.out))); fi=fi+2
    daily.out[,fi:ncol(daily.out)] = cbind(stepsperday, CAD_peaks_spm, CAD_bands_spm, MPA, VPA, MVPA)
    # Create output directory
    if(dir.exists(outputdir)==FALSE) {
      dir.create(outputdir)
    }
    if(dir.exists(paste0(outputdir,"/daySummary"))==FALSE) {
      dir.create(paste0(outputdir,"/daySummary/"))
    }
    write.csv(daily.out, file = paste0(outputdir,"/daySummary/",files[i],"_DaySum",".csv"), row.names = F)
    print(i)
  }

  ################################################################################################################################
  #Calculate means per week plain and weighted

  print("Calculating means per week")

  files = dir(paste0(outputdir,"/daySummary"))

  names.out.2 = c("id","start_date", "valid_days","valid_days_WD", "valid_days_WE",
                  "th_MOD","th_VIG",
                  "stepsperday_pla","stepsperday_wei",
                  "CAD_pk60_spm_pla","CAD_pk60_spm_wei", "CAD_N0s_pk60_spm_pla", "CAD_N0s_pk60_spm_wei",
                  "CAD_pk30_spm_pla","CAD_pk30_spm_wei", "CAD_N0s_pk30_spm_pla", "CAD_N0s_pk30_spm_wei",
                  "CAD_pk1_spm_pla","CAD_pk1_spm_wei",
                  "band_CAD_0_min_pla", "band_CAD_0_min_wei","band_CAD_1-19_min_pla","band_CAD_1-19_min_wei",
                  "band_CAD_20-39_min_pla","band_CAD_20-39_min_wei","band_CAD_40-59_min_pla","band_CAD_40-59_min_wei",
                  "band_CAD_60-79_min_pla","band_CAD_60-79_min_wei","band_CAD_80-99_min_pla","band_CAD_80-99_min_wei",
                  "band_CAD_100-119_min_pla","band_CAD_100-119_min_wei","band_CAD_120+_min_pla","band_CAD_120+_min_wei",
                  "MPA_min_pla","MPA_min_wei","VPA_min_pla","VPA_min_wei","MVPA_min_pla","MVPA_min_wei")

  output = data.frame(matrix(NA, length(files), length(names.out.2)))
  colnames(output) = names.out.2
  #Loop through files to calculate mean variables
  for (i in 1:length(files)){
    D = read.csv(paste0(outputdir,"/daySummary/", files[i]))
    exclude = sum(D$dur_day_min < includedaycrit * 60)
    if(exclude > 0) D = D[-which(D$dur_day_min < includedaycrit * 60),]
    if(exclude_pk30_0 == TRUE){
      zeroes = sum(D$CAD_N0s_pk30_spm > 0)
      if(zeroes > 0) D = D[-which(D$CAD_N0s_pk30_spm > 0),]
    }
    if(exclude_pk60_0 == TRUE){
      zeroes = sum(D$CAD_N0s_pk60_spm > 0)
      if(zeroes > 0) D = D[-which(D$CAD_N0s_pk60_spm > 0),]
    }
    fi=1                                                  #fi is the column of the new output data frame
    output[i,fi] = files[i]; fi=fi+1
    output[i,fi] = D[1,"date"]; fi=fi+1
    output[i,fi] = nrow(D); fi=fi+1
    output[i,fi] = sum(D$wday_num < 6); fi=fi+1
    output[i,fi] = sum(D$wday_num >= 6); fi=fi+1
    output[i,fi:(fi+1)] = c(cadence_MOD, cadence_VIG); fi=fi+2

    for (mi in 7:ncol(daily.out)){
      output[i,fi] = mean(D[,mi]); fi=fi+1
      output[i,fi] = ((mean(D[which(D$wday_num < 6), mi]) * 5) + (mean(D[which(D$wday_num >= 6), mi]) * 2)) / 7; fi=fi+1
    }
    print(i)
  }

  write.csv(output, file = paste0(outputdir,"/personSummary.csv"), row.names = FALSE)
}
