#' Function to calculate and export step metrics.
#' @description This function loads the minute-based files containing the step counts,
#'                calculate the step and cadence metrics and store them in day-level and person-level csv files.
#'
#' @param datadir Character (no default). Directory where the files containing the steps data are stored, e.g., "C:/mydata/".
#' @param outputdir Character (no default). Directory where the output needs to be stored. Note that this function will attempt to create folders in this directory and uses those folder to keep output.
#' @param threshold.MOD Numeric (default = 100)
#' @param threshold.VIG Numeric (default = 130)
#' @param includedaycrit Numeric (default = 10)
#' @param exclude_pk30_0 Logical (default = TRUE)
#' @param exclude_pk60_0 Logical (default = TRUE)
#' @param time_format Character (default = NULL)
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
                        id_sep = "_",
                        threshold.MOD=100, threshold.VIG=130,
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
    ids[i] = unlist(strsplit(files[i], split = id_sep))[1]
  }
  ids = unique(ids)

  print("Calculating features per day")

  #Loop through the files
  for (i in 1:length(ids)) {
    # read data ----
    files2read = grep(ids[i], files_fn, value = TRUE)
    data = readFile(files2read)

    # find midnights
    t = data$timestamp
    mnightsi = grep("00:00:00", t, fixed = T)
    day = rep(NA, times=nrow(data))
    d=1
    #Loop through the rest of the days
    for(j in 1:length(mnightsi)){
      if (j < length(mnightsi)){
        if (j == 1 & mnightsi[j] > 1){
          day[1:mnightsi[j]] = d
        } else if(j == 1 & mnightsi[j] == 1){
          next()
        } else {
          day[mnightsi[j-1]:mnightsi[j]] = d
        }
        d=d+1
      } else {
        day[mnightsi[j-1]:mnightsi[j]] = d; d = d+1
        day[mnightsi[j]:length(day)] = d
      }
    }
    #Loop through days to calculate variables
    date = date_num = wday_num = record.min = stepsperday  = pk60min = N0s_pk60min = N0s_pk30min = pk30min = pk1min = NA
    band_0 = band_1_19 = band_20_39 = band_40_59 = band_60_79 = band_80_99 = band_100_119 = band_120_higher = NA
    MPA = VPA = MVPA = NA
    for (di in 1:length(unique(day))){
      #Date
      date[di] = strsplit(t[which(day==di)[1]], "T")[[1]][1]
      wday_num[di] = format(as.POSIXct(data[which(day==di)[1],1]),"%u")
      #Duration of the day (number of minutes recorded)
      record.min[di] = length(data[which(day==di),"steps"])
      #Steps/day
      stepsperday[di] = sum(data[which(day==di),"steps"])
      #Peaks cadence
      pk60min[di] = mean(sort(data[which(day==di),"steps"], decreasing = TRUE)[1:60])
      N0s_pk60min[di] = length(which(sort(data[which(day==di),"steps"], decreasing = TRUE)[1:60]==0))
      pk30min[di] = mean(sort(data[which(day==di),"steps"], decreasing = TRUE)[1:30])
      N0s_pk30min[di] = length(which(sort(data[which(day==di),"steps"], decreasing = TRUE)[1:30]==0))
      pk1min[di] = max(data[which(day==di),"steps"])
      #Cadence band levels
      band_0[di] = length(which(data[which(day==di),"steps"]==0))
      band_1_19[di] = length(which(data[which(day==di),"steps"] < 20 & data[which(day==di),"steps"] > 0))
      band_20_39[di] = length(which(data[which(day==di),"steps"] < 40 & data[which(day==di),"steps"] >= 20))
      band_40_59[di] = length(which(data[which(day==di),"steps"] < 60 & data[which(day==di),"steps"] >= 40))
      band_60_79[di] = length(which(data[which(day==di),"steps"] < 80 & data[which(day==di),"steps"] >= 60))
      band_80_99[di] = length(which(data[which(day==di),"steps"] < 100 & data[which(day==di),"steps"] >= 80))
      band_100_119[di] = length(which(data[which(day==di),"steps"] < 120 & data[which(day==di),"steps"] >= 100))
      band_120_higher[di] = length(which(data[which(day==di),"steps"] >= 120))
      #MPA, VPA, MVPA
      MPA[di] = length(which(data[which(day==di),"steps"] < threshold.VIG & data[which(day==di),"steps"] >= threshold.MOD))
      VPA[di] = length(which(data[which(day==di),"steps"] >= threshold.VIG))
      MVPA[di] = length(which(data[which(day==di),"steps"] >= threshold.MOD))
    }
    ##OUTPUT PER DAY
    names.out = c("id","date","wday_num","dur_day_min","th_MOD","th_VIG",
                  "stepsperday","CAD_pk60_spm","CAD_N0s_pk60_spm","CAD_pk30_spm", "CAD_N0s_pk30_spm","CAD_pk1_spm",
                  "band_CAD_0_min","band_CAD_1-19_min","band_CAD_20-39_min","band_CAD_40-59_min",
                  "band_CAD_60-79_min","band_CAD_80-99_min","band_CAD_100-119_min","band_CAD_120+_min",
                  "MPA_min","VPA_min","MVPA_min")

    daily.out = data.frame(matrix(NA, length(unique(day[which(is.na(day)==FALSE)])), length(names.out)))
    colnames(daily.out) = names.out

    fi=1
    daily.out[,fi] = files[i]; fi=fi+1
    daily.out[,fi:(fi+1)] = cbind(date, as.numeric(wday_num)); fi=fi+2
    daily.out[,fi] = record.min; fi=fi+1
    daily.out[,fi:(fi+1)] = cbind(rep(threshold.MOD, times=nrow(daily.out)),rep(threshold.VIG, times=nrow(daily.out))); fi=fi+2
    daily.out[,fi:ncol(daily.out)] = cbind(stepsperday, pk60min, N0s_pk60min, pk30min, N0s_pk30min, pk1min,
                                           band_0, band_1_19, band_20_39, band_40_59, band_60_79,
                                           band_80_99, band_100_119, band_120_higher,
                                           MPA, VPA, MVPA)
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
    output[i,fi:(fi+1)] = c(threshold.MOD, threshold.VIG); fi=fi+2

    for (mi in 7:ncol(daily.out)){
      output[i,fi] = mean(D[,mi]); fi=fi+1
      output[i,fi] = ((mean(D[which(D$wday_num < 6), mi]) * 5) + (mean(D[which(D$wday_num >= 6), mi]) * 2)) / 7; fi=fi+1
    }
    print(i)
  }

  write.csv(output, file = paste0(outputdir,"/personSummary.csv"), row.names = FALSE)
}
