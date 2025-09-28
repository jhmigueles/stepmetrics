test_that("reads and formats data correctly", {
  testthat::skip_if_not_installed("RSQLite")

  withr::local_locale(c(LC_TIME = "C", LC_COLLATE = "C"))

  # ISO-8601 checker
  is.ISO8601 <- function(x) {
    grepl("^\\d{4}-\\d{2}-\\d{2}[ T]\\d{2}:\\d{2}:\\d{2}(?:Z|[+-]\\d{2}:?\\d{2})?$", x)
  }

  # helper to parse ISO-8601 to a UTC instant, handling Z and ±hh[:mm]
  parse_iso_utc <- function(x) {
    stopifnot(length(x) == 1L)
    rx <- "^([0-9]{4}-[0-9]{2}-[0-9]{2})[ T]([0-9]{2}:[0-9]{2}:[0-9]{2})(Z|([+-])([0-9]{2}):?([0-9]{2}))?$"
    m <- regexec(rx, x)
    g <- regmatches(x, m)[[1]]
    if (length(g) == 0L) return(as.POSIXct(NA))

    # groups: 1=full, 2=Y-m-d, 3=H:M:S, 4='Z' or offset, 5=sign, 6=hh, 7=mm
    dt <- paste(g[2], g[3])                           # "YYYY-mm-dd HH:MM:SS"
    t  <- as.POSIXct(dt, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

    # compute offset seconds
    off_sec <- 0L
    if (!is.na(g[4]) && nzchar(g[4]) && g[4] != "Z") {
      sign <- if (g[5] == "+") 1L else -1L
      hh   <- as.integer(g[6]); mm <- as.integer(g[7])
      off_sec <- sign * (hh * 3600L + mm * 60L)
    }
    t - off_sec
  }

  # tests ------
  # fitbit files -----
  file1 = c(system.file("extdata/testfiles_fitbit/S001_d1_1min_epoch.csv", package = "stepmetrics"),
            system.file("extdata/testfiles_fitbit/S001_d2_1min_epoch.csv", package = "stepmetrics"),
            system.file("extdata/testfiles_fitbit/S001_d3_1min_epoch.csv", package = "stepmetrics"))
  data1 = readFile(file1)

  expect_equal(dim(data1), c(1440*3, 2))
  expect_equal(colnames(data1), c("timestamp", "steps"))
  expect_true(is.ISO8601(data1$timestamp[1]))

  # actigraph agd files -----
  file2 = system.file("extdata/testfiles_agd/S002_1min.agd", package = "stepmetrics")
  file3 = system.file("extdata/testfiles_agd/3h30sec.agd", package = "stepmetrics")

  # 1min epoch
  # data2 = readFile(file2)
  #
  # expect_equal(dim(data2), c(1440*5, 2)) # the sample agd file contains 5 complete days
  # expect_equal(colnames(data2), c("timestamp", "steps"))
  # expect_true(is.ISO8601(data2$timestamp[1]))

  # 30 sec epoch
  data3 = readFile(file3)

  expect_equal(dim(data3), c(4882, 2))
  expect_equal(colnames(data3), c("timestamp", "steps"))
  expect_true(is.ISO8601(data3$timestamp[1]))

  # actigraph csv files -----
  paths = dir(system.file("extdata/testfiles_actigraph_csv/", package = "stepmetrics"), full.names = TRUE)
  file4 = grep("datatable", paths, value = TRUE)   # with header / timestamp
  file5 = grep("noTS", paths, value = TRUE)        # without header / timestamp
  file6 = grep("semicolon", paths, value = TRUE)        # without header / timestamp
  file7 = grep("date_time", paths, value = TRUE)        # without header / timestamp

  # with header / timestamp
  data4 = readFile(file4)

  expect_equal(dim(data4), dim(data3))
  expect_equal(data4[1, 1], data3[1, 1])
  expect_equal(data4[1, 2], data3[1, 2])
  expect_equal(data4[nrow(data4), 2], data3[nrow(data3), 2])
  expect_equal(data4[nrow(data4), 2], data3[nrow(data3), 2])


  # without header / timestamp
  data5 = readFile(file5)

  expect_equal(dim(data5), dim(data4))
  expect_equal(data5[1, 1], data4[1, 1])
  expect_equal(data5[1, 2], data4[1, 2])
  expect_equal(data5[nrow(data5), 2], data4[nrow(data4), 2])
  expect_equal(data5[nrow(data5), 2], data4[nrow(data4), 2])

  # semicolon separated csv
  data6 = readFile(file6)

  expect_equal(dim(data6), dim(data5))
  expect_equal(data6[1, 1], data5[1, 1])
  expect_equal(data6[1, 2], data5[1, 2])
  expect_equal(data6[nrow(data6), 2], data5[nrow(data5), 2])
  expect_equal(data6[nrow(data6), 2], data5[nrow(data5), 2])

  # one-row header and separated date-time
  data7 = readFile(file7)

  ts  <- parse_iso_utc(data7$timestamp[1])
  exp <- as.POSIXct("2021-07-03 13:48:00", tz = "UTC")
  expect_false(is.na(ts))
  expect_identical(unclass(as.POSIXct(ts, tz = "UTC")), unclass(exp))

  expect_equal(min(data7$steps), 0)
  expect_equal(max(data7$steps), 132)

  # GGIR output
  file8 = system.file("extdata/testfiles_GGIR/output_test/meta/ms2.out/101_1.gt3x.RData",
                      package = "stepmetrics")
  data8 = readFile(file8)

  expect_equal(dim(data8), c(13635, 2))
  expect_true(grepl("19:00:00", data8[1, 1],))
  expect_equal(range(data8[, 2]), c(0, 121))

})
