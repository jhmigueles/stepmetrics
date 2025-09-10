test_that("reads and formats data correctly", {
  testthat::skip_if_not_installed("RSQLite")

  # function to check if is ISO8601
  # https://github.com/wadpac/GGIR/blob/master/R/is.ISO8601.R
  is.ISO8601 = function(x) {
    NNeg = length(unlist(strsplit(x,"[-]")))
    NPos = length(unlist(strsplit(x,"[+]")))
    is.ISO = FALSE
    if (NPos == 2) {
      is.ISO = TRUE
    } else if (NNeg == 4 | NNeg == 2) {
      is.ISO = TRUE
    }
    return(is.ISO)
  }

  # tests ------
  # fitbit files -----
  paths = dir(system.file("extdata", "testfiles_fitbit/", package = "stepmetrics"), full.names = TRUE)
  file1 = paths
  data1 = readFile(file1)

  expect_equal(dim(data1), c(1440*3, 2))
  expect_equal(colnames(data1), c("timestamp", "steps"))
  expect_true(is.ISO8601(data1$timestamp[1]))

  # actigraph agd files -----
  paths = dir(system.file("extdata","testfiles_agd/", package = "stepmetrics"), full.names = TRUE)
  file2 = grep("1min", paths, value = TRUE)
  file3 = grep("30sec", paths, value = TRUE)

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
  paths = dir(system.file("extdata","testfiles_actigraph_csv/", package = "stepmetrics"), full.names = TRUE)
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

  expect_true(grepl("2021-07-03T13:48:00", data7$timestamp[1]))
  expect_true(min(data7$steps) == 0)
  expect_true(max(data7$steps) == 132)

  # GGIR output
  paths = dir(system.file("extdata","testfiles_GGIR//", package = "stepmetrics"), full.names = TRUE)
  file8 = dir(paths, recursive = TRUE, full.names = TRUE)
  data8 = readFile(file8)

  expect_equal(dim(data8), c(13635, 2))
  expect_true(grepl("19:00:00", data8[1, 1],))
  expect_equal(range(data8[, 2]), c(0, 121))

})
