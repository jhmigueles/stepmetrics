test_that("reads and formats data correctly", {

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
  paths = dir(system.file("testfiles_fitbit/", package = "stepmetrics"), full.names = TRUE)
  file1 = paths
  data1 = readFile(file1)

  expect_equal(dim(data1), c(1440*3, 2))
  expect_equal(colnames(data1), c("timestamp", "steps"))
  expect_true(is.ISO8601(data1$timestamp[1]))

  # actigraph agd files -----
  paths = dir(system.file("testfiles_agd/", package = "stepmetrics"), full.names = TRUE)
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
  paths = dir(system.file("testfiles_actigraph_csv/", package = "stepmetrics"), full.names = TRUE)
  file4 = paths[2]   # with header / timestamp
  file5 = paths[1]   # without header / timestamp

  # with header / timestamp
  # data4 = readFile(file4)
  #
  # expect_true(all.equal(data4, data3))
  # if (all.equal(data4, data3) == FALSE) {
  #   print(cbind(data4, data3))
  # }

  # without header / timestamp
  # data5 = readFile(file5)
  #
  # expect_true(all.equal(data5, data4))

})
