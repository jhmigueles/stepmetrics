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
  data = readFile(paths)

  expect_equal(dim(data), c(1440*3, 2))
  expect_equal(colnames(data), c("timestamp", "steps"))
  expect_true(is.ISO8601(data$timestamp[1]))

  # actigraph agd files -----
  paths = dir(system.file("testfiles_agd/", package = "stepmetrics"), full.names = TRUE)
  file1 = grep("1min", paths, value = TRUE)
  file2 = grep("10secs", paths, value = TRUE)

  # 1min epoch
  data = readFile(file1)

  expect_equal(dim(data), c(1440*5, 2)) # the sample agd file contains 5 complete days
  expect_equal(colnames(data), c("timestamp", "steps"))
  expect_true(is.ISO8601(data$timestamp[1]))

  # 10 sec epoch
  data = readFile(file2)

  expect_equal(dim(data), c(15232, 2))
  expect_equal(colnames(data), c("timestamp", "steps"))
  expect_true(is.ISO8601(data$timestamp[1]))

})
