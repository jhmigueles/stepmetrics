test_that("reads and formats Fitbit data", {

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
  paths = dir(system.file("testfiles_fitbit/", package = "stepmetrics"), full.names = TRUE)
  data = readFile(paths)

  expect_equal(dim(data), c(1440*3, 2))
  expect_equal(colnames(data), c("timestamp", "steps"))
  expect_true(is.ISO8601(data$timestamp[1]))

})
