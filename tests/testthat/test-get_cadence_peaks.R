test_that("calculation of cadence peaks works", {

  # we use the testfile: fitbit -----
  paths = dir(system.file("testfiles_fitbit/", package = "stepmetrics"), full.names = TRUE)
  x = readFile(paths)

  # tests -----------
  peaks_to_calculate = c(1, 30, 60)
  peaks = get_cadence_peaks(x$steps, peaks = peaks_to_calculate)

  expect_equal(length(peaks$values), length(peaks$names))
  expect_equal(length(peaks$values), length(peaks_to_calculate)*2)
  expect_equal(unname(peaks$values[1]), max(x$steps))
  expect_equal(unname(peaks$values[2]), mean(sort(x$steps, decreasing = TRUE)[1:30]))
  expect_equal(unname(peaks$values[3]), mean(sort(x$steps, decreasing = TRUE)[1:60]))
})
