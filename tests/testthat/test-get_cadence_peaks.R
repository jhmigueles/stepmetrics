test_that("calculation of cadence peaks works", {

  # we use the testfile: fitbit -----
  paths = dir(system.file("extdata", "testfiles_fitbit/", package = "stepmetrics"), full.names = TRUE)
  x = readFile(paths)

  # tests -----------
  peaks_to_calculate = c(1, 30, 60)
  peaks = get_cadence_peaks(x$steps, peaks = peaks_to_calculate)

  expect_equal(length(peaks$values), length(peaks$names))
  expect_equal(length(peaks$values), length(peaks_to_calculate)*2)
  expect_equal(unname(peaks$values[1]), max(x$steps))
  expect_equal(unname(peaks$values[2]), mean(sort(x$steps, decreasing = TRUE)[1:30]))
  expect_equal(unname(peaks$values[3]), mean(sort(x$steps, decreasing = TRUE)[1:60]))

  # check that all expected names are present
  expected_names <- c("CAD_pk1_spm", "CAD_pk30_spm", "CAD_pk60_spm",
                      "CAD_nZeroes_pk30", "CAD_nZeroes_pk60")
  expect_true(all(expected_names %in% peaks$names))

  # check number of zero values used for the peaks
  expect_equal(unname(peaks$values[4]), sum(sort(x$steps, decreasing = TRUE)[1:1] == 0))
  expect_equal(unname(peaks$values[5]), sum(sort(x$steps, decreasing = TRUE)[1:30] == 0))
  expect_equal(unname(peaks$values[6]), sum(sort(x$steps, decreasing = TRUE)[1:60] == 0))
})
