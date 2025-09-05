test_that("calculation of cadence bands works", {

  # we use the testfile: fitbit -----
  paths = dir(system.file("testfiles_fitbit/", package = "stepmetrics"), full.names = TRUE)
  x = readFile(paths)

  # tests -----------
  bands_to_calculate = c(0, 1, 20, 40, 60, 80, 100, 120, Inf)
  bands = get_cadence_bands(x$steps, bands = bands_to_calculate)

  expect_equal(length(bands$minutes), length(bands$names))
  expect_equal(length(bands$minutes), length(bands_to_calculate) - 1)
  expect_equal(unname(bands$minutes[1]), sum(x$steps == 0))
  expect_equal(unname(bands$minutes[2]), sum(x$steps >= 1 & x$steps < 20))
  expect_equal(unname(bands$minutes[3]), sum(x$steps >= 20 & x$steps < 40))
  expect_equal(unname(bands$minutes[4]), sum(x$steps >= 40 & x$steps < 60))
  expect_equal(unname(bands$minutes[5]), sum(x$steps >= 60 & x$steps < 80))
  expect_equal(unname(bands$minutes[6]), sum(x$steps >= 80 & x$steps < 100))
  expect_equal(unname(bands$minutes[7]), sum(x$steps >= 100 & x$steps < 120))
  expect_equal(unname(bands$minutes[8]), sum(x$steps >= 120))

})
