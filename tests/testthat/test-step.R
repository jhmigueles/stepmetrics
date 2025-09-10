test_that("step.metrics produces output", {

  mkdtemp <- function(prefix = "stepmetrics_") {
    p <- tempfile(prefix); dir.create(p); p
  }

  # produce output ----
  datadir = system.file("extdata/testfiles_fitbit", package = "stepmetrics")
  step.metrics(datadir = datadir, outputdir = getwd())

  # tests ----
  expect_true(dir.exists(out1))
  expect_true(dir.exists(file.path(out1, "daySummary")))
  expect_equal(length(dir(file.path(out1, "daySummary"))), 1)
  expect_true(file.exists(file.path(out1, "personSummary.csv")))

  # test for GGIR output ----
  datadir = system.file("extdata/testfiles_GGIR/output_test", package = "stepmetrics")
  step.metrics(datadir = datadir, outputdir = getwd())

  # tests ----
  expect_true(dir.exists(out2))
  expect_true(dir.exists(file.path(out2, "daySummary")))
  expect_equal(length(dir(file.path(out2, "daySummary"))), 1)
  expect_true(file.exists(file.path(out2, "personSummary.csv")))

})
