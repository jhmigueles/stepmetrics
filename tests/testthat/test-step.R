test_that("step.metrics produces output", {

  # produce output ----
  datadir = system.file("extdata", "testfiles_fitbit", package = "stepmetrics")
  temp_outputdir = tempfile("fitbit_out")
  dir.create(temp_outputdir)
  step.metrics(datadir = datadir, outputdir = temp_outputdir)

  # tests ----
  expect_true(dir.exists(temp_outputdir))
  expect_true(dir.exists(file.path(temp_outputdir, "daySummary")))
  expect_equal(length(dir(file.path(temp_outputdir, "daySummary"))), 1)
  expect_true(file.exists(file.path(temp_outputdir, "personSummary.csv")))

  # test for GGIR output ----
  datadir = dir(system.file("extdata", "testfiles_GGIR", package = "stepmetrics"), full.names = TRUE)
  temp_outputdir = tempfile("ggir_out")
  step.metrics(datadir = datadir, outputdir = temp_outputdir)

  # tests ----
  expect_true(dir.exists(temp_outputdir))
  expect_true(dir.exists(file.path(temp_outputdir, "daySummary")))
  expect_equal(length(dir(file.path(temp_outputdir, "daySummary"))), 1)
  expect_true(file.exists(file.path(temp_outputdir, "personSummary.csv")))

})
