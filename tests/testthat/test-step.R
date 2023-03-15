test_that("step.metrics produces output", {

  # produce output ----
  datadir = system.file("testfiles_fitbit", package = "stepmetrics")
  step.metrics(datadir = datadir, outputdir = "./output/")

  # tests ----
  expect_true(dir.exists("./output"))
  expect_true(dir.exists("./output/daySummary"))
  expect_equal(length(dir("./output/daySummary")), 1)
  expect_true(file.exists("./output/personSummary.csv"))

  # remove generated files -----
  if (dir.exists("./output")) unlink("./output/", recursive = TRUE)
})
