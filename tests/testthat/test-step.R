test_that("step.metrics produces output", {

  mkdtemp <- function(prefix = "stepmetrics_") {
    p <- tempfile(prefix); dir.create(p); p
  }

  # produce output ----
  datadir = system.file("extdata", "testfiles_fitbit", package = "stepmetrics")
  out1 = mkdtemp()
  on.exit(unlink(out1, recursive = TRUE, force = TRUE), add = TRUE)
  step.metrics(datadir = datadir, outputdir = out1)

  # tests ----
  expect_true(dir.exists(out1))
  expect_true(dir.exists(file.path(out1, "daySummary")))
  expect_equal(length(dir(file.path(out1, "daySummary"))), 1)
  expect_true(file.exists(file.path(out1, "personSummary.csv")))

  # test for GGIR output ----
  ggir_root <- system.file("extdata", "testfiles_GGIR", package = "stepmetrics", mustWork = TRUE)
  ggir_out_dirs <- dir(ggir_root, full.names = TRUE, recursive = FALSE)
  ggir_out_dirs <- ggir_out_dirs[grepl("^output_", basename(ggir_out_dirs))]
  expect_gt(length(ggir_out_dirs), 0)
  datadir <- ggir_out_dirs[1L]
  out2 = mkdtemp()
  on.exit(unlink(out2, recursive = TRUE, force = TRUE), add = TRUE)
  step.metrics(datadir = datadir, outputdir = out2)

  # tests ----
  expect_true(dir.exists(out2))
  expect_true(dir.exists(file.path(out2, "daySummary")))
  expect_equal(length(dir(file.path(out2, "daySummary"))), 1)
  expect_true(file.exists(file.path(out2, "personSummary.csv")))

})
