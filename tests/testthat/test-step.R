test_that("step.metrics produces output", {

  mkdtemp <- function(prefix = "stepmetrics_out_") {
    p <- tempfile(prefix); dir.create(p, recursive = TRUE, showWarnings = FALSE); p
  }

  ## 1) Fitbit demo ----
  datadir1 <- system.file("extdata", "testfiles_fitbit",
                          package = "stepmetrics", mustWork = TRUE)

  out1 <- mkdtemp()
  on.exit(unlink(out1, recursive = TRUE, force = TRUE), add = TRUE)

  step.metrics(datadir = datadir1, outputdir = out1)

  expect_true(dir.exists(out1))
  expect_true(dir.exists(file.path(out1, "daySummary")))
  expect_length(list.files(file.path(out1, "daySummary")), 1L)
  expect_true(file.exists(file.path(out1, "personSummary.csv")))

  ## 2) GGIR demo ----
  ggir_root <- system.file("extdata", "testfiles_GGIR",
                           package = "stepmetrics", mustWork = TRUE)
  ggir_out_dirs <- dir(ggir_root, full.names = TRUE, recursive = FALSE)
  ggir_out_dirs <- ggir_out_dirs[grepl("^output_", basename(ggir_out_dirs))]
  expect_gt(length(ggir_out_dirs), 0L)

  datadir2 <- ggir_out_dirs[1L]  # pick one output_* dir

  out2 <- mkdtemp()
  on.exit(unlink(out2, recursive = TRUE, force = TRUE), add = TRUE)

  step.metrics(datadir = datadir2, outputdir = out2)

  expect_true(dir.exists(out2))
  expect_true(dir.exists(file.path(out2, "daySummary")))
  expect_length(list.files(file.path(out2, "daySummary")), 1L)
  expect_true(file.exists(file.path(out2, "personSummary.csv")))
})
