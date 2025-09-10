test_that("checks if a GGIR output is recognized", {

  # It recognizes a GGIR output
  path = system.file("extdata", "testfiles_GGIR/output_test/", package = "stepmetrics")

  expect_true(isGGIRoutput(path))

  # check 1 - does dir exist?
  path = "invented_directory/"

  expect_false(isGGIRoutput(path))

  # check 2 - does name of directory start with output_?
  path = system.file("extdata", "testfiles_fitbit", package = "stepmetrics")

  expect_false(isGGIRoutput(path))

  # check 3 - is meta a subfolder of path?
  dir.create("output_test")
  path = "output_test"

  expect_warning(isGGIRoutput(path))

  # check 4 - is ms2.out a subfolder of meta?
  dir.create("output_test/meta/")
  path = "output_test"

  expect_warning(isGGIRoutput(path))

  # check 5 - is there any file in ms2.out?
  dir.create("output_test/meta/ms2.out")
  path = "output_test"

  expect_warning(isGGIRoutput(path))

  # check 6 - is there any file in ms2.out?
  IMP = list(metashort = data.frame(timestamp = "2021-03-14T14:00:00+0100",
                                    anglez = 10,
                                    ENMO = 0.04))
  save(IMP, file = "output_test/meta/ms2.out/dummy_IMP.RData")
  path = "output_test"

  expect_warning(isGGIRoutput(path))

  # remove generated files
  if (dir.exists("output_test")) unlink("output_test", recursive = TRUE)

})
