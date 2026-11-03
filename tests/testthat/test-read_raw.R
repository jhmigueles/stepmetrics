test_that("read_raw reads an AX6 file with gyroscope and sample-level time", {
  f = testdata("walk_ax6_100hz.cwa")
  raw = suppressWarnings(read_raw(f, tz = "UTC"))
  expect_s3_class(raw, "stepmetrics_raw")
  expect_equal(raw$header$format, "axivity")
  expect_equal(raw$header$device, "Axivity AX6")
  expect_equal(raw$header$sf, 100)
  expect_true(raw$header$has_gyro)
  expect_true(all(c("time", "x", "y", "z", "gx", "gy", "gz") %in% names(raw$data)))
  expect_equal(nrow(raw$data), 2600)
  expect_equal(unique(round(diff(raw$data$time), 6)), 0.01)
  expect_equal(attr(raw$data$time, "tz"), "UTC")
})

test_that("read_raw without gyro drops the gyroscope", {
  f = testdata("walk_ax6_100hz.cwa")
  raw = suppressWarnings(read_raw(f, tz = "UTC", gyro = FALSE))
  expect_false(any(c("gx", "gy", "gz") %in% names(raw$data)))
})

test_that("read_raw reads an AX3 file without gyroscope", {
  f = testdata("walk_ax3_100hz.cwa")
  raw = suppressWarnings(read_raw(f, tz = "UTC"))
  expect_equal(raw$header$device, "Axivity AX3")
  expect_false(raw$header$has_gyro)
  expect_false(any(c("gx", "gy", "gz") %in% names(raw$data)))
})

test_that("read_raw reads the GGIRread test files of other devices", {
  gene = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIRread")
  mtx = system.file("testfiles/mtx_12.5Hz_acc_gyro.BIN", package = "GGIRread")
  skip_if(gene == "" || mtx == "", "GGIRread test files not installed")
  g = suppressWarnings(read_raw(gene))
  expect_equal(g$header$format, "geneactiv")
  expect_equal(g$header$sf, 85.7)
  expect_equal(g$header$duration, nrow(g$data) / 85.7, tolerance = 0.01)
  m = read_raw(mtx)
  expect_equal(m$header$format, "matrix")
  expect_true(m$header$has_gyro)
  expect_true(all(c("gx", "gy", "gz") %in% names(m$data)))
})

test_that("read_raw gives clear errors", {
  expect_error(read_raw("does_not_exist.cwa"), "File not found")
  tmp = withr::local_tempfile(fileext = ".xyz")
  writeLines("a", tmp)
  expect_error(read_raw(tmp), "Unsupported file format")
})
