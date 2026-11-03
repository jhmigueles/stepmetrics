test_that("ActiGraph csv files are read with header and clock time", {
  f = testdata("actigraph_100hz.csv")
  h = raw_header(f, tz = "Europe/Madrid")
  expect_equal(h$format, "actigraph_csv")
  expect_equal(h$device, "ActiGraph")
  expect_equal(h$serial, "TAS1H30182785")
  expect_equal(h$sf, 100)
  expect_equal(format(as.POSIXct(h$start, origin = "1970-01-01", tz = "Europe/Madrid")),
               "2019-09-17 18:40:00")
  raw = read_raw(f, tz = "Europe/Madrid")
  expect_equal(nrow(raw$data), 3000)
  expect_equal(unname(unlist(raw$data[1, c("x", "y", "z")])), c(0, 0.008, 0.996))
  expect_equal(diff(raw$data$time[1:2]), 0.01, tolerance = 1e-5, scale = 1)
  expect_equal(raw$header$duration, 30, tolerance = 1e-6)
})

test_that("ActiGraph csv files with a timestamp column skip it", {
  src = readLines(testdata("actigraph_100hz.csv"))
  tmp = withr::local_tempfile(fileext = ".csv")
  stamps = format(as.POSIXct("2019-09-17 18:40:00", tz = "UTC") + (0:2999) / 100,
                  "%m/%d/%Y %H:%M:%OS3")
  writeLines(c(src[1:10], paste0("Timestamp,", src[11]),
               paste(stamps, src[12:3011], sep = ",")), tmp)
  a = read_raw(testdata("actigraph_100hz.csv"), tz = "UTC")$data
  b = read_raw(tmp, tz = "UTC")$data
  expect_equal(b, a)
})

test_that("Axivity csv files are resampled onto the cwa grid", {
  cwa = suppressWarnings(read_raw(testdata("walk_ax6_100hz.cwa"), tz = "UTC"))$data
  dir = withr::local_tempdir()
  # OmGui export with Unix time and gyroscope
  f6 = file.path(dir, "ax6.csv")
  utils::write.table(data.frame(sprintf("%.3f", cwa$time), cwa$x, cwa$y, cwa$z,
                                cwa$gx, cwa$gy, cwa$gz),
                     f6, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
  h = raw_header(f6, tz = "UTC")
  expect_equal(h$format, "axivity_csv")
  expect_equal(h$sf, 100)
  expect_true(h$has_gyro)
  w = read_raw(f6, tz = "UTC")$data
  expect_equal(nrow(w), nrow(cwa))
  expect_equal(as.matrix(w[, c("x", "y", "z", "gx", "gy", "gz")]),
               as.matrix(cwa[, c("x", "y", "z", "gx", "gy", "gz")]),
               tolerance = 1e-6, ignore_attr = TRUE)
  # export with text timestamps, accelerometer only
  f3 = file.path(dir, "ax3.csv")
  utils::write.table(data.frame(format(as.POSIXct(cwa$time, origin = "1970-01-01", tz = "UTC"),
                                       "%Y-%m-%d %H:%M:%OS3"), cwa$x, cwa$y, cwa$z),
                     f3, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
  w3 = read_raw(f3, tz = "UTC")$data
  expect_false(raw_header(f3, tz = "UTC")$has_gyro)
  expect_equal(w3$x, cwa$x[seq_len(nrow(w3))], tolerance = 1e-3)
})

test_that("unknown csv layouts point to as_raw()", {
  tmp = withr::local_tempfile(fileext = ".csv")
  writeLines(c("a;b", "1;2"), tmp)
  expect_error(read_raw(tmp), "as_raw")
})

test_that("ActiGraph gt3x files match the ActiLife csv export", {
  skip_if_not_installed("read.gt3x")
  gt3x = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x", package = "read.gt3x")
  skip_if(gt3x == "", "read.gt3x example file not installed")
  h = raw_header(gt3x, tz = "Europe/Madrid")
  expect_equal(h$format, "actigraph_gt3x")
  expect_equal(h$sf, 100)
  csv = read_raw(testdata("actigraph_100hz.csv"), tz = "Europe/Madrid")$data
  expect_equal(h$start, csv$time[1])
  g = read_raw(gt3x, tz = "Europe/Madrid")$data
  g = g[seq_len(nrow(csv)), ]
  expect_equal(g$time, csv$time, tolerance = 1e-6)
  expect_equal(as.matrix(g[, c("x", "y", "z")]), as.matrix(csv[, c("x", "y", "z")]),
               tolerance = 0.002, ignore_attr = TRUE)
})

test_that("Movisens files are read from acc.bin or its folder", {
  skip_if_not_installed("unisensR")
  folder = system.file("extdata", "unisensExample", package = "unisensR")
  skip_if(folder == "", "unisensR example not installed")
  h = raw_header(file.path(folder, "acc.bin"), tz = "UTC")
  expect_equal(h$format, "movisens")
  expect_equal(h$sf, 64)
  expect_equal(format(as.POSIXct(h$start, origin = "1970-01-01", tz = "UTC")),
               "2008-07-04 13:27:57")
  raw = read_raw(file.path(folder, "acc.bin"), tz = "UTC")
  expect_equal(nrow(raw$data), h$device_header$n)
  expect_equal(unique(round(diff(raw$data$time), 6)), round(1 / 64, 6))
  expect_equal(raw_header(folder, tz = "UTC")$format, "movisens")
})
