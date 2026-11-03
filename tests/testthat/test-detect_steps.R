register_toy_methods()

test_that("the method registry works", {
  m = step_methods()
  expect_true(all(c("toy", "toy_gyro") %in% m$name))
  expect_equal(m$requires[m$name == "toy_gyro"], "acc+gyro")
  expect_error(detect_steps(make_impacts(1, 3), "nope", sf = 50), "Unknown step-detection method")
  expect_error(detect_steps(make_impacts(1, 3), "toy", sf = 50, params = list(bad = 1)),
               "Unknown parameter")
  expect_error(register_step_method("bad", function(a, b) NULL, "1"), "data, sf, params")
})

test_that("detect_steps finds the steps", {
  times = c(1.0, 2.0, 59.9, 60.3, 61.0, 119.98, 150.5)
  d = make_impacts(times, duration = 180)
  one = detect_steps(d, "toy", sf = 50, tz = "UTC", side = "left")
  expect_equal(nrow(one), length(times))
  expect_equal(as.numeric(one$time) - d$time[1], times, tolerance = 1e-6)
  expect_equal(attr(one, "recording")[["end"]] - d$time[1], 180, tolerance = 1e-6)
  expect_equal(unique(one$side), "left")
  expect_equal(unique(one$method), "toy")
  expect_equal(unique(one$version), "0.0.1")
  expect_equal(attr(one, "params")$threshold, 1.5)
  less = detect_steps(d, "toy", sf = 50, params = list(threshold = 3))
  expect_equal(nrow(less), 0)
  expect_true(all(c("time", "side", "type", "method", "version") %in% names(less)))
})

test_that("methods that need the gyroscope get it, or fail clearly", {
  d = make_impacts(c(1, 2, 3), duration = 5, sf = 100)
  s = detect_steps(d, "toy_gyro", sf = 100)
  expect_equal(nrow(s), 3)
  expect_true("peak_gyro" %in% names(s))
  expect_error(detect_steps(d[, c("time", "x", "y", "z")], "toy_gyro", sf = 100),
               "needs the gyroscope")
  f = testdata("walk_ax3_100hz.cwa")
  expect_error(suppressWarnings(detect_steps(f, "toy_gyro")), "needs the gyroscope")
})

test_that("detect_steps reads files and warns about low sampling rates", {
  f = testdata("walk_ax6_100hz.cwa")
  raw = suppressWarnings(read_raw(f, tz = "UTC"))
  s = suppressWarnings(detect_steps(f, "toy_gyro", tz = "UTC"))
  expect_equal(s$time, detect_steps(raw, "toy_gyro")$time)
  expect_gt(nrow(s), 0)
  d = make_impacts(c(1, 2), duration = 4, sf = 20)
  expect_warning(detect_steps(d, "toy", sf = 20), "below the minimum")
})

test_that("as_raw wraps data in memory", {
  d = make_impacts(c(1, 2), duration = 4, sf = 50)
  raw = as_raw(d, sf = 50, tz = "UTC")
  expect_s3_class(raw, "stepmetrics_raw")
  expect_equal(raw$header$sf, 50)
  expect_error(as_raw(d[, c("time", "x")], sf = 50), "lacks column")
})
