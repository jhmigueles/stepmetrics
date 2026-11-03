test_that("steps are counted per epoch, with empty epochs as 0", {
  t0 = as.numeric(as.POSIXct("2026-11-03 10:00:00", tz = "UTC"))
  e = steps_to_epochs(t0 + c(1, 2, 3, 65, 66, 179), epoch = 60, tz = "UTC")
  expect_equal(e$steps, c(3, 2, 1))
  expect_equal(e$timestamp[1], "2026-11-03T10:00:00+0000")
  e = steps_to_epochs(t0 + c(70), epoch = 60, start = t0, end = t0 + 240, tz = "UTC")
  expect_equal(e$steps, c(0, 1, 0, 0))
  expect_error(steps_to_epochs(t0, epoch = 7), "divide 60")
})

test_that("epochs follow the local clock and step types", {
  t0 = as.numeric(as.POSIXct("2026-07-01 10:00:30", tz = "Europe/Madrid"))
  s = data.frame(time = t0 + c(0, 1, 40), type = c("displacement", "in_place", "in_place"))
  e = steps_to_epochs(s, epoch = 60, tz = "Europe/Madrid", by_type = TRUE)
  expect_equal(e$timestamp[1], "2026-07-01T10:00:00+0200")
  expect_equal(e$steps, c(2, 1))
  expect_equal(e$steps_in_place, c(1, 1))
  expect_equal(e$steps_displacement, c(1, 0))
})

test_that("the output can be read back with readFile", {
  withr::local_timezone("Europe/Madrid")
  t0 = as.numeric(as.POSIXct("2026-11-03 10:00:00", tz = "UTC"))
  tmp = withr::local_tempfile(fileext = ".csv")
  e = steps_to_epochs(t0 + c(1, 2, 3, 65, 66), epoch = 60, start = t0,
                      end = t0 + 600, tz = "Europe/Madrid", file = tmp)
  back = readFile(tmp, tz = "Europe/Madrid")
  expect_equal(back$data$steps, e$steps)
  expect_equal(back$data$timestamp, e$timestamp)
})
