test_that("creates vector for days", {

  # Force a deterministic timezone for the test
  old_tz <- Sys.getenv("TZ", unset = "")
  Sys.setenv(TZ = "UTC")
  on.exit(Sys.setenv(TZ = old_tz), add = TRUE)

  # Build a 4-day, minute-resolution sequence (exactly 1440 per day)
  start <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
  ts <- seq(from = start,
            to   = start + as.difftime(4, units = "days") - 60, # up to 23:59 of day 4
            by   = "1 min")

  # Format as ISO8601 character input
  ts_iso <- format(ts, "%Y-%m-%dT%H:%M:%S%z")

  day <- define_day_indices(ts_iso)

  expect_equal(length(day), length(ts))
  # exactly four days
  expect_equal(length(unique(day)), 4L)
  # each day should have 1440 minutes
  counts <- as.integer(table(day))
  expect_true(all(counts == 1440L), info = paste("Per-day counts:", paste(counts, collapse = ", ")))
  # day labels are 1:4
  expect_identical(unique(as.integer(day)), 1:4)
})
