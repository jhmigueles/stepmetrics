test_that("local clock time is preserved across TZs", {
  f <- system.file("extdata/testfiles_actigraph_csv/separated_date_time.csv",
                   package = "stepmetrics")
  ref <- withr::with_envvar(c(TZ = "Europe/Madrid"),
                            readFile(f))$timestamp[1]
  utc <- withr::with_envvar(c(TZ = "UTC"),
                            readFile(f))$timestamp[1]
  # Same clock time (HH:MM:SS) even if offsets differ
  get_hms <- function(x) sub("^.*T(\\d{2}:\\d{2}:\\d{2}).*$", "\\1", x)
  expect_equal(get_hms(ref), get_hms(utc))
})
