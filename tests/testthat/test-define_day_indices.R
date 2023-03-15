test_that("creates vector for days", {

  chartime2iso8601 = function(x, tz) {
    POStime = as.POSIXlt(as.numeric(as.POSIXlt(x, tz)), origin = "1970-1-1", tz)
    POStimeISO = strftime(POStime, format = "%Y-%m-%dT%H:%M:%S%z")
    return(POStimeISO)
  }

  # dummy timestamp vector to test -----
  ts = seq.POSIXt(from = as.POSIXlt("2023-01-01 00:00:00"),
                  to = as.POSIXlt("2023-01-04 23:59:00"), by = 60)
  ts = chartime2iso8601(format(ts),tz = "")

  # tests -----
  day = define_day_indices(ts)

  expect_equal(length(day), length(ts))
  expect_equal(length(table(day)), 4)
  expect_equal(unique(table(day)), 1440)
  expect_equal(unique(day), 1:4)

})
