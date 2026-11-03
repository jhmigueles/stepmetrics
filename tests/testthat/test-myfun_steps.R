register_toy_methods()

test_that("myfun_steps builds a valid GGIR myfun object", {
  mf = myfun_steps("toy")
  allowed = c("FUN", "parameters", "expected_sample_rate", "expected_unit",
              "colnames", "minlength", "outputres", "outputtype",
              "aggfunction", "timestamp", "reporttype")
  expect_true(all(names(mf) %in% allowed))
  expect_true(is.function(mf$FUN))
  expect_equal(mf$expected_sample_rate, 50)
  expect_equal(mf$expected_unit, "g")
  expect_error(myfun_steps("toy_gyro"), "does not pass")
})

test_that("the GGIR route and detect_steps give the same counts", {
  times = c(1.0, 2.0, 3.1, 30.5, 61.0, 62.2, 119.0, 150.5, 151.4, 152.3)
  d = make_impacts(times, duration = 180)
  mf = myfun_steps("toy", outputres = 1)
  # emulate GGIR: consecutive blocks of 60 s without overlap, timestamp in
  # the first column, then counts summed to a 5-s epoch
  blocks = split(seq_len(nrow(d)), floor((seq_len(nrow(d)) - 1) / (60 * 50)))
  counts = unlist(lapply(blocks, function(i) {
    mf$FUN(cbind(d$time[i], d$x[i], d$y[i], d$z[i]), mf$parameters)
  }))
  expect_equal(length(counts), 180)
  ggir_5s = colSums(matrix(counts, nrow = 5))
  steps = detect_steps(d, "toy", sf = 50, tz = "UTC")
  sm_5s = steps_to_epochs(steps, epoch = 5, tz = "UTC")$steps
  expect_equal(sum(counts), length(times))
  expect_equal(ggir_5s, sm_5s)
})

test_that("the myfun object runs inside GGIR", {
  skip_if_not_installed("GGIR")
  d = make_impacts(c(1, 2, 3, 10), duration = 60)
  mf = myfun_steps("toy")
  mf$timestamp = d$time[1]
  out = GGIR:::applyExtFunction(data = as.matrix(d[, c("x", "y", "z")]),
                                myfun = mf, sf = 50, ws3 = 5)
  expect_equal(sum(out), 4)
})
