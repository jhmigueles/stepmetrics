test_that("benchmark_steps matches steps one to one", {
  ref = c(1, 2, 3, 4, 5)
  det = c(1.05, 2.1, 2.15, 3.4, 4.0, 6)
  b = benchmark_steps(det, ref, tolerance = 0.25)
  s = b$summary
  expect_equal(s$n_reference, 5)
  expect_equal(s$n_detected, 6)
  expect_equal(s$matched, 3) # 1.05, 2.1 (closest of two) and 4.0
  expect_equal(s$false_positives, 3)
  expect_equal(s$false_negatives, 2)
  expect_equal(s$sensitivity, 3 / 5)
  expect_equal(s$precision, 3 / 6)
  expect_equal(s$f1, 2 * 3 / 11)
  expect_equal(s$abs_pct_error, 20)
  expect_equal(b$matches$detected, c(1.05, 2.1, 4.0))
  expect_equal(s$mean_offset_s, mean(c(0.05, 0.1, 0)))
})

test_that("benchmark_steps works per segment and with data frames", {
  ref = data.frame(time = c(1, 2, 3, 11, 12))
  det = data.frame(time = c(1.0, 2.0, 11.1, 12.1, 13))
  segs = data.frame(label = c("walk", "jog"), start = c(0, 10), end = c(10, 20))
  s = benchmark_steps(det, ref, segments = segs)$summary
  expect_equal(s$segment, c("walk", "jog", "all"))
  expect_equal(s$matched, c(2, 2, 4))
  expect_equal(s$false_positives, c(0, 1, 1))
  expect_equal(s$false_negatives, c(1, 0, 1))
})

test_that("benchmark_steps handles empty inputs", {
  s = benchmark_steps(numeric(0), c(1, 2))$summary
  expect_equal(s$matched, 0)
  expect_equal(s$sensitivity, 0)
  expect_true(is.na(s$precision))
})
