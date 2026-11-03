# Toy step-detection methods used only in the tests: one step at every local
# maximum of the acceleration magnitude above a threshold, at least
# `refractory` seconds after the previous one.
toy_core = function(data, sf, params) {
  a = sqrt(data$x^2 + data$y^2 + data$z^2)
  n = length(a)
  if (n < 3) return(NULL)
  i = which(a > params$threshold & a >= c(-Inf, a[-n]) & a > c(a[-1], -Inf))
  if (!length(i)) return(NULL)
  keep = i[1]
  for (j in i[-1]) if (data$time[j] - data$time[keep[length(keep)]] > params$refractory) keep = c(keep, j)
  data.frame(time = data$time[keep], type = "displacement")
}

toy_gyro_core = function(data, sf, params) {
  out = toy_core(data, sf, params)
  if (is.null(out)) return(NULL)
  out$peak_gyro = max(abs(data$gx))
  out
}

register_toy_methods = function() {
  register_step_method("toy", toy_core, version = "0.0.1", sf = 50,
                       min_sf = 25,
                       defaults = list(threshold = 1.5, refractory = 0.3))
  register_step_method("toy_gyro", toy_gyro_core, version = "0.0.1",
                       requires = c("acc", "gyro"), sf = 100,
                       defaults = list(threshold = 1.5, refractory = 0.3))
}

# Synthetic recording: 1 g at rest plus a 2.5 g impact at each step time.
make_impacts = function(step_times, duration, sf = 50, t0 = 1792390000) {
  tm = t0 + (seq_len(round(duration * sf)) - 1) / sf
  z = rep(1, length(tm))
  for (s in step_times) {
    k = which.min(abs(tm - (t0 + s)))
    z[k] = 2.5
    if (k > 1) z[k - 1] = 1.6
    if (k < length(z)) z[k + 1] = 1.6
  }
  data.frame(time = tm, x = 0, y = 0, z = z, gx = 10, gy = 0, gz = 0)
}

testdata = function(name) testthat::test_path("testdata", name)
