# Diagnostic: actual recovered values for test calibration (not in package)
library(erlangR)

mk <- function(n_int = 160, seed = 99, warmup = 1800) {
  set.seed(seed)
  rows <- lapply(seq_len(n_int), function(i) {
    ag <- c(5L, 8L)[(i %% 2) + 1]
    sim <- simulate_erlang_a(0.02, 1 / 240, 1 / 90, ag, horizon = warmup + 1800)
    keep <- sim[sim$arrival >= warmup, , drop = FALSE]
    data.frame(arrivals = nrow(keep), abandoned = sum(!keep$served), agents = ag,
               aht = if (any(keep$served)) mean(keep$handle[keep$served]) else NA_real_)
  })
  do.call(rbind, rows)
}

df <- mk()
f1 <- erlang_fit(df, rate = ~ 1)
f2 <- erlang_fit(df, rate = "interval")
cat(sprintf("synthetic (true patience 90): pooled %.1f | saturated %.1f | log-err pooled %.3f\n",
            1 / f1$theta, 1 / f2$theta, abs(log((1 / f1$theta) / 90))))

fc1 <- erlang_fit(callcenter, rate = ~ dow + tod)
fc2 <- erlang_fit(callcenter, rate = "interval")
cat(sprintf("callcenter (true patience 100): GLM %.1f (CI %.1f-%.1f) | saturated %.1f | aht %.1f (true 280)\n",
            1 / fc1$theta, confint(fc1)[1, 1], confint(fc1)[1, 2],
            1 / fc2$theta, 1 / fc1$mu))
cat(sprintf("timing: fit on callcenter with GLM rate: %.1f s\n",
            system.time(erlang_fit(callcenter, rate = ~ dow + tod))[3]))
