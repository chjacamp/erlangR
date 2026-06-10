# Diagnostic: warm-up sensitivity of patience recovery (not part of package)
library(erlangR)

mk <- function(warmup, n_int = 120, seed = 99) {
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

for (w in c(600, 1800, 3600)) {
  f <- erlang_fit(mk(w))
  cat("warmup", w, "-> patience_hat", round(1 / f$theta, 1),
      " aht_hat", round(1 / f$mu, 1), "\n")
}

fc <- erlang_fit(callcenter)
cat("callcenter (true patience 100, aht 280, warmup 900): patience_hat",
    round(1 / fc$theta, 1), " aht_hat", round(1 / fc$mu, 1), "\n")
