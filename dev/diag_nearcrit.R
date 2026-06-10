# Decisive check: analytic vs long-run simulation at the near-critical config
library(erlangR)

lam <- 0.02; mu <- 1 / 240; theta <- 1 / 90

for (n in c(5L, 8L)) {
  p_an <- p_abandon(n, lam, mu, theta)
  sim <- simulate_erlang_a(lam, mu, theta, n, horizon = 2e6, seed = 123)
  s <- attr(sim, "summary")
  keep <- sim[sim$arrival >= 1800, , drop = FALSE]   # same warm-up as the test
  p_warm <- mean(!keep$served)
  se <- sqrt(p_an * (1 - p_an) / s$n_calls)
  cat(sprintf("n=%d: analytic %.5f | sim-all %.5f | sim-warm %.5f | MC se %.5f | z=%.2f\n",
              n, p_an, s$p_abandon, p_warm, se, (s$p_abandon - p_an) / se))
}

# and the empirical abandonment in the actual test generator, lean arm
mk <- function(n_int = 160, seed = 99, warmup = 1800) {
  set.seed(seed)
  rows <- lapply(seq_len(n_int), function(i) {
    ag <- c(5L, 8L)[(i %% 2) + 1]
    sim <- simulate_erlang_a(lam, mu, theta, ag, horizon = warmup + 1800)
    keep <- sim[sim$arrival >= warmup, , drop = FALSE]
    data.frame(arrivals = nrow(keep), abandoned = sum(!keep$served), agents = ag)
  })
  do.call(rbind, rows)
}
df <- mk()
for (a in c(5L, 8L)) {
  sub <- df[df$agents == a, ]
  cat(sprintf("test-gen arm n=%d: empirical p_ab %.5f over %d calls (analytic %.5f)\n",
              a, sum(sub$abandoned) / sum(sub$arrivals), sum(sub$arrivals),
              p_abandon(a, lam, mu, theta)))
}
