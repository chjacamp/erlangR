# Calibrate the exact-binomial recovery test: pick a high-information design
library(erlangR)

design <- function(agents_arms, n_int, seed) {
  set.seed(seed)
  agents <- rep(agents_arms, length.out = n_int)
  arrivals <- rpois(n_int, 36)
  p_true <- p_abandon(agents, 0.02, 1 / 240, 1 / 90)
  df <- data.frame(arrivals = arrivals,
                   abandoned = rbinom(n_int, arrivals, p_true),
                   agents = agents, aht = 240)
  fit <- erlang_fit(df, period = 1800, rate = ~ 1)
  c(err = abs(log((1 / fit$theta) / 90)), se = fit$se_log_theta)
}

cat("elasticity check: p_abandon at n = 5..9:",
    round(p_abandon(5:9, 0.02, 1 / 240, 1 / 90), 4), "\n")
for (arms in list(c(5L, 8L), c(6L, 7L))) {
  for (s in c(7, 21, 99)) {
    r <- design(arms, 300, s)
    cat(sprintf("arms %s seed %2d: |log err| %.3f (fit SE %.3f)\n",
                paste(arms, collapse = "/"), s, r["err"], r["se"]))
  }
}
