# erlang_fit() must recover known generating parameters from simulated
# interval data, and the downstream predict / expected_loss pipeline must be
# internally consistent.

make_intervals <- function(n_int = 160, seed = 99) {
  set.seed(seed)
  aht <- 240; mu <- 1 / aht; patience <- 90; theta <- 1 / patience
  lam <- 0.02                                   # 72 calls/hour
  agents <- rep(c(5L, 8L), length.out = n_int)  # alternating "experiment"
  rows <- lapply(seq_len(n_int), function(i) {
    # near saturation (rho = 0.96 on the lean arm) the queue needs a long
    # warm-up to reach stationarity; 1800 s keeps the window honest
    sim <- simulate_erlang_a(lam, mu, theta, agents[i], horizon = 3600)
    keep <- sim[sim$arrival >= 1800, , drop = FALSE]
    data.frame(arrivals = nrow(keep), abandoned = sum(!keep$served),
               agents = agents[i],
               aht = if (any(keep$served)) mean(keep$handle[keep$served]) else NA_real_)
  })
  out <- do.call(rbind, rows)
  out$handled <- out$arrivals - out$abandoned
  attr(out, "truth") <- c(aht = aht, patience = patience)
  out
}

test_that("the patience MLE is calibrated when the likelihood is exactly true", {
  # abandonment counts drawn from the binomial model itself: isolates the
  # estimator from queue-simulation and interval-chopping concerns. The
  # assertion is calibration -- error consistent with the fit's own SE --
  # not a lucky point match.
  set.seed(21)
  n_int <- 300
  agents <- rep(c(6L, 7L), length.out = n_int)       # patience-elastic loads
  arrivals <- rpois(n_int, 36)                       # rate 0.02/s over 1800 s
  p_true <- p_abandon(agents, 0.02, 1 / 240, 1 / 90)
  df <- data.frame(arrivals = arrivals,
                   abandoned = rbinom(n_int, arrivals, p_true),
                   agents = agents, aht = 240)
  fit <- erlang_fit(df, period = 1800, rate = ~ 1)
  err <- abs(log((1 / fit$theta) / 90))
  expect_lt(err / fit$se_log_theta, 2.5)             # within 2.5 SE of truth
  expect_lt(err, 0.25)                               # absolute backstop
  ci <- confint(fit)
  expect_true(ci[1, 1] < 90 && 90 < ci[1, 2])
})

test_that("erlang_fit with a pooled rate model recovers DES-generated truth", {
  # end-to-end: data from the event simulator, so this also absorbs the
  # SIPP approximation and binomial-dependence slack; tolerance is wider
  # (the rich arm contributes few, high-leverage abandonment events)
  df <- make_intervals()
  fit <- erlang_fit(df, period = 1800, rate = ~ 1)   # lambda truly constant here
  truth <- attr(df, "truth")
  expect_lt(abs(log((1 / fit$mu) / truth["aht"])), 0.05)         # AHT within ~5%
  expect_lt(abs(log((1 / fit$theta) / truth["patience"])), 0.30) # patience within ~35%
  ci <- confint(fit)
  expect_true(ci[1, 1] < ci[1, 2])
  expect_true(is.finite(logLik(fit)))
  expect_named(coef(fit), c("mu", "theta"))
  expect_output(print(fit), "mean patience")
})

test_that("the saturated per-interval rate inflates patience (documented bias)", {
  df <- make_intervals()
  pooled    <- erlang_fit(df, period = 1800, rate = ~ 1)
  saturated <- erlang_fit(df, period = 1800, rate = "interval")
  # noisy plug-in rates + convex p(lambda) => patience biased upward
  expect_gt(1 / saturated$theta, 1 / pooled$theta)
})

test_that("a Poisson GLM rate model recovers truth on the shipped dataset", {
  # callcenter was generated multiplicatively in dow and tod (see ?callcenter),
  # so the log-linear rate model is correctly specified: patience 100 s,
  # AHT 280 s should come back.
  fit <- erlang_fit(callcenter, rate = ~ dow + tod)
  expect_lt(abs(log((1 / fit$mu) / 280)), 0.05)
  expect_lt(abs(log((1 / fit$theta) / 100)), 0.20)
})

test_that("predict() produces a coherent decision curve", {
  df <- make_intervals()
  fit <- erlang_fit(df, period = 1800, rate = ~ 1)
  pred <- predict(fit, n_agents = 3:12, arrivals_per_hour = 72)
  expect_s3_class(pred, "erlang_pred")
  expect_true(all(diff(pred$p_abandon) < 0))
  expect_true(all(pred$lwr <= pred$p_abandon & pred$p_abandon <= pred$upr))
  expect_true(all(pred$p_abandon >= 0 & pred$p_abandon <= 1))
  expect_equal(pred$abandoned_per_hour, 72 * pred$p_abandon)
})

test_that("expected_loss() finds an interior optimum and adds up", {
  df <- make_intervals()
  fit <- erlang_fit(df, period = 1800, rate = ~ 1)
  pred <- predict(fit, n_agents = 2:20, arrivals_per_hour = 72)
  loss <- expected_loss(pred, value_per_call = 40, cost_per_agent_hour = 20)
  expect_equal(loss$total_per_hour, loss$lost_per_hour + loss$staff_per_hour)
  opt <- attr(loss, "optimum")
  expect_gt(opt, min(loss$n_agents))
  expect_lt(opt, max(loss$n_agents))
  expect_output(print(loss), "cost-minimising")
})

test_that("erlang_fit validates its input", {
  expect_error(erlang_fit(data.frame(arrivals = 1)), "missing column")
  bad <- data.frame(arrivals = 1, abandoned = 2, agents = 1, aht = 100)
  expect_error(erlang_fit(bad), "cannot exceed")
  ok <- data.frame(arrivals = 10, abandoned = 1, agents = 2, aht = 100)
  expect_error(erlang_fit(ok, rate = "nonsense"), "formula")
})

test_that("scheduling helpers behave", {
  expect_equal(schedule_headcount(12, shrinkage = 0.3), 17.5)  # 17.14 -> up to 17.5
  expect_equal(schedule_headcount(10, shrinkage = 0), 10)
  n <- agents_for_abandon(0.02, lambda = 2, mu = 1 / 4, theta = 1 / 2)
  expect_lte(p_abandon(n, 2, 1 / 4, 1 / 2), 0.02)
  if (n > 0) expect_gt(p_abandon(n - 1L, 2, 1 / 4, 1 / 2), 0.02)
})
