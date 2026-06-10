# The event-based simulator and the analytic formulas must agree:
# two fully independent implementations of the same model.

test_that("simulator agrees with analytic measures within Monte Carlo error", {
  lambda <- 0.5; mu <- 1 / 6; theta <- 1 / 3; n <- 4   # offered load 3
  sim <- simulate_erlang_a(lambda, mu, theta, n, horizon = 40000, seed = 42)
  s <- attr(sim, "summary")
  expect_gt(s$n_calls, 15000)

  p_hat <- s$p_abandon
  p_true <- p_abandon(n, lambda, mu, theta)
  se <- sqrt(p_true * (1 - p_true) / s$n_calls)
  expect_lt(abs(p_hat - p_true), 4 * se)

  w_true <- mean_wait(n, lambda, mu, theta)
  expect_lt(abs(s$mean_wait - w_true) / w_true, 0.1)

  occ_true <- occupancy(n, lambda, mu, theta)
  expect_lt(abs(s$occupancy - occ_true), 0.02)
})

test_that("simulator respects basic accounting", {
  sim <- simulate_erlang_a(1, 1 / 5, 1 / 2, 6, horizon = 2000, seed = 7)
  expect_true(all(sim$wait >= 0))
  expect_true(all(is.na(sim$handle[!sim$served])))
  expect_true(all(sim$handle[sim$served] > 0))
  expect_true(all(diff(sim$arrival) > 0))
  # with no agents, everyone abandons after their patience
  sim0 <- simulate_erlang_a(1, 1 / 5, 1 / 2, 0, horizon = 500, seed = 7)
  expect_true(all(!sim0$served))
})
