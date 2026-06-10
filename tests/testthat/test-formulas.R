# Exact and structural properties of the analytic formulas.
# No external oracles: closed-form special cases, limits, and the
# conservation laws of the underlying Markov chain.

test_that("Erlang B and C reproduce exact rational special cases", {
  # n = 2, a = 1: B = (1/2) / (1 + 1 + 1/2) = 1/5; C = 2B / (2 - (1 - B)) = 1/3
  expect_equal(erlang_b(2, 1), 0.2, tolerance = 1e-12)
  expect_equal(erlang_c(2, 1), 1 / 3, tolerance = 1e-12)
  # n = 1, a = 1/2: B = (1/2)/(3/2) = 1/3; C = B/(1 - a(1-B)) = (1/3)/(2/3) = 1/2
  expect_equal(erlang_b(1, 0.5), 1 / 3, tolerance = 1e-12)
  expect_equal(erlang_c(1, 0.5), 0.5, tolerance = 1e-12)
  expect_equal(erlang_b(0, 3), 1)
  expect_equal(erlang_c(5, 5), 1)   # at or above capacity: every call waits
})

test_that("Erlang A with theta = mu collapses to the Poisson (M/M/Inf) law", {
  # When theta = mu the total down-rate is j * mu for every j, so the
  # stationary distribution is Poisson(lambda / mu) exactly.
  lambda <- 5; mu <- 1; theta <- 1; n <- 7
  a <- lambda / mu
  jmax <- 200
  pj <- dpois(0:jmax, a)
  p_ab_direct <- theta * sum(pmax(0:jmax - n, 0) * pj) / lambda
  p_wait_direct <- sum(pj[(n + 1):(jmax + 1)])      # P(X >= n)
  expect_equal(p_abandon(n, lambda, mu, theta), p_ab_direct, tolerance = 1e-12)
  expect_equal(p_wait(n, lambda, mu, theta), p_wait_direct, tolerance = 1e-12)
})

test_that("theta -> 0 recovers Erlang C in the stable regime", {
  lambda <- 8; mu <- 1; n <- 10
  expect_equal(p_wait(n, lambda, mu, theta = 1e-9), erlang_c(n, 8),
               tolerance = 1e-5)
  expect_lt(p_abandon(n, lambda, mu, theta = 1e-9), 1e-6)
})

test_that("conservation laws of the chain hold to numerical precision", {
  # throughput: lambda (1 - P_ab) = mu * n * occupancy, including overload
  for (cfg in list(c(n = 10, lambda = 8, mu = 1, theta = 0.5),
                   c(n = 5, lambda = 12, mu = 1, theta = 0.7),   # heavy overload
                   c(n = 40, lambda = 30, mu = 1, theta = 2))) {
    pab <- p_abandon(cfg["n"], cfg["lambda"], cfg["mu"], cfg["theta"])
    occ <- occupancy(cfg["n"], cfg["lambda"], cfg["mu"], cfg["theta"])
    expect_equal(unname(cfg[["lambda"]] * (1 - pab)),
                 unname(cfg[["mu"]] * cfg[["n"]] * occ), tolerance = 1e-10)
  }
})

test_that("degenerate configurations behave sensibly", {
  expect_equal(p_abandon(0, lambda = 2, mu = 1, theta = 0.5), 1)
  expect_equal(p_wait(0, lambda = 2, mu = 1, theta = 0.5), 1)
  expect_equal(mean_wait(0, lambda = 2, mu = 1, theta = 0.5), 2,  # = 1/theta
               tolerance = 1e-10)
  expect_equal(p_abandon(5, lambda = 0, mu = 1, theta = 1), 0)
  expect_equal(occupancy(5, lambda = 0, mu = 1, theta = 1), 0)
})

test_that("measures are vectorised over n and monotone where they must be", {
  n <- 1:25
  p <- p_abandon(n, lambda = 2, mu = 1 / 4, theta = 1 / 2)
  expect_length(p, 25)
  expect_true(all(diff(p) < 0))                 # more agents, less abandonment
  expect_true(all(p > 0 & p < 1))
  w <- mean_wait(n, lambda = 2, mu = 1 / 4, theta = 1 / 2)
  expect_true(all(diff(w) < 0))
  # p_abandon increasing in theta (used by the predict() uncertainty band)
  expect_true(p_abandon(10, 2, 1 / 4, 1) > p_abandon(10, 2, 1 / 4, 1 / 2))
})

test_that("erlang_a() object collects consistent measures", {
  x <- erlang_a(9, lambda = 2, mu = 1 / 4, theta = 1 / 2)
  expect_s3_class(x, "erlang_a")
  expect_equal(x$p_abandon, p_abandon(9, 2, 1 / 4, 1 / 2))
  expect_equal(x$offered, 8)
  expect_output(print(x), "P\\(abandon\\)")
})
