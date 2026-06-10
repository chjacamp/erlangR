# The erlang_fit() interface: column-role mapping and arrival-rate engines.

renamed <- function() {
  data.frame(when = callcenter$tod, day = callcenter$dow,
             offered = callcenter$arrivals, lost = callcenter$abandoned,
             staffed = callcenter$agents, handle = callcenter$aht)
}

test_that("column roles map via bare names, strings, and expressions", {
  ref <- erlang_fit(callcenter, rate = ~ dow + tod)

  df <- renamed()
  f_bare <- erlang_fit(df, rate = ~ day + when, arrivals = offered,
                       abandoned = lost, agents = staffed, aht = handle)
  expect_equal(f_bare$theta, ref$theta, tolerance = 1e-10)
  expect_equal(f_bare$mu, ref$mu, tolerance = 1e-10)

  f_str <- erlang_fit(df, rate = ~ day + when, arrivals = "offered",
                      abandoned = "lost", agents = "staffed", aht = "handle")
  expect_equal(f_str$theta, ref$theta, tolerance = 1e-10)

  # expressions evaluate in the data: derive abandoned from other columns
  df$answered <- df$offered - df$lost
  f_expr <- erlang_fit(df, rate = ~ day + when, arrivals = offered,
                       abandoned = offered - answered, agents = staffed,
                       aht = handle)
  expect_equal(f_expr$theta, ref$theta, tolerance = 1e-10)

  # and external vectors are reachable through the caller's environment
  staffing <- df$staffed
  f_env <- erlang_fit(df, rate = ~ day + when, arrivals = offered,
                      abandoned = lost, agents = staffing, aht = handle)
  expect_equal(f_env$theta, ref$theta, tolerance = 1e-10)

  expect_error(erlang_fit(df), "could not resolve")
  expect_error(erlang_fit(df, arrivals = "nope", abandoned = lost,
                          agents = staffed, aht = handle), "could not resolve")
})

test_that("a numeric rate vector (bring-your-own forecast) is used as-is", {
  set.seed(11)
  n_int <- 200
  agents <- rep(c(6L, 7L), length.out = n_int)
  expected <- 36                                    # true expected arrivals
  arrivals <- rpois(n_int, expected)
  p_true <- p_abandon(agents, expected / 1800, 1 / 240, 1 / 90)
  df <- data.frame(arrivals = arrivals,
                   abandoned = rbinom(n_int, arrivals, p_true),
                   agents = agents, aht = 240)
  fit <- erlang_fit(df, rate = rep(expected, n_int))
  err <- abs(log((1 / fit$theta) / 90))
  expect_lt(err / fit$se_log_theta, 2.5)
  expect_output(print(fit), "user-supplied")

  expect_error(erlang_fit(df, rate = rep(expected, 3)), "one per row")
  expect_error(erlang_fit(df, rate = rep(-1, n_int)), "non-negative")
})

test_that("base splines work in a plain GLM rate formula", {
  fit <- erlang_fit(callcenter, rate = ~ dow + splines::ns(minute, 8))
  expect_s3_class(fit$rate_model, "glm")
  # generating patience is 100 s; the smooth rate model should land close
  expect_lt(abs(log((1 / fit$theta) / 100)), 0.25)
  ci <- confint(fit)
  expect_true(ci[1, 1] < 100 && 100 < ci[1, 2])
})

test_that("mgcv smooth terms route to gam when mgcv is available", {
  skip_if_not_installed("mgcv")
  fit <- erlang_fit(callcenter, rate = ~ dow + s(minute, k = 10))
  expect_s3_class(fit$rate_model, "gam")
  expect_lt(abs(log((1 / fit$theta) / 100)), 0.25)
  expect_output(print(fit), "Poisson GAM")
})

test_that("non-numeric smooth covariates produce an instructive error", {
  skip_if_not_installed("mgcv")
  # tod is character "HH:MM"; mgcv's own failure here is cryptic, ours is not
  expect_error(erlang_fit(callcenter, rate = ~ dow + s(tod, k = 2)),
               "smooths need numeric input")
  expect_error(erlang_fit(callcenter, rate = ~ s(arm)),
               "smooths need numeric input")
})

test_that("a smooth-looking variable name does not trigger the gam route", {
  # `s` used as a plain variable must not be mistaken for mgcv::s()
  df <- renamed()
  df$s <- as.integer(factor(df$when))
  fit <- erlang_fit(df, rate = ~ s, arrivals = offered, abandoned = lost,
                    agents = staffed, aht = handle)
  expect_s3_class(fit$rate_model, "glm")
})
