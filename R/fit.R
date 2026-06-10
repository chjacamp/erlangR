# Maximum-likelihood fitting from interval data ----------------------------

#' Fit an Erlang-A model to interval-aggregated data
#'
#' Estimates the model parameters from the kind of data a contact centre
#' actually records: per-interval counts of arrivals and abandonments,
#' agents on duty, and mean handle time. Returns a fitted object that works
#' with the familiar `coef()` / `confint()` / `logLik()` / `predict()` /
#' `plot()` workflow.
#'
#' @section Likelihood:
#' Within each interval `i` the system is treated as an Erlang-A queue in
#' steady state (the standard SIPP -- stationary independent period by
#' period -- approximation):
#'
#' \describe{
#'   \item{arrival rate}{`lambda_i` from the rate model chosen by `rate`
#'     (below), under Poisson arrivals;}
#'   \item{service rate}{`mu` by exponential MLE: `1 / mu` is the
#'     handled-call weighted mean of `aht`;}
#'   \item{patience}{`theta` by maximising the binomial log-likelihood
#'     `abandoned_i ~ Binomial(arrivals_i, p_abandon(agents_i, lambda_i, mu, theta))`
#'     over `log(theta)`.}
#' }
#'
#' @section Modelling the arrival rate (please read):
#' `rate = "interval"` plugs each interval's own count in as its rate,
#' `lambda_i = arrivals_i / period`. That is a *saturated* rate model: with
#' typical interval counts its sampling noise is large (CV of 10--20%), and
#' because `p_abandon` is steeply convex in `lambda` near full utilisation,
#' noisy rates inflate predicted abandonment on busy-looking intervals and
#' bias the patience estimate upward -- on the [callcenter] example by
#' roughly +60%. It is kept as an option (and a cautionary tale), not as a
#' recommendation.
#'
#' Passing a one-sided formula instead -- e.g. `rate = ~ dow + tod` --
#' fits a Poisson log-linear model for the arrival counts (with
#' `offset(log(period))`) and uses its fitted rates. Pooling information
#' across intervals shrinks the rate noise and removes the bias; on
#' [callcenter] it recovers the generating patience to within a few
#' percent. `rate = ~ 1` pools everything (one constant rate). This is the
#' usual statistical move: model the nuisance process rather than condition
#' on its noisy realisation.
#'
#' Formulas are not limited to factors. Regression splines from the base
#' `splines` package work in a plain Poisson GLM, e.g.
#' `rate = ~ dow + splines::ns(minute_of_day, 8)`. If the formula contains
#' `mgcv` smooth terms -- `s()`, `te()`, `ti()`, `t2()` -- it is fitted
#' with `mgcv::gam(family = poisson)` instead (mgcv is a Recommended
#' package, so it is present on almost every R installation).
#'
#' Finally, `rate` may be a numeric vector of *expected arrivals per
#' interval* (same units as `arrivals`, one value per row): bring your own
#' forecast from any model -- a GP, `prophet`, a judgmental forecast -- and
#' it is used as-is rather than refitted.
#'
#' The reported standard error and confidence interval for patience come
#' from the curvature of the profile log-likelihood at the optimum and
#' treat the arrival and service rates as plug-ins. Within-interval
#' dependence of caller outcomes means the binomial likelihood is an
#' approximation; intervals are taken as independent. These are the honest
#' caveats -- and the standard operating assumptions in the literature.
#'
#' @param data A data frame with one row per interval. Columns can have any
#'   names: map them with the `arrivals`, `abandoned`, `agents`, and `aht`
#'   arguments. Extra columns (e.g. timestamps) are kept and used by the
#'   plot method when present. See [callcenter] for the expected shape.
#' @param rate Arrival-rate model: a one-sided formula (Poisson GLM;
#'   `mgcv` smooths are routed to [mgcv::gam()]; recommended), a numeric
#'   vector of expected arrivals per interval (a forecast from your own
#'   model, used as-is), or the string `"interval"` for the saturated
#'   per-interval plug-in. See the section below.
#' @param period Interval length in seconds (default `1800`). All fitted
#'   rates are per second.
#' @param arrivals,abandoned,agents,aht Where to find each model input in
#'   `data`: a bare column name, a string, or an expression evaluated in
#'   `data` (e.g. `abandoned = offered - handled`), in the spirit of
#'   `lm(weights = ...)`. Defaults assume columns named as in [callcenter]:
#'   `arrivals` (count), `abandoned` (count), `agents` (on duty), `aht`
#'   (mean handle time of completed calls, seconds; `NA` allowed when
#'   nothing completed).
#'
#' @return An object of class `"erlang_fit"`: a list with elements `theta`,
#'   `mu`, `se_log_theta`, `logLik`, `nobs`, `rate`, `rate_model` (the
#'   glm/gam, when a formula was used), `data` (the input with canonical
#'   columns `arrivals`, `abandoned`, `agents`, `aht` plus the rate
#'   `lambda` and fitted `p_hat`), `period`, and `call`.
#'
#' @seealso [predict.erlang_fit()], [plot.erlang_fit()], [expected_loss()]
#' @examples
#' fit <- erlang_fit(callcenter, rate = ~ dow + tod)
#' fit
#' confint(fit)
#'
#' # columns under different names, mapped explicitly
#' df <- data.frame(t = callcenter$tod, day = callcenter$dow,
#'                  offered = callcenter$arrivals, lost = callcenter$abandoned,
#'                  staffed = callcenter$agents, handle = callcenter$aht)
#' erlang_fit(df, rate = ~ day + t, arrivals = offered, abandoned = lost,
#'            agents = staffed, aht = handle)
#'
#' # spline arrival-rate models over numeric time of day (`minute`):
#' # base splines need no extra dependency
#' erlang_fit(callcenter, rate = ~ dow + splines::ns(minute, 8))
#'
#' # mgcv smooth, if installed (it almost always is)
#' if (requireNamespace("mgcv", quietly = TRUE)) {
#'   erlang_fit(callcenter, rate = ~ dow + s(minute, k = 10))
#' }
#' @export
erlang_fit <- function(data, rate = "interval", period = 1800,
                       arrivals = arrivals, abandoned = abandoned,
                       agents = agents, aht = aht) {
  if (!is.data.frame(data)) stop("`data` must be a data frame")
  caller <- parent.frame()
  resolve <- function(expr, label) {
    v <- tryCatch({
      if (is.character(expr) && length(expr) == 1) {
        if (!expr %in% names(data))
          stop("no column named \"", expr, "\"", call. = FALSE)
        data[[expr]]
      } else {
        eval(expr, data, caller)
      }
    }, error = function(e) {
      stop("could not resolve `", label, " = ", deparse(expr), "`: ",
           conditionMessage(e), "\n  Map it to your data, e.g. erlang_fit(data, ",
           label, " = <column>).", call. = FALSE)
    })
    if (!is.numeric(v)) stop("`", label, "` must be numeric", call. = FALSE)
    if (length(v) != nrow(data))
      stop("`", label, "` must have one value per row of `data`", call. = FALSE)
    v
  }
  arr <- resolve(substitute(arrivals),  "arrivals")
  ab  <- resolve(substitute(abandoned), "abandoned")
  ag  <- resolve(substitute(agents),    "agents")
  aht <- resolve(substitute(aht),       "aht")
  if (any(ab > arr)) stop("`abandoned` cannot exceed `arrivals`")
  if (any(arr < 0) || any(ag < 0)) stop("counts and agents must be non-negative")

  handled <- arr - ab
  use_h <- handled > 0 & !is.na(aht)
  if (!any(use_h)) stop("no completed calls with non-missing `aht`: cannot estimate mu")
  mu_hat <- sum(handled[use_h]) / sum(handled[use_h] * aht[use_h])

  rate_model <- NULL
  if (identical(rate, "interval")) {
    lambda <- arr / period
  } else if (is.numeric(rate)) {
    if (length(rate) != nrow(data))
      stop("a numeric `rate` must give expected arrivals per interval, one per row")
    if (any(!is.finite(rate)) || any(rate < 0))
      stop("expected arrivals must be finite and non-negative")
    lambda <- rate / period
  } else if (inherits(rate, "formula")) {
    glm_data <- data
    glm_data$.arrivals <- arr
    glm_data$.log_period <- log(period)
    f <- stats::update(rate, .arrivals ~ . + offset(.log_period))
    fns <- setdiff(all.names(f), all.vars(f))      # functions called in f
    smooths <- intersect(c("s", "te", "ti", "t2"), fns)
    if (length(smooths)) {
      if (!requireNamespace("mgcv", quietly = TRUE))
        stop("`rate` uses mgcv smooth terms (", paste(smooths, collapse = ", "),
             ") but mgcv is not installed; install it or use splines::ns()/bs() ",
             "inside a plain formula")
      # smooth covariates (positional args of s/te/ti/t2) must be numeric;
      # catch this here because mgcv's own failure is cryptic
      smooth_vars <- character(0)
      walk <- function(e) {
        if (is.call(e)) {
          if (as.character(e[[1]])[1] %in% c("s", "te", "ti", "t2")) {
            args <- as.list(e)[-1]
            nm <- names(args); if (is.null(nm)) nm <- rep("", length(args))
            smooth_vars <<- c(smooth_vars,
                              unlist(lapply(args[nm == ""], all.vars)))
          } else {
            lapply(as.list(e), walk)
          }
        }
      }
      walk(f[[3]])
      for (v in unique(smooth_vars)) {
        val <- if (v %in% names(glm_data)) glm_data[[v]]
               else tryCatch(eval(as.name(v), caller), error = function(e) NULL)
        if (!is.null(val) && !is.numeric(val))
          stop("smooth covariate `", v, "` is ", class(val)[1],
               ", but mgcv smooths need numeric input.\n",
               "  Convert it first -- e.g. for \"HH:MM\" times: ",
               "data$minute <- as.integer(substr(", v, ", 1, 2)) * 60 + ",
               "as.integer(substr(", v, ", 4, 5)),\n",
               "  then rate = ~ ... + s(minute). (The callcenter dataset ",
               "ships this as the `minute` column.)", call. = FALSE)
      }
      rate_model <- tryCatch(
        mgcv::gam(f, data = glm_data, family = stats::poisson()),
        error = function(e) stop("the arrival-rate model (mgcv::gam) failed ",
                                 "to fit: ", conditionMessage(e), call. = FALSE))
    } else {
      rate_model <- tryCatch(
        stats::glm(f, data = glm_data, family = stats::poisson()),
        error = function(e) stop("the arrival-rate model (Poisson glm) failed ",
                                 "to fit: ", conditionMessage(e), call. = FALSE))
    }
    lambda <- unname(stats::fitted(rate_model)) / period
  } else {
    stop('`rate` must be a one-sided formula, a numeric vector of expected ',
         'arrivals, or "interval"')
  }
  data$arrivals <- arr; data$abandoned <- ab; data$agents <- ag; data$aht <- aht

  inf <- arr > 0                       # informative intervals for theta
  p_clamp <- function(p) pmin(pmax(p, 1e-12), 1 - 1e-12)

  # p_abandon for each informative interval, computed once per unique
  # (agents, lambda) configuration -- rate models make many intervals share one
  p_vec <- function(th) {
    key <- paste(ag[inf], signif(lambda[inf], 12))
    first <- !duplicated(key)
    idx <- which(inf)
    pu <- vapply(idx[first], function(i)
      p_abandon(ag[i], lambda[i], mu_hat, th), numeric(1))
    pu[match(key, key[first])]
  }
  loglik_at <- function(log_theta)
    sum(dbinom(ab[inf], arr[inf], p_clamp(p_vec(exp(log_theta))), log = TRUE))

  # patience anywhere from ~1 second to ~1e6 seconds, searched on log scale
  opt <- optimize(loglik_at, interval = log(c(1e-6, 1)), maximum = TRUE,
                  tol = 1e-7)
  log_theta <- opt$maximum

  # curvature of the profile log-likelihood -> SE on log(theta)
  h <- 1e-3
  d2 <- (loglik_at(log_theta + h) - 2 * opt$objective +
           loglik_at(log_theta - h)) / h^2
  se_log_theta <- if (is.finite(d2) && d2 < 0) 1 / sqrt(-d2) else NA_real_

  theta_hat <- exp(log_theta)
  data$lambda <- lambda
  data$p_hat <- 0
  data$p_hat[inf] <- p_vec(theta_hat)

  out <- list(theta = theta_hat, mu = mu_hat, se_log_theta = se_log_theta,
              logLik = opt$objective, nobs = sum(inf), rate = rate,
              rate_model = rate_model, data = data, period = period,
              call = match.call())
  class(out) <- "erlang_fit"
  out
}

#' @export
print.erlang_fit <- function(x, ...) {
  ci <- confint(x)
  rate_lab <- if (inherits(x$rate, "formula")) {
    paste(if (inherits(x$rate_model, "gam")) "Poisson GAM:" else "Poisson GLM:",
          paste(deparse(x$rate), collapse = " "))
  } else if (is.numeric(x$rate)) {
    "user-supplied expected arrivals"
  } else {
    "saturated (per-interval plug-in)"
  }
  cat("Erlang-A model fit (M/M/n+M), interval maximum likelihood\n")
  cat(sprintf("  intervals      : %d informative (period %g s)\n", x$nobs, x$period))
  cat(sprintf("  arrival rate   : %.1f calls/hour on average [%s]\n",
              mean(x$data$lambda) * 3600, rate_lab))
  cat(sprintf("  mean handle    : %.1f s   (mu = %.3g /s, exponential MLE)\n",
              1 / x$mu, x$mu))
  cat(sprintf("  mean patience  : %.1f s   (95%% CI %.1f-%.1f)   <- the fitted parameter\n",
              1 / x$theta, ci[1, 1], ci[1, 2]))
  cat(sprintf("  log-likelihood : %.1f (binomial, df = 1)\n", x$logLik))
  invisible(x)
}

#' @export
coef.erlang_fit <- function(object, ...) {
  c(mu = object$mu, theta = object$theta)
}

#' @export
logLik.erlang_fit <- function(object, ...) {
  structure(object$logLik, df = 1, nobs = object$nobs, class = "logLik")
}

#' Confidence interval for the fitted patience parameter
#'
#' @param object An [erlang_fit()] object.
#' @param parm `"patience"` (mean patience, seconds) or `"theta"`
#'   (abandonment rate, per second).
#' @param level Confidence level.
#' @param ... Unused.
#' @return A one-row matrix in the style of [stats::confint()].
#' @export
confint.erlang_fit <- function(object, parm = c("patience", "theta"),
                               level = 0.95, ...) {
  parm <- match.arg(parm)
  z <- qnorm(1 - (1 - level) / 2)
  lt <- log(object$theta)
  th <- exp(lt + c(-1, 1) * z * object$se_log_theta)
  est <- switch(parm, patience = rev(1 / th), theta = th)
  matrix(est, nrow = 1,
         dimnames = list(parm, sprintf("%.1f %%",
                                       100 * c((1 - level) / 2, 1 - (1 - level) / 2))))
}

#' Predict abandonment as a function of agents on duty
#'
#' The decision curve: `P(abandon | n_agents)` at the fitted service and
#' patience rates, for a stated arrival rate. The uncertainty band
#' propagates the patience confidence interval (arrival and service rates
#' are treated as plug-ins).
#'
#' @param object An [erlang_fit()] object.
#' @param n_agents Integer vector of staffing levels to evaluate.
#' @param arrivals_per_hour Arrival rate to predict at. Defaults to the
#'   average fitted rate in the training data.
#' @param level Level for the patience-uncertainty band.
#' @param ... Unused.
#' @return A data frame of class `"erlang_pred"` with columns `n_agents`,
#'   `p_abandon`, `lwr`, `upr`, `abandoned_per_hour`, `occupancy`.
#' @examples
#' fit <- erlang_fit(callcenter, rate = ~ dow + tod)
#' predict(fit, n_agents = 8:24, arrivals_per_hour = 120)
#' @export
predict.erlang_fit <- function(object, n_agents,
                               arrivals_per_hour = NULL, level = 0.95, ...) {
  if (is.null(arrivals_per_hour))
    arrivals_per_hour <- mean(object$data$lambda) * 3600
  lam <- arrivals_per_hour / 3600          # per second, matching mu/theta
  z <- qnorm(1 - (1 - level) / 2)
  th_band <- exp(log(object$theta) + c(-1, 1) * z * object$se_log_theta)

  p  <- p_abandon(n_agents, lam, object$mu, object$theta)
  lw <- p_abandon(n_agents, lam, object$mu, th_band[1])  # p_abandon increasing in theta
  up <- p_abandon(n_agents, lam, object$mu, th_band[2])
  occ <- occupancy(n_agents, lam, object$mu, object$theta)

  out <- data.frame(n_agents = n_agents, p_abandon = p, lwr = lw, upr = up,
                    abandoned_per_hour = arrivals_per_hour * p,
                    occupancy = occ)
  attr(out, "arrivals_per_hour") <- arrivals_per_hour
  attr(out, "mu") <- object$mu
  attr(out, "theta") <- object$theta
  attr(out, "level") <- level
  class(out) <- c("erlang_pred", "data.frame")
  out
}
