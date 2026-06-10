# Core Erlang-A (M/M/n+M) machinery ---------------------------------------

# Internal: the three stationary-distribution functionals every measure
# needs, computed in log space from the birth-death balance equations.
#
# State j = number of calls in system. Up-rate lambda; down-rate
# min(j, n) * mu + max(0, j - n) * theta. Abandonment makes the chain
# ergodic for every lambda, so this is defined even when lambda >= n * mu.
# The queue tail decays (super-)geometrically once the total down-rate
# exceeds lambda; we truncate when the next weighted term cannot move any
# functional by more than `tol` relatively. Exact to ~tol.
erlang_a_functionals <- function(n_agents, lambda, mu, theta, tol = 1e-14) {
  if (length(n_agents) != 1 || length(lambda) != 1 || length(mu) != 1 ||
      length(theta) != 1)
    stop("internal: erlang_a_functionals() is scalar; vectorise above it")
  if (is.na(n_agents) || n_agents < 0 || n_agents != floor(n_agents))
    stop("`n_agents` must be a non-negative integer")
  if (lambda < 0) stop("`lambda` must be >= 0")
  if (mu <= 0) stop("`mu` must be > 0")
  if (theta <= 0) stop("`theta` must be > 0 (for the theta -> 0 limit use erlang_c())")

  if (lambda == 0)
    return(list(p_wait = as.numeric(n_agents == 0), e_queue = 0, e_busy = 0))

  lse2 <- function(a, b) {
    if (a == -Inf) return(b)
    m <- max(a, b)
    m + log(exp(a - m) + exp(b - m))
  }

  logt      <- 0                                  # log unnormalised pi_0
  logz      <- 0                                  # log normalising sum
  log_wait  <- if (n_agents == 0) 0 else -Inf     # log sum_{j >= n} t_j
  log_queue <- -Inf                               # log sum_{j > n} (j - n) t_j
  log_busy  <- -Inf                               # log sum_j min(j, n) t_j
  j <- 0
  repeat {
    j <- j + 1
    out  <- if (j <= n_agents) j * mu else n_agents * mu + (j - n_agents) * theta
    logt <- logt + log(lambda) - log(out)
    logz <- lse2(logz, logt)
    if (n_agents > 0) log_busy <- lse2(log_busy, log(min(j, n_agents)) + logt)
    if (j >= n_agents) log_wait <- lse2(log_wait, logt)
    if (j > n_agents) {
      log_queue <- lse2(log_queue, log(j - n_agents) + logt)
      if (lambda < out && (log(j - n_agents + 1) + logt) - logz < log(tol)) break
    }
  }
  list(p_wait  = exp(log_wait - logz),
       e_queue = exp(log_queue - logz),
       e_busy  = exp(log_busy - logz))
}

# Internal: recycle arguments and apply a scalar functional extractor.
erlang_a_vapply <- function(n_agents, lambda, mu, theta, f) {
  k <- max(length(n_agents), length(lambda), length(mu), length(theta))
  n_agents <- rep_len(n_agents, k); lambda <- rep_len(lambda, k)
  mu <- rep_len(mu, k); theta <- rep_len(theta, k)
  vapply(seq_len(k), function(i) {
    f(erlang_a_functionals(n_agents[i], lambda[i], mu[i], theta[i]),
      n_agents[i], lambda[i], mu[i], theta[i])
  }, numeric(1))
}

#' Erlang-A (M/M/n+M) performance measures
#'
#' Exact stationary performance measures of the Erlang-A queue: Poisson
#' arrivals at rate `lambda`, `n_agents` parallel agents with exponential
#' service at rate `mu` each (mean handle time `1/mu`), and exponentially
#' distributed caller patience with mean `1/theta` -- a caller still waiting
#' when their patience runs out abandons.
#'
#' The model has exactly three parameters (`lambda`, `mu`, `theta`); the
#' number of agents `n_agents` is the decision variable. All rates share one
#' arbitrary time unit -- per second, per minute, per hour -- as long as you
#' are consistent. `p_abandon()` is the headline quantity:
#' `P(abandon | n_agents)`.
#'
#' Measures are computed from the stationary birth--death distribution (in
#' log space, so thousands of agents are fine) rather than from tabulated
#' approximations, and satisfy the exact conservation laws of the chain,
#' e.g. `lambda * p_abandon = theta * E[queue length]` and
#' `lambda * (1 - p_abandon) = mu * n_agents * occupancy`.
#'
#' Unlike Erlang C, the system is stable for every `lambda` (abandonment
#' drains any overload), so these functions are defined -- and meaningful --
#' even when `lambda >= n_agents * mu`.
#'
#' @param n_agents Number of agents on duty (non-negative integer,
#'   vectorised).
#' @param lambda Arrival rate (calls per unit time).
#' @param mu Service rate per agent (`1 / mean handle time`).
#' @param theta Abandonment rate while waiting (`1 / mean patience`).
#'
#' @return A numeric vector:
#' \describe{
#'   \item{`p_abandon()`}{probability an arriving call eventually abandons.}
#'   \item{`p_wait()`}{probability an arriving call is not served
#'     immediately (all agents busy on arrival).}
#'   \item{`mean_wait()`}{mean time spent waiting in queue, across all
#'     arrivals (served and abandoning), in the unit implied by the rates.}
#'   \item{`occupancy()`}{fraction of paid agent time spent handling calls,
#'     `E[busy agents] / n_agents` (an output of the model, not an input
#'     constraint).}
#' }
#'
#' @references
#' Palm, C. (1946). Research on telephone traffic carried by full
#' availability groups. *Tele*, 1, 107.
#'
#' Mandelbaum, A. and Zeltyn, S. (2007). Service engineering in action: the
#' Palm/Erlang-A queue. In *Advances in Services Innovations*, 17--45.
#'
#' @seealso [erlang_a()] for all measures at once, [erlang_fit()] to
#'   estimate the parameters from data, [simulate_erlang_a()] for a
#'   validating simulator, [erlang_c()] for the no-abandonment limit.
#'
#' @examples
#' # 120 calls/hour, 4-minute mean handle, 2-minute mean patience (per-minute units)
#' p_abandon(n_agents = 6:12, lambda = 2, mu = 1/4, theta = 1/2)
#'
#' # the decision curve is what matters:
#' plot(4:16, p_abandon(4:16, 2, 1/4, 1/2), type = "b",
#'      xlab = "agents", ylab = "P(abandon)")
#' @export
p_abandon <- function(n_agents, lambda, mu, theta) {
  erlang_a_vapply(n_agents, lambda, mu, theta,
                  function(fn, n, l, m, t) if (l == 0) 0 else t * fn$e_queue / l)
}

#' @rdname p_abandon
#' @export
p_wait <- function(n_agents, lambda, mu, theta) {
  erlang_a_vapply(n_agents, lambda, mu, theta,
                  function(fn, n, l, m, t) fn$p_wait)
}

#' @rdname p_abandon
#' @export
mean_wait <- function(n_agents, lambda, mu, theta) {
  erlang_a_vapply(n_agents, lambda, mu, theta,
                  function(fn, n, l, m, t) if (l == 0) 0 else fn$e_queue / l)
}

#' @rdname p_abandon
#' @export
occupancy <- function(n_agents, lambda, mu, theta) {
  erlang_a_vapply(n_agents, lambda, mu, theta,
                  function(fn, n, l, m, t) if (n == 0) 0 else fn$e_busy / n)
}

#' All Erlang-A measures for one configuration
#'
#' Convenience wrapper computing every measure in [p_abandon()] at a single
#' configuration, returned as a small object with a readable print method.
#'
#' @inheritParams p_abandon
#' @return An object of class `"erlang_a"`: a list with the four measures,
#'   the offered load `lambda / mu`, and the parameters.
#' @examples
#' erlang_a(n_agents = 10, lambda = 2, mu = 1/4, theta = 1/2)
#' @export
erlang_a <- function(n_agents, lambda, mu, theta) {
  stopifnot(length(n_agents) == 1)
  fn <- erlang_a_functionals(n_agents, lambda, mu, theta)
  out <- list(
    n_agents  = n_agents,
    lambda    = lambda, mu = mu, theta = theta,
    offered   = lambda / mu,
    p_abandon = if (lambda == 0) 0 else theta * fn$e_queue / lambda,
    p_wait    = fn$p_wait,
    mean_wait = if (lambda == 0) 0 else fn$e_queue / lambda,
    occupancy = if (n_agents == 0) 0 else fn$e_busy / n_agents
  )
  class(out) <- "erlang_a"
  out
}

#' @export
print.erlang_a <- function(x, digits = 4, ...) {
  cat("Erlang-A (M/M/n+M) queue\n")
  cat(sprintf("  agents %d | lambda %.6g | mu %.6g (mean handle %.6g) | theta %.6g (mean patience %.6g)\n",
              x$n_agents, x$lambda, x$mu, 1 / x$mu, x$theta, 1 / x$theta))
  cat(sprintf("  offered load   : %.*g erlangs\n", digits, x$offered))
  cat(sprintf("  P(abandon)     : %.*g\n", digits, x$p_abandon))
  cat(sprintf("  P(wait > 0)    : %.*g\n", digits, x$p_wait))
  cat(sprintf("  mean wait      : %.*g (same time unit as the rates)\n", digits, x$mean_wait))
  cat(sprintf("  occupancy      : %.*g\n", digits, x$occupancy))
  invisible(x)
}
