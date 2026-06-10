# Event-based simulator ----------------------------------------------------

#' Simulate an Erlang-A queue call by call
#'
#' Discrete-event simulation of the M/M/n+M queue under first-come
#' first-served routing: each call draws an exponential patience and an
#' exponential service requirement; a call abandons if no agent frees up
#' before its patience runs out. Useful for validating the analytic
#' formulas, for power analysis when designing staffing experiments, and as
#' the generator behind the [callcenter] example data.
#'
#' All rates share one time unit, as everywhere in the package. The
#' simulator is exact for the model (no time discretisation).
#'
#' @inheritParams p_abandon
#' @param horizon Length of simulated time.
#' @param seed Optional integer; if supplied, sets the RNG seed for
#'   reproducibility.
#' @return A data frame with one row per call: `arrival` (time), `wait`
#'   (time spent in queue -- offered wait if served, patience if abandoned),
#'   `served` (logical), and `handle` (service duration, `NA` for abandoned
#'   calls). A list of realised summary statistics is attached as
#'   `attr(, "summary")`.
#' @examples
#' sim <- simulate_erlang_a(lambda = 2, mu = 1/4, theta = 1/2,
#'                          n_agents = 9, horizon = 5000, seed = 1)
#' attr(sim, "summary")$p_abandon
#' p_abandon(9, 2, 1/4, 1/2)   # analytic counterpart
#' @export
simulate_erlang_a <- function(lambda, mu, theta, n_agents, horizon,
                              seed = NULL) {
  stopifnot(lambda > 0, mu > 0, theta > 0, horizon > 0,
            n_agents >= 0, n_agents == floor(n_agents))
  if (!is.null(seed)) set.seed(seed)

  # arrival times: extend until past the horizon
  gaps <- rexp(ceiling(lambda * horizon * 1.1) + 25, lambda)
  while (sum(gaps) < horizon)
    gaps <- c(gaps, rexp(ceiling(lambda * horizon * 0.2) + 25, lambda))
  arrival <- cumsum(gaps)
  arrival <- arrival[arrival <= horizon]
  k <- length(arrival)
  if (k == 0) {
    out <- data.frame(arrival = numeric(0), wait = numeric(0),
                      served = logical(0), handle = numeric(0))
    attr(out, "summary") <- list(n_calls = 0, p_abandon = NaN, mean_wait = NaN)
    return(out)
  }

  patience <- rexp(k, theta)
  service  <- rexp(k, mu)
  wait   <- numeric(k)
  served <- logical(k)

  if (n_agents == 0) {
    wait <- patience
  } else {
    free <- numeric(n_agents)          # times at which each agent frees up
    for (i in seq_len(k)) {
      a <- which.min(free)
      start <- max(arrival[i], free[a])
      offered_wait <- start - arrival[i]
      if (patience[i] > offered_wait) {
        served[i] <- TRUE
        wait[i]   <- offered_wait
        free[a]   <- start + service[i]
      } else {
        wait[i] <- patience[i]
      }
    }
  }

  out <- data.frame(arrival = arrival, wait = wait, served = served,
                    handle = ifelse(served, service, NA_real_))
  attr(out, "summary") <- list(
    n_calls = k,
    p_abandon = mean(!served),
    mean_wait = mean(wait),
    occupancy = if (n_agents > 0) sum(out$handle, na.rm = TRUE) / (n_agents * horizon) else 0
  )
  out
}
