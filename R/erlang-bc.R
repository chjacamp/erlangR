# Classical Erlang B and C ------------------------------------------------

#' Erlang B and Erlang C
#'
#' The classical loss and delay formulas, included as limiting cases of the
#' Erlang-A model and for reference. `erlang_b()` is the blocking
#' probability of M/M/n/n (arrivals finding all agents busy are lost);
#' `erlang_c()` is the probability of waiting in M/M/n (infinite patience).
#'
#' `erlang_c()` is the `theta -> 0` limit of [p_wait()]; Erlang C has no
#' abandonment and is therefore only defined as a stationary system when
#' `offered < n_agents`. For `offered >= n_agents` the queue diverges and
#' `erlang_c()` returns 1.
#'
#' Both use the standard numerically stable recursion (no factorials), so
#' they are exact to machine precision for thousands of agents.
#'
#' @param n_agents Number of agents (non-negative integer, vectorised).
#' @param offered Offered load in erlangs: `lambda / mu`.
#' @return Numeric vector of probabilities.
#' @examples
#' erlang_b(2, 1)   # exactly 1/5
#' erlang_c(2, 1)   # exactly 1/3
#'
#' # Erlang C as the patient limit of Erlang A:
#' erlang_c(10, 8)
#' p_wait(10, lambda = 8, mu = 1, theta = 1e-8)
#' @export
erlang_b <- function(n_agents, offered) {
  k <- max(length(n_agents), length(offered))
  n_agents <- rep_len(n_agents, k); offered <- rep_len(offered, k)
  vapply(seq_len(k), function(i) {
    n <- n_agents[i]; a <- offered[i]
    if (is.na(n) || n < 0 || n != floor(n))
      stop("`n_agents` must be a non-negative integer")
    if (a < 0) stop("`offered` must be >= 0")
    b <- 1
    if (n > 0) for (s in seq_len(n)) b <- a * b / (s + a * b)
    b
  }, numeric(1))
}

#' @rdname erlang_b
#' @export
erlang_c <- function(n_agents, offered) {
  k <- max(length(n_agents), length(offered))
  n_agents <- rep_len(n_agents, k); offered <- rep_len(offered, k)
  b <- erlang_b(n_agents, offered)
  out <- numeric(k)
  for (i in seq_len(k)) {
    n <- n_agents[i]; a <- offered[i]
    out[i] <- if (n <= a) 1 else n * b[i] / (n - a * (1 - b[i]))
  }
  out
}
