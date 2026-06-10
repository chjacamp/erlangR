# Inverse and scheduling helpers --------------------------------------------

#' Smallest staffing level meeting an abandonment target
#'
#' Inverse of [p_abandon()] in the decision variable: the fewest agents for
#' which predicted abandonment does not exceed `target`. Provided as a
#' convenience; the package's preferred framing is [expected_loss()], which
#' makes the cost of the target explicit instead of treating it as a given.
#'
#' @param target Maximum acceptable `P(abandon)`.
#' @inheritParams p_abandon
#' @param max_agents Search cap.
#' @return Integer staffing level.
#' @examples
#' agents_for_abandon(0.02, lambda = 2, mu = 1/4, theta = 1/2)
#' @export
agents_for_abandon <- function(target, lambda, mu, theta, max_agents = 10000) {
  stopifnot(target > 0, target < 1)
  n <- 0
  while (p_abandon(n, lambda, mu, theta) > target) {
    n <- n + 1
    if (n > max_agents) stop("no staffing level within `max_agents` meets the target")
  }
  n
}

#' Translate agents on duty into scheduled headcount
#'
#' The model's `n_agents` is bodies actually answering during the interval.
#' Rosters lose time to breaks, meetings, sickness and holidays
#' ("shrinkage"), so the scheduled headcount must be larger. This helper
#' performs that translation -- deliberately outside the model: shrinkage is
#' scheduling policy, not a parameter of the queue.
#'
#' @param n_agents Agents required on duty (vectorised).
#' @param shrinkage Fraction of scheduled time an agent is not available to
#'   handle contacts (0 to <1).
#' @param granularity Rounding step for the schedule (default half an
#'   agent, i.e. one part-time slot); result is rounded up.
#' @return Scheduled headcount, `n_agents / (1 - shrinkage)` rounded up to
#'   `granularity`.
#' @examples
#' schedule_headcount(12, shrinkage = 0.3)
#' @export
schedule_headcount <- function(n_agents, shrinkage = 0.3, granularity = 0.5) {
  stopifnot(shrinkage >= 0, shrinkage < 1, granularity > 0)
  ceiling(n_agents / (1 - shrinkage) / granularity) * granularity
}
