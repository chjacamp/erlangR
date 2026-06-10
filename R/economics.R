# Decision layer: staffing as an expected-loss problem ----------------------

#' Expected loss of a staffing level
#'
#' Converts the predicted abandonment curve into money: lost revenue from
#' abandoned calls against the payroll cost of agents, per hour. This is the
#' decision framing the package is built around -- pick `n` to minimise
#' expected total cost, rather than to hit a service-level target.
#'
#' The result is a model prediction, not a verdict: the honest next step is
#' an experiment (e.g. alternate staffing levels week by week, as the
#' [callcenter] example data does) to check the predicted differences where
#' it matters.
#'
#' @param pred An `"erlang_pred"` object from [predict.erlang_fit()].
#' @param value_per_call Revenue (or value) lost when one call abandons.
#' @param cost_per_agent_hour Fully loaded hourly cost of one agent on duty.
#' @return A data frame of class `"erlang_loss"` with columns `n_agents`,
#'   `p_abandon`, `lost_per_hour`, `staff_per_hour`, `total_per_hour`. The
#'   cost-minimising staffing level is in `attr(, "optimum")`.
#' @examples
#' fit <- erlang_fit(callcenter)
#' pred <- predict(fit, n_agents = 6:24, arrivals_per_hour = 120)
#' loss <- expected_loss(pred, value_per_call = 30, cost_per_agent_hour = 26)
#' loss
#' plot(loss)
#' @export
expected_loss <- function(pred, value_per_call, cost_per_agent_hour) {
  if (!inherits(pred, "erlang_pred"))
    stop("`pred` must come from predict() on an erlang_fit object")
  stopifnot(value_per_call >= 0, cost_per_agent_hour >= 0)
  out <- data.frame(
    n_agents       = pred$n_agents,
    p_abandon      = pred$p_abandon,
    lost_per_hour  = pred$abandoned_per_hour * value_per_call,
    staff_per_hour = pred$n_agents * cost_per_agent_hour
  )
  out$total_per_hour <- out$lost_per_hour + out$staff_per_hour
  attr(out, "optimum") <- out$n_agents[which.min(out$total_per_hour)]
  attr(out, "value_per_call") <- value_per_call
  attr(out, "cost_per_agent_hour") <- cost_per_agent_hour
  attr(out, "arrivals_per_hour") <- attr(pred, "arrivals_per_hour")
  class(out) <- c("erlang_loss", "data.frame")
  out
}

#' @export
print.erlang_loss <- function(x, ...) {
  opt <- attr(x, "optimum")
  cat(sprintf("Expected hourly loss at %.0f calls/hour (value/call %.2f, agent cost/hour %.2f)\n",
              attr(x, "arrivals_per_hour"), attr(x, "value_per_call"),
              attr(x, "cost_per_agent_hour")))
  cat(sprintf("  cost-minimising staffing: n = %d agents\n\n", opt))
  show <- x[abs(x$n_agents - opt) <= 3, , drop = FALSE]
  printed <- as.data.frame(show)
  printed$p_abandon <- sprintf("%.2f%%", 100 * printed$p_abandon)
  printed[c("lost_per_hour", "staff_per_hour", "total_per_hour")] <-
    lapply(printed[c("lost_per_hour", "staff_per_hour", "total_per_hour")],
           function(v) sprintf("%.0f", v))
  print(printed, row.names = FALSE)
  invisible(x)
}
