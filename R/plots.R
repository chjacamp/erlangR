# Plot methods -------------------------------------------------------------

.col_blue   <- "#0072B2"
.col_orange <- "#D55E00"
.col_green  <- "#009E73"
.col_grey   <- "grey45"

#' Plot method for fitted Erlang-A models
#'
#' `type = "calibration"` plots observed per-interval abandonment rates
#' against the fitted `P(abandon)` (point area proportional to arrivals, so
#' the eye weights intervals the way the likelihood does). A well-specified
#' model scatters around the 45-degree line.
#' `type = "series"` overlays observed and fitted abandonment for the first
#' `days` days of data -- the day-to-day view a workforce planner recognises.
#'
#' @param x An [erlang_fit()] object.
#' @param type `"calibration"` or `"series"`.
#' @param days Number of days to show when `type = "series"` (requires a
#'   `ts` timestamp column in the fitting data).
#' @param ... Passed to [plot()].
#' @return Invisibly, `x`.
#' @export
plot.erlang_fit <- function(x, type = c("calibration", "series"), days = 7, ...) {
  type <- match.arg(type)
  df <- x$data[x$data$arrivals > 0, , drop = FALSE]
  obs <- df$abandoned / df$arrivals

  if (type == "calibration") {
    lim <- c(0, max(obs, df$p_hat) * 1.05)
    plot(df$p_hat, obs,
         pch = 16, col = grDevices::adjustcolor(.col_blue, 0.45),
         cex = 0.3 + sqrt(df$arrivals) / 5,
         xlim = lim, ylim = lim, xaxs = "i", yaxs = "i",
         xlab = "Fitted P(abandon)", ylab = "Observed abandonment rate",
         main = "Calibration: observed vs fitted abandonment", ...)
    abline(0, 1, lty = 2, col = .col_grey)
    mtext(sprintf("point area ~ arrivals | mean patience %.0f s",
                  1 / x$theta), side = 3, line = 0.2, cex = 0.8, col = .col_grey)
  } else {
    if (!"ts" %in% names(df))
      stop("`type = \"series\"` needs a `ts` timestamp column in the data")
    d <- as.Date(df$ts)
    keep <- d %in% head(unique(d), days)
    df <- df[keep, , drop = FALSE]; obs <- obs[keep]; d <- d[keep]
    idx <- seq_len(nrow(df))
    plot(idx, obs, type = "p", pch = 16, cex = 0.6,
         col = grDevices::adjustcolor(.col_blue, 0.6),
         xlab = "", ylab = "P(abandon)", xaxt = "n",
         main = sprintf("Observed (points) vs fitted (line), first %d days",
                        length(unique(d))), ...)
    lines(idx, df$p_hat, col = .col_orange, lwd = 2)
    bounds <- which(diff(as.integer(d)) != 0) + 0.5
    abline(v = bounds, col = "grey85")
    mids <- tapply(idx, d, mean)
    axis(1, at = mids, labels = format(as.Date(names(mids)), "%a %d"),
         tick = FALSE, cex.axis = 0.8)
  }
  invisible(x)
}

#' Plot method for predicted abandonment curves
#'
#' Draws `P(abandon | n_agents)` with the patience-uncertainty band -- the
#' decision curve that staffing choices should be read off.
#'
#' @param x An `"erlang_pred"` object from [predict.erlang_fit()].
#' @param ... Passed to [plot()].
#' @return Invisibly, `x`.
#' @export
plot.erlang_pred <- function(x, ...) {
  plot(x$n_agents, x$p_abandon, type = "n",
       ylim = c(0, max(x$upr) * 1.05), yaxs = "i",
       xlab = "Agents on duty (n)", ylab = "P(abandon)",
       main = "Predicted abandonment by staffing level", ...)
  polygon(c(x$n_agents, rev(x$n_agents)), c(x$lwr, rev(x$upr)),
          col = grDevices::adjustcolor(.col_blue, 0.18), border = NA)
  lines(x$n_agents, x$p_abandon, col = .col_blue, lwd = 2)
  points(x$n_agents, x$p_abandon, pch = 16, col = .col_blue, cex = 0.8)
  mtext(sprintf("at %.0f calls/hour | AHT %.0f s | patience %.0f s (band: %g%% CI on patience)",
                attr(x, "arrivals_per_hour"), 1 / attr(x, "mu"),
                1 / attr(x, "theta"), 100 * attr(x, "level")),
        side = 3, line = 0.2, cex = 0.8, col = .col_grey)
  invisible(x)
}

#' Plot method for expected-loss analysis
#'
#' Lost revenue falls as staffing rises while payroll climbs; the total is
#' the curve to minimise. The marked optimum is where the marginal agent
#' stops paying for themselves.
#'
#' @param x An `"erlang_loss"` object from [expected_loss()].
#' @param ... Passed to [plot()].
#' @return Invisibly, `x`.
#' @export
plot.erlang_loss <- function(x, ...) {
  opt <- attr(x, "optimum")
  ylim <- c(0, max(x$total_per_hour) * 1.05)
  plot(x$n_agents, x$total_per_hour, type = "n", ylim = ylim, yaxs = "i",
       xlab = "Agents on duty (n)", ylab = "Cost per hour",
       main = "Expected loss: abandonment cost vs payroll", ...)
  lines(x$n_agents, x$lost_per_hour, col = .col_blue, lwd = 2)
  lines(x$n_agents, x$staff_per_hour, col = .col_grey, lwd = 2, lty = 3)
  lines(x$n_agents, x$total_per_hour, col = .col_orange, lwd = 2.5)
  points(x$n_agents, x$total_per_hour, pch = 16, col = .col_orange, cex = 0.7)
  abline(v = opt, lty = 2, col = .col_green)
  mtext(sprintf("minimum total cost at n = %d", opt), side = 3, line = 0.2,
        cex = 0.8, col = .col_green)
  legend("topright", bty = "n", lwd = 2, lty = c(1, 3, 1),
         col = c(.col_blue, .col_grey, .col_orange),
         legend = c("lost revenue (abandonment)", "payroll", "total"))
  invisible(x)
}
