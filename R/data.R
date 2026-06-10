#' Half-hourly contact-centre data with an embedded staffing experiment
#'
#' Four weeks (2026-05-04 to 2026-05-31) of half-hour intervals for a
#' centre open 08:00--18:00, simulated from a known Erlang-A ground truth
#' with [simulate_erlang_a()] so that estimates can be checked against the
#' generating parameters. Staffing alternates week by week between a lean
#' and a rich rule -- the "vary the agents every other week" experiment that
#' lets observed abandonment differences be compared with model predictions.
#'
#' Ground truth: arrival rate follows a within-day curve (peak mid-morning,
#' secondary afternoon hump) scaled by a day-of-week factor and a base of
#' 150 calls/hour; mean handle time 280 s (`mu = 1/280`); mean patience
#' 100 s (`theta = 1/100`). Lean weeks staff `ceiling(offered load) + 1`,
#' rich weeks `+ 4`. Generation script: `data-raw/callcenter.R` (seed 2026,
#' 900 s warm-up discarded per interval).
#'
#' @format A data frame with 560 rows (28 days x 20 intervals):
#' \describe{
#'   \item{ts}{interval start time (`POSIXct`, UTC).}
#'   \item{date}{calendar date.}
#'   \item{tod}{interval start, `"HH:MM"` (character; for display).}
#'   \item{minute}{interval start as minutes since midnight (480--1530) --
#'     the numeric time-of-day to use in spline or smooth rate models,
#'     e.g. `rate = ~ dow + s(minute)`.}
#'   \item{dow}{day of week, `Mon`--`Sun`.}
#'   \item{week}{study week, 1--4.}
#'   \item{arm}{staffing arm, `lean` (weeks 1 and 3) or `rich` (2 and 4).}
#'   \item{agents}{agents on duty.}
#'   \item{arrivals}{calls arriving in the interval.}
#'   \item{abandoned}{of those, calls that abandoned.}
#'   \item{handled}{of those, calls answered (`arrivals - abandoned`).}
#'   \item{aht}{mean handle time of the answered calls, seconds (`NA` if
#'     none).}
#' }
#' @seealso [erlang_fit()] for the model this data is shaped for.
#' @examples
#' head(callcenter)
#' with(callcenter, tapply(abandoned / pmax(arrivals, 1), arm, mean))
"callcenter"
