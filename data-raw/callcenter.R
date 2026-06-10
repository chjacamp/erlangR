# Generates data/callcenter.rda --------------------------------------------
# Run from the package root:  Rscript data-raw/callcenter.R
# Sources the package functions directly so it works before installation.

source("R/erlang-a.R")
source("R/simulate.R")

set.seed(2026)

# Ground truth (documented in ?callcenter -- keep in sync)
aht_true      <- 280                 # seconds; mu = 1/280
patience_true <- 100                 # seconds; theta = 1/100
mu    <- 1 / aht_true
theta <- 1 / patience_true
base_per_hour <- 150

tod_labels  <- sprintf("%02d:%02d", rep(8:17, each = 2), c(0, 30))
tod_minutes <- rep(8:17, each = 2) * 60L + c(0L, 30L)
tod_curve  <- c(0.45, 0.60, 0.78, 0.92, 1.00, 0.98, 0.90, 0.80, 0.72, 0.70,
                0.74, 0.80, 0.85, 0.83, 0.78, 0.70, 0.60, 0.50, 0.42, 0.35)
dow_names <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
dow_mult  <- c(Sun = 0.45, Mon = 1.15, Tue = 1.05, Wed = 1.00, Thu = 0.95,
               Fri = 0.90, Sat = 0.55)

start_date <- as.Date("2026-05-04")           # a Monday
warmup  <- 900                                # discarded, seconds
horizon <- warmup + 1800

rows <- vector("list", 28L * 20L)
r <- 0L
for (d in 0:27) {
  date <- start_date + d
  wd   <- dow_names[as.POSIXlt(date)$wday + 1L]
  week <- d %/% 7L + 1L
  arm  <- if (week %% 2L == 1L) "lean" else "rich"
  for (t in seq_along(tod_labels)) {
    lam_hr  <- base_per_hour * dow_mult[[wd]] * tod_curve[t]
    lam     <- lam_hr / 3600
    offered <- lam / mu
    agents  <- ceiling(offered) + if (arm == "lean") 1L else 4L

    sim  <- simulate_erlang_a(lam, mu, theta, agents, horizon)
    keep <- sim[sim$arrival >= warmup, , drop = FALSE]

    r <- r + 1L
    rows[[r]] <- data.frame(
      ts        = as.POSIXct(paste(date, tod_labels[t]), tz = "UTC"),
      date      = date,
      tod       = tod_labels[t],
      minute    = tod_minutes[t],
      dow       = wd,
      week      = week,
      arm       = arm,
      agents    = agents,
      arrivals  = nrow(keep),
      abandoned = sum(!keep$served),
      handled   = sum(keep$served),
      aht       = if (any(keep$served))
                    round(mean(keep$handle[keep$served]), 1) else NA_real_
    )
  }
}

callcenter <- do.call(rbind, rows)
callcenter$dow <- factor(callcenter$dow, levels = dow_names[c(2:7, 1)])
callcenter$arm <- factor(callcenter$arm, levels = c("lean", "rich"))
rownames(callcenter) <- NULL

dir.create("data", showWarnings = FALSE)
save(callcenter, file = "data/callcenter.rda", compress = "xz")
cat("wrote data/callcenter.rda:", nrow(callcenter), "rows,",
    sum(callcenter$arrivals), "calls,",
    sprintf("%.2f%% overall abandonment\n",
            100 * sum(callcenter$abandoned) / sum(callcenter$arrivals)))
