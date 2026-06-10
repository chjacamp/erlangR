# erlang.R ---------------------------------------------------------------
# Erlang B / C / A functions for contact-centre staffing, parameterized to
# mirror https://www.callcentretools.com/tools/erlang-calculator/
#
# Model:
#  * Erlang C (M/M/N) for service level, ASA, % answered immediately,
#    occupancy, and the agent requirement.
#  * Erlang A (M/M/N+M, exponential patience) for the abandon rate only.
#  * Staffing pipeline: N_required = max(N for SL target, N for occupancy cap),
#    then shrinkage is applied as N / (1 - shrinkage), displayed to the
#    nearest 0.5 agent (half-up), exactly as the site does.

# Erlang B blocking probability, stable iterative form ---------------------
erlang_b <- function(agents, intensity) {
  stopifnot(agents >= 0, intensity >= 0)
  b <- 1
  if (agents == 0) return(1)
  for (k in seq_len(agents)) b <- intensity * b / (k + intensity * b)
  b
}

# Erlang C probability that an arriving call has to wait -------------------
erlang_c <- function(agents, intensity) {
  if (agents <= intensity) return(1)        # unstable queue: every call waits
  b <- erlang_b(agents, intensity)
  agents * b / (agents - intensity * (1 - b))
}

# P(answered within target_sec) -------------------------------------------
service_level <- function(agents, intensity, aht_sec, target_sec) {
  if (agents <= intensity) return(0)
  pw <- erlang_c(agents, intensity)
  max(0, 1 - pw * exp(-(agents - intensity) * target_sec / aht_sec))
}

# Average speed of answer (seconds) ---------------------------------------
asa <- function(agents, intensity, aht_sec) {
  if (agents <= intensity) return(Inf)
  erlang_c(agents, intensity) * aht_sec / (agents - intensity)
}

occupancy <- function(agents, intensity) intensity / agents

pct_immediate <- function(agents, intensity) {
  if (agents <= intensity) return(0)
  1 - erlang_c(agents, intensity)
}

# Erlang A (M/M/N+M): fraction of calls that abandon -----------------------
# Exponential patience with mean patience_sec. Solved from the stationary
# birth-death distribution in log space; abandonment fraction is
# theta * E[(X - N)^+] / lambda (rate conservation).
abandon_rate <- function(agents, intensity, aht_sec, patience_sec,
                         tail_eps = 1e-18, max_queue = 5e6) {
  mu     <- 1 / aht_sec
  lambda <- intensity * mu
  theta  <- 1 / patience_sec
  if (lambda == 0) return(0)

  lse2 <- function(a, b) { m <- max(a, b); m + log(exp(a - m) + exp(b - m)) }

  logt  <- 0       # log unnormalized p_0
  logz  <- 0       # log sum of terms so far
  logeq <- -Inf    # log sum of (j - agents) * t_j over j > agents
  j     <- 0
  repeat {
    jn  <- j + 1
    out <- if (jn <= agents) jn * mu else agents * mu + (jn - agents) * theta
    logt <- logt + log(lambda) - log(out)
    logz <- lse2(logz, logt)
    if (jn > agents) {
      logeq <- lse2(logeq, log(jn - agents) + logt)
      # safe stop: terms decaying and current weighted term negligible
      if (lambda < out && (log(jn - agents) + logt) - logz < log(tail_eps)) break
    }
    if (jn >= max_queue) break
    j <- jn
  }
  theta * exp(logeq - logz) / lambda
}

# Smallest N meeting the service-level target ------------------------------
agents_for_sl <- function(intensity, aht_sec, sl_target, target_sec,
                          max_agents = 10000) {
  n <- max(1, ceiling(intensity))
  while (service_level(n, intensity, aht_sec, target_sec) < sl_target) {
    n <- n + 1
    if (n > max_agents) stop("exceeded max_agents")
  }
  n
}

# Smallest N keeping occupancy at or below the cap -------------------------
agents_for_occupancy <- function(intensity, max_occupancy) {
  max(1, ceiling(intensity / max_occupancy))
}

# Shrinkage display: nearest 0.5 agent, ties rounded up (as the site does) -
staff_with_shrinkage <- function(agents, shrinkage) {
  floor(agents / (1 - shrinkage) * 2 + 0.5) / 2
}

# Per-agent-count what-if metrics ------------------------------------------
whatif_table <- function(n_agents, intensity, aht_sec, target_sec,
                         patience_sec, shrinkage = 0) {
  data.frame(
    agents_sched = staff_with_shrinkage(n_agents, shrinkage),
    agents       = n_agents,
    service_level = 100 * vapply(n_agents, service_level, 0, intensity = intensity,
                                 aht_sec = aht_sec, target_sec = target_sec),
    occupancy     = 100 * vapply(n_agents, occupancy, 0, intensity = intensity),
    asa           = vapply(n_agents, asa, 0, intensity = intensity, aht_sec = aht_sec),
    immediate     = 100 * vapply(n_agents, pct_immediate, 0, intensity = intensity),
    abandon       = 100 * vapply(n_agents, abandon_rate, 0, intensity = intensity,
                                 aht_sec = aht_sec, patience_sec = patience_sec)
  )
}

# Full staffing pipeline mirroring callcentretools.com ---------------------
erlang_staffing <- function(calls, period_min, aht_sec, sl_pct, target_sec,
                            max_occupancy_pct = 100, shrinkage_pct = 0,
                            patience_sec = 60, max_agents = 10000) {
  intensity <- calls * aht_sec / (period_min * 60)   # offered load, erlangs
  n_sl  <- agents_for_sl(intensity, aht_sec, sl_pct / 100, target_sec, max_agents)
  n_occ <- agents_for_occupancy(intensity, max_occupancy_pct / 100)
  n_req <- max(n_sl, n_occ)
  shrink <- shrinkage_pct / 100
  list(
    intensity      = intensity,
    n_sl           = n_sl,
    n_occ          = n_occ,
    n_required     = n_req,
    staff_raw      = n_req / (1 - shrink),
    staff_display  = staff_with_shrinkage(n_req, shrink),
    service_level  = 100 * service_level(n_req, intensity, aht_sec, target_sec),
    occupancy      = 100 * occupancy(n_req, intensity),
    asa            = asa(n_req, intensity, aht_sec),
    immediate      = 100 * pct_immediate(n_req, intensity),
    abandon        = 100 * abandon_rate(n_req, intensity, aht_sec, patience_sec)
  )
}
