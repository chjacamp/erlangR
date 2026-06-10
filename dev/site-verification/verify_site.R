# verify_site.R -----------------------------------------------------------
# Compares erlang.R against outputs captured live from
# https://www.callcentretools.com/tools/erlang-calculator/ on 2026-06-10
# via direct form POSTs. A cell PASSES if the R value agrees with the site
# value at the site's displayed precision (half a unit in the last digit).

source("erlang.R")

ok <- function(computed, site, tol) {
  if (is.infinite(computed) && site > 1e6) return(TRUE)  # site prints float junk at N == A
  abs(computed - site) <= tol + 1e-9
}

total_pass <- 0L
total_cell <- 0L

check_headline <- function(label, res, exp_display, exp_nreq, exp_sl = NA,
                           exp_asa = NA, exp_occ = NA) {
  cells <- list(
    c("staff display", res$staff_display, exp_display, 0),
    c("N required",    res$n_required,    exp_nreq,    0),
    if (!is.na(exp_sl))  c("service level", res$service_level, exp_sl,  0.05),
    if (!is.na(exp_asa)) c("ASA",           res$asa,           exp_asa, 0.05),
    if (!is.na(exp_occ)) c("occupancy",     res$occupancy,     exp_occ, 0.5)
  )
  cells <- Filter(Negate(is.null), cells)
  cat(sprintf("\n== HEADLINE %s ==\n", label))
  for (cl in cells) {
    p <- ok(as.numeric(cl[2]), as.numeric(cl[3]), as.numeric(cl[4]))
    total_pass <<- total_pass + p; total_cell <<- total_cell + 1L
    cat(sprintf("  %-14s R: %-10.4g site: %-10.4g %s\n",
                cl[1], as.numeric(cl[2]), as.numeric(cl[3]),
                if (p) "PASS" else "FAIL"))
  }
}

check_table <- function(label, site, intensity, aht, target, patience,
                        shrink = NA) {
  cat(sprintf("\n== TABLE %s ==\n", label))
  has_shr <- !is.na(shrink)
  hdr <- sprintf("  %3s | %-15s %-15s %-17s %-15s %-15s%s", "N",
                 "SL% (R|site)", "Occ% (R|site)", "ASA (R|site)",
                 "Imm% (R|site)", "Aband% (R|site)",
                 if (has_shr) " Sched (R|site)" else "")
  cat(hdr, "\n")
  for (i in seq_len(nrow(site))) {
    n  <- site$n[i]
    sl <- 100 * service_level(n, intensity, aht, target)
    oc <- 100 * occupancy(n, intensity)
    as_ <- asa(n, intensity, aht)
    im <- 100 * pct_immediate(n, intensity)
    ab <- 100 * abandon_rate(n, intensity, aht, patience)
    cmp <- list(c(sl, site$sl[i], .05), c(oc, site$occ[i], .05),
                c(as_, site$asa[i], .05), c(im, site$imm[i], .05),
                c(ab, site$ab[i], .005))
    if (has_shr) cmp <- c(cmp, list(c(staff_with_shrinkage(n, shrink),
                                      site$sched[i], 0)))
    flags <- vapply(cmp, function(x) ok(x[1], x[2], x[3]), TRUE)
    total_pass <<- total_pass + sum(flags); total_cell <<- total_cell + length(flags)
    fmt <- function(v, s, f) sprintf("%7.1f|%-7.1f%s", v, s, if (f) " " else "X")
    cat(sprintf("  %3d | %s %s %s %s %s%s\n", n,
                fmt(sl, site$sl[i], flags[1]), fmt(oc, site$occ[i], flags[2]),
                fmt(min(as_, 1e9), site$asa[i], flags[3]),
                fmt(im, site$imm[i], flags[4]),
                sprintf("%7.2f|%-7.2f%s", ab, site$ab[i], if (flags[5]) " " else "X"),
                if (has_shr) sprintf(" %6.1f|%-6.1f%s", staff_with_shrinkage(n, shrink),
                                     site$sched[i], if (flags[6]) " " else "X") else ""))
  }
}

# --- Scenario DEFAULT: 400 calls/30 min, AHT 257, SL 80/20, occ 85, shr 30,
#     patience 60.  Site headline: 97 agents (N=68), SL 95.3, ASA 2.6, occ 84.
A1 <- 400 * 257 / 1800
site_default <- data.frame(
  sched = c(83, 84.5, 85.5, 87, 88.5, 90, 91.5, 94.5, 95.5, 97, 98.5, 100),
  n     = c(58:64, 66:70),
  sl    = c(19.3, 37, 51.2, 62.4, 71.3, 78.3, 83.7, 91, 93.5, 95.3, 96.6, 97.6),
  occ   = c(98.5, 96.8, 95.2, 93.6, 92.1, 90.7, 89.2, 86.5, 85.2, 84, 82.8, 81.6),
  asa   = c(250, 99.3, 54.4, 33.6, 22.1, 15, 10.4, 5.2, 3.7, 2.6, 1.9, 1.3),
  imm   = c(13.5, 27, 38.8, 49.1, 58, 65.7, 72.1, 82.1, 85.9, 88.9, 91.4, 93.4),
  ab    = c(6.29, 5.45, 4.68, 3.99, 3.36, 2.81, 2.32, 1.54, 1.23, 0.97, 0.76, 0.59))
res <- erlang_staffing(400, 30, 257, 80, 20, 85, 30, 60)
check_headline("default (occ 85, shr 30)", res, 97, 68, 95.3, 2.6, 84)
check_table("default", site_default, A1, 257, 20, 60, shrink = 0.30)

# --- Scenario S1: occ 100, shr 0.  Headline: 64 agents, SL 83.7, ASA 10.4.
site_s1 <- data.frame(
  n   = 58:67,
  sl  = c(19.3, 37, 51.2, 62.4, 71.3, 78.3, 83.7, 87.9, 91, 93.5),
  occ = c(98.5, 96.8, 95.2, 93.6, 92.1, 90.7, 89.2, 87.9, 86.5, 85.2),
  asa = c(250, 99.3, 54.4, 33.6, 22.1, 15, 10.4, 7.3, 5.2, 3.7),
  imm = c(13.5, 27, 38.8, 49.1, 58, 65.7, 72.1, 77.6, 82.1, 85.9),
  ab  = c(6.29, 5.45, 4.68, 3.99, 3.36, 2.81, 2.32, 1.9, 1.54, 1.23))
res <- erlang_staffing(400, 30, 257, 80, 20, 100, 0, 60)
check_headline("S1 (occ 100, shr 0)", res, 64, 64, 83.7, 10.4)
check_table("S1", site_s1, A1, 257, 20, 60)

# --- Scenario S2: occ 85, shr 25.  Headline: 90.5 agents (N=68).
res <- erlang_staffing(400, 30, 257, 80, 20, 85, 25, 60)
check_headline("S2 (occ 85, shr 25)", res, 90.5, 68, 95.3, 2.6)

# --- Scenario S3: occ 80, shr 30.  Headline: 103 agents (N=72), SL 98.8, ASA 0.6.
site_s3 <- data.frame(
  sched = c(83, 84.5, 85.5, 87, 88.5, 90, 91.5, 100, 101.5, 103, 104.5, 105.5),
  n     = c(58:64, 70:74),
  sl    = c(19.3, 37, 51.2, 62.4, 71.3, 78.3, 83.7, 97.6, 98.3, 98.8, 99.2, 99.4),
  occ   = c(98.5, 96.8, 95.2, 93.6, 92.1, 90.7, 89.2, 81.6, 80.4, 79.3, 78.2, 77.2),
  asa   = c(250, 99.3, 54.4, 33.6, 22.1, 15, 10.4, 1.3, 0.9, 0.6, 0.5, 0.3),
  imm   = c(13.5, 27, 38.8, 49.1, 58, 65.7, 72.1, 93.4, 95, 96.2, 97.2, 97.9),
  ab    = c(6.29, 5.45, 4.68, 3.99, 3.36, 2.81, 2.32, 0.59, 0.45, 0.34, 0.25, 0.19))
res <- erlang_staffing(400, 30, 257, 80, 20, 80, 30, 60)
check_headline("S3 (occ 80, shr 30)", res, 103, 72, 98.8, 0.6)
check_table("S3", site_s3, A1, 257, 20, 60, shrink = 0.30)

# --- Scenario S4: occ 85, shr 29.  Headline: 96 agents (N=68).
res <- erlang_staffing(400, 30, 257, 80, 20, 85, 29, 60)
check_headline("S4 (occ 85, shr 29)", res, 96, 68)

# --- Scenario S5: 1000 calls/60 min, AHT 180, SL 90/10, occ 100, shr 0,
#     patience 120.  Headline: 59 agents, SL 90.8, ASA 3.  A = 50 erlangs.
A5 <- 1000 * 180 / 3600
site_s5 <- data.frame(
  n   = 50:62,
  sl  = c(0, 20.6, 37.4, 51.1, 62.1, 70.9, 77.8, 83.3, 87.5, 90.8, 93.3, 95.1, 96.5),
  occ = c(100, 98, 96.2, 94.3, 92.6, 90.9, 89.3, 87.7, 86.2, 84.7, 83.3, 82, 80.6),
  asa = c(359999967.4, 151.2, 63, 34.7, 21.3, 13.8, 9.3, 6.3, 4.4, 3, 2.1, 1.5, 1),
  imm = c(0, 16, 30, 42.2, 52.6, 61.5, 69.1, 75.4, 80.6, 84.8, 88.3, 91, 93.2),
  ab  = c(6.2, 5.24, 4.39, 3.64, 2.98, 2.41, 1.93, 1.53, 1.2, 0.92, 0.71, 0.53, 0.4))
res <- erlang_staffing(1000, 60, 180, 90, 10, 100, 0, 120)
check_headline("S5 (1000/hr, AHT 180, 90/10)", res, 59, 59, 90.8, 3)
check_table("S5", site_s5, A5, 180, 10, 120)

cat(sprintf("\n==================== %d / %d cells match the live site ====================\n",
            total_pass, total_cell))
