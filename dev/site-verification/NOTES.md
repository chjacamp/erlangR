# erlangR — Erlang C/A staffing model, verified against callcentretools.com

Working notes for an R implementation of the contact-centre Erlang calculator at
<https://www.callcentretools.com/tools/erlang-calculator/>, reverse-engineered and
verified cell-by-cell against the live site on 2026-06-10.

## Files

| File | Purpose |
|---|---|
| `erlang.R` | Core model: Erlang B/C, service level, ASA, occupancy, % answered immediately, Erlang A abandonment, agent search, occupancy cap, shrinkage display, full `erlang_staffing()` pipeline |
| `verify_site.R` | Verification harness: compares every cell of 6 captured site scenarios (282 values) |
| `site_page.html`, `result_*.html` | Raw captures of the site's form page and POSTed results used as the oracle |

Run: `Rscript verify_site.R` (R 4.4.2 at `C:\Program Files\R\R-4.4.2\bin\Rscript.exe`)

**Result: 279/282 cells match at the site's displayed precision; the remaining 3 are
one repeated cell (ASA at N=58, 250.1 vs 250) explained by the site rounding the
traffic intensity to 3 decimals (57.111 erlangs) before computing. With that
rounding applied, 282/282.**

## The site's algorithm (decoded)

1. Offered load `A = calls × AHT_sec / period_sec` (erlangs); the site carries this
   rounded to 3 decimals.
2. Erlang C (M/M/N): `P(wait) = C(N, A)` via the stable Erlang-B recursion.
   - Service level = `1 − C·exp(−(N−A)·T/AHT)`
   - ASA = `C·AHT/(N−A)`; % answered immediately = `1 − C`; occupancy = `A/N`.
3. Agents required `N_req = max(N_sl, N_occ)`:
   - `N_sl` = smallest N with SL ≥ target,
   - `N_occ = ceiling(A / max_occupancy)`.
4. Schedule staff = `N_req / (1 − shrinkage)`, displayed rounded to the nearest
   0.5 agent (ties up).
5. Abandon rate = Erlang A (M/M/N+M, exponential patience = "Average Patience"),
   computed at the same N and offered A. Display-only: it does not feed back into
   the staffing requirement or service level (a "hybrid" model, as the site says).

### Site quirks found (do not replicate)

* The results sentence "(X before shrinkage)" prints the last row of the what-if
  table (`N_req + 2`), not `N_req`. E.g. default scenario: headline 97 agents ⇔ 68
  before shrinkage per its own table, but the text says "(70 before shrinkage)".
  Reproduced across three scenarios (always N_req+2).
* At N = A exactly, the site prints ASA ≈ 359,999,967 s — floating-point residue
  from dividing by `N − A ≈ 5e-7` instead of treating the queue as unstable.
* The what-if table layout: rows `N_sl−6 … N_sl`, a divider, then `N_req−2 … N_req+2`.
