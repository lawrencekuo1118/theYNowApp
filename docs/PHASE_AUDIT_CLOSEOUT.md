# Audit closeout — remaining steps

**Status:** Not final acceptance — test-control P0 fixed; tab-visit still structural-only; live Rf path unproven  
**Branch:** `cursor/audit-closeout-remaining-3eea`  
**Recorded ship (behavior):** v23.30 / bundle `12690107` (tab-visit auto-calc + Rf fallback note)  
**Prior normalize ship:** v23.29 / bundle `12689855`

This document tracks residual items after Phase 0–2 and P1-D. Valuation formula engines are unchanged and are **not** blocked by these test-control / coverage gaps.

---

## Acceptance gate (current)

| Gate | State |
|---|---|
| Closeout runner failure propagation (`set -uo pipefail` + deliberate fail-through-tee proof) | **Required — fixed this revision** |
| Full closeout + Phase 2 re-run after runner fix | **Required before trusting PASS exit** |
| Tab-visit auto-calc interactive / runtime proof | **Open** — structural `grepl` only; full browser E2E deferred |
| Rf fallback note UI | **Fallback path** exercised / structure checked; **live Treasury path** still open |
| Deploy baseline ↔ bundle ↔ display version | Reconcile before treating any further ship as accepted |

**Conclusion:** Do **not** treat this closeout branch as final audit acceptance solely because a prior runner exited 0. Prefer runner exit after the pipefail fix, plus explicit open items below.

---

## Remaining gaps → disposition

| Prior ID | Risk | Disposition |
|---|---|---|
| P1D-DCF-TAB-NA | P2→P1 UX | **Design in code:** visiting `dcf_calculator` fires `.fire_auto_calc_primary("dcf")` when ready and session FV is still missing. Companion fires for DDM / RI / Multiples. **Proof level: structural only** (see below). |
| FAILSOFT-RF-FALLBACK | P1 | **UI:** CAPM Rf note shows amber `capm_rf_fallback_note` when `is_fallback=TRUE` (en-US + zh-TW). Separates **fallback verified** vs **live Treasury still open**. |
| BRIDGE-ZERO-DEFAULT | P1 | **Accepted / documented** — intentional shared bridge (`cash=0`,`debt=0`); not changed. |
| LIVE-UNCOVERED-* | P1 | **Still live-network for Yahoo/TPEx HTTP** — probe helper (`YNOW_LIVE_PROBE=1`). Cloud runs that return `is_fallback=TRUE` prove the **fallback** path only. |
| P1D-NO-SHINYTEST | P2 | **Deferred** — no chromote harness; manual P1-D + structural closeout tests remain. |
| NORM-DUP-PERIOD-CORRUPT | P1 | **Already shipped** in v23.29; regression in Phase 2 + closeout. |
| CLOSEOUT-RUNNER-PIPEFAIL | P0 | **Fixed:** `scripts/run_audit_closeout.sh` uses `set -uo pipefail`; `scripts/test_audit_closeout_pipefail.sh` proves a deliberate `Rscript` non-zero still fails after `tee`. |

---

## P1 — Tab-visit auto-calc (structural vs runtime)

`tests/test_audit_closeout.R` asserts via `grepl()` that `ynow_server.R` contains:

* DCF tab observer + `.fire_auto_calc_primary("dcf")` + skip when `stock_price_estimate_val` already finite  
* Companion DDM / RI / MULT tab keys + `.fire_auto_calc_primary(key)`  

That proves **structure exists**, not that Shiny runtime fires valuation on tab switch.

| Runtime question | Automated proof today |
|---|---|
| Switch to DCF with inputs ready → valuation runs | **No** (deferred E2E) |
| Switch to DDM / RI / MULT → model calc | **No** |
| Inputs not ready → wait; FV already present → skip unnecessary DCF recalc | Structure encodes DCF skip-when-FV; **not** runtime-proven |
| Calc failure keeps clear error vs stale fair value | **No** |

**Accurate status:** structural control established; **execution-layer / interactive verification pending**. Full browser automation remains deferred (`P1D-NO-SHINYTEST`). Do not mark tab-visit as E2E-closed.

---

## P1 — CAPM Rf: fallback vs live

| Path | What is proven | What is not |
|---|---|---|
| **Fallback** | Locale strings; server checks `is_fallback` and surfaces `capm_rf_fallback_note`; live probe that returns `is_fallback=TRUE` exercises this path | — |
| **Live Treasury** | Helper exists (`YNOW_LIVE_PROBE=1` / `cached_get_risk_free_rate_detail`) | Successful live Yahoo ^TNX / TPEx scrape in this closeout program |
| **Numeric impact** | Engineering fallback uses `APP_DEFAULTS$capm_rf` (else 5%) via `.current_rf_detail()` → feeds CAPM Ke / WACC / DCF discounting when Macro Rf is unavailable | Dedicated oracle comparing live Rf vs fallback Ke/WACC/FV deltas under fixed other inputs — **still recommended as follow-up**, not claimed here |

Treat “fallback note appeared” as **failsoft UX verified**, not as “live Rf integration passed.”

---

## Code / control changes

1. `ynow_server.R` — DCF / DDM / RI / MULT **tab-visit auto-calc** when inputs ready (shipped with v23.30).  
2. `ynow_server.R` + `ui_locale.R` — **Rf engineering fallback** note (bilingual; shipped with v23.30).  
3. `tests/test_audit_closeout.R` — structural checks (not E2E).  
4. `scripts/run_audit_closeout.sh` — **`set -uo pipefail`**; runs pipefail meta-proof before suites.  
5. `scripts/test_audit_closeout_pipefail.sh` — deliberate `quit(status=7)` through `| tee` must set runner `ec≠0`.  
6. This report — honest open items; not final acceptance.

---

## Verification

```bash
# Meta-proof alone
bash scripts/test_audit_closeout_pipefail.sh
# Expected: PASS pipefail proof… (exit 0 means swallow-bug is absent)

bash scripts/run_audit_closeout.sh
# Expected: pipefail proof PASS; closeout structural PASS; Phase 2 PASS=38 FAIL=0
# Runner exit 0 is only meaningful after the pipefail fix.
```

Optional:

```bash
YNOW_LIVE_PROBE=1 bash scripts/run_audit_closeout.sh
# If is_fallback=TRUE → record under fallback path only.
```

---

## Audit program status

| Phase | Scope | Outcome |
|---|---|---|
| 0 | Deploy baseline + RCM re-run | 257/0 reproducible |
| 1 (P1-A/B) | Independent DCF/RI/DDM/MULT oracles | 43/0; no formula defects |
| 2 (P1-C) | Data / cache / lineage | 38/0 after normalize fix |
| P1-D | Browser E2E | Composite OK; DCF-tab design closed in code; **runtime tab-visit not E2E-closed** |
| Closeout | Residuals + runner reliability | Runner P0 fixed; structural + Phase 2; **not final acceptance** |

**Still out of scope by design:** full live Yahoo/TPEx matrix, multi-worker cache races, shinytest / chromote automation.

---

## Deploy decision

**Retain:** v23.29 normalize fix; v23.30 tab-visit design + Rf fallback note (already recorded in `DEPLOY_BASELINE.txt` as bundle `12690107` / `display_version=v23.30`).

**Before declaring further ship / acceptance:**

1. Closeout runner pipefail fix + meta-proof — **this revision**  
2. Re-run full closeout + Phase 2 — required  
3. Tab-visit interactive proof **or** keep explicit “not E2E-closed” (this doc)  
4. Reconcile baseline / bundle / display version on any new deploy  

Valuation **formulas** need not change for these test-control issues. Docs/scripts-only revisions do not require another shinyapps deploy.
