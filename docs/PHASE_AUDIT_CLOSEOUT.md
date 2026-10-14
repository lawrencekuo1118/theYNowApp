# Audit closeout — remaining steps

**Status:** Closeout controls implemented and verified (Cloud Agent)  
**Branch:** `cursor/audit-closeout-remaining-3eea`  
**Prior ship:** v23.29 / bundle `12689855` (normalize dup-period fix)

This document closes the residual items left after Phase 0–2 and P1-D, without rewriting valuation formulas.

---

## Remaining gaps → disposition

| Prior ID | Risk | Disposition |
|---|---|---|
| P1D-DCF-TAB-NA | P2→P1 UX | **Fixed:** visiting `dcf_calculator` fires `.fire_auto_calc_primary("dcf")` when ready and session FV is still missing. Companion fires for DDM / RI / Multiples tabs. |
| FAILSOFT-RF-FALLBACK | P1 | **Fixed (UI):** CAPM Rf note shows amber `capm_rf_fallback_note` when `is_fallback=TRUE` (en-US + zh-TW). |
| BRIDGE-ZERO-DEFAULT | P1 | **Accepted / documented** — intentional shared bridge (`cash=0`,`debt=0`); not changed. |
| LIVE-UNCOVERED-* | P1 | **Still live-network for Yahoo/TPEx HTTP** — probe helper available (`YNOW_LIVE_PROBE=1`). In this Cloud Agent run, Rf detail returned `is_fallback=TRUE` / 5% (no venv / live Treasury), which **exercises the fallback UI path** without claiming live Yahoo success. |
| P1D-NO-SHINYTEST | P2 | **Deferred** — no chromote harness; manual P1-D + structural closeout tests remain. |
| NORM-DUP-PERIOD-CORRUPT | P1 | **Already shipped** in v23.29; regression in Phase 2 + closeout. |

---

## Code changes (this closeout)

1. `ynow_server.R` — DCF / DDM / RI / MULT **tab-visit auto-calc** when inputs ready.  
2. `ynow_server.R` + `ui_locale.R` — **Rf engineering fallback** note (bilingual).  
3. `tests/test_audit_closeout.R` + `scripts/run_audit_closeout.sh`.  
4. This report.

---

## Verification

```bash
bash scripts/run_audit_closeout.sh
# Expected: closeout structural PASS; Phase 2 PASS=38 FAIL=0
```

Optional:

```bash
YNOW_LIVE_PROBE=1 bash scripts/run_audit_closeout.sh
```

---

## Audit program status

| Phase | Scope | Outcome |
|---|---|---|
| 0 | Deploy baseline + RCM re-run | 257/0 reproducible |
| 1 (P1-A/B) | Independent DCF/RI/DDM/MULT oracles | 43/0; no formula defects |
| 2 (P1-C) | Data / cache / lineage | 38/0 after normalize fix |
| P1-D | Browser E2E | Composite OK; DCF-tab N/A → **closed by tab-visit calc** |
| Closeout | Residuals above | Structural tests + Rf UI + docs |

**Still out of scope by design:** full live Yahoo/TPEx matrix, multi-worker cache races, shinytest automation.

---

## Deploy impact

Tab-visit auto-calc and Rf fallback note are **app behavior** changes → ship with display version +0.01 after verification. Valuation formula engines unchanged.
