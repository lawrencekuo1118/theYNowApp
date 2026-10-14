# Audit closeout — remaining steps

**Status:** Not final acceptance — runner P0 verified on actual script; tab-visit runtime and live Treasury still open  
**Branch:** `cursor/audit-closeout-remaining-3eea`  
**Recorded ship (behavior):** v23.30 / bundle `12690107` (tab-visit auto-calc + Rf fallback note)  
**Prior normalize ship:** v23.29 / bundle `12689855`

This document tracks residual items after Phase 0–2 and P1-D. Valuation formula engines are unchanged and are **not** blocked by these test-control / coverage gaps.

---

## Acceptance gate (current)

| Gate | State |
|---|---|
| Production runner enables `set -uo pipefail` (source gate on `scripts/run_audit_closeout.sh`) | **Required — verified** |
| Actual-runner integration: failing structural `Rscript` via PATH shim → runner exit ≠ 0 | **Required — verified** |
| Full closeout + Phase 2 re-run with complete logs and captured exit codes | **Required — see evidence below** |
| Tab-visit auto-calc interactive / runtime proof | **Open** — structural `grepl` only; full browser E2E deferred |
| Rf fallback note UI | **Fallback path** structure / failsoft note; **live Treasury path** still open |
| Deploy baseline ↔ bundle ↔ display version | Recorded: `12690107` / `v23.30` (no redeploy for test-runner/docs) |

**Conclusion:** Do **not** treat this closeout branch as final audit acceptance. Runner exit 0 is only meaningful after the pipefail source gate and actual-runner shim integration pass; tab-visit and live Rf remain open.

---

## P0 — Closeout runner failure propagation

### Committed runner requirement

`scripts/run_audit_closeout.sh` must contain an exact line:

```bash
set -uo pipefail
```

Without `pipefail`, Bash pipeline status follows `tee` (exit 0), so:

```bash
( Rscript tests/test_audit_closeout.R ) 2>&1 | tee "$LOG" || ec=1
```

can leave `ec=0` when `Rscript` fails.

### Regression controls (committed)

| Script | Role | Invoked by runner? |
|---|---|---|
| `scripts/test_audit_closeout_pipefail.sh` | **Source gate** on the production runner for `set -uo pipefail` + local `Rscript\|tee` sanity proof + negative fixture (`set -u` alone rejected) | Yes (meta step) |
| `scripts/test_audit_closeout_runner_exit.sh` | **Integration:** PATH shim fails only `tests/test_audit_closeout.R`; invokes **actual** `run_audit_closeout.sh`; captures exit via redirection (not outer `\| tee`); expects non-zero | **No** (standalone — avoids recursion) |

A duplicated local pipeline alone is **not** sufficient: removing `pipefail` from the real runner must fail the source gate, and the shim integration must fail if the real runner swallows status.

### Verified evidence (Cloud Agent, 2026-10-14)

Environment: Linux / bash 5.2.21 / Rscript 4.6.1  
HEAD at verification: see latest commit on this branch (must match `origin/cursor/audit-closeout-remaining-3eea`).

| Step | Command | Exit | Notes |
|---|---|---|---|
| Meta | `bash scripts/test_audit_closeout_pipefail.sh` | **0** | Source gate PASS; local proof PASS |
| Integration | `bash scripts/test_audit_closeout_runner_exit.sh` | **0** | Actual runner exit **1** with shim; `Closeout exit=1` in log |
| Closeout | `bash scripts/run_audit_closeout.sh` | **0** | Structural **PASS=8 FAIL=0**; Phase 2 nested **PASS=38 FAIL=0**; `Closeout exit=0` |
| Phase 2 | `bash scripts/run_phase2_audit.sh` | **0** | **PASS=38 FAIL=0**; `Phase 2 exit=0` |
| Negative | Same integration against temporarily stripped runner (`set -u` only, meta skipped) | harness **1** | Runner exited **0** (`Closeout exit=0`) despite shim — proves swallow; production runner restored afterward |
| Source negative | Meta against stripped runner | **1** | `FAIL pipefail source gate` |

Artifact logs (must be non-empty to claim pass):

- `/opt/cursor/artifacts/audit_closeout_verify_complete.log`
- `/opt/cursor/artifacts/audit_closeout_meta_pipefail.log`
- `/opt/cursor/artifacts/audit_closeout_integration_harness.log`
- `/opt/cursor/artifacts/audit_closeout_full_rerun.log`
- `/opt/cursor/artifacts/audit_closeout_phase2_standalone.log`
- `/opt/cursor/artifacts/audit_closeout_integration_stripped_pipefail.log`
- `/opt/cursor/artifacts/audit_closeout_pipefail_stripped_runner.log`

Do **not** claim a suite passed if its log is incomplete or its exit status was captured only through an outer `| tee` without `pipefail` / `PIPESTATUS`.

---

## Remaining gaps → disposition

| Prior ID | Risk | Disposition |
|---|---|---|
| P1D-DCF-TAB-NA | P2→P1 UX | **Design in code** (tab-visit fire when ready / FV missing). **Proof: structural only** — runtime open. |
| FAILSOFT-RF-FALLBACK | P1 | **UI** bilingual `capm_rf_fallback_note` when `is_fallback=TRUE`. Live Treasury success **open**. |
| BRIDGE-ZERO-DEFAULT | P1 | **Accepted / documented** — intentional shared bridge; not changed. |
| LIVE-UNCOVERED-* | P1 | Live Yahoo/TPEx HTTP still uncovered; `YNOW_LIVE_PROBE=1` is best-effort. |
| P1D-NO-SHINYTEST | P2 | **Deferred** — no chromote / shinytest harness. |
| NORM-DUP-PERIOD-CORRUPT | P1 | **Shipped** v23.29; regression in Phase 2 + closeout. |
| CLOSEOUT-RUNNER-PIPEFAIL | P0 | **Fixed and regression-tested** against the actual runner (source gate + shim integration). |

---

## P1 — Tab-visit auto-calc (structural vs runtime) — OPEN

`tests/test_audit_closeout.R` uses `grepl()` for observers / `.fire_auto_calc_primary` / DCF skip-when-FV. That proves **structure**, not Shiny runtime tab-switch behavior.

| Runtime question | Automated proof today |
|---|---|
| Switch to DCF with inputs ready → valuation runs | **No** (deferred E2E) |
| Switch to DDM / RI / MULT → model calc | **No** |
| Inputs not ready → wait; FV present → skip DCF recalc | Structure only |
| Calc failure keeps clear error vs stale fair value | **No** |

Observational smoke (Smart Analysis numbers for a preloaded ticker) does **not** close this item. Keep status: **structural control established; execution-layer pending**.

---

## P1 — CAPM Rf: fallback vs live — OPEN for live path

| Path | Proven | Not proven |
|---|---|---|
| Fallback | Locale + `is_fallback` UI wiring; probes that return fallback | — |
| Live Treasury | Helper exists | Successful live ^TNX / TPEx in this program |
| Numeric impact | Fallback uses `APP_DEFAULTS$capm_rf` / `default_rf` (else 5%) via `.current_rf_detail()` into CAPM Ke / WACC paths | Dedicated live-vs-fallback Ke/WACC/FV oracle |

---

## Code / control changes

1. `ynow_server.R` / `ui_locale.R` — tab-visit auto-calc + Rf fallback note (**shipped v23.30**).  
2. `tests/test_audit_closeout.R` — structural checks (not E2E).  
3. `scripts/run_audit_closeout.sh` — **`set -uo pipefail`**; embeds meta source/local proof only.  
4. `scripts/test_audit_closeout_pipefail.sh` — source gate + local tee proof.  
5. `scripts/test_audit_closeout_runner_exit.sh` — actual-runner + Rscript shim integration (standalone).  
6. This report — matches committed controls and rerun evidence; **not final acceptance**.

---

## Verification commands

```bash
bash scripts/test_audit_closeout_pipefail.sh
bash scripts/test_audit_closeout_runner_exit.sh   # expect harness exit 0; inner runner exit 1
bash scripts/run_audit_closeout.sh                 # expect Closeout exit=0; PASS=8; Phase2 38/0
bash scripts/run_phase2_audit.sh                   # expect Phase 2 exit=0; PASS=38
```

Optional live probe (does not close live-Treasury gate by itself):

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
| P1-D | Browser E2E | Composite OK; tab-visit **runtime still open** |
| Closeout | Residuals + runner reliability | Runner P0 verified on actual script; **not final acceptance** |

**Still out of scope by design:** full live Yahoo/TPEx matrix, multi-worker cache races, shinytest / chromote automation.

---

## Deploy decision

**Retain:** v23.29 normalize; v23.30 tab-visit design + Rf fallback note (`DEPLOY_BASELINE.txt` → bundle `12690107`, `display_version=v23.30`).

**This revision:** test-runner + documentation only → **do not deploy** solely for these changes.

**Still required before final acceptance:** tab-visit runtime demonstration (or continued explicit deferral) and any live-Rf integration proof you want in scope. Valuation **formulas** unchanged.
