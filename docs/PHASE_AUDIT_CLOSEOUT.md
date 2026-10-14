# Audit closeout — remaining steps

**Status:** Structural closeout + runner P0 verified; **runtime / live Rf / live statements** tracked in `docs/PHASE_AUDIT_ACCEPTANCE.md` under **v24.00**  
**Branch (historical):** `cursor/audit-closeout-remaining-3eea`  
**Current live baseline:** `app_24.0/` / **v24.00** / bundle `12690522`  
**Recorded ship (behavior on v23 line):** v23.30 / bundle `12690107` (tab-visit auto-calc + Rf fallback note)  
**Prior normalize ship:** v23.29 / bundle `12689855`

This document tracks residual items after Phase 0–2 and P1-D. Valuation formula engines are unchanged and are **not** blocked by these test-control / coverage gaps. For objective acceptance evidence beyond structural closeout, see **`PHASE_AUDIT_ACCEPTANCE.md`**.

---

## Acceptance gate (current)

| Gate | State |
|---|---|
| Production runner enables `set -uo pipefail` (source gate on `scripts/run_audit_closeout.sh`) | **Required — verified** |
| Actual-runner integration: failing structural `Rscript` via PATH shim → runner exit ≠ 0 | **Required — verified** |
| Full closeout + Phase 2 re-run with complete logs and captured exit codes | **Required — see evidence below** |
| Tab-visit auto-calc interactive / runtime proof | **Closed under acceptance** — `testServer` + gate unit + chromote E2E |
| Rf fallback note UI | **Fallback** structure kept; **live US+TW Rf** proven in acceptance |
| Live Yahoo / TPEx statements | **Closed under acceptance** — `test_audit_live_statements.R` |
| Multi-worker cache / quote-vs-statement timing | **Closed under acceptance** — `test_audit_live_cache_races.R` |
| chromote / shinytest harness | **Closed under acceptance** — `test_audit_chromote_e2e.R` |
| Deploy baseline ↔ bundle ↔ display version | Live: `12690593` / `v24.01` (`app_24.0/`) |

**Conclusion:** Structural closeout + runner P0 are required gates; they alone are not full acceptance. Full objective evidence (including LIVE-UNCOVERED-3/4 and chromote E2E) is recorded on the v24 baseline in `PHASE_AUDIT_ACCEPTANCE.md`.

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
| P1D-DCF-TAB-NA | P2→P1 UX | **Closed (acceptance)** — structural + `shiny::testServer` empty-FV runtime. chromote deferred. |
| FAILSOFT-RF-FALLBACK | P1 | **UI** bilingual note when fallback; **live US+TW Rf** proven in acceptance. |
| BRIDGE-ZERO-DEFAULT | P1 | **Accepted / documented** — intentional shared bridge; not changed. |
| LIVE-UNCOVERED-1/2 | P1 | **Closed (acceptance)** — Yahoo + TPEx live statements suite. |
| LIVE-UNCOVERED-3/4 | P1 | **Closed (acceptance)** — multi-worker gate/cache + quote/statement tier suite. |
| P1D-NO-SHINYTEST | P2 | **Closed (acceptance)** — `test_audit_chromote_e2e.R` CDP harness. |
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
| P1-D | Browser E2E | Composite OK; tab-visit runtime closed under acceptance (`testServer`) |
| Closeout | Residuals + runner reliability | Runner P0 verified; acceptance continues on v24 |
| Acceptance (v24) | Tab-visit runtime + live Rf + live statements | See `PHASE_AUDIT_ACCEPTANCE.md` |

**Still out of scope by design:** claiming every production ticker’s valuation is numerically perfect.

---

## Deploy decision

**Live baseline:** `app_24.0/` / **v24.00** / bundle `12690522` (includes prior audit behavior fixes).

Historical v23 ships retained for lineage: v23.29 normalize; v23.30 tab-visit + Rf note; v23.31 US/TW Rf.

**This revision (live-statements acceptance):** tests + documentation only → **do not deploy** solely for these changes unless an app defect is fixed.

Valuation **formulas** unchanged.
