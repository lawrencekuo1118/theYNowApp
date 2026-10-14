# Phase 0 — Deploy baseline lock & audit re-run

**Status:** Complete (read-only for valuation formulas; no formula changes)  
**Captured (UTC):** 2026-10-14 13:36–13:37  
**Runner:** Cloud Agent workspace (`/workspace`)  
**Branch at capture:** `cursor/phase0-deploy-baseline-audit-3eea` (forked from `master` @ `11e3da6`)

Artifacts (full logs): `/opt/cursor/artifacts/phase0_audit/`  
Regenerated report in repo: `Audit_RCM_Report.md`  
Suite logs (gitignored): `audit_logs/*.log` (copied under artifacts)

---

## A. Version lock

| Item | Value |
|---|---|
| Workspace `HEAD` | `11e3da6ac6889de4513fec8cc39c90c9a2d57c4a` |
| `HEAD` subject | `Merge pull request #223 … sidebar-full-separators` |
| Working tree at start | Clean (no uncommitted edits before this Phase 0) |
| Deploy baseline file | `scripts/DEPLOY_BASELINE.txt` |
| Live URL | https://hopesmasher1118.shinyapps.io/TheYNowApp/ |
| Live `bundleId` | `12689425` |
| Live `display_version` | **v23.27** |
| Live aligned commit | `629c860eaa25822ee0035e0d6b6986560c43e323` |
| Baseline message | `chore: bump display version to v23.27` |

### HEAD vs deploy baseline

- Baseline **is an ancestor** of `HEAD` (5 commits ahead).
- Commits after baseline: display-version/baseline bookkeeping + Full sidebar separator UI (`ynow_ui.R` + `tests/test_sidebar_full_separators.R`).
- **`app_23.0/` valuation / formula / audit-suite sources are identical** between `629c860` and `11e3da6` for:
  - `setup.R`, `valuation_guard.R`, `ri_module.R`, `earnings_quality_module.R`, `default_config.R`, `market_profile.R`, `ynow_server.R`
  - `tests/test_data_integrity.R`, `tests/test_parameters_boundaries.R`, `tests/test_formula_accuracy.R`
- Therefore: **this re-run exercises the same valuation engine as the live shinyapps bundle** (v23.27 / bundle `12689425`), even though Git tip is slightly ahead for UI chrome.

### `app_14.0/` vs `app_23.0/`

| Path | Role in this repo |
|---|---|
| `app_23.0/` | **Live line** (audit target; deploy scripts; AGENTS.md) |
| `app_14.0/` | Legacy tree still referenced by stale README run/deploy snippets (~33 files vs ~196 in `app_23.0/`) |

**README drift (doc-only, not deploy truth):** README still says「目前版本：`app_14.0/`（v14.0…）」and `shiny::runApp("app_14.0")`, while also correctly saying deploy baseline is `scripts/DEPLOY_BASELINE.txt`. **Do not treat README v14.0 as the live app.**

### Note on earlier inventory numbers

An external inventory mentioned baseline `v23.23` / commit `44f8fce…`. **That is stale relative to this workspace.** Current recorded live baseline is **v23.27 / `629c860` / bundle `12689425`**.

---

## B. Independent re-run of existing audit

Command:

```bash
bash scripts/run_audit.sh
```

| Field | Result |
|---|---|
| Start (UTC) | 2026-10-14T13:36:13Z |
| End (UTC) | 2026-10-14T13:36:20Z |
| Wall time | ~7 seconds |
| Overall exit code | **0** |
| `YNOW_DEBUG_SKIP_PY` | `1` (script default) |
| R | R version **4.6.1** (2026-06-24) |
| Key packages | shiny 1.14.0; dplyr 1.2.1; data.table 1.18.4; jsonlite 2.0.0; memoise 2.0.1; digest 0.6.39 (`testthat` not installed; suites use custom PASS/FAIL printers) |

### Suite results (this run)

| Suite | Log | PASS | FAIL | Exit |
|---|---|---:|---:|---:|
| Phase 2 — Data Integrity | `audit_logs/test_data_integrity.log` | 103 | 0 | 0 |
| Phase 3 — Parameter & Boundary | `audit_logs/test_parameters_boundaries.log` | 86 | 0 | 0 |
| Phase 4 — Formula Accuracy | `audit_logs/test_formula_accuracy.log` | 68 | 0 | 0 |
| RCM generator | → `Audit_RCM_Report.md` | — | — | 0 |
| **Total** | | **257** | **0** | **0** |

Individual suites were **not** required as fallbacks (integrated runner succeeded).

Previous checked-in report was generated **2026-10-12** on Git `e54d92c`. This run regenerates the report on **2026-10-14** for Git `11e3da6` with the same 257/0 counts.

### Formula suite coverage observed in log (not exhaustive inventory)

Closed-form / hand-calc checks present in Phase 4 log include: WACC, CAPM Ke, DDM Gordon, DDM SPM, two-stage DDM, Gordon Terminal Value (+ PV), DCF Gordon EV, MOS, SGR identity, three-scenario DCF per-share / RI / MOS, F-Score, Beneish M-Score components, FCFE conversion identity.

**Still open for later phases (unchanged risk list):** independence of hand references vs engine; MULT/relative multiples completeness; live Yahoo/TPEx/cache E2E; browser UI E2E.

---

## C. Browser / UI E2E check (inventory only)

- No `shinytest` / `shinytest2` / `chromote` browser E2E harness found under `app_23.0/tests/`.
- Many **unit / static / locale / UI-structure** R tests exist (e.g. `test_lite_mode_ui.R`, `test_valuation_ui_param_verify.R`, sidebar/layout tests). These are **not** a substitute for full browser integration (input → live result update → charts/download).

---

## D. Evidence retained (no secrets)

1. Git commit + `DEPLOY_BASELINE.txt` lock → `docs/PHASE0_Deploy_Baseline_Audit.md` (this file) + artifacts `git_baseline_lock.txt`
2. Full suite logs → artifacts `audit_logs/*.log` (and local gitignored `audit_logs/`)
3. Regenerated `Audit_RCM_Report.md`
4. Console + env → artifacts `run_audit_console.log`, `r_env.txt`, `suite_summaries.txt`

No API keys / tokens were printed or stored.

---

## E. Conclusions for next phase

1. **Version alignment (P0):** Live deploy is **`app_23.0` / display `v23.27` / bundle `12689425` / commit `629c860`**. README `app_14.0` / v14.0 is documentation drift. Audit suites target `app_23.0` and match the live valuation tree.
2. **Independent re-run:** **PASS 257 / FAIL 0** on this environment — confirms the prior report’s counts are **reproducible**, not merely historical claims.
3. **Does not close** P1 independence review, MULT coverage, live data E2E, or browser E2E. Those remain next-phase work; do **not** treat this PASS as a final formula certification.

---

## How to reproduce

```bash
git rev-parse HEAD
cat scripts/DEPLOY_BASELINE.txt
bash scripts/run_audit.sh
# inspect:
#   Audit_RCM_Report.md
#   audit_logs/test_*.log
```
