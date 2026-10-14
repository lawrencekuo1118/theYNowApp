# Audit RCM Report — theYNowApp (`app_23.0`)

**Format:** Risk and Control Matrix (RCM)
**Generated at (UTC):** 2026-10-14 13:36:19 UTC
**Git:** `11e3da6`
**Overall result:** **PASS**

Mapped from Phase 1 `Audit_Mapping.md` controls; executed by `scripts/run_audit.sh`.

---

## Suite summary

| Suite | Script | PASS | FAIL | Result |
|---|---|---:|---:|---|
| Phase 2 — Data Integrity | `app_23.0/tests/test_data_integrity.R` | 103 | 0 | Pass |
| Phase 3 — Parameter & Boundary | `app_23.0/tests/test_parameters_boundaries.R` | 86 | 0 | Pass |
| Phase 4 — Formula Accuracy | `app_23.0/tests/test_formula_accuracy.R` | 68 | 0 | Pass |

---

## Risk and Control Matrix

| Audit Item | Target Function / Module | Control Objective | Test Result | Findings / Remediation |
|---|---|---|---|---|
| Offline universe / snapshot CSV schema & non-blank keys | `lab_*_universe CSV; universe_metrics_snapshot; cluster_features_snapshot` | Completeness | Pass | — |
| Macro Buffett time-series continuity | `data/macro_buffett_*.csv` | Completeness | Pass | — |
| Mock Yahoo payload → normalize_all_financials / metric extract | `setup.R::normalize_all_financials / select_clean_metric_row / parse_financial_number` | Accuracy | Pass | — |
| Mock Summary quote / TW finalize / metrics snapshot loader | `extract_quote_price_mcap; lab_finalize_tw_list; lab_load_universe_metrics_snapshot` | Accuracy | Pass | — |
| APP_DEFAULTS & market Rf/tax / CapEx spike / EQ / YNOW lock | `default_config.R::APP_DEFAULTS; market_profile.R; .EQ_THRESHOLDS; ynow_index.R` | Accuracy | Pass | — |
| Near-term g ceiling (−5%…25%) & CapEx spike edges | `ynow_server.R::.clamp_near_term_g_pct; setup.R::ratio_spike_vs_prior / fcf_sync_*` | Accuracy | Pass | — |
| DCF WACC≤g / shares≤0 / neg terminal CF fail-soft | `valuation_guard.R::value_dcf_per_share / validate_stage_years / compute_wacc` | Accuracy | Pass | — |
| P/B neg equity, MOS near-zero, PE nonpositive EPS, WACC resolve | `derive_pb_targets; mos_vs_fair_value; calc_pe_implied_price; resolve_wacc_input` | Accuracy | Pass | — |
| Closed-form WACC / CAPM / DDM / Gordon TV / MOS / SGR / FCFE | `setup.R / valuation_guard.R / .ddm_formula_* / dcf_gordon_tv / fundamental_sgr_identity` | Accuracy | Pass | — |
| Three scenarios (base/bull/bear) DCF per-share / RI / MOS | `value_dcf_per_share; ri_module.R::compute_ri_valuation; mos_vs_fair_value` | Accuracy | Pass | — |
| F-Score 9-signal decomposition (excellent/mixed/poor) | `setup.R::compute_report_f_score` | Accuracy | Pass | — |
| Beneish 8-component + M-Score hand calc (base/bull/bear) | `earnings_quality_module.R::calculate_m_score / .eq_beneish_components` | Accuracy | Pass | — |

---

## Detailed findings

No FAIL items. All Phase 2–4 controls passed.

---

## How to reproduce

```bash
bash scripts/run_audit.sh
# or:
cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_data_integrity.R
cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_parameters_boundaries.R
cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_formula_accuracy.R
```

## Scope notes

- Offline / mock-driven; no live Yahoo or TPEx network required for Phase 2–4 suites.
- Phase 1 inventory: `Audit_Mapping.md`.
- This report is regenerated on each `run_audit.sh` invocation (do not hand-edit).

