# Audit Mapping — theYNowApp (`app_25.0`)

**Role:** ITGC / QA automation inventory  
**Scope:** Live line `app_25.0/` (display baseline per `scripts/DEPLOY_BASELINE.txt`)  
**Phase:** 1–4 complete · **5 complete** (Audit RCM Report)  
**Status:** `scripts/run_audit.sh` → `Audit_RCM_Report.md`

---

## 1. Data Flow（資料流進入點）

### 1.1 Online ingest — Yahoo / HTTP / TW markets

| Category | Entry (file → symbol) | Source | Cache / fallback |
|---|---|---|---|
| Financials + quote | `deep_scraper.py` → `scrape_all_financials*`, `get_summary_quote`, `get_price_history`, `get_market_caps_batch`, `get_returns_1y_batch`, `get_usd_twd_rate`, `get_risk_free_rate_yf`, `get_beta_unlever_inputs*` | Yahoo (`yfinance`) | R `web_crawler.R` memoise fast 15m / slow 24h |
| R bridge | `web_crawler.R` → `cached_scrape_financials`, `cached_get_summary_data`, `cached_get_yahoo_industry`, `cached_get_usd_twd_fx`, `cached_get_risk_free_rate*`, `fetch_beta_unlever_inputs*`, `fetch_sec_*`, `search_ticker_choices` | Calls Python via reticulate | Yahoo rate gate (`.ynow_yahoo_gate_*`) |
| TW Rf curve | `deep_scraper.py` → `get_tw_10y_gov_bond_yield` | TPEx Curve XLS | After fail → `market_profile()$rf_fallback` |
| TW statements fallback | `tpex_financial_summary.py` + `tpex_financial.R` → `apply_tpex_financial_fallback` | TPEx financial summary XLS | When Yahoo IS/BS/CF empty for `.TWO` / ESB |
| TW / US universes | `lab_tw_universe.R`, `lab_sp500_universe.R`, `lab_us_universe.R` | TWSE/TPEx OpenAPI, Wiki/GitHub, listing refresh | Offline CSV under `data/` |
| Backtest prices / Rf | `backtest_module.R` → `fetch_price_history_df`, `fetch_tnx_history_df`, `fetch_pit_rf_history_df`, `fetch_tpex_index_history_df` | Yahoo / TPEx index | Session + cache |
| Macro Buffett / WB | `macro_bubble_indicators.R` → DGBAS / World Bank fetchers | Online preferred | Offline `data/macro_buffett_*.csv` |
| Asset transmission | `asset_transmission_module.R` → `asset_tx_fetch_panel` | Yahoo bars | — |
| Corp logo colors | `corp_logo_colors.py` → `resolve_corp_logo_colors` | HTTP logo fetch | Cached |
| UI orchestration | `ynow_server.R` Search path | Summary → scrape → optional TPEx fallback | Reactives |

**No relational DB.** Persistence = `cachem`/`memoise` disk cache, CSV/JSON snapshots, `data/price_alerts.json`.

### 1.2 Offline snapshots / fixtures (`app_25.0/data`, `tests/data`)

| Artifact | Typical readers | Role |
|---|---|---|
| `data/tw_universe.csv`, `us_universe.csv`, `sp500_universe.csv` | Lab universe modules | Listing caches |
| `data/us_industry_overlay.csv` | `lab_us_*` overlay | Industry mapping |
| `data/universe_metrics_snapshot.csv` | `ynow_index.R`, `lab_industry_method.R` | Blue Chip / ranking features |
| `data/cluster_features_snapshot.csv` | `lab_clustering.R` | Offline cluster ratios |
| `data/ind_mcap_chain_rank_snapshot_US.csv` | `lab_industry_method.R` | Industry-chain rank default |
| `data/macro_buffett_*.csv` | `macro_bubble_indicators.R` | Buffett series fallback |
| `data/ynow_index_basket.csv`, `tynow_index_basket.csv` | `ynow_index.R` | Index baskets |
| `data/price_alerts.json` | `price_alert.R` | Value Alert store |
| `tests/data/*` | Unit tests / TPEx fixture | Offline fixtures |
| `business_breakdown_config.yaml`, `htcdi_config.yaml` | Config modules | Non-market config |

### 1.3 Python reticulate bridges

| Module | Loaded from | Notes |
|---|---|---|
| `deep_scraper.py` | `web_crawler.R` | Primary market data |
| `tpex_financial_summary.py` | `tpex_financial.R` | TW fallback |
| `corp_logo_colors.py` | `web_crawler.R` | UI branding |
| `valuation_pipeline/*.py` | **Not R-wired** | Standalone Phase-1 Python + tests |
| `app_25.0/scripts/*.py` | Offline builders | Snapshot generation |

---

## 2. Key Parameters（關鍵參數）

### 2.1 Master defaults — `default_config.R` → `APP_DEFAULTS`

| Key / symbol | Typical default | Controls |
|---|---|---|
| `stock_code`, `industry_choice`, `years` | TSM / `sc.Foundry` / **5** | Boot ticker, industry, forecast **n** |
| `capm_rf`, `capm_beta`, `capm_rm` | Rf≈5 (US fallback), industry β, Rm | CAPM → Ke |
| `wacc_re`, `wacc_rd`, `wacc_tax`, `wacc_rd_min/max` | Tax 21% US / 20% TW | WACC inputs |
| `sgr`, `wacc_gordon`, `dcf_mode`, `dcf_claim` | gordon / fcff | DCF Gordon vs two-stage; FCFF/FCFE |
| `yr_stage1`, `g_stage1/2`, `wacc_stage1/2` | stage years / growth / rates | Two-stage path |
| `perpetual_g_method`, `g_growth_method` | `"fundamental"` | Terminal / near-term g |
| `apply_g_ceiling` | TRUE | Near-term g clamp **−5%…25%** (`ynow_server.R`) |
| `apply_capex_spike_smooth`, `capex_spike_mult=1.35`, `capex_spike_avg_years=3` | CapEx spike heuristic |
| `ddm_*` | gordon / g=sgr / ke | DDM / GGM / SPM / two-stage |
| `ri_years`, `ri_roe`, `ri_payout` | placeholders until FS load | Residual Income |
| `pb_*`, `pb_target_mode` | justified + industry band | P/B |
| `nav_*` | holdco discount + multiples | NAV |
| `rel_*_multiple` | PE/EV/FCF/PS etc. | Relative multiples (App assumptions) |
| Checklist `cond_*` | MOS floor, HFV C-freq, F-Score min, … | Decision Checklist gates |
| Lab `lab_im_*` | boards, max N, methods | Blue Chip Lab |

Derived boot seeds: `default_beta`, `default_rm`, `default_debt`, `default_g`, `default_rf`, `default_re`, `default_tax`, `default_wacc`, `default_sgr`, `pb_lo/mid/hi` from `industry_standards[[DEFAULT_IND]]` + `market_profile("US")`.

### 2.2 Market / tax / beta sources — `market_profile.R`, `setup.R`

| Symbol | Role |
|---|---|
| `rf_fallback` US **5.0** / TW **1.8** | Engineering Rf after live + last-known fail |
| `wacc_tax` 21% / 20% | Statutory tax by market |
| `statutory_tax_ratio()`, `effective_tax_ratio_from_is()` | Tax helpers (IS clamp 0–50%) |
| `beta_u_apply_source`, `beta_bl_source` default `"summary"` | Yahoo Summary β vs other sources |
| `beta_lookback_months=60`, `beta_min_obs=24` | Rolling β cross-check only |

### 2.3 Bounds / clamps / guards

| Location | Rule |
|---|---|
| `valuation_guard.R` → `validate_stage_years`, WACC−g / Ke−g gates | Stage years; discount > terminal g |
| `setup.R` → `ratio_spike_vs_prior`, CapEx spike | Spike mult vs prior window |
| `setup.R` → `SHARE_UNIT_MISMATCH_RATIO_ADR` (1.25) | ADR share-scale guard |
| `earnings_quality_module.R` → `.EQ_THRESHOLDS` | M-Score alert **−1.78**, watch −2.22, AR–Rev / accruals |
| `ynow_index.R` → `YNOW_INDEX_N=10`, `YNOW_INDEX_MIN_FSCORE=8` | Index membership |
| `htcdi_config.R` → `HTCDI_DEFAULT_CONFIG` | Composite weights / caps |
| `industry_standards.R` → `industry_standards`, `get_box_color` | Peer KPI bands (heuristic) |
| `param_audit.R` | Canonical input-id → module catalog for restore / audit trails (not a formula engine) |

---

## 3. Core Formulas（核心公式位置）

### 3.1 Valuation models

| Model | Primary symbols | File |
|---|---|---|
| Hist FCFF / FCFE | `reconstruct_hist_fcff`, `fcff_to_fcfe*` | `setup.R` |
| DCF EV / TV / equity | `.dcf_formula_ev*`, `dcf_gordon_tv*`, `dcf_ev_to_equity`, `value_dcf_per_share` | `setup.R`, `valuation_guard.R` |
| DCF projection UI | `fcf_projection_module_server` | `fcf_projection_module.R` |
| DDM GGM / SPM / 2-stage | `.ddm_formula_p0`, `.ddm_formula_spm`, `.ddm_formula_two_stage` | `setup.R` (+ `ddm_module.R`) |
| Residual Income | `compute_ri_valuation`, `.ri_formula_v`, `build_roe_path` | `ri_module.R` |
| NAV | `extract_nav_components` + `nav_module_server` | `setup.R`, `nav_module.R` |
| P/B | `derive_pb_targets`, `.pb_formula_p`, `assess_pb_applicability` | `setup.R`, `pb_asset_module.R`, `valuation_guard.R` |
| Relative multiples | `calc_pe_implied_price`, `calc_ev_*`, `calc_ps_*`, `multiple_implied_price` | `relative_multiples_module.R`, `setup.R` |
| SOTP | `sotp_implied_price`, `sotp_module_server` | `setup.R`, `sotp_module.R` |

### 3.2 Quality / screens

| Metric | Primary symbols | File |
|---|---|---|
| Piotroski F-Score | `compute_report_f_score` | `setup.R` (+ YNOW `investment_decision_module.R`) |
| Beneish M-Score | `.eq_beneish_components`, `.eq_beneish_m`, `calculate_m_score`, `evaluate_earnings_quality` | `earnings_quality_module.R` |
| Shenanigans / fraud flags | `evaluate_shenanigans`, `collect_fraud_warnings` | `shenanigans_module.R`, `setup.R` |

### 3.3 Rates, growth, MOS, beta

| Topic | Primary symbols | File |
|---|---|---|
| WACC / CAPM Ke | `compute_wacc`, `capm_cost_of_equity`, `.capm_ke_pct` | `valuation_guard.R`, `setup.R` |
| SGR / terminal g | `calc_fundamental_sgr_pct`, `estimate_perpetual_g`, `estimate_lifecycle_terminal_g` | `setup.R`, `lifecycle_classification.R` |
| MOS | `mos_vs_fair_value` | `setup.R` |
| Unlever β (Hamada) | `.unlever_beta` | `ynow_server.R` |
| Rolling β (cross-check) | `estimate_rolling_beta` | `backtest_module.R` |

### 3.4 Model selection / guards / KPIs

| Topic | Primary symbols | File |
|---|---|---|
| Model recommendation / insufficient FV | `assemble_model_recommendation`, `recommend_valuation_models`, `score_valuation_confidence` | `valuation_guard.R`, `setup.R` |
| Decision Checklist SOP | `decision_checklist_server` | `decision_checklist_module.R` |
| Dashboard KPIs (ROA/ROE/margins/…) | `kpi_module.R` outputs; `build_report_kpi_df` | `kpi_module.R`, `setup.R` |
| HFV / MOS efficacy | `validate_mos_effectiveness`, `build_hfv_*` | `backtest_validation.R` |
| HTCDI | `htcdi_composite`, scorers | `htcdi_engine.R` |

---

## 4. Existing automated coverage (inventory only)

Already under `app_25.0/tests/` (reuse in Phase 2–5; do not duplicate blindly):

| Area | Representative tests |
|---|---|
| Formula / DCF / FCFE | `test_valuation_formulas.R`, `test_valuation_guard.R`, `test_valuation_ui_param_verify.R`, `test_amzn_dcf_edges.R`, `test_tsm_dcf_adr_fx.R` |
| MOS / alignment | `test_mos_vs_fair_value.R`, `test_valuation_alignment.R`, `test_valuation_audit.R` |
| F-Score / EQ / Beneish | `test_fscore_liquidity.R`, `test_earnings_quality.R`, `test_shenanigans.R` |
| Params / restore | `test_param_restore.R`, `test_sidebar_default.R`, `test_market_profile.R`, `test_macro_rf_g.R` |
| Data / fallback | `test_financials_empty_fallback.R`, `test_tpex_financial_summary.R`, `test_cache_tiers.R`, `test_yahoo_rate_gate.R` |
| Snapshots / lab | `test_lab_universe_metrics.R`, `test_lab_quality_snapshot.R`, `test_macro_bubble_indicators.R` |
| Python pipeline | `test_valuation_pipeline.py` (standalone; not R-wired) |

Framework in-repo: standalone Rscript suites + Phase 5 aggregator `scripts/run_audit.sh` → `Audit_RCM_Report.md`.

---

## 5. Phase 2–5 status

| Phase | Candidate targets | Status |
|---|---|---|
| 2 Data integrity | Yahoo/TPEx shaped statements; summary quote fields; snapshot CSV schemas | **Done** — `test_data_integrity.R` |
| 3 Parameter / boundary | `APP_DEFAULTS` lock; WACC−g; CapEx spike; g ceiling; ADR share unit; M-Score thresholds | **Done** — `test_parameters_boundaries.R` |
| 4 Formula accuracy | DCF Gordon/2-stage; DDM; RI; F-Score 9 signals; Beneish 8 components; MOS; SGR; WACC | **Done** — `test_formula_accuracy.R` |
| 5 RCM report | Aggregate suites → `Audit_RCM_Report.md` | **Done** — `scripts/run_audit.sh` |

---

## 6. Phase 2–5 deliverables

| Artifact | Role |
|---|---|
| `app_25.0/tests/test_data_integrity.R` | Completeness + Accuracy on data-flow entry points (offline / mock) |
| `app_25.0/tests/test_parameters_boundaries.R` | Default lock + boundary fail-soft |
| `app_25.0/tests/test_formula_accuracy.R` | Closed-form + 3 scenarios; F-Score 9; Beneish 8 |
| `scripts/run_audit.sh` | Runs Phase 2–4 suites; writes logs under `audit_logs/` |
| `scripts/generate_audit_rcm.R` | Parses logs → Risk and Control Matrix markdown |
| `Audit_RCM_Report.md` | Regenerated RCM (Audit Item / Target / Objective / Result / Findings) |
| `.github/workflows/audit.yml` | Optional CI: run `scripts/run_audit.sh` on push/PR touching audit paths |

```bash
bash scripts/run_audit.sh
# or individual suites:
cd app_25.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_data_integrity.R
cd app_25.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_parameters_boundaries.R
cd app_25.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_formula_accuracy.R
```
