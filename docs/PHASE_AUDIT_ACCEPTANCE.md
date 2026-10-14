# Audit acceptance evidence — beyond structural closeout

**Status:** Objective evidence recorded for tab-visit **Shiny runtime**, Rf numeric impact, **US+TW live Rf**, and **live Yahoo/TPEx statements** (with local `.ynow_venv`)  
**Live baseline:** `app_24.0/` / display **v24.00** (bundle `12690522`)  
**Prior ship on v23 line:** v23.31 / bundle `12690471`  
**Formulas:** unchanged (no engine rewrites)

---

## v24 includes prior audit fixes

Ancestry of the v24 cut (`bc4e464` → `ce1caec`) includes:

| Fix / control | Commit family |
|---|---|
| Closeout runner `pipefail` + source gate + shim | `f2ab63c` / `a34fe1c` / `3a3435a` |
| Tab-visit auto-calc + Rf fallback UI | `fc3418c` (v23.30) |
| Acceptance tab-visit gates + Rf impact | `1ab570f` |
| US quantmod Rf + TW market mode | `4e4db61` (v23.31) |
| Tab-visit `testServer` + US/TW live Rf suites | `524311e` |
| Python Rf error → quantmod fallthrough | `6e860ba` |
| Live line cut to `app_24.0` / v24.00 | `bc4e464` … `ce1caec` |

---

## Objective checklist

| Requirement | Evidence | Status |
|---|---|---|
| Tab-visit **runtime** (not grepl-only) | `test_audit_tab_visit_shiny_runtime.R` via `shiny::testServer`: FV empty → fire; FV present → skip; companions fire; heal after clear | **Met** |
| Tab-visit gate unit + server wire | `test_audit_tab_visit_runtime.R` + `tab_visit_calc_gates.R` in `ynow_server.R` | **Met** |
| Rf numeric impact (Ke / WACC / FV) | `test_audit_rf_impact.R` (β>1 and β<1 CAPM identity) | **Met** |
| Live data paths — US Rf | quantmod `^TNX` and/or Python; `source=live` | **Met** |
| Live data paths — TW Rf | Python `get_risk_free_rate_yf("TW")` via `.ynow_venv`; `source=live` | **Met when venv present** |
| Live Yahoo statements (US + TW + OTC quote) | `test_audit_live_statements.R` — AAPL / 2330.TW / 3363.TWO non-empty IS/BS/CF | **Met** |
| Live TPEx XLS summary + merge | Same suite — OTC raw + `apply_tpex_financial_fallback`; CF not invented | **Met** |

Browser UI smoke remains complementary: primary auto-calc on ticker load often fills FV first; tab-visit is the **heal path** when FV is missing — now proven under `testServer` with empty FV.

---

## How to run

```bash
# Optional but recommended for TW live Rf + statements:
# python3 -m venv app_24.0/.ynow_venv && app_24.0/.ynow_venv/bin/pip install -r app_24.0/requirements.txt

bash scripts/run_audit_acceptance.sh
```

Defaults: US live Rf required; TW live required when `.ynow_venv` exists (`YNOW_ACCEPTANCE_REQUIRE_TW_LIVE=auto`); live statements required (`YNOW_ACCEPTANCE_REQUIRE_LIVE_STATEMENTS=1`).

---

## Key suites (latest local acceptance)

| Suite | Typical result |
|---|---|
| tab-visit gates | PASS=16 |
| tab-visit Shiny runtime | PASS=8 |
| Rf impact | PASS=18 |
| Phase 1 / Phase 2 | 43/0 · 38/0 |
| live Rf paths | US live + TW live (with venv) |
| live statements | Yahoo AAPL / 2330.TW / 3363.TWO + TPEx OTC merge |

---

## App defects fixed before / in v24 cut (already shipped)

1. US Rf quantmod path when Python unavailable  
2. `.rf_market_mode("TW")` no longer collapses to US without `normalize_market_mode`  
3. Python US Rf scrape errors fall through to quantmod  

---

## Still out of scope / not claimed

- Multi-worker shared-cache stamp races (LIVE-UNCOVERED-3)  
- Quote-fresh vs statement-stale timing races (LIVE-UNCOVERED-4)  
- chromote full-app E2E  
- “Every valuation on every ticker is numerically perfect in production”
