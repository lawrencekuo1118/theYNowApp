# Audit acceptance evidence — beyond structural closeout

**Status:** Objective evidence recorded for tab-visit **Shiny runtime**, Rf numeric impact, and **US+TW live Rf** (with local `.ynow_venv`)  
**Live baseline:** `app_24.0/` / display **v24.00** (cut from v23.31 acceptance tip)  
**Prior ship on v23 line:** v23.31 / bundle `12690471`  
**Formulas:** unchanged (no engine rewrites)

---

## Objective checklist

| Requirement | Evidence | Status |
|---|---|---|
| Tab-visit **runtime** (not grepl-only) | `test_audit_tab_visit_shiny_runtime.R` via `shiny::testServer`: FV empty → fire; FV present → skip; companions fire; heal after clear | **Met** |
| Tab-visit gate unit + server wire | `test_audit_tab_visit_runtime.R` + `tab_visit_calc_gates.R` in `ynow_server.R` | **Met** |
| Rf numeric impact (Ke / WACC / FV) | `test_audit_rf_impact.R` (β>1 and β<1 CAPM identity) | **Met** |
| Live data paths — US Rf | quantmod `^TNX` and/or Python; `source=live` | **Met** |
| Live data paths — TW Rf | Python `get_risk_free_rate_yf("TW")` via `.ynow_venv`; `source=live` ≈2.02% | **Met when venv present** |
| Full Yahoo/TPEx **statement** HTTP matrix | Still out of scope for this objective | Open (broader than Rf/live Macro) |

Browser UI smoke remains complementary: primary auto-calc on ticker load often fills FV first; tab-visit is the **heal path** when FV is missing — now proven under `testServer` with empty FV.

---

## How to run

```bash
# Optional but recommended for TW live:
# python3 -m venv app_24.0/.ynow_venv && app_24.0/.ynow_venv/bin/pip install -r app_24.0/requirements.txt

bash scripts/run_audit_acceptance.sh
```

Defaults: US live Rf required; TW live required when `.ynow_venv` exists (`YNOW_ACCEPTANCE_REQUIRE_TW_LIVE=auto`).

---

## Key suites (latest local acceptance)

| Suite | Typical result |
|---|---|
| tab-visit gates | PASS=16 |
| tab-visit Shiny runtime | PASS=8 |
| Rf impact | PASS=18 |
| Phase 1 / Phase 2 | 43/0 · 38/0 |
| live Rf paths | US live + TW live (with venv) |

---

## App defects fixed on this branch (already shipped in v23.31)

1. US Rf quantmod path when Python unavailable  
2. `.rf_market_mode("TW")` no longer collapses to US without `normalize_market_mode`

---

## Still out of scope / not claimed

- Full live financial-statement Yahoo/TPEx matrix  
- Multi-worker cache races  
- chromote full-app E2E  
- “Every valuation on every ticker is numerically perfect in production”
