# Audit acceptance evidence — beyond structural closeout

**Status:** Advanced acceptance evidence recorded; **not** a claim that every live market path is closed  
**Branch:** `cursor/audit-acceptance-runtime-evidence-3eea`  
**Parent closeout:** `cursor/audit-closeout-remaining-3eea`  
**Recorded ship before this revision:** v23.30 / bundle `12690107`

Valuation **formula engines** were not rewritten. This revision adds runtime gate tests, Rf→Ke/WACC/FV impact controls, a US live Rf path when Python is absent, a TW market-mode probe fix, and an acceptance runner with captured exits.

---

## What “correct” means after this revision

| Area | Evidence level | Verdict |
|---|---|---|
| DCF / RI / DDM / MULT formulas (offline independent oracles) | Phase 1 **PASS=43 FAIL=0** | No proven formula defects |
| Normalize / cache / lineage (offline) | Phase 2 **PASS=38 FAIL=0** | Known dup-period defect remains fixed |
| Closeout runner failure propagation | Source gate + actual-runner shim | Reliable |
| Tab-visit auto-calc **gates** (fire / wait / skip) | Pure helpers + fire callback **PASS=16** + server wired to helpers | **Closed at gate layer** |
| Tab-visit **Shiny UI** proves calc-on-tab vs calc-on-load | Browser `Shiny.setInputValue` to `dcf_calculator` | **Partial:** navigation works; values already present from primary auto-calc on ticker load — tab-visit is a **heal path when FV missing** |
| CAPM Rf → Ke → WACC → DCF/FCFE FV (offline) | `test_audit_rf_impact.R` **PASS=18** | **Closed** (includes β>1 vs β<1 sign of ΔKe) |
| US live Rf (^TNX) | `test_audit_live_rf_us.R` + quantmod path when Python absent | **Closed in this environment** (`source=live`) |
| TW live Rf (TPEx) | Needs Python; without it → TW engineering fallback (~1.8%), not US ^TNX | **Live TW open**; mislabel bug fixed |
| Full live Yahoo/TPEx statement matrix | Out of scope | **Open** |

**Honest product statement:** Offline formula/data controls, US live Rf, and tab-visit **gates** are in good standing. That is **not** the same as “every UI path and every live market feed is proven perfect.”

---

## Proven defects fixed this revision

1. **US Rf when Python unavailable** — previously fell to engineering 5% even though Yahoo `^TNX` is reachable via quantmod. Added `.fetch_rf_live_us_quantmod()`.  
2. **`.rf_market_mode("TW")` collapsed to US** when `normalize_market_mode` was not loaded (e.g. crawler sourced alone) — TW probes could report US ^TNX as “TW live”. Now honors explicit TW.

---

## Tab-visit architecture (observed)

1. Primary auto-calc on ticker search often fills `stock_price_estimate_val` before any model tab visit.  
2. DCF tab-visit fires only when searched + ready + **FV missing** (`tab_visit_dcf_should_fire`).  
3. Companion DDM/RI/MULT fire when searched + ready (`tab_visit_companion_*`).  
4. Browser session: `Shiny.setInputValue('sidebar_tabs','dcf_calculator')` switched views; FV already populated — so this session does **not** isolate a fresh tab-visit fire.  
5. Offline gate harness **does** prove fire / skip / wait with an observable callback.

Artifacts: `/opt/cursor/artifacts/acceptance_tab_visit/`

---

## Rf numeric impact (offline)

With Rm fixed, CAPM identity `Ke = β·Rm + Rf·(1−β)` implies:

- **β > 1:** higher fallback Rf **lowers** Ke / WACC and **raises** DCF FV vs a lower live-like Rf  
- **β < 1:** higher fallback Rf **raises** Ke / WACC and **lowers** DCF FV  

Fixture (β=1.2, Rf 5.0% vs 4.25%): ΔKe ≈ −15 bp, ΔWACC ≈ −10.5 bp, ΔDCF per-share ≈ +2.1%.

---

## How to run

```bash
bash scripts/run_audit_acceptance.sh
# Default: YNOW_ACCEPTANCE_REQUIRE_LIVE_RF=1 (US live required)

YNOW_ACCEPTANCE_REQUIRE_LIVE_RF=0 bash scripts/run_audit_acceptance.sh
```

---

## Still open (do not over-claim)

- TW **live** TPEx Rf (Python scraper)  
- Full Yahoo/TPEx statement HTTP matrix and multi-worker cache races  
- Isolating a browser session where FV is empty so DCF tab-visit fire is the **first** write to session FV  
- chromote / shinytest harness (still deferred)

---

## Deploy impact

US Rf quantmod live path + TW market-mode probe fix are **app behavior** → ship with display +0.01 when verified. Formulas unchanged.
