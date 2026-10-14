# Phase 2 — Live Data, Normalization & Cache Integration Audit (P1-C)

**Status:** Offline/structural controls executed; **live HTTP not claimed**  
**Captured (UTC):** 2026-10-14 (Cloud Agent)  
**Runner:** `bash scripts/run_phase2_audit.sh`  
**Suite:** `app_23.0/tests/test_phase2_data_integration.R`  
**Oracles:** `app_23.0/tests/audit_data_oracles.R`  
**Findings JSON:** `docs/phase2_findings.json`

### Deployment baseline locked before testing

| Item | Value |
|---|---|
| Live line | `app_23.0/` |
| `display_version` | **v23.28** |
| `bundleId` | `12689606` |
| Aligned commit | `946a42d` |
| URL | https://hopesmasher1118.shinyapps.io/TheYNowApp/ |
| Workspace tip at run | see `git rev-parse HEAD` in runner log |
| Valuation engines modified? | **No** |

**Honesty rule:** Offline mock PASS ≠ live Yahoo/TPEx integration coverage. Live gaps are listed as `coverage_class=live_uncovered` INFO rows.

---

## Suite counts (this run)

| Metric | Value |
|---|---|
| PASS | **37** |
| FAIL | **1** |
| Findings rows | **30** |
| Exit code | **1** (because of proven normalize defect) |

Reproduce:

```bash
bash scripts/run_phase2_audit.sh
```

---

## Proven defect (do not “fix” without approval)

### NORM-DUP-PERIOD-CORRUPT — `reorder_financial_columns` corrupts duplicate periods

| Field | Detail |
|---|---|
| 風險等級 | **P1** |
| 控制點 | Period integrity / malformed payload |
| 公式／規格 | After coerce, duplicate FY columns must keep distinct values; reorder must not overwrite |
| 獨立基準 | Fixture `Breakdown \| 12/31/2024=100 \| 12/31/2024=999 \| 12/31/2023=90` |
| 實際結果 | **FAIL** — after `reorder_financial_columns`, values become `100\|100\|90` (999 lost); colnames become `12/31/2024\|12/31/2024.1\|…` |
| 錯誤案例 | Malformed / duplicate period columns in cached payload |
| 修正建議 | **Approve separately:** `make.unique` before subset **or** reject duplicates as unavailable. Not applied this phase. |
| 部署影響 | Yahoo annual path rarely emits duplicates; risk if a malformed payload is cached. **Not a DCF/RI/DDM formula bug.** |

---

## Findings matrix (P0/P1 controls)

### A — Lineage (raw → normalize → metrics → FV)

| ID | 風險 | 結果 | 獨立基準 → 觀察 | 部署影響 |
|---|---|---|---|---|
| LINEAGE-DCF | P0 | PASS | Mock FY FCF → `value_dcf_per_share` = `oracle_lineage_dcf_ps` | Offline chain only |
| LINEAGE-PS-ADR | P0 | PASS | TWD equity / ADR shares / FX32 → `per_share_in_quote` | ADR FV display |
| LINEAGE-PS-REFUSE | P0 | PASS | Unaligned ADR → NA (not plausible USD) | Guard |

Each lineage node records source / period / ccy / unit in finding notes where applicable.

### B — Period / TTM / annual

| ID | 風險 | 結果 | Notes |
|---|---|---|---|
| NORM-PERIOD-ORDER | P0 | PASS | Breakdown \| TTM \| newest→oldest |
| NORM-FLOW-TTM | P0 | PASS | Flow current = TTM 105M |
| NORM-STOCK-NO-TTM | P0 | PASS | Stock ignores TTM poison shares=999 |
| NORM-FCF-TTM-EXCLUDE | P0 | PASS | `include_ttm=FALSE` → FY 25M |
| NORM-MISSING-NA | P0 | PASS | Missing metric → NA (not 0) |
| NORM-DUP-PERIOD-CORRUPT | P1 | **FAIL** | See proven defect |
| NORM-EMPTY-DETECT | P0 | PASS | Empty three tables detected |
| NORM-CCY-NO-COPY | P0 | PASS | Missing financialCurrency not copied from quote |

### C — Currency / shares / bridge

| ID | 風險 | 結果 | Notes |
|---|---|---|---|
| SHARES-ADR-AUTO | P0 | PASS | mcap/price when ratio > ADR thr |
| SHARES-ADR-REFUSE | P0 | PASS | Cross-ccy without adjust → NA shares |
| FX-UNSUPPORTED | P0 | PASS | CNY→USD → NA |
| BRIDGE-ZERO-DEFAULT | P1 | PASS (documented risk) | Missing cash/debt → 0 intentionally |

### D — Yahoo / TPEx error simulation (mocks)

| ID | 風險 | 結果 | Coverage |
|---|---|---|---|
| YAHOO-EMPTY | P1 | PASS | offline_mock |
| YAHOO-MALFORMED | P1 | PASS | offline_mock |
| YAHOO-THROW | P1 | PASS | offline_mock |
| TPEX-NO-INVENT-CF | P1 | PASS | structural (+ existing `test_tpex_financial_summary.R`) |

### E — Cache

| ID | 風險 | 結果 | Notes |
|---|---|---|---|
| CACHE-TTL-EXPIRE | P1 | PASS | Runtime memoise expiry (1s TTL) |
| CACHE-TIER-ISOLATION | P1 | PASS | Fast/slow memos isolated |
| CACHE-EMPTY-DROP | P1 | PASS | drop_cache recovers non-empty |
| CACHE-WIRING | P1 | PASS | structural: slow financials / fast summary |

### F — Fail-soft vs plausible FV

| ID | 風險 | 結果 | Notes |
|---|---|---|---|
| FAILSOFT-DCF-SHARES | P0 | PASS | shares NA → ok=FALSE |
| FAILSOFT-PE-NEG | P0 | PASS | neg EPS → unavailable |
| FAILSOFT-RF-FALLBACK | P1 | PASS (structural) | Fallback Rf path exists; UI surfacing → P1-D |

---

## Coverage matrix (source → transform → tests)

| Source / transform | Offline mock | Structural | Live HTTP | Primary tests |
|---|---|---|---|---|
| Yahoo statement payload coerce/normalize | Yes | — | **No** | Phase 2 + `test_data_integrity.R` |
| TTM vs FY flow/stock/FCF | Yes | — | **No** | Phase 2 |
| Duplicate period columns | Yes (**FAIL**) | — | **No** | Phase 2 |
| Empty Yahoo → drop cache | Yes (contract) | Yes (source) | **No** | Phase 2 + `test_financials_empty_fallback.R` |
| Yahoo timeout/429/schema live | — | — | **Uncovered** | LIVE-UNCOVERED-1 |
| TPEx summary merge / no invent CF | Partial | Yes | **Uncovered** | Phase 2 + `test_tpex_financial_summary.R` |
| ADR shares + FX lineage | Yes | — | **No** | Phase 2 + `test_per_share_fx_adr.R` |
| Fast 15m / slow 24h wiring | Runtime short TTL | Yes | Shared-worker races **uncovered** | Phase 2 + `test_cache_tiers.R` |
| Quote fresh vs statement stale timing | — | Tier split | **Uncovered** | LIVE-UNCOVERED-4 |
| Lineage → displayed FV in Shiny UI | Offline DCF only | — | UI E2E → **P1-D** | Phase 2 lineage + future browser |

---

## Residual risks (unresolved)

1. **NORM-DUP-PERIOD-CORRUPT** — proven; fix pending explicit approval.  
2. **BRIDGE-ZERO-DEFAULT** — intentional zeros can look like a complete EV bridge.  
3. **Rf fallback** — numeric rate may still feed CAPM without strong UI warning (verify in P1-D).  
4. **All LIVE-UNCOVERED-*** rows — require staging/live probes.  
5. Quarterly Yahoo columns — live scraper is annual-only; TPEx YTD vs annual mix is only partially covered via existing TPEx tests.

---

## Deliverables

| Artifact | Path |
|---|---|
| Report | `docs/PHASE2_Data_Integration_Audit.md` |
| Findings JSON | `docs/phase2_findings.json` |
| Oracles | `app_23.0/tests/audit_data_oracles.R` |
| Suite | `app_23.0/tests/test_phase2_data_integration.R` |
| Runner | `scripts/run_phase2_audit.sh` |
| Agent log | `/opt/cursor/artifacts/phase2_audit_run.log` |

---

## Next

**P1-D browser E2E:** parameter change → recalculation → chart/download consistency for DCF, RI, DDM, MULT — after deciding whether to approve a fix for `NORM-DUP-PERIOD-CORRUPT`.
