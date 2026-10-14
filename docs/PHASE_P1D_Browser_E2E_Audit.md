# P1-D — Browser E2E Audit (DCF / RI / DDM / MULT)

**Status:** Executed against local Cloud Agent Shiny (`http://127.0.0.1:3838/`)  
**App version shown:** v23.28  
**Ticker exercised:** AAPL  
**Captured (UTC):** 2026-10-14  
**Branch:** includes `NORM-DUP-PERIOD-CORRUPT` fix (`reorder_financial_columns` index reorder)

**Honesty:** This is a **manual browser** pass on the local server, not shinyapps.io production and not an automated shinytest suite.

---

## Baseline

| Item | Value |
|---|---|
| Deploy baseline at start of Phase 2 | v23.28 / bundle `12689606` / `946a42d` |
| Code under test | Phase 2 branch + normalize fix |
| Live HTTP data | Yahoo scrape succeeded for AAPL in-session |

---

## Flows

| Flow | Result | Evidence | Notes |
|---|---|---|---|
| Home load | PASS | `p1d_e2e/01_home.png` | Branding / sidebar present |
| Search AAPL | PASS | `02_after_search.png` | Summary + statements loaded (~5s) |
| DCF tab | PARTIAL | `03_dcf.png` | Chart shows hist/forecast FCFF; **per-share / enterprise boxes N/A** with status「Waiting for valuation」— calc path not fully settled on tab (or gated). Composite later shows DCF Base $201.79 |
| DDM | PASS (via Smart Analysis) | `04_ddm.png` | Surface reachable; no silent fake price observed when unused |
| RI | PASS | `05_ri.png` / composite | Secondary RI FV **$525.46** shown with divergence note |
| MULT (P/E) | PASS | `06_mult.png` | **P/E Implied $136.47** on fair-value chart |
| PDF download | PASS | `YNow_Report_US_Lite_AAPL_2026-10-14.pdf` | ~209 KB written |
| Invalid param gate | PASS | Session notes | g ≫ WACC surfaced as blocked / guided (not Inf FV) |

Composite Smart Analysis (EN): Primary DCF Base **201.79** (Bear 146.46 / Bull 308.79), Secondary RI **525.46**, Current **344.83**, MOS vs primary **−70.9%**, Multiples P/E **136.47**.

---

## Fail-soft / no silent fake FV

| Check | Result |
|---|---|
| Missing / waiting state shows N/A (DCF tab boxes) | Observed — not a fabricated price |
| Invalid growth vs WACC | Blocked with messaging (session) |
| Model divergence disclosed | RI vs DCF called out in reliability text |

---

## Artifacts

| Path | Role |
|---|---|
| `/opt/cursor/artifacts/p1d_e2e/01_home.png` … `06_mult.png` | Screenshots |
| `/opt/cursor/artifacts/p1d_e2e/test_report.md` | Computer-use session notes |
| `/opt/cursor/artifacts/p1d-browser-e2e-demo.mp4` | Screen recording |
| `/opt/cursor/artifacts/p1d_e2e/YNow_Report_US_Lite_AAPL_*.pdf` | Downloaded report |

---

## Findings for follow-up (not blockers for normalize fix)

| ID | Risk | Issue | Remediation suggestion |
|---|---|---|---|
| P1D-DCF-TAB-NA | P2 | DCF calculator tab can show N/A /「Waiting for valuation」while Smart Analysis already has a DCF Base | **Closed in closeout:** tab-visit observer fires `.fire_auto_calc_primary("dcf")` when ready (`ynow_server.R`). See `docs/PHASE_AUDIT_CLOSEOUT.md`. |
| P1D-NO-SHINYTEST | P2 | No chromote/shinytest harness in repo | **Closed under acceptance** — `app_24.0/tests/test_audit_chromote_e2e.R` (headless CDP) |

---

## Relation to Phase 2 fix

`NORM-DUP-PERIOD-CORRUPT` is a **normalize-layer** defect (duplicate FY columns). P1-D did not re-inject malformed duplicate columns through the UI; the fix is covered by `scripts/run_phase2_audit.sh` (**PASS=38 FAIL=0**). Browser E2E confirms end-user valuation surfaces still load and compute on live Yahoo data after the change.
