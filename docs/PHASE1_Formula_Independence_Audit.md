# Phase 1 — Formula Independence & MULT Coverage Audit

**Status:** P1-A + P1-B complete (no valuation formula changes)  
**Scope:** Independent hand/closed-form oracles vs live-line engines under `app_23.0/`  
**Captured (UTC):** 2026-10-14 (Cloud Agent re-run)  
**Runner:** `bash scripts/run_phase1_audit.sh`  
**Suite:** `app_23.0/tests/test_phase1_formula_independence.R`  
**Oracles:** `app_23.0/tests/audit_independent_oracles.R` (**must not** call production formula helpers)  
**Machine findings:** `docs/phase1_findings.json`

### Deploy context at capture

| Item | Value |
|---|---|
| Live line | `app_23.0/` |
| `display_version` | **v23.28** (`scripts/DEPLOY_BASELINE.txt`) |
| `bundleId` | `12689606` |
| Aligned commit | `946a42d` |
| Workspace tip | current Phase 1 branch (test/docs only; valuation sources unchanged) |

**Principle applied:** Prove correctness (or find a reproducible defect) before editing engines. No DCF/RI/DDM/MULT formula code was modified in this phase.

**Relation to Phase 0:** Phase 0 showed the offline RCM suites are **reproducible** (257/0). Phase 1 asks whether tests can **catch real formula errors** by comparing engines to an independent oracle layer.

---

## Suite result (this run)

| Metric | Value |
|---|---|
| PASS | **43** |
| FAIL | **0** |
| Exit code | **0** |
| Structured findings rows | **21** (all `result=PASS`) |

Reproduce:

```bash
bash scripts/run_phase1_audit.sh
# or:
cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_phase1_formula_independence.R
```

---

## Independence design

| Layer | Role |
|---|---|
| `audit_independent_oracles.R` | Pure algebra: PV paths, Gordon TV, EV bridge, DDM Gordon/SPM/two-stage, RI BV roll, MULT identities |
| Production engines | `value_dcf_per_share`, `.dcf_formula_*`, `fcff_to_fcfe`, `.ddm_formula_*`, `compute_ri_valuation`, `build_roe_path`, `calc_*` in `relative_multiples_module.R` |
| Comparison | Intermediate steps (EV, equity, Beg_BV, RI_t, TV, segment EV) + per-share / Implied Price |

Oracles are loaded **before** `setup.R` / modules and never call `.dcf_formula_*`, `.ddm_formula_*`, `compute_ri_valuation`, or `calc_*`.

---

## Findings matrix (required columns)

Risk levels: **P0** = can materially distort fair / Implied Price; **P1** = mode-specific or intermediate path risk; **P2** = indicator / non-price.

### P1-A — DCF

| ID | 風險等級 | 公式規格 | 獨立基準 | 實際結果 | 錯誤案例 | 修正建議 | 部署影響 |
|---|---|---|---|---|---|---|---|
| DCF-FCFF-PS | P0 | EV=Σ CF_t/(1+WACC)^t + Gordon TV; Equity=EV+cash−debt; PS=Equity/shares; WACC>g, CF_n>0, shares>0 | `oracle_dcf_path` | PASS; engine PS − oracle = 0 (tol 1e-6) | WACC≤g; shares≤0; CF_n\<0 → `ok=FALSE` | none | Would distort live FCFF FV if failed; **no defect found** on v23.28 engine |
| DCF-FCFE-BRIDGE | P0 | FCFE at Ke; **no** +cash−debt | `oracle_dcf_path(bridge='none')` | PASS; cash=999 ignored as required | Wrong bridge would pull PS toward (EV+999−50)/shares | none | FCFE mode; **no defect** |
| DCF-FCFF-FCFE-CONV | P1 | FCFE_t=FCFF_t−IAT+g_t×BegDebt; debt rolls | `oracle_fcff_to_fcfe` | PASS y1/y2 | Bad debt roll → y2 drift | none | FCFE claim conversion |
| DCF-TWO-RATE-GATE | P1 | Two-stage needs `yr_stage1`+`yr_stage2`, y1+y2=n; remainder **not** inferred | Expect NA if y2 omitted | PASS → NA | Silent remainder inference would mis-rate years | none — **intentional guard** | Live two-stage DCF fail-soft |
| DCF-TWO-RATE | P1 | Years 1..y1 @ r1, rest @ r2; TV uses r2−g | `oracle_dcf_two_rate` | PASS EV match (1146.924…) | Mis-staged rates | none | Two-stage WACC |
| DCF-GATE-WACC-G | P0 | Discount > terminal g | Literal `ok=FALSE` | PASS | Ungated → Inf/neg TV | none | Guard active |

### P1-A — DDM

| ID | 風險等級 | 公式規格 | 獨立基準 | 實際結果 | 錯誤案例 | 修正建議 | 部署影響 |
|---|---|---|---|---|---|---|---|
| DDM-GORDON | P0 | V0=D1/(Ke−g), D1=D0(1+g) | `oracle_ddm_gordon` | PASS helper = oracle; UI uses same algebra then `round(,2)` | Ke≤g; D0≤0 | none (2dp is **display**, not formula) | DDM Gordon tab |
| DDM-SPM | P1 | V0=E·g/Ke²+D/Ke; Ke>0; **no** Ke>g required | `oracle_ddm_spm` | PASS; also finite when g>Ke | Ke≤0 → NA | none | SPM mode |
| DDM-TWO-STAGE | P0 | High-g then Gordon; explicit D_t & P_n | `oracle_ddm_two_stage` | PASS (n=3) | Ke≤g2 → NA | none | two_stage mode |

### P1-A — Residual Income

| ID | 風險等級 | 公式規格 | 獨立基準 | 實際結果 | 錯誤案例 | 修正建議 | 部署影響 |
|---|---|---|---|---|---|---|---|
| RI-INTRINSIC | P0 | RI_t=(ROE−Ke)·BV; BV roll with payout; TV on RI_n | `oracle_ri` year-by-year | PASS intrinsic + Beg_BV/RI/PV_TV | g≥Ke → error | none | RI tab |
| RI-ROE-LINEAR | P1 | `seq(start, terminal, length.out=n)` | `oracle_roe_linear` | PASS path + revalued intrinsic | Wrong fade scales RI/TV | none | linear / industry fade UI |

### P1-B — MULT / relative valuation

| ID | 風險等級 | 公式規格 | 獨立基準 | 實際結果 | 錯誤案例 | 修正建議 | 部署影響 |
|---|---|---|---|---|---|---|---|
| MULT-PE | P0 | Price=EPS×P/E | `oracle_pe_price` | PASS | EPS≤0 / PE≤0 → unavailable | none | equity family |
| MULT-FWD-PE | P0 | Fwd EPS=Trailing×(1+g/100); × Fwd P/E; **no** price÷P/E | `oracle_forward_pe_price` | PASS end-to-end | missing g → NA | none | Forward P/E sync |
| MULT-PEG | P2 | PEG=PE/g%; **no** Implied Price | `oracle_peg` | PASS | g≤0 | none | indicator only |
| MULT-EV-FCF | P0 | EV=FCF×m; Eq=EV+cash−debt; /shares | `oracle_ev_multiple_price` | PASS (+ EV intermediate) | metric≤0; shares missing | none | enterprise family |
| MULT-EV-EBIT | P0 | same EV bridge on EBIT | same | PASS | metric≤0 | none | enterprise |
| MULT-EV-EBITDA | P0 | same on EBITDA | same | PASS | metric≤0 | none | enterprise |
| MULT-EV-SALES | P0 | same on Revenue | same | PASS | metric≤0 | none | enterprise |
| MULT-EV-ARR | P0 | same on ARR; missing ≠ 0 | same | PASS | ARR NA → unavailable | none | enterprise |
| MULT-PS | P0 | Equity=Rev×P/S; **no** cash−debt (≠ EV/Sales) | `oracle_ps_price` vs EV/Sales contrast | PASS; P/S=150 vs EV/Sales=147 with cash=20,debt=50 | Wrong bridge would equate P/S to EV/Sales | none | equity vs enterprise distinction |
| MULT-SOTP | P1 | Σ(seg×EV/Sales)+non_op → bridge; ≥2 segs | `oracle_sotp_revenue` | PASS; single-seg unavailable | \<2 segments | none | SOTP revenue |

Thin helpers `multiple_implied_price` / `enterprise_implied_price` also matched the same oracles (shares-aware signatures).

---

## Proven defects

**None** in DCF / RI / DDM / MULT pure calculation helpers under the cases above.

Documented **non-defects** (do not “fix”):

1. DDM Gordon UI `round(p0, 2)` — presentation only.  
2. Two-stage DCF NA without `yr_stage2` — intentional `validate_stage_years` gate.  
3. PEG has no Implied Price — by design.  
4. P/S omits net-debt bridge — equity-side method (distinct from EV/Sales).

---

## Residual gaps (not closed by P1-A/B)

| Gap | Why still open |
|---|---|
| P1-C Live Yahoo / TPEx / cache E2E | Offline mocks only |
| P1-D Browser E2E | No shinytest/chromote harness |
| Dollar-scale FCFF projection margins | Unit/path engines checked; full `fcf_projection_module` driver stack not re-derived here |
| Shares / FX / ADR into MULT sync | Covered elsewhere (`test_resolve_shares.R`); not re-audited in this suite |
| Scenario stress metadata vs recomputed FV | Still stronger in Phase 4 RCM than here |

---

## Deliverables

| Artifact | Path |
|---|---|
| Independent oracles | `app_23.0/tests/audit_independent_oracles.R` |
| Phase 1 suite | `app_23.0/tests/test_phase1_formula_independence.R` |
| Runner | `scripts/run_phase1_audit.sh` |
| Findings JSON | `docs/phase1_findings.json` |
| This report | `docs/PHASE1_Formula_Independence_Audit.md` |
| Console log (agent) | `/opt/cursor/artifacts/phase1_audit/phase1_audit_run.log` |

---

## Next recommended step

Proceed to **P1-C** (fixed fixtures + controllable mocks for Yahoo/TPEx/cache) then **P1-D** (browser E2E for DCF/RI/DDM/MULT). Keep engines frozen unless a reproducible numerical defect appears.
