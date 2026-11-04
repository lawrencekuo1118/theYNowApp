---
name: verify-formal-wacc-apply
description: "Bounded property verification of two-class WACC apply: other-claims gate must not clear Gordon/stage WACC fields, and a successful calculate must write the same percent to wacc_gordon + wacc_stage1 + wacc_stage2. Use when editing CAPM→WACC auto-apply, bridge_other_claims / statement_other_equity_claims, two-stage DCF WACC inputs, or .auto_recalc_capm_wacc."
---

# Verify formal: WACC apply (other-claims gate + Gordon/stage write)

## Property (plain words)

**If other capital claims are not a verified 0, leave existing Gordon and stage WACC inputs alone; if claims are 0 and a positive WACC was calculated, write that same percent into `wacc_gordon`, `wacc_stage1`, and `wacc_stage2` together.**

**Harm if broken:** clearing those fields on a failed other-claims gate (or writing only Gordon while stages stay empty) leaves two-stage DCF / sensitivity on blank or stale rates → wrong or empty Fair Value. That is the #636 failure mode.

## Tool and why

**No TLA+ / SMT / proof assistant fits this R Shiny surface well** (apply lives in a server closure; rates are numeric UI fields). The check is **bounded property testing of the real production R function** `ynow_apply_calculated_wacc_fields` in `app_27.0/setup.R`, plus source wiring asserts that `.auto_recalc_capm_wacc` in `app_27.0/ynow_server.R` calls it and writes all three IDs.

- Runner: stock `Rscript` (already required for `app_27.0/tests/`).
- No extra CRAN package (`hedgehog` / QuickCheck not required).
- Generated inputs: fixed seed `636`, default **1000** trials (`YNOW_FORMAL_WACC_N` to raise).

## When to run

Run this skill before finishing any change that touches:

- `ynow_apply_calculated_wacc_fields` or `statement_other_equity_claims*` in `setup.R`
- `.auto_recalc_capm_wacc` / WACC `updateNumericInput` for `wacc_gordon` / `wacc_stage1` / `wacc_stage2` in `ynow_server.R`
- Completeness / auto-apply of stage WACC or `bridge_other_claims`
- Two-stage DCF discount rate plumbing that assumes stage fields were filled by CAPM→WACC

If the covered files changed after **Coverage record → sync commit** below, re-run and move the sync commit forward only after the check still describes the code.

## How to run

```bash
cd app_27.0/tests
YNOW_DEBUG_SKIP_PY=1 Rscript test_formal_wacc_apply.R
# optional denser search:
YNOW_DEBUG_SKIP_PY=1 YNOW_FORMAL_WACC_N=5000 Rscript test_formal_wacc_apply.R
# related contract (other-claims fixtures + server wiring):
YNOW_DEBUG_SKIP_PY=1 Rscript test_two_stage_wacc_other_equity.R
```

Exit **0** = property held inside the stated bounds. Exit **≠0** = real violation or wiring drift — treat as a bug until proven modeling error.

## Bounds and assumptions (honest scope)

| Bound | Scope |
|-------|--------|
| Function under test | `ynow_apply_calculated_wacc_fields(wacc_percent, other_claims, current)` |
| `wacc_percent` | Percent units; pool ∪ continuous draw in **(0, 50]**; also NA / ≤0 / Inf |
| `other_claims` | `{0}` vs nonzero finite ∪ NA / ±Inf (gate is `is.finite(oc) && oc == 0`) |
| Seed fields | Finite percents ∪ NA for the three IDs |
| Trials | Default 1000, seed **636** (reproducible) |
| Server | Asserts call site + three writes; does **not** simulate full Shiny session, shares, price, or CAPM arithmetic |
| Not covered | Full WACC weight math; two-stage EV discount monotonicity; CapEx spike; cache tiers; UI locale |

A pass means: within these bounds the apply contract held on the real helper and the server still routes writes through it — **not** that every DCF number in the app is correct.

## Pass vs modeling error vs real bug

| Observation | Meaning |
|-------------|---------|
| Spot checks + property trials all OK; wiring greps OK | Contract holds at sync commit |
| `cleared_or_mutated_on_fail` counterexample | **Real bug** (pre-#636 shape): gate fail mutated seed fields |
| `incomplete_write` counterexample | **Real bug**: claims=0 and valid WACC but Gordon/stages not all equal to `w` |
| Wiring grep fails but helper properties pass | Server drifted off the helper — **real integration bug** |
| You intentionally changed the product rule | Update helper + this skill + tests together; do not weaken the property to greenwash |

## Prove the detector (scratch break)

Already confirmed when this skill was created: temporarily making the helper `return(list(wacc_gordon=NA,…))` on gate fail caused:

- `FAIL gate NA does not clear seeded fields`
- `FAIL property holds for N generated inputs`
- Counterexample kind `cleared_or_mutated_on_fail` (e.g. `oc=Inf`, seed cleared to NA)

Then the helper was restored and the check returned to all OK. If a future change “passes” after a similar scratch clear-on-fail, the skill/tests are broken — fix the skill, not the product.

## Coverage record

| Item | Path / symbol |
|------|----------------|
| Apply helper | `app_27.0/setup.R` → `ynow_apply_calculated_wacc_fields` |
| Other-claims gate inputs | `app_27.0/setup.R` → `statement_other_equity_claims`, `statement_other_equity_claims_book` |
| Server apply site | `app_27.0/ynow_server.R` → `.auto_recalc_capm_wacc` (calls helper; writes three IDs) |
| Property runner | `app_27.0/tests/test_formal_wacc_apply.R` |
| Contract / fixtures | `app_27.0/tests/test_two_stage_wacc_other_equity.R` |
| **Sync commit** | `SYNC_COMMIT_PLACEHOLDER` — last commit at which this verification was confirmed to describe the code above |

After an intentional change to covered code: re-run the commands in **How to run**, then replace **Sync commit** with the new confirming commit SHA (full or short). Do not advance the sync commit on a failing run.

## Maintenance

Use `/maintain-formal-verification-skill` when covered files move and this check was not re-run, or when a counterexample needs triage. Closest alternate target if this area is already green after a large refactor: CapEx spike gates via `fcf_sync_capex_revenue_margin` / `ratio_spike_vs_prior` (already has spot tests in `test_fcf_capex_sync.R`; less apply-order harm than WACC).
