#!/usr/bin/env bash
# Guard: Live-line CI must keep meaningful path filters and core offline jobs.
# Fails closed if app_27.0/** or scripts/** drop out of on.push / on.pull_request,
# or if valuation / Phase 1 steps disappear (vacuous green risk).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CI_YML="${ROOT}/.github/workflows/ci.yml"

fail() { printf 'test_ci_live_line_paths: FAIL: %s\n' "$*" >&2; exit 1; }
ok() { printf 'test_ci_live_line_paths: OK: %s\n' "$*"; }

[[ -f "$CI_YML" ]] || fail "missing $CI_YML"

# Required path globs on both push and pull_request (avoid vacuous green).
for event in push pull_request; do
  block="$(awk -v ev="$event" '
    $0 ~ "^  " ev ":" {grab=1; next}
    grab && $0 ~ /^  [a-z]/ {exit}
    grab {print}
  ' "$CI_YML")"
  [[ -n "$block" ]] || fail "no '$event' block in ci.yml"
  echo "$block" | grep -Fq 'app_27.0/**' || fail "$event paths missing app_27.0/**"
  echo "$block" | grep -Fq 'scripts/**' || fail "$event paths missing scripts/**"
  echo "$block" | grep -Fq '.github/workflows/**' || fail "$event paths missing .github/workflows/**"
  ok "$event paths include app_27.0/**, scripts/**, .github/workflows/**"
done

grep -Fq 'workflow_dispatch:' "$CI_YML" || fail "workflow_dispatch missing"
ok "workflow_dispatch present"

grep -Fq 'test_valuation_formulas.R' "$CI_YML" || fail "valuation formulas step missing"
grep -Fq 'run_phase1_audit.sh' "$CI_YML" || fail "Phase 1 step missing"
ok "valuation + Phase 1 steps present"

# Every Rscript / Phase 1 step block should set YNOW_DEBUG_SKIP_PY (offline contract).
# Count named offline steps that run R or phase1 and require the env key nearby.
if ! grep -n 'YNOW_DEBUG_SKIP_PY' "$CI_YML" | grep -q .; then
  fail "YNOW_DEBUG_SKIP_PY never set"
fi
skip_count="$(grep -c 'YNOW_DEBUG_SKIP_PY' "$CI_YML" || true)"
[[ "$skip_count" -ge 6 ]] || fail "expected YNOW_DEBUG_SKIP_PY on most offline steps (found $skip_count)"
ok "YNOW_DEBUG_SKIP_PY set ($skip_count occurrences)"

# No shinyapps deploy from this workflow.
if grep -Ei 'deployApp|deploy_app_21|shinyapps' "$CI_YML" >/dev/null 2>&1; then
  fail "ci.yml must not deploy to shinyapps"
fi
ok "no shinyapps deploy in Live-line CI"

# Job name used for branch protection required checks.
grep -Fq 'offline-checks:' "$CI_YML" || fail "job offline-checks missing"
ok "required check job name offline-checks present"

exit 0
