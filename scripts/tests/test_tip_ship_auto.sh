#!/usr/bin/env bash
# Guard: tip-ship.yml must auto-run after green Live-line CI on master,
# keep manual workflow_dispatch, and stay out of ci.yml (no inline shinyapps).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TIP="${ROOT}/.github/workflows/tip-ship.yml"
CI_YML="${ROOT}/.github/workflows/ci.yml"

fail() { printf 'test_tip_ship_auto: FAIL: %s\n' "$*" >&2; exit 1; }
ok() { printf 'test_tip_ship_auto: OK: %s\n' "$*"; }

[[ -f "$TIP" ]] || fail "missing $TIP"
[[ -f "$CI_YML" ]] || fail "missing $CI_YML"

grep -Fq 'workflow_dispatch:' "$TIP" || fail "workflow_dispatch missing (manual Run still required as fallback)"
ok "workflow_dispatch present"

grep -Fq 'workflow_run:' "$TIP" || fail "workflow_run missing (auto tip-ship after Live-line CI)"
grep -Fq 'Live-line CI' "$TIP" || fail "workflow_run must watch Live-line CI"
ok "workflow_run watches Live-line CI"

grep -Fq "github-actions[bot]" "$TIP" || fail "must skip github-actions[bot] to avoid ship loop"
ok "skips github-actions[bot]"

grep -Fq 'auto-gate:' "$TIP" || fail "auto-gate job missing"
grep -Fq 'app_28.0/*' "$TIP" || fail "ship-path filter must include app_28.0/*"
grep -Fq 'scripts/*' "$TIP" || fail "ship-path filter must include scripts/*"
ok "auto-gate + ship-path filter present"

# Live-line CI must remain deploy-free (reusable call would also need this relaxed).
if grep -Ei 'deployApp|deploy_app_21|shinyapps' "$CI_YML" >/dev/null 2>&1; then
  fail "ci.yml must not deploy to shinyapps (auto tip-ship lives in tip-ship.yml)"
fi
ok "ci.yml stays deploy-free"

# No PAT land path revived.
if grep -Eq 'TIP_SHIP_GH_PAT' "$TIP"; then
  # Mentions in comments saying "No TIP_SHIP_GH_PAT" are OK; secret mapping is not.
  if grep -E 'secrets\.TIP_SHIP_GH_PAT|TIP_SHIP_GH_PAT:' "$TIP" >/dev/null 2>&1; then
    fail "TIP_SHIP_GH_PAT secret must not be wired"
  fi
fi
ok "no TIP_SHIP_GH_PAT secret wiring"

# Land = GITHUB_TOKEN PR (not direct master push / not ruleset bypass).
grep -Fq 'tip_ship_land_commits.sh' "$TIP" || fail "tip-ship.yml must land via tip_ship_land_commits.sh"
if grep -Fq 'git push origin "HEAD:' "$TIP"; then
  fail "tip-ship.yml must not direct-push HEAD:branch (use land PR)"
fi
ok "land via tip_ship_land_commits.sh"

exit 0
