#!/usr/bin/env bash
# Guard: tip-ship land uses GITHUB_TOKEN PR + CI + auto-merge (no PAT / no bypass).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TIP="${ROOT}/.github/workflows/tip-ship.yml"
LAND="${ROOT}/scripts/tip_ship_land_commits.sh"
DOCS="${ROOT}/docs/cloud-tip-ship.md"

fail() { printf 'test_tip_ship_land: FAIL: %s\n' "$*" >&2; exit 1; }
ok() { printf 'test_tip_ship_land: OK: %s\n' "$*"; }

[[ -f "$TIP" ]] || fail "missing $TIP"
[[ -f "$LAND" ]] || fail "missing $LAND"
[[ -f "$DOCS" ]] || fail "missing $DOCS"
[[ -x "$LAND" ]] || chmod +x "$LAND"

grep -Fq 'tip_ship_land_commits.sh' "$TIP" || fail "tip-ship.yml must call tip_ship_land_commits.sh"
grep -Fq 'git push origin "HEAD:' "$TIP" && fail "tip-ship.yml must not direct-push HEAD:branch to protected base"
ok "tip-ship.yml uses land script (no direct master push)"

grep -Eq 'secrets\.TIP_SHIP_GH_PAT|TIP_SHIP_GH_PAT:' "$TIP" && fail "TIP_SHIP_GH_PAT must not be wired in tip-ship.yml"
grep -Eq 'TIP_SHIP_GH_PAT:-|secrets\.TIP_SHIP_GH_PAT' "$LAND" && fail "land script must not require TIP_SHIP_GH_PAT"
ok "no TIP_SHIP_GH_PAT"

grep -Fq 'gh pr create' "$LAND" || fail "land script must open a PR"
grep -Fq 'gh workflow run ci.yml' "$LAND" || fail "land script must dispatch Live-line CI"
grep -Fq 'gh pr merge' "$LAND" || fail "land script must enable/merge auto-merge"
grep -Fq 'MERGED' "$LAND" || fail "land script must wait until MERGED"
grep -Fq 'tip-ship/baseline-' "$LAND" || fail "land branch prefix missing"
grep -Fq 'superseded' "$LAND" || fail "land script must auto-close superseded land PRs"
grep -Fq 'offline-checks' "$LAND" || fail "land script must publish/wait offline-checks"
# Forbid an actual `git push -u` invocation (comments may still mention it).
if grep -E '^[[:space:]]*git push -u|[[:space:]]+git push -u' "$LAND" >/dev/null 2>&1; then
  fail "land script must not use git push -u (retargets master upstream)"
fi
grep -Fq 'git push origin "HEAD:refs/heads/' "$LAND" || fail "land script must push land branch without -u"
grep -Fq 'Resource not accessible by integration' "$LAND" || fail "land script must diagnose Actions create-PR setting"
ok "land script PR + CI + auto-merge + supersede"

grep -Fq 'pull-requests: write' "$TIP" || fail "tip-ship.yml needs pull-requests: write"
grep -Fq 'checks: write' "$TIP" || fail "tip-ship.yml needs checks: write"
grep -Fq 'statuses: write' "$TIP" || fail "tip-ship.yml needs statuses: write"
ok "workflow permissions for land"

grep -Fq 'Do **not** add `TIP_SHIP_GH_PAT`' "$DOCS" || fail "docs must forbid TIP_SHIP_GH_PAT"
grep -Fq 'ruleset bypass' "$DOCS" || fail "docs must say no ruleset bypass required"
ok "docs match land path"

exit 0
