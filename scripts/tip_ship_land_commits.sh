#!/usr/bin/env bash
# Land tip-ship bump + DEPLOY_BASELINE commits without a direct push to a
# protected base branch (Tip-ship #3 GH013: offline-checks required on master).
#
# Creates tip-ship/baseline-<version>-<shortsha>, opens a PR into the ship
# branch, dispatches Live-line CI (GITHUB_TOKEN pushes do not fire pull_request
# workflows), and enables auto-merge.
#
# Usage (from repo root, after bump/baseline commits exist on HEAD):
#   bash scripts/tip_ship_land_commits.sh <base-branch>
#
# Env:
#   GH_TOKEN / GITHUB_TOKEN — required
#   GITHUB_REPOSITORY — owner/repo (set by Actions)
# Outputs (GITHUB_OUTPUT when set):
#   sha, short, pr_url, land_branch, mode=pr|noop

set -euo pipefail

BASE_BRANCH="${1:-}"
if [[ -z "${BASE_BRANCH}" ]]; then
  echo "Usage: $0 <base-branch>" >&2
  exit 2
fi

if [[ -z "${GH_TOKEN:-${GITHUB_TOKEN:-}}" ]]; then
  echo "::error::GH_TOKEN/GITHUB_TOKEN required to land tip-ship commits." >&2
  exit 1
fi
export GH_TOKEN="${GH_TOKEN:-$GITHUB_TOKEN}"

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

HEAD_SHA="$(git rev-parse HEAD)"
HEAD_SHORT="$(git rev-parse --short HEAD)"
ORIGIN_BASE="$(git rev-parse "origin/${BASE_BRANCH}" 2>/dev/null || echo none)"

emit() {
  local key="$1" val="$2"
  if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
    echo "${key}=${val}" >>"$GITHUB_OUTPUT"
  fi
}

if [[ "$HEAD_SHA" == "$ORIGIN_BASE" ]]; then
  echo "No new tip-ship commits to land (HEAD already matches origin/${BASE_BRANCH})."
  emit sha "$HEAD_SHA"
  emit short "$HEAD_SHORT"
  emit mode "noop"
  emit pr_url ""
  emit land_branch ""
  exit 0
fi

VERSION="$(sed -n 's/^display_version=//p' scripts/DEPLOY_BASELINE.txt 2>/dev/null | head -1 | tr -d '[:space:]')"
BUNDLE="$(sed -n 's/^bundleId=//p' scripts/DEPLOY_BASELINE.txt 2>/dev/null | head -1 | tr -d '[:space:]')"
VERSION_SAFE="${VERSION:-unknown}"
VERSION_SAFE="${VERSION_SAFE//\//-}"
LAND_BRANCH="tip-ship/baseline-${VERSION_SAFE}-${HEAD_SHORT}"

echo "Landing tip-ship commits via PR: ${LAND_BRANCH} → ${BASE_BRANCH}"
echo "  HEAD=${HEAD_SHORT} display_version=${VERSION:-?} bundleId=${BUNDLE:-?}"

# Detach tip commits onto a land branch (do not push the protected base).
git branch -f "$LAND_BRANCH" HEAD
git push -u origin "HEAD:refs/heads/${LAND_BRANCH}"

# Open PR if none exists for this head.
EXISTING_PR="$(gh pr list --head "$LAND_BRANCH" --base "$BASE_BRANCH" --state open --json url --jq '.[0].url // empty')"
if [[ -n "$EXISTING_PR" ]]; then
  PR_URL="$EXISTING_PR"
  echo "Reusing open tip-ship PR: ${PR_URL}"
else
  TITLE="chore: tip-ship baseline ${VERSION:-?} bundle ${BUNDLE:-?}"
  BODY="$(cat <<EOF
## Tip-ship deployment record

Lands the display-version bump + \`scripts/DEPLOY_BASELINE.txt\` after a successful shinyapps tip-ship.

| Field | Value |
|---|---|
| display_version | \`${VERSION:-?}\` |
| bundleId | \`${BUNDLE:-?}\` |
| ship commit | \`${HEAD_SHA}\` |

Opened by Actions tip-ship so protected \`${BASE_BRANCH}\` does not reject a direct push (GH013 / required \`offline-checks\`). Auto-merge enabled after required checks.

Do **not** tip-ship paused work (e.g. #801) from this record PR.
EOF
)"
  PR_URL="$(
    gh pr create \
      --base "$BASE_BRANCH" \
      --head "$LAND_BRANCH" \
      --title "$TITLE" \
      --body "$BODY"
  )"
  echo "Opened tip-ship PR: ${PR_URL}"
fi

# GITHUB_TOKEN branch pushes do not trigger pull_request workflows — dispatch CI
# so required offline-checks appear on this SHA for auto-merge / branch protection.
if gh workflow list --json name,path --jq '.[] | select(.path==".github/workflows/ci.yml") | .path' | grep -q 'ci.yml'; then
  echo "Dispatching Live-line CI on ${LAND_BRANCH}…"
  gh workflow run ci.yml --ref "$LAND_BRANCH" || {
    echo "::warning::Could not dispatch ci.yml — open the PR and re-run checks manually if auto-merge waits."
  }
else
  echo "::warning::ci.yml not found via gh workflow list; skip dispatch."
fi

# Enable auto-merge (deployment-record PRs are pre-authorized). Prefer squash
# for small chore records; fall back to merge if squash is disabled.
if gh pr merge "$PR_URL" --auto --squash; then
  echo "Auto-merge (squash) enabled for ${PR_URL}"
elif gh pr merge "$PR_URL" --auto --merge; then
  echo "Auto-merge (merge) enabled for ${PR_URL}"
else
  echo "::warning::Could not enable auto-merge for ${PR_URL} — merge manually after offline-checks."
fi

emit sha "$HEAD_SHA"
emit short "$HEAD_SHORT"
emit mode "pr"
emit pr_url "$PR_URL"
emit land_branch "$LAND_BRANCH"
echo "Tip-ship land complete (mode=pr). Master updates after required checks + auto-merge."
