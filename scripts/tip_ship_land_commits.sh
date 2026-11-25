#!/usr/bin/env bash
# Land tip-ship bump + DEPLOY_BASELINE commits without a direct push to a
# protected base branch (Tip-ship #3 GH013: offline-checks required on master).
#
# Causal chain this closes (Tip-ship Actions #3 / run 45631159295):
#   1) deploy_app_21.R uploaded + rolled forward live shinyapps OK
#   2) bump + record_deploy_baseline.R committed on the runner's local master
#   3) `git push origin HEAD:master` rejected GH013 — branch protection requires
#      status check `offline-checks` on master; a direct push never carries those
#      checks, so the push can never succeed
#   4) Actions default GITHUB_TOKEN pushes / PR creates do not fire pull_request
#      or push workflows — workflow_dispatch also does NOT satisfy PR required
#      checks — so land must push with a PAT that triggers real PR CI
#   5) Tip-ship #3 job failed red while live was already on the new bundle
#
# One land path (no direct master push):
#   tip-ship/baseline-* branch (PAT push) → PR → wait for offline-checks →
#   auto-merge → wait until MERGED. Fail the tip-ship job if land fails.
# Release must run only after this script exits 0 ("release after push/merge").
#
# Usage (from repo root, after bump/baseline commits exist on HEAD):
#   bash scripts/tip_ship_land_commits.sh <base-branch>
#
# Env:
#   TIP_SHIP_GH_PAT — required in Actions: classic/fine-grained PAT that can
#     push branches + open/merge PRs and that triggers workflow runs on push/PR
#     (default GITHUB_TOKEN cannot). Names only — never log the value.
#   GH_TOKEN / GITHUB_TOKEN — local fallback only when TIP_SHIP_GH_PAT unset
#     (still requires a token that fires PR checks; Actions tip-ship refuses
#     bare github.token).
#   GITHUB_REPOSITORY — owner/repo (set by Actions)
#   YNOW_TIP_SHIP_LAND_TIMEOUT_SEC — max wait for checks+merge (default 1500)
# Outputs (GITHUB_OUTPUT when set):
#   sha, short, pr_url, land_branch, mode=pr|noop, merged_sha

set -euo pipefail

BASE_BRANCH="${1:-}"
if [[ -z "${BASE_BRANCH}" ]]; then
  echo "Usage: $0 <base-branch>" >&2
  exit 2
fi

LAND_TOKEN="${TIP_SHIP_GH_PAT:-${GH_TOKEN:-${GITHUB_TOKEN:-}}}"
if [[ -z "$LAND_TOKEN" ]]; then
  echo "::error::TIP_SHIP_GH_PAT (or GH_TOKEN) required to land tip-ship commits." >&2
  exit 1
fi

# Actions default GITHUB_TOKEN cannot fire pull_request/push workflows, so
# offline-checks never appear on the land PR and auto-merge never completes.
if [[ -n "${GITHUB_ACTIONS:-}" && -z "${TIP_SHIP_GH_PAT:-}" ]]; then
  echo "::error::TIP_SHIP_GH_PAT secret is required for tip-ship land." >&2
  echo "::error::Default GITHUB_TOKEN pushes do not trigger pull_request CI;" >&2
  echo "::error::workflow_dispatch does not satisfy required PR checks (Bugbot #807)." >&2
  echo "::error::Add repo Actions secret TIP_SHIP_GH_PAT (repo + workflow scopes)." >&2
  exit 1
fi

export GH_TOKEN="$LAND_TOKEN"
export GITHUB_TOKEN="$LAND_TOKEN"

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# Ensure git push uses the land token (PAT), not a stale checkout token.
if [[ -n "${GITHUB_REPOSITORY:-}" ]]; then
  git remote set-url origin "https://x-access-token:${LAND_TOKEN}@github.com/${GITHUB_REPOSITORY}.git"
fi

HEAD_SHA="$(git rev-parse HEAD)"
HEAD_SHORT="$(git rev-parse --short HEAD)"
ORIGIN_BASE="$(git rev-parse "origin/${BASE_BRANCH}" 2>/dev/null || echo none)"
TIMEOUT_SEC="${YNOW_TIP_SHIP_LAND_TIMEOUT_SEC:-1500}"

emit() {
  local key="$1" val="$2"
  if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
    echo "${key}=${val}" >>"$GITHUB_OUTPUT"
  fi
}

retry() {
  local attempts="$1" sleep_sec="$2"
  shift 2
  if [[ "${1:-}" == "--" ]]; then shift; fi
  local i=1 rc=0
  while (( i <= attempts )); do
    if "$@"; then
      return 0
    fi
    rc=$?
    if (( i == attempts )); then
      return "$rc"
    fi
    echo "Retry ${i}/${attempts} failed (rc=${rc}); sleeping ${sleep_sec}s…" >&2
    sleep "$sleep_sec"
    i=$((i + 1))
  done
  return "$rc"
}

if [[ "$HEAD_SHA" == "$ORIGIN_BASE" ]]; then
  echo "No new tip-ship commits to land (HEAD already matches origin/${BASE_BRANCH})."
  emit sha "$HEAD_SHA"
  emit short "$HEAD_SHORT"
  emit mode "noop"
  emit pr_url ""
  emit land_branch ""
  emit merged_sha "$HEAD_SHA"
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
# PAT push must fire push + (after PR open) pull_request → offline-checks.
retry 4 3 -- git push -u origin "HEAD:refs/heads/${LAND_BRANCH}"

EXISTING_PR=""
PR_URL=""
for attempt in 1 2 3 4 5; do
  EXISTING_PR="$(gh pr list --head "$LAND_BRANCH" --base "$BASE_BRANCH" --state open --json url --jq '.[0].url // empty' 2>/dev/null || true)"
  if [[ -n "$EXISTING_PR" ]]; then
    PR_URL="$EXISTING_PR"
    echo "Reusing open tip-ship PR: ${PR_URL}"
    break
  fi
  TITLE="chore: tip-ship baseline ${VERSION:-?} bundle ${BUNDLE:-?}"
  BODY="$(cat <<EOF
## Tip-ship deployment record

Lands the display-version bump + \`scripts/DEPLOY_BASELINE.txt\` after a successful shinyapps tip-ship.

| Field | Value |
|---|---|
| display_version | \`${VERSION:-?}\` |
| bundleId | \`${BUNDLE:-?}\` |
| ship commit | \`${HEAD_SHA}\` |

Opened by Actions tip-ship so protected \`${BASE_BRANCH}\` does not reject a direct push (GH013 / required \`offline-checks\`). Tip-ship waits for required checks + merge before Release.

Do **not** tip-ship paused work (e.g. #801) from this record PR.
EOF
)"
  if PR_URL="$(
    gh pr create \
      --base "$BASE_BRANCH" \
      --head "$LAND_BRANCH" \
      --title "$TITLE" \
      --body "$BODY" 2>/dev/null
  )" && [[ -n "$PR_URL" ]]; then
    echo "Opened tip-ship PR: ${PR_URL}"
    break
  fi
  PR_URL=""
  echo "PR create attempt ${attempt}/5 failed (branch may not be visible yet); retrying…" >&2
  sleep 3
done

if [[ -z "$PR_URL" ]]; then
  echo "::error::Failed to open tip-ship land PR for ${LAND_BRANCH} → ${BASE_BRANCH}." >&2
  echo "::error::Live may already be on the new bundle — open/merge ${LAND_BRANCH} manually." >&2
  exit 1
fi

# Hard-require auto-merge (deployment-record PRs are pre-authorized).
if gh pr merge "$PR_URL" --auto --squash; then
  echo "Auto-merge (squash) enabled for ${PR_URL}"
elif gh pr merge "$PR_URL" --auto --merge; then
  echo "Auto-merge (merge) enabled for ${PR_URL}"
else
  echo "::error::Could not enable auto-merge for ${PR_URL}." >&2
  echo "::error::Enable repo auto-merge / check required reviewers, then re-run tip-ship land." >&2
  exit 1
fi

# Wait until required PR checks (offline-checks from pull_request CI) pass.
# Do not use workflow_dispatch as a substitute — it does not satisfy PR required checks.
echo "Waiting for PR checks on ${PR_URL} (timeout ${TIMEOUT_SEC}s)…"
# gh pr checks: 0=pass, 8=pending, other=fail (see gh help pr checks).
deadline=$((SECONDS + TIMEOUT_SEC))
checks_ok=0
while (( SECONDS < deadline )); do
  set +e
  checks_out="$(gh pr checks "$PR_URL" 2>&1)"
  checks_rc=$?
  set -e
  echo "$checks_out" | tail -n 40 || true
  if [[ "$checks_rc" -eq 0 ]]; then
    checks_ok=1
    break
  fi
  if [[ "$checks_rc" -eq 8 ]]; then
    sleep 15
    continue
  fi
  echo "::error::Land PR checks failed for ${PR_URL} (gh exit ${checks_rc})." >&2
  echo "$checks_out" >&2
  exit 1
done

if [[ "$checks_ok" -ne 1 ]]; then
  echo "::error::Timed out waiting for land PR checks on ${PR_URL}." >&2
  gh pr checks "$PR_URL" >&2 || true
  exit 1
fi
echo "Land PR checks passed."

# Wait until auto-merge completes (MERGED). Fail tip-ship if it never lands.
echo "Waiting for ${PR_URL} to merge into ${BASE_BRANCH}…"
merged_sha=""
while (( SECONDS < deadline )); do
  pr_state="$(gh pr view "$PR_URL" --json state --jq '.state')"
  merged_sha="$(gh pr view "$PR_URL" --json mergeCommit --jq '.mergeCommit.oid // empty')"
  if [[ "$pr_state" == "MERGED" ]]; then
    echo "Land PR merged: ${PR_URL} (mergeCommit=${merged_sha:-unknown})"
    break
  fi
  if [[ "$pr_state" == "CLOSED" ]]; then
    echo "::error::Land PR ${PR_URL} closed without merge." >&2
    exit 1
  fi
  sleep 10
done

final_state="$(gh pr view "$PR_URL" --json state --jq '.state')"
if [[ "$final_state" != "MERGED" ]]; then
  echo "::error::Timed out waiting for land PR merge: ${PR_URL} (state=${final_state})." >&2
  exit 1
fi

# Confirm base branch advanced (Git no longer behind live for this baseline).
git fetch origin "$BASE_BRANCH" --quiet
if [[ -n "$merged_sha" ]]; then
  emit merged_sha "$merged_sha"
else
  emit merged_sha "$(git rev-parse "origin/${BASE_BRANCH}")"
fi

emit sha "$HEAD_SHA"
emit short "$HEAD_SHORT"
emit mode "pr"
emit pr_url "$PR_URL"
emit land_branch "$LAND_BRANCH"
echo "Tip-ship land complete (mode=pr, merged). Safe to publish Release."
echo "Land PR: ${PR_URL}"
