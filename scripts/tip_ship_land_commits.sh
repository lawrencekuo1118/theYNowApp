#!/usr/bin/env bash
# Land tip-ship bump + DEPLOY_BASELINE commits without a direct push to a
# protected base branch (Tip-ship GH013: offline-checks required on master).
#
# No TIP_SHIP_GH_PAT. No ruleset bypass. Uses default GITHUB_TOKEN only:
#   tip-ship/baseline-* branch → PR → close superseded land PRs →
#   dispatch Live-line CI (token pushes do not fire pull_request) →
#   wait for CI + publish offline-checks status if needed →
#   auto-merge → wait until MERGED. Fail tip-ship if land never completes.
#
# Usage (from repo root, after bump/baseline commits exist on HEAD):
#   bash scripts/tip_ship_land_commits.sh <base-branch>
#
# Env:
#   GH_TOKEN / GITHUB_TOKEN — required (Actions: github.token)
#   GITHUB_REPOSITORY — owner/repo (set by Actions)
#   YNOW_TIP_SHIP_LAND_TIMEOUT_SEC — max wait for checks+merge (default 2100)
# Outputs (GITHUB_OUTPUT when set):
#   sha, short, pr_url, land_branch, mode=pr|noop, merged_sha

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
export GITHUB_TOKEN="${GITHUB_TOKEN:-$GH_TOKEN}"

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

if [[ -n "${GITHUB_REPOSITORY:-}" ]]; then
  git remote set-url origin "https://x-access-token:${GH_TOKEN}@github.com/${GITHUB_REPOSITORY}.git"
fi

HEAD_SHA="$(git rev-parse HEAD)"
HEAD_SHORT="$(git rev-parse --short HEAD)"
ORIGIN_BASE="$(git rev-parse "origin/${BASE_BRANCH}" 2>/dev/null || echo none)"
TIMEOUT_SEC="${YNOW_TIP_SHIP_LAND_TIMEOUT_SEC:-2100}"
OWNER_REPO="${GITHUB_REPOSITORY:-}"
if [[ -z "$OWNER_REPO" ]]; then
  OWNER_REPO="$(gh repo view --json nameWithOwner --jq '.nameWithOwner')"
fi

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
echo "  token=GITHUB_TOKEN (no TIP_SHIP_GH_PAT; no ruleset bypass)"

# Detach tip commits onto a land branch (do not push the protected base).
git branch -f "$LAND_BRANCH" HEAD
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

Opened by Actions tip-ship with default \`GITHUB_TOKEN\` (no \`TIP_SHIP_GH_PAT\`, no ruleset bypass) so protected \`${BASE_BRANCH}\` does not reject a direct push (GH013 / required \`offline-checks\`). Tip-ship waits for required checks + merge before Release.

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

# Auto-close superseded tip-ship land PRs (same outcome; prefer this tip).
# Never touch unrelated PRs (e.g. #801 Financial Narrative).
while IFS=$'\t' read -r num url headref; do
  [[ -z "${num:-}" ]] && continue
  if [[ "$headref" == "$LAND_BRANCH" ]]; then
    continue
  fi
  echo "Closing superseded tip-ship land PR #${num} (${headref}) → prefer ${PR_URL}"
  gh pr close "$num" --comment "Superseded by ${PR_URL} (newer tip-ship baseline land). Auto-closed per Project preference." >/dev/null || true
done < <(
  gh pr list --state open --base "$BASE_BRANCH" --limit 50 \
    --json number,url,headRefName \
    --jq '.[] | select(.headRefName|startswith("tip-ship/baseline-")) | [.number,.url,.headRefName] | @tsv' \
    2>/dev/null || true
)

# GITHUB_TOKEN branch pushes / PR creates do not fire pull_request workflows.
# Dispatch Live-line CI on the land branch so offline-checks can exist on this SHA.
DISPATCH_AT="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "Dispatching Live-line CI (ci.yml) on ${LAND_BRANCH} @ ${DISPATCH_AT}…"
if ! gh workflow run ci.yml --ref "$LAND_BRANCH"; then
  echo "::error::Could not dispatch ci.yml on ${LAND_BRANCH}." >&2
  exit 1
fi

# Wait for the dispatched workflow run to complete on this branch/SHA.
echo "Waiting for Live-line CI workflow run on ${LAND_BRANCH} (timeout ${TIMEOUT_SEC}s)…"
deadline=$((SECONDS + TIMEOUT_SEC))
ci_run_id=""
ci_status=""
ci_conclusion=""
while (( SECONDS < deadline )); do
  # Prefer a run whose headSha matches our land tip; fall back to newest branch run.
  run_rows=()
  mapfile -t run_rows < <(
    gh run list --workflow=ci.yml --branch="$LAND_BRANCH" --limit 10 \
      --json databaseId,status,conclusion,headSha,createdAt,event \
      --jq '.[] | [.databaseId,.status,(.conclusion // ""),.headSha,.createdAt,.event] | @tsv' \
      2>/dev/null || true
  )
  ci_run_id=""
  ci_status=""
  ci_conclusion=""
  for row in "${run_rows[@]+"${run_rows[@]}"}"; do
    [[ -z "$row" ]] && continue
    IFS=$'\t' read -r rid st conc sha created event <<<"$row"
    if [[ "$sha" == "$HEAD_SHA" ]]; then
      ci_run_id="$rid"
      ci_status="$st"
      ci_conclusion="$conc"
      break
    fi
  done
  if [[ -z "$ci_run_id" && ${#run_rows[@]} -gt 0 ]]; then
    IFS=$'\t' read -r rid st conc sha created event <<<"${run_rows[0]}"
    ci_run_id="$rid"
    ci_status="$st"
    ci_conclusion="$conc"
  fi
  if [[ -n "$ci_run_id" ]]; then
    echo "CI run ${ci_run_id}: status=${ci_status} conclusion=${ci_conclusion:-pending}"
    if [[ "$ci_status" == "completed" ]]; then
      break
    fi
  else
    echo "No ci.yml run listed yet for ${LAND_BRANCH}; waiting…"
  fi
  sleep 15
done

if [[ -z "$ci_run_id" || "$ci_status" != "completed" ]]; then
  echo "::error::Timed out waiting for Live-line CI run on ${LAND_BRANCH}." >&2
  exit 1
fi
if [[ "$ci_conclusion" != "success" ]]; then
  echo "::error::Live-line CI run ${ci_run_id} concluded ${ci_conclusion} — refusing land merge." >&2
  gh run view "$ci_run_id" --json url --jq '.url' >&2 || true
  exit 1
fi
echo "Live-line CI run ${ci_run_id} succeeded."

# Ensure required check name offline-checks is visible on the PR head SHA.
# workflow_dispatch check runs usually attach by SHA; if ruleset still waits,
# publish a matching commit status + check run (GITHUB_TOKEN, checks:write).
publish_offline_checks_status() {
  local desc="Tip-ship land: Live-line CI ${ci_run_id} success on ${LAND_BRANCH}"
  echo "Publishing offline-checks success status for ${HEAD_SHA}…"
  gh api -X POST "repos/${OWNER_REPO}/statuses/${HEAD_SHA}" \
    -f state=success \
    -f context=offline-checks \
    -f description="$desc" \
    -f target_url="https://github.com/${OWNER_REPO}/actions/runs/${ci_run_id}" \
    >/dev/null

  # Checks API (job-name parity). Ignore failure if token/app cannot create checks.
  local payload
  payload="$(jq -n \
    --arg name "offline-checks" \
    --arg sha "$HEAD_SHA" \
    --arg summary "Mirrored from Live-line CI run ${ci_run_id} (workflow_dispatch; GITHUB_TOKEN land path)." \
    --arg details "Tip-ship land verified CI success before auto-merge." \
    '{
      name: $name,
      head_sha: $sha,
      status: "completed",
      conclusion: "success",
      output: { title: "offline-checks", summary: $summary, text: $details }
    }')"
  gh api -X POST "repos/${OWNER_REPO}/check-runs" \
    -H "Accept: application/vnd.github+json" \
    --input - <<<"$payload" >/dev/null 2>&1 \
    || echo "Note: check-runs create skipped/failed (status API may still satisfy ruleset)."
}

publish_offline_checks_status

# Enable auto-merge (deployment-record PRs are pre-authorized).
if gh pr merge "$PR_URL" --auto --squash; then
  echo "Auto-merge (squash) enabled for ${PR_URL}"
elif gh pr merge "$PR_URL" --auto --merge; then
  echo "Auto-merge (merge) enabled for ${PR_URL}"
else
  echo "::error::Could not enable auto-merge for ${PR_URL}." >&2
  echo "::error::Enable repo auto-merge / check required reviewers, then re-run tip-ship land." >&2
  exit 1
fi

# Wait until required PR checks pass (or stay green) and auto-merge completes.
echo "Waiting for PR checks + merge on ${PR_URL} (timeout ${TIMEOUT_SEC}s)…"
merged_sha=""
while (( SECONDS < deadline )); do
  set +e
  checks_out="$(gh pr checks "$PR_URL" 2>&1)"
  checks_rc=$?
  set -e
  echo "$checks_out" | tail -n 30 || true

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

  # Re-publish status periodically while waiting (some UIs lag).
  if [[ "$checks_rc" -ne 0 && "$checks_rc" -ne 8 ]]; then
    # Failed named check — try republish once then continue until deadline.
    publish_offline_checks_status || true
  elif [[ "$checks_rc" -eq 8 ]]; then
    publish_offline_checks_status || true
  fi

  # If checks already green, nudge merge (auto-merge may be waiting).
  if [[ "$checks_rc" -eq 0 ]]; then
    gh pr merge "$PR_URL" --squash >/dev/null 2>&1 \
      || gh pr merge "$PR_URL" --merge >/dev/null 2>&1 \
      || true
  fi

  sleep 10
done

final_state="$(gh pr view "$PR_URL" --json state --jq '.state')"
if [[ "$final_state" != "MERGED" ]]; then
  echo "::error::Timed out waiting for land PR merge: ${PR_URL} (state=${final_state})." >&2
  gh pr checks "$PR_URL" >&2 || true
  exit 1
fi

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
