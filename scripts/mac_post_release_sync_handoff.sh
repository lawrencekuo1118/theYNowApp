#!/usr/bin/env bash
# Cloud → Mac tip-ship handoff for post-release local backup sync.
#
# When Cloud soft-skips (no CODING/ONEDRIVE paths), record a pending marker and
# try to wake durable Mac automation (GH workflow_dispatch). Does not fail the
# Cloud ship; prints exact operator / Mac-worker commands.
#
# Usage:
#   bash scripts/mac_post_release_sync_handoff.sh
#   bash scripts/mac_post_release_sync_handoff.sh --reason cloud_soft_skip
#   bash scripts/mac_post_release_sync_handoff.sh --from-ci
#
# Env:
#   YNOW_SKIP_MAC_SYNC_HANDOFF=1 — no-op
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PENDING="$REPO_ROOT/scripts/PENDING_MAC_SYNC.env"
BASELINE="$REPO_ROOT/scripts/DEPLOY_BASELINE.txt"
REASON="cloud_soft_skip"
FROM_CI=0
NO_DISPATCH=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --from-ci) FROM_CI=1; shift ;;
    --no-dispatch) NO_DISPATCH=1; shift ;;
    --reason)
      REASON="${2:-cloud_soft_skip}"
      shift 2
      ;;
    --reason=*)
      REASON="${1#--reason=}"
      shift
      ;;
    *)
      printf '[ynow-mac-sync-handoff] WARN: unknown arg %s\n' "$1" >&2
      shift
      ;;
  esac
done

log() { printf '[ynow-mac-sync-handoff] %s\n' "$*"; }
warn() { printf '[ynow-mac-sync-handoff] WARN: %s\n' "$*" >&2; }

if [[ "${YNOW_SKIP_MAC_SYNC_HANDOFF:-}" == "1" ]]; then
  log "YNOW_SKIP_MAC_SYNC_HANDOFF=1 — skip"
  exit 0
fi

baseline_field() {
  local key="$1"
  [[ -f "$BASELINE" ]] || return 0
  sed -n "s/^${key}=//p" "$BASELINE" | head -1 | tr -d '[:space:]'
}

DISPLAY_VERSION="$(baseline_field display_version)"
BASELINE_COMMIT="$(baseline_field commit)"
[[ -z "$BASELINE_COMMIT" ]] && BASELINE_COMMIT="$(baseline_field aligned_git)"
BUNDLE_ID="$(baseline_field bundleId)"
NOW="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
REQUESTED_BY="${YNOW_MAC_SYNC_REQUESTED_BY:-handoff}"
if [[ "$FROM_CI" == "1" ]]; then
  REQUESTED_BY="github-actions"
fi

if [[ -z "$DISPLAY_VERSION" ]]; then
  warn "No display_version in DEPLOY_BASELINE.txt — still writing pending stub."
  DISPLAY_VERSION="unknown"
fi

umask 022
cat >"$PENDING" <<EOF
# Machine-readable handoff for Mac coding + OneDrive post-release sync.
# Written when Cloud tip ships soft-skip (no Mac paths). Cleared when
# scripts/post_release_local_sync.sh succeeds on a host with Mac paths.
# Mac launchd / self-hosted worker / GH self-hosted runner reads this.
status=pending
display_version=${DISPLAY_VERSION}
baseline_commit=${BASELINE_COMMIT}
bundle_id=${BUNDLE_ID}
reason=${REASON}
requested_at=${NOW}
requested_by=${REQUESTED_BY}
note=Run on Mac: bash scripts/post_release_local_sync.sh --force && bash scripts/check-od-sync.sh
EOF
log "Wrote pending marker: $PENDING ($DISPLAY_VERSION)"

# Best-effort: wake GH self-hosted Mac workflow (no-op if no runner / no auth).
if [[ "$NO_DISPATCH" == "1" ]]; then
  log "Skipping workflow_dispatch (--no-dispatch)."
elif command -v gh >/dev/null 2>&1; then
  if gh auth status >/dev/null 2>&1 || [[ -n "${GH_TOKEN:-}${GITHUB_TOKEN:-}" ]]; then
    if gh workflow run post-release-mac-sync.yml \
      -f "display_version=${DISPLAY_VERSION}" \
      -f "reason=${REASON}" >/dev/null 2>&1; then
      log "Dispatched workflow post-release-mac-sync.yml for ${DISPLAY_VERSION}"
    else
      warn "Could not dispatch post-release-mac-sync.yml (workflow missing on default branch, permissions, or runner not registered yet)."
      warn "Mac launchd / Cursor self-hosted worker still apply — see docs."
    fi
  else
    warn "gh not authenticated — skip workflow_dispatch."
  fi
else
  warn "gh not installed — skip workflow_dispatch."
fi

cat <<EOF

========== Mac post-release sync REQUIRED ==========
display_version: ${DISPLAY_VERSION}
baseline_commit: ${BASELINE_COMMIT}
bundle_id:       ${BUNDLE_ID}
pending file:    scripts/PENDING_MAC_SYNC.env

BLOCKER (no Cursor self-hosted Mac worker connected from Cloud):
  On the Mac that holds the coding clone + OneDrive mirror, keep a worker online:

    agent login
    agent worker start

  Docs: https://cursor.com/docs/cloud-agent/self-hosted/my-machines

  Optional always-on (no Cursor agent required):
    bash scripts/macos/install-post-release-sync-launchd.sh

  When the Mac is reachable, catch up immediately:
    cd /Users/lawrencekuo/coding/theYNowApp
    git fetch origin master && git checkout master && git merge --ff-only origin/master
    bash scripts/post_release_local_sync.sh --force
    bash scripts/check-od-sync.sh

  Or: bash scripts/macos/run_auto_post_release_sync.sh --force
====================================================
EOF

exit 0
