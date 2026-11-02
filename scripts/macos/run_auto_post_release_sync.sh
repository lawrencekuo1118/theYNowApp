#!/usr/bin/env bash
# Mac-side auto catch-up for post-release coding + OneDrive sync.
# Intended for launchd, Cursor self-hosted workers, or GH self-hosted runners.
#
# Compares local stamp / PENDING marker / origin DEPLOY_BASELINE display_version.
# When behind (or --force), ff-pulls coding master, runs post_release_local_sync,
# verifies with check-od-sync, clears PENDING.
#
# Usage:
#   bash scripts/macos/run_auto_post_release_sync.sh
#   bash scripts/macos/run_auto_post_release_sync.sh --force
set -euo pipefail

CODING="${YNOW_CODING:-/Users/lawrencekuo/coding/theYNowApp}"
ONEDRIVE="${YNOW_ONEDRIVE:-/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp}"
BRANCH="${YNOW_BRANCH:-master}"
STAMP_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/theynow"
STAMP_FILE="$STAMP_DIR/last-post-release-synced-version"
FORCE=0
for arg in "$@"; do
  case "$arg" in
    --force|-f) FORCE=1 ;;
  esac
done

log() { printf '[ynow-mac-auto-sync] %s\n' "$*"; }
warn() { printf '[ynow-mac-auto-sync] WARN: %s\n' "$*" >&2; }
die() { printf '[ynow-mac-auto-sync] ERROR: %s\n' "$*" >&2; exit 1; }

if [[ ! -d "$CODING/.git" ]]; then
  die "coding clone missing: $CODING (start this on the Mac coding host)"
fi
if [[ ! -d "$ONEDRIVE" ]]; then
  die "OneDrive mirror missing: $ONEDRIVE"
fi

cd "$CODING"
PENDING="$CODING/scripts/PENDING_MAC_SYNC.env"
SYNC_SCRIPT="$CODING/scripts/post_release_local_sync.sh"
CHECK_SCRIPT="$CODING/scripts/check-od-sync.sh"
CLEAR_SCRIPT="$CODING/scripts/clear_pending_mac_sync.sh"

pending_status() {
  [[ -f "$PENDING" ]] || { echo ""; return; }
  sed -n 's/^status=//p' "$PENDING" | head -1 | tr -d '[:space:]'
}

pending_version() {
  [[ -f "$PENDING" ]] || { echo ""; return; }
  sed -n 's/^display_version=//p' "$PENDING" | head -1 | tr -d '[:space:]'
}

log "fetch origin/$BRANCH"
git fetch origin "$BRANCH"
REMOTE_VER="$(git show "origin/${BRANCH}:scripts/DEPLOY_BASELINE.txt" 2>/dev/null | sed -n 's/^display_version=//p' | head -1 | tr -d '[:space:]' || true)"
LOCAL_STAMP="$(cat "$STAMP_FILE" 2>/dev/null | tr -d '[:space:]' || true)"
P_STATUS="$(pending_status)"
P_VER="$(pending_version)"

NEED=0
if [[ "$FORCE" == "1" ]]; then
  NEED=1
  log "force sync requested"
elif [[ "$P_STATUS" == "pending" ]]; then
  NEED=1
  log "PENDING_MAC_SYNC.env status=pending (${P_VER:-unknown})"
elif [[ -n "$REMOTE_VER" && "$REMOTE_VER" != "$LOCAL_STAMP" ]]; then
  NEED=1
  log "stamp behind remote baseline ($LOCAL_STAMP → $REMOTE_VER)"
else
  log "up to date (stamp=${LOCAL_STAMP:-none} remote=${REMOTE_VER:-unknown} pending=${P_STATUS:-none})"
  exit 0
fi

CURRENT_BRANCH="$(git branch --show-current 2>/dev/null || true)"
if [[ "$CURRENT_BRANCH" != "$BRANCH" ]]; then
  warn "coding on '$CURRENT_BRANCH' — checking out $BRANCH for post-release sync"
  git checkout -q "$BRANCH"
fi
if ! git merge --ff-only "origin/$BRANCH"; then
  die "coding cannot ff-only to origin/$BRANCH — resolve locally then re-run"
fi

export YNOW_CODING="$CODING"
export YNOW_ONEDRIVE="$ONEDRIVE"
export YNOW_SYNC_FORCE=1
# Avoid re-entrant handoff noise when paths exist.
export YNOW_SKIP_MAC_SYNC_HANDOFF=1

log "running post_release_local_sync.sh"
bash "$SYNC_SCRIPT" --force
log "verifying check-od-sync.sh"
bash "$CHECK_SCRIPT"
bash "$CLEAR_SCRIPT" "${REMOTE_VER:-$P_VER}"
log "OK — Mac local backup synced for ${REMOTE_VER:-$P_VER}"
