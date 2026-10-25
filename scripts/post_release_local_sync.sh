#!/usr/bin/env bash
# Post-release local backup sync: after a new display_version ship
# (deploy + baseline + GitHub Release), immediately align coding clone +
# OneDrive physical trees to the pushed tip.
#
# Mandatory when CODING + ONEDRIVE paths exist (Mac coding clone / self-hosted).
# Soft-skip (exit 0) on Cloud Agents without those paths — still prints the
# operator reminder so tip-ship reports document the gap.
#
# Usage (from repo root or any cwd; script locates itself):
#   bash scripts/post_release_local_sync.sh
#   bash scripts/post_release_local_sync.sh --force
#
# Env:
#   YNOW_CODING / YNOW_ONEDRIVE — override default Mac paths
#   YNOW_SKIP_POST_RELEASE_SYNC=1 — soft-skip even when paths exist
#   YNOW_SYNC_FORCE=1 — same as --force for sync-workspaces
set -euo pipefail

CODING="${YNOW_CODING:-/Users/lawrencekuo/coding/theYNowApp}"
ONEDRIVE="${YNOW_ONEDRIVE:-/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp}"
WRAPPER="${YNOW_SYNC_BIN:-/Users/lawrencekuo/coding/bin/sync-theynow-workspaces.sh}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SYNC_SCRIPT="$REPO_ROOT/scripts/sync-workspaces.sh"
CHECK_SCRIPT="$REPO_ROOT/scripts/check-od-sync.sh"
FORCE_FLAG=""
for arg in "$@"; do
  case "$arg" in
    --force|-f) FORCE_FLAG="--force" ;;
  esac
done

log() { printf '[ynow-post-release-sync] %s\n' "$*"; }
warn() { printf '[ynow-post-release-sync] WARN: %s\n' "$*" >&2; }

if [[ "${YNOW_SKIP_POST_RELEASE_SYNC:-}" == "1" ]]; then
  log "YNOW_SKIP_POST_RELEASE_SYNC=1 — soft-skip (operator must sync local backups manually)."
  exit 0
fi

missing=()
[[ -d "$CODING/.git" ]] || missing+=("coding clone missing: $CODING")
[[ -d "$ONEDRIVE" ]] || missing+=("OneDrive mirror missing: $ONEDRIVE")

if ((${#missing[@]} > 0)); then
  warn "Local physical backup sync soft-skipped (paths not present on this host):"
  for m in "${missing[@]}"; do
    warn "  - $m"
  done
  warn "REQUIRED on Mac coding clone after every display_version ship:"
  warn "  1) bash scripts/sync-workspaces.sh --force"
  warn "     (or $WRAPPER --force)"
  warn "  2) bash scripts/check-od-sync.sh"
  warn "Do not mark tip-ship complete on Mac until check-od-sync OK."
  exit 0
fi

if [[ ! -x "$SYNC_SCRIPT" && ! -f "$SYNC_SCRIPT" ]]; then
  warn "Missing $SYNC_SCRIPT — cannot sync."
  exit 1
fi

log "Running immediate post-release sync (coding → OneDrive backup)…"
log "CODING=$CODING"
log "ONEDRIVE=$ONEDRIVE"

# Prefer in-repo script; Mac wrapper is optional convenience.
export YNOW_CODING="$CODING"
export YNOW_ONEDRIVE="$ONEDRIVE"
# Post-release always forces past the 90s debounce so the new version lands now.
export YNOW_SYNC_FORCE=1

if [[ -n "$FORCE_FLAG" ]]; then
  bash "$SYNC_SCRIPT" --force
elif [[ -x "$WRAPPER" || -f "$WRAPPER" ]]; then
  # Wrapper delegates to in-repo script when present.
  bash "$WRAPPER" --force
else
  bash "$SYNC_SCRIPT" --force
fi

log "Verifying OneDrive alignment…"
bash "$CHECK_SCRIPT"
log "OK — local physical trees synced as backup of released tip."
