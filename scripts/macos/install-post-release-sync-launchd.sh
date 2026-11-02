#!/usr/bin/env bash
# Install / reload the Mac launchd agent that auto-runs post-release sync.
# Safe to re-run. Requires Mac coding clone paths.
set -euo pipefail

CODING="${YNOW_CODING:-/Users/lawrencekuo/coding/theYNowApp}"
LABEL="com.theynow.post-release-sync"
PLIST_SRC="$CODING/scripts/macos/${LABEL}.plist"
PLIST_DST="${HOME}/Library/LaunchAgents/${LABEL}.plist"
LOG_DIR="${HOME}/Library/Logs"

log() { printf '[ynow-install-launchd] %s\n' "$*"; }
die() { printf '[ynow-install-launchd] ERROR: %s\n' "$*" >&2; exit 1; }

[[ "$(uname -s)" == "Darwin" ]] || die "This installer is for macOS only."
[[ -d "$CODING/.git" ]] || die "coding clone missing: $CODING"
[[ -f "$PLIST_SRC" ]] || die "missing plist: $PLIST_SRC"
[[ -x "$CODING/scripts/macos/run_auto_post_release_sync.sh" || -f "$CODING/scripts/macos/run_auto_post_release_sync.sh" ]] \
  || die "missing runner script"

mkdir -p "$(dirname "$PLIST_DST")" "$LOG_DIR"
chmod +x "$CODING/scripts/macos/run_auto_post_release_sync.sh" \
  "$CODING/scripts/post_release_local_sync.sh" \
  "$CODING/scripts/mac_post_release_sync_handoff.sh" \
  "$CODING/scripts/clear_pending_mac_sync.sh" \
  "$CODING/scripts/sync-workspaces.sh" \
  "$CODING/scripts/check-od-sync.sh" || true

cp -f "$PLIST_SRC" "$PLIST_DST"
# Ensure ProgramArguments path matches this coding clone.
if [[ "$CODING" != "/Users/lawrencekuo/coding/theYNowApp" ]]; then
  /usr/bin/sed -i '' "s|/Users/lawrencekuo/coding/theYNowApp|${CODING}|g" "$PLIST_DST"
fi

launchctl bootout "gui/$(id -u)/${LABEL}" 2>/dev/null || true
launchctl bootstrap "gui/$(id -u)" "$PLIST_DST"
launchctl enable "gui/$(id -u)/${LABEL}" 2>/dev/null || true
launchctl kickstart -k "gui/$(id -u)/${LABEL}" 2>/dev/null || launchctl start "$LABEL" || true

log "Installed $PLIST_DST"
log "Interval: 300s + RunAtLoad"
log "Logs: $LOG_DIR/theynow-post-release-sync.log"
log "Manual catch-up: bash $CODING/scripts/macos/run_auto_post_release_sync.sh --force"
log "Also keep Cursor worker online for Cloud tip-ship handoff:"
log "  agent login && agent worker start"
