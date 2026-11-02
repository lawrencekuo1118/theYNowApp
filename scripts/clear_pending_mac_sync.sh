#!/usr/bin/env bash
# Mark scripts/PENDING_MAC_SYNC.env as synced after a successful Mac local sync.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PENDING="$REPO_ROOT/scripts/PENDING_MAC_SYNC.env"
BASELINE="$REPO_ROOT/scripts/DEPLOY_BASELINE.txt"
STAMP_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/theynow"
STAMP_FILE="$STAMP_DIR/last-post-release-synced-version"

baseline_field() {
  local key="$1"
  [[ -f "$BASELINE" ]] || return 0
  sed -n "s/^${key}=//p" "$BASELINE" | head -1 | tr -d '[:space:]'
}

DISPLAY_VERSION="${1:-}"
if [[ -z "$DISPLAY_VERSION" ]]; then
  DISPLAY_VERSION="$(baseline_field display_version)"
fi
BASELINE_COMMIT="$(baseline_field commit)"
[[ -z "$BASELINE_COMMIT" ]] && BASELINE_COMMIT="$(baseline_field aligned_git)"
BUNDLE_ID="$(baseline_field bundleId)"
NOW="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

mkdir -p "$STAMP_DIR"
if [[ -n "$DISPLAY_VERSION" ]]; then
  printf '%s\n' "$DISPLAY_VERSION" >"$STAMP_FILE"
fi

umask 022
cat >"$PENDING" <<EOF
# Cleared after successful Mac coding + OneDrive post-release sync.
status=synced
display_version=${DISPLAY_VERSION:-unknown}
baseline_commit=${BASELINE_COMMIT}
bundle_id=${BUNDLE_ID}
synced_at=${NOW}
note=OK — local physical trees matched released tip (check-od-sync).
EOF

printf '[ynow-clear-pending-mac-sync] marked synced (%s)\n' "${DISPLAY_VERSION:-unknown}"
