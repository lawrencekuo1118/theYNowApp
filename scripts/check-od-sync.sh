#!/usr/bin/env bash
# Safe OneDrive alignment check — do NOT use git status in OneDrive (cloud mmap timeouts).
set -euo pipefail

CODING="${YNOW_CODING:-/Users/lawrencekuo/coding/theYNowApp}"
ONEDRIVE="${YNOW_ONEDRIVE:-/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp}"
SHA_FILE="$ONEDRIVE/.ynow-synced-sha"
LIVE_APP="${YNOW_LIVE_APP:-app_27.0}"
STATUS_FILE="$ONEDRIVE/.ynow-sync-status.txt"
SYNC_BIN="${YNOW_SYNC_BIN:-/Users/lawrencekuo/coding/bin/sync-theynow-workspaces.sh}"
SYNC_REPO="$CODING/scripts/sync-workspaces.sh"

coding_sha=$(git -C "$CODING" rev-parse HEAD)
coding_short=$(git -C "$CODING" rev-parse --short HEAD)

echo "coding:   $coding_short ($coding_sha)"
if [[ ! -f "$SHA_FILE" ]]; then
  echo "onedrive: MISSING .ynow-synced-sha — run sync-workspaces.sh"
  exit 2
fi
od_sha=$(tr -d '[:space:]' <"$SHA_FILE")
od_short=${od_sha:0:7}
echo "onedrive: $od_short ($od_sha)  [from .ynow-synced-sha]"

rc=0
if [[ "$coding_sha" != "$od_sha" ]]; then
  echo "MISMATCH sha — run: $SYNC_REPO  (or $SYNC_BIN)"
  rc=1
else
  echo "OK sha aligned"
fi

if [[ ! -d "$ONEDRIVE/$LIVE_APP" ]]; then
  echo "MISSING $LIVE_APP on OneDrive — run: $SYNC_REPO --force"
  rc=1
elif [[ ! -f "$ONEDRIVE/$LIVE_APP/app.R" ]]; then
  echo "MISSING $LIVE_APP/app.R on OneDrive — re-run sync --force"
  rc=1
elif ! cmp -s "$CODING/$LIVE_APP/app.R" "$ONEDRIVE/$LIVE_APP/app.R"; then
  echo "MISMATCH $LIVE_APP/app.R — re-run sync --force"
  rc=1
else
  echo "OK $LIVE_APP/app.R matches coding"
fi

if [[ -f "$STATUS_FILE" ]]; then
  status_app=$(awk -F= '/^app=/{print $2; exit}' "$STATUS_FILE" || true)
  if [[ -n "$status_app" && "$status_app" != "$LIVE_APP" ]]; then
    echo "WARN status app=$status_app (expected $LIVE_APP) — re-run sync --force"
    rc=1
  fi
fi

if [[ "$rc" -eq 0 ]]; then
  echo "OK aligned (sha + $LIVE_APP)"
fi
exit "$rc"
