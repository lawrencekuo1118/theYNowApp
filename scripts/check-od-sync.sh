#!/usr/bin/env bash
# Safe OneDrive alignment check — do NOT use git status in OneDrive (cloud mmap timeouts).
set -euo pipefail

CODING="${YNOW_CODING:-/Users/lawrencekuo/coding/theYNowApp}"
ONEDRIVE="${YNOW_ONEDRIVE:-/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp}"
SHA_FILE="$ONEDRIVE/.ynow-synced-sha"

coding_sha=$(git -C "$CODING" rev-parse HEAD)
coding_short=$(git -C "$CODING" rev-parse --short HEAD)
od_sha=$(tr -d '[:space:]' <"$SHA_FILE" 2>/dev/null || true)

echo "coding:   $coding_short ($coding_sha)"
if [[ -z "$od_sha" ]]; then
  echo "onedrive: MISSING .ynow-synced-sha — run sync-theynow-workspaces.sh"
  exit 2
fi
od_short=${od_sha:0:7}
echo "onedrive: $od_short ($od_sha)  [from .ynow-synced-sha]"
if [[ "$coding_sha" == "$od_sha" ]]; then
  echo "OK aligned"
  exit 0
fi
echo "MISMATCH — run: /Users/lawrencekuo/coding/bin/sync-theynow-workspaces.sh"
exit 1
