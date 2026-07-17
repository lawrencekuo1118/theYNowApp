#!/usr/bin/env bash
# Thin launcher for cloud_sync_guard.py (macOS / Linux).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
exec python3 "$ROOT/tools/cloud_sync_guard.py" --project "$ROOT" "$@"
