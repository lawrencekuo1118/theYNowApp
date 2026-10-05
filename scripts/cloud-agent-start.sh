#!/usr/bin/env bash
# Boot-time Shiny dev server for Cloud Agents (live line app_20.0).
# Idempotent: exits 0 when port 3838 is already listening.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PORT="${YNOW_PORT:-3838}"

if command -v ss >/dev/null 2>&1; then
  if ss -ltn "sport = :${PORT}" | grep -q LISTEN; then
    echo "cloud-agent-start: shiny already listening on ${PORT}"
    exit 0
  fi
fi

cd "${ROOT}/app_20.0"
exec Rscript -e "shiny::runApp(host='0.0.0.0', port=${PORT}, launch.browser=FALSE)"
