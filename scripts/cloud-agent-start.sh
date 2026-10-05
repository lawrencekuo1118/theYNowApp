#!/usr/bin/env bash
# Boot-time Shiny dev server for Cloud Agents (live line app_21.0).
# Idempotent: exits 0 when port 3838 is already listening.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PORT="${YNOW_PORT:-3838}"

port_open() {
  if command -v ss >/dev/null 2>&1; then
    ss -ltn "sport = :${PORT}" | grep -q LISTEN
    return
  fi
  python3 - "$PORT" <<'PY'
import socket, sys
port = int(sys.argv[1])
sock = socket.socket()
sock.settimeout(1)
try:
    sys.exit(0 if sock.connect_ex(("127.0.0.1", port)) == 0 else 1)
finally:
    sock.close()
PY
}

if port_open; then
  echo "cloud-agent-start: shiny already listening on ${PORT}"
  exit 0
fi

cd "${ROOT}/app_21.0"
exec Rscript -e "shiny::runApp(host='0.0.0.0', port=${PORT}, launch.browser=FALSE)"
