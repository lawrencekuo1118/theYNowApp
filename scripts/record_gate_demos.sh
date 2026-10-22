#!/usr/bin/env bash
# Record Sign-in gate demos from the local App UI (product surfaces only).
# Non-product gate ids (snap_defaults, inner_lock) are not captured.
# The capture seed is local-only. Never set YNOW_DEMO_CAPTURE=1 on shinyapps.io or any production host.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PORT="${YNOW_DEMO_PORT:-3840}"
BASE_URL="${YNOW_DEMO_BASE_URL:-http://127.0.0.1:${PORT}/}"
LOG="${TMPDIR:-/tmp}/ynow-demo-capture-${PORT}.log"

port_open() {
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
  echo "record_gate_demos: using Shiny already listening on ${PORT}"
else
  echo "record_gate_demos: starting local capture Shiny on ${PORT}"
  # Child process only. Do not export the capture seed into this shell or a deploy.
  nohup env YNOW_DEMO_CAPTURE=1 \
    Rscript -e "shiny::runApp(appDir='${ROOT}/app_26.0', host='127.0.0.1', port=${PORT}, launch.browser=FALSE)" \
    >"${LOG}" 2>&1 &
  echo $! >"${TMPDIR:-/tmp}/ynow-demo-capture-${PORT}.pid"
  disown || true
fi

deadline=$((SECONDS + 180))
while true; do
  code="$(curl -s -o /dev/null -w '%{http_code}' --max-time 3 "${BASE_URL}" || true)"
  if [[ "${code}" == "200" ]]; then
    break
  fi
  if (( SECONDS >= deadline )); then
    echo "record_gate_demos: ${BASE_URL} did not return HTTP 200" >&2
    exit 1
  fi
  sleep 2
done

if [[ -x "${ROOT}/.venv-demos/bin/python" ]]; then
  PY="${ROOT}/.venv-demos/bin/python"
else
  PY="$(command -v python3)"
fi

export YNOW_DEMO_BASE_URL="${BASE_URL}"
"${PY}" "${ROOT}/scripts/generate_gate_demo_videos.py"
