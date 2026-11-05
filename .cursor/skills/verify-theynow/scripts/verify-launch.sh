#!/usr/bin/env bash
# Launch an isolated theYNowApp Shiny instance for verification.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)"
APP_DIR="${ROOT}/app_27.0"
PORT="${YNOW_VERIFY_PORT:-13838}"
RUN_ID="${RUN_ID:-$(date -u +%Y%m%dT%H%M%SZ)-$$}"
RUN_DIR="${YNOW_VERIFY_RUN_DIR:-/tmp/ynow-verify-${RUN_ID}}"
ALLOW_SHARED="${YNOW_VERIFY_ALLOW_SHARED:-0}"
HOST="${YNOW_VERIFY_HOST:-127.0.0.1}"
READY_TRIES="${YNOW_VERIFY_READY_TRIES:-90}"

if [[ ! -d "$APP_DIR" ]]; then
  echo "verify-launch: missing app dir: $APP_DIR" >&2
  exit 1
fi

if [[ "$PORT" == "3838" && "$ALLOW_SHARED" != "1" ]]; then
  echo "verify-launch: refusing shared Cloud port 3838 (set YNOW_VERIFY_ALLOW_SHARED=1 only if you own it exclusively)" >&2
  exit 2
fi

port_listening() {
  local p="$1"
  if command -v ss >/dev/null 2>&1; then
    ss -ltn "sport = :${p}" 2>/dev/null | grep -q LISTEN
    return
  fi
  python3 - "$p" <<'PY'
import socket, sys
port = int(sys.argv[1])
s = socket.socket()
s.settimeout(1)
try:
    sys.exit(0 if s.connect_ex(("127.0.0.1", port)) == 0 else 1)
finally:
    s.close()
PY
}

if port_listening "$PORT"; then
  echo "verify-launch: port ${PORT} already listening — refuse to steal/share" >&2
  exit 3
fi

mkdir -p "$RUN_DIR"
LOG="${RUN_DIR}/shiny.log"
META="${RUN_DIR}/meta.env"

cd "$APP_DIR"
# shellcheck disable=SC2086
nohup Rscript -e "shiny::runApp(host='${HOST}', port=${PORT}, launch.browser=FALSE)" \
  >"$LOG" 2>&1 &
PID=$!

URL="http://${HOST}:${PORT}/"
{
  echo "RUN_ID=${RUN_ID}"
  echo "PID=${PID}"
  echo "PORT=${PORT}"
  echo "HOST=${HOST}"
  echo "URL=${URL}"
  echo "APP_DIR=${APP_DIR}"
  echo "STARTED_AT=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "LOG=${LOG}"
} >"$META"

export YNOW_VERIFY_RUN_DIR="$RUN_DIR"

echo "verify-launch: started pid=${PID} url=${URL} run_dir=${RUN_DIR}"

ready=0
for i in $(seq 1 "$READY_TRIES"); do
  if ! kill -0 "$PID" 2>/dev/null; then
    echo "verify-launch: process died; tail log:" >&2
    tail -n 80 "$LOG" >&2 || true
    exit 4
  fi
  code="$(curl -s -o /tmp/ynow-verify-launch-body.html -w '%{http_code}' "$URL" || true)"
  if [[ "$code" == "200" ]] && grep -q 'The YNow App' /tmp/ynow-verify-launch-body.html; then
    ready=1
    break
  fi
  sleep 2
done

if [[ "$ready" != "1" ]]; then
  echo "verify-launch: not ready after ${READY_TRIES} tries (last http=${code:-none})" >&2
  tail -n 80 "$LOG" >&2 || true
  kill "$PID" 2>/dev/null || true
  exit 5
fi

echo "verify-launch: ready"
echo "YNOW_VERIFY_RUN_DIR=${RUN_DIR}"
