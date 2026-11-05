#!/usr/bin/env bash
# Read-only: is this verify instance worth driving?
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)"
RUN_DIR="${YNOW_VERIFY_RUN_DIR:-}"
ALLOW_SHARED="${YNOW_VERIFY_ALLOW_SHARED:-0}"

if [[ -z "$RUN_DIR" || ! -f "${RUN_DIR}/meta.env" ]]; then
  echo "verify-doctor: FAIL missing YNOW_VERIFY_RUN_DIR/meta.env" >&2
  exit 1
fi

# shellcheck disable=SC1090
source "${RUN_DIR}/meta.env"

fail() { echo "verify-doctor: FAIL $*" >&2; exit 1; }
ok() { echo "verify-doctor: OK $*"; }

[[ -n "${PID:-}" && -n "${PORT:-}" && -n "${URL:-}" ]] || fail "meta incomplete"

kill -0 "$PID" 2>/dev/null || fail "process pid=${PID} not running"

if [[ "$PORT" == "3838" && "$ALLOW_SHARED" != "1" ]]; then
  fail "port 3838 is the shared Cloud boot port — refuse double-drive (YNOW_VERIFY_ALLOW_SHARED=1 only if exclusive)"
fi

listener_pid=""
if command -v ss >/dev/null 2>&1; then
  # ss -ltnp may need privileges for process names; fall back to lsof /fuser.
  listener_pid="$(ss -ltnp "sport = :${PORT}" 2>/dev/null | sed -n 's/.*pid=\([0-9]*\).*/\1/p' | head -1 || true)"
fi
if [[ -z "$listener_pid" ]] && command -v lsof >/dev/null 2>&1; then
  listener_pid="$(lsof -t -iTCP:"${PORT}" -sTCP:LISTEN 2>/dev/null | head -1 || true)"
fi
if [[ -z "$listener_pid" ]] && command -v fuser >/dev/null 2>&1; then
  listener_pid="$(fuser "${PORT}/tcp" 2>/dev/null | tr -s ' ' '\n' | grep -E '^[0-9]+$' | head -1 || true)"
fi

if [[ -n "$listener_pid" && "$listener_pid" != "$PID" ]]; then
  # R wraps the listener; accept if listener is a child of PID.
  ppid="$(ps -o ppid= -p "$listener_pid" 2>/dev/null | tr -d ' ' || true)"
  if [[ "$ppid" != "$PID" && "$listener_pid" != "$PID" ]]; then
    # Also accept if meta PID is an ancestor of listener.
    cur="$listener_pid"
    found=0
    for _ in 1 2 3 4 5 6 7 8; do
      [[ "$cur" == "$PID" ]] && { found=1; break; }
      cur="$(ps -o ppid= -p "$cur" 2>/dev/null | tr -d ' ' || true)"
      [[ -z "$cur" || "$cur" == "1" ]] && break
    done
    [[ "$found" == "1" ]] || fail "port ${PORT} owned by pid=${listener_pid}, meta PID=${PID}"
  fi
fi

body="$(mktemp)"
# URL in meta.env already ends with /; do not append another slash.
BASE_URL="${URL%/}"
code="$(curl -s -o "$body" -w '%{http_code}' "${BASE_URL}/" || true)"
[[ "$code" == "200" ]] || fail "HTTP ${code} from ${BASE_URL}/"
grep -q 'The YNow App' "$body" || fail "body missing 'The YNow App'"

build_file="${ROOT}/app_27.0/www/ynow_build.json"
if [[ -f "$build_file" ]]; then
  expect="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["version"])' "$build_file")"
  if grep -q "data-ynow-build=\"${expect}\"" "$body" || grep -q "${expect}" "$body"; then
    ok "build ${expect}"
  else
    echo "verify-doctor: WARN build ${expect} not found in HTML (UI may still be usable)" >&2
  fi
fi

ok "pid=${PID} port=${PORT} url=${URL}"
echo "PID=${PID}"
echo "PORT=${PORT}"
echo "URL=${URL}"
echo "RUN_ID=${RUN_ID:-}"
rm -f "$body"
