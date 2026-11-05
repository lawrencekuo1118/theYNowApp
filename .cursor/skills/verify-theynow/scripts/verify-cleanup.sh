#!/usr/bin/env bash
# Tear down only the Shiny PID this verify run started. Evidence dirs are left alone.
set -euo pipefail

RUN_DIR="${YNOW_VERIFY_RUN_DIR:-}"
KEEP="${YNOW_VERIFY_KEEP_RUN_DIR:-0}"

if [[ -z "$RUN_DIR" || ! -f "${RUN_DIR}/meta.env" ]]; then
  echo "verify-cleanup: nothing to do (no meta.env)" >&2
  exit 0
fi

# shellcheck disable=SC1090
source "${RUN_DIR}/meta.env"

if [[ -n "${PID:-}" ]] && kill -0 "$PID" 2>/dev/null; then
  echo "verify-cleanup: killing pid=${PID} (port=${PORT:-unknown})"
  kill "$PID" 2>/dev/null || true
  for _ in 1 2 3 4 5 6 7 8 9 10; do
    kill -0 "$PID" 2>/dev/null || break
    sleep 0.5
  done
  if kill -0 "$PID" 2>/dev/null; then
    echo "verify-cleanup: SIGKILL pid=${PID}"
    kill -9 "$PID" 2>/dev/null || true
  fi
else
  echo "verify-cleanup: pid ${PID:-unset} already gone"
fi

# Evidence paths — never delete:
#   /cursor/stores/self/media/verify-theynow/
#   /opt/cursor/artifacts/verify-theynow/
#   .cursor/skills/verify-theynow/artifacts/

if [[ "$KEEP" == "1" ]]; then
  echo "verify-cleanup: keeping run dir ${RUN_DIR}"
else
  rm -rf "$RUN_DIR"
  echo "verify-cleanup: removed scratch ${RUN_DIR}"
fi
