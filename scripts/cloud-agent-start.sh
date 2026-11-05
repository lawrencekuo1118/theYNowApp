#!/usr/bin/env bash
# Boot-time Shiny dev server for Cloud Agents (live line app_27.0).
# Idempotent: exits 0 when port 3838 is already listening.
#
# Same root rules as scripts/cloud-agent-install.sh: prefer app_27.0 under
# $PWD / walk-up; use BASH_SOURCE only when this file is executed; never
# silently take ROOT=/.
set -euo pipefail

ynow_resolve_repo_root() {
  local d marker="app_27.0/app.R"
  if [[ -f "${marker}" ]]; then
    pwd -P
    return 0
  fi
  d="$(pwd -P)"
  while [[ "${d}" != "/" ]]; do
    if [[ -f "${d}/${marker}" ]]; then
      printf '%s\n' "${d}"
      return 0
    fi
    d="$(dirname "${d}")"
  done
  if [[ -n "${BASH_SOURCE[0]:-}" && -f "${BASH_SOURCE[0]}" ]]; then
    d="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
    if [[ -f "${d}/${marker}" ]]; then
      printf '%s\n' "${d}"
      return 0
    fi
  fi
  return 1
}

ROOT="$(ynow_resolve_repo_root)" || {
  echo "cloud-agent-start: cannot find repo root (need app_27.0/app.R under \$PWD or via this script path)" >&2
  exit 1
}
if [[ "${YNOW_CLOUD_START_RESOLVE_ONLY:-}" == "1" ]]; then
  echo "ROOT=${ROOT}"
  exit 0
fi
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

cd "${ROOT}/app_27.0"
exec Rscript -e "shiny::runApp(host='0.0.0.0', port=${PORT}, launch.browser=FALSE)"
