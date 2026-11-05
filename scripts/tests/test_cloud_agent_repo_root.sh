#!/usr/bin/env bash
# Offline checks: Cloud Agent install/start must not resolve ROOT=/ under bash -lc.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

fail=0
pass() { printf 'PASS %s\n' "$*"; }
fail_msg() { printf 'FAIL %s\n' "$*"; fail=1; }

# 1) File execution finds the checkout even from another cwd.
out="$(
  cd /tmp
  YNOW_CLOUD_INSTALL_RESOLVE_ONLY=1 bash "$ROOT/scripts/cloud-agent-install.sh"
)"
if [[ "${out}" == "ROOT=${ROOT}" ]]; then
  pass "install file execution resolves repo root from /tmp"
else
  fail_msg "install file execution: got [${out}] want ROOT=${ROOT}"
fi

out="$(
  cd /tmp
  YNOW_CLOUD_START_RESOLVE_ONLY=1 bash "$ROOT/scripts/cloud-agent-start.sh"
)"
if [[ "${out}" == "ROOT=${ROOT}" ]]; then
  pass "start file execution resolves repo root from /tmp"
else
  fail_msg "start file execution: got [${out}] want ROOT=${ROOT}"
fi

# 2) Login-shell stdin (BASH_SOURCE unset, same as Cloud Agent install) with cwd=repo.
out="$(
  cd "$ROOT"
  YNOW_CLOUD_INSTALL_RESOLVE_ONLY=1 bash -l <"$ROOT/scripts/cloud-agent-install.sh"
)"
if [[ "${out}" == "ROOT=${ROOT}" ]]; then
  pass "install bash -l stdin from repo root"
else
  fail_msg "install bash -l stdin: got [${out}] want ROOT=${ROOT}"
fi

# 3) Missing marker + unset BASH_SOURCE must exit 1 (never print ROOT=/).
set +e
err="$(
  cd /
  YNOW_CLOUD_INSTALL_RESOLVE_ONLY=1 bash -l <"$ROOT/scripts/cloud-agent-install.sh" 2>&1
)"
rc=$?
set -e
if [[ "${rc}" -ne 0 ]] && ! grep -q '^ROOT=/' <<<"${err}"; then
  pass "install bash -l stdin from / exits without ROOT=/"
else
  fail_msg "install from / should fail without ROOT=/; rc=${rc} err=[${err}]"
fi

# 4) Document the historical bug shape (BASH_SOURCE unbound → ROOT=/ → //app_17.0).
hist="$(
  bash -lc '
set -euo pipefail
if [[ -f app_17.0/requirements.txt ]]; then
  ROOT="$(pwd)"
else
  ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
fi
echo "ROOT=${ROOT}"
echo "VENV=${ROOT}/app_17.0/.ynow_venv"
' 2>&1 || true
)"
if grep -q 'BASH_SOURCE\[0\]: unbound variable' <<<"${hist}" \
  && grep -q '^ROOT=/$' <<<"${hist}" \
  && grep -q '^VENV=//app_17.0/.ynow_venv$' <<<"${hist}"; then
  pass "historical BASH_SOURCE trap still reproduces ROOT=/ → //app_17.0"
else
  fail_msg "historical trap shape changed: [${hist}]"
fi

if [[ "${fail}" -ne 0 ]]; then
  echo "test_cloud_agent_repo_root: FAILED" >&2
  exit 1
fi
echo "test_cloud_agent_repo_root: ok"
