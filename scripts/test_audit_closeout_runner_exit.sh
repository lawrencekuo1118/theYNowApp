#!/usr/bin/env bash
# Integration: invoke the *actual* closeout runner with a failing Rscript shim
# for tests/test_audit_closeout.R, and prove the runner exit is non-zero.
#
# Standalone only — do NOT call this from run_audit_closeout.sh (avoids recursion
# with the runner's embedded meta proof).
#
# Exit capture uses redirection (not `| tee`) so this harness cannot swallow the
# runner status even if the parent shell lacks pipefail.
#
# Exit 0 = runner correctly failed; exit 1 = failure was swallowed / unexpected.

set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUNNER="${ROOT}/scripts/run_audit_closeout.sh"
LOG_DIR="${ROOT}/audit_logs"
mkdir -p "${LOG_DIR}"
OUT_LOG="${LOG_DIR}/closeout_runner_exit_integration.log"
ART_LOG=""
if [[ -d /opt/cursor/artifacts ]]; then
  ART_LOG="/opt/cursor/artifacts/audit_closeout_runner_exit_integration.log"
fi

if [[ ! -f "${RUNNER}" ]]; then
  echo "FAIL runner-exit integration: missing ${RUNNER}" >&2
  exit 1
fi

REAL_RSCRIPT="$(command -v Rscript || true)"
if [[ -z "${REAL_RSCRIPT}" ]]; then
  echo "FAIL runner-exit integration: Rscript not on PATH" >&2
  exit 1
fi

TMP="$(mktemp -d "${TMPDIR:-/tmp}/ynow-closeout-shim.XXXXXX")"
cleanup() { rm -rf "${TMP}"; }
trap cleanup EXIT

# Shim: fail only the closeout structural suite; pass everything else through
# (meta proof uses `Rscript -e '...'`; Phase 2 uses a different test file).
cat > "${TMP}/Rscript" <<EOF
#!/usr/bin/env bash
set -u
for arg in "\$@"; do
  case "\$arg" in
    *test_audit_closeout.R*)
      echo "SHIM: deliberate fail for test_audit_closeout.R" >&2
      exit 7
      ;;
  esac
done
exec "${REAL_RSCRIPT}" "\$@"
EOF
chmod +x "${TMP}/Rscript"

export PATH="${TMP}:${PATH}"
export YNOW_DEBUG_SKIP_PY="${YNOW_DEBUG_SKIP_PY:-1}"
export YNOW_LIVE_PROBE=0

set +e
bash "${RUNNER}" >"${OUT_LOG}" 2>&1
rc=$?
set -e

if [[ -n "${ART_LOG}" ]]; then
  cp -f "${OUT_LOG}" "${ART_LOG}"
fi

echo "INFO runner-exit integration: captured exit=${rc}"
echo "INFO log=${OUT_LOG}"

if [[ "${rc}" -eq 0 ]]; then
  echo "FAIL runner-exit integration: actual closeout runner exited 0 despite failing structural Rscript (status may have been swallowed by tee)" >&2
  tail -60 "${OUT_LOG}" >&2 || true
  exit 1
fi

if ! grep -q "SHIM: deliberate fail for test_audit_closeout.R" "${OUT_LOG}"; then
  echo "FAIL runner-exit integration: shim failure marker missing from runner log (PATH shim may not have applied)" >&2
  tail -60 "${OUT_LOG}" >&2 || true
  exit 1
fi

if ! grep -Eq 'Closeout exit=[1-9][0-9]*' "${OUT_LOG}"; then
  echo "FAIL runner-exit integration: runner log lacks non-zero Closeout exit=N" >&2
  tail -60 "${OUT_LOG}" >&2 || true
  exit 1
fi

echo "PASS runner-exit integration: actual closeout runner exit=${rc} with failing structural Rscript (not swallowed)"
exit 0
