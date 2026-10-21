#!/usr/bin/env bash
# Phase 5 — ITGC audit runner (Phases 2–4) + Audit_RCM_Report.md
# Usage: bash scripts/run_audit.sh
# Env:   YNOW_DEBUG_SKIP_PY=1 (default), AUDIT_LOG_DIR, AUDIT_REPORT

set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
APP="${ROOT}/app_26.0"
LOG_DIR="${AUDIT_LOG_DIR:-${ROOT}/audit_logs}"
REPORT="${AUDIT_REPORT:-${ROOT}/Audit_RCM_Report.md}"
export YNOW_DEBUG_SKIP_PY="${YNOW_DEBUG_SKIP_PY:-1}"

mkdir -p "${LOG_DIR}"

if [[ ! -d "${APP}" ]]; then
  echo "ERROR: app_26.0 not found at ${APP}" >&2
  exit 2
fi
if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript not found on PATH" >&2
  exit 2
fi

run_suite() {
  local name="$1"
  local script="$2"
  local log="${LOG_DIR}/${name}.log"
  echo "==> Running ${name}: ${script}"
  (
    cd "${APP}"
    # Keep going so RCM can still be generated from partial results
    set +e
    Rscript "${script}" >"${log}" 2>&1
    ec=$?
    set -e
    exit "${ec}"
  )
  local ec=$?
  if [[ ${ec} -eq 0 ]]; then
    echo "    OK  ${name} (exit ${ec}) → ${log}"
  else
    echo "    FAIL ${name} (exit ${ec}) → ${log}" >&2
  fi
  return "${ec}"
}

ec_di=0
ec_pb=0
ec_fa=0

run_suite "test_data_integrity" "tests/test_data_integrity.R" || ec_di=$?
run_suite "test_parameters_boundaries" "tests/test_parameters_boundaries.R" || ec_pb=$?
run_suite "test_formula_accuracy" "tests/test_formula_accuracy.R" || ec_fa=$?

echo "==> Generating RCM report → ${REPORT}"
set +e
Rscript "${ROOT}/scripts/generate_audit_rcm.R" \
  --log-dir "${LOG_DIR}" \
  --out "${REPORT}"
rcm_ec=$?
set -e

echo "==> Audit complete"
echo "    data_integrity exit=${ec_di}"
echo "    parameters_boundaries exit=${ec_pb}"
echo "    formula_accuracy exit=${ec_fa}"
echo "    rcm_generator exit=${rcm_ec}"
echo "    report: ${REPORT}"

if [[ ${ec_di} -ne 0 || ${ec_pb} -ne 0 || ${ec_fa} -ne 0 || ${rcm_ec} -ne 0 ]]; then
  exit 1
fi
exit 0
