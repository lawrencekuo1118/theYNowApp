#!/usr/bin/env bash
# Phase 2 — Data integration / normalization / cache audit (P1-C)
# Usage: bash scripts/run_phase2_audit.sh
# Env:   YNOW_DEBUG_SKIP_PY=1 (default), PHASE2_FINDINGS_JSON
#
# Exit 1 if any control FAIL (including proven normalize defects).
# Live Yahoo/TPEx HTTP is NOT executed — see report coverage matrix.

set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
APP="${ROOT}/app_24.0"
export YNOW_DEBUG_SKIP_PY="${YNOW_DEBUG_SKIP_PY:-1}"
export PHASE2_FINDINGS_JSON="${PHASE2_FINDINGS_JSON:-${ROOT}/docs/phase2_findings.json}"

if [[ ! -d "${APP}" ]]; then
  echo "ERROR: app_24.0 not found at ${APP}" >&2
  exit 2
fi
if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript not found on PATH" >&2
  exit 2
fi

echo "==> Phase 2 baseline lock"
if [[ -f "${ROOT}/scripts/DEPLOY_BASELINE.txt" ]]; then
  cat "${ROOT}/scripts/DEPLOY_BASELINE.txt"
else
  echo "WARN: DEPLOY_BASELINE.txt missing" >&2
fi
echo "    git HEAD: $(git -C "${ROOT}" rev-parse HEAD 2>/dev/null || echo unknown)"

mkdir -p "$(dirname "${PHASE2_FINDINGS_JSON}")"
echo "==> Phase 2 suite → ${APP}/tests/test_phase2_data_integration.R"
echo "    findings JSON → ${PHASE2_FINDINGS_JSON}"
(
  cd "${APP}"
  Rscript tests/test_phase2_data_integration.R
)
ec=$?
echo "==> Phase 2 exit=${ec}"
exit "${ec}"
