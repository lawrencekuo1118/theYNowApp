#!/usr/bin/env bash
# Phase 1 — Independent formula verification (P1-A + P1-B)
# Usage: bash scripts/run_phase1_audit.sh
# Env:   YNOW_DEBUG_SKIP_PY=1 (default), PHASE1_FINDINGS_JSON

set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
APP="${ROOT}/app_25.0"
export YNOW_DEBUG_SKIP_PY="${YNOW_DEBUG_SKIP_PY:-1}"
export PHASE1_FINDINGS_JSON="${PHASE1_FINDINGS_JSON:-${ROOT}/docs/phase1_findings.json}"

if [[ ! -d "${APP}" ]]; then
  echo "ERROR: app_25.0 not found at ${APP}" >&2
  exit 2
fi
if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript not found on PATH" >&2
  exit 2
fi

mkdir -p "$(dirname "${PHASE1_FINDINGS_JSON}")"
echo "==> Phase 1 formula independence → ${APP}/tests/test_phase1_formula_independence.R"
echo "    findings JSON → ${PHASE1_FINDINGS_JSON}"
(
  cd "${APP}"
  Rscript tests/test_phase1_formula_independence.R
)
ec=$?
echo "==> Phase 1 exit=${ec}"
exit "${ec}"
