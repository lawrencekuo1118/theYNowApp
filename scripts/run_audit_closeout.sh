#!/usr/bin/env bash
# Final audit closeout runner (structural + Phase 2 regression + optional live probe)
# Usage: bash scripts/run_audit_closeout.sh
# Env:   YNOW_LIVE_PROBE=1 to attempt one live Yahoo/Rf call (network)
#
# NOTE: set -uo pipefail is required so Rscript|tee pipelines do not swallow
# Rscript failures (pipeline status would otherwise be tee's exit 0).
# Regression: scripts/test_audit_closeout_pipefail.sh (source gate + local proof)
# Integration (standalone, do not nest here): scripts/test_audit_closeout_runner_exit.sh

set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
APP="${ROOT}/app_24.0"
export YNOW_DEBUG_SKIP_PY="${YNOW_DEBUG_SKIP_PY:-1}"
LOG_DIR="${ROOT}/audit_logs"
mkdir -p "${LOG_DIR}"

echo "==> Baseline"
cat "${ROOT}/scripts/DEPLOY_BASELINE.txt" 2>/dev/null || true
echo "    HEAD=$(git -C "${ROOT}" rev-parse HEAD 2>/dev/null || echo unknown)"

ec=0

echo "==> Meta: pipefail proof (deliberate non-zero through tee)"
if ! bash "${ROOT}/scripts/test_audit_closeout_pipefail.sh"; then
  echo "ERROR: closeout runner cannot reliably detect test failures" >&2
  exit 2
fi

echo "==> Closeout structural tests"
(
  cd "${APP}"
  Rscript tests/test_audit_closeout.R
) 2>&1 | tee "${LOG_DIR}/test_audit_closeout.log" || ec=1

echo "==> Phase 2 data integration (normalize regression)"
(
  cd "${ROOT}"
  bash scripts/run_phase2_audit.sh
) 2>&1 | tee "${LOG_DIR}/phase2_in_closeout.log" || ec=1

if [[ "${YNOW_LIVE_PROBE:-0}" == "1" ]]; then
  echo "==> Live probe (best-effort; failure does not alone fail closeout)"
  (
    cd "${APP}"
    unset YNOW_DEBUG_SKIP_PY || true
    Rscript -e '
      Sys.setenv(YNOW_DEBUG_SKIP_PY = "0")
      source("web_crawler.R", local = TRUE)
      det <- tryCatch(cached_get_risk_free_rate_detail("US"), error = function(e) e)
      if (inherits(det, "error")) {
        cat("LIVE_PROBE Rf: FAIL ", conditionMessage(det), "\n")
      } else {
        cat("LIVE_PROBE Rf: OK source=", det$source %||% "?",
            " is_fallback=", det$is_fallback %||% NA, " rf=", det$rf_pct %||% NA, "\n")
      }
    '
  ) 2>&1 | tee "${LOG_DIR}/live_probe.log" || true
else
  echo "==> Live probe skipped (set YNOW_LIVE_PROBE=1 to enable)"
fi

echo "==> Closeout exit=${ec}"
exit "${ec}"
