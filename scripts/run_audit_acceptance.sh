#!/usr/bin/env bash
# Audit acceptance evidence runner — beyond structural closeout.
# Usage: bash scripts/run_audit_acceptance.sh
# Env:   YNOW_LIVE_PROBE=1 (default 1 here) for live Rf attempt
#        YNOW_ACCEPTANCE_REQUIRE_LIVE_RF=1 to fail if live Rf is not obtained
#
# Captures exits via redirection in the caller; this script uses set -uo pipefail.

set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
APP="${ROOT}/app_24.0"
export YNOW_DEBUG_SKIP_PY="${YNOW_DEBUG_SKIP_PY:-1}"
LOG_DIR="${ROOT}/audit_logs"
mkdir -p "${LOG_DIR}"

echo "==> Acceptance baseline"
cat "${ROOT}/scripts/DEPLOY_BASELINE.txt" 2>/dev/null || true
echo "    HEAD=$(git -C "${ROOT}" rev-parse HEAD 2>/dev/null || echo unknown)"

ec=0

run_step() {
  local name="$1"
  shift
  echo "==> ${name}"
  local log="${LOG_DIR}/acceptance_${name}.log"
  set +e
  "$@" >"${log}" 2>&1
  local rc=$?
  set -e
  echo "    exit=${rc} log=${log}"
  if [[ "${rc}" -ne 0 ]]; then
    ec=1
    tail -30 "${log}" || true
  else
    # Show PASS/FAIL summary lines when present
    grep -E 'PASS=|FAIL=|ALL |NOTE:|LIVE_PROBE|PASS |FAIL ' "${log}" | tail -20 || true
  fi
  return 0
}

run_step pipefail_meta bash "${ROOT}/scripts/test_audit_closeout_pipefail.sh"
run_step runner_exit bash "${ROOT}/scripts/test_audit_closeout_runner_exit.sh"
run_step closeout_structural bash -c "cd '${APP}' && Rscript tests/test_audit_closeout.R"
run_step tab_visit_gates bash -c "cd '${APP}' && Rscript tests/test_audit_tab_visit_runtime.R"
run_step tab_visit_shiny bash -c "cd '${APP}' && Rscript tests/test_audit_tab_visit_shiny_runtime.R"
run_step rf_impact bash -c "cd '${APP}' && Rscript tests/test_audit_rf_impact.R"
run_step phase1 bash "${ROOT}/scripts/run_phase1_audit.sh"
run_step phase2 bash "${ROOT}/scripts/run_phase2_audit.sh"

# Prefer local venv for Python TW Rf (gitignored .ynow_venv)
VENV_PY="${APP}/.ynow_venv/bin/python"
if [[ -x "${VENV_PY}" && -z "${RETICULATE_PYTHON:-}" ]]; then
  export RETICULATE_PYTHON="${VENV_PY}"
  echo "INFO RETICULATE_PYTHON=${RETICULATE_PYTHON}"
fi

# Live Rf paths — US required; TW required when venv present (auto) unless overridden
export YNOW_LIVE_PROBE="${YNOW_LIVE_PROBE:-1}"
export YNOW_ACCEPTANCE_REQUIRE_LIVE_RF="${YNOW_ACCEPTANCE_REQUIRE_LIVE_RF:-1}"
export YNOW_ACCEPTANCE_REQUIRE_TW_LIVE="${YNOW_ACCEPTANCE_REQUIRE_TW_LIVE:-auto}"
echo "==> live_rf_paths (US required; TW=${YNOW_ACCEPTANCE_REQUIRE_TW_LIVE})"
run_step live_rf_paths bash -c "cd '${APP}' && YNOW_DEBUG_SKIP_PY=0 Rscript tests/test_audit_live_rf_paths.R"
# Keep legacy US-only suite as a focused quantmod regression
run_step live_rf_us bash -c "cd '${APP}' && YNOW_DEBUG_SKIP_PY=0 Rscript tests/test_audit_live_rf_us.R"

echo "==> live_rf_probe TW+US detail lines"
LIVE_LOG="${LOG_DIR}/acceptance_live_rf.log"
set +e
(
  cd "${APP}"
  unset YNOW_DEBUG_SKIP_PY || true
  export RETICULATE_PYTHON="${RETICULATE_PYTHON:-}"
  Rscript -e '
    Sys.setenv(YNOW_DEBUG_SKIP_PY = "0")
    if (nzchar(Sys.getenv("RETICULATE_PYTHON")) && requireNamespace("reticulate", quietly = TRUE)) {
      tryCatch(reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE), error = function(e) NULL)
    }
    if (!exists("%||%", mode = "function")) {
      `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
    }
    source("web_crawler.R", local = TRUE)
    tryCatch(memoise::forget(.cached_fetch_rf_live_us), error = function(e) NULL)
    tryCatch(memoise::forget(.cached_fetch_rf_live_tw), error = function(e) NULL)
    for (mkt in c("US", "TW")) {
      det <- tryCatch(cached_get_risk_free_rate_detail(mkt), error = function(e) e)
      if (inherits(det, "error")) {
        cat("LIVE_PROBE Rf ", mkt, ": FAIL ", conditionMessage(det), "\n", sep = "")
      } else {
        cat(
          "LIVE_PROBE Rf ", mkt, ": source=", as.character(det$source %||% "?")[1],
          " is_fallback=", as.character(det$is_fallback %||% NA)[1],
          " rf=", as.character(det$rf_pct %||% NA)[1], "\n",
          sep = ""
        )
      }
    }
  '
) >"${LIVE_LOG}" 2>&1
live_rc=$?
set -e
echo "    exit=${live_rc} log=${LIVE_LOG}"
cat "${LIVE_LOG}" || true

live_ok=0
if grep -Eq 'LIVE_PROBE Rf US: source=live' "${LIVE_LOG}" || \
   grep -Eq 'LIVE_PROBE Rf US: source=last_known' "${LIVE_LOG}"; then
  live_ok=1
  echo "INFO live/last_known US Rf path observed"
else
  echo "INFO US Rf live/last_known NOT observed (fallback or error)"
fi

if [[ "${YNOW_ACCEPTANCE_REQUIRE_LIVE_RF}" == "1" && "${live_ok}" -ne 1 ]]; then
  echo "ERROR: YNOW_ACCEPTANCE_REQUIRE_LIVE_RF=1 but live/last_known US Rf not obtained" >&2
  ec=1
fi
tw_ok=0
if grep -Eq 'LIVE_PROBE Rf TW: source=live' "${LIVE_LOG}" || \
   grep -Eq 'LIVE_PROBE Rf TW: source=last_known' "${LIVE_LOG}"; then
  tw_ok=1
  echo "INFO live/last_known TW Rf path observed"
fi
if [[ "${YNOW_ACCEPTANCE_REQUIRE_TW_LIVE}" == "1" && "${tw_ok}" -ne 1 ]]; then
  echo "ERROR: YNOW_ACCEPTANCE_REQUIRE_TW_LIVE=1 but TW live Rf not obtained" >&2
  ec=1
fi
if [[ "${YNOW_ACCEPTANCE_REQUIRE_TW_LIVE}" == "auto" && -x "${VENV_PY}" && "${tw_ok}" -ne 1 ]]; then
  echo "ERROR: venv present but TW live Rf not obtained" >&2
  ec=1
fi

echo "==> Acceptance exit=${ec}"
exit "${ec}"
