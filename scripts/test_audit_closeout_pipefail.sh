#!/usr/bin/env bash
# Regression controls for closeout runner failure propagation.
#
# 1) Source gate: production scripts/run_audit_closeout.sh must contain
#    `set -uo pipefail`. A duplicated local pipeline alone cannot detect
#    removal of that setting from the real runner.
# 2) Local pipeline proof: with pipefail enabled here, Rscript|tee still
#    surfaces a deliberate non-zero (sanity for the pipeline pattern).
#
# Exit 0 = controls passed; exit 1 = swallow risk / missing pipefail.
# Standalone-safe: does not invoke run_audit_closeout.sh (no recursion).

set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUNNER="${ROOT}/scripts/run_audit_closeout.sh"
LOG_DIR="${ROOT}/audit_logs"
mkdir -p "${LOG_DIR}"
PROOF_LOG="${LOG_DIR}/pipefail_proof.log"

if [[ ! -f "${RUNNER}" ]]; then
  echo "FAIL pipefail source gate: missing ${RUNNER}" >&2
  exit 1
fi

if ! grep -Eq '^[[:space:]]*set -uo pipefail[[:space:]]*$' "${RUNNER}"; then
  echo "FAIL pipefail source gate: ${RUNNER} lacks exact line 'set -uo pipefail'" >&2
  echo "    set-lines in runner:" >&2
  grep -nE '^[[:space:]]*set ' "${RUNNER}" >&2 || echo "    (none)" >&2
  exit 1
fi

# Negative: a runner that only has `set -u` must not satisfy the gate.
BAD_TMP="$(mktemp "${TMPDIR:-/tmp}/ynow-bad-runner.XXXXXX")"
printf '%s\n' '#!/usr/bin/env bash' 'set -u' 'false | tee /dev/null || true' >"${BAD_TMP}"
if grep -Eq '^[[:space:]]*set -uo pipefail[[:space:]]*$' "${BAD_TMP}"; then
  rm -f "${BAD_TMP}"
  echo "FAIL pipefail source gate logic: accepted fixture without pipefail" >&2
  exit 1
fi
rm -f "${BAD_TMP}"

echo "PASS pipefail source gate: runner enables set -uo pipefail"

ec=0
# Deliberate non-zero: Rscript fails; tee succeeds. Without pipefail this pipeline
# often returns 0 and would set ec=0 incorrectly.
(
  Rscript -e 'quit(status = 7)'
) 2>&1 | tee "${PROOF_LOG}" || ec=1

if [[ "${ec}" -eq 0 ]]; then
  echo "FAIL pipefail local proof: Rscript exit 7 was swallowed after tee (ec=0)" >&2
  exit 1
fi

echo "PASS pipefail local proof: deliberate Rscript failure reached harness (ec=${ec})"
exit 0
