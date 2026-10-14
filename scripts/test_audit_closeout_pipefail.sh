#!/usr/bin/env bash
# Meta-control: prove that a failing Rscript after | tee still fails the runner.
# Mirrors scripts/run_audit_closeout.sh pipeline style with set -uo pipefail.
# Exit 0 = failure propagation works; exit 1 = swallow bug still present.

set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOG_DIR="${ROOT}/audit_logs"
mkdir -p "${LOG_DIR}"
PROOF_LOG="${LOG_DIR}/pipefail_proof.log"

ec=0
# Deliberate non-zero: Rscript fails; tee succeeds. Without pipefail this pipeline
# often returns 0 and would set ec=0 incorrectly.
(
  Rscript -e 'quit(status = 7)'
) 2>&1 | tee "${PROOF_LOG}" || ec=1

if [[ "${ec}" -eq 0 ]]; then
  echo "FAIL pipefail proof: Rscript exit 7 was swallowed after tee (ec=0)" >&2
  exit 1
fi

echo "PASS pipefail proof: deliberate Rscript failure reached runner (ec=${ec})"
exit 0
