#!/usr/bin/env bash
# Offline checks for Mac post-release sync handoff (no Mac paths required).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

fail=0
pass() { printf 'PASS %s\n' "$*"; }
fail_msg() { printf 'FAIL %s\n' "$*"; fail=1; }

# 1) Soft-skip without Mac paths must write PENDING and exit 0.
export YNOW_CODING="$TMP/no-coding"
export YNOW_ONEDRIVE="$TMP/no-onedrive"
export YNOW_SKIP_MAC_SYNC_HANDOFF=0
# Avoid real workflow_dispatch during test.
bash "$ROOT/scripts/mac_post_release_sync_handoff.sh" --no-dispatch --reason unit_test >/tmp/ynow-handoff-out.txt
if grep -q '^status=pending' "$ROOT/scripts/PENDING_MAC_SYNC.env"; then
  pass "handoff writes status=pending"
else
  fail_msg "PENDING_MAC_SYNC.env missing status=pending"
fi
if grep -q '^reason=unit_test' "$ROOT/scripts/PENDING_MAC_SYNC.env"; then
  pass "handoff records reason"
else
  fail_msg "reason not recorded"
fi

# 2) post_release_local_sync soft-skip path exits 0 and keeps pending.
YNOW_SKIP_MAC_SYNC_HANDOFF=1 bash "$ROOT/scripts/post_release_local_sync.sh" >/tmp/ynow-prs-out.txt || fail_msg "post_release soft-skip should exit 0"
# Restore pending via handoff for clear test
bash "$ROOT/scripts/mac_post_release_sync_handoff.sh" --no-dispatch --reason unit_test >/dev/null

# 3) clear_pending marks synced
bash "$ROOT/scripts/clear_pending_mac_sync.sh" "v0.00-test" >/dev/null
if grep -q '^status=synced' "$ROOT/scripts/PENDING_MAC_SYNC.env"; then
  pass "clear marks synced"
else
  fail_msg "clear_pending did not mark synced"
fi

# 4) Restore catch-up pending for v27.75 tip (this branch ships the catch-up marker).
cat >"$ROOT/scripts/PENDING_MAC_SYNC.env" <<EOF
# Machine-readable handoff for Mac coding + OneDrive post-release sync.
# Written when Cloud tip ships soft-skip (no Mac paths). Cleared when
# scripts/post_release_local_sync.sh succeeds on a host with Mac paths.
# Mac launchd / self-hosted worker / GH self-hosted runner reads this.
status=pending
display_version=v27.75
baseline_commit=5a3d160f3ec5c22f82dcd8f67b0c42180c85d809
bundle_id=12770153
reason=cloud_soft_skip_catchup
requested_at=2026-11-02T16:57:00Z
requested_by=auto-mac-post-release-sync
note=Catch up v27.75 soft-skip; run bash scripts/post_release_local_sync.sh --force && bash scripts/check-od-sync.sh on Mac.
EOF
pass "restored v27.75 pending catch-up marker"

# 5) Workflow file present
if [[ -f "$ROOT/.github/workflows/post-release-mac-sync.yml" ]]; then
  pass "workflow present"
else
  fail_msg "missing post-release-mac-sync.yml"
fi

# 6) launchd assets present
if [[ -f "$ROOT/scripts/macos/com.theynow.post-release-sync.plist" \
   && -f "$ROOT/scripts/macos/install-post-release-sync-launchd.sh" \
   && -f "$ROOT/scripts/macos/run_auto_post_release_sync.sh" ]]; then
  pass "macos automation assets present"
else
  fail_msg "missing macos automation assets"
fi

if [[ "$fail" -ne 0 ]]; then
  echo "FAILED"
  exit 1
fi
echo "ALL PASS"
exit 0
