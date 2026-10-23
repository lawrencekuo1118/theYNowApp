#!/usr/bin/env bash
# Sync coding clone + OneDrive mirror to GitHub tip of BRANCH.
# Source of truth: origin (GitHub). Live app line: app_27.0/
# Fast path: CODING clone. OneDrive gets key trees via ditto (no git fetch in OD).
# Never run git status / git diff / git push inside OneDrive.
set -euo pipefail

BRANCH="${YNOW_BRANCH:-master}"
CODING="${YNOW_CODING:-/Users/lawrencekuo/coding/theYNowApp}"
ONEDRIVE="${YNOW_ONEDRIVE:-/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp}"
FORCE="${1:-}"
STAMP_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/theynow"
STAMP_FILE="$STAMP_DIR/last-sync"
LOCK_FILE="$STAMP_DIR/sync.lock"
SHA_FILE_OD="$ONEDRIVE/.ynow-synced-sha"
MIN_SECONDS="${YNOW_SYNC_MIN_SECONDS:-90}"
LIVE_APP="${YNOW_LIVE_APP:-app_27.0}"

# Paths that matter for editing / running the live line (plus thin prior for OD convenience).
SYNC_PATHS=(
  "$LIVE_APP"
  "scripts"
  ".cursor/rules"
)

log() { printf '[ynow-sync] %s\n' "$*"; }
die() { printf '[ynow-sync] ERROR: %s\n' "$*" >&2; exit 1; }

with_alarm() {
  local secs="$1"; shift
  perl -e "alarm int($secs); exec @ARGV" "$@"
}

mirror_tree() {
  local rel="$1"
  local src="$CODING/$rel"
  local dst="$ONEDRIVE/$rel"
  if [[ ! -e "$src" ]]; then
    log "skip missing $rel"
    return 0
  fi
  mkdir -p "$(dirname "$dst")"
  with_alarm 180 ditto "$src" "$dst"
  log "mirrored $rel"
}

mkdir -p "$STAMP_DIR"

if [[ "${FORCE}" != "--force" && "${YNOW_SYNC_FORCE:-}" != "1" ]]; then
  if [[ -f "$STAMP_FILE" ]]; then
    last=$(cat "$STAMP_FILE" 2>/dev/null || echo 0)
    now=$(date +%s)
    if (( now - last < MIN_SECONDS )); then
      log "skip (synced $((now - last))s ago; use --force to override)"
      exit 0
    fi
  fi
fi

if [[ -f "$LOCK_FILE" ]]; then
  lock_age=$(( $(date +%s) - $(stat -f %m "$LOCK_FILE" 2>/dev/null || echo 0) ))
  if (( lock_age < 300 )); then
    log "another sync in progress (lock ${lock_age}s old); skip"
    exit 0
  fi
fi
echo $$ >"$LOCK_FILE"
trap 'rm -f "$LOCK_FILE"' EXIT

[[ -d "$CODING/.git" ]] || die "coding clone missing: $CODING"
[[ -d "$ONEDRIVE" ]] || die "OneDrive path missing: $ONEDRIVE"

if [[ "${YNOW_SYNC_MIRROR_ONLY:-}" == "1" ]]; then
  log "1/3 mirror-only (skip fetch/checkout; current coding HEAD)"
else
  log "1/3 fetch GitHub → coding ($BRANCH)"
  with_alarm 120 git -C "$CODING" fetch origin "$BRANCH"
  with_alarm 60 git -C "$CODING" checkout -q "$BRANCH"
  if ! with_alarm 60 git -C "$CODING" merge --ff-only "origin/$BRANCH"; then
    die "coding cannot ff-only to origin/$BRANCH. Resolve local divergence in $CODING."
  fi
fi
CODING_SHA=$(git -C "$CODING" rev-parse HEAD)
CODING_SHORT=$(git -C "$CODING" rev-parse --short HEAD)
if [[ "${YNOW_SYNC_MIRROR_ONLY:-}" == "1" ]]; then
  STATUS_BRANCH=$(git -C "$CODING" branch --show-current 2>/dev/null || echo "$BRANCH")
  [[ -n "$STATUS_BRANCH" ]] || STATUS_BRANCH="$BRANCH"
else
  STATUS_BRANCH="$BRANCH"
fi
log "coding @ $CODING_SHORT ($STATUS_BRANCH)"

set +e
with_alarm 30 git -C "$CODING" diff-index --quiet HEAD --
coding_rc=$?
set -e
if [[ "$coding_rc" -ne 0 ]]; then
  if [[ "$FORCE" == "--force" || "${YNOW_SYNC_FORCE:-}" == "1" ]]; then
    log "WARN: coding has local modifications; continuing due to --force"
  else
    die "coding has uncommitted changes. Commit/stash or pass --force."
  fi
fi

[[ -d "$CODING/$LIVE_APP" ]] || die "$LIVE_APP missing on $BRANCH @ $CODING_SHORT"

log "2/3 mirror key trees coding → OneDrive"
fail=0
for rel in "${SYNC_PATHS[@]}"; do
  if ! mirror_tree "$rel"; then
    log "WARN: failed mirroring $rel"
    fail=1
  fi
done
for f in README.md AGENTS.md; do
  if [[ -f "$CODING/$f" ]]; then
    with_alarm 60 cp -f "$CODING/$f" "$ONEDRIVE/$f" || log "WARN: cp $f failed"
  fi
done

log "3/3 write OneDrive sync markers + lightweight git refs"
mkdir -p "$ONEDRIVE/.git/objects/info" "$ONEDRIVE/.git/refs/heads" "$ONEDRIVE/.git/refs/remotes/origin"
printf '%s\n' "$CODING/.git/objects" >"$ONEDRIVE/.git/objects/info/alternates"
printf '%s\n' "$CODING_SHA" >"$ONEDRIVE/.git/refs/heads/$BRANCH"
printf '%s\n' "$CODING_SHA" >"$ONEDRIVE/.git/refs/remotes/origin/$BRANCH"
printf 'ref: refs/heads/%s\n' "$BRANCH" >"$ONEDRIVE/.git/HEAD"
if [[ ! -f "$ONEDRIVE/.git/config" ]]; then
  printf '%s\n' \
    '[core]' \
    '	repositoryformatversion = 0' \
    '	filemode = true' \
    '	bare = false' \
    '[remote "origin"]' \
    "	url = $(git -C "$CODING" remote get-url origin)" \
    '	fetch = +refs/heads/*:refs/remotes/origin/*' \
    >"$ONEDRIVE/.git/config"
fi

printf '%s\n' "$CODING_SHA" >"$SHA_FILE_OD"
BUNDLE=$(awk -F= '/^bundleId=/{print $2; exit}' "$CODING/scripts/DEPLOY_BASELINE.txt" 2>/dev/null || echo unknown)
printf 'branch=%s\nsha=%s\nshort=%s\napp=%s\nbundle_baseline=%s\nsynced_at=%s\n' \
  "$STATUS_BRANCH" "$CODING_SHA" "$CODING_SHORT" "$LIVE_APP" "$BUNDLE" "$(date -Iseconds)" \
  >"$ONEDRIVE/.ynow-sync-status.txt"

date +%s >"$STAMP_FILE"

if [[ "$fail" -ne 0 ]]; then
  die "partial mirror failure; coding is @ $CODING_SHORT but OneDrive may be incomplete"
fi

if ! cmp -s "$CODING/$LIVE_APP/app.R" "$ONEDRIVE/$LIVE_APP/app.R"; then
  die "$LIVE_APP/app.R mismatch after mirror"
fi

log "OK both @ $CODING_SHORT ($STATUS_BRANCH / $LIVE_APP) — GitHub-aligned"
printf '%s\n' "$CODING_SHA"
