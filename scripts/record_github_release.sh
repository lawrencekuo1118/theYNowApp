#!/usr/bin/env bash
# Create or update a GitHub Release for a display version (e.g. v25.88).
#
# Soft-fail: exits 0 when gh / token unavailable so deploy is never blocked.
#
# Usage:
#   bash scripts/record_github_release.sh [VERSION] [NOTES_FILE]
#   bash scripts/record_github_release.sh v25.89
#
# Env:
#   GH_TOKEN / GITHUB_TOKEN — optional; gh uses existing auth when present
#   YNOW_RELEASE_TARGET     — git ref to tag (default: HEAD)
#   YNOW_SKIP_GITHUB_RELEASE=1 — no-op success
#
# Notes source (first match):
#   1) NOTES_FILE argument
#   2) CHANGELOG.md section matching ## [VERSION]
#   3) Minimal stub pointing at CHANGELOG + DEPLOY_BASELINE

set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"

if [[ "${YNOW_SKIP_GITHUB_RELEASE:-}" == "1" ]]; then
  echo "record_github_release: skipped (YNOW_SKIP_GITHUB_RELEASE=1)"
  exit 0
fi

if ! command -v gh >/dev/null 2>&1; then
  echo "record_github_release: gh not found — skip (install GitHub CLI to publish Releases)"
  exit 0
fi

# Prefer explicit token; otherwise rely on gh auth status (local login / GITHUB_TOKEN).
if [[ -z "${GH_TOKEN:-}" && -z "${GITHUB_TOKEN:-}" ]]; then
  if ! gh auth status >/dev/null 2>&1; then
    echo "record_github_release: no GH_TOKEN/GITHUB_TOKEN and gh not logged in — skip"
    exit 0
  fi
fi

VERSION="${1:-}"
if [[ -z "${VERSION}" ]]; then
  if [[ -f scripts/DEPLOY_BASELINE.txt ]]; then
    VERSION="$(sed -n 's/^display_version=//p' scripts/DEPLOY_BASELINE.txt | head -1 | tr -d '[:space:]')"
  fi
fi
if [[ -z "${VERSION}" || ! "${VERSION}" =~ ^v[0-9]+(\.[0-9]{1,2})?$ ]]; then
  echo "record_github_release: invalid or missing version '${VERSION}' — skip" >&2
  exit 0
fi

NOTES_ARG="${2:-}"
NOTES_FILE="$(mktemp)"
cleanup() { rm -f "${NOTES_FILE}"; }
trap cleanup EXIT

extract_changelog_section() {
  local ver="$1"
  local cl="${ROOT}/CHANGELOG.md"
  [[ -f "${cl}" ]] || return 1
  # Keep a Changelog: ## [v25.88] — ...
  awk -v ver="${ver}" '
    BEGIN { keep=0 }
    $0 ~ "^## \\[" ver "\\]" { keep=1; print; next }
    keep && $0 ~ /^## / { exit }
    keep { print }
  ' "${cl}" | sed '/./,$!d'
}

if [[ -n "${NOTES_ARG}" && -f "${NOTES_ARG}" ]]; then
  cat "${NOTES_ARG}" > "${NOTES_FILE}"
else
  section="$(extract_changelog_section "${VERSION}" || true)"
  if [[ -n "${section}" ]]; then
    printf '%s\n' "${section}" > "${NOTES_FILE}"
  else
    cat > "${NOTES_FILE}" <<EOF
## ${VERSION}

Shipped via \`scripts/deploy_app_21.R\` (display version +0.01).

- See root [CHANGELOG.md](../blob/master/CHANGELOG.md) for entry guidance.
- Deploy baseline: \`scripts/DEPLOY_BASELINE.txt\` (bundleId / commit / URL).

請在 CHANGELOG 補上本版 en-US + zh-TW（台灣投資界用語） substantive notes；勿只留版號。
EOF
  fi
fi

TARGET="${YNOW_RELEASE_TARGET:-HEAD}"
TITLE="theYNowApp ${VERSION}"

if gh release view "${VERSION}" >/dev/null 2>&1; then
  echo "record_github_release: updating existing release ${VERSION}"
  if gh release edit "${VERSION}" --title "${TITLE}" --notes-file "${NOTES_FILE}"; then
    echo "record_github_release: updated ${VERSION}"
  else
    echo "record_github_release: edit failed — soft skip" >&2
  fi
  exit 0
fi

echo "record_github_release: creating ${VERSION} at ${TARGET}"
if gh release create "${VERSION}" \
  --title "${TITLE}" \
  --notes-file "${NOTES_FILE}" \
  --target "${TARGET}"; then
  echo "record_github_release: created ${VERSION}"
else
  echo "record_github_release: create failed — soft skip" >&2
fi
exit 0
