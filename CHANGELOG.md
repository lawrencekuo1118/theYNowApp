# Changelog

All notable **display-version** ships of the live line (`app_25.0/`) are recorded here.

Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Section headers are en-US; prose uses **American English** and **Taiwan Traditional Chinese** (台灣投資界用語) where helpful. Formal finance terms stay in English (WACC, FCFF, MOS, …).

**Canonical live baseline:** `scripts/DEPLOY_BASELINE.txt` (`display_version`, `bundleId`, commit).  
**GitHub Releases:** https://github.com/lawrencekuo1118/theYNowApp/releases  
After each deploy, prefer a Release tag matching `display_version` (see `scripts/record_github_release.sh`).

Older folder trees (`app_8.0`…`app_24.0`) were removed from git (C-DEAD-05); recover from history or prior Releases. This file backfills recent ships that can be evidenced from git log / baseline — it does **not** invent unshipped history.

## [Unreleased]

### Changed

- CI/CD: live-line GitHub Actions (`Live-line CI`) run offline valuation + Phase 1 checks on `app_25.0/**`, `scripts/**`, and workflow paths (no vacuous skip for app PRs).
- Docs: root README ship/CI/Release pointers; `scripts/record_github_release.sh` for soft-fail Release publish after deploy.

## [v25.89] — 2026-10-21

### Fixed

- Macro & Market Trends (`macro_market` / 大盤): zh-TW first paint no longer flashes English; index KPI skeleton paints with lazy HTML while quotes / HTCDI load (PR #351).
- en-US: bake locale into Macro chrome; progressive index quotes; defer HTCDI past first paint.
- zh-TW：總體經濟與大盤趨勢首屏繁中文案與指數 KPI 骨架同步出現，避免長時間空白／英文殘影。

### Changed

- Live baseline bundle `12718055` (`scripts/DEPLOY_BASELINE.txt`).

## [v25.88] — 2026-10-20

### Changed

- Ship after Neon→Postgres account-store naming cleanup (PR #335 / C-STRUCT-01) and related hygiene merges on tip.
- Live baseline bundle `12716154` (`scripts/DEPLOY_BASELINE.txt`).

## [v25.87] — 2026-10-20

### Added

- Major-version must-click update reminder for signed-in users; patch ships still use flame click only (no force logout) — PR #324.
- Supabase `service_role` inject path for account delete / Auth purge — PR #327.

### Changed

- Membership lock A/B unify and delete-account auth follow-ups — PRs #325–#326.
- Baseline bundles `12715820` / `12715846`.

## [v25.86] — 2026-10-20

### Changed

- Intermediate redeploys while landing major-update reminder fixtures and related UI (git shows bump/record commits; see baseline history around bundles `12715728`–`12715787`).

## [v25.85] — 2026-10-20

### Added

- Member **Delete account** hard purge in Account workspace — PR #323.
- PYN point numerals use logo blue–green flow — PR #321.

### Changed

- Gate-demo video refresh (~10s teasers) — PR #322.
- Baseline bundle `12715045`.

## [v25.84] — 2026-10-20

### Changed

- PYN logo-flow / merge of delete-account line onto live tip; baseline bundle `12715019`.

## [v25.83] — 2026-10-20

### Changed

- Gate-demo refresh and PYN logo-flow ships on master tip; baseline bundles `12714947` / `12714968` / `12714993`.

## [v25.82] — 2026-10-20

### Fixed

- Post-login auth chrome: Sign out only; keep ACL-locked sidebar dimming — PR #320.

## [v25.81] — 2026-10-20

### Fixed

- Sysadmin view-as shows the target user’s real session UI — PR #319.

## [v25.80] — 2026-10-20

### Fixed

- After login show only Sign out; guest `display:!important` no longer leaves Sign in/Register visible; ACL locks stay dimmed.
- Lite omit Asset Transmission Root from sidebar (guest-locked / Full-only omit set) — PR #318.

### Notes

- Earlier `v25.xx` ships exist in git / shinyapps history; add entries here when re-documenting with evidence. Prefer updating this file in the version-bump commit or immediately after deploy, then run `bash scripts/record_github_release.sh <version>`.
