# Changelog

All notable **display-version** ships of the live line (`app_25.0/`) are recorded here.

Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Section headers are en-US; prose uses **American English** and **Taiwan Traditional Chinese** (台灣投資界用語) where helpful. Formal finance terms stay in English (WACC, FCFF, MOS, …).

**Canonical live baseline:** `scripts/DEPLOY_BASELINE.txt` (`display_version`, `bundleId`, commit).  
**GitHub Releases:** https://github.com/lawrencekuo1118/theYNowApp/releases  
After each deploy, prefer a Release tag matching `display_version` (see `scripts/record_github_release.sh`).

Older folder trees (`app_8.0`…`app_24.0`) were removed from git (C-DEAD-05); recover from history or prior Releases. This file backfills recent ships that can be evidenced from git log / baseline — it does **not** invent unshipped history.

## [Unreleased]

### Added

- Download Report UI: live remaining free PDF downloads (Asia/Taipei day) plus FYN points rules near the sidebar button; Snapshot CSV shows free-after-sign-in / Lite-free hints (no points gate on Lite).
- zh-TW：下載報告旁顯示今日剩餘免費次數與點數規則；Snapshot 下載標明登入後免費；Lite 維持完全免費。

### Changed

- CI/CD: live-line GitHub Actions (`Live-line CI`) run offline valuation + Phase 1 checks on `app_25.0/**`, `scripts/**`, and workflow paths (no vacuous skip for app PRs).
- Docs: root README ship/CI/Release pointers; `scripts/record_github_release.sh` for soft-fail Release publish after deploy.

## [v25.95] — 2026-10-21

### Added

- Signed-in sidebar tab reorder: **Customize / 自訂設定** on the date row enables long-press-drag of top-level menu items; **Done / 完成** persists order via `ynow_user_prefs` key `sidebar_tab_order` (PR #359).
- en-US / zh-TW: `sidebar_reorder_*` locale keys.
- zh-TW：登入後可自訂側邊欄分頁上下順序（先按自訂設定，長按拖移，再按完成儲存）。

### Changed

- Disabled mistaken page-pin long-press UX from v25.93 so menu long-press is edit-mode reorder only; stock recent-search chip pins unchanged. Prefs API for `pinned_sidebar_tabs` retained.
- Live baseline bundle `12718919` (`scripts/DEPLOY_BASELINE.txt`).

## [v25.94] — 2026-10-21

### Added

- Signed-in search history: persist ticker/company lookups for the last 24 hours via `ynow_user_prefs` key `recent_search_history_24h` (additive; does not share `pinned_recent_search` / stock-pin grid keys); reload into Recent Search chips + empty typeahead on login. Guests stay in-session only (PR #356).
- en-US / zh-TW: `recent_search_24h_hint`, `recent_search_24h_empty`.
- zh-TW：登入後記住近 24 小時搜尋紀錄；超過則自動清除。與釘選代號／頁面釘選分鍵儲存。

### Changed

- Live baseline bundle `12718890` (`scripts/DEPLOY_BASELINE.txt`).

## [v25.93] — 2026-10-21

### Added

- Sidebar page pins: up to 12 tabs in a 3-column tray; persist after login (PR #355).
- zh-TW：側邊欄頁面釘選（最多 12、三欄）；登入後跨工作階段保留。

### Changed

- Live baseline bundle `12718845` (`scripts/DEPLOY_BASELINE.txt`).

## [v25.92] — 2026-10-21

### Fixed

- Sign-in gate demos (`www/demos/<feature>.{mp4,webm}`): ~10s teasers now scroll tab-style UI (KPI / chart / table fixtures) instead of PIL text cards (PR #354).
- en-US: regenerate via `scripts/generate_gate_demo_videos.py` (Playwright scroll of `scripts/gate_demo_pages/`).
- zh-TW：登入閘 demo 改為實際下滑該分頁風格畫面，不再只有文字卡片。

### Changed

- Live baseline bundle `12718190` (`scripts/DEPLOY_BASELINE.txt`).

## [v25.91] — 2026-10-21

### Fixed

- Ticker typeahead: suggest list stays warm when leaving Home/Macro and returns on Company / Blue Chip (PR #352).
- `sc_ticker_suggest_ui` uses `suspendWhenHidden = FALSE`; client re-opens suggestions after tab switch when input has text.
- en-US: stock input recommendations resume after tab changes.
- zh-TW：股票輸入候選清單在切換分頁後仍可依輸入值跳出。

### Changed

- Live baseline bundle `12718171` (`scripts/DEPLOY_BASELINE.txt`).

## [v25.90] — 2026-10-21

### Fixed

- Full mode after sign-in: still-locked sidebar items (ACL / points-gated persist-B) keep guest-identical lock chrome (lock icon + light gray / `.ynow-sidebar-locked`) and are not hidden (PR #353).
- Font Awesome `.fas` display no longer overrides `.ynow-menu-lock` hide — unlock-after-login (A) tabs drop orphan lock glyphs cleanly.
- en-US: post-login still-locked nav matches guest locked style; A tabs unlock normally; Lite omit unchanged.
- zh-TW：登入後仍鎖定的側邊欄項目維持鎖頭圖示＋淺灰（與未登入鎖上相同），不會隱藏消失；僅登入解鎖項目正常解鎖。

### Changed

- Live baseline bundle `12718113` (`scripts/DEPLOY_BASELINE.txt`).

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
