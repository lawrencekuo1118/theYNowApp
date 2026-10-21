# Changelog

All notable **display-version** ships of the live line (`app_26.0/`) are recorded here.

Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Section headers are en-US; prose uses **American English** and **Taiwan Traditional Chinese** (台灣投資界用語) where helpful. Formal finance terms stay in English (WACC, FCFF, MOS, …).

**Canonical live baseline:** `scripts/DEPLOY_BASELINE.txt` (`display_version`, `bundleId`, commit).  
**GitHub Releases:** https://github.com/lawrencekuo1118/theYNowApp/releases  
After each deploy, prefer a Release tag matching `display_version` (see `scripts/record_github_release.sh`).

Older folder trees (`app_8.0`…`app_24.0`) were removed from git (C-DEAD-05); recover from history or prior Releases. This file backfills recent ships that can be evidenced from git log / baseline — it does **not** invent unshipped history.

## [Unreleased]

### Changed

- CI/CD: live-line GitHub Actions (`Live-line CI`) run offline valuation + Phase 1 checks on `app_26.0/**`, `scripts/**`, and workflow paths (no vacuous skip for app PRs).
- Docs: root README ship/CI/Release pointers; `scripts/record_github_release.sh` for soft-fail Release publish after deploy.

## [v26.10] — 2026-10-21

### Fixed

- **Company / 公司**: Last Price, Market Cap, and EPS (TTM) strip uses the same content column width as Company Overview (`--ynow-page-max`, 1200px centered) (PR #371).
- zh-TW：最新股價、Market Cap、EPS (TTM) 指標列寬度與「公司概況」區塊對齊。
- Live baseline bundle `12720553` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.09] — 2026-10-21

### Changed

- **Business Breakdown / 業務拆解**: Order research adds a Taiwan-first source stack (MOPS → company IR → statement cross-check). Each fact shows credibility. Broker and industry sources are estimate-only and cannot fill Backlog or Bookings. Statement proxies (合約負債／RPO／合約資產／存貨／應收帳款／營業現金流) stay cross-checks, not invented Backlog (PR #370).
- zh-TW：訂單研究新增台灣來源優先序（公開資訊觀測站 → 公司 IR → 財報交叉驗證）與可信度標示。券商與產業來源僅供估計，不填入 Backlog／Bookings。財報間接科目只做交叉驗證，不當成在手訂單。
- Live baseline bundle `12720374` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.08] — 2026-10-21

### Changed

- **Business Breakdown / 業務拆解**: Order research now leads with three disclosed facts — order quantity, major suppliers, and major-customer revenue mix. Earnings-call figures are labeled separately from filings. Quality, conversion, cash, and valuation stay in a collapsed secondary section (PR #369).
- zh-TW：訂單研究改以訂單數量、主要供應商、重大客戶營收貢獻為主。法說會數字與財報分開標示來源。其餘層收在次要區。
- Live baseline bundle `12720232` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.07] — 2026-10-21

### Added

- **Business Breakdown / 業務拆解**: Order research panel after concentration charts shows current disclosed status only (scale, quality, conversion, profitability, statement cash items, backlog coverage). KPI cards include Book-to-Bill and backlog coverage, plus an industry lens. Start analysis builds one `OrderResearchPack` for the current ticker. Missing backlog or bookings stays empty. No Bear/Base/Bull table, no EPS/FCFF extrapolation, and no assumed conversion rate (PR #368).
- zh-TW：業務拆解在集中度圖之後新增訂單研究，只顯示已揭露現況（Book-to-Bill、訂單營收覆蓋率、產業透鏡）。未揭露的 Backlog／Bookings 留白。不做 Bear／Base／Bull、EPS／FCFF 外推或假設轉換率。
- Live baseline bundle `12720126` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.06] — 2026-10-21

### Changed

- **Mobile only:** Sign in / Sign out restored beside the sidebar date as **text links** matching the date font size (12px); not buttons. Desktop keeps the header Sign-in control (PR #367).
- zh-TW：僅手機版將登入／登出改回側邊欄日期旁的文字連結（字級與日期一致）；桌面版維持頁首 Sign in。
- Live baseline bundle `12720021` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.05] — 2026-10-21

### Added

- **Business Breakdown / 業務拆解**: TW major customers / suppliers / markets now ingest from **MOPS shareholder annual reports** (Art. 18 附表十六; official `doc.twse.com.tw` PDFs via `pypdf`). US path remains SEC EDGAR concentration notes. Honest empty when undisclosed — no marketing scrapes (PR #366).
- zh-TW：台股主要客戶／供應商／地區別改由公開資訊觀測站股東會年報穩定來源擷取；查無揭露時維持誠實空狀態。

### Changed

- Live baseline bundle `12719937` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.04] — 2026-10-21

### Added

- **Business Breakdown / 業務拆解**: **Company orders & market concentration** block — major markets (filed geography / customer location), major customers, and major suppliers, charted with native plotly (PR #365).
- Prefer SEC 10-K segment / concentration footnotes via existing slow statement cache; honest empty states when undisclosed; TW major-customer/supplier parsers marked pending (no marketing-page estimates).
- zh-TW：業務拆解新增「公司訂單與市場集中度」— 主要市場分布、主要客戶、主要供應商來源；優先財報／附註，查無揭露時誠實空狀態、不推估。

### Changed

- Live baseline bundle `12719767` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.03] — 2026-10-21

### Added

- **Forgot password / 忘記密碼**: login modal requests Supabase Auth recovery email (`POST /auth/v1/recover`); anti-enumeration success copy; recovery return (`type=recovery`) opens set-new-password panel; syncs `ynow_users.password_hash` after `PUT /auth/v1/user` (PR #364).
- Signed-in Account overview **Change password / 變更密碼** (social-only accounts may set a first password without a current password).
- zh-TW：登入視窗可申請密碼重設信；重設連結返回後設定新密碼；帳號總覽可變更 Email 登入密碼。

### Changed

- Live baseline bundle `12719321` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.02] — 2026-10-21

### Added

- Settings **Gain / loss colors**（漲跌底色）: choose follow-market, green/red, red/green, blue/red, or red/blue; persisted in `user_settings` and applied via `--ynow-chg-*` / `data-ynow-gain-scheme` (Macro, HFV, ATX move colors, Blue Chip treemap) (PR #363).
- Account overview **Points top-up**（點數儲值）placeholder: title, short copy, disabled Coming soon CTA — no payment flow.
- zh-TW：設定可選漲跌底色（跟隨市場／綠紅／紅綠／藍紅／紅藍對調）；帳號總覽預留點數儲值區塊（即將開放，不會收費）。

### Changed

- Live baseline bundle `12719276` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.01] — 2026-10-21

### Added

- Sysadmin **Item locks**: configure **unlock condition** per locked item — **Unlock after login** / **Points unlock** (FYN amount) / **Conditional unlock** (free uses and/or time-limited window). Default scope = all users; selected-user overrides allowed (PR #362).
- Persist unlock rules + runtime unlock state in Supabase Postgres (`ynow_access_locks` / `ynow_access_unlock_state`) with fail-closed RLS; file fallback for offline tests.
- 「目前限制」 status lines include the unlock rule (en-US + zh-TW).
- zh-TW：系統管理員項目鎖定可設定解鎖條件（登入即解鎖／點數解鎖／條件解鎖的免費次數或限時）；預設套用所有使用者，亦可覆寫選定使用者。

### Changed

- Admin ACL formal classes: **A login** / **B points** / **C conditional** (membership freemium catalog remains A/B only).
- Live baseline bundle `12719128` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.00] — 2026-10-21

### Added

- Signed-in **Settings** sidebar tab (below About, above Account) with durable prefs: **quick-hide locked items**, preferred language, preferred market on sign-in, compact sidebar spacing (`ynow_user_prefs` key `user_settings`) (PR #360).
- White separator line under About before Settings / Account when signed in.
- en-US / zh-TW: `menu_settings`, `settings_*` (zh-TW：**快速隱藏鎖住項目**; avoid Mainland「一鍵」).
- zh-TW：登入後關於下方白線區隔帳號區；新增設定分頁，可快速隱藏仍鎖住的側邊欄項目並持久化。

### Changed

- Major live-line cut: folder **`app_25.0/` → `app_26.0/`**; display **v26.00** (`YNOW_VERSION_BUMP_MAJOR=1`).
- Live baseline bundle `12719094` (`scripts/DEPLOY_BASELINE.txt`).

## [v25.96] — 2026-10-21## [v25.97] — 2026-10-21

### Fixed

- Sysadmin Item locks: when **Apply to / 套用對象** is **Selected user / 選定使用者**, **Currently restricted / 目前限制** live-updates from the chosen **Select user id / 選擇使用者代號** (not stale first-row / not global-only) (PR #361).
- zh-TW：套用對象為選定使用者時，「目前限制」隨選擇使用者代號即時更新。
- Live baseline recorded in `scripts/DEPLOY_BASELINE.txt` after ship.

## [v25.96] — 2026-10-21

### Changed

- Sidebar **stock pins** from Recent Search: long-press tickers into a **4×3** grid (max **12**); refuse a 13th with toast; signed-in persistence via `pinned_recent_search` (PR #357).
- Remove page-pin tray/CSS/locale so tab long-press stays **customize + drag reorder** only (#359).
- en-US / zh-TW: `recent_search_max_toast`.
- zh-TW：最近搜尋長按釘選股票，最多 12 檔（4×3）；側邊欄分頁長按留給自訂排序。
- Live baseline bundle `12718955` (`scripts/DEPLOY_BASELINE.txt`).

## [v25.95] — 2026-10-21

### Added

- Download Report UI: live remaining free PDF downloads (Asia/Taipei day) plus FYN points rules near the sidebar button; Snapshot CSV shows free-after-sign-in / Lite-free hints (no points gate on Lite) (PR #358).
- zh-TW：下載報告旁顯示今日剩餘免費次數與點數規則；Snapshot 下載標明登入後免費；Lite 維持完全免費。
- Signed-in sidebar tab reorder: **Customize / 自訂設定** on the date row enables long-press-drag of top-level menu items; **Done / 完成** persists order via `ynow_user_prefs` key `sidebar_tab_order` (PR #359).
- en-US / zh-TW: `sidebar_reorder_*` locale keys.
- zh-TW：登入後可自訂側邊欄分頁上下順序（先按自訂設定，長按拖移，再按完成儲存）。

### Changed

- Disabled mistaken page-pin long-press UX from v25.93 so menu long-press is edit-mode reorder only; stock recent-search chip pins unchanged. Prefs API for `pinned_sidebar_tabs` retained.
- Live baseline bundle `12718935` (`scripts/DEPLOY_BASELINE.txt`) — supersedes interim #359-only bundle `12718919` after #358 merge deploy.

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
