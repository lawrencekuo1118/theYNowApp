## [v26.58] — 2026-10-23

### Changed
- Plain-language rename for statement fill vs parameter auto-update UX (#428).

### Notes
- en-US: Mechanism A is **Fill from statements** (manual copy into module fields). Mechanism B is **Auto-update parameters when the ticker changes** (toggle on ticker load), with one-shot **Update parameters from current statements**. Dropped sync jargon for these paths only. Bundle `12728711`.
- zh-TW: 機制 A 統一為「從財報帶入」；機制 B 為「換股時自動更新參數」，單次為「依目前財報更新參數」。這兩條路徑不再用「同步」口吻。Bundle `12728711`。

## [v26.57] — 2026-10-23

### Fixed
- Macro + Model Dashboard: restore visible Value Alert bells on KPI / valueBox surfaces (#424).

### Notes
- en-US: Macro bells were black-on-dark ink; Model Dashboard bells painted under AdminLTE `.small-box` / `.info-box`. Fix appends the bell after wrapped UI, lightens Macro marks, and keeps guest dim readable. Bundle `12728655`.
- zh-TW: Macro KPI 鈴鐺改為淺色半透明；Model Dashboard 將鈴鐺畫在 valueBox／infoBox 之上，避免被 AdminLTE 蓋住。Guest 淡化仍可辨識。Bundle `12728655`。

## [v26.56] — 2026-10-23

### Fixed
- Settings (signed-in): restore **主色系 / Color theme** personalization picker (logo / mono / ocean / forest) on the Settings title row (#423).

### Notes
- en-US: Settings reused Account `ynow-maint-shell` chrome but omitted `ynow_maint_theme_select_ui()`; Account overview and Points ledger still had the control. Persistence unchanged (`localStorage` `ynow_maint_theme`). Bundle `12728551`.
- zh-TW: 登入後「設定」補回與帳號維護頁相同的主色系選項（Logo 藍綠／黑白／藍海／森林）；仍以本機 `localStorage` 套用至維護殼層。Bundle `12728551`。

## [v26.55] — 2026-10-23

### Fixed
- Sidebar circuit: clamp vertical extent so fade-to-black starts by the YNow brand logo; electric blue stroke travel/glow (not image scaleY scroll) (#422).
- Header wordmark: solid black title strip on collapsed-sidebar scroll; no AdminLTE white flash; no vein/pattern through the wordmark (#422).

### Notes
- en-US: Circuit overlay is bottom-anchored (`min(42vh, 380px)`); SVG stroke-dash travel + glow pulse. Title strip stays `#000` when the sidebar is collapsed and the page scrolls. Bundle `12728418`.
- zh-TW: 側邊欄線路最長到 YNow logo 即 fade 至全黑；動態為線路電光（非整圖上下移動）。收合側欄捲動時「The YNow App」標題區維持黑底、無圖樣透出。Bundle `12728418`。

## [v26.54] — 2026-10-22

### Added
- TW Business Breakdown: FinmoConf → latest MOPS 法說會 presentation PDF (zh preferred) as automatable Estimate source for status-only 訂單／供應鏈／今後發展 (#421).

### Notes
- en-US: FinmoConf company HTML only (no `/api/`); amounts only when stated; merges with Fugle; SA last-resort; AlphaMemo human outbound. Bundle `12726282`.
- zh-TW: FinmoConf 公司頁發現最新簡報 PDF（不呼叫 `/api/`）；狀態摘錄＋明列金額；與 Fugle 合併。Bundle `12726282`。

## [v26.53] — 2026-10-22

### Fixed
- Sidebar chip: longer SVG PCB circuit traces (natural-size foot PNG, no stretch) with logo blue↔white strokes and upward fade (#418).

## [v26.52] — 2026-10-22

### Fixed
- shinyapps.io start crash (`exit status 1`): unescaped `"` in `ynowMacroOpenIndex` `querySelector` inside `tags$script(HTML("…"))` broke R parse of `ynow_ui.R` (v26.51 / #419).

### Notes
- en-US: Live tip restored — parse + Listening; no formula changes.
- zh-TW: 修復線上啟動失敗（R 剖析錯誤）；無評價公式變更。

## [v26.47] — 2026-10-22

### Fixed
- Business Breakdown（業務拆解）empty sections collapse: Section IV analysis groups (`orders` / `customers` / `suppliers` / `markets` / `context`) and tall plotly shells no longer leave blank bordered panels when packs are ineligible or honest-empty.
- Chapter III revenue-mix donut mounts only when slices exist; BCG collapses when not eligible. Chapters V–VI and shared chrome hide when empty.

### Notes
- en-US: Empty groups collapse — never estimated; Start analysis toolbar stays. No new valuation formulas.
- zh-TW: 無資料的業務拆解區塊改為收合，不再留下空白面板；絕不推估。開始分析工具列仍保留。

## [v26.46] — 2026-10-22

### en-US
- **業務拆解 / order research:** TW preferred automatable path is now Fugle 法說會備忘錄 HTML (`estimate`). Fact extractors read stated backlog / CapEx / customer share in zh-TW when present. AlphaMemo free transcripts stay human-outbound only (no HTML/API scrape). Seeking Alpha HTML remains last-resort.
- **US free SEC path:** `.ynow_us_ec_from_sec_8k` now reads Python `short_names` / `excerpts` / `full_texts` so Item 2.02-style 8-K text can reach extractors without a paid transcript API key.

### zh-TW
- **業務拆解／訂單研究：** 台股可自動化偏好路徑改為富果直送法說會備忘錄 HTML（估計）。來源文字若明列在手訂單、資本支出、客戶營收占比，才寫入事實。AlphaMemo 公開逐字稿仍僅人工外連（不擷取 HTML／API）。Seeking Alpha HTML 維持最後備援。
- **美股免費 SEC 路徑：** 修正 8-K 附註平行欄位對應，使 Item 2.02 類內文可進入分析，無需付費逐字稿 API key。

## [v26.45] — 2026-10-22

### Changed
- Business Breakdown **Company orders & market concentration**: same analysis types are grouped together (order quantity → major-customer revenue mix → major suppliers → major markets → sources/cross-check context) instead of a scattered three-column filing grid plus a separate Order research block.
- Filing concentration charts sit with the matching disclosed order facts; empty when undisclosed.

### Notes
- en-US: Consolidated UX only — no new valuation formulas; Start analysis still gates disclosed order facts.
- zh-TW: 業務拆解「公司訂單與市場集中度」將相同分析整併同組（訂單數量／重大客戶營收貢獻／主要供應商／主要市場／來源與交叉驗證）；財報集中度與已揭露訂單事實並陳，未揭露留白。

## [v26.42] — 2026-10-22

### Fixed
- Header wordmark strip (`.logo` / `.ynow-app-title`) is **transparent** again so it blends with the black banner.
- Branch veins stay clear of the title text via a horizontal leaves mask (far-left gutter + after ~252px hole under the wordmark), including on hover.
- Trailing version digit remains unclipped (`overflow:visible` + shrink-wrap).

### Notes
- en-US: Single gold title on a transparent brand strip; organic veins decorate beside the wordmark, not through it.
- zh-TW: 標題列字樣底透明融入黑底橫幅；枝狀 veins 不穿過標題文字（含懸停）；版本號末位數字完整可見。

## [v26.40] — 2026-10-22

### Fixed
- Sidebar **Rearrange / 重新排列**: locale switch now updates the button (boot slim map keeps `sidebar_reorder_*`; boot merge no longer wipes full strings).
- Sidebar tab reorder after login: Shiny 1.14 arity-0 page-pin stub no longer aborts the reorder IIFE; `menuRoot()` targets `ul.sidebar-menu` (not the empty `#sidebar_tabs` DIV).

### Notes
- en-US: After sign-in, Rearrange enters edit mode (Done to save); long-press-drag reorders top-level tabs; order persists.
- zh-TW: 登入後按「重新排列」進入編輯（「完成」儲存）；長按拖移調整側邊欄分頁順序並持久化。

## [v26.39] — 2026-10-22

### Fixed
- Sidebar chip: blue circuit is **line strokes only** on black (de-backgrounded PNG); bottom-up glow **fades out upward** into the sidebar.
- Header wordmark hover: no branch/vein flash (`.logo` stays above foliage with opaque black).
- Header wordmark: trailing version digit no longer clipped (e.g. `8` in v26.38 / v26.39).

### Notes
- en-US: Circuit traces dissolve upward on the black sidebar chip; title stays plain black on hover with the full version string visible.
- zh-TW: 側邊欄晶片藍線路僅保留線條並向上淡出融入黑底；標題列懸停不閃枝狀背景；版本號末位數字完整可見。

## [v26.38] — 2026-10-22

### Fixed
- Header wordmark: only one 「The YNow App v…」 — removed #404 pill padding that offset the load-bar base/fill into a ghost duplicate; hide the base track when load is complete.
- Branch veins no longer paint the title block background — restore opaque black `.logo` brand strip; veins remain visible beside the strip (navbar side) only.

### Notes
- en-US: Single gold title on plain black brand chrome; organic vein strokes stay clear of the wordmark box.
- zh-TW: 標題列只保留一個「The YNow App v…」；枝狀 veins 不進入標題區塊黑底，僅在標題旁裝飾。

## [v26.37] — 2026-10-22

### Changed
- Chrome glow: line-stroke-only organic veins flush to the far-left of the black top banner; 「The YNow App」 on an opaque black wordmark pill (no fill under the text).
- Sidebar foot chip: black background with logo-blue circuit line traces only (no teal wash).

### Notes
- en-US / zh-TW: no new user-facing copy.

## [v26.36] — 2026-10-22

### Added
- Chrome atmosphere (A+B): organic-branch veins on black top banner left (logo-green glow) and circuit-trace sidebar foot chip (logo-blue glow).

### Notes
- en-US / zh-TW: no new user-facing copy.
- Follow-up: line-stroke-only veins (no filled color wash); header veins outside title text box.

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

## [v26.35] — 2026-10-22

### Added

- **Count / countdown badges** (PR #402): count-gated controls append `(remaining)/(total)` after the label (Download Report PDF freemium daily free quota; Class C ACL `free_count`). Time-limited Class C windows and active maintenance show a live mini countdown on a secondary/reserved line so the primary label is not covered or squeezed.
- Server-authoritative badges via `ynow_access_quota_badges` + PDF quota push; client tick; refresh after use / expiry.
- zh-TW：次數限制控件標籤後顯示「(剩餘／總次數)」；限時條件與維護視窗以迷你倒數顯示剩餘時間（勿遮住原標籤）。
- Live baseline bundle **12724087** / display **v26.35**.

## [v26.34] — 2026-10-22

### Changed

- **About Data sources** (PR #401): remove the long sidebar「資料來源／Data Source」blurb; add a dedicated About section **資料來源 / Data sources** (en-US + zh-TW via `ui_locale.R`) covering Yahoo/yfinance, Rf (^TNX / TPEx), TWSE·TPEx·MOPS, SEC EDGAR, and how Fair Value / screens use those feeds.
- zh-TW：側邊欄不再顯示長「資料來源」說明；改在關於分頁集中陳述現況來源與估值用法（台灣投資界用語）。
- Live baseline bundle **12724056** / display **v26.34**.

## [v26.33] — 2026-10-22

### Changed

- **Sidebar Rearrange label** (PR #400): after login, the date-row tab-order control reads **Rearrange** (en-US) / **重新排列** (zh-TW) instead of Customize / 自訂設定. Title + guest toast aligned.
- zh-TW：登入後側邊欄日期列「重新排列」；勿用陸系「自定义／自訂設定」行銷口吻。
- Live baseline bundle **12724005** / display **v26.33**.

## [v26.32] — 2026-10-22

### Added

- **System admin Audit log / 操作紀錄** (PR #399): Sys. Admin pill tab records every ops mutation (Item locks apply/remove, maintenance set/clear, view-as / self-view, points adjust, portal enter/exit). Newest first; append-only (no clear/delete UI); each row retained at most **24 hours** (server clock) then auto-purged. Supabase Postgres `ynow_admin_ops_audit` with file fallback; fail closed.
- zh-TW：系統管理「操作紀錄」分頁；累積列表（由新到舊）、介面不提供清除／刪除、逾 24 小時自動清除。
- Live baseline bundle **12723993** / display **v26.32**.

## [v26.31] — 2026-10-22

### Added

- **Banner right logo tab refresh** (PR #398): black top-banner right small logo click soft-refreshes the **current sidebar tab** only (lazy-host remount + optional ticker re-search; no full session reload). While that tab is loading/refreshing, the logo **slowly rotates clockwise** and stops when idle.
- zh-TW：黑色頂欄右側小 logo 點選後只重新整理**目前分頁**；載入／重新整理期間 logo 順時針慢轉，完成後停止。Title／aria：**Refresh current page**／**重新整理目前分頁**。
- Live baseline bundle **12723970** / display **v26.31**.

## [v26.30] — 2026-10-22

### Added

- **Maintenance item catalog completeness** (PR #397): Sys. Admin Maintenance **項目** lists the full shared product/service catalog with Item locks (~99 ids — tabs through fine numeric inputs / sections / reports). Entries with a configured window show **Expected:** / **預計時間:** start–end in the menu (active or scheduled).
- zh-TW：維護「項目」選單補齊產品／服務目錄；已設定時段者顯示「預計時間: 開始-結束」。
- Live baseline bundle **12723900** / display **v26.30**.

## [v26.29] — 2026-10-22

### Added

- **System admin self view modes** (PR #396): next to Switch view — **Guest mode**, **Signed-in user mode** (all-user ACL only), **Omniscient mode** (lift all locks/hides; header title warning red `#DC3545`). Quick apply / 快速套用 (en-US + zh-TW).
- zh-TW：系統管理自身視角（訪客／已登入／全知）；全知模式標題為警示紅。
- Live baseline bundle **12723867** / display **v26.29**.

## [v26.28] — 2026-10-22

### Changed

- **Account guest-hidden (no demo / no Sign-in promo)** (PR #395): `account` joins non-product hide-vs-show surfaces — removed `www/demos/account.*`, generator target, and Account 推廣登入文案 / promo page (guest path is css_hide only).
- zh-TW：帳號工作區對訪客為隱藏／顯示（不鎖、不做推廣登入頁），不再附示範影片。
- Live baseline bundle **12723808** / display **v26.28**.

## [v26.27] — 2026-10-22

### Changed

- **Gate demos product-only** (PR #394): Sign-in teasers ship only for product surfaces (`lab`, `checklist`, `alerts`, `account`, `pdf`, `atx`). Non-product / service ids (`snap_defaults`, `inner_lock`) are hide-vs-show only in product policy — no demo assets (and not Sign-in-promo targets); removed their `www/demos` files and generator targets.
- zh-TW：示範短片僅產品頁／功能；非產品服務項原則上只分隱藏與否、不鎖也不做推廣登入頁，故不再附示範影片。
- Live baseline bundle **12723771** / display **v26.27**.

## [v26.26] — 2026-10-22

### Changed

- **Gate demo videos from live App UI** (PR #393): Sign-in-required teasers under `www/demos/<feature>.{mp4,webm}` are ~10s Playwright scrolls of the running App (real chrome / tabs), not fixture HTML. Regenerate with `bash scripts/record_gate_demos.sh` (local Shiny + `YNOW_DEMO_CAPTURE=1`; never on shinyapps). Deleted `scripts/gate_demo_pages/`.
- zh-TW：需登入閘的示範影片改為本機實際 App 畫面錄製（約 10 秒），不再用假 HTML 頁；以 `record_gate_demos.sh` 重錄。
- Live baseline bundle **12723730** / display **v26.26**.

## [v26.25] — 2026-10-22

### Added

- **System admin Maintenance / 維護** (PR #390): time-windowed page/tab/element outage for **all users**; copy「維護中」+ newline「預計時間: start-end」(en-US: Under maintenance / Expected: …). Content unusable like a lock, but sidebar/menu is **non-clickable** (unlike Item locks / guest_menu_lock). When the configured **end** time is reached, restrictions **auto-lift** on the server clock (no admin click). Persist in Supabase `ynow_maintenance` (fail-closed); distinct from Checklist gate overlay.
- zh-TW：系統管理「維護」可對目錄項目設定時段；進行中顯示「維護中／預計時間」且選單不可點；結束時間到自動恢復。

### Changed

- Home Sign-in · Register · About on one row (PR #389) landed in this tip with #390/#391.
- **Black top-banner Sign in: no white outer border** (PR #392): remove border/outline/box-shadow on header auth controls; keep black banner, white label, and hover wash. Single-entry viewport rule unchanged (≤767 date-row text links; ≥768 banner).
- zh-TW：頂部黑列登入控制項去掉白框／外框，僅保留文字與 hover；窄／寬單一登入入口規則不變。
- Live baseline bundle **12723615** recorded in `scripts/DEPLOY_BASELINE.txt` after deploy.

## [v26.24] — 2026-10-22

### Changed

- **Sign-in gate overlay shell** (PR #391): guest `guest_menu_lock` and ACL whole-tab locks share the Checklist-style surface overlay (white-fade + **Sign in required** / **需登入帳號**). Decision Checklist keeps unlock + SOP demo **1:1 side-by-side**; Lab, Value Alerts, and Asset Transmission Root use the stacked shell; ACL `effect=lock` tabs use the same overlay. Lite omit set unchanged.
- zh-TW：訪客選單鎖與 ACL 整頁鎖定共用決策檢核式覆層（淡白底＋需登入帳號）；決策檢核維持解鎖區與 SOP 示範並排；Lab／警報／資產傳導根系為堆疊殼層。
- Live baseline bundle **12723569** recorded in `scripts/DEPLOY_BASELINE.txt` after deploy.

## [v26.23] — 2026-10-22

### Fixed

- **Single sign-in entry by viewport** (PR #388): at the **768px** breakpoint, only one auth chrome surface is visible. Narrow (`max-width: 767`): Sign in / Sign out as text links beside the sidebar date (12px). Wider (`min-width: 768`): black top-banner Sign in button; date-row auth hidden. Never both.
- zh-TW：窄螢幕僅側欄日期旁登入文字連結；寬螢幕僅頂部黑列按鈕；兩處不同時顯示。
- Root cause: later header `display:flex !important` beat mobile hide; date-row `display:none` lost to shared `.ynow-auth-links { inline-flex }` on desktop.
- Live baseline bundle recorded in `scripts/DEPLOY_BASELINE.txt` after deploy.

## [v26.22] — 2026-10-22

### Changed

- **Decision Checklist Sign-in gate** (PR #387): Unlock Decision Checklist / 解鎖決策檢核 sits **1:1 side-by-side** with Decision Checklist demo — Decision SOP flow / 決策檢核示範——Decision SOP 流程. Locked page fades to white under a surface **Sign in required** / **需登入帳號** overlay (HFV Section B/W; logo blue–green–white CTAs). Lab/alerts stay stacked; Lite omit unchanged.
- zh-TW：決策檢核訪客閘門改為解鎖區塊與 SOP 示範並排 1:1，鎖定頁面淡白為底、需登入帳號覆層覆蓋（非遠處空盒）。
- Live baseline bundle recorded in `scripts/DEPLOY_BASELINE.txt` after deploy.

## [v26.21] — 2026-10-22

### Added

- **Business Breakdown Reconciliation / 對帳 — commercial copy** (PR #386): investment-desk labels (Filed consolidated / Sum of businesses / Unassigned gap), match-vs-gap verdict, per-line gap notes with share of filed; en-US + zh-TW.
- zh-TW：對帳改為「合併財報已申報／各事業加總／尚未歸屬差額／差額佔比」，並以「對得上／尚有差額」結論說明數字怎麼讀。
- Also ships tip with **earnings-call source stack** from PR #385 (AlphaMemo TW outbound + Statementdog US; Seeking Alpha HTML last-resort) when deploying this tip.

### Changed

- Live baseline bundle recorded in `scripts/DEPLOY_BASELINE.txt` after deploy.

## [v26.19] — 2026-10-21

### Added

- **Account points top-up / 點數儲值** (live ship of PR #381): Redeem-code path on Account overview credits FYN or PYN via the existing points ledger (no payment processor). Shows FYN/PYN rules; en-US + zh-TW. Optional `YNOW_POINTS_REDEEM_SEED` for ops codes. Admin grant path unchanged.
- zh-TW：帳號總覽可兌換儲值碼入帳 FYN／PYN；無刷卡結帳；每帳戶每碼一次；可至點數流水確認。
- Does not regress mobile login/points, single-device session, or US earnings-call ingest.
- Live baseline bundle recorded in `scripts/DEPLOY_BASELINE.txt` after deploy.

## [v26.18] — 2026-10-21

### Added

- **US earnings-call ingest / 美股法說會擷取** (live ship of PR #380): BBLab order-research path for US tickers — licensed transcript API (env keys only) → SEC 8-K body → company IR link-out. Seeking Alpha is human outbound only (no SA HTML scrape). Honest empty when undisclosed; TW leave-as-is. Attaches `order_disclosures$earnings_call` + UI block (en-US + zh-TW).
- zh-TW：美股 BBLab 訂單研究可走法說／8-K 擷取路徑；Seeking Alpha 僅人工外連；未揭露時誠實空白；台股路徑不變。
- Does not regress mobile login/points or single-device session (redeployed from current master tip).
- Live baseline bundle recorded in `scripts/DEPLOY_BASELINE.txt` after deploy.

## [v26.17] — 2026-10-21

### Added

- **Single-device session / 單一裝置登入** (live ship of PR #379): One active resume token per user. New sign-in soft-revokes other `ynow_auth_sessions` rows in the same mint transaction. Open tabs poll and clear local chrome when taken over (without wiping the newer token). Same-browser peer tabs get a `storage` hint. en-US + zh-TW kick copy.
- zh-TW：每位使用者僅保留一組有效恢復權杖；其他裝置／分頁登入會撤銷舊權杖，並以通知告知本裝置已登出。明確「登出」與變更密碼仍結束工作階段；30 天恢復機制保留。
- Does not regress mobile login/points (redeployed tip from current master).
- Live baseline bundle recorded in `scripts/DEPLOY_BASELINE.txt` after deploy.

## [v26.16] — 2026-10-21

### Changed

- Mobile login/points tip redeploy to live (bundle `12721592`). Display version bump to v26.16. Single-device session was merged in git (#379) ahead of this tip; live HTML verification for that Feature completed on the following ship.
- zh-TW：行動版登入／點數線上 redeploy；單一裝置登入功能於下一版確認上線。

## [v26.15] — 2026-10-21

### Changed

- **Home load / 首屏效能**: Defer MathJax off the Home critical path — Basic Setup methodology no longer calls `withMathJax()` (which hoisted a blocking CDN script into `<head>`). MathJax loads async on demand for get_started / HFV / Lab. Gate demo videos use `preload=none` without autoplay and play only when visible (PR #378).
- zh-TW：首頁不再為 MathJax CDN 阻塞；評價方法論改為需要時才載入。會員閘示範影片不自動預抓，進入可視區才播放。
- Before (v26.14): HTML ~1,154,586 bytes; MathJax in document; 6× autoplay. After: MathJax off eager methodology path; 0 autoplay.
- Live baseline bundle recorded in `scripts/DEPLOY_BASELINE.txt` after deploy.

## [v26.14] — 2026-10-21

### Fixed

- **Lock first-paint / 鎖定首屏**: Full-mode guest locks (sidebar lock icon + light gray, member gates, Download Report) paint locked in the initial HTML. `ynow-auth-feature` is stamped on `<body>` when account login is enabled, so chrome no longer flashes unlocked while waiting for the Shiny `ynowAuthFeature` websocket message (PR #377).
- zh-TW：完整版訪客鎖定（側邊欄鎖圖示＋淺灰、會員閘、下載報告）在首屏 HTML 即顯示鎖定；登入後點數／ACL 仍鎖定項目維持鎖住樣式，不會被藏起。
- Live baseline bundle recorded in `scripts/DEPLOY_BASELINE.txt` after deploy.

## [v26.13] — 2026-10-21

### Changed

- **Last UI / 上次畫面**: After sign-in or resume, restore last ticker with the last sidebar tab for signed-in members; uid prefs path (PR #376).
- zh-TW：登入或恢復後一併還原上次 ticker 與側邊欄分頁。
- Live baseline bundle `12720900` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.12] — 2026-10-21

### Changed

- **Sign in required / 需登入帳號**: Member and Account gates use the same Section block chrome as Historical Fundamental Validation (`ynow-hfv-chapter`): white B/W panel, kicker + title head. CTAs use logo blue (Sign in) and green outline on white (Register) (PR #374).
- zh-TW：需登入閘改套 HFV 分頁 Section 區塊樣式（黑白面板）；按鈕為 logo 藍／綠／白。
- **Account / 帳戶**: After sign-in or browser resume, restore the member’s last sidebar tab and ticker (`last_ui_surface`); Sign out keeps prefs and only revokes the resume token. Sys. Admin is never restored for members (PR #375).
- zh-TW：登入或瀏覽器恢復後回到上次側邊欄分頁與 ticker；登出只撤銷恢復權杖、保留偏好。一般會員不會被帶回系統管理頁。
- Live baseline bundle `12720734` (`scripts/DEPLOY_BASELINE.txt`).

## [v26.11] — 2026-10-21

### Added

- **Account / 帳戶**: Signed-in members stay signed in after closing the browser or tab. A durable resume token (default 30 days) restores the session on the next visit. Sign out and password change revoke it (PR #372).
- zh-TW：登入後關閉瀏覽器或分頁不會強制登出；下次開啟會自動恢復（預設 30 天）。只有「登出」或變更密碼才會撤銷。
- Live baseline bundle `12720607` (`scripts/DEPLOY_BASELINE.txt`).

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
