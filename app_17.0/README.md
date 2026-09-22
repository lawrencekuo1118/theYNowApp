# The YNow App v17.87 — Valuation Methodology

先分類，再選模型；先推導，再校正；先給區間，再給單點；先做基本面估值。

## v17.87 重點
- **YNOW 決策漏斗**：改為報告式版面（結論分數卡 → F-Score 檢核 → 財報警訊 → 收合附錄），與 HFV 相同閱讀動線
- **目錄**：`app_17.0/`；顯示版號 **v17.87**

## v17.86 重點
- **歷史基本面驗證**：第一章「合理價與市價」圖表移至條件工具列之上（先看圖，再調條件）
- **目錄**：`app_17.0/`；顯示版號 **v17.86**

## v17.85 重點
- **歷史基本面驗證**：改為報告式版面（條件工具列、章節發現、收合附錄）；響應式 chip 控制列
- **目錄**：`app_17.0/`；顯示版號 **v17.85**

## v17.84 重點
- **Lite 資料來源列**：僅保留「快照」與「意見區」（隱藏「測試」）；快照頁僅「系統預設參數」，並過濾與簡化版無關的 APP_DEFAULTS
- **智慧分析**：移除 Previous Close／Market Cap／EPS (TTM) 三個 header KPI（Dashboard 完整版仍保留）
- **目錄**：`app_17.0/`；顯示版號 **v17.84**

## v17.78 重點
- **分群宇宙 N**：依所選「宇宙檔數（N）」分析（含自訂／全部）；修正 Inf 二次 clamp 誤回預設 25
- **目錄**：`app_17.0/`；顯示版號 **v17.78**

## v17.77 重點
- **標題載入條**：`The YNow App v17.xx` 字樣本身作為載入進度條，隨開頁／Shiny busy／`withProgress` 填滿金色
- **目錄**：`app_17.0/`；顯示版號 **v17.77**

## v17.76 重點
- **Blue Chip Ranking 色系**：分頁 box／tab 頂條改用 logo 藍 `#0C5484`（`--ynow-logo-blue`）
- **目錄**：`app_17.0/`；顯示版號 **v17.76**

## v17.75 重點
- **宇宙檔數（N）語意**：N＝分析後明細／排行最終顯示上限（非 Yahoo 撈取檔數）；評估用較大 `eval_n`；合格不足時不湊滿
- **目錄**：`app_17.0/`；顯示版號 **v17.75**

## v17.74 重點
- **Blue Chip 排行**：移除「排行產業／Ranking industry」下拉；依產業前十名一律列出各產業 Top 10
- **目錄**：`app_17.0/`；顯示版號 **v17.74**

## v17.73 重點
- **ADR 產業歸屬**：美股 ADR／外國發行人（TSM、SKHY、BABA、ASML…）載入宇宙後覆寫產業，不再落「未對應產業」
- **含 ADR 篩選**：Blue Chip 新增預設勾選「含 ADR」；取消則排除 ADR 後再套用候選截斷與宇宙檔數 N
- **搜尋績優股語意**：產業×模型 →（可選）排除 ADR → 候選截斷（市值／概念股等）→ 取前 N 檔再評估
- **目錄**：`app_17.0/`；顯示版號 **v17.73**

## v17.72 重點
- **宇宙指標快照**：美股／台股離線補齊 `market_cap`（市值）、`ret_1y`（近一年漲跌幅）；台股另含 MOPS **實收資本額**（`paid_in_capital`）。Blue Chip／Clustering 截斷優先讀快照，缺口再打 Yahoo
- **目錄**：`app_17.0/`；顯示版號 **v17.72**

## v17.71 重點
- **Blue Chip 評估池語意**：宇宙池先依「候選截斷邏輯」全市排序／篩選（市值／概念股等），再取「宇宙檔數（N）」前 N 檔；市值模式在已有市值時不再做 S&P 預篩打亂排序
- **目錄**：`app_17.0/`；顯示版號 **v17.71**

## v17.70 重點
- **Blue Chip**：選「概念股」時，共用控制順序為「概念股群 → 候選截斷邏輯 → 宇宙檔數（N）」（概念股群最優先）
- **目錄**：`app_17.0/`；顯示版號 **v17.70**

## v17.69 重點
- **Blue Chip**：共用控制改為先「候選截斷邏輯」、再「宇宙檔數（N）」；評估池語意同步（先排序／篩選，再取 N）
- **目錄**：`app_17.0/`；顯示版號 **v17.69**

## v17.68 重點
- **產業標準快覽（手機）**：修正 picker 與產業名／KPI 之間過大空白；`pickerInput` 下拉改 `container=body`，並收斂 closed menu 佔高
- **目錄**：`app_17.0/`；顯示版號 **v17.68**

## v17.67 重點
- **手機頁首**：黑色置頂橫幅置中顯示 App 版號標題；不覆蓋漢堡／美股台股／繁中 EN／logo；過長則等比例縮小字級（不以省略號裁切）
- **目錄**：`app_17.0/`；顯示版號 **v17.67**

## v17.66 重點
- **Lifecycle SGR：** 修正自動分檔優先序（高成長科技改走 growth_to_mature，不再誤標成熟科技）；檔位標籤對齊客觀依據；估計法建議 UI 顯示自動檔位與客觀證據（en-US + zh-TW）
- **目錄**：`app_17.0/`；顯示版號 **v17.66**

## v17.65 重點
- **Decision Funnel：** 移除「Smart Decision Matrix — The Decision Funnel」外層黑色框／標題；內層 valueBoxes、判決、F-Score checklist、動能面板不變
- **目錄**：`app_17.0/`；顯示版號 **v17.65**

## v17.64 重點
- **i18n：** CAPM／Get Started（Rf／Rm／β／Rolling／同業去槓桿）detail labels、Decision Funnel 判決／動能文案、高流量 `showNotification` 全面進 `ui_locale.R`（en-US + zh-TW）並經 locale helpers 切換
- **目錄**：`app_17.0/`；顯示版號 **v17.64**

## v17.63 重點
- **Clustering**：分群分析宇宙與結果表遵循共用候選截斷邏輯；Radar focus／Search 代號置頂，其餘依截斷欄位（市值／近一年漲幅等）排序
- **目錄**：`app_17.0/`；顯示版號 **v17.63**

## v17.62 重點
- **Blue Chip「建議評價方法」**：摘要表列順序改為與側欄估值選單一致（上→下：NAV → DCF → DDM → RI → P/B）
- **目錄**：`app_17.0/`；顯示版號 **v17.62**

## v17.61 重點
- **i18n**：補齊 Snapshot／HFV／市場切換／param audit 報告等 en-US＋zh-TW；Defaults 下載與還原檔選擇鈕隨語言切換；修正「基本設定」PDF 頁籤譯文
- **目錄**：`app_17.0/`；顯示版號 **v17.61**

## v17.60 重點
- **Snapshot**：移除頁籤區塊右側無關 chrome 標題（曾誤顯 SUSTAINABLE GROWTH RATE）；locale 標題改為精確比對
- **目錄**：`app_17.0/`；顯示版號 **v17.60**

## v17.59 重點
- **Snapshot 參數還原**：於「目前 App 參數」可下載還原 CSV，並上傳以寫回估值輸入（DCF／DDM／RI／P/B／NAV 等），方便接續分析
- **目錄**：`app_17.0/`；顯示版號 **v17.59**

## v17.58 重點
- **美股宇宙**：Search／Blue Chip 改用 SEC 主要上市全市場目錄（Nasdaq／NYSE；排除 OTC）；評估仍以 N＋截斷，過大時先預篩；S&P GICS 疊加產業鍵
- **目錄**：`app_17.0/`；顯示版號 **v17.58**

## v17.57 重點
- **DCF Overview**：移除「圖表顯示模式」選項；固定顯示各年折現現金流（PV，不含終值）
- **目錄**：`app_17.0/`；顯示版號 **v17.57**

## v17.56 重點
- **Snapshot**：拆成三個小頁籤——手改參數（相對 Search 後基準）、目前 App 參數、系統預設參數（APP_DEFAULTS）
- **目錄**：`app_17.0/`；顯示版號 **v17.56**

## v17.55 重點
- **Blue Chip**：移除區塊上方 Universe size（N）／候選截斷說明長文
- **目錄**：`app_17.0/`；顯示版號 **v17.55**

## v17.54 重點
- **Blue Chip 版面**：將「宇宙檔數（N）」與「候選截斷邏輯」整併並移至 BLUE CHIP 區塊外正上方，供排行／明細／分群共用
- **目錄**：`app_17.0/`；顯示版號 **v17.54**

## v17.53 重點
- **Blue Chip Search**：恢復「候選截斷邏輯」於明細頁；搜尋績優股依評估檔數 N＋截斷規則取評估池，再於排行頁列出合格 Top 10（與分群截斷同步）
- **目錄**：`app_17.0/`；顯示版號 **v17.53**

## v17.52 重點
- **評價方法論**：移除 About「Valuation Methodology｜評價方法論」下的 Process & Guardrails 小頁籤
- **目錄**：`app_17.0/`；顯示版號 **v17.52**

## v17.51 重點
- **Snapshot 參數 PDF（2A）**：勾選主要估值頁 → 擷取目前版面、框選手改參數 → 下載標記 PDF
- **目錄**：`app_17.0/`；顯示版號 **v17.51**

## v17.50 重點
- **Macro Rf 顯示**：即時爬取值一律顯示小數兩位（如 live ~4.998% → Rf=5.00%，不再誤以為整數 fallback 5%）
- **目錄**：`app_17.0/`；顯示版號 **v17.50**

## v17.49 重點
- **清殘留 debug**：移除 `lab_clustering.R`／`deep_scraper.py` 先前 session 的 NDJSON agent log
- **目錄**：`app_17.0/`；顯示版號 **v17.49**

## v17.48 重點
- **參數更動報告（1A+2B）**：Snapshot 相對 Search 後基準列出手動覆寫；前往並框選輸入框（結構化報告，非截圖）
- **手機頁首**：黑色橫幅不再被標題層蓋住漢堡／繁中·EN／小 logo
- **目錄**：`app_17.0/`；顯示版號 **v17.48**

## v17.47 重點
- **Clustering × Data-limited**：分群結果表加「資料覆蓋」欄；Search 對齊財報屬性 `fallback`，其餘依比率特徵稀疏（`<2`）；狀態短註揭露中位數補值
- **目錄**：`app_17.0/`；顯示版號 **v17.47**

## v17.46 重點
- **Blue Chip 明細**：移除「候選截斷邏輯」篩選；評估池固定依市值取 N（分群頁仍可自選截斷）
- **目錄**：`app_17.0/`；顯示版號 **v17.46**

## v17.45 重點
- **手機版頁首**：標題恢復較大字級並視窗置中；右側「繁中／EN」微幅縮小以配合寬度
- **目錄**：`app_17.0/`；顯示版號 **v17.45**

## v17.44 重點
- **NAV 色系**：改為與 SGR 框格相同的粉紅色（AdminLTE maroon `#d81b60`）
- **目錄**：`app_17.0/`；顯示版號 **v17.44**

## v17.43 重點
- **手機版產業快覽**：收緊標題／產業名／兩欄 KPI 框格垂直間距，排版更自然
- **目錄**：`app_17.0/`；顯示版號 **v17.43**

## v17.42 重點
- **Blue Chip 前十名**：釐清 N＝明細列數≠保證 10 列；接上「Piotroski 高門檻」勾選；狀態列顯示合格／已評估
- **目錄**：`app_17.0/`；顯示版號 **v17.42**

## v17.41 重點
- **估值模型頁色系**：NAV／DCF／DDM／RI／P/B 各頁 box／分頁頂條對齊 Model Selector 色（試算綠、回復預設灰不變）
- **目錄**：`app_17.0/`；顯示版號 **v17.41**

## v17.40 重點
- **財報屬性標籤**：High growth 等一律黑底金框金字、無圓點（不再僅 Data-limited 特例）
- **目錄**：`app_17.0/`；顯示版號 **v17.40**

## v17.39 重點
- **Composite valuation**：P/B／NAV 標記改為僅在本工作階段試算（或推薦主模型自動試算）後顯示；Search 後不再因財報同步就預設出現
- **目錄**：`app_17.0/`；顯示版號 **v17.39**

## v17.38 重點
- **Beta Overview**：回復單欄 β 來源選項與標題「β 來源（預設寫入 CAPM）」（撤銷兩欄並排）
- **目錄**：`app_17.0/`；顯示版號 **v17.38**

## v17.37 重點
- **Cluster map**：Radar focus ticker 圓點改以紅色（★）疊加標示
- **目錄**：`app_17.0/`；顯示版號 **v17.37**

## v17.36 重點
- **Same-cluster radar**：Radar focus 以 fuzzy match（如 2330↔2330.TW）解析；缺比率時寫回分群用補值並優先補抓 Search 股，確保焦點 trace（★）可見
- **目錄**：`app_17.0/`；顯示版號 **v17.36**

## v17.35 重點
- **DCF header 對齊**：「預測年數 n」與「選擇 DCF 估值模型」同左欄；「採用現金流」與 claim 建議同右欄（皆 6+6 靠左對齊）
- **目錄**：`app_17.0/`；顯示版號 **v17.35**

## v17.34 重點
- **台股 Rf**：無風險利率改爬取櫃買 TPEx 公債殖利率曲線最近交易日 **10 年期**（非固定 1.8% fallback 當主值）
- **目錄**：`app_17.0/`；顯示版號 **v17.34**

## v17.33 重點
- **啟動修復**：移除 CSS `content: "\00a0"`（R 字串解析成 NUL 導致 shinyapps exit status 1）；預測年數 n 與建議註解改以 padding 對齊
- **目錄**：`app_17.0/`；顯示版號 **v17.33**

## v17.32 重點
- **DCF header**：預測年數 n 與 FCFF／FCFE 建議註解同列垂直對齊
- **目錄**：`app_17.0/`；顯示版號 **v17.32**

## v17.31 重點
- **Clustering 雷達焦點**：預設為 Search 後的 Ticker；Universe（N）強制納入該股（特徵缺值仍保留於分群）
- **目錄**：`app_17.0/`；顯示版號 **v17.31**

## v17.30 重點
- **ADR 股數通知**：Search 後同一則「市值÷股價」toast 只彈一次（固定 id）；P/B／NAV 改頁內 note，不再重複通知
- **目錄**：`app_17.0/`；顯示版號 **v17.30**

## v17.29 重點
- **Macro g／Rf**：Macroeconomic Anchoring 改採即時 Yahoo ^TNX（失敗→最近成功值→工程 fallback 5% 並標明）；不再把固定 5% 當成「就是」十年公債
- **目錄**：`app_17.0/`；顯示版號 **v17.29**

## v17.28 重點
- **評價方法論**：About「Valuation Methodology｜評價方法論」對齊現況——流程紀律、四大引擎（含 P/B＋NAV）、Composite 僅疊已試算模型、Decision Checklist／MOS／F-Score／HFV（否決非買訊）、CapEx 暴衝平滑啟發式、Blue Chip Lab 研究輔助；中英雙語 Process & Guardrails
- **目錄**：`app_17.0/`；顯示版號 **v17.28**

## v17.27 重點
- **About**：隱藏全域 Ticker／Stock Code 搜尋列與「industry info from Yahoo」區塊（`input.sidebar_tabs != 'about'`）；Dashboard／其他分頁搜尋與 Yahoo 產業仍保留
- **目錄**：`app_17.0/`；顯示版號 **v17.27**

## v17.26 重點
- **Blue Chip 候選截斷**：Clustering 宇宙 N 區露出與「明細」相同的四種截斷邏輯（市值／概念股／近一年漲幅／隨機）並雙向同步；補市值缺值→代號排序說明
- **目錄**：`app_17.0/`；顯示版號 **v17.26**

## v17.25 重點
- **About**：更新「關於 The YNow App」中英簡介（含 Decision Checklist、HFV、Blue Chip Lab、Composite）；結尾附 GitHub 連結
- **目錄**：`app_17.0/`；顯示版號 **v17.25**

## v17.24 重點
- **Blue Chip Clustering**：雷達焦點改為主頁 Search／輸入代號（含 `sc` 後備與正規化）；分群後若換股且在結果內會同步焦點
- **N 解析預設**：`lab_parse_im_max_n`／resolve 空值後備改為 25（對齊 UI Evaluation／Universe 預設）
- **目錄**：`app_17.0/`；顯示版號 **v17.24**

## v17.17 重點
- **Blue Chip Clustering**：Clusters（k）預設 3；Universe／Evaluation N 預設 25 且雙向同步；雷達焦點優先目前搜尋代號，並強制納入分群宇宙
- **目錄**：`app_17.0/`；顯示版號 **v17.17**

## v17.16 重點
- **Composite overlays**：目前市價軸僅疊加本工作階段已按過試算／Run（含主模型靜默自動試算）的模型 FV 標記；未試算者不顯示。
- **模型 Beta 頁籤**：DCF／DDM／RI 的「β 來源」區塊移除外框（box chrome），控制項保留。
- **目錄**：`app_17.0/`；顯示版號 **v17.16**

## v17.15 重點

- **DDM Ke 小頁籤**：版面節奏對齊 DCF→WACC（infoBox、公式橫幅、Ke 估算盒、CAPM 鏡像）；與 WACC rₑ／採用估算雙向同步
- **DDM 分頁順序**：Ke ↔ Beta 左右對調（Ke 在前、Beta 在後，對齊 WACC→Beta）
- **目錄**：`app_17.0/`；顯示版號 **v17.15**

## v17.14 重點

- **DDM**：還原 SPM（Sum of Perpetuities；Brown & Abraham）為與 Gordon／二階段並列的第三種模式（`P = E·g/Ke² + D/Ke`）
- **目錄**：`app_17.0/`；顯示版號 **v17.14**

## v17.13 重點

- **綜合估值軸**：各模型 FV 標記改為與 Current price 相同樣式（色塊標籤＋圓點＋價格），並置於同一水平線垂直置中
- **DCF-Model**：「預測年數 n」緊接在「選擇 DCF 估值模型」選項下方（shared header；`input$years`／`dcf_mode`／`dcf_claim` 不變）
- **目錄**：`app_17.0/`；顯示版號 **v17.13**

## v17.12 重點

- **Clustering Lab**：內建離線比率特徵快照（S&P 500 + 台股上市／上櫃）；Yahoo 受限時仍可分群；可用檔數不足時自動調降 k；即時 Yahoo 仍優先覆寫快照
- **目錄**：`app_17.0/`；顯示版號 **v17.12**

## v17.11 重點

- **綜合估值**：冷載不預設顯示低估／合理／高估評估；Current price 軸疊加各模型 FV 標記（DCF／DDM／RI／P/B／NAV）
- **目錄**：`app_17.0/`；顯示版號 **v17.11**

## v17.10 重點

- **Clustering Lab**：Yahoo 比率特徵抓取韌性（chunked fetch + crumb 429 backoff）
- **目錄**：`app_17.0/`；顯示版號 **v17.10**

## v17.08 重點

- **手機頁首**：標題絕對定位區不再蓋住「繁中」（`right` 讓出語言鈕；lang z-index 提高）
- **目錄**：`app_17.0/`；顯示版號 **v17.08**

## v17.07 重點

- **手機頁首**：單列 50px；標題夾在美股／台股與右上小 logo 之間並垂直置中；「a lawrence kuo shiny app」不再被黑色固定頁首蓋住
- **目錄**：`app_17.0/`；顯示版號 **v17.07**

## v17.06 重點

- **手機彩色數值框**：還原響應式（≤991px 兩欄；≤767px 直向全寬，勿再壓成 33% 三欄）；Dashboard KPI grid 維持兩兩並排；回測 metric cards 手機兩欄
- **目錄**：`app_17.0/`；顯示版號 **v17.06**

## v17.05 重點

- **DDM／RI Beta 分頁**：恢復 β 來源設定（與 Basic Setup／DCF 同款 picker）；`beta_u_apply_source` ↔ `ddm_beta_u_apply_source` ↔ `ri_beta_u_apply_source` 雙向同步；P/B／NAV 仍無 Beta
- **美股／台股切換**：緊貼三線漢堡（`left` 依 toggle 右緣；padding／margin 歸零），無空隙
- **產業標準快覽**：移除選單上「Industry Standard／產業標準」字樣；選單＋摘要同列；六格 KPI 區間均分於其下
- **預設參數表**：補齊 NAV／CapEx 暴衝／P/B target mode 等 `APP_DEFAULTS` 欄位標籤
- **目錄**：`app_17.0/`；顯示版號 **v17.05**

## v17.04 重點

- **美股／台股／產業快覽／預設表**：見 v17.05（本版曾部署為過渡 bundle，已被 v17.05 覆蓋）
- **目錄**：`app_17.0/`；顯示版號 **v17.04**

## v17.03 重點

- **美股／台股切換**：以 absolute CSS 釘在三線 icon 右側（JS DOM 搬移為輔），不再卡在右上自訂選單
- **產業標準快覽列**：選單左、摘要／Yahoo 列右，同列並排
- **Beta 導覽提示移除**：刪除「仍在基礎設定 → BETA／DCF WACC」類 helper 文案與 locale keys
- **目錄**：`app_17.0/`；顯示版號 **v17.03**

## v17.01 重點

- **綜合估值整塊**：自 Basic Setup 移至各模型設定頁表頭（主／副模型、Bear–Base–Bull、狀態列與市價帶）
- **側邊欄推薦標籤**：修復 Recommend／推薦徽章可見性（flex 裁切＋handler 就緒重送）
- **Beta 小頁籤**：Basic Setup BETA＋DCF＋DDM＋RI（β→CAPM→Ke）；P/B／NAV 不放冗餘 Beta 分頁
- **目錄**：`app_17.0/`；顯示版號 **v17.01**

## v17 重點

- **獨立純 NAV 模型**：帳面控股淨資產（NAVPS × 倍數）；非市場法分部 SOTP；無需 Justified／SGR
- **側邊欄三分法**：資產基礎法（NAV）｜收益與現金流折現法（DCF／DDM／RI）｜相對估值法（P/B）
- **P/B 專注倍數**：產業／歷史或 Justified（需 SGR）；與純 NAV 分開
- **推薦邏輯**：控股／綜合 → 主模型 NAV；金融／帳面驅動仍以 P/B 為主，資產傾向可副選 NAV
- **目錄**：`app_17.0/`；顯示版號 **v17**（已遞增至 v17.58）
- **Blue Chip 分群 Lab**：K-Means 對比率／成長率特徵分群（星團圖＋同群雷達；預設 N＝25、批次 quote＋快取）；研究用，非買進訊號
- **財報屬性分群**：依三大報表規則標示 KPI／FS 金色點（與產業同業色碼正交）；產業快覽 Yahoo Sector/Industry 列右側顯示屬性標籤
- **頁首置頂固定**：網頁／手機共用 `.main-header { position: fixed }`，捲動不滑掉
- **美股宇宙**：S&P 500 成分同時涵蓋 **Nasdaq** 與 **NYSE** 上市股票（搜尋／Blue Chip；v17.58 起改為全市場主要上市目錄）
- **HFV 情境分類**：相鄰估值日復盤 FV＋市價 → 價值錯位／基本面動能／價格動能 → 教育用 A–D 情境（非下單訊號）

Mature-stock P/E·EV 引擎仍非本版範圍。
