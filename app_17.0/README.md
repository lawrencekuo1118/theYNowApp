# The YNow App v17.41 — Valuation Methodology

先分類，再選模型；先推導，再校正；先給區間，再給單點；先做基本面估值。

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
- **目錄**：`app_17.0/`；顯示版號 **v17**（已遞增至 v17.41）
- **Blue Chip 分群 Lab**：K-Means 對比率／成長率特徵分群（星團圖＋同群雷達；預設 N＝25、批次 quote＋快取）；研究用，非買進訊號
- **財報屬性分群**：依三大報表規則標示 KPI／FS 金色點（與產業同業色碼正交）；產業快覽 Yahoo Sector/Industry 列右側顯示屬性標籤
- **頁首置頂固定**：網頁／手機共用 `.main-header { position: fixed }`，捲動不滑掉
- **美股宇宙**：S&P 500 成分同時涵蓋 **Nasdaq** 與 **NYSE** 上市股票（搜尋／Blue Chip）
- **HFV 情境分類**：相鄰估值日復盤 FV＋市價 → 價值錯位／基本面動能／價格動能 → 教育用 A–D 情境（非下單訊號）

Mature-stock P/E·EV 引擎仍非本版範圍。
