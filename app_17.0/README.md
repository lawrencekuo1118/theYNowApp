# The YNow App v17.01 — Valuation Methodology

先分類，再選模型；先推導，再校正；先給區間，再給單點；先做基本面估值。

## v17.01 重點

- **綜合估值整塊**：自 Basic Setup 移至各模型設定頁表頭（主／副模型、Bear–Base–Bull、狀態列與市價帶）
- **側邊欄推薦標籤**：修復 Recommend／推薦徽章可見性（flex 裁切＋handler 就緒重送）
- **Beta 小頁籤**：僅保留公式實際用到 β 的路徑（Basic Setup BETA＋DCF）；DDM／RI／P/B／NAV 不放冗餘 Beta 分頁
- **目錄**：`app_17.0/`；顯示版號 **v17.01**

## v17 重點

- **獨立純 NAV 模型**：帳面控股淨資產（NAVPS × 倍數）；非市場法分部 SOTP；無需 Justified／SGR
- **側邊欄三分法**：資產基礎法（NAV）｜收益與現金流折現法（DCF／DDM／RI）｜相對估值法（P/B）
- **P/B 專注倍數**：產業／歷史或 Justified（需 SGR）；與純 NAV 分開
- **推薦邏輯**：控股／綜合 → 主模型 NAV；金融／帳面驅動仍以 P/B 為主，資產傾向可副選 NAV
- **目錄**：`app_17.0/`；顯示版號 **v17**（已遞增至 v17.01）
- **Blue Chip 分群 Lab**：K-Means 對比率／成長率特徵分群（星團圖＋同群雷達；預設 N＝25、批次 quote＋快取）；研究用，非買進訊號
- **財報屬性分群**：依三大報表規則標示 KPI／FS 金色點（與產業同業色碼正交）；產業快覽 Yahoo Sector/Industry 列右側顯示屬性標籤
- **頁首置頂固定**：網頁／手機共用 `.main-header { position: fixed }`，捲動不滑掉
- **美股宇宙**：S&P 500 成分同時涵蓋 **Nasdaq** 與 **NYSE** 上市股票（搜尋／Blue Chip）
- **HFV 情境分類**：相鄰估值日復盤 FV＋市價 → 價值錯位／基本面動能／價格動能 → 教育用 A–D 情境（非下單訊號）

Mature-stock P/E·EV 引擎仍非本版範圍。

## 執行

```r
shiny::runApp("app_17.0")
```

進入點為 `app.R`（唯一）。UI／Server 分別在 `ynow_ui.R`／`ynow_server.R`。

線上部署：`Rscript scripts/deploy_app_17.R`

## 方法論一句話

> 先分類，再選模型；先推導，再校正；先給區間，再給單點；先做基本面估值，技術分析只作交易輔助。

## 意見區（Feedback → GitHub Issues）

側邊欄底部 Snapshot／測試旁的 **意見區** 可收集使用者回饋，送出後會建立 GitHub Issue（標籤 `feedback`）。

請設定環境變數：

- `YNOW_FEEDBACK_GITHUB_TOKEN`：具 `issues:write` 的 GitHub PAT（必填才能送出）
- `YNOW_FEEDBACK_GITHUB_REPO`：選填，預設 `lawrencekuo1118/theYNowApp`

**shinyapps.io 注意：** 沒有 Settings → Vars／secrets UI，也無法用 rsconnect `envVars` 注入環境變數。請把上述變數寫入 `app_17.0/.Renviron`（此檔須在 `.gitignore` 中，**永不提交**），再以 `Rscript scripts/deploy_app_17.R` 重新部署；token 會跟著 bundle 上線。
