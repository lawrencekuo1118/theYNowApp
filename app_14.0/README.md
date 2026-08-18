# The YNow App v14.0 — Valuation Methodology

先分類，再選模型；先推導，再校正；先給區間，再給單點；先做基本面估值。

## v14.0 重點

- **以 app_13.0 最新線晋升**：參數彈性、Snapshot／APP_DEFAULTS、Beta UI、WACC rᵈ 等後續優化自此以 14 版為準
- **Dashboard 財報附註 (SEC EDGAR)**：年報 10-K／20-F、季報 10-Q、重大訊息 8-K／6-K 附註／正文擷取（Cash Flow 旁頁籤）
- **Blue Chip**：美股績優篩選（規模 × 產業 × 評價模型；S&P 500；排行榜＋盈餘品質／門檻）
- **YNOW 分頁**：Schilit《財報詭計》四大類 15 招＋三表共通警訊自動判讀（與舊 Fraud Warnings 合併為單一警示區）
- **實驗區 Lab（testing env.）**：要不要持股（持倉回測條件／回測濾鏡）；其他規劃中工具
- **分類 → 主／副模型**：依經濟本質（金融／成長／成熟／控股資產）只保留一個主模型，副模型交叉驗證
- **P/B 有來源**：Justified（ROE vs Ke）＋產業區間＋歷史分位，不再只靠人工拍板倍數
- **Bear / Base / Bull + 可信度**：主模型輸出區間與 低／中／高 可信度，Dashboard 以區間語言呈現
- **基本面優先預設**：永續 g 預設 fundamental；成長股自動建議 two-stage DCF
- **股數級距一致**：DCF／RI／WACC／DDM 與 P/B 共用 share-class 解析（BRK-B、ADR）；搜尋時若財報股數與市值÷股價差超過門檻，自動改用報價股約當股數
- **FCFE**：DCF 可切換為股權現金流（Ke 折現；FCFE＝FCFF−稅後利息＋淨舉債），不必再做 EV→股權橋接
- **二階段 DDM**：高速期 g1 後收斂至永續 g2
- **控股 NAV**：P/B 提供現金／投資科目拆解；NAV＝權益 − 控股折價×投資

Mature-stock P/E·EV 引擎仍非本版範圍。

## 執行

```r
shiny::runApp("app_14.0")
```

進入點為 `app.R`（唯一）。UI／Server 分別在 `ynow_ui.R`／`ynow_server.R`。

線上部署：`Rscript scripts/deploy_app_14.R`

## 方法論一句話

> 先分類，再選模型；先推導，再校正；先給區間，再給單點；先做基本面估值，技術分析只作交易輔助。

## 意見區（Feedback → GitHub Issues）

側邊欄底部 Snapshot／測試旁的 **意見區** 可收集使用者回饋，送出後會建立 GitHub Issue（標籤 `feedback`）。

請設定環境變數：

- `YNOW_FEEDBACK_GITHUB_TOKEN`：具 `issues:write` 的 GitHub PAT（必填才能送出）
- `YNOW_FEEDBACK_GITHUB_REPO`：選填，預設 `lawrencekuo1118/theYNowApp`

**shinyapps.io 注意：** 沒有 Settings → Vars／secrets UI，也無法用 rsconnect `envVars` 注入環境變數。請把上述變數寫入 `app_14.0/.Renviron`（此檔須在 `.gitignore` 中，**永不提交**），再以 `Rscript scripts/deploy_app_14.R` 重新部署；token 會跟著 bundle 上線。
