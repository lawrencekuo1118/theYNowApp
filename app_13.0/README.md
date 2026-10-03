# The YNow App v13.0 — Valuation Methodology

先分類，再選模型；先推導，再校正；先給區間，再給單點；先做基本面估值。

## v13.0 重點

- **分類 → 主／副模型**：依經濟本質（金融／成長／成熟／控股資產）只保留一個主模型，副模型交叉驗證
- **P/B 有來源**：Justified（ROE vs Ke）＋產業區間＋歷史分位，不再只靠人工拍板倍數
- **Bear / Base / Bull + 可信度**：主模型輸出區間與 低／中／高 可信度，Dashboard 以區間語言呈現
- **基本面優先預設**：永續 g 預設 fundamental；成長股自動建議 two-stage DCF
- **股數級距一致**：DCF／RI 與 P/B 共用 share-class 解析（如 BRK-B）

SOTP／NAV 與成熟股 P/E·EV 引擎留待 13.1+。

## 執行

```r
shiny::runApp("app_13.0")
```

進入點為 `app.R`（唯一）。UI／Server 分別在 `ynow_ui.R`／`ynow_server.R`。

## 方法論一句話

> 先分類，再選模型；先推導，再校正；先給區間，再給單點；先做基本面估值，技術分析只作交易輔助。
