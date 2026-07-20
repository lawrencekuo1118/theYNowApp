# theYNowApp

Taiwan stock fundamental analysis Shiny app（雲端版：yfinance，無 Chromote）。

## 維護方式

本機僅維護此路徑，**每個迭代版本一個資料夾**：

`/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp`

- 目前版本：`app_12.0/`（v12.0 — Backtest Logic Optimization）
- 歷史版本：`app 3.0` … `app_11.0`（本機封存；GitHub Releases 另有標籤）

## 執行目前版本

```r
shiny::runApp("app_12.0")
```

Requires R packages used by `app_12.0/setup.R` / `app_12.0/global.R`, and Python deps from `app_12.0/requirements.txt` (yfinance path; no Chrome required for cloud).

## v12.0 重點

回測可信度升級（非新估值模型）：

- Point-in-Time 動態重建 DCF／DDM／RI／P/B（Session-only，無歷史估值倉庫）
- Historical Fair Value Timeline + MOS／訊號可解釋性
- Strategy A 季頻＋滯後曝險；Strategy B 情緒僅能在 A 的 75%–125% 調整
- Alpha Dashboard、B&H Gap、MOS／FV 前瞻驗證、參數高原敏感度

## Cloud notes

- Live: https://hopesmasher1118.shinyapps.io/TheYNowApp/
- Financials via **yfinance** (no Chromote / Chrome on shinyapps.io)
- `requirements.txt` / `py_require` for cloud Python; optional local `.ynow_venv`

## Layout (`app_12.0/`)

| File | Role |
|------|------|
| `ui.R` / `server.R` / `global.R` | Shiny app entry |
| `backtest_module.R` | PIT 多模型回測引擎 |
| `backtest_validation.R` | Alpha／Gap／MOS／高原驗證 |
| `*_module.R` | DCF／DDM／RI／P/B／KPI 等既有模組 |

## Older versions

Historical snapshots live in version folders and as [GitHub Releases](https://github.com/lawrencekuo1118/theYNowApp/releases).
