# theYNowApp

Taiwan stock fundamental analysis Shiny app（雲端版：yfinance，無 Chromote）。

## 維護方式

本機僅維護此路徑，**每個迭代版本一個資料夾**：

`/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp`

- 目前版本：`app_14.0/`（v14.0 — Valuation Methodology + SEC Lab）
- **開發／比對基準以目前 shinyapps 部署為準**（見 `scripts/DEPLOY_BASELINE.txt`），不是單純看 Git tip
- 歷史版本：`app 3.0` … `app_13.0`（本機封存；GitHub Releases 另有標籤）

## 本機開發（建議）

```bash
git checkout master
git pull origin master
```

```r
shiny::runApp("app_14.0")
```

Requires R packages used by `app_14.0/setup.R` / `app_14.0/global.R`, and Python deps from `app_14.0/requirements.txt`（可選本機 `.ynow_venv`）。

## 本機部署到 shinyapps.io

帳號只需設定一次（Tokens 頁）：

```r
rsconnect::setAccountInfo(name = "hopesmasher1118", token = "...", secret = "...")
```

之後在專案根目錄：

```r
rsconnect::deployApp(
  appDir = "app_14.0",
  appName = "TheYNowApp",
  appId = 10907657,
  forceUpdate = TRUE
)
```

或：

```bash
# 可選：用環境變數餵憑證
Rscript scripts/deploy_app_14.R
```

Live: https://hopesmasher1118.shinyapps.io/TheYNowApp/

## v14.0 重點

- 延續 v13 方法論（分類 → 主／副模型；Bear／Base／Bull 區間＋可信度）
- **Dashboard 財報附註 (SEC EDGAR)**（Cash Flow 旁頁籤）：年報 10-K／20-F、季報 10-Q、重大訊息 8-K／6-K
- **實驗區 Lab**（Snapshot 旁「測試」）：美股產業 × 建議評價方法分組＋F-Score 績優篩選

## Cloud notes

- Financials via **yfinance** (no Chromote / Chrome on shinyapps.io)
- `requirements.txt` / `py_require` for cloud Python

## Layout (`app_14.0/`)

| File | Role |
|------|------|
| `app.R` / `ynow_ui.R` / `ynow_server.R` / `global.R` | Shiny 進入點 |
| `setup.R` | 分類器、P/B derive、可信度 |
| `investment_decision_module.R` | 區間決策看板 |
| `deep_scraper.py` / `web_crawler.R` | SEC Lab、資料抓取 |
| `*_module.R` | DCF／DDM／RI／P/B／KPI／回測 |

## Older versions

Historical snapshots live in version folders and as [GitHub Releases](https://github.com/lawrencekuo1118/theYNowApp/releases).
