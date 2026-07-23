# theYNowApp

Taiwan stock fundamental analysis Shiny app（雲端版：yfinance，無 Chromote）。

## 維護方式

本機僅維護此路徑，**每個迭代版本一個資料夾**：

`/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp`

- 目前版本：`app_13.0/`（v13.0 — Valuation Methodology）
- 歷史版本：`app 3.0` … `app_12.0`（本機封存；GitHub Releases 另有標籤）

## 本機開發（建議）

```bash
git checkout master
git pull origin master
```

```r
shiny::runApp("app_13.0")
```

Requires R packages used by `app_13.0/setup.R` / `app_13.0/global.R`, and Python deps from `app_13.0/requirements.txt`（可選本機 `.ynow_venv`）。

## 本機部署到 shinyapps.io

帳號只需設定一次（Tokens 頁）：

```r
rsconnect::setAccountInfo(name = "hopesmasher1118", token = "...", secret = "...")
```

之後在專案根目錄：

```r
rsconnect::deployApp(
  appDir = "app_13.0",
  appName = "TheYNowApp",
  appId = 10907657,
  forceUpdate = TRUE
)
```

或：

```bash
# 可選：用環境變數餵憑證
Rscript scripts/deploy_app_13.R
```

Live: https://hopesmasher1118.shinyapps.io/TheYNowApp/

## v13.0 重點

- 分類 → 主／副模型；Dashboard 以 Bear／Base／Bull 區間＋可信度呈現
- P/B：Justified（ROE/Ke）＋產業＋歷史分位
- 永續 g 預設 fundamental；成長股建議 two-stage；DCF／RI 股數級距一致

## Cloud notes

- Financials via **yfinance** (no Chromote / Chrome on shinyapps.io)
- `requirements.txt` / `py_require` for cloud Python

## Layout (`app_13.0/`)

| File | Role |
|------|------|
| `app.R` / `ynow_ui.R` / `ynow_server.R` / `global.R` | Shiny 進入點 |
| `setup.R` | 分類器、P/B derive、可信度 |
| `investment_decision_module.R` | 區間決策看板 |
| `*_module.R` | DCF／DDM／RI／P/B／KPI／回測 |

## Older versions

Historical snapshots live in version folders and as [GitHub Releases](https://github.com/lawrencekuo1118/theYNowApp/releases).
