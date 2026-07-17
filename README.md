# theYNowApp

Taiwan stock fundamental analysis Shiny app（雲端版：yfinance，無 Chromote）。

## 維護方式

本機僅維護此路徑，**每個迭代版本一個資料夾**：

`/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp`

- 目前版本：`app_11.0/`（v11.0）
- 歷史版本：`app 3.0` … `app_10.0`（本機封存；GitHub Releases 另有標籤）

## 執行目前版本

```r
shiny::runApp("app_11.0")
```

Requires R packages used by `app_11.0/setup.R` / `app_11.0/global.R`, and Python deps from `app_11.0/requirements.txt` (yfinance path; no Chrome required for cloud).

## v11.0 重點

- Get Started（模型建議 + DCF 核心參數）與 Snapshot（參數／公式快照下載）
- Finance Summary 分組卡片網格；KPI 一列五個、φ⁻¹ 等比縮小
- DCF Overview 預設 Simple Mode；WACC 輸入列重排
- 回測說明文字與搜尋下拉互動修正

## Cloud notes

- Live: https://hopesmasher1118.shinyapps.io/TheYNowApp/
- Financials via **yfinance** (no Chromote / Chrome on shinyapps.io)
- `requirements.txt` / `py_require` for cloud Python; optional local `.ynow_venv`
- Selenium is local-only fallback
- Statement shaping aligns with v9 display (TTM column, Yahoo-like row order, reticulate-safe payloads)

## Dev: OneDrive / Google Drive sync guard

開發時若本機 RAM 吃緊，File Provider（OneDrive / Google Drive）同步容易卡住或失敗。用：

```bash
# 持續監控（建議在開發時另開一個 Terminal）
./tools/cloud_sync_guard.sh

# 單次檢查 / 只報告不清理
./tools/cloud_sync_guard.sh --once
./tools/cloud_sync_guard.sh --once --dry-run
```

行為摘要：

- 偵測 `~/Library/CloudStorage` 下 OneDrive / Google Drive 根目錄與同步行程
- 統計 placeholder／暫存同步檔，並讀取近期相關 log 錯誤
- 記憶體使用率偏高或可用 RAM 過低時：分析高 RSS 行程，清除專案／安全快取（`__pycache__`、chromote/R 暫存等），macOS 會嘗試 `purge`
- JSONL 日誌：`~/.ynow_cloud_sync_guard.log`

可選：用 `tools/cloud_sync_guard.macos.plist` 當 LaunchAgent（先改路徑再 `launchctl load`）。

## Layout (`app_11.0/`)

| File | Role |
|------|------|
| `ui.R` / `server.R` / `global.R` | Shiny app entry |
| `setup.R` / `default_config.R` | Environment & defaults |
| `requirements.txt` | Cloud Python dependencies |
| `*_module.R` | Valuation / KPI / decision modules |
| `report_template.Rmd` | Report output |
| `web_crawler.R` / `deep_scraper.py` | Data collection (yfinance-first) |

## Older versions

Historical snapshots live in version folders and as [GitHub Releases](https://github.com/lawrencekuo1118/theYNowApp/releases).
