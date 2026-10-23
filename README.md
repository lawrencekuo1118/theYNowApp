# theYNowApp

US / Taiwan stock fundamental analysis Shiny app（雲端版：yfinance，無 Chromote）。

**Live:** https://hopesmasher1118.shinyapps.io/TheYNowApp/  
**Display version:** v26.64 · **App folder:** `app_27.0/`  
**Deploy baseline:** `scripts/DEPLOY_BASELINE.txt`（bundleId／commit／URL；開發比對以此為準，不是單純看 Git tip）  
**Changelog:** [`CHANGELOG.md`](CHANGELOG.md) · **Releases:** https://github.com/lawrencekuo1118/theYNowApp/releases

## CI / CD

PRs and pushes that touch `app_27.0/**`, `scripts/**`, or `.github/workflows/**` run meaningful **offline** checks (no live Yahoo):

| Workflow | What it runs |
|----------|----------------|
| **Live-line CI** (`.github/workflows/ci.yml`) | `YNOW_DEBUG_SKIP_PY=1 Rscript app_27.0/tests/test_valuation_formulas.R` and `bash scripts/run_phase1_audit.sh` |
| **ITGC Audit** (`.github/workflows/audit.yml`) | Phases 2–4 via `bash scripts/run_audit.sh` + RCM artifact |

Do not rely on path filters that skip the live app tree. Docs/rules-only changes may skip app jobs; app behavior PRs must stay green on Live-line CI.

**Ship flow（活動線）:**

1. Commit + `git push origin HEAD`（coding clone）
2. `Rscript scripts/deploy_app_21.R` → display version **+0.01** → shinyapps upload
3. Update `scripts/DEPLOY_BASELINE.txt` and push
4. Add／update [`CHANGELOG.md`](CHANGELOG.md) entry for that `display_version`（en-US + zh-TW 台灣用語；勿只留版號）
5. `bash scripts/record_github_release.sh v25.xx`（needs `gh` + token; **soft-fail** if unavailable — secrets never in git）

## 維護方式

兩個工作複本對齊 **shinyapps 部署基準**：

| Clone | Path | 用途 |
|-------|------|------|
| Coding（建議 git） | `/Users/lawrencekuo/coding/theYNowApp` | commit／push／deploy |
| OneDrive mirror | `/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp` | 同步鏡像（勿在此跑 `git status`／`git push`） |

- **每個整階版本一個資料夾**（目前活動線：`app_27.0/`；顯示版號於 major cut 後為 `v27.00`，其後 deploy 以 +0.01 補丁）
- 歷史版本：空間命名封存 `app 3.0` … `app 7.0` 與較新的 `app_8.0`…`app_24.0` 皆已自 git 移除（見 Git history／[GitHub Releases](https://github.com/lawrencekuo1118/theYNowApp/releases)）
- 顯示版號摘要：[`CHANGELOG.md`](CHANGELOG.md)；方法論／歷史細節：[`app_27.0/README.md`](app_27.0/README.md)

## 本機開發（建議）

```bash
git checkout master
git pull origin master
```

```r
shiny::runApp("app_27.0")
```

Cursor Cloud：`bash scripts/cloud-agent-start.sh`（`app_27.0/` → `0.0.0.0:3838`）。

Requires R packages used by `app_27.0/setup.R` / `app_27.0/global.R`, and Python deps from `app_27.0/requirements.txt`（可選本機 `app_27.0/.ynow_venv`）。

## 本機部署到 shinyapps.io

帳號只需設定一次（Tokens 頁）：

```r
rsconnect::setAccountInfo(name = "hopesmasher1118", token = "...", secret = "...")
```

之後在專案根目錄：

```bash
# 預設會 +0.01 顯示版號後上傳；重試可用 YNOW_SKIP_VERSION_BUMP=1
Rscript scripts/deploy_app_21.R
# optional alias (same ship path): Rscript scripts/deploy_app_25.R
```

或：

```r
rsconnect::deployApp(
  appDir = "app_27.0",
  appName = "TheYNowApp",
  appId = 10907657,
  forceUpdate = TRUE
)
```

Deploy 後請更新 `scripts/DEPLOY_BASELINE.txt`（bundleId、commit、`display_version`）。

## 目前線重點（v18 → v25）

- **方法論：** 產業／生命週期分類 → 主／副模型；Bear／Base／Bull；Decision Checklist、HFV（否決工具，非買進訊號）、YNOW 漏斗
- **估值：** DCF（FCFF／FCFE）、DDM、RI、P/B／NAV、相對倍數、SOTP；CapEx 暴衝平滑；終值 g／WACC 防呆
- **Macro & Market Trends：** US／TW 訂閱切換；指數 KPI、10Y Rf、產業／概念 vs 大盤、Rolling β（交叉檢驗）
- **Business Breakdown**（完整版）：營運／獲利結構報告式分析；訂單研究聚焦已揭露的訂單數量、主要供應商、重大客戶營收貢獻，並標示台灣來源優先序與可信度（MOPS → IR → 財報交叉驗證；券商／產業僅估計）
- **Asset transmission：** 側邊欄獨立分頁（Lite 亦提供）
- **Blue Chip Leaderboard／分群：** 宇宙檔數 N、ADR、概念股、候選截斷
- **量化回測／HFV：** 報告式章節；Lite 隱藏完整回測實驗室
- **Lite：** 側邊欄 logo 切換；智慧分析（自動主副模型＋預設參數）
- **PDF 個股報告：** logo 藍綠色系、編號分析說明；Full／Lite × US／TW 檔名

## Cloud notes

- Financials via **yfinance** (no Chromote / Chrome on shinyapps.io)
- `app_27.0/requirements.txt` / `py_require` for cloud Python
- UI copy：en-US + zh-TW（台灣投資界用語）；見 `app_27.0/ui_locale.R`

## Layout (`app_27.0/`)

| File / area | Role |
|-------------|------|
| `app.R` / `ynow_ui.R` / `ynow_server.R` / `global.R` | Shiny 進入點 |
| `setup.R` / `default_config.R` / `industry_standards.R` | 分類、預設、產業標準、報告文案 |
| `ui_locale.R` | en-US／zh-TW 字串 |
| `macro_market_module.R` | 總體經濟與大盤 |
| `investment_decision_module.R` / `decision_checklist_module.R` | 區間決策／決策檢核 |
| `htcdi_*.R` / `business_breakdown_*.R` | HFV／Business Breakdown |
| `asset_transmission_module.R` | Asset transmission |
| `lab_*.R` / `backtest_module.R` | Blue Chip／分群／回測 |
| `report_template.Rmd` | Download Report (PDF) |
| `deep_scraper.py` / `web_crawler.R` | SEC／財報擷取 |
| `*_module.R` | DCF／DDM／RI／P/B／KPI／相對倍數／SOTP 等 |
| `tests/` | 定向驗證腳本 |

## Older versions

Historical `app_N.0` trees (including space-named `app 3.0` … `app 7.0`) were removed from git (C-DEAD-05 + follow-up); recover from git history or [GitHub Releases](https://github.com/lawrencekuo1118/theYNowApp/releases).
