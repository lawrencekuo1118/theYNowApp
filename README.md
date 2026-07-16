# theYNowApp

Taiwan stock fundamental analysis Shiny app (current: **v10.0 cloud edition**).

## Run

```r
# from the repository root
shiny::runApp()
```

Requires R packages used by `setup.R` / `global.R`, and Python deps from `requirements.txt` (yfinance path; no Chrome required for cloud).

## Cloud notes (v10.0)

- Financials via **yfinance** (no Chromote / Chrome on shinyapps.io)
- `requirements.txt` / `py_require` for cloud Python; optional local `.ynow_venv`
- Selenium is local-only fallback
- Statement shaping aligns with v9 display (TTM column, Yahoo-like row order, reticulate-safe payloads)
- KPI helpers use multi-alias row matching for yfinance vs Yahoo HTML naming

## Layout

| File | Role |
|------|------|
| `ui.R` / `server.R` / `global.R` | Shiny app entry |
| `setup.R` / `default_config.R` | Environment & defaults |
| `requirements.txt` | Cloud Python dependencies |
| `*_module.R` | Valuation / KPI / decision modules |
| `report_template.Rmd` | Report output |
| `web_crawler.R` / `deep_scraper.py` | Data collection (yfinance-first) |

## Older versions

Historical snapshots are kept as [GitHub Releases](https://github.com/lawrencekuo1118/theYNowApp/releases). The default branch tracks only the current app at repo root.
