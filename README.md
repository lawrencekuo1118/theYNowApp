# theYNowApp

Taiwan stock fundamental analysis Shiny app (current: **v9.0**).

## Run

```r
# from the repository root
shiny::runApp()
```

Requires R packages used by `setup.R` / `global.R`, and Python dependencies for `deep_scraper.py` / `web_crawler.R` if you use those features.

## Layout

| File | Role |
|------|------|
| `ui.R` / `server.R` / `global.R` | Shiny app entry |
| `setup.R` / `default_config.R` | Environment & defaults |
| `*_module.R` | Valuation / KPI / decision modules |
| `report_template.Rmd` | Report output |
| `web_crawler.R` / `deep_scraper.py` | Data collection helpers |

## Older versions

Historical snapshots (`v3.0`–`v8.0`) are kept as [GitHub Releases](https://github.com/lawrencekuo1118/theYNowApp/releases). The default branch tracks only the current app so the tree stays free of duplicate version folders.
