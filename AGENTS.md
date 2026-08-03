# AGENTS.md

## Cursor Cloud specific instructions

theYNowApp is a single R Shiny stock-valuation app. The canonical app lives in `app_13.0/` (see `README.md` and `scripts/DEPLOY_BASELINE.txt`). There is no separate backend, database, or test/lint suite in this repo; the only "backend" is the live Yahoo Finance API accessed through the Python `yfinance` library via `reticulate`.

### Services

| Service | How to run | Notes |
|---------|------------|-------|
| Shiny app (`app_13.0`) | see run command below | Serves the whole product on port 3838 |

### Running the app (IMPORTANT non-obvious caveat)

Run from the repo root, and **initialize reticulate against the local venv BEFORE `runApp`**:

```r
R --quiet -e 'reticulate::use_virtualenv("/workspace/app_13.0/.ynow_venv", required=TRUE); reticulate::py_config(); shiny::runApp("app_13.0", host="0.0.0.0", port=3838, launch.browser=FALSE)'
```

Why the pre-init is required: `app_13.0/web_crawler.R` sources the Python scraper (`deep_scraper.py`) at **startup, top-level**, and only if Python is already initialized (`reticulate::py_available(initialize = FALSE)` is `TRUE`). That top-level source is what makes the scraper functions (e.g. `get_summary_quote`, `fast_get_company_info`, `scrape_all_financials`) visible to the server. On shinyapps.io the cloud branch of `global.R` calls `py_config()` (which initializes Python) before that. The local-venv branch does not, so if you launch `runApp` without pre-initializing reticulate, Python is initialized only lazily and `deep_scraper.py` ends up sourced into a throwaway frame — the scraper functions never bind and every ticker search returns `N/A`. Pre-initializing reticulate (as above) reproduces the shinyapps behavior so data loads correctly. Do not set `FORCE_SHINYAPPS_PYTHON=1` locally: that switches `global.R` to the cloud branch, which builds a separate `py_require`/uv environment instead of the patched `.ynow_venv`.

### Yahoo Finance rate limiting (environment-only patch)

Yahoo Finance rate-limits the plain `python-requests` TLS fingerprint from datacenter / cloud IPs (HTTP 429 "Too Many Requests"); browser-impersonated `curl_cffi` requests succeed. The app is unchanged (it works on shinyapps.io). In this VM the fix is `scripts/ynow_cloud_yf_patch.py`, which idempotently patches the gitignored venv copy of `yfinance/data.py` so its default HTTP session uses `curl_cffi` impersonation. The update script re-applies it after refreshing the venv. If ticker searches suddenly return `N/A`/rate-limit errors, re-run `app_13.0/.ynow_venv/bin/python scripts/ynow_cloud_yf_patch.py`. Note: `.pth`/`sitecustomize`/`usercustomize` startup hooks do **not** work under reticulate's embedded interpreter, which is why the dependency itself is patched.

### Basic checks

- No `.lintr` or `testthat` suite exists. As a quick sanity check, parse all sources: `cd app_13.0 && Rscript -e 'invisible(lapply(list.files(".", "\\.R$"), parse))'`.
- Verify the data layer without the UI: `cd app_13.0 && ./.ynow_venv/bin/python -c "import deep_scraper as ds; print(ds.fast_get_company_info('AAPL'))"`.

### Default ticker

The Ticker input defaults to `TSM`; the app supports US tickers (e.g. `AAPL`) and `.TW`/`.TWO` Taiwan tickers.
