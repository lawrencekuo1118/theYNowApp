# ==========================================
# global.R - 初始化與環境設定（app_20.0）
# ==========================================

# 🐍 Python：本機可選 .ynow_venv；雲端延遲以 py_require / uv 安裝（Listen 之後）
Sys.setenv(RETICULATE_PYTHON = "")
env_dir <- "./.ynow_venv"
python_path <- file.path(env_dir, "bin", "python")

# Shared hosted-connect detector (Posit Connect on shinyapps.io).
if (!exists(".ynow_is_hosted_connect", mode = "function", inherits = FALSE)) {
  .ynow_is_hosted_connect <- function() {
    wd <- tryCatch(normalizePath(getwd(), mustWork = FALSE), error = function(e) getwd())
    nzchar(Sys.getenv("SHINY_SERVER_VERSION")) ||
      grepl("shinyapps", Sys.getenv("HOSTNAME"), ignore.case = TRUE) ||
      grepl("shinyapps", Sys.getenv("R_CONFIG_ACTIVE"), ignore.case = TRUE) ||
      identical(Sys.getenv("FORCE_SHINYAPPS_PYTHON"), "1") ||
      grepl("/srv/connect/apps", wd, fixed = TRUE) ||
      (identical(Sys.getenv("USER"), "shiny") && dir.exists("/srv/connect"))
  }
}
on_shinyapps <- isTRUE(.ynow_is_hosted_connect())

# Never let uv download a managed CPython while the worker is still booting.
# Cold-start installs blocked Listening and timed out Safari / curl.
# Also block when there is no local venv (hosted mis-detect / bare image).
if (isTRUE(on_shinyapps) || !file.exists(python_path)) {
  Sys.setenv(
    UV_PYTHON_DOWNLOADS = "never",
    RETICULATE_USE_MANAGED_VENV = "false"
  )
}

if (file.exists(python_path) && !on_shinyapps) {
  Sys.setenv(RETICULATE_PYTHON = python_path)
} else {
  # 雲端：勿指向不存在的 Python；延遲到 .ynow_ensure_python()
  Sys.unsetenv("RETICULATE_PYTHON")
}

# knitr 設定（適用於 R Markdown）
knitr::opts_chunk$set(comment = NA)
knitr::opts_knit$set(global.par = TRUE)

# 全域選項：避免科學記號、數字位數、輸出寬度
options(scipen = 20, digits = 4, width = 90)

# 📚 套件安裝與載入 --------------------------------------------------------
pacman::p_load(
  # 1. 核心資料處理 (Data Wrangling)
  tidyverse, dplyr, stringr, purrr, magrittr, glue, jsonlite,

  # 2. 視覺化與報表 (Visualization & Reporting)
  ggplot2, ggrepel, plotly, DT, rmarkdown, knitr, scales, pagedown,

  # 3. 網頁與 Python 整合（雲端版不依賴 chromote / Chrome）
  rvest, xml2, reticulate,

  # 4. Shiny 框架與 UI 元件 (Shiny & UI)
  shiny, shinydashboard, shinyjs, shinycustomloader, shinyWidgets, shinyBS, shinycssloaders,

  # 5. 效能優化與快取 (Performance & Cache)
  memoise, cachem, TTR
)

# jsonlite masks shiny::validate after p_load order; restore Shiny's for need()/render*
validate <- shiny::validate
need <- shiny::need

# ==========================================
# 🐍 綁定 Python（延遲初始化）
# ==========================================
# Do NOT call reticulate::py_require() / py_config() synchronously here on
# shinyapps.io. Each cold start was downloading uv + CPython (~33MiB) + packages
# before Shiny could Listen, so Safari / curl saw "server stopped responding".
# Install and bind Python on first real use via .ynow_ensure_python().
.ynow_py_pkgs <- c(
  "pandas", "numpy", "yfinance", "requests", "beautifulsoup4", "lxml",
  "peewee", "platformdirs", "frozendict", "multitasking", "html5lib", "curl_cffi",
  "xlrd"
)
# Keep the old name for any callers that still read `py_pkgs`.
py_pkgs <- .ynow_py_pkgs
.ynow_python_ready <- FALSE
# Armed by shinyApp(onStart=...) after Listening. Until then, refuse uv installs.
.ynow_python_install_armed <- FALSE
.ynow_arm_python_install <- function() {
  .ynow_python_install_armed <<- TRUE
  invisible(TRUE)
}

.ynow_ensure_python <- function() {
  if (identical(Sys.getenv("YNOW_DEBUG_SKIP_PY"), "1")) return(FALSE)
  if (isTRUE(.ynow_python_ready)) {
    return(isTRUE(tryCatch(
      reticulate::py_available(initialize = FALSE),
      error = function(e) FALSE
    )))
  }
  # Hosted / no-venv: never download CPython while the worker is still sourcing.
  need_managed <- isTRUE(on_shinyapps) || !file.exists(python_path)
  if (isTRUE(need_managed) && !isTRUE(.ynow_python_install_armed)) {
    return(FALSE)
  }
  ok <- FALSE
  old_uv <- Sys.getenv("UV_PYTHON_DOWNLOADS", unset = NA_character_)
  old_managed <- Sys.getenv("RETICULATE_USE_MANAGED_VENV", unset = NA_character_)
  # Boot blocks downloads; re-enable only for an on-demand install after Listen.
  if (isTRUE(need_managed)) {
    Sys.setenv(
      UV_PYTHON_DOWNLOADS = "auto",
      RETICULATE_USE_MANAGED_VENV = "true"
    )
    on.exit({
      if (is.na(old_uv)) Sys.setenv(UV_PYTHON_DOWNLOADS = "never")
      else Sys.setenv(UV_PYTHON_DOWNLOADS = old_uv)
      if (is.na(old_managed)) Sys.setenv(RETICULATE_USE_MANAGED_VENV = "false")
      else Sys.setenv(RETICULATE_USE_MANAGED_VENV = old_managed)
    }, add = TRUE)
  }
  tryCatch({
    if (file.exists(python_path) && !on_shinyapps) {
      reticulate::use_virtualenv(env_dir, required = TRUE)
    } else {
      # shinyapps / other hosts without a local venv: install when first needed.
      reticulate::py_require(.ynow_py_pkgs)
    }
    suppressMessages(reticulate::py_config())
    ok <- isTRUE(reticulate::py_available(initialize = FALSE))
  }, error = function(e) {
    ok <<- FALSE
  })
  .ynow_python_ready <<- isTRUE(ok)
  isTRUE(ok)
}

if (file.exists(python_path) && !on_shinyapps) {
  tryCatch(
    reticulate::use_virtualenv(env_dir, required = TRUE),
    error = function(e) NULL
  )
} else if (identical(Sys.getenv("YNOW_DEBUG_SKIP_PY"), "1")) {
  # skip Python init (local parse / unit tests)
}
# else: defer — .ynow_ensure_python() runs from scrapers / TPEx / Yahoo fallbacks

# ==========================================
# 應用程式進入點與全域設定
# ==========================================
# local=TRUE：物件寫入「正在評估 global.R 的環境」（即 app.R 的評估環境）
source("debug_helpers.R", local = TRUE, encoding = "UTF-8")
source("setup.R", local = TRUE, encoding = "UTF-8")
source("market_profile.R", local = TRUE, encoding = "UTF-8")
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("fs_locale_zh_tw.R", local = TRUE, encoding = "UTF-8")
source("web_crawler.R", local = TRUE, encoding = "UTF-8")
source("tpex_financial.R", local = TRUE, encoding = "UTF-8")
source("industry_standards.R", local = TRUE, encoding = "UTF-8")
source("fundamental_profile.R", local = TRUE, encoding = "UTF-8")
source("kpi_module.R", local = TRUE, encoding = "UTF-8")
source("investment_decision_module.R", local = TRUE, encoding = "UTF-8")
source("decision_checklist_module.R", local = TRUE, encoding = "UTF-8")
source("shenanigans_module.R", local = TRUE, encoding = "UTF-8")
source("ddm_module.R", local = TRUE, encoding = "UTF-8")
source("fcf_projection_module.R", local = TRUE, encoding = "UTF-8")
source("ri_module.R", local = TRUE, encoding = "UTF-8")
source("pb_asset_module.R", local = TRUE, encoding = "UTF-8")
source("relative_multiples_module.R", local = TRUE, encoding = "UTF-8")
source("sotp_module.R", local = TRUE, encoding = "UTF-8")
source("nav_module.R", local = TRUE, encoding = "UTF-8")
source("rebal_freq.R", local = TRUE, encoding = "UTF-8")
source("backtest_module.R", local = TRUE, encoding = "UTF-8")
source("backtest_validation.R", local = TRUE, encoding = "UTF-8")
source("lab_concept_groups.R", local = TRUE, encoding = "UTF-8")
source("macro_bubble_indicators.R", local = TRUE, encoding = "UTF-8")
source("ynow_index.R", local = TRUE, encoding = "UTF-8")
source("hccsi_config.R", local = TRUE, encoding = "UTF-8")
source("hccsi_engine.R", local = TRUE, encoding = "UTF-8")
source("hccsi_module.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_schema.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_engine.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_structure.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_module.R", local = TRUE, encoding = "UTF-8")
source("macro_market_module.R", local = TRUE, encoding = "UTF-8")
source("asset_transmission_module.R", local = TRUE, encoding = "UTF-8")
source("lazy_tabs.R", local = TRUE, encoding = "UTF-8")
source("default_config.R", local = TRUE, encoding = "UTF-8")
source("param_audit.R", local = TRUE, encoding = "UTF-8")
source("lab_industry_method.R", local = TRUE, encoding = "UTF-8")
source("lab_clustering.R", local = TRUE, encoding = "UTF-8")
# debug_lab.R is not sourced here; set YNOW_DEBUG=1 and source it locally if needed.
