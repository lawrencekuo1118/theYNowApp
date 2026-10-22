# C-STRUCT-02: About page bodies live in ynow_about_ui.R (sourced by ynow_ui.R).
# Run: cd app_26.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_ynow_about_ui.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat(sprintf("[PASS] %s\n", label))
  } else {
    cat(sprintf("[FAIL] %s\n", label))
    fail <<- fail + 1L
  }
}

facade_path <- file.path(app_dir, "ynow_ui.R")
about_path <- file.path(app_dir, "ynow_about_ui.R")
check("ynow_about_ui.R exists", file.exists(about_path))
check("ynow_ui.R exists", file.exists(facade_path))

facade <- paste(readLines(facade_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
about <- paste(readLines(about_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "facade sources about sibling",
  grepl('source("ynow_about_ui.R", local = TRUE, encoding = "UTF-8")', facade, fixed = TRUE)
)
check(
  "facade does not redefine about builders",
  !grepl(".legal_notices_ui <- function()", facade, fixed = TRUE) &&
    !grepl(".about_bilingual_intro_ui <- function()", facade, fixed = TRUE) &&
    !grepl(".about_lite_intro_ui <- function()", facade, fixed = TRUE)
)
check(
  "sibling defines About builders (intro + data sources + legal)",
  grepl(".legal_notices_ui <- function()", about, fixed = TRUE) &&
    grepl(".about_data_sources_ui <- function()", about, fixed = TRUE) &&
    grepl(".about_bilingual_intro_ui <- function()", about, fixed = TRUE) &&
    grepl(".about_lite_intro_ui <- function()", about, fixed = TRUE)
)
check(
  "About data sources copy uses 資料來源 / Data sources keys",
  grepl("about_data_sources_section_title", about, fixed = TRUE) &&
    grepl("about_data_sources_markets_body", about, fixed = TRUE)
)
check(
  "sidebar no longer hosts data-source blurb",
  !grepl("ynow_data_source_block", facade, fixed = TRUE) &&
    !grepl("ynow_data_source_title", facade, fixed = TRUE) &&
    !grepl("ynow_data_source_body", facade, fixed = TRUE)
)

# Call site + Lite/Full wrappers: About body is lazy (#415) via ynow_lazy_host_about;
# Full/Lite + data sources + legal live in .ynow_page_ui_about (prefs: Lite free About).
m_tab <- regexpr(
  'tabItem\\(\\s*\\n\\s*tabName = "about"',
  facade,
  perl = TRUE
)[1]
check("facade has about tabItem", m_tab > 0L)
about_slice <- if (m_tab > 0L) {
  substr(facade, m_tab, min(nchar(facade), m_tab + 800L))
} else {
  ""
}
check(
  "about tab uses lazy host",
  grepl("ynow_lazy_host_about", about_slice, fixed = TRUE)
)
page_about <- {
  m <- regexpr("\\.ynow_page_ui_about\\s*<-\\s*function", facade, perl = TRUE)[1]
  if (m > 0L) substr(facade, m, min(nchar(facade), m + 1200L)) else ""
}
check(
  "about page builder mounts Full + Lite + data sources + legal",
  grepl("ynow-full-only", page_about, fixed = TRUE) &&
    grepl(".about_bilingual_intro_ui()", page_about, fixed = TRUE) &&
    grepl("ynow-lite-only", page_about, fixed = TRUE) &&
    grepl(".about_lite_intro_ui()", page_about, fixed = TRUE) &&
    grepl(".about_data_sources_ui()", page_about, fixed = TRUE) &&
    grepl(".legal_notices_ui()", page_about, fixed = TRUE)
)
check(
  "about page builder does not mount methodology section",
  !grepl(".valuation_methodology_section_ui(", page_about, fixed = TRUE)
)
check(
  "About data sources documents YNOW/TYNOW indices",
  grepl("about_data_sources_ynow_index_title", about, fixed = TRUE) &&
    grepl("about_data_sources_ynow_index_body", about, fixed = TRUE)
)

check(
  "Full About mentions account sign-in (en + zh-TW)",
  grepl("Account sign-in", about, fixed = TRUE) && grepl("帳號登入", about, fixed = TRUE)
)
check(
  "About copy carries display version",
  grepl("The YNow App (v", about, fixed = TRUE) &&
    (grepl("The YNow App Lite (v", about, fixed = TRUE) ||
      grepl("The YNow App Lite（v", about, fixed = TRUE))
)
check(
  "About mentions Supabase Postgres (not Neon)",
  grepl("Supabase Postgres", about, fixed = TRUE) &&
    !grepl("Neon", about, fixed = TRUE)
)
check(
  "Lite About keeps free core wording",
  grepl("簡化版", about, fixed = TRUE) && grepl("Lite principle", about, fixed = TRUE)
)

parse_ok <- tryCatch({
  parse(about_path)
  TRUE
}, error = function(e) {
  cat("parse error:", conditionMessage(e), "\n")
  FALSE
})
check("ynow_about_ui.R parses", parse_ok)

# Source into an isolated env (tags/htmltools stubs for structure only).
env <- new.env(parent = baseenv())
env$tags <- list(
  div = function(...) NULL,
  img = function(...) NULL,
  ul = function(...) NULL,
  li = function(...) NULL,
  h2 = function(...) NULL,
  h3 = function(...) NULL,
  h4 = function(...) NULL,
  p = function(...) NULL,
  b = function(...) NULL,
  a = function(...) NULL,
  span = function(...) NULL
)
env$fluidRow <- function(...) NULL
env$column <- function(...) NULL
env$tagList <- function(...) NULL
.env_about_locale_stub <- function() {
  list(
    about_data_sources_section_title = "Data sources",
    about_data_sources_lead = "lead",
    about_data_sources_markets_title = "m",
    about_data_sources_markets_body = "mb",
    about_data_sources_rf_title = "r",
    about_data_sources_rf_body = "rb",
    about_data_sources_filings_title = "f",
    about_data_sources_filings_body = "fb",
    about_data_sources_valuation_title = "v",
    about_data_sources_valuation_body = "vb",
    about_data_sources_limits_title = "l",
    about_data_sources_limits_body = "lb",
    legal_section_title = "Legal",
    legal_privacy_title = "Privacy",
    legal_privacy_body = "p",
    legal_ip_title = "IP",
    legal_ip_body = "i",
    legal_risk_title = "Risk",
    legal_risk_body = "r"
  )
}
env$.UI_STRINGS <- list(
  en = .env_about_locale_stub(),
  `zh-TW` = {
    zh <- .env_about_locale_stub()
    zh$about_data_sources_section_title <- "資料來源"
    zh$legal_section_title <- "法律"
    zh$legal_privacy_title <- "隱私"
    zh$legal_ip_title <- "智財"
    zh$legal_risk_title <- "風險"
    zh
  }
)
source_ok <- tryCatch({
  sys.source(about_path, envir = env)
  exists(".legal_notices_ui", envir = env, inherits = FALSE) &&
    exists(".about_data_sources_ui", envir = env, inherits = FALSE) &&
    exists(".about_bilingual_intro_ui", envir = env, inherits = FALSE) &&
    exists(".about_lite_intro_ui", envir = env, inherits = FALSE)
}, error = function(e) {
  cat("source error:", conditionMessage(e), "\n")
  FALSE
})
check("sibling sources About builders into local env", source_ok)

if (fail > 0L) {
  stop(fail, " about-ui seam checks failed")
}
cat("All ynow_about_ui seam checks passed.\n")
