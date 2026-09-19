# Macroeconomic Anchoring g uses Rf with explicit source (no silent fixed Rf=5 as "the" Treasury).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK:", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL:", label, "\n")
  }
}
approx_eq <- function(a, b, tol = 1e-9) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

root <- NULL
for (cand in c(
  normalizePath("..", mustWork = FALSE),
  normalizePath(getwd(), mustWork = FALSE),
  "/Users/lawrencekuo/coding/theYNowApp/app_17.0",
  normalizePath(file.path("..", ".."), mustWork = FALSE)
)) {
  if (nzchar(cand) && file.exists(file.path(cand, "setup.R"))) {
    root <- cand
    break
  }
}
if (is.null(root)) stop("Cannot locate app_17.0/setup.R")


`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

source(file.path(root, "setup.R"), local = TRUE, encoding = "UTF-8")

est_live <- estimate_perpetual_g(
  method = "macro",
  rf_pct = 4.87,
  rf_source = "live",
  rf_label = "美國 10 年期公債（Yahoo ^TNX，即時）",
  locale = "zh-TW"
)
check("live Macro g equals Rf", approx_eq(est_live$g_pct, 4.87))
check("live Macro reason mentions live scrape", grepl("即時抓取", est_live$reason, fixed = TRUE))
check("live Macro reason includes Rf value", grepl("4\\.87", est_live$reason))
check("live Macro rf_source echoed", identical(est_live$rf_source, "live"))

est_fb <- estimate_perpetual_g(
  method = "macro",
  rf_pct = 5,
  rf_source = "fallback",
  rf_label = "Yahoo ^TNX",
  locale = "en"
)
check("fallback Macro labels engineering fallback", grepl("fallback", est_fb$reason, ignore.case = TRUE))
check("fallback Macro says not live yield", grepl("not a live yield", est_fb$reason, fixed = TRUE))
check("fallback Macro g is 5", approx_eq(est_fb$g_pct, 5))

est_lk <- estimate_perpetual_g(
  method = "macro",
  rf_pct = 4.55,
  rf_source = "last_known",
  rf_label = "^TNX",
  locale = "en"
)
check("last_known Macro mentions last successful", grepl("last successful", est_lk$reason, fixed = TRUE))
check("last_known Macro g", approx_eq(est_lk$g_pct, 4.55))

est_na <- estimate_perpetual_g(method = "macro", rf_pct = NA_real_, locale = "en")
check("NA rf uses 5 last resort", approx_eq(est_na$g_pct, 5))
check("NA rf reason is fallback", grepl("fallback", est_na$reason, ignore.case = TRUE))

if (file.exists(file.path(root, "market_profile.R"))) {
  source(file.path(root, "market_profile.R"), local = TRUE, encoding = "UTF-8")
  check("US rf_fallback is 5 last resort", approx_eq(market_profile("US")$rf_fallback, 5))
  check("US note mentions live ^TNX", grepl("\\^TNX", market_profile("US")$data_source_note_zh))
  check("US note mentions fallback 5", grepl("5%", market_profile("US")$data_source_note_zh, fixed = TRUE))
}

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1)
}
cat("All Macro Rf / g checks passed.\n")
