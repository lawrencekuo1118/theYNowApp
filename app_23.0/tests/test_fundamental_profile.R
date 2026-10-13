#!/usr/bin/env Rscript
# Fundamental-profile classifier + focus-metric map (no network).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
suppressPackageStartupMessages(library(htmltools))
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "fundamental_profile.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

.mk_stmt <- function(metric, vals, years = NULL) {
  if (is.null(years)) years <- paste0("FY", seq_along(vals))
  df <- as.data.frame(as.list(setNames(as.numeric(vals), years)), check.names = FALSE)
  df <- cbind(Metric = metric, df, stringsAsFactors = FALSE)
  df
}

.stack <- function(...) {
  do.call(rbind, list(...))
}

# --- Focus map coverage ---
for (pid in names(PROFILE_FOCUS_METRICS)) {
  if (identical(pid, "fallback")) {
    check(paste0("focus empty: ", pid), length(profile_focus_metrics(pid)) == 0L)
  } else {
    check(paste0("focus non-empty: ", pid), length(profile_focus_metrics(pid)) >= 3L)
  }
}
check("is_profile_focus growth rev", is_profile_focus_metric("growth", "rev_growth"))
check("is_profile_focus growth pe false", !is_profile_focus_metric("growth", "pe_ratio"))
check("is_profile_focus financial pe", is_profile_focus_metric("financial_book", "pe_ratio"))
check("fs PE map", identical(fs_item_to_focus_metric("PE Ratio (TTM)"), "pe_ratio"))
check("fs Yield map", identical(fs_item_to_focus_metric("Yield"), "dividend_yield"))

# --- Operating statements keep their profile even with a holding/bank label ---
cf_op <- .stack(.mk_stmt("Free Cash Flow", c(10, 11, 12)))
is_op <- .stack(.mk_stmt("Total Revenue", c(100, 110, 120)))
bs_op <- .stack(.mk_stmt("Total Assets", c(200, 210, 220)))
r_label <- classify_fundamental_profile(
  cf_op, is_op, bs_op,
  industry_text = "Conglomerate Holding", industry_choice = "fn.Conglomerate_Holding"
)
check("holding label alone is not holding_asset", !identical(r_label$profile, "holding_asset"))
r_bank_label <- classify_fundamental_profile(
  cf_op, is_op, bs_op,
  industry_text = "Banks - Diversified", industry_choice = "fn.Banking"
)
check("bank label alone is not financial_book", !identical(r_bank_label$profile, "financial_book"))

# --- Asset shell (revenue << equity, FCF not positive) ---
cf_h <- .stack(.mk_stmt("Free Cash Flow", c(-4, -3, -2)))
is_h <- .stack(.mk_stmt("Total Revenue", c(30, 32, 31)))
bs_h <- .stack(
  .mk_stmt("Total Assets", c(900, 880, 860)),
  .mk_stmt("Stockholders Equity", c(700, 690, 680))
)
r_h <- classify_fundamental_profile(cf_h, is_h, bs_h, industry_text = "Conglomerate Holding", industry_choice = "fn.Conglomerate_Holding")
check("holding_asset", identical(r_h$profile, "holding_asset"))
check("holding focuses leverage", "eqt_multiplier" %in% r_h$focus_metrics)
r_h_plain <- classify_fundamental_profile(cf_h, is_h, bs_h, industry_text = "Software", industry_choice = "tech.Software")
check("asset shell is holding_asset without the label", identical(r_h_plain$profile, "holding_asset"))

# --- Financial book: low turnover and high leverage ---
cf_f <- .stack(.mk_stmt("Free Cash Flow", c(8, 7, 9)))
is_f <- .stack(.mk_stmt("Total Revenue", c(80, 78, 76)))
bs_f <- .stack(
  .mk_stmt("Total Assets", c(2000, 1900, 1800)),
  .mk_stmt("Total Debt", c(1200, 1100, 1000)),
  .mk_stmt("Stockholders Equity", c(400, 390, 380))
)
r_f <- classify_fundamental_profile(cf_f, is_f, bs_f, industry_text = "Banks - Diversified", industry_choice = "fn.Banking")
check("financial_book", identical(r_f$profile, "financial_book"))
check("financial focuses ROE", "roe" %in% r_f$focus_metrics)
r_f_plain <- classify_fundamental_profile(cf_f, is_f, bs_f, industry_text = "Software", industry_choice = "tech.Software")
check("book statements are financial_book without the bank label", identical(r_f_plain$profile, "financial_book"))

# --- Growth (high rev CAGR) ---
# YoY from newest→oldest: 150/120-1=25%, 120/90-1=33% → avg ~29%
is_g <- .stack(
  .mk_stmt("Total Revenue", c(150, 120, 90)),
  .mk_stmt("Gross Profit", c(60, 48, 36))
)
cf_g <- .stack(.mk_stmt("Free Cash Flow", c(5, 8, 3)))  # positive mean, somewhat volatile
bs_g <- .stack(
  .mk_stmt("Total Assets", c(100, 90, 80)),
  .mk_stmt("Stockholders Equity", c(50, 45, 40))
)
r_g <- classify_fundamental_profile(cf_g, is_g, bs_g, industry_text = "Software", industry_choice = "tech.Software")
check("growth", identical(r_g$profile, "growth"))
check("growth focuses rev_growth", "rev_growth" %in% r_g$focus_metrics)

# --- Capital intensive ---
is_c <- .stack(
  .mk_stmt("Total Revenue", c(100, 98, 96)),
  .mk_stmt("Gross Profit", c(30, 29, 28))
)
cf_c <- .stack(
  .mk_stmt("Free Cash Flow", c(8, 9, 8)),
  .mk_stmt("Capital Expenditure", c(-20, -19, -18)),  # CapEx/Rev ~0.19
  .mk_stmt("Cash Dividends Paid", c(0, 0, 0))
)
bs_c <- .stack(
  .mk_stmt("Total Assets", c(250, 240, 230)),
  .mk_stmt("Stockholders Equity", c(100, 95, 90))
)
r_c <- classify_fundamental_profile(cf_c, is_c, bs_c, industry_text = "Semiconductors", industry_choice = "sc.Foundry")
check("capital_intensive", identical(r_c$profile, "capital_intensive"))
check("capital focuses roa", "roa" %in% r_c$focus_metrics)

# --- Mature dividend (stable div, low growth) ---
is_m <- .stack(
  .mk_stmt("Total Revenue", c(105, 103, 101)),
  .mk_stmt("Gross Profit", c(40, 39, 38))
)
cf_m <- .stack(
  .mk_stmt("Free Cash Flow", c(2, 12, -5)),  # unstable FCF
  .mk_stmt("Capital Expenditure", c(-3, -3, -3)),
  .mk_stmt("Cash Dividends Paid", c(-10, -10.2, -9.8))  # stable
)
bs_m <- .stack(
  .mk_stmt("Total Assets", c(120, 118, 116)),
  .mk_stmt("Stockholders Equity", c(60, 58, 56))
)
r_m <- classify_fundamental_profile(cf_m, is_m, bs_m, industry_text = "Household Products", industry_choice = "fmcg.Household_Personal")
check("mature_dividend", identical(r_m$profile, "mature_dividend"))
check("mature focuses yield", "dividend_yield" %in% r_m$focus_metrics)

# --- Single stale dividend year must NOT count as stable (1314-like) ---
a_stale <- ynow_assess_dividends(
  c(NA, NA, NA, -1513940000, NA),
  div_current = NA_real_,
  div_cv_max = 0.50,
  min_years = 2L
)
check("stale single-year div is_div", isTRUE(a_stale$is_div))
check("stale single-year not stable", !isTRUE(a_stale$is_div_stable))
check("stale single-year no current", !isTRUE(a_stale$has_current_div))
a_ok <- ynow_assess_dividends(
  c(-10, -10.2, -9.8),
  div_current = -10,
  div_cv_max = 0.50,
  min_years = 2L
)
check("multi-year current div stable", isTRUE(a_ok$is_div_stable))
cf_stale <- .stack(
  .mk_stmt("Free Cash Flow", c(-5, 2, 1)),
  .mk_stmt("Capital Expenditure", c(-3, -3, -3)),
  .mk_stmt("Cash Dividends Paid", c(NA, NA, -10))  # only oldest year
)
is_stale <- .stack(
  .mk_stmt("Total Revenue", c(100, 106, 112)),  # shrinking / soft
  .mk_stmt("Gross Profit", c(-4, 5, 8))
)
bs_stale <- .stack(
  .mk_stmt("Total Assets", c(120, 118, 116)),
  .mk_stmt("Stockholders Equity", c(60, 58, 56))
)
r_stale <- classify_fundamental_profile(
  cf_stale, is_stale, bs_stale,
  industry_text = "Specialty Chemicals", industry_choice = "chem.Specialty"
)
check("stale div not mature_dividend", !identical(r_stale$profile, "mature_dividend"))

# --- Annotation column ---
df_ann <- annotation_kpi_guide_df("sc.Foundry", profile_id = "growth")
check("annotation has 屬性重視", "屬性重視" %in% names(df_ann))
rev_row <- df_ann[df_ann$指標 == "營收成長率", , drop = FALSE]
check("annotation growth marks rev", nrow(rev_row) == 1L && identical(as.character(rev_row[["屬性重視"]][1]), "★"))

# --- Profile badge HTML ---
badge <- .ynow_fund_profile_badge_ui("capital_intensive", "資本密集", title = "重資產")
badge_html <- as.character(badge)
check("badge has class", grepl("ynow-fund-profile-badge", badge_html, fixed = TRUE))
check("badge has data-profile-id", grepl('data-profile-id="capital_intensive"', badge_html, fixed = TRUE))
check("badge shows label", grepl("資本密集", badge_html, fixed = TRUE))
check("badge has no legend dot", !grepl("ynow-focus-metric-dot", badge_html, fixed = TRUE))
badge_g <- .ynow_fund_profile_badge_ui("growth", "High growth")
badge_g_html <- as.character(badge_g)
check("growth badge no dot", !grepl("ynow-focus-metric-dot", badge_g_html, fixed = TRUE))
check("growth badge label", grepl("High growth", badge_g_html, fixed = TRUE))
check("growth badge has flow", grepl("ynow-htcdi-flow", badge_g_html, fixed = TRUE))
badge_md <- .ynow_fund_profile_badge_ui("mature_dividend", "Mature dividend")
badge_md_html <- as.character(badge_md)
check("mature_dividend badge has flow", grepl("ynow-htcdi-flow", badge_md_html, fixed = TRUE))
badge_dl <- .ynow_fund_profile_badge_ui("fallback", "Data-limited")
badge_dl_html <- as.character(badge_dl)
check("fallback badge no flow", !grepl("ynow-htcdi-flow", badge_dl_html, fixed = TRUE))
check("fallback badge Data-limited text", grepl("Data-limited", badge_dl_html, fixed = TRUE))
check("fallback badge keeps chip class", grepl("ynow-fund-profile-badge", badge_dl_html, fixed = TRUE))
badge_dl_zh <- .ynow_fund_profile_badge_ui("fallback", "資料受限")
badge_dl_zh_html <- as.character(badge_dl_zh)
check("fallback zh no flow", !grepl("ynow-htcdi-flow", badge_dl_zh_html, fixed = TRUE))
check("fallback zh 資料受限 text", grepl("資料受限", badge_dl_zh_html, fixed = TRUE))
flow_span <- .ynow_data_limited_flow_span("Data-limited")
check("flow helper skips Data-limited", identical(as.character(flow_span), "Data-limited"))
flow_span_zh <- .ynow_data_limited_flow_span("資料受限")
check("flow helper skips 資料受限", identical(as.character(flow_span_zh), "資料受限"))
flow_growth <- .ynow_data_limited_flow_span("High growth")
check("flow helper wraps growth", grepl("ynow-htcdi-flow", as.character(flow_growth), fixed = TRUE))
flow_md <- .ynow_data_limited_flow_span("Mature dividend")
check("flow helper wraps Mature dividend", grepl("ynow-htcdi-flow", as.character(flow_md), fixed = TRUE))

# Focus mark is no-op; KPI boxes use gold border class instead
check("focus mark returns NULL", is.null(.ynow_focus_metric_mark("tip")))
# Use color="none" so the helper builds the box without shinydashboard::valueBox
box_focus <- kpi_band_value_box("12%", "毛利率", "none", mark_focus = TRUE, focus_title = "focus")
box_focus_html <- as.character(box_focus)
check("focus box has gold-border class", grepl("ynow-focus-metric-box", box_focus_html, fixed = TRUE))
check("focus box has no gold dot", !grepl("ynow-focus-metric-dot", box_focus_html, fixed = TRUE))
box_plain <- kpi_band_value_box("12%", "毛利率", "none", mark_focus = FALSE)
check("plain box no focus class", !grepl("ynow-focus-metric-box", as.character(box_plain), fixed = TRUE))

ui_path <- file.path(app_dir, "ynow_ui.R")
ui_txt <- paste(readLines(ui_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("CSS shared flow class", grepl("ynow-htcdi-flow", ui_txt, fixed = TRUE))
check("CSS logo stops 0C5484", grepl("#0C5484", ui_txt, fixed = TRUE))
check("CSS logo stops 1AA8B8", grepl("#1AA8B8", ui_txt, fixed = TRUE))
check("CSS logo stops 249C60", grepl("#249C60", ui_txt, fixed = TRUE))
check("CSS background-clip text", grepl("background-clip: text", ui_txt, fixed = TRUE))
kf <- gregexpr("@keyframes ynow-logo-flow", ui_txt, fixed = TRUE)[[1]]
check("single logo-flow keyframes", length(kf) == 1L && kf[[1]] > 0)
check("badge flow override not second animation", grepl(".ynow-fund-profile-badge .ynow-htcdi-flow", ui_txt, fixed = TRUE))
check("Rf still excluded from flow", grepl(".ynow-macro-kpi--rf .ynow-htcdi-flow", ui_txt, fixed = TRUE))
check("CSS gold border on focus KPI", grepl(".small-box.ynow-focus-metric-box", ui_txt, fixed = TRUE))
check("CSS focus gold thicker border", grepl("border: 3px solid var(--ynow-gold", ui_txt, fixed = TRUE))
check("CSS focus gold pulse keyframes", grepl("@keyframes ynow-focus-gold-pulse", ui_txt, fixed = TRUE) &&
        grepl("animation: ynow-focus-gold-pulse", ui_txt, fixed = TRUE))
check("legend uses border swatch", grepl("ynow-focus-metric-swatch", ui_txt, fixed = TRUE))
# width=NULL returns bare .small-box (no Bootstrap col wrapper)
box_null <- kpi_band_value_box("12%", "毛利率", "none", width = NULL, mark_focus = TRUE)
box_null_html <- as.character(box_null)
check("width=NULL focus is bare small-box", grepl("small-box", box_null_html, fixed = TRUE) &&
        !grepl("col-sm-", box_null_html, fixed = TRUE))
check("kpi_module passes width=NULL", {
  km <- paste(readLines(file.path(app_dir, "kpi_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("width = NULL", km, fixed = TRUE) && grepl("\\.unwrap_kpi_box", km)
})

for (fn in c("fundamental_profile.R", "ynow_ui.R", "ynow_server.R", "lab_clustering.R")) {
  parsed <- tryCatch({
    parse(file.path(app_dir, fn), keep.source = FALSE)
    TRUE
  }, error = function(e) {
    cat("PARSE ", fn, ": ", e$message, "\n")
    FALSE
  })
  check(paste("parse", fn), isTRUE(parsed))
}

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All fundamental-profile checks passed.\n")
