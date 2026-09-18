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

# --- Holding ---
cf_h <- .stack(.mk_stmt("Free Cash Flow", c(10, 11, 12)))
is_h <- .stack(.mk_stmt("Total Revenue", c(100, 110, 120)))
bs_h <- .stack(.mk_stmt("Total Assets", c(200, 210, 220)))
r_h <- classify_fundamental_profile(cf_h, is_h, bs_h, industry_text = "Conglomerate Holding", industry_choice = "fn.Conglomerate_Holding")
check("holding_asset", identical(r_h$profile, "holding_asset"))
check("holding focuses leverage", "eqt_multiplier" %in% r_h$focus_metrics)

# --- Financial book ---
r_f <- classify_fundamental_profile(cf_h, is_h, bs_h, industry_text = "Banks - Diversified", industry_choice = "fn.Banking")
check("financial_book", identical(r_f$profile, "financial_book"))
check("financial focuses ROE", "roe" %in% r_f$focus_metrics)

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

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All fundamental-profile checks passed.\n")
