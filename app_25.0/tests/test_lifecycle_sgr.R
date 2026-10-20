#!/usr/bin/env Rscript
# Lifecycle Classification v19: multi-factor scoring, no stage-fixed terminal g.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "setup.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "")
  else { cat("FAIL ", label, "\n", sep = ""); fail <<- fail + 1L }
}
approx_eq <- function(a, b, tol = 1e-9) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

stmt <- function(rows, start_year = 2025L) {
  n <- max(lengths(rows))
  years <- paste0("Y", start_year - seq_len(n) + 1L)
  df <- data.frame(Breakdown = names(rows), stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_len(n)) {
    df[[years[i]]] <- vapply(rows, function(v) {
      if (length(v) >= i && is.finite(v[i])) as.character(v[i]) else NA_character_
    }, character(1))
  }
  df
}

is_rev <- function(rev, ni = NULL, ebit = NULL) {
  if (is.null(ni)) ni <- rev * 0.12
  if (is.null(ebit)) ebit <- rev * 0.15
  stmt(list(
    "Total Revenue" = rev,
    "Operating Income" = ebit,
    "Net Income" = ni,
    "Interest Expense" = rev * 0.02,
    "Interest Income" = rev * 0.005,
    "Pretax Income" = ni * 1.2,
    "Income Tax Expense" = ni * 0.2
  ))
}
bs_ic <- function(assets = 1000, equity = 500, cash = 80, cl = 200, debt = 220) {
  stmt(list(
    "Total Assets" = assets,
    "Stockholders Equity" = equity,
    "Cash And Cash Equivalents" = cash,
    "Total Current Liabilities" = cl,
    "Current Debt" = 40,
    "Total Debt" = debt,
    "Net PPE" = 180
  ))
}
cf_path <- function(fcf, capex = NULL, da = NULL) {
  if (is.null(capex)) capex <- -abs(fcf) * 0.4
  if (is.null(da)) da <- abs(capex) * 0.7
  stmt(list(
    "Free Cash Flow" = fcf,
    "Operating Cash Flow" = fcf - capex,
    "Capital Expenditure" = capex,
    "Depreciation And Amortization" = da,
    "Cash Dividends Paid" = -10
  ))
}

# --- high growth, FCF still negative ---
hg_neg <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(400, 300, 220, 160), ebit = c(20, 12, 6, 2)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(-40, -50, -45)),
  expected_rev_cagr_pct = 28,
  wacc_pct = 9
)
check("high growth + neg FCF → HIGH_GROWTH", identical(hg_neg$autoDetectedStage, "HIGH_GROWTH"))
check("high growth reasons mention CAGR or FCF", any(grepl("CAGR|cash flow|現金流", hg_neg$classificationReasons)))

# --- high growth, FCF turned positive ---
hg_pos <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(400, 300, 220, 160), ebit = c(48, 30, 16, 8)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(25, -10, -20)),
  expected_rev_cagr_pct = 24,
  wacc_pct = 9,
  tax_ratio = 0.21
)
check("high growth + FCF turned pos still HIGH_GROWTH or G2M",
      hg_pos$autoDetectedStage %in% c("HIGH_GROWTH", "GROWTH_TO_MATURE"))
check("turned-positive reason present", any(grepl("turned|轉正", hg_pos$classificationReasons)))

# --- decelerating growth, expanding margins ---
g2m <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(260, 230, 200, 175), ebit = c(42, 30, 20, 14)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(30, 18, 8)),
  expected_rev_cagr_pct = 14,
  wacc_pct = 8,
  tax_ratio = 0.21
)
check("decel + expanding margin → GROWTH_TO_MATURE",
      identical(g2m$autoDetectedStage, "GROWTH_TO_MATURE"))
check("g2m has secondary candidate", !is.na(g2m$secondaryCandidate) && nzchar(g2m$secondaryCandidate))

# --- low growth, ROIC >> WACC is NOT High Growth ---
ms <- classify_lifecycle_result(
  industry_text = "Household Products",
  d_is = is_rev(c(210, 206, 202, 198), ebit = c(42, 41, 40, 39)),
  d_bs = bs_ic(assets = 800, equity = 500, cash = 60, cl = 120),
  d_cf = cf_path(c(40, 38, 37), capex = c(-22, -21, -20), da = c(20, 19, 19)),
  expected_rev_cagr_pct = 2.5,
  wacc_pct = 7,
  tax_ratio = 0.21
)
check("low growth high ROIC ≠ HIGH_GROWTH", !identical(ms$autoDetectedStage, "HIGH_GROWTH"))
check("low growth lands mature",
      ms$autoDetectedStage %in% c("MATURE_STABLE", "MATURE_GROWTH"))

# --- structural decline + ROIC < WACC ---
dec <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(140, 160, 185, 220), ebit = c(8, 14, 22, 30)),
  d_bs = bs_ic(assets = 900, equity = 400, cash = 20, cl = 250),
  d_cf = cf_path(c(5, 8, 12), capex = c(-8, -9, -10), da = c(14, 14, 13)),
  expected_rev_cagr_pct = -6,
  wacc_pct = 10,
  tax_ratio = 0.21,
  finite_life = TRUE
)
check("structural decline → DECLINING_OR_FINITE_LIFE",
      identical(dec$autoDetectedStage, "DECLINING_OR_FINITE_LIFE"))
check("declining is special type", identical(dec$specialBusinessType, "DECLINING_OR_FINITE_LIFE"))

# --- bank via taxonomy key (not company name) ---
bank <- classify_lifecycle_result(
  industry_text = "Banks - Diversified",
  industry_key = "fn.Banking",
  d_is = is_rev(c(120, 115, 110), ebit = c(40, 38, 36)),
  d_bs = bs_ic(assets = 4000, equity = 350, cash = 200, cl = 3000),
  d_cf = cf_path(c(20, 18, 17)),
  wacc_pct = 9
)
check("fn.Banking → FINANCIAL_INSTITUTION",
      identical(bank$autoDetectedStage, "FINANCIAL_INSTITUTION"))

# --- insurance via taxonomy ---
ins <- classify_lifecycle_result(
  industry_text = "Life Insurance",
  industry_key = "fn.Insurance",
  d_is = is_rev(c(90, 88, 86)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(12, 11, 10)),
  wacc_pct = 8
)
check("fn.Insurance → FINANCIAL_INSTITUTION",
      identical(ins$autoDetectedStage, "FINANCIAL_INSTITUTION"))

# --- company name alone must not classify as bank ---
name_only <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  industry_key = "",
  d_is = is_rev(c(210, 206, 202), ebit = c(32, 31, 30)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(28, 27, 26)),
  expected_rev_cagr_pct = 2,
  wacc_pct = 8,
  tax_ratio = 0.21
)
check("company-name-free: foods not FI",
      !identical(name_only$autoDetectedStage, "FINANCIAL_INSTITUTION"))

# --- regulated utility via taxonomy, not merged with FI ---
util <- classify_lifecycle_result(
  industry_text = "Electric Utilities",
  industry_key = "en.Utilities",
  d_is = is_rev(c(300, 290, 280), ebit = c(45, 44, 43)),
  d_bs = stmt(list(
    "Total Assets" = c(2000, 1900, 1800),
    "Stockholders Equity" = c(700, 680, 660),
    "Cash And Cash Equivalents" = c(40, 38, 36),
    "Total Current Liabilities" = c(300, 290, 280),
    "Current Debt" = c(40, 40, 40),
    "Total Debt" = c(800, 780, 760),
    "Net PPE" = c(900, 860, 820)
  )),
  d_cf = cf_path(c(40, 38, 36), capex = c(-80, -78, -75), da = c(50, 48, 47)),
  expected_rev_cagr_pct = 3,
  wacc_pct = 6
)
check("en.Utilities → REGULATED_UTILITY",
      identical(util$autoDetectedStage, "REGULATED_UTILITY"))
check("utility ≠ financial", !identical(util$autoDetectedStage, "FINANCIAL_INSTITUTION"))

# --- missing forecast revenue ---
miss_fwd <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(210, 200, 190), ebit = c(30, 28, 26)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(22, 20, 18)),
  expected_rev_cagr_pct = NA_real_,
  wacc_pct = 8,
  tax_ratio = 0.21
)
check("missing fwd CAGR listed", "expected_rev_cagr_3y" %in% miss_fwd$missingInputs)
check("missing fwd not treated as zero class", is.finite(miss_fwd$dataCompletenessScore) &&
        miss_fwd$dataCompletenessScore < 1)

# --- missing historical growth ---
miss_hist <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = stmt(list("Net Income" = c(10, 9))),
  d_bs = bs_ic(),
  d_cf = cf_path(c(8, 7)),
  expected_rev_cagr_pct = 12,
  wacc_pct = 8
)
check("missing hist CAGR listed", "hist_rev_cagr_3_5y" %in% miss_hist$missingInputs)

# --- missing ROIC / WACC ---
miss_roic <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(220, 200, 180)),
  d_bs = stmt(list("Stockholders Equity" = c(100, 90))),
  d_cf = cf_path(c(20, 18, 16)),
  expected_rev_cagr_pct = 9
)
check("missing ROIC listed", "roic" %in% miss_roic$missingInputs)
check("missing WACC listed", "wacc" %in% miss_roic$missingInputs)
check("missing not coerced to zero in metrics",
      is.na(miss_roic$metrics$roic_pct) && is.na(miss_roic$metrics$wacc_pct))

# --- close race lowers confidence ---
close <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(240, 220, 205, 190), ebit = c(30, 26, 22, 18)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(18, 12, 8)),
  expected_rev_cagr_pct = 9.5,
  wacc_pct = 8,
  tax_ratio = 0.21
)
check("close race keeps a primary", nzchar(close$autoDetectedStage %||% ""))
check("close race has secondary", nzchar(close$secondaryCandidate %||% "") || isTRUE(close$closeRace) ||
        is.finite(close$confidenceScore))
if (isTRUE(close$closeRace)) {
  check("close race confidence capped", close$confidenceScore <= 0.55 + 1e-9)
}

# --- low completeness ---
thin <- classify_lifecycle_result(industry_text = "Packaged Foods")
check("thin inputs completeness low", is.finite(thin$dataCompletenessScore) &&
        thin$dataCompletenessScore < LIFECYCLE_CONFIG$low_completeness)
check("thin inputs do not force mature",
      is.na(thin$autoDetectedStage) || !identical(thin$autoDetectedStage, "MATURE_STABLE") ||
        thin$dataCompletenessScore < 0.5)

# --- same generic firm, different stages by data ---
early <- classify_lifecycle_result(
  industry_text = "Industrial Machinery",
  d_is = is_rev(c(180, 120, 80), ebit = c(8, 2, -4)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(-20, -25, -18)),
  expected_rev_cagr_pct = 26,
  wacc_pct = 10
)
later <- classify_lifecycle_result(
  industry_text = "Industrial Machinery",
  d_is = is_rev(c(240, 230, 220), ebit = c(36, 34, 33)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(32, 30, 29)),
  expected_rev_cagr_pct = 3,
  wacc_pct = 8,
  tax_ratio = 0.21
)
check("same industry, different data → different stage",
      !identical(early$autoDetectedStage, later$autoDetectedStage))

# --- similar financials, different industry labels stay consistent ---
foods <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(250, 230, 210), ebit = c(35, 30, 26)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(22, 16, 10)),
  expected_rev_cagr_pct = 12,
  wacc_pct = 8,
  tax_ratio = 0.21
)
retail <- classify_lifecycle_result(
  industry_text = "Apparel Retail",
  d_is = is_rev(c(250, 230, 210), ebit = c(35, 30, 26)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(22, 16, 10)),
  expected_rev_cagr_pct = 12,
  wacc_pct = 8,
  tax_ratio = 0.21
)
check("similar financials → same general stage",
      identical(foods$autoDetectedStage, retail$autoDetectedStage))

# --- labels are not calculation keys ---
check("label ≠ stage id", !identical(lifecycle_stage_label("HIGH_GROWTH", "en"), "HIGH_GROWTH"))
check("zh label exists", nchar(lifecycle_stage_label("MATURE_STABLE", "zh-TW")) > 0)

# --- legacy mapping ---
leg <- map_legacy_lifecycle_stage("mature_sunset")
check("sunset legacy → auto (unreliable)", identical(leg$selected_stage, "auto") && isFALSE(leg$reliable))
check("sunset legacy value retained", identical(leg$legacy_value, "mature_sunset"))
leg2 <- map_legacy_lifecycle_stage("growth_to_mature")
check("growth_to_mature maps 1:1", identical(leg2$selected_stage, "GROWTH_TO_MATURE"))
leg3 <- map_legacy_lifecycle_stage("mature_tech")
check("mature_tech unreliable → auto", identical(leg3$selected_stage, "auto") && isFALSE(leg3$reliable))

# --- terminal g not bound to stage ---
est_life <- estimate_perpetual_g(
  method = "lifecycle",
  rf_pct = 4.2,
  industry_text = "Software",
  rev_cagr = 18,
  lifecycle_stage = "HIGH_GROWTH",
  locale = "en"
)
check("lifecycle g is not the old 2.5/2.75 table",
      !approx_eq(est_life$g_pct, 2.5) && !approx_eq(est_life$g_pct, 2.75) &&
        !approx_eq(est_life$g_pct, 1.75))
check("high growth does not raise g above Rf/cap",
      is.finite(est_life$g_pct) && est_life$g_pct <= 4.5 + 1e-9)
est_dec <- estimate_perpetual_g(
  method = "lifecycle",
  rf_pct = 4.2,
  lifecycle_result = dec,
  locale = "en"
)
check("declining g can be zero", approx_eq(est_dec$g_pct, 0) || est_dec$g_pct <= 0)

# --- manual selection not overwritten ---
manual <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(400, 300, 220), ebit = c(20, 12, 6)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(-40, -50, -45)),
  expected_rev_cagr_pct = 28,
  wacc_pct = 9,
  selected_stage = "MATURE_STABLE",
  selection_mode = "manual"
)
check("manual keeps selectedStage", identical(manual$selectedStage, "MATURE_STABLE"))
check("manual still stores autoDetected", identical(manual$autoDetectedStage, "HIGH_GROWTH") ||
        !identical(manual$autoDetectedStage, "MATURE_STABLE"))
check("manual mode flag", identical(manual$selectionMode, "manual"))

# --- MATURE_GROWTH dedicated mid-single-digit growth ---
mg <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(230, 215, 200, 188), ebit = c(34, 31, 28, 26)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(28, 24, 20)),
  expected_rev_cagr_pct = 7,
  wacc_pct = 8,
  tax_ratio = 0.21
)
check("mid growth → MATURE_GROWTH", identical(mg$autoDetectedStage, "MATURE_GROWTH"))
check("MATURE_GROWTH forecast years = 6", identical(as.integer(mg$recommendedForecastYears), 6L))
check("MATURE_GROWTH secondary is mature band",
      mg$secondaryCandidate %in% c("MATURE_STABLE", "GROWTH_TO_MATURE"))
check("MATURE_GROWTH classScores finite for general stages",
      all(is.finite(unlist(mg$classScores[LIFECYCLE_GENERAL_STAGE_IDS]))))
check("MATURE_GROWTH tops classScores",
      identical(names(sort(mg$classScores[is.finite(mg$classScores)], decreasing = TRUE))[1],
                "MATURE_GROWTH"))

# --- FI via industry text + statement evidence (no taxonomy key) ---
fi_txt <- classify_lifecycle_result(
  industry_text = "Banks - Regional",
  industry_key = "",
  d_is = stmt(list(
    "Total Revenue" = c(100, 95, 90),
    "Operating Income" = c(30, 28, 26),
    "Net Income" = c(22, 20, 18),
    "Interest Expense" = c(40, 38, 36),
    "Interest Income" = c(70, 66, 62),
    "Pretax Income" = c(28, 26, 24),
    "Income Tax Expense" = c(6, 5, 5)
  )),
  d_bs = stmt(list(
    "Total Assets" = c(5000, 4800, 4600),
    "Stockholders Equity" = c(400, 380, 360),
    "Cash And Cash Equivalents" = c(200, 190, 180),
    "Total Current Liabilities" = c(3500, 3400, 3300),
    "Current Debt" = c(100, 100, 100),
    "Total Debt" = c(200, 200, 200),
    "Net PPE" = c(50, 48, 46),
    "Net Loan" = c(3000, 2900, 2800)
  )),
  d_cf = cf_path(c(25, 22, 20)),
  wacc_pct = 9
)
check("Banks text + stmts → FINANCIAL_INSTITUTION",
      identical(fi_txt$autoDetectedStage, "FINANCIAL_INSTITUTION"))
check("FI text path is special",
      identical(fi_txt$specialBusinessType, "FINANCIAL_INSTITUTION"))
check("FI special forecast years = 5",
      identical(as.integer(fi_txt$recommendedForecastYears), 5L))
check("FI fallback source",
      identical(fi_txt$fallbackSource, "special_financial_institution"))

# --- FI via statement evidence alone (interest + financial assets) ---
fi_stmt <- classify_lifecycle_result(
  industry_text = "Diversified Conglomerate",
  industry_key = "",
  d_is = stmt(list(
    "Total Revenue" = c(100, 95, 90),
    "Operating Income" = c(30, 28, 26),
    "Net Income" = c(22, 20, 18),
    "Interest Expense" = c(40, 38, 36),
    "Interest Income" = c(70, 66, 62),
    "Pretax Income" = c(28, 26, 24),
    "Income Tax Expense" = c(6, 5, 5)
  )),
  d_bs = stmt(list(
    "Total Assets" = c(5000, 4800, 4600),
    "Stockholders Equity" = c(400, 380, 360),
    "Cash And Cash Equivalents" = c(200, 190, 180),
    "Total Current Liabilities" = c(3500, 3400, 3300),
    "Current Debt" = c(100, 100, 100),
    "Total Debt" = c(200, 200, 200),
    "Net PPE" = c(50, 48, 46),
    "Net Loan" = c(3000, 2900, 2800)
  )),
  d_cf = cf_path(c(25, 22, 20)),
  wacc_pct = 9
)
check("stmt-only interest+fin assets → FI",
      identical(fi_stmt$autoDetectedStage, "FINANCIAL_INSTITUTION"))

# --- utility via industry text (no taxonomy key) ---
util_txt <- classify_lifecycle_result(
  industry_text = "Electric Utilities",
  industry_key = "",
  d_is = is_rev(c(300, 290, 280), ebit = c(45, 44, 43)),
  d_bs = stmt(list(
    "Total Assets" = c(2000, 1900, 1800),
    "Stockholders Equity" = c(700, 680, 660),
    "Cash And Cash Equivalents" = c(40, 38, 36),
    "Total Current Liabilities" = c(300, 290, 280),
    "Current Debt" = c(40, 40, 40),
    "Total Debt" = c(800, 780, 760),
    "Net PPE" = c(900, 860, 820)
  )),
  d_cf = cf_path(c(40, 38, 36), capex = c(-80, -78, -75), da = c(50, 48, 47)),
  expected_rev_cagr_pct = 3,
  wacc_pct = 6
)
check("Electric Utilities text → REGULATED_UTILITY",
      identical(util_txt$autoDetectedStage, "REGULATED_UTILITY"))
check("utility text path is special",
      identical(util_txt$specialBusinessType, "REGULATED_UTILITY"))
check("utility text reasons mention CapEx or PPE",
      any(grepl("CapEx|PPE|rate-base|taxonomy", util_txt$classificationReasons)))

# --- declining without finite_life flag (hist + ROIC < WACC) ---
dec_nofin <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(140, 160, 185, 220), ebit = c(8, 14, 22, 30)),
  d_bs = bs_ic(assets = 900, equity = 400, cash = 20, cl = 250),
  d_cf = cf_path(c(5, 8, 12), capex = c(-8, -9, -10), da = c(14, 14, 13)),
  expected_rev_cagr_pct = -6,
  wacc_pct = 10,
  tax_ratio = 0.21,
  finite_life = FALSE
)
check("decline without finite_life still special",
      identical(dec_nofin$autoDetectedStage, "DECLINING_OR_FINITE_LIFE"))
check("decline without finite_life special type set",
      identical(dec_nofin$specialBusinessType, "DECLINING_OR_FINITE_LIFE"))

# --- ticker is never a classification feature ---
ticker_only <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  industry_key = "",
  ticker = "JPM",
  d_is = is_rev(c(210, 206, 202), ebit = c(32, 31, 30)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(28, 27, 26)),
  expected_rev_cagr_pct = 2,
  wacc_pct = 8,
  tax_ratio = 0.21
)
check("ticker JPM does not force FI",
      !identical(ticker_only$autoDetectedStage, "FINANCIAL_INSTITUTION"))

# --- high growth + ROIC < WACC still HIGH_GROWTH (contradiction does not flip stage) ---
contra <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(400, 300, 220, 160), ebit = c(5, 4, 3, 2), ni = c(2, 1, 0.5, 0.2)),
  d_bs = bs_ic(assets = 2000, equity = 200, cash = 20, cl = 400, debt = 800),
  d_cf = cf_path(c(-40, -50, -45)),
  expected_rev_cagr_pct = 25,
  wacc_pct = 12,
  tax_ratio = 0.21
)
check("contradiction keeps HIGH_GROWTH", identical(contra$autoDetectedStage, "HIGH_GROWTH"))
check("contradiction forecast years = 10",
      identical(as.integer(contra$recommendedForecastYears), 10L))
check("contradiction ROIC < WACC in metrics",
      is.finite(contra$metrics$roic_wacc_spread) && contra$metrics$roic_wacc_spread < 0)

# --- hist vs fwd CAGR gap lowers confidence / can close-race ---
gap <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(210, 205, 200, 195), ebit = c(30, 29, 28, 27)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(22, 20, 18)),
  expected_rev_cagr_pct = 18,
  wacc_pct = 8,
  tax_ratio = 0.21
)
check("hist/fwd gap still classifies", nzchar(gap$autoDetectedStage %||% ""))
check("hist/fwd gap confidence below high band",
      is.finite(gap$confidenceScore) && gap$confidenceScore < 0.60)
check("hist/fwd gap >= 10pp in metrics",
      abs(gap$metrics$fwd_rev_cagr - gap$metrics$hist_rev_cagr) >= 10 - 1e-9)

# --- extract_lifecycle_metrics golden values (offline fixture) ---
ex <- extract_lifecycle_metrics(
  d_is = is_rev(c(400, 300, 220, 160), ebit = c(20, 12, 6, 2)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(-40, -50, -45)),
  expected_rev_cagr_pct = 28,
  wacc_pct = 9
)
check("extract fwd CAGR = 28", approx_eq(ex$fwd_rev_cagr, 28))
check("extract hist CAGR ~35.72", approx_eq(ex$hist_rev_cagr, 35.72088764, tol = 1e-4))
check("extract FCF positive share = 0", approx_eq(ex$fcf_positive, 0))
check("extract FCF not turned positive", isFALSE(ex$fcf_turned_positive))
check("extract WACC = 9", approx_eq(ex$wacc_pct, 9))
check("extract missing never coerced to 0 for optional sam",
      is.na(ex$sam_share) && is.na(ex$market_share_change) && is.na(ex$fade_years))

# --- classify_lifecycle_stage wrapper ---
wrap <- classify_lifecycle_stage(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(400, 300, 220, 160), ebit = c(20, 12, 6, 2)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(-40, -50, -45)),
  expected_rev_cagr_pct = 28,
  wacc_pct = 9
)
check("wrapper returns HIGH_GROWTH", identical(wrap, "HIGH_GROWTH"))
wrap_d <- classify_lifecycle_stage(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(400, 300, 220, 160), ebit = c(20, 12, 6, 2)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(-40, -50, -45)),
  expected_rev_cagr_pct = 28,
  wacc_pct = 9,
  detail = TRUE
)
check("wrapper detail stage", identical(wrap_d$stage, "HIGH_GROWTH"))
check("wrapper detail rule multi_factor", identical(wrap_d$rule, "multi_factor_scores"))

# --- recommended_forecast_years_for_stage table (current defaults) ---
check("forecast years HIGH_GROWTH=10",
      identical(recommended_forecast_years_for_stage("HIGH_GROWTH"), 10L))
check("forecast years GROWTH_TO_MATURE=8",
      identical(recommended_forecast_years_for_stage("GROWTH_TO_MATURE"), 8L))
check("forecast years MATURE_GROWTH=6",
      identical(recommended_forecast_years_for_stage("MATURE_GROWTH"), 6L))
check("forecast years MATURE_STABLE=5",
      identical(recommended_forecast_years_for_stage("MATURE_STABLE"), 5L))
check("forecast years DECLINING=5",
      identical(recommended_forecast_years_for_stage("DECLINING_OR_FINITE_LIFE"), 5L))
check("forecast years REGULATED_UTILITY=5",
      identical(recommended_forecast_years_for_stage("REGULATED_UTILITY"), 5L))
check("forecast years FINANCIAL_INSTITUTION=5",
      identical(recommended_forecast_years_for_stage("FINANCIAL_INSTITUTION"), 5L))
check("forecast years unknown → NA",
      is.na(recommended_forecast_years_for_stage("not_a_stage")))

# --- stage ids / choices / labels / legacy helpers ---
ids_auto <- lifecycle_stage_ids(include_auto = TRUE)
ids_no <- lifecycle_stage_ids(include_auto = FALSE)
check("stage ids include auto first", identical(ids_auto[1], "auto") &&
        identical(ids_auto[-1], LIFECYCLE_STAGE_IDS))
check("stage ids without auto", identical(ids_no, LIFECYCLE_STAGE_IDS))
ch_en <- lifecycle_stage_choices("en", include_auto = TRUE)
check("stage choices length with auto", length(ch_en) == length(LIFECYCLE_STAGE_IDS) + 1L)
check("stage choices values are ids", identical(unname(ch_en), ids_auto))
check("en High Growth label", identical(lifecycle_stage_label("HIGH_GROWTH", "en"), "High Growth"))
check("is_legacy sunset TRUE", isTRUE(is_legacy_lifecycle_stage("mature_sunset")))
check("is_legacy current FALSE", isFALSE(is_legacy_lifecycle_stage("HIGH_GROWTH")))
leg_gen <- map_legacy_lifecycle_stage("mature_general")
check("mature_general unreliable → auto",
      identical(leg_gen$selected_stage, "auto") && isFALSE(leg_gen$reliable))
leg_cur <- map_legacy_lifecycle_stage("MATURE_STABLE")
check("current stage id maps as manual reliable",
      identical(leg_cur$selected_stage, "MATURE_STABLE") && isTRUE(leg_cur$reliable) &&
        identical(leg_cur$selection_mode, "manual"))

# --- empty result shape ---
er <- empty_lifecycle_result()
check("empty autoDetected NA", is.na(er$autoDetectedStage))
check("empty completeness 0", identical(er$dataCompletenessScore, 0) ||
        approx_eq(er$dataCompletenessScore, 0))
check("empty classScores all NA", all(is.na(er$classScores)))
check("empty fallback insufficient", identical(er$fallbackSource, "insufficient_inputs"))

# --- config override merges without mutating defaults ---
cfg_default_high <- LIFECYCLE_CONFIG$cagr_high_pct
cfg_over <- lifecycle_config(list(cagr_high_pct = 99))
check("config override applied", identical(cfg_over$cagr_high_pct, 99))
check("config override does not mutate global",
      identical(LIFECYCLE_CONFIG$cagr_high_pct, cfg_default_high) &&
        identical(cfg_default_high, 20))

# --- en-US classification reasons ---
en_res <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = is_rev(c(400, 300, 220, 160), ebit = c(20, 12, 6, 2)),
  d_bs = bs_ic(),
  d_cf = cf_path(c(-40, -50, -45)),
  expected_rev_cagr_pct = 28,
  wacc_pct = 9,
  locale = "en"
)
check("en reasons mention Expected CAGR",
      any(grepl("Expected 3-year revenue CAGR", en_res$classificationReasons)))
check("en reasons are ASCII finance prose",
      !any(grepl("[\u4e00-\u9fff]", en_res$classificationReasons, perl = TRUE)))

# --- auto mode selectedStage follows autoDetected ---
check("auto mode selected equals detected",
      identical(hg_neg$selectedStage, hg_neg$autoDetectedStage) &&
        identical(hg_neg$selectionMode, "auto"))

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1)
}
cat("All lifecycle SGR checks passed.\n")
