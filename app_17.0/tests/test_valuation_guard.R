#!/usr/bin/env Rscript
# Synthetic valuation guards. No ticker is the acceptance case.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "ui_locale.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

stmt <- function(rows, start_year = 2024L) {
  n <- max(lengths(rows))
  years <- as.character(start_year - seq_len(n) + 1L)
  df <- data.frame(Metric = names(rows), stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_len(n)) {
    df[[years[i]]] <- vapply(rows, function(v) {
      if (length(v) >= i && is.finite(v[i])) v[i] else NA_real_
    }, numeric(1))
  }
  df
}

base_is <- function(ni = c(80, 76), rev = c(400, 380), interest = c(8, 8),
                    pretax = c(100, 95), tax = c(20, 19)) {
  stmt(list(
    "Net Income" = ni,
    "Total Revenue" = rev,
    "Interest Expense" = interest,
    "Pretax Income" = pretax,
    "Income Tax Expense" = tax
  ))
}
base_bs <- function(equity = c(500, 480), debt = c(100, 100), assets = c(900, 860),
                    goodwill = c(20, 20), shares = c(100, 100)) {
  stmt(list(
    "Common Stock Equity" = equity,
    "Total Debt" = debt,
    "Total Assets" = assets,
    "Goodwill" = goodwill,
    "Ordinary Shares Number" = shares
  ))
}
base_cf <- function(cfo = c(120, 110), capex = c(-30, -28), div = c(-20, -20)) {
  stmt(list(
    "Operating Cash Flow" = cfo,
    "Capital Expenditure" = capex,
    "Free Cash Flow" = cfo + capex,
    "Cash Dividends Paid" = div
  ))
}

# 1 stable low-leverage mature
rec1 <- recommend_valuation_models(base_cf(), "Packaged Foods", base_is(), base_bs(), industry_choice = "")
check("stable primary dcf", identical(rec1$primary, "dcf"))
check("stable weights not forced equal", {
  is.null(rec1$secondary) || !is.finite(rec1$weight_secondary) ||
    abs(rec1$weight_primary - rec1$weight_secondary) > 0.02
})

# 2 high growth positive FCF
rec2 <- recommend_valuation_models(
  base_cf(cfo = c(90, 40), capex = c(-20, -10)),
  "Packaged Foods",
  base_is(ni = c(40, 20), rev = c(200, 120)),
  base_bs(equity = c(300, 250))
)
check("growth still scores a model", !is.null(rec2$primary))

# 3 high growth negative FCF
rec3 <- recommend_valuation_models(
  base_cf(cfo = c(30, 20), capex = c(-80, -70)),
  "Software",
  base_is(ni = c(-10, -20), rev = c(180, 100), pretax = c(-8, -16), tax = c(0, 0)),
  base_bs()
)
check("neg fcf software not forced pb primary", !identical(rec3$primary, "pb"))

# 4 negative earnings, positive CFO
rec4 <- recommend_valuation_models(
  base_cf(cfo = c(70, 60), capex = c(-15, -12)),
  "Packaged Foods",
  base_is(ni = c(-5, -8), pretax = c(-4, -6), tax = c(0, 0)),
  base_bs()
)
check("neg earnings does not invent pb", !identical(rec4$primary, "pb"))
check("neg earnings has score table", is.data.frame(rec4$model_scores) && nrow(rec4$model_scores) >= 4)

# 5 high leverage still has a claim preference for FCFF
claim5 <- recommend_dcf_claim(
  stmt(list("Total Debt" = c(800, 780), "Stockholders Equity" = c(200, 220))),
  fcff = 50, fcfe = 5
)
check("high leverage prefers fcff", identical(claim5$prefer, "fcff"))

# 6 net cash company: EV bridge adds cash
eq6 <- dcf_ev_to_equity(1000, cash = 400, debt = 50)
check("net cash bridge", abs(eq6 - 1350) < 1e-8)

# 7 negative equity blocks ordinary P/B
pb7 <- derive_pb_targets(roe_pct = 15, ke_pct = 10, g_pct = 3, book_equity = -20,
                         industry_band = list(low = 1, mid = 1.4, high = 1.8))
check("negative equity no pb mid", !is.finite(pb7$mid) && isTRUE(pb7$pb_blocked))

# 8 high goodwill lowers applicability
pb8 <- assess_pb_applicability(equity = 100, goodwill = 80, assets = 120, roe = 10,
                               industry_text = "Packaged Foods", industry_key = "cg.Food")
pb8b <- assess_pb_applicability(equity = 100, goodwill = 5, assets = 120, roe = 10,
                                industry_text = "Packaged Foods", industry_key = "cg.Food")
check("goodwill lowers pb score", pb8$score < pb8b$score)

# 9 asset-heavy book business can select P/B
rec9 <- recommend_valuation_models(
  base_cf(cfo = c(40, 38), capex = c(-25, -24)),
  "Banks - Diversified",
  base_is(ni = c(30, 28), rev = c(100, 96)),
  base_bs(equity = c(400, 390), debt = c(1000, 980), assets = c(2000, 1900), goodwill = c(0, 0)),
  industry_choice = "fn.Banking"
)
check("bank primary pb", identical(rec9$primary, "pb"))

# 10 buybacks lower pb applicability
pb10 <- assess_pb_applicability(equity = 80, buyback_to_equity = 0.6, roe = 20, industry_text = "Packaged Foods")
pb10b <- assess_pb_applicability(equity = 80, buyback_to_equity = 0, roe = 20, industry_text = "Packaged Foods")
check("buyback lowers pb", pb10$score < pb10b$score)

# 11 split does not change equity value
spl <- apply_share_split(shares = 100, equity_value = 500, split_ratio = 4)
check("split equity unchanged", abs(spl$equity - 500) < 1e-8 && abs(spl$per_share - 1.25) < 1e-8)
jump <- detect_share_basis_shift(400, 100)
check("split jump flagged", isTRUE(jump$shift))

# 12 missing industry blocks industry P/B
pb12 <- derive_pb_targets(
  industry_band = list(low = 1, mid = 2, high = 3),
  include_justified = FALSE, include_history = FALSE, industry_key_valid = FALSE
)
check("bad industry ignores band", !is.finite(pb12$mid) && !is.finite(pb12$industry_mid))

# 13 currency mismatch is a share-unit issue, not a valuation calibration
sh13 <- resolve_shares_for_price(
  1e6, price = 10, market_cap = 5e9, ticker = "SYN",
  quote_currency = "USD", financial_currency = "TWD"
)
check("fx share gap uses implied shares", identical(sh13$method, "market_cap_per_price"))

# 14 thousand-share style ratio is not treated as economic dilution only
sh14 <- resolve_shares_for_price(1e6, price = 10, market_cap = 1e10, ticker = "SYN")
check("1000x share gap flagged by ratio", is.finite(sh14$ratio) && sh14$ratio > 100)

# 15 WACC <= g stops DCF
v15 <- value_dcf_per_share(c(100, 110), claim = "fcff", wacc = 0.03, g_term = 0.04, shares = 10)
check("wacc le g stops", !isTRUE(v15$ok) && !is.finite(v15$per_share))

# 16 ROE below Ke can justify P/B below 1, and is not clipped up
pb16 <- derive_pb_targets(roe_pct = 4, ke_pct = 10, g_pct = 2, include_industry = FALSE, include_history = FALSE)
check("roe below ke pb below 1", is.finite(pb16$justified) && pb16$justified < 1 && pb16$justified > 0)
check("no clamp", isFALSE(pb16$clamp_applied))

# 17 high TV share is a material warning
q17 <- dcf_result_quality(explicit_pv = 20, pv_tv = 180, ev = 200, wacc = 0.08, g_term = 0.03)
check("tv weight warning", any(vapply(q17$diagnostics, function(d) d$code == "val_diag_tv_weight", logical(1))))

# 18 abnormal NWC base is not a reason to zero growth; custom 0 stays 0
g18 <- revenue_growth_pct_for_year(1, "gordon", g_est = 0, g_stage1 = 12, g_stage2 = 3, yr_stage1 = 3)
check("zero custom growth kept", is.finite(g18) && abs(g18) < 1e-8)
g18b <- resolve_near_term_growth("fundamental", fundamental_g = 6, custom_g = 20)
check("fundamental ignores custom", abs(g18b$g - 6) < 1e-8)

# 19 one-time acquisition: capex identity uses absolute capex, not financing CF
d_cf19 <- stmt(list(
  "Operating Cash Flow" = c(100, 90),
  "Capital Expenditure" = c(-40, -30),
  "Free Cash Flow" = c(10, 60),
  "Financing Cash Flow" = c(-200, -20)
))
d_is19 <- base_is()
rec_fcff <- reconstruct_hist_fcff(d_cf19, d_is19, tax = 0.21)
check("capex not financing cf", abs(rec_fcff$fcff[1] - (100 + 8 * 0.79 - 40)) < 1e-6)

# 20 one or two peers cannot support a high-confidence industry multiple
pb20 <- derive_pb_targets(
  industry_band = list(low = 1, mid = 3, high = 4),
  include_justified = FALSE, include_history = FALSE, peer_n = 2
)
check("thin peers drop industry blend", !is.finite(pb20$mid))

# Properties
w <- compute_wacc(re = 0.10, rd = 0.05, tax_ratio = 0.21, we = 0.7, wd = 0.3)
check("weights sum 1", isTRUE(w$ok) && abs(w$weight_sum - 1) < 1e-8)
check("wacc inside band", w$wacc >= min(0.10, 0.05 * 0.79) - 1e-8 && w$wacc <= max(0.10, 0.05 * 0.79) + 1e-8)
bad_w <- compute_wacc(re = 0.10, rd = 0.05, tax_ratio = 0.21, we = 0.7, wd = 0.5)
check("bad weights fatal", !isTRUE(bad_w$ok) && !is.finite(bad_w$wacc))
na_w <- resolve_wacc_input(calculated = NA, manual = 0.08, mode = "calculated")
check("na wacc not reused", !isTRUE(na_w$ok) && !is.finite(na_w$value))
man_w <- resolve_wacc_input(calculated = NA, manual = 0.08, mode = "manual")
check("manual labeled", identical(man_w$override_status, "manual"))

v_lo <- value_dcf_per_share(c(100, 105), wacc = 0.08, g_term = 0.02, shares = 10, cash = 0, debt = 0)
v_hi <- value_dcf_per_share(c(100, 105), wacc = 0.10, g_term = 0.02, shares = 10, cash = 0, debt = 0)
v_g <- value_dcf_per_share(c(100, 105), wacc = 0.08, g_term = 0.03, shares = 10, cash = 0, debt = 0)
v_f <- value_dcf_per_share(c(110, 115), wacc = 0.08, g_term = 0.02, shares = 10, cash = 0, debt = 0)
v_sh <- value_dcf_per_share(c(100, 105), wacc = 0.08, g_term = 0.02, shares = 20, cash = 0, debt = 0)
check("value falls in wacc", isTRUE(v_lo$ok) && isTRUE(v_hi$ok) && v_hi$per_share < v_lo$per_share)
check("value rises in g", isTRUE(v_g$ok) && v_g$per_share > v_lo$per_share)
check("value rises in fcf", isTRUE(v_f$ok) && v_f$per_share > v_lo$per_share)
check("dilution lowers per share", isTRUE(v_sh$ok) && abs(v_sh$equity - v_lo$equity) < 1e-6 && v_sh$per_share < v_lo$per_share)

mismatch <- value_dcf_per_share(c(100), claim = "fcff", ke = 0.10, wacc = NA, discount_kind = "ke", g_term = 0.02, shares = 10)
check("fcff rejects ke", !isTRUE(mismatch$ok))
fcfe_bad <- value_dcf_per_share(c(100), claim = "fcfe", wacc = 0.08, ke = NA, discount_kind = "wacc", g_term = 0.02, shares = 10)
check("fcfe rejects wacc", !isTRUE(fcfe_bad$ok))
fcfe_ok <- value_dcf_per_share(c(100), claim = "fcfe", ke = 0.10, wacc = 0.08, g_term = 0.02, shares = 10, cash = 999, debt = 50)
check("fcfe skips cash bridge", isTRUE(fcfe_ok$ok) && abs(fcfe_ok$equity - fcfe_ok$enterprise) < 1e-6 || is.na(fcfe_ok$enterprise))

st_bad <- validate_stage_years(5, 5, 2)
check("stage1 not above horizon", !isTRUE(st_bad$ok))
st_ok <- validate_stage_years(5, 3, 2, g2 = 0.04, g_term = 0.02)
check("stages sum", isTRUE(st_ok$ok))
ev_guess <- .dcf_formula_ev(n = 5, r1 = 0.08, g_term = 0.02, g_near = 0.06, yr_stage1 = 3, g_stage2 = 0.04)
check("missing stage2 not guessed", !is.finite(ev_guess))
ev_ok <- .dcf_formula_ev(n = 5, r1 = 0.08, g_term = 0.02, g_near = 0.06, yr_stage1 = 3, yr_stage2 = 2, g_stage2 = 0.04)
check("explicit two stage finite", is.finite(ev_ok))

pb_price <- .pb_formula_p(basis = 20, pb = 1.5)
check("pb identity", abs(pb_price - 30) < 1e-8)
just_hi <- derive_pb_targets(roe_pct = 12, ke_pct = 10, g_pct = 2, include_industry = FALSE, include_history = FALSE)
just_lo <- derive_pb_targets(roe_pct = 8, ke_pct = 10, g_pct = 2, include_industry = FALSE, include_history = FALSE)
check("justified falls with roe", just_lo$justified < just_hi$justified)

empty <- recommend_valuation_models(data.frame())
check("empty data no pb default", is.null(empty$primary))
check("empty says no secondary", identical(empty$secondary_label, "無適用副模型"))

gap <- derive_pb_targets(
  roe_pct = 40, ke_pct = 8, g_pct = 2,
  industry_band = list(low = 1, mid = 1.2, high = 1.5),
  include_history = FALSE
)
check("wide pb gap not averaged", !isTRUE(gap$blended) && is.finite(gap$justified) && gap$justified > 6)

pv_bad <- dcf_yearly_cf_pv(c(100, 110), c(NA, 0.1))
check("invalid discount not 10pct", !is.finite(pv_bad[1]) && !isTRUE(abs(pv_bad[1] - 100 / 1.1) < 1e-6))

capm_bad <- capm_cost_of_equity(rf = 0.04, beta = 1, rm = 0.09, erp = 0.08)
check("erp mismatch fatal", !isTRUE(capm_bad$ok))
capm_ok <- capm_cost_of_equity(rf = 0.04, beta = 1.1, rm = 0.09, extra_premium = 0.01)
check("extra premium separate", isTRUE(capm_ok$ok) && abs(capm_ok$ke - (0.04 + 1.1 * 0.05 + 0.01)) < 1e-8)

stale <- stale_ticker_diagnostic("AAA", "BBB")
check("stale ticker fatal", !is.null(stale) && identical(stale$level, "fatal"))

fund_g <- resolve_near_term_growth("custom", fundamental_g = 9, custom_g = 4)
check("custom wins when selected", abs(fund_g$g - 4) < 1e-8)
miss_f <- resolve_near_term_growth("fundamental", fundamental_g = NA, custom_g = 4, fallback_g = NA)
check("fundamental does not borrow custom", !isTRUE(miss_f$ok))

ident <- terminal_g_identity(0.03, reinvestment = 0.30, roic = 0.10)
check("g equals rr times roic", isTRUE(ident$consistent))
ident_na <- terminal_g_identity(0.02)
check("simplified gordon disclosed", isTRUE(ident_na$simplified_gordon))

# Provenance fields exist
pr <- valuation_provenance(0.08, "formula", effective_date = "2024-12-31",
                           calculation_method = "WACC", confidence = "calculated",
                           override_status = "system_calculated")
check("provenance fields", all(c("value", "source", "effective_date", "calculation_method", "confidence", "override_status") %in% names(pr)))

# No ticker branch remains in the share resolver
src_shares <- paste(deparse(resolve_shares_for_price), collapse = "\n")
check("no brk share multiplier", !grepl("1500", src_shares, fixed = TRUE))

# UI keys exist in both locales
check("en diagnostic", ui_str("val_diag_no_secondary", "en") != "val_diag_no_secondary")
check("zh diagnostic", grepl("副模型", ui_str("val_diag_no_secondary", "zh-TW")))

if (fail > 0L) {
  cat(fail, " valuation guard check(s) failed.\n", sep = "")
  quit(status = 1L)
}
cat("All valuation guard checks passed.\n")
