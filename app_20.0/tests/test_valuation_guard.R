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
note7 <- format_pb_targets_note(pb7, locale = "zh-TW")
check("negative equity note stays visible", nzchar(note7) && grepl("沒有合成", note7, fixed = TRUE))
note7en <- format_pb_targets_note(pb7, locale = "en")
check("negative equity note en", grepl("No blended P/B", note7en, fixed = TRUE))

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

# --- Lifecycle result is an input, not the model ---
life_growth <- classify_lifecycle_result(
  industry_text = "Packaged Foods",
  d_is = base_is(ni = c(40, 20), rev = c(200, 120)),
  d_bs = base_bs(equity = c(300, 250)),
  d_cf = base_cf(cfo = c(90, 40), capex = c(-20, -10)),
  expected_rev_cagr_pct = 22,
  wacc_pct = 9
)
rec_life <- recommend_valuation_models(
  base_cf(cfo = c(90, 40), capex = c(-20, -10)),
  "Packaged Foods",
  base_is(ni = c(40, 20), rev = c(200, 120)),
  base_bs(equity = c(300, 250)),
  lifecycle_result = life_growth
)
check("lifecycle attached to recommendation", !is.null(rec_life$lifecycle_result))
check("stage does not force a single model name", !identical(rec_life$primary, life_growth$autoDetectedStage))
rec_forced <- recommend_valuation_models(
  base_cf(),
  "Packaged Foods",
  base_is(),
  base_bs(),
  lifecycle_result = classify_lifecycle_result(
    industry_text = "Packaged Foods", d_is = base_is(), d_bs = base_bs(),
    d_cf = base_cf(), expected_rev_cagr_pct = 3, wacc_pct = 8,
    selected_stage = "HIGH_GROWTH", selection_mode = "manual"
  )
)
check("manual HIGH_GROWTH does not alone pick a growth-only model",
      !is.null(rec_forced$primary) && rec_forced$primary %in% c("dcf", "ri", "ddm", "pb", "nav"))

rec_bank <- recommend_valuation_models(
  base_cf(cfo = c(40, 38), capex = c(-25, -24)),
  "Banks - Diversified",
  base_is(ni = c(30, 28), rev = c(100, 96)),
  base_bs(equity = c(400, 390), debt = c(1000, 980), assets = c(2000, 1900), goodwill = c(0, 0)),
  industry_choice = "fn.Banking"
)
check("bank still not FCFF-only primary", !identical(rec_bank$primary, "dcf") ||
        (!is.null(rec_bank$secondary) && !identical(rec_bank$secondary, "dcf")))
check("bank company_type financial or not dcf-only",
      identical(rec_bank$company_type, "financial") || !identical(rec_bank$primary, "dcf"))

rec_util <- recommend_valuation_models(
  base_cf(cfo = c(80, 76), capex = c(-50, -48)),
  "Electric Utilities",
  base_is(ni = c(40, 38), rev = c(300, 290)),
  base_bs(equity = c(700, 680), debt = c(800, 780), assets = c(2000, 1900)),
  industry_choice = "en.Utilities"
)
check("utility logic differs from bank", !identical(rec_util$company_type, rec_bank$company_type) ||
        !identical(rec_util$primary, rec_bank$primary) ||
        identical(rec_util$lifecycle_result$autoDetectedStage, "REGULATED_UTILITY"))

life_low <- life_growth
life_low$confidenceScore <- 0.20
rec_low <- recommend_valuation_models(
  base_cf(), "Packaged Foods", base_is(), base_bs(),
  lifecycle_result = life_low
)
check("low lifecycle confidence is forwarded",
      is.finite(rec_low$confidence_inputs$lifecycle_confidence) &&
        rec_low$confidence_inputs$lifecycle_confidence < 0.45)

# --- Cyclical industries must keep P/B as primary or secondary ---
.has_pb_slot <- function(rec) {
  identical(as.character(rec$primary %||% ""), "pb") ||
    identical(as.character(rec$secondary %||% ""), "pb")
}
cyc_cases <- list(
  list("Steel", "mat.Metals_Mining"),
  list("Marine Shipping", "tr.Logistics_Shipping"),
  list("Specialty Chemicals", "mat.Chemicals"),
  list("Semiconductors - Memory", "sc.Memory"),
  list("Semiconductor Foundry", "sc.Foundry"),
  list("Auto Manufacturers", "auto.Vehicle_Manufacturing"),
  list("Paper & Packaging", "mat.Paper_Packaging"),
  list("Airlines", "tr.Airlines"),
  list("Oil & Gas Integrated", "en.Energy_OilGas")
)
for (cc in cyc_cases) {
  rec_c <- recommend_valuation_models(
    base_cf(), cc[[1]], base_is(), base_bs(), industry_choice = cc[[2]]
  )
  check(paste0("cyclical ", cc[[2]], " has P/B slot"), .has_pb_slot(rec_c))
  check(paste0("cyclical ", cc[[2]], " tagged"), isTRUE(rec_c$confidence_inputs$cyclical_industry))
  check(paste0("cyclical ", cc[[2]], " mentions P/B rule"), {
    grepl("景氣循環", rec_c$reason %||% "") && grepl("P/B", rec_c$reason_en %||% "")
  })
}
rec_steel_txt <- recommend_valuation_models(
  base_cf(), "Steel", base_is(), base_bs(), industry_choice = ""
)
check("text-only steel still gets P/B slot", .has_pb_slot(rec_steel_txt))

rec_foods <- recommend_valuation_models(
  base_cf(), "Packaged Foods", base_is(), base_bs(),
  industry_choice = "fmcg.Food_Beverages"
)
check("staples not forced to P/B", !isTRUE(rec_foods$confidence_inputs$cyclical_industry))

rec_saas <- recommend_valuation_models(
  base_cf(), "Software - Application", base_is(), base_bs(),
  industry_choice = "saas.SaaS_Cloud"
)
check("saas not cyclical", !isTRUE(rec_saas$confidence_inputs$cyclical_industry))
check("saas not forced to P/B", !.has_pb_slot(rec_saas))
check("saas secondary Multiples", identical(as.character(rec_saas$secondary %||% ""), "multiples"))
check("saas never primary multiples", !identical(as.character(rec_saas$primary %||% ""), "multiples"))
check("saas never primary sotp", !identical(as.character(rec_saas$primary %||% ""), "sotp"))
check("saas multiples role cross_check", identical(as.character(rec_saas$secondary_role %||% ""), "cross_check"))

rec_hold <- recommend_valuation_models(
  base_cf(), "Conglomerate", base_is(), base_bs(),
  industry_choice = "fn.Conglomerate_Holding"
)
check("holding prefers NAV primary when applicable", {
  identical(as.character(rec_hold$primary %||% ""), "nav") ||
    identical(as.character(rec_hold$company_type %||% ""), "holding_asset")
})
if (identical(as.character(rec_hold$primary %||% ""), "nav")) {
  check("holding NAV secondary SOTP", identical(as.character(rec_hold$secondary %||% ""), "sotp"))
  check("holding SOTP cross_check", identical(as.character(rec_hold$secondary_role %||% ""), "cross_check"))
}
check("holding never primary SOTP", !identical(as.character(rec_hold$primary %||% ""), "sotp"))
check("holding never primary Multiples", !identical(as.character(rec_hold$primary %||% ""), "multiples"))

rec_neg_eq <- recommend_valuation_models(
  base_cf(),
  "Steel",
  base_is(),
  base_bs(equity = c(-40, -38)),
  industry_choice = "mat.Metals_Mining"
)
check("cyclical negative equity does not force P/B", !.has_pb_slot(rec_neg_eq))

# 1314-like: one historical dividend year, current DPS NA / FCF weak → must not pick DDM primary
cf_1314 <- stmt(list(
  "Operating Cash Flow" = c(20, 80, 60),
  "Capital Expenditure" = c(-40, -30, -25),
  "Free Cash Flow" = c(-20, 50, 35),
  "Cash Dividends Paid" = c(NA, NA, -15)
))
is_1314 <- base_is(ni = c(-30, 10, 20), rev = c(190, 200, 210), pretax = c(-28, 12, 22), tax = c(0, 0, 0))
bs_1314 <- base_bs(equity = c(760, 780, 800), assets = c(1200, 1180, 1160))
rec_1314 <- recommend_valuation_models(
  cf_1314, "Specialty Chemicals", is_1314, bs_1314,
  industry_choice = "chem.Specialty"
)
check("stale-div ticker not DDM primary", !identical(as.character(rec_1314$primary %||% ""), "ddm"))
check("stale-div ticker ddm flag off or low", !isTRUE(rec_1314$ddm) || identical(as.character(rec_1314$primary %||% ""), "pb"))

src_rec <- paste(deparse(assemble_model_recommendation), collapse = "\n")
check("no ticker branch in model recommendation",
      !grepl("\\b(TSM|AAPL|2330\\.TW|NUE|XOM)\\b", src_rec))

# --- Terminal / lifecycle diagnostics reuse existing engine ---
d_gt <- lifecycle_terminal_diagnostics(g_term = 10, wacc = 8)
check("g > WACC fatal", any(vapply(d_gt, function(x) identical(x$code, "val_diag_wacc_gt_g") && identical(x$level, "fatal"), logical(1))))
d_eq <- lifecycle_terminal_diagnostics(g_term = 8, wacc = 8)
check("g = WACC fatal", any(vapply(d_eq, function(x) identical(x$code, "val_diag_wacc_gt_g"), logical(1))))
d_near <- lifecycle_terminal_diagnostics(g_term = 7.2, wacc = 8)
check("g near WACC warning", any(vapply(d_near, function(x) identical(x$code, "val_diag_wacc_g_spread") && identical(x$level, "material"), logical(1))))

rr_ok <- compute_terminal_reinvestment_rate(0.03, 0.12)
check("rr = g/ROIC", isTRUE(rr_ok$ok) && abs(rr_ok$rate - 0.25) < 1e-9)
rr_zero <- compute_terminal_reinvestment_rate(0.03, 0)
check("roic zero no Inf", isTRUE(rr_zero$invalid) && !is.finite(rr_zero$rate))
rr_na <- compute_terminal_reinvestment_rate(0.03, NA_real_)
check("roic missing no NaN", isTRUE(rr_na$invalid) && !is.nan(rr_na$rate %||% 0))

q_tv <- dcf_result_quality(
  explicit_pv = 20, pv_tv = 90, ev = 110, wacc = 0.08, g_term = 0.03
)
check("TV>80% warning", any(vapply(q_tv$diagnostics, function(x) identical(x$code, "val_diag_tv_weight"), logical(1))))

mis <- classify_lifecycle_result(
  industry_text = "Packaged Foods", d_is = base_is(), d_bs = base_bs(), d_cf = base_cf(),
  expected_rev_cagr_pct = 18, wacc_pct = 8,
  selected_stage = "MATURE_STABLE", selection_mode = "manual"
)
d_mis <- lifecycle_terminal_diagnostics(lifecycle_result = mis, g_term = 3, wacc = 8)
check("manual mismatch warning",
      any(vapply(d_mis, function(x) identical(x$code, "val_diag_lifecycle_manual_mismatch"), logical(1))))
check("manual still selected", identical(mis$selectedStage, "MATURE_STABLE"))

thin_life <- classify_lifecycle_result(industry_text = "Packaged Foods")
d_thin <- lifecycle_terminal_diagnostics(lifecycle_result = thin_life, g_term = 3, wacc = 8)
check("low completeness warning",
      any(vapply(d_thin, function(x) identical(x$code, "val_diag_lifecycle_missing_inputs"), logical(1))))

dup <- lifecycle_terminal_diagnostics(g_term = 10, wacc = 8, tv_to_ev = 0.9)
codes <- vapply(dup, function(x) x$code, character(1))
check("no duplicate rule codes", length(codes) == length(unique(codes)))
check("no NaN in diagnostic values",
      all(vapply(dup, function(x) !is.nan(suppressWarnings(as.numeric(x$current_value)[1])) ||
                   is.na(x$current_value), logical(1))))

leg_d <- lifecycle_terminal_diagnostics(
  lifecycle_result = list(
    autoDetectedStage = "MATURE_GROWTH", selectedStage = "MATURE_GROWTH",
    selectionMode = "auto", confidenceScore = 0.7, dataCompletenessScore = 0.8,
    missingInputs = character(0), legacyValue = "mature_tech"
  ),
  g_term = 3, wacc = 8
)
check("legacy migration info",
      any(vapply(leg_d, function(x) identical(x$code, "val_diag_lifecycle_legacy_migration"), logical(1))))

if (fail > 0L) {
  cat(fail, " valuation guard check(s) failed.\n", sep = "")
  quit(status = 1L)
}
cat("All valuation guard checks passed.\n")
