# Unit tests: MOS next-period stats + hist PIT helpers
app_dir <- if (file.exists("backtest_validation.R")) {
  getwd()
} else if (file.exists("app_16.0/backtest_validation.R")) {
  file.path(getwd(), "app_16.0")
} else {
  stop("Cannot locate app_16.0")
}

source(file.path(app_dir, "backtest_module.R"), local = FALSE)
source(file.path(app_dir, "backtest_validation.R"), local = FALSE)

approx_eq <- function(a, b, tol = 1e-8) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}
check <- function(label, cond) {
  if (!isTRUE(cond)) stop("FAIL: ", label)
  message("OK: ", label)
}

# --- inventory table ---
inv <- pit_param_inventory_table()
check("inventory has rows", is.data.frame(inv) && nrow(inv) >= 10)
check("inventory has 狀態 col", "狀態" %in% names(inv))
rf_live <- inv$Live來源[inv$參數 == "Rf"][1]
check("inventory Rf notes TNX for US/TW", grepl("TNX", rf_live, fixed = TRUE))

# --- TW market Rf / statutory tax (no network) ---
mp_path <- file.path(app_dir, "market_profile.R")
if (file.exists(mp_path)) {
  source(mp_path, local = FALSE, encoding = "UTF-8")
  check("TW statutory tax 20%", approx_eq(.default_statutory_tax_ratio("TW"), 0.20))
  check("US statutory tax 21%", approx_eq(.default_statutory_tax_ratio("US"), 0.21))
  # TW PIT Rf now uses same ^TNX path as US (stub fetch; no network)
  called_tnx <- FALSE
  old_tnx <- fetch_tnx_history_df
  fetch_tnx_history_df <<- function(period = "10y") {
    called_tnx <<- TRUE
    data.frame(Date = as.Date("2024-06-01"), Close = 4.25, stringsAsFactors = FALSE)
  }
  on.exit({ fetch_tnx_history_df <<- old_tnx }, add = TRUE)
  tw_rf_hist <- fetch_pit_rf_history_df("5y", market = "TW")
  check("TW pit Rf history uses TNX path", isTRUE(called_tnx) && is.data.frame(tw_rf_hist) && nrow(tw_rf_hist) >= 1)
  # pit_discount_params: NULL tnx_df → session Rf
  dates <- as.Date(c("2024-01-02", "2024-02-01", "2024-03-01", "2024-04-01",
                     "2024-05-01", "2024-06-03", "2024-07-01", "2024-08-01",
                     "2024-09-02", "2024-10-01", "2024-11-01", "2024-12-02",
                     "2025-01-02", "2025-02-03", "2025-03-03", "2025-04-01",
                     "2025-05-01", "2025-06-02", "2025-07-01", "2025-08-01",
                     "2025-09-01", "2025-10-01", "2025-11-03", "2025-12-01",
                     "2026-01-02", "2026-02-02", "2026-03-02", "2026-04-01",
                     "2026-05-01", "2026-06-01", "2026-07-01", "2026-08-03",
                     "2026-09-01"))
  n <- length(dates)
  stock <- 100 * (1.01 ^ seq_len(n))
  bench <- 100 * (1.008 ^ seq_len(n))
  disc <- pit_discount_params(
    list(rf = 0.04, rm = 0.08, ke = 0.09, wacc = 0.08, tax = 0.20, beta_fallback = 1.0,
         beta_lookback_months = 12, beta_min_months = 6),
    stock, bench, dates, as_of = dates[n],
    tnx_df = NULL, fund_row = NULL, price = stock[n], realized_rm = FALSE
  )
  check("NULL tnx uses session Rf", approx_eq(disc$rf, 0.04, 1e-9))
  disc_tnx <- pit_discount_params(
    list(rf = 0.04, rm = 0.08, ke = 0.09, wacc = 0.08, tax = 0.20, beta_fallback = 1.0,
         beta_lookback_months = 12, beta_min_months = 6),
    stock, bench, dates, as_of = dates[n],
    tnx_df = data.frame(Date = dates, Close = rep(4.25, n), stringsAsFactors = FALSE),
    fund_row = NULL, price = stock[n], realized_rm = FALSE
  )
  check("TNX asof overrides session Rf", approx_eq(disc_tnx$rf, 0.0425, 1e-9))
} else {
  message("SKIP: market_profile.R missing for TW Rf/tax checks")
}

# --- hist helpers ---
fund <- list(
  rev_growth = 12, eps_growth = 8, fcf_growth = 10,
  interest_expense = 50, debt = 1000,
  tax_expense = 21, pretax_income = 100,
  g_pit = NA_real_
)
g <- .hist_pit_growth_decimal(fund, fallback = 0.025)
check("growth from YoY fields clamped", approx_eq(g, 0.08, 1e-6))
check("clamp below r", approx_eq(.hist_clamp_g_below_r(0.12, 0.08), 0.075, 1e-6))
check("pit rd", approx_eq(.hist_pit_rd(fund), 0.05, 1e-8))
check("pit tax", approx_eq(.hist_pit_tax(fund), 0.21, 1e-8))
pb <- .hist_justified_pb(roe = 0.15, ke = 0.10, g = 0.03, fallback = 1.5)
check("justified pb", approx_eq(pb, (0.15 - 0.03) / (0.10 - 0.03), 1e-8))

# --- reconstruct hist uses g/rd/tax/pb diagnostics ---
fund_row <- list(
  shares = 100, fcf = 200, cash = 50, debt = 1000,
  ni = 150, equity_book = 1000, dividends_paid = 40,
  rev_growth = 10, eps_growth = 10, fcf_growth = 10,
  g_pit = 0.05,
  interest_expense = 40, tax_expense = 20, pretax_income = 100
)
mp <- list(wacc = 0.09, ke = 0.10, rd = 0.99, tax = 0.99, fv_models = c("dcf", "ri", "pb"))
pit <- reconstruct_fair_value_pit(fund_row, price = 8, mp, use_session_assumptions = FALSE)
check("hist g_used finite", is.finite(pit$g_used))
check("hist rd from interest", approx_eq(pit$rd_used, 0.04, 1e-8))
check("hist tax from pretax", approx_eq(pit$tax_used, 0.20, 1e-8))
check("hist pb justified", is.finite(pit$pb_mid_used) && pit$pb_mid_used > 0)
check("hist fv finite", is.finite(pit$fair_value) && pit$fair_value > 0)
check("hist src_g pit", identical(pit$src_g, "pit"))
check("hist src_rd pit", identical(pit$src_rd, "pit"))
check("hist src_tax pit", identical(pit$src_tax, "pit"))
check("hist src_pb justified", identical(pit$src_pb_mid, "justified"))
check("hist src_n_years app_defaults", identical(pit$src_n_years, "app_defaults"))
check("hist n_years in fallback_keys", grepl("n_years", pit$fallback_keys, fixed = TRUE))

# --- empty model selection: no DCF fallback ---
check("normalize empty → character(0)", length(.normalize_fv_models(list())) == 0L)
check("normalize NULL fv_models → character(0)",
      length(.normalize_fv_models(list(fv_models = character(0)))) == 0L)
mp_none <- list(wacc = 0.09, ke = 0.10, rd = 0.99, tax = 0.99, fv_models = character(0))
pit_none <- reconstruct_fair_value_pit(fund_row, price = 8, mp_none, use_session_assumptions = FALSE)
check("no models → fair_value NA", !is.finite(pit_none$fair_value))
check("no models → mos NA", !is.finite(pit_none$mos))
check("no models still has fv_dcf", is.finite(pit_none$fv_dcf))

# growth/rd/tax fallback sources when PIT fields missing
fund_miss <- list(
  shares = 100, fcf = 200, cash = 50, debt = 0,
  ni = 150, equity_book = 1000, dividends_paid = 0,
  rev_growth = NA_real_, eps_growth = NA_real_, fcf_growth = NA_real_,
  g_pit = NA_real_, interest_expense = NA_real_,
  tax_expense = NA_real_, pretax_income = NA_real_
)
mp_fb <- list(wacc = 0.09, ke = 0.10, rd = 0.06, tax = 0.25, fv_models = "dcf")
pit_fb <- reconstruct_fair_value_pit(fund_miss, price = 8, mp_fb, use_session_assumptions = FALSE)
check("fallback src_g app_defaults", identical(pit_fb$src_g, "app_defaults"))
check("fallback src_rd session", identical(pit_fb$src_rd, "session"))
check("fallback src_tax session", identical(pit_fb$src_tax, "session"))
fb_sum <- summarize_hist_param_fallbacks(data.frame(
  src_g = pit_fb$src_g, src_n_years = pit_fb$src_n_years,
  src_rd = pit_fb$src_rd, src_tax = pit_fb$src_tax,
  src_pb_mid = pit_fb$src_pb_mid, src_rf = "tnx", src_rm = "realized",
  src_beta = "rolling", session_tip = FALSE,
  stringsAsFactors = FALSE
))
check("fallback summary any", isTRUE(fb_sum$any_fallback))
check("fallback summary has g", "g" %in% fb_sum$items$key)
check("fallback summary has n_years", "n_years" %in% fb_sum$items$key)
check("fallback note is model-neutral",
      !grepl("請至|DCF／|DCF（", fb_sum$note, perl = TRUE))
check("fallback scopes are not DCF panels",
      !any(grepl("DCF／|DCF（|請至", fb_sum$items$scope %||% "", perl = TRUE)))
guide_g <- .hist_param_guide("zh-TW")
check("guide g scope neutral",
      grepl("共用系統預設", guide_g$scope[guide_g$key == "g"][1], fixed = TRUE) &&
        !grepl("DCF", guide_g$scope[guide_g$key == "g"][1], fixed = TRUE))
guide_en <- .hist_param_guide("en")
check("guide en g label", identical(guide_en$label[guide_en$key == "g"][1], "Terminal growth g"))
fb_en <- summarize_hist_param_fallbacks(data.frame(
  src_g = "app_defaults", src_n_years = "app_defaults",
  src_rd = "session", src_tax = "session",
  src_pb_mid = "justified", src_rf = "tnx", src_rm = "realized",
  src_beta = "rolling", session_tip = FALSE,
  stringsAsFactors = FALSE
), locale = "en")
check("en fallback note mentions APP_DEFAULTS",
      grepl("APP_DEFAULTS", fb_en$note, fixed = TRUE) &&
        !grepl("DCF panel|go to|please open", fb_en$note, ignore.case = TRUE))

# --- MOS next-period stats (legacy helper still available) ---
vd <- data.frame(
  Date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31",
                   "2021-03-31", "2021-06-30")),
  hist_price = c(100, 110, 105, 120, 115, 130),
  mos = c(0.40, 0.35, -0.05, 0.05, 0.55, 0.12),
  fair_value = c(140, 150, 100, 126, 180, 130),
  stringsAsFactors = FALSE
)
stats <- summarize_mos_next_period_stats(vd)
check("stats rows", nrow(stats) == length(.MOS_BUCKET_LEVELS))
b30 <- stats[stats$bucket == "便宜 MOS[30%,50%)", , drop = FALSE]
check("bucket 30-50 has n>=1", b30$n[1] >= 1L)
check("p_up in [0,1] or NA", is.na(b30$p_up[1]) || (b30$p_up[1] >= 0 && b30$p_up[1] <= 1))

out <- lookup_mos_bucket_outlook(0.42, stats)
check("outlook bucket", identical(out$bucket, "便宜 MOS[30%,50%)"))
check("outlook has note", is.character(out$note) && nzchar(out$note))

out2 <- lookup_mos_bucket_outlook(-0.15, stats)
check("outlook expensive bucket", identical(out2$bucket, "偏貴 MOS<-10%"))

# --- FV market validation: P_{t+1} vs FV_t + magnitude + OOS ---
pairs <- build_fv_convergence_pairs(vd)
check("pairs n=5", nrow(pairs) == 5L)
check("first toward", identical(pairs$outcome[1], "趨近"))
check("second away", identical(pairs$outcome[2], "遠離"))
check("first below FV", identical(pairs$vs_fv[1], "之下"))
check("third above FV", identical(pairs$vs_fv[3], "之上"))
check("gap_next present", "gap_next" %in% names(pairs) && is.finite(pairs$gap_next[1]))
check("ret_next present", "ret_next" %in% names(pairs) && is.finite(pairs$ret_next[1]))
# vd prices: 100,110,105,120,115,130 → first ret = 110/100-1 = +0.10 → 漲
check("first ret_next", abs(pairs$ret_next[1] - (110 / 100 - 1)) < 1e-12)
check("first dir_price 漲", identical(pairs$dir_price[1], "漲"))
# second: 110→105 → 跌
check("second dir_price 跌", identical(pairs$dir_price[2], "跌"))

sum_all <- summarize_fv_market_validation(vd, oos_mode = "insample")
check("sum n=5", sum_all$n == 5L)
check("above+below+flat = n",
      sum_all$n_above + sum_all$n_below + sum_all$n_flat_vs == sum_all$n)
check("up+down+flat_price = n",
      sum_all$n_up + sum_all$n_down + sum_all$n_flat_price == sum_all$n)
check("p_up in [0,1]", is.finite(sum_all$p_up) && sum_all$p_up >= 0 && sum_all$p_up <= 1)
check("median_gap finite", is.finite(sum_all$median_gap))
check("median_ret finite", is.finite(sum_all$median_ret))
check("frame note", grepl("非策略回測", sum_all$frame))
check("mos_outlook list", is.list(sum_all$mos_outlook))
# tip MOS = penultimate finite mos (= 0.55) → 便宜 MOS≥50%
check("mos_outlook bucket", identical(sum_all$mos_outlook$bucket, "便宜 MOS≥50%"))
check("mos_outlook p_up matches stats", {
  st <- sum_all$mos_stats
  hit <- st[st$bucket == "便宜 MOS≥50%", , drop = FALSE]
  nrow(hit) == 1L && is.finite(hit$p_up[1]) &&
    isTRUE(abs(sum_all$mos_outlook$p_up - hit$p_up[1]) < 1e-12)
})

sum_win <- summarize_fv_market_validation(
  vd, from = as.Date("2020-06-01"), to = as.Date("2020-12-31"), oos_mode = "insample"
)
check("window filters", sum_win$n >= 1L && sum_win$n < sum_all$n)

sum_real <- summarize_fv_market_validation(
  vd, as_of = as.Date("2020-09-01"), oos_mode = "realized"
)
check("realized as_of filters", sum_real$n < sum_all$n)

sum_exp <- summarize_fv_market_validation(vd, oos_mode = "expanding")
check("expanding runs", is.character(sum_exp$oos_mode))
check("expanding has dir oos field", "oos_dir_hit_rate" %in% names(sum_exp))

message("ALL PASS")
