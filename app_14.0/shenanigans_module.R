# =========================================================================
# shenanigans_module.R — Schilit《財報詭計》自動判讀（YNOW 分頁）
# 以已抓年報（損益／資產負債／現金流）做相對規則；缺欄位給「資料不足」，不捏造附註。
# 數列慣例：select_clean_metric_row 為 最新財年 → 最舊（不含 TTM）。
#
# 門檻（相對規則，避免魔術常數堆疊）：
#   AR／存貨「明顯快於營收」：YoY 差距 ≥ 15 pp；觀察 ≥ 8 pp
#   DSO／DIO／DPO 惡化：≥ 10 天警示、≥ 5 天觀察
#   毛利率下滑：≥ 1.5 pp 警示（搭配存貨）、單獨 ≥ 3 pp 觀察
#   淨利 vs OCF：連續 ≥ 3 年淨利 > OCF 為警示；2 年觀察
#   營業利益為正但 OCF 為負：直接警示（舊 Fraud Warning）
#   負債／權益 > 2：警示（舊 Fraud Warning）；> 1.5 觀察
#   商譽／資產年增 ≥ 8 pp 或商譽年增 ≥ 期初資產 25%：併購相關警示
#   一次性收益：其他收益／處分利益佔營收年增 ≥ 3 pp 且佔 |淨利| ≥ 15%
#   費用資本化：無形資產 YoY 超營收 ≥ 25 pp
#   Big bath：前期減損／非常項目 ≤ −5% 營收，且本期毛利率回升 ≥ 3 pp
#   FCF 為負且 OCF 為正：觀察；OCF 亦負：警示
# =========================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

SHENANIGAN_THRESHOLDS <- list(
  ar_vs_rev_alert_pp = 15,
  ar_vs_rev_watch_pp = 8,
  dso_alert_days = 10,
  dso_watch_days = 5,
  inv_vs_rev_alert_pp = 15,
  gm_drop_alert_pp = 1.5,
  gm_drop_watch_pp = 3,
  ni_gt_ocf_years_alert = 3L,
  ni_gt_ocf_years_watch = 2L,
  de_alert = 2,
  de_watch = 1.5,
  gw_asset_pp = 8,
  gw_vs_beg_assets = 0.25,
  other_inc_rev_pp = 3,
  other_inc_ni_share = 0.15,
  intang_vs_rev_pp = 25,
  opex_drop_alert_pp = 4,
  opex_drop_watch_pp = 2,
  defrev_vs_rev_pp = 25,
  bath_impair_rev = -0.05,
  bath_gm_rebound_pp = 3,
  oc_f_surge = 0.40,
  ar_drop_factor = -0.20,
  at_drop_rel = 0.10,
  asset_jump = 0.20,
  cash_ni_alert = 0.40,
  cash_ni_watch = 0.70,
  gm_vs_ind_alert_pp = 15,
  gm_vs_ind_watch_pp = 5,
  gm_vs_own_watch_pp = 8,
  dpo_alert_days = 15,
  wc_asset_pp = 10
)

.sh_finite <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  length(x) == 1L && is.finite(x)
}

.sh_pick <- function(df, patterns, include_ttm = FALSE) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return(numeric(0))
  v <- tryCatch(
    select_clean_metric_row_any(df, patterns, include_ttm = include_ttm),
    error = function(e) NA
  )
  if (is.null(v) || (length(v) == 1L && is.na(v[1]))) return(numeric(0))
  suppressWarnings(as.numeric(v))
}

.sh_yoy <- function(v) {
  if (length(v) < 2L || !.sh_finite(v[1]) || !.sh_finite(v[2]) || v[2] == 0) {
    return(NA_real_)
  }
  (v[1] - v[2]) / abs(v[2])
}

.sh_pp <- function(a, b) {
  if (!.sh_finite(a) || !.sh_finite(b)) return(NA_real_)
  (a - b) * 100
}

.sh_ratio <- function(a, b) {
  if (!.sh_finite(a) || !.sh_finite(b) || b == 0) return(NA_real_)
  a / b
}

.sh_days <- function(stock, flow) {
  if (!.sh_finite(stock) || !.sh_finite(flow) || flow == 0) return(NA_real_)
  stock / (flow / 365)
}

.sh_pct_txt <- function(x, digits = 1) {
  if (!.sh_finite(x)) return("—")
  sprintf(paste0("%+.", digits, "f%%"), x * 100)
}

.sh_pp_txt <- function(x, digits = 1) {
  if (!.sh_finite(x)) return("—")
  sprintf(paste0("%+.", digits, "f pp"), x)
}

.sh_num_txt <- function(x, digits = 1, suffix = "") {
  if (!.sh_finite(x)) return("—")
  paste0(sprintf(paste0("%.", digits, "f"), x), suffix)
}

.sh_item <- function(code, category, name, status, reason, value_txt, threshold_txt) {
  data.frame(
    code = as.character(code),
    category = as.character(category),
    name = as.character(name),
    status = as.character(status),
    reason = as.character(reason),
    value_txt = as.character(value_txt),
    threshold_txt = as.character(threshold_txt),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.sh_na_item <- function(code, category, name, why) {
  .sh_item(code, category, name, "資料不足", why, "—", "—")
}

.empty_shenanigans <- function(ok = FALSE, message = "") {
  list(
    ok = isTRUE(ok),
    message = as.character(message %||% ""),
    items = .sh_item(character(0), character(0), character(0), character(0),
                     character(0), character(0), character(0))[0, ],
    n_alert = 0L,
    n_watch = 0L,
    n_pass = 0L,
    n_na = 0L
  )
}

.sh_ctx <- function(d_is, d_bs, d_cf) {
  list(
    rev = .sh_pick(d_is, c("^Total Revenue$")),
    gp = .sh_pick(d_is, c("^Gross Profit$")),
    cogs = .sh_pick(d_is, c("^Cost Of Revenue$", "^Cost Of Goods Sold$")),
    opex = .sh_pick(d_is, OPEX_PATTERNS),
    opinc = .sh_pick(d_is, c("^Operating Income$")),
    pretax = .sh_pick(d_is, c("^Pretax Income$")),
    ni = .sh_pick(d_is, NET_INCOME_PATTERNS),
    other_inc = .sh_pick(d_is, c(
      "^Other Income Expense$",
      "Other Non Operating Income",
      "Gain On Sale",
      "Gain On Disposition",
      "Special Income"
    )),
    impair = .sh_pick(d_is, c(
      "Impairment Of Capital Assets",
      "Asset Impairment",
      "Write Off",
      "Restructuring And Mergern Acquisition",
      "Restructuring"
    )),
    ocf = .sh_pick(d_cf, c("^Operating Cash Flow$")),
    fcf = .sh_pick(d_cf, c("^Free Cash Flow$")),
    icf = .sh_pick(d_cf, c("Investing Cash Flow", "Cash Flow From Investing")),
    capex = .sh_pick(d_cf, c("^Capital Expenditure$", "^Capital Expenditures$")),
    dep = .sh_pick(d_cf, c("^Depreciation And Amortization$", "^Depreciation$")),
    acq = .sh_pick(d_cf, c("Purchase Of Business", "Acquisitions Net", "Net Business Purchase")),
    ar = .sh_pick(d_bs, c(
      "^Accounts Receivable$",
      "^Gross Accounts Receivable$",
      "Net Accounts Receivable"
    )),
    contract_ast = .sh_pick(d_bs, c("Contract Asset", "Unbilled Receivable", "Unbilled Revenue")),
    inv = .sh_pick(d_bs, c("^Inventory$", "^Inventories$")),
    ap = .sh_pick(d_bs, c("^Accounts Payable$")),
    defrev = .sh_pick(d_bs, c(
      "^Current Deferred Revenue$",
      "^Deferred Revenue$",
      "Unearned Revenue"
    )),
    gw = .sh_pick(d_bs, c("^Goodwill$")),
    intang = .sh_pick(d_bs, c(
      "^Other Intangible Assets$",
      "Goodwill And Other Intangible Assets",
      "^Intangible Assets$"
    )),
    ppe = .sh_pick(d_bs, c("^Net PPE$", "Net Property Plant And Equipment")),
    assets = .sh_pick(d_bs, c("^Total Assets$")),
    ca = .sh_pick(d_bs, c("^Current Assets$", "^Total Current Assets$")),
    cl = .sh_pick(d_bs, c("^Current Liabilities$", "^Total Current Liabilities$")),
    debt = .sh_pick(d_bs, c("^Total Debt$")),
    equity = .sh_pick(d_bs, EQUITY_PATTERNS),
    allow = .sh_pick(d_bs, c(
      "Allowance For Doubtful",
      "Allowance For Credit Losses",
      "Provision For Doubtful"
    )),
    op_earn = {
      v <- tryCatch(get_operating_earnings_row(d_is, include_ttm = FALSE), error = function(e) numeric(0))
      if (is.null(v) || (length(v) == 1L && is.na(v[1]))) numeric(0) else suppressWarnings(as.numeric(v))
    }
  )
}

.sh_gm <- function(ctx, i = 1L) {
  .sh_ratio(ctx$gp[i], ctx$rev[i])
}

.sh_ind_gm <- function(industry_key) {
  key <- as.character(industry_key %||% "")[1]
  if (!nzchar(key) || !exists("industry_standards") || !(key %in% names(industry_standards))) {
    return(NULL)
  }
  band <- industry_standards[[key]]$gross_profit_margin
  if (is.null(band) || length(band) < 2L) return(NULL)
  lo <- suppressWarnings(as.numeric(band[1])[1])
  hi <- suppressWarnings(as.numeric(band[2])[1])
  if (!is.finite(lo) || !is.finite(hi)) return(NULL)
  list(lo = lo, hi = hi)
}

# ----- 跨手法 -----
.sh_eval_x1 <- function(ctx, th) {
  ni <- ctx$ni
  ocf <- ctx$ocf
  oe <- ctx$op_earn
  if (length(ni) < 1L || length(ocf) < 1L) {
    return(.sh_na_item("X1", "跨手法", "淨利長期高於營業現金流", "損益或現金流缺淨利／營業現金流。"))
  }
  n <- min(length(ni), length(ocf), 5L)
  hit <- is.finite(ni[seq_len(n)]) & is.finite(ocf[seq_len(n)]) & (ni[seq_len(n)] > ocf[seq_len(n)])
  n_hit <- sum(hit, na.rm = TRUE)
  ni_pos_ocf_neg <- .sh_finite(ni[1]) && .sh_finite(ocf[1]) && ni[1] > 0 && ocf[1] < 0
  oe_gt_ocf <- length(oe) >= 1L && .sh_finite(oe[1]) && .sh_finite(ocf[1]) && ocf[1] < oe[1]
  val <- sprintf("近年淨利>OCF：%d／%d 年；本期 OCF／淨利 %s",
                 n_hit, n, .sh_num_txt(.sh_ratio(ocf[1], ni[1]), 2, "×"))
  thr <- sprintf("警示：≥%d 年或獲利為正、現金為負", th$ni_gt_ocf_years_alert)
  if (isTRUE(ni_pos_ocf_neg) || n_hit >= th$ni_gt_ocf_years_alert) {
    st <- "警示"
    why <- if (isTRUE(ni_pos_ocf_neg)) {
      "帳面獲利為正但營業現金流為負，現金沒有跟著進來。"
    } else {
      "多數年度淨利高於營業現金流，獲利品質偏弱。"
    }
  } else if (n_hit >= th$ni_gt_ocf_years_watch || isTRUE(oe_gt_ocf)) {
    st <- "觀察"
    why <- "部分年度淨利（或營業利益）高於營業現金流，宜對照應收與營運資金。"
  } else {
    st <- "通過"
    why <- "近年營業現金流大致能跟上帳面獲利。"
  }
  .sh_item("X1", "跨手法", "淨利長期高於營業現金流", st, why, val, thr)
}

.sh_eval_x2 <- function(ctx, th) {
  if (length(ctx$ar) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("X2", "跨手法", "應收增速高於營收、DSO 拉長", "資產負債表無應收帳款，或年期不足。"))
  }
  ar_g <- .sh_yoy(ctx$ar)
  rev_g <- .sh_yoy(ctx$rev)
  gap <- .sh_pp(ar_g, rev_g)
  dso0 <- .sh_days(ctx$ar[1], ctx$rev[1])
  dso1 <- .sh_days(ctx$ar[2], ctx$rev[2])
  dso_d <- if (.sh_finite(dso0) && .sh_finite(dso1)) dso0 - dso1 else NA_real_
  val <- sprintf("應收 YoY %s vs 營收 YoY %s（差 %s）；DSO %s→%s 天",
                 .sh_pct_txt(ar_g), .sh_pct_txt(rev_g), .sh_pp_txt(gap),
                 .sh_num_txt(dso1, 0), .sh_num_txt(dso0, 0))
  thr <- sprintf("警示：應收快於營收 ≥%d pp 且 DSO +≥%d 天", th$ar_vs_rev_alert_pp, th$dso_alert_days)
  if (.sh_finite(gap) && gap >= th$ar_vs_rev_alert_pp && .sh_finite(dso_d) && dso_d >= th$dso_alert_days) {
    st <- "警示"; why <- "應收成長明顯快於營收，收款天數也拉長，出貨品質或提前認列風險上升。"
  } else if ((.sh_finite(gap) && gap >= th$ar_vs_rev_watch_pp) ||
             (.sh_finite(dso_d) && dso_d >= th$dso_watch_days)) {
    st <- "觀察"; why <- "應收或收款天數轉差，尚未同時達到警示門檻。"
  } else {
    st <- "通過"; why <- "應收與營收步調大致一致，收款天數未明顯惡化。"
  }
  .sh_item("X2", "跨手法", "應收增速高於營收、DSO 拉長", st, why, val, thr)
}

.sh_eval_x3 <- function(ctx, th) {
  if (length(ctx$inv) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("X3", "跨手法", "存貨增速高於營收且毛利下滑", "無存貨科目（金融／軟體常見）或年期不足。"))
  }
  inv_g <- .sh_yoy(ctx$inv)
  rev_g <- .sh_yoy(ctx$rev)
  gap <- .sh_pp(inv_g, rev_g)
  gm0 <- .sh_gm(ctx, 1L)
  gm1 <- .sh_gm(ctx, 2L)
  gm_d <- .sh_pp(gm0, gm1)
  flow <- if (length(ctx$cogs) >= 1L) ctx$cogs else ctx$rev
  dio0 <- .sh_days(ctx$inv[1], flow[1])
  dio1 <- if (length(flow) >= 2L) .sh_days(ctx$inv[2], flow[2]) else NA_real_
  dio_d <- if (.sh_finite(dio0) && .sh_finite(dio1)) dio0 - dio1 else NA_real_
  val <- sprintf("存貨 YoY %s vs 營收 YoY %s（差 %s）；毛利率 %s；週轉天數 %+s",
                 .sh_pct_txt(inv_g), .sh_pct_txt(rev_g), .sh_pp_txt(gap),
                 .sh_pp_txt(gm_d), .sh_num_txt(dio_d, 0, " 天"))
  thr <- sprintf("警示：存貨快於營收 ≥%d pp 且毛利率下滑 ≥%.1f pp",
                 th$inv_vs_rev_alert_pp, th$gm_drop_alert_pp)
  slow <- .sh_finite(dio_d) && dio_d >= th$dso_watch_days
  if (.sh_finite(gap) && gap >= th$inv_vs_rev_alert_pp &&
      .sh_finite(gm_d) && gm_d <= -th$gm_drop_alert_pp) {
    st <- "警示"; why <- "存貨堆積快於銷售，同時毛利率下滑，滯銷或促銷壓力上升。"
  } else if ((.sh_finite(gap) && gap >= th$ar_vs_rev_watch_pp) ||
             (.sh_finite(gm_d) && gm_d <= -th$gm_drop_watch_pp) || isTRUE(slow)) {
    st <- "觀察"; why <- "存貨、週轉或毛利其中一項轉差，尚未同時達警示。"
  } else {
    st <- "通過"; why <- "存貨與營收步調尚可，毛利率未明顯惡化。"
  }
  .sh_item("X3", "跨手法", "存貨增速高於營收且毛利下滑", st, why, val, thr)
}

.sh_eval_x4 <- function(ctx, th) {
  # 年報無法驗證「季底出貨」；只評一次性收益與反覆非常項目。
  if (length(ctx$rev) < 1L) {
    return(.sh_na_item("X4", "跨手法", "一次性收益／反覆非常項目", "損益表缺營收。"))
  }
  oi <- ctx$other_inc
  ni <- ctx$ni
  rev <- ctx$rev
  oi_share0 <- .sh_ratio(if (length(oi) >= 1L) abs(oi[1]) else NA_real_, abs(ni[1]))
  oi_rev0 <- .sh_ratio(if (length(oi) >= 1L) oi[1] else NA_real_, rev[1])
  oi_rev1 <- .sh_ratio(if (length(oi) >= 2L) oi[2] else NA_real_, if (length(rev) >= 2L) rev[2] else NA_real_)
  oi_rev_pp <- .sh_pp(oi_rev0, oi_rev1)
  impair <- ctx$impair
  n_imp <- if (length(impair) >= 1L) {
    sum(is.finite(impair) & abs(impair) > 0, na.rm = TRUE)
  } else {
    0L
  }
  has_oi <- length(oi) >= 1L && any(is.finite(oi))
  if (!has_oi && n_imp < 1L) {
    return(.sh_item(
      "X4", "跨手法", "一次性收益／反覆非常項目", "資料不足",
      "年報無其他收益／處分利益／減損列，也無法用年報驗證季底出貨。",
      "季底出貨：資料不足", "不捏造附註"
    ))
  }
  val <- sprintf("其他收益／營收差 %s；佔 |淨利| %s；非常項目出現 %d 年",
                 .sh_pp_txt(oi_rev_pp), .sh_pct_txt(oi_share0), n_imp)
  thr <- sprintf("警示：其他收益佔營收年增 ≥%d pp 且佔 |淨利| ≥%.0f%%",
                 th$other_inc_rev_pp, th$other_inc_ni_share * 100)
  spike <- .sh_finite(oi_rev_pp) && oi_rev_pp >= th$other_inc_rev_pp &&
    .sh_finite(oi_share0) && oi_share0 >= th$other_inc_ni_share
  recur <- n_imp >= 3L
  if (isTRUE(spike) || isTRUE(recur)) {
    st <- "警示"
    why <- if (isTRUE(recur)) {
      "減損或重整費用多年反覆出現，不宜當「非經常」。季底出貨無法用年報驗證。"
    } else {
      "其他收益／處分利益突然變大，可能撐住本期獲利。季底出貨無法用年報驗證。"
    }
  } else if (.sh_finite(oi_rev_pp) && abs(oi_rev_pp) >= 1.5) {
    st <- "觀察"; why <- "業外或非常項目有波動；季底出貨無法用年報驗證。"
  } else {
    st <- "通過"; why <- "未看到一次性收益暴衝或年年非常費用。季底出貨仍無法用年報驗證。"
  }
  .sh_item("X4", "跨手法", "一次性收益／反覆非常項目", st, why, val, thr)
}

.sh_eval_x5 <- function(ctx, th) {
  gw <- ctx$gw
  ast <- ctx$assets
  acq <- ctx$acq
  has_gw <- length(gw) >= 1L && any(is.finite(gw))
  has_acq <- length(acq) >= 1L && any(is.finite(acq) & acq != 0)
  if (!has_gw && !has_acq) {
    return(.sh_na_item(
      "X5", "跨手法", "大量併購／關聯交易／更換 CFO",
      "無商譽或收購現金流可評併購；關聯交易與 CFO／會計師更換資料不足。"
    ))
  }
  gw_g <- .sh_yoy(gw)
  gw_ast0 <- .sh_ratio(gw[1], ast[1])
  gw_ast1 <- .sh_ratio(if (length(gw) >= 2L) gw[2] else NA_real_,
                      if (length(ast) >= 2L) ast[2] else NA_real_)
  gw_ast_pp <- .sh_pp(gw_ast0, gw_ast1)
  vs_beg <- if (length(ast) >= 2L && .sh_finite(ast[2]) && ast[2] > 0 &&
                length(gw) >= 2L && .sh_finite(gw[1]) && .sh_finite(gw[2])) {
    (gw[1] - gw[2]) / ast[2]
  } else {
    NA_real_
  }
  val <- sprintf("商譽／資產 %s→%s（差 %s）；商譽相對期初資產 %s",
                 .sh_pct_txt(gw_ast1), .sh_pct_txt(gw_ast0), .sh_pp_txt(gw_ast_pp),
                 .sh_pct_txt(vs_beg))
  thr <- sprintf("警示：商譽／資產 +≥%d pp 或商譽增額 ≥ 期初資產 %.0f%%；CFO／關聯交易仍資料不足",
                 th$gw_asset_pp, th$gw_vs_beg_assets * 100)
  if ((.sh_finite(gw_ast_pp) && gw_ast_pp >= th$gw_asset_pp) ||
      (.sh_finite(vs_beg) && vs_beg >= th$gw_vs_beg_assets)) {
    st <- "警示"; why <- "商譽或收購規模跳升。關聯交易與更換 CFO／會計師仍資料不足，未當作警示依據。"
  } else if (.sh_finite(gw_g) && gw_g >= 0.10) {
    st <- "觀察"; why <- "商譽有增加，規模尚未達警示。關聯交易／CFO 資料不足。"
  } else {
    st <- "通過"; why <- "商譽未大幅擴張。關聯交易與 CFO／會計師更換仍資料不足。"
  }
  .sh_item("X5", "跨手法", "大量併購／關聯交易／更換 CFO", st, why, val, thr)
}

.sh_eval_x6 <- function(ctx, th) {
  # Yahoo 年報幾乎沒有非 GAAP 欄位；現金品質改由 X1／#11 承擔。
  .sh_na_item(
    "X6", "跨手法", "大力推非 GAAP 且定義常改",
    "財報序列沒有穩定的非 GAAP 欄位，不臆測調整後獲利。現金與 GAAP 落差見 X1、手法 11。"
  )
}

# ----- 15 手法 -----
.sh_eval_01 <- function(ctx, th) {
  if (length(ctx$rev) < 2L || length(ctx$ocf) < 2L) {
    return(.sh_na_item("1", "操弄盈餘", "提前認列營收", "營收或營業現金流年期不足。"))
  }
  rev_g <- .sh_yoy(ctx$rev)
  ocf_g <- .sh_yoy(ctx$ocf)
  ar_g <- .sh_yoy(ctx$ar)
  ca_g <- .sh_yoy(ctx$contract_ast)
  gap_ar <- .sh_pp(ar_g, rev_g)
  gap_ca <- .sh_pp(ca_g, rev_g)
  cash_lag <- .sh_finite(rev_g) && .sh_finite(ocf_g) && (rev_g - ocf_g) >= 0.15
  ar_surge <- (.sh_finite(gap_ar) && gap_ar >= th$ar_vs_rev_alert_pp) ||
    (.sh_finite(gap_ca) && gap_ca >= th$ar_vs_rev_alert_pp)
  val <- sprintf("營收 YoY %s；OCF YoY %s；應收差 %s；合約資產差 %s",
                 .sh_pct_txt(rev_g), .sh_pct_txt(ocf_g), .sh_pp_txt(gap_ar), .sh_pp_txt(gap_ca))
  thr <- "警示：營收明顯成長，但現金與應收／合約資產品質跟不上"
  if (.sh_finite(rev_g) && rev_g >= 0.08 && isTRUE(cash_lag) && isTRUE(ar_surge)) {
    st <- "警示"; why <- "營收往前衝，應收或合約資產更快，現金沒跟上，有提前認列疑慮。"
  } else if (isTRUE(cash_lag) || isTRUE(ar_surge)) {
    st <- "觀察"; why <- "營收與現金／應收步調不一致，尚未同時達到警示組合。"
  } else if (length(ctx$ar) < 1L && length(ctx$contract_ast) < 1L) {
    st <- "資料不足"; why <- "無應收或合約資產，無法評估出貨品質。"
  } else {
    st <- "通過"; why <- "未同時出現營收暴衝、應收惡化與現金落後。"
  }
  .sh_item("1", "操弄盈餘", "提前認列營收", st, why, val, thr)
}

.sh_eval_02 <- function(ctx, th, industry_key) {
  gm <- .sh_gm(ctx, 1L)
  if (!.sh_finite(gm) || length(ctx$rev) < 1L) {
    return(.sh_na_item("2", "操弄盈餘", "認列假營收", "缺毛利或營收，無法比對獲利結構。"))
  }
  band <- .sh_ind_gm(industry_key)
  gm_pct <- gm * 100
  own <- ctx$gp / ctx$rev
  own <- own[is.finite(own) & is.finite(ctx$rev) & ctx$rev != 0]
  own_mean <- if (length(own) >= 3L) mean(own, na.rm = TRUE) * 100 else NA_real_
  vs_own <- if (.sh_finite(own_mean)) gm_pct - own_mean else NA_real_
  vs_hi <- if (!is.null(band)) gm_pct - band$hi else NA_real_
  capex_rev <- .sh_ratio(if (length(ctx$capex)) abs(ctx$capex[1]) else NA_real_, ctx$rev[1])
  val <- sprintf("本期毛利率 %.1f%%；相對產業上限 %s；相對自身均值 %s；資本支出／營收 %s",
                 gm_pct, .sh_pp_txt(vs_hi), .sh_pp_txt(vs_own), .sh_pct_txt(capex_rev))
  thr <- sprintf("警示：毛利率高於產業上限 ≥%d pp（員工數資料不足）", th$gm_vs_ind_alert_pp)
  if (.sh_finite(vs_hi) && vs_hi >= th$gm_vs_ind_alert_pp) {
    st <- "警示"; why <- "毛利率遠高於所選產業區間上限，需核對是否混入低成本或虛增營收。員工數資料不足。"
  } else if ((.sh_finite(vs_hi) && vs_hi >= th$gm_vs_ind_watch_pp) ||
             (.sh_finite(vs_own) && vs_own >= th$gm_vs_own_watch_pp)) {
    st <- "觀察"; why <- "毛利率高於產業或自身歷史，尚不足以單獨當成假營收。員工數資料不足。"
  } else if (is.null(band) && length(own) < 3L) {
    st <- "資料不足"; why <- "無產業毛利率區間，自身年期也不足；員工數資料不足。"
  } else {
    st <- "通過"; why <- "毛利率未遠離產業或自身歷史。員工數資料不足，未用來判假營收。"
  }
  .sh_item("2", "操弄盈餘", "認列假營收", st, why, val, thr)
}

.sh_eval_03 <- function(ctx, th) {
  if (length(ctx$ni) < 2L || length(ctx$opinc) < 2L) {
    return(.sh_na_item("3", "操弄盈餘", "一次性活動增加收入", "缺營業利益或淨利年期。"))
  }
  ni_g <- .sh_yoy(ctx$ni)
  op_g <- .sh_yoy(ctx$opinc)
  fork <- .sh_finite(ni_g) && .sh_finite(op_g) && ni_g >= 0.10 && (ni_g - op_g) >= 0.15
  oi_rev_pp <- .sh_pp(
    .sh_ratio(if (length(ctx$other_inc) >= 1L) ctx$other_inc[1] else NA_real_, ctx$rev[1]),
    .sh_ratio(if (length(ctx$other_inc) >= 2L) ctx$other_inc[2] else NA_real_,
              if (length(ctx$rev) >= 2L) ctx$rev[2] else NA_real_)
  )
  val <- sprintf("淨利 YoY %s vs 營業利益 YoY %s；其他收益／營收差 %s",
                 .sh_pct_txt(ni_g), .sh_pct_txt(op_g), .sh_pp_txt(oi_rev_pp))
  thr <- sprintf("警示：淨利明顯快於本業，或業外收益佔營收年增 ≥%d pp", th$other_inc_rev_pp)
  if (isTRUE(fork) || (.sh_finite(oi_rev_pp) && oi_rev_pp >= th$other_inc_rev_pp)) {
    st <- "警示"; why <- "淨利與本業營業利益分叉，或業外收益突然變大，永續性存疑。"
  } else if (.sh_finite(oi_rev_pp) && oi_rev_pp >= 1.5) {
    st <- "觀察"; why <- "業外收益有抬升，幅度尚未達警示。"
  } else {
    st <- "通過"; why <- "淨利與營業利益步調未明顯分叉。"
  }
  .sh_item("3", "操弄盈餘", "一次性活動增加收入", st, why, val, thr)
}

.sh_eval_04 <- function(ctx, th) {
  intang <- ctx$intang
  if (length(intang) < 2L && length(ctx$ppe) < 2L) {
    return(.sh_na_item("4", "操弄盈餘", "把當期費用移到後期", "無無形資產或不動產設備可看資本化。"))
  }
  int_g <- .sh_yoy(intang)
  rev_g <- .sh_yoy(ctx$rev)
  gap <- .sh_pp(int_g, rev_g)
  dep_rate0 <- .sh_ratio(if (length(ctx$dep)) abs(ctx$dep[1]) else NA_real_,
                        if (length(ctx$ppe)) ctx$ppe[1] else ctx$assets[1])
  dep_rate1 <- .sh_ratio(if (length(ctx$dep) >= 2L) abs(ctx$dep[2]) else NA_real_,
                        if (length(ctx$ppe) >= 2L) ctx$ppe[2] else if (length(ctx$assets) >= 2L) ctx$assets[2] else NA_real_)
  dep_drop <- .sh_finite(dep_rate0) && .sh_finite(dep_rate1) && dep_rate1 > 0 &&
    (dep_rate1 - dep_rate0) / dep_rate1 >= 0.20
  allow_ar0 <- .sh_ratio(if (length(ctx$allow)) abs(ctx$allow[1]) else NA_real_, ctx$ar[1])
  allow_ar1 <- .sh_ratio(if (length(ctx$allow) >= 2L) abs(ctx$allow[2]) else NA_real_,
                        if (length(ctx$ar) >= 2L) ctx$ar[2] else NA_real_)
  allow_drop <- .sh_finite(allow_ar0) && .sh_finite(allow_ar1) && allow_ar0 < allow_ar1 * 0.8
  val <- sprintf("無形資產 YoY %s vs 營收 YoY %s（差 %s）；折舊率 %s→%s",
                 .sh_pct_txt(int_g), .sh_pct_txt(rev_g), .sh_pp_txt(gap),
                 .sh_pct_txt(dep_rate1), .sh_pct_txt(dep_rate0))
  thr <- sprintf("警示：無形資產成長超營收 ≥%d pp", th$intang_vs_rev_pp)
  if (.sh_finite(gap) && gap >= th$intang_vs_rev_pp) {
    st <- "警示"; why <- "無形資產擴張遠快於營收，費用可能被資本化延後。"
  } else if (isTRUE(dep_drop) || isTRUE(allow_drop)) {
    st <- "觀察"
    why <- if (isTRUE(allow_drop)) {
      "備抵呆帳佔應收下降。若收款同時變差，費用可能被藏住。"
    } else {
      "折舊率偏低，資產可能折得太慢。"
    }
  } else {
    st <- "通過"; why <- "未看到無形資產暴衝或折舊率明顯下滑。"
  }
  .sh_item("4", "操弄盈餘", "把當期費用移到後期", st, why, val, thr)
}

.sh_eval_05 <- function(ctx, th) {
  if (length(ctx$opex) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("5", "操弄盈餘", "隱藏費用或損失", "缺營業費用或營收年期。"))
  }
  ox0 <- .sh_ratio(ctx$opex[1], ctx$rev[1])
  ox1 <- .sh_ratio(ctx$opex[2], ctx$rev[2])
  drop_pp <- .sh_pp(ox0, ox1)  # negative = opex ratio down
  rev_g <- .sh_yoy(ctx$rev)
  stable_biz <- .sh_finite(rev_g) && abs(rev_g) <= 0.10
  val <- sprintf("費用率 %s→%s（差 %s）；營收 YoY %s",
                 .sh_pct_txt(ox1), .sh_pct_txt(ox0), .sh_pp_txt(drop_pp), .sh_pct_txt(rev_g))
  thr <- sprintf("警示：業務大致沒變（營收 ±10%%）但費用率下降 ≥%d pp", th$opex_drop_alert_pp)
  if (isTRUE(stable_biz) && .sh_finite(drop_pp) && drop_pp <= -th$opex_drop_alert_pp) {
    st <- "警示"; why <- "營收沒有大變，費用率卻突然下降，可能少提準備或把費用藏起來。"
  } else if (.sh_finite(drop_pp) && drop_pp <= -th$opex_drop_watch_pp) {
    st <- "觀察"; why <- "費用率下降，需對照是否真有效率改善。"
  } else {
    st <- "通過"; why <- "費用率未在業務平穩時突然下降。"
  }
  .sh_item("5", "操弄盈餘", "隱藏費用或損失", st, why, val, thr)
}

.sh_eval_06 <- function(ctx, th) {
  def <- ctx$defrev
  ni <- ctx$ni
  rev <- ctx$rev
  def_g <- .sh_yoy(def)
  rev_g <- .sh_yoy(rev)
  gap <- .sh_pp(def_g, rev_g)
  n_m <- min(length(ni), length(rev))
  ni_m <- if (n_m < 1L) {
    numeric(0)
  } else {
    v <- ni[seq_len(n_m)] / rev[seq_len(n_m)]
    v[is.finite(v)]
  }
  smooth2 <- length(ni_m) >= 4L && (max(ni_m) - min(ni_m)) < 0.015
  if (length(def) < 2L && !isTRUE(smooth2)) {
    return(.sh_na_item("6", "操弄盈餘", "把當期收益移到後期", "無遞延收入，獲利年期也不足以看平滑。"))
  }
  val <- sprintf("遞延收入 YoY %s vs 營收 YoY %s（差 %s）；淨利率區間 %s",
                 .sh_pct_txt(def_g), .sh_pct_txt(rev_g), .sh_pp_txt(gap),
                 if (length(ni_m) >= 2L) sprintf("%.1f–%.1f%%", min(ni_m) * 100, max(ni_m) * 100) else "—")
  thr <- sprintf("警示：遞延收入成長超營收 ≥%d pp", th$defrev_vs_rev_pp)
  if (.sh_finite(gap) && gap >= th$defrev_vs_rev_pp) {
    st <- "警示"; why <- "預收／遞延收入異常增加，可能把收益往後挪或提前收現。"
  } else if (isTRUE(smooth2) && length(rev) >= 4L) {
    st <- "觀察"; why <- "獲利異常平滑，需對照準備金與遞延項目（未必是操弄）。"
  } else if (length(def) < 2L) {
    st <- "資料不足"; why <- "無遞延收入序列。"
  } else {
    st <- "通過"; why <- "遞延收入未異常快於營收，獲利也沒有過度平滑。"
  }
  .sh_item("6", "操弄盈餘", "把當期收益移到後期", st, why, val, thr)
}

.sh_eval_07 <- function(ctx, th) {
  imp <- ctx$impair
  if (length(imp) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("7", "操弄盈餘", "把未來費用移到當期（大洗澡）", "無減損／非常項目或年期不足。"))
  }
  # Newest is year t; look at prior year t-1 bath then t rebound.
  bath_prior <- .sh_finite(imp[2]) && .sh_finite(ctx$rev[2]) && ctx$rev[2] != 0 &&
    (imp[2] / ctx$rev[2]) <= th$bath_impair_rev
  gm_d <- .sh_pp(.sh_gm(ctx, 1L), .sh_gm(ctx, 2L))
  ox0 <- .sh_ratio(if (length(ctx$opex)) ctx$opex[1] else NA_real_, ctx$rev[1])
  ox1 <- .sh_ratio(if (length(ctx$opex) >= 2L) ctx$opex[2] else NA_real_,
                   if (length(ctx$rev) >= 2L) ctx$rev[2] else NA_real_)
  ox_improve <- .sh_finite(ox0) && .sh_finite(ox1) && (ox1 - ox0) * 100 >= 3
  bath_now <- .sh_finite(imp[1]) && .sh_finite(ctx$rev[1]) && ctx$rev[1] != 0 &&
    (imp[1] / ctx$rev[1]) <= th$bath_impair_rev
  val <- sprintf("前期減損／營收 %s；本期毛利率差 %s；本期減損／營收 %s",
                 .sh_pct_txt(.sh_ratio(imp[2], ctx$rev[2])), .sh_pp_txt(gm_d),
                 .sh_pct_txt(.sh_ratio(imp[1], ctx$rev[1])))
  thr <- sprintf("警示：前期減損 ≤%.0f%% 營收且本期毛利率回升 ≥%d pp",
                 abs(th$bath_impair_rev) * 100, th$bath_gm_rebound_pp)
  if (isTRUE(bath_prior) && ((.sh_finite(gm_d) && gm_d >= th$bath_gm_rebound_pp) || isTRUE(ox_improve))) {
    st <- "警示"; why <- "大額減損後毛利或費用率神奇改善，有大洗澡後美化後續期間的味道。"
  } else if (isTRUE(bath_now) || isTRUE(bath_prior)) {
    st <- "觀察"; why <- "有大額減損，但尚未看到下一期獲利結構大幅回升。"
  } else {
    st <- "通過"; why <- "沒有「先大減損、再突然變好看」的組合。"
  }
  .sh_item("7", "操弄盈餘", "把未來費用移到當期（大洗澡）", st, why, val, thr)
}

.sh_eval_08 <- function(ctx, th) {
  if (length(ctx$ocf) < 2L) {
    return(.sh_na_item("8", "現金流舞弊", "融資現金流入移到營業活動", "營業現金流年期不足。"))
  }
  ocf_g <- .sh_yoy(ctx$ocf)
  ar_g <- .sh_yoy(ctx$ar)
  debt_g <- .sh_yoy(ctx$debt)
  reverse <- length(ctx$ocf) >= 3L && .sh_finite(ctx$ocf[1]) && .sh_finite(ctx$ocf[2]) &&
    .sh_finite(ctx$ocf[3]) && ctx$ocf[2] > ctx$ocf[3] * 1.4 && ctx$ocf[1] < ctx$ocf[2] * 0.7
  val <- sprintf("OCF YoY %s；應收 YoY %s；有息負債 YoY %s",
                 .sh_pct_txt(ocf_g), .sh_pct_txt(ar_g), .sh_pct_txt(debt_g))
  thr <- sprintf("警示：OCF 暴增 ≥%.0f%% 且應收驟降 ≤%.0f%%（或隔期反轉）",
                 th$oc_f_surge * 100, abs(th$ar_drop_factor) * 100)
  if ((.sh_finite(ocf_g) && ocf_g >= th$oc_f_surge && .sh_finite(ar_g) && ar_g <= th$ar_drop_factor) ||
      isTRUE(reverse)) {
    st <- "警示"; why <- "營業現金流跳升伴隨應收驟降或隔期反轉，可能把應收出售／融資當成營運現金。"
  } else if (.sh_finite(ocf_g) && ocf_g >= th$oc_f_surge && .sh_finite(debt_g) && debt_g >= 0.15) {
    st <- "觀察"; why <- "OCF 與有息負債同步上升，需核對分類是否把融資當營運。"
  } else if (length(ctx$ar) < 2L) {
    st <- "資料不足"; why <- "無應收序列，無法看應收出售跡象。"
  } else {
    st <- "通過"; why <- "未出現 OCF 暴增搭配應收驟降或隔期反轉。"
  }
  .sh_item("8", "現金流舞弊", "融資現金流入移到營業活動", st, why, val, thr)
}

.sh_eval_09 <- function(ctx, th) {
  if (length(ctx$ocf) < 1L || length(ctx$fcf) < 1L) {
    return(.sh_na_item("9", "現金流舞弊", "營運現金流出移到其他活動", "缺營業現金流或自由現金流。"))
  }
  ocf <- ctx$ocf[1]
  fcf <- ctx$fcf[1]
  cap_g <- .sh_yoy(ctx$capex)
  rev_g <- .sh_yoy(ctx$rev)
  cap_vs_dep <- .sh_ratio(if (length(ctx$capex)) abs(ctx$capex[1]) else NA_real_,
                         if (length(ctx$dep)) abs(ctx$dep[1]) else NA_real_)
  val <- sprintf("OCF %s；FCF %s；資本支出／折舊 %s",
                 .sh_num_txt(ocf, 0), .sh_num_txt(fcf, 0), .sh_num_txt(cap_vs_dep, 2, "×"))
  thr <- "警示：營業現金為負；觀察：OCF 為正但 FCF 為負且資本支出成長遠快於營收"
  if (.sh_finite(ocf) && ocf < 0) {
    st <- "警示"; why <- "核心營業現金流為負，本業在失血（舊有舞弊檢核一併納入）。"
  } else if (.sh_finite(ocf) && ocf > 0 && .sh_finite(fcf) && fcf < 0 &&
             .sh_finite(cap_g) && .sh_finite(rev_g) && (cap_g - rev_g) >= 0.25) {
    st <- "觀察"; why <- "帳上營業現金看起來不錯，自由現金流卻很差，投資流出偏像日常開銷或過度資本化。"
  } else if (.sh_finite(ocf) && ocf > 0 && .sh_finite(fcf) && fcf < 0) {
    st <- "觀察"; why <- "自由現金流為負。成長型資本支出也可能如此，需對照折舊與無形資產。"
  } else {
    st <- "通過"; why <- "營業現金與自由現金流未出現「帳上很好、口袋很差」的組合。"
  }
  .sh_item("9", "現金流舞弊", "營運現金流出移到其他活動", st, why, val, thr)
}

.sh_eval_10 <- function(ctx, th) {
  if (length(ctx$ocf) < 2L) {
    return(.sh_na_item("10", "現金流舞弊", "不可持續活動增加營運現金流", "營業現金流年期不足。"))
  }
  cogs <- if (length(ctx$cogs)) ctx$cogs else ctx$rev
  dpo0 <- .sh_days(ctx$ap[1], cogs[1])
  dpo1 <- if (length(ctx$ap) >= 2L && length(cogs) >= 2L) .sh_days(ctx$ap[2], cogs[2]) else NA_real_
  dpo_d <- if (.sh_finite(dpo0) && .sh_finite(dpo1)) dpo0 - dpo1 else NA_real_
  def_g <- .sh_yoy(ctx$defrev)
  inv_g <- .sh_yoy(ctx$inv)
  gm_d <- .sh_pp(.sh_gm(ctx, 1L), .sh_gm(ctx, 2L))
  ocf_up <- .sh_yoy(ctx$ocf)
  val <- sprintf("DPO %+s；預收 YoY %s；存貨 YoY %s；毛利率 %s；OCF YoY %s",
                 .sh_num_txt(dpo_d, 0, " 天"), .sh_pct_txt(def_g), .sh_pct_txt(inv_g),
                 .sh_pp_txt(gm_d), .sh_pct_txt(ocf_up))
  thr <- sprintf("警示：DPO +≥%d 天，或存貨大降且毛利惡化，同時 OCF 上升", th$dpo_alert_days)
  stretch <- .sh_finite(dpo_d) && dpo_d >= th$dpo_alert_days
  prepay <- .sh_finite(def_g) && def_g >= 0.25
  destock <- .sh_finite(inv_g) && inv_g <= -0.15 && .sh_finite(gm_d) && gm_d <= -th$gm_drop_alert_pp
  ocf_ok <- .sh_finite(ocf_up) && ocf_up >= 0.10
  if (isTRUE(ocf_ok) && (isTRUE(stretch) || isTRUE(prepay) || isTRUE(destock))) {
    st <- "警示"; why <- "營業現金改善來自拖長付款、預收增加或去化存貨，較難持續。"
  } else if (isTRUE(stretch) || isTRUE(destock) || isTRUE(prepay)) {
    st <- "觀察"; why <- "營運資金有撐現金的跡象，OCF 尚未同步大增。"
  } else if (length(ctx$ap) < 2L && length(ctx$defrev) < 2L && length(ctx$inv) < 2L) {
    st <- "資料不足"; why <- "無應付、預收或存貨可看營運資金品質。"
  } else {
    st <- "通過"; why <- "未看到靠拖款、預收或去庫存把營業現金推高。"
  }
  .sh_item("10", "現金流舞弊", "不可持續活動增加營運現金流", st, why, val, thr)
}

.sh_eval_11 <- function(ctx, th) {
  ni <- ctx$ni
  ocf <- ctx$ocf
  if (length(ni) < 1L || length(ocf) < 1L || !.sh_finite(ni[1]) || !.sh_finite(ocf[1])) {
    return(.sh_na_item("11", "關鍵指標舞弊", "誤導性指標誇大業績", "缺淨利或營業現金流。"))
  }
  conv <- if (ni[1] > 0) ocf[1] / ni[1] else NA_real_
  val <- sprintf("OCF／淨利 %s（非 GAAP 欄位：資料不足）", .sh_num_txt(conv, 2, "×"))
  thr <- sprintf("無非 GAAP 時改看現金轉換：警示 <%.2f×，觀察 <%.2f×", th$cash_ni_alert, th$cash_ni_watch)
  if (.sh_finite(ni[1]) && ni[1] > 0 && .sh_finite(conv) && conv < th$cash_ni_alert) {
    st <- "警示"; why <- "沒有非 GAAP 欄位可核對調整後獲利。GAAP 淨利與營業現金流差距已偏大。"
  } else if (.sh_finite(ni[1]) && ni[1] > 0 && .sh_finite(conv) && conv < th$cash_ni_watch) {
    st <- "觀察"; why <- "非 GAAP 資料不足。現金轉換偏低，宜與 X1 一併看。"
  } else if (!.sh_finite(conv)) {
    st <- "資料不足"; why <- "非 GAAP 資料不足，且本期淨利非正，現金轉換不便比。"
  } else {
    st <- "通過"; why <- "非 GAAP 資料不足；GAAP 淨利與營業現金流落差尚可。"
  }
  .sh_item("11", "關鍵指標舞弊", "誤導性指標誇大業績", st, why, val, thr)
}

.sh_eval_12 <- function(ctx, th) {
  de <- .sh_ratio(ctx$debt[1], ctx$equity[1])
  wc0 <- if (length(ctx$ca) && length(ctx$cl)) ctx$ca[1] - ctx$cl[1] else NA_real_
  wc1 <- if (length(ctx$ca) >= 2L && length(ctx$cl) >= 2L) ctx$ca[2] - ctx$cl[2] else NA_real_
  wc_ast0 <- .sh_ratio(wc0, ctx$assets[1])
  wc_ast1 <- .sh_ratio(wc1, if (length(ctx$assets) >= 2L) ctx$assets[2] else NA_real_)
  wc_pp <- .sh_pp(wc_ast0, wc_ast1)
  if (!.sh_finite(de) && !.sh_finite(wc_pp)) {
    return(.sh_na_item("12", "關鍵指標舞弊", "扭曲資產負債表指標", "缺負債／權益或營運資金。"))
  }
  val <- sprintf("負債／權益 %s；營運資金／資產差 %s", .sh_num_txt(de, 2, "×"), .sh_pp_txt(wc_pp))
  thr <- sprintf("警示：負債／權益 >%.1f 或營運資金／資產年變 ≥%d pp", th$de_alert, th$wc_asset_pp)
  if (.sh_finite(de) && de > th$de_alert) {
    st <- "警示"; why <- "財務槓桿偏高（舊有負債／權益檢核）。升息或景氣下行時壓力較大。"
  } else if (.sh_finite(wc_pp) && abs(wc_pp) >= th$wc_asset_pp) {
    st <- "警示"; why <- "年底營運資金佔資產比重驟變，可能在美化週轉或槓桿外表。"
  } else if (.sh_finite(de) && de > th$de_watch) {
    st <- "觀察"; why <- "槓桿偏高但尚未超過 2 倍。"
  } else {
    st <- "通過"; why <- "槓桿與營運資金佔比沒有異常跳變。"
  }
  .sh_item("12", "關鍵指標舞弊", "扭曲資產負債表指標", st, why, val, thr)
}

.sh_eval_13 <- function(ctx, th) {
  gw_ast_pp <- .sh_pp(
    .sh_ratio(ctx$gw[1], ctx$assets[1]),
    .sh_ratio(if (length(ctx$gw) >= 2L) ctx$gw[2] else NA_real_,
              if (length(ctx$assets) >= 2L) ctx$assets[2] else NA_real_)
  )
  rev_g <- .sh_yoy(ctx$rev)
  ni_g <- .sh_yoy(ctx$ni)
  if (length(ctx$gw) < 2L) {
    return(.sh_na_item("13", "併購會計舞弊", "人為增加營收與盈餘", "無商譽年期，無法判斷併購灌水。"))
  }
  val <- sprintf("商譽／資產差 %s；營收 YoY %s；淨利 YoY %s",
                 .sh_pp_txt(gw_ast_pp), .sh_pct_txt(rev_g), .sh_pct_txt(ni_g))
  thr <- sprintf("警示：商譽／資產 +≥%d pp 且內生成長弱（營收 <5%%）但淨利變好", th$gw_asset_pp)
  weak_org <- .sh_finite(rev_g) && rev_g < 0.05
  ni_up <- .sh_finite(ni_g) && ni_g >= 0.10
  if (.sh_finite(gw_ast_pp) && gw_ast_pp >= th$gw_asset_pp && isTRUE(weak_org) && isTRUE(ni_up)) {
    st <- "警示"; why <- "商譽跳升、銷售沒有真正變快，獲利卻變好，併購會計美化盈餘的風險上升。"
  } else if (.sh_finite(gw_ast_pp) && gw_ast_pp >= th$gw_asset_pp) {
    st <- "觀察"; why <- "收購留下明顯商譽，內生成長仍需核對。"
  } else {
    st <- "通過"; why <- "沒有「商譽大增、銷售停滯、獲利突好」的組合。"
  }
  .sh_item("13", "併購會計舞弊", "人為增加營收與盈餘", st, why, val, thr)
}

.sh_eval_14 <- function(ctx, th) {
  if (length(ctx$ocf) < 2L || length(ctx$gw) < 2L) {
    return(.sh_na_item("14", "併購會計舞弊", "虛報現金流", "缺營業現金流或商譽年期。"))
  }
  ocf_g <- .sh_yoy(ctx$ocf)
  vs_beg <- if (length(ctx$assets) >= 2L && .sh_finite(ctx$assets[2]) && ctx$assets[2] > 0) {
    (ctx$gw[1] - ctx$gw[2]) / ctx$assets[2]
  } else {
    NA_real_
  }
  val <- sprintf("OCF YoY %s；商譽增額／期初資產 %s", .sh_pct_txt(ocf_g), .sh_pct_txt(vs_beg))
  thr <- "警示：OCF 大增（≥30%）且商譽同步跳升"
  if (.sh_finite(ocf_g) && ocf_g >= 0.30 && .sh_finite(vs_beg) && vs_beg >= 0.08) {
    st <- "警示"; why <- "營業現金流隨商譽／收購跳升，現金品質可能來自併購而非本業。"
  } else if (.sh_finite(ocf_g) && ocf_g >= 0.30 && .sh_finite(vs_beg) && vs_beg >= 0.03) {
    st <- "觀察"; why <- "OCF 與商譽同時上升，幅度尚未達警示。"
  } else {
    st <- "通過"; why <- "營業現金流未隨商譽同步暴衝。"
  }
  .sh_item("14", "併購會計舞弊", "虛報現金流", st, why, val, thr)
}

.sh_eval_15 <- function(ctx, th) {
  if (length(ctx$assets) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("15", "併購會計舞弊", "操縱關鍵指標（併購）", "資產或營收年期不足。"))
  }
  ast_g <- .sh_yoy(ctx$assets)
  at0 <- .sh_ratio(ctx$rev[1], ctx$assets[1])
  at1 <- .sh_ratio(ctx$rev[2], ctx$assets[2])
  at_drop <- if (.sh_finite(at0) && .sh_finite(at1) && at1 != 0) (at1 - at0) / abs(at1) else NA_real_
  dgw <- if (length(ctx$gw) >= 2L && .sh_finite(ctx$gw[1]) && .sh_finite(ctx$gw[2])) ctx$gw[1] - ctx$gw[2] else NA_real_
  dast <- if (.sh_finite(ctx$assets[1]) && .sh_finite(ctx$assets[2])) ctx$assets[1] - ctx$assets[2] else NA_real_
  gw_share <- .sh_ratio(dgw, dast)
  val <- sprintf("資產 YoY %s；週轉率變化 %s；商譽佔資產增額 %s",
                 .sh_pct_txt(ast_g), .sh_pct_txt(at_drop), .sh_pct_txt(gw_share))
  thr <- sprintf("警示：資產擴張 ≥%.0f%%、週轉變差 ≥%.0f%%，且成長多來自商譽",
                 th$asset_jump * 100, th$at_drop_rel * 100)
  if (.sh_finite(ast_g) && ast_g >= th$asset_jump &&
      .sh_finite(at_drop) && at_drop >= th$at_drop_rel &&
      .sh_finite(gw_share) && gw_share >= 0.50) {
    st <- "警示"; why <- "成長幾乎靠資產／商譽變大，銷售效率沒跟上。"
  } else if (.sh_finite(ast_g) && ast_g >= 0.12 && .sh_finite(at_drop) && at_drop >= 0.05) {
    st <- "觀察"; why <- "資產擴張快於銷售效率，併購稀釋週轉的味道。"
  } else if (length(ctx$gw) < 2L) {
    st <- "資料不足"; why <- "無商譽，無法判斷成長是否靠收購堆資產。"
  } else {
    st <- "通過"; why <- "未出現「資產變大、週轉變差、幾乎全是商譽」。"
  }
  .sh_item("15", "併購會計舞弊", "操縱關鍵指標（併購）", st, why, val, thr)
}

#' Schilit 15 + 跨手法 + 舊 Fraud Warning 合併評估
evaluate_shenanigans <- function(d_is, d_bs, d_cf, industry_key = NULL) {
  if (is.null(d_is) || is.null(d_bs) || is.null(d_cf) ||
      !is.data.frame(d_is) || !is.data.frame(d_bs) || !is.data.frame(d_cf) ||
      nrow(d_is) < 1L || nrow(d_bs) < 1L || nrow(d_cf) < 1L) {
    return(.empty_shenanigans(ok = FALSE, message = "載入損益、資產負債與現金流後，將自動判讀。"))
  }
  th <- SHENANIGAN_THRESHOLDS
  ctx <- tryCatch(.sh_ctx(d_is, d_bs, d_cf), error = function(e) NULL)
  if (is.null(ctx)) {
    return(.empty_shenanigans(ok = FALSE, message = "財報讀取失敗，略過自動判讀。"))
  }
  items <- tryCatch({
    rbind(
      .sh_eval_x1(ctx, th),
      .sh_eval_x2(ctx, th),
      .sh_eval_x3(ctx, th),
      .sh_eval_x4(ctx, th),
      .sh_eval_x5(ctx, th),
      .sh_eval_x6(ctx, th),
      .sh_eval_01(ctx, th),
      .sh_eval_02(ctx, th, industry_key),
      .sh_eval_03(ctx, th),
      .sh_eval_04(ctx, th),
      .sh_eval_05(ctx, th),
      .sh_eval_06(ctx, th),
      .sh_eval_07(ctx, th),
      .sh_eval_08(ctx, th),
      .sh_eval_09(ctx, th),
      .sh_eval_10(ctx, th),
      .sh_eval_11(ctx, th),
      .sh_eval_12(ctx, th),
      .sh_eval_13(ctx, th),
      .sh_eval_14(ctx, th),
      .sh_eval_15(ctx, th)
    )
  }, error = function(e) {
    .sh_item("ERR", "跨手法", "自動判讀", "資料不足",
             paste0("計算失敗：", e$message), "—", "—")
  })
  n_alert <- as.integer(sum(items$status == "警示", na.rm = TRUE))
  n_watch <- as.integer(sum(items$status == "觀察", na.rm = TRUE))
  n_pass <- as.integer(sum(items$status == "通過", na.rm = TRUE))
  n_na <- as.integer(sum(items$status == "資料不足", na.rm = TRUE))
  list(
    ok = TRUE,
    message = "",
    items = items,
    n_alert = n_alert,
    n_watch = n_watch,
    n_pass = n_pass,
    n_na = n_na
  )
}

.sh_status_class <- function(status) {
  switch(as.character(status)[1],
         "警示" = "ynow-shen-alert",
         "觀察" = "ynow-shen-watch",
         "通過" = "ynow-shen-pass",
         "ynow-shen-na")
}

.sh_item_card <- function(row) {
  tags$div(
    class = paste("ynow-shen-card", .sh_status_class(row$status)),
    tags$div(
      class = "ynow-shen-card-h",
      tags$span(class = "ynow-shen-code", paste0(row$code, "　", row$name)),
      tags$span(class = paste("ynow-shen-st", .sh_status_class(row$status)), row$status)
    ),
    tags$p(class = "ynow-shen-why", row$reason),
    tags$p(
      class = "ynow-shen-metrics",
      tags$b("目前："), row$value_txt,
      tags$span(class = "ynow-shen-sep", "｜"),
      tags$b("門檻："), row$threshold_txt
    )
  )
}

.sh_cat_block <- function(df, title) {
  if (is.null(df) || nrow(df) < 1L) return(NULL)
  tags$div(
    class = "ynow-shen-cat",
    tags$h5(tags$b(title)),
    lapply(seq_len(nrow(df)), function(i) .sh_item_card(df[i, ]))
  )
}

#' YNOW 分頁：四大類 + 跨手法紅旗（警示／觀察展開；資料不足摺疊）
shenanigans_results_ui <- function(ev) {
  if (is.null(ev) || !isTRUE(ev$ok)) {
    return(tags$div(
      class = "ynow-shen-wrap",
      tags$p(style = "color:#777;font-size:13px;", ev$message %||% "等待財報…")
    ))
  }
  items <- ev$items
  show <- items[items$status %in% c("警示", "觀察"), , drop = FALSE]
  pass <- items[items$status == "通過", , drop = FALSE]
  nas <- items[items$status == "資料不足", , drop = FALSE]
  xflag <- show[show$category == "跨手法", , drop = FALSE]
  cats <- list(
    "一、操弄盈餘" = show[show$category == "操弄盈餘", , drop = FALSE],
    "二、現金流舞弊" = show[show$category == "現金流舞弊", , drop = FALSE],
    "三、關鍵指標舞弊" = show[show$category == "關鍵指標舞弊", , drop = FALSE],
    "四、併購會計舞弊" = show[show$category == "併購會計舞弊", , drop = FALSE]
  )
  tags$div(
    class = "ynow-shen-wrap",
    tags$p(
      class = "ynow-shen-core",
      "會計問題通常在遮掩本業惡化（銷售放緩、毛利變薄、現金變差）。同時看損益、資產負債、現金流、附註精神、非 GAAP。"
    ),
    tags$p(
      class = "ynow-shen-count",
      tags$span(class = "ynow-shen-st ynow-shen-alert", paste0("警示 ", ev$n_alert, " 項")),
      tags$span(class = "ynow-shen-st ynow-shen-watch", paste0("觀察 ", ev$n_watch, " 項")),
      tags$span(class = "ynow-shen-st ynow-shen-pass", paste0("通過 ", ev$n_pass, " 項"))
    ),
    tags$div(
      class = "ynow-shen-strip",
      tags$h4(icon("exclamation-triangle"), " 跨手法紅旗"),
      if (nrow(xflag) < 1L) {
        tags$p(class = "ynow-shen-ok", "跨手法目前無警示或觀察。")
      } else {
        lapply(seq_len(nrow(xflag)), function(i) .sh_item_card(xflag[i, ]))
      }
    ),
    lapply(names(cats), function(nm) .sh_cat_block(cats[[nm]], nm)),
    if (nrow(show) < 1L) {
      tags$p(class = "ynow-shen-ok", "目前沒有警示或觀察。舊有五項現金／槓桿檢核已併入上表。")
    } else {
      NULL
    },
    if (nrow(pass) > 0L) {
      tags$details(
        class = "ynow-shen-fold",
        tags$summary(sprintf("通過 %d 項", nrow(pass))),
        lapply(seq_len(nrow(pass)), function(i) .sh_item_card(pass[i, ]))
      )
    } else {
      NULL
    },
    if (nrow(nas) > 0L) {
      tags$details(
        class = "ynow-shen-fold",
        tags$summary(sprintf("資料不足 %d 項（無欄位就不示警）", nrow(nas))),
        lapply(seq_len(nrow(nas)), function(i) .sh_item_card(nas[i, ]))
      )
    } else {
      NULL
    }
  )
}
