# =========================================================================
# shenanigans_module.R — Schilit《財報詭計》自動判讀（YNOW 分頁）
# 以已抓年報（損益／資產負債／現金流）做相對規則；缺欄位給「資料不足」，不捏造附註。
# 數列慣例：select_clean_metric_row 為 最新財年 → 最舊（不含 TTM）。
#
# 對齊參考（工程啟發式，非準則硬門檻）：
#   Schilit《財報詭計》15 手法 + 三表共通警訊
#   CT 財經〈財報地雷怎麼避〉／〈八個該逃命的警訊〉
#   HBR Sherman & Young〈Where Financial Reporting Still Falls Short〉
#     （非 GAAP、公允價值主觀、操弄營運決策而非帳本）
#
# 門檻摘要見 SHENANIGAN_THRESHOLDS 與 list_all_judgment_rules()。
# =========================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

SHENANIGAN_THRESHOLDS <- list(
  ar_vs_rev_alert_pp = 15,
  ar_vs_rev_watch_pp = 8,
  dso_alert_days = 10,
  dso_watch_days = 5,
  dio_watch_days = 15,
  inv_vs_rev_alert_pp = 15,
  gm_drop_alert_pp = 1.5,
  gm_drop_watch_pp = 3,
  rev_up_gm_alert_rev = 0.15,
  rev_up_gm_alert_pp = 3,
  rev_up_gm_watch_rev = 0.10,
  rev_up_gm_watch_pp = 1.5,
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
  defrev_lag_pp = 15,
  bath_impair_rev = -0.05,
  bath_gm_rebound_pp = 3,
  oc_f_surge = 0.40,
  ar_drop_factor = -0.20,
  at_drop_rel = 0.10,
  asset_jump = 0.20,
  cash_ni_alert = 0.50,
  cash_ni_watch = 0.70,
  gm_vs_ind_alert_pp = 15,
  gm_vs_ind_watch_pp = 5,
  gm_vs_own_watch_pp = 8,
  dpo_alert_days = 15,
  wc_asset_pp = 10,
  capex_dep_alert = 2.5,
  capex_dep_watch = 2.0,
  # X8 存貨↑但毛利↑（矛盾）
  inv_gm_contra_inv = 0.15,
  inv_gm_contra_gm_pp = 1.5,
  inv_gm_contra_watch_inv = 0.10,
  inv_gm_contra_watch_gm_pp = 0.5,
  # X9 流動性／利息保障（CT）
  current_ratio_alert = 1.0,
  quick_ratio_alert = 0.8,
  interest_cover_alert = 1.0,
  interest_cover_watch = 2.0,
  # X10 操弄營運決策（HBR #5）
  disc_cut_alert_pp = 3,
  disc_cut_watch_pp = 1.5,
  overprod_inv = 0.15,
  overprod_cogs_pp = 2,
  # X11 公允價值／無形主觀（HBR #4）
  soft_asset_alert = 0.40,
  soft_asset_jump_pp = 8
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
  int_pat <- if (exists("INTEREST_EXPENSE_PATTERNS")) {
    INTEREST_EXPENSE_PATTERNS
  } else {
    c("^Interest Expense$", "Interest Expense Non Operating", "Net Interest Expense")
  }
  list(
    rev = .sh_pick(d_is, c("^Total Revenue$")),
    gp = .sh_pick(d_is, c("^Gross Profit$")),
    cogs = .sh_pick(d_is, c("^Cost Of Revenue$", "^Cost Of Goods Sold$")),
    opex = .sh_pick(d_is, OPEX_PATTERNS),
    rd = .sh_pick(d_is, c(
      "^Research And Development$",
      "Research & Development",
      "Research Development"
    )),
    sga = .sh_pick(d_is, c(
      "^Selling General And Administration$",
      "^Selling General Administrative$",
      "SG&A",
      "Selling And Marketing Expense"
    )),
    opinc = .sh_pick(d_is, c("^Operating Income$")),
    pretax = .sh_pick(d_is, c("^Pretax Income$")),
    interest = .sh_pick(d_is, int_pat),
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

# ----- 三表共通警訊（不是第 16 招；損益／資產負債／現金流對起來的味道） -----
.sh_eval_x1 <- function(ctx, th) {
  ni <- ctx$ni
  ocf <- ctx$ocf
  oe <- ctx$op_earn
  if (length(ni) < 1L || length(ocf) < 1L) {
    return(.sh_na_item("X1", "三表共通警訊", "淨利長期高於營業現金流", "損益或現金流缺淨利／營業現金流。"))
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
  .sh_item("X1", "三表共通警訊", "淨利長期高於營業現金流", st, why, val, thr)
}

.sh_eval_x2 <- function(ctx, th) {
  if (length(ctx$ar) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("X2", "三表共通警訊", "應收成長快過營收、收款天數變長", "資產負債表無應收帳款，或年期不足。"))
  }
  ar_g <- .sh_yoy(ctx$ar)
  rev_g <- .sh_yoy(ctx$rev)
  gap <- .sh_pp(ar_g, rev_g)
  dso0 <- .sh_days(ctx$ar[1], ctx$rev[1])
  dso1 <- .sh_days(ctx$ar[2], ctx$rev[2])
  dso_d <- if (.sh_finite(dso0) && .sh_finite(dso1)) dso0 - dso1 else NA_real_
  val <- sprintf("應收 YoY %s vs 營收 YoY %s（差 %s）；收款天數 %s→%s 天",
                 .sh_pct_txt(ar_g), .sh_pct_txt(rev_g), .sh_pp_txt(gap),
                 .sh_num_txt(dso1, 0), .sh_num_txt(dso0, 0))
  thr <- sprintf("警示：應收快於營收 ≥%d pp 且收款天數 +≥%d 天", th$ar_vs_rev_alert_pp, th$dso_alert_days)
  if (.sh_finite(gap) && gap >= th$ar_vs_rev_alert_pp && .sh_finite(dso_d) && dso_d >= th$dso_alert_days) {
    st <- "警示"; why <- "應收成長明顯快於營收，收款天數也拉長，出貨品質或提前認列風險上升。"
  } else if ((.sh_finite(gap) && gap >= th$ar_vs_rev_watch_pp) ||
             (.sh_finite(dso_d) && dso_d >= th$dso_watch_days)) {
    st <- "觀察"; why <- "應收或收款天數轉差，尚未同時達到警示門檻。"
  } else {
    st <- "通過"; why <- "應收與營收步調大致一致，收款天數未明顯惡化。"
  }
  .sh_item("X2", "三表共通警訊", "應收成長快過營收、收款天數變長", st, why, val, thr)
}

.sh_eval_x3 <- function(ctx, th) {
  if (length(ctx$inv) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("X3", "三表共通警訊", "存貨增速高於營收且毛利下滑", "無存貨科目（金融／軟體常見）或年期不足。"))
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
  thr <- sprintf("警示：存貨快於營收 ≥%d pp 且毛利率下滑 ≥%.1f pp；存貨天數 +≥%d 天為觀察",
                 th$inv_vs_rev_alert_pp, th$gm_drop_alert_pp, th$dio_watch_days)
  slow <- .sh_finite(dio_d) && dio_d >= th$dio_watch_days
  if (.sh_finite(gap) && gap >= th$inv_vs_rev_alert_pp &&
      .sh_finite(gm_d) && gm_d <= -th$gm_drop_alert_pp) {
    st <- "警示"; why <- "存貨堆積快於銷售，同時毛利率下滑，滯銷或促銷壓力上升。"
  } else if ((.sh_finite(gap) && gap >= th$ar_vs_rev_watch_pp) ||
             (.sh_finite(gm_d) && gm_d <= -th$gm_drop_watch_pp) || isTRUE(slow)) {
    st <- "觀察"
    why <- if (isTRUE(slow)) {
      "存貨週轉天數明顯拉長，需對照同業與跌價提列。"
    } else {
      "存貨、週轉或毛利其中一項轉差，尚未同時達警示。"
    }
  } else {
    st <- "通過"; why <- "存貨與營收步調尚可，毛利率未明顯惡化。"
  }
  .sh_item("X3", "三表共通警訊", "存貨增速高於營收且毛利下滑", st, why, val, thr)
}

.sh_eval_x4 <- function(ctx, th) {
  # 年報無法驗證「季底出貨」；只評一次性收益與反覆非常項目。
  if (length(ctx$rev) < 1L) {
    return(.sh_na_item("X4", "三表共通警訊", "一次性收益／反覆非常項目", "損益表缺營收。"))
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
      "X4", "三表共通警訊", "一次性收益／反覆非常項目", "資料不足",
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
  .sh_item("X4", "三表共通警訊", "一次性收益／反覆非常項目", st, why, val, thr)
}

.sh_eval_x5 <- function(ctx, th) {
  gw <- ctx$gw
  ast <- ctx$assets
  acq <- ctx$acq
  has_gw <- length(gw) >= 1L && any(is.finite(gw))
  has_acq <- length(acq) >= 1L && any(is.finite(acq) & acq != 0)
  if (!has_gw && !has_acq) {
    return(.sh_na_item(
      "X5", "三表共通警訊", "大量購併／關係人交易／更換財務長",
      "無商譽或收購現金流可評購併；關係人交易與財務長／會計師更換資料不足。"
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
  thr <- sprintf("警示：商譽／資產 +≥%d pp 或商譽增額 ≥ 期初資產 %.0f%%；財務長／關係人交易仍資料不足",
                 th$gw_asset_pp, th$gw_vs_beg_assets * 100)
  if ((.sh_finite(gw_ast_pp) && gw_ast_pp >= th$gw_asset_pp) ||
      (.sh_finite(vs_beg) && vs_beg >= th$gw_vs_beg_assets)) {
    st <- "警示"; why <- "商譽或收購規模跳升。關係人交易與更換財務長／會計師仍資料不足，未當作警示依據。"
  } else if (.sh_finite(gw_g) && gw_g >= 0.10) {
    st <- "觀察"; why <- "商譽有增加，規模尚未達警示。關係人交易／財務長資料不足。"
  } else {
    st <- "通過"; why <- "商譽未大幅擴張。關係人交易與財務長／會計師更換仍資料不足。"
  }
  .sh_item("X5", "三表共通警訊", "大量購併／關係人交易／更換財務長", st, why, val, thr)
}

.sh_eval_x6 <- function(ctx, th) {
  # Yahoo 年報幾乎沒有非 GAAP 欄位；現金品質改由 X1／#11 承擔。
  .sh_na_item(
    "X6", "三表共通警訊", "常講調整後獲利、定義卻常改",
    "年報序列沒有穩定的「調整後獲利」欄位，不臆測。現金與帳面獲利落差見 X1、第 11 項。"
  )
}

.sh_eval_x7 <- function(ctx, th) {
  # CT：營收成長遠快於本業體質，但毛利率／淨利率反而下降。
  if (length(ctx$rev) < 2L || length(ctx$gp) < 2L) {
    return(.sh_na_item("X7", "三表共通警訊", "營收成長但毛利／淨利率下滑", "營收或毛利年期不足。"))
  }
  rev_g <- .sh_yoy(ctx$rev)
  gm_d <- .sh_pp(.sh_gm(ctx, 1L), .sh_gm(ctx, 2L))
  nm0 <- .sh_ratio(ctx$ni[1], ctx$rev[1])
  nm1 <- .sh_ratio(if (length(ctx$ni) >= 2L) ctx$ni[2] else NA_real_,
                   if (length(ctx$rev) >= 2L) ctx$rev[2] else NA_real_)
  nm_d <- .sh_pp(nm0, nm1)
  margin_drop <- (.sh_finite(gm_d) && gm_d <= -th$rev_up_gm_alert_pp) ||
    (.sh_finite(nm_d) && nm_d <= -th$rev_up_gm_alert_pp)
  margin_watch <- (.sh_finite(gm_d) && gm_d <= -th$rev_up_gm_watch_pp) ||
    (.sh_finite(nm_d) && nm_d <= -th$rev_up_gm_watch_pp)
  val <- sprintf("營收 YoY %s；毛利率 %s；淨利率 %s",
                 .sh_pct_txt(rev_g), .sh_pp_txt(gm_d), .sh_pp_txt(nm_d))
  thr <- sprintf("警示：營收 YoY≥%.0f%% 且毛利或淨利率下滑 ≥%d pp",
                 th$rev_up_gm_alert_rev * 100, th$rev_up_gm_alert_pp)
  if (.sh_finite(rev_g) && rev_g >= th$rev_up_gm_alert_rev && isTRUE(margin_drop)) {
    st <- "警示"; why <- "營收在衝，毛利或淨利率卻變薄，成長品質可疑（促銷、塞貨或成本失控）。"
  } else if (.sh_finite(rev_g) && rev_g >= th$rev_up_gm_watch_rev && isTRUE(margin_watch)) {
    st <- "觀察"; why <- "營收成長伴隨獲利率下滑，尚未達警示組合。"
  } else {
    st <- "通過"; why <- "未出現「營收變快、獲利率變差」的組合。"
  }
  .sh_item("X7", "三表共通警訊", "營收成長但毛利／淨利率下滑", st, why, val, thr)
}

.sh_eval_x8 <- function(ctx, th) {
  # CT：存貨堆積但毛利率紋風不動／上升 → 成本可能藏進存貨；亦呼應過度生產。
  if (length(ctx$inv) < 2L || length(ctx$gp) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("X8", "三表共通警訊", "存貨堆積但毛利率上升（矛盾）",
                       "無存貨或毛利年期（金融／軟體常見）。"))
  }
  inv_g <- .sh_yoy(ctx$inv)
  gm_d <- .sh_pp(.sh_gm(ctx, 1L), .sh_gm(ctx, 2L))
  val <- sprintf("存貨 YoY %s；毛利率 %s", .sh_pct_txt(inv_g), .sh_pp_txt(gm_d))
  thr <- sprintf("警示：存貨 YoY≥%.0f%% 且毛利率上升 ≥%.1f pp",
                 th$inv_gm_contra_inv * 100, th$inv_gm_contra_gm_pp)
  if (.sh_finite(inv_g) && inv_g >= th$inv_gm_contra_inv &&
      .sh_finite(gm_d) && gm_d >= th$inv_gm_contra_gm_pp) {
    st <- "警示"; why <- "存貨在堆，毛利率卻上升，邏輯矛盾：可能把成本藏進存貨或過度生產美化毛利。"
  } else if (.sh_finite(inv_g) && inv_g >= th$inv_gm_contra_watch_inv &&
             .sh_finite(gm_d) && gm_d >= th$inv_gm_contra_watch_gm_pp) {
    st <- "觀察"; why <- "存貨與毛利同向改善，需核對是否真有產品組合優勢。"
  } else {
    st <- "通過"; why <- "未出現「存貨大增、毛利反而變好」的矛盾組合。"
  }
  .sh_item("X8", "三表共通警訊", "存貨堆積但毛利率上升（矛盾）", st, why, val, thr)
}

.sh_eval_x9 <- function(ctx, th) {
  # CT：流動比／速動比／利息保障倍數。
  cr <- .sh_ratio(ctx$ca[1], ctx$cl[1])
  qr <- if (.sh_finite(ctx$ca[1]) && .sh_finite(ctx$cl[1]) && length(ctx$inv) >= 1L &&
            .sh_finite(ctx$inv[1])) {
    (ctx$ca[1] - ctx$inv[1]) / ctx$cl[1]
  } else if (.sh_finite(cr)) {
    cr
  } else {
    NA_real_
  }
  ebit <- if (length(ctx$opinc) >= 1L && .sh_finite(ctx$opinc[1])) {
    ctx$opinc[1]
  } else if (length(ctx$pretax) >= 1L && length(ctx$interest) >= 1L &&
             .sh_finite(ctx$pretax[1]) && .sh_finite(ctx$interest[1])) {
    ctx$pretax[1] + abs(ctx$interest[1])
  } else {
    NA_real_
  }
  icov <- .sh_ratio(ebit, if (length(ctx$interest) >= 1L) abs(ctx$interest[1]) else NA_real_)
  if (!.sh_finite(cr) && !.sh_finite(icov)) {
    return(.sh_na_item("X9", "三表共通警訊", "流動性與利息保障惡化",
                       "缺流動資產／負債或利息費用。"))
  }
  val <- sprintf("流動比 %s；速動比 %s；利息保障 %s×",
                 .sh_num_txt(cr, 2), .sh_num_txt(qr, 2), .sh_num_txt(icov, 2))
  thr <- sprintf("警示：流動比<%.1f 或速動比<%.1f 或利息保障<%.1f×；觀察：利息保障<%.1f×",
                 th$current_ratio_alert, th$quick_ratio_alert,
                 th$interest_cover_alert, th$interest_cover_watch)
  alert <- (.sh_finite(cr) && cr < th$current_ratio_alert) ||
    (.sh_finite(qr) && qr < th$quick_ratio_alert) ||
    (.sh_finite(icov) && icov < th$interest_cover_alert)
  watch <- .sh_finite(icov) && icov < th$interest_cover_watch
  if (isTRUE(alert)) {
    st <- "警示"; why <- "短期償債或付息能力偏弱，景氣或信貸收緊時壓力較大。"
  } else if (isTRUE(watch)) {
    st <- "觀察"; why <- "利息保障倍數偏低，尚未跌破警戒線。"
  } else {
    st <- "通過"; why <- "流動性與利息保障未見明顯惡化。"
  }
  .sh_item("X9", "三表共通警訊", "流動性與利息保障惡化", st, why, val, thr)
}

.sh_eval_x10 <- function(ctx, th) {
  # HBR：操弄營運決策——砍裁量費用、過度生產攤低單位成本（非違反準則）。
  disc <- if (length(ctx$rd) >= 2L) ctx$rd else if (length(ctx$sga) >= 2L) ctx$sga else ctx$opex
  disc_lab <- if (length(ctx$rd) >= 2L) "R&D" else if (length(ctx$sga) >= 2L) "SG&A" else "營業費用"
  if (length(disc) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("X10", "三表共通警訊", "操弄營運決策痕跡（砍費用／過度生產）",
                       "缺裁量費用或營收年期。"))
  }
  d0 <- .sh_ratio(disc[1], ctx$rev[1])
  d1 <- .sh_ratio(disc[2], ctx$rev[2])
  cut_pp <- .sh_pp(d0, d1)  # negative = cut
  ni_g <- .sh_yoy(ctx$ni)
  ni_ok <- !.sh_finite(ni_g) || ni_g >= -0.05
  inv_g <- .sh_yoy(ctx$inv)
  cogs_r0 <- .sh_ratio(if (length(ctx$cogs)) ctx$cogs[1] else NA_real_, ctx$rev[1])
  cogs_r1 <- .sh_ratio(if (length(ctx$cogs) >= 2L) ctx$cogs[2] else NA_real_,
                       if (length(ctx$rev) >= 2L) ctx$rev[2] else NA_real_)
  cogs_drop_pp <- .sh_pp(cogs_r0, cogs_r1)
  overprod <- .sh_finite(inv_g) && inv_g >= th$overprod_inv &&
    .sh_finite(cogs_drop_pp) && cogs_drop_pp <= -th$overprod_cogs_pp
  cut_alert <- .sh_finite(cut_pp) && cut_pp <= -th$disc_cut_alert_pp && isTRUE(ni_ok)
  cut_watch <- .sh_finite(cut_pp) && cut_pp <= -th$disc_cut_watch_pp && isTRUE(ni_ok)
  val <- sprintf("%s／營收差 %s；淨利 YoY %s；存貨 YoY %s；銷貨成本率差 %s",
                 disc_lab, .sh_pp_txt(cut_pp), .sh_pct_txt(ni_g),
                 .sh_pct_txt(inv_g), .sh_pp_txt(cogs_drop_pp))
  thr <- sprintf("警示：裁量費用率下降 ≥%d pp（獲利未大崩）或存貨大增且銷貨成本率下降",
                 th$disc_cut_alert_pp)
  if (isTRUE(cut_alert) || isTRUE(overprod)) {
    st <- "警示"
    why <- if (isTRUE(overprod) && isTRUE(cut_alert)) {
      "同時出現砍裁量費用與過度生產跡象，短線美化獲利、長線競爭力風險上升。"
    } else if (isTRUE(overprod)) {
      "存貨大增且銷貨成本率下降，可能靠過度生產攤低單位成本。"
    } else {
      sprintf("獲利大致撐住，但%s佔營收明顯下降，可能為達標而砍研發／行銷／維修。", disc_lab)
    }
  } else if (isTRUE(cut_watch)) {
    st <- "觀察"; why <- sprintf("%s佔營收下滑，需對照是否效率改善或短線達標。", disc_lab)
  } else {
    st <- "通過"; why <- "未看到明顯的砍裁量費用或過度生產組合。"
  }
  .sh_item("X10", "三表共通警訊", "操弄營運決策痕跡（砍費用／過度生產）", st, why, val, thr)
}

.sh_eval_x11 <- function(ctx, th) {
  # HBR：公允價值／商譽無形主觀；年報難還原 Level 3，改看軟性資產比重與跳升。
  soft0 <- {
    g <- if (length(ctx$gw) >= 1L && .sh_finite(ctx$gw[1])) ctx$gw[1] else 0
    i <- if (length(ctx$intang) >= 1L && .sh_finite(ctx$intang[1])) ctx$intang[1] else 0
    # Avoid double-count if intang already includes goodwill-like aggregate
    if (length(ctx$intang) >= 1L && .sh_finite(ctx$intang[1]) &&
        length(ctx$gw) >= 1L && .sh_finite(ctx$gw[1]) &&
        ctx$intang[1] >= ctx$gw[1] * 0.95) {
      ctx$intang[1]
    } else {
      g + i
    }
  }
  soft1 <- {
    g <- if (length(ctx$gw) >= 2L && .sh_finite(ctx$gw[2])) ctx$gw[2] else 0
    i <- if (length(ctx$intang) >= 2L && .sh_finite(ctx$intang[2])) ctx$intang[2] else 0
    if (length(ctx$intang) >= 2L && .sh_finite(ctx$intang[2]) &&
        length(ctx$gw) >= 2L && .sh_finite(ctx$gw[2]) &&
        ctx$intang[2] >= ctx$gw[2] * 0.95) {
      ctx$intang[2]
    } else {
      g + i
    }
  }
  sh0 <- .sh_ratio(soft0, ctx$assets[1])
  sh1 <- .sh_ratio(soft1, if (length(ctx$assets) >= 2L) ctx$assets[2] else NA_real_)
  jump <- .sh_pp(sh0, sh1)
  if (!.sh_finite(sh0) && length(ctx$gw) < 1L && length(ctx$intang) < 1L) {
    return(.sh_na_item("X11", "三表共通警訊", "公允價值／無形資產主觀風險",
                       "無商譽或無形資產可評軟性資產比重。"))
  }
  val <- sprintf("商譽＋無形／資產 %s→%s（差 %s）",
                 .sh_pct_txt(sh1), .sh_pct_txt(sh0), .sh_pp_txt(jump))
  thr <- sprintf("警示：軟性資產／總資產 ≥%.0f%% 且年增 ≥%d pp",
                 th$soft_asset_alert * 100, th$soft_asset_jump_pp)
  if (.sh_finite(sh0) && sh0 >= th$soft_asset_alert &&
      .sh_finite(jump) && jump >= th$soft_asset_jump_pp) {
    st <- "警示"; why <- "商譽／無形資產佔比高且快速上升，公允價值與減損假設主觀性高，需核對評價假設。"
  } else if (.sh_finite(sh0) && sh0 >= th$soft_asset_alert) {
    st <- "觀察"; why <- "軟性資產佔比偏高，評價與減損假設對股價敏感。"
  } else if (.sh_finite(jump) && jump >= th$soft_asset_jump_pp) {
    st <- "觀察"; why <- "軟性資產比重跳升，需對照併購與公允價值假設。"
  } else {
    st <- "通過"; why <- "商譽／無形資產比重未見異常膨脹。"
  }
  .sh_item("X11", "三表共通警訊", "公允價值／無形資產主觀風險", st, why, val, thr)
}

# ----- 15 手法 -----
.sh_eval_01 <- function(ctx, th) {
  if (length(ctx$rev) < 2L || length(ctx$ocf) < 2L) {
    return(.sh_na_item("1", "盈餘操縱", "提前認列營收", "營收或營業現金流年期不足。"))
  }
  rev_g <- .sh_yoy(ctx$rev)
  ocf_g <- .sh_yoy(ctx$ocf)
  ar_g <- .sh_yoy(ctx$ar)
  ca_g <- .sh_yoy(ctx$contract_ast)
  def_g <- .sh_yoy(ctx$defrev)
  gap_ar <- .sh_pp(ar_g, rev_g)
  gap_ca <- .sh_pp(ca_g, rev_g)
  # CT：營收大增但合約負債／預收未跟進 → 提前認列
  def_lag_pp <- if (.sh_finite(rev_g) && .sh_finite(def_g)) (rev_g - def_g) * 100 else NA_real_
  def_lag <- .sh_finite(rev_g) && rev_g >= 0.10 && .sh_finite(def_lag_pp) &&
    def_lag_pp >= th$defrev_lag_pp
  cash_lag <- .sh_finite(rev_g) && .sh_finite(ocf_g) && (rev_g - ocf_g) >= 0.15
  ar_surge <- (.sh_finite(gap_ar) && gap_ar >= th$ar_vs_rev_alert_pp) ||
    (.sh_finite(gap_ca) && gap_ca >= th$ar_vs_rev_alert_pp)
  val <- sprintf("營收 YoY %s；OCF YoY %s；應收差 %s；合約資產差 %s；預收落後 %s",
                 .sh_pct_txt(rev_g), .sh_pct_txt(ocf_g), .sh_pp_txt(gap_ar),
                 .sh_pp_txt(gap_ca), .sh_pp_txt(def_lag_pp))
  thr <- sprintf("警示：營收成長但現金／應收惡化；或預收落後營收 ≥%d pp", th$defrev_lag_pp)
  if (.sh_finite(rev_g) && rev_g >= 0.08 && isTRUE(cash_lag) && isTRUE(ar_surge)) {
    st <- "警示"; why <- "營收往前衝，應收或合約資產更快，現金沒跟上，有提前認列疑慮。"
  } else if (isTRUE(def_lag) && (isTRUE(cash_lag) || isTRUE(ar_surge))) {
    st <- "警示"; why <- "營收成長但預收／合約負債明顯落後，且現金或應收同步轉差，提前認列風險上升。"
  } else if (isTRUE(cash_lag) || isTRUE(ar_surge) || isTRUE(def_lag)) {
    st <- "觀察"; why <- "營收與現金／應收／預收步調不一致，尚未同時達到警示組合。"
  } else if (length(ctx$ar) < 1L && length(ctx$contract_ast) < 1L && length(ctx$defrev) < 2L) {
    st <- "資料不足"; why <- "無應收、合約資產或預收，無法評估出貨品質。"
  } else {
    st <- "通過"; why <- "未同時出現營收暴衝、應收惡化、預收落後與現金落後。"
  }
  .sh_item("1", "盈餘操縱", "提前認列營收", st, why, val, thr)
}

.sh_eval_02 <- function(ctx, th, industry_key) {
  gm <- .sh_gm(ctx, 1L)
  if (!.sh_finite(gm) || length(ctx$rev) < 1L) {
    return(.sh_na_item("2", "盈餘操縱", "虛增營收", "缺毛利或營收，無法比對獲利結構。"))
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
  .sh_item("2", "盈餘操縱", "虛增營收", st, why, val, thr)
}

.sh_eval_03 <- function(ctx, th) {
  if (length(ctx$ni) < 2L || length(ctx$opinc) < 2L) {
    return(.sh_na_item("3", "盈餘操縱", "靠一次性項目灌營收", "缺營業利益或淨利年期。"))
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
  .sh_item("3", "盈餘操縱", "靠一次性項目灌營收", st, why, val, thr)
}

.sh_eval_04 <- function(ctx, th) {
  intang <- ctx$intang
  if (length(intang) < 2L && length(ctx$ppe) < 2L) {
    return(.sh_na_item("4", "盈餘操縱", "把當期費用移到後期", "無無形資產或不動產設備可看資本化。"))
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
  ar_g <- .sh_yoy(ctx$ar)
  # CT：應收持續增加，備抵比例卻下降 → 刻意壓低費用
  allow_alert <- isTRUE(allow_drop) && .sh_finite(ar_g) && ar_g >= 0.10
  val <- sprintf("無形資產 YoY %s vs 營收 YoY %s（差 %s）；折舊率 %s→%s；備抵／應收 %s→%s",
                 .sh_pct_txt(int_g), .sh_pct_txt(rev_g), .sh_pp_txt(gap),
                 .sh_pct_txt(dep_rate1), .sh_pct_txt(dep_rate0),
                 .sh_pct_txt(allow_ar1), .sh_pct_txt(allow_ar0))
  thr <- sprintf("警示：無形資產成長超營收 ≥%d pp；或應收成長且備抵佔比下降", th$intang_vs_rev_pp)
  if (.sh_finite(gap) && gap >= th$intang_vs_rev_pp) {
    st <- "警示"; why <- "無形資產擴張遠快於營收，費用可能被資本化延後。"
  } else if (isTRUE(allow_alert)) {
    st <- "警示"; why <- "應收在增加，備抵呆帳佔比卻下降，可能刻意少提費用美化盈餘。"
  } else if (isTRUE(dep_drop) || isTRUE(allow_drop)) {
    st <- "觀察"
    why <- if (isTRUE(allow_drop)) {
      "備抵呆帳佔應收下降。若收款同時變差，費用可能被藏住。"
    } else {
      "折舊率偏低，資產可能折得太慢。"
    }
  } else {
    st <- "通過"; why <- "未看到無形資產暴衝、折舊率明顯下滑或備抵異常放寬。"
  }
  .sh_item("4", "盈餘操縱", "把當期費用移到後期", st, why, val, thr)
}

.sh_eval_05 <- function(ctx, th) {
  if (length(ctx$opex) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("5", "盈餘操縱", "隱藏費用或損失", "缺營業費用或營收年期。"))
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
  .sh_item("5", "盈餘操縱", "隱藏費用或損失", st, why, val, thr)
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
    return(.sh_na_item("6", "盈餘操縱", "把當期收益移到後期", "無遞延收入，獲利年期也不足以看平滑。"))
  }
  val <- sprintf("遞延收入 YoY %s vs 營收 YoY %s（差 %s）；淨利率區間 %s",
                 .sh_pct_txt(def_g), .sh_pct_txt(rev_g), .sh_pp_txt(gap),
                 if (length(ni_m) >= 2L) sprintf("%.1f–%.1f%%", min(ni_m) * 100, max(ni_m) * 100) else "—")
  thr <- sprintf("警示：遞延收入成長超營收 ≥%d pp", th$defrev_vs_rev_pp)
  if (.sh_finite(gap) && gap >= th$defrev_vs_rev_pp) {
    st <- "警示"; why <- "預收／遞延收入異常增加，可能把收益往後挪或提前收現。"
  } else if (isTRUE(smooth2) && length(rev) >= 4L) {
    st <- "觀察"; why <- "獲利異常平滑，需對照準備金與遞延項目（未必是作帳）。"
  } else if (length(def) < 2L) {
    st <- "資料不足"; why <- "無遞延收入序列。"
  } else {
    st <- "通過"; why <- "遞延收入未異常快於營收，獲利也沒有過度平滑。"
  }
  .sh_item("6", "盈餘操縱", "把當期收益移到後期", st, why, val, thr)
}

.sh_eval_07 <- function(ctx, th) {
  imp <- ctx$impair
  if (length(imp) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("7", "盈餘操縱", "把未來費用移到當期（大洗澡）", "無減損／非常項目或年期不足。"))
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
  .sh_item("7", "盈餘操縱", "把未來費用移到當期（大洗澡）", st, why, val, thr)
}

.sh_eval_08 <- function(ctx, th) {
  if (length(ctx$ocf) < 2L) {
    return(.sh_na_item("8", "現金流作假", "把融資當成營業現金", "營業現金流年期不足。"))
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
  .sh_item("8", "現金流作假", "把融資當成營業現金", st, why, val, thr)
}

.sh_eval_09 <- function(ctx, th) {
  if (length(ctx$ocf) < 1L || length(ctx$fcf) < 1L) {
    return(.sh_na_item("9", "現金流作假", "把營業現金流出藏到別項", "缺營業現金流或自由現金流。"))
  }
  ocf <- ctx$ocf[1]
  fcf <- ctx$fcf[1]
  cap_g <- .sh_yoy(ctx$capex)
  rev_g <- .sh_yoy(ctx$rev)
  cap_vs_dep <- .sh_ratio(if (length(ctx$capex)) abs(ctx$capex[1]) else NA_real_,
                         if (length(ctx$dep)) abs(ctx$dep[1]) else NA_real_)
  # CT：資本支出遠高於折舊攤銷，卻看不到明確擴產／營收成長
  capex_heavy <- .sh_finite(cap_vs_dep) && cap_vs_dep >= th$capex_dep_alert &&
    (.sh_finite(rev_g) && rev_g < 0.10)
  capex_watch <- .sh_finite(cap_vs_dep) && cap_vs_dep >= th$capex_dep_watch &&
    (.sh_finite(rev_g) && rev_g < 0.08)
  ni_up_fcf_neg <- length(ctx$ni) >= 2L && .sh_finite(ctx$ni[1]) && .sh_finite(ctx$ni[2]) &&
    ctx$ni[1] > ctx$ni[2] && .sh_finite(fcf) && fcf < 0
  val <- sprintf("OCF %s；FCF %s；資本支出／折舊 %s；營收 YoY %s",
                 .sh_num_txt(ocf, 0), .sh_num_txt(fcf, 0), .sh_num_txt(cap_vs_dep, 2, "×"),
                 .sh_pct_txt(rev_g))
  thr <- sprintf("警示：OCF＜0，或 CapEx／折舊≥%.1f× 且營收未明顯擴張；淨利升但 FCF＜0 亦警示",
                 th$capex_dep_alert)
  if (.sh_finite(ocf) && ocf < 0) {
    st <- "警示"; why <- "核心營業現金流為負，本業在失血（舊有現金／槓桿檢核一併納入）。"
  } else if (isTRUE(capex_heavy) && .sh_finite(fcf) && fcf < 0) {
    st <- "警示"; why <- "資本支出遠高於折舊、營收未明顯擴張，自由現金流又為負，投資流出品質可疑。"
  } else if (isTRUE(ni_up_fcf_neg)) {
    st <- "警示"; why <- "淨利持續成長但自由現金流為負，獲利未轉成可自由運用現金。"
  } else if (.sh_finite(ocf) && ocf > 0 && .sh_finite(fcf) && fcf < 0 &&
             .sh_finite(cap_g) && .sh_finite(rev_g) && (cap_g - rev_g) >= 0.25) {
    st <- "觀察"; why <- "帳上營業現金看起來不錯，自由現金流卻很差，投資流出偏像日常開銷或過度資本化。"
  } else if (isTRUE(capex_watch) || (.sh_finite(ocf) && ocf > 0 && .sh_finite(fcf) && fcf < 0)) {
    st <- "觀察"; why <- "自由現金流為負，或 CapEx 相對折舊偏高。成長型資本支出也可能如此，需對照擴產計畫。"
  } else {
    st <- "通過"; why <- "營業現金與自由現金流未出現「帳上很好、口袋很差」的組合。"
  }
  .sh_item("9", "現金流作假", "把營業現金流出藏到別項", st, why, val, thr)
}

.sh_eval_10 <- function(ctx, th) {
  if (length(ctx$ocf) < 2L) {
    return(.sh_na_item("10", "現金流作假", "用不持久的方式灌營業現金", "營業現金流年期不足。"))
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
  .sh_item("10", "現金流作假", "用不持久的方式灌營業現金", st, why, val, thr)
}

.sh_eval_11 <- function(ctx, th) {
  ni <- ctx$ni
  ocf <- ctx$ocf
  if (length(ni) < 1L || length(ocf) < 1L || !.sh_finite(ni[1]) || !.sh_finite(ocf[1])) {
    return(.sh_na_item("11", "關鍵數字灌水", "用關鍵數字誇大業績", "缺淨利或營業現金流。"))
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
  .sh_item("11", "關鍵數字灌水", "用關鍵數字誇大業績", st, why, val, thr)
}

.sh_eval_12 <- function(ctx, th) {
  de <- .sh_ratio(ctx$debt[1], ctx$equity[1])
  wc0 <- if (length(ctx$ca) && length(ctx$cl)) ctx$ca[1] - ctx$cl[1] else NA_real_
  wc1 <- if (length(ctx$ca) >= 2L && length(ctx$cl) >= 2L) ctx$ca[2] - ctx$cl[2] else NA_real_
  wc_ast0 <- .sh_ratio(wc0, ctx$assets[1])
  wc_ast1 <- .sh_ratio(wc1, if (length(ctx$assets) >= 2L) ctx$assets[2] else NA_real_)
  wc_pp <- .sh_pp(wc_ast0, wc_ast1)
  if (!.sh_finite(de) && !.sh_finite(wc_pp)) {
    return(.sh_na_item("12", "關鍵數字灌水", "美化資產負債表數字", "缺負債／權益或營運資金。"))
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
  .sh_item("12", "關鍵數字灌水", "美化資產負債表數字", st, why, val, thr)
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
    return(.sh_na_item("13", "購併作帳", "購併後灌營收與獲利", "無商譽年期，無法判斷併購灌水。"))
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
  .sh_item("13", "購併作帳", "購併後灌營收與獲利", st, why, val, thr)
}

.sh_eval_14 <- function(ctx, th) {
  if (length(ctx$ocf) < 2L || length(ctx$gw) < 2L) {
    return(.sh_na_item("14", "購併作帳", "購併後灌現金流", "缺營業現金流或商譽年期。"))
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
  .sh_item("14", "購併作帳", "購併後灌現金流", st, why, val, thr)
}

.sh_eval_15 <- function(ctx, th) {
  if (length(ctx$assets) < 2L || length(ctx$rev) < 2L) {
    return(.sh_na_item("15", "購併作帳", "購併後美化週轉等指標", "資產或營收年期不足。"))
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
  .sh_item("15", "購併作帳", "購併後美化週轉等指標", st, why, val, thr)
}

#' Catalog of all Schilit / 三表共通 red-flag codes (static; for docs / Lab filters).
list_shenanigan_red_flag_catalog <- function() {
  data.frame(
    code = c(
      "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11",
      "1", "2", "3", "4", "5", "6", "7",
      "8", "9", "10", "11", "12", "13", "14", "15"
    ),
    category = c(
      rep("三表共通警訊", 11L),
      rep("盈餘操縱", 7L),
      rep("現金流作假", 3L),
      rep("關鍵數字灌水", 2L),
      rep("購併作帳", 3L)
    ),
    name_zh = c(
      "淨利長期高於營業現金流",
      "應收成長快過營收、收款天數變長",
      "存貨增速高於營收且毛利下滑",
      "一次性收益／反覆非常項目",
      "大量購併／關係人交易／更換財務長",
      "常講調整後獲利、定義卻常改",
      "營收成長但毛利／淨利率下滑",
      "存貨堆積但毛利率上升（矛盾）",
      "流動性與利息保障惡化",
      "操弄營運決策痕跡（砍費用／過度生產）",
      "公允價值／無形資產主觀風險",
      "提前認列營收",
      "虛增營收",
      "靠一次性項目灌營收",
      "把當期費用移到後期",
      "隱藏費用或損失",
      "把當期收益移到後期",
      "把未來費用移到當期（大洗澡）",
      "把融資當成營業現金",
      "把營業現金流出藏到別項",
      "用不持久的方式灌營業現金",
      "用關鍵數字誇大業績",
      "美化資產負債表數字",
      "購併後灌營收與獲利",
      "購併後灌現金流",
      "購併後美化週轉等指標"
    ),
    name_en = c(
      "NI persistently above OCF",
      "AR growth outpaces revenue / DSO stretch",
      "Inventory outpaces revenue with GM decline",
      "One-time gains / recurring special items",
      "Heavy M&A / related parties / CFO change",
      "Heavy non-GAAP with shifting definitions",
      "Revenue up but margins down",
      "Inventory up but GM up (contradiction)",
      "Liquidity / interest-coverage stress",
      "Cooking decisions: cut opex / overproduce",
      "Fair-value / soft-asset subjectivity",
      "Recognize revenue too soon",
      "Record bogus revenue",
      "Boost income with one-time items",
      "Shift current expenses to later periods",
      "Hide expenses or losses",
      "Shift current income to later periods",
      "Big bath: pull future expenses forward",
      "Shift financing inflows into CFFO",
      "Shift operating cash outflows elsewhere",
      "Boost CFFO with unsustainable WC moves",
      "Misleading key performance metrics",
      "Distort balance-sheet metrics",
      "M&A: inflate revenue and earnings",
      "M&A: inflate cash flow",
      "M&A: dress up turnover metrics"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Full judgment-rule book: code, category, alert/watch thresholds, source tags.
list_all_judgment_rules <- function() {
  th <- SHENANIGAN_THRESHOLDS
  eq <- if (exists(".EQ_THRESHOLDS")) .EQ_THRESHOLDS else list()
  sh <- list_shenanigan_red_flag_catalog()
  rule_zh <- c(
    sprintf("警示：≥%d 年淨利>OCF，或淨利>0且OCF<0；觀察：≥%d 年",
            th$ni_gt_ocf_years_alert, th$ni_gt_ocf_years_watch),
    sprintf("警示：應收YoY−營收YoY≥%d pp 且收款天數+≥%d；觀察：≥%d pp 或 +≥%d 天",
            th$ar_vs_rev_alert_pp, th$dso_alert_days, th$ar_vs_rev_watch_pp, th$dso_watch_days),
    sprintf("警示：存貨快於營收≥%d pp 且毛利率下滑≥%.1f pp；存貨天數+≥%d 天觀察",
            th$inv_vs_rev_alert_pp, th$gm_drop_alert_pp, th$dio_watch_days),
    sprintf("警示：其他收益／營收年增≥%d pp 且佔|淨利|≥%.0f%%，或減損≥3年反覆",
            th$other_inc_rev_pp, th$other_inc_ni_share * 100),
    sprintf("警示：商譽／資產+≥%d pp 或商譽增額≥期初資產%.0f%%；關係人／換所資料不足",
            th$gw_asset_pp, th$gw_vs_beg_assets * 100),
    "年報無穩定非GAAP欄位→資料不足；現金落差見X1／#11",
    sprintf("警示：營收YoY≥%.0f%% 且毛利或淨利率−≥%d pp；觀察：營收≥%.0f%% 且−≥%.1f pp",
            th$rev_up_gm_alert_rev * 100, th$rev_up_gm_alert_pp,
            th$rev_up_gm_watch_rev * 100, th$rev_up_gm_watch_pp),
    sprintf("警示：存貨YoY≥%.0f%% 且毛利率+≥%.1f pp；觀察：≥%.0f%%／+≥%.1f pp",
            th$inv_gm_contra_inv * 100, th$inv_gm_contra_gm_pp,
            th$inv_gm_contra_watch_inv * 100, th$inv_gm_contra_watch_gm_pp),
    sprintf("警示：流動比<%.1f 或速動比<%.1f 或利息保障<%.1f×；觀察：利息保障<%.1f×",
            th$current_ratio_alert, th$quick_ratio_alert,
            th$interest_cover_alert, th$interest_cover_watch),
    sprintf("警示：裁量費用率−≥%d pp（獲利未大崩）或存貨YoY≥%.0f%%且銷貨成本率−≥%d pp",
            th$disc_cut_alert_pp, th$overprod_inv * 100, th$overprod_cogs_pp),
    sprintf("警示：商譽+無形／資產≥%.0f%% 且年增≥%d pp；觀察：佔比高或跳升",
            th$soft_asset_alert * 100, th$soft_asset_jump_pp),
    sprintf("警示：營收成長+現金落後+應收／合約資產暴衝，或預收落後營收≥%d pp 且同步惡化",
            th$defrev_lag_pp),
    sprintf("警示：毛利率高於產業上限≥%d pp；觀察：≥%d pp 或高於自身均值≥%d pp",
            th$gm_vs_ind_alert_pp, th$gm_vs_ind_watch_pp, th$gm_vs_own_watch_pp),
    sprintf("警示：淨利明顯快於營業利益，或業外／營收年增≥%d pp", th$other_inc_rev_pp),
    sprintf("警示：無形資產YoY超營收≥%d pp，或應收成長且備抵佔比下降", th$intang_vs_rev_pp),
    sprintf("警示：營收±10%%但費用率−≥%d pp；觀察：−≥%d pp",
            th$opex_drop_alert_pp, th$opex_drop_watch_pp),
    sprintf("警示：遞延收入YoY超營收≥%d pp；觀察：淨利率異常平滑", th$defrev_vs_rev_pp),
    sprintf("警示：前期減損≤%.0f%%營收且本期毛利率回升≥%d pp",
            abs(th$bath_impair_rev) * 100, th$bath_gm_rebound_pp),
    sprintf("警示：OCF暴增≥%.0f%%且應收驟降≤%.0f%%（或隔期反轉）",
            th$oc_f_surge * 100, abs(th$ar_drop_factor) * 100),
    sprintf("警示：OCF<0，或CapEx／折舊≥%.1f×且營收弱+FCF<0，或淨利升但FCF<0",
            th$capex_dep_alert),
    sprintf("警示：DPO+≥%d天／預收大增／去庫存+毛利惡化，且OCF上升", th$dpo_alert_days),
    sprintf("警示：OCF／淨利<%.2f×；觀察：<%.2f×（無非GAAP欄位）",
            th$cash_ni_alert, th$cash_ni_watch),
    sprintf("警示：負債／權益>%.1f 或營運資金／資產年變≥%d pp；觀察：D/E>%.1f",
            th$de_alert, th$wc_asset_pp, th$de_watch),
    sprintf("警示：商譽／資產+≥%d pp、營收成長弱但淨利變好", th$gw_asset_pp),
    "警示：OCF YoY≥30% 且商譽增額／期初資產≥8%",
    sprintf("警示：資產擴張≥%.0f%%、週轉變差≥%.0f%%且成長多來自商譽",
            th$asset_jump * 100, th$at_drop_rel * 100)
  )
  src <- c(
    "Schilit/CT/Sloan", "Schilit/CT", "Schilit/CT", "Schilit", "Schilit",
    "HBR non-GAAP", "CT", "CT/HBR overprod", "CT liquidity", "HBR cooking decisions",
    "HBR fair value",
    "Schilit/CT", "Schilit/CT", "Schilit", "Schilit/CT", "Schilit/HBR",
    "Schilit", "Schilit", "Schilit", "Schilit/CT", "Schilit",
    "Schilit/CT/HBR", "Schilit/CT", "Schilit", "Schilit", "Schilit"
  )
  out <- cbind(sh, rule_zh = rule_zh, source = src, stringsAsFactors = FALSE)
  eq_rows <- data.frame(
    code = c("EQ_M", "EQ_AR", "EQ_ACC", "EQ_F", "EQ_QOE"),
    category = rep("盈餘品質", 5L),
    name_zh = c(
      "Beneish M-Score 偏高",
      "應收帳款與營收成長脫鉤",
      "應計項目偏高",
      "Piotroski F-Score 偏弱",
      "盈餘品質 QoE 偏弱"
    ),
    name_en = c(
      "Elevated Beneish M-Score",
      "AR–revenue growth divergence",
      "High accruals ratio",
      "Weak Piotroski F-Score",
      "Weak Quality of Earnings (QoE)"
    ),
    rule_zh = c(
      sprintf("警示：M > %.2f；觀察帶：約 %.2f～%.2f",
              eq$mscore_alert %||% -1.78, eq$mscore_watch %||% -2.22, eq$mscore_alert %||% -1.78),
      sprintf("警示：應收YoY−營收YoY ≥ %d pp；觀察 ≥ %d pp",
              eq$ar_rev_alert_pp %||% 15, eq$ar_rev_watch_pp %||% 8),
      sprintf("警示：(NI−OCF)/平均資產 ≥ %.2f；觀察 ≥ %.2f",
              eq$accruals_alert %||% 0.10, eq$accruals_watch %||% 0.05),
      "警示：F-Score < 4（品質檢核偏弱；非買進訊號）",
      sprintf("綜合應計／現金轉換（警示 OCF/NI < %.2f）／F／M／應收脫鉤 → QoE 警示",
              eq$cash_conv_alert %||% 0.50)
    ),
    source = c("Beneish 1999", "CT/Schilit", "Sloan/accruals", "Piotroski", "YNOW QoE"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rbind(out, eq_rows)
}

#' Schilit 15 招 + 三表共通警訊 + 舊 Fraud Warning 合併評估
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
      .sh_eval_x7(ctx, th),
      .sh_eval_x8(ctx, th),
      .sh_eval_x9(ctx, th),
      .sh_eval_x10(ctx, th),
      .sh_eval_x11(ctx, th),
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
    .sh_item("ERR", "三表共通警訊", "自動判讀", "資料不足",
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

#' YNOW 分頁：四大類 + 三表共通警訊（警示／觀察展開；資料不足摺疊）
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
  xflag <- show[show$category == "三表共通警訊", , drop = FALSE]
  cats <- list(
    "一、盈餘操縱" = show[show$category == "盈餘操縱", , drop = FALSE],
    "二、現金流作假" = show[show$category == "現金流作假", , drop = FALSE],
    "三、關鍵數字灌水" = show[show$category == "關鍵數字灌水", , drop = FALSE],
    "四、購併作帳" = show[show$category == "購併作帳", , drop = FALSE]
  )
  tags$div(
    class = "ynow-shen-wrap",
    tags$p(
      class = "ynow-shen-count",
      tags$span(class = "ynow-shen-st ynow-shen-alert", paste0("警示 ", ev$n_alert, " 項")),
      tags$span(class = "ynow-shen-st ynow-shen-watch", paste0("觀察 ", ev$n_watch, " 項")),
      tags$span(class = "ynow-shen-st ynow-shen-pass", paste0("通過 ", ev$n_pass, " 項"))
    ),
    tags$div(
      class = "ynow-shen-strip",
      tags$h4(icon("exclamation-triangle"), " 三表共通警訊"),
      if (nrow(xflag) < 1L) {
        tags$p(class = "ynow-shen-ok", "目前沒有警示或觀察。")
      } else {
        lapply(seq_len(nrow(xflag)), function(i) .sh_item_card(xflag[i, ]))
      }
    ),
    lapply(names(cats), function(nm) .sh_cat_block(cats[[nm]], nm)),
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
        tags$summary(sprintf("資料不足 %d 項", nrow(nas))),
        lapply(seq_len(nrow(nas)), function(i) .sh_item_card(nas[i, ]))
      )
    } else {
      NULL
    }
  )
}
