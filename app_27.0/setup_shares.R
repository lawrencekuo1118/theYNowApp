# ==========================================
# setup_shares.R — Shares / ADR / equity-structure helpers
# Extracted from setup.R (C-STRUCT-04). Sourced by setup.R.
# ==========================================

# 財報股數 vs 報價約當股數：超過此倍率視為不同股權級距（含常見 ADR 2–10×）
SHARE_UNIT_MISMATCH_RATIO <- 1.5
# 報價幣 ≠ 財報幣（典型 ADR）時用較敏感門檻
SHARE_UNIT_MISMATCH_RATIO_ADR <- 1.25

#' 自 Summary 取「報價幣」股價與市值（二者同幣，供約當股數 = 市值÷股價）
#' @return list(price, market_cap) — 皆為報價幣數值；缺則 NA
extract_quote_price_mcap <- function(summary_df) {
  price <- NA_real_
  market_cap <- NA_real_
  if (is.null(summary_df) || !is.data.frame(summary_df) || nrow(summary_df) < 1) {
    return(list(price = price, market_cap = market_cap))
  }
  if (!("Item" %in% names(summary_df)) || !("Value" %in% names(summary_df))) {
    return(list(price = price, market_cap = market_cap))
  }
  px_row <- summary_df[grepl("^Market Price$", summary_df$Item, ignore.case = TRUE), , drop = FALSE]
  if (nrow(px_row) < 1) {
    px_row <- summary_df[grep("Previous Close|Market Price", summary_df$Item, ignore.case = TRUE), , drop = FALSE]
  }
  if (nrow(px_row) >= 1) {
    price <- parse_financial_number(px_row$Value[1])[1]
  }
  mc_row <- summary_df[summary_df$Item == "Market Cap (intraday)", , drop = FALSE]
  if (nrow(mc_row) < 1) {
    mc_row <- summary_df[grep("^Market Cap", summary_df$Item, ignore.case = TRUE), , drop = FALSE]
  }
  if (nrow(mc_row) >= 1) {
    market_cap <- parse_financial_number(mc_row$Value[1])[1]
  }
  list(price = price, market_cap = market_cap)
}

#' Parse a numeric Summary Item (Shares Outstanding, etc.).
extract_summary_number <- function(summary_df, item_pattern) {
  if (is.null(summary_df) || !is.data.frame(summary_df) || nrow(summary_df) < 1) {
    return(NA_real_)
  }
  if (!("Item" %in% names(summary_df)) || !("Value" %in% names(summary_df))) {
    return(NA_real_)
  }
  hit <- summary_df[grepl(item_pattern, summary_df$Item, ignore.case = TRUE), , drop = FALSE]
  if (nrow(hit) < 1) return(NA_real_)
  parse_financial_number(hit$Value[1])[1]
}

#' Known dual-class tickers: listed class vs other classes (voting nature).
#' Labels are locale keys consumed by equity_structure_slices(locale=).
.DUAL_CLASS_SHARE_KEYS <- list(
  GOOGL = list(listed = "eq_pie_class_a_voting", other = "eq_pie_other_nonvoting"),
  GOOG  = list(listed = "eq_pie_class_c_nonvoting", other = "eq_pie_other_voting"),
  "BRK-A" = list(listed = "eq_pie_class_a_voting", other = "eq_pie_class_b"),
  "BRK-B" = list(listed = "eq_pie_class_b", other = "eq_pie_class_a_voting"),
  META  = list(listed = "eq_pie_class_a_voting", other = "eq_pie_class_b_supervote"),
  FB    = list(listed = "eq_pie_class_a_voting", other = "eq_pie_class_b_supervote")
)

#' Build equity-structure slices by share nature when distinguishable.
#' Priority: (1) Ordinary vs Preferred share counts, (2) dual-class listed vs other,
#' (3) Common vs Preferred book equity (+ Minority Interest).
#' @return data.frame(label, value, unit, color) or NULL if < 2 slices
equity_structure_slices <- function(bs_df, summary_df = NULL, ticker = "",
                                    locale = "en", label_fn = NULL) {
  lab <- function(key, fallback) {
    if (is.function(label_fn)) {
      out <- tryCatch(label_fn(key), error = function(e) NULL)
      if (!is.null(out) && nzchar(as.character(out)[1])) return(as.character(out)[1])
    }
    if (exists("ui_str", mode = "function")) {
      out <- tryCatch(ui_str(key, locale), error = function(e) NULL)
      if (!is.null(out) && nzchar(as.character(out)[1]) && !identical(out, key)) {
        return(as.character(out)[1])
      }
    }
    fallback
  }
  .metric <- function(patterns) {
    tryCatch(
      select_current_metric_any(bs_df, patterns, "stock"),
      error = function(e) NA_real_
    )
  }
  if (is.null(bs_df) || !is.data.frame(bs_df) || nrow(bs_df) < 1) return(NULL)

  ordinary_n <- .metric(c("^Ordinary Shares Number$", "Ordinary Shares Number"))
  preferred_n <- .metric(c("^Preferred Shares Number$", "Preferred Shares Number"))
  common_eq <- .metric(c("^Common Stock Equity$", "Common Stock Equity"))
  preferred_eq <- .metric(c(
    "^Preferred Stock Equity$", "Preferred Stock Equity",
    "^Preferred Stock$", "Preferred Stock"
  ))
  minority <- .metric(c("^Minority Interest$", "Minority Interest"))

  # (1) Share counts: Ordinary vs Preferred
  if (is.finite(ordinary_n) && ordinary_n > 0 &&
      is.finite(preferred_n) && preferred_n > 0) {
    return(data.frame(
      label = c(
        lab("eq_pie_ordinary_voting", "Ordinary (typically voting)"),
        lab("eq_pie_preferred_limited", "Preferred (typically limited / non-voting)")
      ),
      value = c(ordinary_n, preferred_n),
      unit = "shares",
      color = c("#0C5484", "#E67E22"),
      stringsAsFactors = FALSE
    ))
  }

  # (2) Dual-class / multi-class via listed vs total economic shares
  listed <- extract_summary_number(summary_df, "^Shares Outstanding$")
  implied <- extract_summary_number(summary_df, "^Implied Shares Outstanding$")
  if (!is.finite(listed) || listed <= 0) {
    qm <- extract_quote_price_mcap(summary_df)
    if (is.finite(qm$market_cap) && is.finite(qm$price) && qm$price > 0) {
      listed <- qm$market_cap / qm$price
    }
  }
  total_econ <- if (is.finite(implied) && implied > 0) implied else ordinary_n
  if (is.finite(listed) && listed > 0 && is.finite(total_econ) && total_econ > 0) {
    other <- total_econ - listed
    # Material other class (≥8% of total) and listed not already ≈ total
    if (other > total_econ * 0.08 && listed < total_econ * 0.92) {
      tk <- toupper(trimws(as.character(ticker)[1]))
      keys <- .DUAL_CLASS_SHARE_KEYS[[tk]]
      listed_lab <- if (!is.null(keys)) {
        lab(keys$listed, "Listed share class")
      } else {
        lab("eq_pie_listed_class", "Listed share class")
      }
      other_lab <- if (!is.null(keys)) {
        lab(keys$other, "Other share classes")
      } else {
        lab("eq_pie_other_classes", "Other share classes")
      }
      return(data.frame(
        label = c(listed_lab, other_lab),
        value = c(listed, other),
        unit = "shares",
        color = c("#0C5484", "#8B1A1A"),
        stringsAsFactors = FALSE
      ))
    }
  }

  # (3) Book equity: Common vs Preferred (+ Minority Interest)
  # Only when Preferred book equity is present (share-nature signal).
  if (is.finite(preferred_eq) && preferred_eq > 0 &&
      is.finite(common_eq) && common_eq > 0) {
    labels <- c(
      lab("eq_pie_common_equity", "Common equity"),
      lab("eq_pie_preferred_equity", "Preferred equity (typically limited / non-voting)")
    )
    values <- c(common_eq, preferred_eq)
    colors <- c("#0C5484", "#E67E22")
    if (is.finite(minority) && minority > 0) {
      labels <- c(labels, lab("eq_pie_minority", "Minority interest"))
      values <- c(values, minority)
      colors <- c(colors, "#7f8c8d")
    }
    return(data.frame(
      label = labels,
      value = values,
      unit = "currency",
      color = colors,
      stringsAsFactors = FALSE
    ))
  }
  NULL
}

#' 解析適配「目前報價股」的流通股數（雙重股權／ADR 級距）
#' 財報 Ordinary Shares 常為當地普通股；ADR 報價股數 = 市值÷ADR 價。
#' price 與 market_cap 必須同一幣別（請用 extract_quote_price_mcap）。
#' @return list(shares, method, note, ratio)
resolve_shares_for_price <- function(shares_bs,
                                     price = NA_real_,
                                     market_cap = NA_real_,
                                     ticker = "",
                                     quote_currency = NULL,
                                     financial_currency = NULL) {
  shares_bs <- suppressWarnings(as.numeric(shares_bs)[1])
  price <- suppressWarnings(as.numeric(price)[1])
  market_cap <- suppressWarnings(as.numeric(market_cap)[1])
  ticker <- toupper(gsub("\\.", "-", trimws(as.character(ticker %||% "")[1])))
  q_ccy <- tryCatch(normalize_ccy(quote_currency), error = function(e) NA_character_)
  f_ccy <- tryCatch(normalize_ccy(financial_currency), error = function(e) NA_character_)
  adr_fx <- is.character(q_ccy) && is.character(f_ccy) &&
    !is.na(q_ccy) && !is.na(f_ccy) && !identical(q_ccy, f_ccy)

  shares_implied <- if (is.finite(market_cap) && is.finite(price) && price > 0) {
    market_cap / price
  } else {
    NA_real_
  }

  # Dual-class and ADR gaps use the same market-cap / price ratio test.
  # No ticker is assigned a fixed share multiplier.
  if (is.finite(shares_implied) && is.finite(shares_bs) && shares_bs > 0) {
    ratio <- shares_implied / shares_bs
    thr <- if (isTRUE(adr_fx)) SHARE_UNIT_MISMATCH_RATIO_ADR else SHARE_UNIT_MISMATCH_RATIO
    if (is.finite(ratio) && (ratio > thr || ratio < (1 / thr))) {
      kind <- if (isTRUE(adr_fx)) {
        "ADR／報價股與財報普通股級距不符"
      } else {
        "股數單位／股權級距與報價不符"
      }
      return(list(
        shares = shares_implied,
        method = "market_cap_per_price",
        note = sprintf(
          "%s（約當／財報 ≈ %.2fx），搜尋時已自動改用 市值÷股價",
          kind, ratio
        ),
        ratio = ratio
      ))
    }
  }

  if (is.finite(shares_bs) && shares_bs > 0) {
    return(list(
      shares = shares_bs, method = "balance_sheet", note = NULL,
      ratio = if (is.finite(shares_implied) && shares_bs > 0) shares_implied / shares_bs else NA_real_
    ))
  }
  if (is.finite(shares_implied) && shares_implied > 0) {
    return(list(
      shares = shares_implied, method = "market_cap_per_price", note = NULL,
      ratio = NA_real_
    ))
  }
  list(shares = NA_real_, method = "none", note = NULL, ratio = NA_real_)
}

#' 自財報 + Summary 一次解析評價用股數（搜尋／DCF／WACC／DDM 共用）
#' @return list(shares, method, note, ratio, shares_bs, price, market_cap)
resolve_valuation_shares <- function(d_bs,
                                     summary_df,
                                     ticker = "",
                                     quote_currency = NULL,
                                     financial_currency = NULL) {
  shares_bs <- tryCatch(
    select_current_metric_any(d_bs, SHARE_PATTERNS, "stock"),
    error = function(e) NA_real_
  )
  if (!is.finite(shares_bs) || shares_bs <= 0) {
    shares_bs <- tryCatch(
      select_current_metric(
        d_bs,
        "Ordinary Shares Number|Share Issued|Total Shares Outstanding|Basic Average Shares",
        "stock"
      ),
      error = function(e) NA_real_
    )
  }
  qm <- extract_quote_price_mcap(summary_df)
  if (is.null(quote_currency) && !is.null(summary_df)) {
    quote_currency <- attr(summary_df, "currency")
  }
  if (is.null(financial_currency) && !is.null(summary_df)) {
    financial_currency <- attr(summary_df, "financialCurrency")
  }
  sh <- resolve_shares_for_price(
    shares_bs,
    price = qm$price,
    market_cap = qm$market_cap,
    ticker = ticker,
    quote_currency = quote_currency,
    financial_currency = financial_currency
  )
  sh$shares_bs <- shares_bs
  sh$price <- qm$price
  sh$market_cap <- qm$market_cap
  sh
}

#' Cash and Total Debt used by FCFF, FCFE, WACC weights, EV multiples, and SOTP.
#' Missing lines are 0 so every model applies the same bridge, not NA in one and 0 in another.
statement_cash_debt <- function(d_bs) {
  empty <- list(cash = 0, debt = 0)
  if (is.null(d_bs) || !is.data.frame(d_bs) || !nrow(d_bs)) return(empty)
  cash <- tryCatch(
    select_current_metric(
      d_bs,
      "Cash.*Equivalents.*Investments|Cash And Cash Equivalents|^Total Cash$",
      "stock"
    ),
    error = function(e) NA_real_
  )
  debt <- tryCatch(
    select_current_metric(d_bs, "^Total Debt$", "stock"),
    error = function(e) NA_real_
  )
  if (!is.finite(debt)) {
    st <- tryCatch(
      select_current_metric(d_bs, "Current Debt|Short Term Debt", "stock"),
      error = function(e) NA_real_
    )
    lt <- tryCatch(
      select_current_metric(d_bs, "Long Term Debt|^Long-Term Debt$", "stock"),
      error = function(e) NA_real_
    )
    if (is.finite(st) || is.finite(lt)) {
      debt <- sum(c(st, lt)[is.finite(c(st, lt))])
    }
  }
  if (!is.finite(cash)) cash <- 0
  if (!is.finite(debt) || debt < 0) debt <- 0
  list(cash = unname(as.numeric(cash)[1]), debt = unname(as.numeric(debt)[1]))
}

#' Total revenue row shared by DCF Rev0, P/S, and EV/Sales.
statement_total_revenue <- function(d_is) {
  if (is.null(d_is) || !is.data.frame(d_is) || !nrow(d_is)) return(NA_real_)
  tryCatch(
    select_current_metric(d_is, "Total Revenue|^Revenue$|Operating Revenue", "flow"),
    error = function(e) NA_real_
  )
}

#' First finite number. Used so DCF / WACC / Multiples / SOTP read the same override.
bridge_pick_number <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- suppressWarnings(as.numeric(vals))
  hit <- vals[is.finite(vals)]
  if (!length(hit)) return(NA_real_)
  unname(hit[[1]])
}

#' Grey out a numericInput so the page can show a shared value without a second editor.
lock_numeric_input <- function(ui) {
  if (!inherits(ui, "shiny.tag")) return(ui)
  kids <- ui$children
  if (is.null(kids)) return(ui)
  for (i in seq_along(kids)) {
    kid <- kids[[i]]
    if (inherits(kid, "shiny.tag") && identical(kid$name, "input")) {
      kid$attribs$disabled <- "disabled"
      kid$attribs$readonly <- "readonly"
      kids[[i]] <- kid
    }
  }
  ui$children <- kids
  ui
}

#' Same share-class decision as DCF `.valuation_shares`: ADR auto-adjust, else refuse
#' a cross-currency common-share fallback, else balance-sheet shares.
adopt_valuation_shares <- function(sh, statement_ccy = NULL, quote_ccy = NULL) {
  if (is.null(sh)) {
    return(list(shares = NA_real_, note = NULL, method = "none", ratio = NA_real_))
  }
  raw_shares <- suppressWarnings(as.numeric(sh$shares_bs)[1])
  sh_ratio <- suppressWarnings(as.numeric(sh$ratio)[1])
  method <- as.character(sh$method %||% "none")[1]
  if (shares_auto_adjust_method(method) && is.finite(sh$shares) && sh$shares > 0) {
    return(list(shares = sh$shares, note = sh$note, method = method, ratio = sh_ratio))
  }
  st_eff <- infer_statement_currency(statement_ccy, quote_ccy, method)
  if (isTRUE(fx_conversion_required(st_eff, quote_ccy)) ||
      isTRUE(adr_conversion_required(st_eff, quote_ccy, method))) {
    return(list(
      shares = NA_real_,
      note = sh$note,
      method = method,
      ratio = sh_ratio
    ))
  }
  if (is.finite(raw_shares) && raw_shares > 0) {
    return(list(shares = raw_shares, note = NULL, method = "balance_sheet", ratio = sh_ratio))
  }
  list(
    shares = if (is.finite(sh$shares) && sh$shares > 0) sh$shares else NA_real_,
    note = sh$note,
    method = method,
    ratio = sh_ratio
  )
}

#' 是否應自動套用約當股數（ADR／雙重股權等）
shares_auto_adjust_method <- function(method) {
  identical(method, "market_cap_per_price") || identical(method, "brk_b_x1500")
}

#' Fill a missing reporting currency only when the share class is not ADR-aligned.
#' ADR / dual-class must not copy quote (that would label TWD common as USD/ADR).
infer_statement_currency <- function(statement_ccy, quote_ccy, share_method = NULL) {
  st <- normalize_ccy(statement_ccy)
  if (!is.na(st)) return(st)
  q <- normalize_ccy(quote_ccy)
  if (is.na(q)) return(NA_character_)
  if (isTRUE(shares_auto_adjust_method(share_method))) return(NA_character_)
  q
}

#' ADR vs ordinary conversion is needed when share classes actually differ.
#' Same-currency local listings / 1:1 dual-listed names do not require a ratio.
adr_conversion_required <- function(statement_ccy, quote_ccy, share_method = NULL) {
  if (isTRUE(shares_auto_adjust_method(share_method))) return(TRUE)
  st <- infer_statement_currency(statement_ccy, quote_ccy, share_method)
  q <- normalize_ccy(quote_ccy)
  if (is.na(st) || is.na(q)) return(FALSE)
  !identical(st, q)
}

PER_SHARE_ALIGNMENT_CODES <- c(
  "STATEMENT_CURRENCY_UNAVAILABLE",
  "REQUIRED_FX_RATE_MISSING",
  "REQUIRED_FX_RATE_INVALID",
  "APPLICABLE_ADR_RATIO_MISSING",
  "APPLICABLE_ADR_RATIO_INVALID",
  "REQUIRED_PER_SHARE_VALUE_NON_FINITE"
)

#' Per-share inputs the selected valuation method actually needs.
#' Unused names in `required_per_share` are ignored when `method` is set.
.valuation_method_per_share_keys <- function(method) {
  m <- tolower(gsub("[^a-z0-9]+", "", as.character(method %||% "")[1]))
  if (!nzchar(m)) return(character(0))
  switch(
    m,
    dcf = c("dcf", "equity", "fv"),
    ri = c("b0", "bvps"),
    ddm = c("d0"),
    pb = c("bvps"),
    pbbvps = c("bvps"),
    pbtbvps = c("tbvps"),
    pbnavps = c("navps"),
    nav = c("navps"),
    character(0)
  )
}

.required_per_share_values_for_method <- function(required_per_share, method) {
  if (is.null(required_per_share)) return(numeric(0))
  vals <- unlist(required_per_share, use.names = TRUE)
  if (is.null(vals) || length(vals) < 1L) return(numeric(0))
  nms <- names(vals)
  unnamed <- is.null(nms) || !any(nzchar(nms))
  if (unnamed) return(unname(vals))
  keys <- .valuation_method_per_share_keys(method)
  if (!length(keys)) return(unname(vals))
  keep <- nms %in% keys
  if (!any(keep)) return(numeric(0))
  unname(vals[keep])
}

#' Map an internal alignment code to a ui_locale toast key (or NA if OK).
per_share_alignment_toast_key <- function(code) {
  cde <- as.character(code %||% "")[1]
  if (!nzchar(cde) || is.na(cde)) return(NA_character_)
  switch(
    cde,
    STATEMENT_CURRENCY_UNAVAILABLE = "notif_dcf_statement_ccy_unavailable",
    REQUIRED_FX_RATE_MISSING = "notif_dcf_fx_rate_missing",
    REQUIRED_FX_RATE_INVALID = "notif_dcf_fx_rate_invalid",
    APPLICABLE_ADR_RATIO_MISSING = "notif_dcf_adr_ratio_missing",
    APPLICABLE_ADR_RATIO_INVALID = "notif_dcf_adr_ratio_invalid",
    REQUIRED_PER_SHARE_VALUE_NON_FINITE = "notif_dcf_per_share_non_finite",
    NA_character_
  )
}

#' Classify why per-share fair value cannot be computed, or NULL when OK.
#'
#' FX is required only when source and target currencies are known and differ.
#' ADR ratio is required only for ADR vs ordinary (or auto-adjust share-class)
#' comparisons. N/A reporting ccy is not an error unless conversion is needed.
classify_per_share_alignment_failure <- function(statement_ccy,
                                                 quote_ccy,
                                                 usd_twd = NULL,
                                                 share_method = NULL,
                                                 equity_ccy = NULL,
                                                 shares = NULL,
                                                 adr_ratio = NULL,
                                                 required_per_share = NULL,
                                                 method = NULL) {
  q <- normalize_ccy(quote_ccy)
  st <- infer_statement_currency(statement_ccy, q, share_method)
  eq <- normalize_ccy(equity_ccy)
  if (is.na(eq)) eq <- st
  src <- eq
  tgt <- q
  adr_needed <- adr_conversion_required(st, q, share_method)
  aligned <- shares_auto_adjust_method(share_method)

  # Conversion needed but reporting/source ccy cannot be determined.
  src_unknown <- is.na(src)
  tgt_unknown <- is.na(tgt)
  if (isTRUE(adr_needed) && src_unknown && !tgt_unknown) {
    return("STATEMENT_CURRENCY_UNAVAILABLE")
  }
  if (src_unknown && tgt_unknown) {
    src <- NA_character_
  } else if (src_unknown && !tgt_unknown && !isTRUE(adr_needed)) {
    # Ordinary + N/A: infer quote (already in `st` / skip FX).
    src <- tgt
  } else if (!src_unknown && tgt_unknown) {
    return("STATEMENT_CURRENCY_UNAVAILABLE")
  }

  need_fx <- isTRUE(fx_conversion_required(src, tgt))
  if (isTRUE(need_fx)) {
    if (is.na(src) || is.na(tgt)) {
      return("STATEMENT_CURRENCY_UNAVAILABLE")
    }
    if (!isTRUE(.fx_pair_supported(src, tgt))) {
      return("REQUIRED_FX_RATE_MISSING")
    }
    stt <- .usd_twd_rate_status(usd_twd)
    if (identical(stt, "missing")) return("REQUIRED_FX_RATE_MISSING")
    if (identical(stt, "invalid")) return("REQUIRED_FX_RATE_INVALID")
  }

  if (isTRUE(adr_needed)) {
    ratio_num <- if (is.null(adr_ratio) || length(adr_ratio) < 1L) {
      NA_real_
    } else {
      suppressWarnings(as.numeric(adr_ratio)[1])
    }
    # Explicit non-NA ratio that is unusable.
    if (!is.null(adr_ratio) && length(adr_ratio) >= 1L &&
        !is.na(ratio_num) && (!is.finite(ratio_num) || ratio_num <= 0)) {
      return("APPLICABLE_ADR_RATIO_INVALID")
    }
    sh_num <- if (is.null(shares) || length(shares) < 1L) {
      NA_real_
    } else {
      suppressWarnings(as.numeric(shares)[1])
    }
    if (!is.null(shares) && length(shares) >= 1L &&
        !is.na(sh_num) && (!is.finite(sh_num) || sh_num <= 0)) {
      return("APPLICABLE_ADR_RATIO_INVALID")
    }
    # Currencies / share-class differ but resolve did not auto-adjust.
    if (!isTRUE(aligned)) {
      return("APPLICABLE_ADR_RATIO_MISSING")
    }
  }

  req_vals <- .required_per_share_values_for_method(required_per_share, method)
  if (length(req_vals)) {
    bad <- vapply(req_vals, function(v) {
      num <- suppressWarnings(as.numeric(v)[1])
      length(num) != 1L || is.na(num) || !is.finite(num)
    }, logical(1))
    if (any(bad)) return("REQUIRED_PER_SHARE_VALUE_NON_FINITE")
  }

  NULL
}

#' TRUE when per-share in quote currency can be computed (conditional FX + ADR).
per_share_bridge_ready <- function(statement_ccy,
                                   quote_ccy,
                                   usd_twd = NULL,
                                   share_method = NULL,
                                   equity_ccy = NULL,
                                   shares = NULL,
                                   adr_ratio = NULL) {
  code <- classify_per_share_alignment_failure(
    statement_ccy = statement_ccy,
    quote_ccy = quote_ccy,
    usd_twd = usd_twd,
    share_method = share_method,
    equity_ccy = equity_ccy,
    shares = shares,
    adr_ratio = adr_ratio
  )
  is.null(code)
}

#' 將年度 fundamentals 的股數對齊報價股數（折現比較／回測 PIT）
#'
#' 以最新財年財報股數 vs Summary 市值÷股價得固定倍率，
#' 套用到所有財年股數，使 DCF／DDM／BVPS／We 與 ADR 收盤價同一級距。
#' @return fund data.frame；attr(fund, "share_align") = list(method, scale, note, ...)
align_fundamentals_shares_to_quote <- function(fund,
                                              summary_df = NULL,
                                              ticker = "",
                                              quote_currency = NULL,
                                              financial_currency = NULL,
                                              price = NA_real_,
                                              market_cap = NA_real_) {
  empty_align <- function(method = "none", scale = 1, note = NULL, ...) {
    list(method = method, scale = scale, note = note, ...)
  }
  if (is.null(fund) || !is.data.frame(fund) || nrow(fund) < 1L ||
      !("shares" %in% names(fund))) {
    return(fund)
  }
  years <- suppressWarnings(as.integer(fund$year))
  ord <- order(years, decreasing = TRUE, na.last = TRUE)
  latest_bs <- suppressWarnings(as.numeric(fund$shares[ord[1]])[1])

  qm <- tryCatch(
    extract_quote_price_mcap(summary_df),
    error = function(e) list(price = NA_real_, market_cap = NA_real_)
  )
  px <- suppressWarnings(as.numeric(price)[1])
  mc <- suppressWarnings(as.numeric(market_cap)[1])
  if (!is.finite(px) || px <= 0) px <- qm$price
  if (!is.finite(mc) || mc <= 0) mc <- qm$market_cap
  if (is.null(quote_currency) && !is.null(summary_df)) {
    quote_currency <- attr(summary_df, "currency")
  }
  if (is.null(financial_currency) && !is.null(summary_df)) {
    financial_currency <- attr(summary_df, "financialCurrency")
  }

  sh <- resolve_shares_for_price(
    latest_bs,
    price = px,
    market_cap = mc,
    ticker = ticker,
    quote_currency = quote_currency,
    financial_currency = financial_currency
  )

  if (!shares_auto_adjust_method(sh$method) ||
      !is.finite(sh$shares) || sh$shares <= 0 ||
      !is.finite(latest_bs) || latest_bs <= 0) {
    attr(fund, "share_align") <- empty_align(
      method = sh$method %||% "balance_sheet",
      scale = 1,
      note = NULL,
      shares_bs_latest = latest_bs,
      shares_quote = if (is.finite(sh$shares)) sh$shares else latest_bs
    )
    return(fund)
  }

  scale <- sh$shares / latest_bs
  if (!is.finite(scale) || scale <= 0) {
    attr(fund, "share_align") <- empty_align(method = "none", scale = 1)
    return(fund)
  }

  out <- fund
  out$shares <- suppressWarnings(as.numeric(out$shares)) * scale
  attr(out, "share_align") <- empty_align(
    method = sh$method,
    scale = scale,
    note = sh$note,
    shares_bs_latest = latest_bs,
    shares_quote = sh$shares
  )
  out
}
