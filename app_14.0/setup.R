# ==========================================
# setup.R - 財報數據處理與輔助函數模組
# ==========================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

# 圖表／縮寫數字：最多 1 位小數；|x|≥1e3 用 K/M/B/T；保留正負號
# prefix="$" 用於金額；na_str 控制 NA（軸刻度常用 ""）
.format_one_decimal <- function(x) {
  sub("\\.0$", "", sprintf("%.1f", x))
}

format_chart_number <- function(x, prefix = "", na_str = "N/A") {
  if (is.null(x)) return(na_str)
  n <- length(x)
  if (n == 0L) return(character(0))

  nums <- suppressWarnings(as.numeric(x))
  out <- character(n)
  for (i in seq_len(n)) {
    v <- nums[[i]]
    if (length(v) != 1L || is.na(v) || !is.finite(v)) {
      out[[i]] <- na_str
      next
    }
    av <- abs(v)
    if (av >= 1e12) {
      out[[i]] <- paste0(prefix, .format_one_decimal(v / 1e12), "T")
    } else if (av >= 1e9) {
      out[[i]] <- paste0(prefix, .format_one_decimal(v / 1e9), "B")
    } else if (av >= 1e6) {
      out[[i]] <- paste0(prefix, .format_one_decimal(v / 1e6), "M")
    } else if (av >= 1e3) {
      out[[i]] <- paste0(prefix, .format_one_decimal(v / 1e3), "K")
    } else {
      out[[i]] <- paste0(prefix, .format_one_decimal(v))
    }
  }
  out
}

# scales / ggplot 軸刻度專用（NA → 空白）
label_chart_number <- function(prefix = "") {
  function(x) format_chart_number(x, prefix = prefix, na_str = "")
}

# ==========================================
# 💱 Session 幣別（USD / TWD）— 估值與金額顯示共用
# ==========================================
.ynow_ccy_ctx <- new.env(parent = emptyenv())
.ynow_ccy_ctx$session_currency <- "USD"
.ynow_ccy_ctx$fx_usd_twd <- 32
.ynow_ccy_ctx$quote_currency <- "USD"
.ynow_ccy_ctx$statement_currency <- "USD"

normalize_ccy <- function(x) {
  s <- toupper(trimws(as.character(x %||% "")[1]))
  if (!nzchar(s) || identical(s, "NA")) return(NA_character_)
  if (s %in% c("USD", "USDT", "US$")) return("USD")
  if (s %in% c("TWD", "NTD", "NT$", "NT")) return("TWD")
  s
}

# Session display currency defaults to USD; quote/statement metadata still drive FX conversion.
default_session_currency <- function(quote_ccy, stmt_ccy, ticker = "") {
  "USD"
}

set_ynow_currency_context <- function(session_ccy = NULL, fx_usd_twd = NULL,
                                      quote_ccy = NULL, statement_ccy = NULL) {
  if (!is.null(session_ccy)) {
    sc <- normalize_ccy(session_ccy)
    if (!is.na(sc)) .ynow_ccy_ctx$session_currency <- sc
  }
  if (!is.null(fx_usd_twd)) {
    fx <- suppressWarnings(as.numeric(fx_usd_twd)[1])
    if (is.finite(fx) && fx > 0) .ynow_ccy_ctx$fx_usd_twd <- fx
  }
  if (!is.null(quote_ccy)) {
    qc <- normalize_ccy(quote_ccy)
    if (!is.na(qc)) .ynow_ccy_ctx$quote_currency <- qc
  }
  if (!is.null(statement_ccy)) {
    fc <- normalize_ccy(statement_ccy)
    if (!is.na(fc)) .ynow_ccy_ctx$statement_currency <- fc
  }
  invisible(NULL)
}

money_prefix <- function(ccy = NULL) {
  sc <- normalize_ccy(ccy %||% .ynow_ccy_ctx$session_currency)
  if (identical(sc, "TWD")) "NT$" else "$"
}

money_label <- function(ccy = NULL) {
  sc <- normalize_ccy(ccy %||% .ynow_ccy_ctx$session_currency)
  if (identical(sc, "TWD")) "TWD" else "USD"
}

dt_currency_symbol <- function(ccy = NULL) {
  money_prefix(ccy)
}

# USD↔TWD only (v1). Same currency → 1; unknown → 1 with warning-free fallback.
fx_factor <- function(from_ccy, to_ccy, usd_twd = NULL) {
  fr <- normalize_ccy(from_ccy)
  to <- normalize_ccy(to_ccy)
  if (is.na(fr) || is.na(to) || identical(fr, to)) return(1)
  fx <- suppressWarnings(as.numeric(usd_twd %||% .ynow_ccy_ctx$fx_usd_twd)[1])
  if (!is.finite(fx) || fx <= 0) fx <- 32
  if (identical(fr, "USD") && identical(to, "TWD")) return(fx)
  if (identical(fr, "TWD") && identical(to, "USD")) return(1 / fx)
  1
}

money_to_session <- function(x, from_ccy, session_ccy = NULL, usd_twd = NULL) {
  nums <- suppressWarnings(as.numeric(x))
  mult <- fx_factor(from_ccy, session_ccy %||% .ynow_ccy_ctx$session_currency, usd_twd)
  nums * mult
}

# Dashboard 財報顯示：一律四捨五入到小數點第二位（金額／比率等數字欄）
format_financial_display_number <- function(x, digits = 2) {
  nums <- suppressWarnings(as.numeric(x))
  out <- character(length(nums))
  for (i in seq_along(nums)) {
    v <- nums[[i]]
    if (length(v) != 1L || is.na(v) || !is.finite(v)) {
      out[[i]] <- NA_character_
      next
    }
    out[[i]] <- format(round(v, digits), nsmall = digits, scientific = FALSE, trim = TRUE)
  }
  out
}

# Scale all period columns of a Yahoo financial statement DF into session currency.
# Keeps full numeric precision for downstream KPI／估值；顯示四捨五入見 format_financial_df_display。
scale_financial_df_money <- function(df, from_ccy, session_ccy = NULL, usd_twd = NULL) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) < 2) return(df)
  mult <- fx_factor(from_ccy, session_ccy %||% .ynow_ccy_ctx$session_currency, usd_twd)
  if (!is.finite(mult) || abs(mult - 1) < 1e-15) return(df)
  out <- df
  for (j in seq.int(2L, ncol(out))) {
    raw <- as.character(out[[j]])
    nums <- parse_financial_number(raw)
    scaled <- nums * mult
    out[[j]] <- ifelse(
      is.na(scaled),
      raw,
      format(scaled, scientific = FALSE, trim = TRUE, digits = 15)
    )
  }
  out
}

# Dashboard 三大報表表格顯示用：期間欄數字四捨五入到 digits 位
format_financial_df_display <- function(df, digits = 2) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) < 2) return(df)
  out <- df
  for (j in seq.int(2L, ncol(out))) {
    raw <- as.character(out[[j]])
    nums <- parse_financial_number(raw)
    formatted <- format_financial_display_number(nums, digits = digits)
    out[[j]] <- ifelse(is.na(formatted), raw, formatted)
  }
  out
}

# Convert a Yahoo summary Value cell (quote ccy → session) for money-like items.
convert_summary_value_display <- function(item, value, from_ccy, session_ccy = NULL, usd_twd = NULL) {
  it <- as.character(item %||% "")[1]
  raw <- as.character(value %||% "")[1]
  if (!nzchar(raw) || identical(raw, "N/A")) return(raw)
  money_exact <- c(
    "Previous Close", "Open", "Bid", "Ask", "Market Cap (intraday)",
    "EPS (TTM)", "Dividend", "Target Est"
  )
  if (it %in% money_exact) {
    n <- parse_financial_number(raw)[1]
    if (!is.finite(n)) return(raw)
    conv <- money_to_session(n, from_ccy, session_ccy, usd_twd)
    if (it == "Market Cap (intraday)") return(format_money_abbr(conv, session_ccy))
    return(format_financial_display_number(conv, digits = 2)[1])
  }
  if (it %in% c("Day's Range", "52 Week Range") && grepl(" - ", raw, fixed = TRUE)) {
    parts <- strsplit(raw, " - ", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      a <- parse_financial_number(parts[1])[1]
      b <- parse_financial_number(parts[2])[1]
      if (is.finite(a) && is.finite(b)) {
        a2 <- money_to_session(a, from_ccy, session_ccy, usd_twd)
        b2 <- money_to_session(b, from_ccy, session_ccy, usd_twd)
        return(paste0(
          format_financial_display_number(a2, digits = 2)[1],
          " - ",
          format_financial_display_number(b2, digits = 2)[1]
        ))
      }
    }
  }
  # Yield 等百分比：保留 %，數字四捨五入到小數第二位
  if (grepl("%\\s*$", raw)) {
    n_pct <- parse_financial_number(sub("%\\s*$", "", raw))[1]
    if (is.finite(n_pct)) {
      return(paste0(format_financial_display_number(n_pct, digits = 2)[1], "%"))
    }
  }
  # PE / Beta / Volume 等純數字：同樣顯示到小數第二位
  n_plain <- parse_financial_number(raw)[1]
  if (is.finite(n_plain) && !grepl("[A-Za-z%]", gsub("[,\\$]", "", raw))) {
    return(format_financial_display_number(n_plain, digits = 2)[1])
  }
  raw
}

# 金額縮寫（valueBox／圖表標籤共用）；prefix 隨 session 幣別
format_money_abbr <- function(x, ccy = NULL) {
  format_chart_number(x, prefix = money_prefix(ccy))
}

format_dollar_abbr <- function(x) {
  format_money_abbr(x, .ynow_ccy_ctx$session_currency)
}

# 解析含英文單位後綴的財報數字 (e.g. 122.15B, -3.2M, 450K, 1.2T)
parse_financial_number <- function(x) {
  if (length(x) == 0) return(numeric(0))
  s <- trimws(as.character(x))
  out <- rep(NA_real_, length(s))
  invalid <- s %in% c("-", "", "NA", "NaN", "N/A", "--", "null")
  s[invalid] <- NA_character_
  idx <- !is.na(s)
  if (!any(idx)) return(out)
  
  cleaned <- s[idx]
  cleaned <- gsub("[,\\$%]", "", cleaned)
  cleaned <- gsub("\\s+", "", cleaned)
  
  mult <- rep(1, length(cleaned))
  upper <- toupper(cleaned)
  mult[grepl("T$", upper)] <- 1e12
  mult[grepl("B$", upper)] <- 1e9
  mult[grepl("M$", upper)] <- 1e6
  mult[grepl("K$", upper)] <- 1e3
  cleaned <- sub("[TBMK]$", "", upper)
  
  nums <- suppressWarnings(as.numeric(cleaned))
  out[idx] <- nums * mult
  out
}

# 將 Python pandas / payload / 其他表格物件轉為 R data.frame
coerce_financial_df <- function(df) {
  if (is.null(df)) return(NULL)
  if (is.data.frame(df)) return(df)

  # app_11.0：Python 回傳 list(columns=..., data=...) 避免 reticulate 吃掉 pandas
  if (is.list(df) && !is.null(df$columns) && !is.null(df$data)) {
    cols <- as.character(unlist(df$columns, use.names = FALSE))
    rows <- df$data
    if (is.null(rows) || length(rows) == 0) {
      out <- as.data.frame(matrix(nrow = 0, ncol = length(cols)), stringsAsFactors = FALSE)
      names(out) <- cols
      return(out)
    }
    mat <- do.call(rbind, lapply(rows, function(r) {
      r <- as.character(unlist(r, use.names = FALSE))
      length(r) <- length(cols)
      r
    }))
    out <- as.data.frame(mat, stringsAsFactors = FALSE)
    names(out) <- cols
    return(out)
  }

  if (requireNamespace("reticulate", quietly = TRUE)) {
    out <- tryCatch(reticulate::py_to_r(df), error = function(e) NULL)
    if (is.data.frame(out)) return(out)
    # 遞迴處理 py_to_r 後仍是 columns/data 結構的情況
    if (is.list(out) && !is.null(out$columns) && !is.null(out$data)) {
      return(coerce_financial_df(out))
    }
  }
  tryCatch(as.data.frame(df, stringsAsFactors = FALSE), error = function(e) NULL)
}

# 將財報表格欄位重排：Breakdown | TTM | 最新財年 → 最舊財年
reorder_financial_columns <- function(df) {
  df <- coerce_financial_df(df)
  if (is.null(df) || !is.data.frame(df) || ncol(df) < 2) return(df)
  
  label_col <- colnames(df)[1]
  period_cols <- colnames(df)[-1]
  
  is_ttm <- grepl("^ttm$", period_cols, ignore.case = TRUE)
  ttm_cols <- period_cols[is_ttm]
  fy_cols <- period_cols[!is_ttm]
  
  parse_fy_date <- function(x) {
    d <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
    if (!is.na(d)) return(d)
    suppressWarnings(as.Date(x))
  }
  
  if (length(fy_cols) > 0) {
    fy_dates <- vapply(fy_cols, parse_fy_date, FUN.VALUE = as.Date("1970-01-01"))
    fy_cols <- fy_cols[order(fy_dates, decreasing = TRUE)]
  }
  
  df[, c(label_col, ttm_cols, fy_cols), drop = FALSE]
}

# 標準化三表結構（collapsed / expanded 皆重排欄位）
normalize_financial_statement <- function(stmt) {
  if (is.null(stmt)) return(stmt)
  list(
    collapsed = reorder_financial_columns(coerce_financial_df(stmt$collapsed)),
    expanded  = reorder_financial_columns(coerce_financial_df(stmt$expanded))
  )
}

normalize_all_financials <- function(res) {
  if (is.null(res)) return(res)
  lapply(res, normalize_financial_statement)
}

# 從財報 DataFrame 中抽出特定科目的數值陣列
# 欄位順序須為 TTM | 最新財年 → 最舊財年；[1] = 當期（含 TTM 時為 TTM）
select_clean_metric_row <- function(df, metric_name, include_ttm = TRUE) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NA)
  
  row_idx <- grep(metric_name, df[[1]], ignore.case = TRUE)
  if (length(row_idx) == 0) return(NA)
  
  period_cols <- colnames(df)[-1]
  if (!include_ttm) {
    period_cols <- period_cols[!grepl("^ttm$", period_cols, ignore.case = TRUE)]
  }
  if (length(period_cols) == 0) return(NA)
  
  vals <- as.character(df[row_idx[1], period_cols, drop = FALSE])
  parse_financial_number(vals)
}

# 依偏好順序嘗試多個科目別名（yfinance vs Yahoo HTML 命名差異）
# 不可把別名用 | 併成單一 grep：列序會讓「較早出現的寬鬆別名」搶先命中
select_clean_metric_row_any <- function(df, metric_names, include_ttm = TRUE) {
  for (nm in metric_names) {
    vals <- select_clean_metric_row(df, nm, include_ttm = include_ttm)
    if (length(vals) > 0 && !all(is.na(vals))) return(vals)
  }
  NA
}

# 常用科目別名（Yahoo HTML 用 &；yfinance 用 And）
NET_INCOME_PATTERNS <- c(
  "Net Income From Continuing (And|&) Discontinued Operation",
  "Net Income Common Stockholders",
  "^Net Income$"
)
# 利息費用（損益／現金流別名；勿用 | 併成單一 grep）
INTEREST_EXPENSE_PATTERNS <- c(
  "^Interest Expense$",
  "Interest Expense Non Operating",
  "Net Interest Expense",
  "Interest Expense"
)
# D&A：寬科目優先（AMZN 等同時有 Depreciation 與 Depreciation And Amortization）
DA_PATTERNS <- c(
  "^Depreciation And Amortization$",
  "^Depreciation & Amortization$",
  "^Depreciation Amortization Depletion$",
  "^Depreciation, Amortization & Other$",
  "^Depreciation$"
)
# 營運資金變動（Yahoo HTML / yfinance 大小寫與複數差異）
NWC_CHANGE_PATTERNS <- c(
  "^Change In Working Capital$",
  "^Changes In Working Capital$",
  "Change In Working Capital",
  "Changes In Working Capital"
)
INTEREST_PAID_PATTERNS <- c(
  "^Interest Paid$",
  "Cash Interest Paid",
  "Interest Paid"
)

EQUITY_PATTERNS <- c(
  "Common Stock Equity",
  "Stockholders Equity",
  "Total Equity Gross Minority Interest"
)
# 流通股數偏好序（勿用 | 併成單一 grep）
SHARE_PATTERNS <- c(
  "Ordinary Shares Number",
  "Total Shares Outstanding",
  "Share Issued"
)
OPEX_PATTERNS <- c(
  "^Operating Expense$",
  "^Operating Expenses$"
)

# 投資證券未實現損益（用於 OCF vs 營運利潤之現金轉換品質檢查）
UNREALIZED_INVESTMENT_GL_PATTERNS <- c(
  "Unrealized Gain.?Loss On Investment Securities",
  "Net Unrealized Gain.?Loss On Investment Securities",
  "Unrealized Gains?/Losses? On Investment Securities",
  "Unrealized Gain/Loss [Oo]n Investment Securities"
)

#' NI − Unrealized G/L on Investment Securities（缺科目時視為 0）
#' @return numeric vector aligned to NI periods
operating_earnings_from_ni <- function(ni, unrealized) {
  ni <- suppressWarnings(as.numeric(ni))
  if (length(ni) == 0L) return(ni)
  u <- suppressWarnings(as.numeric(unrealized))
  if (length(u) == 0L || (length(u) == 1L && is.na(u))) {
    u <- rep(0, length(ni))
  } else if (length(u) < length(ni)) {
    u <- c(u, rep(NA_real_, length(ni) - length(u)))
  } else if (length(u) > length(ni)) {
    u <- u[seq_along(ni)]
  }
  u[!is.finite(u)] <- 0
  ifelse(is.finite(ni), ni - u, NA_real_)
}

#' 從損益表取營運利潤列（現金品質檢查用）
get_operating_earnings_row <- function(d_is, include_ttm = FALSE) {
  ni <- select_clean_metric_row_any(d_is, NET_INCOME_PATTERNS, include_ttm = include_ttm)
  unreal <- select_clean_metric_row_any(
    d_is, UNREALIZED_INVESTMENT_GL_PATTERNS, include_ttm = include_ttm
  )
  operating_earnings_from_ni(ni, unreal)
}

get_operating_earnings_avg <- function(d_is, include_ttm = FALSE) {
  get_avg(get_operating_earnings_row(d_is, include_ttm = include_ttm))
}

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
  px_row <- summary_df[grep("Previous Close|Market Price", summary_df$Item, ignore.case = TRUE), , drop = FALSE]
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

  # Berkshire Class B = 1/1500 economic interest of Class A
  if (grepl("^BRK-B$", ticker) && is.finite(shares_bs) && shares_bs > 0) {
    if (is.finite(shares_implied) && (shares_implied / shares_bs) > 100) {
      return(list(
        shares = shares_implied,
        method = "market_cap_per_price",
        note = "BRK-B 雙重股權：財報股數偏 A 級，已改用 市值÷股價 作為 B 級約當股數",
        ratio = shares_implied / shares_bs
      ))
    }
    return(list(
      shares = shares_bs * 1500,
      method = "brk_b_x1500",
      note = "BRK-B：以 Class A 股數 × 1500 換算 B 級約當股數",
      ratio = 1500
    ))
  }

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

#' 是否應自動套用約當股數（ADR／雙重股權等）
shares_auto_adjust_method <- function(method) {
  identical(method, "market_cap_per_price") || identical(method, "brk_b_x1500")
}

#' 將年度 fundamentals 的股數對齊報價股數（折現比較／回測 PIT）
#'
#' 以最新財年財報股數 vs Summary 市值÷股價（或 BRK-B 規則）得固定倍率，
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

#' 報價端 Book Value 是否與目前股價同一股權級距（排除 BRK-B 誤用 A 級 BV）
quote_book_value_is_plausible <- function(book_value, price) {
  book_value <- suppressWarnings(as.numeric(book_value)[1])
  price <- suppressWarnings(as.numeric(price)[1])
  if (!is.finite(book_value) || !is.finite(price) || book_value <= 0 || price <= 0) {
    return(FALSE)
  }
  # 隱含 P/B = price/book 落在合理區間才採用
  pb <- price / book_value
  is.finite(pb) && pb >= 0.3 && pb <= 8
}

# 取得當期單一數值：流量科目優先 TTM，存量科目用最新財年
select_current_metric <- function(df, metric_name, type = c("flow", "stock")) {
  type <- match.arg(type)
  include_ttm <- identical(type, "flow")
  vals <- select_clean_metric_row(df, metric_name, include_ttm = include_ttm)
  if (length(vals) == 0 || all(is.na(vals))) return(NA_real_)
  vals[1]
}

select_current_metric_any <- function(df, metric_names, type = c("flow", "stock")) {
  type <- match.arg(type)
  include_ttm <- identical(type, "flow")
  vals <- select_clean_metric_row_any(df, metric_names, include_ttm = include_ttm)
  if (length(vals) == 0 || all(is.na(vals))) return(NA_real_)
  vals[1]
}

#' Mean of (num/den) over the first `n` paired observations (newest-first series).
#' Used for DCF projection margins so a single CapEx spike year (e.g. AMZN) does not dominate.
avg_ratio_newest <- function(num, den, n = 3L) {
  num <- suppressWarnings(as.numeric(num))
  den <- suppressWarnings(as.numeric(den))
  m <- min(length(num), length(den), as.integer(n))
  if (m < 1L) return(NA_real_)
  ratios <- num[seq_len(m)] / den[seq_len(m)]
  ratios <- ratios[is.finite(ratios) & is.finite(den[seq_len(m)]) & den[seq_len(m)] != 0]
  if (length(ratios) < 1L) return(NA_real_)
  mean(ratios)
}

#' True when newest ratio is an outlier vs the prior window (excludes newest from baseline).
ratio_spike_vs_prior <- function(num, den, prior_n = 2L, mult = 1.35) {
  num <- suppressWarnings(as.numeric(num))
  den <- suppressWarnings(as.numeric(den))
  if (length(num) < 2L || length(den) < 2L) return(FALSE)
  latest <- num[1] / den[1]
  if (!is.finite(latest) || !is.finite(den[1]) || den[1] == 0) return(FALSE)
  prior <- avg_ratio_newest(num[-1], den[-1], n = prior_n)
  if (!is.finite(prior) || prior <= 0) return(FALSE)
  isTRUE(latest > mult * prior)
}

# 裁切財務表格至指定科目（含該列）
# Yahoo 網頁列序：營收在上、end_metric 在下 → 保留 1:idx
# 若 end_metric 落在第 1 列（常見於未反轉的 yfinance），改取最後一個命中，避免只剩一列
trim_financial_table <- function(df, end_metric) {
  if (is.null(df) || nrow(df) == 0) return(df)
  idx <- grep(end_metric, df[[1]], ignore.case = TRUE)
  if (length(idx) == 0) return(df)
  end_i <- idx[1]
  if (end_i == 1L && length(idx) == 1L && nrow(df) > 1) {
    # 裁切錨點在頂端 → 視為列序相反，不裁切（保留全表）
    return(df)
  }
  if (end_i == 1L && length(idx) > 1L) {
    end_i <- idx[length(idx)]
  }
  df[seq_len(end_i), , drop = FALSE]
}

# 取得最新一期期末現金餘額
get_latest_cash_position <- function(df_cf) {
  if (is.null(df_cf) || nrow(df_cf) == 0) return(NA)
  cash_kws <- c("End Cash Position", "Ending Cash Position", "Cash at End of Period")
  for (kw in cash_kws) {
    val <- select_clean_metric_row(df_cf, kw)
    if (length(val) > 0 && !all(is.na(val))) return(select_current_metric(df_cf, kw, "flow"))
  }
  return(NA) 
}

# 計算陣列的平均值
get_avg <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(x) == 0) return(NA)
  return(mean(x, na.rm = TRUE))
}

# 計算陣列的平均成長率（輸入須為最新財年 → 最舊財年）
get_avg_growth <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(x) < 2) return(NA)
  
  # YoY = (較新 - 較舊) / |較舊|；勿用 diff()/head()（會把成長算成負值）
  rates <- (head(x, -1) - tail(x, -1)) / abs(tail(x, -1))
  rates <- rates[is.finite(rates)]
  
  if (length(rates) == 0) return(NA)
  mean(rates, na.rm = TRUE) * 100
}

# ==========================================
# 🚨 數據缺失警示 UI 共用函數
# ==========================================
ui_missing_data_alert <- function(check_list, fallback_msg = "系統已自動將上述項目視為 0 代入計算。請確認是否需要手動補齊數值以確保預測精確度。") {
  
  # 找出 NA, NULL 或空字串的項目
  missing_items <- names(check_list)[sapply(check_list, function(x) {
    is.null(x) || is.na(x) || (is.character(x) && trimws(x) == "")
  })]
  
  if (length(missing_items) > 0) {
    shiny::div(
      style = "color: #a94442; background-color: #f2dede; border: 1px solid #ebccd1; padding: 15px; border-radius: 4px; margin-bottom: 20px;",
      shiny::icon("exclamation-triangle"), 
      shiny::tags$b(" 偵測到資料缺漏："), 
      shiny::tags$span(style = "color: #c7254e; font-weight: bold;", paste(missing_items, collapse = "、 ")),
      shiny::br(),
      shiny::span(style = "font-size: 13px;", fallback_msg)
    )
  } else {
    NULL # 無缺值則不回傳 UI
  }
}

# 確保數值安全，若無值則回傳 0
safe_num <- function(x) {
  if (is.null(x) || is.na(x) || !is.numeric(x)) return(0)
  return(x)
}

# Relative ±1% shock for formula-param elasticity tables (DCF / DDM / RI / P/B).
# |ε| is d ln V / d ln x at the current MODEL PARAMETERS, not ticker price/shares.
PARAM_SENSITIVITY_SHOCK <- 0.01

.param_rel_shock <- function(x, sign = -1, shock = PARAM_SENSITIVITY_SHOCK) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x) || abs(x) < 1e-12) return(NA_real_)
  x * (1 + sign * shock)
}

#' Build one row for relative ±1% elasticity tables (DCF / DDM / RI / P/B).
#' Shock is multiplicative on the parameter; Δ估值% ≈ elasticity ε when shock = 1%.
.param_sensitivity_infl_row <- function(param, base_val, unit, p0, p_down, p_up, note = "") {
  d_dn <- if (is.finite(p_down) && is.finite(p0) && abs(p0) > 1e-9) 100 * (p_down - p0) / abs(p0) else NA_real_
  d_up <- if (is.finite(p_up) && is.finite(p0) && abs(p0) > 1e-9) 100 * (p_up - p0) / abs(p0) else NA_real_
  infl <- mean(c(abs(d_dn), abs(d_up)), na.rm = TRUE)
  if (!is.finite(infl)) infl <- NA_real_
  data.frame(
    參數 = param,
    基準值 = if (identical(unit, "%")) sprintf("%.2f%%", base_val) else
      if (identical(unit, "$")) {
        if (exists("format_dollar_abbr", mode = "function")) format_dollar_abbr(base_val) else sprintf("%.2f", base_val)
      } else if (identical(unit, "x")) sprintf("%.2f", base_val) else as.character(base_val),
    `估值Δ% (−1%)` = if (is.finite(d_dn)) sprintf("%+.2f%%", d_dn) else "N/A",
    `估值Δ% (+1%)` = if (is.finite(d_up)) sprintf("%+.2f%%", d_up) else "N/A",
    `｜ε｜` = if (is.finite(infl)) sprintf("%.2f", infl) else "N/A",
    說明 = note,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Elasticity row when the feasible shock on x is not exactly ±1% (e.g. integer n).
#' Converts (ΔV/V)/(Δx/x) into the same ±1% display units as `.param_sensitivity_infl_row`.
.param_sensitivity_infl_row_xy <- function(param, base_val, unit, v0, v_dn, v_up,
                                           x0, x_dn, x_up, note = "",
                                           shock = PARAM_SENSITIVITY_SHOCK) {
  .el <- function(v1, x1) {
    if (!all(is.finite(c(v0, v1, x0, x1))) || abs(v0) <= 1e-9) return(NA_real_)
    if (!is.finite(x0) || abs(x0) < 1e-12 || abs(x1 - x0) < 1e-12) return(NA_real_)
    ((v1 - v0) / abs(v0)) / ((x1 - x0) / x0)
  }
  e_dn <- .el(v_dn, x_dn)
  e_up <- .el(v_up, x_up)
  d_dn <- if (is.finite(e_dn)) 100 * e_dn * (-shock) else NA_real_
  d_up <- if (is.finite(e_up)) 100 * e_up * shock else NA_real_
  p_dn <- if (is.finite(d_dn) && is.finite(v0)) v0 * (1 + d_dn / 100) else NA_real_
  p_up <- if (is.finite(d_up) && is.finite(v0)) v0 * (1 + d_up / 100) else NA_real_
  .param_sensitivity_infl_row(param, base_val, unit, v0, p_dn, p_up, note)
}

#' True when a parameter can take a relative ±1% shock (skip ~0 like DCF g).
.param_sensitivity_rel_ok <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  is.finite(x) && abs(x) > 1e-8
}

#' Canonical DCF enterprise value from formula parameters only (unit starting FCFF).
#' F_t = F0 × product of explicit-period growth; TV = F_n(1+g)/(r2−g).
#' Independent of ticker cash, debt, shares, market price, and FCFF dollar level.
#' Rates are decimals (e.g. 0.08 not 8).
.dcf_formula_ev <- function(n, r1, g_term, g_near = 0, r2 = NULL,
                            yr_stage1 = NULL, g_stage2 = NULL, f0 = 1) {
  n <- suppressWarnings(as.integer(round(as.numeric(n)[1])))
  if (!is.finite(n) || n < 1L) return(NA_real_)
  r1 <- suppressWarnings(as.numeric(r1)[1])
  r2 <- if (is.null(r2)) r1 else suppressWarnings(as.numeric(r2)[1])
  g_term <- suppressWarnings(as.numeric(g_term)[1])
  g_near <- suppressWarnings(as.numeric(g_near)[1])
  f0 <- suppressWarnings(as.numeric(f0)[1])
  if (!is.finite(g_near)) g_near <- 0
  if (!is.finite(r1) || !is.finite(r2) || !is.finite(g_term) || !is.finite(f0)) return(NA_real_)
  if (r1 <= -0.999 || r2 <= -0.999 || g_term >= r2) return(NA_real_)

  two_stage <- !is.null(yr_stage1) && is.finite(as.numeric(yr_stage1)[1]) && n > 1L
  if (two_stage) {
    y1 <- max(1L, min(n, as.integer(round(as.numeric(yr_stage1)[1]))))
    rs <- c(rep(r1, min(y1, n)), rep(r2, max(0L, n - y1)))
    g2 <- if (!is.null(g_stage2) && is.finite(as.numeric(g_stage2)[1])) {
      as.numeric(g_stage2)[1]
    } else {
      g_near
    }
    gs <- c(rep(g_near, max(0L, y1 - 1L)), rep(g2, max(0L, n - y1)))
    if (length(gs) < n - 1L) {
      pad <- if (length(gs) == 0L) g_near else gs[length(gs)]
      gs <- c(gs, rep(pad, n - 1L - length(gs)))
    }
    gs <- gs[seq_len(max(0L, n - 1L))]
  } else {
    rs <- rep(r1, n)
    gs <- rep(g_near, max(0L, n - 1L))
  }

  f <- numeric(n)
  f[1] <- f0
  if (n >= 2L && length(gs) >= 1L) {
    for (t in seq_len(n - 1L)) {
      f[t + 1L] <- f[t] * (1 + gs[t])
    }
  }
  disc <- cumprod(1 + rs)
  if (any(!is.finite(disc)) || any(abs(disc) < 1e-15)) return(NA_real_)
  pv <- sum(f / disc)
  tv <- f[n] * (1 + g_term) / (r2 - g_term)
  pv + tv / disc[n]
}

#' Unit-revenue FCFF path matching `fcf_projection_module` (base Rev = 1).
#' Year-1 revenue grows from 1; FCFF_t = Rev_t × (NOPAT+D&A−CapEx margins) − ΔRev_t × NWC margin.
#' Independent of dollar scale. Growth rates and margins are decimals.
.dcf_unit_fcff_path <- function(n, g_near = 0, g_stage2 = NULL, yr_stage1 = NULL,
                                nopat_m = 0, depre_m = 0, capex_m = 0, nwc_m = 0,
                                two_stage = FALSE) {
  n <- suppressWarnings(as.integer(round(as.numeric(n)[1])))
  if (!is.finite(n) || n < 1L) return(numeric(0))
  g_near <- suppressWarnings(as.numeric(g_near)[1])
  if (!is.finite(g_near)) g_near <- 0
  g2 <- if (!is.null(g_stage2) && is.finite(as.numeric(g_stage2)[1])) {
    as.numeric(g_stage2)[1]
  } else {
    g_near
  }
  nopat_m <- suppressWarnings(as.numeric(nopat_m)[1]); if (!is.finite(nopat_m)) nopat_m <- 0
  depre_m <- suppressWarnings(as.numeric(depre_m)[1]); if (!is.finite(depre_m)) depre_m <- 0
  capex_m <- suppressWarnings(as.numeric(capex_m)[1]); if (!is.finite(capex_m)) capex_m <- 0
  nwc_m <- suppressWarnings(as.numeric(nwc_m)[1]); if (!is.finite(nwc_m)) nwc_m <- 0
  if (isTRUE(two_stage) && !is.null(yr_stage1) && is.finite(as.numeric(yr_stage1)[1]) && n > 1L) {
    y1 <- max(1L, min(n, as.integer(round(as.numeric(yr_stage1)[1]))))
    g_path <- ifelse(seq_len(n) <= y1, g_near, g2)
  } else {
    g_path <- rep(g_near, n)
  }
  rev <- numeric(n)
  fcff <- numeric(n)
  base_rev <- 1
  for (i in seq_len(n)) {
    prev <- if (i == 1L) base_rev else rev[i - 1L]
    rev[i] <- prev * (1 + g_path[i])
    fcff[i] <- rev[i] * (nopat_m + depre_m - capex_m) - (rev[i] - prev) * nwc_m
  }
  fcff
}

#' Discount an explicit FCFF path with the same WACC / Gordon TV as `.dcf_formula_ev`.
.dcf_formula_ev_from_fcff <- function(fcff, r1, g_term, r2 = NULL, yr_stage1 = NULL) {
  fcff <- suppressWarnings(as.numeric(fcff))
  n <- length(fcff)
  if (n < 1L || any(!is.finite(fcff))) return(NA_real_)
  r1 <- suppressWarnings(as.numeric(r1)[1])
  r2 <- if (is.null(r2)) r1 else suppressWarnings(as.numeric(r2)[1])
  g_term <- suppressWarnings(as.numeric(g_term)[1])
  if (!is.finite(r1) || !is.finite(r2) || !is.finite(g_term)) return(NA_real_)
  if (r1 <= -0.999 || r2 <= -0.999 || g_term >= r2) return(NA_real_)
  two_stage <- !is.null(yr_stage1) && is.finite(as.numeric(yr_stage1)[1]) && n > 1L
  if (two_stage) {
    y1 <- max(1L, min(n, as.integer(round(as.numeric(yr_stage1)[1]))))
    rs <- c(rep(r1, min(y1, n)), rep(r2, max(0L, n - y1)))
  } else {
    rs <- rep(r1, n)
  }
  disc <- cumprod(1 + rs)
  if (any(!is.finite(disc)) || any(abs(disc) < 1e-15)) return(NA_real_)
  pv <- sum(fcff / disc)
  tv <- fcff[n] * (1 + g_term) / (r2 - g_term)
  pv + tv / disc[n]
}

#' Present value of each explicit-period cash flow (no terminal value mixed in).
#' `rates` are decimals (WACC or Ke); length 1 is recycled. Invalid rates fall back to 10%.
dcf_yearly_cf_pv <- function(cf, rates) {
  cf <- suppressWarnings(as.numeric(cf))
  n <- length(cf)
  if (n < 1L) return(numeric(0))
  rates <- suppressWarnings(as.numeric(rates))
  if (length(rates) < 1L) rates <- 0.1
  if (length(rates) == 1L) rates <- rep(rates, n)
  if (length(rates) < n) rates <- c(rates, rep(tail(rates, 1), n - length(rates)))
  rates <- rates[seq_len(n)]
  rates[!is.finite(rates) | rates <= -0.999] <- 0.1
  cf / cumprod(1 + rates)
}

#' Gordon terminal value at year n and its t=0 present value (kept off the yearly PV series).
dcf_gordon_tv_pv <- function(last_cf, g, r, discount_factor_n) {
  last_cf <- suppressWarnings(as.numeric(last_cf)[1])
  g <- suppressWarnings(as.numeric(g)[1])
  r <- suppressWarnings(as.numeric(r)[1])
  df_n <- suppressWarnings(as.numeric(discount_factor_n)[1])
  empty <- list(tv = NA_real_, pv_tv = NA_real_)
  if (!is.finite(last_cf) || !is.finite(g) || !is.finite(r) || !is.finite(df_n)) return(empty)
  if (r <= g || abs(df_n) < 1e-15) return(empty)
  tv <- last_cf * (1 + g) / (r - g)
  list(tv = tv, pv_tv = tv / df_n)
}

#' CAPM cost of equity in percent: Ke = Rf + β(Rm − Rf).
.capm_ke_pct <- function(rf_pct, beta, rm_pct) {
  rf_pct <- suppressWarnings(as.numeric(rf_pct)[1])
  beta <- suppressWarnings(as.numeric(beta)[1])
  rm_pct <- suppressWarnings(as.numeric(rm_pct)[1])
  if (!is.finite(rf_pct) || !is.finite(beta) || !is.finite(rm_pct)) return(NA_real_)
  rf_pct + beta * (rm_pct - rf_pct)
}

#' Elasticity rows for CAPM inputs that feed Ke (DDM / RI). `p_at_ke_pct(ke_pct)` returns value.
.param_sensitivity_capm_ke_rows <- function(v0, p_at_ke_pct, rf0, beta0, rm0,
                                            shock = PARAM_SENSITIVITY_SHOCK) {
  rows <- list()
  .rel <- function(x, sign = -1) .param_rel_shock(x, sign = sign, shock = shock)
  .ke <- function(rf = rf0, beta = beta0, rm = rm0) .capm_ke_pct(rf, beta, rm)
  if (!is.finite(.ke()) || !is.finite(v0)) return(rows)
  if (.param_sensitivity_rel_ok(rf0)) {
    rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
      "無風險利率 Rf", rf0, "%", v0,
      p_at_ke_pct(.ke(rf = .rel(rf0, -1))),
      p_at_ke_pct(.ke(rf = .rel(rf0, +1))),
      "CAPM：Ke = Rf + β(Rm−Rf)"
    )
  }
  if (.param_sensitivity_rel_ok(beta0)) {
    rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
      "Beta (β)", beta0, "x", v0,
      p_at_ke_pct(.ke(beta = .rel(beta0, -1))),
      p_at_ke_pct(.ke(beta = .rel(beta0, +1))),
      "CAPM：Ke = Rf + β(Rm−Rf)"
    )
  }
  if (.param_sensitivity_rel_ok(rm0)) {
    rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
      "市場報酬率 Rm", rm0, "%", v0,
      p_at_ke_pct(.ke(rm = .rel(rm0, -1))),
      p_at_ke_pct(.ke(rm = .rel(rm0, +1))),
      "CAPM：Ke = Rf + β(Rm−Rf)"
    )
  }
  rows
}

#' Canonical Gordon DDM price from formula parameters only (unit D0).
#' P0 = D0(1+g)/(Ke−g). Independent of ticker price, shares, and dividend dollars.
#' Rates are decimals (e.g. 0.08 not 8).
.ddm_formula_p0 <- function(d0 = 1, g, ke) {
  d0 <- suppressWarnings(as.numeric(d0)[1])
  g <- suppressWarnings(as.numeric(g)[1])
  ke <- suppressWarnings(as.numeric(ke)[1])
  if (!is.finite(d0) || !is.finite(g) || !is.finite(ke) || ke <= g) return(NA_real_)
  d0 * (1 + g) / (ke - g)
}

#' Two-stage DDM: high-growth g1 for n years, then Gordon at g2.
#' D_t = D0(1+g1)^t for t=1..n; TV = D_n(1+g2)/(Ke−g2); P0 = Σ PV(D_t) + PV(TV).
.ddm_formula_two_stage <- function(d0 = 1, g1, n, g2, ke) {
  d0 <- suppressWarnings(as.numeric(d0)[1])
  g1 <- suppressWarnings(as.numeric(g1)[1])
  g2 <- suppressWarnings(as.numeric(g2)[1])
  ke <- suppressWarnings(as.numeric(ke)[1])
  n <- suppressWarnings(as.integer(round(as.numeric(n)[1])))
  if (!is.finite(d0) || !is.finite(g1) || !is.finite(g2) || !is.finite(ke)) return(NA_real_)
  if (!is.finite(n) || n < 1L) return(.ddm_formula_p0(d0 = d0, g = g2, ke = ke))
  if (ke <= g2) return(NA_real_)
  dts <- d0 * (1 + g1)^seq_len(n)
  dfs <- (1 + ke)^seq_len(n)
  pv_div <- sum(dts / dfs)
  tv <- dts[n] * (1 + g2) / (ke - g2)
  pv_div + tv / dfs[n]
}

#' True when DCF cash-flow claim is FCFE (Ke, equity) rather than FCFF (WACC, EV).
dcf_claim_is_fcfe <- function(claim) {
  identical(as.character(claim %||% "fcff")[1], "fcfe")
}

#' Short cash-flow tag for DCF page labels: "FCFF" or "FCFE".
dcf_cf_tag <- function(claim) {
  if (dcf_claim_is_fcfe(claim)) "FCFE" else "FCFF"
}

#' Discount-rate tag for DCF page labels: "WACC" or "Ke".
dcf_disc_tag <- function(claim) {
  if (dcf_claim_is_fcfe(claim)) "Ke" else "WACC"
}

#' Full Chinese cash-flow label on the DCF page.
dcf_cf_full_zh <- function(claim) {
  if (dcf_claim_is_fcfe(claim)) "股權自由現金流 (FCFE)" else "企業自由現金流 (FCFF)"
}

#' Projection-tab formula banner (FCFF identity, or FCFE conversion + FCFF identity).
dcf_formula_banner_txt <- function(claim) {
  if (dcf_claim_is_fcfe(claim)) {
    "FCFE = FCFF − Interest×(1−T) + Net Borrowing　｜　FCFF = NOPAT + D&A − ΔNWC − CapEx"
  } else {
    "FCFF = NOPAT + D&A - ΔNWC - CapEx"
  }
}

dcf_hist_cf_label <- function(claim) {
  if (dcf_claim_is_fcfe(claim)) "歷史現金流 (FCF)" else "歷史現金流 (FCFF)"
}

dcf_fcst_cf_label <- function(claim) {
  sprintf("預測現金流 (%s)", dcf_cf_tag(claim))
}

dcf_yearly_pv_label <- function(claim) {
  sprintf("各年折現現金流 (PV／%s)", dcf_disc_tag(claim))
}

#' Convert FCFF path to FCFE with conversion components.
#' FCFE_t = FCFF_t − Interest(1−T) + Net borrowing_t.
#' Net borrowing grows starting debt with g_path (constant-leverage approximation).
fcff_to_fcfe_components <- function(fcff, interest_after_tax = 0, debt0 = 0, g_path = 0) {
  fcff <- suppressWarnings(as.numeric(fcff))
  n <- length(fcff)
  empty <- data.frame(
    FCFF = numeric(0), InterestAfterTax = numeric(0),
    NetBorrowing = numeric(0), BeginningDebt = numeric(0), FCFE = numeric(0),
    stringsAsFactors = FALSE
  )
  if (n < 1L) return(empty)
  iat <- suppressWarnings(as.numeric(interest_after_tax)[1])
  if (!is.finite(iat)) iat <- 0
  debt <- suppressWarnings(as.numeric(debt0)[1])
  if (!is.finite(debt) || debt < 0) debt <- 0
  gp <- suppressWarnings(as.numeric(g_path))
  if (length(gp) == 1L) gp <- rep(gp, n)
  if (length(gp) < n) gp <- c(gp, rep(tail(gp, 1), n - length(gp)))
  gp[!is.finite(gp)] <- 0
  nb <- numeric(n)
  beg <- numeric(n)
  out <- numeric(n)
  for (i in seq_len(n)) {
    beg[i] <- debt
    nb[i] <- gp[i] * debt
    cf <- fcff[i]
    out[i] <- if (is.finite(cf)) cf - iat + nb[i] else NA_real_
    debt <- debt + nb[i]
  }
  data.frame(
    FCFF = fcff,
    InterestAfterTax = rep(iat, n),
    NetBorrowing = nb,
    BeginningDebt = beg,
    FCFE = out,
    stringsAsFactors = FALSE
  )
}

#' Convert FCFF path to FCFE: FCFE_t = FCFF_t − Interest(1−T) + Net borrowing_t.
fcff_to_fcfe <- function(fcff, interest_after_tax = 0, debt0 = 0, g_path = 0) {
  fcff_to_fcfe_components(
    fcff, interest_after_tax = interest_after_tax, debt0 = debt0, g_path = g_path
  )$FCFE
}

#' Holding / conglomerate NAV from a balance sheet (book SOTP).
#' NAV = Equity − holdco_discount × identified investments.
#' If no investment lines, NAV = common equity (same as book).
extract_nav_components <- function(df_bs, holdco_discount = 0) {
  empty <- list(
    cash = NA_real_, investments = NA_real_, equity = NA_real_,
    nav = NA_real_, discount = 0, note = "無資產負債表"
  )
  if (is.null(df_bs) || !is.data.frame(df_bs) || nrow(df_bs) == 0) return(empty)
  disc <- suppressWarnings(as.numeric(holdco_discount)[1])
  if (!is.finite(disc)) disc <- 0
  disc <- max(0, min(0.5, disc))
  cash <- tryCatch(
    select_current_metric_any(
      df_bs,
      c(
        "Cash Cash Equivalents And Short Term Investments",
        "Cash And Cash Equivalents",
        "^Total Cash$"
      ),
      "stock"
    ),
    error = function(e) NA_real_
  )
  inv <- tryCatch(
    select_current_metric_any(
      df_bs,
      c(
        "Investmentin Financial Assets",
        "Investments And Advances",
        "Long Term Investments",
        "Available For Sale Securities",
        "Equity Method Investments"
      ),
      "stock"
    ),
    error = function(e) NA_real_
  )
  equity <- tryCatch(
    select_current_metric_any(df_bs, EQUITY_PATTERNS, "stock"),
    error = function(e) NA_real_
  )
  if (!is.finite(inv) || inv < 0) inv <- 0
  if (!is.finite(cash)) cash <- NA_real_
  if (!is.finite(equity) || equity <= 0) {
    return(list(
      cash = cash, investments = inv, equity = equity,
      nav = NA_real_, discount = disc,
      note = "無法取得股東權益，NAV 未計算"
    ))
  }
  nav <- equity - disc * inv
  note <- if (inv > 0 && disc > 0) {
    sprintf("NAV＝權益 − %.0f%%×投資科目（控股折價）", disc * 100)
  } else if (inv > 0) {
    "已辨識投資科目；折價=0 時 NAV＝帳面權益"
  } else {
    "無獨立投資科目，NAV＝帳面淨值"
  }
  list(cash = cash, investments = inv, equity = equity, nav = nav, discount = disc, note = note)
}

#' Canonical P/B fair price from formula parameters only (unit book).
#' P = basis × target P/B. Independent of ticker market price and share count.
.pb_formula_p <- function(basis = 1, pb) {
  basis <- suppressWarnings(as.numeric(basis)[1])
  pb <- suppressWarnings(as.numeric(pb)[1])
  if (!is.finite(basis) || basis <= 0 || !is.finite(pb) || pb <= 0) return(NA_real_)
  basis * pb
}

#' Sort elasticity table rows by |ε| descending.
.param_sensitivity_sort_by_abs_eps <- function(out) {
  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) return(out)
  if (!("｜ε｜" %in% names(out))) return(out)
  infl_num <- suppressWarnings(as.numeric(out$`｜ε｜`))
  out <- out[order(-infl_num, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# 從預測表統一取出 FCFF 序列（相容舊欄位名 FCF）
extract_fcff_series <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(numeric(0))
  if ("FCFF" %in% colnames(df)) return(as.numeric(df$FCFF))
  if ("FCF" %in% colnames(df)) return(as.numeric(df$FCF))
  rep(NA_real_, nrow(df))
}

# 依現金流口徑取出展示／折現序列：FCFF 原樣；FCFE 走既有 fcff_to_fcfe
extract_dcf_claim_series <- function(df, claim = "fcff",
                                     interest_after_tax = 0, debt0 = 0, g_path = 0) {
  fcff <- extract_fcff_series(df)
  if (!dcf_claim_is_fcfe(claim)) return(fcff)
  fcff_to_fcfe(
    fcff, interest_after_tax = interest_after_tax, debt0 = debt0, g_path = g_path
  )
}

# 依 DCF 模式決定各預測年的營收成長率 (%)
revenue_growth_pct_for_year <- function(year_idx, mode, g_est, g_stage1, g_stage2, yr_stage1) {
  g_est <- safe_num(g_est)
  g_stage1 <- safe_num(g_stage1)
  g_stage2 <- safe_num(g_stage2)
  yr_stage1 <- max(1L, as.integer(safe_num(yr_stage1)))
  if (identical(mode, "two_stage") && year_idx <= yr_stage1) return(g_stage1)
  if (identical(mode, "two_stage")) return(g_stage2)
  if (!is.null(g_est) && !is.na(g_est) && g_est != 0) return(g_est)
  g_stage2
}

# 確保第一階段年數有效：0 < yr_stage1 < n
clamp_yr_stage1 <- function(n_years, yr_stage1, default_yr = 3L) {
  n_years <- as.integer(safe_num(n_years))
  yr_stage1 <- as.integer(safe_num(yr_stage1))
  if (n_years <= 1) return(1L)
  if (is.na(yr_stage1) || yr_stage1 <= 0 || yr_stage1 >= n_years) {
    return(max(1L, min(as.integer(default_yr), n_years - 1L)))
  }
  yr_stage1
}

# =========================================================
# 📄 投資意見報告書輔助函數（券商研究報告格式）
# =========================================================

# 蒐集財報警訊／體質警訊（與 YNOW 分頁 Schilit 自動判讀同一套；只列「警示」）
collect_fraud_warnings <- function(d_cf, d_is, d_bs, industry_key = NULL) {
  if (!exists("evaluate_shenanigans", mode = "function")) {
    return(character(0))
  }
  ev <- tryCatch(
    evaluate_shenanigans(d_is, d_bs, d_cf, industry_key = industry_key),
    error = function(e) NULL
  )
  if (is.null(ev) || !isTRUE(ev$ok) || is.null(ev$items) || nrow(ev$items) < 1L) {
    return(character(0))
  }
  al <- ev$items[ev$items$status == "警示", , drop = FALSE]
  if (nrow(al) < 1L) return(character(0))
  paste0(al$code, " ", al$name, "：", al$reason)
}

# Piotroski F-Score（與決策看板 checklist 一致；供 PDF 報告）
compute_report_f_score <- function(d_is, d_bs, d_cf) {
  empty <- list(
    total = NA_real_,
    quality_flag = NA_real_,
    checklist = data.frame(
      `檢驗維度` = character(0),
      `得分` = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  if (is.null(d_is) || is.null(d_bs) || is.null(d_cf) ||
      !is.data.frame(d_is) || !is.data.frame(d_bs) || !is.data.frame(d_cf) ||
      nrow(d_is) == 0 || nrow(d_bs) == 0 || nrow(d_cf) == 0) {
    return(empty)
  }
  get_row2 <- function(df, label) {
    res <- select_clean_metric_row(df, label, include_ttm = FALSE)
    if (length(res) < 2) return(c(NA_real_, NA_real_))
    suppressWarnings(as.numeric(res[1:2]))
  }
  tryCatch({
    net_inc  <- get_row2(d_is, "Net Income Common Stockholders|Net Income$")
    revenue  <- get_row2(d_is, "Total Revenue")
    gp       <- get_row2(d_is, "Gross Profit")
    assets   <- get_row2(d_bs, "Total Assets")
    lt_debt  <- get_row2(d_bs, "Long Term Debt|Total Non Current Liabilities")
    cur_ast  <- get_row2(d_bs, "Total Current Assets")
    cur_liab <- get_row2(d_bs, "Total Current Liabilities")
    shares   <- get_row2(d_bs, "Ordinary Shares Number")
    ocf      <- get_row2(d_cf, "Operating Cash Flow")
    oe_row   <- get_operating_earnings_row(d_is, include_ttm = FALSE)
    op_earn  <- {
      v <- suppressWarnings(as.numeric(oe_row))
      if (length(v) < 1L || all(is.na(v))) {
        c(NA_real_, NA_real_)
      } else {
        c(v[1], if (length(v) >= 2L) v[2] else NA_real_)
      }
    }

    p1 <- ifelse(!is.na(net_inc[1]) && !is.na(assets[1]) && assets[1] != 0 && (net_inc[1] / assets[1]) > 0, 1, 0)
    p2 <- ifelse(!is.na(ocf[1]) && ocf[1] > 0, 1, 0)
    p3 <- ifelse(all(!is.na(net_inc[1:2]), !is.na(assets[1:2])) &&
                   assets[1] != 0 && assets[2] != 0 &&
                   (net_inc[1] / assets[1]) > (net_inc[2] / assets[2]), 1, 0)
    # 盈餘品質：OCF > 營運利潤（淨利 − 投資證券未實現損益）
    p4 <- ifelse(!is.na(ocf[1]) && !is.na(op_earn[1]) && ocf[1] > op_earn[1], 1, 0)
    p5 <- ifelse(all(!is.na(lt_debt[1:2]), !is.na(assets[1:2])) &&
                   assets[1] != 0 && assets[2] != 0 &&
                   (lt_debt[1] / assets[1]) <= (lt_debt[2] / assets[2]), 1, 0)
    p6 <- ifelse(all(!is.na(cur_ast[1:2]), !is.na(cur_liab[1:2])) &&
                   cur_liab[1] != 0 && cur_liab[2] != 0 &&
                   (cur_ast[1] / cur_liab[1]) > (cur_ast[2] / cur_liab[2]), 1, 0)
    p7 <- ifelse(!is.na(shares[1]) && !is.na(shares[2]) && shares[1] <= (shares[2] * 1.02), 1, 0)
    p8 <- ifelse(all(!is.na(gp[1:2]), !is.na(revenue[1:2])) &&
                   revenue[1] != 0 && revenue[2] != 0 &&
                   (gp[1] / revenue[1]) > (gp[2] / revenue[2]), 1, 0)
    p9 <- ifelse(all(!is.na(revenue[1:2]), !is.na(assets[1:2])) &&
                   assets[1] != 0 && assets[2] != 0 &&
                   (revenue[1] / assets[1]) > (revenue[2] / assets[2]), 1, 0)
    scores <- c(p1, p2, p3, p4, p5, p6, p7, p8, p9)
    list(
      total = sum(scores),
      quality_flag = p4,
      checklist = data.frame(
        `檢驗維度` = c(
          "獲利性 (ROA > 0)", "獲利性 (OCF > 0)", "獲利性 (ROA 成長)",
          "獲利性 (盈餘品質：OCF > 營業利益)",
          "安全性 (槓桿下降)", "安全性 (流動比提升)", "安全性 (未大幅增資)",
          "效率 (毛利率提升)", "效率 (資產週轉率提升)"
        ),
        `得分` = ifelse(scores == 1, "通過", "未達標"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }, error = function(e) empty)
}

# 建立 KPI 摘要表（對應研究報告「關鍵財務指標」區塊）
build_report_kpi_df <- function(d_is, d_bs, d_cf) {
  pct <- function(x) if (is.na(x)) "N/A" else paste0(sprintf("%.1f", x), "%")
  num <- function(x) if (is.na(x)) "N/A" else format_dollar_abbr(x)
  
  gp <- get_avg(select_clean_metric_row(d_is, "Gross Profit", include_ttm = FALSE))
  rev <- get_avg(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
  net <- get_avg(select_clean_metric_row_any(d_is, NET_INCOME_PATTERNS, include_ttm = FALSE))
  ocf <- get_avg(select_clean_metric_row(d_cf, "Operating Cash Flow", include_ttm = FALSE))
  fcf <- get_avg(select_clean_metric_row(d_cf, "Free Cash Flow", include_ttm = FALSE))
  assets <- get_avg(select_clean_metric_row(d_bs, "Total Assets", include_ttm = FALSE))
  equity <- get_avg(select_clean_metric_row_any(d_bs, EQUITY_PATTERNS, include_ttm = FALSE))
  
  rev_g <- get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
  roa <- if (!is.na(net) && !is.na(assets) && assets != 0) net / assets * 100 else NA
  roe <- if (!is.na(net) && !is.na(equity) && equity != 0) net / equity * 100 else NA
  fcf_margin <- if (!is.na(fcf) && !is.na(rev) && rev != 0) fcf / rev * 100 else NA
  
  data.frame(
    指標 = c("毛利率", "淨利率", "營收成長率 (年均)", "ROA", "ROE", "FCF 利潤率", "營業現金流 (均)", "自由現金流 (均)"),
  數值 = c(
      pct(if (!is.na(gp) && !is.na(rev) && rev != 0) gp / rev * 100 else NA),
      pct(if (!is.na(net) && !is.na(rev) && rev != 0) net / rev * 100 else NA),
      pct(rev_g),
      pct(roa), pct(roe), pct(fcf_margin),
      num(ocf), num(fcf)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# 投資評等（對應券商 Buy / Hold / Reduce 慣例）
derive_investment_rating <- function(current_price, target_price) {
  cur <- suppressWarnings(as.numeric(current_price))
  tgt <- suppressWarnings(as.numeric(target_price))
  if (length(cur) != 1 || length(tgt) != 1 || is.na(cur) || is.na(tgt) || tgt <= 0) {
    return(list(
      rating = "待評估", rating_en = "NR",
      rating_color = "#6c757d", upside_pct = NA, margin_of_safety = NA
    ))
  }
  upside <- (tgt - cur) / cur * 100
  mos <- (tgt - cur) / tgt * 100
  if (upside >= 15) {
    list(rating = "買進", rating_en = "Buy", rating_color = "#198754", upside_pct = upside, margin_of_safety = mos)
  } else if (upside <= -10) {
    list(rating = "減持", rating_en = "Reduce", rating_color = "#dc3545", upside_pct = upside, margin_of_safety = mos)
  } else {
    list(rating = "持有", rating_en = "Hold", rating_color = "#fd7e14", upside_pct = upside, margin_of_safety = mos)
  }
}

# 推薦估值方法（v13：分類 → 主／副模型）
derive_valuation_method <- function(d_cf, industry_text = "", d_is = NULL, d_bs = NULL,
                                    industry_choice = NULL) {
  rec <- recommend_valuation_models(
    d_cf, industry_text, d_is = d_is, d_bs = d_bs, industry_choice = industry_choice
  )
  list(method = rec$summary_method, rationale = rec$reason,
       primary = rec$primary, secondary = rec$secondary, company_type = rec$company_type)
}

#' 模型選擇器（v13）
#' @return list with company_type, primary, secondary, ddm/dcf/pb/ri flags,
#'   tags, summary_method, reason, suggest_two_stage, confidence_inputs
recommend_valuation_models <- function(d_cf, industry_text = "", d_is = NULL, d_bs = NULL,
                                       industry_choice = NULL) {
  empty <- list(
    company_type = "fallback",
    primary = "pb",
    secondary = NULL,
    ddm = FALSE, dcf = FALSE, pb = TRUE, ri = FALSE,
    tags = "pb",
    summary_method = "P/B／相對估值",
    reason = "資料不足，暫以 P/B 定位。",
    suggest_two_stage = FALSE,
    confidence_inputs = list(
      fcf_cv = NA_real_, div_cv = NA_real_,
      has_fcf = FALSE, has_div = FALSE, has_roe = FALSE,
      data_complete = FALSE
    )
  )
  if (is.null(d_cf) || !is.data.frame(d_cf) || nrow(d_cf) == 0) return(empty)

  fcf_seq <- tryCatch(
    select_clean_metric_row(d_cf, "Free Cash Flow", include_ttm = FALSE),
    error = function(e) NULL
  )
  div_seq <- tryCatch(
    select_clean_metric_row(d_cf, "Cash Dividends Paid", include_ttm = FALSE),
    error = function(e) NULL
  )
  is_fcf_pos <- length(fcf_seq) > 0 && !all(is.na(fcf_seq)) &&
    isTRUE(mean(fcf_seq, na.rm = TRUE) > 0)
  fcf_vals <- suppressWarnings(as.numeric(na.omit(fcf_seq)))
  fcf_cv <- if (length(fcf_vals) >= 2) {
    stats::sd(fcf_vals) / max(abs(mean(fcf_vals)), 1e-9)
  } else {
    NA_real_
  }
  is_fcf_stable <- isTRUE(is_fcf_pos) && (is.na(fcf_cv) || fcf_cv <= 0.75)

  is_div <- length(div_seq) > 0 && !all(is.na(div_seq)) &&
    isTRUE(mean(abs(div_seq), na.rm = TRUE) > 0)
  div_vals <- abs(suppressWarnings(as.numeric(na.omit(div_seq))))
  div_cv <- if (length(div_vals) >= 2 && mean(div_vals) > 0) {
    stats::sd(div_vals) / max(abs(mean(div_vals)), 1e-9)
  } else {
    NA_real_
  }
  is_div_stable <- isTRUE(is_div) && (is.na(div_cv) || div_cv <= 0.50)

  ind_txt <- as.character(industry_text %||% "")
  ind_key <- as.character(industry_choice %||% "")
  is_financial <- grepl(
    "Bank|Insurance|Financial|Conglomerate|fn\\.|Insurance Brokers",
    ind_txt,
    ignore.case = TRUE
  ) || grepl("^fn\\.", ind_key)
  is_holding <- grepl(
    "Conglomerate|Holding|Berkshire|fn\\.Conglomerate",
    paste(ind_txt, ind_key),
    ignore.case = TRUE
  ) || grepl("^fn\\.Conglomerate", ind_key)
  asset_or_book_driven <- isTRUE(is_financial) || grepl(
    "REIT|Real Estate|Asset|Bank|Insurance|Utility|Utilities",
    ind_txt,
    ignore.case = TRUE
  )

  rev_g <- tryCatch(
    get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE)),
    error = function(e) NA_real_
  )
  ni <- tryCatch(select_current_metric_any(d_is, NET_INCOME_PATTERNS, "flow"), error = function(e) NA_real_)
  equity <- tryCatch(select_current_metric_any(d_bs, EQUITY_PATTERNS, "stock"), error = function(e) NA_real_)
  roe <- if (!is.na(ni) && !is.na(equity) && equity > 0) ni / equity * 100 else NA_real_

  conf_in <- list(
    fcf_cv = fcf_cv,
    div_cv = div_cv,
    has_fcf = isTRUE(is_fcf_pos),
    has_div = isTRUE(is_div),
    has_roe = isTRUE(is.finite(roe)),
    data_complete = isTRUE(!is.null(d_is) && !is.null(d_bs) && isTRUE(is_fcf_pos || is_div))
  )

  .pack <- function(company_type, primary, secondary, summary_method, reason,
                    suggest_two_stage = FALSE) {
    flags <- list(ddm = FALSE, dcf = FALSE, pb = FALSE, ri = FALSE)
    flags[[primary]] <- TRUE
    if (!is.null(secondary) && nzchar(as.character(secondary))) {
      flags[[as.character(secondary)]] <- TRUE
    }
    tags <- unique(c(primary, if (!is.null(secondary)) as.character(secondary)))
    tags <- tags[nzchar(tags)]
    list(
      company_type = company_type,
      primary = primary,
      secondary = secondary,
      ddm = isTRUE(flags$ddm),
      dcf = isTRUE(flags$dcf),
      pb = isTRUE(flags$pb),
      ri = isTRUE(flags$ri),
      tags = tags,
      summary_method = summary_method,
      reason = reason,
      suggest_two_stage = isTRUE(suggest_two_stage),
      confidence_inputs = conf_in
    )
  }

  # 1) Holding / conglomerate — P/B + NAV build-up (+ RI)
  if (isTRUE(is_holding)) {
    sec <- if (isTRUE(is.finite(roe) && roe > 0)) "ri" else NULL
    return(.pack(
      "holding_asset", "pb", sec,
      "P/B＋NAV 拆解（控股／綜合）",
      "控股／綜合企業以淨資產為錨：主模型 P/B，並用現金／投資科目做 NAV 拆解（可設控股折價）；ROE>0 時以 RI 交叉驗證。"
    ))
  }

  # 2) Financial / book-driven
  if (isTRUE(asset_or_book_driven) || isTRUE(is_financial)) {
    sec <- if (isTRUE(is.finite(roe) && roe > 0)) {
      "ri"
    } else if (isTRUE(is_div_stable)) {
      "ddm"
    } else {
      NULL
    }
    return(.pack(
      "financial", "pb", sec,
      "P/B（本淨比／資產法）",
      "金融／保險／公用／資產驅動：資本與淨值為尺；主模型 P/B，副模型以 RI（ROE>0）或穩定配息時的 DDM 交叉驗證。"
    ))
  }

  # 3) Growth — two-stage DCF primary
  is_growth <- (is.finite(rev_g) && rev_g > 12 && isTRUE(is_fcf_pos)) ||
    (is.finite(rev_g) && rev_g > 15) ||
    (isTRUE(is_fcf_pos) && !isTRUE(is_fcf_stable) && is.finite(rev_g) && rev_g > 8)
  if (isTRUE(is_growth)) {
    sec <- if (isTRUE(is.finite(roe) && roe > 8)) "ri" else "pb"
    return(.pack(
      "growth", "dcf", sec,
      "DCF（Two-Stage）",
      "高成長或 FCF 仍波動：主模型兩階段 DCF 反映成長收斂；副模型以 RI／P/B 交叉驗證。",
      suggest_two_stage = TRUE
    ))
  }

  # 4) Div-stable, weak FCF → DDM primary
  if (isTRUE(is_div_stable) && !isTRUE(is_fcf_stable)) {
    return(.pack(
      "mature", "ddm", "pb",
      "DDM（股利折現）",
      "股利穩定度高於自由現金流；主模型 DDM，副模型 P/B 作資產底線。"
    ))
  }

  # 5) Mature + stable FCF → DCF primary
  if (isTRUE(is_fcf_stable)) {
    sec <- if (isTRUE(is_div_stable)) "ddm" else if (isTRUE(is.finite(roe) && roe > 8)) "ri" else "pb"
    return(.pack(
      "mature", "dcf", sec,
      if (identical(sec, "ddm")) "DCF + DDM 交叉驗證" else "DCF（自由現金流折現）",
      "自由現金流為正且波動可控：主模型 DCF；副模型依配息／ROE 選 DDM 或 RI。"
    ))
  }

  # 6) Fallback
  .pack(
    "fallback", "pb", if (isTRUE(is.finite(roe) && roe > 0)) "ri" else NULL,
    "P/B／相對估值",
    "配息與 FCF 穩定性不足，折現輸入可信度偏低；先以 P/B 定位，ROE 為正時以 RI 檢查超額報酬。"
  )
}

#' Justified / industry / history P/B targets (v13)
#' @param roe_pct ROE in percent (e.g. 15)
#' @param ke_pct Cost of equity in percent
#' @param g_pct Perpetual growth in percent
#' @param industry_band list(low, mid, high) or numeric length 2–3
#' @param hist_pb numeric vector of trailing P/B observations (optional)
derive_pb_targets <- function(roe_pct = NA_real_, ke_pct = NA_real_, g_pct = NA_real_,
                              industry_band = NULL, hist_pb = NULL,
                              clamp = c(0.3, 6)) {
  lo_c <- clamp[1]; hi_c <- clamp[2]
  .clamp <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x)) return(NA_real_)
    max(lo_c, min(hi_c, x))
  }

  # Industry prior
  ind_lo <- ind_mid <- ind_hi <- NA_real_
  if (!is.null(industry_band)) {
    if (is.list(industry_band)) {
      ind_lo <- suppressWarnings(as.numeric(industry_band$low %||% industry_band[[1]])[1])
      ind_hi <- suppressWarnings(as.numeric(industry_band$high %||% industry_band[[2]])[1])
      ind_mid <- suppressWarnings(as.numeric(
        industry_band$mid %||% (if (length(industry_band) >= 3) industry_band[[3]] else mean(c(ind_lo, ind_hi)))
      )[1])
    } else {
      b <- suppressWarnings(as.numeric(industry_band))
      if (length(b) >= 2) {
        ind_lo <- b[1]; ind_hi <- b[2]
        ind_mid <- if (length(b) >= 3) b[3] else mean(c(ind_lo, ind_hi))
      }
    }
  }

  # Justified P/B ≈ (ROE − g) / (Ke − g)  [levels, not percent]
  just <- NA_real_
  roe <- suppressWarnings(as.numeric(roe_pct)[1]) / 100
  ke  <- suppressWarnings(as.numeric(ke_pct)[1]) / 100
  g   <- suppressWarnings(as.numeric(g_pct)[1]) / 100
  if (is.finite(roe) && is.finite(ke) && is.finite(g) && ke > g) {
    just <- .clamp((roe - g) / (ke - g))
  }

  # History percentiles
  hist_lo <- hist_mid <- hist_hi <- NA_real_
  hp <- suppressWarnings(as.numeric(hist_pb))
  hp <- hp[is.finite(hp) & hp > 0]
  if (length(hp) >= 4) {
    qs <- stats::quantile(hp, probs = c(0.25, 0.50, 0.75), names = FALSE, na.rm = TRUE)
    hist_lo <- .clamp(qs[1]); hist_mid <- .clamp(qs[2]); hist_hi <- .clamp(qs[3])
  }

  sources <- list()
  if (is.finite(just)) sources$justified <- just
  if (is.finite(ind_mid)) sources$industry <- ind_mid
  if (is.finite(hist_mid)) sources$history <- hist_mid

  # Weights: justified 0.45, industry 0.35, history 0.20 (renormalize if missing)
  wmap <- c(justified = 0.45, industry = 0.35, history = 0.20)
  present <- names(sources)
  if (!length(present)) {
    # hard fallback
    base <- if (is.finite(ind_mid)) ind_mid else 1.4
    return(list(
      low = .clamp(if (is.finite(ind_lo)) ind_lo else base * 0.75),
      mid = .clamp(base),
      high = .clamp(if (is.finite(ind_hi)) ind_hi else base * 1.25),
      justified = just, industry_low = ind_lo, industry_mid = ind_mid, industry_high = ind_hi,
      history_low = hist_lo, history_mid = hist_mid, history_high = hist_hi,
      weights_used = character(0),
      source_note = "無可用 Justified／產業／歷史來源，退回通用區間"
    ))
  }
  ww <- wmap[present]
  ww <- ww / sum(ww)
  base <- sum(unlist(sources) * ww)

  lows <- c(just, ind_lo, hist_lo)
  highs <- c(just, ind_hi, hist_hi)
  lows <- lows[is.finite(lows)]
  highs <- highs[is.finite(highs)]
  bear <- if (length(lows)) min(lows) else base * 0.8
  bull <- if (length(highs)) max(highs) else base * 1.2
  # ensure order
  bear <- min(bear, base); bull <- max(bull, base)

  list(
    low = .clamp(bear),
    mid = .clamp(base),
    high = .clamp(bull),
    justified = just,
    industry_low = ind_lo, industry_mid = ind_mid, industry_high = ind_hi,
    history_low = hist_lo, history_mid = hist_mid, history_high = hist_hi,
    weights_used = paste(sprintf("%s=%.0f%%", names(ww), ww * 100), collapse = ", "),
    source_note = paste0("來源權重：", paste(sprintf("%s=%.0f%%", names(ww), ww * 100), collapse = ", "))
  )
}

#' Valuation confidence label from fundamentals quality signals (v13)
#' @return list(level = "低"|"中"|"高", score = 0–100, reasons = character)
score_valuation_confidence <- function(confidence_inputs = list(),
                                       f_score = NA_real_,
                                       primary_base = NA_real_,
                                       secondary_point = NA_real_,
                                       tv_weight = NA_real_) {
  score <- 40
  reasons <- character(0)
  ci <- confidence_inputs %||% list()

  if (isTRUE(ci$data_complete)) {
    score <- score + 15
    reasons <- c(reasons, "財報輸入完整")
  } else {
    reasons <- c(reasons, "財報輸入不完整")
  }
  if (isTRUE(ci$has_fcf)) score <- score + 8
  if (isTRUE(ci$has_div)) score <- score + 5
  if (isTRUE(ci$has_roe)) score <- score + 5

  fcf_cv <- suppressWarnings(as.numeric(ci$fcf_cv)[1])
  if (is.finite(fcf_cv)) {
    if (fcf_cv <= 0.5) {
      score <- score + 12
      reasons <- c(reasons, "FCF 波動可控")
    } else if (fcf_cv > 1) {
      score <- score - 10
      reasons <- c(reasons, "FCF 波動偏高")
    }
  }

  fs <- suppressWarnings(as.numeric(f_score)[1])
  if (is.finite(fs)) {
    if (fs >= 7) {
      score <- score + 12
      reasons <- c(reasons, "F-Score 偏強")
    } else if (fs < 4) {
      score <- score - 12
      reasons <- c(reasons, "F-Score 偏弱")
    }
  }

  pb <- suppressWarnings(as.numeric(primary_base)[1])
  sp <- suppressWarnings(as.numeric(secondary_point)[1])
  if (is.finite(pb) && is.finite(sp) && pb > 0) {
    gap <- abs(sp - pb) / pb
    if (gap <= 0.15) {
      score <- score + 10
      reasons <- c(reasons, "主副模型接近")
    } else if (gap > 0.35) {
      score <- score - 8
      reasons <- c(reasons, "主副模型分歧大")
    }
  }

  tvw <- suppressWarnings(as.numeric(tv_weight)[1])
  if (is.finite(tvw) && tvw > 0.75) {
    score <- score - 10
    reasons <- c(reasons, "終值權重偏高")
  }

  score <- max(0, min(100, round(score)))
  level <- if (score >= 70) "高" else if (score >= 45) "中" else "低"
  list(level = level, score = score, reasons = reasons)
}

#' Map model key to display label
.model_label <- function(key) {
  switch(
    as.character(key %||% ""),
    "dcf" = "DCF",
    "ddm" = "DDM",
    "pb" = "P/B",
    "ri" = "RI",
    as.character(key %||% "N/A")
  )
}

#' Build Bear/Base/Bull scalars for discount models (documented small stresses)
scenario_stress_factors <- function(model = c("dcf", "ddm", "ri")) {
  model <- match.arg(model)
  # deltas in percentage points for rates; near-term growth haircut as multiplier
  switch(
    model,
    dcf = list(
      bear = list(wacc_pp = +1.0, g_pp = -0.5, near_g_mult = 0.85),
      base = list(wacc_pp = 0, g_pp = 0, near_g_mult = 1),
      bull = list(wacc_pp = -1.0, g_pp = +0.5, near_g_mult = 1.10)
    ),
    ddm = list(
      bear = list(ke_pp = +1.0, g_pp = -0.5),
      base = list(ke_pp = 0, g_pp = 0),
      bull = list(ke_pp = -1.0, g_pp = +0.5)
    ),
    ri = list(
      bear = list(ke_pp = +1.0, g_pp = -0.5, roe_pp = -2),
      base = list(ke_pp = 0, g_pp = 0, roe_pp = 0),
      bull = list(ke_pp = -1.0, g_pp = +0.5, roe_pp = +2)
    )
  )
}

# ==========================================
# 永續成長率 g 估計方法（DDM / DCF / RI 共用）
# ==========================================
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

MATURE_TECH_TICKERS <- c(
  "AAPL", "MSFT", "GOOGL", "GOOG", "AMZN", "META", "NVDA", "TSLA",
  "AVGO", "ORCL", "CRM", "ADBE", "INTC", "AMD", "QCOM", "TXN", "TSM"
)

#' 依產業／營收成長自動分類生命週期檔位
#' @return one of mature_sunset | mature_tech | growth_to_mature | mature_general
classify_lifecycle_stage <- function(industry_text = "", ticker = "", rev_cagr = NA_real_) {
  txt <- paste(industry_text %||% "", collapse = " ")
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))

  if (grepl(
    "Bank|Insurance|Utility|Utilities|Financial|Conglomerate|fn\\.|Insurance Brokers|Gas Utilities|Electric Utilities",
    txt, ignore.case = TRUE
  )) {
    return("mature_sunset")
  }

  if (nzchar(tk) && tk %in% MATURE_TECH_TICKERS) {
    return("mature_tech")
  }
  if (grepl(
    "Software|Internet|Semiconductors|Semiconductor|Consumer Electronics|Information Technology| technolo",
    txt, ignore.case = TRUE
  )) {
    return("mature_tech")
  }

  if (is.finite(rev_cagr) && rev_cagr > 8) {
    return("growth_to_mature")
  }
  "mature_general"
}

#' 基本面永續 g（%）= Retention Ratio × ROE
calc_fundamental_sgr_pct <- function(d_is, d_bs, d_cf) {
  if (is.null(d_is) || is.null(d_bs) || !is.data.frame(d_is) || !is.data.frame(d_bs)) {
    return(NA_real_)
  }
  ni <- tryCatch(
    select_current_metric_any(d_is, NET_INCOME_PATTERNS, "flow"),
    error = function(e) NA_real_
  )
  equity <- tryCatch(
    select_current_metric_any(d_bs, EQUITY_PATTERNS, "stock"),
    error = function(e) NA_real_
  )
  if (is.na(ni) || is.na(equity) || equity <= 0) return(NA_real_)

  roe <- ni / equity
  div_paid <- tryCatch({
    if (is.null(d_cf) || !is.data.frame(d_cf)) NA_real_ else {
      abs(select_current_metric(d_cf, "Cash Dividends Paid", "flow"))
    }
  }, error = function(e) NA_real_)

  payout_ratio <- 0
  if (!is.na(ni) && ni > 0 && !is.na(div_paid)) {
    payout_ratio <- min(max(div_paid / ni, 0), 1)
  }
  retention <- 1 - payout_ratio
  round(roe * retention * 100, 2)
}

#' 建議永續成長率估計法（不改寫演算法數值；僅推薦 macro／fundamental／lifecycle）
#' @return list(method, label, reason, fund_sgr_pct, auto_lifecycle)
recommend_perpetual_g_method <- function(rf_pct = NA_real_,
                                         d_is = NULL,
                                         d_bs = NULL,
                                         d_cf = NULL,
                                         industry_text = "",
                                         ticker = "",
                                         wacc_pct = NA_real_,
                                         rev_cagr = NA_real_) {
  rf_pct <- suppressWarnings(as.numeric(rf_pct)[1])
  wacc_pct <- suppressWarnings(as.numeric(wacc_pct)[1])
  if (is.na(rev_cagr) || !is.finite(rev_cagr)) {
    rev_cagr <- tryCatch({
      get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
    }, error = function(e) NA_real_)
  }
  auto_stage <- classify_lifecycle_stage(industry_text, ticker, rev_cagr)
  fund_sgr <- calc_fundamental_sgr_pct(d_is, d_bs, d_cf)

  label_of <- function(m) {
    switch(
      as.character(m)[1],
      "macro" = "總體經濟錨定（Macro）",
      "fundamental" = "基本面公式（Fundamental／SGR）",
      "lifecycle" = "產業生命週期（Lifecycle）",
      as.character(m)[1]
    )
  }

  # 無法算 Retention×ROE → Macro
  if (!is.finite(fund_sgr)) {
    return(list(
      method = "macro",
      label = label_of("macro"),
      reason = "財報不足以計算 Retention×ROE，建議改用 Macro（錨定 Rf）。",
      fund_sgr_pct = NA_real_,
      auto_lifecycle = auto_stage
    ))
  }

  # 成熟科技／高速成長：Fundamental SGR（≈ROE）常偏高，終值宜用 Lifecycle
  if (identical(auto_stage, "mature_tech") && is.finite(fund_sgr) && fund_sgr > 6) {
    return(list(
      method = "lifecycle",
      label = label_of("lifecycle"),
      reason = paste0(
        "目前 Fundamental／SGR≈", round(fund_sgr, 2),
        "%（Retention×ROE），對成熟科技終值偏高；建議改用 Lifecycle（成熟科技約 2.5–3%）。演算法本身未改寫。"
      ),
      fund_sgr_pct = fund_sgr,
      auto_lifecycle = auto_stage
    ))
  }
  if (identical(auto_stage, "growth_to_mature")) {
    return(list(
      method = "lifecycle",
      label = label_of("lifecycle"),
      reason = paste0(
        "營收仍偏高成長（自動分類 growth_to_mature）",
        if (is.finite(rev_cagr)) paste0("，營收 CAGR≈", round(rev_cagr, 1), "%") else "",
        "；終值建議 Lifecycle（≈2.5%），並可考慮 Two-Stage。演算法本身未改寫。"
      ),
      fund_sgr_pct = fund_sgr,
      auto_lifecycle = auto_stage
    ))
  }
  if (identical(auto_stage, "mature_sunset")) {
    return(list(
      method = "lifecycle",
      label = label_of("lifecycle"),
      reason = "金融／公用等高度成熟產業，終值建議 Lifecycle（夕陽檔≈1.5–2%）。演算法本身未改寫。",
      fund_sgr_pct = fund_sgr,
      auto_lifecycle = auto_stage
    ))
  }

  # Fundamental SGR 已逼近／超過 WACC → 建議 Lifecycle 或 Macro，避免終值失控
  if (is.finite(wacc_pct) && is.finite(fund_sgr) && fund_sgr >= (wacc_pct - 1)) {
    return(list(
      method = "lifecycle",
      label = label_of("lifecycle"),
      reason = paste0(
        "Fundamental／SGR≈", round(fund_sgr, 2), "% 接近或高於 WACC（",
        round(wacc_pct, 2), "%）；建議改用 Lifecycle 或 Macro，Fundamental SGR 作為終值 g 易使 TV 失控。"
      ),
      fund_sgr_pct = fund_sgr,
      auto_lifecycle = auto_stage
    ))
  }

  # 一般成熟、SGR 合理 → Fundamental
  list(
    method = "fundamental",
    label = label_of("fundamental"),
    reason = paste0(
      "財報可算 Retention×ROE≈", round(fund_sgr, 2),
      "%，且生命週期非科技巨頭／高速成長；建議維持 Fundamental／SGR。"
    ),
    fund_sgr_pct = fund_sgr,
    auto_lifecycle = auto_stage
  )
}

#' 估計永續成長率（%）並附說明；必要時建議 two-stage
#' @return list(g_pct, reason, lifecycle_stage, suggest_two_stage, g_stage1_pct, auto_lifecycle,
#'   recommended_method, recommend_label, recommend_reason)
estimate_perpetual_g <- function(method = "macro",
                                 rf_pct = NA_real_,
                                 d_is = NULL,
                                 d_bs = NULL,
                                 d_cf = NULL,
                                 industry_text = "",
                                 ticker = "",
                                 lifecycle_stage = "auto",
                                 wacc_pct = NA_real_,
                                 rev_cagr = NA_real_) {
  method <- as.character(method %||% "macro")[1]
  rf_pct <- suppressWarnings(as.numeric(rf_pct)[1])
  wacc_pct <- suppressWarnings(as.numeric(wacc_pct)[1])
  if (is.na(rev_cagr) || !is.finite(rev_cagr)) {
    rev_cagr <- tryCatch({
      get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
    }, error = function(e) NA_real_)
  }

  auto_stage <- classify_lifecycle_stage(industry_text, ticker, rev_cagr)
  stage <- as.character(lifecycle_stage %||% "auto")[1]
  if (!nzchar(stage) || identical(stage, "auto")) stage <- auto_stage

  g_pct <- NA_real_
  reason <- ""
  suggest_two_stage <- FALSE
  g_stage1_pct <- if (is.finite(rev_cagr)) {
    round(max(2, min(rev_cagr, 15)), 2)
  } else {
    NA_real_
  }

  if (identical(method, "fundamental")) {
    g_pct <- calc_fundamental_sgr_pct(d_is, d_bs, d_cf)
    if (is.na(g_pct) || !is.finite(g_pct)) {
      g_pct <- if (is.finite(rf_pct)) round(rf_pct, 2) else 3
      reason <- paste0(
        "Fundamental／SGR：財報不足以計算 Retention×ROE，已回退 Macro（Rf=",
        g_pct, "%）。僅適合成熟、財務結構穩定企業。"
      )
    } else {
      reason <- paste0(
        "Fundamental／SGR：g = Retention Ratio × ROE = ", g_pct,
        "%。應用限制：僅適合成熟、財務結構穩定企業。"
      )
    }
  } else if (identical(method, "lifecycle")) {
    if (identical(stage, "mature_sunset")) {
      g_pct <- 1.75
      reason <- "Lifecycle：夕陽／高度成熟（金融、公用事業等）→ g≈1.75%（通膨附近 1.5–2%）。"
    } else if (identical(stage, "mature_tech")) {
      g_pct <- 2.75
      reason <- "Lifecycle：成熟科技巨頭 → g≈2.75%（長期上限約 2.5–3%）。"
    } else if (identical(stage, "growth_to_mature")) {
      g_pct <- 2.5
      suggest_two_stage <- TRUE
      reason <- paste0(
        "Lifecycle：高速成長轉向成熟 → 終值 g≈2.5%；建議 two-stage，",
        "前段成長向 2–3% 收斂",
        if (is.finite(g_stage1_pct)) paste0("（g1≈", g_stage1_pct, "%）") else "",
        "。"
      )
    } else {
      g_pct <- 2.5
      reason <- "Lifecycle：一般成熟產業 → g≈2.5%。"
    }
    reason <- paste0(reason, " 自動分類=", auto_stage, "；目前採用=", stage, "。")
  } else {
    # macro（預設）：直接套用美國國債利率 Rf
    g_pct <- if (is.finite(rf_pct)) round(rf_pct, 2) else 4
    reason <- paste0("Macroeconomic Anchoring：直接套用美國 10 年期公債殖利率 Rf=", g_pct, "%。")
  }

  if (is.finite(wacc_pct) && is.finite(g_pct) && g_pct >= wacc_pct) {
    reason <- paste0(
      reason, " ⚠ g≥WACC（", round(g_pct, 2), "% ≥ ", round(wacc_pct, 2),
      "%），Gordon 終值分母≤0，DCF/RI 將無法計算。"
    )
  }

  rec <- recommend_perpetual_g_method(
    rf_pct = rf_pct,
    d_is = d_is,
    d_bs = d_bs,
    d_cf = d_cf,
    industry_text = industry_text,
    ticker = ticker,
    wacc_pct = wacc_pct,
    rev_cagr = rev_cagr
  )

  list(
    g_pct = g_pct,
    reason = reason,
    lifecycle_stage = stage,
    auto_lifecycle = auto_stage,
    suggest_two_stage = isTRUE(suggest_two_stage),
    g_stage1_pct = g_stage1_pct,
    recommended_method = rec$method,
    recommend_label = rec$label,
    recommend_reason = rec$reason
  )
}

#' 側邊欄 menuItem 徽章：推薦優先，否則保留原狀態標
.sidebar_badge <- function(recommended, fallback_label = NULL, fallback_color = NULL) {
  if (isTRUE(recommended)) {
    return(list(label = "推薦", color = "red"))
  }
  if (!is.null(fallback_label) && nzchar(fallback_label)) {
    return(list(label = fallback_label, color = fallback_color %||% "green"))
  }
  list(label = NULL, color = NULL)
}

# `%||%` 若環境尚無
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

# 從 Summary 表萃取單一欄位
extract_summary_item <- function(summary_df, pattern, default = "N/A") {
  if (is.null(summary_df) || !is.data.frame(summary_df) || nrow(summary_df) == 0) return(default)
  idx <- grep(pattern, summary_df$Item, ignore.case = TRUE)
  if (length(idx) == 0) return(default)
  val <- summary_df$Value[idx[1]]
  if (is.na(val) || val == "") default else as.character(val)
}

# 附錄財報表格裁切（左側 TTM / 最新期優先）
trim_report_table <- function(df, max_rows = 18, max_cols = 7) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  df <- coerce_financial_df(df)
  df <- reorder_financial_columns(df)
  if (ncol(df) > max_cols) df <- df[, seq_len(max_cols), drop = FALSE]
  if (nrow(df) > max_rows) df <- df[seq_len(max_rows), , drop = FALSE]
  df
}

# 報告用：NULL / 空字串 → NA_real_
.report_num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  n <- suppressWarnings(as.numeric(x[[1]]))
  if (length(n) != 1 || !is.finite(n)) NA_real_ else n
}

# DCF 模式顯示名稱
.report_dcf_mode_label <- function(mode) {
  m <- if (is.null(mode) || length(mode) == 0 || is.na(mode[1])) "" else as.character(mode[1])
  if (identical(m, "gordon")) return("明確預測期 + Gordon 終值")
  if (identical(m, "two_stage")) return("兩階段成長")
  if (!nzchar(m)) return("N/A")
  m
}

# HTML → PDF（shinyapps／本機容器常用 --no-sandbox）
render_report_pdf <- function(html_path, pdf_path) {
  if (!requireNamespace("pagedown", quietly = TRUE)) {
    stop("需要 pagedown 套件以產出 PDF")
  }
  pagedown::chrome_print(
    input = html_path,
    output = pdf_path,
    wait = 2,
    timeout = 120,
    verbose = 0,
    extra_args = c("--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage")
  )
  invisible(pdf_path)
}

# =========================================================
# 🌟 [共用繪圖引擎] 產生具有高度解讀意義的折現互動圖表 (Using Plotly)
# =========================================================
# 此函數會自動處理：大數字格式化 (B/M/K), 負值變紅,  ticker 注入標題, 資訊豐富的懸停提示
generate_safe_line_plot <- function(data, ticker_name, metric_name) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(plotly::plotly_empty() %>%
             plotly::layout(title = paste0(ticker_name, " - ", metric_name, " (無資料)")))
  }

  # 多列命中時取第一列（呼叫端應已優先挑精確科目）
  data <- data[1, , drop = FALSE]

  # 1. 資料清洗與轉換（支援 B/M/K/T 單位後綴；欄位已為 TTM | 最新→最舊）
  labels <- colnames(data)[-1]
  vals <- parse_financial_number(as.character(unlist(data[1, -1], use.names = FALSE)))
  if (length(labels) == 0 || length(vals) == 0) {
    return(plotly::plotly_empty() %>%
             plotly::layout(title = paste0(ticker_name, " - ", metric_name, " (無資料)")))
  }

  # CAGR 僅用財年欄位（排除 TTM）；必須是有限正值才算，避免 if(NA)
  safe_cagr_msg <- ""
  fy_mask <- !grepl("^ttm$", labels, ignore.case = TRUE)
  fy_vals <- vals[fy_mask]
  fy_vals <- fy_vals[is.finite(fy_vals)]
  if (length(fy_vals) >= 2 &&
      isTRUE(fy_vals[1] > 0) &&
      isTRUE(tail(fy_vals, 1) > 0)) {
    n_yr <- length(fy_vals) - 1
    cagr <- ((fy_vals[1] / tail(fy_vals, 1))^(1 / n_yr) - 1) * 100
    if (is.finite(cagr)) {
      safe_cagr_msg <- paste0(" (", n_yr, "Y CAGR: ", round(cagr, 1), "%)")
    }
  }

  # 2. 建立繪圖專用 DataFrame，並設計「更有解讀意義」的懸停文字
  status_txt <- ifelse(
    is.na(vals), "N/A",
    ifelse(vals < 0,
           "<span style='color:red;'>Negative</span>",
           "<span style='color:green;'>Positive</span>")
  )
  plot_df <- data.frame(
    Year = labels,
    Value = vals,
    HoverText = paste0(
      "<b>", ticker_name, " - ", metric_name, "</b><br>",
      "---------------------<br>",
      "年份 (FY): <b>", labels, "</b><br>",
      "數值: <b>", format_dollar_abbr(vals), "</b><br>",
      "狀態: <b>", status_txt, "</b>"
    ),
    stringsAsFactors = FALSE
  )

  # 點色：NA 當非負處理，避免 scale 斷裂
  plot_df$is_neg <- !is.na(plot_df$Value) & plot_df$Value < 0

  # 3. 繪製圖表 (使用 ggplot)
  p <- ggplot(plot_df, aes(x = Year, y = Value, group = 1, text = HoverText)) +
    geom_line(color = "#7f8c8d", linewidth = 1, na.rm = TRUE) +
    geom_point(aes(color = is_neg), size = 2.5, na.rm = TRUE) +
    scale_color_manual(values = c("FALSE" = "#2c3e50", "TRUE" = "#e74c3c"), guide = "none") +
    scale_y_continuous(
      labels = label_chart_number(prefix = money_prefix()),
      expand = expansion(mult = c(0.1, 0.15))
    ) +
    theme_bw() +
    labs(
      title = paste0(ticker_name, " - ", metric_name, safe_cagr_msg),
      x = "Fiscal Period",
      y = ""
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = "#2c3e50"),
      axis.text.x = element_text(face = "bold")
    )

  # 4. 轉換為 plotly 並指定 tooltip
  ggplotly(p, tooltip = "text")
}

# =========================================================
# 💬 意見區：提交 GitHub Issue（YNOW_FEEDBACK_GITHUB_TOKEN）
# =========================================================
.ynow_feedback_github_repo <- function() {
  repo <- Sys.getenv("YNOW_FEEDBACK_GITHUB_REPO", unset = "lawrencekuo1118/theYNowApp")
  repo <- trimws(as.character(repo)[1])
  if (!nzchar(repo) || !grepl("^[^/]+/[^/]+$", repo)) {
    return("lawrencekuo1118/theYNowApp")
  }
  repo
}

.ynow_feedback_github_token <- function() {
  tok <- Sys.getenv("YNOW_FEEDBACK_GITHUB_TOKEN", unset = "")
  if (!nzchar(tok)) tok <- Sys.getenv("GITHUB_TOKEN", unset = "")
  if (!nzchar(tok)) tok <- Sys.getenv("GH_TOKEN", unset = "")
  trimws(as.character(tok)[1])
}

#' Ensure a label exists on the feedback repo (best-effort; ignore failures).
.ynow_ensure_github_label <- function(repo, name, color = "0E8A16", token) {
  if (!nzchar(token) || !nzchar(name)) return(invisible(FALSE))
  if (!requireNamespace("httr", quietly = TRUE)) return(invisible(FALSE))
  url <- sprintf("https://api.github.com/repos/%s/labels", repo)
  res <- tryCatch(
    httr::POST(
      url,
      httr::add_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/vnd.github+json",
        "X-GitHub-Api-Version" = "2022-11-28"
      ),
      httr::user_agent("TheYNowApp-feedback"),
      body = list(name = name, color = color, description = "User feedback from The YNow App"),
      encode = "json",
      httr::timeout(15)
    ),
    error = function(e) NULL
  )
  if (is.null(res)) return(invisible(FALSE))
  code <- httr::status_code(res)
  invisible(code %in% c(201L, 422L)) # created or already exists
}

#' Create a GitHub Issue from the in-app feedback form.
#' @return list(ok=, html_url=, number=, message=)
.ynow_create_feedback_issue <- function(title, body, labels = character(0)) {
  token <- .ynow_feedback_github_token()
  repo <- .ynow_feedback_github_repo()
  if (!nzchar(token)) {
    return(list(
      ok = FALSE,
      html_url = NA_character_,
      number = NA_integer_,
      message = paste0(
        "尚未設定環境變數 YNOW_FEEDBACK_GITHUB_TOKEN（issues:write）。",
        "無法透過 API 建立 Issue。"
      )
    ))
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(list(
      ok = FALSE, html_url = NA_character_, number = NA_integer_,
      message = "伺服器缺少 httr 套件，無法呼叫 GitHub API。"
    ))
  }
  title <- trimws(as.character(title %||% "")[1])
  body <- as.character(body %||% "")[1]
  if (!nzchar(title)) {
    return(list(ok = FALSE, html_url = NA_character_, number = NA_integer_,
                message = "標題不可空白。"))
  }
  labels <- unique(c("feedback", as.character(labels)))
  labels <- labels[nzchar(labels)]
  for (lab in labels) {
    .ynow_ensure_github_label(repo, lab, token = token)
  }
  url <- sprintf("https://api.github.com/repos/%s/issues", repo)
  res <- tryCatch(
    httr::POST(
      url,
      httr::add_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/vnd.github+json",
        "X-GitHub-Api-Version" = "2022-11-28"
      ),
      httr::user_agent("TheYNowApp-feedback"),
      body = list(title = title, body = body, labels = as.list(labels)),
      encode = "json",
      httr::timeout(30)
    ),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(list(ok = FALSE, html_url = NA_character_, number = NA_integer_,
                message = paste0("連線失敗：", conditionMessage(res))))
  }
  code <- httr::status_code(res)
  parsed <- tryCatch(httr::content(res, as = "parsed", type = "application/json"),
                     error = function(e) NULL)
  if (code >= 200 && code < 300 && !is.null(parsed)) {
    return(list(
      ok = TRUE,
      html_url = as.character(parsed$html_url %||% NA_character_),
      number = suppressWarnings(as.integer(parsed$number %||% NA_integer_)),
      message = "ok"
    ))
  }
  # Retry without labels if label validation failed
  if (code == 422L && length(labels) > 0) {
    res2 <- tryCatch(
      httr::POST(
        url,
        httr::add_headers(
          Authorization = paste("Bearer", token),
          Accept = "application/vnd.github+json",
          "X-GitHub-Api-Version" = "2022-11-28"
        ),
        httr::user_agent("TheYNowApp-feedback"),
        body = list(
          title = title,
          body = paste0(body, "\n\n_Labels (requested):_ ", paste(labels, collapse = ", "))
        ),
        encode = "json",
        httr::timeout(30)
      ),
      error = function(e) e
    )
    if (!inherits(res2, "error")) {
      code2 <- httr::status_code(res2)
      parsed2 <- tryCatch(httr::content(res2, as = "parsed", type = "application/json"),
                          error = function(e) NULL)
      if (code2 >= 200 && code2 < 300 && !is.null(parsed2)) {
        return(list(
          ok = TRUE,
          html_url = as.character(parsed2$html_url %||% NA_character_),
          number = suppressWarnings(as.integer(parsed2$number %||% NA_integer_)),
          message = "ok (created without labels)"
        ))
      }
    }
  }
  err_msg <- if (!is.null(parsed$message)) parsed$message else paste("HTTP", code)
  list(ok = FALSE, html_url = NA_character_, number = NA_integer_, message = as.character(err_msg))
}
