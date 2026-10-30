# ==========================================
# setup_fs.R — Financial-statement glue (parse / normalize / metrics)
# Extracted from setup.R (C-STRUCT-04). Sourced by setup.R.
# ==========================================

#' Primary valuation KPI numeral. White result boxes use the logo blue–green flow;
#' filled result boxes keep the numeral white (CSS).
#' Plain text for empty / N/A placeholders so siblings stay non-flow.
ynow_model_result_num <- function(txt) {
  s <- as.character(txt %||% "")[1]
  if (!nzchar(s) || s %in% c("N/A", "NA", "—", "-", "–")) return(s)
  tags$span(class = "ynow-htcdi-flow ynow-model-kpi-result-num", s)
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
    if (grepl("^[Yy]?[0-9]{4}$", x)) return(as.Date(paste0(sub("^[Yy]", "", x), "-12-31")))
    tryCatch(suppressWarnings(as.Date(x)), error = function(e) as.Date(NA))
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
  meta <- res[["_meta"]]
  stmts <- if ("_meta" %in% names(res)) res[names(res) != "_meta"] else res
  out <- lapply(stmts, normalize_financial_statement)
  q_ccy <- NULL
  f_ccy <- NULL
  if (is.list(meta)) {
    q_ccy <- meta$currency %||% meta$quote_currency
    f_ccy <- meta$financialCurrency %||% meta$financial_currency
    attr(out, "currency") <- q_ccy
    attr(out, "financialCurrency") <- f_ccy
  }
  # Stamp quote/statement ccy onto each statement DF so FX scale / ADR
  # resolve still works after nested list access.
  if (!is.null(q_ccy) || !is.null(f_ccy)) {
    out <- lapply(out, function(stmt) {
      if (is.null(stmt)) return(stmt)
      stamp <- function(df) {
        if (is.null(df) || !is.data.frame(df)) return(df)
        if (!is.null(q_ccy)) attr(df, "currency") <- q_ccy
        if (!is.null(f_ccy)) attr(df, "financialCurrency") <- f_ccy
        df
      }
      if (is.data.frame(stmt)) return(stamp(stmt))
      if (is.list(stmt)) {
        if (!is.null(stmt$collapsed)) stmt$collapsed <- stamp(stmt$collapsed)
        if (!is.null(stmt$expanded)) stmt$expanded <- stamp(stmt$expanded)
      }
      stmt
    })
    if (is.list(meta)) {
      attr(out, "currency") <- q_ccy
      attr(out, "financialCurrency") <- f_ccy
    }
  }
  out
}

#' 單張財報是否可視為空（無科目列或無期間欄）
financial_df_is_empty <- function(df) {
  df <- coerce_financial_df(df)
  is.null(df) || !is.data.frame(df) || nrow(df) < 1L || ncol(df) < 2L
}

#' scraped_financials 結構：IS／BS／CF 是否皆空
#' （新上櫃／興櫃常見：Yahoo Summary 有價，但年報三表尚未上線）
financials_is_bs_cf_all_empty <- function(res) {
  if (is.null(res)) return(TRUE)
  .stmt_empty <- function(key) {
    stmt <- tryCatch(res[[key]], error = function(e) NULL)
    if (is.null(stmt)) return(TRUE)
    exp <- stmt$expanded
    if (is.null(exp)) exp <- stmt$collapsed
    financial_df_is_empty(coerce_financial_df(exp))
  }
  .stmt_empty("Income Statement") &&
    .stmt_empty("Balance Sheet") &&
    .stmt_empty("Cash Flow")
}

# 櫃買「財務資料簡報」季報彙總（上櫃 O_*.xls／興櫃 U_*.xls）→ Yahoo IS／BS 缺漏時 soft-fill。
# 僅摘要欄位；非完整 IS／BS／CF。Search／HFV 皆走 should_try_tpex_financial_fallback。
# 勿接 FinMind；勿對未提供科目（尤其 CF／CapEx／FCF）捏造數值。
YNOW_MOPS_HOME_URL <- "https://mops.twse.com.tw/"
YNOW_TPEX_HOME_URL <- "https://www.tpex.org.tw/"
YNOW_TPEX_FINANCIAL_SUMMARY_URL <-
  "https://www.tpex.org.tw/zh-tw/mainboard/listed/financial/summary.html"

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

# Arithmetic across statements must join fiscal columns, never vector positions.
align_financial_flows <- function(d_is, d_cf, include_ttm = TRUE) {
  tables <- list(income = reorder_financial_columns(d_is), cashflow = reorder_financial_columns(d_cf))
  periods <- Reduce(intersect, lapply(tables, function(d) if (is.data.frame(d)) names(d)[-1] else character()))
  if (!include_ttm) periods <- periods[!grepl("^ttm$", periods, ignore.case = TRUE)]
  lapply(tables, function(d) {
    if (!is.data.frame(d)) return(d)
    out <- if (ncol(d)) d[, c(names(d)[1], periods), drop = FALSE] else data.frame(Breakdown = character(nrow(d)))
    if (!length(periods)) out$.unavailable_period <- rep(NA_real_, nrow(out))
    for (key in c("money_ccy", "money_scaled", "financialCurrency", "currency")) attr(out, key) <- attr(d, key)
    out
  })
}

# 常用科目別名（Yahoo HTML 用 &；yfinance 用 And）
NET_INCOME_PATTERNS <- c(
  "^Net Income Common Stockholders$",
  "^Net Income$",
  "^Net Income From Continuing (And|&) Discontinued Operation$"
)
# 利息費用（損益／現金流別名；勿用 | 併成單一 grep）
INTEREST_EXPENSE_PATTERNS <- c(
  "^Interest Expense$",
  "^Interest Expense Non Operating$"
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
CAPEX_PATTERNS <- c(
  "^Capital Expenditure$",
  "^Capital Expenditures$",
  "Purchase Of PPE",
  "Net PPE Purchase And Sale",
  "Capital Expenditure"
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

# 三大現金流合計。yfinance 有時只有 Continuing Activities、沒有 Operating/Investing Cash Flow 列。
CASHFLOW_OCF_PATTERNS <- c(
  "^Operating Cash Flow$",
  "^Cash Flow From Continuing Operating Activities$",
  "^Cash Flow From Operating Activities$"
)
CASHFLOW_ICF_PATTERNS <- c(
  "^Investing Cash Flow$",
  "^Cash Flow From Continuing Investing Activities$",
  "^Cash Flow From Investing Activities$"
)
CASHFLOW_FIN_PATTERNS <- c(
  "^Financing Cash Flow$",
  "^Cash Flow From Continuing Financing Activities$",
  "^Cash Flow From Financing Activities$"
)

cashflow_total_row <- function(df, kind = c("ocf", "icf", "fin"), include_ttm = FALSE) {
  kind <- match.arg(kind)
  pats <- switch(
    kind,
    ocf = CASHFLOW_OCF_PATTERNS,
    icf = CASHFLOW_ICF_PATTERNS,
    fin = CASHFLOW_FIN_PATTERNS
  )
  select_clean_metric_row_any(df, pats, include_ttm = include_ttm)
}

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

