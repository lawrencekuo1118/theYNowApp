# ==========================================
# setup_fx.R — Session currency, FX, money display
# Extracted from setup.R (C-STRUCT-04). Sourced by setup.R.
# ==========================================

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
.ynow_ccy_ctx$fx_usd_twd <- NA_real_
.ynow_ccy_ctx$quote_currency <- "USD"
.ynow_ccy_ctx$statement_currency <- "USD"

normalize_ccy <- function(x) {
  s <- toupper(trimws(as.character(x %||% "")[1]))
  # N/A / unknown markers are not ISO codes — treat separately from a real ccy.
  if (!nzchar(s) || s %in% c(
    "NA", "N/A", "N.A.", "N.A", "NONE", "NULL", "UNKNOWN", "UNK", "-", "--"
  )) {
    return(NA_character_)
  }
  if (s %in% c("USD", "USDT", "US$")) return("USD")
  if (s %in% c("TWD", "NTD", "NT$", "NT")) return("TWD")
  s
}

# Session display currency defaults to USD in US mode; TW market mode sets TWD via market_profile.
default_session_currency <- function(quote_ccy, stmt_ccy, ticker = "") {
  if (exists("get_market_mode", mode = "function") && identical(get_market_mode(), "TW")) {
    return("TWD")
  }
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

#' Margin of Safety vs fair value: (FV − Price) / FV (decimal ratio).
#'
#' Near-zero |FV| (float residue / clean-surplus degeneracy when g≈ROE &
#' payout≈0 → P≈0) must not use `fv != 0` alone — that yields Inf / 1e16 % MOS.
#' @param min_abs_fv absolute floor in quote currency units (default 0.01)
#' @return decimal MOS, or NA_real_ when undefined
mos_vs_fair_value <- function(fv, price, min_abs_fv = 0.01) {
  fv <- suppressWarnings(as.numeric(fv)[1])
  price <- suppressWarnings(as.numeric(price)[1])
  floor_abs <- suppressWarnings(as.numeric(min_abs_fv)[1])
  if (!is.finite(floor_abs) || floor_abs <= 0) floor_abs <- 0.01
  if (!is.finite(fv) || !is.finite(price) || abs(fv) < floor_abs) return(NA_real_)
  (fv - price) / fv
}

# USD↔TWD only (v1). Same currency → 1; unknown or missing live FX → NA (refuse).
fx_factor <- function(from_ccy, to_ccy, usd_twd = NULL) {
  fr <- normalize_ccy(from_ccy)
  to <- normalize_ccy(to_ccy)
  if (is.na(fr) && is.na(to)) return(1)
  if (is.na(fr) || is.na(to)) return(NA_real_)
  if (identical(fr, to)) return(1)
  # Only USD↔TWD is supported. Never silently treat CNY／EUR／… as USD (ADR FX).
  fx <- suppressWarnings(as.numeric(usd_twd %||% .ynow_ccy_ctx$fx_usd_twd)[1])
  if (!is.finite(fx) || fx <= 0) return(NA_real_)
  if (identical(fr, "USD") && identical(to, "TWD")) return(fx)
  if (identical(fr, "TWD") && identical(to, "USD")) return(1 / fx)
  NA_real_
}

statement_quote_units_differ <- function(statement_ccy, quote_ccy) {
  st <- normalize_ccy(statement_ccy)
  q <- normalize_ccy(quote_ccy)
  if (is.na(st) || is.na(q)) return(TRUE)
  !identical(st, q)
}

#' TRUE only when both currencies are known and they differ.
#' Unknown / N/A is not "conversion required" by itself (see classifier).
fx_conversion_required <- function(from_ccy, to_ccy) {
  fr <- normalize_ccy(from_ccy)
  to <- normalize_ccy(to_ccy)
  if (is.na(fr) || is.na(to)) return(FALSE)
  !identical(fr, to)
}

#' USD↔TWD rate usability: "ok" / "missing" / "invalid".
#' Unsupported pairs are handled by the classifier as REQUIRED_FX_RATE_MISSING.
.usd_twd_rate_status <- function(usd_twd) {
  if (is.null(usd_twd) || length(usd_twd) < 1L) return("missing")
  num <- suppressWarnings(as.numeric(usd_twd)[1])
  if (length(num) != 1L || is.na(num)) return("missing")
  if (!is.finite(num) || num <= 0) return("invalid")
  "ok"
}

.fx_pair_supported <- function(from_ccy, to_ccy) {
  fr <- normalize_ccy(from_ccy)
  to <- normalize_ccy(to_ccy)
  if (is.na(fr) || is.na(to)) return(FALSE)
  if (identical(fr, to)) return(TRUE)
  (identical(fr, "USD") && identical(to, "TWD")) ||
    (identical(fr, "TWD") && identical(to, "USD"))
}

#' Per-share in `to_ccy` from an equity total.
#' Damodaran: cash flows, discount rate, and price in the same currency.
#' CFA / 20-F: divide by shares of the quoted security (ADR), not local common.
#' Apply FX only when statement unit ≠ quote unit. If FX is missing or ADR
#' shares are not aligned, return NA — never label TWD/common as USD/ADR.
#' Equivalent TSM 20-F check: NT$ per common × 5 / FX == Equity_TWD / FX / ADR.
per_share_in_quote <- function(equity,
                               shares,
                               equity_ccy,
                               to_ccy,
                               usd_twd = NULL,
                               share_method = NULL,
                               statement_ccy = equity_ccy,
                               shares_aligned = NULL) {
  eq <- suppressWarnings(as.numeric(equity)[1])
  sh <- suppressWarnings(as.numeric(shares)[1])
  st <- infer_statement_currency(statement_ccy, to_ccy, share_method)
  eq_ccy <- normalize_ccy(equity_ccy)
  if (is.na(eq_ccy)) eq_ccy <- st
  aligned <- if (!is.null(shares_aligned)) {
    isTRUE(shares_aligned)
  } else {
    shares_auto_adjust_method(share_method)
  }
  # ADR ratio only when quote vs ordinary share-class actually needs it.
  need_adr <- adr_conversion_required(st, to_ccy, share_method)
  if (isTRUE(need_adr) && !isTRUE(aligned)) return(NA_real_)
  if (!is.finite(eq) || !is.finite(sh) || sh <= 0) return(NA_real_)
  src <- normalize_ccy(eq_ccy)
  tgt <- normalize_ccy(to_ccy)
  # Same-currency (incl. inferred-same): no USD/TWD required.
  # One side unknown after infer: refuse (ADR must not copy quote as USD).
  if (is.na(src) && is.na(tgt)) return(eq / sh)
  if (is.na(src) || is.na(tgt)) return(NA_real_)
  if (identical(src, tgt)) return(eq / sh)
  fx <- fx_factor(src, tgt, usd_twd)
  if (!is.finite(fx)) return(NA_real_)
  eq * fx / sh
}

#' Reporting currency for scaled statement DF (money_ccy attr), else session/statement.
equity_money_ccy <- function(df, session_ccy = NULL, statement_ccy = NULL) {
  tagged <- tryCatch(normalize_ccy(attr(df, "money_ccy")), error = function(e) NA_character_)
  if (!is.na(tagged)) return(tagged)
  if (isTRUE(attr(df, "money_scaled"))) {
    sc <- normalize_ccy(session_ccy)
    if (!is.na(sc)) return(sc)
  }
  st <- normalize_ccy(statement_ccy)
  if (!is.na(st)) return(st)
  NA_character_
}

#' IAS 12: use enacted / user T as a decimal ratio. No 35% or 50% engineering caps.
ias12_tax_ratio <- function(tax, from_pct = FALSE) {
  t <- suppressWarnings(as.numeric(tax)[1])
  if (!is.finite(t)) return(NA_real_)
  if (isTRUE(from_pct)) t <- t / 100
  t
}

after_tax_interest <- function(debt, rd, tax) {
  debt <- suppressWarnings(as.numeric(debt)[1])
  rd <- suppressWarnings(as.numeric(rd)[1])
  tax <- ias12_tax_ratio(tax)
  if (!is.finite(debt) || debt < 0) debt <- 0
  if (!is.finite(rd) || rd < 0) rd <- 0
  if (!is.finite(tax)) return(NA_real_)
  max(0, debt) * rd * (1 - tax)
}

#' Statutory corporate tax ratio for the active (or given) market mode.
#' TW = 20%, US = 21%. Used when IS effective tax is unavailable and caller
#' did not pass an explicit T. Soft-depends on market_profile (sourced after setup).
statutory_tax_ratio <- function(mode = NULL) {
  pct <- NA_real_
  if (exists("market_profile", mode = "function")) {
    m <- mode
    if (is.null(m) && exists("get_market_mode", mode = "function")) {
      m <- tryCatch(get_market_mode(), error = function(e) "US")
    }
    pct <- tryCatch(
      suppressWarnings(as.numeric(market_profile(m)$wacc_tax)[1]),
      error = function(e) NA_real_
    )
  }
  if (!is.finite(pct)) pct <- 21
  t <- pct / 100
  if (!is.finite(t) || t < 0 || t >= 1) 0.21 else t
}

#' Normalize a tax input that may be 0.21 or 21 (%).
.norm_tax_ratio <- function(tax, fallback = NULL) {
  if (is.null(fallback)) fallback <- statutory_tax_ratio()
  t <- suppressWarnings(as.numeric(tax)[1])
  if (!is.finite(t)) t <- suppressWarnings(as.numeric(fallback)[1])
  if (!is.finite(t)) t <- statutory_tax_ratio()
  if (t > 1) t <- t / 100
  if (!is.finite(t) || t < 0 || t >= 1) {
    t <- statutory_tax_ratio()
    if (!is.finite(t) || t < 0 || t >= 1) t <- 0.21
  }
  t
}

#' Effective tax from IS (Income Tax Expense / Pretax Income), clamped 0–50%.
effective_tax_ratio_from_is <- function(d_is, fallback = NULL) {
  if (is.null(fallback)) fallback <- statutory_tax_ratio()
  fb <- .norm_tax_ratio(fallback, fallback = statutory_tax_ratio())
  if (is.null(d_is) || !is.data.frame(d_is) || nrow(d_is) < 1L) return(fb)
  pre <- tryCatch(
    select_clean_metric_row(d_is, "Pretax Income", include_ttm = FALSE),
    error = function(e) NULL
  )
  tx <- tryCatch(
    select_clean_metric_row(d_is, "Income Tax Expense", include_ttm = FALSE),
    error = function(e) NULL
  )
  pre <- suppressWarnings(as.numeric(pre))
  tx <- suppressWarnings(as.numeric(tx))
  n <- min(length(pre), length(tx))
  if (n < 1L) return(fb)
  ratios <- tx[seq_len(n)] / pre[seq_len(n)]
  ok <- is.finite(pre[seq_len(n)]) & pre[seq_len(n)] != 0 & is.finite(ratios)
  ratios <- ratios[ok]
  if (!length(ratios)) return(fb)
  t <- mean(pmax(0, pmin(0.5, ratios)), na.rm = TRUE)
  if (!is.finite(t)) fb else t
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

# 非金額列（股數／EPS／稅率等）：FX 換算時必須略過，否則 TSM 等 ADR
# 會把 Ordinary Shares ≈25.9B 誤除以 ~32 → 破壞 ADR 對齊與每股 FV。
.FS_SKIP_FX_MONEY_RE <- paste0(
  "(EPS|Earnings Per Share|Average Shares|Share Issued|Shares Number|",
  "Treasury Shares|Tax Rate|Tax Rate For Calcs|Shares Outstanding)"
)

#' 該科目列是否為「金額」而應套用 FX（股數／每股／稅率除外）
fs_row_is_fx_money <- function(label) {
  lab <- as.character(label %||% "")[1]
  if (!nzchar(lab)) return(FALSE)
  if (grepl("每股盈餘|加權平均股數|已發行股數|普通股股數|庫藏股數|計算用稅率|稅率", lab)) {
    return(FALSE)
  }
  !grepl(.FS_SKIP_FX_MONEY_RE, lab, ignore.case = TRUE, perl = TRUE)
}

# Scale money period columns of a Yahoo financial statement DF into session currency.
# Keeps full numeric precision for downstream KPI／估值；顯示四捨五入見 format_financial_df_display。
# Share counts / EPS / tax-rate rows are left unchanged (not FX money).
scale_financial_df_money <- function(df, from_ccy, session_ccy = NULL, usd_twd = NULL) {
  .tag_money <- function(x, scaled, ccy) {
    attr(x, "money_scaled") <- isTRUE(scaled)
    attr(x, "money_ccy") <- normalize_ccy(ccy)
    x
  }
  sess <- normalize_ccy(session_ccy %||% .ynow_ccy_ctx$session_currency)
  fr <- normalize_ccy(from_ccy)
  if (is.null(df) || !is.data.frame(df) || ncol(df) < 2) {
    return(.tag_money(df, FALSE, fr))
  }
  mult <- fx_factor(from_ccy, sess, usd_twd)
  if (!is.finite(mult)) {
    return(.tag_money(df, FALSE, fr))
  }
  if (abs(mult - 1) < 1e-15) {
    return(.tag_money(df, TRUE, if (is.na(sess)) fr else sess))
  }
  out <- df
  labels <- as.character(out[[1]])
  scale_row <- vapply(labels, fs_row_is_fx_money, logical(1), USE.NAMES = FALSE)
  for (j in seq.int(2L, ncol(out))) {
    raw <- as.character(out[[j]])
    nums <- parse_financial_number(raw)
    scaled <- ifelse(scale_row & is.finite(nums), nums * mult, nums)
    out[[j]] <- ifelse(
      is.na(scaled) | !is.finite(scaled),
      raw,
      # Keep non-money rows as original raw when not scaled; money rows formatted.
      ifelse(
        scale_row,
        format(scaled, scientific = FALSE, trim = TRUE, digits = 15),
        raw
      )
    )
  }
  .tag_money(out, TRUE, sess)
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
    "Market Price", "Previous Close", "Open", "Bid", "Ask", "Market Cap (intraday)",
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
