# Phase 2 — Independent data-pipeline oracles (no production normalize/select calls).
# Expected values for period alignment, shares/FX lineage, and fail-soft contracts.
# Source before setup.R engines when comparing.

`%||%` <- function(a, b) if (is.null(a)) b else a

.oracle_approx_eq <- function(a, b, tol = 1e-8) {
  a <- suppressWarnings(as.numeric(a)[1])
  b <- suppressWarnings(as.numeric(b)[1])
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

#' Hand parse of financial strings (subset matching production rules for fixtures).
oracle_parse_financial_number <- function(x) {
  s <- trimws(as.character(x))
  if (length(s) == 0L) return(numeric(0))
  out <- rep(NA_real_, length(s))
  bad <- s %in% c("-", "", "NA", "NaN", "N/A", "--", "null")
  s[bad] <- NA_character_
  idx <- !is.na(s)
  if (!any(idx)) return(out)
  cleaned <- gsub("[,\\$%]", "", s[idx])
  cleaned <- gsub("\\s+", "", cleaned)
  upper <- toupper(cleaned)
  mult <- rep(1, length(upper))
  mult[grepl("T$", upper)] <- 1e12
  mult[grepl("B$", upper)] <- 1e9
  mult[grepl("M$", upper)] <- 1e6
  mult[grepl("K$", upper)] <- 1e3
  cleaned <- sub("[TBMK]$", "", upper)
  out[idx] <- suppressWarnings(as.numeric(cleaned)) * mult
  out
}

#' Expected column order after normalize: Breakdown | TTM* | FY newest→oldest.
oracle_period_order <- function(period_cols) {
  period_cols <- as.character(period_cols)
  is_ttm <- grepl("^ttm$", period_cols, ignore.case = TRUE)
  ttm <- period_cols[is_ttm]
  fy <- period_cols[!is_ttm]
  parse_fy <- function(x) {
    d <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
    if (!is.na(d)) return(d)
    suppressWarnings(as.Date(x))
  }
  if (length(fy)) {
    dates <- vapply(fy, parse_fy, FUN.VALUE = as.Date("1970-01-01"))
    fy <- fy[order(dates, decreasing = TRUE)]
  }
  c(ttm, fy)
}

#' Flow metric current = first after TTM-inclusive order; stock = first FY-only.
oracle_current_metric <- function(row_values_named, type = c("flow", "stock")) {
  type <- match.arg(type)
  nms <- names(row_values_named)
  order <- oracle_period_order(nms)
  if (identical(type, "stock")) {
    order <- order[!grepl("^ttm$", order, ignore.case = TRUE)]
  }
  if (!length(order)) return(NA_real_)
  vals <- oracle_parse_financial_number(as.character(row_values_named[order]))
  vals[1]
}

#' Implied ADR / dual-class shares = market_cap / price when ratio vs BS exceeds thr.
oracle_shares_implied <- function(market_cap, price) {
  market_cap <- as.numeric(market_cap)[1]
  price <- as.numeric(price)[1]
  if (!is.finite(market_cap) || !is.finite(price) || price <= 0) return(NA_real_)
  market_cap / price
}

oracle_share_ratio <- function(shares_implied, shares_bs) {
  shares_implied / shares_bs
}

#' ADR threshold used by app (must stay in sync with SHARE_UNIT_MISMATCH_RATIO_ADR).
ORACLE_SHARE_THR_ADR <- 1.25
ORACLE_SHARE_THR_STD <- 1.5

oracle_should_auto_shares <- function(shares_bs, price, market_cap, adr_fx = FALSE) {
  impl <- oracle_shares_implied(market_cap, price)
  if (!is.finite(impl) || !is.finite(shares_bs) || shares_bs <= 0) return(FALSE)
  ratio <- oracle_share_ratio(impl, shares_bs)
  thr <- if (isTRUE(adr_fx)) ORACLE_SHARE_THR_ADR else ORACLE_SHARE_THR_STD
  is.finite(ratio) && (ratio > thr || ratio < (1 / thr))
}

#' USD↔TWD only; unsupported pairs → NA (refuse silent conversion).
oracle_fx_factor <- function(from_ccy, to_ccy, usd_twd) {
  fr <- toupper(trimws(as.character(from_ccy)[1]))
  to <- toupper(trimws(as.character(to_ccy)[1]))
  if (!nzchar(fr) || !nzchar(to) || fr %in% c("NA", "N/A")) return(NA_real_)
  if (identical(fr, to)) return(1)
  fx <- as.numeric(usd_twd)[1]
  if (!is.finite(fx) || fx <= 0) return(NA_real_)
  if (identical(fr, "USD") && identical(to, "TWD")) return(fx)
  if (identical(fr, "TWD") && identical(to, "USD")) return(1 / fx)
  NA_real_
}

#' Lineage oracle: Equity_statement → FX → / ADR shares → per-share quote ccy.
oracle_per_share_quote <- function(equity, shares, from_ccy, to_ccy, usd_twd,
                                   shares_aligned = TRUE) {
  if (!isTRUE(shares_aligned)) return(NA_real_)
  eq <- as.numeric(equity)[1]
  sh <- as.numeric(shares)[1]
  if (!is.finite(eq) || !is.finite(sh) || sh <= 0) return(NA_real_)
  fr <- toupper(trimws(as.character(from_ccy)[1]))
  to <- toupper(trimws(as.character(to_ccy)[1]))
  if (identical(fr, to)) return(eq / sh)
  fx <- oracle_fx_factor(fr, to, usd_twd)
  if (!is.finite(fx)) return(NA_real_)
  eq * fx / sh
}

#' Mini FCFF Gordon lineage: EV from single CF, equity bridge, per-share.
oracle_lineage_dcf_ps <- function(fcff1, wacc, g, cash, debt, shares) {
  r <- as.numeric(wacc)[1]; g <- as.numeric(g)[1]
  cf <- as.numeric(fcff1)[1]
  tv <- cf * (1 + g) / (r - g)
  ev <- cf / (1 + r) + tv / (1 + r)
  eq <- ev + as.numeric(cash)[1] - as.numeric(debt)[1]
  eq / as.numeric(shares)[1]
}

#' Contract: missing cash/debt lines become 0 (documented fail-soft risk).
oracle_cash_debt_missing_is_zero <- function() list(cash = 0, debt = 0)

#' Empty three-statement payload shape (Yahoo soft-fail).
oracle_empty_statements_payload <- function(quote_ccy = "USD", fin_ccy = NA_character_) {
  empty_tbl <- list(
    columns = c("Breakdown"),
    data = list()
  )
  list(
    "Income Statement" = list(collapsed = empty_tbl, expanded = empty_tbl),
    "Balance Sheet" = list(collapsed = empty_tbl, expanded = empty_tbl),
    "Cash Flow" = list(collapsed = empty_tbl, expanded = empty_tbl),
    "_meta" = list(currency = quote_ccy, financialCurrency = fin_ccy)
  )
}
