# ==========================================
# rebal_freq.R — Date_t analysis / rebalance frequency (US & TW)
# ==========================================

`%||%` <- if (exists("%||%", mode = "function")) get("%||%") else function(x, y) {
  if (is.null(x) || length(x) < 1 || (length(x) == 1 && is.na(x))) y else x
}

.normalize_rebal_freq <- function(x, default = "quarterly") {
  x <- tolower(trimws(as.character(x %||% default)[1]))
  if (!nzchar(x) || identical(x, "na")) return(default)
  if (x %in% c("monthly", "month", "m", "mo", "每月", "月")) return("monthly")
  if (x %in% c("yearly", "annual", "annually", "year", "y", "yr", "每年", "年")) {
    return("yearly")
  }
  if (x %in% c("quarterly", "quarter", "q", "每季", "季")) return("quarterly")
  default
}

.rebal_freq_label_zh <- function(freq) {
  switch(.normalize_rebal_freq(freq), monthly = "每月", yearly = "每年", "每季")
}

.rebal_min_points <- function(freq) {
  switch(.normalize_rebal_freq(freq), monthly = 12L, yearly = 3L, 4L)
}

.rebal_period_keys <- function(dates, freq = "quarterly") {
  dates <- as.Date(dates)
  freq <- .normalize_rebal_freq(freq)
  if (identical(freq, "monthly")) return(format(dates, "%Y-%m"))
  if (identical(freq, "yearly")) return(format(dates, "%Y"))
  sprintf("%d-Q%d", as.integer(format(dates, "%Y")),
          ((as.integer(format(dates, "%m")) - 1L) %/% 3L) + 1L)
}

.rebal_indices_for_freq <- function(df, freq = "quarterly") {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L || !"Date" %in% names(df)) {
    return(integer(0))
  }
  keys <- .rebal_period_keys(df$Date, freq)
  ends <- !duplicated(keys, fromLast = TRUE)
  rsi_ok <- if ("RSI" %in% names(df)) !is.na(df$RSI) else TRUE
  ret_ok <- if ("ret20" %in% names(df)) !is.na(df$ret20) else TRUE
  which(ends & rsi_ok & ret_ok)
}

detect_supported_rebal_freqs <- function(dates,
                                         min_monthly = 12L,
                                         min_quarterly = 4L,
                                         min_yearly = 3L,
                                         min_coverage = 0.80) {
  dates <- as.Date(dates)
  dates <- sort(unique(dates[!is.na(dates)]))
  if (length(dates) < as.integer(min_yearly)) return(character(0))
  span_days <- as.numeric(diff(range(dates)))
  if (!is.finite(span_days) || span_days < 60) return(character(0))

  coverage_ok <- function(freq, min_n) {
    keys <- unique(.rebal_period_keys(dates, freq))
    n <- length(keys)
    if (n < as.integer(min_n)) return(FALSE)
    expected <- switch(
      .normalize_rebal_freq(freq),
      monthly = max(as.integer(min_n), as.integer(round(span_days / 30.4375))),
      yearly = max(as.integer(min_n), as.integer(round(span_days / 365.25))),
      max(as.integer(min_n), as.integer(round(span_days / 91.3125)))
    )
    (n / expected) >= as.numeric(min_coverage)
  }

  out <- character(0)
  if (coverage_ok("monthly", min_monthly)) out <- c(out, "monthly")
  if (coverage_ok("quarterly", min_quarterly)) out <- c(out, "quarterly")
  if (coverage_ok("yearly", min_yearly)) out <- c(out, "yearly")
  out
}

infer_rebal_freq_from_dates <- function(dates) {
  dates <- sort(unique(as.Date(dates[!is.na(as.Date(dates))])))
  if (length(dates) < 2L) return(NA_character_)
  med <- stats::median(as.numeric(diff(dates)), na.rm = TRUE)
  if (!is.finite(med)) return(NA_character_)
  if (med <= 45) return("monthly")
  if (med <= 140) return("quarterly")
  "yearly"
}

supported_analysis_freqs <- function(price_dates = NULL, valuation_dates = NULL) {
  from_px <- if (!is.null(price_dates) && length(price_dates) > 0) {
    detect_supported_rebal_freqs(price_dates)
  } else character(0)
  from_vd <- character(0)
  if (!is.null(valuation_dates) && length(valuation_dates) > 0) {
    inferred <- infer_rebal_freq_from_dates(valuation_dates)
    n <- length(unique(as.Date(valuation_dates[!is.na(as.Date(valuation_dates))])))
    if (identical(inferred, "monthly")) {
      from_vd <- "monthly"
      if (n >= 4L) from_vd <- c(from_vd, "quarterly")
      if (n >= 3L) from_vd <- c(from_vd, "yearly")
    } else if (identical(inferred, "quarterly")) {
      from_vd <- "quarterly"
      if (n >= 3L) from_vd <- c(from_vd, "yearly")
    } else if (identical(inferred, "yearly")) {
      from_vd <- "yearly"
    }
  }
  if (length(from_px) > 0) unique(c(from_px, from_vd)) else unique(from_vd)
}
