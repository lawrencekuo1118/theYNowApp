# ==========================================
# setup.R - 財報數據處理與輔助函數模組
# ==========================================

# 轉換數字為 K / M / B 格式
format_dollar_abbr <- function(x) {
  if (is.null(x) || is.na(x) || !is.numeric(x)) return("N/A")
  
  if (abs(x) >= 1e9) {
    paste0("$", round(x / 1e9, 2), "B")
  } else if (abs(x) >= 1e6) {
    paste0("$", round(x / 1e6, 2), "M")
  } else {
    paste0("$", round(x, 2))
  }
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
EQUITY_PATTERNS <- c(
  "Common Stock Equity",
  "Stockholders Equity",
  "Total Equity Gross Minority Interest"
)
OPEX_PATTERNS <- c(
  "^Operating Expense$",
  "^Operating Expenses$"
)

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
ui_missing_data_alert <- function(check_list, fallback_msg = "系統已自動將上述項目視為 0 代入計算。請確認是否需要手動補齊數值以確保預測精準度。") {
  
  # 找出 NA, NULL 或空字串的項目
  missing_items <- names(check_list)[sapply(check_list, function(x) {
    is.null(x) || is.na(x) || (is.character(x) && trimws(x) == "")
  })]
  
  if (length(missing_items) > 0) {
    shiny::div(
      style = "color: #a94442; background-color: #f2dede; border: 1px solid #ebccd1; padding: 15px; border-radius: 4px; margin-bottom: 20px;",
      shiny::icon("exclamation-triangle"), 
      shiny::tags$b(" 偵測到數據缺失："), 
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

# 從預測表統一取出 FCFF 序列（相容舊欄位名 FCF）
extract_fcff_series <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(numeric(0))
  if ("FCFF" %in% colnames(df)) return(as.numeric(df$FCFF))
  if ("FCF" %in% colnames(df)) return(as.numeric(df$FCF))
  rep(NA_real_, nrow(df))
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

# 蒐集財務舞弊 / 體質警訊
collect_fraud_warnings <- function(d_cf, d_is, d_bs) {
  msgs <- character(0)
  fcf <- get_avg(select_clean_metric_row(d_cf, "Free Cash Flow", include_ttm = FALSE))
  ocf <- get_avg(select_clean_metric_row(d_cf, "Operating Cash Flow", include_ttm = FALSE))
  net <- get_avg(select_clean_metric_row_any(d_is, NET_INCOME_PATTERNS, include_ttm = FALSE))
  debt <- get_avg(select_clean_metric_row(d_bs, "Total Debt", include_ttm = FALSE))
  equity <- get_avg(select_clean_metric_row_any(d_bs, EQUITY_PATTERNS, include_ttm = FALSE))
  
  if (!is.na(fcf) && fcf < 0) msgs <- c(msgs, "自由現金流為負，可能面臨營運或資本支出壓力")
  if (!is.na(ocf) && ocf < 0) msgs <- c(msgs, "營業現金流為負，核心業務現金創造能力不足")
  if (!is.na(ocf) && !is.na(net) && ocf < net) msgs <- c(msgs, "營業現金流低於淨利，獲利現金轉換率偏低")
  if (!is.na(net) && !is.na(ocf) && net > 0 && ocf < 0) msgs <- c(msgs, "帳面獲利為正但現金流為負，盈餘品質存疑")
  ratio <- if (!is.na(debt) && !is.na(equity) && equity != 0) debt / equity else NA
  if (!is.na(ratio) && ratio > 2) msgs <- c(msgs, "負債權益比偏高，財務槓桿風險需留意")
  msgs
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

# 推薦估值方法（對應決策模組邏輯）
derive_valuation_method <- function(d_cf, industry_text = "") {
  rec <- recommend_valuation_models(d_cf, industry_text)
  list(method = rec$summary_method, rationale = rec$reason)
}

#' 模型選擇器 → 側邊欄標記用（與 Dashboard「推薦首選」同一套規則）
#' @return list(ddm, dcf, pb, ri, tags=character, summary_method, reason)
recommend_valuation_models <- function(d_cf, industry_text = "", d_is = NULL, d_bs = NULL) {
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

  is_financial <- grepl(
    "Bank|Insurance|Financial|Conglomerate|fn\\.|Insurance Brokers",
    industry_text,
    ignore.case = TRUE
  )
  rev_g <- tryCatch(
    get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE)),
    error = function(e) NA_real_
  )
  ni <- tryCatch(select_current_metric_any(d_is, NET_INCOME_PATTERNS, "flow"), error = function(e) NA_real_)
  equity <- tryCatch(select_current_metric_any(d_bs, EQUITY_PATTERNS, "stock"), error = function(e) NA_real_)
  roe <- if (!is.na(ni) && !is.na(equity) && equity > 0) ni / equity * 100 else NA_real_
  asset_or_book_driven <- isTRUE(is_financial) || grepl(
    "REIT|Real Estate|Asset|Bank|Insurance|Utility|Utilities",
    industry_text,
    ignore.case = TRUE
  )

  out <- list(
    ddm = FALSE, dcf = FALSE, pb = FALSE, ri = FALSE,
    tags = character(0),
    summary_method = "P/B／相對估值",
    reason = "傳統折現模型前提不足，建議以 P/B、淨資產法為主。"
  )

  if (isTRUE(asset_or_book_driven)) {
    out$pb <- TRUE
    out$ri <- isTRUE(!is.na(roe) && roe > 0)
    out$tags <- if (isTRUE(out$ri)) c("pb", "ri") else "pb"
    out$summary_method <- "P/B（本淨比／資產法）"
    out$reason <- "資產／帳面價值驅動產業優先採 P/B；若 ROE 為正，可搭配 RI 檢查超額報酬。"
  } else if (isTRUE(is_div_stable) && !isTRUE(is_fcf_stable)) {
    out$ddm <- TRUE
    out$tags <- "ddm"
    out$summary_method <- "DDM（股利折現）"
    out$reason <- "股利穩定度高於自由現金流穩定度，優先以股東實際現金回報估值。"
  } else if (!isTRUE(is_div_stable) && isTRUE(is_fcf_stable)) {
    out$dcf <- TRUE
    out$ri <- isTRUE(!is.na(roe) && roe > 8)
    out$tags <- "dcf"
    out$summary_method <- "DCF（自由現金流折現）"
    out$reason <- "自由現金流為正且波動可控，優先用 FCFF 折現衡量企業價值。"
  } else if (isTRUE(is_div_stable) && isTRUE(is_fcf_stable)) {
    out$ddm <- TRUE
    out$dcf <- TRUE
    out$ri <- isTRUE(!is.na(roe) && roe > 8)
    out$tags <- c("dcf", "ddm", if (isTRUE(out$ri)) "ri")
    out$summary_method <- "DCF + DDM 交叉驗證"
    out$reason <- "現金流與配息皆穩定，建議用 DCF 與 DDM 交叉驗證；ROE 足夠時可加 RI。"
  } else if (is.finite(rev_g) && rev_g > 12 && isTRUE(is_fcf_pos)) {
    out$dcf <- TRUE
    out$tags <- "dcf"
    out$summary_method <- "DCF（Two-Stage）"
    out$reason <- "營收仍處高成長且 FCF 為正，較適合用 two-stage DCF 反映成長收斂。"
  } else {
    out$pb <- TRUE
    out$tags <- "pb"
    out$summary_method <- "P/B／相對估值"
    out$reason <- "配息與 FCF 穩定性不足，折現模型輸入可信度偏低，先以 P/B／相對估值定位。"
  }
  out
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

#' 估計永續成長率（%）並附說明；必要時建議 two-stage
#' @return list(g_pct, reason, lifecycle_stage, suggest_two_stage, g_stage1_pct, auto_lifecycle)
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

  # 安全閥：g 必須嚴格小於 WACC
  if (is.finite(wacc_pct) && is.finite(g_pct) && g_pct >= wacc_pct) {
    safe_g <- max(0.5, round(wacc_pct - 2, 2))
    reason <- paste0(
      reason, " ⚠ g≥WACC，已壓至 ", safe_g, "%（WACC−2）。"
    )
    g_pct <- safe_g
  }

  list(
    g_pct = g_pct,
    reason = reason,
    lifecycle_stage = stage,
    auto_lifecycle = auto_stage,
    suggest_two_stage = isTRUE(suggest_two_stage),
    g_stage1_pct = g_stage1_pct
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
      "數值: <b>$", ifelse(is.na(vals), "N/A", format(vals, big.mark = ",", scientific = FALSE)), "</b><br>",
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
      labels = scales::label_dollar(scale_cut = scales::cut_short_scale()),
      expand = expansion(mult = c(0.1, 0.15))
    ) +
    theme_bw() +
    labs(
      title = paste0("📈 ", ticker_name, " - ", metric_name, safe_cagr_msg),
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
