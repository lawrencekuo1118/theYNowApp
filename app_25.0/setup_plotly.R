# ==========================================
# setup_plotly.R — Shared Plotly line-chart engine
# Extracted from setup.R (C-STRUCT-04). Sourced by setup.R.
# ==========================================

# =========================================================
# 🌟 [共用繪圖引擎] 產生具有高度解讀意義的折現互動圖表 (Using Plotly)
# =========================================================
# Native plot_ly (no ggplotly) — thinner JSON payload, faster dashboard IS plots.
# Handles B/M/K formatting, negative point color, ticker title, rich hover text.
#' Downsample a time-ordered data.frame for Plotly (keep endpoints).
.ynow_downsample_df <- function(df, max_n = 600L) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  n <- nrow(df)
  max_n <- as.integer(max_n)[1]
  if (!is.finite(n) || !is.finite(max_n) || n <= max_n || max_n < 3L) return(df)
  idx <- unique(c(1L, as.integer(round(seq(1, n, length.out = max_n))), n))
  df[idx, , drop = FALSE]
}

generate_safe_line_plot <- function(data, ticker_name, metric_name) {
  empty_plot <- function(msg) {
    plotly::plotly_empty() %>%
      plotly::layout(title = list(
        text = msg,
        font = list(size = 15, color = "#856404", family = "Arial, sans-serif")
      ))
  }
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(empty_plot(paste0(ticker_name, " - ", metric_name, " (無資料)")))
  }

  # 多列命中時取第一列（呼叫端應已優先挑精確科目）
  data <- data[1, , drop = FALSE]

  # 1. 資料清洗與轉換（支援 B/M/K/T 單位後綴；欄位已為 TTM | 最新→最舊）
  labels <- colnames(data)[-1]
  vals <- parse_financial_number(as.character(unlist(data[1, -1], use.names = FALSE)))
  if (length(labels) == 0 || length(vals) == 0) {
    return(empty_plot(paste0(ticker_name, " - ", metric_name, " (無資料)")))
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

  status_txt <- ifelse(
    is.na(vals), "N/A",
    ifelse(vals < 0, "Negative", "Positive")
  )
  point_colors <- ifelse(!is.na(vals) & vals < 0, "#c0392b", "#0C5484")
  hover <- paste0(
    "<b>", ticker_name, " - ", metric_name, "</b><br>",
    "---------------------<br>",
    "年份 (FY): <b>", labels, "</b><br>",
    "數值: <b>", format_dollar_abbr(vals), "</b><br>",
    "狀態: <b>", status_txt, "</b>"
  )
  title_txt <- paste0(ticker_name, " - ", metric_name, safe_cagr_msg)
  y_prefix <- tryCatch(money_prefix(), error = function(e) "")

  plotly::plot_ly() %>%
    plotly::add_trace(
      x = labels, y = vals, type = "scatter", mode = "lines+markers",
      line = list(color = "#C9A227", width = 2),
      marker = list(size = 8, color = point_colors),
      hoverinfo = "text", text = hover,
      name = metric_name, showlegend = FALSE
    ) %>%
    plotly::layout(
      title = list(
        text = title_txt,
        font = list(size = 15, color = "#856404", family = "Arial, sans-serif")
      ),
      xaxis = list(title = "Fiscal Period", type = "category", tickangle = -30),
      yaxis = list(
        title = "",
        tickprefix = if (nzchar(y_prefix)) y_prefix else NULL,
        tickformat = "~s",
        separatethousands = TRUE
      ),
      margin = list(t = 48, b = 64),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FAFBFC"
    ) %>%
    plotly::config(displayModeBar = FALSE, responsive = TRUE, displaylogo = FALSE)
}
