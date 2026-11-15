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

# =========================================================
# Sensitivity matrices → native Plotly heatmap (display-only)
# Shared by DCF / DDM / RI. Does not change valuation formulas.
# Convention: z rows = discount rate (WACC/Ke), cols = terminal g.
# =========================================================

#' Default red → amber → green colorscale for absolute intrinsic values.
ynow_sensitivity_heatmap_colorscale <- function() {
  list(
    list(0, "#e74c3c"),
    list(0.5, "#f7dc6f"),
    list(1, "#27ae60")
  )
}

#' Build a Plotly heatmap for a WACC/Ke × g intrinsic-value matrix.
#'
#' @param z numeric matrix; rows = discount rates, cols = terminal g (NA = invalid).
#' @param disc_vals numeric vector of discount rates in percent (length = nrow(z)).
#' @param g_vals numeric vector of terminal g in percent (length = ncol(z)).
#' @param disc_label axis / hover label for the discount rate ("WACC" or "Ke").
#' @param title plot title (already localized by caller).
#' @param colorbar_title colorbar title (already localized).
#' @param x_title x-axis title (already localized).
#' @param y_title y-axis title (already localized).
#' @param na_label hover text for non-finite cells.
#' @param hover_extra optional character matrix (same dim as z) appended to hover.
#' @param center_ij optional length-2 integer c(row, col) marking the base-case cell.
#' @return a plotly object
ynow_sensitivity_heatmap <- function(
    z,
    disc_vals,
    g_vals,
    disc_label = "WACC",
    title = "Sensitivity heatmap",
    colorbar_title = "Intrinsic / share",
    x_title = "Terminal g (SGR) (%)",
    y_title = NULL,
    na_label = "Invalid (discount ≤ g)",
    hover_extra = NULL,
    center_ij = NULL
) {
  empty_plot <- function(msg) {
    plotly::plotly_empty() %>%
      plotly::layout(title = list(
        text = msg,
        font = list(size = 14, color = "#856404", family = "Arial, sans-serif")
      ))
  }

  if (is.null(z) || !is.matrix(z) || length(z) < 1L) {
    return(empty_plot(as.character(title %||% "Sensitivity heatmap")))
  }
  nr <- nrow(z)
  nc <- ncol(z)
  disc_vals <- suppressWarnings(as.numeric(disc_vals))
  g_vals <- suppressWarnings(as.numeric(g_vals))
  if (length(disc_vals) != nr || length(g_vals) != nc ||
      any(!is.finite(disc_vals)) || any(!is.finite(g_vals))) {
    return(empty_plot(as.character(title %||% "Sensitivity heatmap")))
  }
  if (is.null(y_title) || !nzchar(as.character(y_title)[1])) {
    y_title <- paste0(disc_label, " (%)")
  }

  hover <- matrix("", nrow = nr, ncol = nc)
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      cell <- z[i, j]
      val_txt <- if (is.finite(cell)) sprintf("%.2f", cell) else as.character(na_label)[1]
      extra <- ""
      if (!is.null(hover_extra) && is.matrix(hover_extra) &&
          nrow(hover_extra) == nr && ncol(hover_extra) == nc) {
        ex <- hover_extra[i, j]
        if (!is.null(ex) && length(ex) >= 1L && !is.na(ex) && nzchar(as.character(ex)[1])) {
          extra <- paste0("<br>", as.character(ex)[1])
        }
      }
      hover[i, j] <- sprintf(
        "%s: %.2f%%<br>g: %.2f%%<br>Intrinsic / share: %s%s",
        disc_label, disc_vals[i], g_vals[j], val_txt, extra
      )
    }
  }

  shapes <- list()
  if (!is.null(center_ij) && length(center_ij) >= 2L) {
    ci <- as.integer(center_ij[1])
    cj <- as.integer(center_ij[2])
    if (is.finite(ci) && is.finite(cj) && ci >= 1L && ci <= nr && cj >= 1L && cj <= nc) {
      dx <- if (nc >= 2L) min(diff(g_vals)) / 2 else 0.25
      dy <- if (nr >= 2L) min(diff(disc_vals)) / 2 else 0.25
      if (!is.finite(dx) || dx <= 0) dx <- 0.25
      if (!is.finite(dy) || dy <= 0) dy <- 0.25
      # Plotly heatmap places category centers at the numeric coords; outline base case.
      shapes <- list(list(
        type = "rect",
        xref = "x", yref = "y",
        x0 = g_vals[cj] - dx * 0.9,
        x1 = g_vals[cj] + dx * 0.9,
        y0 = disc_vals[ci] - dy * 0.9,
        y1 = disc_vals[ci] + dy * 0.9,
        line = list(color = "#2980b9", width = 3),
        fillcolor = "rgba(0,0,0,0)"
      ))
    }
  }

  plotly::plot_ly(
    x = g_vals,
    y = disc_vals,
    z = z,
    type = "heatmap",
    colorscale = ynow_sensitivity_heatmap_colorscale(),
    hoverinfo = "text",
    text = hover,
    colorbar = list(title = as.character(colorbar_title)[1]),
    xgap = 1,
    ygap = 1
  ) %>%
    plotly::layout(
      title = list(text = as.character(title)[1], font = list(size = 14)),
      xaxis = list(
        title = as.character(x_title)[1],
        tickmode = "array",
        tickvals = g_vals,
        ticktext = sprintf("%.1f", g_vals)
      ),
      yaxis = list(
        title = as.character(y_title)[1],
        tickmode = "array",
        tickvals = disc_vals,
        ticktext = sprintf("%.1f", disc_vals)
      ),
      shapes = shapes,
      margin = list(t = 50, b = 56, l = 64, r = 24),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FAFBFC"
    ) %>%
    plotly::config(displayModeBar = FALSE, responsive = TRUE, displaylogo = FALSE)
}

#' Recover numeric discount / g axes from a sensitivity matrix with labeled dimnames.
#' Falls back to evenly spaced indices when labels cannot be parsed.
ynow_sensitivity_axis_vals <- function(mat, axes = NULL, which = c("disc", "g")) {
  which <- match.arg(which)
  if (is.null(mat) || !is.matrix(mat)) return(numeric(0))
  labs <- if (identical(which, "disc")) rownames(mat) else colnames(mat)
  n <- if (identical(which, "disc")) nrow(mat) else ncol(mat)
  parsed <- suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", as.character(labs))))
  if (length(parsed) == n && all(is.finite(parsed))) return(parsed)

  base <- if (identical(which, "disc")) {
    suppressWarnings(as.numeric(axes$disc %||% axes$wacc %||% axes$ke)[1])
  } else {
    suppressWarnings(as.numeric(axes$g)[1])
  }
  if (is.finite(base) && n >= 1L) {
    # Match builders: disc from base+2 → base−2; g from base−1 → base+1.
    if (identical(which, "disc")) {
      return(seq(base + 2, base - 2, length.out = n))
    }
    return(seq(base - 1, base + 1, length.out = n))
  }
  seq_len(n)
}
