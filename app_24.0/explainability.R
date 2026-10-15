# ==========================================
# explainability.R — first-class「可解釋性」panels (not appendix)
# Compact Why-this-value cards for valuation results.
# ==========================================

#' Build a compact Why-this-value panel (core explainability surface).
#'
#' @param title Panel title (already localized)
#' @param rows data.frame with columns label, value (character)
#' @param empty_message Shown when rows is empty / NULL
#' @param footnote Optional short footnote under the rows
ynow_value_why_panel <- function(title,
                                 rows = NULL,
                                 empty_message = NULL,
                                 footnote = NULL) {
  body <- if (is.null(rows) || !is.data.frame(rows) || nrow(rows) < 1L) {
    tags$p(
      class = "ynow-value-why__empty",
      style = "margin:0; color:#888; font-size:12.5px;",
      as.character(empty_message %||% "")[1]
    )
  } else {
    tags$dl(
      class = "ynow-value-why__dl",
      style = "margin:0; display:grid; grid-template-columns:minmax(7em,32%) 1fr; gap:4px 12px; font-size:12.5px; line-height:1.45;",
      lapply(seq_len(nrow(rows)), function(i) {
        tagList(
          tags$dt(
            style = "margin:0; color:#737373; font-weight:600;",
            as.character(rows$label[[i]] %||% "")[1]
          ),
          tags$dd(
            style = "margin:0; color:#222;",
            as.character(rows$value[[i]] %||% "")[1]
          )
        )
      })
    )
  }
  foot <- if (!is.null(footnote) && nzchar(as.character(footnote)[1])) {
    tags$p(
      class = "ynow-value-why__foot",
      style = "margin:8px 0 0 0; color:#888; font-size:11.5px; line-height:1.4;",
      as.character(footnote)[1]
    )
  } else {
    NULL
  }
  tags$div(
    class = "ynow-value-why",
    style = paste0(
      "margin:10px 0 14px 0; padding:12px 14px; ",
      "border:1px solid #d9e2ec; border-left:4px solid #0C5484; ",
      "border-radius:4px; background:#f7fbff;"
    ),
    tags$div(
      class = "ynow-value-why__title",
      style = "margin:0 0 8px 0; font-size:13px; font-weight:700; color:#0C5484;",
      as.character(title %||% "")[1]
    ),
    body,
    foot
  )
}

#' Format a money-ish audit number for Why panels.
ynow_why_fmt_num <- function(x, digits = 2, prefix = "", suffix = "") {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) return("—")
  paste0(prefix, format(round(x, digits), big.mark = ",", scientific = FALSE), suffix)
}

#' Rows explaining the current DCF Fair Value from dcf_audit_snap + inputs.
#'
#' @return data.frame(label, value) or empty frame
ynow_dcf_why_rows <- function(snap,
                              claim = "fcff",
                              mode = "gordon",
                              g_pct = NA_real_,
                              locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  empty <- data.frame(label = character(0), value = character(0), stringsAsFactors = FALSE)
  if (is.null(snap) || !is.list(snap)) return(empty)

  .lab <- function(key, fallback) {
    tryCatch(ui_str(key, loc), error = function(e) fallback)
  }
  claim <- as.character(claim %||% "fcff")[1]
  mode <- as.character(mode %||% "gordon")[1]
  is_fcfe <- identical(claim, "fcfe")
  disc_lab <- if (is_fcfe) {
    .lab("explain_dcf_why_disc_ke", "Discount rate Ke")
  } else {
    .lab("explain_dcf_why_disc_wacc", "Discount rate WACC")
  }
  path_val <- if (is_fcfe) {
    .lab("explain_dcf_why_path_fcfe", "FCFE → Equity PV (Ke)")
  } else {
    .lab("explain_dcf_why_path_fcff", "FCFF → EV (WACC) → Equity bridge")
  }
  mode_val <- if (identical(mode, "two_stage") || identical(mode, "two-stage")) {
    .lab("explain_dcf_why_mode_two", "Two-Stage DCF")
  } else {
    .lab("explain_dcf_why_mode_gordon", "Gordon / single-stage DCF")
  }

  g_use <- suppressWarnings(as.numeric(g_pct)[1])
  if (!is.finite(g_use) && !is.null(snap$g)) {
    g_use <- suppressWarnings(as.numeric(snap$g)[1])
    if (is.finite(g_use) && abs(g_use) <= 1.5) g_use <- g_use * 100
  }
  disc <- suppressWarnings(as.numeric(snap$discount)[1])
  if (is.finite(disc) && abs(disc) <= 1.5) disc <- disc * 100

  rows <- list(
    c(.lab("explain_dcf_why_path", "Path"), path_val),
    c(.lab("explain_dcf_why_mode", "Model"), mode_val),
    c(
      .lab("explain_dcf_why_pv_explicit", "PV of explicit forecast"),
      ynow_why_fmt_num(snap$pv_explicit)
    ),
    c(
      .lab("explain_dcf_why_pv_tv", "PV of Terminal Value"),
      ynow_why_fmt_num(snap$pv_tv)
    ),
    c(
      disc_lab,
      if (is.finite(disc)) sprintf("%.2f%%", disc) else "—"
    ),
    c(
      .lab("explain_dcf_why_g", "Terminal g"),
      if (is.finite(g_use)) sprintf("%.2f%%", g_use) else "—"
    )
  )
  if (!is_fcfe) {
    rows <- c(
      rows,
      list(
        c(
          .lab("explain_dcf_why_cash", "Cash (EV→Equity)"),
          ynow_why_fmt_num(snap$cash)
        ),
        c(
          .lab("explain_dcf_why_debt", "Debt (EV→Equity)"),
          ynow_why_fmt_num(snap$debt)
        )
      )
    )
  }
  rows <- c(
    rows,
    list(
      c(
        .lab("explain_dcf_why_shares", "Shares"),
        ynow_why_fmt_num(snap$shares, digits = 0)
      ),
      c(
        .lab("explain_dcf_why_price", "Fair Value / share"),
        ynow_why_fmt_num(snap$price)
      )
    )
  )
  data.frame(
    label = vapply(rows, `[[`, character(1), 1L),
    value = vapply(rows, `[[`, character(1), 2L),
    stringsAsFactors = FALSE
  )
}
