# ==========================================
# explainability.R — first-class「可解釋性」panels (not appendix)
# Compact Why-this-value cards for valuation results + decision gates.
# Schema for every key number: formula path, inputs, data source, gate rationale.
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
      style = paste0(
        "margin:0; display:grid; grid-template-columns:minmax(7em,32%) 1fr; ",
        "gap:4px 12px; font-size:12.5px; line-height:1.45;"
      ),
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

ynow_why_lab <- function(key, fallback, locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  tryCatch(ui_str(key, loc), error = function(e) fallback)
}

ynow_why_empty_df <- function() {
  data.frame(label = character(0), value = character(0), stringsAsFactors = FALSE)
}

ynow_why_df_from_pairs <- function(rows) {
  if (is.null(rows) || !length(rows)) return(ynow_why_empty_df())
  data.frame(
    label = vapply(rows, `[[`, character(1), 1L),
    value = vapply(rows, `[[`, character(1), 2L),
    stringsAsFactors = FALSE
  )
}

#' Default statement / quote data-source gloss (bilingual via locale keys).
ynow_explain_data_source <- function(locale = "zh-TW") {
  ynow_why_lab(
    "explain_data_source_value",
    "Yahoo Finance / exchange filings (TWSE·TPEx when TW)",
    locale
  )
}

#' Append shared Path-meta rows: Data source (+ optional gate note).
ynow_why_append_meta <- function(rows,
                                 locale = "zh-TW",
                                 gate_note = NULL,
                                 include_source = TRUE) {
  loc <- as.character(locale %||% "zh-TW")[1]
  out <- if (is.null(rows)) list() else rows
  if (isTRUE(include_source)) {
    out <- c(
      out,
      list(c(
        ynow_why_lab("explain_data_source", "Data source", loc),
        ynow_explain_data_source(loc)
      ))
    )
  }
  gn <- as.character(gate_note %||% "")[1]
  if (nzchar(gn)) {
    out <- c(
      out,
      list(c(
        ynow_why_lab("explain_gate_rationale", "Gate rationale", loc),
        gn
      ))
    )
  }
  out
}

#' Rows explaining the current DCF Fair Value from dcf_audit_snap + inputs.
ynow_dcf_why_rows <- function(snap,
                              claim = "fcff",
                              mode = "gordon",
                              g_pct = NA_real_,
                              locale = "zh-TW",
                              gate_note = NULL) {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  if (is.null(snap) || !is.list(snap)) return(ynow_why_empty_df())

  .lab <- function(key, fallback) ynow_why_lab(key, fallback, loc)
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
    c(disc_lab, if (is.finite(disc)) sprintf("%.2f%%", disc) else "—"),
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
  ynow_why_df_from_pairs(ynow_why_append_meta(rows, loc, gate_note = gate_note))
}

#' DDM Why rows (Gordon / SPM / two-stage).
ynow_ddm_why_rows <- function(mode = "gordon",
                              ke_pct = NA_real_,
                              g_pct = NA_real_,
                              d0 = NA_real_,
                              eps = NA_real_,
                              price = NA_real_,
                              locale = "zh-TW",
                              gate_note = NULL) {
  loc <- as.character(locale %||% "zh-TW")[1]
  .lab <- function(key, fallback) ynow_why_lab(key, fallback, loc)
  mode <- as.character(mode %||% "gordon")[1]
  mode_val <- switch(
    mode,
    "spm" = .lab("explain_ddm_mode_spm", "SPM (EPS + fixed dividend)"),
    "two_stage" = .lab("explain_ddm_mode_two", "Two-Stage DDM"),
    .lab("explain_ddm_mode_gordon", "Gordon DDM")
  )
  path_val <- switch(
    mode,
    "spm" = .lab("explain_ddm_path_spm", "V0 = EPS×(1−b)/(Ke−g) + D/Ke"),
    "two_stage" = .lab("explain_ddm_path_two", "High-g dividends → Terminal Gordon"),
    .lab("explain_ddm_path_gordon", "D1 / (Ke − g)")
  )
  ke <- suppressWarnings(as.numeric(ke_pct)[1])
  g <- suppressWarnings(as.numeric(g_pct)[1])
  rows <- list(
    c(.lab("explain_dcf_why_path", "Path"), path_val),
    c(.lab("explain_dcf_why_mode", "Model"), mode_val),
    c(
      .lab("explain_ddm_ke", "Discount rate Ke"),
      if (is.finite(ke)) sprintf("%.2f%%", ke) else "—"
    ),
    c(
      .lab("explain_ddm_g", "Dividend / earnings g"),
      if (is.finite(g)) sprintf("%.2f%%", g) else "—"
    )
  )
  if (identical(mode, "spm")) {
    rows <- c(rows, list(c(
      .lab("explain_ddm_eps", "EPS"),
      ynow_why_fmt_num(eps)
    )))
  } else {
    rows <- c(rows, list(c(
      .lab("explain_ddm_d0", "D0 / dividend"),
      ynow_why_fmt_num(d0)
    )))
  }
  rows <- c(rows, list(c(
    .lab("explain_dcf_why_price", "Fair Value / share"),
    ynow_why_fmt_num(price)
  )))
  ynow_why_df_from_pairs(ynow_why_append_meta(rows, loc, gate_note = gate_note))
}

#' Residual Income Why rows.
ynow_ri_why_rows <- function(b0 = NA_real_,
                             ke_pct = NA_real_,
                             roe_pct = NA_real_,
                             g_pct = NA_real_,
                             pv_ri = NA_real_,
                             tv = NA_real_,
                             price = NA_real_,
                             locale = "zh-TW",
                             gate_note = NULL) {
  loc <- as.character(locale %||% "zh-TW")[1]
  .lab <- function(key, fallback) ynow_why_lab(key, fallback, loc)
  rows <- list(
    c(
      .lab("explain_dcf_why_path", "Path"),
      .lab("explain_ri_path", "B0 + PV of RI + Terminal Value")
    ),
    c(
      .lab("explain_dcf_why_mode", "Model"),
      .lab("explain_ri_mode", "Residual Income (clean-surplus)")
    ),
    c(.lab("explain_ri_b0", "Book equity B0 / share"), ynow_why_fmt_num(b0)),
    c(
      .lab("explain_ri_roe", "ROE"),
      {
        r <- suppressWarnings(as.numeric(roe_pct)[1])
        if (is.finite(r)) sprintf("%.2f%%", r) else "—"
      }
    ),
    c(
      .lab("explain_ddm_ke", "Discount rate Ke"),
      {
        k <- suppressWarnings(as.numeric(ke_pct)[1])
        if (is.finite(k)) sprintf("%.2f%%", k) else "—"
      }
    ),
    c(
      .lab("explain_dcf_why_g", "Terminal g"),
      {
        g <- suppressWarnings(as.numeric(g_pct)[1])
        if (is.finite(g)) sprintf("%.2f%%", g) else "—"
      }
    ),
    c(.lab("explain_ri_pv", "PV of residual income"), ynow_why_fmt_num(pv_ri)),
    c(.lab("explain_ri_tv", "Terminal Value"), ynow_why_fmt_num(tv)),
    c(.lab("explain_dcf_why_price", "Fair Value / share"), ynow_why_fmt_num(price))
  )
  ynow_why_df_from_pairs(ynow_why_append_meta(rows, loc, gate_note = gate_note))
}

#' Justified / target P/B Why rows.
ynow_pb_why_rows <- function(mode = "justified",
                             book = NA_real_,
                             target_pb = NA_real_,
                             roe_pct = NA_real_,
                             ke_pct = NA_real_,
                             g_pct = NA_real_,
                             price = NA_real_,
                             locale = "zh-TW",
                             gate_note = NULL) {
  loc <- as.character(locale %||% "zh-TW")[1]
  .lab <- function(key, fallback) ynow_why_lab(key, fallback, loc)
  mode <- as.character(mode %||% "justified")[1]
  is_just <- identical(mode, "justified")
  rows <- list(
    c(
      .lab("explain_dcf_why_path", "Path"),
      .lab("explain_pb_path", "Book / share × Target P/B")
    ),
    c(
      .lab("explain_dcf_why_mode", "Model"),
      if (is_just) {
        .lab("explain_pb_mode_justified", "Justified P/B (ROE, Ke, g)")
      } else {
        .lab("explain_pb_mode_multiple", "Target P/B multiple")
      }
    ),
    c(.lab("explain_pb_book", "Book / share (BVPS/TBVPS/NAVPS)"), ynow_why_fmt_num(book)),
    c(.lab("explain_pb_target", "Target P/B"), ynow_why_fmt_num(target_pb))
  )
  if (is_just) {
    rows <- c(
      rows,
      list(
        c(
          .lab("explain_ri_roe", "ROE"),
          {
            r <- suppressWarnings(as.numeric(roe_pct)[1])
            if (is.finite(r)) sprintf("%.2f%%", r) else "—"
          }
        ),
        c(
          .lab("explain_ddm_ke", "Discount rate Ke"),
          {
            k <- suppressWarnings(as.numeric(ke_pct)[1])
            if (is.finite(k)) sprintf("%.2f%%", k) else "—"
          }
        ),
        c(
          .lab("explain_dcf_why_g", "Terminal g"),
          {
            g <- suppressWarnings(as.numeric(g_pct)[1])
            if (is.finite(g)) sprintf("%.2f%%", g) else "—"
          }
        )
      )
    )
  }
  rows <- c(rows, list(c(
    .lab("explain_dcf_why_price", "Fair Value / share"),
    ynow_why_fmt_num(price)
  )))
  ynow_why_df_from_pairs(ynow_why_append_meta(rows, loc, gate_note = gate_note))
}

#' NAV Why rows.
ynow_nav_why_rows <- function(navps = NA_real_,
                              multiple = NA_real_,
                              holdco_pct = NA_real_,
                              price = NA_real_,
                              locale = "zh-TW",
                              gate_note = NULL) {
  loc <- as.character(locale %||% "zh-TW")[1]
  .lab <- function(key, fallback) ynow_why_lab(key, fallback, loc)
  rows <- list(
    c(
      .lab("explain_dcf_why_path", "Path"),
      .lab("explain_nav_path", "NAVPS × NAV multiple (book HoldCo)")
    ),
    c(
      .lab("explain_dcf_why_mode", "Model"),
      .lab("explain_nav_mode", "Net Asset Value")
    ),
    c(.lab("explain_nav_navps", "NAVPS"), ynow_why_fmt_num(navps)),
    c(.lab("explain_nav_multiple", "NAV multiple"), ynow_why_fmt_num(multiple)),
    c(
      .lab("explain_nav_holdco", "HoldCo discount"),
      {
        h <- suppressWarnings(as.numeric(holdco_pct)[1])
        if (is.finite(h)) sprintf("%.2f%%", h) else "—"
      }
    ),
    c(.lab("explain_dcf_why_price", "Fair Value / share"), ynow_why_fmt_num(price))
  )
  ynow_why_df_from_pairs(ynow_why_append_meta(rows, loc, gate_note = gate_note))
}

#' Decision Checklist / SOP gate rationale rows (HFV stays veto-only).
ynow_decision_gate_why_rows <- function(sop_gate = NULL, locale = "zh-TW") {
  loc <- as.character(locale %||% "zh-TW")[1]
  .lab <- function(key, fallback) ynow_why_lab(key, fallback, loc)
  if (is.null(sop_gate) || !is.list(sop_gate)) return(ynow_why_empty_df())

  unlocked <- isTRUE(sop_gate$unlocked)
  reason <- as.character(sop_gate$reason %||% "")[1]
  unlock_val <- if (unlocked) {
    .lab("explain_gate_unlocked", "Unlocked — mandatory gates passed")
  } else {
    switch(
      reason,
      "acks" = .lab("explain_gate_locked_acks", "Locked — SOP acknowledgements incomplete"),
      "gates_fail" = .lab("explain_gate_locked_fail", "Locked — a mandatory gate failed"),
      "gates_na" = .lab("explain_gate_locked_na", "Locked — a mandatory gate has no data"),
      .lab("explain_gate_locked_default", "Locked — Decision SOP not satisfied")
    )
  }
  rows <- list(
    c(.lab("explain_gate_status", "Decision SOP"), unlock_val)
  )
  mandatory <- sop_gate$mandatory
  if (is.list(mandatory) && length(mandatory)) {
    for (res in mandatory) {
      if (!is.list(res)) next
      item <- as.character(res$item %||% "")[1]
      st <- as.character(res$status %||% "")[1]
      detail <- as.character(res$detail %||% "")[1]
      item_lab <- switch(
        item,
        "g_sgr" = .lab("explain_gate_item_g_sgr", "g vs SGR"),
        "fscore" = .lab("explain_gate_item_fscore", "F-Score"),
        "bear_base" = .lab("explain_gate_item_bear_base", "Bear / Base MOS"),
        "hfv_veto" = .lab("explain_gate_item_hfv", "HFV veto (not a buy signal)"),
        item
      )
      st_lab <- switch(
        st,
        "pass" = .lab("explain_gate_pass", "Pass"),
        "fail" = .lab("explain_gate_fail", "Fail"),
        "na" = .lab("explain_gate_na", "N/A"),
        st
      )
      val <- if (nzchar(detail)) paste0(st_lab, " — ", detail) else st_lab
      rows <- c(rows, list(c(item_lab, val)))
    }
  }
  ynow_why_df_from_pairs(rows)
}

#' Stack primary FV why + decision gate why into one Smart Analysis surface.
ynow_smart_explain_ui <- function(fv_title,
                                  fv_rows,
                                  fv_empty,
                                  fv_foot,
                                  gate_title,
                                  gate_rows,
                                  gate_empty,
                                  gate_foot = NULL) {
  tagList(
    ynow_value_why_panel(
      title = fv_title,
      rows = fv_rows,
      empty_message = fv_empty,
      footnote = fv_foot
    ),
    ynow_value_why_panel(
      title = gate_title,
      rows = gate_rows,
      empty_message = gate_empty,
      footnote = gate_foot
    )
  )
}
