# =============================================================================
# SOTP module — structural Sum-of-the-Parts (revenue × per-segment EV/Sales)
# Independent sidebar engine (not a Multiples radio family).
# Reuses calc_sotp_revenue_implied / .rel_sotp_segments_from_is from
# relative_multiples_module.R (sourced first in global.R).
# Settings tabs mirror P/B: Overview + same-nature parameter pages.
# =============================================================================

.sotp_formula_banner <- function(id, text) {
  div(
    id = id,
    text,
    style = paste(
      "font-size:16px; font-weight:bold; color:#2C3E50; text-align:center;",
      "margin-bottom:15px; padding:10px; background-color:#F2F4F4; border-radius:8px;"
    )
  )
}
.sotp_settings_note <- function(id, text) {
  div(
    id = id,
    text,
    style = paste(
      "font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
      "margin-bottom:10px; padding:10px; background-color:#F8F9F9;",
      "border-left:4px solid #1a1a1a; border-radius:4px;"
    )
  )
}

sotp_module_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "sotp_calculator",
    fluidRow(
      column(
        12,
        div(
          id = "ynow_sotp_lead",
          style = paste(
            "margin:0 0 12px 0; padding:10px 12px; background:#f8fafc;",
            "border:1px solid #e2e8f0; border-radius:8px; font-size:13px; color:#334155;"
          ),
          tags$b(id = "ynow_sotp_lead_title", "SOTP (Sum of the Parts): "),
          tags$span(
            id = "ynow_sotp_lead_body",
            "Structural framework — decompose ≥2 segment revenues, apply an EV/Sales multiple per segment, ",
            "sum enterprise values, then bridge Cash − Debt to Implied Price. Not a single trading multiple."
          )
        )
      )
    ),
    tabBox(
      title = tags$span(id = "ynow_sotp_box_title", "SOTP"),
      width = "auto",

      # --- Overview ---
      tabPanel(
        title = tags$span(id = "ynow_sotp_tab_overview", "Overview"),
        icon = icon("puzzle-piece"),
        fluidRow(
          column(4, valueBoxOutput(ns("vbx_sotp"), width = 12)),
          column(4, valueBoxOutput(ns("vbx_sotp_ev"), width = 12)),
          column(4, valueBoxOutput(ns("vbx_sotp_n"), width = 12))
        ),
        .sotp_formula_banner(
          "ynow_sotp_formula_banner",
          "Implied EV = Σ(Seg Rev × EV/Sales) + Non-op　｜　Equity = EV + Cash − Debt　｜　Price = Equity ÷ Shares"
        ),
        fluidRow(
          column(width = 4, ynow_calc_btn(ns("btn_calc_sotp"), label = tags$span(id = "ynow_sotp_btn_calc", "Run SOTP"))),
          column(width = 4, ynow_reset_defaults_btn(ns("btn_reset_sotp"))),
          column(
            width = 4,
            br(),
            actionButton(
              ns("btn_sync_sotp"),
              label = tags$span(id = "ynow_sotp_btn_sync", "Sync from statements"),
              icon = icon("sync"), class = "btn-sm",
              style = "background-color:#1a1a1a;color:white;border:none;padding:8px 15px;font-weight:bold;border-radius:5px;margin-top:5px;width:100%;"
            )
          )
        ),
        fluidRow(column(12, uiOutput(ns("ui_sotp_result"))))
      ),

      # --- Segments ---
      tabPanel(
        title = tags$span(id = "ynow_sotp_tab_segments", "Segments"),
        icon = icon("table"),
        h4(tags$b(id = "ynow_sotp_segments_heading_ui", "Segment EV/Sales")),
        .sotp_settings_note(
          "ynow_sotp_settings_seg_note",
          "Segment EV = Segment Revenue × Segment EV/Sales. Requires ≥2 positive segment revenues. Not segment-EBIT SOTP."
        ),
        tags$p(
          id = "ynow_sotp_help",
          class = "help-block",
          "Each segment needs its own EV/Sales. Sync pulls multi-segment revenue from BB Lab when available. Not segment-EBIT SOTP."
        ),
        fluidRow(
          column(4, numericInput(
            ns("default_ev_sales"),
            tags$span(id = "ynow_sotp_lbl_default_mult", "Default EV/Sales"),
            value = APP_DEFAULTS$rel_ev_sales_multiple %||% 3, min = 0.01, step = 0.1
          )),
          column(
            4,
            br(),
            actionButton(
              ns("btn_apply_default_mult"),
              label = tags$span(id = "ynow_sotp_btn_apply_mult", "Apply default to all segments"),
              class = "btn-sm",
              style = "background-color:#334155;color:white;border:none;padding:8px 12px;font-weight:bold;border-radius:5px;margin-top:5px;"
            )
          ),
          column(4, numericInput(
            ns("nonop"),
            tags$span(id = "ynow_sotp_lbl_nonop", "Non-operating assets"),
            value = 0, step = 1
          ))
        ),
        tags$p(
          id = "ynow_sotp_nonop_help",
          class = "help-block",
          "Non-operating assets are added to Σ(segment EV) before the Cash − Debt bridge."
        ),
        uiOutput(ns("ui_segment_inputs"))
      ),

      # --- Bridge ---
      tabPanel(
        title = tags$span(id = "ynow_sotp_tab_bridge", "Bridge"),
        icon = icon("link"),
        h4(tags$b(id = "ynow_sotp_bridge_heading", "Capital bridge & shares")),
        .sotp_settings_note(
          "ynow_sotp_settings_bridge_note",
          "Equity = Implied EV + Cash − Debt (same bridge as DCF); Implied Price = Equity ÷ Shares."
        ),
        uiOutput(ns("txt_shares_note")),
        fluidRow(
          column(4, numericInput(ns("cash"), tags$span(id = "ynow_sotp_lbl_cash", "Cash"), value = NA, step = 1)),
          column(4, numericInput(ns("debt"), tags$span(id = "ynow_sotp_lbl_debt", "Total Debt"), value = NA, step = 1)),
          column(4, numericInput(ns("shares"), tags$span(id = "ynow_sotp_lbl_shares", "Shares (quote)"), value = NA, step = 1))
        ),
        tags$p(
          id = "ynow_sotp_bridge_help",
          class = "help-block",
          "Implied EV = Σ(segment revenue × segment EV/Sales) + non-operating; Equity = EV + Cash − Debt (same bridge as DCF)."
        )
      )
    )
  )
}

sotp_module_server <- function(id,
                               auto_calc_pulse = reactive(0L),
                               d_income_statement = reactive(NULL),
                               d_balance_sheet = reactive(NULL),
                               current_price = reactive(NA),
                               market_cap = reactive(NA),
                               quote_price = reactive(NA),
                               current_ticker = reactive(""),
                               quote_currency = reactive(NA),
                               financial_currency = reactive(NA),
                               ui_locale = reactive("zh-TW")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shares_resolve_note <- reactiveVal(NULL)
    # name, revenue, multiple
    segments_rv <- reactiveVal(
      data.frame(
        name = character(0), revenue = numeric(0), multiple = numeric(0),
        stringsAsFactors = FALSE
      )
    )
    calc_token <- reactiveVal(0L)
    last_result <- reactiveVal(NULL)

    .loc <- function() tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .str <- function(key) ui_str(key, .loc())
    .quote_px <- function() {
      px <- suppressWarnings(as.numeric(quote_price())[1])
      if (!is.finite(px) || px <= 0) px <- suppressWarnings(as.numeric(current_price())[1])
      px
    }
    .default_mult <- function() {
      m <- suppressWarnings(as.numeric(input$default_ev_sales)[1])
      if (!is.finite(m) || m <= 0) m <- APP_DEFAULTS$rel_ev_sales_multiple %||% 3
      m
    }

    .collect_segment_multiples <- function(base) {
      if (!is.data.frame(base) || !nrow(base)) return(base)
      out <- base
      def <- .default_mult()
      for (i in seq_len(nrow(out))) {
        key <- paste0("seg_mult_", i)
        v <- suppressWarnings(as.numeric(input[[key]])[1])
        if (!is.finite(v) || v <= 0) v <- if (is.finite(out$multiple[[i]]) && out$multiple[[i]] > 0) out$multiple[[i]] else def
        out$multiple[[i]] <- v
      }
      out
    }

    sync_from_statements <- function() {
      d_is <- tryCatch(d_income_statement(), error = function(e) NULL)
      d_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      cash <- tryCatch(
        select_current_metric(d_bs, "Cash.*Equivalents.*Investments|Cash And Cash Equivalents|^Total Cash$", "stock"),
        error = function(e) NA_real_
      )
      debt <- tryCatch(select_current_metric(d_bs, "^Total Debt$", "stock"), error = function(e) NA_real_)
      if (!is.finite(debt)) {
        st <- tryCatch(select_current_metric(d_bs, "Current Debt|Short Term Debt", "stock"), error = function(e) NA_real_)
        lt <- tryCatch(select_current_metric(d_bs, "Long Term Debt|^Long-Term Debt$", "stock"), error = function(e) NA_real_)
        if (is.finite(st) || is.finite(lt)) debt <- sum(c(st, lt)[is.finite(c(st, lt))])
      }
      shares_bs <- tryCatch(select_current_metric_any(d_bs, SHARE_PATTERNS, "stock"), error = function(e) NA_real_)
      px_quote <- .quote_px()
      mcap <- suppressWarnings(as.numeric(market_cap())[1])
      tk <- tryCatch(current_ticker(), error = function(e) "")
      sh_adj <- tryCatch(
        resolve_shares_for_price(
          shares_bs, price = px_quote, market_cap = mcap, ticker = tk,
          quote_currency = tryCatch(quote_currency(), error = function(e) NULL),
          financial_currency = tryCatch(financial_currency(), error = function(e) NULL)
        ),
        error = function(e) list(shares = NA_real_, method = "none", note = NULL)
      )
      auto_adj <- shares_auto_adjust_method(sh_adj$method)
      q_ccy0 <- tryCatch(quote_currency(), error = function(e) NA_character_)
      f_ccy0 <- tryCatch(financial_currency(), error = function(e) NA_character_)
      shares <- NA_real_
      if (isTRUE(auto_adj) && is.finite(sh_adj$shares) && sh_adj$shares > 0) {
        shares <- sh_adj$shares
        shares_resolve_note(sh_adj$note)
      } else if (isTRUE(adr_conversion_required(
        infer_statement_currency(f_ccy0, q_ccy0, sh_adj$method), q_ccy0, sh_adj$method
      )) || isTRUE(fx_conversion_required(
        infer_statement_currency(f_ccy0, q_ccy0, sh_adj$method), q_ccy0
      ))) {
        shares <- NA_real_
        shares_resolve_note(.str("rel_multiples_shares_fx_block"))
      } else {
        shares <- if (is.finite(shares_bs) && shares_bs > 0) shares_bs else sh_adj$shares
        shares_resolve_note(
          if (!shares_auto_adjust_method(sh_adj$method %||% "")) .str("rel_multiples_shares_basic") else NULL
        )
      }

      def <- .default_mult()
      # Same BB Lab path as Business Breakdown: income statement + US SEC segment notes.
      notes <- if (exists(".rel_sotp_fetch_segment_notes", mode = "function")) {
        .rel_sotp_fetch_segment_notes(tk)
      } else {
        NULL
      }
      segs <- .rel_sotp_segments_from_is(
        d_is, ticker = tk, statement_currency = f_ccy0, notes = notes
      )
      if (is.data.frame(segs) && nrow(segs)) {
        segs$multiple <- def
      }
      segments_rv(segs)

      updateNumericInput(session, "cash", value = if (is.finite(cash)) round(cash, 2) else NA)
      updateNumericInput(session, "debt", value = if (is.finite(debt)) round(debt, 2) else NA)
      updateNumericInput(session, "shares", value = if (is.finite(shares)) round(shares, 0) else NA)
      invisible(NULL)
    }

    observeEvent(
      list(d_income_statement(), d_balance_sheet(), current_price(), quote_price(),
           market_cap(), current_ticker()),
      { sync_from_statements() },
      ignoreInit = FALSE
    )
    observeEvent(input$btn_sync_sotp, {
      sync_from_statements()
      showNotification(.str("sotp_synced"), type = "message")
    })
    observeEvent(input$btn_apply_default_mult, {
      seg <- segments_rv()
      if (!is.data.frame(seg) || !nrow(seg)) return()
      def <- .default_mult()
      seg$multiple <- def
      segments_rv(seg)
      for (i in seq_len(nrow(seg))) {
        updateNumericInput(session, paste0("seg_mult_", i), value = def)
      }
    })
    observeEvent(input$btn_reset_sotp, {
      updateNumericInput(session, "default_ev_sales", value = APP_DEFAULTS$rel_ev_sales_multiple %||% 3)
      updateNumericInput(session, "nonop", value = 0)
      sync_from_statements()
      last_result(NULL)
      calc_token(0L)
    })

    run_calc <- function() {
      seg <- .collect_segment_multiples(segments_rv())
      calc_sotp_revenue_implied(
        seg,
        ev_sales_multiple = .default_mult(),
        cash = input$cash,
        debt = input$debt,
        shares = input$shares,
        non_operating = input$nonop
      )
    }
    observeEvent(input$btn_calc_sotp, {
      last_result(run_calc())
      calc_token(as.integer(calc_token()) + 1L)
    })
    observeEvent(auto_calc_pulse(), {
      if (isTRUE(as.numeric(auto_calc_pulse())[1] > 0)) {
        last_result(run_calc())
        calc_token(as.integer(calc_token()) + 1L)
      }
    }, ignoreInit = TRUE)

    output$txt_shares_note <- renderUI({
      note <- shares_resolve_note()
      if (!nzchar(note %||% "")) return(NULL)
      tags$p(class = "help-block", style = "color:#9a3412;", note)
    })

    output$ui_segment_inputs <- renderUI({
      seg <- segments_rv()
      if (!is.data.frame(seg) || nrow(seg) < 2L) {
        return(tags$div(
          class = "ynow-macro-callout ynow-macro-callout--warn",
          tags$p(.str("sotp_need_segments"))
        ))
      }
      def <- .default_mult()
      rows <- lapply(seq_len(nrow(seg)), function(i) {
        m0 <- suppressWarnings(as.numeric(seg$multiple[[i]])[1])
        if (!is.finite(m0) || m0 <= 0) m0 <- def
        fluidRow(
          column(4, tags$p(style = "margin-top:25px;font-weight:600;", seg$name[[i]])),
          column(4, tags$p(
            style = "margin-top:25px;",
            sprintf("%s: %s", .str("sotp_col_rev"), format(round(seg$revenue[[i]], 0), big.mark = ","))
          )),
          column(4, numericInput(
            ns(paste0("seg_mult_", i)),
            .str("sotp_col_multiple"),
            value = m0, min = 0.01, step = 0.1
          ))
        )
      })
      tags$div(
        tags$p(tags$b(.str("sotp_segments_heading"))),
        do.call(tagList, rows)
      )
    })

    .fmt_px <- function(x) {
      if (!is.finite(x)) return("—")
      format(round(x, 2), big.mark = ",", nsmall = 2)
    }

    output$vbx_sotp <- renderValueBox({
      res <- last_result()
      val <- if (!is.null(res) && identical(res$status, "ok")) .fmt_px(res$implied_price) else "—"
      valueBox(val, .str("sotp_vbx_price"), icon = icon("puzzle-piece"), color = "black")
    })
    output$vbx_sotp_ev <- renderValueBox({
      res <- last_result()
      val <- if (!is.null(res) && identical(res$status, "ok")) .fmt_px(res$implied_ev) else "—"
      valueBox(val, .str("sotp_vbx_ev"), icon = icon("building"), color = "navy")
    })
    output$vbx_sotp_n <- renderValueBox({
      res <- last_result()
      val <- if (!is.null(res) && identical(res$status, "ok")) as.character(as.integer(res$n_segments)) else "—"
      valueBox(val, .str("sotp_vbx_n"), icon = icon("layer-group"), color = "teal")
    })

    .sotp_unavailable_msg <- function(reason) {
      key <- switch(
        as.character(reason %||% "")[1],
        no_segments = "sotp_need_segments",
        need_multi_segment = "sotp_need_segments",
        shares_missing = "sotp_status_shares_missing",
        multiple_invalid = "sotp_status_multiple_invalid",
        equity_invalid = "sotp_status_equity_invalid",
        "sotp_status_unavailable"
      )
      .str(key)
    }

    output$ui_sotp_result <- renderUI({
      calc_token()
      res <- last_result()
      if (is.null(res)) {
        return(tags$p(class = "ynow-macro-hint", .str("sotp_need_run")))
      }
      if (!identical(res$status, "ok")) {
        return(tags$div(
          class = "ynow-macro-callout ynow-macro-callout--warn",
          tags$p(.sotp_unavailable_msg(res$reason))
        ))
      }
      seg <- res$segments
      seg_rows <- lapply(seq_len(nrow(seg)), function(i) {
        tags$tr(
          tags$td(seg$name[[i]]),
          tags$td(format(round(seg$revenue[[i]], 0), big.mark = ",")),
          tags$td(sprintf("%.2f×", seg$multiple[[i]])),
          tags$td(.fmt_px(seg$value[[i]]))
        )
      })
      tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-condensed table-striped",
          tags$thead(tags$tr(
            tags$th(.str("sotp_col_name")),
            tags$th(.str("sotp_col_rev")),
            tags$th(.str("sotp_col_multiple")),
            tags$th(.str("sotp_col_value"))
          )),
          tags$tbody(seg_rows)
        ),
        tags$p(
          class = "help-block",
          sprintf(
            "%s: %s · EV %s · Equity %s · Cash %s · Debt %s · shares %s",
            .str("rel_multiples_implied_price"),
            .fmt_px(res$implied_price), .fmt_px(res$implied_ev), .fmt_px(res$equity_value),
            .fmt_px(res$cash), .fmt_px(res$debt),
            if (is.finite(res$shares)) format(round(res$shares, 0), big.mark = ",") else "—"
          )
        ),
        tags$p(class = "help-block", .str("sotp_disclaimer"))
      )
    })

    return(list(
      sotp_price = reactive({
        res <- last_result()
        if (!is.null(res) && identical(res$status, "ok")) res$implied_price else NA_real_
      })
    ))
  })
}
