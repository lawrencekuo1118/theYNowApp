# ==========================================
# relative_multiples_module.R
# Relative valuation: P/E, Forward P/E, PEG, EV/FCF
# Implied Price / relative indicator only — not Intrinsic Value.
# Reuses existing shares, FX/ADR guards, cash/debt bridge, FCFF reconstruction.
# ==========================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# ---------- Pure calculation helpers (no Shiny) ----------

.parse_summary_num <- function(x) {
  if (is.null(x) || length(x) < 1L) return(NA_real_)
  if (is.numeric(x)) {
    v <- suppressWarnings(as.numeric(x)[1])
    return(if (is.finite(v)) v else NA_real_)
  }
  s <- trimws(as.character(x)[1])
  if (!nzchar(s) || identical(toupper(s), "N/A") || identical(s, "—")) return(NA_real_)
  v <- suppressWarnings(as.numeric(gsub("[^0-9.eE+-]", "", s)))
  if (is.finite(v)) v else NA_real_
}

#' Trailing / forward P/E → Implied Price = EPS × multiple.
#' EPS ≤ 0 or missing → status unavailable (Missing ≠ 0).
calc_pe_implied_price <- function(eps, pe_multiple) {
  eps <- suppressWarnings(as.numeric(eps)[1])
  pe <- suppressWarnings(as.numeric(pe_multiple)[1])
  if (!is.finite(eps) || eps <= 0) {
    return(list(
      status = "unavailable", reason = "eps_nonpositive",
      implied_price = NA_real_, eps = eps, pe_multiple = pe
    ))
  }
  if (!is.finite(pe) || pe <= 0) {
    return(list(
      status = "unavailable", reason = "pe_invalid",
      implied_price = NA_real_, eps = eps, pe_multiple = pe
    ))
  }
  list(
    status = "ok", reason = NA_character_,
    implied_price = eps * pe, eps = eps, pe_multiple = pe
  )
}

#' Invert Forward EPS from price / forward P/E when explicit Forward EPS is missing.
calc_forward_eps_from_price_pe <- function(price, forward_pe) {
  px <- suppressWarnings(as.numeric(price)[1])
  fpe <- suppressWarnings(as.numeric(forward_pe)[1])
  if (!is.finite(px) || px <= 0 || !is.finite(fpe) || fpe <= 0) return(NA_real_)
  px / fpe
}

#' PEG = P/E ÷ (growth in percent units). Relative indicator only.
#' growth_pct must be the percent number (e.g. 12 for 12%), not a fraction.
#' growth ≤ 0 → NA. Does not label cheap/expensive.
calc_peg <- function(pe, growth_pct,
                     growth_definition = NA_character_,
                     growth_period = NA_character_) {
  pe <- suppressWarnings(as.numeric(pe)[1])
  g <- suppressWarnings(as.numeric(growth_pct)[1])
  if (!is.finite(pe) || pe <= 0) {
    return(list(
      status = "unavailable", reason = "pe_invalid",
      peg = NA_real_, pe = pe, growth_pct = g,
      growth_definition = as.character(growth_definition %||% NA_character_)[1],
      growth_period = as.character(growth_period %||% NA_character_)[1]
    ))
  }
  if (!is.finite(g) || g <= 0) {
    return(list(
      status = "unavailable", reason = "growth_nonpositive",
      peg = NA_real_, pe = pe, growth_pct = g,
      growth_definition = as.character(growth_definition %||% NA_character_)[1],
      growth_period = as.character(growth_period %||% NA_character_)[1]
    ))
  }
  list(
    status = "ok", reason = NA_character_,
    peg = pe / g, pe = pe, growth_pct = g,
    growth_definition = as.character(growth_definition %||% NA_character_)[1],
    growth_period = as.character(growth_period %||% NA_character_)[1]
  )
}

#' EV/FCF → Implied EV → Equity (existing cash−debt bridge) → Implied Price.
#' Uses enterprise-level FCF (FCFF). FCF ≤ 0 → unavailable.
calc_ev_fcf_implied_price <- function(fcf, ev_fcf_multiple, cash, debt, shares) {
  fcf <- suppressWarnings(as.numeric(fcf)[1])
  mult <- suppressWarnings(as.numeric(ev_fcf_multiple)[1])
  cash <- suppressWarnings(as.numeric(cash)[1])
  debt <- suppressWarnings(as.numeric(debt)[1])
  shares <- suppressWarnings(as.numeric(shares)[1])
  if (!is.finite(fcf) || fcf <= 0) {
    return(list(
      status = "unavailable", reason = "fcf_nonpositive",
      implied_ev = NA_real_, equity_value = NA_real_,
      implied_price = NA_real_, fcf = fcf, multiple = mult
    ))
  }
  if (!is.finite(mult) || mult <= 0) {
    return(list(
      status = "unavailable", reason = "multiple_invalid",
      implied_ev = NA_real_, equity_value = NA_real_,
      implied_price = NA_real_, fcf = fcf, multiple = mult
    ))
  }
  if (!is.finite(shares) || shares <= 0) {
    return(list(
      status = "unavailable", reason = "shares_missing",
      implied_ev = fcf * mult, equity_value = NA_real_,
      implied_price = NA_real_, fcf = fcf, multiple = mult
    ))
  }
  if (!is.finite(cash)) cash <- 0
  if (!is.finite(debt)) debt <- 0
  ev <- fcf * mult
  eq <- if (exists("dcf_ev_to_equity", mode = "function")) {
    dcf_ev_to_equity(ev, cash = cash, debt = debt)
  } else {
    ev + cash - debt
  }
  if (!is.finite(eq)) {
    return(list(
      status = "unavailable", reason = "equity_invalid",
      implied_ev = ev, equity_value = eq,
      implied_price = NA_real_, fcf = fcf, multiple = mult
    ))
  }
  list(
    status = "ok", reason = NA_character_,
    implied_ev = ev, equity_value = eq,
    implied_price = eq / shares,
    fcf = fcf, multiple = mult, cash = cash, debt = debt, shares = shares
  )
}

#' Historical revenue CAGR (%) from income statement (latest ← older).
.rel_rev_cagr_pct <- function(d_is, max_years = 5L) {
  if (is.null(d_is) || !is.data.frame(d_is) || !nrow(d_is)) return(NA_real_)
  rev <- tryCatch(
    select_clean_metric_row(
      d_is,
      "Total Revenue|^Revenue$|Operating Revenue",
      include_ttm = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(rev)) return(NA_real_)
  vals <- suppressWarnings(as.numeric(rev))
  vals <- vals[is.finite(vals) & vals > 0]
  if (length(vals) < 2L) return(NA_real_)
  n_keep <- min(length(vals), max(2L, as.integer(max_years)[1]))
  vals <- vals[seq_len(n_keep)]
  n_yr <- length(vals) - 1L
  if (n_yr < 1L) return(NA_real_)
  # Statement columns are newest-first in YNow convention
  newest <- vals[[1]]
  oldest <- vals[[length(vals)]]
  if (!is.finite(newest) || !is.finite(oldest) || oldest <= 0) return(NA_real_)
  ((newest / oldest)^(1 / n_yr) - 1) * 100
}

#' Latest enterprise FCFF from reconstruct_hist_fcff, else NA (do not pretend Yahoo FCF is FCFF).
.rel_latest_fcff <- function(d_cf, d_is = NULL) {
  if (exists("reconstruct_hist_fcff", mode = "function")) {
    rec <- tryCatch(reconstruct_hist_fcff(d_cf, d_is = d_is), error = function(e) NULL)
    if (!is.null(rec) && length(rec$fcff)) {
      v <- suppressWarnings(as.numeric(rec$fcff))
      hit <- which(is.finite(v))[1]
      if (length(hit) && is.finite(v[[hit]])) return(v[[hit]])
    }
  }
  NA_real_
}

# ==========================================
# UI
# ==========================================
relative_multiples_module_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "rel_multiples_calculator",
    fluidRow(
      column(
        12,
        div(
          id = "ynow_rel_multiples_lead",
          style = paste(
            "margin:0 0 12px 0; padding:10px 12px; background:#f8fafc;",
            "border:1px solid #e2e8f0; border-radius:8px; font-size:13px; color:#334155;"
          ),
          tags$b(id = "ynow_rel_multiples_lead_title", "Relative valuation (multiples): "),
          tags$span(
            id = "ynow_rel_multiples_lead_body",
            "Implied Price from selected multiples — not Intrinsic Value / Fair Value. ",
            "PEG is a relative indicator only."
          )
        )
      )
    ),
    tabBox(
      title = tags$span(id = "ynow_rel_multiples_box_title", "P/E · PEG · EV/FCF"),
      width = "auto",
      tabPanel(
        title = tags$span(id = "ynow_rel_multiples_tab_overview", "Overview"),
        icon = icon("percentage"),
        fluidRow(
          column(3, valueBoxOutput(ns("vbx_pe"), width = 12)),
          column(3, valueBoxOutput(ns("vbx_fpe"), width = 12)),
          column(3, valueBoxOutput(ns("vbx_peg"), width = 12)),
          column(3, valueBoxOutput(ns("vbx_evfcf"), width = 12))
        ),
        fluidRow(
          column(width = 6, ynow_calc_btn(ns("btn_calc_rel"), label = tags$span(id = "ynow_rel_multiples_btn_calc", "Run multiples"))),
          column(width = 6, ynow_reset_defaults_btn(ns("btn_reset_rel")))
        ),
        fluidRow(column(12, uiOutput(ns("ui_rel_result"))))
      ),
      tabPanel(
        title = tags$span(id = "ynow_rel_multiples_tab_inputs", "Inputs"),
        icon = icon("sliders-h"),
        h4(tags$b(id = "ynow_rel_multiples_pe_heading", "P/E & Forward P/E")),
        uiOutput(ns("txt_shares_note")),
        fluidRow(
          column(3, numericInput(ns("trailing_eps"), tags$span(id = "ynow_rel_lbl_teps", "Trailing EPS"), value = NA, step = 0.01)),
          column(3, numericInput(ns("pe_multiple"), tags$span(id = "ynow_rel_lbl_pe_mult", "Selected P/E"), value = APP_DEFAULTS$rel_pe_multiple %||% 18, min = 0.1, step = 0.5)),
          column(3, numericInput(ns("forward_eps"), tags$span(id = "ynow_rel_lbl_feps", "Forward EPS"), value = NA, step = 0.01)),
          column(3, numericInput(ns("fwd_pe_multiple"), tags$span(id = "ynow_rel_lbl_fpe_mult", "Selected Forward P/E"), value = APP_DEFAULTS$rel_fwd_pe_multiple %||% 18, min = 0.1, step = 0.5))
        ),
        tags$p(
          id = "ynow_rel_multiples_pe_help",
          class = "help-block",
          "Forward EPS is taken from Yahoo when available, else inverted from price ÷ Forward P/E. No forecasted EPS is invented. EPS ≤ 0 → P/E N/A."
        ),
        hr(),
        h4(tags$b(id = "ynow_rel_multiples_peg_heading", "PEG (relative indicator)")),
        fluidRow(
          column(
            4,
            selectInput(
              ns("peg_growth_src"),
              tags$span(id = "ynow_rel_lbl_peg_src", "Growth definition"),
              choices = c(
                "Central terminal SGR" = "sgr",
                "Historical revenue CAGR" = "rev_cagr"
              ),
              selected = "sgr"
            )
          ),
          column(4, numericInput(ns("peg_growth_pct"), tags$span(id = "ynow_rel_lbl_peg_g", "Growth (%)"), value = NA, step = 0.1)),
          column(4, numericInput(ns("peg_pe"), tags$span(id = "ynow_rel_lbl_peg_pe", "P/E for PEG"), value = NA, min = 0.1, step = 0.5))
        ),
        tags$p(
          id = "ynow_rel_multiples_peg_help",
          class = "help-block",
          "PEG = P/E ÷ growth(%). Growth period/definition are shown in results. Not a buy/sell threshold."
        ),
        hr(),
        h4(tags$b(id = "ynow_rel_multiples_evfcf_heading", "EV/FCF (enterprise FCFF)")),
        fluidRow(
          column(3, numericInput(ns("fcff"), tags$span(id = "ynow_rel_lbl_fcff", "FCFF (enterprise)"), value = NA, step = 1)),
          column(3, numericInput(ns("ev_fcf_multiple"), tags$span(id = "ynow_rel_lbl_evfcf_mult", "EV/FCF multiple"), value = APP_DEFAULTS$rel_ev_fcf_multiple %||% 15, min = 0.1, step = 0.5)),
          column(3, numericInput(ns("cash"), tags$span(id = "ynow_rel_lbl_cash", "Cash"), value = NA, step = 1)),
          column(3, numericInput(ns("debt"), tags$span(id = "ynow_rel_lbl_debt", "Total Debt"), value = NA, step = 1))
        ),
        fluidRow(
          column(4, numericInput(ns("shares"), tags$span(id = "ynow_rel_lbl_shares", "Shares (quote)"), value = NA, step = 1)),
          column(
            4,
            br(),
            actionButton(
              ns("btn_sync_rel"),
              label = tags$span(id = "ynow_rel_multiples_btn_sync", "Sync from statements"),
              icon = icon("sync"),
              class = "btn-sm",
              style = "background-color:#1a1a1a;color:white;border:none;padding:8px 15px;font-weight:bold;border-radius:5px;margin-top:5px;"
            )
          )
        ),
        tags$p(
          id = "ynow_rel_multiples_evfcf_help",
          class = "help-block",
          "Implied EV = FCFF × multiple; Equity = EV + Cash − Debt (same bridge as DCF); Implied Price = Equity ÷ shares. FCFE is not used here."
        )
      )
    )
  )
}

# ==========================================
# Server
# ==========================================
relative_multiples_module_server <- function(id,
                                            auto_calc_pulse = reactive(0L),
                                            summary_df = reactive(NULL),
                                            d_income_statement = reactive(NULL),
                                            d_balance_sheet = reactive(NULL),
                                            d_cash_flow = reactive(NULL),
                                            current_price = reactive(NA),
                                            market_cap = reactive(NA),
                                            quote_price = reactive(NA),
                                            current_ticker = reactive(""),
                                            quote_currency = reactive(NA),
                                            financial_currency = reactive(NA),
                                            central_sgr_pct = reactive(NA),
                                            ui_locale = reactive("zh-TW")) {
  moduleServer(id, function(input, output, session) {
    shares_resolve_note <- reactiveVal(NULL)
    calc_token <- reactiveVal(0L)
    last_result <- reactiveVal(NULL)

    .loc <- function() {
      tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    }
    .str <- function(key) ui_str(key, .loc())

    .quote_px <- function() {
      px <- suppressWarnings(as.numeric(quote_price())[1])
      if (!is.finite(px) || px <= 0) px <- suppressWarnings(as.numeric(current_price())[1])
      px
    }

    sync_from_statements <- function() {
      sum_df <- tryCatch(summary_df(), error = function(e) NULL)
      d_is <- tryCatch(d_income_statement(), error = function(e) NULL)
      d_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      d_cf <- tryCatch(d_cash_flow(), error = function(e) NULL)

      teps <- NA_real_
      if (exists("extract_summary_item", mode = "function")) {
        teps <- .parse_summary_num(extract_summary_item(
          sum_df, "EPS \\(TTM\\)|Trailing EPS|^EPS$", default = NA_character_
        ))
      }
      if (!is.finite(teps) && exists(".report_eps_bvps", mode = "function")) {
        teps <- tryCatch(.report_eps_bvps(sum_df, d_is = d_is, d_bs = d_bs)$eps, error = function(e) NA_real_)
      }

      mkt_pe <- NA_real_
      if (exists("extract_summary_item", mode = "function")) {
        mkt_pe <- .parse_summary_num(extract_summary_item(
          sum_df, "PE Ratio|Trailing P/E|P/E", default = NA_character_
        ))
      }

      fpe_mkt <- NA_real_
      if (exists("extract_summary_item", mode = "function")) {
        fpe_mkt <- .parse_summary_num(extract_summary_item(
          sum_df, "Forward P/E|Forward PE|forwardPE", default = NA_character_
        ))
      }
      # deep_scraper may expose forwardPE via summary Value rows with various labels
      feps <- NA_real_
      if (exists("extract_summary_item", mode = "function")) {
        feps <- .parse_summary_num(extract_summary_item(
          sum_df, "Forward EPS|Fwd EPS|forwardEps", default = NA_character_
        ))
      }
      if (!is.finite(feps)) {
        feps <- calc_forward_eps_from_price_pe(.quote_px(), fpe_mkt)
      }

      pe_def <- if (is.finite(mkt_pe) && mkt_pe > 0) {
        round(mkt_pe, 2)
      } else {
        APP_DEFAULTS$rel_pe_multiple %||% 18
      }
      fpe_def <- if (is.finite(fpe_mkt) && fpe_mkt > 0) {
        round(fpe_mkt, 2)
      } else {
        APP_DEFAULTS$rel_fwd_pe_multiple %||% pe_def
      }

      updateNumericInput(session, "trailing_eps", value = if (is.finite(teps)) round(teps, 4) else NA)
      updateNumericInput(session, "pe_multiple", value = pe_def)
      updateNumericInput(session, "forward_eps", value = if (is.finite(feps)) round(feps, 4) else NA)
      updateNumericInput(session, "fwd_pe_multiple", value = fpe_def)
      updateNumericInput(session, "peg_pe", value = pe_def)

      g_src <- as.character(input$peg_growth_src %||% "sgr")[1]
      g_pct <- if (identical(g_src, "rev_cagr")) {
        .rel_rev_cagr_pct(d_is)
      } else {
        suppressWarnings(as.numeric(central_sgr_pct())[1])
      }
      updateNumericInput(session, "peg_growth_pct", value = if (is.finite(g_pct)) round(g_pct, 2) else NA)

      fcff <- .rel_latest_fcff(d_cf, d_is = d_is)
      cash <- tryCatch(
        select_current_metric(
          d_bs,
          "Cash.*Equivalents.*Investments|Cash And Cash Equivalents|^Total Cash$",
          "stock"
        ),
        error = function(e) NA_real_
      )
      debt <- tryCatch(
        select_current_metric(d_bs, "^Total Debt$", "stock"),
        error = function(e) NA_real_
      )
      if (!is.finite(debt)) {
        st <- tryCatch(select_current_metric(d_bs, "Current Debt|Short Term Debt", "stock"), error = function(e) NA_real_)
        lt <- tryCatch(select_current_metric(d_bs, "Long Term Debt|^Long-Term Debt$", "stock"), error = function(e) NA_real_)
        if (is.finite(st) || is.finite(lt)) {
          debt <- sum(c(st, lt)[is.finite(c(st, lt))])
        }
      }

      shares_bs <- tryCatch(
        select_current_metric_any(d_bs, SHARE_PATTERNS, "stock"),
        error = function(e) NA_real_
      )
      px_quote <- .quote_px()
      mcap <- suppressWarnings(as.numeric(market_cap())[1])
      tk <- tryCatch(current_ticker(), error = function(e) "")
      sh_adj <- tryCatch(
        resolve_shares_for_price(
          shares_bs,
          price = px_quote,
          market_cap = mcap,
          ticker = tk,
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
        note <- if (!shares_auto_adjust_method(sh_adj$method %||% "")) {
          .str("rel_multiples_shares_basic")
        } else {
          NULL
        }
        shares_resolve_note(note)
      }

      updateNumericInput(session, "fcff", value = if (is.finite(fcff)) round(fcff, 2) else NA)
      updateNumericInput(session, "cash", value = if (is.finite(cash)) round(cash, 2) else NA)
      updateNumericInput(session, "debt", value = if (is.finite(debt)) round(debt, 2) else NA)
      updateNumericInput(session, "shares", value = if (is.finite(shares)) round(shares, 0) else NA)
      invisible(NULL)
    }

    observeEvent(
      list(
        summary_df(), d_income_statement(), d_balance_sheet(), d_cash_flow(),
        current_price(), quote_price(), market_cap(), current_ticker(),
        central_sgr_pct()
      ),
      { sync_from_statements() },
      ignoreInit = FALSE
    )
    observeEvent(input$btn_sync_rel, {
      sync_from_statements()
      showNotification(.str("rel_multiples_synced"), type = "message")
    })
    observeEvent(input$peg_growth_src, {
      d_is <- tryCatch(d_income_statement(), error = function(e) NULL)
      g_pct <- if (identical(as.character(input$peg_growth_src)[1], "rev_cagr")) {
        .rel_rev_cagr_pct(d_is)
      } else {
        suppressWarnings(as.numeric(central_sgr_pct())[1])
      }
      updateNumericInput(session, "peg_growth_pct", value = if (is.finite(g_pct)) round(g_pct, 2) else NA)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_reset_rel, {
      updateNumericInput(session, "pe_multiple", value = APP_DEFAULTS$rel_pe_multiple %||% 18)
      updateNumericInput(session, "fwd_pe_multiple", value = APP_DEFAULTS$rel_fwd_pe_multiple %||% 18)
      updateNumericInput(session, "ev_fcf_multiple", value = APP_DEFAULTS$rel_ev_fcf_multiple %||% 15)
      updateSelectInput(session, "peg_growth_src", selected = "sgr")
      sync_from_statements()
      last_result(NULL)
      calc_token(0L)
    })

    run_calc <- function() {
      g_src <- as.character(input$peg_growth_src %||% "sgr")[1]
      g_def <- if (identical(g_src, "rev_cagr")) {
        .str("rel_multiples_growth_rev_cagr")
      } else {
        .str("rel_multiples_growth_sgr")
      }
      g_period <- if (identical(g_src, "rev_cagr")) {
        .str("rel_multiples_period_hist_rev")
      } else {
        .str("rel_multiples_period_terminal_sgr")
      }
      pe <- calc_pe_implied_price(input$trailing_eps, input$pe_multiple)
      fpe <- calc_pe_implied_price(input$forward_eps, input$fwd_pe_multiple)
      if (identical(fpe$status, "unavailable") && identical(fpe$reason, "eps_nonpositive")) {
        fpe$reason <- "forward_eps_unavailable"
      }
      peg_pe <- suppressWarnings(as.numeric(input$peg_pe)[1])
      if (!is.finite(peg_pe) || peg_pe <= 0) {
        peg_pe <- suppressWarnings(as.numeric(input$pe_multiple)[1])
      }
      peg <- calc_peg(
        peg_pe, input$peg_growth_pct,
        growth_definition = g_def, growth_period = g_period
      )
      evf <- calc_ev_fcf_implied_price(
        input$fcff, input$ev_fcf_multiple, input$cash, input$debt, input$shares
      )
      list(pe = pe, forward_pe = fpe, peg = peg, ev_fcf = evf, ran_at = Sys.time())
    }

    observeEvent(input$btn_calc_rel, {
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

    .fmt_px <- function(x) {
      if (!is.finite(x)) return("—")
      format(round(x, 2), big.mark = ",", nsmall = 2)
    }

    output$vbx_pe <- renderValueBox({
      res <- last_result()
      val <- if (!is.null(res) && identical(res$pe$status, "ok")) .fmt_px(res$pe$implied_price) else "—"
      valueBox(val, .str("rel_multiples_vbx_pe"), icon = icon("chart-line"), color = "aqua")
    })
    output$vbx_fpe <- renderValueBox({
      res <- last_result()
      val <- if (!is.null(res) && identical(res$forward_pe$status, "ok")) {
        .fmt_px(res$forward_pe$implied_price)
      } else {
        "—"
      }
      valueBox(val, .str("rel_multiples_vbx_fpe"), icon = icon("binoculars"), color = "light-blue")
    })
    output$vbx_peg <- renderValueBox({
      res <- last_result()
      val <- if (!is.null(res) && identical(res$peg$status, "ok")) {
        sprintf("%.2f", res$peg$peg)
      } else {
        "—"
      }
      valueBox(val, .str("rel_multiples_vbx_peg"), icon = icon("balance-scale"), color = "yellow")
    })
    output$vbx_evfcf <- renderValueBox({
      res <- last_result()
      val <- if (!is.null(res) && identical(res$ev_fcf$status, "ok")) {
        .fmt_px(res$ev_fcf$implied_price)
      } else {
        "—"
      }
      valueBox(val, .str("rel_multiples_vbx_evfcf"), icon = icon("industry"), color = "teal")
    })

    output$ui_rel_result <- renderUI({
      calc_token()
      res <- last_result()
      if (is.null(res)) {
        return(tags$p(class = "ynow-macro-hint", .str("rel_multiples_need_run")))
      }
      .row <- function(model, status, detail) {
        tags$tr(
          tags$td(tags$b(model)),
          tags$td(status),
          tags$td(detail)
        )
      }
      pe_st <- if (identical(res$pe$status, "ok")) {
        .str("rel_multiples_status_ok")
      } else {
        .str("rel_multiples_status_na")
      }
      fpe_st <- if (identical(res$forward_pe$status, "ok")) {
        .str("rel_multiples_status_ok")
      } else if (identical(res$forward_pe$reason, "forward_eps_unavailable") ||
                 identical(res$forward_pe$reason, "eps_nonpositive")) {
        .str("rel_multiples_status_fwd_eps")
      } else {
        .str("rel_multiples_status_na")
      }
      peg_st <- if (identical(res$peg$status, "ok")) {
        .str("rel_multiples_status_ok")
      } else {
        .str("rel_multiples_status_na")
      }
      ev_st <- if (identical(res$ev_fcf$status, "ok")) {
        .str("rel_multiples_status_ok")
      } else if (identical(res$ev_fcf$reason, "fcf_nonpositive")) {
        .str("rel_multiples_status_fcf")
      } else {
        .str("rel_multiples_status_na")
      }
      tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-condensed table-striped",
          tags$thead(tags$tr(
            tags$th(.str("rel_multiples_col_model")),
            tags$th(.str("rel_multiples_col_status")),
            tags$th(.str("rel_multiples_col_detail"))
          )),
          tags$tbody(
            .row(
              .str("rel_multiples_model_pe"),
              pe_st,
              sprintf(
                "%s: %s · P/E %.2f× · EPS %s",
                .str("rel_multiples_implied_price"),
                .fmt_px(res$pe$implied_price),
                if (is.finite(res$pe$pe_multiple)) res$pe$pe_multiple else NA_real_,
                if (is.finite(res$pe$eps)) sprintf("%.4f", res$pe$eps) else "—"
              )
            ),
            .row(
              .str("rel_multiples_model_fpe"),
              fpe_st,
              sprintf(
                "%s: %s · Fwd P/E %.2f× · Fwd EPS %s",
                .str("rel_multiples_implied_price"),
                .fmt_px(res$forward_pe$implied_price),
                if (is.finite(res$forward_pe$pe_multiple)) res$forward_pe$pe_multiple else NA_real_,
                if (is.finite(res$forward_pe$eps)) sprintf("%.4f", res$forward_pe$eps) else "—"
              )
            ),
            .row(
              .str("rel_multiples_model_peg"),
              peg_st,
              sprintf(
                "PEG %s · P/E %.2f · g %.2f%% · %s · %s",
                if (is.finite(res$peg$peg)) sprintf("%.2f", res$peg$peg) else "—",
                if (is.finite(res$peg$pe)) res$peg$pe else NA_real_,
                if (is.finite(res$peg$growth_pct)) res$peg$growth_pct else NA_real_,
                res$peg$growth_definition %||% "—",
                res$peg$growth_period %||% "—"
              )
            ),
            .row(
              .str("rel_multiples_model_evfcf"),
              ev_st,
              sprintf(
                "%s: %s · EV %s · Equity %s · FCFF %s · %.2f×",
                .str("rel_multiples_implied_price"),
                .fmt_px(res$ev_fcf$implied_price),
                .fmt_px(res$ev_fcf$implied_ev),
                .fmt_px(res$ev_fcf$equity_value),
                if (is.finite(res$ev_fcf$fcf)) format(round(res$ev_fcf$fcf, 0), big.mark = ",") else "—",
                if (is.finite(res$ev_fcf$multiple)) res$ev_fcf$multiple else NA_real_
              )
            )
          )
        ),
        tags$p(
          class = "help-block",
          style = "margin-top:8px;",
          .str("rel_multiples_disclaimer")
        )
      )
    })

    return(list(
      pe_price = reactive({
        res <- last_result()
        if (!is.null(res) && identical(res$pe$status, "ok")) res$pe$implied_price else NA_real_
      }),
      forward_pe_price = reactive({
        res <- last_result()
        if (!is.null(res) && identical(res$forward_pe$status, "ok")) {
          res$forward_pe$implied_price
        } else {
          NA_real_
        }
      }),
      peg = reactive({
        res <- last_result()
        if (!is.null(res) && identical(res$peg$status, "ok")) res$peg$peg else NA_real_
      }),
      ev_fcf_price = reactive({
        res <- last_result()
        if (!is.null(res) && identical(res$ev_fcf$status, "ok")) {
          res$ev_fcf$implied_price
        } else {
          NA_real_
        }
      })
    ))
  })
}
