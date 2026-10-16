# Four sidebar pages (enterprise / earnings / revenue). Book value stays pb_asset_module.
# Page chrome matches P/B: Overview (KPI row, formula bar, Run / Reset) + Settings.
# input['mod_rel-rel_mode'] remains so a hidden mode radio can steer the active Implied Price.
.rel_formula_banner <- function(id, text) {
  div(
    id = id,
    text,
    style = paste(
      "font-size:16px; font-weight:bold; color:#2C3E50; text-align:center;",
      "margin-bottom:15px; padding:10px; background-color:#F2F4F4; border-radius:8px;"
    )
  )
}
.rel_settings_note <- function(id, text) {
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

.rel_run_row <- function(ns, calc_id, reset_id, sync_id) {
  fluidRow(
    column(width = 4, ynow_calc_btn(ns(calc_id), label = tags$span(class = "ynow-rel-calc-lbl", "Run multiples"))),
    column(width = 4, ynow_reset_defaults_btn(ns(reset_id))),
    column(
      width = 4,
      br(),
      actionButton(
        ns(sync_id),
        label = tags$span(class = "ynow-rel-sync-lbl", "Sync from statements"),
        icon = icon("sync"), class = "btn-sm",
        style = "background-color:#1a1a1a;color:white;border:none;padding:8px 15px;font-weight:bold;border-radius:5px;margin-top:5px;width:100%;"
      )
    )
  )
}

relative_multiples_module_ui <- function(id) {
  ns <- NS(id)
  .mode <- function(val) sprintf("input['mod_rel-rel_mode'] == '%s'", val)
  tagList(
    div(
      style = "display:none;",
      radioButtons(
        ns("rel_mode"),
        label = tags$span(id = "ynow_rel_mode_label", "Select capital-claim family:"),
        choices = list(
          "Equity (P/E · Fwd P/E · PEG · P/S)" = "equity",
          "Enterprise (EV/FCF · EV/EBIT · EV/EBITDA · EV/Sales · EV/ARR)" = "enterprise",
          "Revenue (P/S · EV/Sales · EV/ARR)" = "revenue"
        ),
        selected = .rel_normalize_mode(APP_DEFAULTS$rel_mode %||% "equity")
      ),
      tags$p(id = "ynow_rel_mode_help", .mode("equity"))
    ),
    tabItem(
      tabName = "rel_mult_ev",
      tabBox(
        title = tags$span(id = "ynow_rel_ev_box", "Enterprise value multiples"),
        width = "auto",
        tabPanel(
          title = tags$span(id = "ynow_rel_multiples_tab_enterprise", "Overview"),
          icon = icon("building"),
          fluidRow(
            class = "ynow-model-kpi-row",
            column(4, class = "ynow-model-kpi-result", valueBoxOutput(ns("vbx_evebitda"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-2", valueBoxOutput(ns("vbx_evebit"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-3", valueBoxOutput(ns("vbx_evfcf"), width = 12))
          ),
          .rel_formula_banner(
            "ynow_rel_formula_enterprise",
            "Implied EV = Metric × Multiple　｜　Equity = EV + Cash − Debt　｜　Price = Equity ÷ Shares"
          ),
          .rel_run_row(ns, "btn_calc_rel_ev", "btn_reset_rel_ev", "btn_sync_rel_ev")
        ),
        tabPanel(
          "Settings",
          icon = icon("cogs"),
          h4(tags$b(id = "ynow_rel_multiples_ev_heading", "Enterprise multiples")),
          .rel_settings_note(
            "ynow_rel_settings_ev_note",
            "Implied EV = Metric × Multiple; Equity = EV + Cash − Debt; Implied Price = Equity ÷ Shares. EV/EBITDA is the cross-capital-structure comparison; it fails when EBITDA is negative."
          ),
          fluidRow(
            column(3, numericInput(ns("ebitda"), tags$span(id = "ynow_rel_lbl_ebitda", "EBITDA"), value = NA, step = 1)),
            column(3, numericInput(ns("ev_ebitda_multiple"), tags$span(id = "ynow_rel_lbl_evebitda_mult", "EV/EBITDA"), value = APP_DEFAULTS$rel_ev_ebitda_multiple %||% 10, min = 0.1, step = 0.5)),
            column(3, numericInput(ns("ebit"), tags$span(id = "ynow_rel_lbl_ebit", "EBIT"), value = NA, step = 1)),
            column(3, numericInput(ns("ev_ebit_multiple"), tags$span(id = "ynow_rel_lbl_evebit_mult", "EV/EBIT"), value = APP_DEFAULTS$rel_ev_ebit_multiple %||% 12, min = 0.1, step = 0.5))
          ),
          fluidRow(
            column(3, numericInput(ns("fcff"), tags$span(id = "ynow_rel_lbl_fcff", "FCFF"), value = NA, step = 1)),
            column(3, numericInput(ns("ev_fcf_multiple"), tags$span(id = "ynow_rel_lbl_evfcf_mult", "EV/FCF"), value = APP_DEFAULTS$rel_ev_fcf_multiple %||% 15, min = 0.1, step = 0.5))
          ),
          tags$p(id = "ynow_rel_multiples_ev_help", class = "help-block", "FCFE is not used. EV/Sales and EV/ARR live on the revenue page."),
          hr(style = "border-top:1px solid #BDC3C7;"),
          h4(tags$b(id = "ynow_rel_multiples_tab_bridge", "Capital bridge & shares")),
          .rel_settings_note(
            "ynow_rel_settings_bridge_note",
            "Shared with revenue multiples and SOTP: Equity = EV + Cash − Debt; Price = Equity ÷ Shares."
          ),
          uiOutput(ns("txt_shares_note")),
          fluidRow(
            column(4, numericInput(ns("cash"), tags$span(id = "ynow_rel_lbl_cash", "Cash"), value = NA, step = 1)),
            column(4, numericInput(ns("debt"), tags$span(id = "ynow_rel_lbl_debt", "Total Debt"), value = NA, step = 1)),
            column(4, numericInput(ns("shares"), tags$span(id = "ynow_rel_lbl_shares", "Shares (quote)"), value = NA, step = 1))
          ),
          tags$p(
            id = "ynow_rel_multiples_bridge_help", class = "help-block",
            "Cash / Debt used for the EV→Equity bridge. Earnings P/E does not use this bridge."
          )
        )
      )
    ),
    tabItem(
      tabName = "rel_multiples_calculator",
      tabBox(
        title = tags$span(id = "ynow_rel_earn_box", "Earnings multiples"),
        width = "auto",
        tabPanel(
          title = tags$span(id = "ynow_rel_multiples_tab_equity", "Overview"),
          icon = icon("chart-line"),
          fluidRow(
            class = "ynow-model-kpi-row",
            column(4, class = "ynow-model-kpi-result", valueBoxOutput(ns("vbx_pe"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-2", valueBoxOutput(ns("vbx_fpe"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-3", valueBoxOutput(ns("vbx_peg"), width = 12))
          ),
          .rel_formula_banner(
            "ynow_rel_formula_equity",
            "Implied Price = EPS × P/E　｜　PEG = P/E ÷ g(%)"
          ),
          .rel_run_row(ns, "btn_calc_rel", "btn_reset_rel", "btn_sync_rel"),
          fluidRow(column(12, uiOutput(ns("ui_rel_result")))),
          uiOutput(ns("ui_rel_param_matrix"))
        ),
        tabPanel(
          "Settings",
          icon = icon("cogs"),
          h4(tags$b(id = "ynow_rel_multiples_pe_heading", "P/E & Forward P/E")),
          .rel_settings_note(
            "ynow_rel_settings_pe_note",
            "Implied Price = Trailing EPS × Selected P/E; Forward Implied Price = Forward EPS × Selected Forward P/E."
          ),
          fluidRow(
            column(3, numericInput(ns("trailing_eps"), tags$span(id = "ynow_rel_lbl_teps", "Trailing EPS"), value = NA, step = 0.01)),
            column(3, numericInput(ns("pe_multiple"), tags$span(id = "ynow_rel_lbl_pe_mult", "Selected P/E"), value = APP_DEFAULTS$rel_pe_multiple %||% 18, min = 0.1, step = 0.5)),
            column(3, numericInput(ns("forward_eps"), tags$span(id = "ynow_rel_lbl_feps", "Forward EPS"), value = NA, step = 0.01)),
            column(3, numericInput(ns("fwd_pe_multiple"), tags$span(id = "ynow_rel_lbl_fpe_mult", "Selected Forward P/E"), value = APP_DEFAULTS$rel_fwd_pe_multiple %||% 18, min = 0.1, step = 0.5))
          ),
          tags$p(
            id = "ynow_rel_multiples_pe_help", class = "help-block",
            "Trailing EPS from latest statements; Forward EPS = Trailing × (1+g) using SGR／revenue CAGR. Selected P/E from App industry defaults — not Yahoo market P/E or price÷P/E back-solve. EPS ≤ 0 → N/A."
          ),
          hr(style = "border-top:1px solid #BDC3C7;"),
          h4(tags$b(id = "ynow_rel_multiples_peg_heading", "PEG (relative indicator)")),
          .rel_settings_note(
            "ynow_rel_settings_peg_note",
            "PEG = P/E ÷ growth(%). Relative indicator only — not a buy/sell threshold."
          ),
          fluidRow(
            column(4, selectInput(
              ns("peg_growth_src"),
              tags$span(id = "ynow_rel_lbl_peg_src", "Growth definition"),
              choices = c("Central terminal SGR" = "sgr", "Historical revenue CAGR" = "rev_cagr"),
              selected = "sgr"
            )),
            column(4, numericInput(ns("peg_growth_pct"), tags$span(id = "ynow_rel_lbl_peg_g", "Growth (%)"), value = NA, step = 0.1)),
            column(4, numericInput(ns("peg_pe"), tags$span(id = "ynow_rel_lbl_peg_pe", "P/E for PEG"), value = NA, min = 0.1, step = 0.5))
          ),
          tags$p(id = "ynow_rel_multiples_peg_help", class = "help-block", "PEG = P/E ÷ growth(%). Not a buy/sell threshold.")
        )
      )
    ),
    tabItem(
      tabName = "rel_mult_rev",
      tabBox(
        title = tags$span(id = "ynow_rel_rev_box", "Revenue multiples"),
        width = "auto",
        tabPanel(
          "Overview",
          icon = icon("shopping-cart"),
          fluidRow(
            class = "ynow-model-kpi-row",
            column(4, class = "ynow-model-kpi-result", valueBoxOutput(ns("vbx_ps"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-2", valueBoxOutput(ns("vbx_evsales"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-3", valueBoxOutput(ns("vbx_evarr"), width = 12))
          ),
          .rel_formula_banner(
            "ynow_rel_formula_revenue",
            "P/S: Implied Equity = Revenue × P/S　｜　EV/Sales: Equity = EV + Cash − Debt"
          ),
          .rel_run_row(ns, "btn_calc_rel_rev", "btn_reset_rel_rev", "btn_sync_rel_rev")
        ),
        tabPanel(
          "Settings",
          icon = icon("cogs"),
          h4(tags$b(id = "ynow_rel_multiples_ps_heading", "P/S (equity sales)")),
          .rel_settings_note(
            "ynow_rel_settings_ps_note",
            "Implied Equity = Revenue × P/S; Implied Price = Equity ÷ Shares (no Cash−Debt bridge). Use when earnings and EBITDA are not usable. Do not compare unlike gross margins with the same P/S."
          ),
          fluidRow(
            column(4, numericInput(ns("ps_revenue"), tags$span(id = "ynow_rel_lbl_ps_revenue", "Revenue"), value = NA, step = 1)),
            column(4, numericInput(ns("ps_multiple"), tags$span(id = "ynow_rel_lbl_ps_mult", "P/S"), value = APP_DEFAULTS$rel_ps_multiple %||% 3, min = 0.01, step = 0.1))
          ),
          tags$p(id = "ynow_rel_multiples_ps_help", class = "help-block", "Equity-side: Implied Equity = Revenue × P/S; Implied Price = Equity ÷ shares. No Cash−Debt bridge (unlike EV/Sales)."),
          tags$p(id = "ynow_rel_multiples_ps_rev_note", class = "help-block", "P/S Revenue syncs with Enterprise Revenue when you Sync from statements; you may override either field."),
          hr(style = "border-top:1px solid #BDC3C7;"),
          h4(tags$b("EV/Sales & EV/ARR")),
          fluidRow(
            column(3, numericInput(ns("revenue"), tags$span(id = "ynow_rel_lbl_revenue", "Revenue"), value = NA, step = 1)),
            column(3, numericInput(ns("ev_sales_multiple"), tags$span(id = "ynow_rel_lbl_evsales_mult", "EV/Sales"), value = APP_DEFAULTS$rel_ev_sales_multiple %||% 3, min = 0.01, step = 0.1)),
            column(3, numericInput(ns("arr"), tags$span(id = "ynow_rel_lbl_arr", "ARR"), value = NA, step = 1)),
            column(3, numericInput(ns("ev_arr_multiple"), tags$span(id = "ynow_rel_lbl_evarr_mult", "EV/ARR"), value = APP_DEFAULTS$rel_ev_arr_multiple %||% 10, min = 0.1, step = 0.5))
          ),
          tags$p(id = "ynow_rel_multiples_arr_help", class = "help-block", "ARR is not in core statements — enter manually or leave blank (N/A). Cash / Debt / Shares are on the enterprise-value Settings page.")
        )
      )
    )
  )
}
