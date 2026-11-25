# ==========================================
# pb_asset_module.R — Asset-Based Valuation
# Three independent sub-models (own multiple each):
#   Book Value Valuation     — BVPS  × Target P/B
#   Tangible Book Valuation  — TBVPS × Target P/TBV
#   Net Asset Value Valuation— NAVPS × Target P/NAV
#   (no MTM → Adjusted BVPS, not NAVPS)
# Justified P/B (ROE、Ke、SGR／g) primarily feeds the BVPS multiple;
# industry / history can seed each mode’s band. Pure NAV (no multiple) stays
# the separate NAV model — same family, different route.
# ==========================================

# ==========================================
# 🖥️ 前端 UI
# ==========================================
pb_asset_module_shell_ui <- function(id) {
  ns <- NS(id)
  # Sub-model radio sits above the whole「P/B & ASSET VALUE」block (DDM-style
  # chrome), so Overview / Settings / Target all see the same choice.
  tagList(
    tabBox(
      width = "auto",
      tabPanel(
        "",
        tags$div(
          class = "ynow-pb-basis-chrome",
          radioButtons(
            ns("basis"),
            label = ui_i18n("pb_basis_label"),
            choices = c(
              "Book Value (BVPS × P/B)" = "bvps",
              "Tangible Book (TBVPS × P/TBV)" = "tbvps",
              "Net Asset Value (NAVPS × P/NAV)" = "navps"
            ),
            selected = APP_DEFAULTS$pb_basis
          ),
          tags$p(
            id = ns("ynow_pb_basis_help"),
            class = "help-block",
            ui_i18n("pb_basis_help")
          ),
          uiOutput(ns("ui_basis_recommend"))
        )
      )
    ),
    # Overview is a left sub-tab of this Settings tabBox (not a separate body sibling).
    tabBox(
      title = "P/B & ASSET VALUE", width = "auto",
      tabPanel(
        "P/B Overview", value = "pb_overview", icon = icon("landmark"),
        fluidRow(
          class = "ynow-model-kpi-row",
          column(3, class = "ynow-model-kpi-result ynow-model-kpi-outline", `data-ynow-lock` = "output:pb_fair_value", valueBoxOutput(ns("vbx_fair"), width = 12)),
          column(3, class = "ynow-model-kpi-tone-2", valueBoxOutput(ns("vbx_bvps"), width = 12)),
          column(3, class = "ynow-model-kpi-tone-3", valueBoxOutput(ns("vbx_tbvps"), width = 12)),
          column(3, class = "ynow-model-kpi-tone-4", valueBoxOutput(ns("vbx_mkt_pb"), width = 12))
        ),
        uiOutput(ns("value_why")),
        fluidRow(
          div(
            id = ns("ynow_pb_formula_banner"),
            ui_i18n("pb_formula_banner"),
            style = "font-size: 16px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
          )
        ),
        ynow_calc_reset_row(
          ynow_calc_btn(ns("btn_calc_pb"), "試算 Jst. P/B"),
          ynow_reset_defaults_btn(ns("btn_reset_pb"))
        ),
        fluidRow(
          column(width = 12,
                 uiOutput(ns("ui_pb_result")),
                 br(),
                 box(title = ui_i18n("pb_band_title"), width = 12, status = "primary", solidHeader = TRUE,
                     tableOutput(ns("tbl_pb_band")),
                     plotOutput(ns("plt_pb_band"), height = "280px")
                 )
          )
        )
      ),
      tabPanel(
        "P/B Settings", value = "pb_settings", icon = icon("cogs"),
        tags$p(
          class = "ynow-pb-settings-lead help-block",
          ui_i18n("pb_settings_lead")
        ),
      fluidRow(
        column(
          12,
          actionButton(
            ns("btn_sync_bv"),
            tagList(icon("sync"), tags$span(ui_i18n("pb_sync_bv"))),
            class = "btn-sm",
            style = paste(
              "background-color:#1a1a1a; color:white; border:none;",
              "padding:8px 15px; font-weight:bold; border-radius:5px; margin-bottom:10px;"
            )
          )
        )
      ),
      uiOutput(ns("txt_shares_resolve_note")),
      fluidRow(column(12, uiOutput(ns("alert_missing_bv")))),
      # --- BVPS module (visible only when basis == bvps) ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'bvps'", ns("basis")),
        tags$div(
          class = "ynow-pb-param-section",
          `data-ynow-pb-section` = "bvps",
          tags$h4(class = "ynow-pb-param-section-title", tags$b(ui_i18n("pb_section_bvps_title"))),
          tags$div(class = "ynow-pb-param-formula", ui_i18n("pb_section_bvps_formula")),
          tags$div(class = "ynow-pb-param-fair", ui_i18n("pb_section_bvps_fair")),
          tags$p(class = "help-block ynow-pb-param-note", ui_i18n("pb_components_help")),
          fluidRow(
            column(4, numericInput(
              ns("common_equity"), ui_i18n("pb_common_equity_label"),
              value = NA, step = 1, min = 0
            )),
            column(4, numericInput(
              ns("shares"), ui_i18n("pb_shares_label"),
              value = NA, step = 1, min = 0
            )),
            column(4, numericInput(
              ns("bvps"), ui_i18n("pb_bvps_label"),
              value = APP_DEFAULTS$pb_bvps, step = 0.1, min = 0
            ))
          )
        )
      ),
      # --- TBVPS module ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'tbvps'", ns("basis")),
        tags$div(
          class = "ynow-pb-param-section",
          `data-ynow-pb-section` = "tbvps",
          tags$h4(class = "ynow-pb-param-section-title", tags$b(ui_i18n("pb_section_tbvps_title"))),
          tags$div(class = "ynow-pb-param-formula", ui_i18n("pb_section_tbvps_formula")),
          tags$div(class = "ynow-pb-param-fair", ui_i18n("pb_section_tbvps_fair")),
          tags$p(class = "help-block ynow-pb-param-note", ui_i18n("pb_section_tbvps_note")),
          tags$p(class = "help-block ynow-pb-param-note", ui_i18n("pb_components_help")),
          fluidRow(
            column(4, numericInput(
              ns("tb_common_equity"), ui_i18n("pb_common_equity_label"),
              value = NA, step = 1, min = 0
            )),
            column(4, numericInput(
              ns("tb_shares"), ui_i18n("pb_shares_label"),
              value = NA, step = 1, min = 0
            )),
            column(4, numericInput(
              ns("tbvps"), ui_i18n("pb_tbvps_label"),
              value = APP_DEFAULTS$pb_tbvps, step = 0.1, min = 0
            ))
          ),
          fluidRow(
            column(4, numericInput(
              ns("goodwill"), ui_i18n("pb_goodwill_label"),
              value = NA, step = 1, min = 0
            )),
            column(4, numericInput(
              ns("intangibles"), ui_i18n("pb_intangibles_label"),
              value = NA, step = 1, min = 0
            ))
          )
        )
      ),
      # --- NAVPS / Adjusted BVPS module ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'navps'", ns("basis")),
        tags$div(
          class = "ynow-pb-param-section",
          `data-ynow-pb-section` = "navps",
          tags$h4(class = "ynow-pb-param-section-title", tags$b(ui_i18n("pb_section_navps_title"))),
          tags$div(class = "ynow-pb-param-formula", ui_i18n("pb_section_navps_formula")),
          tags$div(class = "ynow-pb-param-fair", ui_i18n("pb_section_navps_fair")),
          tags$p(class = "help-block ynow-pb-param-note", ui_i18n("pb_components_help")),
          uiOutput(ns("ui_nav_metric_note")),
          fluidRow(
            column(4, numericInput(
              ns("nav_common_equity"), ui_i18n("pb_common_equity_label"),
              value = NA, step = 1, min = 0
            )),
            column(4, numericInput(
              ns("nav_shares"), ui_i18n("pb_shares_label"),
              value = NA, step = 1, min = 0
            )),
            column(4, numericInput(
              ns("navps"), ui_i18n("pb_navps_label"),
              value = NA, step = 0.1, min = 0
            ))
          ),
          fluidRow(
            column(4, numericInput(
              ns("nav_delta_asset"), ui_i18n("pb_delta_asset_label"),
              value = NA, step = 1
            )),
            column(4, numericInput(
              ns("nav_delta_liability"), ui_i18n("pb_delta_liability_label"),
              value = NA, step = 1
            )),
            column(4, numericInput(
              ns("investments"), ui_i18n("pb_investments_label"),
              value = NA, step = 1, min = 0
            ))
          ),
          fluidRow(
            column(4, numericInput(
              ns("holdco_discount"), ui_i18n("pb_holdco_label"),
              value = APP_DEFAULTS$pb_holdco_discount * 100,
              min = 0, max = 50, step = 1
            )),
            column(8, checkboxInput(
              ns("nav_apply_holdco"),
              ui_i18n("pb_nav_apply_holdco"),
              value = isTRUE(APP_DEFAULTS$pb_nav_apply_holdco)
            ))
          ),
          tags$p(class = "help-block", ui_i18n("pb_holdco_help")),
          tableOutput(ns("tbl_nav_breakdown"))
        )
      )
    ),
    tabPanel(
      "目標倍數", value = "target_pb", icon = icon("bullseye"),
      h4(tags$b(ui_i18n("pb_target_tab_title"))),
      tags$p(class = "help-block", ui_i18n("pb_target_tab_lead")),
      radioButtons(
        ns("target_mode"),
        label = tags$span(style = "font-weight:bold;", ui_i18n("pb_target_mode_label")),
        choices = c(
          "產業／歷史倍數（無需 SGR）" = "multiples",
          "Justified P/B（需 ROE、Ke、SGR／g；主要給 BVPS）" = "justified"
        ),
        selected = APP_DEFAULTS$pb_target_mode %||% "justified",
        inline = TRUE
      ),
      uiOutput(ns("ui_target_mode_help")),
      # Each sub-model owns its multiple band — not a shared Target P/B.
      conditionalPanel(
        condition = sprintf("input['%s'] == 'bvps'", ns("basis")),
        fluidRow(
          column(4, numericInput(ns("pb_low"),  ui_i18n("pb_mult_pb_low"),  value = APP_DEFAULTS$pb_low,  step = 0.05, min = 0.1)),
          column(4, numericInput(ns("pb_mid"),  ui_i18n("pb_mult_pb_mid"),  value = APP_DEFAULTS$pb_mid,  step = 0.05, min = 0.1)),
          column(4, numericInput(ns("pb_high"), ui_i18n("pb_mult_pb_high"), value = APP_DEFAULTS$pb_high, step = 0.05, min = 0.1))
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'tbvps'", ns("basis")),
        fluidRow(
          column(4, numericInput(ns("ptbv_low"),  ui_i18n("pb_mult_ptbv_low"),  value = APP_DEFAULTS$ptbv_low,  step = 0.05, min = 0.1)),
          column(4, numericInput(ns("ptbv_mid"),  ui_i18n("pb_mult_ptbv_mid"),  value = APP_DEFAULTS$ptbv_mid,  step = 0.05, min = 0.1)),
          column(4, numericInput(ns("ptbv_high"), ui_i18n("pb_mult_ptbv_high"), value = APP_DEFAULTS$ptbv_high, step = 0.05, min = 0.1))
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'navps'", ns("basis")),
        fluidRow(
          column(4, numericInput(ns("pnav_low"),  ui_i18n("pb_mult_pnav_low"),  value = APP_DEFAULTS$pnav_low,  step = 0.05, min = 0.1)),
          column(4, numericInput(ns("pnav_mid"),  ui_i18n("pb_mult_pnav_mid"),  value = APP_DEFAULTS$pnav_mid,  step = 0.05, min = 0.1)),
          column(4, numericInput(ns("pnav_high"), ui_i18n("pb_mult_pnav_high"), value = APP_DEFAULTS$pnav_high, step = 0.05, min = 0.1))
        )
      ),
      uiOutput(ns("ui_pb_source_note")),
      fluidRow(
        column(12,
               checkboxInput(ns("use_industry_pb"),
                             tags$span(style = "font-weight: bold;", ui_i18n("pb_use_industry")),
                             value = APP_DEFAULTS$pb_use_industry)
        )
      ),
      fluidRow(
        column(12,
               tags$p(
                 id = ns("ynow_pb_settings_reset_hint"),
                 style = "margin-top: 10px; color: #7f8c8d; font-size: 12px;",
                 ui_i18n("pb_settings_reset_hint")
               )
        )
      )
    )
    )
  )
}

pb_asset_module_body_ui <- function(id) {
  ns <- NS(id)
  # Overview lives in the shell Settings tabBox; body keeps formula sensitivity only.
  tagList(
    .model_param_sensitivity_box(
      "P/B 公式參數：每股估值貢獻與敏感度",
      ns("param_sensitivity_table")
    )
  )
}

pb_asset_module_ui <- function(id) {
  .ynow_wave2_tab_item("pb_calculator")
}

# ==========================================
# ⚙️ 後端 Server
# ==========================================
pb_asset_module_server <- function(id,
                                   auto_calc_pulse = reactive(0L),
                                   d_balance_sheet,
                                   d_income_statement = reactive(NULL),
                                   current_price = reactive(NA),
                                   market_cap = reactive(NA),
                                   quote_price = reactive(NA),
                                   current_ticker = reactive(""),
                                   quote_currency = reactive(NA),
                                   financial_currency = reactive(NA),
                                   industry_choice = reactive(NULL),
                                   industry_text = reactive(""),
                                   central_ke = reactive(NA),
                                   central_g_pct = reactive(NA),
                                   hist_prices = reactive(NULL),
                                   capm_rf = reactive(NA),
                                   capm_beta = reactive(NA),
                                   capm_rm = reactive(NA),
                                   use_estimated_re = reactive(FALSE),
                                   ui_locale = reactive("zh-TW"), integrity_ready = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    .echo_guard <- ynow_input_echo_guard(input, session, current_ticker, function() input$basis %||% "bvps")
    updateNumericInput <- .echo_guard$update


    .loc <- function() {
      tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    }
    
    nav_shares <- reactiveVal(NA_real_)
    nav_components <- reactiveVal(NULL)
    basis_manual <- reactiveVal(FALSE)
    last_recommend <- reactiveVal(NULL)

    .pb_basis_val <- function() {
      basis <- as.character(input$basis %||% "bvps")[1]
      if (identical(basis, "tbvps")) return(safe_num(input$tbvps))
      if (identical(basis, "navps")) return(safe_num(input$navps))
      safe_num(input$bvps)
    }
    .pb_has_mtm_inputs <- function() {
      .pb_has_mtm(input$nav_delta_asset, input$nav_delta_liability)
    }
    .pb_nav_kind <- function() {
      .pb_nav_metric_kind(input$nav_delta_asset, input$nav_delta_liability)
    }
    .pb_basis_label <- function(basis = NULL) {
      b <- as.character((basis %||% input$basis) %||% "bvps")[1]
      if (identical(b, "tbvps")) return("TBVPS")
      if (identical(b, "navps")) {
        return(if (identical(.pb_nav_kind(), "adjusted_bvps")) "Adjusted BVPS" else "NAVPS")
      }
      "BVPS"
    }
    .pb_active_multiple_name <- function(basis = NULL) {
      .pb_multiple_name(basis %||% input$basis %||% "bvps")
    }
    .pb_active_multiples <- function(basis = NULL) {
      b <- as.character((basis %||% input$basis) %||% "bvps")[1]
      if (identical(b, "tbvps")) {
        return(list(
          low = safe_num(input$ptbv_low),
          mid = safe_num(input$ptbv_mid),
          high = safe_num(input$ptbv_high)
        ))
      }
      if (identical(b, "navps")) {
        return(list(
          low = safe_num(input$pnav_low),
          mid = safe_num(input$pnav_mid),
          high = safe_num(input$pnav_high)
        ))
      }
      list(
        low = safe_num(input$pb_low),
        mid = safe_num(input$pb_mid),
        high = safe_num(input$pb_high)
      )
    }
    .pb_write_multiples <- function(basis, low, mid, high) {
      b <- as.character(basis %||% "bvps")[1]
      if (identical(b, "tbvps")) {
        updateNumericInput(session, "ptbv_low", value = round(low, 2))
        updateNumericInput(session, "ptbv_mid", value = round(mid, 2))
        updateNumericInput(session, "ptbv_high", value = round(high, 2))
      } else if (identical(b, "navps")) {
        updateNumericInput(session, "pnav_low", value = round(low, 2))
        updateNumericInput(session, "pnav_mid", value = round(mid, 2))
        updateNumericInput(session, "pnav_high", value = round(high, 2))
      } else {
        updateNumericInput(session, "pb_low", value = round(low, 2))
        updateNumericInput(session, "pb_mid", value = round(mid, 2))
        updateNumericInput(session, "pb_high", value = round(high, 2))
      }
    }

    .pb_money_to_quote <- function(amount, df_bs, share_method = "balance_sheet") {
      amt <- suppressWarnings(as.numeric(amount)[1])
      if (!is.finite(amt)) return(NA_real_)
      q_ccy <- tryCatch(quote_currency(), error = function(e) NA_character_)
      f_ccy <- tryCatch(financial_currency(), error = function(e) NA_character_)
      eq_ccy <- equity_money_ccy(df_bs, session_ccy = q_ccy, statement_ccy = f_ccy)
      per_share_in_quote(
        amt, 1,
        equity_ccy = eq_ccy,
        to_ccy = q_ccy,
        share_method = share_method,
        statement_ccy = f_ccy
      )
    }

    # Recompute only the named mode(s). Never rewrite another basis's per-share
    # field — a manual BVPS override must survive goodwill / HoldCo edits.
    .pb_recompute_from_parts <- function(modes = "bvps") {
      modes <- unique(as.character(modes %||% character()))
      if ("bvps" %in% modes) {
        bv <- .pb_bvps_from_parts(input$common_equity, input$shares)
        if (is.finite(bv)) updateNumericInput(session, "bvps", value = round(bv, 2))
      }
      if ("tbvps" %in% modes) {
        tb <- .pb_tbvps_from_parts(
          input$tb_common_equity, input$goodwill, input$intangibles, input$tb_shares
        )
        if (is.finite(tb)) updateNumericInput(session, "tbvps", value = round(tb, 2))
      }
      if ("navps" %in% modes) {
        disc_pct <- suppressWarnings(as.numeric(input$holdco_discount)[1])
        if (!is.finite(disc_pct)) disc_pct <- APP_DEFAULTS$pb_holdco_discount * 100
        apply_hc <- if (.pb_has_mtm_inputs()) isTRUE(input$nav_apply_holdco) else TRUE
        nv <- .pb_navps_from_parts(
          input$nav_common_equity,
          investments = input$investments,
          holdco_discount = disc_pct / 100,
          shares = input$nav_shares,
          delta_asset = input$nav_delta_asset,
          delta_liability = input$nav_delta_liability,
          apply_holdco = apply_hc
        )
        if (is.finite(nv)) updateNumericInput(session, "navps", value = round(nv, 2))
      }
    }

    .pb_refresh_nav_breakdown <- function() {
      sh <- suppressWarnings(as.numeric(input$nav_shares)[1])
      if (!is.finite(sh) || sh <= 0) return()
      disc_pct <- suppressWarnings(as.numeric(input$holdco_discount)[1])
      if (!is.finite(disc_pct)) disc_pct <- APP_DEFAULTS$pb_holdco_discount * 100
      inv_in <- suppressWarnings(as.numeric(input$investments)[1])
      eq_in <- suppressWarnings(as.numeric(input$nav_common_equity)[1])
      if (!(is.finite(eq_in) && eq_in > 0)) return()
      inv_use <- if (is.finite(inv_in) && inv_in >= 0) inv_in else 0
      disc <- max(0, min(0.5, disc_pct / 100))
      da <- suppressWarnings(as.numeric(input$nav_delta_asset)[1])
      dl <- suppressWarnings(as.numeric(input$nav_delta_liability)[1])
      has_mtm <- .pb_has_mtm(da, dl)
      apply_hc <- if (has_mtm) isTRUE(input$nav_apply_holdco) else TRUE
      if (!is.finite(da)) da <- 0
      if (!is.finite(dl)) dl <- 0
      nav_tot <- if (has_mtm) {
        .pb_nav_total_from_parts(
          eq_in, delta_asset = da, delta_liability = dl,
          investments = inv_use, holdco_discount = disc, apply_holdco = apply_hc
        )
      } else {
        eq_in - disc * inv_use
      }
      kind <- if (has_mtm) "navps" else "adjusted_bvps"
      note <- if (!has_mtm) {
        if (inv_use > 0 && disc > 0) {
          sprintf("無市價重估 → Adjusted BVPS＝權益 − %.0f%%×投資", disc * 100)
        } else {
          "無市價重估 ΔAsset／ΔLiability → 標為 Adjusted BVPS（非完整 NAVPS）"
        }
      } else if (isTRUE(apply_hc) && inv_use > 0 && disc > 0) {
        sprintf("NAV＝Equity＋ΔA−ΔL − %.0f%%×投資（勿與 P/NAV 重複折價）", disc * 100)
      } else {
        "NAV＝Equity＋ΔAsset−ΔLiability；HoldCo 由 Target P/NAV 反映"
      }
      nav_components(list(
        cash = NA_real_,
        investments = inv_use,
        equity = eq_in,
        delta_asset = da,
        delta_liability = dl,
        nav = nav_tot,
        discount = disc,
        kind = kind,
        note = note
      ))
      nav_shares(sh)
    }

    .pb_apply_recommend <- function(force = FALSE) {
      d_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      eq <- tryCatch(common_stock_equity(d_bs), error = function(e) NA_real_)
      gw <- suppressWarnings(as.numeric(input$goodwill)[1])
      intang <- suppressWarnings(as.numeric(input$intangibles)[1])
      inv <- suppressWarnings(as.numeric(input$investments)[1])
      rec <- .pb_recommend_basis(
        industry_key = tryCatch(industry_choice(), error = function(e) NULL),
        common_equity = eq,
        goodwill = gw,
        intangibles = intang,
        investments = inv,
        has_mtm = .pb_has_mtm_inputs()
      )
      last_recommend(rec)
      if (isTRUE(force) || !isTRUE(basis_manual())) {
        updateRadioButtons(session, "basis", selected = rec$basis)
      }
      invisible(rec)
    }

    sync_navps <- function(df_bs, shares, share_method = "balance_sheet") {
      disc_pct <- suppressWarnings(as.numeric(input$holdco_discount)[1])
      if (!is.finite(disc_pct)) disc_pct <- APP_DEFAULTS$pb_holdco_discount * 100
      navc <- extract_nav_components(df_bs, holdco_discount = disc_pct / 100)
      nav_components(navc)
      inv_q <- .pb_money_to_quote(navc$investments, df_bs, share_method = share_method)
      if (is.finite(inv_q)) {
        updateNumericInput(session, "investments", value = round(inv_q, 0))
      } else if (is.finite(navc$investments)) {
        updateNumericInput(session, "investments", value = round(navc$investments, 0))
      }
      eq_q <- .pb_money_to_quote(navc$equity, df_bs, share_method = share_method)
      if (is.finite(eq_q)) {
        updateNumericInput(session, "nav_common_equity", value = round(eq_q, 0))
      }
      if (is.finite(shares) && shares > 0) {
        updateNumericInput(session, "nav_shares", value = round(shares, 0))
      }
      if (is.finite(navc$nav) && is.finite(shares) && shares > 0) {
        q_ccy <- tryCatch(quote_currency(), error = function(e) NA_character_)
        f_ccy <- tryCatch(financial_currency(), error = function(e) NA_character_)
        eq_ccy <- equity_money_ccy(df_bs, session_ccy = q_ccy, statement_ccy = f_ccy)
        navps <- per_share_in_quote(
          navc$nav, shares,
          equity_ccy = eq_ccy,
          to_ccy = q_ccy,
          share_method = share_method,
          statement_ccy = f_ccy
        )
        if (is.finite(navps)) updateNumericInput(session, "navps", value = round(navps, 2))
      }
    }

    # --- 從產業標準推估本淨比區間（若未定義則回傳 NULL）---
    industry_pb_band <- reactive({
      ind <- industry_choice()
      if (is.null(ind) || !nzchar(ind) || !exists("industry_standards")) return(NULL)
      std <- industry_standards[[ind]]
      if (is.null(std)) return(NULL)
      if (!is.null(std$pb_band) && length(std$pb_band) >= 2) {
        lo <- std$pb_band[1]; hi <- std$pb_band[2]
        mid <- if (length(std$pb_band) >= 3) std$pb_band[3] else mean(c(lo, hi))
        return(list(low = lo, mid = mid, high = hi))
      }
      NULL
    })

    current_roe_pct <- reactive({
      d_is <- tryCatch(d_income_statement(), error = function(e) NULL)
      d_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      ni <- tryCatch(common_net_income(d_is), error = function(e) NA_real_)
      eq <- tryCatch(common_stock_equity(d_bs), error = function(e) NA_real_)
      if (is.finite(ni) && is.finite(eq) && eq > 0) ni / eq * 100 else NA_real_
    })

    hist_pb_series <- reactive({
      px <- tryCatch(hist_prices(), error = function(e) NULL)
      # History multiple uses the active sub-model metric (P/B, P/TBV, or P/NAV).
      bv <- .pb_basis_val()
      if (is.null(px) || !is.data.frame(px) || !("Close" %in% names(px)) || !is.finite(bv) || bv <= 0) {
        return(NULL)
      }
      closes <- suppressWarnings(as.numeric(px$Close))
      closes <- closes[is.finite(closes) & closes > 0]
      if (length(closes) < 4) return(NULL)
      closes / bv
    })

    pb_targets_derived <- reactive({
      key_status <- tryCatch(validate_industry_key(industry_choice()), error = function(e) list(ok = FALSE))
      ind_band <- if (isTRUE(input$use_industry_pb) && isTRUE(key_status$ok)) industry_pb_band() else NULL
      d_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      eq <- tryCatch(common_stock_equity(d_bs), error = function(e) NA_real_)
      basis <- as.character(input$basis %||% "bvps")[1]
      # Justified P/B is the classic (ROE−g)/(Ke−g) path — primarily for BVPS × P/B.
      use_just <- identical(as.character(input$target_mode %||% "justified")[1], "justified") &&
        identical(basis, "bvps")
      derive_pb_targets(
        roe_pct = if (isTRUE(use_just)) current_roe_pct() else NA_real_,
        ke_pct = if (isTRUE(use_just)) suppressWarnings(as.numeric(central_ke())[1]) * 100 else NA_real_,
        g_pct = if (isTRUE(use_just)) suppressWarnings(as.numeric(central_g_pct())[1]) else NA_real_,
        industry_band = ind_band,
        hist_pb = hist_pb_series(),
        include_justified = isTRUE(use_just),
        include_industry = isTRUE(input$use_industry_pb),
        include_history = TRUE,
        book_equity = eq,
        industry_key_valid = isTRUE(key_status$ok),
        locale = .loc()
      )
    })

    shares_resolve_note <- reactiveVal(NULL)
    pb_source_note <- reactiveVal("")

    output$ui_target_mode_help <- renderUI({
      mode <- as.character(input$target_mode %||% "justified")[1]
      basis <- as.character(input$basis %||% "bvps")[1]
      mult <- .pb_active_multiple_name(basis)
      txt <- if (identical(mode, "multiples")) {
        sprintf("本模式只用產業帶與／或歷史倍數合成目標 %s，不套用 Justified，亦不需要 SGR。", mult)
      } else if (!identical(basis, "bvps")) {
        sprintf("Justified 公式主要服務 BVPS×P/B；目前子模型改以產業／歷史合成目標 %s。", mult)
      } else {
        "Justified P/B ≈ (ROE − g)/(Ke − g)；g 取中央 SGR。可再與產業／歷史合成。"
      }
      tags$p(style = "font-size:12px; color:#64748b; margin:0 0 10px 0;", txt)
    })

    output$ui_basis_recommend <- renderUI({
      rec <- last_recommend()
      if (is.null(rec)) return(NULL)
      loc <- .loc()
      reason <- if (identical(loc, "en")) rec$reason_en else rec$reason_zh
      conf <- as.character(rec$confidence %||% "normal")[1]
      disabled <- rec$disabled %||% character(0)
      bits <- character(0)
      if (length(disabled)) {
        bits <- c(bits, sprintf(
          if (identical(loc, "en")) "Disabled: %s." else "停用：%s。",
          paste(toupper(disabled), collapse = ", ")
        ))
      }
      if (identical(conf, "low")) {
        bits <- c(bits, if (identical(loc, "en")) "NAV confidence: low." else "NAV 信心：低。")
      }
      if (isTRUE(rec$prefer_other)) {
        bits <- c(bits, if (identical(loc, "en")) "Prefer DCF / P/E when available." else "有 DCF／P/E 時優先。")
      }
      tags$div(
        class = "ynow-pb-recommend",
        style = paste0(
          "margin: 6px 0 4px 0; padding: 8px 10px; border-radius: 6px; font-size: 12px; ",
          if (identical(conf, "low")) {
            "background:#fff7ed; border:1px solid #fdba74; color:#9a3412;"
          } else {
            "background:#f8fafc; border:1px solid #cbd5e1; color:#334155;"
          }
        ),
        tags$b(if (identical(loc, "en")) "Suggested model: " else "建議子模型："),
        toupper(rec$basis %||% "bvps"),
        " — ", reason,
        if (length(bits)) tags$div(style = "margin-top:4px;", paste(bits, collapse = " ")) else NULL
      )
    })

    output$ui_nav_metric_note <- renderUI({
      kind <- .pb_nav_kind()
      loc <- .loc()
      if (identical(kind, "navps")) {
        tags$p(
          class = "help-block",
          style = "color:#1e3a8a;",
          if (identical(loc, "en")) {
            "Mark-to-market deltas present → metric labeled NAVPS. Avoid double-counting HoldCo in both NAV and Target P/NAV."
          } else {
            "已有市價重估 Δ → 標為 NAVPS。勿在 NAV 與 Target P/NAV 重複套用控股折價。"
          }
        )
      } else {
        tags$p(
          class = "help-block",
          style = "color:#9a3412;",
          if (identical(loc, "en")) {
            "No mark-to-market ΔAsset/ΔLiability → Equity − HoldCo×investments is Adjusted BVPS, not full NAVPS (low confidence)."
          } else {
            "無市價重估 ΔAsset／ΔLiability → Equity − HoldCo×投資為 Adjusted BVPS，非完整 NAVPS（低信心）。"
          }
        )
      }
    })

    observeEvent(input$basis, {
      # User (or recommend) changed the radio — treat subsequent picks as manual
      # once the user clicks after the first auto-apply.
      isolate({
        rec <- last_recommend()
        cur <- as.character(input$basis %||% "")[1]
        if (!is.null(rec) && !identical(cur, as.character(rec$basis %||% "")[1])) {
          basis_manual(TRUE)
        }
      })
    }, ignoreInit = TRUE)

    output$ui_pb_source_note <- renderUI({
      d <- tryCatch(pb_targets_derived(), error = function(e) NULL)
      note <- pb_source_note()
      mode <- as.character(input$target_mode %||% "justified")[1]
      body <- if (!is.null(d)) format_pb_targets_note(d, locale = .loc(), mode = mode) else ""
      if (nzchar(note %||% "")) body <- paste(c(body, note), collapse = "\n")
      if (!nzchar(body)) return(NULL)
      blocked <- is.null(d) || !is.finite(suppressWarnings(as.numeric(d$mid)[1]))
      tags$div(
        style = if (blocked) {
          "margin: 0 0 10px 0; padding: 8px 10px; background: #fff7ed; border: 1px solid #fdba74; border-radius: 8px; color: #9a3412; font-size: 12px; white-space: pre-wrap;"
        } else {
          "margin: 0 0 10px 0; padding: 8px 10px; background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 8px; color: #1e3a8a; font-size: 12px; white-space: pre-wrap;"
        },
        body
      )
    })
    
    # --- 財報同步 BVPS / TBVPS / NAVPS 與組成科目（ADR／雙重股權：一律自動約當報價股）---
    sync_book_values <- function() {
      req(d_balance_sheet())
      df_bs <- d_balance_sheet()
      updateNumericInput(session, "bvps", value = NA_real_)
      updateNumericInput(session, "tbvps", value = NA_real_)
      updateNumericInput(session, "navps", value = NA_real_)
      updateNumericInput(session, "common_equity", value = NA_real_)
      updateNumericInput(session, "shares", value = NA_real_)
      updateNumericInput(session, "tb_common_equity", value = NA_real_)
      updateNumericInput(session, "tb_shares", value = NA_real_)
      updateNumericInput(session, "nav_common_equity", value = NA_real_)
      updateNumericInput(session, "nav_shares", value = NA_real_)
      updateNumericInput(session, "goodwill", value = NA_real_)
      updateNumericInput(session, "intangibles", value = NA_real_)
      updateNumericInput(session, "investments", value = NA_real_)
      updateNumericInput(session, "nav_delta_asset", value = NA_real_)
      updateNumericInput(session, "nav_delta_liability", value = NA_real_)
      
      equity <- common_stock_equity(df_bs)
      
      shares_bs <- select_current_metric_any(
        df_bs,
        SHARE_PATTERNS,
        "stock"
      )
      px_quote <- suppressWarnings(as.numeric(quote_price())[1])
      if (!is.finite(px_quote) || px_quote <= 0) {
        px_quote <- suppressWarnings(as.numeric(current_price())[1])
      }
      mcap <- suppressWarnings(as.numeric(market_cap())[1])
      tk <- tryCatch(current_ticker(), error = function(e) "")
      sh_adj <- resolve_shares_for_price(
        shares_bs,
        price = px_quote,
        market_cap = mcap,
        ticker = tk,
        quote_currency = tryCatch(quote_currency(), error = function(e) NULL),
        financial_currency = tryCatch(financial_currency(), error = function(e) NULL)
      )

      auto_adj <- shares_auto_adjust_method(sh_adj$method)
      q_ccy0 <- tryCatch(quote_currency(), error = function(e) NA_character_)
      f_ccy0 <- tryCatch(financial_currency(), error = function(e) NA_character_)
      if (isTRUE(auto_adj) && is.finite(sh_adj$shares) && sh_adj$shares > 0) {
        shares <- sh_adj$shares
        shares_resolve_note(sh_adj$note)
      } else if (isTRUE(adr_conversion_required(
        infer_statement_currency(f_ccy0, q_ccy0, sh_adj$method), q_ccy0, sh_adj$method
      )) || isTRUE(fx_conversion_required(
        infer_statement_currency(f_ccy0, q_ccy0, sh_adj$method), q_ccy0
      ))) {
        shares <- NA_real_
        shares_resolve_note("報價幣≠財報幣且無法約當 ADR 股數，不顯示每股淨值")
      } else {
        if (is.finite(shares_bs) && shares_bs > 0) {
          shares <- shares_bs
        } else {
          shares <- sh_adj$shares
        }
        shares_resolve_note(NULL)
      }
      
      # TBVPS 扣除：優先 Goodwill + Other Intangible Assets；
      # 若僅有合計列則只用一次，避免與獨立 Goodwill 雙重扣減
      goodwill <- select_current_metric(df_bs, "^Goodwill$", "stock")
      other_intang <- select_current_metric(df_bs, "^Other Intangible Assets$", "stock")
      combined_gi <- select_current_metric(
        df_bs,
        "Goodwill And Other Intangible Assets|Goodwill & Other Intangible Assets",
        "stock"
      )
      gw_stmt <- NA_real_
      intang_stmt <- NA_real_
      if (!is.na(goodwill) || !is.na(other_intang)) {
        gw_stmt <- ifelse(is.na(goodwill), 0, goodwill)
        intang_stmt <- ifelse(is.na(other_intang), 0, other_intang)
        intang_deduct <- gw_stmt + intang_stmt
      } else if (!is.na(combined_gi)) {
        gw_stmt <- 0
        intang_stmt <- combined_gi
        intang_deduct <- combined_gi
      } else {
        loose <- select_current_metric(df_bs, "^Intangible Assets$", "stock")
        intang_deduct <- ifelse(is.na(loose), NA_real_, loose)
        gw_stmt <- 0
        intang_stmt <- ifelse(is.na(loose), NA_real_, loose)
      }
      
      if (!is.na(equity) && !is.na(shares) && shares > 0) {
        q_ccy <- tryCatch(quote_currency(), error = function(e) NA_character_)
        f_ccy <- tryCatch(financial_currency(), error = function(e) NA_character_)
        eq_ccy <- equity_money_ccy(df_bs, session_ccy = q_ccy, statement_ccy = f_ccy)
        meth <- sh_adj$method %||% "balance_sheet"
        bvps <- per_share_in_quote(
          equity, shares,
          equity_ccy = eq_ccy,
          to_ccy = q_ccy,
          share_method = meth,
          statement_ccy = f_ccy
        )
        tbv_base <- if (is.finite(intang_deduct)) max(equity - intang_deduct, 0) else equity
        tbvps <- per_share_in_quote(
          tbv_base, shares,
          equity_ccy = eq_ccy,
          to_ccy = q_ccy,
          share_method = meth,
          statement_ccy = f_ccy
        )
        if (is.finite(bvps)) {
          eq_q <- .pb_money_to_quote(equity, df_bs, share_method = meth)
          gw_q <- .pb_money_to_quote(gw_stmt, df_bs, share_method = meth)
          intang_q <- .pb_money_to_quote(intang_stmt, df_bs, share_method = meth)
          sh_round <- round(shares, 0)
          if (is.finite(eq_q)) {
            eq_round <- round(eq_q, 0)
            updateNumericInput(session, "common_equity", value = eq_round)
            updateNumericInput(session, "tb_common_equity", value = eq_round)
            updateNumericInput(session, "nav_common_equity", value = eq_round)
          }
          updateNumericInput(session, "shares", value = sh_round)
          updateNumericInput(session, "tb_shares", value = sh_round)
          updateNumericInput(session, "nav_shares", value = sh_round)
          if (is.finite(gw_q)) {
            updateNumericInput(session, "goodwill", value = round(gw_q, 0))
          } else {
            updateNumericInput(session, "goodwill", value = 0)
          }
          if (is.finite(intang_q)) {
            updateNumericInput(session, "intangibles", value = round(intang_q, 0))
          } else {
            updateNumericInput(session, "intangibles", value = 0)
          }
          updateNumericInput(session, "bvps", value = round(bvps, 2))
          if (is.finite(tbvps)) updateNumericInput(session, "tbvps", value = round(tbvps, 2))
          nav_shares(shares)
          sync_navps(df_bs, shares, share_method = meth)
          # ADR／股數級距 toast 僅由 Search 發出一次；此處只更新頁內 shares_resolve_note
        } else {
          showNotification("無法從財報推算 BVPS（匯率／ADR 股數未對齊）", type = "warning", duration = 6)
        }
      } else {
        showNotification("無法從財報推算 BVPS，請手動輸入淨值與股數相關科目", type = "warning", duration = 6)
      }
    }
    
    observeEvent(
      list(
        d_balance_sheet(), current_price(), quote_price(), market_cap(), current_ticker()
      ),
      {
        sync_book_values()
      },
      ignoreInit = FALSE
    )
    
    observeEvent(input$btn_sync_bv, {
      sync_book_values()
      .pb_apply_recommend(force = FALSE)
      showNotification("已從財報帶入 BVPS／TBVPS／NAVPS", type = "message")
    })

    # Component edits → refresh only that mode’s per-share result.
    observeEvent(
      list(input$common_equity, input$shares),
      {
        .pb_recompute_from_parts("bvps")
      },
      ignoreInit = TRUE
    )
    observeEvent(
      list(input$tb_common_equity, input$tb_shares, input$goodwill, input$intangibles),
      {
        .pb_recompute_from_parts("tbvps")
      },
      ignoreInit = TRUE
    )
    observeEvent(
      list(
        input$nav_common_equity, input$nav_shares,
        input$investments, input$holdco_discount,
        input$nav_delta_asset, input$nav_delta_liability,
        input$nav_apply_holdco
      ),
      {
        .pb_recompute_from_parts("navps")
        .pb_refresh_nav_breakdown()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      list(industry_choice(), d_balance_sheet(), input$goodwill, input$intangibles, input$investments,
           input$nav_delta_asset, input$nav_delta_liability),
      {
        .pb_apply_recommend(force = FALSE)
      },
      ignoreInit = FALSE
    )
    
    output$txt_shares_resolve_note <- renderUI({
      note <- shares_resolve_note()
      if (is.null(note) || !nzchar(note)) return(NULL)
      tags$p(
        style = paste0(
          "font-size: 12px; margin: 0 0 10px 0; padding: 8px 10px; border-left: 3px solid #e67e22;",
          " background: #fff8e6; color: #b85c00;"
        ),
        paste0("已自動對齊報價股：", note)
      )
    })
    
    observeEvent(list(input$use_industry_pb, input$target_mode, industry_choice(), industry_text(),
                      current_roe_pct(), central_ke(), central_g_pct(), hist_pb_series(),
                      input$bvps, input$tbvps, input$navps, input$basis), {
      tgt <- pb_targets_derived()
      if (is.null(tgt) || !is.finite(tgt$mid)) return()
      basis <- as.character(input$basis %||% "bvps")[1]
      # Write only the active sub-model’s multiple band (each mode owns its inputs).
      .pb_write_multiples(basis, tgt$low, tgt$mid, tgt$high)
      pb_source_note(tgt$source_note %||% "")
    }, ignoreInit = FALSE)
    
    observeEvent(input$btn_reset_pb, {
      basis_manual(FALSE)
      updateNumericInput(session, "pb_low",  value = APP_DEFAULTS$pb_low)
      updateNumericInput(session, "pb_mid",  value = APP_DEFAULTS$pb_mid)
      updateNumericInput(session, "pb_high", value = APP_DEFAULTS$pb_high)
      updateNumericInput(session, "ptbv_low",  value = APP_DEFAULTS$ptbv_low)
      updateNumericInput(session, "ptbv_mid",  value = APP_DEFAULTS$ptbv_mid)
      updateNumericInput(session, "ptbv_high", value = APP_DEFAULTS$ptbv_high)
      updateNumericInput(session, "pnav_low",  value = APP_DEFAULTS$pnav_low)
      updateNumericInput(session, "pnav_mid",  value = APP_DEFAULTS$pnav_mid)
      updateNumericInput(session, "pnav_high", value = APP_DEFAULTS$pnav_high)
      updateRadioButtons(session, "basis", selected = APP_DEFAULTS$pb_basis)
      updateCheckboxInput(session, "use_industry_pb", value = APP_DEFAULTS$pb_use_industry)
      updateRadioButtons(session, "target_mode", selected = APP_DEFAULTS$pb_target_mode %||% "justified")
      updateNumericInput(session, "holdco_discount", value = APP_DEFAULTS$pb_holdco_discount * 100)
      updateCheckboxInput(session, "nav_apply_holdco", value = isTRUE(APP_DEFAULTS$pb_nav_apply_holdco))
      updateNumericInput(session, "nav_delta_asset", value = NA_real_)
      updateNumericInput(session, "nav_delta_liability", value = NA_real_)
      sync_book_values()
      .pb_apply_recommend(force = TRUE)
      # After sync+recommend, refresh active multiples from derive if available.
      tgt <- tryCatch(pb_targets_derived(), error = function(e) NULL)
      if (!is.null(tgt) && is.finite(tgt$mid)) {
        .pb_write_multiples(input$basis %||% APP_DEFAULTS$pb_basis, tgt$low, tgt$mid, tgt$high)
      }
      showNotification("🔁 Asset-Based 參數已回復預設", type = "message")
    })
    
    output$alert_missing_bv <- renderUI({
      # Single-mode Settings: only flag the selected book metric as missing.
      basis <- as.character(input$basis %||% "bvps")[1]
      nav_label <- if (identical(.pb_nav_kind(), "adjusted_bvps")) "Adjusted BVPS" else "NAVPS"
      check_list <- switch(
        basis,
        tbvps = list("TBVPS" = input$tbvps),
        navps = stats::setNames(list(input$navps), nav_label),
        list("BVPS" = input$bvps)
      )
      # Gate: negative equity / tangible equity
      rec <- last_recommend()
      disabled <- rec$disabled %||% character(0)
      if (basis %in% disabled) {
        msg <- if (identical(basis, "tbvps")) {
          "Tangible common equity ≤ 0 — P/TBV (TBVPS) is disabled for this name."
        } else {
          "Common equity ≤ 0 — classic P/B (BVPS) is disabled for this name."
        }
        return(div(
          style = "color:#9a3412; background:#fff7ed; border:1px solid #fdba74; padding:10px; border-radius:6px; margin-bottom:8px;",
          icon("ban"), " ", msg
        ))
      }
      ui_missing_data_alert(
        check_list = check_list,
        fallback_msg = "請先載入財報或手動輸入每股淨值，否則無法計算合理價。"
      )
    })

    output$tbl_nav_breakdown <- renderTable({
      navc <- nav_components()
      if (is.null(navc) || !is.finite(navc$equity)) {
        return(data.frame(科目 = "NAV 拆解", 金額 = "載入財報後顯示", check.names = FALSE))
      }
      sh <- suppressWarnings(as.numeric(nav_shares())[1])
      navps <- if (is.finite(navc$nav) && is.finite(sh) && sh > 0) navc$nav / sh else NA_real_
      fmt <- function(x) {
        if (!is.finite(x)) return("N/A")
        format(round(x, 0), big.mark = ",", scientific = FALSE)
      }
      metric_lbl <- if (identical(navc$kind %||% "adjusted_bvps", "navps")) "NAVPS" else "Adjusted BVPS"
      data.frame(
        科目 = c("股東權益", "ΔAsset", "ΔLiability", "已辨識投資", "控股折價", "NAV／調整後權益", metric_lbl, "說明"),
        金額 = c(
          fmt(navc$equity),
          fmt(navc$delta_asset %||% NA_real_),
          fmt(navc$delta_liability %||% NA_real_),
          fmt(navc$investments),
          sprintf("%.0f%%", 100 * (navc$discount %||% 0)),
          fmt(navc$nav),
          if (is.finite(navps)) sprintf("%.2f", navps) else "N/A",
          as.character(navc$note %||% "")
        ),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
    
    .pb_calc_requested <- function() {
      btn <- suppressWarnings(as.integer(input$btn_calc_pb)[1])
      pulse <- suppressWarnings(as.integer(auto_calc_pulse())[1])
      (is.finite(btn) && btn >= 1L) || (is.finite(pulse) && pulse >= 1L)
    }

    # --- 核心計算：Fair Price = active metric × that mode’s own multiple ---
    pb_calc <- eventReactive(list(input$btn_calc_pb, auto_calc_pulse()), {
      if (!isTRUE(integrity_ready())) return(list(status = "idle"))
      if (!isTRUE(.pb_calc_requested())) {
        return(list(status = "idle"))
      }
      basis <- as.character(input$basis %||% "bvps")[1]
      rec <- last_recommend()
      disabled <- rec$disabled %||% character(0)
      if (basis %in% disabled) {
        return(list(
          status = "error",
          message = if (identical(basis, "tbvps")) {
            "Tangible common equity ≤ 0 — cannot run P/TBV."
          } else {
            "Common equity ≤ 0 — cannot run classic P/B."
          }
        ))
      }
      basis_val <- .pb_basis_val()
      if (is.na(basis_val) || basis_val <= 0) {
        return(list(status = "error", message = "計算無效：請先提供有效的 BVPS／TBVPS／NAVPS（須 > 0）。"))
      }
      mult <- .pb_active_multiples(basis)
      lo <- mult$low; mid <- mult$mid; hi <- mult$high
      mult_name <- .pb_active_multiple_name(basis)
      if (lo <= 0 || mid <= 0 || hi <= 0) {
        return(list(status = "error", message = sprintf("目標 %s 倍數必須大於 0。", mult_name)))
      }
      if (lo > mid || mid > hi) {
        return(list(status = "error", message = sprintf("請維持 保守 ≤ 基準 ≤ 樂觀 的 %s 順序。", mult_name)))
      }
      
      px <- suppressWarnings(as.numeric(current_price()))
      mkt_pb <- if (length(px) == 1 && !is.na(px) && basis_val > 0) px / basis_val else NA_real_
      nav_kind <- if (identical(basis, "navps")) .pb_nav_kind() else NA_character_
      
      list(
        status = "success",
        basis = basis,
        basis_val = basis_val,
        basis_label = .pb_basis_label(basis),
        multiple_name = mult_name,
        nav_kind = nav_kind,
        fair_low = .pb_formula_p(basis_val, lo),
        fair_mid = .pb_formula_p(basis_val, mid),
        fair_high = .pb_formula_p(basis_val, hi),
        pb_low = lo, pb_mid = mid, pb_high = hi,
        market_price = px,
        market_pb = mkt_pb
      )
    }, ignoreNULL = FALSE)
    
    # 初次／參數變更時若尚未按過也可顯示提示；正式結果依按鈕
    output$ui_pb_result <- renderUI({
      res <- pb_calc()
      if (identical(res$status, "idle")) {
        return(div(style = "color: #7f8c8d; padding: 15px; text-align: center;",
                   ui_str("pb_idle_hint", .loc())))
      }
      if (res$status == "error") {
        return(div(style = "color: #d9534f; font-weight: bold; padding: 15px; background-color: #fdf2f2; border-left: 5px solid #d9534f; border-radius: 4px;",
                   icon("exclamation-triangle"), " ", res$message))
      }
      
      mkt_txt <- if (!is.na(res$market_price)) paste0(money_prefix(quote_currency()), round(res$market_price, 2)) else "N/A"
      mkt_pb_txt <- if (!is.na(res$market_pb)) sprintf("%.2f×", res$market_pb) else "N/A"
      upside <- if (!is.na(res$market_price) && res$market_price > 0) {
        (res$fair_mid - res$market_price) / res$market_price * 100
      } else NA_real_
      upside_txt <- if (is.na(upside)) "N/A" else sprintf("%+.1f%%", upside)
      upside_color <- if (is.na(upside)) "#7f8c8d" else if (upside >= 15) "#00a65a" else if (upside <= -10) "#d9534f" else "#f39c12"
      
      div(style = "display: flex; justify-content: space-between; align-items: stretch; gap: 10px; padding: 20px; background-color: #fcfcfc; border: 1px solid #ddd; border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.05); flex-wrap: wrap;",
          div(style = "text-align: center; flex: 1; min-width: 120px;",
              p(style = "font-size: 13px; color: #7f8c8d; margin-bottom: 5px; font-weight: bold;", "估值基礎"),
              p(style = "font-size: 22px; color: #2c3e50; font-weight: bold; margin: 0;", paste0(money_prefix(quote_currency()), round(res$basis_val, 2))),
              p(style = "font-size: 12px; color: #95a5a6;", res$basis_label %||% .pb_basis_label(res$basis))
          ),
          div(style = "text-align: center; flex: 1; min-width: 120px;",
              p(style = "font-size: 13px; color: #7f8c8d; margin-bottom: 5px; font-weight: bold;", "基準目標價"),
              p(style = "font-size: 28px; color: #1a1a1a; font-weight: bold; margin: 0;", paste0(money_prefix(quote_currency()), round(res$fair_mid, 2))),
              p(style = "font-size: 12px; color: #95a5a6;", sprintf("@ %.2f× %s", res$pb_mid, res$multiple_name %||% "P/B"))
          ),
          div(style = "text-align: center; flex: 1; min-width: 120px;",
              p(style = "font-size: 13px; color: #7f8c8d; margin-bottom: 5px; font-weight: bold;", "市價／市價本淨比"),
              p(style = "font-size: 22px; color: #2c3e50; font-weight: bold; margin: 0;", mkt_txt),
              p(style = "font-size: 12px; color: #95a5a6;", mkt_pb_txt)
          ),
          div(style = "text-align: center; flex: 1; min-width: 140px; background-color: #f5f5f5; padding: 12px; border-radius: 8px; border-left: 4px solid #1a1a1a;",
              p(style = "font-size: 13px; color: #2471a3; margin-bottom: 5px; font-weight: bold;", "相對基準潛在報酬"),
              p(style = paste0("font-size: 28px; font-weight: bold; margin: 0; color: ", upside_color, ";"), upside_txt)
          )
      )
    })
    
    output$vbx_fair <- renderValueBox({
      live <- tryCatch(pb_live_band(), error = function(e) NULL)
      raw <- if (is.null(live) || !is.finite(live$mid)) {
        "N/A"
      } else {
        paste0(money_prefix(quote_currency()), round(live$mid, 2))
      }
      valueBox(
        ynow_model_result_num(raw),
        ui_str("pb_vbx_fair", .loc()),
        icon = icon("balance-scale"), color = "aqua"
      )
    })

    output$vbx_bvps <- renderValueBox({
      val <- input$bvps
      valueBox(
        if (is.null(val) || is.na(val)) "N/A" else paste0(money_prefix(quote_currency()), round(val, 2)),
        "每股帳面淨值 BVPS", icon = icon("book"), color = "aqua"
      )
    })
    
    output$vbx_tbvps <- renderValueBox({
      val <- input$tbvps
      valueBox(
        if (is.null(val) || is.na(val)) "N/A" else paste0(money_prefix(quote_currency()), round(val, 2)),
        "有形每股淨值 TBVPS", icon = icon("cube"), color = "light-blue"
      )
    })
    
    output$vbx_mkt_pb <- renderValueBox({
      basis_val <- .pb_basis_val()
      px <- suppressWarnings(as.numeric(current_price()))
      mkt_pb <- if (length(px) == 1 && !is.na(px) && !is.na(basis_val) && basis_val > 0) px / basis_val else NA
      valueBox(
        if (is.na(mkt_pb)) "N/A" else sprintf("%.2f×", mkt_pb),
        paste0("目前市價／", .pb_basis_label()), icon = icon("chart-bar"), color = "maroon"
      )
    })
    
    output$tbl_pb_band <- renderTable({
      req(.pb_calc_requested())
      res <- pb_calc()
      req(res$status == "success")
      mult_col <- paste0("目標 ", res$multiple_name %||% "P/B")
      out <- data.frame(
        情境 = c("保守", "基準", "樂觀"),
        Multiple = sprintf("%.2f×", c(res$pb_low, res$pb_mid, res$pb_high)),
        `合理股價` = sprintf("$%.2f", c(res$fair_low, res$fair_mid, res$fair_high)),
        check.names = FALSE
      )
      names(out)[2] <- mult_col
      out
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c", width = "100%")
    
    output$plt_pb_band <- renderPlot({
      req(.pb_calc_requested())
      res <- pb_calc()
      req(res$status == "success")
      
      df <- data.frame(
        Scenario = factor(c("保守", "基準", "樂觀"), levels = c("保守", "基準", "樂觀")),
        Price = c(res$fair_low, res$fair_mid, res$fair_high)
      )
      
      p <- ggplot(df, aes(x = Scenario, y = Price, fill = Scenario)) +
        geom_col(width = 0.55, alpha = 0.85) +
        geom_text(aes(label = format_dollar_abbr(Price)), vjust = -0.4, fontface = "bold", size = 4.2) +
        scale_fill_manual(values = c("保守" = "#7f8c8d", "基準" = "#2980b9", "樂觀" = "#27ae60")) +
        scale_y_continuous(labels = label_chart_number(prefix = money_prefix(quote_currency()))) +
        theme_minimal(base_size = 14) +
        labs(title = "P/B 合理價區間", x = NULL, y = paste0("每股合理價 (", money_label(), ")")) +
        theme(legend.position = "none", plot.title = element_text(face = "bold")) +
        expand_limits(y = max(df$Price, na.rm = TRUE) * 1.15)
      
      if (!is.na(res$market_price)) {
        p <- p + geom_hline(yintercept = res$market_price, linetype = "dashed", color = "#c0392b", linewidth = 1) +
          annotate("text", x = 1.2, y = res$market_price,
                   label = paste0("市價 ", format_dollar_abbr(res$market_price)),
                   vjust = -0.6, color = "#c0392b", fontface = "bold")
      }
      p
    })
    
    # Live band from current inputs (module value boxes only).
    # Composite / decision exports must stay calc-gated (see pb_price / pb_band).
    pb_live_band <- reactive({
      basis_val <- .pb_basis_val()
      mult <- .pb_active_multiples()
      lo <- mult$low; mid <- mult$mid; hi <- mult$high
      if (is.na(basis_val) || basis_val <= 0 || lo <= 0 || mid <= 0 || hi <= 0) return(NULL)
      list(
        low = .pb_formula_p(basis_val, lo),
        mid = .pb_formula_p(basis_val, mid),
        high = .pb_formula_p(basis_val, hi),
        basis_val = basis_val,
        multiple_name = .pb_active_multiple_name()
      )
    })

    output$param_sensitivity_table <- renderTable({
      shock_pct <- if (exists("PARAM_SENSITIVITY_SHOCK", inherits = TRUE)) PARAM_SENSITIVITY_SHOCK else 0.01
      basis0 <- .pb_basis_val()
      mult0 <- .pb_active_multiples()
      mid0 <- mult0$mid
      mult_name <- .pb_active_multiple_name()
      .p <- function(basis_u = 1, pb = mid0) .pb_formula_p(basis = basis_u, pb = pb)
      v0 <- .p()
      validate(need(
        is.finite(v0),
        sprintf("基準公式尚未就緒：請先提供目標 %s（選定每股指標僅影響顯示基準值）。", mult_name)
      ))
      .rel <- function(x, sign = -1) .param_rel_shock(x, sign = sign, shock = shock_pct)
      basis_show <- if (is.finite(basis0) && basis0 > 0) basis0 else 1
      basis_unit <- if (is.finite(basis0) && basis0 > 0) money_prefix(quote_currency()) else "x"
      rows <- list(
        .param_sensitivity_infl_row(
          .pb_basis_label(),
          basis_show, basis_unit, v0,
          .p(basis_u = 1 - shock_pct), .p(basis_u = 1 + shock_pct),
          "P ∝ 選定基礎 ⇒ |ε|=1（公式；與帳面金額／股價無關）"
        )
      )
      if (.param_sensitivity_rel_ok(mid0) && mid0 > 0) {
        rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
          paste0("基準目標 ", mult_name), mid0, "x", v0,
          .p(pb = .rel(mid0, -1)), .p(pb = .rel(mid0, +1)),
          sprintf("P ∝ 目標 %s ⇒ |ε|=1（公式；與個股無關）", mult_name)
        )
      }
      roe0 <- tryCatch(current_roe_pct(), error = function(e) NA_real_)
      ke0 <- suppressWarnings(as.numeric(central_ke())[1]) * 100
      g0 <- suppressWarnings(as.numeric(central_g_pct())[1])
      basis_now <- as.character(input$basis %||% "bvps")[1]
      use_just <- identical(as.character(input$target_mode %||% "justified")[1], "justified") &&
        identical(basis_now, "bvps")
      ind_band <- if (isTRUE(input$use_industry_pb)) {
        tryCatch(industry_pb_band(), error = function(e) NULL)
      } else {
        NULL
      }
      hist0 <- tryCatch(hist_pb_series(), error = function(e) NULL)
      tgt0 <- tryCatch(
        derive_pb_targets(
          roe_pct = if (isTRUE(use_just)) roe0 else NA_real_,
          ke_pct = if (isTRUE(use_just)) ke0 else NA_real_,
          g_pct = if (isTRUE(use_just)) g0 else NA_real_,
          industry_band = ind_band, hist_pb = hist0,
          include_justified = isTRUE(use_just),
          include_industry = isTRUE(input$use_industry_pb),
          include_history = TRUE
        ),
        error = function(e) NULL
      )
      just0 <- if (!is.null(tgt0)) suppressWarnings(as.numeric(tgt0$justified)[1]) else NA_real_
      .p_tgt <- function(roe = roe0, ke = ke0, g = g0, band = ind_band) {
        d <- derive_pb_targets(
          roe_pct = if (isTRUE(use_just)) roe else NA_real_,
          ke_pct = if (isTRUE(use_just)) ke else NA_real_,
          g_pct = if (isTRUE(use_just)) g else NA_real_,
          industry_band = band, hist_pb = hist0,
          include_justified = isTRUE(use_just),
          include_industry = isTRUE(input$use_industry_pb),
          include_history = TRUE
        )
        mid <- suppressWarnings(as.numeric(d$mid)[1])
        .pb_formula_p(basis = 1, pb = mid)
      }
      if (isTRUE(use_just) && is.finite(just0)) {
        if (.param_sensitivity_rel_ok(roe0)) {
          rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
            "Justified ROE", roe0, "%", v0,
            .p_tgt(roe = .rel(roe0, -1)), .p_tgt(roe = .rel(roe0, +1)),
            "目標 P/B：(ROE−g)/(Ke−g) 再與產業／歷史合成"
          )
        }
        if (.param_sensitivity_rel_ok(ke0)) {
          rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
            "股權成本 Ke", ke0, "%", v0,
            .p_tgt(ke = .rel(ke0, -1)), .p_tgt(ke = .rel(ke0, +1)),
            "Justified P/B 分母 Ke−g（合成後目標）"
          )
        }
        if (.param_sensitivity_rel_ok(g0)) {
          rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
            "永續成長率 g", g0, "%", v0,
            .p_tgt(g = .rel(g0, -1)), .p_tgt(g = .rel(g0, +1)),
            "Justified P/B：(ROE−g)/(Ke−g)"
          )
        }
        if (isTRUE(use_estimated_re())) {
          rf0 <- suppressWarnings(as.numeric(capm_rf())[1])
          beta0 <- suppressWarnings(as.numeric(capm_beta())[1])
          rm0 <- suppressWarnings(as.numeric(capm_rm())[1])
          capm_rows <- .param_sensitivity_capm_ke_rows(
            v0, function(ke_pct) .p_tgt(ke = ke_pct),
            rf0, beta0, rm0, shock = shock_pct
          )
          if (length(capm_rows)) rows <- c(rows, capm_rows)
        }
      }
      ind_mid <- if (is.list(ind_band)) suppressWarnings(as.numeric(ind_band$mid)[1]) else NA_real_
      if (isTRUE(input$use_industry_pb) && .param_sensitivity_rel_ok(ind_mid) && ind_mid > 0) {
        .band_at <- function(mid) {
          b <- ind_band
          b$mid <- mid
          b
        }
        rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
          "產業本淨比", ind_mid, "x", v0,
          .p_tgt(band = .band_at(.rel(ind_mid, -1))),
          .p_tgt(band = .band_at(.rel(ind_mid, +1))),
          "derive_pb_targets 產業權重來源"
        )
      }
      .param_sensitivity_sort_by_abs_eps(do.call(rbind, rows))
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s", width = "100%")

    # Core explainability: Why this P/B Fair Value (not appendix)
    output$value_why <- renderUI({
      loc <- .loc()
      empty_msg <- tryCatch(
        ui_str("explain_pb_why_empty", loc),
        error = function(e) "Run Search / Calculate P/B to see book × target multiple drivers."
      )
      if (!exists("ynow_value_why_panel", mode = "function") ||
          !exists("ynow_pb_why_rows", mode = "function")) {
        return(tags$p(style = "color:#888;", empty_msg))
      }
      res <- tryCatch(pb_calc(), error = function(e) NULL)
      ok <- !is.null(res) && identical(res$status, "success")
      mode <- as.character(input$target_mode %||% "justified")[1]
      rows <- if (isTRUE(ok)) {
        ynow_pb_why_rows(
          mode = mode,
          book = res$basis_val,
          target_pb = res$pb_mid,
          price = res$fair_mid,
          locale = loc
        )
      } else {
        NULL
      }
      ynow_value_why_panel(
        title = tryCatch(ui_str("explain_pb_why_title", loc), error = function(e) "Why this Fair Value (P/B)"),
        rows = rows,
        empty_message = empty_msg,
        footnote = tryCatch(ui_str("explain_pb_why_foot", loc), error = function(e) NULL)
      )
    })

    return(list(
      # Calc-only exports: Composite overlays must not appear until 試算 / Run
      # (or silent primary auto-calc). Keep pb_live_band for in-module value boxes.
      source_ready = .echo_guard$ready,
      pb_price = reactive({
        res <- tryCatch(pb_calc(), error = function(e) NULL)
        if (!is.null(res) && identical(res$status, "success")) res$fair_mid else NA_real_
      }),
      pb_band = reactive({
        res <- tryCatch(pb_calc(), error = function(e) NULL)
        if (is.null(res) || !identical(res$status, "success")) return(NULL)
        list(
          low = res$fair_low, mid = res$fair_mid, high = res$fair_high,
          market_pb = res$market_pb, basis_val = res$basis_val
        )
      })
    ))
  })
}
