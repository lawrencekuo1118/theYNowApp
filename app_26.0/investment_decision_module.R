# =========================================================================
# Investment Decision Scorecard — YNOW page + composite valuation
# Top → bottom:
#   Masthead → KPI jump row (QoE → F-Score → Statement alerts)
#   → YNOW statement verdict (manipulation / earnings-quality label only)
#   → Automated risk matrix (independent composite overview)
#   → I. Statement quality — QoE → F-Score
#   → II. Statement alerts — Beneish M-Score → Schilit
# Verdict uses statement numbers only (QoE, F-Score, M-Score, Schilit alerts) —
# never MOS, price, or momentum. Veto / risk context; not a buy signal.
# Alerts KPI counts Schilit + QoE red flags (incl. Beneish M-Score alerts).
# Dynamic industry bubble & weight concentration lives on Macro (page bottom).
# Same markup for Lite/Full.
# Copy: ui_str / funnel_* keys (en-US + zh-TW)
# =========================================================================

library(shiny)
library(shinydashboard)
library(TTR)
library(glue)

#' Statement-only YNOW verdict from financials (no MOS / price / momentum).
#' @return list(class, icon, title_key, text_key) or NULL if inputs insufficient
ynow_statement_verdict <- function(eq_pack, f_score, f_quality, n_alert) {
  f_score <- suppressWarnings(as.numeric(f_score)[1])
  f_quality <- suppressWarnings(as.numeric(f_quality)[1])
  n_alert <- suppressWarnings(as.integer(n_alert)[1])
  if (!is.finite(f_score) || !is.finite(f_quality)) return(NULL)

  eq_ok <- is.list(eq_pack) && isTRUE(eq_pack$ok)
  m_flag <- if (eq_ok) as.character(eq_pack$m_flag %||% "")[1] else ""
  qoe_flag <- if (eq_ok) as.character(eq_pack$qoe_flag %||% "")[1] else ""
  risk_level <- if (eq_ok) as.character(eq_pack$risk_level %||% "")[1] else ""
  n_red <- if (eq_ok) length(eq_pack$red_flags %||% character(0)) else 0L
  if (!is.finite(n_alert)) n_alert <- 0L

  # 1) Elevated manipulation / fraud screen
  if (identical(m_flag, "警示") || identical(risk_level, "高") || n_red >= 2L) {
    return(list(
      class = "alert-danger", icon = "skull-crossbones",
      title_key = "funnel_v_manip_title", text_key = "funnel_v_manip_text"
    ))
  }
  # 2) Weak cash-earnings / F-Score quality gate
  if (f_score < 4 || identical(as.numeric(f_quality), 0) || identical(qoe_flag, "警示")) {
    return(list(
      class = "alert-danger", icon = "exclamation-triangle",
      title_key = "funnel_v_quality_weak_title", text_key = "funnel_v_quality_weak_text"
    ))
  }
  # 3) Watch band — soft flags or Schilit alerts
  if (identical(m_flag, "觀察") || identical(risk_level, "中") ||
      identical(qoe_flag, "觀察") || n_alert > 0L) {
    return(list(
      class = "alert-warning", icon = "eye",
      title_key = "funnel_v_watch_title", text_key = "funnel_v_watch_text"
    ))
  }
  # 4) Screens clear on loaded statements (still not a buy signal)
  if (identical(risk_level, "低") && f_score >= 7 &&
      (identical(qoe_flag, "通過") || !nzchar(qoe_flag)) && n_alert == 0L) {
    return(list(
      class = "alert-info", icon = "check-circle",
      title_key = "funnel_v_clear_title", text_key = "funnel_v_clear_text"
    ))
  }
  list(
    class = "alert-secondary", icon = "balance-scale",
    title_key = "funnel_v_mixed_title", text_key = "funnel_v_mixed_text"
  )
}

# -------------------------------------------
# 1. UI：YNOW two-block page + momentum panel
# -------------------------------------------
#' Shared composite valuation block (Bear–Base–Bull, status bar, chart).
#' Main/sub-model + confidence notes render under the chart footer note.
#' Mount once in the model-page header — not on Basic Setup.
decision_valuation_compare_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_valuation_compare"))
}

decision_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "ynow-funnel-report ynow-ynow-page",

      tags$div(
        class = "ynow-funnel-report__masthead",
        h2(tags$b(id = "ynow_funnel_page_title", "YNOW")),
        # Always-visible page chrome — not Notes / 附註. True footnotes stay collapsed.
        tags$p(
          id = "ynow_funnel_page_sub",
          class = "ynow-funnel-report__lead",
          paste0(
            "點選盈餘品質 (QoE)、品質檢核 (F-Score)、財報警訊（含 Beneish M-Score）可捲動至對應區塊。",
            "閱讀順序：財報結論 → 自動化風險矩陣 → 財報體質（QoE／F-Score）→ 財報警訊（M-Score／Schilit）。",
            "YNOW 專注財報操縱／盈餘品質，結論僅依已載入財報數字；不需先完成「決策檢核」Decision SOP；",
            "決策檢核仍可作選用紀律流程。風險矩陣為綜合概覽。",
            "動態產業泡沫與權重集中度在「總體經濟與大盤趨勢」分頁最下方。"
          )
        )
      ),

      fluidRow(
        class = "ynow-funnel-scorecards ynow-funnel-kpi-jump-row",
        column(
          width = 4,
          tags$a(
            href = "#ynow_funnel_verdict",
            class = "ynow-funnel-kpi-jump",
            id = "ynow_kpi_jump_qoe",
            role = "button",
            `aria-label` = "跳至 YNOW 財報結論（QoE 為結論輸入）",
            uiOutput(ns("vbox_qoe"))
          )
        ),
        column(
          width = 4,
          tags$a(
            href = "#ynow_funnel_fscore",
            class = "ynow-funnel-kpi-jump",
            id = "ynow_kpi_jump_fscore",
            role = "button",
            `aria-label` = "跳至品質檢核 (F-Score) 清單",
            uiOutput(ns("vbox_fscore"))
          )
        ),
        column(
          width = 4,
          tags$a(
            href = "#ynow_funnel_ch2",
            class = "ynow-funnel-kpi-jump",
            id = "ynow_kpi_jump_alerts",
            role = "button",
            `aria-label` = "跳至第二章財報警訊（含 Beneish M-Score）",
            uiOutput(ns("vbox_fraud"))
          )
        )
      ),

      # Statement verdict — QoE / F-Score / M-Score / Schilit only; no SOP prerequisite
      tags$section(
        class = "ynow-funnel-chapter ynow-funnel-verdict",
        id = "ynow_funnel_verdict",
        `data-ynow-block` = "verdict",
        tags$div(
          class = "ynow-funnel-chapter__head",
          tags$span(
            class = "ynow-funnel-chapter__kicker",
            id = "ynow_funnel_verdict_kicker",
            "財報結論"
          ),
          tags$h3(
            class = "ynow-funnel-chapter__title",
            id = "ynow_funnel_verdict_title",
            "YNOW 財報結論"
          )
        ),
        tags$div(
          class = "ynow-funnel-chapter__body",
          uiOutput(ns("ui_recommendation")),
          ynow_notes_block(
            tags$p(
              id = "ynow_funnel_verdict_lead",
              class = "ynow-funnel-chapter__lead",
              paste0(
                "依財報數字綜合 QoE、F-Score、Beneish M-Score 與 Schilit 警訊的操縱／品質標籤（非下單指令，亦不含 MOS／股價／動能）。",
                "搜尋載入財報後即可顯示，不需先解鎖「決策檢核」Decision SOP；",
                "決策檢核仍可作選用紀律。下方自動化風險矩陣為並列參考。"
              )
            )
          )
        )
      ),

      # Independent composite risk overview (F-Score × M-Score × AR–Rev × Accruals)
      tags$section(
        class = "ynow-funnel-chapter ynow-funnel-risk",
        id = "ynow_funnel_risk",
        `data-ynow-block` = "risk_matrix",
        tags$div(
          class = "ynow-funnel-chapter__head",
          tags$span(
            class = "ynow-funnel-chapter__kicker",
            id = "ynow_funnel_risk_kicker",
            "綜合概覽"
          ),
          tags$h3(
            class = "ynow-funnel-chapter__title",
            id = "ynow_funnel_risk_title",
            "自動化風險矩陣"
          )
        ),
        tags$div(
          class = "ynow-funnel-chapter__body",
          tags$div(
            class = "ynow-funnel-table-wrap",
            id = "ynow_eq_risk_anchor",
            uiOutput(ns("risk_matrix_panel"))
          ),
          ynow_notes_block(
            tags$p(
              id = "ynow_funnel_risk_lead",
              class = "ynow-funnel-chapter__lead",
              paste0(
                "將 F-Score、Beneish M-Score、應收／營收脫鉤與應計交叉成綜合風險旗標；",
                "屬否決／風險警語，非買進訊號。細節分見下方第一章（品質）與第二章（舞弊／詭計）。"
              )
            )
          )
        )
      ),

      tags$section(
        class = "ynow-funnel-chapter",
        id = "ynow_funnel_ch1",
        `data-ynow-block` = "quality",
        tags$div(
          class = "ynow-funnel-chapter__head",
          tags$span(
            class = "ynow-funnel-chapter__kicker",
            id = "ynow_funnel_ch1_kicker",
            "第一章"
          ),
          tags$h3(
            class = "ynow-funnel-chapter__title",
            id = "ynow_funnel_ch1_title",
            "財報體質"
          )
        ),
        tags$div(
          class = "ynow-funnel-chapter__body",
          tags$div(
            class = "ynow-funnel-table-wrap",
            id = "ynow_eq_dashboard_anchor",
            uiOutput(ns("qoe_dashboard"))
          ),
          h4(
            style = "display:none;",
            tags$span(
              id = "ynow_funnel_fscore_list_title",
              "F-Score 品質檢核清單"
            )
          ),
          tags$div(
            class = "ynow-funnel-table-wrap",
            id = "ynow_funnel_fscore",
            uiOutput(ns("fscore_panel"))
          ),
          ynow_notes_block(
            tags$p(
              id = "ynow_funnel_ch1_lead",
              class = "ynow-funnel-chapter__lead",
              paste0(
                "先看盈餘品質 (Quality of Earnings) 獲利含金量評分，再對照 Piotroski F-Score 九項品質檢核；",
                "通過／未達標僅供品質檢核，不單獨構成買進理由。舞弊／操弄風險見第二章 Beneish M-Score。"
              )
            )
          )
        )
      ),

      tags$section(
        class = "ynow-funnel-chapter",
        id = "ynow_funnel_ch2",
        `data-ynow-block` = "alerts",
        tags$div(
          class = "ynow-funnel-chapter__head",
          tags$span(
            class = "ynow-funnel-chapter__kicker",
            id = "ynow_funnel_ch2_kicker",
            "第二章"
          ),
          tags$h3(
            class = "ynow-funnel-chapter__title",
            id = "ynow_funnel_ch2_title",
            "財報警訊"
          )
        ),
        tags$div(
          class = "ynow-funnel-chapter__body",
          tags$div(
            class = "ynow-funnel-table-wrap",
            id = "ynow_mscore_widget_anchor",
            uiOutput(ns("mscore_widget"))
          ),
          tags$div(
            class = "ynow-funnel-table-wrap",
            uiOutput(ns("shenanigans_panel"))
          ),
          ynow_notes_block(
            tags$p(
              id = "ynow_funnel_ch2_lead",
              class = "ynow-funnel-chapter__lead",
              paste0(
                "Beneish M-Score（盈餘操弄／舞弊風險）與 Schilit 財報詭計自動判讀；",
                "警示／觀察優先展開。屬否決／風險警語，非買進訊號。",
                "上方「自動化風險矩陣」為含 F-Score 的綜合交叉總覽。"
              )
            )
          )
        )
      )
    )
  )
}

#' Momentum explanation + status (below backtest MOS/FV; same id as decision_server)
decision_momentum_panel_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = tagList(
        icon("chart-line"),
        tags$span(id = "ynow_funnel_mom_box_title", "Trend momentum (trading aid)")
      ),
      width = 12, status = "success", solidHeader = FALSE,
      collapsible = TRUE, collapsed = TRUE,
      tags$p(
        style = "margin: 0 0 12px 0; font-size: 12.5px; color: #555; line-height: 1.5;",
        tags$span(
          id = "ynow_funnel_mom_intro",
          paste0(
            "Technical Timing aid only — it does not set fair value or the YNOW statement verdict. ",
            "This panel only asks whether near-term trend has turned bullish, for sizing rhythm."
          )
        )
      ),
      fluidRow(
        valueBoxOutput(ns("vbox_momentum"), width = 4),
        column(
          width = 8,
          uiOutput(ns("ui_momentum_detail"))
        )
      ),
      tags$hr(style = "margin: 8px 0 12px 0; border-top: 1px solid #dfe6e9;"),
      tags$h5(tags$b(tags$span(id = "ynow_funnel_mom_logic_title", "Logic & conditions"))),
      tags$ul(
        style = "font-size: 13px; line-height: 1.55; color: #333; margin-bottom: 10px;",
        tags$li(
          tags$b(id = "ynow_funnel_mom_cond1_label", "Cond1: "),
          tags$span(
            id = "ynow_funnel_mom_cond1",
            "Latest close > SMA(20) and > SMA(60)"
          )
        ),
        tags$li(
          tags$b(id = "ynow_funnel_mom_cond2_label", "Cond2: "),
          tags$span(
            id = "ynow_funnel_mom_cond2",
            "SMA(20) > SMA(60) (short MA above long MA)"
          )
        ),
        tags$li(
          tags$span(
            id = "ynow_funnel_mom_bull_rule",
            "Bullish confirmed when Cond1 and Cond2 both hold; otherwise \"Range / bearish bias\"."
          )
        )
      ),
      tags$h5(tags$b(tags$span(id = "ynow_funnel_mom_data_title", "Data sources"))),
      tags$ul(
        style = "font-size: 13px; line-height: 1.55; color: #333; margin-bottom: 6px;",
        tags$li(tags$span(
          id = "ynow_funnel_mom_data_1",
          "Daily closes: Yahoo Finance (prefer yfinance; fallback quantmod / Yahoo)."
        )),
        tags$li(tags$span(
          id = "ynow_funnel_mom_data_2",
          "Fetch ~1Y history; decision uses ~180 trading days; MAs via TTR::SMA."
        )),
        tags$li(tags$span(
          id = "ynow_funnel_mom_data_3",
          paste0(
            "Unlike Quant Backtest Lab \"sentiment strategy\" momentum / RSI overlays, ",
            "this panel is dual-MA confirmation only for YNOW Funnel Timing."
          )
        ))
      )
    )
  )
}

# -------------------------------------------
# 2. Server：primary band + secondary check + confidence
# -------------------------------------------
decision_server <- function(id, d_is, d_bs, d_cf, intrinsic_val_dcf, intrinsic_val_ddm, current_price, hist_price_data, industry_text,
                            intrinsic_val_pb = reactive(NA),
                            intrinsic_val_nav = reactive(NA),
                            intrinsic_val_ri = reactive(NA),
                            model_rec = reactive(NULL),
                            primary_band = reactive(NULL),
                            secondary_point = reactive(NA),
                            model_points = reactive(NULL),
                            model_point_labels = reactive(NULL),
                            active_model_key = reactive(NA_character_),
                            confidence = reactive(NULL),
                            industry_key = reactive(NULL),
                            ui_locale = reactive("en"),
                            sop_verdict_gate = reactive(list(unlocked = FALSE)),
                            current_ticker = reactive("")) {
  moduleServer(id, function(input, output, session) {

    # YNOW analyzes independently — Decision Checklist / Decision SOP is optional.
    .sop_unlocked <- function() TRUE

    .pick_num <- function(x) {
      x <- suppressWarnings(as.numeric(x)[1])
      if (length(x) != 1 || is.null(x) || is.na(x) || !is.finite(x) || x == 0) NA_real_ else x
    }

    .ui_loc <- function() {
      tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "en")
    }

    .str <- function(key, ...) {
      msg <- ui_str(key, .ui_loc())
      dots <- list(...)
      nms <- names(dots)
      if (length(dots) && !is.null(nms)) {
        for (nm in nms) {
          if (!nzchar(nm)) next
          msg <- gsub(
            paste0("{", nm, "}"),
            as.character(dots[[nm]] %||% ""),
            msg,
            fixed = TRUE
          )
        }
      }
      msg
    }

    .collect_model_points <- function() {
      pts <- tryCatch(model_points(), error = function(e) NULL)
      if (is.list(pts) && length(pts)) {
        out <- lapply(pts, .pick_num)
        if (is.null(names(out)) || !any(nzchar(names(out)))) {
          names(out) <- paste0("m", seq_along(out))
        }
        return(out)
      }
      list(
        dcf = .pick_num(tryCatch(intrinsic_val_dcf(), error = function(e) NA)),
        ddm = .pick_num(tryCatch(intrinsic_val_ddm(), error = function(e) NA)),
        ri  = .pick_num(tryCatch(intrinsic_val_ri(), error = function(e) NA)),
        pb  = .pick_num(tryCatch(intrinsic_val_pb(), error = function(e) NA)),
        nav = .pick_num(tryCatch(intrinsic_val_nav(), error = function(e) NA))
      )
    }

    .point_label <- function(key) {
      labs <- tryCatch(model_point_labels(), error = function(e) NULL)
      if (is.list(labs)) {
        lab <- labs[[as.character(key)[1]]]
        if (nzchar(as.character(lab %||% "")[1])) return(as.character(lab)[1])
      }
      .model_label(key)
    }

    f_score_eval <- reactive({
      empty <- list(
        total = NA_real_,
        quality_flag = NA_real_,
        checklist = data.frame(
          `檢驗維度` = character(0),
          `得分` = character(0),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      )
      is_df <- tryCatch(d_is(), error = function(e) NULL)
      bs_df <- tryCatch(d_bs(), error = function(e) NULL)
      cf_df <- tryCatch(d_cf(), error = function(e) NULL)
      res <- tryCatch(
        compute_report_f_score(is_df, bs_df, cf_df),
        error = function(e) empty
      )
      if (!is.list(res)) res <- empty
      # Checklist still uses 1/0 → pass/fail labels for localize_fscore_checklist
      if (is.data.frame(res$checklist) && nrow(res$checklist) > 0 &&
          is.character(res$checklist$`得分`)) {
        res$checklist$`得分` <- ifelse(res$checklist$`得分` == "通過", 1, 0)
      }
      if (length(res$total) != 1L || !is.finite(res$total)) res$total <- NA_real_
      if (length(res$quality_flag) != 1L || !is.finite(res$quality_flag)) {
        res$quality_flag <- NA_real_
      }
      res
    })

    primary_values <- reactive({
      band <- tryCatch(primary_band(), error = function(e) NULL)
      if (is.list(band) && !is.null(band$base)) {
        return(list(
          bear = .pick_num(band$bear),
          base = .pick_num(band$base),
          bull = .pick_num(band$bull),
          label = as.character(band$label %||% .str("funnel_primary_fallback"))
        ))
      }
      rec <- tryCatch(model_rec(), error = function(e) NULL)
      prim <- as.character(rec$primary %||% "")
      dcf_v <- .pick_num(tryCatch(intrinsic_val_dcf(), error = function(e) NA))
      ddm_v <- .pick_num(tryCatch(intrinsic_val_ddm(), error = function(e) NA))
      pb_v  <- .pick_num(tryCatch(intrinsic_val_pb(), error = function(e) NA))
      nav_v <- .pick_num(tryCatch(intrinsic_val_nav(), error = function(e) NA))
      ri_v  <- .pick_num(tryCatch(intrinsic_val_ri(), error = function(e) NA))
      base <- switch(prim, "dcf" = dcf_v, "ddm" = ddm_v, "pb" = pb_v, "nav" = nav_v, "ri" = ri_v, dcf_v)
      if (is.na(base)) {
        base <- if (!is.na(dcf_v)) dcf_v else if (!is.na(ddm_v)) ddm_v else if (!is.na(ri_v)) ri_v else if (!is.na(nav_v)) nav_v else pb_v
      }
      list(bear = NA_real_, base = base, bull = NA_real_, label = .model_label(prim))
    })

    mos_calc <- reactive({
      base <- primary_values()$base
      curr_p <- .pick_num(current_price())
      # Near-zero Base (RI clean-surplus residue) must not divide — use shared guard.
      if (exists("mos_vs_fair_value", mode = "function")) {
        return(mos_vs_fair_value(base, curr_p))
      }
      if (is.na(base) || is.na(curr_p) || !is.finite(base) || abs(base) < 0.01) {
        return(NA_real_)
      }
      (base - curr_p) / base
    })

    mom_status <- reactive({
      req(hist_price_data())
      prices <- hist_price_data()$Close
      ma20 <- tail(SMA(prices, 20), 1)
      ma60 <- tail(SMA(prices, 60), 1)
      curr_p <- tail(prices, 1)
      cond1 <- curr_p > ma20 && curr_p > ma60
      cond2 <- ma20 > ma60
      list(
        triggered = (isTRUE(cond1) && isTRUE(cond2)),
        cond1 = isTRUE(cond1),
        cond2 = isTRUE(cond2),
        price = if (length(curr_p) == 1 && is.finite(curr_p)) as.numeric(curr_p) else NA_real_,
        ma20 = if (length(ma20) == 1 && is.finite(ma20)) as.numeric(ma20) else NA_real_,
        ma60 = if (length(ma60) == 1 && is.finite(ma60)) as.numeric(ma60) else NA_real_,
        dist_to_ma20 = if (is.na(ma20) || is.na(curr_p) || !is.finite(ma20) || !is.finite(curr_p) || ma20 == 0) {
          NA_real_
        } else {
          (curr_p - ma20) / ma20
        },
        n_obs = length(prices)
      )
    })

    final_recommendation <- reactive({
      .ui_loc() # locale toggle refreshes verdict
      ev <- tryCatch(f_score_eval(), error = function(e) NULL)
      f_score <- if (is.list(ev)) ev$total else NA_real_
      f_quality <- if (is.list(ev)) ev$quality_flag else NA_real_
      eq <- tryCatch(eq_eval(), error = function(e) NULL)
      n_al <- tryCatch(fraud_flag_n(), error = function(e) NA_integer_)
      pack <- ynow_statement_verdict(eq, f_score, f_quality, n_al)
      if (is.null(pack) || !is.list(pack)) return(NULL)
      list(
        class = pack$class,
        icon = pack$icon,
        title = .str(pack$title_key),
        text = .str(pack$text_key)
      )
    })

    .alert_tk <- function() {
      tryCatch(toupper(trimws(as.character(current_ticker() %||% "")[1])), error = function(e) "")
    }

    output$vbox_fscore <- renderUI({
      .ui_loc()
      score <- tryCatch(f_score_eval()$total, error = function(e) NA_real_)
      tk <- .alert_tk()
      if (length(score) != 1L || is.null(score) || is.na(score) || !is.finite(score)) {
        box <- valueBox(
          "—",
          .str("funnel_vbox_fscore"),
          icon = icon("gem"),
          color = "light-blue",
          width = NULL
        )
        if (!exists("ynow_alertable", mode = "function")) return(box)
        return(ynow_alertable(
          box, metric = "f_score", ticker = tk, value = NA_real_,
          label = .str("funnel_vbox_fscore"), ready = FALSE, default_kind = "value_lte"
        ))
      }
      color <- if (score >= 7) "green" else if (score >= 4) "yellow" else "red"
      box <- valueBox(
        paste0(score, " / 9"),
        .str("funnel_vbox_fscore"),
        icon = icon("gem"),
        color = color,
        width = NULL
      )
      if (!exists("ynow_alertable", mode = "function")) return(box)
      ynow_alertable(
        box,
        metric = "f_score",
        ticker = tk,
        value = as.numeric(score),
        label = .str("funnel_vbox_fscore"),
        ready = nzchar(tk),
        default_kind = "value_lte"
      )
    })

    output$vbox_qoe <- renderUI({
      .ui_loc()
      eq <- tryCatch(eq_eval(), error = function(e) NULL)
      tk <- .alert_tk()
      grade <- if (is.list(eq) && isTRUE(eq$ok)) {
        as.character(eq$qoe_grade %||% "")[1]
      } else {
        ""
      }
      score <- if (is.list(eq) && isTRUE(eq$ok)) {
        suppressWarnings(as.numeric(eq$qoe_score)[1])
      } else {
        NA_real_
      }
      subtitle <- .str("funnel_vbox_qoe")
      ready <- nzchar(tk) && nzchar(grade) && grade != "—" && is.finite(score)
      if (!ready) {
        box <- valueBox(
          "—",
          subtitle,
          icon = icon("flask"),
          color = "light-blue",
          width = NULL
        )
        if (!exists("ynow_alertable", mode = "function")) return(box)
        return(ynow_alertable(
          box, metric = "qoe_grade", ticker = tk, value = NA_real_,
          label = subtitle, ready = FALSE, default_kind = "grade_lte"
        ))
      }
      color <- if (grade %in% c("A", "B")) "green" else if (identical(grade, "C")) "yellow" else "red"
      box <- valueBox(
        paste0(grade, " · ", as.integer(round(score))),
        subtitle,
        icon = icon("flask"),
        color = color,
        width = NULL
      )
      if (!exists("ynow_alertable", mode = "function")) return(box)
      g_ord <- if (exists("price_alert_grade_to_ord", mode = "function")) {
        price_alert_grade_to_ord(grade)
      } else {
        NA_real_
      }
      ynow_alertable(
        box,
        metric = "qoe_grade",
        ticker = tk,
        value = g_ord,
        label = subtitle,
        ready = nzchar(tk) && is.finite(g_ord),
        default_kind = "grade_lte"
      )
    })

    shen_eval <- reactive({
      .ui_loc()
      is_df <- tryCatch(d_is(), error = function(e) NULL)
      bs_df <- tryCatch(d_bs(), error = function(e) NULL)
      cf_df <- tryCatch(d_cf(), error = function(e) NULL)
      key <- tryCatch(industry_key(), error = function(e) NULL)
      tryCatch(
        evaluate_shenanigans(is_df, bs_df, cf_df, industry_key = key),
        error = function(e) .empty_shenanigans(ok = FALSE, message = .str("funnel_shen_skip"))
      )
    })

    eq_eval <- reactive({
      .ui_loc()
      is_df <- tryCatch(d_is(), error = function(e) NULL)
      bs_df <- tryCatch(d_bs(), error = function(e) NULL)
      cf_df <- tryCatch(d_cf(), error = function(e) NULL)
      fs <- tryCatch(f_score_eval(), error = function(e) NULL)
      tryCatch(
        evaluate_earnings_quality(is_df, bs_df, cf_df, f_score_pack = fs),
        error = function(e) .eq_empty(ok = FALSE, message = .str("funnel_eq_skip"))
      )
    })

    # Statement-alerts KPI: Schilit n_alert + QoE red_flags
    # (red_flags already includes Beneish M-Score alert when m_flag == 警示).
    fraud_flag_n <- reactive({
      n_shen <- {
        ev <- tryCatch(shen_eval(), error = function(e) NULL)
        if (is.null(ev) || !isTRUE(ev$ok)) NA_integer_ else as.integer(ev$n_alert)
      }
      n_eq <- {
        eq <- tryCatch(eq_eval(), error = function(e) NULL)
        if (is.null(eq) || !isTRUE(eq$ok)) 0L else as.integer(length(eq$red_flags))
      }
      if (!is.finite(n_shen)) return(NA_integer_)
      as.integer(n_shen + n_eq)
    })

    output$vbox_fraud <- renderUI({
      .ui_loc()
      n <- tryCatch(fraud_flag_n(), error = function(e) NA_integer_)
      tk <- .alert_tk()
      if (length(n) != 1L || is.null(n) || is.na(n) || !is.finite(n)) {
        box <- valueBox(
          "—",
          .str("funnel_vbox_fraud"),
          icon = icon("exclamation-triangle"),
          color = "light-blue",
          width = NULL
        )
        if (!exists("ynow_alertable", mode = "function")) return(box)
        return(ynow_alertable(
          box, metric = "statement_alert_count", ticker = tk, value = NA_real_,
          label = .str("funnel_vbox_fraud"), ready = FALSE, default_kind = "value_gte"
        ))
      }
      n <- as.integer(n)
      box <- valueBox(
        .str("funnel_fraud_items", n = n),
        .str("funnel_vbox_fraud"),
        icon = icon("exclamation-triangle"),
        color = if (n > 0L) "red" else "green",
        width = NULL
      )
      if (!exists("ynow_alertable", mode = "function")) return(box)
      ynow_alertable(
        box,
        metric = "statement_alert_count",
        ticker = tk,
        value = as.numeric(n),
        label = .str("funnel_vbox_fraud"),
        ready = nzchar(tk),
        default_kind = "value_gte"
      )
    })

    output$mscore_widget <- renderUI({
      loc <- .ui_loc()
      pack <- tryCatch(eq_eval(), error = function(e) NULL)
      tk <- .alert_tk()
      ui <- beneish_m_score_widget_ui(pack, locale = loc)
      if (!exists("ynow_alertable", mode = "function")) return(ui)
      m <- if (is.list(pack) && isTRUE(pack$ok)) suppressWarnings(as.numeric(pack$m_score)[1]) else NA_real_
      ynow_alertable(
        ui,
        metric = "beneish_m_score",
        ticker = tk,
        value = m,
        label = if (identical(loc, "en")) {
          "Beneish M-Score (earnings-manipulation screen)"
        } else {
          "Beneish M-Score（盈餘操縱風險）"
        },
        ready = nzchar(tk) && is.finite(m),
        default_kind = "value_gte"
      )
    })

    output$qoe_dashboard <- renderUI({
      loc <- .ui_loc()
      pack <- tryCatch(eq_eval(), error = function(e) NULL)
      tk <- .alert_tk()
      ui <- earnings_quality_dashboard_ui(pack, locale = loc, alert_ticker = tk)
      ui
    })

    # Publish YNOW quality metrics for 到值通知 checks (session-scoped).
    observe({
      tk <- .alert_tk()
      if (!nzchar(tk)) return()
      mets <- list()
      sc <- tryCatch(f_score_eval()$total, error = function(e) NA_real_)
      if (is.finite(sc)) mets$f_score <- as.numeric(sc)
      mos <- tryCatch(mos_calc(), error = function(e) NA_real_)
      if (is.finite(mos)) mets$ynow_mos_pct <- as.numeric(mos) * 100
      n_al <- tryCatch(fraud_flag_n(), error = function(e) NA_integer_)
      if (is.finite(n_al)) mets$statement_alert_count <- as.numeric(n_al)
      eq <- tryCatch(eq_eval(), error = function(e) NULL)
      if (is.list(eq) && isTRUE(eq$ok)) {
        m <- suppressWarnings(as.numeric(eq$m_score)[1])
        if (is.finite(m)) mets$beneish_m_score <- m
        if (exists("price_alert_grade_to_ord", mode = "function")) {
          g <- price_alert_grade_to_ord(eq$qoe_grade)
          if (is.finite(g)) mets$qoe_grade <- g
        }
      }
      cur <- session$userData$value_alert_metrics
      if (!is.list(cur)) cur <- list()
      cur[[tk]] <- modifyList(as.list(cur[[tk]] %||% list()), mets)
      session$userData$value_alert_metrics <- cur
    })

    output$risk_matrix_panel <- renderUI({
      loc <- .ui_loc()
      pack <- tryCatch(eq_eval(), error = function(e) NULL)
      earnings_risk_matrix_ui(pack, locale = loc)
    })

    output$shenanigans_panel <- renderUI({
      ev <- tryCatch(shen_eval(), error = function(e) NULL)
      shenanigans_results_ui(ev)
    })

    output$vbox_momentum <- renderValueBox({
      .ui_loc()
      status <- tryCatch(mom_status(), error = function(e) NULL)
      triggered <- is.list(status) && isTRUE(status$triggered)
      color <- if (triggered) "green" else "teal"
      txt <- if (triggered) .str("funnel_mom_bull") else .str("funnel_mom_sideways")
      valueBox(txt, .str("funnel_mom_box_title"), icon = icon("chart-line"), color = color)
    })

    output$ui_momentum_detail <- renderUI({
      .ui_loc()
      status <- tryCatch(mom_status(), error = function(e) NULL)
      if (is.null(status) || !is.list(status)) {
        return(tags$p(
          style = "color:#888; font-size:13px; margin-top:8px;",
          .str("funnel_mom_waiting")
        ))
      }
      fmt_px <- function(x) {
        if (!is.finite(x)) return("—")
        sprintf("%.2f", x)
      }
      fmt_pct <- function(x) {
        if (!is.finite(x)) return("—")
        sprintf("%+.1f%%", x * 100)
      }
      mark <- function(ok) if (isTRUE(ok)) "✅" else "❌"
      tags$div(
        style = "font-size: 13px; line-height: 1.6; color: #333; padding-top: 4px;",
        tags$p(
          style = "margin: 0 0 8px 0;",
          tags$b(.str("funnel_mom_readings")),
          .str(
            "funnel_mom_readings_fmt",
            p = fmt_px(status$price),
            ma20 = fmt_px(status$ma20),
            ma60 = fmt_px(status$ma60),
            dist = fmt_pct(status$dist_to_ma20),
            n = if (is.finite(status$n_obs)) as.integer(status$n_obs) else "—"
          )
        ),
        tags$p(
          style = "margin: 0;",
          .str(
            "funnel_mom_cond_line",
            c1 = mark(status$cond1),
            c2 = mark(status$cond2)
          ),
          tags$b(if (isTRUE(status$triggered)) .str("funnel_mom_bull") else .str("funnel_mom_sideways"))
        )
      )
    })

    output$ui_recommendation <- renderUI({
      .ui_loc()
      rec <- final_recommendation()
      if (is.null(rec) || !is.list(rec)) {
        return(div(
          class = "alert alert-info ynow-waiting-verdict",
          tags$p(
            style = "margin:0;font-size:13px;line-height:1.5;",
            .str("funnel_v_waiting_text")
          )
        ))
      }
      div(class = paste("alert", rec$class),
          h4(icon(rec$icon), " ", rec$title),
          p(rec$text))
    })

    output$fscore_panel <- renderUI({
      loc <- .ui_loc()
      ev <- tryCatch(f_score_eval(), error = function(e) NULL)
      df <- if (is.list(ev)) ev$checklist else NULL
      fscore_results_ui(df, loc)
    })

    output$ui_valuation_compare <- renderUI({
      loc <- .ui_loc()
      str <- function(key) ui_str(key, loc)

      pv <- primary_values()
      p_curr <- .pick_num(current_price())
      if (is.na(p_curr)) {
        return(div(class = "alert alert-info", str("composite_waiting_market")))
      }

      rec <- tryCatch(model_rec(), error = function(e) NULL)
      prim <- as.character(rec$primary %||% "")
      sec <- as.character(rec$secondary %||% "")
      conf <- tryCatch(confidence(), error = function(e) NULL)
      sec_pt <- .pick_num(tryCatch(secondary_point(), error = function(e) NA))
      pts <- .collect_model_points()
      active_key <- as.character(tryCatch(active_model_key(), error = function(e) NA_character_) %||% "")[1]

      bear <- pv$bear
      base <- pv$base
      bull <- pv$bull
      # Do not invent a Base from unrelated models — keep assessment empty until primary band is ready
      model_vals <- unlist(pts, use.names = FALSE)
      model_vals <- model_vals[is.finite(model_vals)]
      has_primary_base <- !is.na(base)
      has_any_model_fv <- length(model_vals) > 0L || !is.na(sec_pt)

      if (!has_primary_base && !has_any_model_fv) {
        return(div(class = "alert alert-info ynow-waiting-val", str("composite_waiting_val")))
      }

      rec_title <- paste0(
        str("composite_main_model"), " ", .point_label(prim),
        if (nzchar(sec)) paste0(" ｜ ", str("composite_sub_model"), " ", .point_label(sec)) else ""
      )
      rec_desc <- if (exists(".pick_rec_reason", mode = "function")) {
        .pick_rec_reason(rec, loc)
      } else if (identical(loc, "en") && nzchar(as.character(rec$reason_en %||% "")[1])) {
        as.character(rec$reason_en)[1]
      } else {
        as.character(rec$reason %||% "")[1]
      }
      score_paren_l <- if (identical(loc, "en")) " (" else "（"
      score_paren_r <- if (identical(loc, "en")) ")" else "）"
      reason_sep <- if (identical(loc, "en")) "; " else "；"
      conf_txt <- if (is.list(conf) && !is.null(conf$level)) {
        paste0(
          str("composite_confidence_prefix"), " ", conf$level,
          if (!is.null(conf$score)) paste0(score_paren_l, conf$score, score_paren_r) else "",
          if (length(conf$reasons)) {
            paste0(" — ", paste(utils::head(conf$reasons, 3), collapse = reason_sep))
          } else {
            ""
          }
        )
      } else {
        paste0(str("composite_confidence_prefix"), " ", str("composite_confidence_calculating"))
      }

      all_vals <- stats::na.omit(c(p_curr, bear, base, bull, sec_pt, model_vals))
      if (!length(all_vals)) {
        return(div(class = "alert alert-info ynow-waiting-val", str("composite_waiting_val")))
      }
      min_val <- min(all_vals) * 0.85
      max_val <- max(all_vals) * 1.15
      range_val <- max(max_val - min_val, 1)
      pos <- function(x) if (is.na(x)) NA_real_ else (x - min_val) / range_val * 100

      pos_curr <- pos(p_curr)
      pos_base <- pos(base)
      pos_bear <- pos(bear)
      pos_bull <- pos(bull)
      band_left <- if (!is.na(pos_bear) && !is.na(pos_bull)) min(pos_bear, pos_bull) else 0
      band_width <- if (!is.na(pos_bear) && !is.na(pos_bull)) abs(pos_bull - pos_bear) else 0
      band_opacity <- if (band_width > 0) 0.85 else 0
      base_opacity <- if (is.na(pos_base)) 0 else 1
      pos_base_css <- if (is.na(pos_base)) 0 else pos_base

      # Assessment verdict only when primary Base is available (no cold-load Fair/Overvalued)
      if (isTRUE(has_primary_base)) {
        status_text <- str("composite_fair")
        status_color <- "#f39c12"
        if (p_curr < base * 0.8) {
          status_text <- str("composite_undervalued")
          status_color <- "#00a65a"
        } else if (p_curr > base * 1.2) {
          status_text <- str("composite_overvalued")
          status_color <- "#d9534f"
        }
        status_html <- paste0(
          "<span style='color: ", status_color, ";'>",
          htmltools::htmlEscape(status_text), "</span>"
        )
      } else {
        status_color <- "#bdc3c7"
        status_html <- paste0(
          "<span style='color: #95a5a6; font-weight: 500;'>",
          htmltools::htmlEscape(str("composite_status_pending")), "</span>"
        )
      }

      px_sym <- tryCatch(money_prefix(), error = function(e) "$")
      if (!nzchar(as.character(px_sym %||% "")[1])) px_sym <- "$"
      fmt <- function(x) {
        if (is.na(x) || !is.finite(x)) return("—")
        # Collapse float residue (±1e-15) so Base does not show "-0.00"
        if (abs(x) < 0.005) x <- 0
        paste0(px_sym, sprintf("%.2f", x))
      }
      upside <- if (!is.na(base) && is.finite(base) && p_curr > 0) {
        (base - p_curr) / p_curr * 100
      } else {
        NA_real_
      }
      upside_txt <- if (is.na(upside) || !is.finite(upside)) "—" else sprintf("%+.1f%%", upside)

      model_colors <- c(
        dcf = "#2980b9", ddm = "#8e44ad", ri = "#16a085",
        pb = "#d35400", nav = "#d81b60",
        multiples = "#7f8c8d", sotp = "#5d6d7e"
      )
      # Model FV markers: same visual stack as Current price (pill + circle + $),
      # same vertical baseline (top: -10px) — not staggered ticks below the axis.
      overlay_html <- ""
      overlay_keys <- names(pts)
      if (is.null(overlay_keys)) overlay_keys <- character(0)
      for (k in overlay_keys) {
        v <- .pick_num(pts[[k]])
        if (is.na(v)) next
        p_x <- pos(v)
        if (is.na(p_x)) next
        col <- unname(model_colors[[k]] %||% "#566573")
        is_active <- nzchar(active_key) && identical(k, active_key)
        is_prim <- nzchar(prim) && identical(k, prim)
        is_sec <- nzchar(sec) && identical(k, sec)
        lab <- .point_label(k)
        z <- if (is_active) 12 else if (is_prim) 9 else 8
        role_tag <- if (is_prim) "★" else if (is_sec) "◇" else ""
        pill_label <- paste0(role_tag, lab)
        overlay_html <- paste0(
          overlay_html,
          "<div class='ynow-composite-model-mark' style='position:absolute; top:-10px; left:",
          p_x, "%; transform:translateX(-50%); z-index:", z,
          "; text-align:center;'>",
          "<div style='font-size:12px; color:white; background:", col,
          "; padding:2px 6px; border-radius:4px; white-space:nowrap;",
          if (is_active) " box-shadow:0 0 0 2px rgba(44,62,80,0.35);" else "",
          "'>",
          htmltools::htmlEscape(pill_label), "</div>",
          "<div style='width:12px; height:12px; background:", col,
          "; border:2px solid white; border-radius:50%; margin:2px auto;'></div>",
          "<div style='font-size:15px; color:", col, "; font-weight:bold;'>",
          htmltools::htmlEscape(fmt(v)),
          "</div></div>"
        )
      }
      axis_height <- 80
      tk_alert <- .alert_tk()
      .band_card <- function(label, value_html, meta_html = NULL, bg, value_color, value_size = "20px") {
        tags$div(
          class = "ynow-composite-band-card",
          style = paste0(
            "flex:1; min-width:120px; padding:10px; background:", bg,
            "; border-radius:6px; position:relative;"
          ),
          tags$div(style = "font-size:12px; color:#888;", label),
          tags$div(
            style = paste0("font-size:", value_size, "; font-weight:700; color:", value_color, ";"),
            HTML(value_html)
          ),
          if (!is.null(meta_html)) tags$div(style = "font-size:12px; color:#555;", HTML(meta_html))
        )
      }
      bear_card <- .band_card("Bear", htmltools::htmlEscape(fmt(bear)), NULL, "#fdf2f2", "#c0392b")
      base_card <- .band_card(
        "Base",
        htmltools::htmlEscape(fmt(base)),
        paste0(
          htmltools::htmlEscape(str("composite_upside_prefix")),
          htmltools::htmlEscape(upside_txt)
        ),
        "#f5f5f5", "#1a1a1a", value_size = "22px"
      )
      bull_card <- .band_card("Bull", htmltools::htmlEscape(fmt(bull)), NULL, "#eafaf1", "#1e8449")
      sec_card <- .band_card(
        str("composite_secondary_check"),
        htmltools::htmlEscape(fmt(sec_pt)),
        htmltools::htmlEscape(.point_label(sec)),
        "#f4f6f7", "#566573", value_size = "18px"
      )
      # Primary Base + secondary check: Value Alert targets (到值通知)
      if (exists("ynow_alertable", mode = "function")) {
        base_card <- ynow_alertable(
          base_card,
          metric = "fair_value",
          ticker = tk_alert,
          value = if (is.finite(base)) base else NA_real_,
          label = paste(str("composite_main_model"), .point_label(prim), "Base"),
          ready = nzchar(tk_alert) && is.finite(base) && base > 0,
          default_kind = "below_fair",
          fair_value = if (is.finite(base)) base else NA_real_
        )
        sec_card <- ynow_alertable(
          sec_card,
          metric = "fair_value_secondary",
          ticker = tk_alert,
          value = if (is.finite(sec_pt)) sec_pt else NA_real_,
          label = paste(str("composite_sub_model"), .point_label(sec)),
          ready = nzchar(tk_alert) && nzchar(sec) && is.finite(sec_pt) && sec_pt > 0,
          default_kind = "below_fair",
          fair_value = if (is.finite(sec_pt)) sec_pt else NA_real_
        )
      }

      tags$div(
        class = "ynow-composite-valuation",
        style = paste0(
          "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); ",
          "margin-bottom: 12px; border-top: 3px solid ", status_color, ";"
        ),
        tags$div(
          style = "display:flex; gap:14px; flex-wrap:wrap; margin-bottom: 18px;",
          bear_card, base_card, bull_card, sec_card
        ),
        HTML(paste0(
          "<h4 style='margin-top: 0; font-weight: bold;'><i class='fa fa-balance-scale'></i> ",
          htmltools::htmlEscape(str("composite_status_prefix")),
          status_html, "</h4>",
          "<div style='position: relative; height: ", axis_height, "px; margin-top: 28px; margin-bottom: 8px;'>",
          "<div style='position: absolute; top: 28px; left: 0; right: 0; height: 10px; background: #ecf0f1; border-radius: 5px;'></div>",
          "<div style='position: absolute; top: 28px; left: ", band_left, "%; width: ", band_width,
          "%; height: 10px; background: #aed6f1; border-radius: 5px; opacity: ", band_opacity, ";'></div>",
          "<div style='position: absolute; top: 0; left: ", pos_base_css, "%; transform: translateX(-50%); opacity: ", base_opacity, ";'>",
          "<div style='font-size: 11px; color: #7f8c8d;'>Base</div>",
          "<div style='width: 3px; height: 35px; background: #1a1a1a; margin: 0 auto;'></div></div>",
          "<div style='position: absolute; top: -10px; left: ", pos_curr, "%; transform: translateX(-50%); z-index: 10;'>",
          "<div style='font-size: 12px; color: white; background: #2c3e50; padding: 2px 6px; border-radius: 4px;'>",
          htmltools::htmlEscape(str("composite_current_price")), "</div>",
          "<div style='width: 12px; height: 12px; background: #2c3e50; border: 2px solid white; border-radius: 50%; margin: 2px auto;'></div>",
          "<div style='font-size: 15px; color: #2c3e50; font-weight: bold;'>",
          htmltools::htmlEscape(fmt(p_curr)), "</div></div>",
          overlay_html,
          "</div>",
          "<div class='ynow-composite-chart-notes' style='margin-top: 8px;'>",
          "<p class='ynow-composite-footer-note' style='margin: 0 0 10px 0; font-size: 12px; color: #888;'>",
          htmltools::htmlEscape(str("composite_footer_note")), "</p>",
          "<div class='ynow-composite-model-notes' style='background: #1a1a1a15; border-left: 5px solid #222222; padding: 12px; border-radius: 4px;'>",
          "<h5 style='color: #222222; margin-top: 0; margin-bottom: 6px; font-weight: bold;'>", htmltools::htmlEscape(rec_title), "</h5>",
          "<p style='margin-bottom: 6px; font-size: 13px; color: #555;'>", htmltools::htmlEscape(rec_desc), "</p>",
          "<p style='margin: 0; font-size: 12.5px; color: #333;'><b>", htmltools::htmlEscape(conf_txt), "</b></p>",
          "</div>",
          "</div>"
        ))
      )
    })
  })
}
