# =========================================================================
# Investment Decision Scorecard — Decision Funnel + composite valuation
# Dimensions: Quality (F-Score) -> Value (MOS) -> Timing (momentum, aid)
# Funnel copy: ui_str / funnel_* keys (en-US + zh-TW)
# =========================================================================

library(shiny)
library(shinydashboard)
library(TTR)
library(glue)

# -------------------------------------------
# 1. UI：Decision Funnel + momentum panel
# -------------------------------------------
#' Shared composite valuation block (main/sub model, Bear–Base–Bull, status bar).
#' Mount once in the model-page header — not on Basic Setup.
decision_valuation_compare_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_valuation_compare"))
}

decision_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("vbox_fscore"), width = 4),
      uiOutput(ns("vbox_mos")),
      uiOutput(ns("vbox_fraud"))
    ),
    fluidRow(
      # Verdict on top; F-Score checklist below (no outer box chrome / title)
      column(
        width = 12,
        uiOutput(ns("ui_recommendation"))
      ),
      column(
        width = 12,
        style = "margin-top: 12px;",
        h4(tags$span(
          id = "ynow_funnel_fscore_list_title",
          "F-Score quality checklist"
        )),
        tableOutput(ns("table_checklist"))
      )
    ),
    fluidRow(
      column(
        width = 12,
        style = "padding: 0 15px 8px 15px;",
        uiOutput(ns("shenanigans_panel"))
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
      width = 12, status = "success", solidHeader = TRUE,
      collapsible = TRUE, collapsed = FALSE,
      tags$p(
        style = "margin: 0 0 12px 0; font-size: 12.5px; color: #555; line-height: 1.5;",
        tags$span(
          id = "ynow_funnel_mom_intro",
          paste0(
            "Technical Timing aid only — it does not set fair value. ",
            "The Decision Funnel prioritizes F-Score / MOS; this panel only asks whether ",
            "near-term trend has turned bullish, for sizing rhythm."
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
          tags$b("Cond1: "),
          tags$span(
            id = "ynow_funnel_mom_cond1",
            "Latest close > SMA(20) and > SMA(60)"
          )
        ),
        tags$li(
          tags$b("Cond2: "),
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
            "Unlike Testing \"sentiment strategy\" momentum / RSI overlays, ",
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
                            active_model_key = reactive(NA_character_),
                            confidence = reactive(NULL),
                            industry_key = reactive(NULL),
                            ui_locale = reactive("en")) {
  moduleServer(id, function(input, output, session) {

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

    f_score_eval <- reactive({
      req(d_is(), d_bs(), d_cf())
      res <- compute_report_f_score(d_is(), d_bs(), d_cf())
      # Checklist table still uses 1/0 → pass/fail labels
      if (is.data.frame(res$checklist) && nrow(res$checklist) > 0 &&
          is.character(res$checklist$`得分`)) {
        res$checklist$`得分` <- ifelse(res$checklist$`得分` == "通過", 1, 0)
      }
      if (!is.finite(res$total)) res$total <- 0
      if (!is.finite(res$quality_flag)) res$quality_flag <- 0
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
      if (is.na(base) || is.na(curr_p) || base == 0) return(NA_real_)
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
      f_score <- f_score_eval()$total
      f_quality <- f_score_eval()$quality_flag
      mos <- mos_calc()
      mom <- mom_status()
      if (f_score < 4 || f_quality == 0) {
        return(list(
          class = "alert-danger", icon = "skull-crossbones",
          title = .str("funnel_v_trap_title"),
          text = .str("funnel_v_trap_text")
        ))
      }
      if (!is.na(mos) && mos < 0) {
        if (mom$triggered) {
          return(list(
            class = "alert-warning", icon = "fire",
            title = .str("funnel_v_hot_over_title"),
            text = .str("funnel_v_hot_over_text")
          ))
        }
        return(list(
          class = "alert-warning", icon = "hourglass-half",
          title = .str("funnel_v_over_weak_title"),
          text = .str("funnel_v_over_weak_text")
        ))
      }
      if (!is.na(mos) && mos >= 0.2) {
        if (mom$triggered) {
          return(list(
            class = "alert-success", icon = "rocket",
            title = .str("funnel_v_davis_title"),
            text = .str("funnel_v_davis_text")
          ))
        }
        return(list(
          class = "alert-info", icon = "anchor",
          title = .str("funnel_v_value_wait_title"),
          text = .str("funnel_v_value_wait_text")
        ))
      }
      list(
        class = "alert-secondary", icon = "balance-scale",
        title = .str("funnel_v_neutral_title"),
        text = .str("funnel_v_neutral_text")
      )
    })

    output$vbox_fscore <- renderValueBox({
      .ui_loc()
      score <- f_score_eval()$total
      color <- if (score >= 7) "green" else if (score >= 4) "yellow" else "red"
      valueBox(
        paste0(score, " / 9"),
        .str("funnel_vbox_fscore"),
        icon = icon("gem"),
        color = color
      )
    })

    output$vbox_mos <- renderUI({
      .ui_loc()
      val <- tryCatch(mos_calc(), error = function(e) NA_real_)
      if (length(val) != 1L || is.null(val) || is.na(val) || !is.finite(val)) {
        return(NULL)
      }
      conf <- tryCatch(confidence(), error = function(e) NULL)
      conf_lab <- if (is.list(conf) && !is.null(conf$level)) {
        .str("funnel_vbox_mos_conf", level = conf$level)
      } else {
        ""
      }
      v_pct <- round(as.numeric(val) * 100, 1)
      color <- if (v_pct >= 20) "green" else if (v_pct >= 0) "yellow" else "red"
      valueBox(
        paste0(v_pct, "%"),
        paste0(.str("funnel_vbox_mos"), conf_lab),
        icon = icon("shield-halved"),
        color = color,
        width = 4
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

    fraud_flag_n <- reactive({
      ev <- tryCatch(shen_eval(), error = function(e) NULL)
      if (is.null(ev) || !isTRUE(ev$ok)) return(NA_integer_)
      as.integer(ev$n_alert)
    })

    output$vbox_fraud <- renderUI({
      .ui_loc()
      n <- tryCatch(fraud_flag_n(), error = function(e) NA_integer_)
      if (length(n) != 1L || is.null(n) || is.na(n) || !is.finite(n)) return(NULL)
      n <- as.integer(n)
      valueBox(
        .str("funnel_fraud_items", n = n),
        .str("funnel_vbox_fraud"),
        icon = icon("exclamation-triangle"),
        color = if (n > 0L) "red" else "green",
        width = 4
      )
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
      rec <- final_recommendation()
      div(class = paste("alert", rec$class),
          h4(icon(rec$icon), " ", rec$title),
          p(rec$text))
    })

    output$table_checklist <- renderTable({
      .ui_loc()
      df <- f_score_eval()$checklist
      if (nrow(df) > 0) {
        df$`得分` <- ifelse(df$`得分` == 1, .str("funnel_pass"), .str("funnel_fail"))
      }
      df
    }, striped = TRUE, hover = TRUE, width = "100%")

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
        return(div(class = "alert alert-info", str("composite_waiting_val")))
      }

      rec_title <- paste0(
        str("composite_main_model"), " ", .model_label(prim),
        if (nzchar(sec)) paste0(" ｜ ", str("composite_sub_model"), " ", .model_label(sec)) else ""
      )
      rec_desc <- as.character(rec$reason %||% "")
      conf_txt <- if (is.list(conf) && !is.null(conf$level)) {
        paste0(
          str("composite_confidence_prefix"), " ", conf$level,
          if (!is.null(conf$score)) paste0("（", conf$score, "）") else "",
          if (length(conf$reasons)) paste0(" — ", paste(utils::head(conf$reasons, 3), collapse = "；")) else ""
        )
      } else {
        paste0(str("composite_confidence_prefix"), " ", str("composite_confidence_calculating"))
      }

      all_vals <- stats::na.omit(c(p_curr, bear, base, bull, sec_pt, model_vals))
      if (!length(all_vals)) {
        return(div(class = "alert alert-info", str("composite_waiting_val")))
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

      fmt <- function(x) if (is.na(x)) "—" else sprintf("$%.2f", x)
      upside <- if (!is.na(base) && p_curr > 0) (base - p_curr) / p_curr * 100 else NA_real_
      upside_txt <- if (is.na(upside)) "—" else sprintf("%+.1f%%", upside)

      model_colors <- c(
        dcf = "#2980b9", ddm = "#8e44ad", ri = "#16a085",
        pb = "#d35400", nav = "#d81b60"
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
        lab <- .model_label(k)
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
          htmltools::htmlEscape(sprintf("$%.2f", v)),
          "</div></div>"
        )
      }
      axis_height <- 80

      HTML(paste0(
        "<div class='ynow-composite-valuation' style='background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 12px; border-top: 3px solid ", status_color, ";'>",
        "<div style='background: #1a1a1a15; border-left: 5px solid #222222; padding: 12px; border-radius: 4px; margin-bottom: 16px;'>",
        "<h5 style='color: #222222; margin-top: 0; font-weight: bold;'>", htmltools::htmlEscape(rec_title), "</h5>",
        "<p style='margin-bottom: 6px; font-size: 13px; color: #555;'>", htmltools::htmlEscape(rec_desc), "</p>",
        "<p style='margin: 0; font-size: 12.5px; color: #333;'><b>", htmltools::htmlEscape(conf_txt), "</b></p>",
        "</div>",
        "<div style='display:flex; gap:14px; flex-wrap:wrap; margin-bottom: 18px;'>",
        "<div style='flex:1; min-width:120px; padding:10px; background:#fdf2f2; border-radius:6px;'>",
        "<div style='font-size:12px; color:#888;'>Bear</div>",
        "<div style='font-size:20px; font-weight:700; color:#c0392b;'>", fmt(bear), "</div></div>",
        "<div style='flex:1; min-width:120px; padding:10px; background:#f5f5f5; border-radius:6px;'>",
        "<div style='font-size:12px; color:#888;'>Base</div>",
        "<div style='font-size:22px; font-weight:700; color:#1a1a1a;'>", fmt(base), "</div>",
        "<div style='font-size:12px; color:#555;'>", htmltools::htmlEscape(str("composite_upside_prefix")), upside_txt, "</div></div>",
        "<div style='flex:1; min-width:120px; padding:10px; background:#eafaf1; border-radius:6px;'>",
        "<div style='font-size:12px; color:#888;'>Bull</div>",
        "<div style='font-size:20px; font-weight:700; color:#1e8449;'>", fmt(bull), "</div></div>",
        "<div style='flex:1; min-width:120px; padding:10px; background:#f4f6f7; border-radius:6px;'>",
        "<div style='font-size:12px; color:#888;'>", htmltools::htmlEscape(str("composite_secondary_check")), "</div>",
        "<div style='font-size:18px; font-weight:700; color:#566573;'>", fmt(sec_pt), "</div>",
        "<div style='font-size:12px; color:#777;'>", htmltools::htmlEscape(.model_label(sec)), "</div></div>",
        "</div>",
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
        "<div style='font-size: 15px; color: #2c3e50; font-weight: bold;'>$", round(p_curr, 2), "</div></div>",
        overlay_html,
        "</div>",
        "<p style='margin: 8px 0 0 0; font-size: 12px; color: #888;'>",
        htmltools::htmlEscape(str("composite_footer_note")), "</p>",
        "</div>"
      ))
    })
  })
}
