# =========================================================================
# 投資決策通過檢核表（Decision Checklist）— 可自訂參數、通過／否決摘要
# 啟發式預設；非學術標準。HFV 僅作否決工具，不作看漲依據。
# =========================================================================

.DC_ITEM_DEFS <- list(
  bear_base = list(
    default_on = TRUE,
    mandatory_suggest = TRUE,
    conds = list(
      bear_mos_floor = list(default = -10, step = 1, min = -50, max = 50)
    )
  ),
  base_mos = list(
    default_on = TRUE,
    conds = list(
      base_mos_floor = list(default = 15, step = 1, min = -50, max = 80)
    )
  ),
  g_sgr = list(
    default_on = TRUE,
    conds = list(
      g_sgr_gap_min = list(default = 0.5, step = 0.1, min = 0, max = 20),
      sgr_wacc_buffer = list(default = 2, step = 0.25, min = 0, max = 20)
    )
  ),
  model_align = list(
    default_on = TRUE,
    conds = list()
  ),
  hfv_veto = list(
    default_on = TRUE,
    mandatory_suggest = TRUE,
    conds = list(
      max_c_freq = list(default = 35, step = 1, min = 0, max = 100)
    )
  ),
  fscore = list(
    default_on = TRUE,
    conds = list(
      fscore_min = list(default = 5, step = 1, min = 0, max = 9)
    )
  ),
  no_rank_chase = list(
    default_on = TRUE,
    conds = list()
  )
)

.dc_chk_id <- function(item) paste0("chk_", item)
.dc_cond_id <- function(item, cond) paste0("cond_", item, "_", cond)

.dc_checks_ui <- function() {
  items <- names(.DC_ITEM_DEFS)
  tagList(
    tags$p(
      id = "ynow_dc_panel_hint",
      class = "ynow-dc-panel-hint",
      "Check items to include them in the gate. Condition inputs appear only after the parent box is checked."
    ),
    lapply(items, function(item) {
      def <- .DC_ITEM_DEFS[[item]]
      chk <- .dc_chk_id(item)
      cond_names <- names(def$conds)
      tags$div(
        class = "ynow-dc-item",
        `data-dc-item` = item,
        checkboxInput(
          chk,
          label = tags$span(
            class = "ynow-dc-label-wrap",
            tags$span(class = "ynow-dc-label", id = paste0("ynow_dc_label_", item), item),
            if (isTRUE(def$mandatory_suggest)) {
              tags$span(class = "ynow-dc-badge", id = paste0("ynow_dc_badge_", item), "Default ON")
            } else {
              NULL
            }
          ),
          value = isTRUE(def$default_on)
        ),
        tags$p(class = "ynow-dc-hint", id = paste0("ynow_dc_hint_", item), ""),
        if (length(cond_names) > 0L || identical(item, "model_align")) {
          conditionalPanel(
            condition = sprintf("input['%s'] == true", chk),
            tags$div(
              class = "ynow-dc-conds",
              if (identical(item, "model_align")) {
                selectInput(
                  "dc_user_primary",
                  "Adopted primary model",
                  choices = c("DCF" = "dcf", "DDM" = "ddm", "RI" = "ri", "P/B" = "pb", "NAV" = "nav"),
                  selected = "dcf",
                  width = "100%"
                )
              } else {
                NULL
              },
              lapply(cond_names, function(cn) {
                meta <- def$conds[[cn]]
                numericInput(
                  .dc_cond_id(item, cn),
                  cn,
                  value = meta$default,
                  min = meta$min,
                  max = meta$max,
                  step = meta$step,
                  width = "100%"
                )
              })
            )
          )
        } else {
          NULL
        }
      )
    })
  )
}

decision_checklist_tab_ui <- function() {
  tabItem(
    tabName = "decision_checklist",
    fluidRow(
      column(
        width = 12,
        h2(tags$b(id = "ynow_dc_page_title", "Decision Checklist")),
        p(
          id = "ynow_dc_page_sub",
          "Pick checklist items that match your investment decision style, then set condition values."
        ),
        tags$hr()
      )
    ),
    fluidRow(
      column(
        width = 7,
        box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = tagList(icon("tasks"), tags$span(id = "ynow_dc_box_checks", "Checklist & Conditions")),
          .dc_checks_ui()
        )
      ),
      column(
        width = 5,
        box(
          width = 12, status = "success", solidHeader = TRUE,
          title = tagList(icon("clipboard-check"), tags$span(id = "ynow_dc_box_summary", "Pass / Fail Summary")),
          uiOutput("dc_live_snapshot"),
          tags$hr(style = "margin: 10px 0;"),
          uiOutput("dc_summary_panel")
        )
      )
    )
  )
}

.dc_eval_item <- function(item, on, conds, ctx) {
  if (!isTRUE(on)) {
    return(list(status = "skip", detail = ctx$str_skip))
  }
  switch(
    item,
    bear_base = {
      mb <- ctx$mos_bear
      ms <- ctx$mos_base
      floor_pct <- suppressWarnings(as.numeric(conds$bear_mos_floor)[1])
      if (!is.finite(floor_pct)) floor_pct <- -10
      if (!is.finite(mb) || !is.finite(ms)) {
        return(list(status = "na", detail = ctx$str_no_data))
      }
      if (is.finite(ctx$fv_bear) && is.finite(ctx$fv_base) && ctx$fv_bear > ctx$fv_base + 1e-9) {
        return(list(
          status = "fail",
          detail = sprintf(ctx$str_bear_base_order_fail, ctx$fv_bear, ctx$fv_base)
        ))
      }
      if (mb * 100 < floor_pct) {
        return(list(
          status = "fail",
          detail = sprintf(ctx$str_bear_mos_fail, mb * 100, floor_pct)
        ))
      }
      list(status = "pass", detail = sprintf(ctx$str_bear_base_pass, mb * 100, ms * 100))
    },
    base_mos = {
      ms <- ctx$mos_base
      floor_pct <- suppressWarnings(as.numeric(conds$base_mos_floor)[1])
      if (!is.finite(floor_pct)) floor_pct <- 15
      if (!is.finite(ms)) {
        return(list(status = "na", detail = ctx$str_no_data))
      }
      if (ms * 100 < floor_pct) {
        return(list(status = "fail", detail = sprintf(ctx$str_base_mos_fail, ms * 100, floor_pct)))
      }
      list(status = "pass", detail = sprintf(ctx$str_base_mos_pass, ms * 100, floor_pct))
    },
    g_sgr = {
      g_near <- ctx$g_near_pct
      sgr <- ctx$sgr_pct
      wacc <- ctx$wacc_pct
      gap_min <- suppressWarnings(as.numeric(conds$g_sgr_gap_min)[1])
      buf <- suppressWarnings(as.numeric(conds$sgr_wacc_buffer)[1])
      if (!is.finite(gap_min)) gap_min <- 0.5
      if (!is.finite(buf)) buf <- 2
      if (!is.finite(g_near) || !is.finite(sgr) || !is.finite(wacc)) {
        return(list(status = "na", detail = ctx$str_no_data))
      }
      gap <- abs(g_near - sgr)
      ok_gap <- gap + 1e-9 >= gap_min
      ok_buf <- (wacc - sgr) + 1e-9 >= buf
      if (!ok_gap || !ok_buf) {
        return(list(
          status = "fail",
          detail = sprintf(ctx$str_g_sgr_fail, g_near, sgr, gap, gap_min, wacc, buf)
        ))
      }
      list(status = "pass", detail = sprintf(ctx$str_g_sgr_pass, g_near, sgr, gap, wacc - sgr))
    },
    model_align = {
      rec <- ctx$rec_primary
      chosen <- ctx$user_primary
      if (!nzchar(rec)) {
        return(list(status = "na", detail = ctx$str_no_rec))
      }
      if (!nzchar(chosen)) {
        return(list(status = "na", detail = ctx$str_no_data))
      }
      if (!identical(chosen, rec)) {
        return(list(
          status = "fail",
          detail = sprintf(ctx$str_model_align_fail, toupper(chosen), toupper(rec))
        ))
      }
      list(status = "pass", detail = sprintf(ctx$str_model_align_pass, toupper(rec)))
    },
    hfv_veto = {
      sc <- ctx$hfv_scenarios
      max_c <- suppressWarnings(as.numeric(conds$max_c_freq)[1])
      if (!is.finite(max_c)) max_c <- 35
      if (is.null(sc) || !is.list(sc) || as.integer(sc$n %||% 0L) < 1L) {
        return(list(status = "na", detail = ctx$str_hfv_no_data))
      }
      frq <- sc$freq
      p_c <- if (!is.null(frq) && "C" %in% names(frq)) {
        suppressWarnings(as.numeric(frq[["C"]])[1])
      } else {
        NA_real_
      }
      lead <- as.character(sc$most_frequent %||% NA_character_)[1]
      p_c_pct <- if (is.finite(p_c)) p_c * 100 else NA_real_
      if (identical(lead, "C")) {
        return(list(
          status = "fail",
          detail = sprintf(ctx$str_hfv_c_lead_fail, if (is.finite(p_c_pct)) p_c_pct else NA_real_)
        ))
      }
      if (is.finite(p_c_pct) && p_c_pct >= max_c) {
        return(list(
          status = "fail",
          detail = sprintf(ctx$str_hfv_c_freq_fail, p_c_pct, max_c)
        ))
      }
      list(
        status = "pass",
        detail = sprintf(
          ctx$str_hfv_veto_pass,
          if (nzchar(lead) && !is.na(lead)) lead else "—",
          if (is.finite(p_c_pct)) p_c_pct else NA_real_,
          max_c
        )
      )
    },
    fscore = {
      fs <- ctx$fscore
      mn <- suppressWarnings(as.numeric(conds$fscore_min)[1])
      if (!is.finite(mn)) mn <- 5
      if (!is.finite(fs)) {
        return(list(status = "na", detail = ctx$str_no_data))
      }
      if (fs < mn) {
        return(list(status = "fail", detail = sprintf(ctx$str_fscore_fail, fs, mn)))
      }
      list(status = "pass", detail = sprintf(ctx$str_fscore_pass, fs, mn))
    },
    no_rank_chase = {
      list(status = "pass", detail = ctx$str_no_rank_pass)
    },
    list(status = "na", detail = ctx$str_no_data)
  )
}

decision_checklist_server <- function(
    input, output, session,
    ui_locale,
    primary_band,
    current_price,
    model_rec,
    g_near_pct,
    sgr_pct,
    wacc_pct,
    hfv_scenarios,
    fscore_total
) {
  .pick <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (length(x) != 1L || is.null(x) || is.na(x) || !is.finite(x)) NA_real_ else x
  }

  .mos <- function(fv, px) {
    fv <- .pick(fv)
    px <- .pick(px)
    if (!is.finite(fv) || !is.finite(px) || fv == 0) return(NA_real_)
    (fv - px) / fv
  }

  .dc_apply_locale_labels <- function(loc) {
    session$sendCustomMessage("ynowDcLocale", list(
      locale = loc,
      panel_hint = ui_str("dc_panel_hint", loc),
      badge = ui_str("dc_badge_default_on", loc),
      items = lapply(names(.DC_ITEM_DEFS), function(item) {
        def <- .DC_ITEM_DEFS[[item]]
        list(
          id = item,
          label = ui_str(paste0("dc_label_", item), loc),
          hint = ui_str(paste0("dc_hint_", item), loc),
          mandatory = isTRUE(def$mandatory_suggest),
          conds = lapply(names(def$conds), function(cn) {
            list(
              input_id = .dc_cond_id(item, cn),
              label = ui_str(paste0("dc_cond_", cn), loc)
            )
          })
        )
      })
    ))
  }

  observe({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    .dc_apply_locale_labels(loc)
  })

  observe({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    rec <- tryCatch(model_rec(), error = function(e) NULL)
    prim <- as.character(rec$primary %||% "")[1]
    choices <- c(
      stats::setNames("dcf", ui_str("dc_model_dcf", loc)),
      stats::setNames("ddm", ui_str("dc_model_ddm", loc)),
      stats::setNames("ri", ui_str("dc_model_ri", loc)),
      stats::setNames("pb", ui_str("dc_model_pb", loc)),
      stats::setNames("nav", ui_str("dc_model_nav", loc))
    )
    sel <- isolate(input$dc_user_primary)
    if (is.null(sel) || !sel %in% unname(choices)) {
      sel <- if (nzchar(prim) && prim %in% unname(choices)) prim else "dcf"
    }
    updateSelectInput(
      session, "dc_user_primary",
      label = ui_str("dc_cond_user_primary", loc),
      choices = choices,
      selected = sel
    )
  })

  observeEvent(model_rec(), {
    rec <- model_rec()
    prim <- as.character(rec$primary %||% "")[1]
    if (!nzchar(prim)) return()
    cur <- isolate(input$dc_user_primary)
    if (is.null(cur) || !nzchar(as.character(cur)) || identical(cur, prim)) {
      updateSelectInput(session, "dc_user_primary", selected = prim)
    }
  }, ignoreNULL = TRUE)

  live_ctx <- reactive({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    band <- tryCatch(primary_band(), error = function(e) NULL)
    px <- tryCatch(.pick(current_price()), error = function(e) NA_real_)
    fv_bear <- if (is.list(band)) .pick(band$bear) else NA_real_
    fv_base <- if (is.list(band)) .pick(band$base) else NA_real_
    rec <- tryCatch(model_rec(), error = function(e) NULL)
    list(
      mos_bear = .mos(fv_bear, px),
      mos_base = .mos(fv_base, px),
      fv_bear = fv_bear,
      fv_base = fv_base,
      price = px,
      rec_primary = as.character(rec$primary %||% "")[1],
      user_primary = as.character(input$dc_user_primary %||% "")[1],
      g_near_pct = tryCatch(.pick(g_near_pct()), error = function(e) NA_real_),
      sgr_pct = tryCatch(.pick(sgr_pct()), error = function(e) NA_real_),
      wacc_pct = tryCatch(.pick(wacc_pct()), error = function(e) NA_real_),
      hfv_scenarios = tryCatch(hfv_scenarios(), error = function(e) NULL),
      fscore = tryCatch(.pick(fscore_total()), error = function(e) NA_real_),
      str_skip = ui_str("dc_status_skip", loc),
      str_no_data = ui_str("dc_status_no_data", loc),
      str_no_rec = ui_str("dc_status_no_rec", loc),
      str_hfv_no_data = ui_str("dc_status_hfv_no_data", loc),
      str_bear_base_pass = ui_str("dc_detail_bear_base_pass", loc),
      str_bear_base_order_fail = ui_str("dc_detail_bear_base_order_fail", loc),
      str_bear_mos_fail = ui_str("dc_detail_bear_mos_fail", loc),
      str_base_mos_pass = ui_str("dc_detail_base_mos_pass", loc),
      str_base_mos_fail = ui_str("dc_detail_base_mos_fail", loc),
      str_g_sgr_pass = ui_str("dc_detail_g_sgr_pass", loc),
      str_g_sgr_fail = ui_str("dc_detail_g_sgr_fail", loc),
      str_model_align_pass = ui_str("dc_detail_model_align_pass", loc),
      str_model_align_fail = ui_str("dc_detail_model_align_fail", loc),
      str_hfv_veto_pass = ui_str("dc_detail_hfv_veto_pass", loc),
      str_hfv_c_lead_fail = ui_str("dc_detail_hfv_c_lead_fail", loc),
      str_hfv_c_freq_fail = ui_str("dc_detail_hfv_c_freq_fail", loc),
      str_fscore_pass = ui_str("dc_detail_fscore_pass", loc),
      str_fscore_fail = ui_str("dc_detail_fscore_fail", loc),
      str_no_rank_pass = ui_str("dc_detail_no_rank_pass", loc)
    )
  })

  eval_results <- reactive({
    ctx <- live_ctx()
    items <- names(.DC_ITEM_DEFS)
    lapply(items, function(item) {
      def <- .DC_ITEM_DEFS[[item]]
      on <- isTRUE(input[[.dc_chk_id(item)]])
      conds <- list()
      for (cn in names(def$conds)) {
        conds[[cn]] <- input[[.dc_cond_id(item, cn)]]
      }
      res <- .dc_eval_item(item, on, conds, ctx)
      res$item <- item
      res$on <- on
      res
    })
  })

  output$dc_live_snapshot <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    ctx <- live_ctx()
    fmt_mos <- function(x) {
      if (!is.finite(x)) return(ui_str("dc_na", loc))
      sprintf("%+.1f%%", x * 100)
    }
    sc <- ctx$hfv_scenarios
    hfv_line <- {
      if (is.null(sc) || !is.list(sc) || as.integer(sc$n %||% 0L) < 1L) {
        ui_str("dc_live_hfv_none", loc)
      } else {
        lead <- as.character(sc$most_frequent %||% "—")[1]
        frq <- sc$freq
        p_c <- if (!is.null(frq) && "C" %in% names(frq) && is.finite(frq[["C"]])) {
          sprintf("%.0f%%", 100 * as.numeric(frq[["C"]]))
        } else {
          "—"
        }
        sprintf(ui_str("dc_live_hfv_fmt", loc), lead, p_c, as.integer(sc$n %||% 0L))
      }
    }
    tags$div(
      class = "ynow-dc-live",
      tags$div(tags$b(ui_str("dc_live_title", loc))),
      tags$ul(
        style = "margin:6px 0 0 0;padding-left:18px;font-size:13px;line-height:1.55;",
        tags$li(sprintf(ui_str("dc_live_mos_bear", loc), fmt_mos(ctx$mos_bear))),
        tags$li(sprintf(ui_str("dc_live_mos_base", loc), fmt_mos(ctx$mos_base))),
        tags$li(hfv_line)
      ),
      tags$p(
        style = "margin:8px 0 0 0;font-size:11.5px;color:#6c757d;line-height:1.45;",
        ui_str("dc_live_hfv_note", loc)
      )
    )
  })

  output$dc_summary_panel <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    rows <- eval_results()
    active <- Filter(function(r) isTRUE(r$on), rows)
    n_pass <- sum(vapply(active, function(r) identical(r$status, "pass"), logical(1)))
    n_fail <- sum(vapply(active, function(r) identical(r$status, "fail"), logical(1)))
    n_na <- sum(vapply(active, function(r) identical(r$status, "na"), logical(1)))
    overall <- if (length(active) < 1L) {
      "idle"
    } else if (n_fail > 0L) {
      "fail"
    } else if (n_na > 0L && n_pass == 0L) {
      "na"
    } else if (n_na > 0L) {
      "partial"
    } else {
      "pass"
    }
    overall_lab <- switch(
      overall,
      pass = ui_str("dc_overall_pass", loc),
      fail = ui_str("dc_overall_fail", loc),
      na = ui_str("dc_overall_na", loc),
      partial = ui_str("dc_overall_partial", loc),
      ui_str("dc_overall_idle", loc)
    )
    overall_col <- switch(
      overall,
      pass = "#1e7e34",
      fail = "#c0392b",
      na = "#6c757d",
      partial = "#d68910",
      "#6c757d"
    )
    status_badge <- function(st) {
      lab <- switch(
        st,
        pass = ui_str("dc_badge_pass", loc),
        fail = ui_str("dc_badge_fail", loc),
        na = ui_str("dc_badge_na", loc),
        skip = ui_str("dc_badge_skip", loc),
        st
      )
      col <- switch(
        st,
        pass = "#1e7e34",
        fail = "#c0392b",
        na = "#6c757d",
        skip = "#adb5bd",
        "#333"
      )
      tags$span(
        style = paste0(
          "display:inline-block;min-width:3.2em;padding:1px 6px;border-radius:3px;",
          "font-size:11px;font-weight:700;color:#fff;background:", col, ";"
        ),
        lab
      )
    }
    tags$div(
      tags$div(
        style = paste0(
          "margin:0 0 12px 0;padding:10px 12px;border-left:4px solid ", overall_col, ";",
          "background:#f8f9fa;font-size:14px;font-weight:700;color:", overall_col, ";"
        ),
        overall_lab,
        tags$span(
          style = "margin-left:8px;font-size:12px;font-weight:500;color:#555;",
          sprintf(ui_str("dc_overall_counts", loc), n_pass, n_fail, n_na)
        )
      ),
      tags$div(
        lapply(rows, function(r) {
          if (!isTRUE(r$on)) {
            return(tags$div(
              class = "ynow-dc-row ynow-dc-row-skip",
              status_badge("skip"),
              tags$span(
                style = "margin-left:8px;font-size:12.5px;color:#888;",
                ui_str(paste0("dc_label_", r$item), loc)
              )
            ))
          }
          tags$div(
            class = "ynow-dc-row",
            style = "margin:0 0 10px 0;padding:8px 10px;background:#fff;border:1px solid #e9ecef;border-radius:4px;",
            tags$div(
              status_badge(r$status),
              tags$span(
                style = "margin-left:8px;font-size:13px;font-weight:600;",
                ui_str(paste0("dc_label_", r$item), loc)
              )
            ),
            tags$div(
              style = "margin:4px 0 0 0;font-size:12px;color:#555;line-height:1.45;",
              r$detail
            )
          )
        })
      )
    )
  })
}
