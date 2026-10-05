# =========================================================================
# 投資決策通過檢核表（Decision Checklist）— 可自訂參數、檢核摘要
# 啟發式預設；非學術標準。HFV 僅作否決工具，不作看漲依據。
# 排版：檢核摘要滿版置頂；各主題項目左右並排；閘門順序體質 → 估值 → 模型 → 否決 → 紀律。
# 僅 mandatory_suggest（預設勾選）項目開機預勾；其餘 opt-in。
# 機構級 SOP：三步確認（SGR／敏感度／財報品質）+ 強制閘門通過後，才解鎖 YNOW Buy/Sell/Hold。
# Lite：側欄隱藏（ynow_ui.R body.ynow-lite）。
# =========================================================================

# SOP 強制納入評估（不受使用者取消勾選影響解鎖邏輯）
.DC_SOP_MANDATORY_ITEMS <- c("g_sgr", "fscore", "bear_base", "hfv_veto")

.dc_sop_default_conds <- function(item) {
  def <- .DC_ITEM_DEFS[[item]]
  if (is.null(def)) return(list())
  out <- list()
  for (cn in names(def$conds)) {
    out[[cn]] <- def$conds[[cn]]$default
  }
  out
}

.dc_institutional_sop_unlock <- function(ack_sgr, ack_sens, ack_quality, mandatory_results) {
  acks_ok <- isTRUE(ack_sgr) && isTRUE(ack_sens) && isTRUE(ack_quality)
  if (!acks_ok) {
    return(list(
      unlocked = FALSE,
      reason = "acks",
      n_mandatory_fail = sum(vapply(mandatory_results, function(r) identical(r$status, "fail"), logical(1))),
      n_mandatory_na = sum(vapply(mandatory_results, function(r) identical(r$status, "na"), logical(1)))
    ))
  }
  n_fail <- sum(vapply(mandatory_results, function(r) identical(r$status, "fail"), logical(1)))
  n_na <- sum(vapply(mandatory_results, function(r) identical(r$status, "na"), logical(1)))
  if (n_fail > 0L) {
    return(list(unlocked = FALSE, reason = "gates_fail", n_mandatory_fail = n_fail, n_mandatory_na = n_na))
  }
  if (n_na > 0L) {
    return(list(unlocked = FALSE, reason = "gates_na", n_mandatory_fail = n_fail, n_mandatory_na = n_na))
  }
  list(unlocked = TRUE, reason = "ok", n_mandatory_fail = 0L, n_mandatory_na = 0L)
}

# Investor-desk order (display + evaluation). default_on follows mandatory_suggest only.
.DC_ITEM_DEFS <- list(
  fscore = list(
    section = "quality",
    default_on = FALSE,
    mandatory_suggest = FALSE,
    conds = list(
      fscore_min = list(default = 5, step = 1, min = 0, max = 9)
    )
  ),
  bear_base = list(
    section = "valuation",
    default_on = TRUE,
    mandatory_suggest = TRUE,
    conds = list(
      bear_mos_floor = list(default = -10, step = 1, min = -50, max = 50)
    )
  ),
  base_mos = list(
    section = "valuation",
    default_on = FALSE,
    mandatory_suggest = FALSE,
    conds = list(
      base_mos_floor = list(default = 15, step = 1, min = -50, max = 80)
    )
  ),
  model_align = list(
    section = "model",
    default_on = FALSE,
    mandatory_suggest = FALSE,
    conds = list()
  ),
  g_sgr = list(
    section = "model",
    default_on = FALSE,
    mandatory_suggest = FALSE,
    conds = list(
      g_sgr_gap_min = list(default = 0.5, step = 0.1, min = 0, max = 20),
      sgr_wacc_buffer = list(default = 2, step = 0.25, min = 0, max = 20)
    )
  ),
  hfv_veto = list(
    section = "veto",
    default_on = TRUE,
    mandatory_suggest = TRUE,
    conds = list(
      max_c_freq = list(default = 35, step = 1, min = 0, max = 100)
    )
  ),
  no_rank_chase = list(
    section = "discipline",
    default_on = FALSE,
    mandatory_suggest = FALSE,
    conds = list()
  )
)

.DC_SECTION_ORDER <- c("quality", "valuation", "model", "veto", "discipline")

.dc_chk_id <- function(item) paste0("chk_", item)
.dc_cond_id <- function(item, cond) paste0("cond_", item, "_", cond)

.dc_str <- function(key, loc = "en") {
  if (exists("ui_str", mode = "function")) {
    tryCatch(ui_str(key, loc), error = function(e) key)
  } else {
    key
  }
}

.dc_item_block <- function(item) {
  def <- .DC_ITEM_DEFS[[item]]
  chk <- .dc_chk_id(item)
  cond_names <- names(def$conds)
  # Bake investor-facing copy into initial HTML (en defaults) so lazy-mounted
  # Checklist & Conditions never flash code-style param ids like bear_mos_floor.
  label0 <- .dc_str(paste0("dc_label_", item), "en")
  hint0 <- .dc_str(paste0("dc_hint_", item), "en")
  badge0 <- .dc_str("dc_badge_default_on", "en")
  tags$div(
    class = paste0(
      "ynow-dc-item",
      if (isTRUE(def$mandatory_suggest)) " ynow-dc-item--default" else ""
    ),
    `data-dc-item` = item,
    checkboxInput(
      chk,
      label = tags$span(
        class = "ynow-dc-label-wrap",
        tags$span(class = "ynow-dc-label", id = paste0("ynow_dc_label_", item), label0),
        if (isTRUE(def$mandatory_suggest)) {
          tags$span(class = "ynow-dc-badge", id = paste0("ynow_dc_badge_", item), badge0)
        } else {
          NULL
        }
      ),
      value = isTRUE(def$default_on)
    ),
    tags$p(class = "ynow-dc-hint", id = paste0("ynow_dc_hint_", item), hint0),
    if (length(cond_names) > 0L || identical(item, "model_align")) {
      conditionalPanel(
        condition = sprintf("input['%s'] == true", chk),
        tags$div(
          class = "ynow-dc-conds",
          if (identical(item, "model_align")) {
            tagList(
              selectInput(
                "dc_user_primary",
                .dc_str("dc_cond_user_primary", "en"),
                # Fair Value primaries only — Multiples / SOTP are Implied Price cross-checks
                choices = c(
                  "DCF" = "dcf", "DDM" = "ddm", "RI" = "ri", "P/B" = "pb", "NAV" = "nav"
                ),
                selected = "dcf",
                width = "100%"
              ),
              tags$p(
                class = "ynow-dc-cond-hint",
                id = "ynow_dc_cond_hint_user_primary",
                .dc_str("dc_cond_hint_user_primary", "en")
              )
            )
          } else {
            NULL
          },
          lapply(cond_names, function(cn) {
            meta <- def$conds[[cn]]
            input_id <- .dc_cond_id(item, cn)
            tagList(
              numericInput(
                input_id,
                .dc_str(paste0("dc_cond_", cn), "en"),
                value = meta$default,
                min = meta$min,
                max = meta$max,
                step = meta$step,
                width = "100%"
              ),
              tags$p(
                class = "ynow-dc-cond-hint",
                id = paste0("ynow_dc_cond_hint_", cn),
                .dc_str(paste0("dc_cond_hint_", cn), "en")
              )
            )
          })
        )
      )
    } else {
      NULL
    }
  )
}

.dc_checks_ui <- function() {
  items_by_section <- lapply(.DC_SECTION_ORDER, function(sec) {
    names(.DC_ITEM_DEFS)[vapply(.DC_ITEM_DEFS, function(d) identical(d$section, sec), logical(1))]
  })
  names(items_by_section) <- .DC_SECTION_ORDER

  tagList(
    ynow_notes_block(
      tags$p(
        id = "ynow_dc_panel_hint",
        class = "ynow-dc-panel-hint",
        .dc_str("dc_panel_hint", "en")
      )
    ),
    lapply(.DC_SECTION_ORDER, function(sec) {
      sec_items <- items_by_section[[sec]]
      if (!length(sec_items)) return(NULL)
      tags$section(
        class = "ynow-dc-section",
        `data-dc-section` = sec,
        tags$h4(
          class = "ynow-dc-section-title",
          id = paste0("ynow_dc_section_", sec),
          .dc_str(paste0("dc_section_", sec), "en")
        ),
        tags$div(
          class = "ynow-dc-section-items",
          lapply(sec_items, .dc_item_block)
        )
      )
    })
  )
}

.dc_sop_wizard_ui <- function() {
  tagList(
    tags$div(
      class = "ynow-dc-sop",
      tags$p(
        id = "ynow_dc_sop_intro",
        class = "ynow-dc-sop-intro",
        .dc_str("dc_sop_intro", "en")
      ),
      tags$ol(
        class = "ynow-dc-sop-steps",
        tags$li(
          class = "ynow-dc-sop-step",
          `data-sop-step` = "sgr",
          tags$h5(tags$span(id = "ynow_dc_sop_step1_title", .dc_str("dc_sop_step1_title", "en"))),
          uiOutput("dc_sop_step_sgr_body"),
          checkboxInput(
            "sop_ack_sgr",
            label = tags$span(id = "ynow_dc_sop_ack_sgr_label", .dc_str("dc_sop_ack_sgr", "en")),
            value = FALSE
          )
        ),
        tags$li(
          class = "ynow-dc-sop-step",
          `data-sop-step` = "sensitivity",
          tags$h5(tags$span(id = "ynow_dc_sop_step2_title", .dc_str("dc_sop_step2_title", "en"))),
          uiOutput("dc_sop_step_sens_body"),
          checkboxInput(
            "sop_ack_sensitivity",
            label = tags$span(id = "ynow_dc_sop_ack_sens_label", .dc_str("dc_sop_ack_sensitivity", "en")),
            value = FALSE
          )
        ),
        tags$li(
          class = "ynow-dc-sop-step",
          `data-sop-step` = "quality",
          tags$h5(tags$span(id = "ynow_dc_sop_step3_title", .dc_str("dc_sop_step3_title", "en"))),
          uiOutput("dc_sop_step_quality_body"),
          checkboxInput(
            "sop_ack_quality",
            label = tags$span(id = "ynow_dc_sop_ack_qual_label", .dc_str("dc_sop_ack_quality", "en")),
            value = FALSE
          )
        )
      ),
      uiOutput("dc_sop_unlock_banner")
    )
  )
}

decision_checklist_tab_body_ui <- function() {
  tagList(
    fluidRow(
      column(
        width = 12,
        h2(tags$b(id = "ynow_dc_page_title", .dc_str("dc_page_title", "en"))),
        p(
          id = "ynow_dc_page_sub",
          .dc_str("dc_page_sub", "en")
        ),
        tags$hr()
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          width = 12, status = "warning", solidHeader = TRUE,
          title = tagList(
            icon("user-shield"),
            tags$span(id = "ynow_dc_box_sop", .dc_str("dc_box_sop", "en"))
          ),
          .dc_sop_wizard_ui()
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          width = 12, status = "success", solidHeader = TRUE,
          title = tagList(
            icon("clipboard-check"),
            tags$span(id = "ynow_dc_box_summary", .dc_str("dc_box_summary", "en"))
          ),
          tags$div(
            class = "ynow-dc-summary-top",
            uiOutput("dc_live_snapshot"),
            tags$hr(style = "margin: 10px 0;"),
            uiOutput("dc_summary_panel")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = tagList(
            icon("tasks"),
            tags$span(id = "ynow_dc_box_checks", .dc_str("dc_box_checks", "en"))
          ),
          .dc_checks_ui()
        )
      )
    )
  )
}

decision_checklist_tab_ui <- function() {
  tabItem(
    tabName = "decision_checklist",
    uiOutput("ynow_lazy_host_decision_checklist")
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
    fscore_total,
    quality_snapshot = reactive(list()),
    val_meta = reactive(list()),
    sop_gate_out = NULL,
    current_ticker = reactive(NULL)
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
    # Server-side label updates (reliable after lazy mount) + client hint text.
    for (item in names(.DC_ITEM_DEFS)) {
      def <- .DC_ITEM_DEFS[[item]]
      for (cn in names(def$conds)) {
        tryCatch(
          updateNumericInput(
            session,
            .dc_cond_id(item, cn),
            label = ui_str(paste0("dc_cond_", cn), loc)
          ),
          error = function(e) NULL
        )
      }
    }
    session$sendCustomMessage("ynowDcLocale", list(
      locale = loc,
      panel_hint = ui_str("dc_panel_hint", loc),
      badge = ui_str("dc_badge_default_on", loc),
      sections = lapply(.DC_SECTION_ORDER, function(sec) {
        list(
          id = sec,
          title = ui_str(paste0("dc_section_", sec), loc)
        )
      }),
      items = lapply(names(.DC_ITEM_DEFS), function(item) {
        def <- .DC_ITEM_DEFS[[item]]
        conds <- lapply(names(def$conds), function(cn) {
          list(
            input_id = .dc_cond_id(item, cn),
            label = ui_str(paste0("dc_cond_", cn), loc),
            hint = ui_str(paste0("dc_cond_hint_", cn), loc),
            hint_id = paste0("ynow_dc_cond_hint_", cn)
          )
        })
        if (identical(item, "model_align")) {
          conds <- c(
            conds,
            list(list(
              input_id = "dc_user_primary",
              label = ui_str("dc_cond_user_primary", loc),
              hint = ui_str("dc_cond_hint_user_primary", loc),
              hint_id = "ynow_dc_cond_hint_user_primary"
            ))
          )
        }
        list(
          id = item,
          label = ui_str(paste0("dc_label_", item), loc),
          hint = ui_str(paste0("dc_hint_", item), loc),
          mandatory = isTRUE(def$mandatory_suggest),
          conds = conds
        )
      })
    ))
  }

  observe({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    .dc_apply_locale_labels(loc)
    session$sendCustomMessage("ynowDcSopLocale", list(
      locale = loc,
      intro = ui_str("dc_sop_intro", loc),
      box_sop = ui_str("dc_box_sop", loc),
      step1 = ui_str("dc_sop_step1_title", loc),
      step2 = ui_str("dc_sop_step2_title", loc),
      step3 = ui_str("dc_sop_step3_title", loc),
      ack_sgr = ui_str("dc_sop_ack_sgr", loc),
      ack_sens = ui_str("dc_sop_ack_sensitivity", loc),
      ack_qual = ui_str("dc_sop_ack_quality", loc)
    ))
  })

  observeEvent(current_ticker(), {
    tryCatch(updateCheckboxInput(session, "sop_ack_sgr", value = FALSE), error = function(e) NULL)
    tryCatch(updateCheckboxInput(session, "sop_ack_sensitivity", value = FALSE), error = function(e) NULL)
    tryCatch(updateCheckboxInput(session, "sop_ack_quality", value = FALSE), error = function(e) NULL)
  }, ignoreInit = TRUE)

  # Lazy tab mount: re-apply investor copy once Checklist & Conditions is in the DOM.
  observeEvent(input$sidebar_tabs, {
    if (!identical(as.character(input$sidebar_tabs)[1], "decision_checklist")) return()
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    session$onFlushed(function() {
      tryCatch(.dc_apply_locale_labels(loc), error = function(e) NULL)
    }, once = TRUE)
  }, ignoreInit = TRUE)

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

  sop_mandatory_results <- reactive({
    ctx <- live_ctx()
    lapply(.DC_SOP_MANDATORY_ITEMS, function(item) {
      def <- .DC_ITEM_DEFS[[item]]
      conds <- .dc_sop_default_conds(item)
      for (cn in names(def$conds)) {
        v <- input[[.dc_cond_id(item, cn)]]
        if (!is.null(v)) conds[[cn]] <- v
      }
      res <- .dc_eval_item(item, TRUE, conds, ctx)
      res$item <- item
      res$on <- TRUE
      res
    })
  })

  sop_state <- reactive({
    ack_sgr <- isTRUE(input$sop_ack_sgr)
    ack_sens <- isTRUE(input$sop_ack_sensitivity)
    ack_qual <- isTRUE(input$sop_ack_quality)
    .dc_institutional_sop_unlock(
      ack_sgr, ack_sens, ack_qual,
      mandatory_results = sop_mandatory_results()
    )
  })

  if (!is.null(sop_gate_out)) {
    observe({
      st <- sop_state()
      mandatory <- sop_mandatory_results()
      sop_gate_out(list(
        unlocked = isTRUE(st$unlocked),
        reason = st$reason %||% "",
        mandatory = mandatory
      ))
    })
  }

  output$dc_sop_step_sgr_body <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    ctx <- live_ctx()
    fmt <- function(x) {
      if (!is.finite(x)) return(ui_str("dc_na", loc))
      sprintf("%.2f%%", x)
    }
    tags$div(
      class = "ynow-dc-sop-step-body",
      tags$p(
        style = "font-size:13px;line-height:1.55;margin:0 0 8px 0;",
        ui_str("dc_sop_step1_lead", loc)
      ),
      tags$ul(
        style = "margin:0;padding-left:18px;font-size:13px;line-height:1.55;",
        tags$li(sprintf(ui_str("dc_sop_live_g_near", loc), fmt(ctx$g_near_pct))),
        tags$li(sprintf(ui_str("dc_sop_live_sgr", loc), fmt(ctx$sgr_pct))),
        tags$li(sprintf(ui_str("dc_sop_live_wacc", loc), fmt(ctx$wacc_pct)))
      )
    )
  })

  output$dc_sop_step_sens_body <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    meta <- tryCatch(val_meta(), error = function(e) list())
    fmt_pp <- function(x) {
      if (!is.finite(x)) return(ui_str("dc_na", loc))
      sprintf("%.2f pp", x)
    }
    fmt_pct <- function(x) {
      if (!is.finite(x)) return(ui_str("dc_na", loc))
      sprintf("%.1f%%", x)
    }
    tags$div(
      class = "ynow-dc-sop-step-body",
      tags$p(
        style = "font-size:13px;line-height:1.55;margin:0 0 8px 0;",
        ui_str("dc_sop_step2_lead", loc)
      ),
      tags$ul(
        style = "margin:0;padding-left:18px;font-size:13px;line-height:1.55;",
        tags$li(sprintf(
          ui_str("dc_sop_live_spread", loc),
          fmt_pp(suppressWarnings(as.numeric(meta$spread_pp)[1]))
        )),
        tags$li(sprintf(
          ui_str("dc_sop_live_tv_weight", loc),
          fmt_pct(suppressWarnings(as.numeric(meta$tv_weight_pct)[1]))
        ))
      ),
      tags$p(
        style = "font-size:12px;color:#6c757d;margin:8px 0 0 0;",
        ui_str("dc_sop_step2_matrix_hint", loc)
      )
    )
  })

  output$dc_sop_step_quality_body <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    qs <- tryCatch(quality_snapshot(), error = function(e) list())
    fs <- suppressWarnings(as.numeric(qs$fscore)[1])
    qoe <- as.character(qs$qoe_grade %||% "")[1]
    qsc <- suppressWarnings(as.numeric(qs$qoe_score)[1])
    red_n <- suppressWarnings(as.integer(qs$red_n %||% 0L)[1])
    fmt_fs <- if (is.finite(fs)) sprintf("%.0f / 9", fs) else ui_str("dc_na", loc)
    fmt_qoe <- if (nzchar(qoe)) qoe else ui_str("dc_na", loc)
    fmt_qsc <- if (is.finite(qsc)) sprintf("%.0f", qsc) else "—"
    tags$div(
      class = "ynow-dc-sop-step-body",
      tags$p(
        style = "font-size:13px;line-height:1.55;margin:0 0 8px 0;",
        ui_str("dc_sop_step3_lead", loc)
      ),
      tags$ul(
        style = "margin:0;padding-left:18px;font-size:13px;line-height:1.55;",
        tags$li(sprintf(ui_str("dc_sop_live_fscore", loc), fmt_fs)),
        tags$li(sprintf(ui_str("dc_sop_live_qoe", loc), fmt_qoe, fmt_qsc)),
        tags$li(sprintf(ui_str("dc_sop_live_red_flags", loc), if (is.finite(red_n)) red_n else "—"))
      )
    )
  })

  output$dc_sop_unlock_banner <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    st <- sop_state()
    col <- if (isTRUE(st$unlocked)) "#1e7e34" else "#c0392b"
    bg <- if (isTRUE(st$unlocked)) "#eafaf1" else "#fdecea"
    msg <- if (isTRUE(st$unlocked)) {
      ui_str("dc_sop_unlocked", loc)
    } else {
      switch(
        as.character(st$reason %||% "")[1],
        acks = ui_str("dc_sop_locked_acks", loc),
        gates_fail = ui_str("dc_sop_locked_gates_fail", loc),
        gates_na = ui_str("dc_sop_locked_gates_na", loc),
        ui_str("dc_sop_locked_default", loc)
      )
    }
    tags$div(
      class = "ynow-dc-sop-banner",
      style = paste0(
        "margin-top:12px;padding:10px 12px;border-left:4px solid ", col,
        ";background:", bg, ";font-size:13px;font-weight:600;color:", col, ";"
      ),
      icon(if (isTRUE(st$unlocked)) "lock-open" else "lock"),
      " ",
      msg
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
      ynow_notes_block(
        locale = loc,
        tags$p(
          class = "ynow-dc-hfv-note",
          style = "margin:0;font-size:11.5px;color:#6c757d;line-height:1.45;",
          ui_str("dc_live_hfv_note", loc)
        )
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
        class = "ynow-dc-summary-grid",
        lapply(rows, function(r) {
          if (!isTRUE(r$on)) {
            return(tags$div(
              class = "ynow-dc-row ynow-dc-row-skip",
              style = "margin:0;padding:8px 10px;background:#fafafa;border:1px dashed #e9ecef;border-radius:4px;",
              status_badge("skip"),
              tags$span(
                style = "margin-left:8px;font-size:12.5px;color:#888;",
                ui_str(paste0("dc_label_", r$item), loc)
              )
            ))
          }
          tags$div(
            class = "ynow-dc-row",
            style = "margin:0;padding:8px 10px;background:#fff;border:1px solid #e9ecef;border-radius:4px;",
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
