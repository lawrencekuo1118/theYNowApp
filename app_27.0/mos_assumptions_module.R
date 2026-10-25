# mos_assumptions_module.R — investor MOS assumption hub (MOS Assumptions / MOS假設)
#
# Single place for investor-tunable MOS floors, HFV scenario bands, alert discount,
# and Quant Lab MOS exposure ladder / VG weight. Fundamental MOS *results*
# (Smart Analysis, report, Checklist pass/fail) stay on their own surfaces.
#
# Full: guest-locked like Decision Checklist (Lite omits). Feature gate reuses
# checklist so one sign-in unlocks both hygiene surfaces.

.mos_assump_str <- function(key, loc = "en") {
  if (exists("ui_str", mode = "function")) {
    tryCatch(ui_str(key, loc), error = function(e) key)
  } else {
    key
  }
}

.mos_assump_num <- function(x, default) {
  v <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(v)) default else v
}

#' Build HFV scenario threshold list from session inputs / APP_DEFAULTS (decimals).
mos_assumptions_hfv_thr <- function(input = NULL, defaults = NULL) {
  d <- if (is.null(defaults) && exists("APP_DEFAULTS")) {
    as.list(APP_DEFAULTS)
  } else {
    as.list(defaults %||% list())
  }
  near <- .mos_assump_num(
    if (!is.null(input)) input$mos_hfv_near_pct else NULL,
    .mos_assump_num(d$mos_hfv_near_pct, 10)
  ) / 100
  cheap <- .mos_assump_num(
    if (!is.null(input)) input$mos_hfv_deep_cheap_pct else NULL,
    .mos_assump_num(d$mos_hfv_deep_cheap_pct, 20)
  ) / 100
  rich <- .mos_assump_num(
    if (!is.null(input)) input$mos_hfv_deep_rich_pct else NULL,
    .mos_assump_num(d$mos_hfv_deep_rich_pct, -20)
  ) / 100
  flat <- .mos_assump_num(
    if (!is.null(input)) input$mos_hfv_flat_band_pct else NULL,
    .mos_assump_num(d$mos_hfv_flat_band_pct, 2)
  ) / 100
  strong <- .mos_assump_num(
    if (!is.null(input)) input$mos_hfv_price_strong_up_pct else NULL,
    .mos_assump_num(d$mos_hfv_price_strong_up_pct, 5)
  ) / 100
  list(
    momentum_flat_band = flat,
    price_strong_up = strong,
    mos_near = near,
    mos_deep_cheap = cheap,
    mos_deep_rich = rich
  )
}

#' MOS hysteresis ladder cut-points (decimal) from session / defaults.
mos_assumptions_ladder <- function(input = NULL, defaults = NULL) {
  d <- if (is.null(defaults) && exists("APP_DEFAULTS")) {
    as.list(APP_DEFAULTS)
  } else {
    as.list(defaults %||% list())
  }
  list(
    deep = .mos_assump_num(
      if (!is.null(input)) input$mos_ladder_deep_pct else NULL,
      .mos_assump_num(d$mos_ladder_deep_pct, 30)
    ) / 100,
    mid = .mos_assump_num(
      if (!is.null(input)) input$mos_ladder_mid_pct else NULL,
      .mos_assump_num(d$mos_ladder_mid_pct, 10)
    ) / 100,
    fair = .mos_assump_num(
      if (!is.null(input)) input$mos_ladder_fair_pct else NULL,
      .mos_assump_num(d$mos_ladder_fair_pct, 0)
    ) / 100,
    mild_rich = .mos_assump_num(
      if (!is.null(input)) input$mos_ladder_mild_rich_pct else NULL,
      .mos_assump_num(d$mos_ladder_mild_rich_pct, -10)
    ) / 100
  )
}

#' Topic block with optional right-aligned tab shortcut button(s).
#' @param shortcuts list of lists: id, tab, label_en (locale via push)
.mos_assump_block <- function(title_id, title_en, lead_id, lead_en,
                              shortcuts = NULL, ...) {
  goto_btns <- NULL
  if (length(shortcuts)) {
    goto_btns <- tags$div(
      class = "ynow-mos-assump-gotos",
      lapply(shortcuts, function(sc) {
        tags$button(
          type = "button",
          id = sc$id,
          class = "btn btn-xs btn-default ynow-mos-assump-goto",
          `data-tab` = sc$tab,
          sc$label_en
        )
      })
    )
  }
  tags$section(
    class = "ynow-mos-assump-block",
    tags$div(
      class = "ynow-mos-assump-title-row",
      tags$h4(id = title_id, title_en),
      goto_btns
    ),
    tags$p(id = lead_id, class = "ynow-mos-assump-lead", lead_en),
    tags$div(class = "ynow-mos-assump-fields", ...)
  )
}

mos_assumptions_tab_body_ui <- function() {
  d <- if (exists("APP_DEFAULTS")) APP_DEFAULTS else list()
  tagList(
    tags$style(HTML("
      .ynow-mos-assump-block { margin: 0 0 22px 0; padding: 14px 16px;
        background: #fafbfc; border: 1px solid #e6eaee; border-radius: 8px; }
      .ynow-mos-assump-title-row { display: flex; align-items: center;
        justify-content: space-between; gap: 8px 12px; margin: 0 0 6px 0; flex-wrap: wrap; }
      .ynow-mos-assump-title-row h4 { margin: 0; font-size: 16px; flex: 1 1 auto; min-width: 0; }
      .ynow-mos-assump-gotos { display: flex; flex-wrap: wrap; gap: 6px;
        justify-content: flex-end; flex: 0 0 auto; margin-left: auto; }
      .ynow-mos-assump-goto { white-space: nowrap; }
      .ynow-mos-assump-lead { margin: 0 0 12px 0; color: #5c6b73; font-size: 13px; }
      .ynow-mos-assump-fields .form-group { margin-bottom: 10px; }
      .ynow-mos-assump-hint { margin: -4px 0 10px 0; color: #7a8690; font-size: 12px; }
      .ynow-mos-assump-risk { margin: 8px 0 0 0; padding: 8px 10px; background: #fff8f0;
        border-left: 3px solid #e67e22; font-size: 12px; color: #6b4e2e; }
      .box.box-solid.box-primary.ynow-mos-assump-panel,
      body.ynow-theme-model .content-wrapper .box.box-solid.box-primary.ynow-mos-assump-panel,
      body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-primary.ynow-mos-assump-panel,
      body.ynow-theme-atx .content-wrapper .box.box-solid.box-primary.ynow-mos-assump-panel {
        border: none !important;
        box-shadow: none !important;
      }
    ")),
    tags$script(HTML("
      (function () {
        if (document.documentElement.getAttribute('data-ynow-mos-assump-goto') === '1') return;
        document.documentElement.setAttribute('data-ynow-mos-assump-goto', '1');
        document.addEventListener('click', function (ev) {
          var btn = ev.target && ev.target.closest
            ? ev.target.closest('.ynow-mos-assump-goto')
            : null;
          if (!btn) return;
          ev.preventDefault();
          var tab = btn.getAttribute('data-tab');
          if (!tab) return;
          if (typeof window.ynowGotoTab === 'function') {
            window.ynowGotoTab(tab);
            return;
          }
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
          }
          var a = document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]');
          if (a) { try { a.click(); } catch (e) {} }
        });
      })();
    ")),
    fluidRow(
      column(
        width = 12,
        h2(tags$b(id = "ynow_mos_assump_page_title", .mos_assump_str("mos_assump_page_title", "en"))),
        p(id = "ynow_mos_assump_page_sub", .mos_assump_str("mos_assump_page_sub", "en")),
        tags$hr()
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "box box-solid box-primary ynow-mos-assump-panel",
          tags$div(
            class = "box-header",
            tags$h3(
              class = "box-title",
              tagList(
                icon("shield-alt"),
                tags$span(id = "ynow_mos_assump_box_title", .mos_assump_str("mos_assump_box_title", "en"))
              )
            )
          ),
          tags$div(
            class = "box-body",
          # --- Block 1: Decision hygiene floors (canonical Checklist ids) ---
          .mos_assump_block(
            "ynow_mos_assump_b1_title", .mos_assump_str("mos_assump_b1_title", "en"),
            "ynow_mos_assump_b1_lead", .mos_assump_str("mos_assump_b1_lead", "en"),
            shortcuts = list(list(
              id = "ynow_mos_assump_goto_checklist",
              tab = "decision_checklist",
              label_en = .mos_assump_str("mos_assump_goto_checklist", "en")
            )),
            checkboxInput(
              "chk_bear_base",
              tags$span(id = "ynow_mos_assump_chk_bear_base_label", .mos_assump_str("dc_label_bear_base", "en")),
              value = isTRUE(d$chk_bear_base %||% TRUE)
            ),
            tags$p(id = "ynow_mos_assump_chk_bear_base_hint", class = "ynow-mos-assump-hint",
                   .mos_assump_str("dc_hint_bear_base", "en")),
            numericInput(
              "cond_bear_base_bear_mos_floor",
              tags$span(id = "ynow_mos_assump_bear_floor_label", .mos_assump_str("dc_cond_bear_mos_floor", "en")),
              value = .mos_assump_num(d$cond_bear_base_bear_mos_floor, -10),
              min = -50, max = 50, step = 1, width = "100%"
            ),
            tags$p(id = "ynow_mos_assump_bear_floor_hint", class = "ynow-mos-assump-hint",
                   .mos_assump_str("dc_cond_hint_bear_mos_floor", "en")),
            checkboxInput(
              "chk_base_mos",
              tags$span(id = "ynow_mos_assump_chk_base_mos_label", .mos_assump_str("dc_label_base_mos", "en")),
              value = isTRUE(d$chk_base_mos %||% FALSE)
            ),
            tags$p(id = "ynow_mos_assump_chk_base_mos_hint", class = "ynow-mos-assump-hint",
                   .mos_assump_str("dc_hint_base_mos", "en")),
            numericInput(
              "cond_base_mos_base_mos_floor",
              tags$span(id = "ynow_mos_assump_base_floor_label", .mos_assump_str("dc_cond_base_mos_floor", "en")),
              value = .mos_assump_num(d$cond_base_mos_base_mos_floor, 15),
              min = -50, max = 80, step = 1, width = "100%"
            ),
            tags$p(id = "ynow_mos_assump_base_floor_hint", class = "ynow-mos-assump-hint",
                   .mos_assump_str("dc_cond_hint_base_mos_floor", "en"))
          ),
          # --- Block 2: Price alert discount ---
          .mos_assump_block(
            "ynow_mos_assump_b2_title", .mos_assump_str("mos_assump_b2_title", "en"),
            "ynow_mos_assump_b2_lead", .mos_assump_str("mos_assump_b2_lead", "en"),
            shortcuts = list(list(
              id = "ynow_mos_assump_goto_alerts",
              tab = "price_alerts",
              label_en = .mos_assump_str("mos_assump_goto_alerts", "en")
            )),
            numericInput(
              "mos_alert_default_pct",
              tags$span(id = "ynow_mos_assump_alert_label", .mos_assump_str("mos_assump_alert_label", "en")),
              value = .mos_assump_num(d$mos_alert_default_pct, 0),
              min = 0, max = 90, step = 1, width = "100%"
            ),
            tags$p(id = "ynow_mos_assump_alert_hint", class = "ynow-mos-assump-hint",
                   .mos_assump_str("mos_assump_alert_hint", "en"))
          ),
          # --- Block 3: HFV scenario bands ---
          .mos_assump_block(
            "ynow_mos_assump_b3_title", .mos_assump_str("mos_assump_b3_title", "en"),
            "ynow_mos_assump_b3_lead", .mos_assump_str("mos_assump_b3_lead", "en"),
            shortcuts = list(list(
              id = "ynow_mos_assump_goto_hfv",
              tab = "hfv",
              label_en = .mos_assump_str("mos_assump_goto_hfv", "en")
            )),
            fluidRow(
              column(4, numericInput(
                "mos_hfv_near_pct",
                tags$span(id = "ynow_mos_assump_hfv_near_label", .mos_assump_str("mos_assump_hfv_near", "en")),
                value = .mos_assump_num(d$mos_hfv_near_pct, 10),
                min = 0, max = 50, step = 1, width = "100%"
              )),
              column(4, numericInput(
                "mos_hfv_deep_cheap_pct",
                tags$span(id = "ynow_mos_assump_hfv_cheap_label", .mos_assump_str("mos_assump_hfv_cheap", "en")),
                value = .mos_assump_num(d$mos_hfv_deep_cheap_pct, 20),
                min = 0, max = 80, step = 1, width = "100%"
              )),
              column(4, numericInput(
                "mos_hfv_deep_rich_pct",
                tags$span(id = "ynow_mos_assump_hfv_rich_label", .mos_assump_str("mos_assump_hfv_rich", "en")),
                value = .mos_assump_num(d$mos_hfv_deep_rich_pct, -20),
                min = -80, max = 0, step = 1, width = "100%"
              ))
            ),
            fluidRow(
              column(6, numericInput(
                "mos_hfv_flat_band_pct",
                tags$span(id = "ynow_mos_assump_hfv_flat_label", .mos_assump_str("mos_assump_hfv_flat", "en")),
                value = .mos_assump_num(d$mos_hfv_flat_band_pct, 2),
                min = 0, max = 20, step = 0.5, width = "100%"
              )),
              column(6, numericInput(
                "mos_hfv_price_strong_up_pct",
                tags$span(id = "ynow_mos_assump_hfv_strong_label", .mos_assump_str("mos_assump_hfv_strong", "en")),
                value = .mos_assump_num(d$mos_hfv_price_strong_up_pct, 5),
                min = 0, max = 50, step = 0.5, width = "100%"
              ))
            ),
            tags$p(id = "ynow_mos_assump_hfv_hint", class = "ynow-mos-assump-hint",
                   .mos_assump_str("mos_assump_hfv_hint", "en")),
            tags$div(
              id = "ynow_mos_assump_hfv_risk",
              class = "ynow-mos-assump-risk",
              .mos_assump_str("mos_assump_hfv_risk", "en")
            )
          ),
          # --- Block 4: Strategy exposure (Quant Lab Mode A) ---
          .mos_assump_block(
            "ynow_mos_assump_b4_title", .mos_assump_str("mos_assump_b4_title", "en"),
            "ynow_mos_assump_b4_lead", .mos_assump_str("mos_assump_b4_lead", "en"),
            shortcuts = list(list(
              id = "ynow_mos_assump_goto_lab",
              tab = "lab_notes",
              label_en = .mos_assump_str("mos_assump_goto_lab", "en")
            )),
            sliderInput(
              "bt_w_vg",
              tags$span(id = "ynow_mos_assump_wvg_label", .mos_assump_str("bt_w_vg_label", "en")),
              min = 0, max = 1,
              value = .mos_assump_num(d$bt_w_vg, 0.7),
              step = 0.01
            ),
            tags$p(id = "ynow_mos_assump_wvg_hint", class = "ynow-mos-assump-hint",
                   .mos_assump_str("bt_w_vg_hint", "en")),
            tags$b(id = "ynow_mos_assump_ladder_title", .mos_assump_str("mos_assump_ladder_title", "en")),
            fluidRow(
              column(3, numericInput(
                "mos_ladder_deep_pct",
                tags$span(id = "ynow_mos_assump_lad_deep_label", .mos_assump_str("mos_assump_lad_deep", "en")),
                value = .mos_assump_num(d$mos_ladder_deep_pct, 30),
                min = 0, max = 100, step = 1, width = "100%"
              )),
              column(3, numericInput(
                "mos_ladder_mid_pct",
                tags$span(id = "ynow_mos_assump_lad_mid_label", .mos_assump_str("mos_assump_lad_mid", "en")),
                value = .mos_assump_num(d$mos_ladder_mid_pct, 10),
                min = -20, max = 80, step = 1, width = "100%"
              )),
              column(3, numericInput(
                "mos_ladder_fair_pct",
                tags$span(id = "ynow_mos_assump_lad_fair_label", .mos_assump_str("mos_assump_lad_fair", "en")),
                value = .mos_assump_num(d$mos_ladder_fair_pct, 0),
                min = -30, max = 50, step = 1, width = "100%"
              )),
              column(3, numericInput(
                "mos_ladder_mild_rich_pct",
                tags$span(id = "ynow_mos_assump_lad_mild_label", .mos_assump_str("mos_assump_lad_mild", "en")),
                value = .mos_assump_num(d$mos_ladder_mild_rich_pct, -10),
                min = -50, max = 20, step = 1, width = "100%"
              ))
            ),
            tags$p(id = "ynow_mos_assump_ladder_hint", class = "ynow-mos-assump-hint",
                   .mos_assump_str("mos_assump_ladder_hint", "en")),
            tags$div(
              id = "ynow_mos_assump_strat_risk",
              class = "ynow-mos-assump-risk",
              .mos_assump_str("mos_assump_strat_risk", "en")
            )
          )
          )
        )
      )
    )
  )
}

mos_assumptions_tab_ui <- function() {
  gate <- if (exists("ynow_member_gate_ui", mode = "function")) {
    ynow_member_gate_ui(
      "ynow_mos_assump_guest_gate",
      "ynow_mos_assump_gate_title",
      "ynow_mos_assump_gate_body",
      "ynow_mos_assump_gate_signin",
      feature = "checklist"
    )
  } else {
    NULL
  }
  # Eager body (not lazy): Checklist / HFV / Lab read the same input ids
  # (chk_bear_base, cond_*_mos_floor, bt_w_vg, mos_hfv_*, mos_ladder_*) from first paint.
  body <- tags$div(
    id = "ynow_mos_assump_body",
    class = "ynow-member-body",
    `data-ynow-lock` = "section:mos_assumptions_body",
    mos_assumptions_tab_body_ui()
  )
  surface <- if (exists("ynow_member_gate_surface_ui", mode = "function")) {
    ynow_member_gate_surface_ui(feature = "checklist", gate = gate, body = body)
  } else {
    tagList(gate, body)
  }
  tabItem(tabName = "mos_assumptions", surface)
}

#' Locale push for MOS Assumptions labels (call from ui_locale observer).
mos_assumptions_push_locale <- function(session, loc = "en") {
  if (is.null(session) || !exists("ui_str", mode = "function")) return(invisible(NULL))
  set_txt <- function(id, key) {
    tryCatch(
      session$sendCustomMessage("ynowSetText", list(id = id, text = ui_str(key, loc))),
      error = function(e) invisible(NULL)
    )
  }
  keys <- c(
    ynow_mos_assump_page_title = "mos_assump_page_title",
    ynow_mos_assump_page_sub = "mos_assump_page_sub",
    ynow_mos_assump_box_title = "mos_assump_box_title",
    ynow_mos_assump_b1_title = "mos_assump_b1_title",
    ynow_mos_assump_b1_lead = "mos_assump_b1_lead",
    ynow_mos_assump_goto_checklist = "mos_assump_goto_checklist",
    ynow_mos_assump_b2_title = "mos_assump_b2_title",
    ynow_mos_assump_b2_lead = "mos_assump_b2_lead",
    ynow_mos_assump_goto_alerts = "mos_assump_goto_alerts",
    ynow_mos_assump_b3_title = "mos_assump_b3_title",
    ynow_mos_assump_b3_lead = "mos_assump_b3_lead",
    ynow_mos_assump_goto_hfv = "mos_assump_goto_hfv",
    ynow_mos_assump_b4_title = "mos_assump_b4_title",
    ynow_mos_assump_b4_lead = "mos_assump_b4_lead",
    ynow_mos_assump_goto_lab = "mos_assump_goto_lab",
    ynow_mos_assump_chk_bear_base_label = "dc_label_bear_base",
    ynow_mos_assump_chk_bear_base_hint = "dc_hint_bear_base",
    ynow_mos_assump_bear_floor_label = "dc_cond_bear_mos_floor",
    ynow_mos_assump_bear_floor_hint = "dc_cond_hint_bear_mos_floor",
    ynow_mos_assump_chk_base_mos_label = "dc_label_base_mos",
    ynow_mos_assump_chk_base_mos_hint = "dc_hint_base_mos",
    ynow_mos_assump_base_floor_label = "dc_cond_base_mos_floor",
    ynow_mos_assump_base_floor_hint = "dc_cond_hint_base_mos_floor",
    ynow_mos_assump_alert_label = "mos_assump_alert_label",
    ynow_mos_assump_alert_hint = "mos_assump_alert_hint",
    ynow_mos_assump_hfv_near_label = "mos_assump_hfv_near",
    ynow_mos_assump_hfv_cheap_label = "mos_assump_hfv_cheap",
    ynow_mos_assump_hfv_rich_label = "mos_assump_hfv_rich",
    ynow_mos_assump_hfv_flat_label = "mos_assump_hfv_flat",
    ynow_mos_assump_hfv_strong_label = "mos_assump_hfv_strong",
    ynow_mos_assump_hfv_hint = "mos_assump_hfv_hint",
    ynow_mos_assump_hfv_risk = "mos_assump_hfv_risk",
    ynow_mos_assump_wvg_label = "bt_w_vg_label",
    ynow_mos_assump_wvg_hint = "bt_w_vg_hint",
    ynow_mos_assump_ladder_title = "mos_assump_ladder_title",
    ynow_mos_assump_lad_deep_label = "mos_assump_lad_deep",
    ynow_mos_assump_lad_mid_label = "mos_assump_lad_mid",
    ynow_mos_assump_lad_fair_label = "mos_assump_lad_fair",
    ynow_mos_assump_lad_mild_label = "mos_assump_lad_mild",
    ynow_mos_assump_ladder_hint = "mos_assump_ladder_hint",
    ynow_mos_assump_strat_risk = "mos_assump_strat_risk",
    ynow_mos_assump_gate_title = "auth_gate_checklist_kicker",
    ynow_mos_assump_gate_body = "mos_assump_gate_body"
  )
  for (id in names(keys)) set_txt(id, keys[[id]])
  invisible(NULL)
}
