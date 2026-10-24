# ==========================================
# ui.R - 前端介面設計
# ==========================================

#' Display / deploy build id from www/ynow_build.json (baked at UI source time).
.ynow_read_build_version <- function() {
  paths <- c(
    file.path("www", "ynow_build.json"),
    "ynow_build.json"
  )
  for (p in paths) {
    if (!file.exists(p)) next
    raw <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    m <- regmatches(raw, regexpr('"version"\\s*:\\s*"[^"]+"', raw))
    if (length(m) == 1L && nzchar(m[[1]])) {
      return(sub('^"version"\\s*:\\s*"([^"]+)".*$', "\\1", m[[1]]))
    }
  }
  "v27.12"
}
.YNOW_BUILD_VERSION <- .ynow_read_build_version()

#' Major segment of a display version (`v27.12` → 25; `v26` → 26).
#' Matches deploy policy: +0.01 is patch; folder / `YNOW_VERSION_BUMP_MAJOR` cut advances major.
.ynow_display_major <- function(label) {
  label <- as.character(label)[1]
  if (!is.character(label) || !nzchar(label) || is.na(label)) return(NA_integer_)
  m <- regmatches(label, regexpr("^[vV]?([0-9]+)", label))
  if (!length(m) || !nzchar(m[[1]])) return(NA_integer_)
  as.integer(sub("^[vV]?", "", m[[1]]))
}

#' TRUE when remote display version advances the major segment vs the session page.
.ynow_is_major_display_bump <- function(session_label, remote_label) {
  a <- .ynow_display_major(session_label)
  b <- .ynow_display_major(remote_label)
  is.finite(a) && is.finite(b) && as.integer(b) > as.integer(a)
}

# Backtest Zone：欄位下方小字說明
.bt_hint <- function(text) {
  tags$p(
    style = "margin: -6px 0 14px 0; font-size: 11.5px; line-height: 1.45; color: #777;",
    text
  )
}

.bt_section_intro <- function(text) {
  tags$p(style = "margin: 0 0 12px 0; font-size: 12.5px; color: #555; line-height: 1.5;", text)
}

#' Blue Chip Models filter: Absolute vs Relative groups, one `lab_im_methods` input.
.lab_im_method_cb <- function(value, label_id, label_text, selected) {
  input_tag <- tags$input(type = "checkbox", name = "lab_im_methods", value = value)
  if (isTRUE(value %in% selected)) {
    input_tag$attribs$checked <- "checked"
  }
  tags$label(class = "checkbox-inline", input_tag, tags$span(id = label_id, label_text))
}

.lab_im_methods_filter_ui <- function(selected = character(0)) {
  selected <- as.character(selected %||% character(0))
  abs_keys <- if (exists("LAB_METHOD_ABS_KEYS", inherits = TRUE)) {
    as.character(LAB_METHOD_ABS_KEYS)
  } else {
    c("nav", "pb", "dcf", "ddm", "ri")
  }
  rel_keys <- if (exists("LAB_METHOD_REL_KEYS", inherits = TRUE)) {
    as.character(LAB_METHOD_REL_KEYS)
  } else {
    c("enterprise", "earnings", "revenue", "book")
  }
  # Short filter labels (no 「模型」/ "-Model" suffix — sidebar menus keep those).
  label_map <- c(
    nav = "NAV", pb = "Justified P/B", dcf = "DCF", ddm = "DDM", ri = "RI",
    enterprise = "Enterprise value", earnings = "Earnings",
    revenue = "Revenue", book = "Book multiples"
  )
  id_map <- c(
    nav = "ynow_lab_im_method_nav",
    pb = "ynow_lab_im_method_pb",
    dcf = "ynow_lab_im_method_dcf",
    ddm = "ynow_lab_im_method_ddm",
    ri = "ynow_lab_im_method_ri",
    enterprise = "ynow_lab_im_method_enterprise",
    earnings = "ynow_lab_im_method_earnings",
    revenue = "ynow_lab_im_method_revenue",
    book = "ynow_lab_im_method_book"
  )
  mk_group <- function(keys) {
    lapply(keys, function(k) {
      .lab_im_method_cb(
        k,
        id_map[[k]],
        if (!is.null(label_map[[k]])) label_map[[k]] else k,
        selected
      )
    })
  }
  tags$div(
    id = "lab_im_methods",
    class = paste(
      "form-group shiny-input-checkboxgroup shiny-input-container",
      "shiny-input-container-inline"
    ),
    tags$label(
      class = "control-label",
      id = "lab_im_methods-label",
      `for` = "lab_im_methods",
      tags$span(id = "ynow_lab_im_methods_label", "模型")
    ),
    tags$div(
      class = "ynow-lab-im-methods-groups",
      tags$div(
        class = "ynow-lab-im-methods-group ynow-lab-im-methods-group--abs",
        tags$span(
          id = "ynow_lab_im_methods_abs_label",
          class = "ynow-lab-im-methods-group-label",
          "絕對"
        ),
        tags$div(class = "shiny-options-group", mk_group(abs_keys))
      ),
      tags$div(
        class = "ynow-lab-im-methods-group ynow-lab-im-methods-group--rel",
        tags$span(
          id = "ynow_lab_im_methods_rel_label",
          class = "ynow-lab-im-methods-group-label",
          "相對"
        ),
        tags$div(class = "shiny-options-group", mk_group(rel_keys))
      )
    )
  )
}

# C-STRUCT-02: About page bodies (Full / Lite / legal). Call site stays on tab "about".
source("ynow_about_ui.R", local = TRUE, encoding = "UTF-8")
# C-STRUCT-02 seam 2: cookie banner / panel / reopen + prefs JS. CSS + applyUiLocale stay here.
source("ynow_cookie_notice_ui.R", local = TRUE, encoding = "UTF-8")


#' 品牌首頁：一句用途 + 四個主要入口
.home_brand_ui <- function() {
  tagList(
    tags$div(
      class = "ynow-home",
      tags$div(
        class = "ynow-home-logo-wrap ynow-lite-toggle",
        id = "ynow_home_lite_toggle",
        role = "button",
        tabindex = "0",
        `aria-pressed` = "false",
        `aria-label` = "Toggle Lite mode",
        title = "Click to switch Lite / Full",
        tags$img(
          class = "ynow-home-logo",
          src = "ynow-logo-full-480.png",
          alt = "YNow — WH.Y VALUE NOW",
          width = 132,
          draggable = "false"
        ),
        tags$span(
          class = "ynow-lite-badge",
          id = "ynow_home_lite_badge",
          "LITE"
        )
      ),
      tags$h2(id = "ynow_home_title", class = "ynow-home-title", "The YNow App"),
      tags$p(
        id = "ynow_home_lead",
        class = "ynow-home-lead",
        "A valuation desk for US and Taiwan equities."
      ),
      tags$p(
        id = "ynow_home_method",
        class = "ynow-home-method ynow-full-only",
        "Classify the business, choose the model, then decide."
      ),
      tags$p(
        id = "ynow_home_method_lite",
        class = "ynow-home-method ynow-lite-only",
        "Classify the business, then read fair value and YNOW."
      ),
      tags$div(
        class = "ynow-home-grid",
        role = "group",
        `aria-labelledby` = "ynow_home_title",
        # Order mirrors sidebar after Home: Macro → Blue Chip → Company → YNOW → Value → Action
        tags$button(
          type = "button",
          class = "ynow-home-card",
          `data-tab` = "macro_market",
          tags$p(id = "ynow_home_market_k", class = "ynow-home-card-k", "Market"),
          tags$p(
            id = "ynow_home_market_d",
            class = "ynow-home-card-d",
            "YNOW / TYNOW, major boards, Rf, and industry performance versus the benchmark."
          )
        ),
        tags$button(
          type = "button",
          class = "ynow-home-card",
          `data-tab` = "bluechip",
          tags$p(id = "ynow_home_bluechip_k", class = "ynow-home-card-k", "Blue Chip"),
          tags$p(
            id = "ynow_home_bluechip_d",
            class = "ynow-home-card-d",
            "Peer ranking pools, truncate rules, clustering, and custom screen."
          )
        ),
        tags$button(
          type = "button",
          class = "ynow-home-card",
          `data-tab` = "dashboard",
          tags$p(id = "ynow_home_company_k", class = "ynow-home-card-k", "Company"),
          tags$p(
            id = "ynow_home_company_d",
            class = "ynow-home-card-d",
            "Industry-standard snapshot, KPIs, and financial statements."
          )
        ),
        tags$button(
          type = "button",
          class = "ynow-home-card",
          `data-tab` = "sensitivity",
          tags$p(id = "ynow_home_ynow_k", class = "ynow-home-card-k", "YNOW"),
          tags$p(
            id = "ynow_home_ynow_d",
            class = "ynow-home-card-d",
            "F-Score and statement alerts. A quality screen, not a buy signal."
          )
        ),
        tags$button(
          type = "button",
          class = "ynow-home-card",
          `data-tab` = "get_started",
          `data-tab-lite` = "smart_analysis",
          tags$p(id = "ynow_home_value_k", class = "ynow-home-card-k ynow-full-only", "Fundamental Value"),
          tags$p(id = "ynow_home_value_k_lite", class = "ynow-home-card-k ynow-lite-only", "Smart Analysis"),
          tags$p(
            id = "ynow_home_value_d",
            class = "ynow-home-card-d ynow-full-only",
            "Fair value and MOS across DCF, DDM, RI, and P/B."
          ),
          tags$p(
            id = "ynow_home_value_d_lite",
            class = "ynow-home-card-d ynow-lite-only",
            "Fair value and MOS from the recommended model."
          )
        ),
        tags$button(
          type = "button",
          class = "ynow-home-card ynow-full-only",
          `data-tab` = "decision_checklist",
          tags$p(id = "ynow_home_decide_k", class = "ynow-home-card-k", "Action"),
          tags$p(
            id = "ynow_home_decide_d",
            class = "ynow-home-card-d",
            "F-Score, statement alerts, and the Decision Checklist."
          )
        )
      ),
      tags$div(
        class = "ynow-home-legal",
        # One row under quick-pick: Sign in · Register · About; disclaimer below.
        tags$div(
          class = "ynow-home-legal-links",
          if (exists("ynow_auth_home_ui", mode = "function")) ynow_auth_home_ui(),
          tags$span(
            class = "ynow-home-legal-sep ynow-auth-pair-sep",
            "·",
            `aria-hidden` = "true"
          ),
          tags$button(
            type = "button",
            id = "ynow_home_legal_about",
            class = "ynow-home-legal-link",
            `data-tab` = "about",
            "About · Privacy, IP & risk"
          )
        ),
        tags$p(
          id = "ynow_home_legal",
          class = "ynow-home-legal-text",
          paste0(
            "Research and education only — not investment advice. ",
            "You alone bear all investment decisions and losses."
          )
        )
      )
    ),
    tags$script(HTML("
      (function () {
        if (document.documentElement.getAttribute('data-ynow-home') === '1') return;
        document.documentElement.setAttribute('data-ynow-home', '1');
        document.addEventListener('click', function (ev) {
          var card = ev.target && ev.target.closest
            ? ev.target.closest('.ynow-home-card, .ynow-home-legal-link')
            : null;
          if (!card) return;
          ev.preventDefault();
          var lite = document.body && document.body.classList.contains('ynow-lite');
          var tab = card.getAttribute('data-tab');
          if (lite) tab = card.getAttribute('data-tab-lite') || tab;
          if (!tab) return;
          /* Lite home Value card → smart_analysis; Full → get_started / other data-tab */
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
    "))
  )
}

#' KPI 色碼圖例：藍→紅→黑→白；金色屬性重視改一行文字註解（非 chip 框）
.kpi_band_color_legend_ui <- function() {
  tags$div(
    class = "ynow-kpi-legend-wrap",
    tags$div(
      class = "ynow-ann-legend ynow-kpi-legend-chips",
      tags$div(
        class = "ynow-ann-chip ynow-kpi-legend-chip",
        tags$span(class = "ynow-ann-swatch", style = "background:#3c8dbc;"),
        tags$span(id = "ynow_kpi_legend_blue", "藍 · 優於同業 (Better)")
      ),
      tags$div(
        class = "ynow-ann-chip ynow-kpi-legend-chip",
        tags$span(class = "ynow-ann-swatch", style = "background:#dd4b39;"),
        tags$span(id = "ynow_kpi_legend_red", "紅 · 劣於同業 (Worse)")
      ),
      tags$div(
        class = "ynow-ann-chip ynow-kpi-legend-chip",
        tags$span(class = "ynow-ann-swatch", style = "background:#222;"),
        tags$span(id = "ynow_kpi_legend_black", "黑 · 與同業一致 (In band)")
      ),
      tags$div(
        class = "ynow-ann-chip ynow-kpi-legend-chip",
        tags$span(class = "ynow-ann-swatch",
                  style = "background:#fff; border:1px solid #999;"),
        tags$span(id = "ynow_kpi_legend_white", "白 · 無法比較／N/A")
      )
    ),
    tags$p(
      class = "ynow-focus-metric-note",
      tags$span(
        class = "ynow-focus-metric-swatch ynow-focus-metric-swatch--legend",
        `aria-hidden` = "true"
      ),
      tags$span(id = "ynow_kpi_legend_focus", "金 · 本財報屬性關鍵指標")
    )
  )
}

# Shared gray「回復預設」／試算 helpers live in setup.R (ynow_reset_defaults_btn, ynow_calc_btn).

#' Shared CAPM / Beta settings block (canonical IDs on DCF → WACC).
#' DDM → Ke uses the same layout with `id_prefix = "ddm_"`; server keeps mirrors in sync.
#' @param calc_id actionButton id
#' @param result_id htmlOutput id for CAPM result text
#' @param width shinydashboard box width (1–12)
#' @param id_prefix optional prefix for input ids (e.g. `"ddm_"` → `ddm_capm_rf`)
#' @param box_title_id DOM id for locale title
#' @param sync_label_id DOM id for sync-checkbox label
#' @param rf_note_id uiOutput id for Rf source note
#' @param btn_label actionButton label (locale may overwrite)
capm_beta_settings_ui <- function(title = "CAPM Estimate rₑ",
                                  calc_id = "calc_capm",
                                  result_id = "capm_result",
                                  width = 6,
                                  id_prefix = "",
                                  box_title_id = "ynow_capm_box_title",
                                  sync_label_id = "ynow_sync_gs_beta_label",
                                  rf_note_id = "capm_rf_source_note",
                                  btn_label = "Estimate rₑ (CAPM)") {
  pid <- function(x) {
    if (nzchar(as.character(id_prefix %||% "")[1])) paste0(id_prefix, x) else x
  }
  box(
    width = width,
    h4(title, id = box_title_id),
    numericInput(pid("capm_rf"), "Risk-free rate Rf (%)", value = APP_DEFAULTS$capm_rf, step = 0.01),
    uiOutput(rf_note_id),
    numericInput(pid("capm_rm"), "Market return Rm (%)", value = APP_DEFAULTS$capm_rm, step = 0.01),
    numericInput(pid("capm_beta"), "Beta (β)", value = APP_DEFAULTS$capm_beta, step = 0.01),
    checkboxInput(
      pid("sync_gs_beta"),
      tags$span(id = sync_label_id, style = "font-weight: bold;", "Sync with Basic Setup"),
      value = isTRUE(APP_DEFAULTS$sync_gs_beta)
    ),
    actionButton(calc_id, btn_label, class = "btn-primary ynow-btn-calc-capm"),
    tags$br(), htmlOutput(result_id)
  )
}

#' Left companion on DDM → Ke (mirrors WACC tab’s rᵈ column role: context beside CAPM).
ddm_ke_bridge_settings_ui <- function(width = 6) {
  box(
    width = width,
    h4("Ke and central rₑ", id = "ynow_ddm_ke_bridge_title"),
    tags$p(
      id = "ynow_ddm_ke_bridge_help",
      style = "margin:0 0 10px 0;color:#666;font-size:12px;line-height:1.55;",
      paste0(
        "DDM discounts at equity cost Ke, the same central Ke as DCF→WACC rₑ. ",
        "When \"Use estimated Ke\" is checked, Ke follows CAPM; uncheck to override and keep bidirectional sync with WACC rₑ. ",
        "Choose β source on this model's Beta (β) tab."
      )
    ),
    tags$div(
      style = "padding:8px;border-left:4px solid #222222;background:#f5f5f5;font-size:13px;margin-bottom:10px;",
      tags$b("CAPM："), " Ke = Rf + β × (Rm − Rf)",
      tags$br(),
      tags$span(style = "color:#666;font-size:12px;", "ERP = Rm − Rf (equity risk premium)")
    ),
    htmlOutput("ddm_ke_bridge_status")
  )
}

#' rᵈ estimation block: Interest Expense ÷ Interest-bearing Debt (pre-tax rᵈ).
#' Layout companion to CAPM on the WACC tab (left 50%).
rd_estimate_settings_ui <- function(width = 6) {
  box(
    width = width,
    h4("估算 rᵈ", id = "ynow_rd_box_title"),
    numericInput(
      "rd_interest_expense", "利息費用",
      value = NA_real_, min = 0, step = 1
    ),
    uiOutput("rd_interest_source_note"),
    numericInput(
      "rd_interest_bearing_debt", "有息負債",
      value = NA_real_, min = 0, step = 1
    ),
    uiOutput("rd_debt_source_note"),
    fluidRow(
      column(6, numericInput(
        "wacc_rd_min", "估算 rᵈ 下限 (%)",
        value = APP_DEFAULTS$wacc_rd_min, min = 0, step = 0.1
      )),
      column(6, numericInput(
        "wacc_rd_max", "估算 rᵈ 上限 (%)",
        value = APP_DEFAULTS$wacc_rd_max, min = 0, step = 0.1
      ))
    ),
    checkboxInput(
      "use_estimated_rd",
      tags$span(id = "ynow_use_estimated_rd_label", "採用估算 rᵈ（利息／有息負債）"),
      value = isTRUE(APP_DEFAULTS$use_est_rd)
    ),
    actionButton("calc_rd", "估算 rᵈ", class = "btn-primary"),
    tags$br(), htmlOutput("rd_result")
  )
}

#' Canonical + mirrored β-source radio IDs (bidirectional sync in server).
#' Basic Setup ↔ DCF ↔ DDM ↔ RI. P/B / NAV do not host Beta tabs.
.BETA_U_APPLY_SOURCE_IDS <- c(
  "beta_u_apply_source",
  "dcf_beta_u_apply_source",
  "ddm_beta_u_apply_source",
  "ri_beta_u_apply_source"
)

#' Matching "sync selected β" buttons (same order as .BETA_U_APPLY_SOURCE_IDS).
.BETA_U_APPLY_BTN_IDS <- c(
  "apply_beta_u_selected",
  "dcf_apply_beta_u_selected",
  "ddm_apply_beta_u_selected",
  "ri_apply_beta_u_selected"
)

#' Shared β source picker (Yahoo / industry / Bottom-Up / Hamada / manual).
#' Unique input_id per placement; server keeps all IDs in sync.
#' @param input_id radioButtons id
#' @param apply_btn_id actionButton id (sync selected β into CAPM)
#' @param include_hidden_purpose keep legacy beta_purpose input (Basic Setup only)
#' @param include_crosscheck render beta_crosscheck_panel under the picker
beta_source_picker_ui <- function(input_id,
                                  apply_btn_id,
                                  include_hidden_purpose = FALSE,
                                  include_crosscheck = FALSE,
                                  extra = NULL) {
  default_selected <- tryCatch(
    APP_DEFAULTS$beta_u_apply_source,
    error = function(e) "summary"
  )
  if (!nzchar(as.character(default_selected %||% "")[1])) default_selected <- "summary"

  tagList(
    tags$p(
      class = "ynow-beta-source-heading",
      style = "font-weight:600; margin:0 0 10px 0;",
      "Beta source (default writes into CAPM)"
    ),
    # choiceNames／choiceValues 由 server 動態覆寫（數字粗體 + 各選項旁說明）
    radioButtons(
      input_id,
      label = NULL,
      choiceNames = list(
        HTML("Yahoo Finance Summary β <b>n/a</b> <span style='color:#666;font-size:12px;'>— Yahoo Finance Summary \"Beta (5Y Monthly)\"; default writes into CAPM.</span>"),
        HTML("Industry default β <b>n/a</b> <span style='color:#666;font-size:12px;'>— Structural β for the selected industry.</span>"),
        HTML("Peer-average Bottom-Up (βᵤ→βe) <b>n/a</b> <span style='color:#666;font-size:12px;'>— Unlevered mean / median βᵤ of peer companies.</span>"),
        HTML("Unlevered βᵤ <b>n/a</b> <span style='color:#666;font-size:12px;'>— Hamada βᵤ = β_L / (1+(1−T)·D/E).</span>"),
        HTML("Manual βe <b>n/a</b>")
      ),
      choiceValues = list("summary", "industry", "bottomup", "unlever_firm", "manual"),
      selected = default_selected,
      inline = FALSE
    ),
    if (isTRUE(include_hidden_purpose)) {
      tags$div(
        style = "display:none;",
        radioButtons(
          "beta_purpose",
          NULL,
          choices = c("valuation" = "valuation"),
          selected = "valuation"
        )
      )
    },
    tags$p(
      class = "ynow-beta-rolling-help help-block",
      style = "margin-top:0;",
      "Rolling estimates are for cross-check only and are not written into CAPM (hence omitted above)."
    ),
    actionButton(
      apply_btn_id,
      "Sync selected β now",
      class = "btn-success ynow-btn-sync-selected-beta",
      icon = icon("check")
    ),
    extra,
    if (isTRUE(include_crosscheck)) {
      tagList(tags$br(), tags$br(), uiOutput("beta_crosscheck_panel"))
    }
  )
}

#' Model Beta tab: shared picker + note (replaces old "moved to Basic Setup" stub).
#' No outer box/well chrome — controls sit flush in the tab.
.beta_model_source_section_ui <- function(input_id, apply_btn_id, extra = NULL) {
  fluidRow(
    column(
      width = 12,
      beta_source_picker_ui(
        input_id = input_id,
        apply_btn_id = apply_btn_id,
        extra = extra
      )
    )
  )
}

#' Beta Overview：來源寫入 CAPM + Rolling 僅對照
beta_overview_section_ui <- function() {
  tagList(
    fluidRow(
      uiOutput("vbx_beta_summary", class = "col-sm-6"),
      valueBoxOutput("vbx_beta_industry", width = 6)
    ),
    fluidRow(
      valueBoxOutput("vbx_beta_unlever_bottomup", width = 4),
      uiOutput("vbx_beta_unlever_firm", class = "col-sm-4"),
      valueBoxOutput("vbx_beta_estimated", width = 4)
    ),
    fluidRow(
      box(
        width = 12, status = "success", solidHeader = FALSE,
        beta_source_picker_ui(
          input_id = "beta_u_apply_source",
          apply_btn_id = "apply_beta_u_selected",
          include_hidden_purpose = TRUE,
          include_crosscheck = TRUE
        )
      )
    )
  )
}

#' Peer unlever: Bottom-Up peer average (left) + firm Hamada / manual βe (right)
beta_unlever_section_ui <- function() {
  tagList(
    fluidRow(
      box(
        width = 7, status = "warning", solidHeader = FALSE,
        tags$h5(id = "ynow_beta_bottomup_heading", style = "margin-top:0;", "Bottom-Up peer average (primary valuation estimate)"),
        selectizeInput(
          "beta_peers",
          "Peer / competitor tickers (multi-select or type)",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            create = TRUE,
            placeholder = "e.g. INTC, AMD, AVGO …",
            plugins = list("remove_button"),
            maxItems = 15
          )
        ),
        radioButtons(
          "beta_bottomup_agg",
          "βᵤ aggregation",
          choices = c("Mean" = "mean", "Median" = "median"),
          selected = APP_DEFAULTS$beta_bottomup_agg,
          inline = TRUE
        ),
        helpText(
          id = "ynow_beta_bottomup_help",
          paste0(
            "Flow: peer equity β → unlever → mean / median βᵤ. ",
            "If peers are empty, fall back to industry β and industry leverage (data-limited proxy)."
          )
        ),
        actionButton(
          "calc_beta_bottomup", "Calculate Bottom-Up βᵤ",
          class = "btn-primary ynow-btn-calc-beta-bottomup", icon = icon("calculator")
        ),
        tags$br(), tags$br(),
        htmlOutput("beta_bottomup_result"),
        tags$br(),
        tableOutput("beta_bottomup_peers_table")
      ),
      box(
        width = 5, status = "warning", solidHeader = FALSE,
        tags$h5(id = "ynow_beta_unlever_heading", style = "margin-top:0;", "Unlevered βᵤ (Hamada)"),
        tags$div(
          style = "display:none;",
          radioButtons(
            "beta_bl_source", "Levered Beta (β_L) source",
            choices = c(
              "Finance Summary (Yahoo 5Y Monthly)" = "summary",
              "Rolling estimate (run Rolling tab first)" = "rolling",
              "Auto (Summary → Rolling)" = "auto"
            ),
            selected = APP_DEFAULTS$beta_bl_source,
            inline = FALSE
          ),
          # 隱藏保留：舊版再槓桿設定已移除，僅維持 input ID 相容
          radioButtons(
            "beta_relever_de_mode", NULL,
            choices = c("current" = "current"),
            selected = "current"
          ),
          numericInput("beta_target_de", NULL, value = NA, min = 0, max = 10, step = 0.01)
        ),
        helpText(
          id = "ynow_beta_unlever_help",
          paste0(
            "Hamada (debt β≈0): βᵤ = β_L / (1+(1−T)·D/E). ",
            "β_L defaults to Yahoo Finance Summary \"Beta (5Y Monthly)\"; T from WACC; D/E = Total Debt ÷ equity market value. ",
            "Pick \"Unlevered βᵤ\" as β source to write into CAPM; levered β_L itself is not written into CAPM."
          )
        ),
        htmlOutput("beta_unlever_firm_result"),
        tags$hr(),
        tags$h5(id = "ynow_beta_manual_heading", "Manual βe"),
        numericInput(
          "beta_u_manual",
          NULL,
          value = APP_DEFAULTS$beta_u_manual,
          min = 0, max = 5, step = 0.01
        ),
        helpText(
          id = "ynow_beta_manual_help",
          paste0(
            "After selecting \"Manual βe\" as β source, this value writes into CAPM; ",
            "editing here also switches the source to manual and syncs."
          )
        )
      )
    )
  )
}

#' Rolling β estimate (Basic Setup)
#' Cross-check only: compare vs valuation β; do not write into CAPM.
beta_rolling_section_ui <- function() {
  tagList(
    fluidRow(
      box(
        width = 5, status = "primary", solidHeader = TRUE,
        title = tagList(
          icon("sliders-h"),
          tags$span(id = "ynow_beta_rolling_settings_title", "Estimate settings (cross-check only)")
        ),
        selectizeInput(
          "beta_bench", "Benchmark index",
          choices = market_profile("US")$beta_bench_choices,
          selected = APP_DEFAULTS$beta_bench,
          options = list(
            create = TRUE,
            placeholder = "Pick a common index, or enter a ticker…",
            maxItems = 1
          )
        ),
        selectInput(
          "beta_lookback_months", "Lookback window (cross-check)",
          choices = c(
            "1Y (12 months)" = 12,
            "2Y (24 months)" = 24,
            "5Y (60 months, Yahoo-aligned)" = 60
          ),
          selected = as.character(APP_DEFAULTS$beta_lookback_months)
        ),
        # 技術門檻：固定預設，不另開冷門參數
        tags$div(
          style = "display:none;",
          numericInput(
            "beta_min_obs", "Min observation months",
            value = APP_DEFAULTS$beta_min_obs,
            min = 12, max = 60, step = 1
          )
        ),
        helpText(
          id = "ynow_beta_rolling_help_body",
          paste0(
            "Rolling β measures recent equity sensitivity to the market and often embeds sentiment. ",
            "Use it only to cross-check valuation β — it is not written into CAPM / Ke / WACC. ",
            "β = Cov(Rᵢ, Rₘ) / Var(Rₘ); compare 1Y / 2Y / 5Y windows."
          )
        ),
        actionButton(
          "calc_beta_est", "Estimate Rolling β (cross-check)",
          class = "btn-primary ynow-btn-calc-beta-est", icon = icon("calculator")
        ),
        tags$br(), tags$br(),
        htmlOutput("beta_est_result")
      ),
      box(
        width = 7, status = "info", solidHeader = TRUE,
        title = tagList(
          icon("exchange-alt"),
          tags$span(id = "ynow_beta_rolling_compare_title", "Window comparison")
        ),
        tableOutput("beta_window_table"),
        tags$hr(),
        plotOutput("plt_beta_scatter", height = "280px")
      )
    )
  )
}

#' Consideration-dimensions comparison table (Model Selector annotation + shared HTML).
#' Dual `.ynow-loc-en` / `.ynow-loc-zh` tables; visibility follows `html[lang]`.
.consideration_dimensions_table <- function() {
  tags$div(
    style = "overflow-x: auto;",
    tags$div(
      class = "ynow-loc-en",
      HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #2C3E50; color: white;'>
                                   <tr>
                                     <th>Dimension</th>
                                     <th>DDM</th>
                                     <th>DCF</th>
                                     <th>RI</th>
                                     <th>P/B</th>
                                     <th>NAV</th>
                                     <th>Multiples</th>
                                     <th>SOTP</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>Primary data</b></td>
                                     <td>Cash flow (dividends)</td>
                                     <td>Cash flow (CFO / CapEx)</td>
                                     <td>Income + balance sheet</td>
                                     <td>Balance sheet (equity / tangible book)</td>
                                     <td>Balance sheet (equity / investments)</td>
                                     <td>P&L / cash flow / revenue; ARR manual</td>
                                     <td>Multi-segment revenue (BB Lab) + Cash / Debt</td>
                                   </tr>
                                   <tr>
                                     <td><b>Investor lens</b></td>
                                     <td>Minority (dividend claim)</td>
                                     <td>Control / M&A (cash generation)</td>
                                     <td>Either (esp. negative FCF)</td>
                                     <td>Banks / insurance / book anchor</td>
                                     <td>HoldCo / conglomerate</td>
                                     <td>Growth / intangibles cross-check</td>
                                     <td>Multi-segment structure cross-check</td>
                                   </tr>
                                   <tr>
                                     <td><b>Typical fit</b></td>
                                     <td>Mature stable payout</td>
                                     <td>Growth / expansion, positive FCF</td>
                                     <td>Asset-heavy, unstable FCF</td>
                                     <td>Banks, insurance, REIT</td>
                                     <td>HoldCo / group book</td>
                                     <td>SaaS / platforms / high growth</td>
                                     <td>HoldCo / multi-segment conglomerates</td>
                                   </tr>
                                   <tr>
                                     <td><b>Payout dependence</b></td>
                                     <td><span class='label label-danger'>Very high</span></td>
                                     <td><span class='label label-success'>Low</span></td>
                                     <td><span class='label label-success'>Very low</span></td>
                                     <td><span class='label label-success'>Low</span></td>
                                     <td><span class='label label-success'>Low</span></td>
                                     <td><span class='label label-success'>Low</span></td>
                                     <td><span class='label label-success'>Low</span></td>
                                   </tr>
                                   <tr>
                                     <td><b>Typical limits</b></td>
                                     <td>Breaks with no / volatile dividends</td>
                                     <td>Hard when FCF is chronically negative or highly cyclical</td>
                                     <td>Biased when book / ROE is distorted</td>
                                     <td>Weak when intangibles dominate</td>
                                     <td>Not market-method SOTP; no investments → equity</td>
                                     <td>Multiples move with sentiment; not Fair Value</td>
                                     <td>Needs ≥2 positive segment revenues; not EBIT SOTP</td>
                                   </tr>
                                 </tbody>
                               </table>")
    ),
    tags$div(
      class = "ynow-loc-zh",
      HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #2C3E50; color: white;'>
                                   <tr>
                                     <th>考慮維度</th>
                                     <th>DDM</th>
                                     <th>DCF</th>
                                     <th>RI</th>
                                     <th>P/B</th>
                                     <th>NAV</th>
                                     <th>Multiples</th>
                                     <th>SOTP</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>主要資料來源</b></td>
                                     <td>現金流量表（股利）</td>
                                     <td>現金流量表（CFO／CapEx）</td>
                                     <td>損益表＋資產負債表</td>
                                     <td>資產負債表（權益／有形淨值）</td>
                                     <td>資產負債表（權益／投資科目）</td>
                                     <td>損益／現金流／營收；ARR 手動</td>
                                     <td>多部門營收（BB Lab）＋Cash／Debt</td>
                                   </tr>
                                   <tr>
                                     <td><b>投資人觀點</b></td>
                                     <td>小股東（配息請求權）</td>
                                     <td>控股／併購（造血能力）</td>
                                     <td>皆可（尤其負 FCF）</td>
                                     <td>金融／保險／帳面錨</td>
                                     <td>控股／綜合企業</td>
                                     <td>成長／無形資產交叉</td>
                                     <td>多事業部結構交叉</td>
                                   </tr>
                                   <tr>
                                     <td><b>典型適用</b></td>
                                     <td>成熟穩健配息</td>
                                     <td>成長／擴張、FCF 為正</td>
                                     <td>資產密集、FCF 不穩</td>
                                     <td>銀行、保險、REIT</td>
                                     <td>控股／集團帳面</td>
                                     <td>SaaS／平台／高成長</td>
                                     <td>控股／綜合多部門</td>
                                   </tr>
                                   <tr>
                                     <td><b>對配息依賴</b></td>
                                     <td><span class='label label-danger'>極高</span></td>
                                     <td><span class='label label-success'>低</span></td>
                                     <td><span class='label label-success'>極低</span></td>
                                     <td><span class='label label-success'>低</span></td>
                                     <td><span class='label label-success'>低</span></td>
                                     <td><span class='label label-success'>低</span></td>
                                     <td><span class='label label-success'>低</span></td>
                                   </tr>
                                   <tr>
                                     <td><b>典型限制</b></td>
                                     <td>不配息／配息波動大時失效</td>
                                     <td>FCF 長期為負或高度循環時難估</td>
                                     <td>帳面／ROE 失真時偏誤</td>
                                     <td>無形資產主導時失準</td>
                                     <td>非市場法 SOTP；無投資科目時＝權益</td>
                                     <td>倍數隨市場情緒；非 Fair Value</td>
                                     <td>需 ≥2 正值部門營收；非 EBIT SOTP</td>
                                   </tr>
                                 </tbody>
                               </table>")
    )
  )
}

#' Collapsible Model Selector footnote: consideration dimensions (default closed).
#' Reuses `.ynow-notes` chrome; title class is NOT `.ynow-notes__title` so
#' applyUiLocale does not overwrite it with the generic Notes / 附註 string.
.model_selector_dimensions_annotation_ui <- function() {
  tags$details(
    id = "ynow_ms_dims_details",
    class = "ynow-notes ynow-ms-dims",
    tags$summary(
      class = "ynow-notes__summary",
      id = "ynow_ms_dims_summary",
      title = "Show or hide consideration dimensions",
      `aria-label` = "Show or hide consideration dimensions",
      tags$span(
        class = "ynow-ms-dims__title",
        id = "ynow_ms_dims_title",
        "考慮維度"
      )
    ),
    tags$div(
      class = "ynow-notes__body",
      .consideration_dimensions_table()
    )
  )
}

#' Valuation methodology guide (Decision Matrix + model tabs).
#' Mounted at the bottom of Basic Setup (Full only; Lite hides Basic Setup).
#' Title/lead use column(12); tabBox keeps width=12 (shinydashboard always emits col-sm-N).
#' Locale: `.ynow-loc-en` / `.ynow-loc-zh` toggle via `html[lang]` (see CSS + applyUiLocale).
.valuation_methodology_section_ui <- function(collapsible = TRUE, collapsed = FALSE) {
  # collapsible/collapsed kept for call-site compatibility (no outer box to collapse)
  .method_table_fv_en <- HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #2C3E50; color: white;'>
                                   <tr>
                                     <th>Comparison</th>
                                     <th>DDM</th>
                                     <th>DCF</th>
                                     <th>RI</th>
                                     <th>P/B</th>
                                     <th>NAV</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>Cash flow / anchor</b></td>
                                     <td>DPS D (equity claim)</td>
                                     <td>FCFF (firm) or FCFE (equity)</td>
                                     <td>Book equity + residual income</td>
                                     <td>BVPS / TBVPS / HoldCo NAVPS × target P/B</td>
                                     <td>Book HoldCo NAVPS × multiple</td>
                                   </tr>
                                   <tr>
                                     <td><b>Discount / multiple</b></td>
                                     <td>Ke (CAPM)</td>
                                     <td>FCFF: WACC; FCFE: Ke</td>
                                     <td>Ke (CAPM)</td>
                                     <td>Target multiple (Justified / industry / history)</td>
                                     <td>Discount / premium multiple (not Justified)</td>
                                   </tr>
                                   <tr>
                                     <td><b>Growth g / years</b></td>
                                     <td>Dividend perpetual g; SPM earnings g; two-stage g1 / n1</td>
                                     <td>Explicit forecast years n; terminal SGR (vs WACC / Ke)</td>
                                     <td>RI forecast horizon; terminal g (vs Ke)</td>
                                     <td>Justified needs g; industry / history multiples do not require SGR</td>
                                     <td>No forecast horizon n</td>
                                   </tr>
                                   <tr>
                                     <td><b>Core formula</b></td>
                                     <td>Gordon: P₀=D₁/(Ke−g); SPM; two-stage ΣPV(D)+PV(TV)</td>
                                     <td>EV＝ΣPV(FCFF)+PV(TV) or Equity＝ΣPV(FCFE)+PV(TV)</td>
                                     <td>V₀=B₀+ΣPV(RI)+PV(TV_RI)</td>
                                     <td>P=(BVPS / TBVPS / NAVPS)×Target P/B</td>
                                     <td>P=NAVPS×NAV multiple</td>
                                   </tr>
                                   <tr>
                                     <td><b>Output role</b></td>
                                     <td>Fair Value per share</td>
                                     <td>Fair Value (FCFF via EV bridge)</td>
                                     <td>Fair Value intrinsic per share</td>
                                     <td>Fair Value range (Bear / Base / Bull)</td>
                                     <td>Fair Value (HoldCo / asset anchor)</td>
                                   </tr>
                                 </tbody>
                               </table>")
  .method_table_fv_zh <- HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #2C3E50; color: white;'>
                                   <tr>
                                     <th>對照項目</th>
                                     <th>DDM</th>
                                     <th>DCF</th>
                                     <th>RI</th>
                                     <th>P/B</th>
                                     <th>NAV</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>現金流／錨定</b></td>
                                     <td>每股股利 D（股權請求權）</td>
                                     <td>FCFF（企業）或 FCFE（股權）</td>
                                     <td>帳面淨值 + 超額盈餘</td>
                                     <td>BVPS／TBVPS／控股 NAVPS × 目標 P/B</td>
                                     <td>帳面控股淨資產 NAVPS × 倍數</td>
                                   </tr>
                                   <tr>
                                     <td><b>折現率／倍數</b></td>
                                     <td>Ke（CAPM）</td>
                                     <td>FCFF：WACC；FCFE：Ke</td>
                                     <td>Ke（CAPM）</td>
                                     <td>目標倍數（Justified／產業／歷史）</td>
                                     <td>折價／溢價倍數（非 Justified）</td>
                                   </tr>
                                   <tr>
                                     <td><b>成長率 g／年數</b></td>
                                     <td>股利永續 g；SPM 盈餘 g；二階段 g1／n1</td>
                                     <td>明確預測年數 n；終值 SGR（相對 WACC／Ke）</td>
                                     <td>RI 預測期；終值 g（相對 Ke）</td>
                                     <td>Justified 需 g；產業／歷史倍數不強制 SGR</td>
                                     <td>不涉及預測年數 n</td>
                                   </tr>
                                   <tr>
                                     <td><b>核心公式</b></td>
                                     <td>Gordon：P₀=D₁/(Ke−g)；SPM；二階段 ΣPV(D)+PV(TV)</td>
                                     <td>EV＝ΣPV(FCFF)+PV(TV) 或 Equity＝ΣPV(FCFE)+PV(TV)</td>
                                     <td>V₀=B₀+ΣPV(RI)+PV(TV_RI)</td>
                                     <td>P=(BVPS／TBVPS／NAVPS)×Target P/B</td>
                                     <td>P=NAVPS×NAV multiple</td>
                                   </tr>
                                   <tr>
                                     <td><b>輸出角色</b></td>
                                     <td>Fair Value 每股合理價</td>
                                     <td>Fair Value（FCFF 先 EV 再橋接）</td>
                                     <td>Fair Value 每股內在價值</td>
                                     <td>Fair Value 區間（Bear／Base／Bull）</td>
                                     <td>Fair Value（控股／資產錨）</td>
                                   </tr>
                                 </tbody>
                               </table>")
  .method_table_rel_en <- HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #34495e; color: white;'>
                                   <tr>
                                     <th>Comparison</th>
                                     <th>Multiples (market multiples)</th>
                                     <th>SOTP (sum of the parts)</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>Anchor</b></td>
                                     <td>EPS / EBIT / EBITDA / FCFF / Revenue / ARR × trading multiple</td>
                                     <td>≥2 segment revenues × segment EV/Sales (+ non-operating assets)</td>
                                   </tr>
                                   <tr>
                                     <td><b>Bridge</b></td>
                                     <td>Enterprise: EV→Equity＝EV+Cash−Debt; P/S: equity side, no debt bridge</td>
                                     <td>Implied EV→Equity＝EV+Cash−Debt (same bridge as DCF)</td>
                                   </tr>
                                   <tr>
                                     <td><b>Growth / years</b></td>
                                     <td>No forecast horizon n; PEG uses growth % as a relative metric</td>
                                     <td>No forecast horizon n</td>
                                   </tr>
                                   <tr>
                                     <td><b>Core formula</b></td>
                                     <td>Implied Price＝EPS×P/E; or Equity＝(Metric×Multiple+Cash−Debt)÷Shares</td>
                                     <td>Implied EV＝Σ(Seg Rev×EV/Sales)+Non-op; Price＝Equity÷Shares</td>
                                   </tr>
                                   <tr>
                                     <td><b>Output role</b></td>
                                     <td>Market-pricing Reference (not Intrinsic Value; never Fair Value secondary)</td>
                                     <td>Meta-Framework companion (often paired with NAV for HoldCos; not equal-weight FV)</td>
                                   </tr>
                                 </tbody>
                               </table>")
  .method_table_rel_zh <- HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #34495e; color: white;'>
                                   <tr>
                                     <th>對照項目</th>
                                     <th>Multiples（市場倍數）</th>
                                     <th>SOTP（分部加總）</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>錨定</b></td>
                                     <td>EPS／EBIT／EBITDA／FCFF／Revenue／ARR × 交易倍數</td>
                                     <td>≥2 部門營收 × 各部門 EV/Sales（＋非營業資產）</td>
                                   </tr>
                                   <tr>
                                     <td><b>橋接</b></td>
                                     <td>Enterprise：EV→Equity＝EV+Cash−Debt；P/S：股權面、無負債橋接</td>
                                     <td>Implied EV→Equity＝EV+Cash−Debt（同 DCF 橋接）</td>
                                   </tr>
                                   <tr>
                                     <td><b>成長／年數</b></td>
                                     <td>不涉及預測年數 n；PEG 用成長％作相對指標</td>
                                     <td>不涉及預測年數 n</td>
                                   </tr>
                                   <tr>
                                     <td><b>核心公式</b></td>
                                     <td>Implied Price＝EPS×P/E；或 Equity＝(Metric×Multiple+Cash−Debt)÷Shares</td>
                                     <td>Implied EV＝Σ(Seg Rev×EV/Sales)+Non-op；Price＝Equity÷Shares</td>
                                   </tr>
                                   <tr>
                                     <td><b>輸出角色</b></td>
                                     <td>市場定價參照 Reference（非 Intrinsic Value；不作 Fair Value 副模型）</td>
                                     <td>綜合型估值框架 Meta-Framework（控股／綜合常配 NAV；不與 FV 等權）</td>
                                   </tr>
                                 </tbody>
                               </table>")
  tagList(
    fluidRow(
      column(
        width = 12,
        class = "ynow-about-section",
        h3(
          class = "ynow-about-section-title",
          tags$b(id = "ynow_method_section_title", "Valuation Methodology")
        ),
        # Do NOT call shiny::withMathJax() here — it hoists a parser-blocking
        # MathJax CDN <script> into <head> for the whole app (Home first paint).
        # Client loads MathJax on demand when .ynow-needs-mathjax is shown.
        tags$div(
          class = "ynow-needs-mathjax",
          `data-ynow-mathjax` = "methodology",
          style = "display:none",
          `aria-hidden` = "true"
        ),
        p(
          class = "ynow-about-section-lead ynow-loc-zh",
          id = "ynow_method_lead_zh",
          tags$b("先分類，再選模型；先推導，再校正；先給區間，再給單點。"),
          " Fair Value 引擎：DCF（FCFF／WACC、FCFE／Ke）、DDM（Gordon／SPM／二階段）、RI、P/B、NAV。",
          " Multiples 為市場定價參照（Reference）；SOTP 為綜合型估值框架（Meta-Framework），可混用絕對／相對透鏡。",
          " 選對路徑與算對數字同等重要；以下說明流程紀律、適用場景與核心公式。"
        ),
        p(
          class = "ynow-about-section-lead ynow-loc-en",
          id = "ynow_method_lead_en",
          tags$b("Classify before models; derive before calibrating; ranges before point targets. "),
          "Fair Value: DCF (FCFF/WACC, FCFE/Ke), DDM (Gordon / SPM / two-stage), RI, P/B, NAV. ",
          "Multiples is a market-pricing Reference; SOTP is a Meta-Framework that can mix absolute and relative lenses. ",
          "Path selection matters as much as the arithmetic—process, fit, and formulas below."
        )
      )
    ),
    fluidRow(
      tabBox(
        title = tags$span(id = "ynow_method_tabbox_title", "Model Selection Guide"),
        width = 12,
        side = "left",

        # Tab: Decision Matrix (bilingual tables)
        tabPanel(
          title = tags$span(id = "ynow_method_tab_matrix", "Decision Matrix"),
          icon = icon("table"),
          tags$p(
            id = "ynow_method_matrix_fv_note",
            class = "help-block",
            style = "margin:0 0 10px 0;",
            "Fair Value engines (Intrinsic Value / Fair Value primaries)."
          ),
          tags$div(
            class = "ynow-loc-en",
            style = "overflow-x: auto; margin-bottom: 18px;",
            .method_table_fv_en
          ),
          tags$div(
            class = "ynow-loc-zh",
            style = "overflow-x: auto; margin-bottom: 18px;",
            .method_table_fv_zh
          ),
          tags$p(
            id = "ynow_method_matrix_rel_note",
            class = "help-block",
            style = "margin:0 0 10px 0;",
            "Multiples = market-pricing Reference; SOTP = Meta-Framework — neither is a Fair Value primary or HFV overlay."
          ),
          tags$div(
            class = "ynow-loc-en",
            style = "overflow-x: auto; margin-bottom: 18px;",
            .method_table_rel_en
          ),
          tags$div(
            class = "ynow-loc-zh",
            style = "overflow-x: auto; margin-bottom: 18px;",
            .method_table_rel_zh
          )
          # Consideration dimensions table lives under Model Selector as a
          # collapsed annotation (see .model_selector_dimensions_annotation_ui).
        ),

        # Tab: DDM
        tabPanel(
          "Dividend Discount Model (DDM)",
          icon = icon("hand-holding-usd"),
          tags$div(
            class = "ynow-loc-en",
            h4(tags$b("Dividend Discount Model (Gordon / SPM / two-stage)")),
            p("DDM values common equity as the present value of future cash dividends. Cash flows are dividends and the discount rate is Ke — a different claim level from FCFF/WACC DCF. This app’s perpetual-growth family includes the Gordon Growth Model (GGM) and the Sum of Perpetuities Method (SPM)."),
            tags$ul(
              tags$li(
                tags$b("Gordon (GGM): "),
                tags$b("$$P_0 = \\frac{D_1}{K_e - g} = \\frac{D_0 \\times (1 + g)}{K_e - g}$$"),
                " Assumes a stable payout ratio with dividends growing at g. Constraint: g < Ke."
              ),
              tags$li(
                tags$b("SPM (sum of perpetuities): "),
                tags$b("$$P_0 = \\frac{E \\times g}{K_e^2} + \\frac{D}{K_e}$$"),
                " Brown & Abraham (2012): discounts a level-dividend perpetuity separately from growth funded by retained earnings; less input-sensitive. When ROE = Ke and g = ROE × retention, it matches forward-dividend GGM. EPS uses the D0 page estimate/latest EPS; D uses this year’s dividend."
              ),
              tags$li(tags$b("Two-stage: "), "Dividends grow at g₁ for n₁ years, then Gordon terminal value at perpetual g₂. Constraint: g₂ < Ke."),
              tags$li(tags$b("$$D_t = D_0(1+g_1)^t,\\quad TV = \\frac{D_{n_1}(1+g_2)}{K_e-g_2},\\quad P_0 = \\sum_{t=1}^{n_1}\\frac{D_t}{(1+K_e)^t} + \\frac{TV}{(1+K_e)^{n_1}}$$"))
            ),
            p("Dividend growth g (two-stage g₂; SPM earnings g) can sync with the central terminal SGR or be overridden on the DDM page. A fundamental check is $$g = ROE \\times Retention\\ Ratio$$ (or ROA × RR), but do not force-equate it to FCFF terminal g. Prefer GGM vs SPM after judging whether payout looks closer to a fixed payout ratio or a fixed dollar dividend.")
          ),
          tags$div(
            class = "ynow-loc-zh",
            h4(tags$b("股利折現模型（Gordon／SPM／二階段）")),
            p("DDM 將普通股價值視為未來現金股利的現值。現金流是股利、折現率是 Ke，與以 FCFF／WACC 為核心的 DCF 屬不同層級。本 App 的永續成長家族含 Gordon Growth Model (GGM) 與 Sum of Perpetuities Method (SPM)。"),
            tags$ul(
              tags$li(
                tags$b("Gordon (GGM)："),
                tags$b("$$P_0 = \\frac{D_1}{K_e - g} = \\frac{D_0 \\times (1 + g)}{K_e - g}$$"),
                " 假設現金發放率維持固定比例，股利隨盈餘以 g 成長。約束：g < Ke。"
              ),
              tags$li(
                tags$b("SPM（永續和）："),
                tags$b("$$P_0 = \\frac{E \\times g}{K_e^2} + \\frac{D}{K_e}$$"),
                " Brown & Abraham (2012)：將「定額股利永續」與「保留盈餘創造之成長」分開折現；對輸入較不敏感。當 ROE = Ke 且 g = ROE × 保留率時，與前向股利版 GGM 等價。EPS 取 D0 分頁之預估／最新 EPS；D 取今年股利。"
              ),
              tags$li(tags$b("二階段："), "前 n₁ 年股利以 g₁ 成長，之後以永續 g₂ 做 Gordon 終值。約束：g₂ < Ke。"),
              tags$li(tags$b("$$D_t = D_0(1+g_1)^t,\\quad TV = \\frac{D_{n_1}(1+g_2)}{K_e-g_2},\\quad P_0 = \\sum_{t=1}^{n_1}\\frac{D_t}{(1+K_e)^t} + \\frac{TV}{(1+K_e)^{n_1}}$$"))
            ),
            p("股利成長率 g（二階段的 g₂；SPM 的盈餘成長 g）可與中央終值 SGR 同步，亦可在 DDM 分頁單獨覆寫。基本面法可參考 $$g = ROE \\times Retention\\ Ratio$$（或 ROA × RR），但不宜與 FCFF 終值 g 強制畫上等號。選擇 GGM 或 SPM 時，宜先觀察公司股利政策較接近「固定配息率」或「固定股利金額」。")
          )
        ),

        # Tab: DCF
        tabPanel(
          "Discounted Cash Flow (DCF)",
          icon = icon("money-bill-wave"),
          tags$div(
            class = "ynow-loc-en",
            h4(tags$b("Free Cash Flow Discount Model (FCFF / FCFE)")),
            p("DCF focuses on cash generation. Default path discounts FCFF at WACC to enterprise value (EV), then bridges +Cash −Debt to equity before dividing by shares. Historical cash flows rebuild as CFO + after-tax interest − |CapEx| (Yahoo Free Cash Flow is after-tax and closer to FCFE — do not pair it with WACC). FCFE mode converts FCFF to equity cash flows and discounts at Ke without a net-cash/debt bridge. This app’s “Gordon” mode is an explicit forecast plus Gordon terminal value, not a one-period EV = FCF₁/(WACC−g). If terminal FCFF is negative, skip the perpetual terminal and prefer RI. CapEx spike smoothing is an optional engineering heuristic (adjustable multiple threshold / averaging years)."),
            tags$ul(
              tags$li(tags$b("$$FCFF = CFO + Interest\\times(1-T) - CapEx$$")),
              tags$li(tags$b("$$FCFF = NOPAT + D\\&A - \\Delta NWC - CapEx$$")),
              tags$li(tags$b("$$FCFE = FCFF - Interest\\times(1-T) + Net\\ Borrowing$$")),
              tags$li(tags$b("FCFF: "), tags$b("$$Enterprise\\ Value = \\sum \\frac{FCFF_t}{(1+WACC)^t} + \\frac{TV}{(1+WACC)^n}$$")),
              tags$li(tags$b("FCFE: "), tags$b("$$Equity = \\sum \\frac{FCFE_t}{(1+K_e)^t} + \\frac{TV}{(1+K_e)^n}$$")),
              tags$li(tags$b("$$Terminal\\ Value = \\frac{CF_n \\times (1 + g)}{r - g}$$"))
            ),
            p("The Live forecast table anchors on NOPAT (EBIT×(1−T)), not NI. FCFE net borrowing uses a constant-leverage approximation: each year’s net borrowing = g × beginning debt. Two-stage mode converges terminal growth to SGR after the high-growth phase; FCFF requires g < WACC and FCFE requires g < Ke.")
          ),
          tags$div(
            class = "ynow-loc-zh",
            h4(tags$b("自由現金流折現模型 (FCFF／FCFE)")),
            p("DCF 關注企業造血能力。預設以 FCFF 用 WACC 折現得到企業價值（EV），再加現金、減負債橋接至股權價值後才除以流通股數。歷史現金流以 CFO＋稅後利息−|CapEx| 重建（Yahoo Free Cash Flow 為稅後、近 FCFE，不可直接配 WACC）。亦可切換 FCFE：將 FCFF 轉成股權現金流後以 Ke 折現，不再加減淨現金／負債。本 app 的「Gordon」模式為明確預測期加上 Gordon 終值，而非單期 EV = FCF₁/(WACC−g)。若末期 FCFF 為負則略過永續終值，並建議改用 RI。CapEx 暴衝平滑為可選工程啟發式（可調倍數閾值／均值年數）。"),
            tags$ul(
              tags$li(tags$b("$$FCFF = CFO + Interest\\times(1-T) - CapEx$$")),
              tags$li(tags$b("$$FCFF = NOPAT + D\\&A - \\Delta NWC - CapEx$$")),
              tags$li(tags$b("$$FCFE = FCFF - Interest\\times(1-T) + Net\\ Borrowing$$")),
              tags$li(tags$b("FCFF："), tags$b("$$Enterprise\\ Value = \\sum \\frac{FCFF_t}{(1+WACC)^t} + \\frac{TV}{(1+WACC)^n}$$")),
              tags$li(tags$b("FCFE："), tags$b("$$Equity = \\sum \\frac{FCFE_t}{(1+K_e)^t} + \\frac{TV}{(1+K_e)^n}$$")),
              tags$li(tags$b("$$Terminal\\ Value = \\frac{CF_n \\times (1 + g)}{r - g}$$"))
            ),
            p("Live 預測表以 NOPAT（EBIT×(1−T)）為錨，不是淨利 NI。FCFE 的淨舉債採固定槓桿近似：各年淨舉債＝g×期初負債。兩階段模式在高速成長期後將終值成長率收斂至 SGR；FCFF 約束 g < WACC，FCFE 約束 g < Ke。")
          )
        ),

        # Tab: RI
        tabPanel(
          "Residual Income (RI)",
          icon = icon("gem"),
          tags$div(
            class = "ynow-loc-en",
            h4(tags$b("Residual Income Model")),
            p("RI starts from book equity and discounts earnings above the cost of equity. Useful when FCF is negative but book value and ROE remain informative; the discount rate is Ke (same equity claim level as DDM)."),
            tags$ul(
              tags$li(tags$b("$$RI_t = (ROE_t - K_e) \\times B_{t-1}$$")),
              tags$li(tags$b("$$B_t = B_{t-1} + NI_t \\times (1 - Payout)$$")),
              tags$li(tags$b("$$V_0 = B_0 + \\sum_{t=1}^{n} \\frac{RI_t}{(1+K_e)^t} + \\frac{TV_{RI}}{(1+K_e)^n}$$")),
              tags$li(tags$b("$$TV_{RI} = \\frac{RI_n \\times (1 + g)}{K_e - g}$$"))
            ),
            p("This app supports fixed ROE, linear fade, or industry ROE; terminal g must satisfy g < Ke. When ROE < Ke, residual income is negative (value destruction).")
          ),
          tags$div(
            class = "ynow-loc-zh",
            h4(tags$b("剩餘收益模型 (Residual Income)")),
            p("RI 以帳面淨值為起點，將「超過股權成本的盈餘」折現加總。適合 FCF 為負、但淨值與 ROE 具參考性的企業；折現率使用 Ke（與 DDM 同屬股權層級）。"),
            tags$ul(
              tags$li(tags$b("$$RI_t = (ROE_t - K_e) \\times B_{t-1}$$")),
              tags$li(tags$b("$$B_t = B_{t-1} + NI_t \\times (1 - Payout)$$")),
              tags$li(tags$b("$$V_0 = B_0 + \\sum_{t=1}^{n} \\frac{RI_t}{(1+K_e)^t} + \\frac{TV_{RI}}{(1+K_e)^n}$$")),
              tags$li(tags$b("$$TV_{RI} = \\frac{RI_n \\times (1 + g)}{K_e - g}$$"))
            ),
            p("本 app 可設定固定 ROE、線性淡化或產業 ROE；終值成長 g 須滿足 g < Ke。當 ROE < Ke 時，剩餘收益為負，代表價值減損。")
          )
        ),

        # Tab: P/B
        tabPanel(
          "Price-to-Book (P/B)",
          icon = icon("landmark"),
          tags$div(
            class = "ynow-loc-en",
            h4(tags$b("Price-to-Book: Justified Fair Value vs relative multiples")),
            p("Multiply BVPS, tangible book, or HoldCo NAVPS by a target P/B. The Target source decides the method: Justified P/B from ROE, Ke, and SGR/g is Book / Asset Fair Value; industry or history multiples are Relative Implied Price (Multiples · Book). Pure book NAV without a multiple uses the separate NAV model — same asset/book family, different route."),
            tags$ul(
              tags$li(tags$b("$$BVPS = \\frac{Common\\ Equity}{Shares}$$")),
              tags$li(tags$b("$$TBVPS = \\frac{Common\\ Equity - Goodwill - Intangibles}{Shares}$$")),
              tags$li(tags$b("$$P = (BVPS\\ /\\ TBVPS\\ /\\ NAVPS) \\times Target\\ P/B$$")),
              tags$li(tags$b("Justified\\ P/B \\approx \\frac{ROE - g}{K_e - g}"))
            ),
            p("For dual-class / ADR cases where quoted shares ≠ statement shares, share count is auto-implied (market cap ÷ price), same as DCF / RI / backtests.")
          ),
          tags$div(
            class = "ynow-loc-zh",
            h4(tags$b("本淨比：Justified Fair Value 與相對倍數")),
            p("以每股帳面淨值、有形淨值或控股 NAVPS 乘上目標本淨比。目標倍數來源決定方法性質：由 ROE、Ke、SGR／g 推導的 Justified P/B 屬帳面／資產型 Fair Value；產業或歷史交易倍數屬相對估值 Implied Price（Multiples · Book）。純帳面 NAV（無倍數）請用獨立「NAV」模型——兩者同屬資產／帳面家族，但路徑分開。"),
            tags$ul(
              tags$li(tags$b("$$BVPS = \\frac{Common\\ Equity}{Shares}$$")),
              tags$li(tags$b("$$TBVPS = \\frac{Common\\ Equity - Goodwill - Intangibles}{Shares}$$")),
              tags$li(tags$b("$$P = (BVPS\\ /\\ TBVPS\\ /\\ NAVPS) \\times Target\\ P/B$$")),
              tags$li(tags$b("Justified\\ P/B \\approx \\frac{ROE - g}{K_e - g}"))
            ),
            p("雙重股權／ADR 等「報價股數 ≠ 財報股數」時，與 DCF／RI／回測相同，一律自動約當股數（市值÷股價）。")
          )
        ),
        tabPanel(
          "Net Asset Value (NAV)",
          icon = icon("sitemap"),
          tags$div(
            class = "ynow-loc-en",
            h4(tags$b("Pure NAV (book HoldCo net assets)")),
            p("Values the stock of assets today: Fair Value = NAVPS × discount/premium multiple. HoldCo discount applies only to identified investment accounts; with none, NAV = book equity. This is a book decomposition — not market-method segment SOTP — and does not need Justified/SGR."),
            tags$ul(
              tags$li(tags$b("$$NAV = Equity - Holdco\\ Discount \\times Identified\\ Investments$$")),
              tags$li(tags$b("$$P = NAVPS \\times NAV\\ Multiple$$"))
            )
          ),
          tags$div(
            class = "ynow-loc-zh",
            h4(tags$b("純 NAV（帳面控股淨資產）")),
            p("評估「現在家底（存量）」：合理價 = NAVPS × 折價／溢價倍數。控股折價只套用在已辨識投資科目；無投資科目時 NAV＝帳面權益。此為帳面拆解，不是市場法分部 SOTP，亦不需要 Justified／SGR。"),
            tags$ul(
              tags$li(tags$b("$$NAV = Equity - Holdco\\ Discount \\times Identified\\ Investments$$")),
              tags$li(tags$b("$$P = NAVPS \\times NAV\\ Multiple$$"))
            )
          )
        ),

        tabPanel(
          "Multiples",
          icon = icon("percentage"),
          tags$div(
            class = "ynow-loc-en",
            h4(tags$b("Market multiples (Implied Price)")),
            p("Relative valuation splits into Equity and Enterprise claim families (not Intrinsic Value / Fair Value primaries). Do not equal-weight with DCF / DDM / RI / P/B / NAV, and do not use as HFV overlays."),
            tags$ul(
              tags$li(tags$b("Equity: "), tags$b("$$Implied\\ Price = EPS \\times P/E$$"), "; ", tags$b("$$Implied\\ Equity = Revenue \\times P/S$$"), "; PEG = P/E ÷ g(%) (relative metric)."),
              tags$li(tags$b("Enterprise: "), tags$b("$$Implied\\ EV = Metric \\times Multiple$$"), "; ", tags$b("$$Equity = EV + Cash - Debt$$"), "; Price = Equity ÷ Shares (shared bridge with SOTP; EV/FCF uses FCFF)."),
              tags$li(tags$b("SOTP (sidebar): "), "Enterprise-structural — sum segment revenue × EV/Sales, then the same Cash−Debt bridge.")
            ),
            p("Shared assumptions: Shares; Enterprise / SOTP also share Cash, Debt, and default EV/Sales tiers. Unique assumptions are in the Multiples Overview parameter matrix.")
          ),
          tags$div(
            class = "ynow-loc-zh",
            h4(tags$b("市場倍數法（Implied Price）")),
            p("相對估值依資本請求權分為 Equity／Enterprise 兩族（非 Intrinsic Value／Fair Value 主模型）。不可與 DCF／DDM／RI／P/B／NAV 等權平均，亦不可作為 HFV 疊加。"),
            tags$ul(
              tags$li(tags$b("Equity："), tags$b("$$Implied\\ Price = EPS \\times P/E$$"), "；", tags$b("$$Implied\\ Equity = Revenue \\times P/S$$"), "；PEG = P/E ÷ g(%)（相對指標）。"),
              tags$li(tags$b("Enterprise："), tags$b("$$Implied\\ EV = Metric \\times Multiple$$"), "；", tags$b("$$Equity = EV + Cash - Debt$$"), "；Price = Equity ÷ Shares（與 SOTP 共用橋接；EV/FCF 用 FCFF）。"),
              tags$li(tags$b("SOTP（側欄）："), "Enterprise-structural — 部門營收 × EV/Sales 加總後再用同一 Cash−Debt 橋接。")
            ),
            p("共用假設：Shares；Enterprise／SOTP 另共用 Cash、Debt、EV/Sales 預設層級。獨特假設見 Multiples Overview 參數矩陣。")
          )
        ),

        tabPanel(
          "SOTP",
          icon = icon("puzzle-piece"),
          tags$div(
            class = "ynow-loc-en",
            h4(tags$b("SOTP (Meta-Framework)")),
            p("Sum-of-the-Parts is a Meta-Framework: split the firm into segments and value each with the best lens (DCF, EV/EBITDA, EV/Sales, NAV), then sum EV, subtract corporate overhead and net debt. This app’s engine implements the revenue × EV/Sales structural path (≥2 segments) with the shared Cash−Debt×Shares bridge — often paired with NAV for HoldCos/conglomerates; not Equity P/E／P/S, and not segment-EBIT SOTP."),
            tags$ul(
              tags$li(tags$b("$$Segment\\ EV = Segment\\ Revenue \\times EV/Sales$$")),
              tags$li(tags$b("$$Implied\\ EV = \\sum Segment\\ EV + Non\\text{-}operating$$")),
              tags$li(tags$b("$$Equity = Implied\\ EV + Cash - Debt$$"), "; ", tags$b("$$Price = Equity \\div Shares$$"))
            ),
            p("Shared: Cash / Debt / Shares, default EV/Sales. Unique: segment revenues, segment EV/Sales, non-operating assets.")
          ),
          tags$div(
            class = "ynow-loc-zh",
            h4(tags$b("分部加總 SOTP（綜合型估值框架／Meta-Framework）")),
            p("SOTP 是綜合型估值框架：把公司拆成事業板塊，各板塊可混用 DCF、EV/EBITDA、EV/Sales、NAV 等最適模型，再加總 EV、扣除集團 Overhead 與淨負債。本 App 引擎實作為部門營收 × EV/Sales 結構路徑（需 ≥2 部門）並共用 Cash−Debt×Shares 橋接——控股／綜合常與 NAV 搭配；非 Equity 的 P/E／P/S，亦非部門 EBIT SOTP。"),
            tags$ul(
              tags$li(tags$b("$$Segment\\ EV = Segment\\ Revenue \\times EV/Sales$$")),
              tags$li(tags$b("$$Implied\\ EV = \\sum Segment\\ EV + Non\\text{-}operating$$")),
              tags$li(tags$b("$$Equity = Implied\\ EV + Cash - Debt$$"), "；", tags$b("$$Price = Equity \\div Shares$$"))
            ),
            p("共用：Cash／Debt／Shares、Default EV/Sales。獨特：部門營收、各段 EV/Sales、Non-operating assets。")
          )
        )
      )
    )
  )
}

.shared_bridge_box <- function() {
  tags$div(
    `data-ynow-lock` = "section:shared_bridge",
  box(
    title = tagList(
      icon("link"),
      tags$span(id = "ynow_shared_bridge_title", "Cash、Debt、股數與營收（各模型共用）")
    ),
    width = 12, status = "primary", solidHeader = TRUE,
    tags$p(
      id = "ynow_shared_bridge_help",
      class = "help-block",
      paste0(
        "Cash、Total Debt、股數與總營收請只在這裡填一次。",
        "需要這些數字的模型——DCF、WACC 權重、Enterprise Multiples、P/S、SOTP——都會讀這裡。",
        "終值 g 與 Ke 仍在 SGR 與 CAPM 調整。",
        "SOTP 部門營收、股利 g、近端營收成長各自設定。"
      )
    ),
    fluidRow(
      column(3, numericInput("bridge_cash", "Cash", value = NA, step = 1)),
      column(3, numericInput("bridge_debt", "Total Debt", value = NA, step = 1)),
      column(3, numericInput("bridge_shares", "股數", value = NA, step = 1)),
      column(3, numericInput("bridge_revenue", "總營收", value = NA, step = 1))
    ),
    tags$button(
      id = "bridge_reload",
      type = "button",
      class = "btn btn-default action-button btn-sm",
      icon("sync"),
      tags$span(id = "ynow_shared_bridge_reset", "從財報帶入")
    )
  )
  )
}

.dcf_core_params_box <- function() {
  # Chrome／欄寬對齊下方 BETA tabBox：左側單一頁籤 + 右側大標英文；同為 col-sm-12
  tags$div(
    `data-ynow-lock` = "section:sgr_box",
    tabBox(
      title = "SUSTAINABLE GROWTH RATE",
      width = 12,
      tabPanel(
        "SGR",
        icon = icon("seedling"),
        # 與 BETA Overview 同款 valueBox 列：各 50% 並排
        fluidRow(
          tags$div(`data-ynow-lock` = "output:sgr_pct", valueBoxOutput("vbx_sgr_pct", width = 6)),
          valueBoxOutput("vbx_session_g", width = 6)
        ),
        fluidRow(
          column(
            width = 6,
            tags$h5(tags$b(id = "ynow_sgr_method_title", "終值永續成長率 (SGR) 評價方法")),
            selectInput(
              "perpetual_g_method",
              NULL,
              choices = c(
                "總體經濟錨定（Macro）" = "macro",
                "基本面公式（Fundamental／SGR）" = "fundamental",
                "產業生命週期（Lifecycle）" = "lifecycle"
              ),
              selected = APP_DEFAULTS$perpetual_g_method
            )
          ),
          column(
            width = 6,
            tags$h5(tags$b(id = "ynow_lifecycle_stage_title", "生命週期檔位")),
            selectInput(
              "lifecycle_stage",
              NULL,
              choices = c(
                "自動偵測" = "auto",
                "High Growth" = "HIGH_GROWTH",
                "Growth-to-Mature" = "GROWTH_TO_MATURE",
                "Mature Growth" = "MATURE_GROWTH",
                "Mature Stable" = "MATURE_STABLE",
                "Declining / Finite Life" = "DECLINING_OR_FINITE_LIFE",
                "Regulated Utility" = "REGULATED_UTILITY",
                "Financial Institution" = "FINANCIAL_INSTITUTION"
              ),
              selected = APP_DEFAULTS$lifecycle_stage
            ),
            helpText(
              id = "ynow_lifecycle_stage_help",
              "Auto-detect 依多因子評分；手動選擇不會被覆寫。選項不含固定終值 g。"
            )
          )
        ),
        helpText(
          id = "ynow_sgr_method_help",
          "Macro：採用即時擷取的市場 10 年期公債 Rf（美股 Yahoo ^TNX；台股櫃買 TPEx 公債殖利率曲線 10 年期；失敗則最近成功值，再失敗才工程 fallback 並標明）。",
          "Fundamental：Retention×ROE（僅適合成熟穩健企業）。",
          "Lifecycle：以經濟錨定估計終值 g，生命週期檔位不寫入固定百分比。"
        ),
        uiOutput("txt_perpetual_g_method_suggest"),
        uiOutput("txt_lifecycle_classification"),
        tags$h5(tags$b("估計依據")),
        uiOutput("txt_perpetual_g_reason"),
        uiOutput("txt_sgr_identity"),
        tags$hr(style = "margin: 12px 0;"),
        numericInput(
          "sgr",
          "自訂 SGR (%)",
          value = APP_DEFAULTS$sgr
        )
      )
    )
  )
}

.dcf_two_stage_params_box <- function() {
  tags$div(
    `data-ynow-lock` = "section:dcf_two_stage",
  conditionalPanel(
    condition = "input.dcf_mode == 'two_stage'",
    box(
      title = tagList(icon("layer-group"), ui_i18n("dcf_two_stage_title")),
      width = 12, status = "warning", solidHeader = TRUE,
      tags$p(style = "margin: 0 0 6px 0; font-size: 12.5px; color: #555;", tags$b(ui_i18n("dcf_stage1_heading"))),
      numericInput("yr_stage1", ui_i18n("dcf_yr_stage1_label"), value = APP_DEFAULTS$yr_stage1),
      numericInput("g_stage1", ui_i18n("dcf_g1_label"), value = APP_DEFAULTS$g_stage1),
      helpText(
        id = "ynow_g_stage1_help",
        ui_str("g_stage1_help", "en")
      ),
      conditionalPanel(
        condition = "input.dcf_claim != 'fcfe'",
        numericInput("wacc_stage1", ui_i18n("dcf_wacc1_label"), value = APP_DEFAULTS$wacc_stage1, step = 0.01)
      ),
      tags$p(style = "margin: 10px 0 6px 0; font-size: 12.5px; color: #555;", tags$b(ui_i18n("dcf_stage2_heading"))),
      helpText(ui_i18n("dcf_stage2_help")),
      conditionalPanel(
        condition = "input.dcf_claim != 'fcfe'",
        numericInput("wacc_stage2", ui_i18n("dcf_wacc2_label"), value = APP_DEFAULTS$wacc_stage2, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.dcf_claim == 'fcfe'",
        helpText(ui_i18n("dcf_fcfe_stage_help"))
      )
    )
  )
  )
}

.ddm_two_stage_params_box <- function() {
  tags$div(
    `data-ynow-lock` = "section:ddm_two_stage",
    conditionalPanel(
      condition = "input['mod_ddm-ddm_mode'] == 'two_stage'",
      box(
        title = tagList(icon("layer-group"), ui_i18n("ddm_two_stage_title")),
        width = 12, status = "warning", solidHeader = TRUE,
        tags$p(style = "margin: 0 0 6px 0; font-size: 12.5px; color: #555;", tags$b(ui_i18n("ddm_stage1_heading"))),
        numericInput(
          "mod_ddm-yr_stage1", ui_i18n("ddm_yr_stage1_label"),
          value = APP_DEFAULTS$ddm_yr_stage1, min = 1, max = 30, step = 1
        ),
        numericInput(
          "mod_ddm-g_stage1", ui_i18n("ddm_g1_label"),
          value = APP_DEFAULTS$ddm_g_stage1, step = 0.1
        ),
        tags$p(style = "margin: 10px 0 6px 0; font-size: 12.5px; color: #555;", tags$b(ui_i18n("ddm_stage2_heading"))),
        helpText(id = "ynow_ddm_two_stage_help", ui_str("ddm_two_stage_help", "en"))
      )
    )
  )
}

.ddm_formula_banner <- function() {
  tagList(
    conditionalPanel(
      condition = "input['mod_ddm-ddm_mode'] == 'gordon' || input['mod_ddm-ddm_mode'] == null || input['mod_ddm-ddm_mode'] == ''",
      div(
        id = "ynow_ddm_formula_gordon",
        "V₀ = D₁ / (r − g)　｜　D₁ = D₀ × (1 + g)　｜　r = Ke (CAPM)",
        style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
      )
    ),
    conditionalPanel(
      condition = "input['mod_ddm-ddm_mode'] == 'spm'",
      div(
        id = "ynow_ddm_formula_spm",
        "V₀ = (E × g) / r² + D / r　｜　r = Ke (CAPM)　｜　SPM",
        style = "font-size: 16px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
      )
    ),
    conditionalPanel(
      condition = "input['mod_ddm-ddm_mode'] == 'two_stage'",
      div(
        id = "ynow_ddm_formula_two_stage",
        "V₀ = Σ Dₜ / (1+r)ᵗ + Pₙ / (1+r)ⁿ　｜　Pₙ = Dₙ × (1+g₂) / (r − g₂)",
        style = "font-size: 16px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
      )
    )
  )
}

#' Shared bottom block: formula-param contribution / relative ±1% elasticity.
.model_param_sensitivity_box <- function(title, table_id) {
  # Core explainability: keep open by default (not an appendix drawer).
  fluidRow(
    box(
      title = tagList(icon("percentage"), title),
      width = 12, status = "info", solidHeader = TRUE,
      collapsible = TRUE, collapsed = FALSE,
      tags$div(
        style = "overflow-x:auto;",
        tableOutput(table_id)
      ),
      tags$div(
        class = "help-block ynow-loc-en",
        style = "margin:12px 0 0 0; font-size:12px; line-height:1.55; color:#737373;",
        tags$p(style = "margin:0 0 4px 0; font-weight:600; color:#555;", "How to check"),
        tags$ul(
          style = "margin:0; padding-left:1.25em;",
          tags$li("|ε| is evaluated on the formula inputs (WACC / Ke, terminal g, n, We / Wd, payout, FCF mix). It does not use the share price, share count, or cash / debt scale, so two names with the same formula inputs share the same |ε| ranking."),
          tags$li("The elasticity of terminal g and the discount rate depends on (r − g). Changing either input changes |ε|."),
          tags$li("One input moves at a time (relative ±1%). Integer years n use ±1 year, scaled to a 1% equivalent elasticity."),
          tags$li("Value Δ% is the formula-value change when that input moves 1%. |ε| is the average of the two absolute sides."),
          tags$li("Level inputs (FCFF / FCFE / D0 / B0 / BVPS / target P/B) have |ε| ≈ 1 when the formula is linear in them.")
        )
      ),
      tags$div(
        class = "help-block ynow-loc-zh",
        style = "margin:12px 0 0 0; font-size:12px; line-height:1.55; color:#737373;",
        tags$p(style = "margin:0 0 4px 0; font-weight:600; color:#555;", "複核方式"),
        tags$ul(
          style = "margin:0; padding-left:1.25em;",
          tags$li("「｜ε｜」由估值公式在目前模型參數（WACC／Ke、終值 g、n、We／Wd、配息、FCF 佔比等）上評估，與個股價格、股數、現金／負債規模無關；兩檔股票只要公式參數相同，｜ε｜與排序即相同。"),
          tags$li("終值 g 與折現率的彈性取決於 (r−g)，故改其中任一參數才會改｜ε｜。"),
          tags$li("一次只變動一個參數（相對 ±1%）；整數年數 n 以 ±1 年換算成 1% 等價彈性。"),
          tags$li("「估值Δ%」≈ 該參數變動 1% 時公式價值的變動幅度；「｜ε｜」= 兩側 |Δ估值%| 平均。"),
          tags$li("水準參數（FCFF／FCFE／D0／B0／BVPS／目標 P/B）在公式為線性時｜ε｜≈1。")
        )
      )
    )
  )
}

# ---- Lazy page bodies (mounted once via uiOutput hosts) ----

#' Lazy page body for tab `macro_market` (mounted once per session).
#' @param locale first-paint locale (`en` / `zh-TW`); language switches still update via applyUiLocale.
#' @param market_mode Active market so industry choices bake US ≠ TW at mount.
.ynow_page_ui_macro_market <- function(locale = NULL, market_mode = NULL) {
  loc <- tryCatch({
    if (!is.null(locale) && nzchar(as.character(locale)[1])) {
      if (exists("normalize_ui_locale", mode = "function")) {
        normalize_ui_locale(locale)
      } else {
        as.character(locale)[1]
      }
    } else {
      "en"
    }
  }, error = function(e) "en")
  mm <- tryCatch({
    if (!is.null(market_mode) && nzchar(as.character(market_mode)[1])) {
      if (exists("normalize_market_mode", mode = "function")) {
        normalize_market_mode(market_mode)
      } else {
        toupper(as.character(market_mode)[1])
      }
    } else if (exists("get_market_mode", mode = "function")) {
      get_market_mode()
    } else {
      "US"
    }
  }, error = function(e) "US")
  tagList(
    macro_market_ui("macro", locale = loc, market_mode = mm)
  )
}

#' Lazy page body for tab `bluechip` (mounted once per session).
#' @param market_mode Active market so concept-group choices bake US ≠ TW at mount.
#' @param locale UI locale for labels / placeholder.
.ynow_page_ui_bluechip <- function(market_mode = NULL, locale = NULL) {
  .bc_mm <- tryCatch({
    if (!is.null(market_mode) && nzchar(as.character(market_mode)[1])) {
      if (exists("normalize_market_mode", mode = "function")) {
        normalize_market_mode(market_mode)
      } else {
        toupper(as.character(market_mode)[1])
      }
    } else if (exists("get_market_mode", mode = "function")) {
      get_market_mode()
    } else {
      "US"
    }
  }, error = function(e) "US")
  if (!.bc_mm %in% c("US", "TW")) .bc_mm <- "US"
  .bc_loc <- tryCatch({
    if (!is.null(locale) && nzchar(as.character(locale)[1])) {
      if (exists("normalize_ui_locale", mode = "function")) {
        normalize_ui_locale(locale)
      } else {
        as.character(locale)[1]
      }
    } else {
      "zh-TW"
    }
  }, error = function(e) "zh-TW")

  tagList(
  # Industry mcap-weighted upside board (auto; above Candidate truncate)
          fluidRow(
            column(
              width = 12,
              tags$div(
                class = "ynow-lab-im-ind-mcap-block",
                style = "margin: 0 0 16px 0; padding: 12px 14px; border: 1px solid #e5e5e5; border-radius: 4px; background: #fafafa;",
                tags$div(
                  class = "ynow-lab-im-ind-mcap-title-row",
                  tags$div(
                    class = "ynow-lab-im-ind-mcap-title-left",
                    tags$h4(
                      id = "ynow_lab_im_ind_mcap_title",
                      style = "margin:0; font-size:15px; font-weight:600;",
                      "產業市值加權漲幅"
                    ),
                    tags$span(
                      id = "ynow_lab_im_ind_mcap_badge",
                      style = "font-size:11px; color:#888;",
                      "預設排名"
                    )
                  ),
                  actionButton(
                    "lab_im_refresh_ind_mcap",
                    tags$span(id = "ynow_lab_im_refresh_ind_mcap_label", "更新排名"),
                    icon = icon("sync"),
                    class = "btn-default btn-sm ynow-lab-im-refresh-ind-mcap",
                    title = paste0(
                      "重新評估並計算全部產業鏈的市值加權年化估值漲幅排名；",
                      "完成後寫入預設快照，供之後開啟時直接顯示。"
                    )
                  )
                ),
                tags$div(
                  id = "ynow_lab_im_ind_mcap_help",
                  style = "color:#888; font-size:12px; line-height:1.45; margin:0 0 8px 0;",
                  paste0(
                    "顯示全部產業鏈（半導體／科技／金融…），排序用獨立抽樣評估樣本的 ",
                    "Σ(市值×年化估值漲幅)/Σ(市值)；總市值／檔數＝該產業鏈全宇宙個股（非搜尋 Top-N）。",
                    "排除未對應。與候選截斷、概念股、宇宙檔數 N、產業／模型篩選、品質門檻無關。",
                    "開啟績優股排行榜會在背景自動重算；按「更新排名」可手動重算並寫入預設快照。"
                  )
                ),
                uiOutput("lab_im_ind_mcap_status"),
                tags$div(
                  class = "ynow-lab-im-ind-mcap-treemap-wrap",
                  style = "margin: 0 0 10px 0; width: 100%;",
                  tags$div(
                    id = "ynow_lab_im_ind_mcap_treemap_legend",
                    class = "ynow-lab-im-ind-mcap-treemap-legend",
                    style = "color:#888; font-size:11.5px; line-height:1.4; margin:0 0 6px 0;",
                    "面積＝產業鏈總市值；顏色＝市值加權年化估值漲幅（美股漲綠／台股漲紅）。納入全部產業鏈。"
                  ),
                  plotlyOutput("lab_im_ind_mcap_treemap", height = "320px")
                ),
                tags$div(
                  style = "overflow-x:auto;",
                  DT::dataTableOutput("lab_im_ind_mcap_table")
                )
              )
            )
          ),
  # Shared pool controls above BLUE CHIP (semantic: truncate rule → then take N)
          # Concept groups appear beside truncate when mode is concept.
          fluidRow(
            column(
              width = 12,
              class = "ynow-lab-im-pool-controls",
              style = "margin: 0 0 14px 0;",
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    "lab_im_pool_rank",
                    tags$span(id = "ynow_lab_im_pool_rank_label", "候選截斷邏輯"),
                    choices = lab_im_pool_rank_choices(),
                    selected = APP_DEFAULTS$lab_im_pool_rank,
                    width = "100%"
                  )
                ),
                conditionalPanel(
                  condition = "input.lab_im_pool_rank == 'concept'",
                  class = "col-sm-4",
                  selectizeInput(
                    "lab_im_concepts",
                    tags$span(id = "ynow_lab_im_concepts_label", "概念股群"),
                    choices = lab_concept_group_choices(.bc_mm, .bc_loc),
                    selected = APP_DEFAULTS$lab_im_concepts,
                    multiple = TRUE,
                    options = list(
                      placeholder = tryCatch(
                        ui_str("lab_im_concepts_placeholder", .bc_loc),
                        error = function(e) "依目前市場（美股／台股）選擇一或多個概念股群…"
                      ),
                      plugins = list("remove_button")
                    ),
                    width = "100%"
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    "lab_im_max_n",
                    tags$span(id = "ynow_lab_im_max_n_label", "宇宙檔數（N）"),
                    choices = lab_im_max_n_select_choices(),
                    selected = APP_DEFAULTS$lab_im_max_n,
                    width = "100%"
                  ),
                  conditionalPanel(
                    condition = "input.lab_im_max_n == 'custom'",
                    numericInput(
                      "lab_im_max_n_custom",
                      tags$span(id = "ynow_lab_im_max_n_custom_label", "自訂檔數"),
                      value = 25,
                      min = 1,
                      max = 500,
                      step = 1,
                      width = "100%"
                    )
                  )
                )
              )
            )
          ),
          tabBox(
            title = "BLUE CHIP",
            id = "bluechip_im_report",
            width = "auto",

            tabPanel(
              title = "排行",
              value = "im_filters",
              icon = icon("trophy"),
              uiOutput("lab_im_bluechip_blurb"),
              tags$hr(),
              fluidRow(
                column(
                  width = 12,
                  tags$div(
                    class = "ynow-lab-im-lb-scope",
                    style = "margin:0 0 10px 0;",
                    radioButtons(
                      "lab_im_lb_mode",
                      tags$span(id = "ynow_lab_im_lb_mode_label", "排行視角"),
                      choiceNames = list(
                        tags$span(id = "ynow_lab_im_lb_mode_overall", "整體前十名"),
                        tags$span(id = "ynow_lab_im_lb_mode_by_ind", "選定產業前十名"),
                        tags$span(id = "ynow_lab_im_lb_mode_undervalued", "價值低估前十名")
                      ),
                      choiceValues = list("overall", "by_industry", "undervalued"),
                      selected = {
                        m0 <- as.character(APP_DEFAULTS$lab_im_lb_mode %||% "overall")[1]
                        if (m0 %in% c("overall", "by_industry", "undervalued")) m0 else "overall"
                      },
                      inline = TRUE
                    ),
                    tags$div(
                      id = "ynow_lab_im_lb_scope_help",
                      style = "color:#888; font-size:12px; line-height:1.45; margin:-4px 0 8px 0;",
                      paste0(
                        "整體前十名：跨本次已評估產業依年化估值漲幅取 Top 10，並顯示產業欄。",
                        "選定產業前十名：所選產業內單一 Top 10；產業內排名自該產業第 1 名起算（非跨產業流水號）。",
                        "價值低估前十名：跨產業依推薦主模型合理價相對市價的總潛在漲幅（非年化）由大到小；只列仍被低估者。",
                        "產業市值加權漲幅（全部產業鏈）在上方獨立區塊（預設快照；按「更新排名」可重算）。",
                        "前十名只從合格者取最多 10 檔；合格不足 10 時不會湊滿。"
                      )
                    )
                  ),
                  uiOutput("lab_im_leader_note"),
                  uiOutput("lab_im_leaderboard_status"),
                  DT::dataTableOutput("lab_im_leaderboard")
                )
              ),
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = tags$div(
                    class = "ynow-lab-im-query-title-row",
                    tags$span(id = "ynow_lab_im_query_title", "查詢條件"),
                    actionButton(
                      "lab_im_refresh_universe",
                      tags$span(id = "ynow_lab_im_refresh_label", "更新名單"),
                      icon = icon("sync"),
                      class = "btn-default btn-sm ynow-lab-im-refresh-in-title",
                      title = paste0(
                        "重新擷取目前市場的成分／上市／上櫃／興櫃名單，",
                        "並為目前篩選池寫入盈餘品質、Piotroski F-Score 與財報警訊數。",
                        "24 小時內已更新的代號沿用既有快照。"
                      )
                    )
                  ),
                  tags$div(
                    class = "ynow-lab-im-universe-row",
                    uiOutput("lab_im_universe_meta")
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      class = "ynow-full-only",
                      tags$div(
                        class = "ynow-lab-im-industry-filters",
                        shinyWidgets::pickerInput(
                          "lab_im_chains",
                          tags$span(id = "ynow_lab_im_chains_label", "產業鏈"),
                          choices = lab_industry_chain_picker_choices(),
                          selected = unname(lab_industry_chain_picker_choices()),
                          multiple = TRUE,
                          width = "100%",
                          options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE,
                            liveSearch = TRUE,
                            selectedTextFormat = "count > 2",
                            noneSelectedText = "全部產業鏈",
                            size = 10,
                            container = "body",
                            dropupAuto = TRUE
                          )
                        ),
                        tags$span(
                          id = "ynow_lab_im_chains_hint",
                          class = "ynow-lab-im-quality-hint",
                          style = "display:block; color:#888; font-size:12px; margin:-2px 0 8px 0;",
                          "上層：半導體／科技／金融…；未選＝全部產業鏈。"
                        ),
                        shinyWidgets::pickerInput(
                          "lab_im_industries",
                          tags$span(id = "ynow_lab_im_industries_label", "子產業"),
                          choices = lab_industry_sub_picker_choices(),
                          selected = unname(lab_industry_sub_picker_choices()),
                          multiple = TRUE,
                          width = "100%",
                          options = shinyWidgets::pickerOptions(
                            actionsBox = TRUE,
                            liveSearch = TRUE,
                            selectedTextFormat = "count > 2",
                            noneSelectedText = "全部子產業",
                            size = 10,
                            container = "body",
                            dropupAuto = TRUE
                          )
                        ),
                        tags$span(
                          id = "ynow_lab_im_industries_hint",
                          class = "ynow-lab-im-quality-hint",
                          style = "display:block; color:#888; font-size:12px; margin:-2px 0 8px 0;",
                          "下層：依所選產業鏈列出子產業（如 IC 設計、晶圓代工）；未選＝該產業鏈下全部子產業。"
                        )
                      ),
                      tags$div(
                        class = "ynow-lab-im-boards ynow-tw-only",
                        style = "margin:8px 0 4px 0;",
                        checkboxGroupInput(
                          "lab_im_boards",
                          tags$span(id = "ynow_lab_im_boards_label", "板別"),
                          choiceNames = list(
                            tags$span(id = "ynow_lab_im_board_twse", "上市"),
                            tags$span(id = "ynow_lab_im_board_tpex", "上櫃"),
                            tags$span(id = "ynow_lab_im_board_esb", "興櫃")
                          ),
                          choiceValues = list("TWSE", "TPEX", "ESB"),
                          selected = APP_DEFAULTS$lab_im_boards,
                          inline = TRUE
                        ),
                        tags$span(
                          id = "ynow_lab_im_boards_hint",
                          class = "ynow-lab-im-quality-hint ynow-full-only",
                          style = "display:block; color:#888; font-size:12px; margin-top:-4px;",
                          "複選上市／上櫃／興櫃；預設上市＋上櫃。興櫃資料覆蓋較不穩，勾選後才納入績優評估池。"
                        )
                      ),
                      tags$div(
                        class = "ynow-lab-im-methods",
                        # Absolute (Fair Value + SOTP) vs Relative (Multiples families).
                        # Same input id `lab_im_methods` — filter logic unchanged.
                        .lab_im_methods_filter_ui(APP_DEFAULTS$lab_im_methods),
                        tags$span(
                          id = "ynow_lab_im_methods_locked_hint",
                          class = "ynow-lab-im-quality-hint ynow-full-only",
                          style = "display:block; color:#888; font-size:12px; margin-top:2px;",
                          paste0(
                            "若某評價模型未被任何個股列為主模型，則維持不勾選並鎖定。",
                            "相對估值的企業價值、盈餘、營收、淨值，在該類被當成 Multiples 參照時才可勾選",
                            "（樣式同美股 Listed）。「建議評價方法」表格列與此篩選列一致。"
                          )
                        )
                      )
                    ),
                    column(
                      width = 6,
                      class = "ynow-lab-im-filter-col",
                      # 盈餘品質｜Piotroski 高門檻｜無財報警訊：美／台股皆同列（Lite／Full 皆同）
                      tags$div(
                        class = "row ynow-lab-im-eq-gate-row",
                        column(
                          width = 4,
                          class = "ynow-lab-im-eq-col",
                          tags$div(
                            class = "ynow-lab-im-quality",
                            checkboxInput(
                              "lab_im_eq_only",
                              tags$span(id = "ynow_lab_im_eq_label", "盈餘品質"),
                              value = isTRUE(APP_DEFAULTS$lab_im_eq_only)
                            ),
                            tags$span(
                              id = "ynow_lab_im_eq_hint",
                              class = "ynow-lab-im-quality-hint ynow-full-only",
                              paste0(
                                "預設不勾選：勾選後排行榜／明細只列盈餘品質通過者；",
                                "維持不勾選則不過濾。合格不足 N 時不湊滿。"
                              )
                            )
                          )
                        ),
                        column(
                          width = 4,
                          class = "ynow-lab-im-gate-col",
                          tags$div(
                            class = "ynow-lab-im-quality",
                            checkboxInput(
                              "lab_im_gate_only",
                              tags$span(id = "ynow_lab_im_gate_label", "Piotroski 高門檻"),
                              value = isTRUE(APP_DEFAULTS$lab_im_gate_only)
                            ),
                            tags$span(
                              id = "ynow_lab_im_gate_hint",
                              class = "ynow-lab-im-quality-hint ynow-full-only",
                              paste0(
                                "預設不勾選：勾選後前十名與明細只列 Piotroski F-Score≥7（品質檢核）者；",
                                "維持不勾選則不設 F-Score 門檻。合格不足 N 或不足 10 時不會湊滿。"
                              )
                            )
                          )
                        ),
                        column(
                          width = 4,
                          class = "ynow-lab-im-no-alert-col",
                          tags$div(
                            class = "ynow-lab-im-quality",
                            checkboxInput(
                              "lab_im_no_alert",
                              tags$span(id = "ynow_lab_im_no_alert_label", "無財報警訊"),
                              value = isTRUE(APP_DEFAULTS$lab_im_no_alert)
                            ),
                            tags$span(
                              id = "ynow_lab_im_no_alert_hint",
                              class = "ynow-lab-im-quality-hint ynow-full-only",
                              paste0(
                                "預設不勾選：勾選後排行榜／明細只列財報警訊為 0 者（Schilit 警示＋盈餘品質紅旗，",
                                "與 YNOW「財報警訊」同一套定義）；維持不勾選則不過濾。合格不足 N 時不湊滿。"
                              )
                            )
                          )
                        )
                      ),
                      # 美股：僅「上市」＋含 ADR 同列並排（台股板別在左欄）
                      tags$div(
                        class = "row ynow-lab-im-us-listed-adr-row ynow-us-only",
                        style = "margin-top:12px;",
                        column(
                          width = 6,
                          class = "ynow-lab-im-us-listed-col",
                          tags$div(
                            class = "ynow-lab-im-quality ynow-lab-im-us-listed",
                            # Fixed listed-only scope for US (primary listings); not user-togglable
                            tags$div(
                              class = "checkbox",
                              tags$label(
                                tags$input(
                                  id = "lab_im_us_listed_display",
                                  type = "checkbox",
                                  checked = "checked",
                                  disabled = "disabled"
                                ),
                                tags$span(id = "ynow_lab_im_us_listed_label", "上市")
                              )
                            ),
                            tags$span(
                              id = "ynow_lab_im_us_listed_hint",
                              class = "ynow-lab-im-quality-hint ynow-full-only",
                              "美股績優池僅含主要上市（Nasdaq／NYSE／NYSE American）。"
                            )
                          )
                        ),
                        column(
                          width = 6,
                          class = "ynow-lab-im-include-adr",
                          tags$div(
                            class = "ynow-lab-im-quality",
                            checkboxInput(
                              "lab_im_include_adr",
                              tags$span(id = "ynow_lab_im_include_adr_label", "含 ADR"),
                              value = isTRUE(APP_DEFAULTS$lab_im_include_adr)
                            ),
                            tags$span(
                              id = "ynow_lab_im_include_adr_hint",
                              class = "ynow-lab-im-quality-hint ynow-full-only",
                              paste0(
                                "預設勾選：評估池含美股上市 ADR／外國發行人；",
                                "取消勾選則排除 ADR 後再套用候選截斷與宇宙檔數 N。"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  tags$div(
                    class = "ynow-lab-im-method-summary ynow-full-only",
                    style = "margin: 0 0 12px 0;",
                    DT::dataTableOutput("lab_im_summary")
                  ),
                  uiOutput("lab_im_filter_count"),
                  tags$div(
                    class = "ynow-lab-im-actions",
                    actionButton(
                      "lab_im_run_fscore", "搜尋績優股",
                      icon = icon("chart-line"),
                      class = "btn-success",
                      title = "Piotroski 高門檻（F-Score≥7）＋年化估值漲幅排序"
                    ),
                    downloadButton(
                      "lab_im_download_report", "下載本頁報告",
                      icon = icon("download"),
                      class = "btn btn-default",
                      title = "下載目前篩選、摘要與明細（不必重新評估）"
                    )
                  ),
                  tags$p(
                    id = "ynow_lab_im_eval_hint",
                    style = "margin-top:10px; color:#888; font-size:12px;",
                    "提示：評估需逐檔抓 Yahoo 財報與估值，檔數愈多愈久。"
                  )
                )
              ),
              # Lite：查詢條件區塊外正下方 — 盈餘品質說明
              tags$div(
                id = "ynow_lab_im_eq_explain",
                class = "ynow-lab-im-eq-explain ynow-lite-only",
                tags$b(id = "ynow_lab_im_eq_explain_title", "盈餘品質："),
                tags$span(
                  id = "ynow_lab_im_eq_explain_body",
                  paste0(
                    "勾選後，排行榜只列通過盈餘品質檢核者（OCF 與營運獲利交叉比對，",
                    "協助排除現金流與帳面獲利落差過大的標的）；預設不勾選則不過濾。",
                    "合格不足 N 時不湊滿。"
                  )
                )
              ),
              # Detail table at bottom of Rankings (Detail sub-tab removed)
              tags$hr(style = "margin: 18px 0 12px 0; border-color: #e5e8eb;"),
              p(
                id = "ynow_lab_im_detail_intro",
                "本次已評估檔的明細（按年化估值漲幅排序）。宇宙檔數（N）＝分析後最終顯示上限：候選截斷與評分後，最多顯示 N 檔合格列；條件不足時不湊滿。"
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = tags$span(
                    id = "ynow_lab_im_detail_box_title",
                    "明細（按年化估值漲幅排序）"
                  ),
                  DT::dataTableOutput("lab_im_table") %>% shinycssloaders::withSpinner()
                )
              )
            ),

            tabPanel(
              title = "分群",
              value = "im_cluster",
              icon = icon("project-diagram"),
              tags$p(
                id = "ynow_lab_cluster_blurb",
                paste0(
                  "研究用分群 Lab：僅以比率／成長率做 K-Means（不把金額放入模型），",
                  "降低公司規模對距離的干擾。語意標籤為描述性啟發式，非買進／賣出訊號。"
                )
              ),
              tags$p(
                id = "ynow_lab_cluster_disclaimer",
                style = "color:#a94442; font-size:12px; margin-top:-6px;",
                "僅供研究／教育，非投資建議，亦非買進訊號。"
              ),
              tags$hr(),
              fluidRow(
                column(
                  width = 2,
                  numericInput(
                    "lab_cluster_k",
                    tags$span(id = "ynow_lab_cluster_k_label", "群數（k）"),
                    value = APP_DEFAULTS$lab_cluster_k, min = 2, max = 8, step = 1, width = "100%"
                  )
                ),
                column(
                  width = 2,
                  selectInput(
                    "lab_cluster_x",
                    tags$span(id = "ynow_lab_cluster_x_label", "散點 X"),
                    choices = c(
                      "ROE" = "ROE",
                      "Operating Margin" = "Operating_Margin",
                      "Rev YoY" = "Rev_YoY",
                      "OpInc YoY" = "OpInc_YoY",
                      "Debt Ratio" = "Debt_Ratio",
                      "Trailing P/E" = "PE_Ratio",
                      "P/B" = "PB_Ratio"
                    ),
                    selected = APP_DEFAULTS$lab_cluster_x,
                    width = "100%"
                  )
                ),
                column(
                  width = 2,
                  selectInput(
                    "lab_cluster_y",
                    tags$span(id = "ynow_lab_cluster_y_label", "散點 Y"),
                    choices = c(
                      "ROE" = "ROE",
                      "Operating Margin" = "Operating_Margin",
                      "Rev YoY" = "Rev_YoY",
                      "OpInc YoY" = "OpInc_YoY",
                      "Debt Ratio" = "Debt_Ratio",
                      "Trailing P/E" = "PE_Ratio",
                      "P/B" = "PB_Ratio"
                    ),
                    selected = APP_DEFAULTS$lab_cluster_y,
                    width = "100%"
                  )
                ),
                column(
                  width = 3,
                  tags$div(
                    class = "ynow-sc-wrap ynow-ticker-typeahead",
                    style = "width: 100%; max-width: none;",
                    textInput(
                      "lab_cluster_focus",
                      tags$span(id = "ynow_lab_cluster_focus_label", "Radar focus ticker"),
                      value = "",
                      width = "100%",
                      placeholder = "e.g. AAPL / 2330"
                    ),
                    uiOutput("lab_cluster_focus_suggest_ui")
                  )
                ),
                column(
                  width = 3,
                  tags$div(
                    style = "margin-top: 24px;",
                    actionButton(
                      "lab_cluster_run",
                      tags$span(id = "ynow_lab_cluster_run_label", "執行分群"),
                      icon = icon("object-ungroup"),
                      class = "btn-success"
                    )
                  )
                )
              ),
              tags$p(
                id = "ynow_lab_cluster_hint",
                style = "color:#888; font-size:12px;",
                paste0(
                  "沿用「排行」頁目前的產業／模型篩選（若有）。",
                  "流程：宇宙池先依「候選截斷邏輯」全市排序／篩選（市值／概念股／近一年漲幅／隨機），",
                  "再依所選「宇宙檔數（N）」作分群分析（非固定預設檔數）。",
                  "Search 代號與雷達焦點代號（若有填寫）一律強制納入宇宙（N）；雷達焦點無預設代號。",
                  "擷取 Yahoo 比率特徵；若 Yahoo 受限則改用內建離線快照。"
                )
              ),
              fluidRow(
                box(
                  width = 7, status = "primary", solidHeader = TRUE,
                  title = tags$span(id = "ynow_lab_cluster_map_title", "分群星團圖"),
                  # renderUI destroys plotlyOutput when idle (empty plotly leaves stale widgets)
                  uiOutput("lab_cluster_scatter_ui")
                ),
                box(
                  width = 5, status = "info", solidHeader = TRUE,
                  title = tags$span(id = "ynow_lab_cluster_radar_title", "同群雷達圖"),
                  uiOutput("lab_cluster_radar_ui")
                )
              ),
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = tags$span(id = "ynow_lab_cluster_table_title", "分群結果"),
                  uiOutput("lab_cluster_table_ui")
                )
              )
            ),

            tabPanel(
              title = "自訂選股",
              value = "im_screener",
              icon = icon("filter"),
              if (exists("screener_panel_ui", mode = "function")) {
                screener_panel_ui(embed = "bluechip")
              } else {
                tags$p(id = "ynow_scr_missing", "Custom Screen unavailable.")
              }
            )
          )
  )
}

#' Lazy page body for tab `hfv` (mounted once per session).
.ynow_page_ui_hfv <- function() {
  tagList(
  withMathJax(),
          tags$div(
            class = "ynow-hfv-report",

            # --- Masthead ---
            tags$div(
              class = "ynow-hfv-report__masthead",
              h2(tags$b(id = "ynow_hfv_page_title", "Historical Fundamental Validation")),
              p(
                id = "ynow_hfv_page_sub",
                class = "ynow-hfv-report__lead",
                paste0(
                  "A point-in-time review of theoretical fair value versus market price: ",
                  "next-period odds, position vs FV, and historical scenario taxonomy. ",
                  "This is a validation report—not a trading backtest (see Quant Backtest Lab)."
                )
              )
            ),

            # --- How to read (top of page, collapsed by default) ---
            box(
              title = tagList(
                icon("book-open"),
                tags$span(id = "ynow_hfv_sec_method", "How to read this report")
              ),
              width = NULL,
              status = "primary",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              tags$div(
                class = "ynow-hfv-method",
                tags$p(
                  id = "ynow_hfv_method_body",
                  style = "font-size:12.5px;color:#444;line-height:1.55;margin:0 0 8px 0;",
                  paste0(
                    "Validation report — not a trading backtest. ",
                    "Chart Overlay Models (multi) drive chart FV lines and scenario A–D ",
                    "(average FV when several; show only with an A–D conclusion). ",
                    "Replay Model (single) drives odds, tip P(up), MOS, gap-to-FV, and the pair table."
                  )
                ),
                tags$p(
                  id = "ynow_hfv_sum_scenario_matrix",
                  style = "font-size:12px;color:#555;line-height:1.5;margin:0 0 6px 0;",
                  paste0(
                    "A Golden pit: FV↑, Price↓, Price ≪ FV · ",
                    "B Davis double: FV↑, Price↑, Price ≈ FV · ",
                    "C Value trap: FV↓, Price↓, Price < FV · ",
                    "D Bubble hype: FV≤flat, Price strong↑, Price ≫ FV · ",
                    "other = unmatched."
                  )
                ),
                tags$p(
                  id = "ynow_hfv_scenario_thresh_note",
                  style = "font-size:11.5px;color:#666;line-height:1.45;margin:0 0 4px 0;",
                  paste0(
                    "Scenario bands (from MOS Assumptions; defaults shown): flat |Δ|/prev ≤ 2%; |MOS| ≤ 10% ≈ FV; ",
                    "MOS ≥ 20% ≪ FV; MOS ≤ −20% ≫ FV; D also needs price momentum ≥ +5%. ",
                    "Adjust thresholds on the MOS Assumptions tab."
                  )
                ),
                tags$p(
                  style = "font-size:11.5px;color:#888;line-height:1.45;margin:0;",
                  id = "ynow_hfv_method_data_note",
                  paste0(
                    "Data: Yahoo annuals may be restated; PIT = period_end + ~90d (no soft bypass). ",
                    "g ≠ terminal SGR; missing CapEx/ΔNWC not invented as 0. ",
                    "TW OTC/ESB may use TPEx IS/BS fill (CF never invented). n<5 illustrative only."
                  )
                )
              )
            ),

            # Page-level common control (under How-to-read): analysis frequency
            tags$div(
              class = "ynow-hfv-page-controls",
              role = "group",
              `aria-label` = "HFV page controls",
              id = "ynow_hfv_page_controls",
              tags$div(
                class = "ynow-hfv-page-controls__freq",
                uiOutput("bt_fv_analysis_freq_ui")
              )
            ),

            # --- Chapter I: FV vs market ---
            tags$section(
              class = "ynow-hfv-chapter",
              tags$div(
                class = "ynow-hfv-chapter__head",
                tags$span(class = "ynow-hfv-chapter__kicker", id = "ynow_hfv_ch1_kicker", "Section I"),
                tags$h3(
                  class = "ynow-hfv-chapter__title",
                  id = "ynow_hfv_ch1_title",
                  "Fair value vs market price"
                )
              ),
              tags$div(
                class = "ynow-hfv-chapter__body",
                uiOutput("bt_valuation_summary"),
                # Chart overlay controls sit directly above the FV vs price chart
                tags$div(
                  class = "ynow-hfv-toolbar ynow-hfv-toolbar--above-chart",
                  role = "group",
                  `aria-label` = "HFV chart overlay controls",
                  id = "ynow_hfv_chart_overlay_controls",
                  tags$div(
                    class = "ynow-hfv-toolbar__group ynow-hfv-toolbar__group--wide",
                    checkboxGroupInput(
                      "bt_fv_models",
                      "Chart overlay models",
                      inline = TRUE,
                      # Sidebar L→R: NAV → DCF → DDM → RI → P/B (no Multiples/SOTP)
                      choices = ynow_fv_engine_choices(),
                      selected = APP_DEFAULTS$bt_fv_models
                    ),
                    tags$p(
                      id = "ynow_hfv_rel_models_note",
                      class = "help-block",
                      style = "margin:4px 0 0 0; font-size:12px;",
                      "Multiples (Reference) and SOTP (Meta-Framework) are not historical Fair Value engines — they are not available as HFV chart overlays."
                    ),
                    checkboxInput(
                      "bt_hfv_show_bench",
                      tags$span(id = "ynow_hfv_show_bench_label", "Show benchmark"),
                      value = isTRUE(APP_DEFAULTS$bt_hfv_show_bench)
                    ),
                    tags$p(
                      id = "ynow_hfv_overlay_vs_replay_note",
                      class = "help-block ynow-hfv-overlay-replay-note",
                      paste0(
                        "Overlay (multi): chart FV lines + scenario A–D (average FV when several). ",
                        "Replay (Section II, single): odds / tip P(up) / MOS / gap / pair table."
                      )
                    )
                  )
                ),
                plotlyOutput("bt_hfv_timeline", height = "420px") %>% withSpinner(),
                # Scenarios under chart; Overlay-only; hidden until A–D conclusion
                uiOutput("bt_hfv_scenario_findings"),
                uiOutput("bt_session_params")
              )
            ),

            # --- Chapter II: Investor summary ---
            tags$section(
              class = "ynow-hfv-chapter",
              tags$div(
                class = "ynow-hfv-chapter__head",
                tags$span(class = "ynow-hfv-chapter__kicker", id = "ynow_hfv_ch2_kicker", "Section II"),
                tags$h3(
                  class = "ynow-hfv-chapter__title",
                  id = "ynow_hfv_ch2_title",
                  "Investor summary"
                )
              ),
              tags$p(
                class = "ynow-hfv-chapter__lead",
                id = "ynow_hfv_ch2_lead",
                "Set Replay model and sample scope, then read the sample snapshot and tip market-price odds."
              ),
              tags$div(
                class = "ynow-hfv-chapter__body",
                tags$div(
                  class = "ynow-hfv-toolbar ynow-hfv-toolbar--in-ch2",
                  role = "group",
                  `aria-label` = "HFV validation controls",
                  id = "ynow_hfv_toolbar",
                  tags$div(
                    class = "ynow-hfv-toolbar__group",
                    radioButtons(
                      "bt_fv_replay_model",
                      "Replay model",
                      inline = TRUE,
                      # Sidebar L→R: NAV → DCF → DDM → RI → P/B (no Multiples/SOTP)
                      choices = ynow_fv_engine_choices(),
                      selected = APP_DEFAULTS$bt_fv_replay_model
                    )
                  ),
                  tags$div(
                    class = "ynow-hfv-toolbar__group",
                    radioButtons(
                      "bt_fv_conv_window",
                      "Sample window",
                      inline = TRUE,
                      choices = c(
                        "All" = "all",
                        "1Y" = "1y",
                        "3Y" = "3y",
                        "5Y" = "5y",
                        "Custom" = "custom"
                      ),
                      selected = APP_DEFAULTS$bt_fv_conv_window
                    ),
                    conditionalPanel(
                      condition = "input.bt_fv_conv_window == 'custom'",
                      dateRangeInput(
                        "bt_fv_conv_custom",
                        NULL,
                        start = Sys.Date() - 365 * 3,
                        end = Sys.Date(),
                        language = "zh-TW"
                      )
                    )
                  ),
                  tags$div(
                    class = "ynow-hfv-toolbar__group ynow-hfv-toolbar__group--wide",
                    radioButtons(
                      "bt_fv_oos_mode",
                      "Validation sample scope",
                      inline = TRUE,
                      choices = c(
                        "Realized next period only (default)" = "realized",
                        "Expanding-window OOS hit rates" = "expanding",
                        "Include unrealized next period (in-sample)" = "insample"
                      ),
                      selected = APP_DEFAULTS$bt_fv_oos_mode
                    )
                  )
                ),
                tags$div(
                  class = "ynow-hfv-findings-block",
                  # Locale target kept for applyUiLocale; chapter II title already names this block.
                  tags$span(id = "ynow_hfv_sec_results", style = "display:none;", "Sample snapshot"),
                  uiOutput("bt_hfv_investor_summary")
                )
              )
            ),

            # --- Chapter III: Market price & MOS ---
            tags$section(
              class = "ynow-hfv-chapter",
              tags$div(
                class = "ynow-hfv-chapter__head",
                tags$span(class = "ynow-hfv-chapter__kicker", id = "ynow_hfv_ch3_kicker", "Section III"),
                tags$h3(
                  class = "ynow-hfv-chapter__title",
                  id = "ynow_hfv_ch3_title",
                  "Next-period market price & MOS"
                )
              ),
              tags$p(
                class = "ynow-hfv-chapter__lead",
                id = "ynow_hfv_ch3_lead",
                "Did market price rise or fall between valuation dates? MOS outlook conditions tip odds (causal expanding-window)."
              ),
              tags$div(
                class = "ynow-hfv-chapter__body",
                uiOutput("bt_hfv_price_findings")
              )
            ),

            # --- Chapter IV: Gap to FV ---
            tags$section(
              class = "ynow-hfv-chapter",
              tags$div(
                class = "ynow-hfv-chapter__head",
                tags$span(class = "ynow-hfv-chapter__kicker", id = "ynow_hfv_ch4_kicker", "Section IV"),
                tags$h3(
                  class = "ynow-hfv-chapter__title",
                  id = "ynow_hfv_ch4_title",
                  "Gap to theoretical FV"
                )
              ),
              tags$p(
                class = "ynow-hfv-chapter__lead",
                id = "ynow_hfv_ch4_lead",
                "Did |P−FV_t| shrink or expand when P_next arrived? Chart = landing (P_next − FV) / FV."
              ),
              tags$div(
                class = "ynow-hfv-chapter__body",
                uiOutput("bt_hfv_fv_findings"),
                tags$div(
                  class = "ynow-hfv-findings-block",
                  style = "margin-top:14px;",
                  # Locale target; chapter IV lead already names the chart.
                  tags$span(id = "ynow_hfv_chart_gap", style = "display:none;", "Landing magnitude (P_next − FV) / FV"),
                  plotlyOutput("bt_fv_conv_plot", height = "280px") %>% withSpinner()
                )
              )
            ),

            # --- Chapter V: Period detail appendix (scenarios live under Ch1 chart) ---
            tags$section(
              class = "ynow-hfv-chapter",
              tags$div(
                class = "ynow-hfv-chapter__head",
                tags$span(class = "ynow-hfv-chapter__kicker", id = "ynow_hfv_ch5_kicker", "Section V"),
                tags$h3(
                  class = "ynow-hfv-chapter__title",
                  id = "ynow_hfv_ch5_title",
                  "Period detail (appendix)"
                )
              ),
              tags$p(
                class = "ynow-hfv-chapter__lead",
                id = "ynow_hfv_ch5_lead",
                "Pair-by-pair outcomes for audit — not a buy/sell score."
              ),
              tags$div(
                class = "ynow-hfv-chapter__body",
                tags$div(
                  class = "ynow-hfv-findings-block",
                  tags$span(id = "ynow_hfv_table_detail", style = "display:none;", "Pair table"),
                  tags$div(
                    style = "overflow-x:auto; width:100%;",
                    tags$style(HTML("#bt_fv_conv_table table { width: 100% !important; }")),
                    tableOutput("bt_fv_conv_table")
                  )
                )
              )
            ),

            # --- Appendix: param inventory ---
            box(
              title = tagList(
                icon("table"),
                tags$span(id = "ynow_hfv_param_inv_title", "US Valuation Replay Inventory (Live vs Hist PIT)")
              ),
              width = NULL,
              status = "primary",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              tags$p(
                id = "ynow_hfv_param_inv_help",
                style = "font-size:12.5px;color:#444;line-height:1.55;",
                "Historical theoretical values are rebuilt from then-available data; ",
                tags$b("hist DCF prefers NOPAT / D&A / CapEx / ΔNWC margin path"),
                ", else falls back to Gordon geometry ", tags$code("FCF0×(1+g)^t"), "; ",
                tags$b("not"),
                " the Live DCF page revenue→NOPAT / CapEx / ΔNWC forecast table."
              ),
              tags$div(style = "overflow-x:auto;", tableOutput("bt_param_inventory"))
            )
          )
  )
}

#' Lazy page body for tab `lab_notes` (mounted once per session).
.ynow_page_ui_lab_notes <- function() {
  tagList(
  withMathJax(),
          tags$div(
            class = "ynow-backtest-report",

            # --- Masthead ---
            tags$div(
              class = "ynow-backtest-report__masthead",
              h2(tags$b(id = "ynow_lab_notes_title", "Quantitative Backtest Lab"))
            ),

            # --- Intro + Backtest Zone run (4:1, vertically centered, light-gray band) ---
            tags$div(
              class = "ynow-backtest-zone-band",
              role = "group",
              `aria-label` = "Backtest run controls",
              id = "ynow_bt_toolbar",
              tags$div(
                class = "ynow-backtest-zone-band__lead",
                p(
                  id = "ynow_lab_notes_sub",
                  class = "ynow-backtest-report__lead",
                  paste0(
                    "Point-in-time strategy NAV and holding gates: read performance and the wealth-index chart first, ",
                    "then exposure versus buy-and-hold. Related controls sit under each chapter (collapsed by default). ",
                    "This is a quantitative backtest report—not Historical Fundamental Validation (see Hist. FV Validation)."
                  )
                )
              ),
              tags$div(
                class = "ynow-backtest-zone-band__run",
                tags$span(
                  class = "ynow-backtest-toolbar__label",
                  id = "ynow_bt_zone_title",
                  "Backtest Zone"
                ),
                actionButton(
                  "run_bt", "Run Backtest",
                  icon = icon("play"),
                  class = "btn-warning",
                  style = "margin:0; white-space:nowrap; font-weight:600;"
                ),
                uiOutput("bt_run_status")
              )
            ),

            # --- Chapter I: Performance KPIs ---
            tags$section(
              class = "ynow-backtest-chapter",
              tags$div(
                class = "ynow-backtest-chapter__head",
                tags$span(
                  class = "ynow-backtest-chapter__kicker",
                  id = "ynow_bt_ch1_kicker",
                  "Section I"
                ),
                tags$h3(
                  class = "ynow-backtest-chapter__title",
                  id = "ynow_bt_ch1_title",
                  "Performance summary"
                )
              ),
              tags$div(
                class = "ynow-backtest-chapter__body",
                uiOutput("perf_metrics")
              )
            ),

            # --- Ch I controls: parameter updates (default collapsed); Run lives in zone band above ---
            box(
              title = tagList(
                icon("play-circle"),
                tags$span(id = "ynow_bt_sec_run_controls", "Run controls & parameter updates")
              ),
              width = NULL,
              status = "warning",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              class = "ynow-bt-run-panel",
              checkboxInput(
                "bt_param_auto",
                "Auto-update parameters when the ticker changes",
                value = isTRUE(APP_DEFAULTS$bt_param_auto)
              ),
              tags$p(
                id = "ynow_bt_param_auto_hint",
                class = "ynow-backtest-inline-hint",
                paste0(
                  "When on, searching or loading a new company overwrites holding thresholds, exposure / sentiment weights, ",
                  "and aligns the HFV recommended valuation model. Manual edits turn this off."
                )
              ),
              actionButton(
                "bt_refresh_params", "Update parameters from current statements",
                icon = icon("sync"), class = "btn-default btn-block",
                style = "margin-bottom: 10px;"
              ),
              tags$p(
                id = "ynow_bt_refresh_params_hint",
                class = "ynow-backtest-inline-hint",
                "One-shot: recompute thresholds and weights from the loaded company statements (use after turning auto-update off)."
              )
            ),

            # --- Chapter II: Strategy NAV ---
            tags$section(
              class = "ynow-backtest-chapter",
              tags$div(
                class = "ynow-backtest-chapter__head",
                tags$span(
                  class = "ynow-backtest-chapter__kicker",
                  id = "ynow_bt_ch2_kicker",
                  "Section II"
                ),
                tags$h3(
                  class = "ynow-backtest-chapter__title",
                  id = "ynow_bt_ch2_title",
                  "Strategy NAV (wealth index, start = 1)"
                )
              ),
              tags$div(
                class = "ynow-backtest-chapter__body",
                tags$p(
                  id = "ynow_bt_ch2_lead",
                  class = "ynow-backtest-chapter__lead",
                  paste0(
                    "This is a wealth index, not a share price. Both strategies share the holding gate; ",
                    "paths differ—Fundamental NAV = Exp_A × daily returns; Sentiment NAV = Exp_B × daily returns ",
                    "(Exp_A mixed with momentum / RSI). Actual price on the HFV discount chart is not comparable here."
                  )
                ),
                # NAV window controls sit directly above the wealth-index chart
                tags$div(
                  class = "ynow-backtest-toolbar ynow-backtest-toolbar--chapter ynow-backtest-toolbar--above-chart",
                  role = "group",
                  `aria-label` = "NAV window controls",
                  id = "ynow_bt_nav_controls",
                  tags$div(
                    class = "ynow-backtest-toolbar__group ynow-backtest-toolbar__group--wide",
                    radioButtons(
                      "bt_nav_window",
                      "NAV window (each series resets to 1 at the window start)",
                      inline = TRUE,
                      choices = c(
                        "All" = "all",
                        "1Y" = "1y",
                        "3Y" = "3y",
                        "5Y" = "5y",
                        "Custom" = "custom"
                      ),
                      selected = APP_DEFAULTS$bt_nav_window
                    ),
                    conditionalPanel(
                      condition = "input.bt_nav_window == 'custom'",
                      dateRangeInput(
                        "bt_nav_custom",
                        NULL,
                        start = Sys.Date() - 365,
                        end = Sys.Date(),
                        language = "zh-TW"
                      )
                    )
                  )
                ),
                plotlyOutput("bt_equity_plot", height = "400px") %>% withSpinner(),
                tags$ul(
                  id = "ynow_bt_equity_legend",
                  class = "ynow-backtest-legend",
                  tags$li(
                    tags$b(id = "ynow_bt_leg_fund_label", "Fundamental NAV"),
                    tags$span(
                      id = "ynow_bt_leg_fund_body",
                      " (orange) = holding gate + MOS sizing × daily returns, from 1."
                    )
                  ),
                  tags$li(
                    tags$b(id = "ynow_bt_leg_sent_label", "Sentiment NAV"),
                    tags$span(
                      id = "ynow_bt_leg_sent_body",
                      " (blue) = Exp_A mixed with momentum / RSI; see Sentiment Parameters under Chapter IV."
                    )
                  ),
                  tags$li(
                    tags$b(id = "ynow_bt_leg_bh_label", "Stock buy-and-hold"),
                    tags$span(
                      id = "ynow_bt_leg_bh_body",
                      " (green) = 100% wealth index; "
                    ),
                    tags$b(id = "ynow_bt_leg_bench_label", "Benchmark"),
                    tags$span(
                      id = "ynow_bt_leg_bench_body",
                      " (gray dashed) = SPY or 0050.TW wealth index by market mode."
                    )
                  ),
                  tags$li(
                    id = "ynow_bt_leg_hfv_note",
                    "Per-share FV vs actual price: Hist. FV Validation sidebar — do not mix with this chart."
                  )
                )
              )
            ),

            # --- Chapter III: Exposure & vs B&H ---
            tags$section(
              class = "ynow-backtest-chapter",
              tags$div(
                class = "ynow-backtest-chapter__head",
                tags$span(
                  class = "ynow-backtest-chapter__kicker",
                  id = "ynow_bt_ch3_kicker",
                  "Section III"
                ),
                tags$h3(
                  class = "ynow-backtest-chapter__title",
                  id = "ynow_bt_ch3_title",
                  "Exposure paths & vs buy-and-hold"
                )
              ),
              tags$div(
                class = "ynow-backtest-chapter__body",
                fluidRow(
                  column(
                    width = 6,
                    tags$h5(
                      class = "ynow-backtest-subhead",
                      id = "ynow_bt_exposure_title",
                      "Two-mode exposure paths"
                    ),
                    uiOutput("bt_exposure_stats"),
                    plotlyOutput("bt_exposure_plot", height = "260px") %>% withSpinner()
                  ),
                  column(
                    width = 6,
                    tags$h5(
                      class = "ynow-backtest-subhead",
                      id = "ynow_bt_bh_gap_title",
                      "Relative to buy-and-hold"
                    ),
                    uiOutput("bt_bh_gap")
                  )
                )
              )
            ),

            # --- Ch III controls: Holding gate (default collapsed) ---
            tags$div(
              `data-ynow-lock` = "section:bt_holding_gate",
              box(
                title = tagList(
                  icon("filter"),
                  tags$span(id = "ynow_bt_sec_hold_gate", "Holding gate: position filters")
                ),
                width = NULL,
                status = "warning",
                solidHeader = FALSE,
                collapsible = TRUE,
                collapsed = TRUE,
                tags$p(
                  id = "ynow_bt_hold_gate_intro",
                  class = "ynow-backtest-chapter__lead",
                  paste0(
                    "On each rebalance date (monthly / quarterly / yearly by analysis frequency), all four filters must pass ",
                    "to allow a position; otherwise both Fundamental and Sentiment strategies stay flat (Exp_A = Exp_B = 0). ",
                    "Thresholds are shared with the backtest engine and the KPI filter."
                  )
                ),
                fluidRow(
                  column(3, tipify(numericInput("bt_net_margin", "Net margin threshold (%)", APP_DEFAULTS$bt_net_margin),
                                   "Auto mode uses about half of the company's historical net margin.", placement = "top")),
                  column(3, tipify(numericInput("bt_rev_growth", "Revenue growth threshold (%)", APP_DEFAULTS$bt_rev_growth),
                                   "Auto mode uses about half of historical revenue growth.", placement = "top")),
                  column(3, tipify(numericInput("bt_eps_growth", "EPS / net income growth threshold (%)", APP_DEFAULTS$bt_eps_growth),
                                   "Auto mode uses about half of net income growth.", placement = "top")),
                  column(3, tipify(numericInput("bt_fcf_cv", "FCF CV ceiling (%)", APP_DEFAULTS$bt_fcf_cv),
                                   "Auto mode uses FCF CV × 1.25.", placement = "top"))
                ),
                tags$hr(),
                tags$div(
                  style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap; margin-bottom:8px;",
                  tags$span(
                    style = "font-size:13px; font-weight:600;",
                    id = "ynow_bt_kpi_filter_label",
                    "KPI filter"
                  ),
                  actionButton(
                    "bt_kpi_filter", "Match current company",
                    icon = icon("filter"),
                    class = "btn-sm",
                    style = "background-color: #222222; color: #ffffff; border: 1px solid #111111; font-size: 12px; padding: 6px 14px; border-radius: 4px; font-weight: 600;"
                  ),
                  uiOutput("bt_filter_badge")
                ),
                tags$p(
                  id = "ynow_bt_kpi_filter_hint",
                  style = "margin: 0 0 8px 0; font-size: 12px; color: #666;",
                  "Compare Dashboard-loaded company KPIs to the thresholds above (same Great Filter as the backtest)."
                ),
                uiOutput("bt_filter_detail")
              )
            ),

            # --- Chapter IV: MOS / FV signal validation ---
            tags$section(
              class = "ynow-backtest-chapter",
              tags$div(
                class = "ynow-backtest-chapter__head",
                tags$span(
                  class = "ynow-backtest-chapter__kicker",
                  id = "ynow_bt_ch4_kicker",
                  "Section IV"
                ),
                tags$h3(
                  class = "ynow-backtest-chapter__title",
                  id = "ynow_bt_ch4_title",
                  "Signal validation: MOS & Fair Value"
                )
              ),
              tags$div(
                class = "ynow-backtest-chapter__body",
                tags$p(
                  id = "ynow_bt_ch4_lead",
                  class = "ynow-backtest-chapter__lead",
                  paste0(
                    "Checks whether undervaluation coincides with better forward returns—the core test of whether ",
                    "the backtest signal can stand. Parameter sensitivity belongs on the YNOW tab (WACC×g matrix)."
                  )
                ),
                tags$div(
                  class = "ynow-bt-validate",
                  fluidRow(
                    column(
                      6,
                      tags$div(
                        class = "ynow-bt-validate-col",
                        tags$div(
                          class = "ynow-bt-validate-panel",
                          tags$h5(tags$b(id = "ynow_bt_mos_eff_title", "MOS effectiveness")),
                          tags$p(
                            id = "ynow_bt_mos_eff_hint",
                            class = "ynow-backtest-inline-hint",
                            "Forward 1Y / 3Y / 5Y returns by MOS bucket: do higher MOS buckets earn more?"
                          ),
                          tags$div(style = "overflow-x:auto;", tableOutput("bt_mos_table"))
                        )
                      )
                    ),
                    column(
                      6,
                      tags$div(
                        class = "ynow-bt-validate-col",
                        tags$div(
                          class = "ynow-bt-validate-panel",
                          tags$h5(tags$b(id = "ynow_bt_fv_edge_title", "Fair Value predictive edge")),
                          uiOutput("bt_fv_edge"),
                          tags$div(style = "overflow-x:auto;", tableOutput("bt_fv_table"))
                        )
                      )
                    )
                  )
                )
              )
            ),

            # --- Ch IV controls: Strategy parameters (default collapsed) ---
            tags$div(
              `data-ynow-lock` = "section:bt_strategy_params",
            box(
              title = tagList(
                icon("sliders-h"),
                tags$span(id = "ynow_bt_sec_strategy_params", "Strategy parameters")
              ),
              width = NULL,
              status = "primary",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              uiOutput("bt_param_notes"),
              tags$p(
                id = "ynow_bt_params_gate_note",
                class = "ynow-backtest-chapter__lead",
                paste0(
                  "Holding gate (net margin / revenue growth / EPS growth / FCF volatility — fail → Exp_A = 0) ",
                  "is under Chapter III. Here adjust sizing and sentiment weights only."
                )
              ),
              tags$div(
                class = "ynow-bt-params",
                tabBox(
                  title = NULL,
                  width = 12,
                  tabPanel(
                    title = tagList(
                      icon("balance-scale"),
                      tags$span(id = "ynow_bt_tab_fundamental", "Fundamental")
                    ),
                    value = "bt_fundamental",
                    tags$p(
                      id = "ynow_bt_fund_intro",
                      class = "ynow-backtest-chapter__lead",
                      "Mode A: Exp_A → orange NAV line. MOS buckets set sizing; MOS uses HFV Replay-model fair value."
                    ),
                    fluidRow(
                      column(
                        6,
                        tags$div(
                          class = "ynow-backtest-callout",
                          tags$b(id = "ynow_bt_w_vg_moved_title", "MOS / Value Gap weight"),
                          tags$br(),
                          tags$span(
                            id = "ynow_bt_w_vg_moved_body",
                            "Set on the MOS Assumptions tab (investor MOS exposure settings)."
                          )
                        )
                      ),
                      column(
                        6,
                        tags$div(
                          class = "ynow-backtest-callout",
                          tags$b(id = "ynow_bt_mos_ladder_title", "MOS lag exposure (baseline map)"),
                          tags$br(),
                          tags$span(
                            id = "ynow_bt_mos_ladder_body",
                            paste0(
                              "Cut-points are set on MOS Assumptions. Default map: ",
                              "MOS≥30%→near max holding; ≥10%→~72%×cap; ≥0%→~44%×cap; ≥−10%→~17%×cap; else flat. ",
                              "(Max / floor holding and \"Closer to buy-and-hold\" are under Sentiment.)"
                            )
                          )
                        )
                      )
                    )
                  ),
                  tabPanel(
                    title = tagList(
                      icon("bolt"),
                      tags$span(id = "ynow_bt_tab_sentiment", "Sentiment")
                    ),
                    value = "bt_sentiment",
                    tags$div(
                      class = "ynow-bt-mode-b",
                      tags$p(
                        id = "ynow_bt_sent_intro",
                        class = "ynow-backtest-chapter__lead",
                        paste0(
                          "Mode B: mix momentum / RSI onto Exp_A (hot→fuller, cold→conservative). ",
                          "Blue line = Sentiment NAV from 1. Unrelated to HFV actual price."
                        )
                      ),
                      tags$div(
                        class = "ynow-bt-mode-b-grid",
                        fluidRow(
                          column(
                            3,
                            sliderInput("bt_w_mom", "Momentum relative weight", 0, 1, APP_DEFAULTS$bt_w_mom, step = 0.01),
                            tags$p(
                              id = "ynow_bt_w_mom_hint",
                              class = "ynow-backtest-inline-hint",
                              "With RSI forms the sentiment score, then mixes with Exp_A."
                            )
                          ),
                          column(
                            3,
                            sliderInput("bt_w_rsi", "RSI relative weight", 0, 1, APP_DEFAULTS$bt_w_rsi, step = 0.01),
                            tags$p(
                              id = "ynow_bt_w_rsi_hint",
                              class = "ynow-backtest-inline-hint",
                              "Overbought lowers the sentiment target; oversold raises it."
                            ),
                            checkboxInput(
                              "bt_news_on",
                              label = tags$span(id = "ynow_bt_news_on", "News sentiment factor"),
                              value = FALSE
                            ),
                            tags$p(
                              id = "ynow_bt_news_hint",
                              class = "ynow-backtest-inline-hint",
                              paste0(
                                "Uses today's headline score only on the latest rebalance. ",
                                "Very negative news blocks an RSI oversold add; very positive news blocks an overbought cut. ",
                                "Earlier dates stay momentum / RSI."
                              )
                            )
                          ),
                          column(
                            3,
                            sliderInput("bt_max_exp", "Max holding cap", 0.5, 1, APP_DEFAULTS$bt_max_exp, step = 0.01),
                            tags$p(
                              id = "ynow_bt_max_exp_hint",
                              class = "ynow-backtest-inline-hint",
                              "Set to 1.00 to remove structural underweight vs buy-and-hold."
                            )
                          ),
                          column(
                            3,
                            sliderInput("bt_min_exp_pass", "Min holding after gate pass", 0, 0.4, APP_DEFAULTS$bt_min_exp_pass, step = 0.01),
                            tags$p(
                              id = "ynow_bt_min_exp_hint",
                              class = "ynow-backtest-inline-hint",
                              "Floor size when the gate passes and valuation is not extremely rich."
                            )
                          )
                        )
                      ),
                      tags$div(
                        class = "ynow-bt-fit-row",
                        fluidRow(
                          column(
                            12,
                            tags$div(
                              class = "ynow-bt-fit-panel",
                              actionButton(
                                "bt_fit_bh_preset", "Closer to buy-and-hold",
                                icon = icon("chart-line"),
                                class = "btn-success",
                                style = "font-weight:600;"
                              ),
                              tags$div(
                                id = "ynow_bt_fit_bh_hint",
                                style = "margin-top:8px;font-size:11px;color:#666;",
                                "Quick apply: max=100%, min=40%, w_vg=0.35 (weaker de-risking). Turns auto-update parameters off."
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
            ),

            # --- Appendix: trend momentum (timing aid) ---
            decision_momentum_panel_ui("main_decision"),

            # --- Appendix: methodology ---
            box(
              title = tagList(
                icon("book-open"),
                tags$span(id = "ynow_bt_sec_methodology", "Data sources & methodology notes")
              ),
              width = NULL,
              status = "primary",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              uiOutput("bt_methodology_notes")
            )
          )
  )
}

#' Lazy page body for tab `asset_transmission` (mounted once per session).
#' @param locale first-paint locale (`en` / `zh-TW`); language switches still update via applyUiLocale.
.ynow_page_ui_asset_transmission <- function(locale = "en") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    "en"
  }
  .atx_s <- function(key) {
    if (exists("ui_str", mode = "function")) ui_str(key, loc) else key
  }
  tagList(
    fluidRow(
      column(
        width = 12,
        h2(tags$b(id = "ynow_atx_page_title", .atx_s("atx_page_title"))),
        p(
          id = "ynow_atx_page_sub",
          .atx_s("atx_page_sub")
        ),
        tags$hr()
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "box box-solid box-primary ynow-atx-panel",
          tags$div(
            class = "box-header",
            tags$h3(
              class = "box-title",
              tagList(icon("project-diagram"), tags$span(id = "ynow_atx_box_title", .atx_s("atx_box_title")))
            )
          ),
          tags$div(
            class = "box-body",
            tags$p(
              id = "ynow_atx_box_body",
              style = "color:#555; line-height:1.5; margin:0 0 10px 0;",
              .atx_s("atx_box_body")
            ),
            asset_transmission_ui("atx", locale = loc)
          )
        )
      )
    )
  )
}

#' Lazy page body for tab `about` (no inputs; safe for Home first HTML).
.ynow_page_ui_about <- function() {
  tagList(
    tags$div(
      class = "ynow-full-only",
      .about_bilingual_intro_ui()
    ),
    tags$div(
      class = "ynow-lite-only",
      .about_lite_intro_ui()
    ),
    tags$hr(style = "margin: 20px 0 12px 0; border-color: #e5e8eb;"),
    .about_data_sources_ui(),
    tags$hr(style = "margin: 20px 0 12px 0; border-color: #e5e8eb;"),
    .legal_notices_ui()
  )
}

#' Lazy page body for tab `feedback` (submit-only inputs).
.ynow_page_ui_feedback <- function() {
  tagList(
    fluidRow(
      column(
        width = 12,
        h2(tags$b(ui_i18n("feedback_page_title"))),
        p(ui_i18n("feedback_page_lead")),
        tags$p(
          style = "color:#888;font-size:13px;",
          ui_i18n("feedback_token_note")
        ),
        tags$hr()
      )
    ),
    fluidRow(
      column(
        width = 7,
        box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = tagList(icon("edit"), ui_i18n("feedback_form_title")),
          selectInput(
            "feedback_category", ui_i18n("feedback_category_label"),
            choices = c(
              "Suggestion" = "enhancement",
              "Bug report" = "bug",
              "Experience" = "ux",
              "Other" = "other"
            ),
            selected = "enhancement"
          ),
          textInput(
            "feedback_title", ui_i18n("feedback_title_label"),
            value = "",
            placeholder = ui_str("feedback_title_ph", "en")
          ) %>% tagAppendAttributes(`data-i18n-ph` = "feedback_title_ph"),
          textAreaInput(
            "feedback_body", ui_i18n("feedback_body_label"),
            value = "",
            rows = 8,
            placeholder = ui_str("feedback_body_ph", "en")
          ) %>% tagAppendAttributes(`data-i18n-ph` = "feedback_body_ph"),
          textInput(
            "feedback_contact", ui_i18n("feedback_contact_label"),
            value = "",
            placeholder = ui_str("feedback_contact_ph", "en")
          ) %>% tagAppendAttributes(`data-i18n-ph` = "feedback_contact_ph"),
          checkboxInput(
            "feedback_include_context",
            ui_i18n("feedback_include_context"),
            value = TRUE
          ),
          actionButton(
            "feedback_submit", ui_i18n("feedback_submit"),
            class = "btn-success",
            icon = icon("paper-plane")
          ),
          uiOutput("feedback_status")
        )
      ),
      column(
        width = 5,
        box(
          width = 12, status = "info", solidHeader = TRUE,
          title = ui_i18n("feedback_track_title"),
          tags$ul(
            tags$li(ui_i18n("feedback_track_1")),
            tags$li(
              ui_i18n("feedback_track_2"),
              " ",
              tags$code("feedback"),
              " ",
              tags$a(
                href = "https://github.com/lawrencekuo1118/theYNowApp/issues?q=is%3Aissue+label%3Afeedback",
                target = "_blank",
                rel = "noopener noreferrer",
                ui_i18n("feedback_track_link")
              )
            ),
            tags$li(ui_i18n("feedback_track_3"))
          )
        )
      )
    )
  )
}

#' Lazy page body for tab `sensitivity` (YNOW; display-only decision_ui).
.ynow_page_ui_sensitivity <- function() {
  decision_ui("main_decision")
}

#' Lazy page body for tab `smart_analysis` (outputs only).
.ynow_page_ui_smart_analysis <- function() {
  tagList(
    uiOutput("smart_analysis_summary"),
    uiOutput("smart_value_why"),
    fluidRow(
      box(
        width = 12, status = "primary", solidHeader = TRUE,
        title = tags$span(id = "ynow_smart_chart_title", "Fair value comparison"),
        plotlyOutput("smart_analysis_chart", height = "360px") %>% shinycssloaders::withSpinner()
      )
    ),
    uiOutput("smart_analysis_reason")
  )
}

#' Lazy page body for tab `company_advance` (BBLab UI; server stays eager).
.ynow_page_ui_company_advance <- function() {
  tags$div(
    class = "ynow-full-only ynow-company-advance-bblab",
    id = "ynow_company_advance_bblab",
    business_breakdown_lab_ui("bblab")
  )
}

#' Lazy page body for tab `testing` — dedicated system-admin portal (Full-only).
#' Internal tabName stays `testing` for sidebar/lazy wiring; UI copy is System admin.
#' Login: username `ynow` + Asia/Taipei calendar password YYYYMMDD (not end-user Email login).
.ynow_page_ui_testing <- function() {
  tagList(
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "ynow-sysadmin-login-chrome",
          h2(tags$b(id = "ynow_testing_page_title", "System admin")),
          p(
            id = "ynow_testing_page_sub",
            paste0(
              "Dedicated system-admin portal (Full mode). Lite hides this entry. ",
              "Sign in with the portal credentials below, then manage points ",
              "or switch to a de-identified user’s live view."
            )
          ),
          tags$hr(),
          tags$p(
            id = "ynow_testing_box_body",
            style = "color:#555; line-height:1.5;",
            paste0(
              "This portal replaces the old Testing sandbox home. ",
              "Adjust FYN / PYN, then use Switch view to open that user’s current UI ",
              "(balances, spend gates, and other persisted account state). ",
              "Title Loading turns silver while view-as is active."
            )
          )
        ),
        tags$div(
          class = "box box-solid ynow-pts-sysadmin-box",
          tags$div(
            class = "box-header with-border ynow-sysadmin-login-chrome",
            tags$h3(
              class = "box-title",
              tags$span(id = "ynow_pts_sysadmin_title", "System admin")
            )
          ),
          tags$div(
            class = "box-body",
            tags$p(
              id = "ynow_pts_sysadmin_intro",
              class = "ynow-sysadmin-login-chrome",
              style = "color:#555; line-height:1.5;",
              paste0(
                "Dedicated system-admin login (separate from end-user Email sign-in). ",
                "Users are listed with opaque ids only (no raw email, name, or phone). ",
                "Adjust FYN / PYN; changes persist and match what the user sees after sign-in. ",
                "Switch view replays that user’s persisted settings in the live UI."
              )
            ),
            # Locked: username + password + Enter only (hidden after unlock).
            fluidRow(
              class = "ynow-sysadmin-login-chrome",
              column(
                width = 4,
                textInput(
                  "ynow_pts_sysadmin_login_user",
                  label = tags$span(
                    id = "ynow_pts_sysadmin_login_user_label",
                    "Username"
                  ),
                  value = "ynow",
                  placeholder = "ynow"
                )
              ),
              column(
                width = 4,
                passwordInput(
                  "ynow_pts_sysadmin_login_pass",
                  label = tags$span(
                    id = "ynow_pts_sysadmin_login_pass_label",
                    "Password"
                  ),
                  value = "",
                  placeholder = ""
                )
              ),
              column(
                width = 4,
                tags$div(
                  class = "ynow-sysadmin-enter-wrap",
                  style = "margin-top: 25px;",
                  actionButton(
                    "ynow_pts_sysadmin_enter",
                    label = tags$span(
                      id = "ynow_pts_sysadmin_enter_label",
                      "Enter system admin"
                    ),
                    class = "btn-primary"
                  )
                )
              )
            ),
            # Unlocked: one control-panel bar (Exit / Switch view / Self view modes).
            # Signed-in user mode is the default on enter — no Signed-in button.
            # Self-view exit lives on this bar (no floating reminder banner).
            tags$div(
              id = "ynow_sysadmin_control_panel",
              class = "ynow-sysadmin-control-panel ynow-sysadmin-unlocked-actions",
              role = "toolbar",
              `aria-label` = "System admin control panel",
              tags$div(
                class = "ynow-sysadmin-control-panel__inner",
                tags$div(
                  class = "ynow-sysadmin-control-panel__cluster ynow-sysadmin-control-panel__cluster--session",
                  actionButton(
                    "ynow_pts_sysadmin_exit",
                    label = tags$span(
                      id = "ynow_pts_sysadmin_exit_label",
                      "Exit system admin"
                    ),
                    class = "btn-default btn-sm"
                  ),
                  actionButton(
                    "ynow_pts_sysadmin_viewas",
                    label = tags$span(
                      id = "ynow_pts_sysadmin_viewas_label",
                      "Switch view"
                    ),
                    class = "btn-info btn-sm"
                  )
                ),
                tags$span(
                  class = "ynow-sysadmin-control-panel__rule",
                  `aria-hidden` = "true"
                ),
                tags$div(
                  class = paste(
                    "ynow-sysadmin-control-panel__cluster",
                    "ynow-sysadmin-control-panel__cluster--self",
                    "ynow-sysadmin-self-view-group"
                  ),
                  tags$span(
                    id = "ynow_pts_sysadmin_self_view_label",
                    class = "ynow-sysadmin-control-panel__label",
                    "Self view"
                  ),
                  actionButton(
                    "ynow_pts_sysadmin_self_guest",
                    label = tags$span(
                      id = "ynow_pts_sysadmin_self_guest_label",
                      "Guest mode"
                    ),
                    class = "btn-default btn-sm",
                    title = "Quick apply guest restrictions"
                  ),
                  actionButton(
                    "ynow_pts_sysadmin_self_omniscient",
                    label = tags$span(
                      id = "ynow_pts_sysadmin_self_omniscient_label",
                      "Omniscient mode"
                    ),
                    # Same chrome as Guest mode (btn-default); red text via shared class.
                    class = "btn-default btn-sm ynow-sysadmin-btn-omniscient",
                    title = "Quick apply — lift all locks and hides"
                  ),
                  actionButton(
                    "ynow_pts_sysadmin_self_exit",
                    label = tags$span(
                      id = "ynow_pts_sysadmin_self_exit_label",
                      "Exit self view"
                    ),
                    class = "btn-default btn-sm ynow-sysadmin-control-panel__exit-self"
                  )
                )
              )
            ),
            tags$p(
              id = "ynow_pts_sysadmin_self_help",
              class = "ynow-sysadmin-control-panel-help text-muted",
              style = "margin:6px 0 0; font-size:12px; line-height:1.45; display:none;",
              paste0(
                "Quick apply self view (guest / omniscient). ",
                "Signed-in user mode is the default. Separate from Switch view of a selected user."
              )
            ),
            uiOutput("ynow_pts_sysadmin_panel")
          )
        )
      )
    )
  )
}

# tabItems() rejects a list of pages. Flatten one list argument so the three
# Multiples sidebar pages can come from a single module UI call.
.ynow_tab_items <- function(...) {
  parts <- list(...)
  flat <- list()
  .push <- function(x) {
    if (is.null(x)) return()
    flat[[length(flat) + 1L]] <<- x
  }
  for (p in parts) {
    if (is.null(p)) {
      next
    } else if (inherits(p, "shiny.tag")) {
      .push(p)
    } else if (is.list(p)) {
      for (q in p) .push(q)
    } else {
      .push(p)
    }
  }
  do.call(shinydashboard::tabItems, flat)
}

ui <- dashboardPage(
  skin = "black",
  # Browser tab. Header logo stays the HTML progress title; do not let that markup become <title>.
  title = "The YNow App",

  dashboardHeader(
                title = HTML(paste0(
                  '<span class="ynow-app-title" id="ynow_app_title" ',
                  'data-ynow-build="', .YNOW_BUILD_VERSION, '" ',
                  'role="progressbar" aria-valuemin="0" aria-valuemax="100" aria-valuenow="0" ',
                  'aria-label="The YNow App loading">',
                  '<span class="ynow-app-title-base" aria-hidden="true">The YNow App v27.12</span>',
                  '<span class="ynow-app-title-fill" aria-hidden="true">',
                  '<span class="ynow-app-title-fill-inner">The YNow App v27.12</span>',
                  '</span></span>'
                )),
    titleWidth = 250,
    tags$li(
      id = "ynow-market-header",
      class = "dropdown ynow-market-header",
      tags$div(
        class = "ynow-market-stack",
        role = "group",
        `aria-label` = "Market",
        tags$button(
          type = "button",
          class = "ynow-mkt-btn active",
          id = "ynow_mkt_btn_us",
          `data-value` = "US",
          `aria-label` = "US",
          title = "US",
          # Fixed-size SVG so US/TW flags match pixel-for-pixel (emoji glyphs differ).
          # Intrinsic size matches viewBox aspect (19:10). CSS sets height=25px hoist;
          # length (width) follows automatically — do not stretch.
          HTML(paste0(
            '<svg class="ynow-mkt-flag" aria-hidden="true" width="38" height="20" viewBox="0 0 19 10" ',
            'preserveAspectRatio="xMidYMid meet" xmlns="http://www.w3.org/2000/svg" focusable="false">',
            '<rect width="19" height="10" fill="#B22234"/>',
            '<path d="M0 1.1h19M0 3.3h19M0 5.5h19M0 7.7h19M0 9.9h19" stroke="#fff" stroke-width="1.1"/>',
            '<rect width="7.6" height="5.5" fill="#3C3B6E"/>',
            '</svg>'
          ))
        ),
        tags$button(
          type = "button",
          class = "ynow-mkt-btn",
          id = "ynow_mkt_btn_tw",
          `data-value` = "TW",
          `aria-label` = "TW",
          title = "TW",
          # Intrinsic size matches viewBox aspect (18:12 = 3:2).
          HTML(paste0(
            '<svg class="ynow-mkt-flag" aria-hidden="true" width="30" height="20" viewBox="0 0 18 12" ',
            'preserveAspectRatio="xMidYMid meet" xmlns="http://www.w3.org/2000/svg" focusable="false">',
            '<rect width="18" height="12" fill="#FE0000"/>',
            '<rect width="9" height="6" fill="#000095"/>',
            '<circle cx="4.5" cy="3" r="2.1" fill="#fff"/>',
            '<circle cx="4.5" cy="3" r="1.25" fill="#000095"/>',
            '</svg>'
          ))
        )
      )
    ),
    # Language (zh-TW ↔ en-US) — custom buttons (same stability pattern as US/TW;
    # avoid shinyWidgets radioGroupButtons re-render that displaces the control on mobile).
    # Vertically centered with the small header logo (auth links live in sidebar + Home).
    tags$li(
      class = "dropdown ynow-auth-header",
      if (exists("ynow_auth_links_ui", mode = "function")) ynow_auth_links_ui("sidebar")
    ),
    tags$li(
      class = "dropdown ynow-lang-header ynow-hdr-toggle",
      tags$div(
        class = "ynow-lang-stack",
        id = "ynow_lang_stack",
        role = "group",
        `aria-label` = "Language",
        tags$button(
          type = "button",
          class = "ynow-lang-btn",
          id = "ynow_lang_btn_zh",
          `data-value` = "zh-TW",
          "繁中"
        ),
        tags$button(
          type = "button",
          class = "ynow-lang-btn active",
          id = "ynow_lang_btn_en",
          `data-value` = "en",
          "EN"
        )
      )
    ),
    # FYN / PYN balances — right of locale switch, vertically stacked.
    # Hidden until sign-in (ynowPtsBanner visible=true removes ynow-pts-hidden).
    tags$li(
      class = "dropdown ynow-pts-header ynow-pts-hidden",
      id = "ynow-pts-header",
      tags$div(
        class = "ynow-pts-banner",
        id = "ynow_pts_banner",
        role = "status",
        `aria-label` = "Points",
        `aria-hidden` = "true",
        tags$div(
          class = "ynow-pts-banner-line",
          # Empty until sign-in fills via ynowPtsBanner (guest must not hold points copy).
          tags$span(id = "ynow_pts_banner_fyn_label", ""),
          tags$span(id = "ynow_pts_banner_fyn_value", class = "ynow-pts-banner-val", "")
        ),
        tags$div(
          class = "ynow-pts-banner-line",
          tags$span(id = "ynow_pts_banner_pyn_label", ""),
          tags$span(
            id = "ynow_pts_banner_pyn_value",
            class = "ynow-pts-banner-val ynow-pyn-num",
            ""
          )
        )
      )
    ),
    # Logo + USD／TWD under logo (modest gap below black header).
    # Click = soft-refresh the active sidebar tab (not a full session reload).
    tags$li(
      id = "ynow-header-logo",
      class = "dropdown ynow-header-logo",
      tags$img(
        id = "ynow_header_logo_home",
        class = "ynow-header-logo-mark",
        src = "ynow-logo-mark-40.png",
        width = 36,
        height = 36,
        alt = "YNow",
        role = "button",
        tabindex = "0",
        title = "Refresh current page",
        `aria-label` = "Refresh current page",
        `aria-busy` = "false"
      ),
      tags$div(
        class = "ynow-ccy-float",
        role = "group",
        `aria-label` = "Currency",
        shinyWidgets::radioGroupButtons(
          inputId = "session_ccy_pick",
          label = NULL,
          choices = c("USD", "TWD"),
          selected = APP_DEFAULTS$session_ccy,
          status = "default",
          size = "xs",
          individual = TRUE
        ),
        tags$div(
          class = "ynow-hdr-ccy-status",
          textOutput("hdr_ccy_status", inline = TRUE)
        )
      )
    )
  ),
  
  dashboardSidebar(
    width = 250,
    collapsed = TRUE,
    # Single auth surface by viewport: ≤767 date-row text links; ≥768 header button.
    column(
      width = 12,
      tags$div(
        class = "ynow-sidebar-date-row",
        # Narrow only (CSS @media max-width:767): auth text links left of date.
        if (exists("ynow_auth_links_ui", mode = "function")) ynow_auth_links_ui("date_row"),
        tags$span(class = "ynow-sidebar-date", textOutput("today", inline = TRUE)),
        # Narrow only (CSS): FYN/PYN right of login date. Wider keeps top header banner.
        tags$div(
          class = "ynow-pts-sidebar ynow-pts-hidden",
          id = "ynow-pts-sidebar",
          tags$div(
            class = "ynow-pts-sidebar-banner",
            id = "ynow_pts_sidebar_banner",
            role = "status",
            `aria-label` = "Points",
            `aria-hidden` = "true",
            tags$div(
              class = "ynow-pts-sidebar-line",
              tags$span(id = "ynow_pts_sidebar_fyn_label", ""),
              tags$span(id = "ynow_pts_sidebar_fyn_value", class = "ynow-pts-sidebar-val", "")
            ),
            tags$div(
              class = "ynow-pts-sidebar-line",
              tags$span(id = "ynow_pts_sidebar_pyn_label", ""),
              tags$span(
                id = "ynow_pts_sidebar_pyn_value",
                class = "ynow-pts-sidebar-val ynow-pyn-num",
                ""
              )
            )
          )
        ),
        # Signed-in only (CSS): rearrange sidebar tab order (edit gate for drag-reorder).
        tags$button(
          type = "button",
          id = "ynow_sidebar_reorder_btn",
          class = "ynow-sidebar-reorder-btn",
          title = "Rearrange sidebar tab order",
          `aria-label` = "Rearrange sidebar tab order",
          `aria-pressed` = "false",
          tags$i(class = "fas fa-sliders-h", `aria-hidden` = "true"),
          tags$span(id = "ynow_sidebar_reorder_btn_label", class = "ynow-sidebar-reorder-btn-label", "Rearrange")
        )
      ),
      tags$hr(class = "ynow-sidebar-date-hr")
    ),
    
    # Must stay inside column(width=12): bare sidebarMenu after Bootstrap
    # floated cols collapses to width:0 / left:250 (menu invisible).
    # 「推薦」badges are patched in-place (no renderMenu remount).
    # Page-pin tray removed; stock pins live under Recent Search. Tab long-press
    # is reorder-edit only (customize button).
    column(width = 12,
           sidebarMenu(
             id = "sidebar_tabs",
             menuItem(
               text = tags$span(id = "ynow_menu_home", "Home"),
               tabName = "home",
               icon = icon("home"),
               selected = TRUE
             ),
             menuItem(
               text = tags$span(id = "ynow_menu_macro", "Macro & Market Trends"),
               tabName = "macro_market",
               icon = icon("globe-asia")
             ),
             menuItem(
               text = if (exists("ynow_sidebar_guest_lock_label", mode = "function")) {
                 ynow_sidebar_guest_lock_label(
                   "asset_transmission", "ynow_menu_asset_transmission", "Asset Transmission Root"
                 )
               } else {
                 tags$span(id = "ynow_menu_asset_transmission", "Asset Transmission Root")
               },
               tabName = "asset_transmission",
               icon = icon("project-diagram")
             ),
             menuItem("Blue Chip Leaderboard", tabName = "bluechip", icon = icon("star")),
             menuItem("Company", tabName = "dashboard", icon = icon("chart-line")),
             menuItem(
               text = tags$span(id = "ynow_menu_company_advance", "Business Breakdown"),
               tabName = "company_advance",
               icon = icon("layer-group")
             ),
             menuItem("YNOW", tabName = "sensitivity", icon = icon("clock")),
             menuItem(
               text = tags$span(id = "ynow_menu_smart", "Smart Analysis"),
               tabName = "smart_analysis",
               icon = icon("magic")
             ),
             menuItem("Model Dashboard", tabName = "get_started", icon = icon("play-circle")),
             menuItem(
               text = tags$span(id = "ynow_menu_cat_asset", "Appr. Asset-based"),
               icon = icon("building"),
               startExpanded = FALSE,
               menuSubItem(
                 text = tags$span(id = "ynow_menu_nav", "NAV"),
                 tabName = "nav_calculator",
                 icon = icon("sitemap")
               ),
               menuSubItem(
                 text = tags$span(id = "ynow_menu_pb", "Justified P/B"),
                 tabName = "pb_calculator",
                 icon = icon("landmark")
               )
             ),
             menuItem(
               text = tags$span(id = "ynow_menu_cat_income", "Appr. Income / Cashflow"),
               icon = icon("chart-area"),
               startExpanded = FALSE,
               menuSubItem("DCF-Model", tabName = "dcf_calculator", icon = icon("calculator")),
               menuSubItem("DDM", tabName = "ddm_calculator", icon = icon("hand-holding-usd")),
               menuSubItem("RI-Model", tabName = "ri_calculator", icon = icon("gem"))
             ),
             menuItem(
               text = tags$span(id = "ynow_menu_cat_relative", "Appr. Relative Valuation"),
               icon = icon("percentage"),
               startExpanded = FALSE,
               menuSubItem(
                 text = tags$span(id = "ynow_menu_rel_ev", "Enterprise value"),
                 tabName = "rel_mult_ev",
                 icon = icon("building")
               ),
               menuSubItem(
                 text = tags$span(id = "ynow_menu_rel_earn", "Earnings"),
                 tabName = "rel_multiples_calculator",
                 icon = icon("chart-line")
               ),
               menuSubItem(
                 text = tags$span(id = "ynow_menu_rel_rev", "Revenue"),
                 tabName = "rel_mult_rev",
                 icon = icon("shopping-cart")
               ),
               menuSubItem(
                 text = tags$span(id = "ynow_menu_rel_book", "Book multiples"),
                 tabName = "pb_calculator",
                 icon = icon("percentage")
               )
             ),
             menuItem(
               text = tags$span(id = "ynow_menu_cat_meta", "Appr. Meta-Framework"),
               icon = icon("puzzle-piece"),
               startExpanded = FALSE,
               menuSubItem(
                 text = tags$span(id = "ynow_menu_sotp", "SOTP"),
                 tabName = "sotp_calculator",
                 icon = icon("puzzle-piece")
               )
             ),
             # 歷史基本面驗證（HFV）：理論估值 vs 實際市值 — 非策略回測
             menuItem("Hist. FV Validation", tabName = "hfv", icon = icon("balance-scale")),
             # Guest-locked on Full (ynow_sidebar_guest_lock_tabs); Lite omits via CSS
             menuItem(
               text = if (exists("ynow_sidebar_guest_lock_label", mode = "function")) {
                 ynow_sidebar_guest_lock_label("lab_notes", "ynow_menu_backtest", "Quant Backtest Lab")
               } else {
                 tagList(
                   icon("lock", class = "ynow-menu-lock"),
                   tags$span(id = "ynow_menu_backtest", "Quant Backtest Lab")
                 )
               },
               tabName = "lab_notes",
               icon = icon("flask")
             ),
             menuItem(
               text = if (exists("ynow_sidebar_guest_lock_label", mode = "function")) {
                 ynow_sidebar_guest_lock_label(
                   "decision_checklist", "ynow_menu_decision_checklist", "Decision Checklist"
                 )
               } else {
                 tagList(
                   icon("lock", class = "ynow-menu-lock"),
                   tags$span(id = "ynow_menu_decision_checklist", "Decision Checklist")
                 )
               },
               tabName = "decision_checklist",
               icon = icon("clipboard-check")
             ),
             menuItem(
               text = if (exists("ynow_sidebar_guest_lock_label", mode = "function")) {
                 ynow_sidebar_guest_lock_label(
                   "mos_assumptions", "ynow_menu_mos_assumptions", "MOS Assumptions"
                 )
               } else {
                 tagList(
                   icon("lock", class = "ynow-menu-lock"),
                   tags$span(id = "ynow_menu_mos_assumptions", "MOS Assumptions")
                 )
               },
               tabName = "mos_assumptions",
               icon = icon("shield-alt")
             ),
             menuItem(
               text = if (exists("ynow_sidebar_guest_lock_label", mode = "function")) {
                 ynow_sidebar_guest_lock_label(
                   "price_alerts", "ynow_menu_price_alert", "Value Alerts"
                 )
               } else {
                 tagList(
                   icon("lock", class = "ynow-menu-lock"),
                   tags$span(id = "ynow_menu_price_alert", "Value Alerts")
                 )
               },
               tabName = "price_alerts",
               icon = icon("bell")
             ),
             menuItem("About", tabName = "about", icon = icon("info-circle")),
             # Settings + Account: Full + signed-in only (CSS). Omit NULL via .list.
             .list = if (exists("ynow_compact_ui", mode = "function")) {
               ynow_compact_ui(
                 if (exists("ynow_settings_menu_item", mode = "function")) {
                   ynow_settings_menu_item()
                 },
                 if (exists("ynow_admin_menu_item", mode = "function")) ynow_admin_menu_item()
               )
             } else {
               list()
             }
             # Snapshot 不放主選單（避免巢狀 li 被瀏覽器抬出隱藏）；改由底部捷徑切換
           ),
           hr()
    ),
    
    column(width = 12,
           # Pinned tickers sit between the white separator above and "Recent Search:".
           uiOutput("pinned_recent_search"),
           h5(id = "ynow_recent_search_label", "Recent Search:"),
           uiOutput("recentsearch"),
           hr()
    ),
    
    column(width = 12,
           tags$div(
             class = "ynow-sidebar-brand ynow-lite-toggle",
             id = "ynow_lite_toggle",
             role = "button",
             tabindex = "0",
             `aria-pressed` = "false",
             `aria-label` = "Toggle Lite mode",
             title = "Click to switch Lite / Full",
             tags$div(
               class = "ynow-sidebar-brand-inner",
               tags$img(
                 class = "ynow-sidebar-logo-full",
                 src = "ynow-logo-full-480.png",
                 alt = "YNow — WH.Y VALUE NOW",
                 draggable = "false"
               ),
               tags$span(
                 class = "ynow-lite-badge",
                 id = "ynow_lite_badge",
                 "LITE"
               )
             )
           ),
           div(class = "ynow-sidebar-download-wrap",
               downloadButton("download_report", "Download Report (PDF)",
                              class = "ynow-sidebar-download-btn",
                              style = "background-color: #1a1a1a; color: #ffffff; border: 1px solid #000000; box-shadow: none; text-shadow: none;"),
               tags$div(
                 id = "ynow_download_report_quota",
                 class = "ynow-download-report-quota",
                 role = "status",
                 `aria-live` = "polite",
                 tags$p(
                   id = "ynow_pdf_quota_remaining",
                   class = "ynow-pdf-quota-line ynow-pdf-quota-remaining",
                   "Sign-in required. After sign-in: 3 free PDF downloads each Asia/Taipei day."
                 ),
                 tags$p(
                   id = "ynow_pdf_quota_rules",
                   class = "ynow-pdf-quota-line ynow-pdf-quota-rules",
                   "Points rule: after free quota, each new download costs 10 FYN. Same ticker + settings repeat is free."
                 )
               )
           )
    ),
    
    # Snapshot：側邊欄內容流最底部低調捷徑（勿用 absolute，會跑到搜尋框）
    # Foot links row + copyright directly below (copyright must not overlap the three links).
    # Decorative chip + logo-blue glow only — do not reintroduce sidebar「資料來源」blurb.
    column(
      width = 12,
      class = "ynow-sidebar-snapshot-foot",
      tags$div(
        class = "ynow-sidebar-foot-glow",
        `aria-hidden` = "true"
      ),
      tags$div(
        class = "ynow-sidebar-foot-chip",
        tags$div(
          class = "ynow-sidebar-snapshot-foot-links",
          tags$a(
            href = "#shiny-tab-snapshot",
            `data-toggle` = "tab",
            `data-value` = "snapshot",
            class = "ynow-sidebar-snapshot-link",
            onclick = "if (window.ynowShowSidebarFootTab) { window.ynowShowSidebarFootTab('snapshot'); } else if (window.Shiny) { Shiny.setInputValue('sidebar_tabs', 'snapshot', {priority: 'event'}); } return false;",
            icon("camera", class = "fa-fw"),
            tags$span(id = "ynow_snapshot_link_label", " Snapshot")
          ),
          # System admin：側邊欄專用入口（完整版）；Lite／已登入帳號隱藏；內部 tabName 仍為 testing
          tags$a(
            id = "ynow_sidebar_test_btn",
            href = "#shiny-tab-testing",
            `data-toggle` = "tab",
            `data-value` = "testing",
            class = "ynow-sidebar-snapshot-link ynow-sidebar-test-link ynow-full-only",
            onclick = "if (document.body && document.body.classList.contains('ynow-authed')) { return false; } if (window.Shiny) { Shiny.setInputValue('sidebar_test_click', (window.__ynowTestClicks=(window.__ynowTestClicks||0)+1), {priority: 'event'}); } if (window.ynowShowSidebarFootTab) { window.ynowShowSidebarFootTab('testing'); } else if (window.Shiny) { Shiny.setInputValue('sidebar_tabs', 'testing', {priority: 'event'}); } return false;",
            icon("user-shield", class = "fa-fw"),
            tags$span(id = "ynow_test_link_label", " Sys. Admin")
          ),
          # 意見區：收集使用者回饋，系統性開 GitHub Issue
          tags$a(
            id = "ynow_sidebar_feedback_btn",
            href = "#shiny-tab-feedback",
            `data-toggle` = "tab",
            `data-value` = "feedback",
            class = "ynow-sidebar-snapshot-link ynow-sidebar-feedback-link",
            onclick = "if (window.Shiny) { Shiny.setInputValue('sidebar_feedback_click', (window.__ynowFeedbackClicks=(window.__ynowFeedbackClicks||0)+1), {priority: 'event'}); } if (window.ynowShowSidebarFootTab) { window.ynowShowSidebarFootTab('feedback'); } else if (window.Shiny) { Shiny.setInputValue('sidebar_tabs', 'feedback', {priority: 'event'}); } return false;",
            icon("comment-dots", class = "fa-fw"),
            tags$span(id = "ynow_feedback_link_label", " Feedback")
          ),
        ),
        tags$p(
          id = "ynow_sidebar_copyright",
          class = "ynow-sidebar-copyright",
          "2026 © YNow Team. All rights reserved."
        )
      )
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    # Decorative leaf/branch motif for black top banner (logo-green glow).
    # Injected into .main-header so it stacks under title/nav (not over them).
    tags$script(HTML(paste0(
      "(function(){",
      "function ynowPlaceChromeLeaves(){",
      "var h=document.querySelector('.main-header');",
      "if(!h||h.querySelector('.ynow-chrome-header-leaves'))return;",
      "var el=document.createElement('div');",
      "el.className='ynow-chrome-header-leaves';",
      "el.setAttribute('aria-hidden','true');",
      "var img=document.createElement('img');",
      "img.className='ynow-chrome-header-veins';",
      "img.src='ynow-chrome-veins-branches.png';",
      "img.alt='';",
      "img.setAttribute('aria-hidden','true');",
      "img.draggable=false;",
      "el.appendChild(img);",
      # Far-left of .main-header; opaque .logo brand strip keeps veins out of the wordmark.
      "h.insertBefore(el,h.firstChild);",
      "}",
      # Drawn PCB traces from the foot chip upward — do NOT stretch the chip PNG.
      # Vertical clamp: fade to full black by the YNow sidebar brand logo (never paint
      # above/through it into the menu). Base traces = SOLID continuous strokes;
      # motion = short electric glow segment traveling on duplicate paths (dashoffset)
      # — never translateY / scaleY scroll of the whole asset; never dashed base lines.
      "function ynowPlaceChromeCircuit(){",
      "var s=document.querySelector('.main-sidebar');",
      "if(!s||s.querySelector('.ynow-chrome-sidebar-circuit'))return;",
      "var el=document.createElement('div');",
      "el.className='ynow-chrome-sidebar-circuit';",
      "el.setAttribute('aria-hidden','true');",
      "var glow=document.createElement('div');",
      "glow.className='ynow-chrome-sidebar-circuit-glow';",
      "var wrap=document.createElement('div');",
      "wrap.className='ynow-chrome-sidebar-circuit-strokes';",
      "wrap.innerHTML=",
      # viewBox height 480: foot (y=480) up to brand-logo band (~y=40) — clamped short.
      "'<svg class=\"ynow-chrome-sidebar-circuit-svg\" viewBox=\"0 0 230 480\" ",
      "preserveAspectRatio=\"xMidYMax meet\" aria-hidden=\"true\" focusable=\"false\">",
      "<defs>",
      "<linearGradient id=\"ynowCircuitStrokeGrad\" x1=\"0\" y1=\"1\" x2=\"0\" y2=\"0\">",
      "<stop offset=\"0%\" stop-color=\"#0C5484\">",
      "<animate attributeName=\"stop-color\" values=\"#0C5484;#7ec8f0;#0C5484\" dur=\"3.2s\" repeatCount=\"indefinite\"/>",
      "</stop>",
      "<stop offset=\"42%\" stop-color=\"#5eb0e0\">",
      "<animate attributeName=\"stop-color\" values=\"#5eb0e0;#ffffff;#0C5484;#5eb0e0\" dur=\"3.2s\" repeatCount=\"indefinite\"/>",
      "</stop>",
      "<stop offset=\"100%\" stop-color=\"#0C5484\">",
      "<animate attributeName=\"stop-color\" values=\"#0C5484;#ffffff;#0C5484\" dur=\"3.2s\" repeatCount=\"indefinite\"/>",
      "</stop>",
      "</linearGradient>",
      "<filter id=\"ynowCircuitGlow\" x=\"-40%\" y=\"-40%\" width=\"180%\" height=\"180%\">",
      "<feGaussianBlur stdDeviation=\"1.4\" result=\"b\"/>",
      "<feMerge><feMergeNode in=\"b\"/><feMergeNode in=\"SourceGraphic\"/></feMerge>",
      "</filter>",
      "</defs>",
      # Solid continuous PCB traces (line strokes only — no dash gaps on base paths).
      "<g class=\"ynow-ckt-base\" fill=\"none\" stroke=\"url(#ynowCircuitStrokeGrad)\" ",
      "stroke-linecap=\"round\" stroke-linejoin=\"round\" filter=\"url(#ynowCircuitGlow)\">",
      "<path class=\"ynow-ckt-p\" stroke-width=\"1.35\" d=\"M18 480 V340 L32 326 V250 L18 236 V140 L32 126 V48\"/>",
      "<path class=\"ynow-ckt-p\" stroke-width=\"1.1\" d=\"M36 480 V360 L48 348 V270 L36 258 V175 L50 162 V72\"/>",
      "<path class=\"ynow-ckt-p\" stroke-width=\"1.45\" d=\"M58 480 V330 L72 316 V230 L58 216 V130 L72 116 V40\"/>",
      "<path class=\"ynow-ckt-p\" stroke-width=\"1.2\" d=\"M84 480 V370 L98 356 V280 L84 266 V185 L98 172 V60\"/>",
      "<path class=\"ynow-ckt-p\" stroke-width=\"1.5\" d=\"M112 480 V320 L126 306 V220 L112 206 V120 L126 106 V36\"/>",
      "<path class=\"ynow-ckt-p\" stroke-width=\"1.15\" d=\"M138 480 V350 L152 336 V255 L138 241 V155 L152 142 V52\"/>",
      "<path class=\"ynow-ckt-p\" stroke-width=\"1.4\" d=\"M164 480 V325 L178 311 V235 L164 221 V135 L178 122 V44\"/>",
      "<path class=\"ynow-ckt-p\" stroke-width=\"1.2\" d=\"M190 480 V375 L204 361 V290 L190 276 V190 L204 176 V68\"/>",
      "<path class=\"ynow-ckt-p\" stroke-width=\"1.35\" d=\"M212 480 V345 L222 335 V260 L212 250 V150 L222 140 V55\"/>",
      "</g>",
      # Traveling electric glow: short dash on duplicate paths (not the visible wire style).
      "<g class=\"ynow-ckt-travel\" fill=\"none\" stroke=\"#7ec8f0\" stroke-linecap=\"round\" ",
      "stroke-linejoin=\"round\" filter=\"url(#ynowCircuitGlow)\">",
      "<path class=\"ynow-ckt-glow\" stroke-width=\"1.7\" d=\"M18 480 V340 L32 326 V250 L18 236 V140 L32 126 V48\"/>",
      "<path class=\"ynow-ckt-glow\" stroke-width=\"1.45\" d=\"M36 480 V360 L48 348 V270 L36 258 V175 L50 162 V72\"/>",
      "<path class=\"ynow-ckt-glow\" stroke-width=\"1.8\" d=\"M58 480 V330 L72 316 V230 L58 216 V130 L72 116 V40\"/>",
      "<path class=\"ynow-ckt-glow\" stroke-width=\"1.55\" d=\"M84 480 V370 L98 356 V280 L84 266 V185 L98 172 V60\"/>",
      "<path class=\"ynow-ckt-glow\" stroke-width=\"1.85\" d=\"M112 480 V320 L126 306 V220 L112 206 V120 L126 106 V36\"/>",
      "<path class=\"ynow-ckt-glow\" stroke-width=\"1.5\" d=\"M138 480 V350 L152 336 V255 L138 241 V155 L152 142 V52\"/>",
      "<path class=\"ynow-ckt-glow\" stroke-width=\"1.75\" d=\"M164 480 V325 L178 311 V235 L164 221 V135 L178 122 V44\"/>",
      "<path class=\"ynow-ckt-glow\" stroke-width=\"1.55\" d=\"M190 480 V375 L204 361 V290 L190 276 V190 L204 176 V68\"/>",
      "<path class=\"ynow-ckt-glow\" stroke-width=\"1.7\" d=\"M212 480 V345 L222 335 V260 L212 250 V150 L222 140 V55\"/>",
      "</g>",
      "<g fill=\"url(#ynowCircuitStrokeGrad)\">",
      "<circle class=\"ynow-ckt-node\" cx=\"32\" cy=\"48\" r=\"2.2\"/>",
      "<circle class=\"ynow-ckt-node\" cx=\"50\" cy=\"72\" r=\"2\"/>",
      "<circle class=\"ynow-ckt-node\" cx=\"72\" cy=\"40\" r=\"2.4\"/>",
      "<circle class=\"ynow-ckt-node\" cx=\"98\" cy=\"60\" r=\"2\"/>",
      "<circle class=\"ynow-ckt-node\" cx=\"126\" cy=\"36\" r=\"2.5\"/>",
      "<circle class=\"ynow-ckt-node\" cx=\"152\" cy=\"52\" r=\"2.1\"/>",
      "<circle class=\"ynow-ckt-node\" cx=\"178\" cy=\"44\" r=\"2.3\"/>",
      "<circle class=\"ynow-ckt-node\" cx=\"204\" cy=\"68\" r=\"2\"/>",
      "<circle class=\"ynow-ckt-node\" cx=\"222\" cy=\"55\" r=\"2.2\"/>",
      "</g>",
      "</svg>';",
      "el.appendChild(glow);",
      "el.appendChild(wrap);",
      "s.insertBefore(el,s.firstChild);",
      "}",
      "function ynowPlaceChromeAtmosphere(){",
      "ynowPlaceChromeLeaves();",
      "ynowPlaceChromeCircuit();",
      "}",
      "if(document.readyState==='loading'){",
      "document.addEventListener('DOMContentLoaded',ynowPlaceChromeAtmosphere);",
      "}else{ynowPlaceChromeAtmosphere();}",
      "})();"
    ))),
    # View-as chrome: visible on every tab while admin impersonates a user.
    tags$div(
      id = "ynow_viewas_chrome",
      class = "ynow-viewas-chrome",
      role = "status",
      `aria-live` = "polite",
      tags$strong(
        id = "ynow_viewas_chrome_active_label",
        "User view active"
      ),
      tags$code(id = "ynow_viewas_chrome_pid", ""),
      tags$span(
        id = "ynow_viewas_chrome_hint",
        style = "opacity:0.92;",
        "Title Loading is silver. Chrome, unlocks, balances, and prefs follow this user."
      ),
      tags$button(
        id = "ynow_viewas_chrome_exit",
        type = "button",
        class = "btn btn-default btn-sm",
        tags$span(
          id = "ynow_viewas_chrome_exit_label",
          "Exit user view"
        )
      )
    ),
    # Self-view reminder toast removed — exit lives on the control-panel bar.
    # C-STRUCT-02 seam 2: cookie banner / panel / reopen + prefs JS (sibling).
    .ynow_cookie_notice_ui(),
    # Account login modal/CSS — omit when disabled (do not inject NULL children)
    do.call(
      tagList,
      if (exists("ynow_compact_ui", mode = "function")) {
        ynow_compact_ui(
          if (exists("ynow_auth_css", mode = "function")) ynow_auth_css(),
          if (exists("ynow_auth_modal_ui", mode = "function")) ynow_auth_modal_ui()
        )
      } else {
        list()
      }
    ),
    # MathJax: never on Home first paint. Basic Setup methodology uses
    # .ynow-needs-mathjax + deferred loader below. HFV / Backtest Lab keep
    # withMathJax() inside lazy tab builders (not in the initial HTML).

    tags$head(
      # Deferred MathJax + gate-demo video arming (Home-critical-path hygiene).
      tags$script(HTML("
        (function () {
          var MJ_SRC = 'https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML';
          var mjState = 'idle';
          var mjWait = [];
          function runMjQueue() {
            var q = mjWait.slice();
            mjWait = [];
            q.forEach(function (fn) { try { fn(); } catch (eQ) {} });
          }
          function ensureMathJax(cb) {
            if (typeof cb === 'function') mjWait.push(cb);
            if (window.MathJax && window.MathJax.Hub) {
              mjState = 'ready';
              runMjQueue();
              return;
            }
            if (mjState === 'loading') return;
            mjState = 'loading';
            var s = document.createElement('script');
            s.src = MJ_SRC;
            s.async = true;
            s.onload = function () {
              mjState = 'ready';
              runMjQueue();
            };
            s.onerror = function () { mjState = 'idle'; mjWait = []; };
            (document.head || document.documentElement).appendChild(s);
          }
          function typesetMathJax() {
            ensureMathJax(function () {
              try {
                if (window.MathJax && MathJax.Hub) {
                  MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
                }
              } catch (eT) {}
            });
          }
          function paneNeedsMathJax(pane) {
            if (!pane) return false;
            if (pane.querySelector && pane.querySelector('.ynow-needs-mathjax')) return true;
            var id = pane.id || '';
            return id === 'shiny-tab-hfv' || id === 'shiny-tab-lab_notes' ||
              id === 'shiny-tab-get_started';
          }
          function maybeLoadMathJaxForActive() {
            var active = document.querySelector('.tab-pane.active');
            if (paneNeedsMathJax(active)) typesetMathJax();
          }
          window.ynowEnsureMathJax = ensureMathJax;
          window.ynowTypesetMathJax = typesetMathJax;
          document.addEventListener('shown.bs.tab', function () {
            maybeLoadMathJaxForActive();
          });
          document.addEventListener('shiny:value', function (ev) {
            try {
              var name = ev && ev.detail && ev.detail.name;
              if (!name) return;
              if (String(name).indexOf('ynow_lazy_host_hfv') >= 0 ||
                  String(name).indexOf('ynow_lazy_host_lab_notes') >= 0) {
                typesetMathJax();
              }
            } catch (eV) {}
          });
          if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', function () {
              setTimeout(maybeLoadMathJaxForActive, 0);
            });
          } else {
            setTimeout(maybeLoadMathJaxForActive, 0);
          }

          function armGateVideos(root) {
            var scope = root && root.querySelectorAll ? root : document;
            scope.querySelectorAll('video.ynow-gate-demo-video').forEach(function (v) {
              if (v.__ynowGateArmed) return;
              v.__ynowGateArmed = true;
              if (!('IntersectionObserver' in window)) return;
              var io = new IntersectionObserver(function (entries) {
                entries.forEach(function (en) {
                  if (en.isIntersecting && en.intersectionRatio >= 0.2) {
                    try { if (v.paused) v.play(); } catch (eP) {}
                  } else {
                    try { if (!v.paused) v.pause(); } catch (eS) {}
                  }
                });
              }, { threshold: [0, 0.2, 0.5] });
              io.observe(v);
            });
          }
          window.ynowArmGateDemoVideos = armGateVideos;
          if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', function () { armGateVideos(document); });
          } else {
            armGateVideos(document);
          }
          document.addEventListener('shiny:value', function () {
            setTimeout(function () { armGateVideos(document); }, 0);
          });
        })();
      ")),
      tags$style(HTML('
        /* YNOW monochrome chrome: black/white (keep KPI bg-blue / Schilit red-green) */
        :root {
          --ynow-ink: #1a1a1a;
          --ynow-ink-soft: #333333;
          --ynow-line: #d0d0d0;
          --ynow-wash: #f5f5f5;
          /* Sign-in / unlock gates: logo mono (not AdminLTE warning amber) */
          --ynow-gate-bg: #f5f5f5;
          --ynow-gate-border: #d0d0d0;
          --ynow-gate-accent: #1a1a1a;
          --ynow-gate-ink: #1a1a1a;
          --ynow-gate-ink-soft: #333333;
          /* 亮面金屬金（非土黃／ochre）；fallback 給不支援 clip 的環境） */
          --ynow-gold: #F5C518;
          /* 白底標題用深金（對比足夠）；圖標可用亮金 */
          --ynow-gold-ink: #856404;
          --ynow-gold-deep: #C9A227;
          /* logo 圖檔主色（藍／綠）— 現金流序列等資料色 */
          --ynow-logo-blue: #0C5484;
          --ynow-logo-green: #249C60;
          --ynow-logo-cyan: #1AA8B8;
          /* Chrome atmosphere (Balanced): soft glow alphas from logo blue/green */
          --ynow-chrome-blue-glow: rgba(12, 84, 132, 0.55);
          --ynow-chrome-green-glow: rgba(36, 156, 96, 0.45);
          --ynow-chrome-blue-edge: rgba(12, 84, 132, 0.35);
          --ynow-chrome-blue-halo: rgba(12, 84, 132, 0.22);
          /* Shared content column — same as Macro & Market Trends (.ynow-macro-report) */
          --ynow-page-max: 1200px;
          --ynow-logo-flow-gradient: linear-gradient(
            105deg,
            #0C5484 0%,
            #1578A0 16%,
            #1AA8B8 32%,
            #249C60 50%,
            #1AA8B8 68%,
            #0C5484 100%
          );
          /* HTCDI numerals only — flame flow (not shared with YNOW / Data-limited logo fill) */
          --ynow-htcdi-flame-gradient: linear-gradient(
            105deg,
            #4A0E0E 0%,
            #8B1A1A 12%,
            #C0392B 24%,
            #E67E22 38%,
            #F39C12 50%,
            #F7CA18 62%,
            #FFE66D 74%,
            #E67E22 88%,
            #922B21 100%
          );
          --ynow-thermo-ocean-gradient: linear-gradient(
            105deg,
            #021526 0%,
            #0C5484 16%,
            #1565C0 32%,
            #1AA8B8 48%,
            #4FC3F7 62%,
            #01579B 78%,
            #0C5484 100%
          );
          --ynow-thermo-mix-gradient: linear-gradient(
            105deg,
            #8B1A1A 0%,
            #E67E22 12%,
            #FFE66D 22%,
            #1AA8B8 38%,
            #0C5484 50%,
            #4FC3F7 62%,
            #C0392B 78%,
            #F39C12 88%,
            #01579B 100%
          );
          /* Model Selector｜估值模型推薦 色系（各模型頁主題；與小卡 icon 色一致） */
          --ynow-model-nav: #d81b60;
          --ynow-model-dcf: #00a65a;
          --ynow-model-ddm: #f39c12;
          --ynow-model-ri: #605ca8;
          --ynow-model-pb: #3c8dbc;
          --ynow-model-multiples: #17a2b8;
          --ynow-model-sotp: #343a40;
          --ynow-gold-gradient: linear-gradient(
            105deg,
            #FFF6C8 0%,
            #FFE566 18%,
            #F5C518 36%,
            #C9A227 50%,
            #FFE566 68%,
            #FFF8D0 100%
          );
          /* View-as mode: silver Loading / title (admin impersonation signal) */
          --ynow-silver: #C0C0C0;
          --ynow-silver-ink: #6E6E6E;
          --ynow-silver-deep: #8A8A8A;
          --ynow-silver-gradient: linear-gradient(
            105deg,
            #F4F4F4 0%,
            #E0E0E0 18%,
            #C0C0C0 36%,
            #8A8A8A 50%,
            #E0E0E0 68%,
            #F8F8F8 100%
          );
        }

        @keyframes ynow-gold-shine {
          0%, 100% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
        }
        @keyframes ynow-silver-shine {
          0%, 100% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
        }
        @keyframes ynow-logo-flow {
          0%, 100% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
        }
        @keyframes ynow-htcdi-flame-flow {
          0%, 100% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
        }
        @keyframes ynow-thermo-ocean-flow {
          0%, 100% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
        }
        @keyframes ynow-thermo-mix-flow {
          0%, 100% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
        }
        .ynow-thermo-tone {
          margin-left: 8px;
          font-weight: 700;
          background-size: 280% 100%;
          -webkit-background-clip: text;
          background-clip: text;
          color: transparent;
          -webkit-text-fill-color: transparent;
        }
        .ynow-thermo-track {
          position: relative;
          margin-top: 6px;
          height: 10px;
          border-radius: 5px;
          background-size: 280% 100%;
        }
        .ynow-thermo-marker {
          position: absolute;
          top: -2px;
          width: 3px;
          height: 14px;
          margin-left: -1px;
          border-radius: 1px;
          background: #fff;
          box-shadow: 0 0 0 1px rgba(0, 0, 0, 0.45);
        }
        .ynow-thermo-flame {
          background-image: var(--ynow-htcdi-flame-gradient);
          animation: ynow-htcdi-flame-flow 2.4s ease-in-out infinite;
        }
        .ynow-thermo-ocean {
          background-image: var(--ynow-thermo-ocean-gradient);
          animation: ynow-thermo-ocean-flow 3s ease-in-out infinite;
        }
        .ynow-thermo-mix {
          background-image: var(--ynow-thermo-mix-gradient);
          animation: ynow-thermo-mix-flow 3.2s ease-in-out infinite;
        }
        .ynow-thermo-track.ynow-thermo-empty {
          background: #eee;
        }
        .ynow-thermo-tone.ynow-thermo-empty {
          background: none;
          color: #666;
          -webkit-text-fill-color: #666;
          animation: none;
        }
        @media (prefers-reduced-motion: reduce) {
          .ynow-thermo-flame,
          .ynow-thermo-ocean,
          .ynow-thermo-mix {
            animation: none;
          }
        }

        /* Shiny toasts: keep above AdminLTE chrome; mobile must not clip / bury them */
        #shiny-notification-panel {
          position: fixed !important;
          z-index: 20000 !important;
          pointer-events: none;
        }
        #shiny-notification-panel .shiny-notification {
          pointer-events: auto;
          box-shadow: 0 4px 16px rgba(0, 0, 0, 0.18);
        }
        @media (max-width: 767px) {
          #shiny-notification-panel {
            top: auto !important;
            bottom: 12px !important;
            right: 8px !important;
            left: 8px !important;
            width: auto !important;
            max-width: none !important;
          }
          #shiny-notification-panel .shiny-notification {
            width: 100% !important;
            max-width: none !important;
            font-size: 13px;
            line-height: 1.4;
            margin: 0 0 8px 0 !important;
          }
        }

        /*
         * Mobile inline multi-option wrap (checkboxGroup / radioButtons inline=TRUE).
         * Bootstrap3 sets .checkbox-inline+.checkbox-inline / .radio-inline+.radio-inline
         * { margin-left: 10px }, so wrapped items indent as if still on the first row.
         * Flex + gap + zero sibling margin keeps every new row left-aligned and
         * vertically centered. Specialized toolbars (HFV grid chips, etc.) keep
         * their own display:!important rules.
         */
        @media (max-width: 767px) {
          .shiny-input-checkboxgroup .shiny-options-group:has(> .checkbox-inline),
          .shiny-input-checkboxgroup .shiny-options-group:has(> label.checkbox-inline),
          .shiny-input-radiogroup .shiny-options-group:has(> .radio-inline),
          .shiny-input-radiogroup .shiny-options-group:has(> label.radio-inline) {
            display: flex;
            flex-wrap: wrap;
            flex-direction: row;
            align-items: center;
            align-content: flex-start;
            justify-content: flex-start;
            column-gap: 12px;
            row-gap: 8px;
            clear: none;
          }
          .shiny-options-group > .checkbox-inline,
          .shiny-options-group > .radio-inline,
          .shiny-options-group > label.checkbox-inline,
          .shiny-options-group > label.radio-inline {
            float: none;
            margin-left: 0 !important;
            margin-right: 0 !important;
            vertical-align: middle;
          }
        }

        /* 標題＝載入進度條：底層淡金軌道 + 金色填滿層（隨 --ynow-load-pct） */
        /* z-index must stay above header veins (z4) on :hover too — otherwise
           AdminLTE/skin hover specificity drops the logo under foliage and the
           branch graphic flashes through the wordmark. Title strip stays #000
           (never AdminLTE skin-black white / never vein pattern through wordmark). */
        .main-header .logo,
        .main-header .logo:hover {
          position: relative;
          z-index: 5;
          font-weight: bold;
          color: #FFD700 !important;
          background-color: #000 !important;
          background-image: none !important;
          filter: none !important;
        }
        .main-header .logo .ynow-app-title {
          --ynow-load-pct: 0%;
          position: relative;
          display: inline-block;
          font-weight: bold;
          color: transparent !important;
          background-image: none !important;
          -webkit-text-fill-color: transparent;
          animation: none;
          filter: none !important;
          line-height: 1.15;
          vertical-align: middle;
        }
        /* Update-available: cursor only here; flame fill overrides come after gold title rules */
        body.ynow-update-available .main-header .logo,
        body.ynow-update-available .main-header .logo .ynow-app-title {
          cursor: pointer;
        }
        /* Major bump + signed-in: must-click reminder (never auto-reload / never force logout) */
        .ynow-major-update-reminder {
          display: none;
          position: fixed;
          inset: 0;
          z-index: 11000;
          align-items: flex-start;
          justify-content: center;
          padding: 72px 16px 24px;
          box-sizing: border-box;
          background: rgba(12, 14, 18, 0.55);
          pointer-events: none;
        }
        body.ynow-authed.ynow-update-major .ynow-major-update-reminder {
          display: flex;
          /* Keep root non-blocking so the flame title stays clickable. */
          pointer-events: none;
        }
        body.ynow-authed.ynow-update-major .main-header {
          z-index: 11050 !important;
        }
        .ynow-major-update-card {
          width: min(440px, 100%);
          margin-top: 8px;
          padding: 18px 20px 16px;
          border-radius: 10px;
          background: linear-gradient(165deg, #1a1f27 0%, #12161c 100%);
          border: 1px solid rgba(230, 126, 34, 0.55);
          box-shadow:
            0 0 0 1px rgba(192, 57, 43, 0.25),
            0 18px 40px rgba(0, 0, 0, 0.45);
          color: #f3f4f6;
          font-family: inherit;
          pointer-events: auto;
        }
        .ynow-major-update-title {
          margin: 0 0 8px;
          font-size: 17px;
          font-weight: 700;
          letter-spacing: 0.01em;
          color: #ffd27a;
        }
        .ynow-major-update-body {
          margin: 0 0 12px;
          font-size: 13.5px;
          line-height: 1.5;
          color: #e5e7eb;
        }
        .ynow-major-update-hint {
          margin: 0;
          font-size: 12.5px;
          line-height: 1.45;
          color: #f0a36b;
          font-weight: 600;
        }
        body.ynow-authed.ynow-update-major .main-header .logo .ynow-app-title {
          outline: 2px solid rgba(230, 126, 34, 0.85);
          outline-offset: 3px;
          border-radius: 4px;
        }
        .main-header .logo .ynow-app-title-base {
          display: inline-block;
          font-weight: bold;
          white-space: nowrap;
          color: rgba(245, 197, 24, 0.32) !important;
          -webkit-text-fill-color: rgba(245, 197, 24, 0.32);
          text-shadow: none;
        }
        .main-header .logo .ynow-app-title-fill {
          position: absolute;
          left: 0;
          top: 0;
          bottom: 0;
          width: var(--ynow-load-pct, 0%);
          max-width: 100%;
          overflow: hidden;
          white-space: nowrap;
          pointer-events: none;
          transition: width 0.28s ease-out;
        }
        .main-header .logo .ynow-app-title-fill-inner {
          display: inline-block;
          font-weight: bold;
          white-space: nowrap;
          color: #FFD700 !important;
          background-color: transparent !important;
          background-image: var(--ynow-gold-gradient) !important;
          background-size: 220% 100%;
          background-repeat: no-repeat;
          -webkit-background-clip: text;
          background-clip: text;
          -webkit-text-fill-color: transparent;
          filter: none !important;
        }
        .main-header .logo .ynow-app-title.is-loading .ynow-app-title-fill-inner {
          animation: none;
        }
        .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill {
          width: 100%;
          max-width: none;
          overflow: visible;
          transition: width 0.2s ease-out;
        }
        .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner {
          animation: ynow-gold-shine 2.6s ease-in-out infinite;
        }
        /* View-as: gold Loading → silver (place after gold; before deploy flame) */
        body.ynow-view-as .main-header .logo {
          color: var(--ynow-silver, #C0C0C0) !important;
        }
        body.ynow-view-as .main-header .logo .ynow-app-title-base {
          color: rgba(192, 192, 192, 0.38) !important;
          -webkit-text-fill-color: rgba(192, 192, 192, 0.38);
        }
        body.ynow-view-as .main-header .logo .ynow-app-title-fill-inner {
          color: var(--ynow-silver, #C0C0C0) !important;
          background-image: var(--ynow-silver-gradient) !important;
          background-size: 220% 100%;
          background-repeat: no-repeat;
          -webkit-background-clip: text;
          background-clip: text;
          -webkit-text-fill-color: transparent;
        }
        body.ynow-view-as .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner {
          animation: ynow-silver-shine 2.6s ease-in-out infinite;
        }
        /* Omniscient self-view: warning-red title (clear admin cue; after silver) */
        body.ynow-omniscient .main-header .logo {
          color: #DC3545 !important;
        }
        body.ynow-omniscient .main-header .logo .ynow-app-title-base {
          color: rgba(220, 53, 69, 0.42) !important;
          -webkit-text-fill-color: rgba(220, 53, 69, 0.42);
        }
        body.ynow-omniscient .main-header .logo .ynow-app-title-fill-inner {
          color: #DC3545 !important;
          background-image: none !important;
          background-color: transparent !important;
          -webkit-background-clip: border-box;
          background-clip: border-box;
          -webkit-text-fill-color: #DC3545 !important;
          animation: none !important;
        }
        body.ynow-omniscient .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner {
          animation: none !important;
        }
        @supports not ((-webkit-background-clip: text) or (background-clip: text)) {
          .main-header .logo .ynow-app-title-fill-inner {
            -webkit-text-fill-color: #FFD700 !important;
            color: #FFD700 !important;
            background-image: none !important;
            text-shadow:
              0 0 6px rgba(255, 230, 120, 0.85),
              0 0 14px rgba(255, 200, 40, 0.45),
              0 1px 2px rgba(0, 0, 0, 0.85);
          }
          .main-header .logo .ynow-app-title-base {
            color: rgba(255, 215, 0, 0.35) !important;
            -webkit-text-fill-color: rgba(255, 215, 0, 0.35);
          }
          body.ynow-view-as .main-header .logo .ynow-app-title-fill-inner {
            -webkit-text-fill-color: #C0C0C0 !important;
            color: #C0C0C0 !important;
            background-image: none !important;
            text-shadow:
              0 0 6px rgba(220, 220, 220, 0.75),
              0 0 14px rgba(160, 160, 160, 0.4),
              0 1px 2px rgba(0, 0, 0, 0.85);
          }
          body.ynow-view-as .main-header .logo .ynow-app-title-base {
            color: rgba(192, 192, 192, 0.4) !important;
            -webkit-text-fill-color: rgba(192, 192, 192, 0.4);
          }
          body.ynow-omniscient .main-header .logo .ynow-app-title-fill-inner {
            -webkit-text-fill-color: #DC3545 !important;
            color: #DC3545 !important;
            background-image: none !important;
            text-shadow: none;
          }
          body.ynow-omniscient .main-header .logo .ynow-app-title-base {
            color: rgba(220, 53, 69, 0.42) !important;
            -webkit-text-fill-color: rgba(220, 53, 69, 0.42);
          }
        }
        /* Fixed chrome while view-as is active (exit back to system admin) */
        .ynow-viewas-chrome {
          display: none;
          position: fixed;
          z-index: 19000;
          left: 50%;
          transform: translateX(-50%);
          top: 8px;
          max-width: min(920px, calc(100vw - 24px));
          padding: 8px 12px;
          border-radius: 6px;
          background: rgba(32, 32, 36, 0.92);
          color: #f0f0f0;
          border: 1px solid rgba(192, 192, 192, 0.55);
          box-shadow: 0 4px 18px rgba(0, 0, 0, 0.28);
          font-size: 13px;
          line-height: 1.4;
          align-items: center;
          gap: 10px;
          flex-wrap: wrap;
        }
        body.ynow-view-as .ynow-viewas-chrome {
          display: flex;
        }
        /* Self-view active toast/banner intentionally omitted (exit on control panel). */
        .ynow-viewas-chrome code {
          color: #e8e8e8;
          background: rgba(255, 255, 255, 0.08);
          padding: 1px 6px;
          border-radius: 3px;
        }
        .ynow-viewas-chrome .btn {
          margin: 0;
          padding: 4px 10px;
          font-size: 12px;
        }
        /* Site-wide cookie notice: bottom bar, not a full-screen lock */
        .ynow-cookie-banner {
          display: none;
          position: fixed;
          z-index: 18500;
          left: 12px;
          right: 12px;
          bottom: 12px;
          max-width: 920px;
          margin: 0 auto;
          max-height: min(70vh, 480px);
          overflow: auto;
          padding: 12px 14px;
          background: #fff;
          color: var(--ynow-ink, #1a1a1a);
          border: 1px solid var(--ynow-line, #d0d0d0);
          border-left: 4px solid var(--ynow-ink, #1a1a1a);
          border-radius: 6px;
          box-shadow: 0 4px 18px rgba(0, 0, 0, 0.18);
          font-size: 13px;
          line-height: 1.45;
        }
        .ynow-cookie-banner:not([hidden]) {
          display: block;
        }
        .ynow-cookie-banner__body {
          margin: 0;
        }
        .ynow-cookie-banner__actions {
          display: flex;
          flex-wrap: wrap;
          gap: 8px;
          margin-top: 10px;
        }
        .ynow-cookie-banner .btn {
          margin: 0;
          padding: 4px 10px;
          font-size: 12px;
        }
        .ynow-cookie-panel {
          display: none;
          margin-top: 10px;
          padding-top: 10px;
          border-top: 1px solid var(--ynow-line, #d0d0d0);
        }
        .ynow-cookie-panel:not([hidden]) {
          display: block;
        }
        .ynow-cookie-panel__title {
          margin: 0 0 8px 0;
          font-weight: 700;
        }
        .ynow-cookie-row {
          display: flex;
          align-items: flex-start;
          gap: 8px;
          margin: 0 0 8px 0;
          font-weight: 400;
        }
        .ynow-cookie-row input {
          margin-top: 3px;
        }
        .ynow-cookie-always {
          margin-left: 6px;
          font-size: 12px;
          color: var(--ynow-ink-soft, #333);
        }
        .ynow-cookie-hint {
          display: block;
          margin-top: 2px;
          font-size: 12px;
          color: var(--ynow-ink-soft, #333);
        }
        .ynow-cookie-reopen {
          position: fixed;
          z-index: 18400;
          right: 16px;
          left: auto;
          bottom: 16px;
          margin: 0;
          padding: 6px 12px;
          font-size: 12px;
          box-shadow: 0 2px 10px rgba(0, 0, 0, 0.22);
        }
        .ynow-cookie-reopen .fa {
          margin-right: 6px;
        }
        .ynow-cookie-reopen[hidden] {
          display: none !important;
        }
        @media (max-width: 767px) {
          .ynow-cookie-banner {
            left: 8px;
            right: 8px;
            bottom: 8px;
            max-height: 60vh;
          }
          .ynow-cookie-banner .btn,
          .ynow-cookie-reopen {
            font-size: 13px;
          }
          .ynow-cookie-reopen {
            right: 8px;
            left: auto;
            bottom: 8px;
          }
        }
        @media (prefers-reduced-motion: reduce) {
          .ynow-cookie-banner,
          .ynow-cookie-panel,
          .ynow-cookie-reopen {
            transition: none;
            animation: none;
          }
        }
        /* Access restrictions: hide = remove; lock = keep visible with locked chrome.
           Unified locked sidebar nav chrome (ACL + Full guest locks):
           .ynow-sidebar-locked + .ynow-access-locked-nav share opacity + lock glyph. */
        .sidebar-menu li.ynow-access-hidden { display: none !important; }
        .sidebar-menu li.ynow-sidebar-locked > a,
        .sidebar-menu li.ynow-access-locked-nav > a,
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a:has(> .ynow-menu-lock) {
          opacity: 0.55;
        }
        /* Maintenance (維護): unusable like lock, but menu is NON-clickable
           (unlike ACL / guest_menu_lock which still navigate into a gate). */
        .sidebar-menu li.ynow-maintenance-nav > a,
        .nav-tabs > li.ynow-maintenance-nav > a {
          opacity: 0.55;
          pointer-events: none !important;
          cursor: not-allowed !important;
        }
        .tab-pane.ynow-maintenance-pane > *:not(.ynow-maintenance-banner) {
          display: none !important;
        }
        .tab-pane.ynow-maintenance-pane::before {
          content: attr(data-maint-note);
          display: block;
          margin: 18px;
          padding: 14px 16px;
          border: 1px solid var(--ynow-gate-border, #d0d0d0);
          border-left: 4px solid var(--ynow-gate-accent, #1a1a1a);
          background: var(--ynow-gate-bg, #f5f5f5);
          color: var(--ynow-gate-ink-soft, #333333);
          border-radius: 6px;
          white-space: pre-line;
        }
        .form-group.ynow-maintenance-item,
        .shiny-input-container.ynow-maintenance-item,
        [data-ynow-lock].ynow-maintenance-item,
        a.btn.ynow-maintenance-item,
        button.ynow-maintenance-item {
          position: relative;
          opacity: 0.72;
          pointer-events: none !important;
          cursor: not-allowed !important;
        }
        .form-group.ynow-maintenance-item::after,
        .shiny-input-container.ynow-maintenance-item::after,
        [data-ynow-lock].ynow-maintenance-item::after {
          content: attr(data-maint-note);
          display: block;
          margin-top: 6px;
          font-size: 12px;
          color: var(--ynow-gate-ink-soft, #333333);
          white-space: pre-line;
        }
        .form-group.ynow-access-hidden,
        .shiny-input-container.ynow-access-hidden,
        [data-ynow-lock].ynow-access-hidden,
        a.btn.ynow-access-hidden,
        button.ynow-access-hidden { display: none !important; }
        .form-group.ynow-access-locked-input,
        .shiny-input-container.ynow-access-locked-input,
        [data-ynow-lock].ynow-access-locked-section {
          position: relative;
          opacity: 0.72;
          pointer-events: none;
        }
        .form-group.ynow-access-locked-input::after,
        .shiny-input-container.ynow-access-locked-input::after,
        [data-ynow-lock].ynow-access-locked-section::after {
          content: attr(data-lock-note);
          display: block;
          margin-top: 6px;
          font-size: 12px;
          color: var(--ynow-gate-ink-soft, #333333);
          white-space: normal;
        }
        a.btn.ynow-access-locked-btn,
        button.ynow-access-locked-btn {
          opacity: 0.55;
          pointer-events: none;
          cursor: not-allowed;
        }
        .tab-pane.ynow-access-hidden-pane { display: none !important; }
        .tab-pane.ynow-access-locked-pane > * { display: none !important; }
        .ynow-atx-lock-tpl { display: none !important; }
        .tab-pane.ynow-access-locked-pane.ynow-atx-locked-pane > .ynow-atx-lock-host,
        .tab-pane.ynow-access-locked-pane > .ynow-tab-lock-host {
          display: block !important;
          position: relative;
          min-height: min(72vh, 760px);
        }
        .tab-pane.ynow-access-locked-pane > .ynow-atx-lock-host > .ynow-member-gate-fade-backdrop,
        .tab-pane.ynow-access-locked-pane > .ynow-tab-lock-host > .ynow-member-gate-fade-backdrop,
        body.ynow-authed .tab-pane.ynow-access-locked-pane > .ynow-atx-lock-host > .ynow-member-gate-fade-backdrop,
        body.ynow-authed .tab-pane.ynow-access-locked-pane > .ynow-tab-lock-host > .ynow-member-gate-fade-backdrop {
          display: block !important;
          position: absolute;
          inset: 0;
          z-index: 0;
          overflow: hidden;
          pointer-events: none;
          background: #f7f7f7;
        }
        .tab-pane.ynow-access-locked-pane > .ynow-atx-lock-host > .ynow-member-gate-fade-backdrop::after,
        .tab-pane.ynow-access-locked-pane > .ynow-tab-lock-host > .ynow-member-gate-fade-backdrop::after {
          content: "";
          position: absolute;
          inset: 0;
          pointer-events: none;
          background: linear-gradient(
            180deg,
            rgba(255, 255, 255, 0.55) 0%,
            rgba(255, 255, 255, 0.88) 42%,
            rgba(255, 255, 255, 0.97) 100%
          );
        }
        .tab-pane.ynow-access-locked-pane > .ynow-atx-lock-host > .ynow-member-gate.ynow-member-gate--surface-overlay,
        .tab-pane.ynow-access-locked-pane > .ynow-tab-lock-host > .ynow-member-gate.ynow-member-gate--surface-overlay {
          display: flex !important;
          position: absolute;
          inset: 0;
          z-index: 2;
          flex-direction: column;
          max-width: none !important;
          width: auto !important;
          margin: 0 !important;
          overflow: auto;
          background: rgba(255, 255, 255, 0.92) !important;
          border: 1px solid #e6e6e6 !important;
          box-shadow: none !important;
        }
        .tab-pane.ynow-atx-locked-pane::before,
        .tab-pane.ynow-access-locked-pane:has(> .ynow-tab-lock-host)::before {
          content: none;
          display: none !important;
        }
        /* Inner-tab lock (section / input / report): promo only while that tab is selected. */
        .ynow-inner-tab-lock-tpl { display: none !important; }
        .nav-tabs > li.ynow-inner-tab-locked > a {
          font-size: 0 !important;
          line-height: 1;
        }
        .nav-tabs > li.ynow-inner-tab-locked > a > .ynow-inner-tab-lock,
        .nav-tabs > li.ynow-inner-tab-locked > a > i.ynow-menu-lock {
          display: inline-block !important;
          font-size: 13px !important;
          width: auto;
          margin: 0;
        }
        .nav-tabs > li.ynow-inner-tab-locked > a > i:not(.ynow-menu-lock),
        .nav-tabs > li.ynow-inner-tab-locked > a > span {
          display: none !important;
        }
        .tab-pane.ynow-inner-locked-pane > .ynow-inner-tab-lock-host { display: none; }
        .tab-pane.active.ynow-inner-locked-pane > .ynow-inner-tab-lock-host { display: block; }
        .tab-pane.ynow-inner-locked-pane .ynow-access-locked-section,
        .tab-pane.ynow-inner-locked-pane .ynow-access-locked-input,
        .tab-pane.ynow-inner-locked-pane .ynow-access-locked-btn {
          display: none !important;
        }
        .tab-pane.ynow-access-locked-pane::before {
          content: attr(data-lock-note);
          display: block;
          margin: 18px;
          padding: 14px 16px;
          border: 1px solid var(--ynow-gate-border, #d0d0d0);
          border-left: 4px solid var(--ynow-gate-accent, #1a1a1a);
          background: var(--ynow-gate-bg, #f5f5f5);
          color: var(--ynow-gate-ink-soft, #333333);
          border-radius: 6px;
          white-space: normal;
        }
        /* Sysadmin portal: after login hide all login/intro chrome */
        /* Shared sign-in / unlock gate chrome (black–white logo palette) */
        .ynow-gate-callout {
          margin: 0 0 12px 0;
          padding: 12px 14px;
          background: var(--ynow-gate-bg, #f5f5f5);
          border: 1px solid var(--ynow-gate-border, #d0d0d0);
          border-left: 4px solid var(--ynow-gate-accent, #1a1a1a);
          border-radius: 4px;
          color: var(--ynow-gate-ink-soft, #333333);
          font-size: 13px;
          line-height: 1.45;
        }
        /* Sign-in gate promo + member FAQ (HFV Section B/W; mobile-scannable) */
        .ynow-gate-extras {
          margin: 12px 0 16px 0;
        }
        /* Decision Checklist: unlock block + SOP demo 1:1 side-by-side */
        .ynow-gate-extras--checklist-split .ynow-gate-promo-demo-row {
          display: flex;
          flex-wrap: nowrap;
          align-items: stretch;
          gap: 12px;
          margin: 0 0 10px 0;
        }
        .ynow-gate-extras--checklist-split .ynow-gate-promo-demo-row > .ynow-gate-promo,
        .ynow-gate-extras--checklist-split .ynow-gate-promo-demo-row > .ynow-gate-demo {
          flex: 1 1 0;
          width: 0;
          min-width: 0;
          margin: 0;
          display: flex;
          flex-direction: column;
        }
        .ynow-gate-extras--checklist-split .ynow-gate-promo-demo-row > .ynow-gate-demo .ynow-gate-demo-video {
          flex: 1 1 auto;
          max-height: none;
          min-height: 160px;
          object-fit: contain;
        }
        @media (max-width: 767px) {
          .ynow-gate-extras--checklist-split .ynow-gate-promo-demo-row {
            flex-direction: column;
            flex-wrap: wrap;
          }
          .ynow-gate-extras--checklist-split .ynow-gate-promo-demo-row > .ynow-gate-promo,
          .ynow-gate-extras--checklist-split .ynow-gate-promo-demo-row > .ynow-gate-demo {
            width: 100%;
            flex: 1 1 auto;
          }
          .ynow-gate-extras--checklist-split .ynow-gate-promo-demo-row > .ynow-gate-demo .ynow-gate-demo-video {
            max-height: 220px;
            min-height: 0;
          }
        }
        /* Page-gate guest: faded shell + Sign-in overlay. --checklist kept as alias. */
        .ynow-member-gate-surface--checklist,
        .ynow-member-gate-surface--locked {
          position: relative;
          min-height: min(72vh, 760px);
        }
        .ynow-member-gate-fade-backdrop {
          display: none;
          pointer-events: none;
          user-select: none;
        }
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed)
          .ynow-member-gate-surface--checklist > .ynow-member-gate-fade-backdrop,
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed)
          .ynow-member-gate-surface--locked > .ynow-member-gate-fade-backdrop {
          display: block;
          position: absolute;
          inset: 0;
          z-index: 0;
          overflow: hidden;
          background: #f7f7f7;
        }
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed)
          .ynow-member-gate-surface--checklist > .ynow-member-gate-fade-backdrop::after,
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed)
          .ynow-member-gate-surface--locked > .ynow-member-gate-fade-backdrop::after {
          content: "";
          position: absolute;
          inset: 0;
          z-index: 1;
          pointer-events: none;
          background: linear-gradient(
            180deg,
            rgba(255, 255, 255, 0.55) 0%,
            rgba(255, 255, 255, 0.88) 42%,
            rgba(255, 255, 255, 0.97) 100%
          );
        }
        .ynow-dc-fade-skel {
          position: relative;
          z-index: 0;
          padding: 12px 10px 24px 10px;
          opacity: 0.55;
          filter: grayscale(0.15);
        }
        .ynow-dc-fade-skel__box {
          margin: 0 0 14px 0;
          padding: 14px 16px 16px 16px;
          border: 1px solid #e0e0e0;
          border-radius: 4px;
          background: #fff;
        }
        .ynow-dc-fade-skel__bar {
          height: 10px;
          margin: 0 0 10px 0;
          border-radius: 2px;
          background: #d8d8d8;
          width: 88%;
        }
        .ynow-dc-fade-skel__bar--title {
          height: 14px;
          width: 42%;
          background: #c8c8c8;
        }
        .ynow-dc-fade-skel__bar--mid { width: 64%; }
        .ynow-dc-fade-skel__bar--short { width: 36%; margin-bottom: 0; }
        .ynow-dc-fade-skel__grid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 10px;
          margin-top: 12px;
        }
        .ynow-dc-fade-skel__cell {
          height: 48px;
          border-radius: 3px;
          background: #ececec;
          border: 1px solid #e2e2e2;
        }
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed)
          .ynow-member-gate-surface--checklist > .ynow-member-gate.ynow-member-gate--surface-overlay,
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed)
          .ynow-member-gate-surface--locked > .ynow-member-gate.ynow-member-gate--surface-overlay {
          position: absolute;
          inset: 0;
          z-index: 2;
          display: flex !important;
          flex-direction: column;
          max-width: none !important;
          width: auto !important;
          margin: 0 !important;
          border-radius: 8px;
          overflow: auto;
          background: rgba(255, 255, 255, 0.92) !important;
          border: 1px solid #e6e6e6 !important;
          box-shadow: none !important;
        }
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed)
          .ynow-member-gate-surface--checklist > .ynow-member-gate--surface-overlay > .ynow-hfv-chapter__body,
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed)
          .ynow-member-gate-surface--locked > .ynow-member-gate--surface-overlay > .ynow-hfv-chapter__body {
          flex: 1 1 auto;
          overflow: auto;
        }
        body.ynow-lite .ynow-member-gate-fade-backdrop,
        body.ynow-authed .ynow-member-gate-fade-backdrop {
          display: none !important;
        }
        .ynow-gate-promo {
          margin: 0 0 10px 0;
          padding: 0;
          border: 1px solid #e6e6e6;
          border-radius: 8px;
          overflow: hidden;
          background: #fff;
        }
        .ynow-gate-promo__banner {
          display: flex;
          align-items: center;
          gap: 10px;
          padding: 12px 16px;
          background: #fff;
          color: #1a1a1a;
          border-bottom: 1px solid #ececec;
        }
        .ynow-gate-promo__badge {
          display: inline-block;
          flex: 0 0 auto;
          padding: 2px 8px;
          border: 1px solid #d0d0d0;
          border-radius: 2px;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.08em;
          line-height: 1.4;
          color: #888;
        }
        .ynow-gate-promo-kicker {
          font-size: 13px;
          font-weight: 600;
          letter-spacing: 0.02em;
          line-height: 1.35;
          color: #1a1a1a;
        }
        .ynow-gate-promo-lead {
          margin: 10px 12px 6px 12px;
          font-size: 13px;
          line-height: 1.45;
          color: var(--ynow-gate-ink-soft, #333333);
        }
        .ynow-gate-promo__list {
          margin: 0 12px 12px 12px;
          padding: 0 0 0 18px;
          color: var(--ynow-gate-ink-soft, #333333);
          font-size: 12.5px;
          line-height: 1.45;
        }
        .ynow-gate-promo__list > li {
          margin: 0 0 4px 0;
        }
        .ynow-gate-promo__list > li::marker {
          color: var(--ynow-gate-accent, #1a1a1a);
        }
        .ynow-gate-demo {
          margin: 0 0 10px 0;
          padding: 8px;
          border: 1px solid var(--ynow-gate-border, #d0d0d0);
          border-radius: 4px;
          background: #fff;
        }
        .ynow-gate-demo-caption {
          margin: 0 0 6px 0;
          font-size: 12px;
          font-weight: 600;
          color: var(--ynow-gate-ink, #1a1a1a);
        }
        .ynow-gate-demo-video {
          display: block;
          width: 100%;
          max-width: 100%;
          height: auto;
          max-height: 220px;
          background: #1a1a1a;
          border: 1px solid #000;
          border-radius: 2px;
        }
        .ynow-gate-extras--compact .ynow-gate-demo-video {
          max-height: 140px;
        }
        .ynow-gate-extras--compact .ynow-gate-promo-lead,
        .ynow-gate-extras--compact .ynow-gate-promo__list {
          font-size: 11.5px;
        }
        .ynow-snap-defaults-locked .ynow-gate-extras {
          margin-top: 10px;
        }
        /* Guest: sidebar PDF control shows a small lock (not download) */
        .ynow-sidebar-download-wrap #download_report > i.ynow-sidebar-dl-lock {
          font-size: 0.92em;
          margin-right: 2px;
          opacity: 0.95;
        }
        .ynow-gate-faq {
          margin: 0;
          border: 1px solid var(--ynow-gate-border, #d0d0d0);
          border-left: 4px solid var(--ynow-gate-accent, #1a1a1a);
          border-radius: 4px;
          background: #fff;
        }
        .ynow-gate-faq > .ynow-gate-faq__summary {
          list-style: none;
          cursor: pointer;
          padding: 10px 12px;
          font-size: 13px;
          font-weight: 600;
          color: var(--ynow-gate-ink, #1a1a1a);
          user-select: none;
        }
        .ynow-gate-faq > .ynow-gate-faq__summary::-webkit-details-marker {
          display: none;
        }
        .ynow-gate-faq > .ynow-gate-faq__summary::marker {
          content: "";
        }
        .ynow-gate-faq > .ynow-gate-faq__summary::before {
          content: "+";
          display: inline-block;
          width: 1.1em;
          margin-right: 6px;
          font-weight: 700;
          color: var(--ynow-gate-accent, #1a1a1a);
        }
        .ynow-gate-faq[open] > .ynow-gate-faq__summary::before {
          content: "–";
        }
        .ynow-gate-faq__body {
          padding: 0 12px 10px 12px;
          border-top: 1px solid var(--ynow-gate-border, #d0d0d0);
        }
        .ynow-gate-faq-item {
          margin: 10px 0 0 0;
        }
        .ynow-gate-faq-item > .ynow-gate-faq-q {
          margin: 0 0 2px 0;
          font-size: 12.5px;
          font-weight: 600;
          color: var(--ynow-gate-ink, #1a1a1a);
        }
        .ynow-gate-faq-item > .ynow-gate-faq-a {
          margin: 0;
          font-size: 12.5px;
          line-height: 1.45;
          color: var(--ynow-gate-ink-soft, #333333);
        }
        @media (max-width: 767px) {
          .ynow-hfv-chapter.ynow-member-gate:not(.ynow-member-gate--surface-overlay),
          .ynow-hfv-chapter.ynow-auth-gate,
          .ynow-hfv-chapter.ynow-gate-box:not(.ynow-member-gate--surface-overlay),
          .ynow-member-gate.ynow-gate-box:not(.ynow-member-gate--surface-overlay),
          .ynow-auth-gate.ynow-gate-box {
            max-width: 100% !important;
            margin-left: 8px !important;
            margin-right: 8px !important;
          }
          .ynow-member-gate-surface--checklist,
          .ynow-member-gate-surface--locked {
            min-height: min(78vh, 820px);
            margin-left: 0;
            margin-right: 0;
          }
          .ynow-gate-promo-lead,
          .ynow-gate-promo__list,
          .ynow-gate-faq-item > .ynow-gate-faq-a {
            font-size: 12px;
          }
        }
        /* Sign-in required: reuse HFV Section chapter chrome (B/W panel) */
        .ynow-hfv-chapter.ynow-gate-box,
        .ynow-hfv-chapter.ynow-member-gate,
        .ynow-hfv-chapter.ynow-auth-gate {
          margin: 0 0 18px 0;
          padding: 0;
          background: #fff !important;
          border: 1px solid #e6e6e6 !important;
          border-radius: 8px;
          overflow: hidden;
          box-shadow: none !important;
        }
        .ynow-gate-box > .ynow-hfv-chapter__head,
        .ynow-member-gate > .ynow-hfv-chapter__head,
        .ynow-auth-gate > .ynow-hfv-chapter__head {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 6px 12px;
          padding: 12px 16px;
          background: #fff !important;
          border-bottom: 1px solid #ececec;
          color: #1a1a1a !important;
        }
        .ynow-gate-box > .ynow-hfv-chapter__head .ynow-hfv-chapter__title,
        .ynow-member-gate > .ynow-hfv-chapter__head .ynow-hfv-chapter__title,
        .ynow-auth-gate > .ynow-hfv-chapter__head .ynow-hfv-chapter__title,
        .ynow-gate-box > .ynow-hfv-chapter__head .ynow-hfv-chapter__title *,
        .ynow-member-gate > .ynow-hfv-chapter__head .ynow-hfv-chapter__title *,
        .ynow-auth-gate > .ynow-hfv-chapter__head .ynow-hfv-chapter__title * {
          margin: 0;
          font-size: 16px;
          font-weight: 700;
          line-height: 1.35;
          color: #1a1a1a !important;
        }
        .ynow-gate-box > .ynow-hfv-chapter__body,
        .ynow-member-gate > .ynow-hfv-chapter__body,
        .ynow-auth-gate > .ynow-hfv-chapter__body {
          padding: 12px 16px 16px 16px;
          background: #fff !important;
          color: var(--ynow-gate-ink-soft, #333333);
        }
        /* CTAs: logo blue–green–white (beat Shiny btn-default + AdminLTE) */
        .ynow-gate-box .btn.ynow-auth-open,
        .ynow-member-gate .btn.ynow-auth-open,
        .ynow-auth-gate .btn.ynow-auth-open,
        .ynow-gate-box .btn.btn-default.btn-primary.ynow-auth-open,
        .ynow-member-gate .btn.btn-default.btn-primary.ynow-auth-open,
        .ynow-auth-gate .btn.btn-default.btn-primary.ynow-auth-open,
        .ynow-gate-box .ynow-auth-pair--gate > .btn.ynow-auth-open,
        .ynow-member-gate .ynow-auth-pair--gate > .btn.ynow-auth-open,
        .ynow-auth-gate .ynow-auth-pair--gate > .btn.ynow-auth-open {
          background-color: var(--ynow-logo-blue, #0C5484) !important;
          background-image: none !important;
          border-color: #0a446c !important;
          color: #fff !important;
        }
        .ynow-gate-box .btn.ynow-auth-open:hover,
        .ynow-member-gate .btn.ynow-auth-open:hover,
        .ynow-auth-gate .btn.ynow-auth-open:hover {
          background-color: #0a446c !important;
          border-color: #083554 !important;
          color: #fff !important;
        }
        .ynow-gate-box .btn.ynow-auth-register-open,
        .ynow-member-gate .btn.ynow-auth-register-open,
        .ynow-auth-gate .btn.ynow-auth-register-open,
        .ynow-gate-box .ynow-auth-register-open,
        .ynow-member-gate .ynow-auth-register-open,
        .ynow-auth-gate .ynow-auth-register-open {
          background: #fff !important;
          color: var(--ynow-logo-green, #249C60) !important;
          border: 1px solid var(--ynow-logo-green, #249C60) !important;
        }
        .ynow-gate-box .btn.ynow-auth-register-open:hover,
        .ynow-member-gate .btn.ynow-auth-register-open:hover,
        .ynow-auth-gate .btn.ynow-auth-register-open:hover {
          background: #f3faf6 !important;
          color: #1e8552 !important;
          border-color: #1e8552 !important;
        }
        /* Keep HFV Section B/W under model / Blue Chip / ATX accent themes */
        body.ynow-theme-model .content-wrapper .ynow-hfv-chapter.ynow-gate-box,
        body.ynow-theme-model .content-wrapper .ynow-hfv-chapter.ynow-member-gate,
        body.ynow-theme-model .content-wrapper .ynow-hfv-chapter.ynow-auth-gate,
        body.ynow-theme-bluechip .content-wrapper .ynow-hfv-chapter.ynow-gate-box,
        body.ynow-theme-bluechip .content-wrapper .ynow-hfv-chapter.ynow-member-gate,
        body.ynow-theme-bluechip .content-wrapper .ynow-hfv-chapter.ynow-auth-gate,
        body.ynow-theme-atx .content-wrapper .ynow-hfv-chapter.ynow-gate-box,
        body.ynow-theme-atx .content-wrapper .ynow-hfv-chapter.ynow-member-gate,
        body.ynow-theme-atx .content-wrapper .ynow-hfv-chapter.ynow-auth-gate {
          border-color: #e6e6e6 !important;
          background: #fff !important;
        }
        body.ynow-theme-model .content-wrapper .ynow-gate-box > .ynow-hfv-chapter__head,
        body.ynow-theme-model .content-wrapper .ynow-member-gate > .ynow-hfv-chapter__head,
        body.ynow-theme-model .content-wrapper .ynow-auth-gate > .ynow-hfv-chapter__head,
        body.ynow-theme-bluechip .content-wrapper .ynow-gate-box > .ynow-hfv-chapter__head,
        body.ynow-theme-bluechip .content-wrapper .ynow-member-gate > .ynow-hfv-chapter__head,
        body.ynow-theme-bluechip .content-wrapper .ynow-auth-gate > .ynow-hfv-chapter__head,
        body.ynow-theme-atx .content-wrapper .ynow-gate-box > .ynow-hfv-chapter__head,
        body.ynow-theme-atx .content-wrapper .ynow-member-gate > .ynow-hfv-chapter__head,
        body.ynow-theme-atx .content-wrapper .ynow-auth-gate > .ynow-hfv-chapter__head {
          background: #fff !important;
          color: #1a1a1a !important;
        }
        /* Guest 「Sign in required」gates: hide stock Ticker / Yahoo industry chrome */
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed)
          .content-wrapper:has(#shiny-tab-lab_notes.active) .ynow-stock-chrome,
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed)
          .content-wrapper:has(#shiny-tab-decision_checklist.active) .ynow-stock-chrome,
          .content-wrapper:has(#shiny-tab-mos_assumptions.active) .ynow-stock-chrome,
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed)
          .content-wrapper:has(#shiny-tab-price_alerts.active) .ynow-stock-chrome {
          display: none !important;
        }
        body.ynow-sysadmin-on .ynow-sysadmin-login-chrome,
        body.ynow-sysadmin-on .ynow-sysadmin-login-row {
          display: none !important;
        }
        /* Belt-and-suspenders: never show the locked hint while unlocked */
        body.ynow-sysadmin-on #ynow_pts_sysadmin_need_enter {
          display: none !important;
        }
        /* One control-panel bar (cohesive strip) — stays visible in self-view */
        .ynow-sysadmin-control-panel {
          display: none;
          margin-top: 8px;
          width: 100%;
          box-sizing: border-box;
          padding: 8px 10px;
          border: 1px solid #d6d6d6;
          border-radius: 6px;
          background: linear-gradient(180deg, #f7f7f8 0%, #efefef 100%);
          box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.7);
        }
        .ynow-sysadmin-control-panel__inner {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 8px 10px;
          width: 100%;
        }
        .ynow-sysadmin-control-panel__cluster {
          display: inline-flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 6px;
        }
        .ynow-sysadmin-control-panel__rule {
          display: inline-block;
          width: 1px;
          align-self: stretch;
          min-height: 28px;
          background: #cfcfcf;
          margin: 0 2px;
        }
        .ynow-sysadmin-control-panel__label {
          font-weight: 600;
          color: #444;
          font-size: 13px;
          margin-right: 2px;
          white-space: nowrap;
        }
        .ynow-sysadmin-control-panel .btn {
          margin: 0;
        }
        .ynow-sysadmin-control-panel__exit-self {
          display: none !important;
        }
        body.ynow-self-view .ynow-sysadmin-control-panel__exit-self {
          display: inline-block !important;
        }
        /* Active self-view mode cue on the bar (signed-in = default, no button) */
        body.ynow-self-view-guest #ynow_pts_sysadmin_self_guest,
        body.ynow-omniscient #ynow_pts_sysadmin_self_omniscient {
          box-shadow: inset 0 0 0 1px #888;
          background: #e8e8e8 !important;
        }
        body.ynow-sysadmin-on:not(.ynow-view-as) .ynow-sysadmin-control-panel {
          display: flex !important;
        }
        body.ynow-sysadmin-on:not(.ynow-view-as) #ynow_pts_sysadmin_self_help {
          display: block !important;
        }
        /* System admin: 「套用變更」/ Apply changes → red bg + white text */
        /* Currently restricted — clear list under apply target (not buried prose). */
        .ynow-access-restricted-panel {
          margin: 8px 0 12px;
          padding: 8px 10px;
          border: 1px solid rgba(0, 0, 0, 0.12);
          border-radius: 4px;
          background: rgba(0, 0, 0, 0.03);
          font-size: 12.5px;
        }
        .ynow-access-restricted-panel--apply {
          margin-top: 6px;
          background: rgba(217, 83, 79, 0.06);
          border-color: rgba(217, 83, 79, 0.25);
        }
        .ynow-access-restricted-panel__title {
          margin-bottom: 6px;
        }
        .ynow-access-restricted-panel__list {
          margin: 0;
          padding-left: 1.2em;
        }
        .ynow-access-restricted-panel__item,
        .ynow-access-restricted-panel__none {
          margin: 2px 0;
        }
        .ynow-access-restricted-panel__block + .ynow-access-restricted-panel__block {
          margin-top: 8px;
          padding-top: 6px;
          border-top: 1px dashed rgba(0, 0, 0, 0.1);
        }
        .ynow-access-restricted-panel__uid {
          font-weight: 600;
          margin-bottom: 2px;
        }
        .ynow-access-restricted-panel__empty {
          margin: 0;
        }
        .ynow-pts-sysadmin-box .btn.ynow-sysadmin-btn-apply,
        .ynow-pts-sysadmin-box .btn.btn-warning.ynow-sysadmin-btn-apply,
        .ynow-pts-sysadmin-box .btn.btn-danger.ynow-sysadmin-btn-apply {
          background: #c0392b !important;
          border: 1px solid #a93226 !important;
          color: #fff !important;
          box-shadow: none !important;
        }
        .ynow-pts-sysadmin-box .btn.ynow-sysadmin-btn-apply:hover,
        .ynow-pts-sysadmin-box .btn.ynow-sysadmin-btn-apply:focus,
        .ynow-pts-sysadmin-box .btn.ynow-sysadmin-btn-apply:active {
          background: #a93226 !important;
          border-color: #922b21 !important;
          color: #fff !important;
        }
        .ynow-pts-sysadmin-box .btn.ynow-sysadmin-btn-apply span {
          color: #fff !important;
        }
        /* Omniscient: same bg as Guest (btn-default); warning-red text */
        .ynow-sysadmin-self-view-group .btn.ynow-sysadmin-btn-omniscient,
        .ynow-sysadmin-self-view-group .btn.btn-default.ynow-sysadmin-btn-omniscient {
          color: #DC3545 !important;
        }
        .ynow-sysadmin-self-view-group .btn.ynow-sysadmin-btn-omniscient span {
          color: #DC3545 !important;
        }
        /* View-as: hide control panel + grant workspace. Self-view: hide grant only. */
        body.ynow-view-as .ynow-sysadmin-control-panel,
        body.ynow-view-as .ynow-sysadmin-unlocked-actions,
        body.ynow-view-as .ynow-pts-sysadmin-box,
        body.ynow-view-as .ynow-sysadmin-login-chrome,
        body.ynow-view-as #ynow_pts_sysadmin_self_help,
        body.ynow-self-view .ynow-pts-sysadmin-box,
        body.ynow-self-view .ynow-sysadmin-login-chrome {
          display: none !important;
        }
        @media (max-width: 767px) {
          .ynow-sysadmin-control-panel__inner {
            width: 100%;
          }
          .ynow-sysadmin-control-panel__rule {
            display: none;
          }
          .ynow-sysadmin-control-panel .btn {
            flex: 1 1 auto;
            min-width: 40%;
          }
        }
        /* Pending deploy: flame title must override gold fill/shine (place after gold rules) */
        body.ynow-update-available .main-header .logo .ynow-app-title-base {
          color: rgba(192, 57, 43, 0.38) !important;
          -webkit-text-fill-color: rgba(192, 57, 43, 0.38) !important;
          animation: none !important;
        }
        body.ynow-update-available .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill,
        body.ynow-update-available .main-header .logo .ynow-app-title-fill {
          width: 100% !important;
        }
        body.ynow-update-available .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner,
        body.ynow-update-available .main-header .logo .ynow-app-title-fill-inner {
          color: transparent !important;
          background-color: transparent !important;
          background-image: var(--ynow-htcdi-flame-gradient) !important;
          background-size: 220% 100% !important;
          background-repeat: no-repeat !important;
          -webkit-background-clip: text !important;
          background-clip: text !important;
          -webkit-text-fill-color: transparent !important;
          animation: ynow-htcdi-flame-flow 2.4s ease-in-out infinite !important;
          text-decoration: none !important;
          filter: none !important;
        }
        @supports not ((-webkit-background-clip: text) or (background-clip: text)) {
          body.ynow-update-available .main-header .logo .ynow-app-title-fill-inner,
          body.ynow-update-available .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner {
            -webkit-text-fill-color: #E67E22 !important;
            color: #E67E22 !important;
            background-image: none !important;
            animation: none !important;
            text-shadow:
              0 0 6px rgba(255, 120, 40, 0.85),
              0 0 14px rgba(192, 57, 43, 0.55),
              0 1px 2px rgba(0, 0, 0, 0.85);
          }
          body.ynow-update-available .main-header .logo .ynow-app-title-base {
            color: rgba(192, 57, 43, 0.45) !important;
            -webkit-text-fill-color: rgba(192, 57, 43, 0.45) !important;
          }
        }
        @media (prefers-reduced-motion: reduce) {
          body.ynow-update-available .main-header .logo .ynow-app-title-fill-inner,
          body.ynow-update-available .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner {
            animation: none !important;
            background-position: 0% 50% !important;
          }
        }
        /* View-as silver must win over gold AND deploy-flame (impersonation signal) */
        body.ynow-view-as .main-header .logo,
        body.ynow-view-as.ynow-update-available .main-header .logo {
          color: var(--ynow-silver, #C0C0C0) !important;
        }
        body.ynow-view-as .main-header .logo .ynow-app-title-base,
        body.ynow-view-as.ynow-update-available .main-header .logo .ynow-app-title-base {
          color: rgba(192, 192, 192, 0.42) !important;
          -webkit-text-fill-color: rgba(192, 192, 192, 0.42) !important;
          animation: none !important;
        }
        body.ynow-view-as .main-header .logo .ynow-app-title-fill-inner,
        body.ynow-view-as .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner,
        body.ynow-view-as.ynow-update-available .main-header .logo .ynow-app-title-fill-inner,
        body.ynow-view-as.ynow-update-available .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner {
          color: var(--ynow-silver, #C0C0C0) !important;
          background-image: var(--ynow-silver-gradient) !important;
          background-size: 220% 100% !important;
          background-repeat: no-repeat !important;
          -webkit-background-clip: text !important;
          background-clip: text !important;
          -webkit-text-fill-color: transparent !important;
          animation: ynow-silver-shine 2.6s ease-in-out infinite !important;
          filter: none !important;
        }
        body.ynow-view-as .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill,
        body.ynow-view-as .main-header .logo .ynow-app-title-fill {
          width: 100% !important;
        }
        /* Logo strip underlay: solid black so veins never paint through wordmark */
        .skin-black .main-header .logo::before {
          content: "";
          position: absolute;
          inset: 0;
          z-index: -1;
          pointer-events: none;
          background-color: #000 !important;
          background-image: none !important;
        }

        .content-wrapper, .right-side { background-color: #f7f7f7; }
        /* 網頁／手機共用：黑色頁首橫列置頂固定，捲動不滑掉 */
        .main-header {
          position: fixed !important;
          top: 0 !important;
          left: 0 !important;
          right: 0 !important;
          width: 100% !important;
          z-index: 1030 !important;
          overflow: hidden;
          background-color: #000 !important;
        }
        /* Black top-banner left (A+B): line-stroke veins flush left of .main-header.
           Asset: www/ynow-chrome-veins-branches.png. Logo/title strip is solid #000
           (blocks veins + AdminLTE skin-black white). Mask still clears wordmark band. */
        .ynow-chrome-header-leaves {
          position: absolute;
          left: 0;
          top: 0;
          bottom: 0;
          width: min(42%, 520px);
          /* Above navbar fill (z3); below .logo (z5). Mask excludes wordmark column. */
          z-index: 4;
          pointer-events: none;
          overflow: hidden;
          background: transparent;
          opacity: 0.95;
          animation: ynow-chrome-green-pulse 4.8s ease-in-out infinite;
          /* Far-left gutter (~0–40px) + resume after logo (~250px); hole under title text */
          -webkit-mask-image: linear-gradient(
            90deg,
            #000 0%,
            #000 32px,
            transparent 48px,
            transparent 248px,
            #000 252px,
            #000 100%
          );
          mask-image: linear-gradient(
            90deg,
            #000 0%,
            #000 32px,
            transparent 48px,
            transparent 248px,
            #000 252px,
            #000 100%
          );
        }
        .ynow-chrome-header-veins {
          position: absolute;
          left: 0;
          top: -40%;
          width: 100%;
          height: 180%;
          object-fit: cover;
          object-position: left 42%;
          /* PNG is already green strokes on transparent — soft glow only, no invert/fill */
          filter: drop-shadow(0 0 3px rgba(36, 156, 96, 0.4));
          opacity: 0.9;
          -webkit-mask-image: linear-gradient(90deg, #000 0%, #000 62%, transparent 100%);
          mask-image: linear-gradient(90deg, #000 0%, #000 62%, transparent 100%);
          pointer-events: none;
          user-select: none;
        }
        @keyframes ynow-chrome-green-pulse {
          0%, 100% { opacity: 0.72; }
          50% { opacity: 0.95; }
        }
        /* Solid black brand strip — idle / hover / focus / collapse+scroll must match.
           Overrides AdminLTE .skin-black .main-header > .logo { background:#ffffff }.
           Keep z-index above veins; no pattern through wordmark. */
        .skin-black .main-header .logo,
        .skin-black .main-header > .logo,
        .skin-black .main-header .logo:hover,
        .skin-black .main-header > .logo:hover,
        .skin-black .main-header .logo:focus,
        .skin-black .main-header .logo:active,
        .skin-black.sidebar-collapse .main-header .logo,
        .skin-black.sidebar-collapse .main-header > .logo,
        .skin-black.sidebar-mini.sidebar-collapse .main-header .logo,
        .skin-black.sidebar-mini.sidebar-collapse .main-header > .logo {
          background-color: #000 !important;
          background-image: none !important;
          z-index: 5 !important;
        }
        .skin-black .main-header .logo::before,
        .skin-black .main-header .logo:hover::before,
        .skin-black.sidebar-collapse .main-header .logo::before {
          content: "";
          position: absolute;
          inset: 0;
          z-index: -1;
          pointer-events: none;
          background-color: #000 !important;
          background-image: none !important;
        }
        /* No padding on .ynow-app-title: #404 pill padding offset the absolute fill vs
           in-flow base and painted a second ghost wordmark. Keep load-bar layers aligned.
           Shrink-wrap to the full wordmark (incl. trailing version digit); do not let
           AdminLTE .logo overflow:hidden + 15px pad clip 「…v26.xx」. Black strip, no pattern. */
        .main-header .logo,
        .main-header .logo:hover {
          overflow: visible !important;
          padding-left: 8px !important;
          padding-right: 8px !important;
          box-sizing: border-box;
          background-color: #000 !important;
          background-image: none !important;
        }
        .main-header .logo .ynow-app-title,
        .main-header .logo:hover .ynow-app-title {
          position: relative;
          z-index: 6;
          display: inline-block;
          width: max-content;
          max-width: none;
          box-sizing: border-box;
          padding: 0;
          margin: 0 4px;
          border-radius: 0;
          background-color: transparent !important;
          background-image: none !important;
          box-shadow: none;
          white-space: nowrap;
          overflow: visible;
        }
        .main-header .logo .ynow-app-title-base,
        .main-header .logo .ynow-app-title-fill-inner {
          white-space: nowrap;
        }
        /* After load completes, hide the faint base track so only one wordmark remains. */
        .main-header .logo .ynow-app-title.is-complete .ynow-app-title-base {
          visibility: hidden;
        }
        .main-header > .logo,
        .main-header > .logo:hover {
          position: relative;
          z-index: 5 !important;
        }
        .main-header > .navbar {
          position: relative;
          z-index: 3;
        }
        .main-header .navbar > .sidebar-toggle,
        .main-header .navbar .nav {
          position: relative;
          z-index: 5;
        }
        @media (prefers-reduced-motion: reduce) {
          .ynow-chrome-header-leaves {
            animation: none;
            opacity: 0.88;
          }
        }
        @media (max-width: 767px) {
          .ynow-chrome-header-leaves {
            left: 0;
            width: min(70%, 360px);
            /* Mobile: far-left strokes only — keep clear of centered wordmark */
            -webkit-mask-image: linear-gradient(90deg, #000 0%, #000 16%, transparent 28%);
            mask-image: linear-gradient(90deg, #000 0%, #000 16%, transparent 28%);
          }
          .main-header .logo .ynow-app-title {
            margin: 0 4px;
            padding: 0;
            background-color: transparent !important;
          }
        }
        .content-wrapper,
        .right-side {
          margin-top: 50px !important;
        }
        /* Pin sidebar to the viewport and scroll inside .sidebar so the
           bottom Lite brand (#ynow_lite_toggle) is reachable without scrolling
           the whole .wrapper. Desktop: Bootstrap col-* floats collapse .sidebar
           height to padding-bottom:10px with overflow:visible — brand sat below vh.
           Mobile: same internal scroll when the off-canvas sidebar is open. */
        @media (min-width: 768px) {
          .main-sidebar,
          .skin-black .main-sidebar {
            position: fixed !important;
            top: 0 !important;
            left: 0 !important;
            height: 100vh !important;
            min-height: 100vh !important;
            max-height: 100vh !important;
            padding-top: 50px !important;
            overflow: hidden !important;
          }
          .main-sidebar .sidebar,
          .main-sidebar #sidebarItemExpanded {
            height: calc(100vh - 50px) !important;
            max-height: calc(100vh - 50px) !important;
            overflow-x: hidden !important;
            overflow-y: auto !important;
            -webkit-overflow-scrolling: touch;
            box-sizing: border-box !important;
          }
          .main-sidebar .sidebar::after {
            content: "";
            display: table;
            clear: both;
          }
        }
        @media (max-width: 767px) {
          .main-sidebar,
          .skin-black .main-sidebar {
            overflow: hidden !important;
          }
          .main-sidebar .sidebar,
          .main-sidebar #sidebarItemExpanded {
            height: calc(100vh - 50px) !important;
            max-height: calc(100vh - 50px) !important;
            overflow-x: hidden !important;
            overflow-y: auto !important;
            -webkit-overflow-scrolling: touch;
            box-sizing: border-box !important;
          }
          .main-sidebar .sidebar::after {
            content: "";
            display: table;
            clear: both;
          }
        }
        /* Reserve band under fixed header for credit (left) + USD/TWD (right) */
        .content-wrapper > .content {
          padding-top: 52px;
        }
        /* All sidebar tabs share Macro & Market Trends content width + center */
        .content-wrapper > .content > .tab-content {
          width: 100%;
          max-width: 100%;
        }
        .content-wrapper > .content > .tab-content > .tab-pane {
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin-left: auto;
          margin-right: auto;
          box-sizing: border-box;
        }
        .content-wrapper > .content > .tab-content > .tab-pane .box,
        .content-wrapper > .content > .tab-content > .tab-pane .nav-tabs-custom {
          max-width: 100%;
        }
        /* Keep Plotly / DT / images fluid inside the shared column */
        .content-wrapper > .content > .tab-content > .tab-pane .plotly,
        .content-wrapper > .content > .tab-content > .tab-pane .html-widget,
        .content-wrapper > .content > .tab-content > .tab-pane .dataTables_wrapper,
        .content-wrapper > .content > .tab-content > .tab-pane img {
          max-width: 100%;
        }
        @media (max-width: 767px) {
          .content-wrapper > .content > .tab-content > .tab-pane {
            max-width: 100%;
            padding-left: 0;
            padding-right: 0;
          }
        }
        .skin-black .main-header .navbar {
          background-color: var(--ynow-ink) !important;
          position: relative;
        }
        /* 側欄漢堡：僅三條白線，無外框／無 FA 字型邊框；整顆按鈕與線條皆垂直置中 */
        .main-header .navbar > .sidebar-toggle,
        .skin-black .main-header .navbar .sidebar-toggle {
          color: transparent !important;
          border: none !important;
          border-right: none !important;
          box-shadow: none !important;
          outline: none !important;
          background-color: transparent !important;
          background-image: none !important;
          position: relative !important;
          box-sizing: border-box !important;
          float: left;
          width: 44px;
          min-width: 44px;
          height: 50px !important;
          min-height: 50px !important;
          max-height: 50px !important;
          padding: 0 !important;
          margin: 0 !important;
          display: inline-flex !important;
          align-items: center !important;
          justify-content: center !important;
          line-height: 0 !important;
        }
        .main-header .navbar > .sidebar-toggle:before,
        .skin-black .main-header .navbar .sidebar-toggle:before {
          content: "" !important;
          display: block !important;
          width: 18px;
          height: 14px;
          margin: 0;
          padding: 0;
          border: none !important;
          border-radius: 0 !important;
          /* 14px 高的三線 icon，flex 可正確垂直置中（勿用 2px+box-shadow） */
          background-color: transparent !important;
          background-image: linear-gradient(
            to bottom,
            #fff 0px, #fff 2px,
            transparent 2px, transparent 6px,
            #fff 6px, #fff 8px,
            transparent 8px, transparent 12px,
            #fff 12px, #fff 14px
          ) !important;
          box-shadow: none !important;
          position: relative !important;
          left: auto !important;
          top: auto !important;
          transform: none !important;
          font-family: none !important;
          font-weight: normal !important;
          line-height: 0 !important;
        }
        /* Locale blocks (Valuation Methodology tables, etc.).
           Default / en / missing lang → hide Chinese; zh* → hide English. */
        html:not([lang^="zh"]) .ynow-loc-zh { display: none !important; }
        html[lang^="zh"] .ynow-loc-en { display: none !important; }
        /* Prefer zh: hide English chrome until boot applyUiLocale rewrites menus
           (avoids EN→繁中 flash when navigator.languages prefers Chinese). */
        html[data-ynow-boot-zh="1"]:not([data-ynow-locale-ready="1"]) .sidebar-menu,
        html[data-ynow-boot-zh="1"]:not([data-ynow-locale-ready="1"]) .ynow-lang-header {
          visibility: hidden;
        }
        .main-header .navbar > .sidebar-toggle .icon-bar,
        .skin-black .main-header .navbar .sidebar-toggle .icon-bar {
          display: none !important;
        }
        .main-header .navbar > .sidebar-toggle .fa,
        .main-header .navbar > .sidebar-toggle .fas,
        .skin-black .main-header .navbar .sidebar-toggle .fa,
        .skin-black .main-header .navbar .sidebar-toggle .fas {
          display: none !important;
        }
        .skin-black .main-header .navbar .sidebar-toggle:hover,
        .skin-black .main-header .navbar .sidebar-toggle:focus,
        .skin-black .main-header .navbar .sidebar-toggle:active {
          background-color: transparent !important;
          color: transparent !important;
          border: none !important;
          box-shadow: none !important;
        }
        .skin-black .main-header .navbar .sidebar-toggle:hover:before,
        .skin-black .main-header .navbar .sidebar-toggle:focus:before {
          background-color: transparent !important;
          background-image: linear-gradient(
            to bottom,
            #fff 0px, #fff 2px,
            transparent 2px, transparent 6px,
            #fff 6px, #fff 8px,
            transparent 8px, transparent 12px,
            #fff 12px, #fff 14px
          ) !important;
          box-shadow: none !important;
        }

        /* 美股／台股：釘在三線 icon 右側上下堆疊（滿 50px／各 25px）；國旗等寬等高。
           absolute + JS left 可在 DOM 搬移前／後都貼齊 toggle（避免停在右欄 custom-menu）。 */
        .main-header .navbar {
          position: relative !important;
        }
        .main-header .navbar > .sidebar-toggle,
        .skin-black .main-header .navbar .sidebar-toggle {
          padding-right: 0 !important;
          margin-right: 0 !important;
        }
        .main-header .navbar > #ynow-market-header.ynow-market-header,
        .main-header .navbar #ynow-market-header.ynow-market-header,
        .main-header .navbar-custom-menu #ynow-market-header.ynow-market-header,
        .main-header .navbar-custom-menu .navbar-nav > li#ynow-market-header.ynow-market-header {
          position: absolute !important;
          left: 44px !important; /* fallback；JS 依 toggle.offsetWidth 覆寫 */
          right: auto !important;
          top: 0 !important;
          float: none !important;
          /* Same fly length for both flags; stack height = full 50px header.
             W = 50×57/68 so US(19:10)+TW(3:2) heights sum to 50 at shared width. */
          width: calc(50px * 57 / 68) !important;
          height: 50px !important;
          margin: 0 !important;
          padding: 0 !important;
          list-style: none !important;
          display: block !important;
          visibility: visible !important;
          z-index: 1040;
        }
        /* Flag stack: shared length, native aspect each, combined hoist = 50px, no seam.
           Height share US:TW = (10/19):(2/3) = 15:19. */
        #ynow-market-header .ynow-market-stack {
          display: flex;
          flex-direction: column;
          width: calc(50px * 57 / 68);
          height: 50px;
          margin: 0;
          padding: 0;
          gap: 0;
          overflow: hidden;
          border: none;
          background: transparent;
        }
        #ynow-market-header .ynow-mkt-btn {
          box-sizing: border-box;
          width: 100%;
          height: auto;
          min-height: 0;
          max-height: none;
          margin: 0;
          padding: 0;
          border: 0;
          border-radius: 0;
          background: transparent;
          color: #fff;
          font-size: 14px;
          font-weight: 400;
          line-height: 0;
          cursor: pointer;
          display: flex;
          align-items: stretch;
          justify-content: stretch;
          overflow: hidden;
        }
        /* viewBox height parts → flex grow (US 10/19, TW 12/18) */
        #ynow-market-header #ynow_mkt_btn_us {
          flex: 15 0 0;
        }
        #ynow-market-header #ynow_mkt_btn_tw {
          flex: 19 0 0;
        }
        #ynow-market-header .ynow-mkt-btn .ynow-mkt-flag {
          display: block;
          /* Cell aspect matches each flag → fill flush at shared length. */
          width: 100% !important;
          height: 100% !important;
          max-width: none;
          max-height: none;
          flex: 1 1 auto;
          margin: 0;
          padding: 0;
          pointer-events: none;
          /* Inactive market: black-and-white; active restores full color. */
          filter: grayscale(1);
          -webkit-filter: grayscale(1);
        }
        #ynow-market-header .ynow-mkt-btn + .ynow-mkt-btn {
          border: 0;
          border-top: 0;
          margin-top: 0;
        }
        #ynow-market-header .ynow-mkt-btn.active {
          background: transparent;
          color: #fff;
          border: 0;
        }
        #ynow-market-header .ynow-mkt-btn.active .ynow-mkt-flag {
          filter: none;
          -webkit-filter: none;
        }
        #ynow-market-header .ynow-mkt-btn:focus {
          outline: none;
        }

        /* 台股（桌面）：dashboardHeader 以中華民國國旗填滿；美股維持全黑。
           手機改回黑底（見下方 max-width: 767px 覆寫）。 */
        body.ynow-market-tw .main-header {
          position: relative;
          background-color: transparent !important;
        }
        body.ynow-market-tw .main-header::before {
          content: "";
          position: absolute;
          inset: 0;
          z-index: 0;
          pointer-events: none;
          background-image: url("roc_flag.svg");
          background-size: cover;
          background-position: center center;
          background-repeat: no-repeat;
        }
        body.ynow-market-tw .main-header::after {
          content: "";
          position: absolute;
          inset: 0;
          z-index: 0;
          pointer-events: none;
          background: rgba(0, 0, 0, 0.42);
        }
        body.ynow-market-tw .main-header > .logo,
        body.ynow-market-tw .main-header > .navbar {
          position: relative;
          z-index: 1;
        }
        body.ynow-market-tw .skin-black .main-header .navbar {
          background-color: transparent !important;
          background-image: none !important;
        }
        /* 台股 logo：容器透明讓國旗透出；金色仍在內層 .ynow-app-title */
        body.ynow-market-tw .skin-black .main-header .logo,
        body.ynow-market-tw .skin-black .main-header .logo:hover {
          background-color: transparent !important;
          background-image: none !important;
        }
        body.ynow-market-tw .skin-black .main-header .logo::before {
          display: none;
        }
        body.ynow-market-tw .main-header .navbar > .sidebar-toggle,
        body.ynow-market-tw .skin-black .main-header .navbar .sidebar-toggle {
          color: transparent !important;
          border: none !important;
          background-color: transparent !important;
        }
        body.ynow-market-tw .main-header .navbar > .sidebar-toggle:before,
        body.ynow-market-tw .skin-black .main-header .navbar .sidebar-toggle:before {
          background-color: transparent !important;
          background-image: linear-gradient(
            to bottom,
            #fff 0px, #fff 2px,
            transparent 2px, transparent 6px,
            #fff 6px, #fff 8px,
            transparent 8px, transparent 12px,
            #fff 12px, #fff 14px
          ) !important;
          box-shadow: none !important;
        }
        body.ynow-market-tw .main-header .navbar > .sidebar-toggle .icon-bar,
        body.ynow-market-tw .skin-black .main-header .navbar .sidebar-toggle .icon-bar {
          display: none !important;
        }
        body.ynow-market-tw .skin-black .main-header .navbar .sidebar-toggle:hover {
          background-color: transparent !important;
          color: transparent !important;
        }
        body.ynow-market-tw .main-header .logo,
        body.ynow-market-tw .main-header .logo:hover {
          filter: none !important;
        }
        body.ynow-market-tw .main-header .logo .ynow-app-title,
        body.ynow-market-tw .main-header .logo .ynow-app-title-fill-inner {
          filter: none !important;
        }
        body.ynow-market-tw .main-header .logo .ynow-app-title-fill-inner {
          text-shadow: 0 1px 3px rgba(0, 0, 0, 0.65);
        }
        body.ynow-market-tw .main-header .navbar .nav > li > a,
        body.ynow-market-tw .ynow-market-header,
        body.ynow-market-tw .ynow-lang-header {
          color: #fff !important;
          text-shadow: 0 1px 3px rgba(0, 0, 0, 0.85);
        }
        .skin-black .wrapper,
        .skin-black .main-sidebar,
        .skin-black .left-side {
          background-color: var(--ynow-ink) !important;
        }
        .skin-black .sidebar-menu > li:hover > a,
        .skin-black .sidebar-menu > li.active > a {
          background: #000 !important;
          border-left-color: #fff !important;
        }
        .box.box-primary { border-top-color: var(--ynow-ink) !important; }
        .box.box-solid.box-primary > .box-header {
          color: #fff !important;
          background: var(--ynow-ink) !important;
          background-color: var(--ynow-ink) !important;
        }
        .box.box-solid.box-primary {
          border: 1px solid var(--ynow-ink) !important;
        }
        /* Asset transmission: no outer frame around the Testing panel. */
        .box.box-solid.box-primary.ynow-atx-panel,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-primary.ynow-atx-panel,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-primary.ynow-atx-panel,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-primary.ynow-atx-panel {
          border: none !important;
          box-shadow: none !important;
        }
        .box.box-solid.box-primary > .box-header a,
        .box.box-solid.box-primary > .box-header .btn {
          color: #fff !important;
        }
        .box.box-info { border-top-color: var(--ynow-ink-soft) !important; }
        .box.box-solid.box-info > .box-header {
          color: #fff !important;
          background: var(--ynow-ink-soft) !important;
          background-color: var(--ynow-ink-soft) !important;
        }
        .box.box-solid.box-info {
          border: 1px solid var(--ynow-ink-soft) !important;
        }
        .box.box-solid.box-info > .box-header a,
        .box.box-solid.box-info > .box-header .btn {
          color: #fff !important;
        }
        .btn-primary {
          background-color: var(--ynow-ink) !important;
          border-color: #000 !important;
          color: #fff !important;
        }
        .btn-primary:hover,
        .btn-primary:focus,
        .btn-primary:active,
        .btn-primary.active,
        .open > .dropdown-toggle.btn-primary {
          background-color: #000 !important;
          border-color: #000 !important;
          color: #fff !important;
        }
        .btn-info {
          background-color: var(--ynow-ink-soft) !important;
          border-color: #222 !important;
          color: #fff !important;
        }
        .btn-info:hover,
        .btn-info:focus,
        .btn-info:active,
        .btn-info.active {
          background-color: #111 !important;
          border-color: #000 !important;
          color: #fff !important;
        }
        /* Valuation「試算」預設 logo 綠白字；開著模型分頁時改由該模型主色覆寫 */
        .btn.ynow-btn-calc,
        .btn.ynow-btn-calc:focus,
        .btn.ynow-btn-calc:active,
        .btn.ynow-btn-calc.active,
        .btn.ynow-btn-calc:visited {
          background-color: var(--ynow-logo-green) !important;
          background-image: none !important;
          border-color: #1e7a4c !important;
          color: #ffffff !important;
          text-shadow: none !important;
        }
        .btn.ynow-btn-calc:hover {
          background-color: #1e8552 !important;
          background-image: none !important;
          border-color: #196b42 !important;
          color: #ffffff !important;
        }
        .btn.ynow-btn-calc .fa,
        .btn.ynow-btn-calc .fas {
          color: #ffffff !important;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: var(--ynow-ink) !important;
        }
        .nav-tabs-custom > .nav-tabs > li.active > a,
        .nav-tabs-custom > .nav-tabs > li.active:hover > a {
          border-top-color: transparent;
          color: var(--ynow-ink) !important;
        }

        /* --- 估值模型頁色系（對齊 Model Selector 七卡；試算鈕另見各主題覆寫）--- */
        body.ynow-theme-nav { --ynow-model-accent: var(--ynow-model-nav); }
        body.ynow-theme-dcf { --ynow-model-accent: var(--ynow-model-dcf); }
        body.ynow-theme-ddm { --ynow-model-accent: var(--ynow-model-ddm); }
        body.ynow-theme-ri  { --ynow-model-accent: var(--ynow-model-ri); }
        body.ynow-theme-pb  { --ynow-model-accent: var(--ynow-model-pb); }
        body.ynow-theme-multiples { --ynow-model-accent: var(--ynow-model-multiples); }
        body.ynow-theme-sotp { --ynow-model-accent: var(--ynow-model-sotp); }

        /* Model Selector cards — same accent tokens as page themes */
        .ynow-model-card--nav { --ynow-card-accent: var(--ynow-model-nav); }
        .ynow-model-card--dcf { --ynow-card-accent: var(--ynow-model-dcf); }
        .ynow-model-card--ddm { --ynow-card-accent: var(--ynow-model-ddm); }
        .ynow-model-card--ri { --ynow-card-accent: var(--ynow-model-ri); }
        .ynow-model-card--pb { --ynow-card-accent: var(--ynow-model-pb); }
        .ynow-model-card--multiples { --ynow-card-accent: var(--ynow-model-multiples); }
        .ynow-model-card--sotp { --ynow-card-accent: var(--ynow-model-sotp); }
        .ynow-model-card__icon { color: var(--ynow-card-accent, #333); }
        .ynow-model-card--primary {
          border-color: var(--ynow-card-accent) !important;
          background: #fffaf2 !important;
        }
        .ynow-model-card--primary .ynow-model-card__badge {
          background: var(--ynow-card-accent) !important;
        }

        body.ynow-theme-model .content-wrapper .box.box-primary,
        body.ynow-theme-model .content-wrapper .box.box-info,
        body.ynow-theme-model .content-wrapper .box.box-success,
        body.ynow-theme-model .content-wrapper .box.box-warning,
        body.ynow-theme-model .content-wrapper .box.box-danger {
          border-top-color: var(--ynow-model-accent) !important;
        }
        body.ynow-theme-model .content-wrapper .box.box-solid.box-primary,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-info,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-success,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-warning,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-danger {
          border: 1px solid var(--ynow-model-accent) !important;
        }
        body.ynow-theme-model .content-wrapper .box.box-solid.box-primary > .box-header,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-info > .box-header,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-success > .box-header,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-warning > .box-header,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-danger > .box-header {
          color: #fff !important;
          background: var(--ynow-model-accent) !important;
          background-color: var(--ynow-model-accent) !important;
        }
        body.ynow-theme-model .content-wrapper .box.box-solid.box-primary > .box-header a,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-primary > .box-header .btn,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-info > .box-header a,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-info > .box-header .btn,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-success > .box-header a,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-success > .box-header .btn,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-warning > .box-header a,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-warning > .box-header .btn,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-danger > .box-header a,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-danger > .box-header .btn {
          color: #fff !important;
        }
        body.ynow-theme-model .content-wrapper .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: var(--ynow-model-accent) !important;
        }
        body.ynow-theme-model .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a,
        body.ynow-theme-model .content-wrapper .nav-tabs-custom > .nav-tabs > li.active:hover > a {
          color: var(--ynow-model-accent) !important;
        }
        /* Model KPI palette: result = full accent; siblings = same family, distinct mixes */
        body.ynow-theme-model {
          --ynow-model-kpi-result: var(--ynow-model-accent);
          --ynow-model-kpi-s2: color-mix(in srgb, var(--ynow-model-accent) 72%, #000000);
          --ynow-model-kpi-s3: color-mix(in srgb, var(--ynow-model-accent) 58%, #ffffff);
          --ynow-model-kpi-s4: color-mix(in srgb, var(--ynow-model-accent) 82%, #4a5568);
          --ynow-model-kpi-muted: color-mix(in srgb, var(--ynow-model-accent) 64%, #2c3e50);
        }
        body.ynow-theme-model .content-wrapper .small-box,
        body.ynow-theme-model .tab-content .small-box {
          background-color: var(--ynow-model-kpi-muted) !important;
        }
        body.ynow-theme-model .content-wrapper .info-box.bg-aqua,
        body.ynow-theme-model .content-wrapper .info-box.bg-green,
        body.ynow-theme-model .content-wrapper .info-box.bg-yellow,
        body.ynow-theme-model .content-wrapper .info-box.bg-red,
        body.ynow-theme-model .content-wrapper .info-box.bg-maroon,
        body.ynow-theme-model .content-wrapper .info-box.bg-purple,
        body.ynow-theme-model .content-wrapper .info-box.bg-navy,
        body.ynow-theme-model .content-wrapper .info-box.bg-teal,
        body.ynow-theme-model .content-wrapper .info-box.bg-olive,
        body.ynow-theme-model .content-wrapper .info-box.bg-orange,
        body.ynow-theme-model .content-wrapper .info-box.bg-light-blue,
        body.ynow-theme-model .content-wrapper .info-box.bg-black,
        body.ynow-theme-model .content-wrapper .info-box.bg-blue {
          background-color: var(--ynow-model-kpi-muted) !important;
        }
        body.ynow-theme-model .content-wrapper .info-box .info-box-icon {
          background-color: var(--ynow-model-kpi-muted) !important;
        }
        body.ynow-theme-model .ynow-model-kpi-result .small-box,
        body.ynow-theme-model .ynow-model-kpi-result .info-box,
        body.ynow-theme-model .ynow-model-kpi-result .info-box .info-box-icon {
          background-color: var(--ynow-model-kpi-result) !important;
        }
        /* Listed primary KPIs only: white fill + model-accent border / text
           (NAV fair, DCF/DDM per-share, RI FV, Jst. P/B fair, P/E Implied, SOTP Implied).
           Other result / sibling KPIs keep filled family tones above.
           Specificity must beat .content-wrapper .info-box.bg-* muted fills. */
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box,
        body.ynow-theme-model .tab-content .ynow-model-kpi-outline .small-box,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box,
        body.ynow-theme-model .tab-content .ynow-model-kpi-outline .info-box,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-aqua,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-green,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-yellow,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-red,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-maroon,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-purple,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-navy,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-teal,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-olive,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-orange,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-light-blue,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-black,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box.bg-blue,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-aqua,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-green,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-yellow,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-red,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-maroon,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-purple,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-navy,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-teal,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-olive,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-orange,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-light-blue,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box.bg-blue,
        body.ynow-theme-model .ynow-model-kpi-result.ynow-model-kpi-outline .small-box,
        body.ynow-theme-model .ynow-model-kpi-result.ynow-model-kpi-outline .info-box {
          background-color: #ffffff !important;
          background-image: none !important;
          border: 2px solid var(--ynow-model-accent) !important;
          color: var(--ynow-model-accent) !important;
          box-shadow: none !important;
        }
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box .info-box-icon,
        body.ynow-theme-model .ynow-model-kpi-result.ynow-model-kpi-outline .info-box .info-box-icon {
          background-color: #ffffff !important;
          color: var(--ynow-model-accent) !important;
        }
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box .inner h3,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box .inner p,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box .info-box-number,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box .info-box-text {
          color: var(--ynow-model-accent) !important;
        }
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box .icon,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box .icon > i,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box .info-box-icon > i {
          color: var(--ynow-model-accent) !important;
        }
        /* Final valuation numeral: filled KPI stays white; white box uses logo blue–green flow. */
        body.ynow-theme-model .ynow-model-kpi-result-num.ynow-htcdi-flow {
          background-image: none !important;
          -webkit-background-clip: border-box !important;
          background-clip: border-box !important;
          -webkit-text-fill-color: #ffffff !important;
          color: #ffffff !important;
          animation: none !important;
          font-weight: 800;
        }
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .small-box .inner h3 .ynow-model-kpi-result-num.ynow-htcdi-flow,
        body.ynow-theme-model .content-wrapper .ynow-model-kpi-outline .info-box .info-box-number .ynow-model-kpi-result-num.ynow-htcdi-flow,
        body.ynow-theme-model .ynow-model-kpi-result.ynow-model-kpi-outline .small-box .inner h3 .ynow-model-kpi-result-num.ynow-htcdi-flow,
        body.ynow-theme-model .ynow-model-kpi-result.ynow-model-kpi-outline .info-box .info-box-number .ynow-model-kpi-result-num.ynow-htcdi-flow {
          background-image: var(--ynow-logo-flow-gradient, linear-gradient(105deg, #0C5484 0%, #1AA8B8 50%, #249C60 100%)) !important;
          background-size: 220% 100% !important;
          -webkit-background-clip: text !important;
          background-clip: text !important;
          -webkit-text-fill-color: transparent !important;
          color: transparent !important;
          animation: ynow-logo-flow 2.6s ease-in-out infinite !important;
        }
        body.ynow-theme-model .ynow-model-kpi-tone-2 .small-box,
        body.ynow-theme-model .ynow-model-kpi-tone-2 .info-box,
        body.ynow-theme-model .ynow-model-kpi-tone-2 .info-box .info-box-icon {
          background-color: var(--ynow-model-kpi-s2) !important;
        }
        body.ynow-theme-model .ynow-model-kpi-tone-3 .small-box,
        body.ynow-theme-model .ynow-model-kpi-tone-3 .info-box,
        body.ynow-theme-model .ynow-model-kpi-tone-3 .info-box .info-box-icon {
          background-color: var(--ynow-model-kpi-s3) !important;
        }
        body.ynow-theme-model .ynow-model-kpi-tone-4 .small-box,
        body.ynow-theme-model .ynow-model-kpi-tone-4 .info-box,
        body.ynow-theme-model .ynow-model-kpi-tone-4 .info-box .info-box-icon {
          background-color: var(--ynow-model-kpi-s4) !important;
        }
        /* Sidebar active rail matches the open model tab */
        body.ynow-theme-model.skin-black .main-sidebar .sidebar-menu > li.active > a,
        body.ynow-theme-model.skin-black .main-sidebar .sidebar-menu > li.menu-open > a,
        body.ynow-theme-model.skin-black .main-sidebar .sidebar-menu .treeview-menu > li.active > a {
          border-left-color: var(--ynow-model-accent) !important;
        }
        /* 共用 Composite／years header：左側色條提示目前模型 */
        body.ynow-theme-model .ynow-header-composite-row,
        body.ynow-theme-model .ynow-dcf-mode-row,
        body.ynow-theme-model .ynow-header-years-suggest-row {
          border-left: 4px solid var(--ynow-model-accent);
          padding-left: 6px;
          margin-left: 0;
        }
        /* Waiting for valuation…：底色跟隨目前評價模型分頁主題色 */
        body.ynow-theme-model .ynow-header-composite-row .ynow-waiting-val.alert {
          background-color: var(--ynow-model-accent) !important;
          border-color: var(--ynow-model-accent) !important;
          color: #ffffff !important;
        }
        /* 試算／Run：未列入下列主題的按鈕維持 logo 綠；列入者改該模型主色白字 */
        body.ynow-theme-model .btn.ynow-btn-calc,
        body.ynow-theme-model .btn.ynow-btn-calc:hover,
        body.ynow-theme-model .btn.ynow-btn-calc:focus,
        body.ynow-theme-model .btn.ynow-btn-calc:active,
        body.ynow-theme-model .btn.ynow-btn-calc.active {
          background-color: var(--ynow-logo-green) !important;
          border-color: #1e7a4c !important;
          color: #ffffff !important;
        }
        body.ynow-theme-nav .btn.ynow-btn-calc,
        body.ynow-theme-nav .btn.ynow-btn-calc:hover,
        body.ynow-theme-nav .btn.ynow-btn-calc:focus,
        body.ynow-theme-nav .btn.ynow-btn-calc:active,
        body.ynow-theme-nav .btn.ynow-btn-calc.active,
        body.ynow-theme-dcf .btn.ynow-btn-calc,
        body.ynow-theme-dcf .btn.ynow-btn-calc:hover,
        body.ynow-theme-dcf .btn.ynow-btn-calc:focus,
        body.ynow-theme-dcf .btn.ynow-btn-calc:active,
        body.ynow-theme-dcf .btn.ynow-btn-calc.active,
        body.ynow-theme-ddm .btn.ynow-btn-calc,
        body.ynow-theme-ddm .btn.ynow-btn-calc:hover,
        body.ynow-theme-ddm .btn.ynow-btn-calc:focus,
        body.ynow-theme-ddm .btn.ynow-btn-calc:active,
        body.ynow-theme-ddm .btn.ynow-btn-calc.active,
        body.ynow-theme-ri .btn.ynow-btn-calc,
        body.ynow-theme-ri .btn.ynow-btn-calc:hover,
        body.ynow-theme-ri .btn.ynow-btn-calc:focus,
        body.ynow-theme-ri .btn.ynow-btn-calc:active,
        body.ynow-theme-ri .btn.ynow-btn-calc.active,
        body.ynow-theme-pb .btn.ynow-btn-calc,
        body.ynow-theme-pb .btn.ynow-btn-calc:hover,
        body.ynow-theme-pb .btn.ynow-btn-calc:focus,
        body.ynow-theme-pb .btn.ynow-btn-calc:active,
        body.ynow-theme-pb .btn.ynow-btn-calc.active,
        body.ynow-theme-multiples .btn.ynow-btn-calc,
        body.ynow-theme-multiples .btn.ynow-btn-calc:hover,
        body.ynow-theme-multiples .btn.ynow-btn-calc:focus,
        body.ynow-theme-multiples .btn.ynow-btn-calc:active,
        body.ynow-theme-multiples .btn.ynow-btn-calc.active,
        body.ynow-theme-sotp .btn.ynow-btn-calc,
        body.ynow-theme-sotp .btn.ynow-btn-calc:hover,
        body.ynow-theme-sotp .btn.ynow-btn-calc:focus,
        body.ynow-theme-sotp .btn.ynow-btn-calc:active,
        body.ynow-theme-sotp .btn.ynow-btn-calc.active {
          background-color: var(--ynow-model-accent) !important;
          background-image: none !important;
          border-color: var(--ynow-model-accent) !important;
          color: #ffffff !important;
        }
        body.ynow-theme-nav .btn.ynow-btn-calc .fa,
        body.ynow-theme-nav .btn.ynow-btn-calc .fas,
        body.ynow-theme-dcf .btn.ynow-btn-calc .fa,
        body.ynow-theme-dcf .btn.ynow-btn-calc .fas,
        body.ynow-theme-ddm .btn.ynow-btn-calc .fa,
        body.ynow-theme-ddm .btn.ynow-btn-calc .fas,
        body.ynow-theme-ri .btn.ynow-btn-calc .fa,
        body.ynow-theme-ri .btn.ynow-btn-calc .fas,
        body.ynow-theme-pb .btn.ynow-btn-calc .fa,
        body.ynow-theme-pb .btn.ynow-btn-calc .fas,
        body.ynow-theme-multiples .btn.ynow-btn-calc .fa,
        body.ynow-theme-multiples .btn.ynow-btn-calc .fas,
        body.ynow-theme-sotp .btn.ynow-btn-calc .fa,
        body.ynow-theme-sotp .btn.ynow-btn-calc .fas {
          color: #ffffff !important;
        }
        body.ynow-theme-model .btn.ynow-btn-reset,
        body.ynow-theme-model .btn.ynow-btn-reset:hover,
        body.ynow-theme-model .btn.ynow-btn-reset:focus,
        body.ynow-theme-model .btn.ynow-btn-reset:active {
          background-color: #7f8c8d !important;
          border-color: #6c757d !important;
          color: #ffffff !important;
        }

        /* --- Blue Chip：藍海色系（TW／US 共用，不隨市場模式變色）---
           Palette: #0F5A90 → #0A72AE → #33B1D2 → #65AEC7 → #8CD0EB */
        body.ynow-theme-bluechip {
          --ynow-bluechip-deep: #0F5A90;
          --ynow-bluechip-ocean: #0A72AE;
          --ynow-bluechip-cyan: #33B1D2;
          --ynow-bluechip-sky: #65AEC7;
          --ynow-bluechip-light: #8CD0EB;
          --ynow-bluechip-accent: var(--ynow-bluechip-ocean);
          --ynow-bluechip-accent-deep: var(--ynow-bluechip-deep);
          --ynow-bluechip-gradient: linear-gradient(
            105deg,
            #0F5A90 0%,
            #0A72AE 28%,
            #33B1D2 55%,
            #65AEC7 78%,
            #8CD0EB 100%
          );
          --ynow-bluechip-wash: linear-gradient(
            180deg,
            rgba(15, 90, 144, 0.09) 0%,
            rgba(51, 177, 210, 0.06) 42%,
            rgba(140, 208, 235, 0.04) 100%
          );
          accent-color: var(--ynow-bluechip-ocean);
        }
        body.ynow-theme-bluechip .content-wrapper {
          background-color: #f3f8fc !important;
          background-image: var(--ynow-bluechip-wash) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .box.box-primary,
        body.ynow-theme-bluechip .content-wrapper .box.box-info,
        body.ynow-theme-bluechip .content-wrapper .box.box-success,
        body.ynow-theme-bluechip .content-wrapper .box.box-warning {
          border-top-color: var(--ynow-bluechip-accent) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-primary,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-info,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-success,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-warning {
          border: 1px solid var(--ynow-bluechip-accent) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-primary > .box-header,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-info > .box-header,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-success > .box-header,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-warning > .box-header {
          color: #fff !important;
          background: var(--ynow-bluechip-gradient) !important;
          background-color: var(--ynow-bluechip-accent) !important;
          border-bottom-color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-primary > .box-header a,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-primary > .box-header .btn,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-info > .box-header a,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-info > .box-header .btn,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-success > .box-header a,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-success > .box-header .btn,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-warning > .box-header a,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-warning > .box-header .btn {
          color: #fff !important;
        }
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: var(--ynow-bluechip-cyan) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a,
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.active:hover > a {
          color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li > a > .fa,
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li > a > .fas {
          color: var(--ynow-bluechip-ocean);
        }
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a > .fa,
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a > .fas {
          color: var(--ynow-bluechip-deep);
        }
        /* tabBox title strip */
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.header {
          color: var(--ynow-bluechip-deep) !important;
        }
        /* Custom screen (自訂選股) — ocean accents inside Blue Chip */
        body.ynow-theme-bluechip .ynow-scr-dnd-bluechip .ynow-scr-cat-btn.is-active {
          border-left-color: var(--ynow-bluechip-cyan) !important;
          color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .ynow-scr-dnd-bluechip .ynow-scr-pack-btn.is-active {
          border-color: var(--ynow-bluechip-ocean) !important;
          background: rgba(140, 208, 235, 0.22) !important;
          color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .ynow-scr-dnd-bluechip .ynow-scr-card-add:hover,
        body.ynow-theme-bluechip .ynow-scr-dnd-bluechip .ynow-scr-card-remove:hover {
          border-color: var(--ynow-bluechip-ocean) !important;
          color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .ynow-scr-dnd-bluechip .ynow-scr-list-selected.ynow-scr-drop-active {
          outline-color: var(--ynow-bluechip-cyan) !important;
          background: rgba(140, 208, 235, 0.18) !important;
        }
        body.ynow-theme-bluechip .ynow-scr-dnd-bluechip .ynow-scr-dnd-cats {
          background: rgba(15, 90, 144, 0.06);
          border-color: rgba(10, 114, 174, 0.28);
        }
        body.ynow-theme-bluechip .ynow-scr-dnd-bluechip .ynow-scr-dnd-mid,
        body.ynow-theme-bluechip .ynow-scr-dnd-bluechip .ynow-scr-dnd-right {
          border-color: rgba(10, 114, 174, 0.28);
        }
        @media (max-width: 768px) {
          body.ynow-theme-bluechip .ynow-scr-dnd-bluechip .ynow-scr-cat-btn.is-active {
            border-color: var(--ynow-bluechip-ocean) !important;
            background: rgba(140, 208, 235, 0.22) !important;
          }
        }
        body.ynow-theme-bluechip .content-wrapper .ynow-lab-im-pool-controls {
          border-left: 4px solid var(--ynow-bluechip-cyan);
          padding: 10px 12px;
          margin-bottom: 14px;
          border-radius: 0 6px 6px 0;
          background: linear-gradient(
            90deg,
            rgba(15, 90, 144, 0.08) 0%,
            rgba(140, 208, 235, 0.12) 100%
          );
        }
        /* Primary actions — ocean (incl. former green「搜尋績優股」) */
        body.ynow-theme-bluechip .content-wrapper .btn-success,
        body.ynow-theme-bluechip .content-wrapper .btn-success:focus,
        body.ynow-theme-bluechip .content-wrapper .btn-success:active,
        body.ynow-theme-bluechip .content-wrapper .btn-success.active,
        body.ynow-theme-bluechip .content-wrapper #lab_im_run_fscore,
        body.ynow-theme-bluechip .content-wrapper #lab_im_run_fscore:focus,
        body.ynow-theme-bluechip .content-wrapper #lab_im_run_fscore:active {
          background-color: var(--ynow-bluechip-ocean) !important;
          border-color: var(--ynow-bluechip-deep) !important;
          color: #fff !important;
          background-image: none !important;
        }
        body.ynow-theme-bluechip .content-wrapper .btn-success:hover,
        body.ynow-theme-bluechip .content-wrapper #lab_im_run_fscore:hover {
          background-color: var(--ynow-bluechip-deep) !important;
          border-color: var(--ynow-bluechip-deep) !important;
          color: #fff !important;
        }
        body.ynow-theme-bluechip .content-wrapper .btn-primary,
        body.ynow-theme-bluechip .content-wrapper .btn-info {
          background-color: var(--ynow-bluechip-ocean) !important;
          border-color: var(--ynow-bluechip-deep) !important;
          color: #fff !important;
        }
        body.ynow-theme-bluechip .content-wrapper .btn-primary:hover,
        body.ynow-theme-bluechip .content-wrapper .btn-info:hover {
          background-color: var(--ynow-bluechip-deep) !important;
          border-color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .btn-default {
          border-color: var(--ynow-bluechip-sky) !important;
          color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .btn-default:hover {
          background-color: rgba(140, 208, 235, 0.25) !important;
          border-color: var(--ynow-bluechip-cyan) !important;
          color: var(--ynow-bluechip-deep) !important;
        }
        /* Form focus / selectize / radio */
        body.ynow-theme-bluechip .content-wrapper .form-control:focus,
        body.ynow-theme-bluechip .content-wrapper .selectize-input.focus {
          border-color: var(--ynow-bluechip-cyan) !important;
          box-shadow: 0 0 0 2px rgba(51, 177, 210, 0.25) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .radio input[type="radio"],
        body.ynow-theme-bluechip .content-wrapper .checkbox input[type="checkbox"] {
          accent-color: var(--ynow-bluechip-ocean);
        }
        body.ynow-theme-bluechip .content-wrapper a:not(.btn) {
          color: var(--ynow-bluechip-ocean);
        }
        body.ynow-theme-bluechip .content-wrapper a:not(.btn):hover {
          color: var(--ynow-bluechip-deep);
        }
        body.ynow-theme-bluechip .content-wrapper .ynow-lab-im-eq-explain {
          background: rgba(140, 208, 235, 0.18);
          border-color: var(--ynow-bluechip-sky);
          color: #2a4a5c;
        }
        body.ynow-theme-bluechip .content-wrapper .ynow-lab-im-eq-explain b {
          color: var(--ynow-bluechip-deep);
        }
        body.ynow-theme-bluechip .content-wrapper .label-primary,
        body.ynow-theme-bluechip .content-wrapper .badge-primary,
        body.ynow-theme-bluechip .content-wrapper .label-info,
        body.ynow-theme-bluechip .content-wrapper .badge-info {
          background-color: var(--ynow-bluechip-ocean) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .progress-bar,
        body.ynow-theme-bluechip .content-wrapper .progress-bar-success,
        body.ynow-theme-bluechip .content-wrapper .progress-bar-info {
          background-color: var(--ynow-bluechip-cyan) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .small-box,
        body.ynow-theme-bluechip .content-wrapper .small-box.bg-aqua,
        body.ynow-theme-bluechip .content-wrapper .small-box.bg-blue,
        body.ynow-theme-bluechip .content-wrapper .small-box.bg-green {
          background-color: var(--ynow-bluechip-ocean) !important;
        }
        /* Sidebar active rail stays ocean on Blue Chip (both markets) */
        body.ynow-theme-bluechip.skin-black .main-sidebar .sidebar-menu > li.active > a,
        body.ynow-theme-bluechip.skin-black .main-sidebar .sidebar-menu > li.menu-open > a {
          border-left-color: var(--ynow-bluechip-cyan) !important;
        }
        /* DT / tables light ocean header tint */
        body.ynow-theme-bluechip .content-wrapper .dataTables_wrapper .dataTables_paginate .paginate_button.current,
        body.ynow-theme-bluechip .content-wrapper .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
          background: var(--ynow-bluechip-ocean) !important;
          border-color: var(--ynow-bluechip-deep) !important;
          color: #fff !important;
        }
        body.ynow-theme-bluechip .content-wrapper table.table > thead > tr > th {
          border-bottom-color: var(--ynow-bluechip-sky) !important;
          color: var(--ynow-bluechip-deep);
        }
        body.ynow-theme-bluechip .content-wrapper table.ynow-sortable-dt thead th {
          cursor: pointer;
        }
        /* Spinner / loaders */
        body.ynow-theme-bluechip .content-wrapper .load-container .loader,
        body.ynow-theme-bluechip .content-wrapper .shiny-spinner-output-container .load-container {
          color: var(--ynow-bluechip-cyan);
        }

        /* --- Testing / Asset transmission：森林色系（TW／US 共用；圖表綠漲紅跌不變）---
           Palette: #464704 → #9CA889 → #F3F5E7 → #B7A78C → #423926 */
        body.ynow-theme-atx {
          --ynow-atx-moss: #464704;
          --ynow-atx-sage: #9CA889;
          --ynow-atx-ivory: #F3F5E7;
          --ynow-atx-khaki: #B7A78C;
          --ynow-atx-brown: #423926;
          --ynow-atx-accent: var(--ynow-atx-moss);
          --ynow-atx-accent-deep: var(--ynow-atx-brown);
          --ynow-atx-gradient: linear-gradient(
            105deg,
            #423926 0%,
            #464704 28%,
            #6e7340 55%,
            #9CA889 78%,
            #B7A78C 100%
          );
          --ynow-atx-wash: linear-gradient(
            180deg,
            rgba(70, 71, 4, 0.08) 0%,
            rgba(156, 168, 137, 0.10) 42%,
            rgba(243, 245, 231, 0.55) 100%
          );
          accent-color: var(--ynow-atx-moss);
        }
        body.ynow-theme-atx .content-wrapper {
          background-color: var(--ynow-atx-ivory) !important;
          background-image: var(--ynow-atx-wash) !important;
        }
        body.ynow-theme-atx .content-wrapper h2,
        body.ynow-theme-atx .content-wrapper h2 b,
        body.ynow-theme-atx .content-wrapper #ynow_atx_page_title {
          color: var(--ynow-atx-brown) !important;
        }
        body.ynow-theme-atx .content-wrapper #ynow_atx_page_sub,
        body.ynow-theme-atx .content-wrapper #ynow_atx_box_body {
          color: #5c5346 !important;
        }
        body.ynow-theme-atx .content-wrapper hr {
          border-top-color: var(--ynow-atx-khaki) !important;
        }
        body.ynow-theme-atx .content-wrapper .box.box-primary,
        body.ynow-theme-atx .content-wrapper .box.box-info,
        body.ynow-theme-atx .content-wrapper .box.box-success,
        body.ynow-theme-atx .content-wrapper .box.box-warning {
          border-top-color: var(--ynow-atx-accent) !important;
        }
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-primary,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-info,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-success,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-warning {
          border: 1px solid var(--ynow-atx-khaki) !important;
        }
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-primary > .box-header,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-info > .box-header,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-success > .box-header,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-warning > .box-header {
          color: #fff !important;
          background: var(--ynow-atx-gradient) !important;
          background-color: var(--ynow-atx-accent) !important;
          border-bottom-color: var(--ynow-atx-brown) !important;
        }
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-primary > .box-header a,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-primary > .box-header .btn,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-info > .box-header a,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-info > .box-header .btn,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-success > .box-header a,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-success > .box-header .btn,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-warning > .box-header a,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-warning > .box-header .btn {
          color: #fff !important;
        }
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: var(--ynow-atx-sage) !important;
        }
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a,
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.active:hover > a {
          color: var(--ynow-atx-brown) !important;
        }
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li > a > .fa,
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li > a > .fas {
          color: var(--ynow-atx-moss);
        }
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a > .fa,
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a > .fas {
          color: var(--ynow-atx-brown);
        }
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.header {
          color: var(--ynow-atx-brown) !important;
        }
        body.ynow-theme-atx .content-wrapper .btn-success,
        body.ynow-theme-atx .content-wrapper .btn-success:focus,
        body.ynow-theme-atx .content-wrapper .btn-success:active,
        body.ynow-theme-atx .content-wrapper .btn-success.active,
        body.ynow-theme-atx .content-wrapper .btn-primary,
        body.ynow-theme-atx .content-wrapper .btn-info {
          background-color: var(--ynow-atx-moss) !important;
          border-color: var(--ynow-atx-brown) !important;
          color: #fff !important;
          background-image: none !important;
        }
        body.ynow-theme-atx .content-wrapper .btn-success:hover,
        body.ynow-theme-atx .content-wrapper .btn-primary:hover,
        body.ynow-theme-atx .content-wrapper .btn-info:hover {
          background-color: var(--ynow-atx-brown) !important;
          border-color: var(--ynow-atx-brown) !important;
          color: #fff !important;
        }
        body.ynow-theme-atx .content-wrapper .btn-default {
          border-color: var(--ynow-atx-khaki) !important;
          color: var(--ynow-atx-brown) !important;
          background-color: rgba(243, 245, 231, 0.85) !important;
        }
        body.ynow-theme-atx .content-wrapper .btn-default:hover {
          background-color: rgba(156, 168, 137, 0.28) !important;
          border-color: var(--ynow-atx-sage) !important;
          color: var(--ynow-atx-brown) !important;
        }
        body.ynow-theme-atx .content-wrapper .form-control:focus,
        body.ynow-theme-atx .content-wrapper .selectize-input.focus {
          border-color: var(--ynow-atx-sage) !important;
          box-shadow: 0 0 0 2px rgba(156, 168, 137, 0.35) !important;
        }
        body.ynow-theme-atx .content-wrapper .radio input[type="radio"],
        body.ynow-theme-atx .content-wrapper .checkbox input[type="checkbox"] {
          accent-color: var(--ynow-atx-moss);
        }
        body.ynow-theme-atx .content-wrapper a:not(.btn) {
          color: var(--ynow-atx-moss);
        }
        body.ynow-theme-atx .content-wrapper a:not(.btn):hover {
          color: var(--ynow-atx-brown);
        }
        body.ynow-theme-atx .content-wrapper .label-primary,
        body.ynow-theme-atx .content-wrapper .badge-primary,
        body.ynow-theme-atx .content-wrapper .label-info,
        body.ynow-theme-atx .content-wrapper .badge-info {
          background-color: var(--ynow-atx-moss) !important;
        }
        body.ynow-theme-atx .content-wrapper .progress-bar,
        body.ynow-theme-atx .content-wrapper .progress-bar-success,
        body.ynow-theme-atx .content-wrapper .progress-bar-info {
          background-color: var(--ynow-atx-sage) !important;
        }
        body.ynow-theme-atx .content-wrapper .small-box,
        body.ynow-theme-atx .content-wrapper .small-box.bg-aqua,
        body.ynow-theme-atx .content-wrapper .small-box.bg-blue,
        body.ynow-theme-atx .content-wrapper .small-box.bg-green {
          background-color: var(--ynow-atx-moss) !important;
        }
        body.ynow-theme-atx.skin-black .main-sidebar .sidebar-menu > li.active > a,
        body.ynow-theme-atx.skin-black .main-sidebar .sidebar-menu > li.menu-open > a {
          border-left-color: var(--ynow-atx-sage) !important;
        }
        body.ynow-theme-atx .content-wrapper .dataTables_wrapper .dataTables_paginate .paginate_button.current,
        body.ynow-theme-atx .content-wrapper .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
          background: var(--ynow-atx-moss) !important;
          border-color: var(--ynow-atx-brown) !important;
          color: #fff !important;
        }
        body.ynow-theme-atx .content-wrapper table.table > thead > tr > th {
          border-bottom-color: var(--ynow-atx-khaki) !important;
          color: var(--ynow-atx-brown);
        }
        body.ynow-theme-atx .content-wrapper .load-container .loader,
        body.ynow-theme-atx .content-wrapper .shiny-spinner-output-container .load-container {
          color: var(--ynow-atx-sage);
        }
        /* ATX chrome follows forest palette; map canvas + move colors stay as designed. */
        body.ynow-theme-atx .content-wrapper .ynow-atx-summary,
        body.ynow-theme-atx .content-wrapper .ynow-atx h4 {
          color: var(--ynow-atx-brown);
        }
        body.ynow-theme-atx .content-wrapper .ynow-atx-legend,
        body.ynow-theme-atx .content-wrapper .ynow-atx-note,
        body.ynow-theme-atx .content-wrapper .ynow-atx-status {
          color: #5c5346;
        }
        body.ynow-theme-atx .content-wrapper .ynow-atx-regime,
        body.ynow-theme-atx .content-wrapper .ynow-atx-path {
          background: rgba(243, 245, 231, 0.92);
          border-color: var(--ynow-atx-khaki);
        }
        body.ynow-theme-atx .content-wrapper .ynow-atx-regime h4,
        body.ynow-theme-atx .content-wrapper .ynow-atx-path h4 {
          color: var(--ynow-atx-brown);
        }
        body.ynow-theme-atx .content-wrapper .ynow-atx-load-bar {
          background: linear-gradient(
            90deg,
            var(--ynow-atx-moss) 0%,
            var(--ynow-atx-sage) 55%,
            var(--ynow-atx-khaki) 100%
          );
        }
        body.ynow-theme-atx .content-wrapper .ynow-atx-load-track {
          background: rgba(243, 245, 231, 0.22);
          box-shadow: inset 0 0 0 1px rgba(183, 167, 140, 0.35);
        }

        /* Dashboard 損益表／現金流量表：圖標與選取頂條對齊 logo 金 */
        #dashboard_fin_report > .nav-tabs > li > a[data-value="Income Statement"] > .fa,
        #dashboard_fin_report > .nav-tabs > li > a[data-value="Income Statement"] > .fas,
        #dashboard_fin_report > .nav-tabs > li > a[data-value="Cash Flow"] > .fa,
        #dashboard_fin_report > .nav-tabs > li > a[data-value="Cash Flow"] > .fas {
          color: var(--ynow-gold-deep) !important;
        }
        #dashboard_fin_report > .nav-tabs > li.active > a[data-value="Income Statement"] > .fa,
        #dashboard_fin_report > .nav-tabs > li.active > a[data-value="Income Statement"] > .fas,
        #dashboard_fin_report > .nav-tabs > li.active > a[data-value="Cash Flow"] > .fa,
        #dashboard_fin_report > .nav-tabs > li.active > a[data-value="Cash Flow"] > .fas {
          color: var(--ynow-gold) !important;
        }
        #dashboard_fin_report > .nav-tabs > li.active:has(> a[data-value="Income Statement"]),
        #dashboard_fin_report > .nav-tabs > li.active:has(> a[data-value="Cash Flow"]) {
          border-top-color: var(--ynow-gold) !important;
        }
        @media (max-width: 767px) {
          #dashboard_fin_report > .nav-tabs > li > a[data-value="Income Statement"] > .fa,
          #dashboard_fin_report > .nav-tabs > li > a[data-value="Income Statement"] > .fas,
          #dashboard_fin_report > .nav-tabs > li > a[data-value="Cash Flow"] > .fa,
          #dashboard_fin_report > .nav-tabs > li > a[data-value="Cash Flow"] > .fas {
            font-size: 13px;
          }
        }
        /* 損益表／現金流量表圖表區：金色頂條 chrome（對齊 logo） */
        .ynow-fs-chart {
          border-top: 3px solid var(--ynow-gold);
          margin-top: 6px;
          padding-top: 10px;
          background: linear-gradient(180deg, rgba(245, 197, 24, 0.06) 0%, rgba(255, 255, 255, 0) 28px);
        }
        .ynow-fs-chart-heading {
          display: flex;
          align-items: center;
          gap: 8px;
          margin: 0 0 8px 0;
          font-size: 13px;
          font-weight: 700;
          color: var(--ynow-gold-ink);
          letter-spacing: 0.01em;
        }
        .ynow-fs-chart-heading > .fa,
        .ynow-fs-chart-heading > .fas {
          color: var(--ynow-gold-deep);
          font-size: 14px;
          width: 1.2em;
          text-align: center;
        }
        @media (max-width: 767px) {
          .ynow-fs-chart {
            border-top-width: 2px;
            padding-top: 8px;
          }
          .ynow-fs-chart-heading {
            font-size: 12px;
          }
        }
        .nav-pills > li.active > a,
        .nav-pills > li.active > a:hover,
        .nav-pills > li.active > a:focus {
          background-color: var(--ynow-ink) !important;
        }
        .pagination > .active > a,
        .pagination > .active > span,
        .pagination > .active > a:hover,
        .pagination > .active > span:hover {
          background-color: var(--ynow-ink) !important;
          border-color: var(--ynow-ink) !important;
        }
        /* Content links only — never paint dark ink onto skin-black sidebar
           (global #333 / hover #000 on #1a1a1a reads as “dimmed / disabled”
           and a solid black band where labels vanish). */
        a { color: var(--ynow-ink-soft); }
        a:hover, a:focus { color: #000; }
        .skin-black .main-sidebar a,
        .skin-black .main-sidebar .sidebar-menu > li > a,
        .skin-black .main-sidebar .sidebar-menu .treeview-menu > li > a {
          color: rgba(255, 255, 255, 0.92) !important;
        }
        .skin-black .main-sidebar a:hover,
        .skin-black .main-sidebar a:focus,
        .skin-black .sidebar-menu > li:hover > a,
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li.menu-open > a {
          color: #ffffff !important;
        }
        /* Full guest PDF download: muted / light-gray locked control (lock icon via JS).
           Menu-item locks use the shared .ynow-sidebar-locked / ACL opacity above.
           Lite keeps Download Report open — never apply the locked chrome. */
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-sidebar-download-wrap #download_report,
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-sidebar-download-btn {
          background-color: #2e2e2e !important;
          color: #a3a3a3 !important;
          border-color: #666666 !important;
          box-shadow: none !important;
        }
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-sidebar-download-wrap #download_report:hover,
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-sidebar-download-btn:hover {
          background-color: #353535 !important;
          color: #b8b8b8 !important;
          border-color: #777777 !important;
        }
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-download-report-quota {
          color: #a3a3a3;
        }
        /* Lite: hide lock glyphs — Full-only locked surfaces are hidden, not shown locked. */
        body.ynow-lite .ynow-menu-lock,
        body.ynow-lite .ynow-sidebar-dl-lock,
        body.ynow-lite .ynow-inner-tab-lock,
        body.ynow-lite .ynow-access-lock {
          display: none !important;
        }
        .label-primary, .badge-primary {
          background-color: var(--ynow-ink) !important;
        }
        /* Sidebar 推薦／備選 badges — overflow:visible so flex cannot clip the chip */
        .sidebar-menu > li > a,
        .sidebar-menu .treeview-menu > li > a {
          display: flex !important;
          align-items: center !important;
          flex-wrap: nowrap !important;
          overflow: visible !important;
        }
        .sidebar-menu > li > a > span:not(.pull-right-container):not(.ynow-rec-slot),
        .sidebar-menu .treeview-menu > li > a > span:not(.pull-right-container):not(.ynow-rec-slot),
        .sidebar-menu > li > a > .ynow-menu-label,
        .sidebar-menu .treeview-menu > li > a > .ynow-menu-label {
          flex: 1 1 auto;
          min-width: 0;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .sidebar-menu > li > a > .pull-right-container,
        .sidebar-menu .treeview-menu > li > a > .pull-right-container {
          float: none !important;
          position: static !important;
          margin-left: 4px !important;
          margin-right: 0 !important;
          display: inline-flex !important;
          align-items: center !important;
          flex: 0 0 auto;
          gap: 4px;
        }
        .sidebar-menu > li > a > .pull-right-container > .fa.pull-right,
        .sidebar-menu > li > a > .pull-right-container > .fas.pull-right,
        .sidebar-menu > li > a > .fa.pull-right,
        .sidebar-menu > li > a > .fas.pull-right,
        .sidebar-menu > li > a > .fa-angle-left,
        .sidebar-menu > li > a > .fas.fa-angle-left {
          float: none !important;
          position: static !important;
          top: auto !important;
          right: auto !important;
          margin: 0 0 0 4px !important;
          flex: 0 0 auto;
          transform: none !important;
        }
        .sidebar-menu .ynow-rec-slot {
          display: inline-flex !important;
          align-items: center !important;
          flex: 0 0 auto !important;
          margin-left: 6px !important;
          max-width: none !important;
          overflow: visible !important;
        }
        .sidebar-menu small.ynow-sidebar-badge {
          display: inline-block !important;
          visibility: visible !important;
          opacity: 1 !important;
          float: none !important;
          position: static !important;
          flex: 0 0 auto !important;
          align-self: center;
          margin: 0 !important;
          padding: 2px 6px !important;
          white-space: nowrap !important;
          line-height: 1.2 !important;
          font-size: 10px !important;
          font-weight: 700 !important;
          border-radius: 3px !important;
          max-width: none !important;
          transform: none !important;
          z-index: 5;
        }
        .sidebar-menu small.ynow-sidebar-badge.ynow-rec-primary {
          background-color: #dd4b39 !important;
          color: #ffffff !important;
        }
        .sidebar-menu small.ynow-sidebar-badge.ynow-rec-secondary {
          background-color: #6c757d !important;
          color: #ffffff !important;
        }
        .sidebar-menu small.ynow-sidebar-badge.ynow-rec-reference {
          background-color: #17a2b8 !important;
          color: #ffffff !important;
        }
        .sidebar-menu .treeview-menu {
          overflow: visible !important;
        }
        .progress-bar-primary {
          background-color: var(--ynow-ink) !important;
        }
        .text-primary { color: var(--ynow-ink) !important; }
        .bg-primary { background-color: var(--ynow-ink) !important; }
        /* Do not remap .bg-blue — KPI band 「優於區間」uses AdminLTE blue */

        #shiny-tab-get_started > h2 { font-weight: 800 !important; }
        /* 側邊欄：僅目前選取頁面粗體（勿固定加粗基礎設定） */
        .sidebar-menu > li > a {
          font-weight: 400 !important;
        }
        .sidebar-menu > li.active > a,
        .sidebar-menu > li.menu-open > a {
          font-weight: 700 !important;
        }
        /* 側邊欄圖示固定寬度欄：英文標籤左緣對齊（父層＋子分頁；勿動 pull-right 展開箭頭）
           Exclude .ynow-menu-lock — FA .fas { display } must not override lock hide/show. */
        .sidebar-menu > li > a > .fa:not(.pull-right):not(.ynow-menu-lock),
        .sidebar-menu > li > a > .fas:not(.pull-right):not(.ynow-menu-lock),
        .sidebar-menu > li > a > .far:not(.pull-right):not(.ynow-menu-lock),
        .sidebar-menu > li > a > .glyphicon:not(.pull-right):not(.ynow-menu-lock),
        .sidebar-menu > li > a > .ion:not(.pull-right):not(.ynow-menu-lock),
        .sidebar-menu .treeview-menu > li > a > .fa:not(.pull-right):not(.ynow-menu-lock),
        .sidebar-menu .treeview-menu > li > a > .fas:not(.pull-right):not(.ynow-menu-lock),
        .sidebar-menu .treeview-menu > li > a > .far:not(.pull-right):not(.ynow-menu-lock),
        .sidebar-menu .treeview-menu > li > a > .glyphicon:not(.pull-right):not(.ynow-menu-lock),
        .sidebar-menu .treeview-menu > li > a > .ion:not(.pull-right):not(.ynow-menu-lock) {
          display: inline-block;
          flex: 0 0 1.35em;
          width: 1.35em;
          min-width: 1.35em;
          margin-right: 8px;
          text-align: center;
          vertical-align: middle;
        }
        /* 估值分類子分頁標籤：黑底白字（僅 treeview-menu，不改父層分類標籤） */
        .skin-black .sidebar-menu .treeview-menu {
          background-color: #000 !important;
          padding-top: 0 !important;
          padding-bottom: 0 !important;
        }
        .skin-black .sidebar-menu .treeview-menu > li > a {
          background-color: #000 !important;
          color: #fff !important;
          border-left: 3px solid transparent;
        }
        .skin-black .sidebar-menu .treeview-menu > li > a > .fa,
        .skin-black .sidebar-menu .treeview-menu > li > a > .glyphicon,
        .skin-black .sidebar-menu .treeview-menu > li > a > .ion {
          color: #fff !important;
        }
        .skin-black .sidebar-menu .treeview-menu > li > a:hover {
          background-color: #1a1a1a !important;
          color: #fff !important;
          border-left-color: rgba(255, 255, 255, 0.55);
        }
        .skin-black .sidebar-menu .treeview-menu > li.active > a,
        .skin-black .sidebar-menu .treeview-menu > li.active > a:hover {
          background-color: #000 !important;
          color: #fff !important;
          font-weight: 700 !important;
          border-left-color: #fff;
        }
        /* Snapshot／Sys. Admin／Feedback：同一列；版權宣告在按鈕列正下方（勿重疊） */
        /* A+B chip foot + DRAWN traces: LINE strokes only on #000.
           Foot chip keeps www/ynow-chrome-veins-circuit.png at NATURAL size (no stretch).
           Vertical clamp: bottom-anchored; fade to full black by YNow brand logo
           (never paint above/through logo into menu/header). Base PCB traces are SOLID
           continuous strokes; electric blue glow travels on duplicate .ynow-ckt-glow
           paths — never dashed base wires; never translateY/scaleY of the whole asset. */
        .ynow-chrome-sidebar-circuit {
          position: absolute;
          left: 0;
          right: 0;
          bottom: 0;
          top: auto;
          /* Cap: foot → YNow logo band only (~brandFromBottom); not full sidebar */
          height: min(42vh, 380px);
          max-height: 420px;
          pointer-events: none;
          z-index: 0;
          overflow: hidden;
          /* Fade-out completes by overlay top (= YNow logo); full black above */
          -webkit-mask-image: linear-gradient(
            to top,
            #000 0%,
            #000 22%,
            rgba(0, 0, 0, 0.75) 48%,
            rgba(0, 0, 0, 0.28) 72%,
            transparent 92%
          );
          mask-image: linear-gradient(
            to top,
            #000 0%,
            #000 22%,
            rgba(0, 0, 0, 0.75) 48%,
            rgba(0, 0, 0, 0.28) 72%,
            transparent 92%
          );
        }
        .ynow-chrome-sidebar-circuit-glow {
          position: absolute;
          left: 8%;
          right: 8%;
          bottom: 0;
          height: 55%;
          background: radial-gradient(
            ellipse 78% 100% at 50% 100%,
            rgba(12, 84, 132, 0.36),
            transparent 72%
          );
          filter: blur(8px);
          transform-origin: bottom center;
          animation: ynow-chrome-blue-pulse 3.6s ease-in-out infinite;
          pointer-events: none;
        }
        .ynow-chrome-sidebar-circuit-strokes {
          position: absolute;
          left: 0;
          right: 0;
          bottom: 0;
          top: 0;
          pointer-events: none;
        }
        .ynow-chrome-sidebar-circuit-svg {
          position: absolute;
          left: 0;
          right: 0;
          bottom: 0;
          width: 100%;
          height: 100%;
          display: block;
          overflow: visible;
          filter: drop-shadow(0 0 2.5px rgba(94, 176, 224, 0.55));
          opacity: 0.9;
          /* Electric glow pulse on strokes — NO scaleY / translate scroll */
          animation: ynow-chrome-circuit-electric 3.6s ease-in-out infinite;
        }
        /* Visible circuit wires: solid continuous strokes (no dash gaps). */
        .ynow-chrome-sidebar-circuit-svg .ynow-ckt-p {
          stroke: url(#ynowCircuitStrokeGrad);
          stroke-dasharray: none;
          stroke-dashoffset: 0;
        }
        /* Traveling glow only: short lit segment + long gap (reads as pulse on solid wire). */
        .ynow-chrome-sidebar-circuit-svg .ynow-ckt-glow {
          stroke: #7ec8f0;
          stroke-dasharray: 14 220;
          stroke-dashoffset: 0;
          opacity: 0.85;
          animation: ynow-chrome-circuit-travel 2.8s linear infinite;
        }
        .ynow-chrome-sidebar-circuit-svg .ynow-ckt-glow:nth-child(2n) {
          animation-duration: 3.4s;
          animation-direction: reverse;
          stroke-dasharray: 11 240;
        }
        .ynow-chrome-sidebar-circuit-svg .ynow-ckt-glow:nth-child(3n) {
          animation-duration: 2.2s;
          stroke-dasharray: 16 200;
        }
        .ynow-chrome-sidebar-circuit-svg .ynow-ckt-node {
          animation: ynow-chrome-circuit-node-pulse 2.4s ease-in-out infinite;
        }
        .ynow-chrome-sidebar-circuit-svg .ynow-ckt-node:nth-child(2n) {
          animation-delay: 0.4s;
        }
        @keyframes ynow-chrome-circuit-travel {
          to { stroke-dashoffset: -234; }
        }
        @keyframes ynow-chrome-circuit-electric {
          0%, 100% {
            opacity: 0.72;
            filter: drop-shadow(0 0 1.5px rgba(12, 84, 132, 0.45));
          }
          50% {
            opacity: 0.98;
            filter: drop-shadow(0 0 4px rgba(94, 176, 224, 0.75));
          }
        }
        @keyframes ynow-chrome-circuit-node-pulse {
          0%, 100% { opacity: 0.55; }
          50% { opacity: 1; }
        }
        @keyframes ynow-chrome-circuit-bw-flow {
          0%, 100% { opacity: 0.78; }
          50% { opacity: 0.95; }
        }
        /* Keep scrollable sidebar content above the fixed circuit overlay */
        .main-sidebar > .sidebar,
        .main-sidebar > #sidebarItemExpanded {
          position: relative;
          z-index: 1;
        }
        .ynow-sidebar-snapshot-foot {
          display: flex !important;
          flex-direction: column !important;
          flex-wrap: nowrap !important;
          align-items: stretch;
          justify-content: flex-start;
          gap: 0;
          width: 100%;
          box-sizing: border-box;
          margin-top: 8px !important;
          margin-bottom: 10px !important;
          padding: 10px 8px 12px !important;
          text-align: left !important;
          position: relative;
        }
        .ynow-sidebar-foot-glow {
          position: absolute;
          left: 18%;
          right: 18%;
          bottom: 8px;
          height: 42%;
          background: radial-gradient(
            ellipse 80% 100% at 50% 100%,
            rgba(12, 84, 132, 0.4),
            transparent 70%
          );
          filter: blur(6px);
          transform-origin: bottom center;
          animation: ynow-chrome-blue-pulse 3.6s ease-in-out infinite;
          pointer-events: none;
          z-index: 0;
          /* Soft dissolve upward into black sidebar */
          -webkit-mask-image: linear-gradient(to top, #000 0%, #000 25%, transparent 100%);
          mask-image: linear-gradient(to top, #000 0%, #000 25%, transparent 100%);
        }
        @keyframes ynow-chrome-blue-pulse {
          0%, 100% { opacity: 0.4; transform: scaleY(0.72) scaleX(0.96); }
          50% { opacity: 0.85; transform: scaleY(1.08) scaleX(1.02); }
        }
        .ynow-sidebar-foot-chip {
          position: relative;
          z-index: 1;
          overflow: hidden;
          border-radius: 16px;
          /* Thin edge only — chip must not read as a filled photo panel */
          border: 1px solid rgba(12, 84, 132, 0.28);
          background: #000;
          box-shadow: none;
          padding: 8px 12px 6px;
          box-sizing: border-box;
          width: 100%;
        }
        .ynow-sidebar-foot-chip::before {
          content: "";
          position: absolute;
          left: 0;
          right: 0;
          bottom: 0;
          top: 0;
          border-radius: inherit;
          /* NATURAL-size chip stroke art — never stretch to full sidebar height */
          background-color: transparent;
          background-image: url("ynow-chrome-veins-circuit.png");
          background-repeat: no-repeat;
          background-size: 118% auto;
          background-position: center bottom;
          opacity: 0.88;
          /* Glow/opacity pulse only — no scaleY image scroll */
          animation: ynow-chrome-circuit-bw-flow 4.8s ease-in-out infinite;
          filter:
            drop-shadow(0 0 2px rgba(12, 84, 132, 0.55))
            hue-rotate(-8deg)
            saturate(1.15);
          -webkit-mask-image: linear-gradient(to top, #000 0%, #000 35%, transparent 88%);
          mask-image: linear-gradient(to top, #000 0%, #000 35%, transparent 88%);
          pointer-events: none;
          z-index: 0;
        }
        .ynow-sidebar-foot-chip > * {
          position: relative;
          z-index: 1;
        }
        @media (prefers-reduced-motion: reduce) {
          .ynow-sidebar-foot-glow,
          .ynow-chrome-sidebar-circuit-glow {
            animation: none;
            opacity: 0.55;
            transform: none;
          }
          .ynow-sidebar-foot-chip::before,
          .ynow-chrome-sidebar-circuit-svg {
            animation: none;
            opacity: 0.75;
            filter: drop-shadow(0 0 2px rgba(12, 84, 132, 0.45));
          }
          .ynow-chrome-sidebar-circuit-svg .ynow-ckt-p,
          .ynow-chrome-sidebar-circuit-svg .ynow-ckt-glow,
          .ynow-chrome-sidebar-circuit-svg .ynow-ckt-node {
            animation: none !important;
          }
          .ynow-chrome-sidebar-circuit-svg .ynow-ckt-glow {
            opacity: 0;
          }
          .ynow-chrome-sidebar-circuit-svg stop animate {
            display: none;
          }
        }
        .ynow-sidebar-snapshot-foot-links {
          display: flex !important;
          flex-direction: row !important;
          flex-wrap: nowrap !important;
          align-items: center;
          justify-content: space-between;
          gap: 4px;
          width: 100%;
          box-sizing: border-box;
        }
        .main-sidebar .ynow-sidebar-snapshot-foot.col-sm-12,
        .main-sidebar .ynow-sidebar-snapshot-foot.col-xs-12 {
          padding-left: 8px !important;
          padding-right: 8px !important;
        }
        .ynow-sidebar-foot-chip .ynow-sidebar-snapshot-link {
          display: inline-flex !important;
          align-items: center;
          justify-content: center;
          flex: 1 1 0;
          min-width: 0;
          white-space: nowrap;
          font-size: 11px !important;
          font-weight: 500 !important;
          color: rgba(255,255,255,0.78) !important;
          text-decoration: none !important;
          padding: 3px 2px;
          border-radius: 4px;
          opacity: 1;
          line-height: 1.25;
        }
        .ynow-sidebar-snapshot-link {
          display: inline-flex !important;
          align-items: center;
          justify-content: center;
          flex: 1 1 0;
          min-width: 0;
          white-space: nowrap;
          font-size: 10px !important;
          font-weight: 400 !important;
          color: rgba(255,255,255,0.45) !important;
          text-decoration: none !important;
          padding: 2px 1px;
          border-radius: 3px;
          opacity: 0.75;
          line-height: 1.2;
        }
        .ynow-sidebar-snapshot-link .fa,
        .ynow-sidebar-snapshot-link .fas,
        .ynow-sidebar-snapshot-link .fa-fw {
          width: 1em;
          min-width: 1em;
          margin-right: 2px;
          text-align: center;
          flex: 0 0 auto;
        }
        .ynow-sidebar-snapshot-link:hover {
          color: rgba(255,255,255,0.85) !important;
          opacity: 1;
          background: rgba(255,255,255,0.06);
        }
        .ynow-sidebar-foot-chip .ynow-sidebar-snapshot-link:hover {
          color: #fff !important;
          background: rgba(12, 84, 132, 0.28);
        }
        .ynow-sidebar-foot-chip .ynow-sidebar-copyright {
          color: rgba(255, 255, 255, 0.72);
          margin-top: 4px;
          padding-top: 4px;
          border-top: 1px solid rgba(12, 84, 132, 0.28);
        }
        .ynow-sidebar-feedback-link,
        .ynow-sidebar-test-link { margin-left: 0; }
        /* Recent Search: clickable tickers → re-Search + 個股 */
        .ynow-recent-search {
          display: flex;
          flex-wrap: wrap;
          gap: 6px;
          padding: 0 2px 2px 2px;
          min-height: 1.4em;
        }
        /* Pinned tickers from Recent Search: max 12 in a 3-column grid (4×3). */
        .ynow-pinned-search {
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 6px;
          padding: 2px 2px 8px 2px;
          min-height: 0;
          margin: 0 0 2px 0;
        }
        .ynow-pinned-search:empty { display: none; padding: 0; margin: 0; }
        .ynow-pinned-search-label {
          grid-column: 1 / -1;
          margin: 0 0 2px 0;
          padding: 0 2px;
          font-size: 11px;
          font-weight: 600;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: rgba(255, 214, 102, 0.85);
          opacity: 0.95;
        }
        .ynow-recent-search-empty {
          color: rgba(255, 255, 255, 0.45);
          font-size: 12px;
          line-height: 1.4;
          margin: 0;
        }
        .ynow-recent-chip {
          display: inline-block;
          padding: 2px 8px;
          border-radius: 3px;
          border: 1px solid rgba(255, 255, 255, 0.22);
          background: rgba(255, 255, 255, 0.06);
          color: rgba(255, 255, 255, 0.92) !important;
          font-size: 12px;
          font-weight: 600;
          letter-spacing: 0.02em;
          line-height: 1.45;
          cursor: pointer;
          user-select: none;
          -webkit-user-select: none;
          touch-action: manipulation;
          text-decoration: none !important;
        }
        .ynow-pinned-chip {
          display: flex;
          align-items: center;
          justify-content: center;
          gap: 4px;
          min-height: 36px;
          padding: 6px 4px;
          border-radius: 4px;
          border: 1px solid rgba(255, 214, 102, 0.55);
          background: rgba(255, 214, 102, 0.12);
          box-shadow: inset 0 0 0 1px rgba(255, 214, 102, 0.08);
          color: rgba(255, 255, 255, 0.92) !important;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.02em;
          line-height: 1.2;
          text-align: center;
          cursor: pointer;
          user-select: none;
          -webkit-user-select: none;
          touch-action: manipulation;
          text-decoration: none !important;
          overflow: hidden;
        }
        .ynow-pinned-chip::before {
          content: "";
          flex: 0 0 auto;
          display: inline-block;
          width: 0.5em;
          height: 0.5em;
          border: 1.5px solid rgba(255, 214, 102, 0.95);
          border-radius: 1px 1px 55% 55%;
          box-shadow: 0 2px 0 -0.5px rgba(255, 214, 102, 0.7);
          transform: rotate(-35deg);
        }
        .ynow-pinned-chip-text {
          display: block;
          max-width: 100%;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .ynow-recent-chip:hover,
        .ynow-recent-chip:focus {
          background: rgba(255, 255, 255, 0.14);
          border-color: rgba(255, 255, 255, 0.4);
          outline: none;
        }
        .ynow-pinned-chip:hover,
        .ynow-pinned-chip:focus {
          background: rgba(255, 214, 102, 0.22);
          border-color: rgba(255, 214, 102, 0.75);
          outline: none;
        }
        .ynow-recent-chip:focus-visible,
        .ynow-pinned-chip:focus-visible {
          box-shadow: 0 0 0 2px rgba(255, 255, 255, 0.35);
        }
        .ynow-recent-chip.ynow-chip-longpress,
        .ynow-pinned-chip.ynow-chip-longpress {
          transform: scale(0.96);
          border-color: rgba(255, 214, 102, 0.9);
          background: rgba(255, 214, 102, 0.28);
          transition: transform 0.12s ease-out, border-color 0.12s ease-out;
        }
        /* 側欄版權：緊接 Snapshot／Sys. Admin／意見區列下方，全寬、不與按鈕重疊 */
        .ynow-sidebar-copyright {
          flex: 0 0 auto;
          width: 100%;
          box-sizing: border-box;
          clear: both;
          margin: 6px 0 0;
          padding: 6px 4px 8px;
          font-size: 10px;
          line-height: 1.35;
          text-align: center;
          color: rgba(255, 255, 255, 0.55);
          letter-spacing: 0.01em;
        }
        body.sidebar-collapse .ynow-sidebar-copyright {
          display: none;
        }
        @media (min-width: 768px) {
          body.sidebar-mini.sidebar-collapse .main-sidebar:hover .ynow-sidebar-copyright {
            display: block;
          }
        }
        .ynow-sidebar-brand {
          display: flex;
          justify-content: center;
          align-items: center;
          padding: 8px 10px 6px 10px;
          cursor: pointer;
          user-select: none;
          outline: none;
        }
        .ynow-sidebar-brand:hover .ynow-sidebar-logo-full {
          opacity: 1;
        }
        .ynow-sidebar-brand:focus-visible {
          box-shadow: inset 0 0 0 2px rgba(255,255,255,0.35);
          border-radius: 4px;
        }
        .ynow-sidebar-brand-inner {
          position: relative;
          display: inline-flex;
          justify-content: center;
          align-items: center;
          width: min(200px, 92%);
          max-width: 100%;
        }
        .ynow-sidebar-logo-full {
          width: 100%;
          height: auto;
          max-width: 100%;
          object-fit: contain;
          display: block;
          opacity: 0.98;
        }
        .ynow-lite-badge {
          display: none;
          position: absolute;
          right: 2px;
          bottom: 2px;
          font-size: 9px;
          font-weight: 700;
          letter-spacing: 0.06em;
          line-height: 1;
          padding: 2px 4px;
          color: #1a1a1a;
          background: #f0c14b;
          border-radius: 2px;
          box-shadow: 0 0 0 1px rgba(0,0,0,0.25);
          pointer-events: none;
        }
        body.ynow-lite .ynow-lite-badge {
          display: inline-block;
        }
        .ynow-bblab--report {
          max-width: var(--ynow-page-max, 1200px);
          margin: 0 auto 36px;
          padding: 8px 4px 24px;
          width: 100%;
          box-sizing: border-box;
        }
        .ynow-bblab-report__cover {
          margin: 0 0 18px 0;
          padding: 8px 0 18px 0;
          border-bottom: 2px solid #0C5484;
        }
        .ynow-bblab-report__kicker {
          margin: 0 0 6px 0;
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          color: #0C5484;
        }
        .ynow-bblab-report__title {
          margin: 0 0 10px 0;
          font-size: 28px;
          font-weight: 700;
          line-height: 1.2;
          color: #12263a;
        }
        .ynow-bblab-report__deck {
          margin: 0 0 14px 0;
          max-width: 46em;
          font-size: 14px;
          line-height: 1.55;
          color: #4a5560;
        }
        .ynow-bblab-report__meta {
          display: flex;
          flex-wrap: wrap;
          gap: 16px 28px;
          margin-top: 14px;
        }
        .ynow-bblab-report__meta-item { min-width: 140px; }
        .ynow-bblab-report__meta-item--grow { flex: 1 1 220px; }
        .ynow-bblab-report__meta-lab {
          display: block;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.06em;
          text-transform: uppercase;
          color: #6a7680;
          margin-bottom: 2px;
        }
        .ynow-bblab-report__meta-val {
          font-size: 15px;
          font-weight: 600;
          color: #12263a;
        }
        .ynow-bblab-report__toolbar {
          margin: 0 0 22px 0;
          padding: 12px 14px 6px;
          background: #f6f8fa;
          border: 1px solid #d9dee3;
          border-radius: 4px;
        }
        .ynow-bblab-report__toolbar-check { padding-top: 22px; }
        /* Shiny defaults .shiny-input-container to 300px — too narrow for the GM fallback label. */
        .ynow-bblab-report__toolbar-check .shiny-input-container,
        .ynow-bblab-report__toolbar-check .checkbox,
        .ynow-bblab-report__toolbar-check label {
          width: auto !important;
          max-width: 100% !important;
        }
        .ynow-bblab-report__toolbar-check label,
        .ynow-bblab-report__toolbar-check #ynow_bblab_fallback_gm_label {
          white-space: nowrap;
        }
        .ynow-bblab-report__hint {
          margin: 0 0 6px 0;
          font-size: 12px;
          color: #6a7680;
        }
        .ynow-bblab-report__section {
          margin: 0 0 28px 0;
          padding: 0 0 8px 0;
        }
        .ynow-bblab-report__section--muted {
          padding: 12px 14px;
          background: #fafbfc;
          border: 1px solid #e4e8ec;
          border-radius: 4px;
        }
        .ynow-bblab-report__section-head {
          display: flex;
          align-items: baseline;
          gap: 10px;
          margin: 0 0 8px 0;
          padding-bottom: 6px;
          border-bottom: 1px solid #d9dee3;
        }
        .ynow-bblab-report__section-num {
          font-size: 13px;
          font-weight: 700;
          color: #0C5484;
          min-width: 1.6em;
        }
        .ynow-bblab-report__section-title {
          margin: 0;
          font-size: 18px;
          font-weight: 700;
          color: #12263a;
        }
        .ynow-bblab-report__section-help {
          margin: 0 0 12px 0;
          font-size: 13px;
          line-height: 1.5;
          color: #5a6570;
        }
        .ynow-bblab-report__section-body { margin-top: 4px; }
        .ynow-bblab__listed-notice {
          margin: 0 0 14px 0;
          padding: 8px 12px;
          font-size: 13px;
          font-weight: 600;
          line-height: 1.45;
          color: #1b3a4b;
          background: #eef6f8;
          border: 1px solid #1AA8B8;
          border-left-width: 4px;
          border-radius: 3px;
        }
        .ynow-bblab__listed-scope {
          margin: 8px 0 0 0;
          font-weight: 600;
          color: #1b3a4b;
        }
        .ynow-bblab-card {
          border: 1px solid #d9dee3;
          border-radius: 6px;
          padding: 10px 12px;
          margin-bottom: 12px;
          background: #fff;
          min-height: 160px;
        }
        .ynow-bblab-card--focus { outline: 2px solid #1AA8B8; }
        .ynow-bblab-card--neutral { background: #f4f6f7; border-color: #c5ccd1; }
        .ynow-bblab-card__metrics { padding-left: 1.1em; margin: 8px 0 0 0; }
        .ynow-bblab-tag {
          display: inline-block;
          margin-left: 6px;
          font-size: 10px;
          padding: 1px 6px;
          border-radius: 2px;
          background: #eef2f4;
          color: #555;
        }
        .ynow-bblab-recon--pass { color: #1e7a46; font-weight: 700; }
        .ynow-bblab-recon--fail { color: #b33b3b; font-weight: 700; }
        .ynow-bblab-chapter__num {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 1.55em;
          height: 1.55em;
          margin-right: 8px;
          font-size: 12px;
          font-weight: 700;
          color: #fff;
          background: #0C5484;
          border-radius: 50%;
          vertical-align: middle;
        }
        .ynow-bblab-subhead {
          margin: 18px 0 6px 0;
          font-size: 15px;
          font-weight: 700;
          color: #0C5484;
        }
        .ynow-bblab-kpis {
          display: flex;
          flex-wrap: wrap;
          gap: 10px 14px;
          margin: 8px 0 4px 0;
        }
        .ynow-bblab-kpi {
          min-width: 140px;
          padding: 8px 10px;
          background: #f7fafb;
          border: 1px solid #d5dee3;
          border-radius: 4px;
        }
        .ynow-bblab-kpi__lab {
          display: block;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.02em;
          color: #4a5b66;
          margin-bottom: 2px;
        }
        .ynow-bblab-kpi__val {
          font-size: 16px;
          font-weight: 700;
          color: #1a1a1a;
        }
        .ynow-bblab-kpi__unit {
          font-size: 11px;
          color: #666;
          margin-left: 4px;
        }
        .ynow-bblab-is {
          margin: 8px 0 4px 0;
          max-width: 720px;
        }
        .ynow-bblab-is__meta {
          display: flex;
          flex-wrap: wrap;
          gap: 12px 28px;
          margin: 0 0 10px 0;
          color: #4a5b66;
          font-size: 13px;
        }
        .ynow-bblab-is__meta-lab {
          font-weight: 700;
          margin-right: 6px;
        }
        .ynow-bblab-is__table {
          width: 100%;
          border-collapse: collapse;
          font-variant-numeric: tabular-nums;
        }
        .ynow-bblab-is__row td {
          padding: 5px 8px;
          vertical-align: baseline;
        }
        .ynow-bblab-is__op {
          width: 1.6em;
          color: #0C5484;
          font-weight: 700;
          text-align: center;
        }
        .ynow-bblab-is__lab {
          color: #1a1a1a;
        }
        .ynow-bblab-is__amt {
          text-align: right;
          font-weight: 700;
          white-space: nowrap;
        }
        .ynow-bblab-is__fml {
          padding-left: 14px;
          color: #5b6b75;
          font-size: 12px;
          white-space: nowrap;
        }
        .ynow-bblab-is__row--total td {
          border-top: 1px solid #0C5484;
          padding-top: 8px;
        }
        .ynow-bblab-is__row--ratio td {
          color: #4a5b66;
          font-weight: 500;
        }
        .ynow-bblab-is__row--ni td {
          border-top: 1px dotted #c5d0d6;
          padding-top: 10px;
        }
        .ynow-bblab-split {
          margin: 0;
          padding-left: 1.2em;
        }
        .ynow-bblab-split li { margin-bottom: 4px; }
        /* ---- Full mode: short white dashed separators between sidebar groups ----
           (no line under Home / above About)
           Asset Transmission | Blue Chip
           YNOW | Model Dashboard  (Smart Analysis is hidden in Full)
           Hist. FV Validation | Quant Backtest Lab
           Inset via ::before so the dash is shorter than full sidebar width.
        */
        /* Unified lock glyph: Full guest locks + ACL / still-locked tabs (.ynow-sidebar-locked).
           !important beats Font Awesome .fas { display: var(--fa-display,inline-block) }. */
        .sidebar-menu .ynow-menu-lock,
        .sidebar-menu > li > a > .ynow-menu-lock,
        .sidebar-menu .treeview-menu > li > a > .ynow-menu-lock {
          display: none !important;
          font-size: 11px;
          width: 1em;
          margin: 0 4px 0 2px;
          opacity: 0.95;
          vertical-align: -1px;
        }
        .sidebar-menu li.ynow-sidebar-locked > a > .ynow-menu-lock,
        .sidebar-menu li.ynow-access-locked-nav > a > .ynow-menu-lock,
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a > .ynow-menu-lock {
          display: inline-block !important;
        }
        /* Lock replaces the tab glyph while locked (guest gate or ACL / persist). */
        .sidebar-menu li.ynow-sidebar-locked > a > i:not(.ynow-menu-lock):not(.pull-right),
        .sidebar-menu li.ynow-access-locked-nav > a > i:not(.ynow-menu-lock):not(.pull-right),
        body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a:has(.ynow-menu-lock) > i:not(.ynow-menu-lock):not(.pull-right) {
          display: none !important;
        }
        body:not(.ynow-lite) .sidebar-menu > li:has(> a[data-value="bluechip"]),
        body:not(.ynow-lite) .sidebar-menu > li:has(> a[data-value="get_started"]),
        body:not(.ynow-lite) .sidebar-menu > li:has(> a[data-value="lab_notes"]) {
          position: relative;
          margin-top: 8px;
          padding-top: 8px;
          border-top: none;
        }
        body:not(.ynow-lite) .sidebar-menu > li:has(> a[data-value="bluechip"])::before,
        body:not(.ynow-lite) .sidebar-menu > li:has(> a[data-value="get_started"])::before,
        body:not(.ynow-lite) .sidebar-menu > li:has(> a[data-value="lab_notes"])::before {
          content: "";
          position: absolute;
          top: 0;
          left: 22%;
          right: 22%;
          border-top: 1px dashed #ffffff;
          pointer-events: none;
        }
        /* ---- Lite mode: hide Full-only chrome; show Smart Analysis ---- */
        .sidebar-menu a[data-value="smart_analysis"],
        .sidebar-menu li:has(> a[data-value="smart_analysis"]) {
          display: none !important;
        }
        body.ynow-lite .sidebar-menu a[data-value="smart_analysis"],
        body.ynow-lite .sidebar-menu li:has(> a[data-value="smart_analysis"]) {
          display: block !important;
        }
        /* Lite sidebar omit (guest-locked + Full-only models + Account):
           injected from ynow_sidebar_lite_omit_css() / ynow_sidebar_lite_omit_tabs(). */
        body.ynow-lite .ynow-full-only {
          display: none !important;
        }
        /* Lite: board KPIs stay non-clickable; YNOW／TYNOW + HTCDI keep expand/collapse. */
        body.ynow-lite .ynow-macro-kpi--clickable:not(.ynow-macro-kpi--ynow):not(.ynow-macro-kpi--htcdi) {
          cursor: default !important;
          pointer-events: none;
        }
        body.ynow-lite #ynow_macro_index_hint {
          display: none !important;
        }
        /* Prefer data-value: locale applyTabLabels may strip title span ids */
        body.ynow-lite #dashboard_fin_report .nav-tabs > li:has(#ynow_dash_annotation_tab),
        body.ynow-lite #dashboard_fin_report .nav-tabs > li:has(> a[data-value="Annotation"]) {
          display: none !important;
        }
        /* Lite foot: Snapshot + Feedback only. Sys. Admin + BBLab chrome stay Full-only.
           company_advance / guest-locked tabs come from ynow_sidebar_lite_omit_css(). */
        body.ynow-lite .ynow-sidebar-test-link,
        body.ynow-lite .ynow-company-advance-bblab,
        body.ynow-lite .ynow-bblab {
          display: none !important;
        }
        /* Signed-in account users: hide Sys. Admin foot link (portal is guest/ops only).
           Beat .ynow-sidebar-snapshot-link { display: inline-flex !important }. */
        body.ynow-authed .ynow-sidebar-snapshot-foot a.ynow-sidebar-snapshot-link.ynow-sidebar-test-link,
        body.ynow-authed .ynow-sidebar-snapshot-foot #ynow_sidebar_test_btn,
        body.ynow-authed a#ynow_sidebar_test_btn.ynow-sidebar-test-link {
          display: none !important;
          visibility: hidden !important;
          pointer-events: none !important;
        }
        /* Lite Snapshot stays free: keep snap_audit + snap_current.
           Only omit System defaults (APP_DEFAULTS) — never show a lock glyph here.
           Note: shiny tabset id may sit on the ul.nav-tabs itself (not a wrapper). */
        body.ynow-lite #snapshot_report > li:has(> a[data-value="snap_defaults"]),
        body.ynow-lite #snapshot_report .nav-tabs > li:has(> a[data-value="snap_defaults"]) {
          display: none !important;
        }
        /* Snapshot APP_DEFAULTS (Full): tab stays; title becomes a lock until sysadmin. */
        #snapshot_report a[data-value="snap_defaults"] > .ynow-snap-tab-lock {
          display: none;
        }
        body:not(.ynow-lite):not(.ynow-sysadmin-on) #snapshot_report a[data-value="snap_defaults"] {
          font-size: 0 !important;
          line-height: 1;
        }
        body:not(.ynow-lite):not(.ynow-sysadmin-on) #snapshot_report a[data-value="snap_defaults"] > .ynow-snap-tab-lock {
          display: inline-block !important;
          font-size: 13px !important;
          width: auto;
          margin: 0;
        }
        body:not(.ynow-lite):not(.ynow-sysadmin-on) #snapshot_report a[data-value="snap_defaults"] > i:not(.ynow-snap-tab-lock),
        body:not(.ynow-lite):not(.ynow-sysadmin-on) #snapshot_report a[data-value="snap_defaults"] > span {
          display: none !important;
        }
        body:not(.ynow-sysadmin-on) .ynow-snap-defaults-body {
          display: none !important;
        }
        /* Developer explainability / audit Why panels: sysadmin portal only */
        body:not(.ynow-sysadmin-on) .ynow-admin-dev-explain {
          display: none !important;
        }
        .ynow-snap-defaults-locked {
          display: none;
          margin: 0 0 12px 0;
          padding: 12px 14px;
          background: var(--ynow-gate-bg, #f5f5f5);
          border: 1px solid var(--ynow-gate-border, #d0d0d0);
          border-left: 4px solid var(--ynow-gate-accent, #1a1a1a);
          border-radius: 4px;
          color: var(--ynow-gate-ink-soft, #333333);
          font-size: 13px;
          line-height: 1.45;
        }
        /* Promo lives in the snap_defaults pane, so inactive tabs do not show it. */
        body:not(.ynow-sysadmin-on) #shiny-tab-snap_defaults .ynow-snap-defaults-locked {
          display: block !important;
        }
        body.ynow-sysadmin-on .ynow-snap-defaults-locked {
          display: none !important;
        }
        .ynow-lite-only {
          display: none !important;
        }
        body.ynow-lite .ynow-lite-only {
          display: block !important;
        }
        /* Brand home: sidebar-aligned doors (Macro → Blue Chip → Company → YNOW → Value → Action) */
        .ynow-home {
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin: 12px auto 28px;
          padding: 28px 8px 8px;
          background: transparent;
          color: #222;
          border-radius: 0;
          text-align: center;
          box-sizing: border-box;
        }
        .ynow-home-logo-wrap {
          position: relative;
          display: inline-block;
          margin: 0 0 18px;
          padding: 0;
          border: 0;
          background: transparent;
          cursor: pointer;
          user-select: none;
          outline: none;
        }
        .ynow-home-logo-wrap:focus-visible {
          outline: 2px solid #2f9e6b;
          outline-offset: 3px;
          border-radius: 8px;
        }
        .ynow-home-logo {
          width: 132px;
          height: auto;
          display: block;
          margin: 0;
        }
        .ynow-home-logo-wrap .ynow-lite-badge {
          right: -30px;
          bottom: 2px;
        }
        .ynow-home-title {
          margin: 0;
          font-size: 26px;
          font-weight: 600;
          letter-spacing: -0.02em;
          color: #1a1a1a;
        }
        .ynow-home-lead {
          margin: 10px 0 0;
          font-size: 16px;
          line-height: 1.45;
          color: #333;
        }
        .ynow-home-method {
          margin: 6px 0 22px;
          font-size: 13px;
          line-height: 1.45;
          color: #666;
        }
        .ynow-home-grid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 10px;
          text-align: center;
        }
        .ynow-home-card {
          display: block;
          width: 100%;
          margin: 0;
          padding: 14px 14px 15px;
          text-align: center;
          color: #222;
          background: transparent;
          border: 1px solid #d0d5da;
          border-radius: 8px;
          cursor: pointer;
        }
        .ynow-home-card:hover,
        .ynow-home-card:focus-visible {
          border-color: #2f9e6b;
          outline: none;
        }
        .ynow-home-card-k {
          margin: 0 0 4px;
          font-size: 14px;
          font-weight: 650;
          color: #111;
          text-align: center;
        }
        .ynow-home-card-d {
          margin: 0;
          font-size: 12.5px;
          line-height: 1.45;
          color: #555;
          text-align: center;
        }
        @media (max-width: 720px) {
          .ynow-home { padding: 24px 12px 18px; max-width: 100%; }
          .ynow-home-grid { grid-template-columns: 1fr; }
        }
        @media (min-width: 721px) and (max-width: 991px) {
          .ynow-home-grid { grid-template-columns: 1fr 1fr; }
        }
        @media (min-width: 992px) {
          .ynow-home-grid { grid-template-columns: 1fr 1fr 1fr; }
        }
        .ynow-home-legal {
          margin: 18px 0 0 0;
          padding-top: 14px;
          border-top: 1px solid #e5e8eb;
          text-align: center;
        }
        /* Sign in · Register · About on one horizontal row (not stacked). */
        .ynow-home-legal-links {
          display: inline-flex;
          flex-direction: row;
          flex-wrap: wrap;
          align-items: center;
          justify-content: center;
          gap: 0;
          margin: 0 0 10px 0;
          max-width: 100%;
        }
        .ynow-home-legal-links > .ynow-auth-links--home {
          display: inline-flex;
          margin: 0;
          justify-content: center;
        }
        .ynow-home-legal-links > .ynow-home-legal-sep {
          display: inline-block;
          margin: 0 6px;
          color: #0b57d0;
          font-size: 12px;
          line-height: 1.4;
          user-select: none;
        }
        /* Lite / no auth mount: hide orphan · before About (signed-in keeps · between Sign out and About). */
        body.ynow-lite .ynow-home-legal-links > .ynow-home-legal-sep,
        .ynow-home-legal-links:not(:has(.ynow-auth-links--home)) > .ynow-home-legal-sep {
          display: none;
        }
        .ynow-home-legal-text {
          margin: 0;
          font-size: 11.5px;
          line-height: 1.5;
          color: #666;
          text-align: center;
        }
        .ynow-home-legal-link {
          display: inline-block;
          margin: 0;
          padding: 0;
          border: none;
          background: transparent;
          color: #0b57d0;
          font-size: 12px;
          line-height: 1.4;
          cursor: pointer;
          text-decoration: underline;
          text-underline-offset: 2px;
          white-space: nowrap;
        }
        .ynow-home-legal-link:hover,
        .ynow-home-legal-link:focus-visible {
          color: #0842a0;
          outline: none;
        }
        .ynow-sidebar-date-hr {
          margin: 6px 8px 10px;
          border-top-color: rgba(255,255,255,0.18);
        }
        /* Lite Smart Analysis: lead blurb above composite status (no page heading) */
        .ynow-smart-lite-blurb-wrap {
          margin: 0 0 10px 0;
        }
        .ynow-smart-lite-blurb {
          margin: 0;
          font-size: 14px;
          line-height: 1.55;
          color: #444;
        }
        .ynow-smart-card-row {
          display: flex;
          flex-wrap: wrap;
          gap: 12px;
          margin: 0 0 14px 0;
        }
        .ynow-smart-card-row > .ynow-alertable {
          flex: 1 1 220px;
          min-width: 0;
        }
        .ynow-smart-card-row > .ynow-alertable .ynow-smart-card {
          flex: none;
          width: 100%;
          height: 100%;
        }
        .ynow-smart-card {
          flex: 1 1 220px;
          border: 1px solid #ddd;
          border-radius: 6px;
          padding: 12px 14px;
          background: #fff;
          min-height: 96px;
        }
        .ynow-smart-card-kicker {
          font-size: 11px;
          color: #777;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          margin: 0 0 4px 0;
        }
        .ynow-smart-card-title {
          font-size: 14px;
          font-weight: 700;
          margin: 0 0 6px 0;
          color: #222;
        }
        .ynow-smart-card-value {
          font-size: clamp(22px, 3.6vw, 30px);
          font-weight: 800;
          color: #111;
          margin: 0;
          font-variant-numeric: tabular-nums;
          line-height: 1.15;
        }
        .ynow-smart-card-meta {
          font-size: 12px;
          color: #666;
          margin: 6px 0 0 0;
          line-height: 1.4;
        }
        /* 側邊欄分頁 Loading：標籤右側動態「…」 */
        .sidebar-menu a > .ynow-tab-busy-dots {
          display: none;
          margin-left: 3px;
          font-weight: 700;
          letter-spacing: 0.02em;
          color: #249C60;
          vertical-align: baseline;
          white-space: nowrap;
        }
        .sidebar-menu a.ynow-tab-is-busy > .ynow-tab-busy-dots {
          display: inline;
        }
        .sidebar-menu a.ynow-tab-is-busy {
          font-weight: 600;
        }
        .sidebar-menu .ynow-tab-busy-dots > span {
          display: inline-block;
          opacity: 0.2;
          animation: ynow-tab-dot-pulse 1.2s infinite ease-in-out;
        }
        .sidebar-menu .ynow-tab-busy-dots > span:nth-child(2) {
          animation-delay: 0.2s;
        }
        .sidebar-menu .ynow-tab-busy-dots > span:nth-child(3) {
          animation-delay: 0.4s;
        }
        @keyframes ynow-tab-dot-pulse {
          0%, 80%, 100% { opacity: 0.15; }
          40% { opacity: 1; }
        }

        /* 側邊欄 PDF 下載：換行／縮放，避免長標籤撐破側欄 */
        .ynow-sidebar-download-wrap {
          padding: 6px 10px 10px 10px;
          text-align: center;
          margin-top: 4px;
          max-width: 100%;
          box-sizing: border-box;
        }
        .ynow-download-report-quota {
          margin: 6px 0 0 0;
          font-size: 11px;
          line-height: 1.35;
          color: #d9d9d9;
          text-align: left;
        }
        .ynow-download-report-quota .ynow-pdf-quota-line {
          margin: 0 0 4px 0;
        }
        .ynow-download-report-quota .ynow-pdf-quota-line:last-child {
          margin-bottom: 0;
        }
        .ynow-download-report-quota .ynow-pdf-quota-rules {
          color: #c4c4c4;
        }
        .ynow-snapshot-download-quota {
          margin: 0 0 10px 0;
          font-size: 12px;
          line-height: 1.4;
          color: #666;
        }
        body.ynow-lite .ynow-snapshot-download-quota {
          color: #555;
        }
        /* Count (remaining/total) + time-window mini countdown — never cover primary label. */
        .ynow-count-badge {
          font-weight: 600;
          white-space: nowrap;
          margin-left: 0.2em;
        }
        .ynow-ttl-countdown {
          display: block;
          font-size: 10px;
          line-height: 1.2;
          font-variant-numeric: tabular-nums;
          opacity: 0.88;
          margin-top: 2px;
          white-space: nowrap;
          max-width: 100%;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .sidebar-menu li > a .ynow-ttl-countdown,
        .sidebar-menu li .ynow-ttl-countdown {
          color: inherit;
          font-weight: 400;
        }
        .form-group.ynow-quota-badge-host,
        .shiny-input-container.ynow-quota-badge-host,
        [data-ynow-lock].ynow-quota-badge-host {
          position: relative;
          padding-bottom: 14px;
        }
        .form-group.ynow-quota-badge-host > .ynow-ttl-countdown,
        .shiny-input-container.ynow-quota-badge-host > .ynow-ttl-countdown,
        [data-ynow-lock].ynow-quota-badge-host > .ynow-ttl-countdown {
          position: absolute;
          left: 0;
          right: 0;
          bottom: 0;
          margin-top: 0;
          pointer-events: none;
        }
        a.btn.ynow-quota-badge-host,
        button.ynow-quota-badge-host,
        #download_report.ynow-quota-badge-host {
          position: relative;
        }
        .ynow-sidebar-download-wrap #download_report,
        .ynow-sidebar-download-btn {
          display: inline-block;
          width: 100%;
          max-width: 100%;
          box-sizing: border-box;
          white-space: normal !important;
          word-break: break-word;
          overflow-wrap: anywhere;
          line-height: 1.25;
          padding: 8px 10px;
          font-size: 13px;
          font-weight: bold;
          text-align: center;
        }
        /* 主選單不應再出現 Snapshot */
        .sidebar-menu a[data-value="snapshot"] { display: none !important; }
        .sidebar-menu li:has(> a[data-value="snapshot"]) { display: none !important; }
        .sidebar-menu a[data-value="feedback"] { display: none !important; }
        .sidebar-menu li:has(> a[data-value="feedback"]) { display: none !important; }
        /* 公司全稱：允許換行，避免被切掉；台股中英雙語直向堆疊 */
        .ynow-corpname {
          font-weight: bold;
          color: #333333;
          margin: 0;
          line-height: 1.25;
          white-space: normal;
          overflow-wrap: anywhere;
          word-break: break-word;
        }
        .ynow-corpname .ynow-corpname-stack {
          display: inline-flex;
          flex-direction: column;
          align-items: stretch;
          width: fit-content;
          max-width: 100%;
        }
        .ynow-corpname .ynow-corpname-zh {
          display: block;
          text-align: left;
        }
        .ynow-corpname .ynow-corpname-en {
          display: block;
          text-align: right;
          align-self: stretch;
          font-size: 0.82em;
          font-weight: 600;
          color: #555555;
          margin-top: 2px;
          line-height: 1.2;
        }
        .ynow-corpname .ynow-corpname-single {
          display: inline;
        }
        /* Search 後：官方 logo 主色（固定兩色；缺→黑；純黑單色→白）套用到公司全稱 */
        .ynow-corpname .ynow-corpname-logo-solid {
          color: var(--ynow-corp-c1, #333333);
        }
        .ynow-corpname .ynow-corpname-logo-grad {
          background-image: linear-gradient(
            90deg,
            var(--ynow-corp-c1, #333333) 0%,
            var(--ynow-corp-c2, #000000) 100%
          );
          -webkit-background-clip: text;
          background-clip: text;
          color: transparent;
          -webkit-text-fill-color: transparent;
        }
        .ynow-corpname .ynow-corpname-zh[style*="color:"],
        .ynow-corpname .ynow-corpname-en[style*="color:"] {
          -webkit-text-fill-color: currentColor;
        }
        /* 台股英文列第二色為白時：淺陰影避免溶進白底 */
        .ynow-corpname .ynow-corpname-en-on-white {
          text-shadow:
            0 0 1px rgba(51, 51, 51, 0.55),
            0 1px 0 rgba(51, 51, 51, 0.35);
        }
        /* LAST PRICE / MARKET CAP / EPS (TTM): icon fill follows the company-name logo pair */
        body.ynow-corp-logo #ibx_stockprice .info-box-icon,
        body.ynow-corp-logo #ibx_marketcap .info-box-icon,
        body.ynow-corp-logo #ibx_EPS .info-box-icon {
          background-color: var(--ynow-corp-c1) !important;
          background-image: linear-gradient(
            90deg,
            var(--ynow-corp-c1, #333333) 0%,
            var(--ynow-corp-c2, #000000) 100%
          ) !important;
        }
        /* Header Language toggle (in black bar); Currency floats under logo */
        /* 繁中／EN 與小 logo 在頁首列垂直置中對齊 */
        .main-header .navbar-custom-menu {
          height: 50px !important;
          display: flex !important;
          align-items: center !important;
        }
        .main-header .navbar-custom-menu > .navbar-nav {
          display: flex !important;
          flex-direction: row;
          align-items: center !important;
          flex-wrap: nowrap !important;
          height: 50px !important;
          margin: 0 !important;
        }
        /* Auth header: desktop-only flex (≤767 hides it — see mobile block + auth_account.R).
           Do not force display:flex here or it beats the mobile display:none !important. */
        .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header,
        .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header,
        .main-header .navbar-custom-menu .navbar-nav > li#ynow-header-logo.ynow-header-logo {
          float: none !important;
          height: 50px !important;
          min-height: 50px !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          margin: 0 !important;
          padding-top: 0 !important;
          padding-bottom: 0 !important;
          flex-shrink: 0 !important;
        }
        @media (min-width: 768px) {
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-auth-header {
            float: none !important;
            height: 50px !important;
            min-height: 50px !important;
            display: flex !important;
            align-items: center !important;
            justify-content: center !important;
            margin: 0 !important;
            padding: 0 12px 0 8px !important;
            flex-shrink: 0 !important;
            list-style: none !important;
            overflow: visible !important;
          }
        }
        .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header {
          padding: 0 10px 0 4px !important;
          list-style: none !important;
          overflow: visible !important;
        }
        .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header {
          padding: 0 14px 0 8px !important;
          list-style: none !important;
          overflow: visible !important;
        }
        /* Guests: hide pts (beats flex !important on header li / Lite keep-visible). */
        .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header.ynow-pts-hidden,
        body.ynow-lite .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header.ynow-pts-hidden,
        body.ynow-lite #ynow-pts-header.ynow-pts-hidden,
        #ynow-pts-header.ynow-pts-hidden {
          display: none !important;
          visibility: hidden !important;
          opacity: 0 !important;
          width: 0 !important;
          min-width: 0 !important;
          max-width: 0 !important;
          padding: 0 !important;
          margin: 0 !important;
          overflow: hidden !important;
          pointer-events: none !important;
        }
        .ynow-pts-banner {
          display: flex !important;
          flex-direction: column !important;
          align-items: flex-start !important;
          justify-content: center !important;
          gap: 1px;
          line-height: 1.15;
          color: #f5f5f5;
          font-size: 11px;
          font-weight: 600;
          letter-spacing: 0.01em;
          white-space: nowrap;
          min-width: 7.5em;
        }
        .ynow-pts-banner-line {
          display: flex;
          flex-direction: row;
          align-items: baseline;
          gap: 4px;
        }
        .ynow-pts-banner-val {
          font-variant-numeric: tabular-nums;
          font-weight: 700;
        }
        .ynow-lang-stack {
          display: inline-flex !important;
          flex-direction: row !important;
          flex-wrap: nowrap !important;
          align-items: center !important;
          justify-content: center !important;
          height: 28px;
          margin: 0;
          padding: 0;
          gap: 0;
          flex-shrink: 0 !important;
          white-space: nowrap !important;
        }
        .ynow-lang-btn {
          box-sizing: border-box;
          flex: 0 0 auto;
          min-width: 42px;
          height: 28px;
          margin: 0;
          padding: 0 8px;
          border: 1px solid rgba(255,255,255,0.35);
          border-radius: 0;
          background: rgba(255,255,255,0.12);
          color: #fff !important;
          font-size: 12px;
          font-weight: 700;
          line-height: 26px;
          cursor: pointer;
          white-space: nowrap !important;
          text-shadow: none;
        }
        .ynow-lang-btn + .ynow-lang-btn {
          border-left-width: 0;
        }
        .ynow-lang-btn.active {
          background: #fff !important;
          color: #222 !important;
          border-color: #fff !important;
        }
        .ynow-lang-btn:focus {
          outline: none;
        }

        /* USD／TWD: under logo with modest gap below black header */
        .main-header,
        .main-header .navbar,
        .main-header .navbar-custom-menu,
        .main-header .navbar-custom-menu > .navbar-nav {
          overflow: visible !important;
        }
        .ynow-ccy-float {
          position: absolute;
          top: 100%;
          right: 14px;
          margin: 8px 0 0 0;
          padding: 0;
          z-index: 1035;
          display: flex;
          flex-direction: column;
          align-items: flex-end;
          gap: 4px;
          line-height: 1.15;
          pointer-events: auto;
          max-width: min(320px, calc(100vw - 16px));
        }
        .ynow-ccy-float .form-group,
        .ynow-ccy-float .shiny-input-container {
          margin: 0 !important;
        }
        .ynow-ccy-float .btn-group-xs > .btn,
        .ynow-ccy-float .btn-xs {
          background: #ffffff !important;
          border: 1px solid var(--ynow-ink) !important;
          color: var(--ynow-ink) !important;
          font-weight: 700 !important;
          min-width: 42px;
          box-shadow: 0 1px 2px rgba(26, 26, 26, 0.12);
        }
        .ynow-ccy-float .btn-group-xs > .btn.active,
        .ynow-ccy-float .btn-xs.active {
          background: var(--ynow-ink) !important;
          color: #fff !important;
          border-color: var(--ynow-ink) !important;
          box-shadow: none !important;
        }
        .ynow-ccy-float .radiobtn { margin: 0 !important; }
        .ynow-ccy-float .ynow-hdr-ccy-status,
        .ynow-ccy-float .ynow-hdr-ccy-status .shiny-text-output {
          display: block;
          font-size: 10px;
          color: rgba(26, 26, 26, 0.62);
          white-space: normal;
          overflow-wrap: anywhere;
          word-break: break-word;
          max-width: min(280px, calc(100vw - 24px));
          text-align: right;
          line-height: 1.3;
          margin: 0;
        }
        /* Credit: document-flow subband (left), same row as USD/TWD — not fixed/sticky */
        .ynow-hdr-subband {
          position: relative;
          display: flex;
          align-items: center;
          min-height: 36px;
          margin: -44px 0 10px 0;
          padding: 0;
          max-width: calc(100% - 140px);
          pointer-events: none;
        }
        .ynow-hdr-subband .ynow-credit-flow {
          pointer-events: auto;
          position: static;
          float: none;
          display: flex;
          align-items: center;
          margin: 0;
          padding: 0;
          max-width: 100%;
        }
        .ynow-hdr-subband .ynow-credit-text {
          margin: 0;
          padding: 0;
          font-size: 13px;
          font-weight: 600;
          line-height: 1.2;
          color: #666666;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        @media (max-width: 767px) {
          .ynow-hdr-subband {
            max-width: calc(100% - 120px);
            /* Do not pull into fixed black header (was -40px → covered when header stacked to 100px) */
            margin-top: 4px;
            margin-bottom: 8px;
          }
          .ynow-hdr-subband .ynow-credit-text {
            font-size: 11px;
          }
          .content-wrapper > .content {
            padding-top: 40px;
          }
        }
        /* Compact composite valuation status (replaces KPI row on model pages) */
        .ynow-val-status-header {
          background: #ffffff;
          padding: 14px 16px;
          border-radius: 8px;
          box-shadow: 0 1px 2px rgba(26, 26, 26, 0.08);
          margin-bottom: 4px;
        }
        .ynow-val-status-header h4 {
          margin: 0;
          font-weight: 700;
          font-size: 16px;
          line-height: 1.35;
        }
        .ynow-company-overview {
          margin: 0 0 14px 0;
          padding: 14px 16px;
          background: #f7f8fa;
          border: 1px solid #e2e5ea;
          border-radius: 2px;
        }
        .ynow-company-overview-head {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          justify-content: space-between;
          gap: 6px 12px;
          margin: 0 0 10px 0;
        }
        .ynow-company-overview-head-main {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 8px 10px;
          min-width: 0;
          flex: 1 1 auto;
        }
        .ynow-company-overview-title {
          font-size: 14px;
          font-weight: 700;
          color: #222;
          margin: 0;
          letter-spacing: 0.01em;
        }
        /* Important dates: Data-limited–style compact chips (inline, not a block) */
        .ynow-key-date-chips {
          display: inline-flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 6px;
        }
        .ynow-key-date-chip {
          display: inline-flex;
          align-items: center;
          gap: 6px;
          padding: 3px 10px;
          border-radius: 4px;
          border: 1px solid var(--ynow-gold, #F5C518) !important;
          background: #000000 !important;
          color: var(--ynow-gold, #F5C518) !important;
          font-size: 12px;
          font-weight: 700;
          white-space: nowrap;
          line-height: 1.2;
        }
        .ynow-key-date-chip .k {
          font-size: 10px;
          font-weight: 600;
          letter-spacing: 0.02em;
          opacity: 0.9;
        }
        .ynow-key-date-chip .v {
          font-variant-numeric: tabular-nums;
          font-weight: 700;
        }
        .ynow-own-kpi-row { margin-bottom: 8px; }
        .ynow-own-kpi-block h4 {
          font-size: 14px;
          font-weight: 700;
          margin: 0 0 8px 0;
          color: #222;
        }
        .ynow-own-kpi-grid {
          display: flex;
          flex-wrap: wrap;
          gap: 10px;
        }
        .ynow-own-kpi-cell {
          min-width: 120px;
          padding: 8px 10px;
          border: 1px solid #e5e5e5;
          background: #fafafa;
        }
        .ynow-own-kpi-label {
          font-size: 12px;
          color: #666;
          margin-bottom: 4px;
        }
        .ynow-own-kpi-value {
          font-size: 20px;
          font-weight: 700;
          font-variant-numeric: tabular-nums;
          color: #111;
        }
        .ynow-own-flow-value { font-size: 22px; }
        .ynow-own-kpi-cred,
        .ynow-own-muted,
        .ynow-own-waiting {
          font-size: 12px;
          color: #777;
          line-height: 1.45;
        }
        .ynow-own-holders-wrap { overflow-x: auto; }
        .ynow-own-holders,
        .ynow-own-top5 { font-size: 12.5px; }
        .ynow-own-holder-link {
          color: #0A72AE;
          font-weight: 600;
          text-decoration: underline;
          cursor: pointer;
        }
        .ynow-own-holder-selected { color: #0F5A90; }
        .ynow-own-detail {
          border: 1px solid #e5e5e5;
          padding: 10px 12px;
          background: #fcfcfc;
          min-height: 160px;
        }
        .ynow-own-detail h4,
        .ynow-own-detail h5 {
          margin-top: 0;
          font-size: 13px;
          font-weight: 700;
        }
        .ynow-own-notes {
          margin: 10px 0 0 0;
          padding-left: 18px;
          font-size: 12px;
          color: #666;
        }
        .ynow-corp-events {
          margin: 0;
          padding: 4px 0 8px 0;
        }
        .ynow-corp-events-h {
          font-size: 14px;
          font-weight: 700;
          margin: 0 0 8px 0;
          color: #222;
        }
        .ynow-corp-events-source,
        .ynow-corp-events-waiting {
          font-size: 12px;
          color: #777;
          margin: 8px 0 0 0;
          line-height: 1.45;
        }
        .ynow-corp-events-cand ul {
          margin: 0;
          padding-left: 18px;
          font-size: 12.5px;
          line-height: 1.45;
        }
        .ynow-corp-cb-table {
          font-size: 12.5px;
          margin-top: 8px;
        }
        .ynow-corp-cb-table th {
          white-space: nowrap;
        }
        .ynow-company-overview-source {
          font-size: 11px;
          color: #888;
          margin: 0;
        }
        .ynow-company-overview-meta {
          display: flex;
          flex-wrap: wrap;
          gap: 8px 10px;
          margin: 0 0 10px 0;
        }
        .ynow-company-overview-chip {
          display: inline-flex;
          flex-direction: column;
          gap: 2px;
          min-width: 0;
          max-width: 100%;
          padding: 6px 10px;
          background: #fff;
          border: 1px solid #e6e8ec;
          border-radius: 2px;
        }
        .ynow-company-overview-chip .k {
          font-size: 10px;
          font-weight: 700;
          color: #888;
          text-transform: uppercase;
          letter-spacing: 0.04em;
        }
        .ynow-company-overview-chip .v {
          font-size: 12.5px;
          font-weight: 600;
          color: #222;
          line-height: 1.35;
          word-break: break-word;
        }
        .ynow-company-overview-chip a.v {
          color: #1a5f9e;
          text-decoration: none;
        }
        .ynow-company-overview-chip a.v:hover {
          text-decoration: underline;
        }
        .ynow-company-overview-summary {
          font-size: 13px;
          line-height: 1.55;
          color: #333;
          margin: 0;
        }
        .ynow-company-overview-summary.ynow-co-clamp {
          display: -webkit-box;
          -webkit-box-orient: vertical;
          -webkit-line-clamp: 4;
          overflow: hidden;
        }
        .ynow-company-overview-toggle {
          margin-top: 6px;
          padding: 0;
          border: none;
          background: none;
          color: #1a5f9e;
          font-size: 12px;
          font-weight: 600;
          cursor: pointer;
        }
        .ynow-company-overview-toggle:hover {
          text-decoration: underline;
        }
        .ynow-company-overview-waiting {
          font-size: 13px;
          color: #777;
          margin: 0;
          line-height: 1.45;
        }
        @media (max-width: 767px) {
          .ynow-company-overview {
            padding: 12px;
          }
          .ynow-company-overview-chip {
            flex: 1 1 calc(50% - 10px);
          }
        }
        .ynow-ind-overview-block {
          margin: 0 0 12px 0;
          padding: 12px 14px;
          background: #f5f5f5;
          border-left: 4px solid #222222;
          border-radius: 4px;
        }
        .ynow-ind-overview-block .ynow-ind-overview-title {
          font-size: 13px;
          font-weight: 700;
          color: #222222;
          margin: 0 0 8px 0;
        }
        .ynow-ind-overview-row {
          display: flex;
          flex-direction: row;
          align-items: flex-start;
          gap: 16px;
          flex-wrap: wrap;
        }
        .ynow-ind-overview-picker {
          flex: 0 1 280px;
          min-width: 200px;
          max-width: 360px;
        }
        .ynow-ind-overview-summary {
          flex: 1 1 260px;
          min-width: 0;
        }
        .ynow-ind-overview-block .form-group {
          margin-bottom: 0;
        }
        @media (max-width: 767px) {
          .ynow-ind-overview-block {
            padding: 10px 12px;
          }
          .ynow-ind-overview-block .ynow-ind-overview-title {
            margin: 0 0 4px 0;
          }
          .ynow-ind-overview-row {
            flex-direction: column;
            gap: 4px;
            align-items: stretch;
          }
          .ynow-ind-overview-picker {
            max-width: none;
            width: 100%;
            flex: 0 0 auto;
            /* Keep closed picker to toggle height; menu is portaled via container=body */
            position: relative;
            z-index: 5;
          }
          .ynow-ind-overview-picker .form-group,
          .ynow-ind-overview-picker .shiny-input-container {
            margin-bottom: 0 !important;
          }
          .ynow-ind-overview-picker .bootstrap-select,
          .ynow-ind-overview-picker .btn-group.bootstrap-select {
            margin-bottom: 0 !important;
            width: 100% !important;
            position: relative;
            /* Do not let an in-flow leftover menu stretch this cell (~size×row ≈ huge white gap) */
            height: auto !important;
            max-height: none;
          }
          /* Closed menu must never consume vertical layout (size=12 ≈ 1/3 viewport) */
          .ynow-ind-overview-picker .bootstrap-select:not(.open):not(.show) > .dropdown-menu,
          .ynow-ind-overview-picker .bootstrap-select:not(.open):not(.show) .dropdown-menu {
            display: none !important;
            height: 0 !important;
            max-height: 0 !important;
            min-height: 0 !important;
            overflow: hidden !important;
            margin: 0 !important;
            padding: 0 !important;
            border: 0 !important;
            visibility: hidden !important;
            pointer-events: none !important;
          }
          /* If a menu remains inside the picker (no bs-container), keep it absolute */
          .ynow-ind-overview-picker .bootstrap-select > .dropdown-menu {
            position: absolute !important;
            top: 100% !important;
            left: 0 !important;
            right: auto !important;
            z-index: 1060 !important;
          }
          .ynow-ind-overview-picker select.selectpicker,
          .ynow-ind-overview-picker select.bs-select-hidden {
            display: none !important;
            height: 0 !important;
            max-height: 0 !important;
            margin: 0 !important;
            padding: 0 !important;
            border: 0 !important;
            position: absolute !important;
            clip: rect(0, 0, 0, 0) !important;
          }
          .ynow-ind-overview-summary {
            margin: 0;
            flex: 0 0 auto;
            min-height: 0 !important;
          }
          .ynow-ind-overview-block .shiny-html-output,
          .ynow-ind-overview-block .shiny-bound-output {
            min-height: 0 !important;
            height: auto !important;
          }
          .ynow-ind-overview-block .ynow-ind-yahoo-row {
            margin-top: 2px;
          }
          .ynow-ind-overview-metrics {
            margin-top: 6px;
          }
          .ynow-ind-snapshot-chips,
          .ynow-kpi-legend-chips {
            gap: 6px;
            row-gap: 6px;
            column-gap: 6px;
          }
        }
        /* DCF shared header：上列「選擇模型／採用現金流」與下列「預測年數 n／建議」同寬靠左對齊 */
        #ibx_EPS { margin-bottom: 8px; }
        .ynow-dcf-mode-row,
        .ynow-header-years-suggest-row {
          display: flex;
          flex-wrap: wrap;
          align-items: flex-start;
          margin-left: 0;
          margin-right: 0;
          padding: 0;
        }
        .ynow-dcf-mode-row > [class*="col-"],
        .ynow-header-years-suggest-row > [class*="col-"] {
          float: none;
          padding-left: 10px;
          padding-right: 10px;
        }
        .ynow-dcf-mode-row .form-group { margin-bottom: 8px; }
        .ynow-dcf-mode-col,
        .ynow-header-years {
          padding: 0 0 8px 0;
          margin-top: 0;
          width: 100%;
        }
        .ynow-header-years .form-group { margin-bottom: 0; }
        .ynow-dcf-mode-col > .form-group > label.control-label,
        .ynow-header-years label.control-label {
          font-size: 13px;
          font-weight: 700;
          color: #333;
          margin-bottom: 5px;
          min-height: 18px;
          line-height: 1.35;
          display: block;
          text-align: left;
        }
        .ynow-dcf-claim-col {
          padding: 0 0 8px 0;
          width: 100%;
        }
        .ynow-dcf-claim-suggest-wrap {
          padding: 0 0 8px 0;
          width: 100%;
        }
        .ynow-dcf-claim-suggest-wrap .ynow-dcf-claim-suggest {
          margin: 0;
          padding: 8px 10px;
          background: #f5f5f5;
          border-left: 4px solid #222;
          font-size: 12px;
          color: #444;
          line-height: 1.5;
          box-sizing: border-box;
          min-height: 34px;
          text-align: left;
        }
        /* Header KPI strip: same content column as Company Overview (.tab-pane) */
        .content-wrapper .ynow-header-kpi-shell {
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin-left: auto;
          margin-right: auto;
          box-sizing: border-box;
        }
        .content-wrapper .ynow-header-kpi-shell > .ynow-header-kpi-row {
          margin-left: 0;
          margin-right: 0;
        }
        .content-wrapper .ynow-header-kpi-shell > .ynow-header-kpi-row > [class*="col-"]:first-child {
          padding-left: 0;
        }
        .content-wrapper .ynow-header-kpi-shell > .ynow-header-kpi-row > [class*="col-"]:last-child {
          padding-right: 0;
        }
        @media (max-width: 767px) {
          .content-wrapper .ynow-header-kpi-shell {
            max-width: 100%;
          }
        }
        /* Header KPI：Last Price／Market Cap／EPS — logo 綠圖示；無金框 */
        .ynow-header-kpi-row .info-box {
          background: #ffffff;
          border: 1px solid rgba(26, 26, 26, 0.10);
          border-top: none;
          box-shadow: 0 1px 2px rgba(26, 26, 26, 0.06);
          background-image: none;
        }
        .ynow-header-kpi-row .info-box .info-box-icon,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-purple,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-aqua,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-green,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-yellow,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-blue,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-teal {
          background-color: var(--ynow-logo-green) !important;
          color: #ffffff !important;
        }
        .ynow-header-kpi-row .info-box .info-box-icon > .fa,
        .ynow-header-kpi-row .info-box .info-box-icon > .fas {
          color: #ffffff !important;
        }
        .ynow-header-kpi-row .info-box .info-box-text {
          color: var(--ynow-gold-ink) !important;
          font-weight: 700;
          letter-spacing: 0.02em;
        }
        .ynow-header-kpi-row .info-box .info-box-number,
        .ynow-header-kpi-row .info-box .info-box-number h3 {
          color: var(--ynow-ink) !important;
        }
        /* 手機：LAST PRICE / MARKET CAP / EPS (TTM) 同列 1:1:1 不換行 */
        @media (max-width: 767px) {
          .content-wrapper .ynow-header-kpi-row {
            display: flex !important;
            flex-wrap: nowrap !important;
            align-items: stretch;
          }
          .content-wrapper .ynow-header-kpi-row > [class*="col-"] {
            width: 33.333% !important;
            max-width: 33.333% !important;
            flex: 1 1 0 !important;
            float: none !important;
            display: block !important;
            clear: none !important;
            margin-left: 0 !important;
            margin-right: 0 !important;
            padding-left: 4px;
            padding-right: 4px;
            box-sizing: border-box;
          }
          .content-wrapper .ynow-header-kpi-row .info-box {
            margin-bottom: 8px;
            min-height: 0 !important;
            height: auto !important;
          }
          .content-wrapper .ynow-header-kpi-row .info-box .info-box-icon {
            display: none !important;
          }
          .content-wrapper .ynow-header-kpi-row .info-box .info-box-content {
            margin-left: 0 !important;
            padding: 8px 6px !important;
          }
          .content-wrapper .ynow-header-kpi-row .info-box .info-box-number,
          .content-wrapper .ynow-header-kpi-row .info-box .info-box-number h3 {
            font-size: clamp(15px, 4.5vw, 22px) !important;
            font-weight: 800 !important;
          }
          .content-wrapper .ynow-header-kpi-row .info-box .info-box-text {
            font-size: clamp(8px, 2.2vw, 10px) !important;
            line-height: 1.2 !important;
          }
        }

        /* HFV report shell: narrative chapters + responsive control toolbar */
        .ynow-hfv-report {
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin: 0 auto 24px auto;
          box-sizing: border-box;
        }
        .ynow-hfv-report__masthead {
          margin: 0 0 16px 0;
          padding: 4px 2px 8px 2px;
          /* No rule under the page lead — keep masthead flush into how-to-read / chapters */
          border-bottom: none;
        }
        .ynow-hfv-report__masthead h2 {
          margin: 0 0 8px 0;
          font-size: clamp(20px, 2.6vw, 28px);
          font-weight: 700;
          letter-spacing: -0.01em;
          color: #1a1a1a;
        }
        .ynow-hfv-report__lead {
          margin: 0;
          max-width: 58em;
          font-size: 13.5px;
          line-height: 1.55;
          color: #555;
        }
        .ynow-hfv-chapter__lead {
          margin: 0;
          padding: 10px 16px 0 16px;
          max-width: none;
          font-size: 13px;
          line-height: 1.5;
          color: #555;
        }
        .ynow-hfv-toolbar {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
          gap: 12px 14px;
          width: 100%;
          margin: 0 0 18px 0;
          padding: 14px 16px;
          background: #fafafa;
          border: 1px solid #e4e4e4;
          border-radius: 8px;
          box-sizing: border-box;
        }
        .ynow-hfv-toolbar--above-chart {
          margin: 8px 0 12px 0;
          grid-template-columns: 1fr;
        }
        /* Page-level common controls (under How-to-read): compact analysis frequency */
        .ynow-hfv-page-controls {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 8px 16px;
          width: 100%;
          margin: 0 0 16px 0;
          padding: 8px 12px;
          background: #f7f8fa;
          border: 1px solid #e6e8eb;
          border-radius: 6px;
          box-sizing: border-box;
        }
        .ynow-hfv-page-controls__freq {
          flex: 1 1 auto;
          min-width: 0;
        }
        .ynow-hfv-page-controls .form-group {
          margin: 0 !important;
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 8px 12px;
        }
        .ynow-hfv-page-controls .shiny-input-radiogroup > label.control-label {
          display: inline-block;
          margin: 0;
          padding: 0;
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.02em;
          text-transform: none;
          color: #555;
          line-height: 1.3;
          white-space: nowrap;
        }
        .ynow-hfv-page-controls .shiny-options-group {
          display: inline-flex !important;
          flex-wrap: wrap !important;
          align-items: center;
          gap: 6px;
          margin: 0 !important;
          padding: 0 !important;
          clear: none;
        }
        .ynow-hfv-page-controls .radio,
        .ynow-hfv-page-controls .radio-inline {
          float: none !important;
          display: inline-flex !important;
          align-items: center;
          margin: 0 !important;
          padding: 0 !important;
        }
        .ynow-hfv-page-controls .radio > label,
        .ynow-hfv-page-controls .radio-inline,
        .ynow-hfv-page-controls .radio-inline > label {
          display: inline-flex !important;
          align-items: center;
          justify-content: center;
          margin: 0 !important;
          padding: 4px 10px !important;
          min-height: 28px;
          font-size: 12px;
          font-weight: 600;
          line-height: 1.2;
          color: #444;
          background: #fff;
          border: 1px solid #d0d5dd;
          border-radius: 999px;
          cursor: pointer;
          white-space: nowrap;
        }
        .ynow-hfv-page-controls .shiny-options-group input[type="radio"] {
          position: absolute !important;
          opacity: 0 !important;
          width: 0 !important;
          height: 0 !important;
          margin: 0 !important;
          pointer-events: none;
        }
        .ynow-hfv-page-controls .shiny-options-group .radio:has(input:checked) > label,
        .ynow-hfv-page-controls .shiny-options-group .radio-inline:has(input:checked) {
          color: #0c5484;
          background: #e8f2f8;
          border-color: #0c5484;
        }
        .ynow-hfv-page-controls .ynow-hfv-page-controls__hint {
          margin: 0;
          font-size: 11px;
          line-height: 1.35;
          color: #888;
        }
        /* Section II: two equal columns + full-width OOS row; options fill each group */
        .ynow-hfv-toolbar--in-ch2 {
          margin: 0 0 16px 0;
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
        .ynow-hfv-toolbar__group {
          min-width: 0;
          width: 100%;
          display: flex;
          flex-direction: column;
          gap: 6px;
        }
        .ynow-hfv-toolbar__group--wide {
          grid-column: 1 / -1;
        }
        .ynow-hfv-toolbar__group .form-group {
          margin: 0;
          width: 100%;
        }
        .ynow-hfv-toolbar__group > label.control-label,
        .ynow-hfv-toolbar .shiny-input-radiogroup > label.control-label,
        .ynow-hfv-toolbar .shiny-input-checkboxgroup > label.control-label {
          display: block;
          margin: 0 0 6px 0;
          padding: 0;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: #666;
          line-height: 1.35;
        }
        .ynow-hfv-toolbar .shiny-options-group {
          display: flex !important;
          flex-wrap: wrap !important;
          align-items: stretch;
          gap: 6px;
          margin: 0;
          padding: 0;
          clear: none;
          width: 100%;
        }
        /* Ch2: equal cells, at most two rows, fill group width (keep pill chips) */
        .ynow-hfv-toolbar--in-ch2 #bt_fv_replay_model .shiny-options-group,
        .ynow-hfv-toolbar--in-ch2 #bt_fv_conv_window .shiny-options-group {
          display: grid !important;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 6px 8px;
        }
        .ynow-hfv-toolbar--in-ch2 #bt_fv_oos_mode .shiny-options-group {
          display: grid !important;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 6px 8px;
        }
        .ynow-hfv-toolbar--above-chart #bt_fv_models .shiny-options-group {
          display: grid !important;
          grid-template-columns: repeat(5, minmax(0, 1fr));
          gap: 6px 8px;
        }
        .ynow-hfv-toolbar .radio,
        .ynow-hfv-toolbar .radio-inline,
        .ynow-hfv-toolbar .checkbox,
        .ynow-hfv-toolbar .checkbox-inline {
          float: none !important;
          display: inline-flex !important;
          align-items: center;
          margin: 0 !important;
          padding: 0 !important;
          min-height: 0;
        }
        .ynow-hfv-toolbar--in-ch2 .radio,
        .ynow-hfv-toolbar--in-ch2 .radio-inline,
        .ynow-hfv-toolbar--in-ch2 .checkbox,
        .ynow-hfv-toolbar--in-ch2 .checkbox-inline,
        .ynow-hfv-toolbar--above-chart #bt_fv_models .checkbox,
        .ynow-hfv-toolbar--above-chart #bt_fv_models .checkbox-inline {
          width: 100%;
          display: flex !important;
        }
        .ynow-hfv-toolbar .radio > label,
        .ynow-hfv-toolbar .radio-inline,
        .ynow-hfv-toolbar .radio-inline > label,
        .ynow-hfv-toolbar .checkbox > label,
        .ynow-hfv-toolbar .checkbox-inline,
        .ynow-hfv-toolbar .checkbox-inline > label {
          margin: 0 !important;
          padding: 7px 12px 7px 28px !important;
          border: 1px solid #d0d0d0;
          border-radius: 999px;
          background: #fff;
          font-size: 12.5px;
          font-weight: 500;
          line-height: 1.25;
          color: #222;
          white-space: nowrap;
          cursor: pointer;
          transition: border-color .12s ease, background .12s ease, box-shadow .12s ease;
        }
        .ynow-hfv-toolbar--in-ch2 .radio > label,
        .ynow-hfv-toolbar--in-ch2 .radio-inline,
        .ynow-hfv-toolbar--in-ch2 .radio-inline > label,
        .ynow-hfv-toolbar--in-ch2 .checkbox > label,
        .ynow-hfv-toolbar--in-ch2 .checkbox-inline,
        .ynow-hfv-toolbar--in-ch2 .checkbox-inline > label,
        .ynow-hfv-toolbar--above-chart #bt_fv_models .checkbox > label,
        .ynow-hfv-toolbar--above-chart #bt_fv_models .checkbox-inline,
        .ynow-hfv-toolbar--above-chart #bt_fv_models .checkbox-inline > label {
          width: 100%;
          box-sizing: border-box;
          justify-content: center;
          text-align: center;
          /* No left gutter for native radio/checkbox — those are visually hidden below */
          padding: 8px 12px !important;
        }
        .ynow-hfv-toolbar--in-ch2 #bt_fv_oos_mode .radio > label,
        .ynow-hfv-toolbar--in-ch2 #bt_fv_oos_mode .radio-inline,
        .ynow-hfv-toolbar--in-ch2 #bt_fv_oos_mode .radio-inline > label {
          white-space: normal;
          min-height: 40px;
          align-items: center;
        }
        /* Default: radio/checkbox sit in the label left padding */
        .ynow-hfv-toolbar .radio input[type="radio"],
        .ynow-hfv-toolbar .radio-inline input[type="radio"],
        .ynow-hfv-toolbar .checkbox input[type="checkbox"],
        .ynow-hfv-toolbar .checkbox-inline input[type="checkbox"] {
          position: absolute;
          margin-left: -20px;
          margin-top: 1px;
        }
        /* Chip option groups: hide native dots so they never cover centered label text.
           Selection is shown by the dark filled chip (input:checked). */
        .ynow-hfv-toolbar .shiny-options-group input[type="radio"],
        .ynow-hfv-toolbar .shiny-options-group input[type="checkbox"] {
          position: absolute !important;
          left: 0 !important;
          top: 0 !important;
          width: 1px !important;
          height: 1px !important;
          margin: 0 !important;
          padding: 0 !important;
          opacity: 0 !important;
          clip: rect(0, 0, 0, 0);
          pointer-events: none;
        }
        .ynow-hfv-toolbar .shiny-options-group .radio > label:hover,
        .ynow-hfv-toolbar .shiny-options-group .radio-inline:hover,
        .ynow-hfv-toolbar .shiny-options-group .checkbox > label:hover,
        .ynow-hfv-toolbar .shiny-options-group .checkbox-inline:hover {
          border-color: #999;
          background: #fff;
        }
        .ynow-hfv-toolbar .shiny-options-group .radio:has(input:checked) > label,
        .ynow-hfv-toolbar .shiny-options-group .radio-inline:has(input:checked),
        .ynow-hfv-toolbar .shiny-options-group .checkbox:has(input:checked) > label,
        .ynow-hfv-toolbar .shiny-options-group .checkbox-inline:has(input:checked) {
          border-color: #1a1a1a;
          background: #1a1a1a;
          color: #fff;
          box-shadow: 0 1px 2px rgba(0,0,0,0.12);
        }
        /* Standalone checkbox (Show benchmark): plain flex row, never a filled chip */
        .ynow-hfv-toolbar .checkbox:not(.checkbox-inline) > label {
          display: inline-flex !important;
          align-items: center !important;
          gap: 8px;
          border: none !important;
          background: transparent !important;
          box-shadow: none !important;
          border-radius: 0 !important;
          padding: 2px 0 !important;
          color: #333 !important;
          width: auto !important;
          justify-content: flex-start !important;
          text-align: left !important;
          white-space: normal !important;
          min-height: 0 !important;
        }
        .ynow-hfv-toolbar .checkbox:not(.checkbox-inline) input[type="checkbox"] {
          position: static !important;
          margin: 0 !important;
          opacity: 1 !important;
          width: auto !important;
          height: auto !important;
          clip: auto !important;
          pointer-events: auto !important;
          flex: 0 0 auto;
        }
        .ynow-hfv-toolbar__hint {
          margin: 2px 0 0 0;
          font-size: 11px;
          line-height: 1.45;
          color: #888;
        }
        .ynow-hfv-chapter {
          margin: 0 0 18px 0;
          padding: 0;
          background: #fff;
          border: 1px solid #e6e6e6;
          border-radius: 8px;
          overflow: hidden;
        }
        .ynow-hfv-chapter__head {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 6px 12px;
          padding: 12px 16px;
          background: #fff;
          border-bottom: 1px solid #ececec;
        }
        .ynow-hfv-chapter__kicker {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.06em;
          text-transform: uppercase;
          color: #888;
        }
        .ynow-hfv-chapter__title {
          margin: 0;
          font-size: 16px;
          font-weight: 700;
          color: #1a1a1a;
          line-height: 1.35;
        }
        .ynow-hfv-chapter__body {
          padding: 12px 16px 16px 16px;
        }
        .ynow-hfv-chapter--appendix .ynow-hfv-chapter__body {
          background: #fafafa;
        }
        .ynow-hfv-report > .box {
          margin-bottom: 14px;
          border-radius: 8px;
          box-shadow: none;
          border: 1px solid #e6e6e6;
        }
        .ynow-hfv-report > .box > .box-header {
          padding: 10px 14px;
        }
        .ynow-hfv-report > .box > .box-body {
          padding: 12px 14px 14px 14px;
        }
        .ynow-hfv-findings-block:last-child {
          margin-bottom: 0;
        }
        .ynow-hfv-findings-block > h5 {
          margin: 0 0 10px 0;
          font-size: 13px;
          font-weight: 700;
          color: #333;
        }
        .ynow-hfv-kpi-grid {
          display: grid;
          grid-template-columns: repeat(4, minmax(0, 1fr));
          gap: 10px;
          align-items: stretch;
          width: 100%;
          margin: 0 0 10px 0;
          box-sizing: border-box;
        }
        .ynow-hfv-kpi-grid--3 {
          grid-template-columns: repeat(3, minmax(0, 1fr));
        }
        .ynow-hfv-kpi-cell {
          display: flex;
          flex-direction: column;
          gap: 4px;
          min-height: 88px;
          height: 100%;
          padding: 10px 12px;
          box-sizing: border-box;
          border: 1px solid #e8e8e8;
          border-radius: 4px;
          background: #fff;
        }
        /* Gain/loss chrome: CSS vars from Settings (auto follows US/TW market). */
        body {
          --ynow-chg-up: #1e7a46;
          --ynow-chg-down: #c0392b;
          --ynow-chg-up-wash: #f7fbf8;
          --ynow-chg-down-wash: #fdf7f7;
        }
        body.ynow-market-tw:not([data-ynow-gain-scheme]),
        body.ynow-market-tw[data-ynow-gain-scheme="auto"] {
          --ynow-chg-up: #c0392b;
          --ynow-chg-down: #1e7a46;
          --ynow-chg-up-wash: #fdf7f7;
          --ynow-chg-down-wash: #f7fbf8;
        }
        body[data-ynow-gain-scheme="green_red"] {
          --ynow-chg-up: #1e7a46;
          --ynow-chg-down: #c0392b;
          --ynow-chg-up-wash: #f7fbf8;
          --ynow-chg-down-wash: #fdf7f7;
        }
        body[data-ynow-gain-scheme="red_green"] {
          --ynow-chg-up: #c0392b;
          --ynow-chg-down: #1e7a46;
          --ynow-chg-up-wash: #fdf7f7;
          --ynow-chg-down-wash: #f7fbf8;
        }
        body[data-ynow-gain-scheme="blue_red"] {
          --ynow-chg-up: #0C5484;
          --ynow-chg-down: #c0392b;
          --ynow-chg-up-wash: #f3f7fb;
          --ynow-chg-down-wash: #fdf7f7;
        }
        body[data-ynow-gain-scheme="red_blue"] {
          --ynow-chg-up: #c0392b;
          --ynow-chg-down: #0C5484;
          --ynow-chg-up-wash: #fdf7f7;
          --ynow-chg-down-wash: #f3f7fb;
        }
        /* HFV bullish/bearish tones follow --ynow-chg-* (Settings / market auto). */
        .ynow-hfv-bull { color: var(--ynow-chg-up) !important; }
        .ynow-hfv-bear { color: var(--ynow-chg-down) !important; }
        .ynow-hfv-kpi-cell--bull {
          background: var(--ynow-chg-up-wash);
          border-left: 4px solid var(--ynow-chg-up);
        }
        .ynow-hfv-kpi-cell--bear {
          background: var(--ynow-chg-down-wash);
          border-left: 4px solid var(--ynow-chg-down);
        }
        .ynow-hfv-kpi-cell--pair {
          background: #fafafa;
          border-left: 4px solid #adb5bd;
        }
        .ynow-hfv-kpi-pair-vals {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 2px 4px;
          line-height: 1.2;
        }
        .ynow-hfv-kpi-pair-sep {
          color: #999 !important;
          font-weight: 500;
        }
        .ynow-hfv-overlay-replay-note {
          margin: 8px 0 0 0 !important;
          font-size: 11.5px !important;
          line-height: 1.45;
          color: #666;
        }
        .ynow-hfv-scenario-under-chart {
          margin-top: 14px;
        }
        @media (max-width: 991px) {
          .ynow-hfv-kpi-grid,
          .ynow-hfv-kpi-grid--3 {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }
        }
        @media (max-width: 767px) {
          .ynow-hfv-toolbar,
          .ynow-hfv-toolbar--in-ch2 {
            grid-template-columns: 1fr;
            padding: 12px;
            gap: 14px;
          }
          .ynow-hfv-toolbar__group--wide {
            grid-column: auto;
          }
          .ynow-hfv-toolbar--in-ch2 #bt_fv_replay_model .shiny-options-group,
          .ynow-hfv-toolbar--in-ch2 #bt_fv_conv_window .shiny-options-group,
          .ynow-hfv-toolbar--in-ch2 #bt_fv_oos_mode .shiny-options-group,
          .ynow-hfv-toolbar--above-chart #bt_fv_models .shiny-options-group {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }
          .ynow-hfv-page-controls .form-group {
            flex-direction: column;
            align-items: flex-start;
          }
          .ynow-hfv-toolbar .radio > label,
          .ynow-hfv-toolbar .radio-inline,
          .ynow-hfv-toolbar .checkbox > label,
          .ynow-hfv-toolbar .checkbox-inline {
            white-space: normal;
            max-width: 100%;
          }
          .ynow-hfv-kpi-grid,
          .ynow-hfv-kpi-grid--3 {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }
          .ynow-hfv-chapter__body {
            padding: 12px;
          }
          .ynow-hfv-chapter__head {
            padding: 10px 12px;
          }
          .ynow-hfv-chapter__lead {
            padding: 8px 12px 0 12px;
          }
          .ynow-hfv-report__masthead {
            padding-bottom: 12px;
          }
        }

        /* HFV 設定：統計期間／驗證樣本範圍共用 label→選項間距與區塊節奏 */
        .ynow-hfv-settings .shiny-input-radiogroup {
          margin-top: 0;
          margin-bottom: 12px;
        }
        .ynow-hfv-settings .shiny-input-radiogroup > label.control-label {
          display: block;
          margin-top: 0;
          margin-bottom: 6px;
          padding: 0;
          line-height: 1.4;
        }
        .ynow-hfv-settings .shiny-input-radiogroup .shiny-options-group {
          margin-top: 0;
          margin-bottom: 0;
          padding-top: 0;
          padding-left: 0;
        }
        /* 驗證樣本範圍：工具列內改為橫向 chip；舊直排規則僅保留於非 toolbar */
        .ynow-hfv-settings #bt_fv_oos_mode .shiny-options-group {
          display: flex !important;
          flex-direction: column !important;
          flex-wrap: nowrap !important;
          align-items: flex-start !important;
          column-gap: 0 !important;
          row-gap: 4px;
          text-align: left;
        }
        .ynow-hfv-settings #bt_fv_oos_mode .radio {
          display: block !important;
          float: none !important;
          width: auto;
          max-width: 100%;
          margin-top: 0 !important;
          margin-bottom: 0 !important;
          min-height: 0;
          padding-left: 0;
          text-align: left;
        }
        .ynow-hfv-settings #bt_fv_oos_mode .radio > label {
          display: inline-block;
          text-align: left;
          white-space: normal;
          font-weight: normal;
          margin: 0;
          padding-left: 20px;
        }
      ')),
      # Lite sidebar omit CSS (guest-locked ∪ Full-only ∪ Account)
      if (exists("ynow_sidebar_lite_omit_css", mode = "function")) {
        tags$style(HTML(ynow_sidebar_lite_omit_css()))
      } else if (exists("ynow_sidebar_guest_lock_lite_css", mode = "function")) {
        tags$style(HTML(ynow_sidebar_guest_lock_lite_css()))
      },
      # Guest-lock map (lock glyphs) + Lite omit list + membership A/B download ids
      tags$script(HTML(paste0(
        if (exists("ynow_sidebar_guest_lock_js_map", mode = "function")) {
          paste0(
            "window.__YNOW_SIDEBAR_GUEST_LOCK_TABS = ",
            jsonlite::toJSON(ynow_sidebar_guest_lock_js_map(), auto_unbox = TRUE, null = "null"),
            ";"
          )
        } else {
          "window.__YNOW_SIDEBAR_GUEST_LOCK_TABS = {lab_notes:true,decision_checklist:true,mos_assumptions:true,price_alerts:true,asset_transmission:true};"
        },
        if (exists("ynow_sidebar_persist_lock_js_map", mode = "function")) {
          paste0(
            "window.__YNOW_SIDEBAR_PERSIST_LOCK_TABS = ",
            jsonlite::toJSON(ynow_sidebar_persist_lock_js_map(), auto_unbox = TRUE, null = "null"),
            ";"
          )
        } else {
          "window.__YNOW_SIDEBAR_PERSIST_LOCK_TABS = {};"
        },
        if (exists("ynow_sidebar_lite_omit_tabs", mode = "function")) {
          paste0(
            "window.__YNOW_LITE_OMIT_TABS = ",
            jsonlite::toJSON(ynow_sidebar_lite_omit_tabs(), auto_unbox = TRUE),
            ";"
          )
        } else {
          "window.__YNOW_LITE_OMIT_TABS = [];"
        },
        if (exists("ynow_gated_download_ids", mode = "function")) {
          .ynow_dl_ids <- ynow_gated_download_ids()
          .ynow_dl_tiers <- if (exists("ynow_membership_lock_tier", mode = "function")) {
            stats::setNames(
              lapply(.ynow_dl_ids, function(id) ynow_membership_lock_tier(id)),
              .ynow_dl_ids
            )
          } else {
            list()
          }
          paste0(
            "window.__YNOW_GATED_DOWNLOAD_IDS = ",
            jsonlite::toJSON(as.character(.ynow_dl_ids), auto_unbox = TRUE),
            ";",
            "window.__YNOW_GATED_DOWNLOAD_TIERS = ",
            jsonlite::toJSON(.ynow_dl_tiers, auto_unbox = TRUE, null = "null"),
            ";"
          )
        } else {
          paste0(
            "window.__YNOW_GATED_DOWNLOAD_IDS = ",
            '["download_report","download_snapshot","download_param_restore",',
            '"download_defaults","lab_im_download_report","bblab-export_data",',
            '"bblab-export_chart","param_audit_pdf_go"];',
            "window.__YNOW_GATED_DOWNLOAD_TIERS = {};"
          )
        }
      ))),
      # Boot locale maps (slim): click-time apply so 繁中／EN do not depend on websocket timing
      tags$script(HTML(paste0(
        "window.__YNOW_UI_LOCALE_BOOT = ",
        jsonlite::toJSON(ui_locale_boot_bundle(), auto_unbox = TRUE, null = "null"),
        ";"
      ))),
      # First-paint preferred locale from navigator (before body paint of dual-lang blocks).
      tags$script(HTML("
        (function () {
          function ynowPreferredUiLocale() {
            var tags = [];
            try {
              if (navigator.languages && navigator.languages.length) {
                for (var i = 0; i < navigator.languages.length; i++) {
                  var t = String(navigator.languages[i] || '').trim();
                  if (t) tags.push(t);
                }
              } else if (navigator.language) {
                var one = String(navigator.language || '').trim();
                if (one) tags.push(one);
              }
            } catch (eLang) {}
            for (var j = 0; j < tags.length; j++) {
              var raw = String(tags[j] || '').toLowerCase().replace(/_/g, '-');
              raw = raw.split(';')[0];
              if (!raw) continue;
              if (raw === 'zh' || raw.indexOf('zh-') === 0 ||
                  raw === 'chinese' || raw.indexOf('chinese-') === 0) {
                return 'zh-TW';
              }
              if (raw === 'en' || raw.indexOf('en-') === 0 ||
                  raw === 'english' || raw.indexOf('english-') === 0) {
                return 'en';
              }
            }
            return 'en';
          }
          var pref = ynowPreferredUiLocale();
          window.__ynowPreferredLocale = pref;
          try {
            document.documentElement.setAttribute('lang', pref === 'zh-TW' ? 'zh-TW' : 'en');
          } catch (eSet) {}
          if (pref === 'zh-TW') {
            try {
              document.documentElement.setAttribute('data-ynow-boot-zh', '1');
              window.__ynowLocaleBootGuard = 'zh-TW';
            } catch (eBoot) {}
          }
          function syncLangBtns() {
            try {
              var zh = document.getElementById('ynow_lang_btn_zh');
              var en = document.getElementById('ynow_lang_btn_en');
              if (zh) zh.classList.toggle('active', pref === 'zh-TW');
              if (en) en.classList.toggle('active', pref === 'en');
            } catch (eBtn) {}
          }
          if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', syncLangBtns);
          } else {
            syncLangBtns();
          }
        })();
      ")),
      tags$script(HTML("
        (function () {
          var TAB_PARENT = {
            nav_calculator: 'ynow_menu_cat_asset',
            pb_calculator: 'ynow_menu_cat_asset',
            dcf_calculator: 'ynow_menu_cat_income',
            ddm_calculator: 'ynow_menu_cat_income',
            ri_calculator: 'ynow_menu_cat_income',
            rel_mult_ev: 'ynow_menu_cat_relative',
            rel_multiples_calculator: 'ynow_menu_cat_relative',
            rel_mult_rev: 'ynow_menu_cat_relative',
            sotp_calculator: 'ynow_menu_cat_meta'
          };
          var LAST_BADGE_MAP = null;

          function clearRecBadges(a) {
            if (!a) return;
            a.querySelectorAll('small.ynow-sidebar-badge, small.ynow-rec-badge, .ynow-rec-slot').forEach(function (b) {
              b.remove();
            });
          }

          /* Wrap bare text so flex can shrink the label; otherwise badges clip */
          function ensureShrinkableLabel(a) {
            if (!a) return;
            var nodes = Array.prototype.slice.call(a.childNodes);
            for (var i = 0; i < nodes.length; i++) {
              var n = nodes[i];
              if (n.nodeType !== 3) continue;
              var t = String(n.textContent || '').replace(/\\s+/g, ' ').trim();
              if (!t) continue;
              var span = document.createElement('span');
              span.className = 'ynow-menu-label';
              span.textContent = ' ' + t + ' ';
              a.replaceChild(span, n);
            }
          }

          function findChevron(a) {
            if (!a) return null;
            var kids = a.children;
            for (var i = 0; i < kids.length; i++) {
              var el = kids[i];
              if (!el.classList) continue;
              if (el.classList.contains('pull-right-container')) return el;
              if (el.classList.contains('pull-right') &&
                  (el.classList.contains('fa') || el.classList.contains('fas') ||
                   el.classList.contains('far') || el.classList.contains('glyphicon'))) {
                return el;
              }
              if (el.classList.contains('fa-angle-left') || el.classList.contains('fa-angle-down')) {
                return el;
              }
            }
            return null;
          }

          function setRecBadgeOnAnchor(a, role, labels) {
            if (!a) return;
            clearRecBadges(a);
            if (!role || role === 'reference') return;
            ensureShrinkableLabel(a);
            var lab = labels || {};
            var badge = document.createElement('small');
            var cls = 'ynow-rec-secondary';
            var text = lab.secondary || 'Secondary';
            if (role === 'primary') {
              cls = 'ynow-rec-primary';
              text = lab.primary || 'Recommend';
            } else if (role === 'reference') {
              cls = 'ynow-rec-reference';
              text = lab.reference || 'Reference';
            }
            badge.className = 'ynow-sidebar-badge ' + cls;
            badge.textContent = text;
            var slot = document.createElement('span');
            slot.className = 'ynow-rec-slot';
            slot.appendChild(badge);
            var chev = findChevron(a);
            if (chev) a.insertBefore(slot, chev);
            else a.appendChild(slot);
          }

          function setTabBadge(tab, role, labels) {
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]');
            setRecBadgeOnAnchor(a, role || '', labels);
          }

          function setParentBadges(map, labels) {
            var rolesByParent = {
              ynow_menu_cat_asset: [],
              ynow_menu_cat_income: [],
              ynow_menu_cat_relative: [],
              ynow_menu_cat_meta: []
            };
            Object.keys(TAB_PARENT).forEach(function (tab) {
              var role = (map && map[tab] && map[tab].role) ? String(map[tab].role) : '';
              var pid = TAB_PARENT[tab];
              if (role && rolesByParent[pid]) rolesByParent[pid].push(role);
            });
            Object.keys(rolesByParent).forEach(function (pid) {
              var span = document.getElementById(pid);
              if (!span) return;
              var a = span.closest ? span.closest('a') : null;
              if (!a) a = span.parentElement;
              var roles = rolesByParent[pid] || [];
              var parentRole = '';
              if (roles.indexOf('primary') >= 0) parentRole = 'primary';
              else if (roles.indexOf('secondary') >= 0) parentRole = 'secondary';
              setRecBadgeOnAnchor(a, parentRole, labels);
            });
          }

          function applySidebarBadges(map) {
            LAST_BADGE_MAP = map || null;
            var labels = (map && map.labels) || {};
            var tabs = ['dcf_calculator', 'ddm_calculator', 'pb_calculator', 'rel_mult_ev', 'rel_multiples_calculator', 'rel_mult_rev', 'sotp_calculator', 'ri_calculator', 'nav_calculator'];
            tabs.forEach(function (t) {
              var role = (map && map[t] && map[t].role) ? String(map[t].role) : '';
              if (!role && map && map[t] && map[t].on) role = 'primary';
              setTabBadge(t, role, labels);
            });
            setParentBadges(map, labels);
          }

          function registerBadgeHandler() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerBadgeHandler, 50);
              return;
            }
            Shiny.addCustomMessageHandler('ynowSidebarBadges', applySidebarBadges);
            function pingReady() {
              if (!(window.Shiny && Shiny.setInputValue)) {
                setTimeout(pingReady, 50);
                return;
              }
              Shiny.setInputValue('ynow_sidebar_badges_ready', Date.now(), {priority: 'event'});
            }
            pingReady();
            if (window.jQuery) {
              jQuery(document).on('shiny:connected shiny:sessioninitialized', function () {
                setTimeout(pingReady, 0);
                setTimeout(pingReady, 400);
                if (LAST_BADGE_MAP) applySidebarBadges(LAST_BADGE_MAP);
              });
            }
          }
          registerBadgeHandler();

          /* 美股／台股：搬到三線 toggle 正後方。
             Desktop: absolute left = toggle 右緣（無空隙）。
             Mobile: static flex sibling after toggle (black-bar leftmost chrome). */
          function placeMarketHeaderByToggle() {
            var navbar = document.querySelector('.main-header .navbar');
            var market = document.getElementById('ynow-market-header');
            if (!navbar || !market) return;
            var toggle = null;
            var kids = navbar.children;
            for (var i = 0; i < kids.length; i++) {
              if (kids[i].classList && kids[i].classList.contains('sidebar-toggle')) {
                toggle = kids[i];
                break;
              }
            }
            if (!toggle) toggle = navbar.querySelector('a.sidebar-toggle, .sidebar-toggle');
            if (!toggle) return;
            /* Always re-assert beside toggle — AdminLTE may steal the node back
               into .navbar-custom-menu / .navbar-nav after our first pass. */
            if (!(market.parentNode === navbar && market.previousElementSibling === toggle)) {
              navbar.insertBefore(market, toggle.nextSibling);
            }
            var mobile = window.innerWidth <= 767;
            if (mobile) {
              market.style.setProperty('left', 'auto', 'important');
              market.style.setProperty('right', 'auto', 'important');
              market.style.setProperty('top', 'auto', 'important');
              market.style.setProperty('position', 'static', 'important');
              market.style.setProperty('float', 'none', 'important');
              market.style.setProperty('display', 'flex', 'important');
              market.style.setProperty('align-items', 'center', 'important');
            } else {
              var leftPx = Math.round(toggle.offsetLeft + toggle.offsetWidth);
              if (!(leftPx > 0)) leftPx = Math.round(toggle.offsetWidth) || 44;
              market.style.left = leftPx + 'px';
              market.style.top = '0';
              market.style.position = '';
            }
            market.style.marginLeft = '0';
            market.style.paddingLeft = '0';
            try { placeMobileAppTitle(); } catch (eTitle) {}
          }
          window.ynowPlaceMarketHeaderByToggle = placeMarketHeaderByToggle;
          /* Mobile: app title viewport-centered in the gap; scale font to fit — never cover chrome. */
          function placeMobileAppTitle() {
            var logo = document.querySelector('.main-header .logo');
            if (!logo) return;
            var title = logo.querySelector('.ynow-app-title');
            var base = title ? title.querySelector('.ynow-app-title-base') : null;
            var fillInner = title ? title.querySelector('.ynow-app-title-fill-inner') : null;
            if (window.innerWidth > 767) {
              logo.style.left = '';
              logo.style.right = '';
              if (title) {
                title.style.maxWidth = '';
                title.style.fontSize = '';
              }
              if (base) base.style.fontSize = '';
              if (fillInner) fillInner.style.fontSize = '';
              return;
            }
            logo.style.left = '0px';
            logo.style.right = '0px';
            if (!title || !base) return;
            var mkt = document.getElementById('ynow-market-header');
            var custom = document.querySelector('.main-header .navbar-custom-menu');
            var ptsHdr = document.getElementById('ynow-pts-header');
            var langHdr = document.querySelector('.main-header .ynow-lang-header');
            var gap = 10;
            var leftClear = mkt ? Math.round(mkt.getBoundingClientRect().right) : 90;
            /* Right clear: protect locale + FYN/PYN pts + mark — title must not paint over them. */
            var rightEdge = window.innerWidth;
            var pushLeft = function (el) {
              if (!el) return;
              try {
                var cs = window.getComputedStyle(el);
                if (cs.display === 'none' || cs.visibility === 'hidden') return;
                var r = el.getBoundingClientRect();
                if (r.width > 0 && r.left > 0) rightEdge = Math.min(rightEdge, r.left);
              } catch (ePush) {}
            };
            pushLeft(ptsHdr);
            pushLeft(langHdr);
            pushLeft(document.querySelector('.main-header .ynow-auth-header'));
            if (custom) pushLeft(custom);
            var rightClear = Math.max(80, Math.round(window.innerWidth - rightEdge));
            var maxW = Math.max(64, window.innerWidth - leftClear - rightClear - gap * 2);
            title.style.maxWidth = maxW + 'px';
            title.style.overflow = 'hidden';
            title.style.textOverflow = 'clip';
            /* Prefer readable size; shrink proportionally so full version string stays visible */
            var fs = 16;
            var minFs = 9;
            base.style.fontSize = fs + 'px';
            if (fillInner) fillInner.style.fontSize = fs + 'px';
            title.style.fontSize = fs + 'px';
            var guard = 0;
            while (base.scrollWidth > maxW + 1 && fs > minFs && guard < 24) {
              fs -= 0.5;
              base.style.fontSize = fs + 'px';
              if (fillInner) fillInner.style.fontSize = fs + 'px';
              title.style.fontSize = fs + 'px';
              guard++;
            }
          }
          window.placeMobileAppTitle = placeMobileAppTitle;

          /* Header title = load progress bar (fills with boot / Shiny busy / withProgress) */
          document.title = 'The YNow App';

          (function initYnowTitleLoadBar() {
            var pct = 0;
            var firstIdleDone = false;
            var busyTimer = null;
            var completeTimer = null;
            function titleEl() {
              return document.getElementById('ynow_app_title') ||
                document.querySelector('.main-header .logo .ynow-app-title');
            }
            function setPct(n, opts) {
              opts = opts || {};
              n = Math.max(0, Math.min(100, Number(n) || 0));
              if (!opts.force && n < pct && pct >= 100 && firstIdleDone && !opts.busy) {
                return;
              }
              if (!opts.force && n < pct && !opts.busy) {
                /* allow only forward during boot; busy may reset */
                if (!(opts.allowDrop)) return;
              }
              pct = n;
              var el = titleEl();
              if (!el) return;
              el.style.setProperty('--ynow-load-pct', pct + '%');
              el.setAttribute('aria-valuenow', String(Math.round(pct)));
              if (pct < 100) {
                el.classList.add('is-loading');
                el.classList.remove('is-complete');
              }
            }
            function markComplete() {
              setPct(100, { force: true });
              var el = titleEl();
              if (!el) return;
              el.classList.remove('is-loading');
              el.classList.add('is-complete');
              el.setAttribute('aria-valuenow', '100');
              firstIdleDone = true;
            }
            function readShinyProgressPct() {
              var bar = document.querySelector('.shiny-progress .progress-bar');
              if (!bar) return null;
              var styleW = bar.style.width || '';
              var m = String(styleW).match(/([0-9.]+)[ ]*%/);
              if (m) return Math.max(0, Math.min(100, parseFloat(m[1])));
              try {
                var parent = bar.parentElement;
                if (parent && parent.clientWidth > 0) {
                  return Math.max(0, Math.min(100, (bar.clientWidth / parent.clientWidth) * 100));
                }
              } catch (e) {}
              return null;
            }
            function onBusy() {
              if (completeTimer) {
                clearTimeout(completeTimer);
                completeTimer = null;
              }
              var el = titleEl();
              if (el) {
                el.classList.add('is-loading');
                el.classList.remove('is-complete');
              }
              var sp = readShinyProgressPct();
              if (sp != null) {
                setPct(Math.max(8, sp), { force: true, busy: true, allowDrop: true });
              } else {
                setPct(Math.max(pct < 100 ? pct : 12, 12), { force: true, busy: true, allowDrop: true });
              }
              if (busyTimer) clearInterval(busyTimer);
              busyTimer = setInterval(function () {
                var p = readShinyProgressPct();
                if (p != null) {
                  setPct(Math.max(8, Math.min(98, p)), { force: true, busy: true, allowDrop: true });
                  return;
                }
                if (pct < 90) setPct(pct + 1.2, { force: true, busy: true });
              }, 180);
            }
            function onIdle() {
              if (busyTimer) {
                clearInterval(busyTimer);
                busyTimer = null;
              }
              setPct(100, { force: true });
              if (completeTimer) clearTimeout(completeTimer);
              completeTimer = setTimeout(markComplete, 320);
            }
            setPct(6, { force: true });
            function onDomReady() { setPct(28); }
            function onWinLoad() { setPct(48); }
            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', onDomReady);
            } else {
              onDomReady();
            }
            if (document.readyState === 'complete') {
              onWinLoad();
            } else {
              window.addEventListener('load', onWinLoad);
            }
            function bindShinyLoad() {
              if (!window.jQuery) {
                setTimeout(bindShinyLoad, 40);
                return;
              }
              var $doc = jQuery(document);
              $doc.on('shiny:connected', function () { setPct(66); });
              $doc.on('shiny:sessioninitialized', function () { setPct(82); });
              $doc.on('shiny:busy', onBusy);
              $doc.on('shiny:idle', onIdle);
              /* If already idle after connect, complete */
              setTimeout(function () {
                if (!firstIdleDone && pct >= 66) onIdle();
              }, 1800);
            }
            bindShinyLoad();
            if (typeof MutationObserver !== 'undefined') {
              var obs = new MutationObserver(function () {
                var p = readShinyProgressPct();
                if (p == null) return;
                var el = titleEl();
                if (el && el.classList.contains('is-loading')) {
                  setPct(Math.max(8, Math.min(99, p)), { force: true, busy: true, allowDrop: true });
                }
              });
              obs.observe(document.documentElement, {
                childList: true,
                subtree: true,
                attributes: true,
                attributeFilter: ['style', 'class']
              });
            }
          })();

          /* Sidebar tab label: animated 「…」 while that tab's feature is loading */
          (function initYnowSidebarTabBusyDots() {
            var sourcesByTab = {};
            var shinyArmTimer = null;
            var shinyTab = null;

            function remapTab(tab) {
              tab = String(tab || '');
              if (tab === 'business_breakdown_lab') return 'company_advance';
              return tab;
            }
            function currentTab() {
              if (typeof window.ynowCurrentSidebarTab === 'function') {
                try { return remapTab(window.ynowCurrentSidebarTab() || ''); } catch (e0) {}
              }
              var active = document.querySelector('.sidebar-menu li.active > a[data-value]');
              if (active) return remapTab(active.getAttribute('data-value') || '');
              var pane = document.querySelector('.tab-content > .tab-pane.active');
              if (pane && pane.id && pane.id.indexOf('shiny-tab-') === 0) {
                return remapTab(pane.id.slice('shiny-tab-'.length));
              }
              return '';
            }
            function tabAnchor(tab) {
              tab = remapTab(tab);
              if (!tab) return null;
              return document.querySelector('.main-sidebar .sidebar-menu a[data-value=\"' + tab + '\"]') ||
                     document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]');
            }
            function ensureDots(a) {
              if (!a) return null;
              var existing = null;
              for (var i = 0; i < a.children.length; i++) {
                if (a.children[i].classList && a.children[i].classList.contains('ynow-tab-busy-dots')) {
                  existing = a.children[i];
                  break;
                }
              }
              if (existing) return existing;
              var dots = document.createElement('span');
              dots.className = 'ynow-tab-busy-dots';
              dots.setAttribute('aria-hidden', 'true');
              dots.innerHTML = '<span>.</span><span>.</span><span>.</span>';
              var before = null;
              for (var j = 0; j < a.children.length; j++) {
                var el = a.children[j];
                if (!el.classList) continue;
                if (el.classList.contains('ynow-rec-slot') ||
                    el.classList.contains('pull-right-container') ||
                    el.classList.contains('fa-angle-left') ||
                    el.classList.contains('fa-angle-down')) {
                  before = el;
                  break;
                }
              }
              if (before) a.insertBefore(dots, before);
              else a.appendChild(dots);
              return dots;
            }
            function syncHeaderLogoSpin() {
              var mark = document.getElementById('ynow_header_logo_home');
              if (!mark || !mark.classList) return;
              var tab = currentTab();
              var busy = !!(tab && sourcesByTab[tab] && Object.keys(sourcesByTab[tab]).length > 0);
              /* Pending nav/refresh: spin even before the pane switch settles. */
              if (!busy) {
                Object.keys(sourcesByTab).forEach(function (t) {
                  var s = sourcesByTab[t] || {};
                  if (s.refresh || s.nav) busy = true;
                });
              }
              mark.classList.toggle('ynow-logo-spinning', busy);
              if (!busy) mark.classList.remove('ynow-logo-spin-fast');
              try { mark.setAttribute('aria-busy', busy ? 'true' : 'false'); } catch (eSpin) {}
            }
            function releaseFastRefresh(tab) {
              var mark = document.getElementById('ynow_header_logo_home');
              if (!mark || !mark.classList || !mark.classList.contains('ynow-logo-spin-fast')) return;
              tab = remapTab(tab);
              if (!tab || tabHasOtherBusy(tab)) return;
              if (sourcesByTab[tab] && sourcesByTab[tab].refresh) {
                setTabBusy(tab, false, 'refresh');
              }
            }
            function tabHasOtherBusy(tab) {
              var src = (tab && sourcesByTab[tab]) ? sourcesByTab[tab] : {};
              var keys = Object.keys(src);
              for (var i = 0; i < keys.length; i++) {
                if (keys[i] !== 'refresh' && src[keys[i]]) return true;
              }
              return false;
            }
            function clearOtherTabBusy(keep) {
              keep = remapTab(keep);
              Object.keys(sourcesByTab).forEach(function (t) {
                if (keep && t === keep) return;
                if (!sourcesByTab[t]) return;
                delete sourcesByTab[t];
                applyVisual(t);
              });
            }
            function focusCurrentTabBusy(keep) {
              keep = remapTab(keep);
              if (keep) setTabBusy(keep, true, 'refresh');
              clearOtherTabBusy(keep);
            }
            function applyVisual(tab) {
              tab = remapTab(tab);
              var a = tabAnchor(tab);
              if (!a) {
                syncHeaderLogoSpin();
                return;
              }
              var src = sourcesByTab[tab] || {};
              var busy = Object.keys(src).length > 0;
              ensureDots(a);
              a.classList.toggle('ynow-tab-is-busy', busy);
              var li = a.closest ? a.closest('li') : a.parentElement;
              if (li && li.classList) li.classList.toggle('ynow-tab-is-busy', busy);
              if (busy) {
                try { a.setAttribute('aria-busy', 'true'); } catch (e1) {}
              } else {
                try { a.removeAttribute('aria-busy'); } catch (e2) {}
              }
              syncHeaderLogoSpin();
            }
            function setTabBusy(tab, on, source) {
              tab = remapTab(tab);
              source = String(source || 'server');
              if (!tab) return;
              var hadOther = tabHasOtherBusy(tab);
              if (!sourcesByTab[tab]) sourcesByTab[tab] = {};
              if (on) {
                sourcesByTab[tab][source] = true;
              } else {
                delete sourcesByTab[tab][source];
                if (!Object.keys(sourcesByTab[tab]).length) delete sourcesByTab[tab];
              }
              applyVisual(tab);
              if (!on && source !== 'refresh' && hadOther && !tabHasOtherBusy(tab)) {
                releaseFastRefresh(tab);
              }
            }
            function clearSource(source) {
              Object.keys(sourcesByTab).forEach(function (tab) {
                if (sourcesByTab[tab] && sourcesByTab[tab][source]) {
                  var hadOther = tabHasOtherBusy(tab);
                  delete sourcesByTab[tab][source];
                  if (!Object.keys(sourcesByTab[tab]).length) delete sourcesByTab[tab];
                  applyVisual(tab);
                  if (source !== 'refresh' && hadOther && !tabHasOtherBusy(tab)) {
                    releaseFastRefresh(tab);
                  }
                }
              });
            }
            function hasShinyProgress() {
              return !!document.querySelector('.shiny-progress, .shiny-progress-container .progress');
            }
            function onShinyBusy() {
              /* Show 「…」 as soon as the loading screen / busy state starts — no arm delay. */
              if (shinyArmTimer) {
                clearTimeout(shinyArmTimer);
                shinyArmTimer = null;
              }
              var tab = currentTab();
              if (!tab) return;
              shinyTab = tab;
              setTabBusy(tab, true, 'shiny');
            }
            function onShinyIdle() {
              if (shinyArmTimer) {
                clearTimeout(shinyArmTimer);
                shinyArmTimer = null;
              }
              if (shinyTab) {
                setTabBusy(shinyTab, false, 'shiny');
                shinyTab = null;
              }
              var idleTab = currentTab();
              var hadOther = tabHasOtherBusy(idleTab);
              clearSource('shiny');
              clearSource('nav');
              clearSource('progress');
              var markIdle = document.getElementById('ynow_header_logo_home');
              var fastIdle = !!(markIdle && markIdle.classList &&
                markIdle.classList.contains('ynow-logo-spin-fast'));
              /* Fast spin outlives an early shiny:idle that has not seen
                 feature work yet. It stops once that work drops off. */
              if (!fastIdle) clearSource('refresh');
              else if (hadOther && !tabHasOtherBusy(idleTab)) releaseFastRefresh(idleTab);
              syncHeaderLogoSpin();
            }
            function onSidebarTabClick(ev) {
              var t = ev.target;
              if (!t || !t.closest) return;
              var a = t.closest('.sidebar-menu a[data-value]');
              if (!a) return;
              var tab = remapTab(a.getAttribute('data-value') || '');
              if (!tab) return;
              /* Immediate feedback on the tab that is about to load. */
              setTabBusy(tab, true, 'nav');
            }
            function watchProgressDom() {
              if (typeof MutationObserver === 'undefined') return;
              var root = document.body || document.documentElement;
              if (!root || root.nodeType !== 1) return;
              var armed = false;
              var obs = new MutationObserver(function () {
                if (hasShinyProgress()) {
                  if (armed) return;
                  armed = true;
                  var tab = currentTab();
                  if (tab) setTabBusy(tab, true, 'progress');
                } else if (armed) {
                  armed = false;
                  clearSource('progress');
                }
              });
              try {
                obs.observe(root, { childList: true, subtree: true });
              } catch (eWatch) { /* body may be unavailable mid-parse */ }
            }
            function register() {
              if (!window.Shiny || !Shiny.addCustomMessageHandler) {
                setTimeout(register, 50);
                return;
              }
              Shiny.addCustomMessageHandler('ynowTabBusy', function (payload) {
                if (!payload) return;
                var tab = payload.tab;
                var busy = payload.busy === true || payload.busy === 'true' || payload.busy === 1;
                var n = payload.n != null && payload.n !== '' ? Number(payload.n) : NaN;
                var focusN = window.__ynowLogoFocusN || 0;
                if (!busy && focusN && isFinite(n) && n < focusN) return;
                setTabBusy(tab, busy, 'server');
              });
              Shiny.addCustomMessageHandler('ynowBtnBusy', function (payload) {
                if (!payload || !payload.id) return;
                var id = String(payload.id);
                var busy = payload.busy === true || payload.busy === 'true' || payload.busy === 1;
                var label = payload.label != null ? String(payload.label) : '';
                window.__ynowBtnBusy = window.__ynowBtnBusy || {};
                window.__ynowBtnBusy[id] = busy;
                var el = document.getElementById(id);
                if (!el) return;
                var span = null;
                if (payload.label_id) {
                  span = document.getElementById(String(payload.label_id));
                }
                if (!span) {
                  span = document.getElementById('ynow_' + id + '_label');
                }
                if (span) {
                  span.textContent = label;
                } else {
                  var icon = el.querySelector('i');
                  var iconHtml = icon ? icon.outerHTML + ' ' : '';
                  el.innerHTML = iconHtml + label;
                }
                el.classList.toggle('ynow-btn-is-busy', busy);
                try {
                  el.setAttribute('aria-busy', busy ? 'true' : 'false');
                } catch (eBusy) {}
              });
              if (window.jQuery) {
                jQuery(document).on('shiny:busy', onShinyBusy);
                jQuery(document).on('shiny:idle', onShinyIdle);
                jQuery(document).on('click', '.sidebar-menu a[data-value]', onSidebarTabClick);
                watchProgressDom();
              } else {
                setTimeout(register, 50);
              }
            }
            function registerLogoSpinHandler() {
              if (!window.Shiny || !Shiny.addCustomMessageHandler) {
                setTimeout(registerLogoSpinHandler, 50);
                return;
              }
              Shiny.addCustomMessageHandler('ynowHeaderLogoSpin', function (payload) {
                var mark = document.getElementById('ynow_header_logo_home');
                if (!mark || !mark.classList) return;
                var on = !!(payload && (payload.spin === true || payload.spin === 'true' || payload.spin === 1));
                var n = payload && payload.n != null && payload.n !== '' ? Number(payload.n) : 0;
                if (!isFinite(n)) n = 0;
                var focusN = window.__ynowLogoFocusN || 0;
                if (!on && focusN && n < focusN) return;
                var tab = payload && payload.tab ? remapTab(payload.tab) : currentTab();
                if (!on && mark.classList.contains('ynow-logo-spin-fast') && tabHasOtherBusy(tab)) return;
                if (on && tab) {
                  setTabBusy(tab, true, 'refresh');
                } else if (tab) {
                  setTabBusy(tab, false, 'refresh');
                } else {
                  mark.classList.toggle('ynow-logo-spinning', on);
                  try { mark.setAttribute('aria-busy', on ? 'true' : 'false'); } catch (eHs) {}
                }
                if (on && payload && payload.rate === 'fast') {
                  mark.classList.add('ynow-logo-spin-fast');
                }
              });
            }
            register();
            registerLogoSpinHandler();
            window.ynowSetSidebarTabBusy = setTabBusy;
            window.ynowSyncHeaderLogoSpin = syncHeaderLogoSpin;
            window.ynowClearOtherTabBusy = clearOtherTabBusy;
            window.ynowFocusCurrentTabBusy = focusCurrentTabBusy;
            /* After applyUiLocale rewrites menu HTML, restore busy-dot visuals. */
            window.ynowReapplyAllTabBusyVisuals = function () {
              Object.keys(sourcesByTab).forEach(function (tab) { applyVisual(tab); });
              syncHeaderLogoSpin();
            };
          })();

          /* Deploy refresh: poll ynow_build.json; never auto-reload / never force logout.
             Patch (+0.01): soft flame on title; click The YNow App to refresh.
             Major (vN → vN+1): same flame + must-click reminder for every signed-in session. */
          (function initYnowDeployRefresh() {
            var POLL_MS = 45000;
            var sessionBuild = null;
            var remoteBuild = null;
            var pollTimer = null;
            var LAST_UPDATE_STRINGS = null;
            var authObs = null;

            function titleEl() {
              return document.getElementById('ynow_app_title') ||
                document.querySelector('.main-header .logo .ynow-app-title');
            }
            function logoEl() {
              return document.querySelector('.main-header .logo');
            }
            function isSignedIn() {
              return !!(document.body && document.body.classList.contains('ynow-authed'));
            }
            /* Major = integer before decimal (v27.12 → 25), same as YNOW_VERSION_BUMP_MAJOR cut. */
            function displayMajor(version) {
              /* Use [0-9] not \\d — this block lives in an R character string. */
              var m = String(version || '').trim().match(/^[vV]?([0-9]+)/);
              if (!m) return NaN;
              return parseInt(m[1], 10);
            }
            function isMajorBump(sessionVer, remoteVer) {
              var a = displayMajor(sessionVer);
              var b = displayMajor(remoteVer);
              return isFinite(a) && isFinite(b) && b > a;
            }
            window.ynowDisplayMajor = displayMajor;
            window.ynowIsMajorDisplayBump = isMajorBump;
            function readSessionBuild() {
              if (sessionBuild) return sessionBuild;
              var el = titleEl();
              var fromDom = el && el.getAttribute('data-ynow-build');
              if (fromDom) {
                sessionBuild = String(fromDom).trim();
                return sessionBuild;
              }
              sessionBuild = '';
              return sessionBuild;
            }
            function updateTipText(major) {
              var s = LAST_UPDATE_STRINGS || {};
              if (major) {
                return s.update_major_click ||
                  'Major update — click The YNow App to refresh';
              }
              return s.update_available_click ||
                'New version available — click The YNow App to refresh';
            }
            function ensureMajorReminderEl() {
              var el = document.getElementById('ynow_major_update_reminder');
              if (el) return el;
              el = document.createElement('div');
              el.id = 'ynow_major_update_reminder';
              el.className = 'ynow-major-update-reminder';
              el.setAttribute('role', 'alertdialog');
              el.setAttribute('aria-modal', 'true');
              el.setAttribute('aria-labelledby', 'ynow_major_update_title');
              el.setAttribute('aria-describedby', 'ynow_major_update_body');
              /* Build with DOM APIs — avoid quote conflicts inside the R HTML string. */
              var card = document.createElement('div');
              card.className = 'ynow-major-update-card';
              var titleP = document.createElement('p');
              titleP.id = 'ynow_major_update_title';
              titleP.className = 'ynow-major-update-title';
              var bodyP = document.createElement('p');
              bodyP.id = 'ynow_major_update_body';
              bodyP.className = 'ynow-major-update-body';
              var hintP = document.createElement('p');
              hintP.className = 'ynow-major-update-hint';
              hintP.setAttribute('data-ynow-major-hint', '1');
              card.appendChild(titleP);
              card.appendChild(bodyP);
              card.appendChild(hintP);
              el.appendChild(card);
              /* Reminder is informational only — refresh stays on flame title click. */
              el.addEventListener('click', function (ev) {
                var hitCard = ev.target && ev.target.closest
                  ? ev.target.closest('.ynow-major-update-card')
                  : null;
                if (!hitCard) {
                  ev.preventDefault();
                  ev.stopPropagation();
                  var t = titleEl();
                  if (t && typeof t.focus === 'function') {
                    try { t.focus(); } catch (eFocus) {}
                  }
                  return;
                }
                /* Card click: highlight flame title; do not reload here. */
                ev.preventDefault();
                ev.stopPropagation();
                var title = titleEl();
                if (title && typeof title.focus === 'function') {
                  try { title.focus(); } catch (eFocus2) {}
                }
              }, true);
              (document.body || document.documentElement).appendChild(el);
              return el;
            }
            function fillMajorReminderStrings() {
              var s = LAST_UPDATE_STRINGS || {};
              var root = document.getElementById('ynow_major_update_reminder');
              if (!root) return;
              var title = root.querySelector('#ynow_major_update_title');
              var body = root.querySelector('#ynow_major_update_body');
              var hint = root.querySelector('[data-ynow-major-hint]');
              if (title) {
                title.textContent = s.update_major_title || 'Major update available';
              }
              if (body) {
                body.textContent = s.update_major_body ||
                  ('A new major version is live. Click the flaming The YNow App title to refresh. ' +
                   'Your session stays signed in — we never auto-reload or force sign-out.');
              }
              if (hint) {
                hint.textContent = s.update_major_hint ||
                  'Click the flaming The YNow App title to refresh';
              }
            }
            function applyUpdateChrome() {
              var pending = !!(remoteBuild && sessionBuild && remoteBuild !== sessionBuild);
              var major = pending && isMajorBump(sessionBuild, remoteBuild);
              document.body.classList.toggle('ynow-update-available', pending);
              document.body.classList.toggle('ynow-update-major', !!major);
              if (major && isSignedIn()) {
                ensureMajorReminderEl();
                fillMajorReminderStrings();
              }
              var tip = updateTipText(!!major);
              var title = titleEl();
              var logo = logoEl();
              if (title) {
                if (pending) {
                  title.setAttribute('title', tip);
                  title.setAttribute('aria-label', tip);
                  title.setAttribute('role', 'button');
                  title.setAttribute('tabindex', '0');
                } else {
                  title.removeAttribute('title');
                  if (title.getAttribute('role') === 'button') {
                    title.setAttribute('role', 'progressbar');
                  }
                  title.removeAttribute('tabindex');
                }
              }
              if (logo) {
                if (pending) logo.setAttribute('title', tip);
                else logo.removeAttribute('title');
              }
            }
            window.ynowSetUpdateLocaleStrings = function (strings) {
              LAST_UPDATE_STRINGS = strings || null;
              applyUpdateChrome();
            };
            function markRemote(version) {
              var v = String(version || '').trim();
              if (!v) return;
              remoteBuild = v;
              applyUpdateChrome();
            }
            function fetchBuild() {
              var url = 'ynow_build.json?_=' + Date.now();
              return fetch(url, { cache: 'no-store', credentials: 'same-origin' })
                .then(function (res) {
                  if (!res || !res.ok) throw new Error('build fetch failed');
                  return res.json();
                })
                .then(function (data) {
                  if (data && data.version) markRemote(data.version);
                })
                .catch(function () { /* keep last known; no auto-reload */ });
            }
            function reloadForUpdate() {
              /* Hard navigation so the browser loads the new Shiny bundle / assets.
                 Intentional flame-title click only — never called from poll / auth. */
              try {
                var u = new URL(window.location.href);
                u.searchParams.set('_ynow_refresh', String(Date.now()));
                window.location.replace(u.toString());
              } catch (eReload) {
                window.location.reload();
              }
            }
            function onTitleActivate(ev) {
              if (!document.body.classList.contains('ynow-update-available')) return;
              var hit = ev.target && ev.target.closest
                ? ev.target.closest('#ynow_app_title, .main-header .logo')
                : null;
              if (!hit) return;
              /* Do not steal clicks from market/lang chrome inside the navbar. */
              if (ev.target.closest && ev.target.closest(
                '.ynow-market-header, .ynow-lang-header, .navbar-custom-menu, .sidebar-toggle'
              )) return;
              /* Reminder overlay must not intercept title clicks (title is above overlay). */
              ev.preventDefault();
              ev.stopPropagation();
              reloadForUpdate();
            }
            function bindClick() {
              if (document.documentElement.getAttribute('data-ynow-deploy-refresh') === '1') return;
              document.documentElement.setAttribute('data-ynow-deploy-refresh', '1');
              document.addEventListener('click', onTitleActivate, true);
              document.addEventListener('keydown', function (ev) {
                if (!document.body.classList.contains('ynow-update-available')) return;
                var t = ev.target;
                if (!t || t.id !== 'ynow_app_title') return;
                if (ev.key !== 'Enter' && ev.key !== ' ') return;
                ev.preventDefault();
                reloadForUpdate();
              }, true);
            }
            function bindAuthObserver() {
              if (authObs || !document.body || typeof MutationObserver === 'undefined') return;
              authObs = new MutationObserver(function () {
                /* Signed-in mid-session after a major deploy still needs the reminder. */
                applyUpdateChrome();
              });
              authObs.observe(document.body, {
                attributes: true,
                attributeFilter: ['class']
              });
            }
            function startPolling() {
              readSessionBuild();
              bindAuthObserver();
              fetchBuild();
              setTimeout(fetchBuild, 3000);
              setTimeout(fetchBuild, 10000);
              if (pollTimer) clearInterval(pollTimer);
              pollTimer = setInterval(fetchBuild, POLL_MS);
            }
            bindClick();
            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', startPolling);
            } else {
              startPolling();
            }
            document.addEventListener('visibilitychange', function () {
              if (!document.hidden) fetchBuild();
            });
            window.addEventListener('focus', function () { fetchBuild(); });
            function bindShinyHooks() {
              if (!window.jQuery) {
                setTimeout(bindShinyHooks, 50);
                return;
              }
              /* After reconnect (e.g. post-deploy), re-check build — still no auto-reload. */
              jQuery(document).on('shiny:connected shiny:reconnected', function () {
                fetchBuild();
              });
            }
            bindShinyHooks();
          })();

          function bindMarketModeButtons() {
            placeMarketHeaderByToggle();
            var stack = document.querySelector('#ynow-market-header .ynow-market-stack');
            if (!stack) return;
            if (stack.getAttribute('data-ynow-bound') === '1') return;
            stack.setAttribute('data-ynow-bound', '1');
            stack.addEventListener('click', function (ev) {
              var btn = ev.target && ev.target.closest ? ev.target.closest('.ynow-mkt-btn') : null;
              if (!btn || !stack.contains(btn)) return;
              var val = btn.getAttribute('data-value');
              if (!val) return;
              stack.querySelectorAll('.ynow-mkt-btn').forEach(function (b) {
                b.classList.toggle('active', b === btn);
              });
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('market_mode_pick', val, {priority: 'event'});
                // Force Clustering Lab clear even if mode was already applied client-side
                Shiny.setInputValue('lab_cluster_clear_tick', Date.now(), {priority: 'event'});
              }
            });
            function pushInitial() {
              if (!(window.Shiny && Shiny.setInputValue)) {
                setTimeout(pushInitial, 50);
                return;
              }
              var active = stack.querySelector('.ynow-mkt-btn.active');
              var val = active ? active.getAttribute('data-value') : 'US';
              Shiny.setInputValue('market_mode_pick', val || 'US', {priority: 'event'});
            }
            pushInitial();
          }
          /* Language: stable custom buttons (no shinyWidgets re-render / mobile drift).
             Do not push an initial value — server + device_ui_locale own first paint;
             a premature EN push can race and lock out device zh-TW. */
          function bindLangModeButtons() {
            var stack = document.getElementById('ynow_lang_stack') ||
              document.querySelector('.ynow-lang-header .ynow-lang-stack');
            if (!stack) return;
            if (stack.getAttribute('data-ynow-bound') === '1') return;
            stack.setAttribute('data-ynow-bound', '1');
            stack.addEventListener('click', function (ev) {
              var btn = ev.target && ev.target.closest ? ev.target.closest('.ynow-lang-btn') : null;
              if (!btn || !stack.contains(btn)) return;
              var val = btn.getAttribute('data-value');
              if (!val) return;
              stack.querySelectorAll('.ynow-lang-btn').forEach(function (b) {
                b.classList.toggle('active', b === btn);
              });
              /* Sticky pending pick blocks mismatched (stale) websocket applies.
                 Cancel prior verify timers so an EN click's delayed check cannot
                 run after a later 繁中 click. */
              if (window.__ynowLocaleVerifyTimer) {
                clearTimeout(window.__ynowLocaleVerifyTimer);
                window.__ynowLocaleVerifyTimer = null;
              }
              window.__ynowLocaleUserPick = val;
              window.__ynowLocaleUserPickSeq = (window.__ynowLocaleUserPickSeq || 0) + 1;
              var pickSeq = window.__ynowLocaleUserPickSeq;
              try {
                document.documentElement.setAttribute('lang', val === 'zh-TW' ? 'zh-TW' : 'en');
              } catch (eLangOpt) {}
              /* Click-time boot apply: real menu strings (首頁／關於) without waiting
                 on ynowUiLocale. Server push still refreshes the full string map. */
              try {
                var bootRoot = window.__YNOW_UI_LOCALE_BOOT || {};
                var boot = bootRoot[val] || (val === 'zh-TW' ? bootRoot['zh-TW'] : bootRoot.en);
                if (boot && typeof applyUiLocale === 'function') {
                  var mkt = document.body.classList.contains('ynow-market-tw') ? 'TW' : 'US';
                  applyUiLocale({
                    locale: boot.locale || val,
                    market: mkt,
                    strings: boot.strings || {},
                    tabs: boot.tabs || {},
                    boxes: boot.boxes || []
                  }, {fromBoot: true});
                }
              } catch (eBoot) {}
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('ui_locale_pick', val, {priority: 'event'});
                var expect = val;
                window.__ynowLocaleVerifyTimer = setTimeout(function () {
                  window.__ynowLocaleVerifyTimer = null;
                  if (pickSeq !== window.__ynowLocaleUserPickSeq) return;
                  var home = document.querySelector('.sidebar-menu a[data-value=\"home\"]');
                  var about = document.querySelector('.sidebar-menu a[data-value=\"about\"]');
                  var sp = home ? home.querySelector('.ynow-menu-label, span:not(.pull-right-container):not(.ynow-rec-slot)') : null;
                  var spA = about ? about.querySelector('.ynow-menu-label, span:not(.pull-right-container):not(.ynow-rec-slot)') : null;
                  var homeTxt = ((sp && sp.textContent) || (home && home.textContent) || '').replace(/\\s+/g, ' ').trim();
                  var aboutTxt = ((spA && spA.textContent) || (about && about.textContent) || '').replace(/\\s+/g, ' ').trim();
                  var needZh = expect === 'zh-TW';
                  var stringsOk = needZh
                    ? (/首頁/.test(homeTxt) && /關於/.test(aboutTxt))
                    : (/Home/i.test(homeTxt) && /About/i.test(aboutTxt));
                  if (stringsOk || !(window.Shiny && Shiny.setInputValue)) return;
                  Shiny.setInputValue('ui_locale_pick', expect, {priority: 'event'});
                  try {
                    var bootRoot2 = window.__YNOW_UI_LOCALE_BOOT || {};
                    var boot2 = bootRoot2[expect] || (expect === 'zh-TW' ? bootRoot2['zh-TW'] : bootRoot2.en);
                    if (boot2 && typeof applyUiLocale === 'function') {
                      var mkt2 = document.body.classList.contains('ynow-market-tw') ? 'TW' : 'US';
                      applyUiLocale({
                        locale: boot2.locale || expect,
                        market: mkt2,
                        strings: boot2.strings || {},
                        tabs: boot2.tabs || {},
                        boxes: boot2.boxes || []
                      }, {fromBoot: true});
                    }
                  } catch (eBoot2) {}
                }, 1200);
              }
            });
          }
          function armMarketHeaderPlacement() {
            try { placeMarketHeaderByToggle(); } catch (ePlace0) {}
            try { bindMarketModeButtons(); } catch (eBindM) {}
            try { bindLangModeButtons(); } catch (eBindL) {}
            if (typeof MutationObserver !== 'undefined') {
              try {
                var hdrNav = document.querySelector('.main-header .navbar');
                if (hdrNav && hdrNav.nodeType === 1 && !hdrNav.getAttribute('data-ynow-mkt-obs')) {
                  hdrNav.setAttribute('data-ynow-mkt-obs', '1');
                  var placeTimer = null;
                  var mktObs = new MutationObserver(function () {
                    if (placeTimer) clearTimeout(placeTimer);
                    placeTimer = setTimeout(function () {
                      try { placeMarketHeaderByToggle(); } catch (ePlace1) {}
                    }, 0);
                  });
                  /* Navbar child moves + custom-menu rebuilds that steal the market li. */
                  mktObs.observe(hdrNav, { childList: true, subtree: false });
                  var custom = document.querySelector('.main-header .navbar-custom-menu');
                  if (custom && custom.nodeType === 1) {
                    mktObs.observe(custom, { childList: true, subtree: true });
                  }
                }
              } catch (eObs) { /* never block placement timers */ }
            }
          }
          /* Register timers BEFORE arm so a MutationObserver throw cannot skip them. */
          [0, 50, 150, 250, 500, 1000, 2000, 4000].forEach(function (ms) {
            setTimeout(function () {
              try { placeMarketHeaderByToggle(); } catch (eT) {}
            }, ms);
          });
          /* Mobile: keep nudging until flags sit after the hamburger (AdminLTE
             sometimes rebuilds .navbar-nav and steals the market li back). */
          (function keepMarketBesideToggleOnMobile() {
            var tries = 0;
            var iv = setInterval(function () {
              tries += 1;
              try {
                if (typeof window.ynowPlaceMarketHeaderByToggle === 'function') {
                  window.ynowPlaceMarketHeaderByToggle();
                } else {
                  placeMarketHeaderByToggle();
                }
              } catch (eIv) {}
              var market = document.getElementById('ynow-market-header');
              var toggle = document.querySelector('.main-header .navbar > .sidebar-toggle') ||
                document.querySelector('.main-header .navbar .sidebar-toggle');
              var ok = !!(market && toggle && market.parentNode === toggle.parentNode &&
                market.previousElementSibling === toggle);
              if (ok) {
                document.documentElement.setAttribute('data-ynow-mkt-beside-toggle', '1');
                clearInterval(iv);
              } else if (tries >= 60) {
                clearInterval(iv);
              }
            }, 200);
          })();
          if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', armMarketHeaderPlacement);
          } else {
            armMarketHeaderPlacement();
          }
          window.addEventListener('resize', function () {
            try { placeMarketHeaderByToggle(); } catch (eR) {}
            try { placeMobileAppTitle(); } catch (eR2) {}
          });
          if (window.jQuery) {
            jQuery(document).on(
              'shiny:connected shiny:sessioninitialized shiny:reconnected',
              function () {
                try { placeMarketHeaderByToggle(); } catch (eS) {}
              }
            );
          }
          document.addEventListener('click', function (ev) {
            var t = ev.target;
            if (t && t.closest && t.closest('.sidebar-menu .treeview > a')) {
              setTimeout(function () {
                if (LAST_BADGE_MAP) applySidebarBadges(LAST_BADGE_MAP);
              }, 0);
              setTimeout(function () {
                if (LAST_BADGE_MAP) applySidebarBadges(LAST_BADGE_MAP);
              }, 200);
            }
          });

          /* ---- UI locale (en / zh-TW) in-place chrome labels ---- */
          function setMenuLabel(tab, label) {
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]');
            if (!a || !label) return;
            var icon = a.querySelector('i.fa:not(.pull-right), i.fas:not(.pull-right), i.far:not(.pull-right), i.glyphicon:not(.pull-right), i.ion:not(.pull-right)');
            var prc = a.querySelector('.pull-right-container');
            var angle = a.querySelector('i.fa.pull-right, i.fas.pull-right, i.far.pull-right, i.glyphicon.pull-right, i.fa-angle-left, i.fas.fa-angle-left');
            var slots = a.querySelectorAll('.ynow-rec-slot');
            var badges = a.querySelectorAll('small.ynow-sidebar-badge, small.ynow-rec-badge');
            var busyDots = a.querySelector('.ynow-tab-busy-dots');
            var wasBusy = a.classList.contains('ynow-tab-is-busy');
            var iconClone = icon ? icon.cloneNode(true) : null;
            var prcClone = prc ? prc.cloneNode(true) : null;
            var angleClone = (!prc && angle) ? angle.cloneNode(true) : null;
            var busyDotsClone = busyDots ? busyDots.cloneNode(true) : null;
            var slotClones = [];
            if (slots.length) {
              slots.forEach(function (s) { slotClones.push(s.cloneNode(true)); });
            } else {
              badges.forEach(function (b) {
                var slot = document.createElement('span');
                slot.className = 'ynow-rec-slot';
                slot.appendChild(b.cloneNode(true));
                slotClones.push(slot);
              });
            }
            a.innerHTML = '';
            if (iconClone) a.appendChild(iconClone);
            /* Full guest locks — same tab ids as ynow_sidebar_guest_lock_tabs() / Lite hide. */
            var loginTabs = window.__YNOW_SIDEBAR_GUEST_LOCK_TABS || {
              lab_notes: true,
              decision_checklist: true,
              mos_assumptions: true,
              price_alerts: true,
              asset_transmission: true
            };
            if (loginTabs[tab]) {
              var lock = document.createElement('i');
              lock.className = 'fas fa-lock ynow-menu-lock';
              lock.setAttribute('aria-hidden', 'true');
              a.appendChild(lock);
            }
            var lab = document.createElement('span');
            lab.className = 'ynow-menu-label';
            lab.textContent = ' ' + label + ' ';
            a.appendChild(lab);
            if (busyDotsClone) a.appendChild(busyDotsClone);
            slotClones.forEach(function (sc) { a.appendChild(sc); });
            if (prcClone) a.appendChild(prcClone);
            else if (angleClone) a.appendChild(angleClone);
            if (wasBusy) {
              a.classList.add('ynow-tab-is-busy');
              try { a.setAttribute('aria-busy', 'true'); } catch (eBusy) {}
              var liBusy = a.closest ? a.closest('li') : a.parentElement;
              if (liBusy && liBusy.classList) liBusy.classList.add('ynow-tab-is-busy');
            }
          }

          function setNavLinkLabel(a, label) {
            if (!a || !label) return;
            /* 勿覆寫含 Shiny 動態輸出的頁籤（如 FCFF／FCFE） */
            if (a.querySelector('.shiny-text-output, .shiny-html-output, .shiny-bound-output')) return;
            var icon = a.querySelector('i');
            var iconClone = icon ? icon.cloneNode(true) : null;
            a.innerHTML = '';
            if (iconClone) {
              a.appendChild(iconClone);
              a.appendChild(document.createTextNode(' '));
            }
            a.appendChild(document.createTextNode(label));
          }

          function applyTabLabels(tabMap) {
            if (!tabMap) return;
            document.querySelectorAll('.nav-tabs a[data-value], .navbar-nav a[data-value]').forEach(function (a) {
              var dv = a.getAttribute('data-value');
              if (dv && tabMap[dv]) setNavLinkLabel(a, tabMap[dv]);
            });
          }

          function applyBoxHeaders(specs) {
            if (!specs || !specs.length) return;
            document.querySelectorAll('.nav-tabs-custom > .nav-tabs > li.header').forEach(function (li) {
              var text = (li.textContent || '').replace(/\\s+/g, ' ').trim();
              if (!text) return;
              for (var i = 0; i < specs.length; i++) {
                var sp = specs[i] || {};
                var aliases = sp.match || [];
                var hit = false;
                for (var j = 0; j < aliases.length; j++) {
                  var al = String(aliases[j] || '').replace(/\\s+/g, ' ').trim();
                  /* Exact match only — substring hits remapped unrelated titles (e.g. Snapshot). */
                  if (al && text === al) { hit = true; break; }
                }
                if (!hit || !sp.label) continue;
                var icon = li.querySelector('i');
                var iconClone = icon ? icon.cloneNode(true) : null;
                li.innerHTML = '';
                if (iconClone) {
                  li.appendChild(iconClone);
                  li.appendChild(document.createTextNode(' '));
                }
                li.appendChild(document.createTextNode(sp.label));
                break;
              }
            });
          }

          function applyUiLocale(payload, opts) {
            /* opts.fromBoot: click-time slim boot maps; ignored for control flow
               because __ynowLocaleUserPick stays sticky after a manual pick. */
            opts = opts || {};
            var s = (payload && payload.strings) || {};
            var incoming = payload && payload.locale ? String(payload.locale) : 'en';
            var pendingPick = window.__ynowLocaleUserPick ? String(window.__ynowLocaleUserPick) : '';
            /* Drop stale websocket locale pushes that disagree with the sticky
               manual pick (e.g. late queued `en` after an early 繁中 click). */
            if (pendingPick && incoming !== pendingPick) {
              return;
            }
            /* First-paint zh guard: drop early server `en` until matching zh arrives
               (Accept-Language / device_ui_locale), so boot 繁中 is not overwritten.
               Boot applies themselves do not clear the guard. */
            var bootGuard = window.__ynowLocaleBootGuard
              ? String(window.__ynowLocaleBootGuard)
              : '';
            if (!pendingPick && bootGuard && incoming !== bootGuard) {
              return;
            }
            if (bootGuard && incoming === bootGuard && !opts.fromBoot) {
              window.__ynowLocaleBootGuard = null;
            }
            try {
              if (payload && payload.strings) {
                /* Boot slim maps must merge into any prior full push; replacing
                   wiped keys like sidebar_reorder_* and stuck the button in English. */
                if (opts.fromBoot && window.__ynowUiLocaleStrings &&
                    typeof window.__ynowUiLocaleStrings === 'object') {
                  var mergedBoot = {};
                  var prevS = window.__ynowUiLocaleStrings;
                  Object.keys(prevS).forEach(function (k) { mergedBoot[k] = prevS[k]; });
                  Object.keys(payload.strings).forEach(function (k) {
                    mergedBoot[k] = payload.strings[k];
                  });
                  window.__ynowUiLocaleStrings = mergedBoot;
                  s = mergedBoot;
                } else {
                  window.__ynowUiLocaleStrings = payload.strings;
                }
              }
            } catch (eStoreLoc) {}
            if (typeof window.ynowSetUpdateLocaleStrings === 'function') {
              window.ynowSetUpdateLocaleStrings(s);
            }
            var menu = {
              home: s.menu_home,
              macro_market: s.menu_macro_market,
              asset_transmission: s.menu_asset_transmission,
              dashboard: s.menu_dashboard,
              company_advance: s.menu_company_advance,
              smart_analysis: s.menu_smart_analysis,
              get_started: s.menu_get_started,
              dcf_calculator: s.menu_dcf,
              ddm_calculator: s.menu_ddm,
              pb_calculator: s.menu_pb || s.menu_rel_book,
              rel_mult_ev: s.menu_rel_ev,
              rel_multiples_calculator: s.menu_rel_earn || s.menu_rel_multiples,
              rel_mult_rev: s.menu_rel_rev,
              sotp_calculator: s.menu_sotp,
              ri_calculator: s.menu_ri,
              nav_calculator: s.menu_nav,
              sensitivity: s.menu_ynow,
              lab_notes: s.menu_backtest,
              bluechip: s.menu_bluechip,
              hfv: s.menu_hfv,
              decision_checklist: s.menu_decision_checklist,
              mos_assumptions: s.menu_mos_assumptions,
              price_alerts: s.menu_price_alert,
              about: s.menu_about,
              settings: s.menu_settings
            };
            var menuSettings = document.getElementById('ynow_menu_settings');
            if (menuSettings && s.menu_settings) menuSettings.textContent = s.menu_settings;
            Object.keys(menu).forEach(function (k) {
              if (menu[k]) setMenuLabel(k, menu[k]);
            });
            if (typeof window.ynowReapplyAllTabBusyVisuals === 'function') {
              try { window.ynowReapplyAllTabBusyVisuals(); } catch (eBusyVis) {}
            }
            if (typeof window.ynowApplyAccessLocks === 'function') {
              try { window.ynowApplyAccessLocks(); } catch (eAccessLock) {}
            }
            var menuMacro = document.getElementById('ynow_menu_macro');
            if (menuMacro && s.menu_macro_market) menuMacro.textContent = s.menu_macro_market;
            var menuAtx = document.getElementById('ynow_menu_asset_transmission');
            if (menuAtx && s.menu_asset_transmission) menuAtx.textContent = s.menu_asset_transmission;
            var menuCompanyAdvance = document.getElementById('ynow_menu_company_advance');
            if (menuCompanyAdvance && s.menu_company_advance) {
              menuCompanyAdvance.textContent = s.menu_company_advance;
            }
            var headerLogoHome = document.getElementById('ynow_header_logo_home');
            var headerLogoTitle = (s.header_logo_refresh_title || s.header_logo_home_title || '');
            if (headerLogoHome && headerLogoTitle) {
              headerLogoHome.setAttribute('title', headerLogoTitle);
              headerLogoHome.setAttribute('aria-label', headerLogoTitle);
            }
            if (typeof window.ynowSyncSidebarReorderBtnLocale === 'function') {
              try { window.ynowSyncSidebarReorderBtnLocale(s); } catch (eReorderLoc) {}
            }
            var smartLab = document.getElementById('ynow_menu_smart');
            if (smartLab && s.menu_smart_analysis) smartLab.textContent = s.menu_smart_analysis;
            var menuBt = document.getElementById('ynow_menu_backtest');
            if (menuBt && s.menu_backtest) menuBt.textContent = s.menu_backtest;
            setBtText('ynow_bblab_page_title', 'bblab_page_title');
            setBtText('ynow_bblab_report_kicker', 'bblab_report_kicker');
            setBtText('ynow_bblab_page_sub', 'bblab_page_sub');
            setBtText('ynow_bblab_listed_only_notice', 'bblab_listed_only_notice');
            setBtText('ynow_bblab_listed_only_scope', 'bblab_listed_only_scope');
            setBtText('ynow_bblab_shared_ticker_hint', 'bblab_shared_ticker_hint');
            setBtText('ynow_bblab_company_label', 'bblab_company_label');
            setBtText('ynow_bblab_period_label', 'bblab_period_label');
            setBtText('ynow_bblab_statement_ccy_label', 'bblab_statement_ccy_label');
            setBtText('ynow_bblab_ch1_title', 'bblab_ch1_title');
            setBtText('ynow_bblab_ch1_help', 'bblab_ch1_help');
            setBtText('ynow_bblab_ch2_title', 'bblab_ch2_title');
            setBtText('ynow_bblab_ch2_help', 'bblab_ch2_help');
            setBtText('ynow_bblab_struct_title', 'bblab_struct_title');
            setBtText('ynow_bblab_struct_help', 'bblab_struct_help');
            setBtText('ynow_bblab_struct_biz_h', 'bblab_struct_biz_h');
            setBtText('ynow_bblab_struct_adj_h', 'bblab_struct_adj_h');
            setBtText('ynow_bblab_struct_conc_h', 'bblab_struct_conc_h');
            setBtText('ynow_bblab_bcg_title', 'bblab_bcg_title');
            setBtText('ynow_bblab_bcg_help', 'bblab_bcg_help');
            setBtText('ynow_bblab_ch3_title', 'bblab_ch3_title');
            setBtText('ynow_bblab_ch3_help', 'bblab_ch3_help');
            setBtText('ynow_bblab_ch3_current_label', 'bblab_ch3_current_label');
            setBtText('ynow_bblab_ch4_title', 'bblab_ch4_title');
            setBtText('ynow_bblab_ch4_help', 'bblab_ch4_help');
            setBtText('ynow_bblab_ch4_limited', 'bblab_ch4_limited');
            setBtText('ynow_bblab_ch5_title', 'bblab_ch5_title');
            setBtText('ynow_bblab_ch5_help', 'bblab_ch5_help');
            setBtText('ynow_bblab_ch6_title', 'bblab_ch6_title');
            setBtText('ynow_bblab_ch6_help', 'bblab_ch6_help');
            setBtText('ynow_bblab_recon_notes_title', 'bblab_recon_notes_title');
            setBtText('ynow_bblab_ch7_title', 'bblab_ch7_title');
            setBtText('ynow_bblab_summary_title', 'bblab_ch2_title');
            setBtText('ynow_bblab_chart_title', 'bblab_ch3_title');
            setBtText('ynow_bblab_cards_title', 'bblab_ch5_title');
            setBtText('ynow_bblab_recon_title', 'bblab_ch6_title');
            setBtText('ynow_bblab_sources_title', 'bblab_ch7_title');
            setBtText('ynow_bblab_sources_chrome', 'bblab_sources_chrome');
            setBtText('ynow_bblab_shared_title', 'bblab_shared_title');
            setBtText('ynow_bblab_expand_other', 'bblab_expand_other');
            setBtText('ynow_bblab_fallback_gm_label', 'bblab_fallback_gm_label');
            setBtText('ynow_bblab_geo_veto_why', 'bblab_geo_veto_why');
            setBtText('ynow_bblab_orders_title', 'bblab_orders_title');
            setBtText('ynow_bblab_orders_help', 'bblab_orders_help');
            setBtText('ynow_bblab_orders_qty_title', 'bblab_orders_qty_title');
            setBtText('ynow_bblab_orders_qty_help', 'bblab_orders_qty_help');
            setBtText('ynow_bblab_orders_from_filings_customers', 'bblab_orders_from_filings');
            setBtText('ynow_bblab_orders_from_filings_suppliers', 'bblab_orders_from_filings');
            setBtText('ynow_bblab_orders_disclosed_facts_customers', 'bblab_orders_disclosed_facts');
            setBtText('ynow_bblab_orders_disclosed_facts_suppliers', 'bblab_orders_disclosed_facts');
            setBtText('ynow_bblab_orders_markets_title', 'bblab_orders_markets_title');
            setBtText('ynow_bblab_orders_markets_help', 'bblab_orders_markets_help');
            setBtText('ynow_bblab_orders_customers_title', 'bblab_orders_customers_title');
            setBtText('ynow_bblab_orders_customers_help', 'bblab_orders_customers_help');
            setBtText('ynow_bblab_orders_suppliers_title', 'bblab_orders_suppliers_title');
            setBtText('ynow_bblab_orders_suppliers_help', 'bblab_orders_suppliers_help');
            setBtText('ynow_bblab_or_title', 'bblab_or_title');
            setBtText('ynow_bblab_or_help', 'bblab_or_help');
            setBtText('ynow_bblab_or_start', 'bblab_or_start');
            setBtText('ynow_bblab_overlap_why', 'bblab_overlap_why');
            setBtText('ynow_macro_page_title', 'macro_page_title');
            setBtText('ynow_macro_page_sub', 'macro_page_sub');
            setBtText('ynow_macro_rf_title', 'macro_rf_title');
            setBtText('ynow_macro_htcdi_title', 'htcdi_title');
            var htcdiBox = document.getElementById('ynow_macro_htcdi_box');
            var htcdiAlert = htcdiBox ? String(htcdiBox.getAttribute('data-htcdi-alert') || '') : '';
            if (htcdiAlert.toLowerCase() === 'unavailable') {
              setBtText('ynow_macro_htcdi_hint', 'htcdi_unavailable');
            } else {
              setBtText('ynow_macro_htcdi_hint', 'htcdi_click_hint');
            }
            setBtText('ynow_macro_htcdi_disclosure', 'htcdi_disclosure');
            setBtText('ynow_macro_htcdi_overview_title', 'htcdi_overview_title');
            setBtText('ynow_macro_htcdi_formula', 'htcdi_formula_eq');
            setBtText('ynow_macro_htcdi_formula_parts', 'htcdi_formula_parts');
            setBtText('ynow_macro_htcdi_layer_title', 'htcdi_layer_title');
            setBtText('ynow_macro_htcdi_network_title', 'htcdi_network_title');
            setBtText('ynow_macro_htcdi_contagion_paths', 'htcdi_contagion_paths');
            setBtText('ynow_macro_htcdi_network_note', 'htcdi_network_note');
            setBtText('ynow_macro_htcdi_term_bridge', 'htcdi_term_bridge');
            setBtText('ynow_macro_htcdi_map_stmt', 'htcdi_term_map_stmt');
            setBtText('ynow_macro_htcdi_map_mkt', 'htcdi_term_map_mkt');
            setBtText('ynow_macro_htcdi_map_inf', 'htcdi_term_map_inf');
            setBtText('ynow_macro_htcdi_map_traj', 'htcdi_term_map_traj');
            setBtText('ynow_macro_htcdi_in_title', 'htcdi_in_composite_title');
            setBtText('ynow_macro_htcdi_in_help', 'htcdi_in_composite_help');
            setBtText('ynow_macro_htcdi_method_title', 'htcdi_method_title');
            setBtText('ynow_macro_index_hint', 'macro_index_chart_hint');
            setBtText('ynow_macro_ynow_index_hint', 'macro_ynow_index_chart_hint');
            setBtText('ynow_own_index_overlay_label', 'macro_own_index_overlay_label');
            setBtText('ynow_own_index_overlay_hint', 'macro_own_index_overlay_hint');
            ['ynow_index_title', 'ynow_index_chapter_title', 'ynow_index_rule',
             'ynow_index_chart_note', 'ynow_index_constituents',
             'ynow_index_col_ticker', 'ynow_index_col_name', 'ynow_index_col_mcap',
             'ynow_index_col_weight', 'ynow_index_col_last', 'ynow_index_col_chg'].forEach(function(id) {
              var el = document.getElementById(id);
              if (!el) return;
              var key = el.getAttribute('data-i18n') || id;
              setBtText(id, key);
            });
            setBtText('ynow_macro_tw_signal_title', 'macro_tw_signal_title');
            setBtText('ynow_macro_theme_title', 'macro_theme_title');
            setBtText('ynow_macro_theme_help', 'macro_theme_help');
            setBtText('ynow_macro_industry_label', 'macro_industry_label');
            setBtText('ynow_macro_concept_label', 'macro_concept_label');
            setBtText('ynow_macro_concept_cloud_hint', 'macro_concept_cloud_hint');
            setBtText('ynow_macro_period_label', 'macro_period_label');
            setBtText('ynow_macro_fx_lock', 'macro_fx_lock');
            setBtText('ynow_macro_bubble_title', 'macro_bubble_title');
            setBtText('ynow_macro_bubble_sub', 'macro_bubble_sub');
            setBtText('ynow_macro_bubble_topn_label', 'macro_bubble_topn_label');
            setBtText('ynow_macro_bubble_attr_label', 'macro_bubble_attr_label');
            setBtText('ynow_macro_bubble_conc_title', 'macro_bubble_conc_title');
            setBtText('ynow_macro_bubble_top_list_title', 'macro_bubble_top_list_title');
            setBtText('ynow_macro_bubble_conc_note', 'macro_bubble_conc_note');
            setBtText('ynow_macro_bubble_attr_title', 'macro_bubble_attr_title');
            setBtText('ynow_macro_bubble_buffett_title', 'macro_bubble_buffett_title');
            setBtText('ynow_macro_bubble_buffett_note', 'macro_bubble_buffett_note');
            var macroRefresh = document.getElementById('macro-refresh');
            if (macroRefresh && s.btn_macro_refresh) {
              var ri = macroRefresh.querySelector('i');
              var runIconHtml = ri ? ri.outerHTML + ' ' : '';
              macroRefresh.innerHTML = runIconHtml + s.btn_macro_refresh;
            }
            /* Lazy Macro host may insert after this push (hasDom false). Re-apply
               once #ynow_macro_page_title appears so zh-TW chrome is not stuck English. */
            try {
              if (!document.getElementById('ynow_macro_page_title') &&
                  typeof MutationObserver !== 'undefined' &&
                  !window.__ynowMacroLocaleWatch) {
                window.__ynowMacroLocaleWatch = true;
                var macroWatchPayload = payload;
                var macroHost = document.getElementById('ynow_lazy_host_macro_market') ||
                  document.body;
                var macroMo = new MutationObserver(function () {
                  if (!document.getElementById('ynow_macro_page_title')) return;
                  macroMo.disconnect();
                  window.__ynowMacroLocaleWatch = false;
                  try { applyUiLocale(macroWatchPayload); } catch (eRe) {}
                });
                if (macroHost) {
                  macroMo.observe(macroHost, { childList: true, subtree: true });
                }
              }
            } catch (eMacroWatch) {}
            var catAsset = document.getElementById('ynow_menu_cat_asset');
            if (catAsset && s.menu_cat_asset) catAsset.textContent = s.menu_cat_asset;
            var catIncome = document.getElementById('ynow_menu_cat_income');
            if (catIncome && s.menu_cat_income) catIncome.textContent = s.menu_cat_income;
            var catRel = document.getElementById('ynow_menu_cat_relative');
            if (catRel && s.menu_cat_relative) catRel.textContent = s.menu_cat_relative;
            var catMeta = document.getElementById('ynow_menu_cat_meta');
            if (catMeta && s.menu_cat_meta) catMeta.textContent = s.menu_cat_meta;
            var menuRel = document.getElementById('ynow_menu_rel_multiples');
            if (menuRel && s.menu_rel_multiples) menuRel.textContent = s.menu_rel_multiples;
            var menuSotp = document.getElementById('ynow_menu_sotp');
            if (menuSotp && s.menu_sotp) menuSotp.textContent = s.menu_sotp;
            setBtText('ynow_rel_multiples_lead_title', 'rel_multiples_lead_title');
            setBtText('ynow_rel_multiples_lead_body', 'rel_multiples_lead_body');
            setBtText('ynow_rel_multiples_box_title', 'rel_multiples_box_title');
            setBtText('ynow_rel_multiples_tab_overview', 'rel_multiples_tab_overview');
            setBtText('ynow_rel_multiples_tab_equity', 'rel_multiples_tab_equity');
            setBtText('ynow_rel_multiples_tab_enterprise', 'rel_multiples_tab_enterprise');
            setBtText('ynow_rel_multiples_tab_bridge', 'rel_multiples_tab_bridge');
            setBtText('ynow_rel_param_matrix_title', 'rel_param_matrix_title');
            setBtText('ynow_rel_formula_equity', 'rel_formula_equity');
            setBtText('ynow_rel_multiples_btn_calc', 'rel_multiples_btn_calc');
            setBtText('ynow_rel_multiples_btn_sync', 'rel_multiples_btn_sync');
            setBtText('ynow_rel_mode_label', 'rel_mode_label');
            setBtText('ynow_rel_mode_help', 'rel_mode_help');
            setBtText('ynow_rel_formula_earnings', 'rel_formula_earnings');
            setBtText('ynow_rel_formula_enterprise', 'rel_formula_enterprise');
            setBtText('ynow_rel_formula_ps', 'rel_formula_ps');
            setBtText('ynow_rel_settings_pe_note', 'rel_settings_pe_note');
            setBtText('ynow_rel_settings_peg_note', 'rel_settings_peg_note');
            setBtText('ynow_rel_settings_ev_note', 'rel_settings_ev_note');
            setBtText('ynow_rel_settings_ps_note', 'rel_settings_ps_note');
            setBtText('ynow_rel_settings_bridge_note', 'rel_settings_bridge_note');
            setBtText('ynow_rel_multiples_bridge_heading', 'rel_multiples_bridge_heading');
            setBtText('ynow_rel_multiples_ps_rev_note', 'rel_multiples_ps_rev_note');
            setBtText('ynow_rel_multiples_pe_heading', 'rel_multiples_pe_heading');
            setBtText('ynow_rel_multiples_peg_heading', 'rel_multiples_peg_heading');
            setBtText('ynow_rel_multiples_ev_heading', 'rel_multiples_ev_heading');
            setBtText('ynow_rel_multiples_ps_heading', 'rel_multiples_ps_heading');
            setBtText('ynow_rel_multiples_pe_help', 'rel_multiples_pe_help');
            setBtText('ynow_rel_multiples_peg_help', 'rel_multiples_peg_help');
            setBtText('ynow_rel_multiples_ev_help', 'rel_multiples_ev_help');
            setBtText('ynow_rel_multiples_ps_help', 'rel_multiples_ps_help');
            setBtText('ynow_rel_multiples_arr_help', 'rel_multiples_arr_help');
            setBtText('ynow_rel_multiples_bridge_help', 'rel_multiples_bridge_help');
            setBtText('ynow_sotp_lead_title', 'sotp_lead_title');
            setBtText('ynow_sotp_lead_body', 'sotp_lead_body');
            setBtText('ynow_sotp_box_title', 'sotp_box_title');
            setBtText('ynow_sotp_tab_overview', 'sotp_tab_overview');
            setBtText('ynow_sotp_tab_segments', 'sotp_tab_segments');
            setBtText('ynow_sotp_tab_bridge', 'sotp_tab_bridge');
            setBtText('ynow_sotp_formula_banner', 'sotp_formula_banner');
            setBtText('ynow_sotp_settings_seg_note', 'sotp_settings_seg_note');
            setBtText('ynow_sotp_settings_bridge_note', 'sotp_settings_bridge_note');
            setBtText('ynow_sotp_bridge_heading', 'sotp_bridge_heading');
            setBtText('ynow_sotp_nonop_help', 'sotp_nonop_help');
            setBtText('ynow_sotp_btn_calc', 'sotp_btn_calc');
            setBtText('ynow_sotp_btn_sync', 'sotp_btn_sync');
            setBtText('ynow_sotp_btn_apply_mult', 'sotp_btn_apply_mult');
            setBtText('ynow_sotp_help', 'sotp_help');
            setBtText('ynow_sotp_bridge_help', 'sotp_bridge_help');
            var smartTitle = document.getElementById('ynow_smart_page_title');
            if (smartTitle && s.smart_page_title) smartTitle.textContent = s.smart_page_title;
            var smartSub = document.getElementById('ynow_smart_page_sub');
            if (smartSub && s.smart_page_sub) smartSub.textContent = s.smart_page_sub;
            var smartChart = document.getElementById('ynow_smart_chart_title');
            if (smartChart && s.smart_chart_title) smartChart.textContent = s.smart_chart_title;
            document.querySelectorAll('.ynow-lite-toggle').forEach(function (liteToggle) {
              if (s.lite_toggle_title) liteToggle.setAttribute('title', s.lite_toggle_title);
              if (s.lite_toggle_aria) liteToggle.setAttribute('aria-label', s.lite_toggle_aria);
            });
            if (LAST_BADGE_MAP) {
              LAST_BADGE_MAP.labels = {
                primary: s.menu_badge_primary || (LAST_BADGE_MAP.labels && LAST_BADGE_MAP.labels.primary) || '推薦',
                secondary: s.menu_badge_secondary || (LAST_BADGE_MAP.labels && LAST_BADGE_MAP.labels.secondary) || '備選',
                reference: s.menu_badge_reference || (LAST_BADGE_MAP.labels && LAST_BADGE_MAP.labels.reference) || '參照'
              };
              applySidebarBadges(LAST_BADGE_MAP);
            }
            applyTabLabels((payload && payload.tabs) || {});
            applyBoxHeaders((payload && payload.boxes) || []);
            var methodTitle = document.getElementById('ynow_method_section_title');
            if (methodTitle && s.method_section_title) methodTitle.textContent = s.method_section_title;
            var methodFvNote = document.getElementById('ynow_method_matrix_fv_note');
            if (methodFvNote && s.method_matrix_fv_note) methodFvNote.textContent = s.method_matrix_fv_note;
            var methodRelNote = document.getElementById('ynow_method_matrix_rel_note');
            if (methodRelNote && s.method_matrix_rel_note) methodRelNote.textContent = s.method_matrix_rel_note;
            var hfvTitle = document.getElementById('ynow_hfv_page_title');
            if (hfvTitle && s.hfv_page_title) hfvTitle.textContent = s.hfv_page_title;
            var hfvSub = document.getElementById('ynow_hfv_page_sub');
            if (hfvSub && s.hfv_page_sub) hfvSub.textContent = s.hfv_page_sub;
            var dcTitle = document.getElementById('ynow_dc_page_title');
            if (dcTitle && s.dc_page_title) dcTitle.textContent = s.dc_page_title;
            var dcSub = document.getElementById('ynow_dc_page_sub');
            if (dcSub && s.dc_page_sub) dcSub.textContent = s.dc_page_sub;
            var dcBoxChecks = document.getElementById('ynow_dc_box_checks');
            if (dcBoxChecks && s.dc_box_checks) dcBoxChecks.textContent = s.dc_box_checks;
            var dcBoxSum = document.getElementById('ynow_dc_box_summary');
            if (dcBoxSum && s.dc_box_summary) dcBoxSum.textContent = s.dc_box_summary;
            var dcBoxSop = document.getElementById('ynow_dc_box_sop');
            if (dcBoxSop && s.dc_box_sop) dcBoxSop.textContent = s.dc_box_sop;
            var dcSopIntro = document.getElementById('ynow_dc_sop_intro');
            if (dcSopIntro && s.dc_sop_intro) dcSopIntro.textContent = s.dc_sop_intro;
            var hfvMethod = document.getElementById('ynow_hfv_sec_method');
            if (hfvMethod && s.hfv_sec_method) hfvMethod.textContent = s.hfv_sec_method;
            var hfvResults = document.getElementById('ynow_hfv_sec_results');
            if (hfvResults && s.hfv_sec_results) hfvResults.textContent = s.hfv_sec_results;
            var hfvCh1K = document.getElementById('ynow_hfv_ch1_kicker');
            if (hfvCh1K && s.hfv_ch1_kicker) hfvCh1K.textContent = s.hfv_ch1_kicker;
            var hfvCh1T = document.getElementById('ynow_hfv_ch1_title');
            if (hfvCh1T && s.hfv_ch1_title) hfvCh1T.textContent = s.hfv_ch1_title;
            var hfvCh2K = document.getElementById('ynow_hfv_ch2_kicker');
            if (hfvCh2K && s.hfv_ch2_kicker) hfvCh2K.textContent = s.hfv_ch2_kicker;
            var hfvCh2T = document.getElementById('ynow_hfv_ch2_title');
            if (hfvCh2T && s.hfv_ch2_title) hfvCh2T.textContent = s.hfv_ch2_title;
            var hfvCh2Lead = document.getElementById('ynow_hfv_ch2_lead');
            if (hfvCh2Lead && s.hfv_ch2_lead) hfvCh2Lead.textContent = s.hfv_ch2_lead;
            var hfvCh3K = document.getElementById('ynow_hfv_ch3_kicker');
            if (hfvCh3K && s.hfv_ch3_kicker) hfvCh3K.textContent = s.hfv_ch3_kicker;
            var hfvCh3T = document.getElementById('ynow_hfv_ch3_title');
            if (hfvCh3T && s.hfv_ch3_title) hfvCh3T.textContent = s.hfv_ch3_title;
            var hfvCh3Lead = document.getElementById('ynow_hfv_ch3_lead');
            if (hfvCh3Lead && s.hfv_ch3_lead) hfvCh3Lead.textContent = s.hfv_ch3_lead;
            var hfvCh4K = document.getElementById('ynow_hfv_ch4_kicker');
            if (hfvCh4K && s.hfv_ch4_kicker) hfvCh4K.textContent = s.hfv_ch4_kicker;
            var hfvCh4T = document.getElementById('ynow_hfv_ch4_title');
            if (hfvCh4T && s.hfv_ch4_title) hfvCh4T.textContent = s.hfv_ch4_title;
            var hfvCh4Lead = document.getElementById('ynow_hfv_ch4_lead');
            if (hfvCh4Lead && s.hfv_ch4_lead) hfvCh4Lead.textContent = s.hfv_ch4_lead;
            var hfvCh5K = document.getElementById('ynow_hfv_ch5_kicker');
            if (hfvCh5K && s.hfv_ch5_kicker) hfvCh5K.textContent = s.hfv_ch5_kicker;
            var hfvCh5T = document.getElementById('ynow_hfv_ch5_title');
            if (hfvCh5T && s.hfv_ch5_title) hfvCh5T.textContent = s.hfv_ch5_title;
            var hfvCh5Lead = document.getElementById('ynow_hfv_ch5_lead');
            if (hfvCh5Lead && s.hfv_ch5_lead) hfvCh5Lead.textContent = s.hfv_ch5_lead;
            var hfvOvNote = document.getElementById('ynow_hfv_overlay_vs_replay_note');
            if (hfvOvNote && s.hfv_overlay_vs_replay_note) hfvOvNote.textContent = s.hfv_overlay_vs_replay_note;
            var hfvParamInv = document.getElementById('ynow_hfv_param_inv_title');
            if (hfvParamInv && (s.hfv_param_inv_title || s.box_hfv_param_inventory)) {
              hfvParamInv.textContent = s.hfv_param_inv_title || s.box_hfv_param_inventory;
            }
            var hfvParamHelp = document.getElementById('ynow_hfv_param_inv_help');
            if (hfvParamHelp && s.hfv_param_inv_help) hfvParamHelp.textContent = s.hfv_param_inv_help;
            var hfvChart = document.getElementById('ynow_hfv_chart_gap');
            if (hfvChart && s.hfv_chart_gap) hfvChart.textContent = s.hfv_chart_gap;
            var hfvTable = document.getElementById('ynow_hfv_table_detail');
            if (hfvTable && s.hfv_table_detail) hfvTable.textContent = s.hfv_table_detail;
            var hfvDataNote = document.getElementById('ynow_hfv_method_data_note');
            if (hfvDataNote && s.hfv_method_data_note) hfvDataNote.textContent = s.hfv_method_data_note;
            var hfvMethodBody = document.getElementById('ynow_hfv_method_body');
            if (hfvMethodBody && s.hfv_method_body) hfvMethodBody.textContent = s.hfv_method_body;
            var hfvScMatrix = document.getElementById('ynow_hfv_sum_scenario_matrix');
            if (hfvScMatrix && s.hfv_sum_scenario_matrix) hfvScMatrix.textContent = s.hfv_sum_scenario_matrix;
            var hfvScThresh = document.getElementById('ynow_hfv_scenario_thresh_note');
            if (hfvScThresh && s.hfv_scenario_thresh_note) hfvScThresh.textContent = s.hfv_scenario_thresh_note;
            var fsIsHead = document.getElementById('ynow_fs_is_chart_heading');
            if (fsIsHead && s.tab_income_statement) fsIsHead.textContent = s.tab_income_statement;
            var fsCfHead = document.getElementById('ynow_fs_cf_chart_heading');
            if (fsCfHead && s.tab_cash_flow) fsCfHead.textContent = s.tab_cash_flow;
            var hfvSessTitle = document.getElementById('ynow_hfv_session_params_title');
            if (hfvSessTitle && s.hfv_session_params_title) hfvSessTitle.textContent = s.hfv_session_params_title;
            var hfvToolbar = document.getElementById('ynow_hfv_toolbar') || document.querySelector('.ynow-hfv-toolbar');
            if (hfvToolbar && s.hfv_toolbar_aria) hfvToolbar.setAttribute('aria-label', s.hfv_toolbar_aria);
            var hfvPageCtl = document.getElementById('ynow_hfv_page_controls');
            if (hfvPageCtl && s.hfv_page_controls_aria) hfvPageCtl.setAttribute('aria-label', s.hfv_page_controls_aria);
            var hfvOverlayCtl = document.getElementById('ynow_hfv_chart_overlay_controls');
            if (hfvOverlayCtl && s.hfv_chart_overlay_aria) hfvOverlayCtl.setAttribute('aria-label', s.hfv_chart_overlay_aria);
            var chartModelsLab = document.querySelector('label[for=\"bt_fv_models\"]');
            if (chartModelsLab && s.hfv_chart_models_label) chartModelsLab.textContent = s.hfv_chart_models_label;
            var replayLab = document.querySelector('label[for=\"bt_fv_replay_model\"]');
            if (replayLab && s.hfv_replay_model_label) replayLab.textContent = s.hfv_replay_model_label;
            var oosLab = document.querySelector('label[for=\"bt_fv_oos_mode\"]');
            if (oosLab && s.hfv_oos_mode_label) oosLab.textContent = s.hfv_oos_mode_label;
            var freqLab = document.querySelector('label[for=\"bt_fv_analysis_freq\"]');
            if (freqLab && s.hfv_analysis_freq_label) freqLab.textContent = s.hfv_analysis_freq_label;
            var labTitle = document.getElementById('ynow_lab_notes_title');
            if (labTitle && s.lab_notes_title) labTitle.textContent = s.lab_notes_title;
            var labSub = document.getElementById('ynow_lab_notes_sub');
            if (labSub && s.lab_notes_sub) labSub.textContent = s.lab_notes_sub;
            var btZone = document.getElementById('ynow_bt_zone_title');
            if (btZone && s.bt_zone_title) btZone.textContent = s.bt_zone_title;
            var btToolbar = document.getElementById('ynow_bt_toolbar') || document.querySelector('.ynow-backtest-toolbar');
            if (btToolbar && s.bt_toolbar_aria) btToolbar.setAttribute('aria-label', s.bt_toolbar_aria);
            var btNavCtl = document.getElementById('ynow_bt_nav_controls');
            if (btNavCtl && s.bt_nav_controls_aria) btNavCtl.setAttribute('aria-label', s.bt_nav_controls_aria);
            var navWinLab = document.querySelector('label[for=\"bt_nav_window\"]');
            if (navWinLab && s.bt_nav_window_label) navWinLab.textContent = s.bt_nav_window_label;
            function setBtText(id, key) {
              var el = document.getElementById(id);
              if (el && s[key]) el.textContent = s[key];
            }
            setBtText('ynow_cookie_banner_body', 'cookie_banner_body');
            setBtText('ynow_cookie_settings_btn', 'cookie_settings_btn');
            setBtText('ynow_cookie_accept_all', 'cookie_accept_all');
            setBtText('ynow_cookie_reject_optional', 'cookie_reject_optional');
            setBtText('ynow_cookie_save', 'cookie_save');
            setBtText('ynow_cookie_panel_title', 'cookie_panel_title');
            setBtText('ynow_cookie_necessary_label', 'cookie_necessary_label');
            setBtText('ynow_cookie_necessary_hint', 'cookie_necessary_hint');
            setBtText('ynow_cookie_analytics_label', 'cookie_analytics_label');
            setBtText('ynow_cookie_analytics_hint', 'cookie_analytics_hint');
            setBtText('ynow_cookie_preferences_label', 'cookie_preferences_label');
            setBtText('ynow_cookie_preferences_hint', 'cookie_preferences_hint');
            setBtText('ynow_cookie_always_on', 'cookie_always_on');
            setBtText('ynow_cookie_reopen_label', 'cookie_reopen');
            setBtText('ynow_sidebar_copyright', 'sidebar_copyright');
            var cookiePin = document.getElementById('ynow_cookie_reopen');
            if (cookiePin && s.cookie_reopen) cookiePin.setAttribute('title', s.cookie_reopen);
            ['ynow_home_title', 'ynow_home_lead', 'ynow_home_method', 'ynow_home_method_lite',
             'ynow_home_market_k', 'ynow_home_market_d',
             'ynow_home_bluechip_k', 'ynow_home_bluechip_d',
             'ynow_home_company_k', 'ynow_home_company_d',
             'ynow_home_ynow_k', 'ynow_home_ynow_d',
             'ynow_home_value_k', 'ynow_home_value_d',
             'ynow_home_value_k_lite', 'ynow_home_value_d_lite',
             'ynow_home_decide_k', 'ynow_home_decide_d',
             'ynow_home_legal', 'ynow_home_legal_about'].forEach(function (id) {
              var el = document.getElementById(id);
              if (!el) return;
              var key = id.replace(/^ynow_/, '');
              setBtText(id, key);
            });
            setBtText('ynow_bt_ch1_kicker', 'bt_ch1_kicker');
            setBtText('ynow_bt_ch1_title', 'bt_ch1_title');
            setBtText('ynow_bt_ch2_kicker', 'bt_ch2_kicker');
            setBtText('ynow_bt_ch2_title', 'bt_ch2_title');
            setBtText('ynow_bt_ch2_lead', 'bt_ch2_lead');
            setBtText('ynow_bt_ch3_kicker', 'bt_ch3_kicker');
            setBtText('ynow_bt_ch3_title', 'bt_ch3_title');
            setBtText('ynow_bt_ch4_kicker', 'bt_ch4_kicker');
            setBtText('ynow_bt_ch4_title', 'bt_ch4_title');
            setBtText('ynow_bt_ch4_lead', 'bt_ch4_lead');
            setBtText('ynow_bt_exposure_title', 'bt_exposure_title');
            setBtText('ynow_bt_bh_gap_title', 'bt_bh_gap_title');
            setBtText('ynow_bt_mos_eff_title', 'bt_mos_eff_title');
            setBtText('ynow_bt_mos_eff_hint', 'bt_mos_eff_hint');
            setBtText('ynow_bt_fv_edge_title', 'bt_fv_edge_title');
            setBtText('ynow_bt_leg_fund_label', 'bt_leg_fund_label');
            setBtText('ynow_bt_leg_fund_body', 'bt_leg_fund_body');
            setBtText('ynow_bt_leg_sent_label', 'bt_leg_sent_label');
            setBtText('ynow_bt_leg_sent_body', 'bt_leg_sent_body');
            setBtText('ynow_bt_leg_bh_label', 'bt_leg_bh_label');
            setBtText('ynow_bt_leg_bh_body', 'bt_leg_bh_body');
            setBtText('ynow_bt_leg_bench_label', 'bt_leg_bench_label');
            setBtText('ynow_bt_leg_bench_body', 'bt_leg_bench_body');
            setBtText('ynow_bt_leg_hfv_note', 'bt_leg_hfv_note');
            setBtText('ynow_bt_sec_hold_gate', 'bt_sec_hold_gate');
            setBtText('ynow_bt_hold_gate_intro', 'bt_hold_gate_intro');
            setBtText('ynow_bt_kpi_filter_label', 'bt_kpi_filter_label');
            setBtText('ynow_bt_kpi_filter_hint', 'bt_kpi_filter_hint');
            setBtText('ynow_bt_sec_run_controls', 'bt_sec_run_controls');
            setBtText('ynow_bt_param_auto_hint', 'bt_param_auto_hint');
            setBtText('ynow_bt_refresh_params_hint', 'bt_refresh_params_hint');
            setBtText('ynow_bt_sec_strategy_params', 'bt_sec_strategy_params');
            setBtText('ynow_bt_params_gate_note', 'bt_params_gate_note');
            setBtText('ynow_bt_tab_fundamental', 'bt_tab_fundamental');
            setBtText('ynow_bt_tab_sentiment', 'bt_tab_sentiment');
            setBtText('ynow_bt_fund_intro', 'bt_fund_intro');
            setBtText('ynow_bt_w_vg_moved_title', 'bt_w_vg_moved_title');
            setBtText('ynow_bt_w_vg_moved_body', 'bt_w_vg_moved_body');
            setBtText('ynow_bt_mos_ladder_title', 'bt_mos_ladder_title');
            setBtText('ynow_bt_mos_ladder_body', 'bt_mos_ladder_body');
            setBtText('ynow_bt_sent_intro', 'bt_sent_intro');
            setBtText('ynow_bt_w_mom_hint', 'bt_w_mom_hint');
            setBtText('ynow_bt_w_rsi_hint', 'bt_w_rsi_hint');
            setBtText('ynow_bt_news_on', 'bt_news_on');
            setBtText('ynow_bt_news_hint', 'bt_news_hint');
            setBtText('ynow_bt_max_exp_hint', 'bt_max_exp_hint');
            setBtText('ynow_bt_min_exp_hint', 'bt_min_exp_hint');
            setBtText('ynow_bt_fit_bh_hint', 'bt_fit_bh_hint');
            setBtText('ynow_bt_sec_methodology', 'bt_sec_methodology');
            var recent = document.getElementById('ynow_recent_search_label');
            if (recent && s.recent_search) recent.textContent = s.recent_search;
            if (recent && s.recent_search_24h_hint && recent.getAttribute('data-ynow-signed-in') === '1') {
              recent.setAttribute('title', s.recent_search_24h_hint);
            }
            var scLab = document.querySelector('label[for=\"sc\"]');
            if (scLab && s.ticker_label) scLab.textContent = s.ticker_label;
            var scInput = document.getElementById('sc');
            if (scInput) {
              var mktPh = (payload && payload.market) ? String(payload.market) : 'US';
              var scPh = (mktPh === 'TW')
                ? (s.ticker_placeholder_tw || 'e.g. 2330')
                : (s.ticker_placeholder_us || 'e.g. AAPL / TSM');
              scInput.setAttribute('placeholder', scPh);
              scInput.placeholder = scPh;
            }
            var yahooIndLab = document.getElementById('ynow_industry_info_yahoo');
            if (yahooIndLab && s.industry_info_yahoo) yahooIndLab.textContent = s.industry_info_yahoo;
            var indLab = document.querySelector('label[for=\"industry_choice\"]');
            if (indLab) {
              indLab.style.display = 'none';
              indLab.textContent = '';
            }
            var sgrMethodTitle = document.getElementById('ynow_sgr_method_title');
            if (sgrMethodTitle && s.sgr_method_title) sgrMethodTitle.textContent = s.sgr_method_title;
            var sharedBridgeTitle = document.getElementById('ynow_shared_bridge_title');
            if (sharedBridgeTitle && s.shared_bridge_title) sharedBridgeTitle.textContent = s.shared_bridge_title;
            var sharedBridgeHelp = document.getElementById('ynow_shared_bridge_help');
            if (sharedBridgeHelp && s.shared_bridge_help) sharedBridgeHelp.textContent = s.shared_bridge_help;
            var sharedBridgeReset = document.getElementById('ynow_shared_bridge_reset');
            if (sharedBridgeReset && s.shared_bridge_reset) sharedBridgeReset.textContent = s.shared_bridge_reset;
            var fcfRevHelp = document.getElementById('ynow_fcf_revenue_shared_help');
            if (fcfRevHelp && s.fcf_revenue_shared_help) fcfRevHelp.textContent = s.fcf_revenue_shared_help;
            [['bridge_cash', 'shared_bridge_cash'], ['bridge_debt', 'shared_bridge_debt'],
             ['bridge_shares', 'shared_bridge_shares'], ['bridge_revenue', 'shared_bridge_revenue']].forEach(function (pair) {
              var lab = document.querySelector('label[for=\"' + pair[0] + '\"]');
              if (lab && s[pair[1]]) lab.textContent = s[pair[1]];
            });
            var lifeTitle = document.getElementById('ynow_lifecycle_stage_title');
            if (lifeTitle && s.lifecycle_stage_title) lifeTitle.textContent = s.lifecycle_stage_title;
            var lifeHelp = document.getElementById('ynow_lifecycle_stage_help');
            if (lifeHelp && s.lifecycle_stage_help) lifeHelp.textContent = s.lifecycle_stage_help;
            var sgrMethodHelp = document.getElementById('ynow_sgr_method_help');
            if (sgrMethodHelp && s.sgr_method_help) sgrMethodHelp.textContent = s.sgr_method_help;
            var sgrCustomLab = document.querySelector('label[for=\"sgr\"]');
            if (sgrCustomLab && s.sgr_custom_label) sgrCustomLab.textContent = s.sgr_custom_label;
            var indOverviewTitle = document.getElementById('ynow_ind_overview_title');
            if (indOverviewTitle && s.industry_overview_title) indOverviewTitle.textContent = s.industry_overview_title;
            var valStatusPrefix = document.getElementById('ynow_val_status_prefix');
            if (valStatusPrefix && s.composite_status_prefix) valStatusPrefix.textContent = s.composite_status_prefix;
            var g1Help = document.getElementById('ynow_g_stage1_help');
            if (g1Help && s.g_stage1_help) g1Help.textContent = s.g_stage1_help;
            var ddmModeHelp = document.getElementById('ynow_ddm_mode_help');
            if (ddmModeHelp && s.ddm_mode_help) ddmModeHelp.textContent = s.ddm_mode_help;
            var ddmD0Help = document.getElementById('ynow_ddm_d0_help');
            if (ddmD0Help && s.ddm_d0_help) ddmD0Help.textContent = s.ddm_d0_help;
            var ddmSpmTip = document.getElementById('ynow_ddm_spm_tip');
            if (ddmSpmTip && s.ddm_spm_tip) ddmSpmTip.textContent = s.ddm_spm_tip;
            var ddmGSyncHelp = document.getElementById('ynow_ddm_g_sync_help');
            if (ddmGSyncHelp && s.ddm_g_sync_help) ddmGSyncHelp.textContent = s.ddm_g_sync_help;
            var ddmSpmGNote = document.getElementById('ynow_ddm_spm_g_note');
            if (ddmSpmGNote && s.ddm_spm_g_note) ddmSpmGNote.textContent = s.ddm_spm_g_note;
            var ddmFormulaGordon = document.getElementById('ynow_ddm_formula_gordon');
            if (ddmFormulaGordon && s.ddm_formula_gordon) ddmFormulaGordon.textContent = s.ddm_formula_gordon;
            var ddmFormulaSpm = document.getElementById('ynow_ddm_formula_spm');
            if (ddmFormulaSpm && s.ddm_formula_spm) ddmFormulaSpm.textContent = s.ddm_formula_spm;
            var ddmFormulaTwo = document.getElementById('ynow_ddm_formula_two_stage');
            if (ddmFormulaTwo && s.ddm_formula_two_stage) ddmFormulaTwo.textContent = s.ddm_formula_two_stage;
            var ddmOverviewHint = document.getElementById('ynow_ddm_overview_hint');
            if (ddmOverviewHint && s.ddm_overview_hint) ddmOverviewHint.textContent = s.ddm_overview_hint;
            var ddmD0Banner = document.getElementById('ynow_ddm_d0_banner');
            if (ddmD0Banner && s.ddm_d0_banner) ddmD0Banner.textContent = s.ddm_d0_banner;
            var ddmTwoStageHelp = document.getElementById('ynow_ddm_two_stage_help');
            if (ddmTwoStageHelp && s.ddm_two_stage_help) ddmTwoStageHelp.textContent = s.ddm_two_stage_help;
            var waccBoxTitle = document.getElementById('ynow_wacc_box_title');
            if (waccBoxTitle && s.wacc_box_title) waccBoxTitle.textContent = s.wacc_box_title;
            var waccHelp = document.getElementById('ynow_wacc_help');
            if (waccHelp && s.wacc_help) waccHelp.textContent = s.wacc_help;
            var rdBoxTitle = document.getElementById('ynow_rd_box_title');
            if (rdBoxTitle && s.rd_box_title) rdBoxTitle.textContent = s.rd_box_title;
            var rdIntLab = document.querySelector('label[for=\"rd_interest_expense\"]');
            if (rdIntLab && s.rd_interest_label) rdIntLab.textContent = s.rd_interest_label;
            var rdDebtLab = document.querySelector('label[for=\"rd_interest_bearing_debt\"]');
            if (rdDebtLab && s.rd_debt_label) rdDebtLab.textContent = s.rd_debt_label;
            var rdMinLab = document.querySelector('label[for=\"wacc_rd_min\"]');
            if (rdMinLab && s.rd_min_label) rdMinLab.textContent = s.rd_min_label;
            var rdMaxLab = document.querySelector('label[for=\"wacc_rd_max\"]');
            if (rdMaxLab && s.rd_max_label) rdMaxLab.textContent = s.rd_max_label;
            var useRdLab = document.getElementById('ynow_use_estimated_rd_label');
            if (useRdLab && s.use_estimated_rd_label) useRdLab.textContent = s.use_estimated_rd_label;
            var btnCalcRd = document.getElementById('calc_rd');
            if (btnCalcRd && s.btn_calc_rd) {
              var rdIcon = btnCalcRd.querySelector('i');
              var rdIconHtml = rdIcon ? rdIcon.outerHTML + ' ' : '';
              btnCalcRd.innerHTML = rdIconHtml + s.btn_calc_rd;
            }
            var btnCalcWacc = document.getElementById('calc_wacc');
            if (btnCalcWacc && s.btn_calc_wacc) {
              var wIcon = btnCalcWacc.querySelector('i');
              var wIconHtml = wIcon ? wIcon.outerHTML + ' ' : '';
              btnCalcWacc.innerHTML = wIconHtml + s.btn_calc_wacc;
            }
            function setBtnLabel(id, label) {
              if (!id || !label) return;
              var el = document.getElementById(id);
              if (!el) return;
              var ic = el.querySelector('i');
              el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + label;
            }
            setBtnLabel('calc', s.btn_calc_dcf);
            setBtnLabel('mod_ddm-btn_calc_ddm', s.btn_calc_ddm);
            setBtnLabel('mod_ri-btn_calc_ri', s.btn_calc_ri);
            setBtnLabel('mod_pb-btn_calc_pb', s.btn_calc_pb);
            setBtnLabel('mod_nav-btn_calc_nav', s.btn_calc_nav);
            if (s.btn_reset_defaults) {
              document.querySelectorAll('.ynow-btn-reset').forEach(function (el) {
                var ic = el.querySelector('i');
                el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + s.btn_reset_defaults;
              });
            }
            var riCalcHint = document.getElementById('mod_ri-ynow_ri_calc_hint');
            if (riCalcHint && s.ri_calc_hint) riCalcHint.textContent = s.ri_calc_hint;
            var riSettingsHint = document.getElementById('mod_ri-ynow_ri_settings_recalc_hint');
            if (riSettingsHint && s.ri_settings_recalc_hint) riSettingsHint.textContent = s.ri_settings_recalc_hint;
            var pbSettingsHint = document.getElementById('mod_pb-ynow_pb_settings_reset_hint');
            if (pbSettingsHint && s.pb_settings_reset_hint) pbSettingsHint.textContent = s.pb_settings_reset_hint;
            var navSettingsHint = document.getElementById('mod_nav-ynow_nav_settings_reset_hint');
            if (navSettingsHint && s.nav_settings_reset_hint) navSettingsHint.textContent = s.nav_settings_reset_hint;
            var capmBoxTitle = document.getElementById('ynow_capm_box_title');
            if (capmBoxTitle && s.capm_box_title) capmBoxTitle.textContent = s.capm_box_title;
            var ddmCapmBoxTitle = document.getElementById('ynow_ddm_capm_box_title');
            if (ddmCapmBoxTitle && s.ddm_capm_box_title) ddmCapmBoxTitle.textContent = s.ddm_capm_box_title;
            var ddmKeBoxTitle = document.getElementById('ynow_ddm_ke_box_title');
            if (ddmKeBoxTitle && s.ddm_ke_box_title) ddmKeBoxTitle.textContent = s.ddm_ke_box_title;
            var ddmKeHelp = document.getElementById('ynow_ddm_ke_help');
            if (ddmKeHelp && s.ddm_ke_help) ddmKeHelp.textContent = s.ddm_ke_help;
            var ddmKeBridgeTitle = document.getElementById('ynow_ddm_ke_bridge_title');
            if (ddmKeBridgeTitle && s.ddm_ke_bridge_title) ddmKeBridgeTitle.textContent = s.ddm_ke_bridge_title;
            var ddmKeBridgeHelp = document.getElementById('ynow_ddm_ke_bridge_help');
            if (ddmKeBridgeHelp && s.ddm_ke_bridge_help) ddmKeBridgeHelp.textContent = s.ddm_ke_bridge_help;
            var ddmUseEstKe = document.getElementById('ynow_ddm_use_estimated_ke_label');
            if (ddmUseEstKe && s.ddm_use_estimated_ke_label) ddmUseEstKe.textContent = s.ddm_use_estimated_ke_label;
            setBtnLabel('calc_ddm_ke', s.btn_calc_ddm_ke);
            setBtnLabel('calc_ddm_capm', s.btn_calc_ddm_capm);
            var syncGsLab = document.getElementById('ynow_sync_gs_beta_label');
            if (syncGsLab && s.sync_gs_beta_label) syncGsLab.textContent = s.sync_gs_beta_label;
            var ddmSyncGsLab = document.getElementById('ynow_ddm_sync_gs_beta_label');
            if (ddmSyncGsLab && s.sync_gs_beta_label) ddmSyncGsLab.textContent = s.sync_gs_beta_label;
            document.querySelectorAll('.ynow-beta-source-heading').forEach(function (el) {
              if (s.beta_source_heading) el.textContent = s.beta_source_heading;
            });
            document.querySelectorAll('.ynow-beta-rolling-help').forEach(function (el) {
              if (s.beta_rolling_help) el.textContent = s.beta_rolling_help;
            });
            document.querySelectorAll('.ynow-btn-sync-selected-beta').forEach(function (el) {
              if (!s.btn_sync_selected_beta) return;
              var ic = el.querySelector('i');
              el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + s.btn_sync_selected_beta;
            });
            setBtnLabel('calc_capm', s.btn_calc_capm);
            setBtnLabel('calc_ddm_capm', s.btn_calc_ddm_capm || s.btn_calc_capm);
            document.querySelectorAll('.ynow-btn-calc-capm').forEach(function (el) {
              if (!s.btn_calc_capm) return;
              var ic = el.querySelector('i');
              /* Keep DDM CAPM button on its own key when present */
              var lab = (el.id === 'calc_ddm_capm' && s.btn_calc_ddm_capm) ? s.btn_calc_ddm_capm : s.btn_calc_capm;
              el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + lab;
            });
            var rfLab = document.querySelector('label[for=\"capm_rf\"]');
            if (rfLab && s.capm_rf_label) rfLab.textContent = s.capm_rf_label;
            var rmLab = document.querySelector('label[for=\"capm_rm\"]');
            if (rmLab && s.capm_rm_label) rmLab.textContent = s.capm_rm_label;
            var ddmRfLab = document.querySelector('label[for=\"ddm_capm_rf\"]');
            if (ddmRfLab && s.capm_rf_label) ddmRfLab.textContent = s.capm_rf_label;
            var ddmRmLab = document.querySelector('label[for=\"ddm_capm_rm\"]');
            if (ddmRmLab && s.capm_rm_label) ddmRmLab.textContent = s.capm_rm_label;
            var useEstRe = document.getElementById('ynow_use_estimated_re_label');
            if (useEstRe && s.use_estimated_re_label) useEstRe.textContent = s.use_estimated_re_label;
            var explainCoreTitle = document.getElementById('ynow_explain_core_callout_title');
            if (explainCoreTitle && s.explain_core_callout_title) {
              explainCoreTitle.textContent = s.explain_core_callout_title;
            }
            var explainCoreBody = document.getElementById('ynow_explain_core_callout_body');
            if (explainCoreBody && s.explain_core_callout_body) {
              explainCoreBody.textContent = s.explain_core_callout_body;
            }
            var gsModelSel = document.getElementById('ynow_gs_model_selector_title');
            if (gsModelSel && s.gs_model_selector_title) gsModelSel.textContent = s.gs_model_selector_title;
            var gsBetaOv = document.getElementById('ynow_gs_beta_overview_help');
            if (gsBetaOv && s.gs_beta_overview_help) gsBetaOv.textContent = s.gs_beta_overview_help;
            var gsPeer = document.getElementById('ynow_gs_peer_unlever_help');
            if (gsPeer && s.gs_peer_unlever_help) gsPeer.textContent = s.gs_peer_unlever_help;
            var gsRoll = document.getElementById('ynow_gs_rolling_help');
            if (gsRoll && s.gs_rolling_help) gsRoll.textContent = s.gs_rolling_help;
            var rollSetTitle = document.getElementById('ynow_beta_rolling_settings_title');
            if (rollSetTitle && s.beta_rolling_settings_title) rollSetTitle.textContent = s.beta_rolling_settings_title;
            var rollCmpTitle = document.getElementById('ynow_beta_rolling_compare_title');
            if (rollCmpTitle && s.beta_rolling_compare_title) rollCmpTitle.textContent = s.beta_rolling_compare_title;
            var rollHelpBody = document.getElementById('ynow_beta_rolling_help_body');
            if (rollHelpBody && s.beta_rolling_help_body) rollHelpBody.textContent = s.beta_rolling_help_body;
            var benchLab = document.querySelector('label[for=\"beta_bench\"]');
            if (benchLab && s.beta_bench_label) benchLab.textContent = s.beta_bench_label;
            var lookLab = document.querySelector('label[for=\"beta_lookback_months\"]');
            if (lookLab && s.beta_lookback_label) lookLab.textContent = s.beta_lookback_label;
            setBtnLabel('calc_beta_est', s.btn_calc_beta_est);
            var buHead = document.getElementById('ynow_beta_bottomup_heading');
            if (buHead && s.beta_bottomup_heading) buHead.textContent = s.beta_bottomup_heading;
            var peersLab = document.querySelector('label[for=\"beta_peers\"]');
            if (peersLab && s.beta_peers_label) peersLab.textContent = s.beta_peers_label;
            var buHelp = document.getElementById('ynow_beta_bottomup_help');
            if (buHelp && s.beta_bottomup_help) buHelp.textContent = s.beta_bottomup_help;
            setBtnLabel('calc_beta_bottomup', s.btn_calc_beta_bottomup);
            var unlHead = document.getElementById('ynow_beta_unlever_heading');
            if (unlHead && s.beta_unlever_heading) unlHead.textContent = s.beta_unlever_heading;
            var unlHelp = document.getElementById('ynow_beta_unlever_help');
            if (unlHelp && s.beta_unlever_help) unlHelp.textContent = s.beta_unlever_help;
            var manHead = document.getElementById('ynow_beta_manual_heading');
            if (manHead && s.beta_manual_heading) manHead.textContent = s.beta_manual_heading;
            var manHelp = document.getElementById('ynow_beta_manual_help');
            if (manHelp && s.beta_manual_help) manHelp.textContent = s.beta_manual_help;
            var funnelPageTitle = document.getElementById('ynow_funnel_page_title');
            if (funnelPageTitle && s.funnel_page_title) funnelPageTitle.textContent = s.funnel_page_title;
            var funnelPageSub = document.getElementById('ynow_funnel_page_sub');
            if (funnelPageSub && s.funnel_page_sub) funnelPageSub.textContent = s.funnel_page_sub;
            var funnelVerdictK = document.getElementById('ynow_funnel_verdict_kicker');
            if (funnelVerdictK && s.funnel_verdict_kicker) funnelVerdictK.textContent = s.funnel_verdict_kicker;
            var funnelVerdictT = document.getElementById('ynow_funnel_verdict_title');
            if (funnelVerdictT && s.funnel_verdict_title) funnelVerdictT.textContent = s.funnel_verdict_title;
            var funnelVerdictLead = document.getElementById('ynow_funnel_verdict_lead');
            if (funnelVerdictLead && s.funnel_verdict_lead) funnelVerdictLead.textContent = s.funnel_verdict_lead;
            var funnelRiskK = document.getElementById('ynow_funnel_risk_kicker');
            if (funnelRiskK && s.funnel_risk_kicker) funnelRiskK.textContent = s.funnel_risk_kicker;
            var funnelRiskT = document.getElementById('ynow_funnel_risk_title');
            if (funnelRiskT && s.funnel_risk_title) funnelRiskT.textContent = s.funnel_risk_title;
            var funnelRiskLead = document.getElementById('ynow_funnel_risk_lead');
            if (funnelRiskLead && s.funnel_risk_lead) funnelRiskLead.textContent = s.funnel_risk_lead;
            var funnelCh1K = document.getElementById('ynow_funnel_ch1_kicker');
            if (funnelCh1K && s.funnel_ch1_kicker) funnelCh1K.textContent = s.funnel_ch1_kicker;
            var funnelCh1T = document.getElementById('ynow_funnel_ch1_title');
            if (funnelCh1T && s.funnel_ch1_title) funnelCh1T.textContent = s.funnel_ch1_title;
            var funnelCh1Lead = document.getElementById('ynow_funnel_ch1_lead');
            if (funnelCh1Lead && s.funnel_ch1_lead) funnelCh1Lead.textContent = s.funnel_ch1_lead;
            var funnelCh2K = document.getElementById('ynow_funnel_ch2_kicker');
            if (funnelCh2K && s.funnel_ch2_kicker) funnelCh2K.textContent = s.funnel_ch2_kicker;
            var funnelCh2T = document.getElementById('ynow_funnel_ch2_title');
            if (funnelCh2T && s.funnel_ch2_title) funnelCh2T.textContent = s.funnel_ch2_title;
            var funnelCh2Lead = document.getElementById('ynow_funnel_ch2_lead');
            if (funnelCh2Lead && s.funnel_ch2_lead) funnelCh2Lead.textContent = s.funnel_ch2_lead;
            var funnelCh3K = document.getElementById('ynow_funnel_ch3_kicker');
            if (funnelCh3K && s.funnel_ch3_kicker) funnelCh3K.textContent = s.funnel_ch3_kicker;
            var funnelCh3T = document.getElementById('ynow_funnel_ch3_title');
            if (funnelCh3T && s.funnel_ch3_title) funnelCh3T.textContent = s.funnel_ch3_title;
            var funnelCh3Lead = document.getElementById('ynow_funnel_ch3_lead');
            if (funnelCh3Lead && s.funnel_ch3_lead) funnelCh3Lead.textContent = s.funnel_ch3_lead;
            if (s.notes_title) {
              document.querySelectorAll('.ynow-notes__title').forEach(function (el) {
                el.textContent = s.notes_title;
              });
            }
            if (s.notes_toggle_aria) {
              document.querySelectorAll('.ynow-notes__summary').forEach(function (el) {
                if (el.id === 'ynow_ms_dims_summary') return;
                el.setAttribute('title', s.notes_toggle_aria);
                el.setAttribute('aria-label', s.notes_toggle_aria);
              });
            }
            var msDimsTitle = document.getElementById('ynow_ms_dims_title');
            if (msDimsTitle && s.gs_ms_dims_title) msDimsTitle.textContent = s.gs_ms_dims_title;
            var msDimsSummary = document.getElementById('ynow_ms_dims_summary');
            if (msDimsSummary && s.gs_ms_dims_toggle_aria) {
              msDimsSummary.setAttribute('title', s.gs_ms_dims_toggle_aria);
              msDimsSummary.setAttribute('aria-label', s.gs_ms_dims_toggle_aria);
            }
            var kpiJumpQoe = document.getElementById('ynow_kpi_jump_qoe');
            if (kpiJumpQoe && s.funnel_kpi_jump_qoe_aria) kpiJumpQoe.setAttribute('aria-label', s.funnel_kpi_jump_qoe_aria);
            var kpiJumpFs = document.getElementById('ynow_kpi_jump_fscore');
            if (kpiJumpFs && s.funnel_kpi_jump_fscore_aria) kpiJumpFs.setAttribute('aria-label', s.funnel_kpi_jump_fscore_aria);
            var kpiJumpAl = document.getElementById('ynow_kpi_jump_alerts');
            if (kpiJumpAl && s.funnel_kpi_jump_alerts_aria) kpiJumpAl.setAttribute('aria-label', s.funnel_kpi_jump_alerts_aria);
            var funnelSecMethod = document.getElementById('ynow_funnel_sec_method');
            if (funnelSecMethod && s.funnel_sec_method) funnelSecMethod.textContent = s.funnel_sec_method;
            var funnelMethodBody = document.getElementById('ynow_funnel_method_body');
            if (funnelMethodBody && s.funnel_method_body) funnelMethodBody.textContent = s.funnel_method_body;
            var funnelMethodCaveat = document.getElementById('ynow_funnel_method_caveat');
            if (funnelMethodCaveat && s.funnel_method_caveat) funnelMethodCaveat.textContent = s.funnel_method_caveat;
            var funnelFs = document.getElementById('ynow_funnel_fscore_list_title');
            if (funnelFs && s.funnel_fscore_list_title) funnelFs.textContent = s.funnel_fscore_list_title;
            var funnelMomTitle = document.getElementById('ynow_funnel_mom_box_title');
            if (funnelMomTitle && s.funnel_mom_box_title) funnelMomTitle.textContent = s.funnel_mom_box_title;
            var funnelMomIntro = document.getElementById('ynow_funnel_mom_intro');
            if (funnelMomIntro && s.funnel_mom_intro) funnelMomIntro.textContent = s.funnel_mom_intro;
            var funnelMomLogic = document.getElementById('ynow_funnel_mom_logic_title');
            if (funnelMomLogic && s.funnel_mom_logic_title) funnelMomLogic.textContent = s.funnel_mom_logic_title;
            var funnelMomCond1 = document.getElementById('ynow_funnel_mom_cond1');
            if (funnelMomCond1 && s.funnel_mom_cond1) funnelMomCond1.textContent = s.funnel_mom_cond1;
            var funnelMomCond1Lab = document.getElementById('ynow_funnel_mom_cond1_label');
            if (funnelMomCond1Lab && s.funnel_mom_cond1_label) funnelMomCond1Lab.textContent = s.funnel_mom_cond1_label;
            var funnelMomCond2 = document.getElementById('ynow_funnel_mom_cond2');
            if (funnelMomCond2 && s.funnel_mom_cond2) funnelMomCond2.textContent = s.funnel_mom_cond2;
            var funnelMomCond2Lab = document.getElementById('ynow_funnel_mom_cond2_label');
            if (funnelMomCond2Lab && s.funnel_mom_cond2_label) funnelMomCond2Lab.textContent = s.funnel_mom_cond2_label;
            var funnelMomBull = document.getElementById('ynow_funnel_mom_bull_rule');
            if (funnelMomBull && s.funnel_mom_bull_rule) funnelMomBull.textContent = s.funnel_mom_bull_rule;
            var funnelMomData = document.getElementById('ynow_funnel_mom_data_title');
            if (funnelMomData && s.funnel_mom_data_title) funnelMomData.textContent = s.funnel_mom_data_title;
            var funnelMomD1 = document.getElementById('ynow_funnel_mom_data_1');
            if (funnelMomD1 && s.funnel_mom_data_1) funnelMomD1.textContent = s.funnel_mom_data_1;
            var funnelMomD2 = document.getElementById('ynow_funnel_mom_data_2');
            if (funnelMomD2 && s.funnel_mom_data_2) funnelMomD2.textContent = s.funnel_mom_data_2;
            var funnelMomD3 = document.getElementById('ynow_funnel_mom_data_3');
            if (funnelMomD3 && s.funnel_mom_data_3) funnelMomD3.textContent = s.funnel_mom_data_3;
            var kpiBlue = document.getElementById('ynow_kpi_legend_blue');
            if (kpiBlue && s.kpi_legend_blue) kpiBlue.textContent = s.kpi_legend_blue;
            var kpiRed = document.getElementById('ynow_kpi_legend_red');
            if (kpiRed && s.kpi_legend_red) kpiRed.textContent = s.kpi_legend_red;
            var kpiBlack = document.getElementById('ynow_kpi_legend_black');
            if (kpiBlack && s.kpi_legend_black) kpiBlack.textContent = s.kpi_legend_black;
            var kpiWhite = document.getElementById('ynow_kpi_legend_white');
            if (kpiWhite && s.kpi_legend_white) kpiWhite.textContent = s.kpi_legend_white;
            var kpiFocus = document.getElementById('ynow_kpi_legend_focus');
            if (kpiFocus && s.kpi_legend_focus_metric) kpiFocus.textContent = s.kpi_legend_focus_metric;
            var fundProfLab = document.getElementById('ynow_fund_profile_label');
            if (fundProfLab && s.fund_profile_label) fundProfLab.textContent = s.fund_profile_label + '：';
            var fundBadge = document.getElementById('ynow_fund_profile_badge_text');
            if (fundBadge) {
              var pid = fundBadge.getAttribute('data-profile-id') || '';
              var pkey = pid ? ('fund_profile_' + pid) : '';
              if (pkey && s[pkey]) fundBadge.textContent = s[pkey];
            }
            if (typeof window.ynowSyncDownloadReportLabel === 'function') {
              window.ynowSyncDownloadReportLabel();
            } else {
              var dl = document.getElementById('download_report');
              if (dl && s.download_report) {
                /* Keep current icon (download vs lock); sync may swap after. */
                var icon = dl.querySelector('i');
                var iconHtml = icon ? icon.outerHTML + ' ' : '';
                dl.innerHTML = iconHtml + s.download_report;
                if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                  window.ynowSyncSidebarDownloadIcon();
                }
              }
            }
            try {
              if (typeof window.ynowApplyPdfQuotaCopy === 'function') {
                window.ynowApplyPdfQuotaCopy(s);
              } else {
                var remEl = document.getElementById('ynow_pdf_quota_remaining');
                var rulesEl = document.getElementById('ynow_pdf_quota_rules');
                var liteOn = document.body && document.body.classList.contains('ynow-lite');
                if (liteOn) {
                  if (remEl && (s.pdf_quota_hint_lite || s.pdf_quota_hint)) {
                    remEl.textContent = s.pdf_quota_hint_lite || s.pdf_quota_hint;
                  }
                  if (rulesEl) rulesEl.textContent = '';
                } else if (remEl && s.pdf_quota_hint) {
                  remEl.textContent = s.pdf_quota_hint;
                }
              }
            } catch (ePdfQ) {}
            var snapQuota = document.getElementById('ynow_snapshot_download_quota');
            if (snapQuota) {
              var liteSnap = document.body && document.body.classList.contains('ynow-lite');
              var snapHint = liteSnap
                ? (s.snapshot_download_quota_hint_lite || s.snapshot_download_quota_hint)
                : (s.snapshot_download_quota_hint || '');
              if (snapHint) snapQuota.textContent = snapHint;
            }
            var snap = document.getElementById('ynow_snapshot_link_label');
            if (snap && s.snapshot_link) snap.textContent = s.snapshot_link;
            var snapTitle = document.getElementById('ynow_snapshot_page_title');
            if (snapTitle && s.snapshot_page_title) snapTitle.textContent = s.snapshot_page_title;
            var snapHelp = document.getElementById('ynow_snapshot_page_help');
            if (snapHelp && s.snapshot_page_help) snapHelp.textContent = s.snapshot_page_help;
            var snapHelpLite = document.getElementById('ynow_snapshot_page_help_lite');
            if (snapHelpLite && s.snapshot_page_help_lite) snapHelpLite.textContent = s.snapshot_page_help_lite;
            var snapDefaultsHelpLite = document.getElementById('ynow_snapshot_defaults_help_lite');
            if (snapDefaultsHelpLite && s.snapshot_defaults_help_lite) {
              snapDefaultsHelpLite.textContent = s.snapshot_defaults_help_lite;
            }
            var snapTabAudit = document.getElementById('ynow_snapshot_tab_audit');
            if (snapTabAudit && (s.snapshot_tab_audit || s.param_audit_title)) {
              snapTabAudit.textContent = s.snapshot_tab_audit || s.param_audit_title;
            }
            var snapTabCurrent = document.getElementById('ynow_snapshot_tab_current');
            if (snapTabCurrent && s.snapshot_tab_current) snapTabCurrent.textContent = s.snapshot_tab_current;
            var snapTabDefaults = document.getElementById('ynow_snapshot_tab_defaults');
            if (snapTabDefaults && s.snapshot_tab_defaults) snapTabDefaults.textContent = s.snapshot_tab_defaults;
            var snapDefaultsHelp = document.getElementById('ynow_snapshot_defaults_help');
            if (snapDefaultsHelp && s.snapshot_defaults_help) snapDefaultsHelp.textContent = s.snapshot_defaults_help;
            var snapDefaultsLocked = document.getElementById('ynow_snapshot_defaults_locked_msg');
            if (snapDefaultsLocked && s.snapshot_defaults_locked) {
              snapDefaultsLocked.textContent = s.snapshot_defaults_locked;
            }
            var dlSnapBtn = document.getElementById('ynow_download_snapshot_btn');
            if (dlSnapBtn && s.download_snapshot_btn) dlSnapBtn.textContent = s.download_snapshot_btn;
            var dlRestoreBtn = document.getElementById('ynow_download_param_restore_btn');
            if (dlRestoreBtn && s.download_param_restore_btn) dlRestoreBtn.textContent = s.download_param_restore_btn;
            var restoreTitle = document.getElementById('ynow_param_restore_title');
            if (restoreTitle && s.param_restore_title) restoreTitle.textContent = s.param_restore_title;
            var restoreHelp = document.getElementById('ynow_param_restore_help');
            if (restoreHelp && s.param_restore_help) restoreHelp.textContent = s.param_restore_help;
            var restoreFileLab = document.getElementById('ynow_param_restore_file_label');
            if (restoreFileLab && s.param_restore_file_label) restoreFileLab.textContent = s.param_restore_file_label;
            var restoreBtn = document.getElementById('ynow_param_restore_btn');
            if (restoreBtn && s.param_restore_btn) restoreBtn.textContent = s.param_restore_btn;
            var dlDefaultsBtn = document.getElementById('ynow_download_defaults_btn');
            if (dlDefaultsBtn && s.download_defaults_btn) dlDefaultsBtn.textContent = s.download_defaults_btn;
            var restoreBrowseRoot = document.getElementById('param_restore_file');
            if (restoreBrowseRoot) {
              var restoreBrowse = restoreBrowseRoot.closest('.form-group, .shiny-input-container');
              if (restoreBrowse) {
                var browseBtn = restoreBrowse.querySelector('.btn-file, .input-group-btn label, .input-group-btn .btn');
                if (browseBtn && s.param_restore_browse_btn) {
                  var onlyText = true;
                  browseBtn.childNodes.forEach(function (n) {
                    if (n.nodeType === 3 && String(n.textContent || '').trim()) {
                      n.textContent = ' ' + s.param_restore_browse_btn + ' ';
                      onlyText = false;
                    }
                  });
                  if (onlyText && !browseBtn.querySelector('i')) browseBtn.textContent = s.param_restore_browse_btn;
                }
                var ph = restoreBrowse.querySelector('input[type=\"text\"]');
                if (ph && s.param_restore_placeholder) {
                  ph.setAttribute('placeholder', s.param_restore_placeholder);
                  ph.placeholder = s.param_restore_placeholder;
                }
              }
            }
            /* Market buttons show equal-size flag glyphs; locale only updates a11y labels. */
            var mktUs = document.getElementById('ynow_mkt_btn_us');
            if (mktUs && s.market_us) {
              mktUs.setAttribute('aria-label', s.market_us);
              mktUs.setAttribute('title', s.market_us);
            }
            var mktTw = document.getElementById('ynow_mkt_btn_tw');
            if (mktTw && s.market_tw) {
              mktTw.setAttribute('aria-label', s.market_tw);
              mktTw.setAttribute('title', s.market_tw);
            }
            var mktStack = document.querySelector('#ynow-market-header .ynow-market-stack');
            if (mktStack && s.market_hint) mktStack.setAttribute('aria-label', s.market_hint);
            var langStack = document.getElementById('ynow_lang_stack') ||
              document.querySelector('.ynow-lang-header .ynow-lang-stack');
            if (langStack && s.hdr_lang_label) langStack.setAttribute('aria-label', s.hdr_lang_label);
            var langZh = document.getElementById('ynow_lang_btn_zh');
            var langEn = document.getElementById('ynow_lang_btn_en');
            if (langZh && s.hdr_lang_zh) langZh.textContent = s.hdr_lang_zh;
            if (langEn && s.hdr_lang_en) langEn.textContent = s.hdr_lang_en;
            var loc = (payload && payload.locale) ? String(payload.locale) : 'en';
            document.querySelectorAll('.ynow-lang-stack .ynow-lang-btn').forEach(function (b) {
              b.classList.toggle('active', b.getAttribute('data-value') === loc);
            });
            var ccyFloat = document.querySelector('.ynow-ccy-float');
            if (ccyFloat && s.hdr_ccy_label) ccyFloat.setAttribute('aria-label', s.hdr_ccy_label);
            setBtText('ynow_hfv_rel_models_note', 'hfv_rel_models_note');
            setBtText('ynow_lab_im_methods_label', 'lab_im_methods_label');
            setBtText('ynow_lab_im_methods_abs_label', 'lab_im_methods_abs_label');
            setBtText('ynow_lab_im_methods_rel_label', 'lab_im_methods_rel_label');
            setBtText('ynow_lab_im_methods_locked_hint', 'lab_im_methods_locked_hint');
            setBtText('ynow_lab_im_method_nav', 'lab_im_method_nav');
            setBtText('ynow_lab_im_method_pb', 'lab_im_method_pb');
            setBtText('ynow_lab_im_method_dcf', 'lab_im_method_dcf');
            setBtText('ynow_lab_im_method_ddm', 'lab_im_method_ddm');
            setBtText('ynow_lab_im_method_ri', 'lab_im_method_ri');
            setBtText('ynow_lab_im_method_enterprise', 'lab_im_method_enterprise');
            setBtText('ynow_lab_im_method_earnings', 'lab_im_method_earnings');
            setBtText('ynow_lab_im_method_revenue', 'lab_im_method_revenue');
            setBtText('ynow_lab_im_method_book', 'lab_im_method_book');
            setBtText('ynow_menu_pb', 'menu_pb');
            setBtText('ynow_menu_rel_book', 'menu_rel_book');
            setBtText('ynow_lab_im_chains_label', 'lab_im_chains_label');
            setBtText('ynow_lab_im_chains_hint', 'lab_im_chains_hint');
            setBtText('ynow_lab_im_industries_label', 'lab_im_industries_label');
            setBtText('ynow_lab_im_industries_hint', 'lab_im_industries_hint');
            var hfvBenchLab = document.getElementById('ynow_hfv_show_bench_label');
            if (hfvBenchLab && s.hfv_show_bench) hfvBenchLab.textContent = s.hfv_show_bench;
            var convLab = document.querySelector('label[for=\"bt_fv_conv_window\"]');
            if (convLab && s.hfv_conv_window_label) convLab.textContent = s.hfv_conv_window_label;
            var paHelp = document.getElementById('ynow_param_audit_help');
            if (paHelp && s.param_audit_help) paHelp.textContent = s.param_audit_help;
            var paPdfTitle = document.getElementById('ynow_param_audit_pdf_title');
            if (paPdfTitle && s.param_audit_pdf_title) paPdfTitle.textContent = s.param_audit_pdf_title;
            var paPdfHelp = document.getElementById('ynow_param_audit_pdf_help');
            if (paPdfHelp && s.param_audit_pdf_help) paPdfHelp.textContent = s.param_audit_pdf_help;
            var paPdfPages = document.getElementById('ynow_param_audit_pdf_pages_label');
            if (paPdfPages && s.param_audit_pdf_pages_label) paPdfPages.textContent = s.param_audit_pdf_pages_label;
            var paPdfBtn = document.getElementById('ynow_param_audit_pdf_btn');
            if (paPdfBtn && s.param_audit_pdf_btn) paPdfBtn.textContent = s.param_audit_pdf_btn;
            document.querySelectorAll('.ynow-pdf-page-lab').forEach(function (el) {
              var k = el.getAttribute('data-key');
              if (k && s[k]) el.textContent = s[k];
            });
            document.querySelectorAll('.ynow-pdf-pages-group-label').forEach(function (el) {
              var k = el.getAttribute('data-key');
              if (k && s[k]) el.textContent = s[k];
            });
            var testL = document.getElementById('ynow_test_link_label');
            if (testL && s.test_link) testL.textContent = s.test_link;
            var fb = document.getElementById('ynow_feedback_link_label');
            if (fb && s.feedback_link) fb.textContent = s.feedback_link;
            var testTitle = document.getElementById('ynow_testing_page_title');
            if (testTitle && s.testing_page_title) testTitle.textContent = s.testing_page_title;
            var testSub = document.getElementById('ynow_testing_page_sub');
            if (testSub && s.testing_page_sub) testSub.textContent = s.testing_page_sub;
            var testBoxBody = document.getElementById('ynow_testing_box_body');
            if (testBoxBody && s.testing_box_body) testBoxBody.textContent = s.testing_box_body;
            var ptsSysTitle = document.getElementById('ynow_pts_sysadmin_title');
            if (ptsSysTitle && s.pts_sysadmin_title) ptsSysTitle.textContent = s.pts_sysadmin_title;
            var ptsSysIntro = document.getElementById('ynow_pts_sysadmin_intro');
            if (ptsSysIntro && s.pts_sysadmin_intro) ptsSysIntro.textContent = s.pts_sysadmin_intro;
            var ptsSysEnter = document.getElementById('ynow_pts_sysadmin_enter_label');
            if (ptsSysEnter && s.pts_sysadmin_enter) ptsSysEnter.textContent = s.pts_sysadmin_enter;
            var ptsSysExit = document.getElementById('ynow_pts_sysadmin_exit_label');
            if (ptsSysExit && s.pts_sysadmin_exit) ptsSysExit.textContent = s.pts_sysadmin_exit;
            var ptsLoginUserLab = document.getElementById('ynow_pts_sysadmin_login_user_label');
            if (ptsLoginUserLab && s.pts_sysadmin_login_user) ptsLoginUserLab.textContent = s.pts_sysadmin_login_user;
            var ptsLoginPassLab = document.getElementById('ynow_pts_sysadmin_login_pass_label');
            if (ptsLoginPassLab && s.pts_sysadmin_login_pass) ptsLoginPassLab.textContent = s.pts_sysadmin_login_pass;
            var ptsViewAs = document.getElementById('ynow_pts_sysadmin_viewas_label');
            if (ptsViewAs && s.pts_sysadmin_viewas) ptsViewAs.textContent = s.pts_sysadmin_viewas;
            var ptsViewAsActive = document.getElementById('ynow_pts_sysadmin_viewas_active_label');
            if (ptsViewAsActive && s.pts_sysadmin_viewas_active) ptsViewAsActive.textContent = s.pts_sysadmin_viewas_active;
            var vaChromeActive = document.getElementById('ynow_viewas_chrome_active_label');
            if (vaChromeActive && s.pts_sysadmin_viewas_active) vaChromeActive.textContent = s.pts_sysadmin_viewas_active;
            var vaChromeHint = document.getElementById('ynow_viewas_chrome_hint');
            if (vaChromeHint && s.pts_sysadmin_viewas_chrome_hint) vaChromeHint.textContent = s.pts_sysadmin_viewas_chrome_hint;
            var vaChromeExit = document.getElementById('ynow_viewas_chrome_exit_label');
            if (vaChromeExit && s.pts_sysadmin_viewas_exit) vaChromeExit.textContent = s.pts_sysadmin_viewas_exit;
            var selfViewLab = document.getElementById('ynow_pts_sysadmin_self_view_label');
            if (selfViewLab && s.pts_sysadmin_self_view_label) selfViewLab.textContent = s.pts_sysadmin_self_view_label;
            var selfGuestLab = document.getElementById('ynow_pts_sysadmin_self_guest_label');
            if (selfGuestLab && s.pts_sysadmin_self_guest) selfGuestLab.textContent = s.pts_sysadmin_self_guest;
            var selfOmniLab = document.getElementById('ynow_pts_sysadmin_self_omniscient_label');
            if (selfOmniLab && s.pts_sysadmin_self_omniscient) selfOmniLab.textContent = s.pts_sysadmin_self_omniscient;
            var selfExitLab = document.getElementById('ynow_pts_sysadmin_self_exit_label');
            if (selfExitLab && s.pts_sysadmin_self_exit) selfExitLab.textContent = s.pts_sysadmin_self_exit;
            var selfHelp = document.getElementById('ynow_pts_sysadmin_self_help');
            if (selfHelp && s.pts_sysadmin_self_help) selfHelp.textContent = s.pts_sysadmin_self_help;
            var ctrlPanel = document.getElementById('ynow_sysadmin_control_panel');
            if (ctrlPanel && s.pts_sysadmin_control_panel_aria) {
              ctrlPanel.setAttribute('aria-label', s.pts_sysadmin_control_panel_aria);
            }
            var adminOpsTab = document.getElementById('ynow_admin_ops_tab_label');
            if (adminOpsTab && s.admin_ops_tab) adminOpsTab.textContent = s.admin_ops_tab;
            var adminOpsControlsTab = document.getElementById('ynow_admin_ops_controls_tab_label');
            if (adminOpsControlsTab && s.admin_ops_controls_tab) {
              adminOpsControlsTab.textContent = s.admin_ops_controls_tab;
            }
            var adminOpsTitle = document.getElementById('ynow_admin_ops_title');
            if (adminOpsTitle && s.admin_ops_title) adminOpsTitle.textContent = s.admin_ops_title;
            var adminOpsHelp = document.getElementById('ynow_admin_ops_help');
            if (adminOpsHelp && s.admin_ops_help) adminOpsHelp.textContent = s.admin_ops_help;
            var adminOpsEmpty = document.getElementById('ynow_admin_ops_empty');
            if (adminOpsEmpty && s.admin_ops_empty) adminOpsEmpty.textContent = s.admin_ops_empty;
            var ptsColon = (payload && payload.locale === 'zh-TW') ? '：' : ':';
            var ptsHdrForLocale = document.getElementById('ynow-pts-header');
            var ptsSideForLocale = document.getElementById('ynow-pts-sidebar');
            var ptsGuestHidden = !!(
              (ptsHdrForLocale && ptsHdrForLocale.classList.contains('ynow-pts-hidden')) ||
              (ptsSideForLocale && ptsSideForLocale.classList.contains('ynow-pts-hidden'))
            );
            /* Do not refill FYN/PYN banner labels while guest-hidden. */
            if (!ptsGuestHidden) {
              var ptsFynLab = document.getElementById('ynow_pts_banner_fyn_label');
              if (ptsFynLab && s.pts_fyn_label) ptsFynLab.textContent = s.pts_fyn_label + ptsColon;
              var ptsPynLab = document.getElementById('ynow_pts_banner_pyn_label');
              if (ptsPynLab && s.pts_pyn_label) ptsPynLab.textContent = s.pts_pyn_label + ptsColon;
              var ptsSideFynLab = document.getElementById('ynow_pts_sidebar_fyn_label');
              if (ptsSideFynLab && s.pts_fyn_label) ptsSideFynLab.textContent = s.pts_fyn_label + ptsColon;
              var ptsSidePynLab = document.getElementById('ynow_pts_sidebar_pyn_label');
              if (ptsSidePynLab && s.pts_pyn_label) ptsSidePynLab.textContent = s.pts_pyn_label + ptsColon;
            }
            var atxTitle = document.getElementById('ynow_atx_page_title');
            if (atxTitle && s.atx_page_title) atxTitle.textContent = s.atx_page_title;
            var atxSub = document.getElementById('ynow_atx_page_sub');
            if (atxSub && s.atx_page_sub) atxSub.textContent = s.atx_page_sub;
            var atxBoxTitle = document.getElementById('ynow_atx_box_title');
            if (atxBoxTitle && s.atx_box_title) atxBoxTitle.textContent = s.atx_box_title;
            var atxBoxBody = document.getElementById('ynow_atx_box_body');
            if (atxBoxBody && s.atx_box_body) atxBoxBody.textContent = s.atx_box_body;
            var atxNodesTab = document.getElementById('ynow_atx_nodes_tab');
            if (atxNodesTab && s.atx_nodes_title) atxNodesTab.textContent = s.atx_nodes_title;
            var atxEdgesTab = document.getElementById('ynow_atx_edges_tab');
            if (atxEdgesTab && s.atx_edges_title) atxEdgesTab.textContent = s.atx_edges_title;
            var runBt = document.getElementById('run_bt');
            if (runBt && s.btn_run_bt) {
              var runIcon = runBt.querySelector('i');
              var runIconHtml = runIcon ? runIcon.outerHTML + ' ' : '';
              runBt.innerHTML = runIconHtml + s.btn_run_bt;
            }
            setBtnLabel('bt_kpi_filter', s.btn_bt_kpi_filter);
            setBtnLabel('bt_refresh_params', s.btn_bt_refresh_params);
            setBtnLabel('bt_fit_bh_preset', s.btn_bt_fit_bh);
            var labImRun = document.getElementById('lab_im_run_fscore');
            if (labImRun && s.btn_lab_im_run) {
              var labBusyMap = window.__ynowBtnBusy || {};
              var labRunLbl = labBusyMap['lab_im_run_fscore']
                ? (s.btn_lab_im_run_busy || s.btn_lab_im_run)
                : s.btn_lab_im_run;
              var labIcon = labImRun.querySelector('i');
              var labIconHtml = labIcon ? labIcon.outerHTML + ' ' : '';
              labImRun.innerHTML = labIconHtml + labRunLbl;
            }
            var tickerSearchBtn = document.getElementById('search');
            if (tickerSearchBtn && s.btn_ticker_search) {
              var tkBusyMap = window.__ynowBtnBusy || {};
              var tkLbl = tkBusyMap['search']
                ? (s.btn_ticker_search_busy || s.btn_ticker_search)
                : s.btn_ticker_search;
              var tkIcon = tickerSearchBtn.querySelector('i');
              var tkIconHtml = tkIcon ? tkIcon.outerHTML + ' ' : '';
              tickerSearchBtn.innerHTML = tkIconHtml + tkLbl;
            }
            var labMaxNLabel = document.getElementById('ynow_lab_im_max_n_label');
            if (labMaxNLabel && s.lab_im_max_n_label) labMaxNLabel.textContent = s.lab_im_max_n_label;
            var labMaxNCustom = document.getElementById('ynow_lab_im_max_n_custom_label');
            if (labMaxNCustom && s.lab_im_max_n_custom_label) labMaxNCustom.textContent = s.lab_im_max_n_custom_label;
            var labPoolRank = document.getElementById('ynow_lab_im_pool_rank_label');
            if (labPoolRank && s.lab_im_pool_rank_label) labPoolRank.textContent = s.lab_im_pool_rank_label;
            var labConcepts = document.getElementById('ynow_lab_im_concepts_label');
            if (labConcepts && s.lab_im_concepts_label) labConcepts.textContent = s.lab_im_concepts_label;
            var labDetailIntro = document.getElementById('ynow_lab_im_detail_intro');
            if (labDetailIntro && s.lab_im_detail_intro) labDetailIntro.textContent = s.lab_im_detail_intro;
            setBtText('ynow_lab_im_detail_box_title', 'lab_im_detail_box_title');
            var labLbMode = document.getElementById('ynow_lab_im_lb_mode_label');
            if (labLbMode && s.lab_im_lb_mode_label) labLbMode.textContent = s.lab_im_lb_mode_label;
            var labLbOverall = document.getElementById('ynow_lab_im_lb_mode_overall');
            if (labLbOverall && s.lab_im_lb_mode_overall) labLbOverall.textContent = s.lab_im_lb_mode_overall;
            var labLbByInd = document.getElementById('ynow_lab_im_lb_mode_by_ind');
            if (labLbByInd && s.lab_im_lb_mode_by_industry) labLbByInd.textContent = s.lab_im_lb_mode_by_industry;
            var labLbUndervalued = document.getElementById('ynow_lab_im_lb_mode_undervalued');
            if (labLbUndervalued && s.lab_im_lb_mode_undervalued) labLbUndervalued.textContent = s.lab_im_lb_mode_undervalued;
            var labIndMcapTitle = document.getElementById('ynow_lab_im_ind_mcap_title');
            if (labIndMcapTitle && s.lab_im_ind_mcap_title) labIndMcapTitle.textContent = s.lab_im_ind_mcap_title;
            var labIndMcapBadge = document.getElementById('ynow_lab_im_ind_mcap_badge');
            if (labIndMcapBadge && s.lab_im_ind_mcap_badge) labIndMcapBadge.textContent = s.lab_im_ind_mcap_badge;
            var labIndMcapHelp = document.getElementById('ynow_lab_im_ind_mcap_help');
            if (labIndMcapHelp && s.lab_im_ind_mcap_help) labIndMcapHelp.textContent = s.lab_im_ind_mcap_help;
            var labIndMcapTreemapLegend = document.getElementById('ynow_lab_im_ind_mcap_treemap_legend');
            if (labIndMcapTreemapLegend && s.lab_im_ind_mcap_treemap_legend) {
              labIndMcapTreemapLegend.textContent = s.lab_im_ind_mcap_treemap_legend;
            }
            var labIndMcapRefreshLabel = document.getElementById('ynow_lab_im_refresh_ind_mcap_label');
            if (labIndMcapRefreshLabel && s.btn_lab_im_refresh_ind_mcap) {
              var indBusyMap = window.__ynowBtnBusy || {};
              labIndMcapRefreshLabel.textContent = indBusyMap['lab_im_refresh_ind_mcap']
                ? (s.btn_lab_im_refresh_ind_mcap_busy || s.btn_lab_im_refresh_ind_mcap)
                : s.btn_lab_im_refresh_ind_mcap;
            }
            var labIndMcapRefreshBtn = document.getElementById('lab_im_refresh_ind_mcap');
            if (labIndMcapRefreshBtn && s.lab_im_refresh_ind_mcap_title) {
              labIndMcapRefreshBtn.setAttribute('title', s.lab_im_refresh_ind_mcap_title);
            }
            var labBoardsLabel = document.getElementById('ynow_lab_im_boards_label');
            if (labBoardsLabel && s.lab_im_boards_label) labBoardsLabel.textContent = s.lab_im_boards_label;
            var labBoardTwse = document.getElementById('ynow_lab_im_board_twse');
            if (labBoardTwse && s.lab_im_board_twse) labBoardTwse.textContent = s.lab_im_board_twse;
            var labBoardTpex = document.getElementById('ynow_lab_im_board_tpex');
            if (labBoardTpex && s.lab_im_board_tpex) labBoardTpex.textContent = s.lab_im_board_tpex;
            var labBoardEsb = document.getElementById('ynow_lab_im_board_esb');
            if (labBoardEsb && s.lab_im_board_esb) labBoardEsb.textContent = s.lab_im_board_esb;
            var labBoardsHint = document.getElementById('ynow_lab_im_boards_hint');
            if (labBoardsHint && s.lab_im_boards_hint) labBoardsHint.textContent = s.lab_im_boards_hint;
            var labLbHelp = document.getElementById('ynow_lab_im_lb_scope_help');
            if (labLbHelp && s.lab_im_lb_scope_help) labLbHelp.textContent = s.lab_im_lb_scope_help;
            var labGateLabel = document.getElementById('ynow_lab_im_gate_label');
            if (labGateLabel && s.lab_im_gate_label) labGateLabel.textContent = s.lab_im_gate_label;
            var labGateHint = document.getElementById('ynow_lab_im_gate_hint');
            if (labGateHint && s.lab_im_gate_hint) labGateHint.textContent = s.lab_im_gate_hint;
            var labNoAlertLabel = document.getElementById('ynow_lab_im_no_alert_label');
            if (labNoAlertLabel && s.lab_im_no_alert_label) labNoAlertLabel.textContent = s.lab_im_no_alert_label;
            var labNoAlertHint = document.getElementById('ynow_lab_im_no_alert_hint');
            if (labNoAlertHint && s.lab_im_no_alert_hint) labNoAlertHint.textContent = s.lab_im_no_alert_hint;
            var labImRunTitle = document.getElementById('lab_im_run_fscore');
            if (labImRunTitle && s.lab_im_run_title) labImRunTitle.setAttribute('title', s.lab_im_run_title);
            var annFs = document.getElementById('ynow_ann_fscore_crossref');
            if (annFs && s.ann_fscore_crossref) annFs.textContent = s.ann_fscore_crossref;
            var labUsListedLabel = document.getElementById('ynow_lab_im_us_listed_label');
            if (labUsListedLabel && s.lab_im_us_listed_label) labUsListedLabel.textContent = s.lab_im_us_listed_label;
            var labUsListedHint = document.getElementById('ynow_lab_im_us_listed_hint');
            if (labUsListedHint && s.lab_im_us_listed_hint) labUsListedHint.textContent = s.lab_im_us_listed_hint;
            var labAdrLabel = document.getElementById('ynow_lab_im_include_adr_label');
            if (labAdrLabel && s.lab_im_include_adr_label) labAdrLabel.textContent = s.lab_im_include_adr_label;
            var labAdrHint = document.getElementById('ynow_lab_im_include_adr_hint');
            if (labAdrHint && s.lab_im_include_adr_hint) labAdrHint.textContent = s.lab_im_include_adr_hint;
            var labEqLabel = document.getElementById('ynow_lab_im_eq_label');
            if (labEqLabel && s.lab_im_eq_label) labEqLabel.textContent = s.lab_im_eq_label;
            var labEqHint = document.getElementById('ynow_lab_im_eq_hint');
            if (labEqHint && s.lab_im_eq_hint) labEqHint.textContent = s.lab_im_eq_hint;
            var labEqExplainTitle = document.getElementById('ynow_lab_im_eq_explain_title');
            if (labEqExplainTitle && s.lab_im_eq_explain_title) labEqExplainTitle.textContent = s.lab_im_eq_explain_title;
            var labEqExplainBody = document.getElementById('ynow_lab_im_eq_explain_body');
            if (labEqExplainBody && s.lab_im_eq_explain_body) labEqExplainBody.textContent = s.lab_im_eq_explain_body;
            var labQueryTitle = document.getElementById('ynow_lab_im_query_title');
            if (labQueryTitle && s.lab_im_query_title) labQueryTitle.textContent = s.lab_im_query_title;
            var labEvalHint = document.getElementById('ynow_lab_im_eval_hint');
            if (labEvalHint && s.lab_im_eval_hint) labEvalHint.textContent = s.lab_im_eval_hint;
            var labRefreshLabel = document.getElementById('ynow_lab_im_refresh_label');
            if (labRefreshLabel && s.btn_lab_im_refresh) {
              var uniBusyMap = window.__ynowBtnBusy || {};
              labRefreshLabel.textContent = uniBusyMap['lab_im_refresh_universe']
                ? (s.btn_lab_im_refresh_busy || s.btn_lab_im_refresh)
                : s.btn_lab_im_refresh;
            }
            var labRefreshBtn = document.getElementById('lab_im_refresh_universe');
            if (labRefreshBtn && s.lab_im_refresh_title) labRefreshBtn.setAttribute('title', s.lab_im_refresh_title);
            var labDl = document.getElementById('lab_im_download_report');
            if (labDl && s.btn_lab_im_download) {
              var dlIcon = labDl.querySelector('i');
              var dlIconHtml = dlIcon ? dlIcon.outerHTML + ' ' : '';
              labDl.innerHTML = dlIconHtml + s.btn_lab_im_download;
              if (s.lab_im_download_title) labDl.setAttribute('title', s.lab_im_download_title);
            }
            var clusterBlurb = document.getElementById('ynow_lab_cluster_blurb');
            if (clusterBlurb && s.lab_cluster_blurb) clusterBlurb.textContent = s.lab_cluster_blurb;
            var clusterDisc = document.getElementById('ynow_lab_cluster_disclaimer');
            if (clusterDisc && s.lab_cluster_disclaimer) clusterDisc.textContent = s.lab_cluster_disclaimer;
            var clusterHint = document.getElementById('ynow_lab_cluster_hint');
            if (clusterHint && s.lab_cluster_hint) clusterHint.textContent = s.lab_cluster_hint;
            var clusterK = document.getElementById('ynow_lab_cluster_k_label');
            if (clusterK && s.lab_cluster_k_label) clusterK.textContent = s.lab_cluster_k_label;
            var clusterX = document.getElementById('ynow_lab_cluster_x_label');
            if (clusterX && s.lab_cluster_x_label) clusterX.textContent = s.lab_cluster_x_label;
            var clusterY = document.getElementById('ynow_lab_cluster_y_label');
            if (clusterY && s.lab_cluster_y_label) clusterY.textContent = s.lab_cluster_y_label;
            var clusterFocus = document.getElementById('ynow_lab_cluster_focus_label');
            if (clusterFocus && s.lab_cluster_focus_label) clusterFocus.textContent = s.lab_cluster_focus_label;
            var clusterRun = document.getElementById('ynow_lab_cluster_run_label');
            if (clusterRun && s.btn_lab_cluster_run) {
              var clBusyMap = window.__ynowBtnBusy || {};
              clusterRun.textContent = clBusyMap['lab_cluster_run']
                ? (s.btn_lab_cluster_run_busy || s.btn_lab_cluster_run)
                : s.btn_lab_cluster_run;
            }
            var clusterMap = document.getElementById('ynow_lab_cluster_map_title');
            if (clusterMap && s.lab_cluster_map_title) clusterMap.textContent = s.lab_cluster_map_title;
            var clusterRadar = document.getElementById('ynow_lab_cluster_radar_title');
            if (clusterRadar && s.lab_cluster_radar_title) clusterRadar.textContent = s.lab_cluster_radar_title;
            var clusterTable = document.getElementById('ynow_lab_cluster_table_title');
            if (clusterTable && s.lab_cluster_table_title) clusterTable.textContent = s.lab_cluster_table_title;
            document.querySelectorAll('[data-i18n]').forEach(function (el) {
              var key = el.getAttribute('data-i18n');
              if (!key || s[key] == null || s[key] === '') return;
              if (el.children && el.children.length) return;
              el.textContent = s[key];
            });
            document.querySelectorAll('[data-i18n-ph]').forEach(function (el) {
              var key = el.getAttribute('data-i18n-ph');
              if (!key || s[key] == null || s[key] === '') return;
              var target = (el.matches && el.matches('input,textarea')) ? el : el.querySelector('input,textarea');
              if (target) target.setAttribute('placeholder', s[key]);
            });
            document.documentElement.setAttribute('lang', (payload && payload.locale) || 'en');
            var mkt = (payload && payload.market) ? String(payload.market) : 'US';
            document.body.classList.toggle('ynow-market-tw', mkt === 'TW');
            document.querySelectorAll('#ynow-market-header .ynow-mkt-btn').forEach(function (b) {
              b.classList.toggle('active', b.getAttribute('data-value') === mkt);
            });
            /* Keep __ynowLocaleUserPick sticky after the first manual click.
               Clearing it on server-confirm let a late queued `en` overwrite
               首頁／關於 after an early 繁中 apply had already succeeded. */
            try {
              document.documentElement.setAttribute('data-ynow-locale-ready', '1');
            } catch (eReady) {}
          }
          window.applyUiLocale = applyUiLocale;
          /* Preferred-locale first paint: apply slim boot map as soon as DOM exists
             so Chinese devices never flash English menus then switch. */
          (function ynowApplyBootPreferredLocale() {
            function run() {
              if (window.__ynowBootLocaleApplied) return;
              var pref = window.__ynowPreferredLocale || 'en';
              if (pref !== 'zh-TW') {
                window.__ynowBootLocaleApplied = true;
                try {
                  document.documentElement.setAttribute('data-ynow-locale-ready', '1');
                } catch (eReadyEn) {}
                return;
              }
              try {
                var bootRoot = window.__YNOW_UI_LOCALE_BOOT || {};
                var boot = bootRoot['zh-TW'] || bootRoot['zh-TW'];
                if (!boot || typeof applyUiLocale !== 'function') return;
                window.__ynowBootLocaleApplied = true;
                var mkt = (document.body && document.body.classList.contains('ynow-market-tw'))
                  ? 'TW' : 'US';
                applyUiLocale({
                  locale: boot.locale || 'zh-TW',
                  market: mkt,
                  strings: boot.strings || {},
                  tabs: boot.tabs || {},
                  boxes: boot.boxes || []
                }, {fromBoot: true});
              } catch (eBootPref) {}
            }
            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', run);
            } else {
              run();
            }
          })();

          function applyDcLocale(payload) {
            if (!payload) return;
            var hint = document.getElementById('ynow_dc_panel_hint');
            if (hint && payload.panel_hint) hint.textContent = payload.panel_hint;
            (payload.sections || []).forEach(function (sec) {
              var st = document.getElementById('ynow_dc_section_' + sec.id);
              if (st && sec.title) st.textContent = sec.title;
            });
            var items = payload.items || [];
            items.forEach(function (it) {
              var lab = document.getElementById('ynow_dc_label_' + it.id);
              if (lab && it.label) lab.textContent = it.label;
              var h = document.getElementById('ynow_dc_hint_' + it.id);
              if (h && it.hint) h.textContent = it.hint;
              var badge = document.getElementById('ynow_dc_badge_' + it.id);
              if (badge && payload.badge) badge.textContent = payload.badge;
              (it.conds || []).forEach(function (c) {
                var el = document.querySelector('label[for=\"' + c.input_id + '\"]');
                if (el && c.label) el.textContent = c.label;
                var ch = c.hint_id ? document.getElementById(c.hint_id) : null;
                if (ch && c.hint) ch.textContent = c.hint;
              });
            });
          }
          var __ynowLocaleResyncTimer = null;
          function requestLocaleResync(reason) {
            if (!(window.Shiny && Shiny.setInputValue)) return;
            // Debounce: each push ships a large strings payload; flooding delays applyUiLocale.
            if (__ynowLocaleResyncTimer) clearTimeout(__ynowLocaleResyncTimer);
            __ynowLocaleResyncTimer = setTimeout(function () {
              __ynowLocaleResyncTimer = null;
              Shiny.setInputValue('ynow_locale_resync', Date.now(), {priority: 'event'});
            }, 120);
          }
          window.ynowRequestLocaleResync = requestLocaleResync;
          function registerLocaleHandler() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerLocaleHandler, 50);
              return;
            }
            /* Shiny 1.14+ requires handler.length === 1. applyUiLocale(payload, opts)
               has arity 2 for click-time boot opts — register a unary wrapper so
               addCustomMessageHandler does not throw and abort this script (which
               would skip the Lite toggle IIFE below). */
            var addHandler = function (type, fn) {
              try {
                Shiny.addCustomMessageHandler(type, fn);
              } catch (eAdd) {
                /* Re-register on shiny:connected may hit already-added; keep first. */
              }
            };
            addHandler('ynowUiLocale', function (payload) {
              applyUiLocale(payload);
            });
            addHandler('ynowAuthLocale', function (payload) {
              if (!payload) return;
              var set = function (id, val) {
                var el = document.getElementById(id);
                if (el && val) el.textContent = val;
              };
              var setAll = function (sel, val) {
                if (!val) return;
                var nodes = document.querySelectorAll(sel);
                for (var i = 0; i < nodes.length; i++) nodes[i].textContent = val;
              };
              setAll('.ynow-auth-open-label', payload.sign_in);
              setAll('.ynow-auth-signout-label', payload.sign_out);
              setAll('.ynow-auth-register-label', payload.register_btn || payload.register);
              window.__ynowAuthCopy = payload;
              set('ynow_auth_header_label', payload.sign_in);
              set('ynow_auth_date_label', payload.sign_in);
              set('ynow_auth_home_label', payload.sign_in);
              set('ynow_auth_sign_out_label', payload.sign_out);
              set('ynow_auth_sign_out_date_label', payload.sign_out);
              set('ynow_auth_sign_out_home_label', payload.sign_out);
              set('ynow_auth_modal_title', payload.modal_title);
              set('ynow_auth_modal_help', payload.modal_help);
              set('ynow_auth_email_label', payload.email_label);
              set('ynow_auth_password_label', payload.password_label);
              set('ynow_auth_submit_label', payload.submit);
              set('ynow_auth_cancel_label', payload.cancel);
              set('ynow_auth_or_email', payload.or_email);
              set('ynow_auth_google_label', payload.google);
              set('ynow_auth_facebook_label', payload.facebook);
              set('ynow_auth_linkedin_label', payload.linkedin);
              set('ynow_auth_forgot_label', payload.forgot);
              set('ynow_auth_reset_help', payload.reset_help);
              set('ynow_auth_reset_password_label', payload.reset_password_label);
              set('ynow_auth_reset_password2_label', payload.reset_password2_label);
              set('ynow_auth_reset_submit_label', payload.reset_submit);
              set('ynow_auth_reset_back_label', payload.reset_back);
              set('ynow_auth_change_pw_title', payload.change_pw_title);
              set('ynow_auth_change_pw_help', payload.change_pw_help);
              set('ynow_auth_change_current_label', payload.change_pw_current);
              set('ynow_auth_change_new_label', payload.change_pw_new);
              set('ynow_auth_change_new2_label', payload.change_pw_confirm);
              set('ynow_auth_change_submit_label', payload.change_pw_submit);
              set('ynow_auth_register_label', payload.register_btn || payload.register);
              set('ynow_auth_register_header_label', payload.register_btn || payload.register);
              set('ynow_auth_register_home_label', payload.register_btn || payload.register);
              set('ynow_acct_gate_register_label', payload.register_btn || payload.register);
              set('ynow_auth_resend_label', payload.resend);
              set('ynow_menu_admin', payload.menu_admin);
              set('ynow_menu_admin_overview', payload.menu_admin_overview);
              set('ynow_menu_pts_ledger', payload.menu_pts_ledger);
              set('ynow_menu_settings', payload.menu_settings);
              set('ynow_settings_box_title', payload.settings_title);
              set('ynow_settings_intro', payload.settings_intro);
              set('ynow_settings_hide_locked_label', payload.settings_hide_locked);
              set('ynow_settings_hide_locked_help', payload.settings_hide_locked_help);
              set('ynow_settings_ui_locale_label', payload.settings_ui_locale);
              set('ynow_settings_market_label', payload.settings_market);
              set('ynow_settings_sidebar_compact_label', payload.settings_sidebar_compact);
              set('ynow_settings_sidebar_compact_help', payload.settings_sidebar_compact_help);
              set('ynow_settings_gain_loss_label', payload.settings_gain_loss);
              set('ynow_settings_gain_loss_help', payload.settings_gain_loss_help);
              set('ynow_settings_save_label', payload.settings_save);
              set('ynow_pts_topup_title', payload.pts_topup_title);
              set('ynow_pts_topup_copy', payload.pts_topup_copy);
              set('ynow_pts_topup_help', payload.pts_topup_help);
              set('ynow_pts_redeem_label', payload.pts_topup_redeem_label);
              set('ynow_pts_redeem_submit_label', payload.pts_topup_redeem_cta);
              set('ynow_pts_topup_cta_label', payload.pts_topup_cta);
              (function () {
                var inp = document.getElementById('ynow_pts_redeem_code');
                if (inp && payload.pts_topup_redeem_placeholder) {
                  inp.setAttribute('placeholder', payload.pts_topup_redeem_placeholder);
                }
              })();
              set('ynow_admin_box_title', payload.admin_title);
              set('ynow_admin_intro', payload.admin_intro);
              set('ynow_admin_delete_account_label', payload.delete_account);
              setAll('.ynow-auth-delete-account-label', payload.delete_account);
              set('ynow_auth_delete_modal_title', payload.delete_modal_title);
              set('ynow_auth_delete_modal_warn', payload.delete_modal_warn);
              set('ynow_auth_delete_modal_type', payload.delete_modal_type);
              set('ynow_auth_delete_confirm_label', payload.delete_modal_confirm);
              set('ynow_pts_box_title', payload.pts_box_title);
              set('ynow_pts_ledger_intro', payload.pts_ledger_intro);
              set('ynow_pa_gate_title', payload.pa_gate_title);
              set('ynow_pa_gate_signin_label', payload.sign_in);
              set('ynow_lab_gate_title', payload.pa_gate_title);
              set('ynow_lab_gate_signin_label', payload.sign_in);
              set('ynow_atx_gate_title', payload.pa_gate_title);
              set('ynow_atx_gate_signin_label', payload.sign_in);
              set('ynow_chk_gate_title', payload.pa_gate_title);
              set('ynow_chk_gate_signin_label', payload.sign_in);
              setAll('.ynow-auth-gate-title', payload.pa_gate_title);
              setAll('.ynow-gate-section-kicker', payload.gate_section_kicker || 'Member access');
              setAll('.ynow-member-gate-signin-label', payload.sign_in);
              var setFeature = function (feat, map) {
                if (!feat || !map) return;
                var roots = document.querySelectorAll('[data-ynow-gate-feature=\"' + feat + '\"]');
                for (var ri = 0; ri < roots.length; ri++) {
                  var root = roots[ri];
                  var applySel = function (sel, val) {
                    if (!val) return;
                    var nodes = root.querySelectorAll(sel);
                    for (var ni = 0; ni < nodes.length; ni++) nodes[ni].textContent = val;
                  };
                  applySel('.ynow-gate-body', map.body);
                  applySel('.ynow-gate-promo-kicker', map.kicker);
                  applySel('.ynow-gate-promo-lead', map.lead);
                  applySel('.ynow-gate-promo-b1', map.b1);
                  applySel('.ynow-gate-promo-b2', map.b2);
                  applySel('.ynow-gate-promo-b3', map.b3);
                  applySel('.ynow-gate-demo-caption', map.demo_caption);
                }
                // Body ids on member gates live outside data-feature children sometimes —
                // also set known gate body element ids.
                if (feat === 'lab') set('ynow_lab_gate_body', map.body);
                if (feat === 'checklist') set('ynow_chk_gate_body', map.body);
                if (feat === 'alerts') set('ynow_pa_gate_body', map.body);
                if (feat === 'atx') set('ynow_atx_gate_body', map.body);
                if (feat === 'snap_defaults') set('ynow_snapshot_defaults_locked_msg', map.body);
              };
              setFeature('lab', {
                body: payload.gate_lab_body, kicker: payload.gate_lab_kicker,
                lead: payload.gate_lab_lead, b1: payload.gate_lab_b1,
                b2: payload.gate_lab_b2, b3: payload.gate_lab_b3,
                demo_caption: payload.gate_lab_demo_caption
              });
              setFeature('checklist', {
                body: payload.gate_checklist_body, kicker: payload.gate_checklist_kicker,
                lead: payload.gate_checklist_lead, b1: payload.gate_checklist_b1,
                b2: payload.gate_checklist_b2, b3: payload.gate_checklist_b3,
                demo_caption: payload.gate_checklist_demo_caption
              });
              setFeature('alerts', {
                body: payload.gate_alerts_body, kicker: payload.gate_alerts_kicker,
                lead: payload.gate_alerts_lead, b1: payload.gate_alerts_b1,
                b2: payload.gate_alerts_b2, b3: payload.gate_alerts_b3,
                demo_caption: payload.gate_alerts_demo_caption
              });
              setFeature('pdf', {
                body: payload.gate_pdf_body, kicker: payload.gate_pdf_kicker,
                lead: payload.gate_pdf_lead, b1: payload.gate_pdf_b1,
                b2: payload.gate_pdf_b2, b3: payload.gate_pdf_b3,
                demo_caption: payload.gate_pdf_demo_caption
              });
              setFeature('snap_defaults', {
                body: payload.gate_snap_defaults_body, kicker: payload.gate_snap_defaults_kicker,
                lead: payload.gate_snap_defaults_lead, b1: payload.gate_snap_defaults_b1,
                b2: payload.gate_snap_defaults_b2, b3: payload.gate_snap_defaults_b3,
                demo_caption: payload.gate_snap_defaults_demo_caption
              });
              setFeature('inner_lock', {
                body: payload.gate_inner_lock_body, kicker: payload.gate_inner_lock_kicker,
                lead: payload.gate_inner_lock_lead, b1: payload.gate_inner_lock_b1,
                b2: payload.gate_inner_lock_b2, b3: payload.gate_inner_lock_b3,
                demo_caption: payload.gate_inner_lock_demo_caption
              });
              setFeature('atx', {
                body: payload.gate_atx_body, kicker: payload.gate_atx_kicker,
                lead: payload.gate_atx_lead, b1: payload.gate_atx_b1,
                b2: payload.gate_atx_b2, b3: payload.gate_atx_b3,
                demo_caption: payload.gate_atx_demo_caption
              });
              setAll('.ynow-gate-faq-title', payload.gate_faq_title);
              setAll('.ynow-gate-faq-q1', payload.gate_faq_q1);
              setAll('.ynow-gate-faq-a1', payload.gate_faq_a1);
              setAll('.ynow-gate-faq-q2', payload.gate_faq_q2);
              setAll('.ynow-gate-faq-a2', payload.gate_faq_a2);
              setAll('.ynow-gate-faq-q3', payload.gate_faq_q3);
              setAll('.ynow-gate-faq-a3', payload.gate_faq_a3);
              setAll('.ynow-gate-faq-q4', payload.gate_faq_q4);
              setAll('.ynow-gate-faq-a4', payload.gate_faq_a4);
            });
            window.__ynowPdfQuotaState = window.__ynowPdfQuotaState || {
              state: 'guest',
              left: null,
              limit: 3,
              cost: 10,
              remaining: '',
              rules: '',
              snap: ''
            };
            window.__ynowQuotaBadges = window.__ynowQuotaBadges || [];
            window.__ynowQuotaServerSkewMs = window.__ynowQuotaServerSkewMs || 0;
            window.ynowFmtCountdown = function (endsAtIso) {
              if (!endsAtIso) return '';
              var endMs = Date.parse(endsAtIso);
              if (!isFinite(endMs)) return '';
              var nowMs = Date.now() + (window.__ynowQuotaServerSkewMs || 0);
              var sec = Math.floor((endMs - nowMs) / 1000);
              if (sec <= 0) return '';
              var h = Math.floor(sec / 3600);
              var m = Math.floor((sec % 3600) / 60);
              var s = sec % 60;
              var pad = function (n) { return (n < 10 ? '0' : '') + String(n); };
              if (h > 0) return h + ':' + pad(m) + ':' + pad(s);
              return m + ':' + pad(s);
            };
            window.ynowSyncDownloadReportLabel = function () {
              var dl = document.getElementById('download_report');
              if (!dl) return;
              var s = window.__ynowUiLocaleStrings || {};
              var base = s.download_report || 'Download Report (PDF)';
              var st = window.__ynowPdfQuotaState || {};
              var liteOn = document.body && document.body.classList.contains('ynow-lite');
              var label = base;
              if (!liteOn && st.state !== 'lite' &&
                  st.left != null && st.limit != null &&
                  isFinite(Number(st.left)) && isFinite(Number(st.limit))) {
                var fmt = s.quota_count_fmt || '(%s/%s)';
                var countTxt = String(fmt)
                  .replace('%s', String(st.left))
                  .replace('%s', String(st.limit));
                label = base + ' ' + countTxt;
              }
              var icon = dl.querySelector(':scope > i, i');
              var iconHtml = '';
              if (icon && icon.parentNode === dl) {
                iconHtml = icon.outerHTML + ' ';
              } else if (icon) {
                iconHtml = icon.outerHTML + ' ';
              }
              var ttl = dl.querySelector(':scope > .ynow-ttl-countdown');
              var ttlHtml = ttl ? ttl.outerHTML : '';
              dl.innerHTML = iconHtml + label + ttlHtml;
              if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                window.ynowSyncSidebarDownloadIcon();
              }
            };
            window.ynowClearQuotaBadgeChrome = function () {
              document.querySelectorAll('.ynow-count-badge, .ynow-ttl-countdown').forEach(function (el) {
                /* Keep PDF label-embedded count; only remove Class C / maint nodes. */
                if (el.classList.contains('ynow-count-badge') &&
                    el.getAttribute('data-ynow-badge-src') === 'pdf') {
                  return;
                }
                if (el.parentNode) el.parentNode.removeChild(el);
              });
              document.querySelectorAll('.ynow-quota-badge-host').forEach(function (el) {
                el.classList.remove('ynow-quota-badge-host');
              });
            };
            window.ynowQuotaBadgeHosts = function (itemId) {
              var id = String(itemId || '');
              var hosts = [];
              if (id.indexOf('tab:') === 0) {
                var tab = id.slice(4);
                document.querySelectorAll('a[data-value]').forEach(function (a) {
                  if (a.getAttribute('data-value') === tab) hosts.push(a);
                });
              } else if (id.indexOf('report:') === 0) {
                var btn = document.getElementById(id.slice(7));
                if (btn) hosts.push(btn);
              } else if (id.indexOf('section:') === 0 || id.indexOf('output:') === 0) {
                document.querySelectorAll('[data-ynow-lock]').forEach(function (el) {
                  if (el.getAttribute('data-ynow-lock') === id) hosts.push(el);
                });
              } else if (id.indexOf('input:') === 0) {
                /* Retired catalog kind; keep DOM resolve for any stale client payload. */
                var inpLegacy = document.getElementById(id.slice(6));
                var fgLegacy = inpLegacy && (inpLegacy.closest('.form-group') ||
                  inpLegacy.closest('.shiny-input-container'));
                if (fgLegacy) hosts.push(fgLegacy);
              }
              return hosts;
            };
            window.ynowEnsureCountBadge = function (host, remaining, total, src) {
              if (!host || remaining == null || total == null) return;
              if (!isFinite(Number(remaining)) || !isFinite(Number(total))) return;
              var s = window.__ynowUiLocaleStrings || {};
              var fmt = s.quota_count_fmt || '(%s/%s)';
              var txt = String(fmt)
                .replace('%s', String(remaining))
                .replace('%s', String(total));
              /* Download Report PDF freemium already embeds count in the button label. */
              if (host.id === 'download_report' && src !== 'pdf') return;
              if (host.id === 'download_report' && src === 'pdf') {
                if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                  window.ynowSyncDownloadReportLabel();
                }
                return;
              }
              var badge = null;
              var kids = host.children || [];
              var bi;
              for (bi = 0; bi < kids.length; bi++) {
                if (kids[bi].classList && kids[bi].classList.contains('ynow-count-badge')) {
                  badge = kids[bi];
                  break;
                }
              }
              if (!badge) {
                badge = document.createElement('span');
                badge.className = 'ynow-count-badge';
                host.appendChild(badge);
              }
              badge.setAttribute('data-ynow-badge-src', src || 'acl');
              badge.textContent = ' ' + txt;
              host.classList.add('ynow-quota-badge-host');
            };
            window.ynowEnsureTtlCountdown = function (host, endsAtIso, src) {
              if (!host || !endsAtIso) return;
              var txt = window.ynowFmtCountdown(endsAtIso);
              var el = null;
              var kids = host.children || [];
              var i;
              for (i = 0; i < kids.length; i++) {
                if (kids[i].classList && kids[i].classList.contains('ynow-ttl-countdown')) {
                  el = kids[i];
                  break;
                }
              }
              if (!txt) {
                if (el && el.parentNode) el.parentNode.removeChild(el);
                return;
              }
              if (!el) {
                el = document.createElement('span');
                el.className = 'ynow-ttl-countdown';
                host.appendChild(el);
              }
              el.setAttribute('data-ends-at', endsAtIso);
              el.setAttribute('data-ynow-badge-src', src || 'acl');
              el.setAttribute('aria-live', 'off');
              el.textContent = txt;
              host.classList.add('ynow-quota-badge-host');
            };
            window.ynowApplyQuotaBadges = function () {
              if (typeof window.ynowClearQuotaBadgeChrome === 'function') {
                window.ynowClearQuotaBadgeChrome();
              }
              var badges = window.__ynowQuotaBadges || [];
              badges.forEach(function (row) {
                if (!row || !row.id) return;
                var hosts = window.ynowQuotaBadgeHosts(row.id) || [];
                hosts.forEach(function (host) {
                  if (row.remaining != null && row.total != null) {
                    window.ynowEnsureCountBadge(host, row.remaining, row.total, 'acl');
                  }
                  if (row.ends_at) {
                    window.ynowEnsureTtlCountdown(host, row.ends_at, 'acl');
                  }
                });
              });
              /* Maintenance windows: live countdown to window_end. */
              var maint = window.__ynowMaintEntries || [];
              maint.forEach(function (row) {
                if (!row || !row.id || !row.window_end) return;
                var hosts = window.ynowQuotaBadgeHosts(row.id) || [];
                hosts.forEach(function (host) {
                  window.ynowEnsureTtlCountdown(host, row.window_end, 'maint');
                });
              });
              if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                window.ynowSyncDownloadReportLabel();
              }
            };
            window.ynowTickQuotaCountdowns = function () {
              var expired = false;
              document.querySelectorAll('.ynow-ttl-countdown[data-ends-at]').forEach(function (el) {
                var ends = el.getAttribute('data-ends-at');
                var txt = window.ynowFmtCountdown(ends);
                if (!txt) {
                  expired = true;
                  if (el.parentNode) el.parentNode.removeChild(el);
                  return;
                }
                el.textContent = txt;
              });
              if (expired) {
                if (window.Shiny && Shiny.setInputValue) {
                  Shiny.setInputValue(
                    'ynow_quota_badge_refresh',
                    (window.__ynowQuotaBadgeRefreshClicks =
                      (window.__ynowQuotaBadgeRefreshClicks || 0) + 1),
                    { priority: 'event' }
                  );
                }
              }
            };
            if (!window.__ynowQuotaTickTimer) {
              window.__ynowQuotaTickTimer = setInterval(function () {
                if (typeof window.ynowTickQuotaCountdowns === 'function') {
                  window.ynowTickQuotaCountdowns();
                }
              }, 1000);
            }
            window.ynowApplyPdfQuotaCopy = function (localeBundle) {
              var st = window.__ynowPdfQuotaState || {};
              var s = localeBundle || window.__ynowUiLocaleStrings || {};
              var rem = document.getElementById('ynow_pdf_quota_remaining');
              var rules = document.getElementById('ynow_pdf_quota_rules');
              var snap = document.getElementById('ynow_snapshot_download_quota');
              var liteOn = document.body && document.body.classList.contains('ynow-lite');
              if (liteOn || st.state === 'lite') {
                if (rem) rem.textContent = s.pdf_quota_hint_lite || st.remaining || '';
                if (rules) rules.textContent = '';
                if (snap) {
                  snap.textContent = s.snapshot_download_quota_hint_lite || st.snap || '';
                }
                if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                  window.ynowSyncDownloadReportLabel();
                }
                return;
              }
              if (st.remaining) {
                if (rem) rem.textContent = st.remaining;
                if (rules) rules.textContent = st.rules || '';
                if (snap && st.snap) snap.textContent = st.snap;
                if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                  window.ynowSyncDownloadReportLabel();
                }
                return;
              }
              if (rem && s.pdf_quota_hint) rem.textContent = s.pdf_quota_hint;
              if (rules && s.pdf_quota_rules_fmt) {
                var cost = (st.cost != null) ? String(st.cost) : '10';
                rules.textContent = String(s.pdf_quota_rules_fmt).replace('%s', cost);
              }
              if (snap && s.snapshot_download_quota_hint) {
                snap.textContent = s.snapshot_download_quota_hint;
              }
              if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                window.ynowSyncDownloadReportLabel();
              }
            };
            addHandler('ynowPdfQuota', function (payload) {
              if (!payload) return;
              window.__ynowPdfQuotaState = {
                state: payload.state || 'guest',
                left: payload.left,
                limit: payload.limit,
                cost: payload.cost,
                remaining: payload.remaining || '',
                rules: payload.rules || '',
                snap: payload.snap || ''
              };
              var rem = document.getElementById('ynow_pdf_quota_remaining');
              var rules = document.getElementById('ynow_pdf_quota_rules');
              var snap = document.getElementById('ynow_snapshot_download_quota');
              if (rem) rem.textContent = payload.remaining || '';
              if (rules) rules.textContent = payload.rules || '';
              if (snap && payload.snap != null) snap.textContent = payload.snap || '';
              if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                window.ynowSyncDownloadReportLabel();
              }
            });
            addHandler('ynowPtsRedeemStatus', function (payload) {
              var el = document.getElementById('ynow_pts_redeem_status');
              if (!el || !payload) return;
              el.textContent = payload.text || '';
              if (payload.ok === true) el.setAttribute('data-ok', '1');
              else if (payload.ok === false) el.setAttribute('data-ok', '0');
              else el.setAttribute('data-ok', '');
            });
            addHandler('ynowPtsBanner', function (payload) {
              if (!payload) return;
              var pairs = [
                {
                  fynLab: 'ynow_pts_banner_fyn_label',
                  pynLab: 'ynow_pts_banner_pyn_label',
                  fynVal: 'ynow_pts_banner_fyn_value',
                  pynVal: 'ynow_pts_banner_pyn_value',
                  wrap: 'ynow-pts-header',
                  banner: 'ynow_pts_banner'
                },
                {
                  fynLab: 'ynow_pts_sidebar_fyn_label',
                  pynLab: 'ynow_pts_sidebar_pyn_label',
                  fynVal: 'ynow_pts_sidebar_fyn_value',
                  pynVal: 'ynow_pts_sidebar_pyn_value',
                  wrap: 'ynow-pts-sidebar',
                  banner: 'ynow_pts_sidebar_banner'
                }
              ];
              var i, el, wrap, banner;
              /* Guests: hide + clear DOM (do not map empty to em-dash placeholders). */
              if (payload.visible === false) {
                for (i = 0; i < pairs.length; i++) {
                  el = document.getElementById(pairs[i].fynLab); if (el) el.textContent = '';
                  el = document.getElementById(pairs[i].pynLab); if (el) el.textContent = '';
                  el = document.getElementById(pairs[i].fynVal); if (el) el.textContent = '';
                  el = document.getElementById(pairs[i].pynVal); if (el) el.textContent = '';
                  wrap = document.getElementById(pairs[i].wrap);
                  if (wrap) wrap.classList.add('ynow-pts-hidden');
                  banner = document.getElementById(pairs[i].banner);
                  if (banner) banner.setAttribute('aria-hidden', 'true');
                }
                return;
              }
              for (i = 0; i < pairs.length; i++) {
                el = document.getElementById(pairs[i].fynLab);
                if (el && payload.fyn_label) el.textContent = payload.fyn_label;
                el = document.getElementById(pairs[i].pynLab);
                if (el && payload.pyn_label) el.textContent = payload.pyn_label;
                el = document.getElementById(pairs[i].fynVal);
                if (el) el.textContent = (payload.fyn != null && payload.fyn !== '') ? String(payload.fyn) : '—';
                el = document.getElementById(pairs[i].pynVal);
                if (el) el.textContent = (payload.pyn != null && payload.pyn !== '') ? String(payload.pyn) : '—';
                wrap = document.getElementById(pairs[i].wrap);
                if (wrap) wrap.classList.remove('ynow-pts-hidden');
                banner = document.getElementById(pairs[i].banner);
                if (banner) banner.setAttribute('aria-hidden', 'false');
              }
            });
            /* Apply signed-in chrome without mutating real auth (view-as preview). */
            window.ynowApplySessionLikeAuthChrome = function (on, labels) {
              if (!document.body) return;
              var openLinks = document.querySelectorAll('.ynow-auth-open');
              var regLinks = document.querySelectorAll('.ynow-auth-register-open');
              var outLinks = document.querySelectorAll('.ynow-auth-signout');
              var i;
              for (i = 0; i < openLinks.length; i++) {
                openLinks[i].style.display = on ? 'none' : '';
              }
              for (i = 0; i < regLinks.length; i++) {
                regLinks[i].style.display = on ? 'none' : '';
              }
              for (i = 0; i < outLinks.length; i++) {
                outLinks[i].style.display = on ? '' : 'none';
              }
              if (labels && labels.label_sign_out) {
                var outLbls = document.querySelectorAll('.ynow-auth-signout-label');
                for (i = 0; i < outLbls.length; i++) outLbls[i].textContent = labels.label_sign_out;
              }
              if (typeof window.ynowSyncSysAdminFootVisibility === 'function') {
                window.ynowSyncSysAdminFootVisibility(!!on);
              }
              if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                window.ynowSyncSidebarDownloadIcon();
              }
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                window.ynowSyncGuestSidebarLocked();
              }
            };
            addHandler('ynowViewAsMode', function (payload) {
              if (!payload) return;
              var on = !!payload.active;
              if (on) {
                document.body.classList.add('ynow-view-as');
                /* Session-like Current App: unlock guest chrome as if this user is signed in. */
                document.body.classList.add('ynow-authed');
                window.__ynowViewAsSessionLike = true;
                window.ynowApplySessionLikeAuthChrome(true, {
                  label_sign_out: payload.label_sign_out || ''
                });
              } else {
                document.body.classList.remove('ynow-view-as');
                window.__ynowViewAsSessionLike = false;
                /* Real AuthState restore is pushed by server; clear only if still fake. */
                if (!window.__ynowRealAuthed) {
                  document.body.classList.remove('ynow-authed');
                  window.ynowApplySessionLikeAuthChrome(false, null);
                } else {
                  window.ynowApplySessionLikeAuthChrome(true, null);
                }
              }
              var pidEl = document.getElementById('ynow_viewas_chrome_pid');
              if (pidEl) pidEl.textContent = on && payload.public_id ? String(payload.public_id) : '';
              var actEl = document.getElementById('ynow_viewas_chrome_active_label');
              if (actEl && payload.active_label) actEl.textContent = String(payload.active_label);
              var hintEl = document.getElementById('ynow_viewas_chrome_hint');
              if (hintEl && payload.hint) hintEl.textContent = String(payload.hint);
              var exitLab = document.getElementById('ynow_viewas_chrome_exit_label');
              if (exitLab && payload.exit_label) exitLab.textContent = String(payload.exit_label);
            });
            addHandler('ynowSysadminMode', function (payload) {
              if (!payload) return;
              if (payload.on) {
                document.body.classList.add('ynow-sysadmin-on');
                /* Lite omits snap_defaults entirely — do not force that locked tab. */
              } else {
                document.body.classList.remove('ynow-sysadmin-on');
                /* Leaving portal also clears self-view chrome classes. */
                document.body.classList.remove('ynow-self-view', 'ynow-omniscient',
                  'ynow-self-view-guest', 'ynow-self-view-signed-in');
                window.__ynowSelfViewMode = '';
                try {
                  var activeSnap = document.querySelector('#snapshot_report .nav-tabs li.active a');
                  var onDefaults = activeSnap &&
                    activeSnap.getAttribute('data-value') === 'snap_defaults';
                  /* Leaving sysadmin: leave APP_DEFAULTS; Lite uses free Snapshot tabs. */
                  if (onDefaults) {
                    var fallback = document.querySelector('#snapshot_report a[data-value=\"snap_current\"]') ||
                      document.querySelector('#snapshot_report a[data-value=\"snap_audit\"]');
                    if (fallback) {
                      if (window.jQuery) window.jQuery(fallback).tab('show');
                      else if (typeof fallback.click === 'function') fallback.click();
                    }
                  }
                } catch (eOff) {}
              }
            });
            window.__ynowSelfViewMode = window.__ynowSelfViewMode || '';
            addHandler('ynowSelfViewMode', function (payload) {
              if (!payload) return;
              var mode = String(payload.mode || '').toLowerCase();
              var on = mode === 'guest' || mode === 'signed_in' || mode === 'omniscient';
              document.body.classList.remove(
                'ynow-self-view', 'ynow-omniscient',
                'ynow-self-view-guest', 'ynow-self-view-signed-in'
              );
              window.__ynowSelfViewMode = on ? mode : '';
              if (on) {
                document.body.classList.add('ynow-self-view');
                if (mode === 'guest') {
                  document.body.classList.add('ynow-self-view-guest');
                  document.body.classList.remove('ynow-authed');
                  window.__ynowSelfViewSessionLike = false;
                  window.ynowApplySessionLikeAuthChrome(false, null);
                } else if (mode === 'signed_in') {
                  document.body.classList.add('ynow-self-view-signed-in');
                  document.body.classList.add('ynow-authed');
                  window.__ynowSelfViewSessionLike = true;
                  window.ynowApplySessionLikeAuthChrome(true, {
                    label_sign_out: payload.label_sign_out || ''
                  });
                } else if (mode === 'omniscient') {
                  document.body.classList.add('ynow-omniscient');
                  document.body.classList.add('ynow-authed');
                  window.__ynowSelfViewSessionLike = true;
                  window.ynowApplySessionLikeAuthChrome(true, {
                    label_sign_out: payload.label_sign_out || ''
                  });
                }
              } else {
                window.__ynowSelfViewSessionLike = false;
                /* Restore real auth chrome unless view-as is still faking it. */
                if (window.__ynowViewAsSessionLike) {
                  document.body.classList.add('ynow-authed');
                  window.ynowApplySessionLikeAuthChrome(true, null);
                } else if (!window.__ynowRealAuthed) {
                  document.body.classList.remove('ynow-authed');
                  window.ynowApplySessionLikeAuthChrome(false, null);
                } else {
                  document.body.classList.add('ynow-authed');
                  window.ynowApplySessionLikeAuthChrome(true, null);
                }
              }
              if (payload.exit_label) {
                var exitLab = document.getElementById('ynow_pts_sysadmin_self_exit_label');
                if (exitLab) exitLab.textContent = String(payload.exit_label);
              }
              if (typeof window.ynowApplyAccessLocks === 'function') {
                try { window.ynowApplyAccessLocks(); } catch (eAl) {}
              }
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                try { window.ynowSyncGuestSidebarLocked(); } catch (eGs) {}
              }
            });
            function ynowEnsureAtxLockPromo(pane) {
              if (!pane) return;
              var kids = pane.children || [];
              for (var hi = 0; hi < kids.length; hi++) {
                if (kids[hi].classList && kids[hi].classList.contains('ynow-atx-lock-host')) return;
              }
              var tpl = document.getElementById('ynow_atx_lock_promo_tpl');
              if (!tpl) return;
              var host = tpl.cloneNode(true);
              host.removeAttribute('id');
              host.removeAttribute('hidden');
              host.classList.remove('ynow-atx-lock-tpl');
              host.classList.add('ynow-atx-lock-host');
              pane.insertBefore(host, pane.firstChild);
            }
            function ynowEnsureSidebarTabLockSurface(pane) {
              if (!pane) return;
              var kids = pane.children || [];
              for (var hi = 0; hi < kids.length; hi++) {
                if (kids[hi].classList && (
                  kids[hi].classList.contains('ynow-tab-lock-host') ||
                  kids[hi].classList.contains('ynow-atx-lock-host')
                )) return;
              }
              var tpl = document.getElementById('ynow_inner_tab_lock_promo_tpl');
              if (!tpl) return;
              var promo = tpl.cloneNode(true);
              promo.removeAttribute('id');
              promo.removeAttribute('hidden');
              promo.classList.remove('ynow-inner-tab-lock-tpl');
              promo.classList.add('ynow-hfv-chapter');
              promo.classList.add('ynow-member-gate');
              promo.classList.add('ynow-gate-box');
              promo.classList.add('ynow-member-gate--surface-overlay');
              var fade = document.createElement('div');
              fade.className = 'ynow-member-gate-fade-backdrop';
              fade.setAttribute('aria-hidden', 'true');
              var surface = document.createElement('div');
              surface.className = 'ynow-member-gate-surface ynow-member-gate-surface--checklist ynow-member-gate-surface--locked ynow-tab-lock-host';
              surface.appendChild(fade);
              surface.appendChild(promo);
              pane.insertBefore(surface, pane.firstChild);
            }
            function ynowEnsureInnerLockPromo(pane) {
              if (!pane) return;
              var kids = pane.children || [];
              for (var hi = 0; hi < kids.length; hi++) {
                if (kids[hi].classList && kids[hi].classList.contains('ynow-inner-tab-lock-host')) return;
              }
              var tpl = document.getElementById('ynow_inner_tab_lock_promo_tpl');
              if (!tpl) return;
              var host = tpl.cloneNode(true);
              host.removeAttribute('id');
              host.removeAttribute('hidden');
              host.classList.remove('ynow-inner-tab-lock-tpl');
              host.classList.add('ynow-inner-tab-lock-host');
              pane.insertBefore(host, pane.firstChild);
            }
            /* Lock target is a block, input, or report inside a tab — not a whole sidebar page. */
            function ynowMarkLockedInnerTarget(el) {
              if (!el || !el.closest) return;
              var pane = el.closest('.tab-pane');
              if (!pane || !pane.id) return;
              var value = pane.id.indexOf('shiny-tab-') === 0 ? pane.id.slice(10) : '';
              var anchor = null;
              if (value) {
                var links = document.querySelectorAll('a[data-value]');
                for (var i = 0; i < links.length; i++) {
                  if (links[i].getAttribute('data-value') === value) { anchor = links[i]; break; }
                }
              }
              if (!anchor) anchor = document.querySelector('a[href=\"#' + pane.id + '\"]');
              if (!anchor) return;
              var li = anchor.closest ? anchor.closest('li') : null;
              if (!li) return;
              var inSidebar = !!(anchor.closest && anchor.closest('.sidebar-menu'));
              if (inSidebar) {
                li.classList.add('ynow-access-locked-nav');
                li.classList.add('ynow-sidebar-locked');
              } else {
                li.classList.add('ynow-inner-tab-locked');
                if (!anchor.querySelector('.ynow-inner-tab-lock')) {
                  var lock = document.createElement('i');
                  lock.className = 'fas fa-lock ynow-menu-lock ynow-inner-tab-lock';
                  lock.setAttribute('aria-hidden', 'true');
                  anchor.insertBefore(lock, anchor.firstChild);
                }
              }
              pane.classList.add('ynow-inner-locked-pane');
              ynowEnsureInnerLockPromo(pane);
            }
            window.ynowApplyAccessLocks = function () {
              var entries = window.__ynowAccessEntries || [];
              var note = window.__ynowAccessLockNote || '';
              /* Omniscient self-view: lift every ACL lock/hide (entries ignored). */
              var omniscient = !!(document.body && document.body.classList.contains('ynow-omniscient'));
              if (omniscient) entries = [];
              document.querySelectorAll('i.ynow-access-lock, i.ynow-inner-tab-lock').forEach(function (el) {
                if (el.parentNode) el.parentNode.removeChild(el);
              });
              document.querySelectorAll('.ynow-inner-tab-lock-host, .ynow-atx-lock-host, .ynow-tab-lock-host').forEach(function (el) {
                if (el.parentNode) el.parentNode.removeChild(el);
              });
              var clearSel = [
                '.ynow-access-hidden',
                '.ynow-access-locked-nav',
                '.ynow-sidebar-locked',
                '.ynow-access-locked-input',
                '.ynow-access-locked-section',
                '.ynow-access-locked-btn',
                '.ynow-access-locked-pane',
                '.ynow-access-hidden-pane',
                '.ynow-access-locked',
                '.ynow-inner-tab-locked',
                '.ynow-inner-locked-pane',
                '.ynow-atx-locked-pane'
              ].join(',');
              document.querySelectorAll(clearSel).forEach(function (el) {
                el.classList.remove(
                  'ynow-access-hidden',
                  'ynow-access-locked-nav',
                  'ynow-sidebar-locked',
                  'ynow-access-locked-input',
                  'ynow-access-locked-section',
                  'ynow-access-locked-btn',
                  'ynow-access-locked-pane',
                  'ynow-access-hidden-pane',
                  'ynow-access-locked',
                  'ynow-inner-tab-locked',
                  'ynow-inner-locked-pane',
                  'ynow-atx-locked-pane'
                );
                el.removeAttribute('data-lock-note');
              });
              if (omniscient) {
                if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                  window.ynowSyncGuestSidebarLocked();
                }
                return;
              }
              entries.forEach(function (row) {
                if (!row || !row.id) return;
                var id = String(row.id);
                var effect = String(row.effect || 'hide').toLowerCase();
                if (effect !== 'lock') effect = 'hide';
                if (id.indexOf('tab:') === 0) {
                  var tab = id.slice(4);
                  document.querySelectorAll('a[data-value]').forEach(function (a) {
                    if (a.getAttribute('data-value') !== tab) return;
                    var li = a.closest('li');
                    if (!li) return;
                    if (effect === 'hide') li.classList.add('ynow-access-hidden');
                    else {
                      li.classList.add('ynow-access-locked-nav');
                      li.classList.add('ynow-sidebar-locked');
                    }
                  });
                  var pane = document.getElementById('shiny-tab-' + tab);
                  if (pane) {
                    if (effect === 'hide') {
                      pane.classList.add('ynow-access-hidden-pane');
                    } else {
                      pane.classList.add('ynow-access-locked-pane');
                      pane.setAttribute('data-lock-note', note);
                      if (tab === 'asset_transmission') {
                        pane.classList.add('ynow-atx-locked-pane');
                        ynowEnsureAtxLockPromo(pane);
                      } else if (typeof ynowEnsureSidebarTabLockSurface === 'function') {
                        ynowEnsureSidebarTabLockSurface(pane);
                      }
                    }
                  }
                } else if (id.indexOf('report:') === 0) {
                  var btn = document.getElementById(id.slice(7));
                  if (!btn) return;
                  if (effect === 'hide') btn.classList.add('ynow-access-hidden');
                  else {
                    btn.classList.add('ynow-access-locked-btn');
                    btn.setAttribute('title', note);
                    ynowMarkLockedInnerTarget(btn);
                  }
                } else if (id.indexOf('section:') === 0 || id.indexOf('output:') === 0) {
                  document.querySelectorAll('[data-ynow-lock]').forEach(function (el) {
                    if (el.getAttribute('data-ynow-lock') !== id) return;
                    if (effect === 'hide') el.classList.add('ynow-access-hidden');
                    else {
                      el.classList.add('ynow-access-locked-section');
                      el.setAttribute('data-lock-note', note);
                      ynowMarkLockedInnerTarget(el);
                    }
                  });
                } else if (id.indexOf('input:') === 0) {
                  /* Retired; stale payloads only. */
                  var inp = document.getElementById(id.slice(6));
                  var fg = inp && (inp.closest('.form-group') || inp.closest('.shiny-input-container'));
                  if (!fg) return;
                  if (effect === 'hide') fg.classList.add('ynow-access-hidden');
                  else {
                    fg.classList.add('ynow-access-locked-input');
                    fg.setAttribute('data-lock-note', note);
                    ynowMarkLockedInnerTarget(fg);
                  }
                }
              });
            document.querySelectorAll('.sidebar-menu li.ynow-access-locked-nav > a').forEach(function (a) {
              if (a.querySelector('.ynow-menu-lock')) {
                /* Ensure unified chrome class even when guest lock glyph already exists. */
                var li0 = a.closest ? a.closest('li') : a.parentElement;
                if (li0 && li0.classList) li0.classList.add('ynow-sidebar-locked');
                return;
              }
              var lock = document.createElement('i');
              lock.className = 'fas fa-lock ynow-menu-lock ynow-access-lock';
              lock.setAttribute('aria-hidden', 'true');
              var glyph = a.querySelector(':scope > i:not(.ynow-menu-lock):not(.pull-right)');
              if (glyph) glyph.insertAdjacentElement('afterend', lock);
              else a.insertBefore(lock, a.firstChild);
            });
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                window.ynowSyncGuestSidebarLocked();
              }
            };
            /* Full guest locks share .ynow-sidebar-locked with ACL + persist (B) locks.
               After sign-in: unlock-after-login (A) tabs drop chrome; still-locked
               (ACL / persist B) keep guest-identical lock icon + gray — never hide. */
            window.ynowEnsureSidebarLockGlyph = function (a) {
              if (!a || a.querySelector('.ynow-menu-lock')) return;
              var lock = document.createElement('i');
              lock.className = 'fas fa-lock ynow-menu-lock';
              lock.setAttribute('aria-hidden', 'true');
              var glyph = a.querySelector(':scope > i:not(.ynow-menu-lock):not(.pull-right)');
              if (glyph) glyph.insertAdjacentElement('afterend', lock);
              else a.insertBefore(lock, a.firstChild);
            };
            window.ynowSyncGuestSidebarLocked = function () {
              var tabs = window.__YNOW_SIDEBAR_GUEST_LOCK_TABS || {
                lab_notes: true,
                decision_checklist: true,
                mos_assumptions: true,
                price_alerts: true,
                asset_transmission: true
              };
              /* Tier B / points-gated sidebar tabs: keep locked chrome after auth. */
              var persist = window.__YNOW_SIDEBAR_PERSIST_LOCK_TABS || {};
              var omniscient = !!(document.body && document.body.classList.contains('ynow-omniscient'));
              /* View-as / signed-in self-view / omniscient are signed-in previews — not guest-locked. */
              var guest = !!(document.body &&
                document.body.classList.contains('ynow-auth-feature') &&
                !document.body.classList.contains('ynow-lite') &&
                !document.body.classList.contains('ynow-authed') &&
                !document.body.classList.contains('ynow-view-as') &&
                !document.body.classList.contains('ynow-omniscient'));
              document.querySelectorAll('.sidebar-menu a[data-value]').forEach(function (a) {
                var tab = a.getAttribute('data-value') || '';
                var li = a.closest ? a.closest('li') : a.parentElement;
                if (!li || !li.classList) return;
                if (omniscient) {
                  /* Lift guest + persist B chrome; ACL already cleared by applyAccessLocks. */
                  li.classList.remove('ynow-sidebar-locked');
                  var glyph = a.querySelector('.ynow-menu-lock:not(.ynow-access-lock)');
                  if (glyph && glyph.parentNode && !li.classList.contains('ynow-access-locked-nav')) {
                    /* Keep ACL lock glyph only if somehow still marked; otherwise drop freemium glyph. */
                  }
                  if (tabs[tab] || persist[tab]) {
                    var g0 = a.querySelector(':scope > .ynow-menu-lock');
                    if (g0 && g0.parentNode) g0.parentNode.removeChild(g0);
                  }
                  return;
                }
                /* Still-locked after auth: ACL + persist B — same chrome as guest. */
                if (li.classList.contains('ynow-access-locked-nav') || persist[tab]) {
                  li.classList.add('ynow-sidebar-locked');
                  li.classList.remove('ynow-access-hidden');
                  if (typeof window.ynowEnsureSidebarLockGlyph === 'function') {
                    window.ynowEnsureSidebarLockGlyph(a);
                  }
                  return;
                }
                if (guest && tabs[tab]) {
                  li.classList.add('ynow-sidebar-locked');
                  if (typeof window.ynowEnsureSidebarLockGlyph === 'function') {
                    window.ynowEnsureSidebarLockGlyph(a);
                  }
                } else if (tabs[tab]) {
                  /* A unlock-after-login: drop locked chrome only (item stays visible). */
                  li.classList.remove('ynow-sidebar-locked');
                }
              });
              if (typeof window.ynowSyncSidebarPinLocks === 'function') {
                try { window.ynowSyncSidebarPinLocks(); } catch (ePinLock) {}
              }
            };
            addHandler('ynowAccessLocks', function (payload) {
              if (!payload) return;
              var entries = payload.entries;
              if (!entries || !entries.length) {
                /* Backward compat: plain ids list → hide */
                entries = (payload.ids || []).map(function (id) {
                  return { id: id, effect: 'hide' };
                });
              }
              window.__ynowAccessEntries = entries;
              window.__ynowAccessLocks = (payload.ids || entries.map(function (e) {
                return e && e.id;
              })).filter(Boolean);
              window.__ynowAccessLockNote = payload.note || '';
              window.__ynowQuotaBadges = payload.badges || [];
              if (payload.server_now) {
                var sn = Date.parse(payload.server_now);
                if (isFinite(sn)) window.__ynowQuotaServerSkewMs = sn - Date.now();
              }
              if (window.ynowApplyAccessLocks) window.ynowApplyAccessLocks();
              if (window.ynowApplyMaintenance) window.ynowApplyMaintenance();
              if (window.ynowApplyQuotaBadges) window.ynowApplyQuotaBadges();
            });
            /* System admin maintenance: separate from ACL / guest locks.
               Menu items are non-clickable; content shows 維護中 / Under maintenance. */
            window.ynowApplyMaintenance = function () {
              var entries = window.__ynowMaintEntries || [];
              var clearSel = [
                '.ynow-maintenance-nav',
                '.ynow-maintenance-pane',
                '.ynow-maintenance-item'
              ].join(',');
              document.querySelectorAll(clearSel).forEach(function (el) {
                el.classList.remove(
                  'ynow-maintenance-nav',
                  'ynow-maintenance-pane',
                  'ynow-maintenance-item'
                );
                el.removeAttribute('data-maint-note');
                el.removeAttribute('aria-disabled');
                el.removeAttribute('tabindex');
              });
              entries.forEach(function (row) {
                if (!row || !row.id) return;
                var id = String(row.id);
                var note = String(row.note || '');
                if (id.indexOf('tab:') === 0) {
                  var tab = id.slice(4);
                  document.querySelectorAll('a[data-value]').forEach(function (a) {
                    if (a.getAttribute('data-value') !== tab) return;
                    var li = a.closest ? a.closest('li') : a.parentElement;
                    if (!li || !li.classList) return;
                    li.classList.add('ynow-maintenance-nav');
                    a.setAttribute('aria-disabled', 'true');
                    a.setAttribute('tabindex', '-1');
                    if (note) li.setAttribute('data-maint-note', note);
                  });
                  var pane = document.getElementById('shiny-tab-' + tab);
                  if (pane) {
                    pane.classList.add('ynow-maintenance-pane');
                    if (note) pane.setAttribute('data-maint-note', note);
                  }
                } else if (id.indexOf('report:') === 0) {
                  var btn = document.getElementById(id.slice(7));
                  if (!btn) return;
                  btn.classList.add('ynow-maintenance-item');
                  if (note) btn.setAttribute('title', note.replace(/\\n/g, ' '));
                  if (note) btn.setAttribute('data-maint-note', note);
                } else if (id.indexOf('section:') === 0 || id.indexOf('output:') === 0) {
                  document.querySelectorAll('[data-ynow-lock]').forEach(function (el) {
                    if (el.getAttribute('data-ynow-lock') !== id) return;
                    el.classList.add('ynow-maintenance-item');
                    if (note) el.setAttribute('data-maint-note', note);
                  });
                } else if (id.indexOf('input:') === 0) {
                  var inp = document.getElementById(id.slice(6));
                  var fg = inp && (inp.closest('.form-group') || inp.closest('.shiny-input-container'));
                  if (!fg) return;
                  fg.classList.add('ynow-maintenance-item');
                  if (note) fg.setAttribute('data-maint-note', note);
                }
              });
            };
            window.__ynowMaintRefreshTimer = null;
            window.ynowScheduleMaintRefresh = function (nextEndIso) {
              if (window.__ynowMaintRefreshTimer) {
                try { clearTimeout(window.__ynowMaintRefreshTimer); } catch (eClr) {}
                window.__ynowMaintRefreshTimer = null;
              }
              if (!nextEndIso) return;
              var endMs = Date.parse(nextEndIso);
              if (!isFinite(endMs)) return;
              var delay = Math.max(500, endMs - Date.now() + 400);
              /* Cap client wait; server invalidateLater also pushes. */
              if (delay > 120000) delay = 120000;
              window.__ynowMaintRefreshTimer = setTimeout(function () {
                window.__ynowMaintRefreshTimer = null;
                if (window.Shiny && Shiny.setInputValue) {
                  Shiny.setInputValue(
                    'ynow_maint_refresh',
                    (window.__ynowMaintRefreshClicks = (window.__ynowMaintRefreshClicks || 0) + 1),
                    { priority: 'event' }
                  );
                }
              }, delay);
            };
            addHandler('ynowMaintenance', function (payload) {
              if (!payload) payload = {};
              var entries = payload.entries || [];
              window.__ynowMaintEntries = entries;
              window.__ynowMaintIds = (payload.ids || entries.map(function (e) {
                return e && e.id;
              })).filter(Boolean);
              if (payload.server_now) {
                var sn2 = Date.parse(payload.server_now);
                if (isFinite(sn2)) window.__ynowQuotaServerSkewMs = sn2 - Date.now();
              }
              if (window.ynowApplyMaintenance) window.ynowApplyMaintenance();
              if (window.ynowApplyQuotaBadges) window.ynowApplyQuotaBadges();
              if (window.ynowScheduleMaintRefresh) {
                window.ynowScheduleMaintRefresh(payload.next_end || null);
              }
            });
            /* Hard block navigation clicks on maintenance menu items (capture). */
            if (!document.documentElement.getAttribute('data-ynow-maint-click-guard')) {
              document.documentElement.setAttribute('data-ynow-maint-click-guard', '1');
              document.addEventListener('click', function (ev) {
                var a = ev.target && ev.target.closest ? ev.target.closest('a') : null;
                if (!a) return;
                var li = a.closest ? a.closest('li.ynow-maintenance-nav') : null;
                if (!li) return;
                try { ev.preventDefault(); } catch (ePd) {}
                try { ev.stopPropagation(); } catch (eSp) {}
                return false;
              }, true);
            }
            document.addEventListener('shiny:inputchanged', function () {
              if (window.ynowApplyAccessLocks) window.ynowApplyAccessLocks();
              if (window.ynowApplyMaintenance) window.ynowApplyMaintenance();
            });
            document.addEventListener('shiny:value', function () {
              if (window.ynowApplyAccessLocks) window.ynowApplyAccessLocks();
              if (window.ynowApplyMaintenance) window.ynowApplyMaintenance();
            });
            (function bindViewAsChromeExit() {
              var btn = document.getElementById('ynow_viewas_chrome_exit');
              if (!btn || btn.__ynowBound) return;
              btn.__ynowBound = true;
              btn.addEventListener('click', function () {
                if (!window.Shiny || !Shiny.setInputValue) return;
                Shiny.setInputValue(
                  'ynow_pts_sysadmin_viewas_exit',
                  (window.__ynowViewAsExitClicks = (window.__ynowViewAsExitClicks || 0) + 1),
                  { priority: 'event' }
                );
              });
            })();
            /* Exit self view is a native actionButton on the control-panel bar. */
            addHandler('ynowAuthFeature', function (payload) {
              if (!payload) return;
              if (payload.on) document.body.classList.add('ynow-auth-feature');
              else document.body.classList.remove('ynow-auth-feature');
              if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                window.ynowSyncSidebarDownloadIcon();
              }
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                window.ynowSyncGuestSidebarLocked();
              }
            });
            /* Forgot-password recovery return: swap modal to set-new-password panel. */
            addHandler('ynowAuthResetMode', function (payload) {
              var modal = document.getElementById('ynow_auth_modal');
              if (!modal) return;
              var on = !!(payload && payload.on);
              if (on) modal.classList.add('ynow-auth-mode-reset');
              else modal.classList.remove('ynow-auth-mode-reset');
              var copy = window.__ynowAuthCopy || {};
              var title = document.getElementById('ynow_auth_modal_title');
              if (title) {
                if (on && copy.modal_title_reset) title.textContent = copy.modal_title_reset;
                else if (!on && copy.modal_title) title.textContent = copy.modal_title;
              }
            });
            /* Immediate social OAuth: PKCE + location.assign (shared by custom
               message + client click capture so the popup/redirect feels instant). */
            window.ynowStartOAuthRedirect = function (payload) {
              if (!payload || !payload.url) return false;
              var go = function (url) {
                try { window.location.assign(String(url)); }
                catch (eGo) { window.location.href = String(url); }
              };
              var tabEl = document.querySelector('.sidebar-menu li.active > a[data-value]');
              var tab = (payload.restore_tab && String(payload.restore_tab)) ||
                (tabEl ? (tabEl.getAttribute('data-value') || '') : '');
              var baseUrl = String(payload.url);
              var finish = function (url) { go(url); };
              try {
                if (tab) sessionStorage.setItem('ynow_pre_oauth_tab', tab);
                var verifier = (typeof window.ynowPkceVerifier === 'function')
                  ? window.ynowPkceVerifier() : '';
                if (!verifier || typeof window.ynowPkceChallenge !== 'function') {
                  finish(baseUrl);
                  return true;
                }
                sessionStorage.setItem('ynow_oauth_code_verifier', verifier);
                window.ynowPkceChallenge(verifier).then(function (challenge) {
                  var join = baseUrl.indexOf('?') >= 0 ? '&' : '?';
                  var url = baseUrl + join +
                    'code_challenge=' + encodeURIComponent(challenge) +
                    '&code_challenge_method=S256';
                  finish(url);
                }).catch(function () {
                  finish(baseUrl);
                });
              } catch (ePkce) {
                finish(baseUrl);
              }
              return true;
            };
            addHandler('ynowAuthOAuthRedirect', function (payload) {
              window.ynowStartOAuthRedirect(payload);
            });
            addHandler('ynowAuthOAuthBootstrap', function (payload) {
              window.__ynowOAuthBootstrap = payload || null;
            });
            if (!window.__ynowOAuthSocialCapture) {
              window.__ynowOAuthSocialCapture = true;
              document.addEventListener('click', function (ev) {
                var t = ev.target;
                if (!t || !t.closest) return;
                var hit = t.closest(
                  '#ynow_auth_google, #ynow_auth_facebook, #ynow_auth_linkedin, button.ynow-auth-social'
                );
                if (!hit) return;
                var boot = window.__ynowOAuthBootstrap;
                if (!boot || !boot.ready || !boot.providers) return;
                var id = hit.id || '';
                var provider = '';
                if (id === 'ynow_auth_google' || hit.classList.contains('ynow-auth-social--google')) {
                  provider = 'google';
                } else if (id === 'ynow_auth_facebook' || hit.classList.contains('ynow-auth-social--facebook')) {
                  provider = 'facebook';
                } else if (id === 'ynow_auth_linkedin' || hit.classList.contains('ynow-auth-social--linkedin')) {
                  provider = 'linkedin';
                }
                if (!provider) return;
                var meta = boot.providers[provider];
                if (!meta || !meta.url) return;
                /* Disabled in dashboard: let Shiny show the in-app note. */
                if (meta.enabled === false) return;
                try { ev.preventDefault(); } catch (ePd) {}
                try { ev.stopImmediatePropagation(); } catch (eSip) {}
                try { ev.stopPropagation(); } catch (eSp) {}
                window.ynowStartOAuthRedirect({ url: String(meta.url), restore_tab: '' });
              }, true);
            }
            /* Sys. Admin foot link: hide while signed in (CSS + inline, sign-out restores). */
            function ynowSyncSysAdminFootVisibility(loggedIn) {
              var el = document.getElementById('ynow_sidebar_test_btn');
              if (!el) return;
              if (loggedIn) {
                el.style.setProperty('display', 'none', 'important');
                el.style.setProperty('visibility', 'hidden', 'important');
                el.style.setProperty('pointer-events', 'none', 'important');
                el.setAttribute('aria-hidden', 'true');
                el.setAttribute('tabindex', '-1');
              } else {
                el.style.removeProperty('display');
                el.style.removeProperty('visibility');
                el.style.removeProperty('pointer-events');
                el.removeAttribute('aria-hidden');
                el.removeAttribute('tabindex');
              }
            }
            window.ynowSyncSysAdminFootVisibility = ynowSyncSysAdminFootVisibility;
            /* Sidebar PDF button: Full guest → lock icon; Lite / signed-in → download icon.
               Lite never locks Download Report (free surface). */
            function ynowSyncSidebarDownloadIcon() {
              var dl = document.getElementById('download_report');
              if (!dl || !document.body) return;
              var isLite = document.body.classList.contains('ynow-lite');
              var locked = !isLite &&
                document.body.classList.contains('ynow-auth-feature') &&
                !document.body.classList.contains('ynow-authed') &&
                !document.body.classList.contains('ynow-view-as');
              var icon = dl.querySelector('i');
              if (!icon) {
                icon = document.createElement('i');
                icon.setAttribute('role', 'presentation');
                if (dl.firstChild) {
                  dl.insertBefore(icon, dl.firstChild);
                  dl.insertBefore(document.createTextNode(' '), icon.nextSibling);
                } else {
                  dl.appendChild(icon);
                  dl.appendChild(document.createTextNode(' '));
                }
              }
              if (locked) {
                icon.className = 'fas fa-lock ynow-sidebar-dl-lock';
                icon.setAttribute('aria-label', 'lock icon');
              } else {
                icon.className = 'fas fa-download';
                icon.setAttribute('aria-label', 'download icon');
              }
            }
            window.ynowSyncSidebarDownloadIcon = ynowSyncSidebarDownloadIcon;
            try { ynowSyncSidebarDownloadIcon(); } catch (eDlIcon0) {}
            try {
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                window.ynowSyncGuestSidebarLocked();
              }
            } catch (eGuestLock0) {}
            addHandler('ynowAuthPostLogin', function (payload) {
              if (!payload) return;
              /* Success: mark authed BEFORE tab restore / sidebar work so any
                 subsequent Sign-in / BS toggleModal open is vetoed. */
              if (payload.ok && document.body) {
                document.body.classList.add('ynow-authed');
                window.__ynowRealAuthed = true;
                ynowSyncSysAdminFootVisibility(true);
                if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                  window.ynowSyncSidebarDownloadIcon();
                }
                if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                  window.ynowSyncGuestSidebarLocked();
                }
              }
              try {
                if (window.jQuery) jQuery('#ynow_auth_modal').modal('hide');
              } catch (eHide) {}
              var tab = '';
              var ticker = '';
              try {
                tab = String(payload.restore_tab || sessionStorage.getItem('ynow_pre_oauth_tab') || '');
                ticker = String(payload.restore_ticker || '');
                sessionStorage.removeItem('ynow_oauth_code_verifier');
                sessionStorage.removeItem('ynow_pre_oauth_tab');
              } catch (eClr) {}
              /* Sys. Admin is guest/ops-only: never restore testing after sign-in. */
              if (payload.ok && tab === 'testing') tab = 'home';
              if (payload.ok) {
                try { ynowLastUiWrite(tab || null, ticker || null); } catch (eUiW) {}
                if (tab) ynowApplyRestoredTab(tab);
                if (ticker) ynowApplyRestoredTicker(ticker);
              }
            });
            /* Collapse sidebar, then show login modal (Sign in / PA gate).
               Uses window.ynowRequestAuthModal (defined later). Shiny 1.14+
               requires handler.length === 1 — zero-arg handlers never register. */
            addHandler('ynowAuthOpenModal', function (_msg) {
              if (document.body && document.body.classList.contains('ynow-authed')) return;
              var mode = (_msg && _msg.mode) ? String(_msg.mode) : '';
              if (typeof window.ynowRequestAuthModal === 'function') {
                window.ynowRequestAuthModal(mode);
              } else {
                setTimeout(function () {
                  try {
                    if (document.body && document.body.classList.contains('ynow-authed')) return;
                    if (window.jQuery) jQuery('#ynow_auth_modal').modal('show');
                  } catch (eModal) {}
                }, 140);
              }
            });
            addHandler('ynowUserSettings', function (payload) {
              if (!document.body || !document.body.classList) return;
              var hide = !!(payload && payload.hide_locked_sidebar);
              var compact = !!(payload && payload.sidebar_compact);
              if (hide) document.body.classList.add('ynow-hide-locked-nav');
              else document.body.classList.remove('ynow-hide-locked-nav');
              if (compact) document.body.classList.add('ynow-sidebar-compact');
              else document.body.classList.remove('ynow-sidebar-compact');
              var gain = (payload && payload.gain_loss_scheme) ?
                String(payload.gain_loss_scheme) : 'auto';
              gain = gain.replace(/\\s+/g, '_').toLowerCase();
              var allowedGain = {
                auto: 1, green_red: 1, red_green: 1, blue_red: 1, red_blue: 1
              };
              if (!allowedGain[gain]) gain = 'auto';
              document.body.setAttribute('data-ynow-gain-scheme', gain);
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                try { window.ynowSyncGuestSidebarLocked(); } catch (eSync) {}
              }
            });
            /* Durable browser resume: localStorage token survives tab/browser close.
               Cleared only on Sign out / revoke / expired resume. */
            var YNOW_AUTH_RESUME_KEY = 'ynow_auth_resume_v1';
            /* Last member UI surface (tab + ticker). Survives Sign out; used for
               early restore when a resume token is present. */
            var YNOW_LAST_UI_KEY = 'ynow_last_ui_v1';
            var ynowAuthResumeSent = false;
            var ynowAuthPersistWrite = function (token) {
              try {
                if (token) localStorage.setItem(YNOW_AUTH_RESUME_KEY, String(token));
                else localStorage.removeItem(YNOW_AUTH_RESUME_KEY);
              } catch (ePersist) {}
            };
            var ynowAuthPersistRead = function () {
              try { return localStorage.getItem(YNOW_AUTH_RESUME_KEY) || ''; }
              catch (eRead) { return ''; }
            };
            var ynowLastUiRead = function () {
              try {
                var raw = localStorage.getItem(YNOW_LAST_UI_KEY) || '';
                if (!raw) return { tab: '', ticker: '' };
                var o = JSON.parse(raw);
                return {
                  tab: o && o.tab ? String(o.tab) : '',
                  ticker: o && o.ticker ? String(o.ticker) : ''
                };
              } catch (eUi) { return { tab: '', ticker: '' }; }
            };
            var ynowLastUiWrite = function (tab, ticker) {
              try {
                var cur = ynowLastUiRead();
                var next = {
                  tab: (tab != null && String(tab)) ? String(tab) : (cur.tab || ''),
                  ticker: (ticker != null && String(ticker)) ? String(ticker) : (cur.ticker || '')
                };
                if (next.tab === 'testing') next.tab = 'home';
                localStorage.setItem(YNOW_LAST_UI_KEY, JSON.stringify(next));
              } catch (eW) {}
            };
            window.ynowLastUiWrite = ynowLastUiWrite;
            window.ynowLastUiRead = ynowLastUiRead;
            var ynowApplyRestoredTab = function (tab) {
              tab = String(tab || '');
              if (!tab || tab === 'testing') return;
              try {
                if (typeof window.ynowShowSidebarFootTab === 'function' &&
                    (tab === 'snapshot' || tab === 'feedback')) {
                  window.ynowShowSidebarFootTab(tab);
                } else if (typeof window.ynowGotoTab === 'function') {
                  window.ynowGotoTab(tab);
                } else if (window.Shiny && Shiny.setInputValue) {
                  Shiny.setInputValue('sidebar_tabs', tab, { priority: 'event' });
                }
              } catch (eTab) {}
            };
            var ynowApplyRestoredTicker = function (ticker) {
              var tk = String(ticker || '');
              if (!tk || !(window.Shiny && Shiny.setInputValue)) return;
              try {
                Shiny.setInputValue('ynow_auth_restore_ticker', tk, { priority: 'event' });
                var sc = document.getElementById('sc');
                if (sc) {
                  sc.value = tk;
                  Shiny.setInputValue('sc', tk, { priority: 'event' });
                  Shiny.setInputValue('ticker_typeahead', tk, { priority: 'event' });
                }
              } catch (eTk) {}
            };
            var ynowAuthResumeOffer = function () {
              if (ynowAuthResumeSent) return;
              if (!window.Shiny || !Shiny.setInputValue) return;
              var tok = ynowAuthPersistRead();
              if (!tok) return;
              ynowAuthResumeSent = true;
              /* Apply last UI early so first authed paint is closer to last screen. */
              try {
                var ui = ynowLastUiRead();
                if (ui.tab) ynowApplyRestoredTab(ui.tab);
              } catch (eEarly) {}
              Shiny.setInputValue('ynow_auth_resume_token', String(tok), { priority: 'event' });
            };
            addHandler('ynowAuthPersist', function (payload) {
              if (!payload) return;
              if (payload.clear) {
                ynowAuthPersistWrite('');
                ynowAuthResumeSent = false;
                return;
              }
              if (payload.token) {
                ynowAuthPersistWrite(payload.token);
                ynowAuthResumeSent = true;
              }
            });
            /* Single-device: another tab/device replaced or cleared the resume key. */
            window.addEventListener('storage', function (ev) {
              try {
                if (!ev || ev.key !== 'ynow_auth_resume_v1') return;
                if (!document.body || !document.body.classList.contains('ynow-authed')) return;
                if (!window.Shiny || !Shiny.setInputValue) return;
                Shiny.setInputValue(
                  'ynow_auth_session_peer_takeover',
                  (window.__ynowPeerTakeover = (window.__ynowPeerTakeover || 0) + 1),
                  { priority: 'event' }
                );
              } catch (ePeer) {}
            });
            addHandler('ynowAuthSessionTaken', function (payload) {
              try {
                ynowAuthPersistWrite('');
                ynowAuthResumeSent = false;
                if (document.body) {
                  document.body.classList.remove('ynow-authed');
                  document.body.classList.remove('ynow-hide-locked-nav');
                  document.body.classList.remove('ynow-sidebar-compact');
                }
                if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                  window.ynowSyncGuestSidebarLocked();
                }
                if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                  window.ynowSyncSidebarDownloadIcon();
                }
                var msg = (payload && payload.message) ? String(payload.message) : '';
                if (msg && window.Shiny && Shiny.notifications) {
                  /* Server also showNotification; client toast is best-effort. */
                }
              } catch (eTaken) {}
            });
            document.addEventListener('shiny:connected', function () {
              setTimeout(ynowAuthResumeOffer, 0);
            });
            document.addEventListener('shiny:sessioninitialized', function () {
              setTimeout(ynowAuthResumeOffer, 0);
            });
            addHandler('ynowAuthState', function (payload) {
              if (!payload) return;
              var realLoggedIn = !!(payload.logged_in);
              window.__ynowRealAuthed = realLoggedIn;
              var viewAs = !!(document.body && document.body.classList.contains('ynow-view-as'));
              var selfMode = window.__ynowSelfViewMode || '';
              /* Self-view guest forces guest chrome; signed_in / omniscient force member chrome. */
              var selfSigned = (selfMode === 'signed_in' || selfMode === 'omniscient');
              var selfGuest = (selfMode === 'guest');
              /* View-as / self signed-in keep session-like chrome even when ops admin is not email-authed. */
              var loggedIn = selfGuest ? false : (realLoggedIn || viewAs || selfSigned);
              if (loggedIn) document.body.classList.add('ynow-authed');
              else {
                document.body.classList.remove('ynow-authed');
                document.body.classList.remove('ynow-hide-locked-nav');
                document.body.classList.remove('ynow-sidebar-compact');
                document.body.setAttribute('data-ynow-gain-scheme', 'auto');
              }
              ynowSyncSysAdminFootVisibility(loggedIn);
              if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                window.ynowSyncSidebarDownloadIcon();
              }
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                window.ynowSyncGuestSidebarLocked();
              }
              var openLinks = document.querySelectorAll('.ynow-auth-open');
              var regLinks = document.querySelectorAll('.ynow-auth-register-open');
              var outLinks = document.querySelectorAll('.ynow-auth-signout');
              var i;
              for (i = 0; i < openLinks.length; i++) {
                openLinks[i].style.display = loggedIn ? 'none' : '';
              }
              for (i = 0; i < regLinks.length; i++) {
                regLinks[i].style.display = loggedIn ? 'none' : '';
              }
              for (i = 0; i < outLinks.length; i++) {
                outLinks[i].style.display = loggedIn ? '' : 'none';
              }
              if (payload.label_sign_in) {
                var openLbls = document.querySelectorAll('.ynow-auth-open-label');
                for (i = 0; i < openLbls.length; i++) openLbls[i].textContent = payload.label_sign_in;
              }
              if (payload.label_sign_out) {
                var outLbls = document.querySelectorAll('.ynow-auth-signout-label');
                for (i = 0; i < outLbls.length; i++) outLbls[i].textContent = payload.label_sign_out;
              }
              if (payload.label_register) {
                var regLbls = document.querySelectorAll('.ynow-auth-register-label');
                for (i = 0; i < regLbls.length; i++) regLbls[i].textContent = payload.label_register;
              }
              /* Leave Sys. Admin tab if still open after sign-in (link is CSS-hidden).
                 Skip while view-as — Exit user view owns return to the portal. */
              if (realLoggedIn && !viewAs && window.Shiny && Shiny.setInputValue) {
                try {
                  var active = document.querySelector(
                    '.sidebar-menu li.active > a[data-value], .sidebar-menu a.active[data-value]'
                  );
                  var footActive = document.querySelector(
                    '.ynow-sidebar-snapshot-foot a.active[data-value], ' +
                    '.ynow-sidebar-snapshot-foot a[aria-expanded=\"true\"][data-value]'
                  );
                  var cur = (active && active.getAttribute('data-value')) ||
                    (footActive && footActive.getAttribute('data-value')) || '';
                  var hash = (window.location.hash || '').replace(/^#shiny-tab-/, '');
                  if (cur === 'testing' || hash === 'testing') {
                    Shiny.setInputValue('sidebar_tabs', 'home', { priority: 'event' });
                  }
                } catch (eLeaveTest) {}
              }
            });
            addHandler('ynowDcLocale', applyDcLocale);
            addHandler('ynowDcSopLocale', function (payload) {
              if (!payload) return;
              var set = function (id, val) {
                var el = document.getElementById(id);
                if (el && val) el.textContent = val;
              };
              set('ynow_dc_sop_intro', payload.intro);
              set('ynow_dc_box_sop', payload.box_sop);
              set('ynow_dc_sop_step1_title', payload.step1);
              set('ynow_dc_sop_step2_title', payload.step2);
              set('ynow_dc_sop_step3_title', payload.step3);
              set('ynow_dc_sop_ack_sgr_label', payload.ack_sgr);
              set('ynow_dc_sop_ack_sens_label', payload.ack_sens);
              set('ynow_dc_sop_ack_qual_label', payload.ack_qual);
            });
            addHandler('ynowCorpLogoColors', function (payload) {
              var root = document.documentElement;
              var c1 = payload && payload.c1 ? String(payload.c1) : '';
              var c2 = payload && payload.c2 ? String(payload.c2) : '';
              if (!c1 || !c2) {
                root.style.removeProperty('--ynow-corp-c1');
                root.style.removeProperty('--ynow-corp-c2');
                document.body.classList.remove('ynow-corp-logo');
                return;
              }
              root.style.setProperty('--ynow-corp-c1', c1);
              root.style.setProperty('--ynow-corp-c2', c2);
              document.body.classList.add('ynow-corp-logo');
            });
            addHandler('ynowModelTheme', function (payload) {
              var tab = (payload && payload.tab) ? String(payload.tab) : '';
              /* Keys match Model Selector card accents (NAV/DCF/DDM/RI/P/B/Multiples/SOTP). */
              var map = {
                nav_calculator: 'nav',
                dcf_calculator: 'dcf',
                ddm_calculator: 'ddm',
                ri_calculator: 'ri',
                pb_calculator: 'pb',
                rel_mult_ev: 'multiples',
                rel_multiples_calculator: 'multiples',
                rel_mult_rev: 'multiples',
                sotp_calculator: 'sotp'
              };
              var key = map[tab] || null;
              ['nav', 'dcf', 'ddm', 'ri', 'pb', 'multiples', 'sotp'].forEach(function (k) {
                document.body.classList.toggle('ynow-theme-' + k, k === key);
              });
              document.body.classList.toggle('ynow-theme-model', !!key);
              document.body.classList.toggle('ynow-theme-bluechip', tab === 'bluechip');
              document.body.classList.toggle('ynow-theme-atx', tab === 'asset_transmission');
            });
            /* Models filter: lock methods with no primary names (Listed-style disabled, unchecked). */
            addHandler('ynowLabImMethodsLock', function (payload) {
              var avail = (payload && payload.available) ? payload.available.map(String) : [];
              var root = document.getElementById('lab_im_methods');
              if (!root) return;
              var inputs = root.querySelectorAll('input[type=checkbox]');
              var changed = false;
              inputs.forEach(function (el) {
                var ok = avail.indexOf(String(el.value)) >= 0;
                var label = el.closest('label');
                if (label) label.classList.toggle('ynow-lab-im-method--locked', !ok);
                if (!ok) {
                  if (el.checked) {
                    el.checked = false;
                    changed = true;
                  }
                  el.disabled = true;
                  el.setAttribute('aria-disabled', 'true');
                } else {
                  el.disabled = false;
                  el.removeAttribute('aria-disabled');
                }
              });
              if (changed && window.Shiny && Shiny.inputBindings) {
                $(root).trigger('change');
              }
            });
          }
          registerLocaleHandler();
          /* Re-broadcast locale after connect so a dropped ynowUiLocale cannot stick. */
          if (window.jQuery) {
            jQuery(document).on(
              'shiny:connected shiny:sessioninitialized shiny:reconnected',
              function () {
                registerLocaleHandler();
                requestLocaleResync('shiny-lifecycle');
              }
            );
          } else {
            document.addEventListener('shiny:connected', function () {
              registerLocaleHandler();
              requestLocaleResync('shiny-lifecycle');
            });
          }

          /* Device / browser language → initial UI locale (mapping done in R). */
          function pushDeviceUiLocale() {
            if (!(window.Shiny && Shiny.setInputValue)) {
              setTimeout(pushDeviceUiLocale, 50);
              return;
            }
            if (window.__ynowDeviceLocalePushed) return;
            window.__ynowDeviceLocalePushed = true;
            var tags = [];
            try {
              if (navigator.languages && navigator.languages.length) {
                for (var i = 0; i < navigator.languages.length; i++) {
                  var t = String(navigator.languages[i] || '').trim();
                  if (t) tags.push(t);
                }
              } else if (navigator.language) {
                var one = String(navigator.language || '').trim();
                if (one) tags.push(one);
              }
            } catch (eLang) {}
            Shiny.setInputValue('device_ui_locale', tags.join(','), {priority: 'event'});
          }
          pushDeviceUiLocale();

          /* OAuth return: Google/Facebook/Supabase redirect lands with ?code= or #access_token= */
          /* PKCE helpers for Supabase Auth authorize → token exchange */
          window.ynowPkceVerifier = function () {
            try {
              var a = new Uint8Array(32);
              (window.crypto || window.msCrypto).getRandomValues(a);
              var s = '';
              for (var i = 0; i < a.length; i++) s += String.fromCharCode(a[i]);
              return btoa(s).replace(/\\+/g, '-').replace(/\\//g, '_').replace(/=+$/g, '');
            } catch (eV) {
              return '';
            }
          };
          window.ynowPkceChallenge = function (verifier) {
            var data = new TextEncoder().encode(String(verifier || ''));
            return (window.crypto || window.msCrypto).subtle.digest('SHA-256', data).then(function (buf) {
              var bytes = new Uint8Array(buf);
              var s = '';
              for (var i = 0; i < bytes.length; i++) s += String.fromCharCode(bytes[i]);
              return btoa(s).replace(/\\+/g, '-').replace(/\\//g, '_').replace(/=+$/g, '');
            });
          };
          window.ynowPersistPreAuthTab = function () {
            try {
              var a = document.querySelector('.sidebar-menu li.active > a[data-value]');
              var tab = a ? (a.getAttribute('data-value') || '') : '';
              if (tab) sessionStorage.setItem('ynow_pre_oauth_tab', tab);
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('ynow_auth_restore_tab', tab || '', { priority: 'event' });
              }
              return tab;
            } catch (eP) { return ''; }
          };
          function ynowParseQuery(qs) {
            var out = {};
            var raw = String(qs || '');
            if (raw.charAt(0) === '?' || raw.charAt(0) === '#') raw = raw.slice(1);
            raw.split('&').forEach(function (pair) {
              if (!pair) return;
              var i = pair.indexOf('=');
              var k = i < 0 ? pair : pair.slice(0, i);
              var v = i < 0 ? '' : pair.slice(i + 1);
              try {
                out[decodeURIComponent(String(k).split('+').join(' '))] =
                  decodeURIComponent(String(v).split('+').join(' '));
              } catch (eDec) {
                out[k] = v;
              }
            });
            return out;
          }
          function ynowPushOAuthCallback() {
            if (!(window.Shiny && Shiny.setInputValue)) {
              setTimeout(ynowPushOAuthCallback, 50);
              return;
            }
            if (window.__ynowOAuthCallbackPushed) return;
            var q = ynowParseQuery(window.location.search || '');
            var h = ynowParseQuery(window.location.hash || '');
            var access = h.access_token || q.access_token || '';
            var code = q.code || h.code || '';
            var state = q.state || h.state || '';
            var err = q.error || h.error || '';
            var authType = h.type || q.type || '';
            if (!access && !code && !err) return;
            window.__ynowOAuthCallbackPushed = true;
            var storedTab = '';
            var codeVerifier = '';
            try {
              storedTab = sessionStorage.getItem('ynow_pre_oauth_tab') || '';
              codeVerifier = sessionStorage.getItem('ynow_oauth_code_verifier') || '';
            } catch (eS) {}
            Shiny.setInputValue('ynow_auth_oauth_callback', {
              access_token: String(access || ''),
              refresh_token: String(h.refresh_token || q.refresh_token || ''),
              code: String(code || ''),
              state: String(state || ''),
              type: String(authType || ''),
              provider: String(q.provider || h.provider || ''),
              error: String(err || ''),
              code_verifier: String(codeVerifier || ''),
              restore_tab: String(storedTab || '')
            }, {priority: 'event'});
            try {
              var path = window.location.pathname || '/';
              if (window.history && history.replaceState) {
                history.replaceState(null, '', path);
              } else {
                window.location.hash = '';
              }
            } catch (eClear) {}
          }
          ynowPushOAuthCallback();
          document.addEventListener('shiny:connected', function () {
            ynowPushOAuthCallback();
          });

          /* ---- YNOW KPI cards: click / keyboard scroll to chapter anchors ---- */
          function ynowScrollFunnelAnchor(id) {
            if (!id) return false;
            var el = document.getElementById(id);
            if (!el) return false;
            try { el.scrollIntoView({ behavior: 'smooth', block: 'start' }); }
            catch (e) { try { el.scrollIntoView(true); } catch (e2) {} }
            return true;
          }
          function ynowOnFunnelKpiJump(ev) {
            var a = ev.target && ev.target.closest ? ev.target.closest('a.ynow-funnel-kpi-jump') : null;
            if (!a) return;
            var href = a.getAttribute('href') || '';
            if (href.charAt(0) !== '#') return;
            var id = href.slice(1);
            if (!ynowScrollFunnelAnchor(id)) return;
            ev.preventDefault();
            if (window.history && history.replaceState) {
              try { history.replaceState(null, '', href); } catch (e3) {}
            }
          }
          document.addEventListener('click', ynowOnFunnelKpiJump);
          document.addEventListener('keydown', function (ev) {
            if (ev.key !== ' ' && ev.key !== 'Spacebar') return;
            var a = ev.target && ev.target.closest ? ev.target.closest('a.ynow-funnel-kpi-jump') : null;
            if (!a) return;
            ev.preventDefault();
            a.click();
          });

          /* ---- Lite mode: sidebar logo click toggles body.ynow-lite + LITE badge ---- */
          (function () {
            /* Sidebar tabs Lite must leave (ynow_sidebar_lite_omit_tabs) + Sys. Admin foot. */
            var FULL_ONLY_TABS = (window.__YNOW_LITE_OMIT_TABS || []).slice();
            if (FULL_ONLY_TABS.indexOf('testing') < 0) FULL_ONLY_TABS.push('testing');
            if (!FULL_ONLY_TABS.length) {
              FULL_ONLY_TABS = [
                'get_started', 'nav_calculator', 'dcf_calculator', 'ddm_calculator',
                'ri_calculator', 'pb_calculator', 'rel_mult_ev', 'rel_multiples_calculator',
                'rel_mult_rev', 'sotp_calculator', 'hfv',
                'decision_checklist', 'mos_assumptions', 'lab_notes', 'price_alerts', 'testing', 'company_advance',
                'asset_transmission', 'account_admin', 'account_points', 'settings'
              ];
            }
            function remapLegacyTab(tab) {
              tab = String(tab || '');
              if (tab === 'business_breakdown_lab') return 'company_advance';
              if (tab === 'bluechip_lab') return 'bluechip';
              return tab;
            }
            window.ynowRemapLegacyTab = remapLegacyTab;
            function ynowTabAnchor(tab) {
              return document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]') ||
                     document.querySelector('a[data-value=\"' + tab + '\"]');
            }
            function currentSidebarTab() {
              var active = document.querySelector('.sidebar-menu li.active > a[data-value]');
              if (active) return remapLegacyTab(active.getAttribute('data-value') || '');
              var pane = document.querySelector('.tab-content > .tab-pane.active');
              if (pane) {
                var id = pane.id || '';
                if (id.indexOf('shiny-tab-') === 0) {
                  return remapLegacyTab(id.slice('shiny-tab-'.length));
                }
                return remapLegacyTab(pane.getAttribute('data-value') || '');
              }
              return '';
            }
            function gotoTab(tab) {
              tab = remapLegacyTab(tab);
              if (!tab) return;
              /* Signed-in members: Sys. Admin portal is guest/ops only. */
              if (tab === 'testing' && document.body &&
                  document.body.classList.contains('ynow-authed')) {
                tab = 'home';
              }
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
              }
              var a = ynowTabAnchor(tab);
              if (a) {
                try { a.click(); } catch (e) {}
              }
              if (document.body && document.body.classList.contains('ynow-authed') &&
                  typeof window.ynowLastUiWrite === 'function') {
                try { window.ynowLastUiWrite(tab, null); } catch (eUi) {}
              }
            }
            window.ynowGotoTab = gotoTab;
            window.ynowCurrentSidebarTab = currentSidebarTab;
            function bindModelSelectorJump() {
              if (document.documentElement.getAttribute('data-ynow-ms-jump') === '1') return;
              document.documentElement.setAttribute('data-ynow-ms-jump', '1');
              document.addEventListener('click', function (ev) {
                var card = ev.target && ev.target.closest
                  ? ev.target.closest('.ynow-model-card[data-tab]')
                  : null;
                if (!card) return;
                ev.preventDefault();
                var tab = card.getAttribute('data-tab');
                if (tab) gotoTab(tab);
              });
              document.addEventListener('keydown', function (ev) {
                var card = ev.target && ev.target.closest
                  ? ev.target.closest('.ynow-model-card[data-tab]')
                  : null;
                if (!card) return;
                if (ev.key !== 'Enter' && ev.key !== ' ') return;
                ev.preventDefault();
                var tab = card.getAttribute('data-tab');
                if (tab) gotoTab(tab);
              });
            }
            bindModelSelectorJump();
            function refreshCurrentSidebarTab() {
              var tab = currentSidebarTab();
              if (!tab) tab = 'home';
              var mark = document.getElementById('ynow_header_logo_home');
              var focus = !!(mark && mark.classList && mark.classList.contains('ynow-logo-spinning'));
              window.__ynowLogoNonce = (window.__ynowLogoNonce || 0) + 1;
              var n = window.__ynowLogoNonce;
              if (focus) {
                window.__ynowLogoFocusN = n;
                if (mark && mark.classList) mark.classList.add('ynow-logo-spin-fast');
                if (typeof window.ynowFocusCurrentTabBusy === 'function') {
                  try { window.ynowFocusCurrentTabBusy(tab); } catch (eFocus) {}
                } else if (typeof window.ynowClearOtherTabBusy === 'function') {
                  try { window.ynowClearOtherTabBusy(tab); } catch (eClear) {}
                }
              }
              if (typeof window.ynowSetSidebarTabBusy === 'function') {
                try { window.ynowSetSidebarTabBusy(tab, true, 'refresh'); } catch (eBusy) {}
              } else if (mark && mark.classList) {
                mark.classList.add('ynow-logo-spinning');
                try { mark.setAttribute('aria-busy', 'true'); } catch (eA0) {}
              }
              if (focus && mark && mark.classList) mark.classList.add('ynow-logo-spin-fast');
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('ynow_tab_refresh', {
                  tab: tab,
                  t: Date.now(),
                  focus: focus,
                  n: n
                }, { priority: 'event' });
              }
            }
            window.ynowRefreshCurrentSidebarTab = refreshCurrentSidebarTab;
            function bindHeaderLogoRefresh() {
              if (document.documentElement.getAttribute('data-ynow-header-logo-refresh') === '1') return;
              document.documentElement.setAttribute('data-ynow-header-logo-refresh', '1');
              /* Keep legacy flag so older tests still see a bind marker. */
              document.documentElement.setAttribute('data-ynow-header-logo-home', '1');
              document.addEventListener('click', function (ev) {
                var mark = ev.target && ev.target.closest
                  ? ev.target.closest('#ynow_header_logo_home, .ynow-header-logo-mark')
                  : null;
                if (!mark) return;
                // Currency controls sit under the same <li>; only the mark refreshes.
                if (ev.target && ev.target.closest && ev.target.closest('.ynow-ccy-float')) return;
                ev.preventDefault();
                refreshCurrentSidebarTab();
              });
              document.addEventListener('keydown', function (ev) {
                var t = ev.target;
                if (!t || t.id !== 'ynow_header_logo_home') return;
                if (ev.key !== 'Enter' && ev.key !== ' ') return;
                ev.preventDefault();
                refreshCurrentSidebarTab();
              });
            }
            bindHeaderLogoRefresh();
            /* Alias for older references / tests. */
            window.bindHeaderLogoHome = bindHeaderLogoRefresh;
            function remapLegacyHash() {
              var h = String(location.hash || '');
              if (h === '#shiny-tab-business_breakdown_lab') {
                gotoTab('company_advance');
                try { history.replaceState(null, '', '#shiny-tab-company_advance'); } catch (eH) {}
              }
            }
            function registerGotoTabHandler() {
              if (!window.Shiny || !Shiny.addCustomMessageHandler) {
                setTimeout(registerGotoTabHandler, 50);
                return;
              }
              Shiny.addCustomMessageHandler('ynowGotoTab', function (payload) {
                var tab = payload && payload.tab;
                if (tab) gotoTab(tab);
              });
              /* Search YNOW / TYNOW (/TYNOW): open Macro and expand the own-index KPI. */
              Shiny.addCustomMessageHandler('ynowMacroOpenIndex', function (payload) {
                var sym = payload && payload.symbol ? String(payload.symbol) : '';
                if (!sym) return;
                var tries = 0;
                function tryOpen() {
                  tries += 1;
                  var el = document.querySelector(
                    '.ynow-macro-kpi[data-macro-index=\"' + sym + '\"], #ynow_macro_idx_' +
                      String(sym).toLowerCase().replace(/[^a-z0-9]+/g, '')
                  );
                  if (el && typeof el.click === 'function') {
                    el.click();
                    return;
                  }
                  if (window.Shiny && Shiny.setInputValue) {
                    try {
                      Shiny.setInputValue('macro-index_click', sym, {priority: 'event'});
                    } catch (eClick) {}
                  }
                  if (tries < 48) setTimeout(tryOpen, 250);
                }
                setTimeout(tryOpen, 400);
              });
            }
            registerGotoTabHandler();
            window.addEventListener('hashchange', remapLegacyHash);
            function applyLiteMode(on, opts) {
              opts = opts || {};
              var enabled = !!on;
              document.body.classList.toggle('ynow-lite', enabled);
              document.querySelectorAll('.ynow-lite-toggle').forEach(function (brand) {
                brand.setAttribute('aria-pressed', enabled ? 'true' : 'false');
              });
              try {
                window.sessionStorage.setItem('ynow_lite_mode', enabled ? '1' : '0');
              } catch (e0) {}
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('ynow_lite_mode', enabled, {priority: 'event'});
              }
              if (enabled) ensureLiteSnapshotFreeTab();
              try {
                if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                  window.ynowSyncSidebarDownloadIcon();
                }
              } catch (eDlLite) {}
              try {
                if (typeof window.ynowApplyPdfQuotaCopy === 'function') {
                  window.ynowApplyPdfQuotaCopy(window.__ynowUiLocaleStrings || null);
                } else {
                  var pdfQuotaRem = document.getElementById('ynow_pdf_quota_remaining');
                  var pdfQuotaRules = document.getElementById('ynow_pdf_quota_rules');
                  var locBundle = window.__ynowUiLocaleStrings || null;
                  if (locBundle) {
                    if (enabled) {
                      if (pdfQuotaRem && (locBundle.pdf_quota_hint_lite || locBundle.pdf_quota_hint)) {
                        pdfQuotaRem.textContent = locBundle.pdf_quota_hint_lite || locBundle.pdf_quota_hint;
                      }
                      if (pdfQuotaRules) pdfQuotaRules.textContent = '';
                    } else if (pdfQuotaRem && locBundle.pdf_quota_hint) {
                      pdfQuotaRem.textContent = locBundle.pdf_quota_hint;
                    }
                  }
                }
                var snapQ = document.getElementById('ynow_snapshot_download_quota');
                var locB2 = window.__ynowUiLocaleStrings || null;
                if (snapQ && locB2) {
                  snapQ.textContent = enabled
                    ? (locB2.snapshot_download_quota_hint_lite || locB2.snapshot_download_quota_hint || '')
                    : (locB2.snapshot_download_quota_hint || '');
                }
              } catch (eQLite) {}
              remapLegacyHash();
              if (opts.navigate === false) return;
              var tab = currentSidebarTab();
              if (enabled) {
                if (FULL_ONLY_TABS.indexOf(tab) >= 0) gotoTab('smart_analysis');
              } else if (tab === 'smart_analysis') {
                gotoTab('dashboard');
              }
            }
            function ensureLiteSnapshotFreeTab() {
              if (!document.body.classList.contains('ynow-lite')) return;
              /* Lite keeps Snapshot free: audit + current. Never land on locked APP_DEFAULTS. */
              var active = document.querySelector('#snapshot_report .nav-tabs li.active a');
              var dv = active && active.getAttribute('data-value');
              if (dv === 'snap_audit' || dv === 'snap_current') return;
              var a = document.querySelector('#snapshot_report a[data-value=\"snap_current\"]') ||
                document.querySelector('#snapshot_report a[data-value=\"snap_audit\"]');
              if (!a) return;
              try {
                if (window.jQuery) window.jQuery(a).tab('show');
                else if (typeof a.click === 'function') a.click();
              } catch (eSnap) {}
            }
            window.ensureLiteSnapshotFreeTab = ensureLiteSnapshotFreeTab;
            /* Legacy alias — older call sites / tests. */
            window.ensureLiteSnapshotDefaultsTab = ensureLiteSnapshotFreeTab;
            /* Foot links (Snapshot / Testing / Feedback) are outside .sidebar-menu.
               Activate the pane directly — do not a.click() the foot anchor (recursion
               with return false) and do not rely on Bootstrap after preventDefault. */
            function showSidebarFootTab(tab) {
              tab = remapLegacyTab(tab);
              if (!tab) return;
              /* Signed-in members: never open Sys. Admin (testing) via foot link. */
              if (tab === 'testing' && document.body &&
                  document.body.classList.contains('ynow-authed')) {
                tab = 'home';
              }
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
              }
              var pane = document.getElementById('shiny-tab-' + tab);
              if (pane) {
                try {
                  if (window.jQuery) {
                    jQuery(pane).addClass('active').siblings('.tab-pane').removeClass('active in');
                  } else {
                    var parent = pane.parentNode;
                    if (parent && parent.children) {
                      Array.prototype.forEach.call(parent.children, function (sib) {
                        if (sib !== pane && sib.classList) {
                          sib.classList.remove('active');
                          sib.classList.remove('in');
                        }
                      });
                    }
                    pane.classList.add('active');
                  }
                } catch (eShow) {}
              }
              try { history.replaceState(null, '', '#shiny-tab-' + tab); } catch (eH) {}
              if (tab === 'snapshot') {
                setTimeout(function () {
                  try {
                    if (window.ensureLiteSnapshotFreeTab) window.ensureLiteSnapshotFreeTab();
                  } catch (eSnap2) {}
                }, 0);
              }
            }
            window.ynowShowSidebarFootTab = showSidebarFootTab;
            function toggleLite() {
              applyLiteMode(!document.body.classList.contains('ynow-lite'));
            }
            function bindLiteToggle() {
              if (document.documentElement.getAttribute('data-ynow-lite-delegated') === '1') return;
              document.documentElement.setAttribute('data-ynow-lite-delegated', '1');
              document.addEventListener('click', function (ev) {
                var brand = ev.target && ev.target.closest ? ev.target.closest('.ynow-lite-toggle') : null;
                if (!brand) return;
                ev.preventDefault();
                ev.stopPropagation();
                toggleLite();
              }, true);
              document.addEventListener('keydown', function (ev) {
                var brand = ev.target && ev.target.closest ? ev.target.closest('.ynow-lite-toggle') : null;
                if (!brand) return;
                if (ev.key === 'Enter' || ev.key === ' ' || ev.keyCode === 13 || ev.keyCode === 32) {
                  ev.preventDefault();
                  toggleLite();
                }
              }, true);
            }
            function restoreLiteFromStorage() {
              var saved = null;
              try { saved = window.sessionStorage.getItem('ynow_lite_mode'); } catch (e1) {}
              applyLiteMode(saved === '1', {navigate: false});
              remapLegacyHash();
            }
            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', function () {
                bindLiteToggle();
                restoreLiteFromStorage();
              });
            } else {
              bindLiteToggle();
              restoreLiteFromStorage();
            }
            if (window.jQuery) {
              jQuery(document).on('shiny:connected shiny:sessioninitialized', function () {
                bindLiteToggle();
                restoreLiteFromStorage();
              });
            }
            window.__ynowApplyLiteMode = applyLiteMode;
          })();

          /* ---- Param audit (1A+2B): go to tab + highlight input ---- */
          function ynowClearParamHighlight() {
            document.querySelectorAll('.ynow-param-highlight').forEach(function (el) {
              el.classList.remove('ynow-param-highlight');
            });
          }
          function ynowFindInputEl(inputId) {
            if (!inputId) return null;
            var id = String(inputId);
            var el = document.getElementById(id);
            if (el) return el;
            el = document.querySelector('[id=\"' + id.replace(/\"/g, '') + '\"]');
            if (el) return el;
            var named = document.querySelectorAll('[name=\"' + id.replace(/\"/g, '') + '\"]');
            if (named && named.length) {
              return named[0].closest('.form-group, .shiny-input-container, .radio, .checkbox') || named[0];
            }
            return null;
          }
          function ynowGotoAndHighlight(tab, inputId) {
            ynowClearParamHighlight();
            tab = window.ynowRemapLegacyTab ? window.ynowRemapLegacyTab(tab) : tab;
            if (tab && window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
            }
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('a[data-value=\"' + tab + '\"]');
            if (a) {
              try { a.click(); } catch (e) {}
            }
            setTimeout(function () {
              var el = ynowFindInputEl(inputId);
              if (!el) return;
              var box = el.closest('.form-group, .shiny-input-container, .box-body > div, .radio, .checkbox') || el;
              box.classList.add('ynow-param-highlight');
              try { box.scrollIntoView({behavior: 'smooth', block: 'center'}); } catch (e2) {
                try { box.scrollIntoView(true); } catch (e3) {}
              }
              setTimeout(function () { box.classList.remove('ynow-param-highlight'); }, 4500);
            }, 350);
          }
          window.ynowGotoAndHighlight = ynowGotoAndHighlight;

          /* g ≥ WACC／Ke: expand sidebar, pulse Model Dashboard, jump to SGR */
          function ynowSidebarPushToggle() {
            return document.querySelector('.main-header .sidebar-toggle') ||
                   document.querySelector('a.sidebar-toggle') ||
                   document.querySelector('[data-toggle=\"push-menu\"]');
          }
          function ynowEnsureSidebarOpen() {
            var body = document.body;
            if (!body) return;
            if (!body.classList.contains('sidebar-collapse')) return;
            var toggle = ynowSidebarPushToggle();
            if (toggle) {
              try { toggle.click(); } catch (e0) {}
            }
            // Fallback if AdminLTE click handler did not expand
            setTimeout(function () {
              if (body.classList.contains('sidebar-collapse')) {
                body.classList.remove('sidebar-collapse');
                try {
                  if (window.jQuery) {
                    jQuery('body').trigger('expanded.pushMenu');
                  }
                } catch (e1) {}
              }
            }, 80);
          }
          /* Sign-in: collapse sidebar before the login modal appears.
             Mobile AdminLTE uses body.sidebar-open (often WITH sidebar-collapse
             still set). Desktop uses sidebar-collapse alone. Do not early-return
             on sidebar-collapse when sidebar-open is present. */
          function ynowEnsureSidebarCollapsed() {
            var body = document.body;
            if (!body) return;
            var isMobile = false;
            try {
              isMobile = !!(window.matchMedia && window.matchMedia('(max-width: 767px)').matches);
            } catch (eMm) {
              isMobile = window.innerWidth <= 767;
            }
            var openMobile = body.classList.contains('sidebar-open');
            var collapsed = body.classList.contains('sidebar-collapse');
            // Already fully closed for this viewport.
            if (isMobile || openMobile) {
              if (!openMobile) return;
            } else if (collapsed) {
              return;
            }
            var toggle = ynowSidebarPushToggle();
            if (toggle) {
              try { toggle.click(); } catch (e0) {}
            }
            setTimeout(function () {
              if (isMobile || body.classList.contains('sidebar-open')) {
                body.classList.remove('sidebar-open');
                body.classList.remove('sidebar-collapse');
                try {
                  if (window.jQuery) {
                    jQuery('body').trigger('collapsed.pushMenu');
                  }
                } catch (e1) {}
              } else if (!body.classList.contains('sidebar-collapse')) {
                body.classList.add('sidebar-collapse');
                try {
                  if (window.jQuery) {
                    jQuery('body').trigger('collapsed.pushMenu');
                  }
                } catch (e2) {}
              }
            }, 80);
          }
          window.ynowEnsureSidebarCollapsed = ynowEnsureSidebarCollapsed;
          function ynowIsAuthed() {
            return !!(document.body && document.body.classList.contains('ynow-authed'));
          }
          window.ynowIsAuthed = ynowIsAuthed;
          try {
            if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
              window.ynowSyncSidebarDownloadIcon();
            }
          } catch (eDlIcon) {}
          /* Full guest click gate for proprietary downloads / capture exports.
             Ids from membership_locks catalog (window.__YNOW_GATED_DOWNLOAD_IDS);
             each has explicit A/B tier in __YNOW_GATED_DOWNLOAD_TIERS.
             Lite stays open (Download Report free). Public statement CSVs stay open. */
          document.addEventListener('click', function (ev) {
            if (!document.body) return;
            if (!document.body.classList.contains('ynow-auth-feature')) return;
            if (document.body.classList.contains('ynow-lite')) return;
            if (ynowIsAuthed()) return;
            var ids = (window.__YNOW_GATED_DOWNLOAD_IDS && window.__YNOW_GATED_DOWNLOAD_IDS.length)
              ? window.__YNOW_GATED_DOWNLOAD_IDS
              : [
                  'download_report',
                  'download_snapshot',
                  'download_param_restore',
                  'download_defaults',
                  'lab_im_download_report',
                  'bblab-export_data',
                  'bblab-export_chart',
                  'param_audit_pdf_go'
                ];
            var sel = ids.map(function (id) { return '#' + id; }).join(',');
            var node = ev.target && ev.target.closest ? ev.target.closest(sel) : null;
            if (!node) return;
            try { ev.preventDefault(); } catch (ePd) {}
            try { ev.stopPropagation(); } catch (eSp) {}
            try {
              if (typeof window.ynowRequestAuthModal === 'function') {
                window.ynowRequestAuthModal();
              } else if (typeof window.ynowShowAuthModal === 'function') {
                window.ynowShowAuthModal();
              }
            } catch (eModal) {}
          }, true);
          function ynowShowAuthModal() {
            // Already signed in: never reopen login modal (post-login sidebar race).
            if (ynowIsAuthed()) return;
            try {
              if (window.jQuery && jQuery.fn && jQuery.fn.modal) {
                jQuery('#ynow_auth_modal').modal('show');
                return;
              }
            } catch (eShow) {}
            var el = document.getElementById('ynow_auth_modal');
            if (el) {
              el.classList.add('in');
              el.style.display = 'block';
              el.setAttribute('aria-hidden', 'false');
              document.body.classList.add('modal-open');
            }
          }
          window.ynowShowAuthModal = ynowShowAuthModal;
          /* Open login modal from client (collapse sidebar first). Used by the
             Shiny custom message and by a capture-phase click on Sign in so the
             modal does not depend on a server round-trip that can be lost. */
          function ynowApplyAuthModalMode(mode) {
            var authMode = (mode === 'register') ? 'register' : 'signin';
            var modal = document.getElementById('ynow_auth_modal');
            if (modal) modal.setAttribute('data-ynow-auth-mode', authMode);
            var copy = window.__ynowAuthCopy || {};
            var title = document.getElementById('ynow_auth_modal_title');
            if (title) {
              var next = authMode === 'register'
                ? (copy.modal_title_register || copy.register_btn || '')
                : (copy.modal_title || '');
              if (next) title.textContent = next;
            }
            var pw = document.getElementById('ynow_auth_password');
            if (pw) {
              pw.setAttribute('autocomplete', authMode === 'register' ? 'new-password' : 'current-password');
            }
          }
          window.ynowApplyAuthModalMode = ynowApplyAuthModalMode;
          function ynowRequestAuthModal() {
            if (ynowIsAuthed()) return;
            var mode = (arguments.length && arguments[0]) ? arguments[0] : '';
            try { ynowApplyAuthModalMode(mode); } catch (eMode) {}
            try {
              if (typeof window.ynowPersistPreAuthTab === 'function') window.ynowPersistPreAuthTab();
            } catch (eTab) {}
            try { ynowEnsureSidebarCollapsed(); } catch (eCol) {}
            setTimeout(function () {
              try { ynowShowAuthModal(); } catch (eModal) {}
            }, 140);
          }
          window.ynowRequestAuthModal = ynowRequestAuthModal;
          // Veto Bootstrap modal-show after sign-in (shinyBS toggleModal open).
          // NOTE: this JS lives inside an R double-quoted string — never put
          // bare double-quotes in comments here or source(ynow_ui.R) fails.
          if (!window.__ynowAuthModalShowVeto) {
            window.__ynowAuthModalShowVeto = true;
            var vetoShow = function (e) {
              if (!ynowIsAuthed()) return;
              try { if (e && e.preventDefault) e.preventDefault(); } catch (ePd) {}
              try { if (e && e.stopImmediatePropagation) e.stopImmediatePropagation(); } catch (eSip) {}
              try {
                if (window.jQuery) {
                  var $m = jQuery('#ynow_auth_modal');
                  setTimeout(function () {
                    try { $m.modal('hide'); } catch (eH) {}
                  }, 0);
                }
              } catch (eHide) {}
              return false;
            };
            var forceHideIfAuthed = function () {
              if (!ynowIsAuthed()) return;
              try {
                if (window.jQuery) jQuery('#ynow_auth_modal').modal('hide');
              } catch (eH2) {}
            };
            try {
              if (window.jQuery) {
                jQuery(document).on('show.bs.modal', '#ynow_auth_modal', vetoShow);
                jQuery(document).on('shown.bs.modal', '#ynow_auth_modal', forceHideIfAuthed);
              }
            } catch (eBind) {}
            document.addEventListener('show.bs.modal', function (ev) {
              var t = ev && ev.target;
              if (!t || t.id !== 'ynow_auth_modal') return;
              vetoShow(ev);
            }, true);
          }
          if (!window.__ynowAuthOpenCapture) {
            window.__ynowAuthOpenCapture = true;
            document.addEventListener('click', function (ev) {
              var t = ev.target;
              if (!t || !t.closest) return;
              var regHit = t.closest(
                'a.ynow-auth-register-open, .ynow-auth-register-open, #ynow_auth_register_header_label, #ynow_auth_register_home_label'
              );
              if (regHit) {
                var regAuthed = ynowIsAuthed();
                var regDisp = '';
                try { regDisp = window.getComputedStyle(regHit).display || ''; } catch (eRd) {}
                if (regAuthed || regDisp === 'none' || regHit.offsetParent === null) {
                  try { ev.preventDefault(); } catch (ePdR) {}
                  try { ev.stopPropagation(); } catch (eSpR) {}
                  return;
                }
                ynowRequestAuthModal('register');
                return;
              }
              var hit = t.closest(
                'a.ynow-auth-open, .ynow-auth-open, #ynow_pa_gate_signin, #ynow_pa_gate_signin_label, #ynow_lab_gate_signin, #ynow_lab_gate_signin_label, #ynow_chk_gate_signin, #ynow_chk_gate_signin_label'
              );
              if (!hit) return;
              var authed = ynowIsAuthed();
              var disp = '';
              try { disp = window.getComputedStyle(hit).display || ''; } catch (eD) {}
              var invisible = authed || disp === 'none' || hit.offsetParent === null;
              if (invisible) {
                try { ev.preventDefault(); } catch (ePd) {}
                try { ev.stopPropagation(); } catch (eSp) {}
                return;
              }
              ynowRequestAuthModal();
            }, true);
          }
          function ynowClearSidebarGuide() {
            document.querySelectorAll('.ynow-sidebar-guide-pulse').forEach(function (el) {
              el.classList.remove('ynow-sidebar-guide-pulse');
            });
            document.querySelectorAll('.ynow-sidebar-guide-hint').forEach(function (el) {
              el.remove();
            });
          }
          function ynowPulseSidebarMenu(tab, hintText) {
            ynowClearSidebarGuide();
            tab = window.ynowRemapLegacyTab ? window.ynowRemapLegacyTab(tab) : tab;
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]');
            if (!a) return;
            var li = a.closest('li') || a;
            li.classList.add('ynow-sidebar-guide-pulse');
            if (hintText) {
              var hint = document.createElement('small');
              hint.className = 'ynow-sidebar-guide-hint';
              hint.textContent = String(hintText);
              a.appendChild(hint);
            }
            try { li.scrollIntoView({behavior: 'smooth', block: 'nearest'}); } catch (e2) {}
            setTimeout(ynowClearSidebarGuide, 10000);
          }
          function ynowGuideModelDashboard(payload) {
            payload = payload || {};
            var tab = payload.tab || 'get_started';
            var inputId = payload.inputId || 'sgr';
            var lite = document.body && document.body.classList.contains('ynow-lite');
            // Lite hides Model Dashboard; keep toast-only (silent heal covers Lite calc).
            if (lite && tab === 'get_started') return;
            if (payload.openSidebar !== false) ynowEnsureSidebarOpen();
            setTimeout(function () {
              ynowPulseSidebarMenu(tab, payload.menuHint || '');
              ynowGotoAndHighlight(tab, inputId);
            }, 220);
          }
          window.ynowGuideModelDashboard = ynowGuideModelDashboard;
          (function registerGuideModelDashboard() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerGuideModelDashboard, 50);
              return;
            }
            Shiny.addCustomMessageHandler('ynowGuideModelDashboard', ynowGuideModelDashboard);
          })();

          document.addEventListener('click', function (ev) {
            var btn = ev.target && ev.target.closest ? ev.target.closest('.ynow-param-goto') : null;
            if (!btn) return;
            ev.preventDefault();
            ynowGotoAndHighlight(btn.getAttribute('data-tab'), btn.getAttribute('data-input-id'));
          });

          /* ---- Param audit 2A: annotated page screenshots → PDF ---- */
          function ynowLoadScriptOnce(src) {
            return new Promise(function (resolve, reject) {
              if (document.querySelector('script[data-ynow-src=\"' + src + '\"]')) {
                resolve();
                return;
              }
              var s = document.createElement('script');
              s.src = src;
              s.async = true;
              s.setAttribute('data-ynow-src', src);
              s.onload = function () { resolve(); };
              s.onerror = function () { reject(new Error('load fail: ' + src)); };
              document.head.appendChild(s);
            });
          }
          function ynowEnsurePdfLibs() {
            var h2c = 'https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js';
            var jsp = 'https://cdn.jsdelivr.net/npm/jspdf@2.5.2/dist/jspdf.umd.min.js';
            return ynowLoadScriptOnce(h2c).then(function () { return ynowLoadScriptOnce(jsp); });
          }
          function ynowSleep(ms) {
            return new Promise(function (r) { setTimeout(r, ms); });
          }
          function ynowActivateTab(tab) {
            tab = window.ynowRemapLegacyTab ? window.ynowRemapLegacyTab(tab) : tab;
            if (typeof window.ynowShowSidebarFootTab === 'function' &&
                (tab === 'snapshot' || tab === 'testing' || tab === 'feedback')) {
              try { window.ynowShowSidebarFootTab(tab); } catch (eFoot) {}
              return;
            }
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
            }
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('a[data-value=\"' + tab + '\"]');
            if (a) {
              try { a.click(); } catch (e) {}
            }
          }
          function ynowTabPane(tab) {
            tab = window.ynowRemapLegacyTab ? window.ynowRemapLegacyTab(tab) : tab;
            return document.getElementById('shiny-tab-' + tab) ||
                   document.querySelector('.tab-pane[data-value=\"' + tab + '\"]') ||
                   document.querySelector('.tab-content > .tab-pane.active');
          }
          function ynowActivateInnerTab(pane, inner) {
            if (!pane || !inner) return false;
            var key = String(inner).trim();
            if (!key) return false;
            var links = pane.querySelectorAll(
              '.nav-tabs a[data-toggle=\"tab\"], .nav-tabs a[data-bs-toggle=\"tab\"], ' +
              'a[data-toggle=\"tab\"], a[data-bs-toggle=\"tab\"]'
            );
            for (var i = 0; i < links.length; i++) {
              var a = links[i];
              var dv = (a.getAttribute('data-value') || '').trim();
              var href = (a.getAttribute('href') || '').replace(/^#/, '');
              var text = (a.textContent || '').replace(/\\s+/g, ' ').trim();
              if (dv === key || href === key || text === key ||
                  text.indexOf(key) === 0 || key.indexOf(text) === 0) {
                try { a.click(); } catch (eClick) {}
                return true;
              }
            }
            return false;
          }
          async function ynowWaitForCapturePane(tab, maxMs) {
            var deadline = Date.now() + (maxMs || 4000);
            var lazyHost = document.getElementById('ynow_lazy_host_' + tab);
            while (Date.now() < deadline) {
              var pane = ynowTabPane(tab);
              if (pane && pane.classList.contains('active')) {
                if (!lazyHost) return pane;
                if (lazyHost.children && lazyHost.children.length > 0) return pane;
                if (lazyHost.querySelector && lazyHost.querySelector('*')) return pane;
              }
              await ynowSleep(120);
            }
            return ynowTabPane(tab);
          }
          function ynowFindInputBox(inputId) {
            var el = ynowFindInputEl(inputId);
            if (!el) return null;
            return el.closest('.form-group, .shiny-input-container, .radio, .checkbox, .box') || el;
          }
          function ynowAnnotateCanvas(canvas, pane, changes) {
            if (!canvas || !pane || !changes || !changes.length) return canvas;
            var ctx = canvas.getContext('2d');
            var paneRect = pane.getBoundingClientRect();
            var sx = canvas.width / Math.max(pane.scrollWidth || pane.clientWidth || 1, 1);
            var sy = canvas.height / Math.max(pane.scrollHeight || pane.clientHeight || 1, 1);
            changes.forEach(function (ch) {
              var box = ynowFindInputBox(ch.input_id);
              if (!box) return;
              var r = box.getBoundingClientRect();
              var x = (r.left - paneRect.left + pane.scrollLeft) * sx;
              var y = (r.top - paneRect.top + pane.scrollTop) * sy;
              var w = Math.max(r.width * sx, 8);
              var h = Math.max(r.height * sy, 8);
              ctx.save();
              ctx.strokeStyle = '#f39c12';
              ctx.lineWidth = Math.max(2, 3 * sx);
              ctx.strokeRect(x - 4 * sx, y - 4 * sy, w + 8 * sx, h + 8 * sy);
              var label = (ch.label || ch.input_id) + ': ' + (ch.baseline || '—') + ' → ' + (ch.current || '—');
              ctx.font = Math.max(11, Math.round(12 * sx)) + 'px sans-serif';
              ctx.fillStyle = 'rgba(243,156,18,0.92)';
              var tw = ctx.measureText(label).width + 10;
              var ty = Math.max(0, y - 18 * sy);
              ctx.fillRect(x - 4 * sx, ty, tw, 16 * sy);
              ctx.fillStyle = '#111';
              ctx.fillText(label, x, ty + 12 * sy);
              ctx.restore();
            });
            return canvas;
          }
          function ynowAddCanvasToPdf(pdf, canvas, title) {
            var pageW = pdf.internal.pageSize.getWidth();
            var pageH = pdf.internal.pageSize.getHeight();
            var margin = 28;
            var maxW = pageW - margin * 2;
            var maxH = pageH - margin * 2 - 24;
            var imgW = maxW;
            var imgH = canvas.height * (imgW / canvas.width);
            if (imgH > maxH) {
              imgH = maxH;
              imgW = canvas.width * (imgH / canvas.height);
            }
            pdf.setFontSize(11);
            pdf.setTextColor(40);
            if (title) pdf.text(String(title), margin, margin - 8);
            var data = canvas.toDataURL('image/jpeg', 0.92);
            pdf.addImage(data, 'JPEG', margin, margin + 4, imgW, imgH);
          }
          async function ynowRunParamAuditPdf(payload) {
            var status = document.getElementById('ynow_param_audit_pdf_status');
            var setStatus = function (t) { if (status) status.textContent = t || ''; };
            var strings = (payload && payload.strings) || {};
            try {
              setStatus(strings.busy || 'Capturing…');
              await ynowEnsurePdfLibs();
              var html2canvasFn = window.html2canvas;
              var jsPDF = (window.jspdf && window.jspdf.jsPDF) ? window.jspdf.jsPDF : window.jsPDF;
              if (!html2canvasFn || !jsPDF) throw new Error('libs missing');
              var pageSpecs = (payload && payload.pages) || [];
              var tabs = (payload && payload.tabs) || [];
              var changes = (payload && payload.changes) || [];
              var pageTitles = (payload && payload.page_titles) || {};
              if (!pageSpecs.length && tabs.length) {
                pageSpecs = tabs.map(function (t) {
                  return { id: t, tab: t, inner: '', annotate_tab: t, title: pageTitles[t] || t };
                });
              }
              if (!pageSpecs.length) {
                setStatus(strings.need_pages || 'Select pages');
                return;
              }
              var pdf = new jsPDF({ orientation: 'portrait', unit: 'pt', format: 'a4' });
              var margin = 40;
              pdf.setFontSize(16);
              pdf.text(payload.title || 'Parameter adjustments PDF', margin, 56);
              pdf.setFontSize(11);
              pdf.text((payload.ticker || '') + '  ·  ' + (payload.baseline_at || ''), margin, 78);
              pdf.setFontSize(10);
              var y = 100;
              pdf.text(strings.summary || 'Adjusted parameters:', margin, y);
              y += 16;
              if (!changes.length) {
                pdf.text(strings.no_changes || '(No manual adjustments vs baseline)', margin, y);
              } else {
                changes.forEach(function (ch) {
                  var line = (ch.section || '') + ' / ' + (ch.label || ch.input_id) +
                    ': ' + (ch.baseline || '—') + ' → ' + (ch.current || '—');
                  var lines = pdf.splitTextToSize(line, pdf.internal.pageSize.getWidth() - margin * 2);
                  if (y + lines.length * 12 > pdf.internal.pageSize.getHeight() - 40) {
                    pdf.addPage();
                    y = 48;
                  }
                  pdf.text(lines, margin, y);
                  y += lines.length * 12 + 4;
                });
              }
              var lazyTabs = { bluechip: 1, hfv: 1, lab_notes: 1, company_advance: 1,
                               macro_market: 1, asset_transmission: 1, testing: 1 };
              for (var i = 0; i < pageSpecs.length; i++) {
                var spec = pageSpecs[i] || {};
                var tab = spec.tab || spec.id || '';
                var inner = spec.inner || '';
                var annotateTab = spec.annotate_tab || tab;
                var title = spec.title || pageTitles[spec.id] || pageTitles[tab] || tab;
                setStatus((strings.busy || 'Capturing…') + ' (' + (i + 1) + '/' + pageSpecs.length + ')');
                ynowActivateTab(tab);
                var waitMs = lazyTabs[tab] ? 4500 : 900;
                var pane = await ynowWaitForCapturePane(tab, waitMs);
                if (!pane) continue;
                if (inner) {
                  ynowActivateInnerTab(pane, inner);
                  await ynowSleep(400);
                } else {
                  await ynowSleep(200);
                }
                try { pane.scrollTop = 0; } catch (e0) {}
                var canvas = await html2canvasFn(pane, {
                  scale: 1,
                  useCORS: true,
                  logging: false,
                  backgroundColor: '#ffffff',
                  windowWidth: Math.max(pane.scrollWidth, pane.clientWidth || 800),
                  height: Math.min(pane.scrollHeight || pane.clientHeight || 800, 5000)
                });
                var tabChanges = changes.filter(function (c) {
                  var ct = c.tab || '';
                  if (window.ynowRemapLegacyTab) ct = window.ynowRemapLegacyTab(ct);
                  return ct === annotateTab || ct === tab ||
                         (annotateTab === 'bluechip' && (c.tab === 'bluechip_lab' || ct === 'bluechip'));
                });
                ynowAnnotateCanvas(canvas, pane, tabChanges);
                pdf.addPage();
                ynowAddCanvasToPdf(pdf, canvas, title);
              }
              ynowActivateTab('snapshot');
              await ynowSleep(200);
              var fname = (payload.filename || 'YNow_param_audit') + '.pdf';
              pdf.save(fname);
              setStatus(strings.done || 'PDF downloaded.');
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('param_audit_pdf_done', Date.now(), {priority: 'event'});
              }
            } catch (err) {
              console.error(err);
              setStatus(strings.err || 'PDF capture failed.');
              try { ynowActivateTab('snapshot'); } catch (e2) {}
            }
          }
          function registerParamAuditPdfHandler() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerParamAuditPdfHandler, 50);
              return;
            }
            Shiny.addCustomMessageHandler('ynowParamAuditPdf', function (payload) {
              ynowRunParamAuditPdf(payload || {});
            });
          }
          registerParamAuditPdfHandler();
        })();
      ")),

      # Value alert (到值通知): right-click / long-press → quick set; bell mark
      tags$style(HTML("
        .ynow-alertable {
          position: relative;
          -webkit-touch-callout: none;
        }
        .ynow-alertable > .info-box,
        .ynow-alertable > .small-box,
        .ynow-alertable > .ynow-smart-card,
        .ynow-alertable > .ynow-fs-card,
        .ynow-alertable > .ynow-mscore-widget,
        .ynow-alertable > .ynow-eq-score-pill,
        .ynow-alertable > .ynow-composite-band-card,
        .ynow-alertable > .ynow-macro-kpi {
          height: 100%;
        }
        .ynow-composite-valuation > div:first-child > .ynow-alertable {
          flex: 1 1 120px;
          min-width: 120px;
          display: flex;
          flex-direction: column;
        }
        .ynow-composite-valuation .ynow-alertable > .ynow-composite-band-card {
          flex: 1 1 auto;
        }
        .ynow-macro-kpi-row .ynow-alertable {
          display: block;
          height: 100%;
        }
        /* 到值通知標示：預設無色半透明黑（淺底）；深色 Macro KPI 另用淺色鈴鐺 */
        .ynow-alert-bell {
          position: absolute;
          top: 6px;
          right: 8px;
          z-index: 6;
          width: 16px;
          height: 16px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          color: rgba(0, 0, 0, 0.35);
          opacity: 1;
          pointer-events: none;
        }
        .ynow-alertable--ready .ynow-alert-bell,
        .ynow-alertable--ready:hover .ynow-alert-bell,
        .ynow-alertable--blocked .ynow-alert-bell {
          color: rgba(0, 0, 0, 0.35);
        }
        /* Macro dark ink cards: black bells were invisible on #0b1220 */
        .ynow-macro-kpi-row .ynow-alert-bell,
        .ynow-alertable:has(> .ynow-macro-kpi) .ynow-alert-bell {
          color: rgba(255, 255, 255, 0.55);
        }
        .ynow-macro-kpi-row .ynow-alertable--ready .ynow-alert-bell,
        .ynow-macro-kpi-row .ynow-alertable--ready:hover .ynow-alert-bell,
        .ynow-macro-kpi-row .ynow-alertable--blocked .ynow-alert-bell,
        .ynow-alertable:has(> .ynow-macro-kpi).ynow-alertable--ready .ynow-alert-bell,
        .ynow-alertable:has(> .ynow-macro-kpi).ynow-alertable--blocked .ynow-alert-bell {
          color: rgba(255, 255, 255, 0.55);
        }
        /* Model Dashboard / valueBox: keep bell above AdminLTE icon chrome */
        .ynow-alertable:has(> .small-box) .ynow-alert-bell,
        .ynow-alertable:has(> .info-box) .ynow-alert-bell {
          z-index: 8;
          top: 8px;
          right: 10px;
        }
        .ynow-alertable--ready {
          cursor: context-menu;
        }
        /* Macro index cards stay left-clickable; still advertise 到值通知 via context-menu */
        .ynow-alertable--ready > .ynow-macro-kpi--clickable {
          cursor: context-menu;
        }
        .ynow-alertable--ready:hover {
          box-shadow: 0 0 0 1px rgba(12, 84, 132, 0.25);
        }
        .ynow-macro-kpi-row .ynow-alertable--ready:hover {
          box-shadow: 0 0 0 1px rgba(201, 162, 39, 0.55);
        }
        .ynow-alertable--blocked {
          cursor: not-allowed;
        }
        .ynow-alertable--blocked::after {
          content: '';
          pointer-events: none;
        }
        .ynow-pa-modal {
          margin: -8px -4px 0 -4px;
        }
        .ynow-pa-modal__hero {
          background: linear-gradient(145deg, #0f172a 0%, #1e3a5f 55%, #0c5484 100%);
          color: #f8fafc;
          border-radius: 10px;
          padding: 16px 18px 14px;
          margin-bottom: 14px;
        }
        .ynow-pa-modal__eyebrow {
          display: flex;
          align-items: center;
          gap: 8px;
          font-size: 12px;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          opacity: 0.85;
          margin-bottom: 8px;
        }
        .ynow-pa-modal__bell {
          display: inline-flex;
          color: rgba(255, 255, 255, 0.45);
          opacity: 1;
        }
        .ynow-pa-modal__ticker {
          font-size: 22px;
          font-weight: 700;
          letter-spacing: 0.02em;
          line-height: 1.2;
        }
        .ynow-pa-modal__metric {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 8px 14px;
          margin-top: 8px;
        }
        .ynow-pa-modal__metric-label {
          font-size: 13px;
          opacity: 0.8;
        }
        .ynow-pa-modal__metric-value {
          font-size: 28px;
          font-weight: 700;
          font-variant-numeric: tabular-nums;
        }
        .ynow-pa-modal__hint {
          margin: 10px 0 0 0;
          font-size: 12px;
          line-height: 1.45;
          opacity: 0.78;
        }
        .ynow-pa-modal__body .form-group {
          margin-bottom: 12px;
        }
        .ynow-pa-modal__section {
          margin: 4px 0 6px 0;
          font-size: 12px;
          font-weight: 600;
          color: #334155;
          letter-spacing: 0.02em;
        }
        .ynow-pa-modal__channels {
          background: #f1f5f9;
          border-radius: 8px;
          padding: 10px 12px 4px;
          margin-bottom: 10px;
        }
        .ynow-pa-modal__save {
          min-width: 120px;
        }
        .modal-footer {
          border-top: 1px solid #e2e8f0;
        }
      ")),
      tags$script(HTML("
        (function () {
          var LONG_MS = 520;
          var timer = null;
          var startX = 0;
          var startY = 0;
          var moved = false;

          function closestAlertable(el) {
            if (!el || !el.closest) return null;
            return el.closest('[data-ynow-alert=\"1\"]');
          }

          function payloadFrom(el) {
            if (!el) return null;
            return {
              ready: el.getAttribute('data-ynow-alert-ready') || '0',
              metric: el.getAttribute('data-ynow-alert-metric') || '',
              ticker: el.getAttribute('data-ynow-alert-ticker') || '',
              value: el.getAttribute('data-ynow-alert-value') || '',
              label: el.getAttribute('data-ynow-alert-label') || '',
              kind: el.getAttribute('data-ynow-alert-kind') || 'price_gte',
              fair: el.getAttribute('data-ynow-alert-fair') || '',
              nonce: Date.now()
            };
          }

          function openOrBlock(el) {
            var payload = payloadFrom(el);
            if (!payload) return;
            if (!(window.Shiny && Shiny.setInputValue)) return;
            if (payload.ready !== '1' || !payload.value) {
              Shiny.setInputValue('ynow_kpi_alert_blocked', payload, {priority: 'event'});
              return;
            }
            Shiny.setInputValue('ynow_kpi_alert_open', payload, {priority: 'event'});
          }

          document.addEventListener('contextmenu', function (ev) {
            var el = closestAlertable(ev.target);
            if (!el) return;
            ev.preventDefault();
            openOrBlock(el);
          }, true);

          document.addEventListener('touchstart', function (ev) {
            var el = closestAlertable(ev.target);
            if (!el) return;
            if (!ev.touches || !ev.touches.length) return;
            moved = false;
            startX = ev.touches[0].clientX;
            startY = ev.touches[0].clientY;
            timer = setTimeout(function () {
              timer = null;
              if (moved) return;
              openOrBlock(el);
            }, LONG_MS);
          }, {passive: true, capture: true});

          document.addEventListener('touchmove', function (ev) {
            if (!timer) return;
            if (!ev.touches || !ev.touches.length) return;
            var dx = Math.abs(ev.touches[0].clientX - startX);
            var dy = Math.abs(ev.touches[0].clientY - startY);
            if (dx > 12 || dy > 12) {
              moved = true;
              clearTimeout(timer);
              timer = null;
            }
          }, {passive: true, capture: true});

          document.addEventListener('touchend', function () {
            if (timer) {
              clearTimeout(timer);
              timer = null;
            }
          }, {passive: true, capture: true});

          document.addEventListener('touchcancel', function () {
            if (timer) {
              clearTimeout(timer);
              timer = null;
            }
          }, {passive: true, capture: true});
        })();
      ")),
      
      tags$style(HTML("
        .selectize-dropdown-content {
          max-height: 300px !important;
          overflow-y: auto !important;
        }
        .selectize-dropdown {
          max-height: 300px !important;
        }

        /* Param audit highlight pulse */
        .ynow-param-highlight {
          outline: 3px solid #f39c12 !important;
          outline-offset: 3px;
          box-shadow: 0 0 0 4px rgba(243, 156, 18, 0.35) !important;
          border-radius: 4px;
          transition: box-shadow 0.2s ease;
        }

        /* g ≥ WACC／Ke: pulse Model Dashboard in the sidebar */
        .sidebar-menu > li.ynow-sidebar-guide-pulse > a {
          outline: 2px solid #f39c12 !important;
          outline-offset: -2px;
          box-shadow: inset 0 0 0 9999px rgba(243, 156, 18, 0.18) !important;
          animation: ynow-sidebar-guide-pulse 1.1s ease-in-out 3;
        }
        .sidebar-menu > li.ynow-sidebar-guide-pulse > a .ynow-sidebar-guide-hint {
          display: block;
          margin-top: 2px;
          font-size: 10.5px;
          font-weight: 700;
          color: #f39c12;
          line-height: 1.25;
          white-space: normal;
        }
        @keyframes ynow-sidebar-guide-pulse {
          0%, 100% { box-shadow: inset 0 0 0 9999px rgba(243, 156, 18, 0.12); }
          50% { box-shadow: inset 0 0 0 9999px rgba(243, 156, 18, 0.32); }
        }

        /* Ticker typeahead suggest (Ticker/Stock Code + Radar focus): 黑字白底 */
        .ynow-ticker-suggest,
        #sc_ticker_suggest,
        #lab_cluster_focus_suggest {
          position: absolute;
          z-index: 2000;
          left: 0;
          right: 0;
          top: 100%;
          margin-top: 2px;
          max-height: 260px;
          overflow-y: auto;
          background: #ffffff;
          border: 1px solid #cccccc;
          border-radius: 4px;
          box-shadow: 0 4px 10px rgba(0,0,0,0.12);
          display: none;
        }
        .ynow-ticker-suggest .ynow-suggest-item,
        #sc_ticker_suggest .ynow-suggest-item,
        #lab_cluster_focus_suggest .ynow-suggest-item {
          display: block;
          width: 100%;
          padding: 8px 12px;
          color: #000000 !important;
          background: #ffffff;
          border: 0;
          border-bottom: 1px solid #eeeeee;
          text-align: left;
          font-size: 13px;
          cursor: pointer;
        }
        .ynow-ticker-suggest .ynow-suggest-item:hover,
        .ynow-ticker-suggest .ynow-suggest-item:focus,
        #sc_ticker_suggest .ynow-suggest-item:hover,
        #sc_ticker_suggest .ynow-suggest-item:focus,
        #lab_cluster_focus_suggest .ynow-suggest-item:hover,
        #lab_cluster_focus_suggest .ynow-suggest-item:focus {
          background: #f2f2f2;
          color: #000000 !important;
          outline: none;
        }
        .ynow-ticker-suggest .ynow-suggest-sym,
        #sc_ticker_suggest .ynow-suggest-sym,
        #lab_cluster_focus_suggest .ynow-suggest-sym {
          font-weight: 700;
          color: #000000;
          margin-right: 8px;
        }
        .ynow-ticker-suggest .ynow-suggest-lab,
        #sc_ticker_suggest .ynow-suggest-lab,
        #lab_cluster_focus_suggest .ynow-suggest-lab {
          color: #222222;
          font-weight: 400;
        }
        .ynow-sc-row {
          display: flex;
          flex-direction: row;
          flex-wrap: nowrap;
          align-items: flex-end;
          gap: 10px;
          max-width: 420px;
          margin-bottom: 8px;
        }
        .ynow-sc-wrap {
          position: relative;
          flex: 1 1 auto;
          width: 260px;
          max-width: 280px;
          min-width: 0;
        }
        .ynow-sc-wrap .form-group {
          margin-bottom: 0 !important;
        }
        .ynow-sc-wrap .control-label {
          display: block;
          margin-bottom: 5px;
        }
        .btn.ynow-btn-is-busy,
        #search.ynow-btn-is-busy {
          opacity: 0.92;
          cursor: progress;
        }
        .ynow-sc-row #search.ynow-search-btn,
        #search.ynow-search-btn {
          flex: 0 0 auto;
          align-self: flex-end;
          height: 34px;
          padding: 6px 14px;
          margin: 0 0 0 0;
          background: #111111 !important;
          border: 1px solid #000000 !important;
          color: #F5C518 !important;
          font-weight: 700;
          line-height: 1.2;
          box-shadow: none;
        }
        .ynow-sc-row #search.ynow-search-btn:hover,
        .ynow-sc-row #search.ynow-search-btn:focus,
        #search.ynow-search-btn:hover,
        #search.ynow-search-btn:focus {
          background: #000000 !important;
          border-color: #000000 !important;
          color: #C9A227 !important;
        }
        .ynow-sc-row #search.ynow-search-btn .fa,
        .ynow-sc-row #search.ynow-search-btn .fas,
        #search.ynow-search-btn .fa,
        #search.ynow-search-btn .fas {
          color: #F5C518 !important;
        }
        @media (max-width: 480px) {
          .ynow-sc-row {
            max-width: 100%;
          }
          .ynow-sc-wrap {
            width: auto;
            max-width: calc(100% - 100px);
          }
        }
        
        /* 大數字框（valueBox / infoBox）統一響應式：換行、縮字、避免溢出
           （還原 v16.42：手機勿再壓成 33% 三欄；不用 [class*=…] 屬性選擇器） */
        .content-wrapper .row > .col-xs-12,
        .content-wrapper .row > .col-sm-2,
        .content-wrapper .row > .col-sm-3,
        .content-wrapper .row > .col-sm-4,
        .content-wrapper .row > .col-sm-6,
        .content-wrapper .row > .col-sm-12 {
          min-width: 0;
        }
        .content-wrapper .small-box,
        .content-wrapper .info-box,
        .tab-content .small-box,
        .tab-content .info-box {
          min-width: 0;
          max-width: 100%;
          width: 100%;
          box-sizing: border-box;
          overflow: hidden;
        }
        /* Same-row KPI boxes stretch to equal height (valueBoxOutput nests col-sm-* on uiOutput).
           Attr selectors use single quotes because this CSS block is inside a double-quoted HTML string. */
        .content-wrapper .row:has(> [class*='col-'] .small-box),
        .tab-content .row:has(> [class*='col-'] .small-box),
        .tab-pane .row:has(> [class*='col-'] .small-box) {
          display: flex;
          flex-wrap: wrap;
          align-items: stretch;
        }
        .content-wrapper .row:has(> [class*='col-'] .small-box) > [class*='col-'],
        .tab-content .row:has(> [class*='col-'] .small-box) > [class*='col-'],
        .tab-pane .row:has(> [class*='col-'] .small-box) > [class*='col-'] {
          display: flex;
          flex-direction: column;
          float: none;
        }
        .content-wrapper .row:has(> [class*='col-'] .small-box) > [class*='col-'] > .shiny-html-output,
        .tab-content .row:has(> [class*='col-'] .small-box) > [class*='col-'] > .shiny-html-output,
        .tab-pane .row:has(> [class*='col-'] .small-box) > [class*='col-'] > .shiny-html-output,
        .content-wrapper .row:has(> [class*='col-'] .small-box) > [class*='col-'] > [class*='col-'],
        .tab-content .row:has(> [class*='col-'] .small-box) > [class*='col-'] > [class*='col-'],
        .tab-pane .row:has(> [class*='col-'] .small-box) > [class*='col-'] > [class*='col-'] {
          flex: 1 1 auto;
          width: 100%;
          max-width: 100%;
          height: 100%;
          display: flex;
          flex-direction: column;
          float: none;
          padding-left: 0;
          padding-right: 0;
        }
        .content-wrapper .row:has(> [class*='col-'] .small-box) .small-box,
        .tab-content .row:has(> [class*='col-'] .small-box) .small-box,
        .tab-pane .row:has(> [class*='col-'] .small-box) .small-box {
          flex: 1 1 auto;
          width: 100%;
          height: 100%;
        }
        /* Global boxes: keep original alignment; numerals much larger than labels. */
        .content-wrapper .small-box .inner {
          min-width: 0;
          padding-right: 44px;
          flex: 1 1 auto;
        }
        .content-wrapper .small-box .inner h3,
        .content-wrapper .small-box .inner h3 * {
          font-size: clamp(18px, 2.6vw + 0.45rem, 36px) !important;
          font-weight: 800;
          line-height: 1.15 !important;
          font-variant-numeric: tabular-nums;
          letter-spacing: -0.02em;
          /* Keep the whole figure on one line — never orphan a trailing digit. */
          white-space: nowrap !important;
          overflow-wrap: normal;
          word-break: keep-all;
          overflow: hidden;
          text-overflow: ellipsis;
          max-width: 100%;
        }
        .content-wrapper .small-box .inner p {
          font-size: clamp(10px, 1.1vw, 12px) !important;
          white-space: normal;
          overflow-wrap: anywhere;
          line-height: 1.25;
        }
        .content-wrapper .info-box-content {
          min-width: 0;
          overflow: hidden;
        }
        .content-wrapper .info-box .info-box-number,
        .content-wrapper .info-box .info-box-number h3 {
          font-size: clamp(16px, 2.2vw + 0.35rem, 30px) !important;
          font-weight: 800;
          line-height: 1.15 !important;
          font-variant-numeric: tabular-nums;
          white-space: nowrap !important;
          overflow-wrap: normal;
          word-break: keep-all;
          overflow: hidden;
          text-overflow: ellipsis;
          max-width: 100%;
        }
        .content-wrapper .info-box .info-box-text {
          font-size: clamp(10px, 1.1vw, 12px) !important;
          white-space: normal;
          overflow-wrap: anywhere;
        }
        /* 平板：較窄欄的彩色 BOX 兩欄並排（header KPI 列維持 1:1:1） */
        @media (max-width: 991px) {
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-2:has(.small-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-3:has(.small-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-4:has(.small-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-2:has(.info-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-3:has(.info-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-4:has(.info-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-2:has(.small-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-3:has(.small-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-4:has(.small-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-2:has(.info-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-3:has(.info-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-4:has(.info-box) {
            width: 50% !important;
            float: left !important;
            clear: none;
          }
        }
        /* 手機：頁籤／內容區彩色 BOX 直向全寬（勿再壓成 33% 三欄）
           例外：.ynow-header-kpi-row 維持 LAST PRICE／MARKET CAP／EPS 1:1:1 */
        @media (max-width: 767px) {
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-xs-12:has(.small-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-2:has(.small-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-3:has(.small-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-4:has(.small-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-6:has(.small-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-12:has(.small-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-xs-12:has(.info-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-2:has(.info-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-3:has(.info-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-4:has(.info-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-6:has(.info-box),
          .content-wrapper .row:not(.ynow-header-kpi-row) > .col-sm-12:has(.info-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-xs-12:has(.small-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-2:has(.small-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-3:has(.small-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-4:has(.small-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-6:has(.small-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-12:has(.small-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-xs-12:has(.info-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-2:has(.info-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-3:has(.info-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-4:has(.info-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-6:has(.info-box),
          .tab-content .row:not(.ynow-header-kpi-row) > .col-sm-12:has(.info-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-xs-12:has(.small-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-sm-2:has(.small-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-sm-3:has(.small-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-sm-4:has(.small-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-sm-6:has(.small-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-sm-12:has(.small-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-xs-12:has(.info-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-sm-2:has(.info-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-sm-3:has(.info-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-sm-4:has(.info-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-sm-6:has(.info-box),
          .tab-pane .row:not(.ynow-header-kpi-row) > .col-sm-12:has(.info-box) {
            width: 100% !important;
            max-width: 100% !important;
            float: none !important;
            display: block !important;
            clear: both !important;
            margin-left: 0 !important;
            margin-right: 0 !important;
          }
          .content-wrapper .small-box,
          .content-wrapper .info-box,
          .tab-content .small-box,
          .tab-content .info-box {
            margin-bottom: 10px !important;
          }
          .content-wrapper .small-box .inner {
            padding-right: 12px;
            padding-left: 12px;
          }
          .content-wrapper .small-box .icon {
            display: none;
          }
          .content-wrapper .small-box .inner h3,
          .content-wrapper .small-box .inner h3 * {
            font-size: clamp(20px, 6vw, 28px) !important;
          }
          .content-wrapper .small-box .inner p {
            font-size: clamp(10px, 2.6vw, 12px) !important;
          }
          .content-wrapper .info-box {
            min-height: 0;
            height: auto !important;
          }
          .content-wrapper .info-box .info-box-icon {
            width: 48px;
            height: 48px;
            font-size: 20px;
            line-height: 48px;
          }
          .content-wrapper .info-box .info-box-content {
            margin-left: 48px;
            padding: 6px 10px 6px 12px;
          }
          .content-wrapper .info-box .info-box-number,
          .content-wrapper .info-box .info-box-number h3 {
            font-size: clamp(18px, 5.2vw, 24px) !important;
          }
          .content-wrapper .info-box .info-box-text {
            font-size: clamp(10px, 2.6vw, 12px) !important;
          }
        }

        /* Finance Summary 卡片網格 */
        .ynow-fs-wrap {
          margin-bottom: 14px;
        }
        .ynow-fs-section {
          margin-bottom: 16px;
        }
        .ynow-fs-section-title {
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.06em;
          text-transform: uppercase;
          color: #666666;
          margin: 0 0 8px 0;
          padding-bottom: 4px;
          border-bottom: 1px solid #e5e5e5;
        }
        .ynow-fs-grid {
          display: grid;
          grid-template-columns: repeat(5, minmax(0, 1fr));
          gap: 10px;
        }
        @media (max-width: 992px) {
          .ynow-fs-grid { grid-template-columns: repeat(3, minmax(0, 1fr)); }
        }
        /* 手機：兩兩並排（FINANCIAL REPORT Finance Summary 卡片） */
        @media (max-width: 767px) {
          .ynow-fs-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 8px; }
        }
        .ynow-fs-card {
          background: linear-gradient(165deg, #fafafa 0%, #f0f0f0 100%);
          border: 1px solid #e0e0e0;
          border-left: 3px solid #222222;
          border-radius: 4px;
          padding: 10px 12px;
          min-height: 64px;
          display: flex;
          flex-direction: column;
          justify-content: center;
          transition: border-color 0.15s ease, background 0.15s ease;
        }
        .ynow-fs-card:hover {
          background: #ffffff;
          border-left-color: #555555;
        }
        .ynow-fs-label {
          font-size: 11px;
          font-weight: 600;
          color: #777777;
          line-height: 1.25;
          margin-bottom: 4px;
        }
        .ynow-fs-value {
          font-size: clamp(16px, 3vw, 22px);
          font-weight: 800;
          color: #111111;
          font-variant-numeric: tabular-nums;
          letter-spacing: -0.01em;
          line-height: 1.2;
          word-break: break-word;
          overflow-wrap: anywhere;
        }
        /* 財報屬性「重視指標」：金色外框（取代圓點）；與 APP 標題金色一致 */
        .ynow-focus-metric-swatch {
          display: inline-block;
          width: 14px;
          height: 14px;
          margin: 0 6px 0 0;
          border: 2px solid var(--ynow-gold, #F5C518);
          border-radius: 3px;
          background: transparent;
          vertical-align: middle;
          box-sizing: border-box;
        }
        .ynow-focus-metric-swatch--legend {
          margin: 0 6px 0 0;
        }
        /* Focus KPI gold frame: thicker + pulse (do not reuse title gold-shine) */
        @keyframes ynow-focus-gold-pulse {
          0%, 100% {
            border-color: var(--ynow-gold, #F5C518);
            box-shadow: 0 0 0 1px rgba(201, 162, 39, 0.28), 0 0 0 0 rgba(245, 197, 24, 0);
          }
          50% {
            border-color: var(--ynow-gold-deep, #C9A227);
            box-shadow: 0 0 0 2px rgba(201, 162, 39, 0.55), 0 0 10px 1px rgba(245, 197, 24, 0.38);
          }
        }
        .content-wrapper .small-box.ynow-focus-metric-box,
        .tab-content .small-box.ynow-focus-metric-box,
        .ynow-fs-card.ynow-focus-metric-box {
          border: 3px solid var(--ynow-gold, #F5C518) !important;
          box-shadow: 0 0 0 1px rgba(201, 162, 39, 0.28);
          animation: ynow-focus-gold-pulse 1.6s ease-in-out infinite;
        }
        .ynow-fs-card.ynow-focus-metric-box {
          border-radius: 6px;
        }
        /* Legacy dot class hidden if any residual markup remains */
        .ynow-focus-metric-dot {
          display: none !important;
        }
        .ynow-kpi-legend-wrap {
          margin: 0 0 10px 0;
        }
        .ynow-focus-metric-note {
          margin: 6px 2px 0 2px;
          padding: 0;
          border: none;
          background: transparent;
          font-size: 12px;
          line-height: 1.35;
          color: #666666;
          font-weight: 500;
        }
        /* Yahoo Sector/Industry 列：屬性標籤靠右 */
        .ynow-ind-yahoo-row {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 10px 14px;
          margin-top: 4px;
          font-size: 12px;
          color: #777;
          line-height: 1.45;
          flex-wrap: wrap;
        }
        .ynow-ind-yahoo-text {
          min-width: 0;
          flex: 1 1 auto;
        }

        /* 財報屬性標籤：黑底＋金框；解析後標籤字用藍綠動態（Mature dividend 等） */
        .ynow-fund-profile-badge {
          display: inline-flex;
          align-items: center;
          gap: 0;
          margin-left: auto;
          padding: 3px 10px;
          border-radius: 4px;
          border: 1px solid var(--ynow-gold, #F5C518) !important;
          background: #000000 !important;
          color: var(--ynow-gold, #F5C518) !important;
          font-size: 12px;
          font-weight: 700;
          white-space: nowrap;
          line-height: 1.2;
        }
        /* Resolved profiles: logo-flow blue–green text (not HTCDI flame). */
        .ynow-fund-profile-badge .ynow-htcdi-flow {
          color: transparent !important;
          -webkit-text-fill-color: transparent !important;
        }
        .ynow-fund-profile-badge .ynow-focus-metric-dot {
          display: none !important;
        }
        @media (max-width: 767px) {
          .ynow-ind-yahoo-row {
            align-items: flex-start;
          }
          .ynow-fund-profile-badge {
            margin-left: 0;
          }
        }
        /* Snapshot / HFV 摘要數字：維持可覆寫 class，尺寸回預設 */
        .ynow-kpi-stat-label {
          font-size: 11px !important;
          color: #666 !important;
          line-height: 1.25 !important;
          margin: 0 !important;
        }
        .ynow-kpi-stat-value {
          font-size: clamp(18px, 3.6vw, 24px) !important;
          font-weight: 800 !important;
          line-height: 1.2 !important;
          margin: 0 !important;
          font-variant-numeric: tabular-nums;
          overflow-wrap: anywhere;
        }
        .ynow-kpi-stat-note {
          font-size: 10px !important;
          color: #888 !important;
          margin-top: 2px !important;
          line-height: 1.25 !important;
        }
        .ynow-kpi-stat-params {
          font-size: 12px !important;
          line-height: 1.4 !important;
        }
        /* HFV：此刻參數 — 圖表正下方、全寬、鍵值網格（勿擠成一行） */
        .ynow-hfv-session-params {
          width: 100%;
          box-sizing: border-box;
          margin: 14px 0 0 0;
          padding: 12px 14px 14px 14px;
          background: #fafafa;
          border: 1px solid #e6e6e6;
          border-left: 4px solid var(--ynow-gold-deep, #C9A227);
          border-radius: 4px;
        }
        .ynow-hfv-session-params__title {
          display: flex;
          align-items: center;
          gap: 8px;
          margin: 0 0 10px 0;
          font-size: 13px;
          font-weight: 700;
          color: var(--ynow-gold-ink, #856404);
          letter-spacing: 0.01em;
        }
        .ynow-hfv-session-params__title > .fa,
        .ynow-hfv-session-params__title > .fas {
          color: var(--ynow-gold-deep, #C9A227);
          width: 1.15em;
          text-align: center;
        }
        .ynow-hfv-session-params__grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(148px, 1fr));
          gap: 8px 12px;
        }
        .ynow-hfv-session-params__item {
          min-width: 0;
          padding: 7px 10px;
          background: #fff;
          border: 1px solid #ececec;
          border-radius: 4px;
        }
        .ynow-hfv-session-params__key {
          display: block;
          font-size: 10.5px;
          font-weight: 600;
          color: #777;
          letter-spacing: 0.02em;
          margin: 0 0 3px 0;
          line-height: 1.25;
        }
        .ynow-hfv-session-params__val {
          display: block;
          font-size: 13px;
          font-weight: 600;
          color: #1a1a1a;
          font-variant-numeric: tabular-nums;
          line-height: 1.35;
          word-break: break-word;
          overflow-wrap: anywhere;
        }
        .ynow-hfv-session-params__span {
          grid-column: 1 / -1;
        }
        @media (max-width: 767px) {
          .ynow-hfv-session-params {
            margin-top: 10px;
            padding: 10px 10px 12px 10px;
          }
          .ynow-hfv-session-params__grid {
            grid-template-columns: repeat(2, minmax(0, 1fr));
            gap: 6px 8px;
          }
          .ynow-hfv-session-params__val {
            font-size: 12px;
          }
        }
        .ynow-kpi-hero-value {
          font-size: clamp(16px, 4vw, 22px) !important;
          font-weight: 700 !important;
          line-height: 1.2 !important;
          overflow-wrap: anywhere;
        }

        /* KPI：φ⁻¹ 等比例縮小 + 一列五個左排 */
        .ynow-kpi-grid {
          display: flex;
          flex-wrap: wrap;
          justify-content: flex-start;
          align-items: stretch;
          margin-left: -4px;
          margin-right: -4px;
          clear: both;
        }
        .ynow-kpi-grid > * {
          width: 20% !important;
          max-width: 20% !important;
          flex: 0 0 20%;
          float: none !important;
          padding-left: 4px;
          padding-right: 4px;
          box-sizing: border-box;
        }
        @media (max-width: 992px) {
          .ynow-kpi-grid > * {
            width: 33.333% !important;
            max-width: 33.333% !important;
            flex-basis: 33.333%;
          }
        }
        @media (max-width: 767px) {
          /* FINANCIAL REPORT／Dashboard：KPI 框格兩兩並排（勿三欄過窄） */
          .ynow-kpi-grid > * {
            width: 50% !important;
            max-width: 50% !important;
            flex-basis: 50%;
          }
          .ynow-kpi-grid .small-box .inner h3 {
            font-size: clamp(18px, 5vw, 26px) !important;
          }
          .ynow-kpi-grid .small-box .inner p {
            font-size: clamp(9px, 2.4vw, 11px) !important;
          }
          .ynow-kpi-grid .small-box .icon-large {
            display: none !important;
          }
        }
        .ynow-kpi-grid .small-box {
          aspect-ratio: 1.618 / 1 !important;
          display: flex !important;
          flex-direction: column !important;
          justify-content: center !important;
          align-items: stretch !important;
          float: none !important;
          width: 100% !important;
          min-height: 74px !important;
          height: auto !important;
          border-radius: 5px !important;
          margin-bottom: 9px !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05) !important;
        }
        /* N/A / 未設產業區間：白（KPI 僅黑白紅藍） */
        .ynow-kpi-grid .small-box.ynow-kpi-na {
          background-color: #fff !important;
          color: #444 !important;
          border: 1px solid #ddd;
        }
        .ynow-kpi-grid .small-box.ynow-kpi-na .icon-large {
          color: rgba(0, 0, 0, 0.12);
        }
        /* KPI by Sheet / Crossover KPIs only: H/V-center text & values; value >> label */
        .ynow-kpi-grid .small-box .inner {
          display: flex !important;
          flex-direction: column !important;
          justify-content: center !important;
          align-items: center !important;
          flex: 1 1 auto !important;
          width: 100% !important;
          height: 100% !important;
          box-sizing: border-box !important;
          padding: 6px 9px !important;
          padding-right: 9px !important;
          text-align: center !important;
        }
        .ynow-kpi-grid .small-box .inner h3 {
          font-size: clamp(18px, 3.4vw, 28px) !important;
          font-weight: 800 !important;
          margin: 0 0 5px 0 !important;
          white-space: normal !important;
          overflow-wrap: anywhere;
          text-align: center !important;
          width: 100% !important;
          max-width: 100% !important;
        }
        .ynow-kpi-grid .small-box .inner p {
          font-size: clamp(9px, 1.2vw, 11px) !important;
          opacity: 0.9;
          font-weight: 500 !important;
          margin: 0 !important;
          line-height: 1.2 !important;
          text-align: center !important;
          width: 100% !important;
        }
        .ynow-kpi-grid .small-box .icon-large {
          font-size: 37px !important;
          top: 9px !important;
          right: 9px !important;
          opacity: 0.12 !important;
        }
        /* Focus KPI value-alert: bell inside gold frame (alertable wraps .small-box) */
        .ynow-kpi-grid .ynow-alertable {
          position: relative;
          display: block;
          width: 100%;
        }
        .ynow-kpi-grid .ynow-alertable > [class*='col-'] {
          width: 100% !important;
          max-width: 100% !important;
          float: none !important;
          padding-left: 0 !important;
          padding-right: 0 !important;
          box-sizing: border-box;
        }
        .ynow-kpi-grid .ynow-alertable .ynow-alert-bell {
          top: 8px;
          right: 8px;
          z-index: 5;
        }
        .ynow-kpi-section-title {
          font-size: 13px;
          font-weight: 700;
          color: #333;
          margin: 12px 0 6px 0;
        }
        /* Annotation 頁籤 */
        .ynow-ann-legend {
          display: flex;
          flex-wrap: wrap;
          gap: 10px;
          margin: 0 0 14px 0;
        }
        .ynow-ann-chip {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          padding: 8px 12px;
          border-radius: 6px;
          background: #fafafa;
          border: 1px solid #e5e5e5;
          font-size: 13px;
          color: #333;
          min-width: 168px;
        }
        /* 產業標準六格：手機兩欄；≥768px 六欄均分。色碼圖例：手機兩欄／桌面四欄 */
        .ynow-ind-overview-metrics {
          width: 100%;
          margin-top: 10px;
        }
        .ynow-ind-snapshot-chips,
        .ynow-kpi-legend-chips {
          display: grid !important;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 10px;
          align-items: stretch;
        }
        .ynow-ind-snapshot-chip,
        .ynow-kpi-legend-chip {
          min-width: 0 !important;
          width: 100%;
          box-sizing: border-box;
        }
        .ynow-ind-snapshot-chip {
          flex-direction: column;
          align-items: flex-start;
          gap: 2px;
        }
        @media (min-width: 768px) {
          .ynow-ind-snapshot-chips {
            grid-template-columns: repeat(6, minmax(0, 1fr));
          }
          .ynow-kpi-legend-chips {
            grid-template-columns: repeat(4, minmax(0, 1fr));
          }
        }
        .ynow-ann-swatch {
          width: 14px;
          height: 14px;
          border-radius: 50%;
          flex: 0 0 14px;
          box-shadow: inset 0 0 0 1px rgba(0,0,0,0.08);
        }
        .ynow-ann-note {
          margin: 0 0 16px 0;
          padding: 10px 12px;
          background: #f5f5f5;
          border-left: 4px solid #222222;
          border-radius: 4px;
          font-size: 12.5px;
          color: #555;
          line-height: 1.5;
        }
        .ynow-ann-h {
          margin: 18px 0 8px 0;
          font-size: 14px;
          font-weight: 700;
          color: #222222;
        }

        /* Blue Chip：查詢條件標題橫幅右側 — 更新名單 */
        .box-header > .box-title:has(.ynow-lab-im-query-title-row) {
          display: block;
          float: none;
          width: 100%;
        }
        .ynow-lab-im-query-title-row {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 10px;
          width: 100%;
        }
        .ynow-lab-im-query-title-row > #ynow_lab_im_query_title {
          flex: 1 1 auto;
          min-width: 0;
        }
        .ynow-lab-im-query-title-row > .btn.ynow-lab-im-refresh-in-title,
        .ynow-lab-im-refresh-in-title {
          flex: 0 0 auto;
          white-space: nowrap;
          margin: 0;
          padding: 2px 10px;
          font-size: 12px;
          line-height: 1.35;
          background: rgba(255, 255, 255, 0.14) !important;
          border: 1px solid rgba(255, 255, 255, 0.55) !important;
          color: #fff !important;
        }
        .ynow-lab-im-query-title-row > .btn.ynow-lab-im-refresh-in-title:hover,
        .ynow-lab-im-refresh-in-title:hover {
          background: rgba(255, 255, 255, 0.26) !important;
          border-color: rgba(255, 255, 255, 0.75) !important;
          color: #fff !important;
        }

        /* Blue Chip：產業市值加權漲幅區塊熱圖（表格上方） */
        .ynow-lab-im-ind-mcap-treemap-wrap {
          border: 1px solid #e8e8e8;
          border-radius: 3px;
          background: #fff;
          padding: 8px 8px 4px 8px;
        }
        .ynow-lab-im-ind-mcap-treemap-wrap .js-plotly-plot,
        .ynow-lab-im-ind-mcap-treemap-wrap .plotly {
          width: 100% !important;
        }
        /* Blue Chip：產業市值加權漲幅標題列右側 — 更新排名 */
        .ynow-lab-im-ind-mcap-title-row {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 10px;
          width: 100%;
          margin: 0 0 6px 0;
          flex-wrap: wrap;
        }
        .ynow-lab-im-ind-mcap-title-left {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 8px;
          flex: 1 1 auto;
          min-width: 0;
        }
        /* Beat body.ynow-theme-bluechip .content-wrapper .btn-default (deep text) */
        body.ynow-theme-bluechip .content-wrapper
          .ynow-lab-im-ind-mcap-title-row > .btn.btn-default.ynow-lab-im-refresh-ind-mcap,
        body.ynow-theme-bluechip .content-wrapper
          .btn.btn-default.ynow-lab-im-refresh-ind-mcap,
        .ynow-lab-im-ind-mcap-title-row > .btn.ynow-lab-im-refresh-ind-mcap,
        .ynow-lab-im-refresh-ind-mcap {
          flex: 0 0 auto;
          white-space: nowrap;
          margin: 0;
          padding: 2px 10px;
          font-size: 12px;
          line-height: 1.35;
          background: #0C5484 !important;
          border: 1px solid #0C5484 !important;
          color: #fff !important;
        }
        body.ynow-theme-bluechip .content-wrapper
          .ynow-lab-im-ind-mcap-title-row > .btn.btn-default.ynow-lab-im-refresh-ind-mcap:hover,
        body.ynow-theme-bluechip .content-wrapper
          .btn.btn-default.ynow-lab-im-refresh-ind-mcap:hover,
        .ynow-lab-im-ind-mcap-title-row > .btn.ynow-lab-im-refresh-ind-mcap:hover,
        .ynow-lab-im-refresh-ind-mcap:hover {
          background: #0a4570 !important;
          border-color: #0a4570 !important;
          color: #fff !important;
        }
        body.ynow-theme-bluechip .content-wrapper
          .btn.ynow-lab-im-refresh-ind-mcap i,
        body.ynow-theme-bluechip .content-wrapper
          .btn.ynow-lab-im-refresh-ind-mcap #ynow_lab_im_refresh_ind_mcap_label,
        .ynow-lab-im-refresh-ind-mcap i,
        .ynow-lab-im-refresh-ind-mcap #ynow_lab_im_refresh_ind_mcap_label {
          color: #fff !important;
        }
        /* Blue Chip：美股績優篩選（對齊 Dashboard FINANCIAL REPORT：tabBox + 查詢條件／內容 box） */
        .ynow-lab-im-universe-row {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 10px;
          margin: 0 0 14px 0;
          font-size: 12.5px;
          color: #556;
        }
        .ynow-lab-im-universe-row .ynow-lab-im-universe-meta {
          flex: 1 1 auto;
          min-width: 0;
        }
        .ynow-lab-im-muted {
          color: #888;
          font-size: 11.5px;
        }
        .ynow-lab-im-filter-count {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 8px 12px;
          margin: 4px 0 10px 0;
          padding: 8px 12px;
          border: 1px solid #c5d4e8;
          border-radius: 6px;
          background: #f4f8fc;
          color: #243447;
          font-size: 13px;
          line-height: 1.35;
        }
        .ynow-lab-im-filter-count .ynow-lab-im-filter-count-label {
          color: #4a5a6a;
          font-weight: 600;
        }
        .ynow-lab-im-filter-count .ynow-lab-im-filter-count-n {
          font-size: 1.35em;
          font-weight: 800;
          color: #0b5cab;
          font-variant-numeric: tabular-nums;
        }
        .ynow-lab-im-filter-count .ynow-lab-im-filter-count-unit {
          color: #4a5a6a;
          font-weight: 600;
        }
        .ynow-lab-im-filter-count .ynow-lab-im-filter-count-hint {
          flex: 1 1 100%;
          color: #6a7a8a;
          font-size: 11.5px;
          font-weight: 400;
        }
        .ynow-lab-im-actions {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 8px;
          margin-top: 4px;
        }
        .ynow-lab-im-actions .btn {
          white-space: nowrap;
          padding: 6px 14px;
          font-weight: 700;
        }
        .ynow-lab-im-methods-groups {
          display: flex;
          flex-direction: column;
          gap: 8px;
          margin-top: 2px;
        }
        .ynow-lab-im-methods-group {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 8px 12px;
        }
        .ynow-lab-im-methods-group-label {
          flex: 0 0 auto;
          min-width: 2.75em;
          font-size: 12px;
          font-weight: 700;
          color: #555;
          letter-spacing: 0.02em;
        }
        .ynow-pdf-pages-groups {
          display: flex;
          flex-direction: column;
          gap: 10px;
          margin-top: 4px;
        }
        .ynow-pdf-pages-group {
          display: flex;
          flex-wrap: wrap;
          align-items: flex-start;
          gap: 6px 12px;
          padding: 8px 10px;
          background: #f7f9fc;
          border: 1px solid #e2e8f0;
          border-radius: 4px;
        }
        .ynow-pdf-pages-group-label {
          flex: 0 0 100%;
          font-size: 12px;
          font-weight: 700;
          color: #334;
          letter-spacing: 0.02em;
          margin: 0 0 2px 0;
        }
        .ynow-pdf-pages-group .shiny-options-group {
          display: inline-flex;
          flex-wrap: wrap;
          flex-direction: row;
          align-items: center;
          column-gap: 12px;
          row-gap: 6px;
          flex: 1 1 auto;
        }
        .ynow-pdf-pages .checkbox-inline {
          margin-left: 0 !important;
          margin-right: 0 !important;
          padding-left: 18px;
          font-size: 12.5px;
        }
        .ynow-lab-im-methods .shiny-options-group,
        .ynow-lab-im-boards .shiny-options-group,
        .ynow-lab-im-lb-scope .shiny-options-group {
          display: inline-flex;
          flex-wrap: wrap;
          flex-direction: row;
          align-items: center;
          align-content: flex-start;
          justify-content: flex-start;
          column-gap: 12px;
          row-gap: 8px;
        }
        .ynow-lab-im-methods-group .shiny-options-group {
          flex: 1 1 auto;
        }
        .ynow-lab-im-methods .checkbox-inline,
        .ynow-lab-im-boards .checkbox-inline,
        .ynow-lab-im-lb-scope .radio-inline {
          margin-left: 0 !important;
          margin-right: 0 !important;
          padding-left: 18px;
          vertical-align: middle;
        }
        /* Unused primary model: Listed-style locked checkbox (unchecked, not clickable) */
        .ynow-lab-im-methods .checkbox-inline.ynow-lab-im-method--locked,
        .ynow-lab-im-us-listed input[disabled] {
          cursor: not-allowed;
        }
        .ynow-lab-im-methods .checkbox-inline.ynow-lab-im-method--locked {
          opacity: 0.55;
          color: #888;
        }
        .ynow-lab-im-methods .checkbox-inline.ynow-lab-im-method--locked input[type=checkbox] {
          cursor: not-allowed;
          pointer-events: none;
        }
        .ynow-lab-im-quality .form-group {
          margin-bottom: 4px;
        }
        .ynow-lab-im-quality-hint {
          font-size: 12px;
          color: #888;
          line-height: 1.35;
          display: block;
          margin: -2px 0 0 0;
        }
        /* Lite：產業／模型欄隱藏後，篩選欄拉滿；盈餘品質與 ADR 同列 */
        body.ynow-lite .ynow-lab-im-filter-col {
          width: 100% !important;
          max-width: 100%;
        }
        .ynow-lab-im-eq-gate-row,
        .ynow-lab-im-us-listed-adr-row {
          margin-left: -8px;
          margin-right: -8px;
        }
        .ynow-lab-im-eq-gate-row > [class*='col-'],
        .ynow-lab-im-us-listed-adr-row > [class*='col-'] {
          padding-left: 8px;
          padding-right: 8px;
        }
        /* 台股：隱藏美股專用列（上市固定＋含 ADR） */
        body.ynow-market-tw .ynow-us-only {
          display: none !important;
        }
        /* 美股：隱藏台股板別（上市／上櫃／興櫃） */
        body:not(.ynow-market-tw) .ynow-tw-only {
          display: none !important;
        }
        .ynow-lab-im-eq-explain {
          margin: 2px 0 14px 0;
          padding: 8px 10px;
          color: #555;
          font-size: 12.5px;
          line-height: 1.5;
          background: #f7f8fa;
          border: 1px solid #e5e7eb;
          border-radius: 4px;
        }
        .ynow-lab-im-eq-explain b {
          color: #222;
          margin-right: 4px;
        }
        .ynow-lab-pill {
          display: inline-block;
          padding: 2px 8px;
          border-radius: 10px;
          font-size: 11.5px;
          font-weight: 700;
          line-height: 1.4;
          white-space: nowrap;
        }
        .ynow-lab-pill-fs {
          background: #f0f0f0;
          color: #222222;
          border: 1px solid #c8c8c8;
        }
        .ynow-lab-pill-eq-ok {
          background: #e7f6ee;
          color: #1b6b3a;
          border: 1px solid #9fd4b3;
        }
        .ynow-lab-pill-eq-no {
          background: #fdecea;
          color: #a94442;
          border: 1px solid #f0b8b4;
        }
        .ynow-lab-pill-gate-ok {
          background: #fff4d6;
          color: #8a5a00;
          border: 1px solid #e6c36a;
        }
        .ynow-lab-pill-gate-no {
          background: #f4f4f4;
          color: #666;
          border: 1px solid #d0d0d0;
        }
        .ynow-lab-pill-muted {
          background: #f7f7f7;
          color: #999;
          border: 1px solid #e5e5e5;
          font-weight: 600;
        }

        /* YNOW page — three stacked blocks (same chapter chrome as HFV) */
        .ynow-funnel-report {
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin: 0 auto 24px auto;
          box-sizing: border-box;
        }
        .ynow-funnel-report__masthead {
          margin: 0 0 16px 0;
          padding: 4px 2px 14px 2px;
          border-bottom: 2px solid #1a1a1a;
        }
        .ynow-funnel-report__masthead h2 {
          margin: 0 0 8px 0;
          font-size: clamp(20px, 2.6vw, 28px);
          font-weight: 700;
          letter-spacing: -0.01em;
          color: #1a1a1a;
        }
        .ynow-funnel-report__lead {
          margin: 0;
          max-width: 58em;
          font-size: 13.5px;
          line-height: 1.55;
          color: #555;
        }
        .ynow-funnel-chapter {
          margin: 0 0 18px 0;
          padding: 0;
          background: #fff;
          border: 1px solid #e6e6e6;
          border-radius: 8px;
          overflow: hidden;
        }
        .ynow-funnel-chapter__head {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 6px 12px;
          padding: 12px 16px;
          background: #fff;
          border-bottom: 1px solid #ececec;
        }
        .ynow-funnel-chapter__kicker {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.06em;
          text-transform: uppercase;
          color: #888;
        }
        .ynow-funnel-chapter__title {
          margin: 0;
          font-size: 16px;
          font-weight: 700;
          color: #1a1a1a;
          line-height: 1.35;
        }
        .ynow-funnel-chapter__body {
          padding: 14px 16px 16px 16px;
        }
        .ynow-funnel-chapter__lead {
          margin: 0 0 12px 0;
          font-size: 12.5px;
          line-height: 1.5;
          color: #666;
        }
        /* Notes / 附註 — collapsible annotation chrome (YNOW + siblings) */
        .ynow-notes {
          margin: 0 0 12px 0;
          border: 1px solid #ececec;
          border-radius: 6px;
          background: #fafafa;
        }
        /* Model Selector footnote: consideration dimensions (default collapsed) */
        .ynow-ms-dims {
          margin: 14px 0 0 0;
        }
        .ynow-notes > .ynow-notes__summary {
          cursor: pointer;
          list-style: none;
          padding: 8px 12px;
          font-size: 12.5px;
          font-weight: 700;
          letter-spacing: 0.02em;
          color: #444;
          user-select: none;
        }
        .ynow-notes > .ynow-notes__summary::-webkit-details-marker {
          display: none;
        }
        .ynow-notes > .ynow-notes__summary::marker {
          content: '';
        }
        .ynow-notes > .ynow-notes__summary::before {
          content: '\\25B8';
          display: inline-block;
          width: 1.1em;
          color: #888;
          font-weight: 700;
        }
        .ynow-notes[open] > .ynow-notes__summary::before {
          content: '\\25BE';
        }
        .ynow-notes__body {
          padding: 0 12px 10px 12px;
        }
        .ynow-notes__body > :last-child {
          margin-bottom: 0;
        }
        .ynow-funnel-chapter__body > .ynow-notes .ynow-funnel-chapter__lead {
          margin: 0;
        }
        .ynow-funnel-report__masthead > .ynow-funnel-report__lead {
          margin-top: 0;
          margin-bottom: 0;
        }
        .ynow-dc-item > .ynow-notes,
        .ynow-dc-live > .ynow-notes,
        .ynow-macro-kpi > .ynow-notes {
          margin-top: 8px;
          margin-bottom: 0;
        }
        .ynow-funnel-toolbar {
          margin: 0 0 14px 0;
        }
        .ynow-funnel-toolbar .form-group {
          margin-bottom: 8px;
        }
        .ynow-funnel-kpi-row {
          margin: 0 0 12px 0;
        }
        .ynow-funnel-chapter .ynow-macro-bubble h4 {
          margin: 0 0 8px 0;
          font-size: 14px;
          font-weight: 700;
          color: #1a1a1a;
        }
        .ynow-funnel-chapter .ynow-macro-hint {
          margin: 0 0 10px 0;
        }
        .ynow-funnel-scorecards {
          margin: 0 0 4px 0;
        }
        .ynow-funnel-scorecards .info-box,
        .ynow-funnel-scorecards .small-box {
          margin-bottom: 12px;
        }
        .ynow-funnel-kpi-jump-row {
          margin: 0 0 16px 0;
        }
        .ynow-funnel-kpi-jump-row > .col-sm-4 {
          margin-bottom: 0;
        }
        a.ynow-funnel-kpi-jump {
          display: block;
          color: inherit;
          text-decoration: none !important;
          cursor: pointer;
          border-radius: 8px;
        }
        a.ynow-funnel-kpi-jump:hover,
        a.ynow-funnel-kpi-jump:focus {
          text-decoration: none !important;
          color: inherit;
        }
        a.ynow-funnel-kpi-jump:focus {
          outline: 2px solid #1a1a1a;
          outline-offset: 3px;
        }
        a.ynow-funnel-kpi-jump .small-box,
        a.ynow-funnel-kpi-jump .info-box {
          margin-bottom: 0;
          width: 100%;
          border-radius: 8px;
          box-shadow: none;
          border: 1px solid #e6e6e6;
          min-height: 108px;
          cursor: pointer;
        }
        #ynow_funnel_verdict,
        #ynow_funnel_risk,
        #ynow_funnel_ch1,
        #ynow_funnel_ch2,
        #ynow_funnel_ch3,
        #ynow_funnel_fscore {
          scroll-margin-top: 72px;
        }
        .ynow-funnel-table-wrap {
          overflow-x: auto;
          width: 100%;
        }
        .ynow-funnel-table-wrap table {
          width: 100% !important;
        }
        .ynow-funnel-report > .box {
          margin-bottom: 14px;
          border-radius: 8px;
          box-shadow: none;
          border: 1px solid #e6e6e6;
        }
        .ynow-funnel-report > .box > .box-header {
          padding: 10px 14px;
        }
        .ynow-funnel-report > .box > .box-body {
          padding: 12px 14px 14px 14px;
        }
        @media (max-width: 767px) {
          .ynow-funnel-chapter__body {
            padding: 12px;
          }
          .ynow-funnel-report__masthead {
            padding-bottom: 12px;
          }
        }

        /* Testing — Quantitative Backtest — report shell (same logic as HFV / YNOW) */
        /* Macro & Market Trends — logo 藍綠主題；數字 KPI＝黑底金字；同列等高；手機響應式 */
        .ynow-macro-report {
          --ynow-macro-blue: var(--ynow-logo-blue, #0C5484);
          --ynow-macro-green: var(--ynow-logo-green, #249C60);
          --ynow-macro-gold: var(--ynow-gold, #F5C518);
          --ynow-macro-gold-deep: var(--ynow-gold-deep, #C9A227);
          --ynow-macro-ink: #0b1220;
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin: 0 auto 24px auto;
          padding: 0 2px;
          box-sizing: border-box;
        }
        .ynow-macro-report__masthead {
          margin: 0 0 14px 0;
          padding: 8px 12px 14px 12px;
          border-bottom: 3px solid var(--ynow-macro-green);
          border-radius: 0 0 8px 8px;
          background: linear-gradient(
            105deg,
            rgba(12, 84, 132, 0.10) 0%,
            rgba(36, 156, 96, 0.08) 55%,
            rgba(12, 84, 132, 0.04) 100%
          );
        }
        .ynow-macro-report__masthead h2 {
          margin: 0 0 8px 0;
          font-size: clamp(20px, 2.6vw, 28px);
          font-weight: 700;
          color: var(--ynow-macro-blue);
        }
        .ynow-macro-report__lead {
          margin: 0;
          font-size: 13.5px;
          line-height: 1.55;
          color: #3d4f5f;
          max-width: 72em;
        }
        .ynow-macro-chapter {
          margin: 18px 0 22px 0;
          padding: 12px 12px 14px 12px;
          border: 1px solid rgba(12, 84, 132, 0.18);
          border-left: 4px solid var(--ynow-macro-green);
          border-radius: 8px;
          background: rgba(12, 84, 132, 0.03);
        }
        .ynow-macro-theme-title-row {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 10px;
          width: 100%;
          margin: 0 0 8px 0;
        }
        .ynow-macro-theme-title-row > #ynow_macro_theme_title {
          flex: 1 1 auto;
          min-width: 0;
          margin: 0;
        }
        .ynow-macro-theme-title-row > .btn.ynow-macro-refresh-in-title,
        .ynow-macro-refresh-in-title {
          flex: 0 0 auto;
          white-space: nowrap;
          margin: 0;
          padding: 4px 12px;
          font-size: 13px;
          line-height: 1.35;
          background: #fff !important;
          border: 1px solid var(--ynow-macro-blue, #0C5484) !important;
          color: var(--ynow-macro-blue, #0C5484) !important;
        }
        .ynow-macro-theme-title-row > .btn.ynow-macro-refresh-in-title:hover,
        .ynow-macro-refresh-in-title:hover {
          background: rgba(12, 84, 132, 0.08) !important;
        }
        .ynow-macro-chapter h3,
        .ynow-macro-chapter h4 {
          margin: 0 0 8px 0;
          font-size: 16px;
          font-weight: 700;
          color: var(--ynow-macro-blue);
        }
        .ynow-macro-chapter h4 {
          font-size: 14px;
          color: var(--ynow-macro-green);
        }
        .ynow-macro-chapter__lead {
          margin: 0 0 12px 0;
          max-width: 72em;
        }
        .ynow-macro-concept-cloud-wrap {
          margin: 4px 0 14px 0;
        }
        .ynow-macro-concept-cloud-wrap > .control-label {
          display: block;
          margin-bottom: 4px;
          font-weight: 700;
          color: var(--ynow-macro-blue, #0C5484);
        }
        .ynow-macro-concept-cloud-wrap > .ynow-macro-hint {
          margin: 0 0 8px 0;
        }
        .ynow-macro-concept-cloud .shiny-options-group {
          display: flex;
          flex-wrap: wrap;
          gap: 8px 10px;
          align-items: center;
          margin: 0;
        }
        .ynow-macro-concept-cloud .checkbox-inline,
        .ynow-macro-concept-cloud label.checkbox-inline {
          margin: 0 !important;
          padding: 2px 10px !important;
          border: 1px solid rgba(12, 84, 132, 0.28);
          border-radius: 999px;
          background: #f7fafc;
          line-height: 1.25;
          white-space: nowrap;
          transition: background 0.15s ease, border-color 0.15s ease, color 0.15s ease;
        }
        .ynow-macro-concept-cloud .checkbox-inline input[type='checkbox'] {
          position: absolute;
          opacity: 0;
          width: 0;
          height: 0;
          margin: 0;
          pointer-events: none;
        }
        .ynow-macro-concept-cloud .ynow-macro-cloud-word {
          font-weight: 600;
          color: #0b1220;
          letter-spacing: 0.01em;
        }
        .ynow-macro-concept-cloud .checkbox-inline:hover {
          border-color: rgba(12, 84, 132, 0.55);
          background: #eef5fb;
        }
        .ynow-macro-concept-cloud .checkbox-inline:has(input:checked),
        .ynow-macro-concept-cloud .checkbox-inline.active {
          background: rgba(12, 84, 132, 0.12);
          border-color: #0C5484;
        }
        .ynow-macro-concept-cloud .checkbox-inline:has(input:checked) .ynow-macro-cloud-word,
        .ynow-macro-concept-cloud .checkbox-inline.active .ynow-macro-cloud-word {
          color: #0C5484;
        }
        .ynow-macro-chapter > .ynow-notes {
          margin-top: 10px;
        }
        .ynow-macro-card {
          background: #fff;
          border: 1px solid rgba(12, 84, 132, 0.22);
          border-top: 3px solid var(--ynow-macro-blue);
          border-radius: 8px;
          padding: 12px 14px;
          margin-bottom: 12px;
          min-height: 110px;
          height: 100%;
        }
        .ynow-macro-card h4 {
          margin: 0 0 8px 0;
          font-size: 13px;
          font-weight: 700;
          color: var(--ynow-macro-blue);
        }
        /* KPI 列：同列等高（flex） */
        .ynow-macro-kpi-row {
          display: flex;
          flex-wrap: wrap;
          margin-left: -8px;
          margin-right: -8px;
        }
        .ynow-macro-kpi-row > [class*='col-'] {
          display: flex;
          flex-direction: column;
          padding-left: 8px;
          padding-right: 8px;
          margin-bottom: 12px;
        }
        /* Static skeleton until nested index_kpi_row uiOutput flushes */
        .ynow-macro-kpi-host .ynow-macro-kpi--skeleton {
          opacity: 0.85;
        }
        .ynow-macro-kpi-host:has(#macro-index_kpi_row > *) > .ynow-macro-kpi-skeleton {
          display: none !important;
        }
        .ynow-macro-kpi,
        .ynow-macro-rf {
          background: var(--ynow-macro-ink);
          border: 1px solid rgba(201, 162, 39, 0.45);
          border-radius: 8px;
          padding: 12px 14px;
          margin-bottom: 0;
          flex: 1 1 auto;
          width: 100%;
          display: flex;
          flex-direction: column;
          justify-content: space-between;
          min-height: 112px;
          box-shadow: 0 2px 8px rgba(12, 84, 132, 0.12);
        }
        .ynow-macro-kpi__label {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: var(--ynow-macro-gold-deep);
        }
        .ynow-macro-kpi__value,
        .ynow-macro-rf__value {
          font-size: clamp(22px, 4.6vw, 32px);
          font-weight: 800;
          color: var(--ynow-macro-gold);
          line-height: 1.15;
          margin: 6px 0;
          word-break: break-word;
          font-variant-numeric: tabular-nums;
        }
        .ynow-macro-rf__value {
          font-size: clamp(24px, 5.2vw, 34px);
        }
        .ynow-macro-kpi__chg { font-size: 12.5px; font-weight: 600; }
        .ynow-macro-kpi__sym {
          font-size: 11px;
          color: rgba(245, 197, 24, 0.55);
          margin-top: auto;
        }
        .ynow-macro-kpi--clickable {
          cursor: pointer;
          transition: border-color 0.15s ease, box-shadow 0.15s ease;
        }
        .ynow-macro-kpi--clickable:hover {
          border-color: rgba(201, 162, 39, 0.85);
        }
        .ynow-macro-kpi--selected {
          border-color: var(--ynow-macro-gold);
          box-shadow: 0 0 0 2px rgba(201, 162, 39, 0.45);
        }
        .ynow-macro-index-hist {
          width: 100%;
          max-width: 100%;
          margin: 0 0 12px 0;
        }
        .ynow-macro-index-hist__card {
          width: 100%;
          min-height: 0;
        }
        .ynow-macro-index-hist__card:has(.ynow-index-detail-ready) .ynow-index-pending {
          display: none;
        }
        .ynow-own-index-overlay {
          margin: 0 0 10px 0;
          padding: 8px 10px;
          background: #f7f9fc;
          border: 1px solid #e4eaef;
          border-radius: 4px;
        }
        /* Overlay index checkboxes: keep readable spacing (labels were overlapping). */
        .ynow-own-index-overlay .shiny-options-group {
          display: flex;
          flex-direction: row;
          flex-wrap: wrap;
          align-items: center;
          gap: 10px 28px;
          margin-top: 4px;
          row-gap: 10px;
        }
        .ynow-own-index-overlay .shiny-options-group .checkbox,
        .ynow-own-index-overlay .shiny-options-group .checkbox-inline {
          display: inline-flex;
          align-items: center;
          flex: 0 0 auto;
          margin: 0 !important;
          padding: 0 4px 0 0 !important;
          float: none !important;
          min-width: 0;
        }
        .ynow-own-index-overlay .shiny-options-group .checkbox + .checkbox,
        .ynow-own-index-overlay .shiny-options-group .checkbox-inline + .checkbox-inline {
          margin-left: 0 !important;
        }
        .ynow-own-index-overlay .shiny-options-group label {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          margin: 0 !important;
          padding: 0 !important;
          white-space: nowrap;
          font-weight: normal;
          line-height: 1.35;
        }
        .ynow-own-index-overlay .shiny-options-group input[type='checkbox'] {
          position: static !important;
          margin: 0 !important;
          flex: 0 0 auto;
        }
        .ynow-own-index-overlay .ynow-macro-hint {
          margin: 6px 0 0 0;
        }
        .ynow-macro-kpi .ynow-macro-up,
        .ynow-macro-rf .ynow-macro-up,
        .ynow-macro-up { color: var(--ynow-chg-up, var(--ynow-macro-green)); }
        .ynow-macro-kpi .ynow-macro-down,
        .ynow-macro-rf .ynow-macro-down,
        .ynow-macro-down { color: var(--ynow-chg-down, #c0392b); }
        /* TW auto scheme: red up / green down via --ynow-chg-* on body.ynow-market-tw. */
        body.ynow-market-tw .ynow-macro-kpi .ynow-macro-up,
        body.ynow-market-tw .ynow-macro-rf .ynow-macro-up,
        body.ynow-market-tw .ynow-macro-up { color: var(--ynow-chg-up, #c0392b); }
        body.ynow-market-tw .ynow-macro-kpi .ynow-macro-down,
        body.ynow-market-tw .ynow-macro-rf .ynow-macro-down,
        body.ynow-market-tw .ynow-macro-down {
          color: var(--ynow-chg-down, var(--ynow-macro-green));
        }
        .ynow-macro-hint {
          margin: 6px 0 0 0;
          font-size: 12px;
          line-height: 1.45;
          color: #4a5d6c;
        }
        .ynow-macro-kpi .ynow-macro-hint,
        .ynow-macro-rf .ynow-macro-hint {
          color: rgba(245, 197, 24, 0.72);
        }
        .ynow-macro-callout {
          border-radius: 8px;
          padding: 10px 12px;
          margin: 0 0 14px 0;
        }
        .ynow-macro-callout--mode {
          background: rgba(12, 84, 132, 0.08);
          border: 1px solid rgba(12, 84, 132, 0.22);
        }
        .ynow-macro-callout--warn {
          background: rgba(36, 156, 96, 0.08);
          border: 1px solid rgba(36, 156, 96, 0.35);
          color: #1a3d2c;
        }
        .ynow-macro-kicker {
          display: block;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: var(--ynow-macro-blue);
          margin-bottom: 4px;
        }
        .ynow-macro-badge {
          display: inline-block;
          font-size: 13px;
          font-weight: 700;
          padding: 3px 10px;
          border-radius: 4px;
        }
        .ynow-macro-badge--us {
          background: var(--ynow-macro-blue);
          color: #fff;
        }
        .ynow-macro-badge--tw {
          background: var(--ynow-macro-green);
          color: #fff;
        }
        .ynow-macro-report .btn.btn-default {
          border-color: var(--ynow-macro-blue);
          color: var(--ynow-macro-blue);
        }
        .ynow-macro-report .btn.btn-default:hover {
          background: var(--ynow-macro-blue);
          color: #fff;
        }
        .ynow-macro-report .form-control:focus {
          border-color: var(--ynow-macro-green);
          box-shadow: 0 0 0 2px rgba(36, 156, 96, 0.2);
        }
        .ynow-macro-report .selectize-input.focus {
          border-color: var(--ynow-macro-green);
          box-shadow: 0 0 0 2px rgba(36, 156, 96, 0.2);
        }
        .ynow-macro-chart-row {
          display: flex;
          flex-wrap: wrap;
          margin-left: -8px;
          margin-right: -8px;
        }
        .ynow-macro-chart-row > [class*='col-'] {
          padding-left: 8px;
          padding-right: 8px;
          margin-bottom: 12px;
        }
        .ynow-bubble-pair-row {
          align-items: flex-start;
        }
        .ynow-bubble-pair-col {
          display: flex;
          flex-direction: column;
          float: none;
          min-width: 0;
          max-width: 100%;
          overflow: hidden;
          box-sizing: border-box;
        }
        @media (min-width: 768px) {
          .ynow-bubble-pair-row > .ynow-bubble-pair-col {
            flex: 0 0 50%;
            width: 50%;
            max-width: 50%;
          }
        }
        .ynow-bubble-attr-plot-host,
        .ynow-bubble-conc-stack {
          width: 100%;
          max-width: 100%;
          min-width: 0;
          overflow: hidden;
        }
        .ynow-bubble-pair-col .shiny-spinner-output-container,
        .ynow-bubble-pair-col .plotly,
        .ynow-bubble-pair-col .js-plotly-plot,
        .ynow-bubble-pair-col .html-widget {
          width: 100% !important;
          max-width: 100% !important;
        }
        .ynow-bubble-buffett-plot-wrap {
          width: 100%;
          max-width: none;
          margin: 0 0 8px 0;
        }
        .ynow-bubble-buffett-plot-wrap .plotly,
        .ynow-bubble-buffett-plot-wrap .js-plotly-plot,
        .ynow-bubble-buffett-plot-wrap .html-widget {
          width: 100% !important;
          max-width: none !important;
        }
        .ynow-macro-overlay-plot-wrap {
          width: 100%;
          max-width: none;
          margin: 0 0 8px 0;
        }
        .ynow-macro-overlay-plot-wrap .plotly,
        .ynow-macro-overlay-plot-wrap .js-plotly-plot,
        .ynow-macro-overlay-plot-wrap .html-widget {
          width: 100% !important;
          max-width: none !important;
        }
        /* Rf / alert 列也等高 */
        .ynow-macro-rf-row {
          display: flex;
          flex-wrap: wrap;
          margin-left: -8px;
          margin-right: -8px;
        }
        .ynow-macro-rf-row > [class*='col-'] {
          display: flex;
          flex-direction: column;
          padding-left: 8px;
          padding-right: 8px;
          margin-bottom: 12px;
        }
        .ynow-macro-rf-row .ynow-macro-card {
          flex: 1 1 auto;
          width: 100%;
        }
        .ynow-macro-kpi--htcdi.ynow-htcdi-alert--watch {
          border-color: rgba(230, 168, 23, 0.85);
        }
        .ynow-macro-kpi--htcdi.ynow-htcdi-alert--warning {
          border-color: rgba(211, 84, 0, 0.9);
        }
        .ynow-macro-kpi--htcdi.ynow-htcdi-alert--critical {
          border-color: rgba(192, 57, 43, 0.95);
          box-shadow: 0 0 0 2px rgba(192, 57, 43, 0.25);
        }
        .ynow-macro-kpi--htcdi.ynow-htcdi-alert--unavailable {
          border-color: rgba(12, 84, 132, 0.35);
        }
        .ynow-htcdi-flow,
        .ynow-pyn-num {
          background-image: var(--ynow-logo-flow-gradient, linear-gradient(105deg, #0C5484 0%, #1AA8B8 50%, #249C60 100%));
          background-size: 220% 100%;
          background-repeat: no-repeat;
          -webkit-background-clip: text;
          background-clip: text;
          -webkit-text-fill-color: transparent;
          color: transparent;
          animation: ynow-logo-flow 2.6s ease-in-out infinite;
          font-weight: 700;
        }
        /* Account / ledger <code> PYN balance: clear chip wash so logo-flow clip shows. */
        .ynow-maint-meta-grid code.ynow-pyn-num,
        #ynow_pts_pyn_value.ynow-pyn-num,
        #ynow_pts_ledger_pyn_value.ynow-pyn-num {
          background: transparent !important;
          background-image: var(--ynow-logo-flow-gradient, linear-gradient(105deg, #0C5484 0%, #1AA8B8 50%, #249C60 100%)) !important;
          background-size: 220% 100% !important;
          background-repeat: no-repeat !important;
          -webkit-background-clip: text !important;
          background-clip: text !important;
          -webkit-text-fill-color: transparent !important;
          color: transparent !important;
          animation: ynow-logo-flow 2.6s ease-in-out infinite !important;
          padding: 0 !important;
        }
        /* HTCDI only: flame dynamic fill (KPI, four pillars, expand tables). */
        .ynow-macro-kpi--htcdi .ynow-htcdi-flow,
        .ynow-htcdi-sub .ynow-htcdi-flow,
        .ynow-htcdi-table .ynow-htcdi-flow,
        .ynow-htcdi-expand__card .ynow-htcdi-flow {
          background-image: var(--ynow-htcdi-flame-gradient, linear-gradient(105deg, #8B1A1A 0%, #E67E22 40%, #FFE66D 70%, #C0392B 100%));
          animation: ynow-htcdi-flame-flow 2.4s ease-in-out infinite;
        }
        .ynow-macro-own-index .ynow-macro-kpi__value.ynow-htcdi-flow,
        .ynow-macro-kpi--ynow .ynow-macro-kpi__value.ynow-htcdi-flow {
          color: transparent;
          -webkit-text-fill-color: transparent;
        }
        .ynow-htcdi-unavailable {
          color: #6b7c8a;
          -webkit-text-fill-color: #6b7c8a;
          font-weight: 600;
        }
        .ynow-macro-kpi--rf .ynow-htcdi-flow,
        .ynow-macro-rf__value.ynow-htcdi-flow {
          animation: none;
          background-image: none;
          -webkit-text-fill-color: inherit;
          color: inherit;
        }
        @media (prefers-reduced-motion: reduce) {
          .ynow-htcdi-flow,
          .ynow-pyn-num { animation: none; background-position: 0% 50%; }
          .ynow-macro-kpi--htcdi .ynow-htcdi-flow,
          .ynow-htcdi-sub .ynow-htcdi-flow,
          .ynow-htcdi-table .ynow-htcdi-flow,
          .ynow-htcdi-expand__card .ynow-htcdi-flow {
            animation: none;
            background-position: 0% 50%;
          }
        }
        @supports not ((-webkit-background-clip: text) or (background-clip: text)) {
          .ynow-htcdi-flow,
          .ynow-pyn-num,
          .ynow-fund-profile-badge .ynow-htcdi-flow {
            background-image: none;
            -webkit-text-fill-color: #0C5484;
            color: #0C5484;
            animation: none;
          }
          .ynow-macro-kpi--htcdi .ynow-htcdi-flow,
          .ynow-htcdi-sub .ynow-htcdi-flow,
          .ynow-htcdi-table .ynow-htcdi-flow,
          .ynow-htcdi-expand__card .ynow-htcdi-flow {
            -webkit-text-fill-color: #E67E22;
            color: #E67E22;
          }
        }
        .ynow-macro-htcdi-expand { width: 100%; margin: 0 0 12px 0; }
        .ynow-htcdi-expand__card h4 { margin: 12px 0 6px 0; font-size: 14px; }
        .ynow-htcdi-network { margin: 0 0 12px 0; }
        .ynow-htcdi-network__lead {
          margin: 0 0 4px 0;
          font-size: 13px;
          font-weight: 700;
          color: #2c3e50;
        }
        .ynow-htcdi-chain-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
          gap: 10px;
          margin: 8px 0 4px 0;
        }
        .ynow-htcdi-chain-card {
          border: 1px solid #e0b56a;
          background: linear-gradient(180deg, #fffaf0 0%, #ffffff 70%);
          border-radius: 8px;
          padding: 10px 12px;
          box-shadow: 0 1px 0 rgba(44, 62, 80, 0.04);
        }
        .ynow-htcdi-chain-card__title {
          font-size: 12px;
          font-weight: 700;
          color: #8a4b00;
          margin: 0 0 8px 0;
        }
        .ynow-htcdi-chain-flow {
          display: flex;
          flex-wrap: wrap;
          align-items: stretch;
          gap: 6px;
        }
        .ynow-htcdi-chain-arrow {
          align-self: center;
          color: #b07a2a;
          font-weight: 700;
          flex: 0 0 auto;
        }
        .ynow-htcdi-chain-node {
          flex: 1 1 88px;
          min-width: 88px;
          max-width: 180px;
          border-radius: 6px;
          padding: 6px 8px;
          border: 1px solid #d5dde5;
          background: #f7f9fb;
          display: flex;
          flex-direction: column;
          gap: 2px;
        }
        .ynow-htcdi-chain-node--cooling {
          border-color: #e0a050;
          background: #fff3e0;
        }
        .ynow-htcdi-chain-node--ok {
          border-color: #c5d0da;
          background: #f4f7fa;
          opacity: 0.88;
        }
        .ynow-htcdi-chain-node--layer {
          min-width: 110px;
        }
        .ynow-htcdi-chain-node__lab {
          font-size: 12px;
          font-weight: 700;
          color: #2c3e50;
          line-height: 1.25;
        }
        .ynow-htcdi-chain-node__state {
          font-size: 10px;
          font-weight: 700;
          color: #b35c00;
          letter-spacing: 0.02em;
          text-transform: uppercase;
        }
        .ynow-htcdi-chain-node__mem {
          font-size: 10px;
          color: #5d6d7e;
          line-height: 1.3;
          word-break: break-word;
        }
        @media (max-width: 575px) {
          .ynow-htcdi-chain-grid { grid-template-columns: 1fr; }
          .ynow-htcdi-chain-node { max-width: none; }
        }
        .ynow-htcdi-table-wrap { overflow-x: auto; margin: 0 0 10px 0; }
        .ynow-htcdi-table { font-size: 12px; margin-bottom: 0; }
        .ynow-htcdi-term-badge {
          display: inline-block;
          min-width: 2.6em;
          padding: 1px 7px;
          border-radius: 3px;
          font-weight: 700;
          font-size: 11px;
          letter-spacing: 0.02em;
          line-height: 1.5;
          border: 1px solid transparent;
        }
        .ynow-htcdi-term--stmt .ynow-htcdi-term-badge,
        .ynow-htcdi-term-badge.ynow-htcdi-term--stmt {
          color: #0b4f6c; background: #d9eef7; border-color: #9ec9dc;
        }
        .ynow-htcdi-term--mkt .ynow-htcdi-term-badge,
        .ynow-htcdi-term-badge.ynow-htcdi-term--mkt {
          color: #1b5e20; background: #e3f2e5; border-color: #9cc7a2;
        }
        .ynow-htcdi-term--inf .ynow-htcdi-term-badge,
        .ynow-htcdi-term-badge.ynow-htcdi-term--inf {
          color: #8a4b00; background: #fff0d6; border-color: #e0b56a;
        }
        .ynow-htcdi-term--traj .ynow-htcdi-term-badge,
        .ynow-htcdi-term-badge.ynow-htcdi-term--traj {
          color: #4a148c; background: #f0e6f8; border-color: #c4a3de;
        }
        .ynow-htcdi-table th.ynow-htcdi-term--stmt,
        .ynow-htcdi-table td.ynow-htcdi-term--stmt { background-color: rgba(217, 238, 247, 0.45); }
        .ynow-htcdi-table th.ynow-htcdi-term--mkt,
        .ynow-htcdi-table td.ynow-htcdi-term--mkt { background-color: rgba(227, 242, 229, 0.45); }
        .ynow-htcdi-table th.ynow-htcdi-term--inf,
        .ynow-htcdi-table td.ynow-htcdi-term--inf { background-color: rgba(255, 240, 214, 0.55); }
        .ynow-htcdi-table th.ynow-htcdi-term--traj,
        .ynow-htcdi-table td.ynow-htcdi-term--traj { background-color: rgba(240, 230, 248, 0.5); }
        .ynow-htcdi-table--inputs tr.ynow-htcdi-term-groups th {
          text-align: center; border-bottom: 0; padding-bottom: 4px;
        }
        .ynow-htcdi-table--inputs tr.ynow-htcdi-term-metrics th {
          font-weight: 600; white-space: nowrap;
        }
        .ynow-htcdi-term-bridge {
          margin: 12px 0 10px 0;
          padding: 10px 12px;
          background: #f7f9fb;
          border: 1px solid #dce3ea;
          border-radius: 6px;
        }
        .ynow-htcdi-term-bridge__lead {
          margin: 0 0 8px 0;
          font-size: 12px;
          color: #34495e;
        }
        .ynow-htcdi-term-map {
          list-style: none;
          margin: 0;
          padding: 0;
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 6px 14px;
        }
        .ynow-htcdi-term-map li {
          display: flex;
          align-items: center;
          gap: 6px;
          font-size: 12px;
          color: #2c3e50;
          min-width: 0;
        }
        .ynow-htcdi-term-map__arrow { color: #7f8c8d; flex: 0 0 auto; }
        @media (max-width: 767px) {
          .ynow-htcdi-term-map { grid-template-columns: 1fr; }
        }
        .ynow-htcdi-formula-banner { margin: 8px 0 14px 0; }
        .ynow-htcdi-formula-banner__eq {
          font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center;
          margin: 0 0 8px 0; padding: 10px; background-color: #F2F4F4; border-radius: 8px;
        }
        .ynow-htcdi-formula-banner__parts {
          font-size: 13px; color: #555; text-align: center; margin: 0 0 8px 0;
        }
        .ynow-htcdi-pair { margin: 0 0 10px 0; }
        .ynow-htcdi-sub .ynow-macro-kpi__value { font-size: clamp(16px, 3.6vw, 22px); }
        @media (max-width: 991px) {
          .ynow-macro-kpi-row > [class*='col-'] {
            width: 50%;
            float: none;
          }
          /* Rf : YNOW : HTCDI → stack as Rf full, then YNOW|HTCDI half-half */
          .ynow-macro-rf-row > .col-md-6 {
            width: 100%;
          }
          .ynow-macro-rf-row > .col-md-3 {
            width: 50%;
          }
        }
        @media (max-width: 575px) {
          .ynow-macro-report {
            margin-bottom: 16px;
            padding: 0;
          }
          .ynow-macro-report__masthead {
            padding: 8px 10px 12px 10px;
          }
          .ynow-macro-chapter {
            padding: 10px 8px 12px 8px;
            margin: 12px 0 14px 0;
          }
          .ynow-macro-kpi-row > [class*='col-'] {
            width: 50%;
          }
          .ynow-macro-kpi,
          .ynow-macro-rf {
            min-height: 100px;
            padding: 10px 11px;
          }
          .ynow-macro-rf-row > [class*='col-'] {
            width: 100%;
            float: none;
          }
          .ynow-macro-chart-row > [class*='col-'] {
            width: 100%;
            float: none;
          }
        }
        .ynow-backtest-report {
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin: 0 auto 24px auto;
          box-sizing: border-box;
        }
        /* After report-shell rules: force full bleed on small screens */
        @media (max-width: 767px) {
          .ynow-home,
          .ynow-macro-report,
          .ynow-hfv-report,
          .ynow-funnel-report,
          .ynow-backtest-report,
          .ynow-bblab--report {
            max-width: 100%;
          }
        }
        .ynow-backtest-report__masthead {
          margin: 0 0 12px 0;
          padding: 4px 2px 10px 2px;
          border-bottom: 2px solid #1a1a1a;
        }
        .ynow-backtest-report__masthead h2 {
          margin: 0;
          font-size: clamp(20px, 2.6vw, 28px);
          font-weight: 700;
          letter-spacing: -0.01em;
          color: #1a1a1a;
        }
        .ynow-backtest-report__lead {
          margin: 0;
          max-width: none;
          font-size: 13.5px;
          line-height: 1.55;
          color: #555;
        }
        /* Masthead intro + Run: one light-gray band, 4:1, vertically centered */
        .ynow-backtest-zone-band {
          display: grid;
          grid-template-columns: 4fr 1fr;
          gap: 14px 18px;
          align-items: center;
          margin: 0 0 18px 0;
          padding: 14px 16px;
          background: #fafafa;
          border: 1px solid #e4e4e4;
          border-radius: 8px;
          box-sizing: border-box;
        }
        .ynow-backtest-zone-band__lead {
          min-width: 0;
        }
        .ynow-backtest-zone-band__run {
          min-width: 0;
          display: flex;
          flex-direction: column;
          align-items: stretch;
          justify-content: center;
          gap: 8px;
        }
        .ynow-backtest-zone-band__run .ynow-backtest-toolbar__label {
          text-align: center;
        }
        .ynow-backtest-zone-band__run .btn {
          width: 100%;
        }
        .ynow-backtest-toolbar {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
          gap: 12px 14px;
          margin: 0 0 18px 0;
          padding: 14px 16px;
          background: #fafafa;
          border: 1px solid #e4e4e4;
          border-radius: 8px;
          box-sizing: border-box;
        }
        .ynow-backtest-toolbar--above-chart {
          margin: 4px 0 12px 0;
        }
        .ynow-backtest-toolbar__group {
          min-width: 0;
          display: flex;
          flex-direction: column;
          gap: 6px;
        }
        .ynow-backtest-toolbar__group--wide {
          grid-column: 1 / -1;
        }
        .ynow-backtest-toolbar__group--run {
          flex-direction: row;
          flex-wrap: wrap;
          align-items: center;
          gap: 10px 14px;
        }
        .ynow-backtest-toolbar__label {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: #666;
          line-height: 1.35;
        }
        .ynow-backtest-toolbar__group .form-group {
          margin: 0;
        }
        .ynow-backtest-toolbar__group > label.control-label,
        .ynow-backtest-toolbar .shiny-input-radiogroup > label.control-label {
          display: block;
          margin: 0 0 6px 0;
          padding: 0;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: #666;
          line-height: 1.35;
        }
        .ynow-backtest-toolbar .shiny-options-group {
          display: flex !important;
          flex-wrap: wrap !important;
          align-items: stretch;
          gap: 6px;
          margin: 0;
          padding: 0;
          clear: none;
        }
        .ynow-backtest-toolbar .radio,
        .ynow-backtest-toolbar .radio-inline {
          float: none !important;
          display: inline-flex !important;
          align-items: center;
          margin: 0 !important;
          padding: 0 !important;
          min-height: 0;
        }
        .ynow-backtest-toolbar .radio > label,
        .ynow-backtest-toolbar .radio-inline,
        .ynow-backtest-toolbar .radio-inline > label {
          margin: 0 !important;
          padding: 7px 12px 7px 28px !important;
          border: 1px solid #d0d0d0;
          border-radius: 999px;
          background: #fff;
          font-size: 12.5px;
          font-weight: 500;
          line-height: 1.25;
          color: #222;
          white-space: nowrap;
          cursor: pointer;
          transition: border-color .12s ease, background .12s ease, box-shadow .12s ease;
        }
        .ynow-backtest-toolbar .radio input[type='radio'],
        .ynow-backtest-toolbar .radio-inline input[type='radio'] {
          position: absolute;
          margin-left: -20px;
          margin-top: 1px;
        }
        .ynow-backtest-toolbar .radio > label:hover,
        .ynow-backtest-toolbar .radio-inline:hover {
          border-color: #999;
          background: #fff;
        }
        .ynow-backtest-toolbar .radio:has(input:checked) > label,
        .ynow-backtest-toolbar .radio-inline:has(input:checked) {
          border-color: #1a1a1a;
          background: #1a1a1a;
          color: #fff;
          box-shadow: 0 1px 2px rgba(0,0,0,0.12);
        }
        .ynow-backtest-chapter {
          margin: 0 0 18px 0;
          padding: 0;
          background: #fff;
          border: 1px solid #e6e6e6;
          border-radius: 8px;
          overflow: hidden;
        }
        .ynow-backtest-chapter__head {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 6px 12px;
          padding: 12px 16px;
          background: #fff;
          border-bottom: 1px solid #ececec;
        }
        .ynow-backtest-chapter__kicker {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.06em;
          text-transform: uppercase;
          color: #888;
        }
        .ynow-backtest-chapter__title {
          margin: 0;
          font-size: 16px;
          font-weight: 700;
          color: #1a1a1a;
          line-height: 1.35;
        }
        .ynow-backtest-chapter__body {
          padding: 14px 16px 16px 16px;
        }
        .ynow-backtest-chapter__lead {
          margin: 0 0 12px 0;
          font-size: 12.5px;
          line-height: 1.5;
          color: #666;
        }
        .ynow-backtest-subhead {
          margin: 0 0 10px 0;
          font-size: 13px;
          font-weight: 700;
          color: #333;
        }
        .ynow-backtest-legend {
          margin: 12px 0 0 0;
          padding-left: 18px;
          font-size: 12px;
          color: #666;
          line-height: 1.55;
        }
        .ynow-backtest-inline-hint {
          margin: -4px 0 12px 0;
          font-size: 11.5px;
          line-height: 1.45;
          color: #777;
        }
        .ynow-backtest-callout {
          margin-top: 8px;
          padding: 12px;
          background: #f5f5f5;
          border: 1px solid #d0d0d0;
          border-radius: 5px;
          font-size: 12px;
          color: #444;
          line-height: 1.55;
        }
        .ynow-backtest-report > .box,
        .ynow-backtest-report > .row > .col-sm-12 > .box,
        .ynow-backtest-report > .fluid-row > .col-sm-12 > .box {
          margin-bottom: 14px;
          border-radius: 8px;
          box-shadow: none;
          border: 1px solid #e6e6e6;
        }
        .ynow-backtest-report > .box > .box-header,
        .ynow-backtest-report .box > .box-header {
          padding: 10px 14px;
        }
        .ynow-backtest-report > .box > .box-body,
        .ynow-backtest-report .box > .box-body {
          padding: 12px 14px 14px 14px;
        }
        .ynow-backtest-report .ynow-bt-params .nav-tabs-custom {
          margin-bottom: 0;
          box-shadow: none;
        }
        @media (max-width: 767px) {
          .ynow-backtest-zone-band {
            grid-template-columns: 1fr;
            align-items: stretch;
          }
          .ynow-backtest-zone-band__run .ynow-backtest-toolbar__label {
            text-align: left;
          }
          .ynow-backtest-toolbar {
            grid-template-columns: 1fr;
            padding: 12px;
            gap: 14px;
          }
          .ynow-backtest-toolbar__group--wide {
            grid-column: auto;
          }
          .ynow-backtest-toolbar .radio > label,
          .ynow-backtest-toolbar .radio-inline {
            white-space: normal;
            max-width: 100%;
          }
          .ynow-backtest-chapter__body {
            padding: 12px;
          }
          .ynow-backtest-report__masthead {
            padding-bottom: 10px;
          }
        }

        /* YNOW：Schilit 自動判讀 */
        .ynow-shen-wrap { margin: 0 0 16px 0; }
        .ynow-shen-count { display: flex; flex-wrap: wrap; gap: 8px; margin: 0 0 12px 0; }
        .ynow-shen-st {
          display: inline-block; font-size: 11.5px; font-weight: 700;
          padding: 2px 8px; border-radius: 4px;
        }
        .ynow-shen-alert { background: #c0392b; color: #fff; }
        .ynow-shen-watch { background: #f4e6c8; color: #7a4e00; }
        .ynow-shen-pass { background: #e8f6ee; color: #1e7a45; }
        .ynow-shen-na { background: #eee; color: #666; }
        .ynow-shen-strip {
          background: #fdf2f0; border: 1px solid #f0c9c4; border-radius: 6px;
          padding: 10px 12px; margin: 0 0 14px 0;
        }
        .ynow-shen-strip h4 { margin: 0 0 8px 0; font-size: 15px; color: #922b21; }
        .ynow-shen-cat { margin: 0 0 12px 0; }
        .ynow-shen-cat h5 { margin: 0 0 8px 0; font-size: 14px; color: #222222; }
        .ynow-shen-card {
          border: 1px solid #e4e8ec; border-radius: 6px;
          padding: 8px 10px; margin: 0 0 8px 0; background: #fff;
        }
        .ynow-shen-card.ynow-shen-alert { border-left: 4px solid #c0392b; }
        .ynow-shen-card.ynow-shen-watch { border-left: 4px solid #d4a017; }
        .ynow-shen-card.ynow-shen-pass { border-left: 4px solid #27ae60; }
        .ynow-shen-card.ynow-shen-na { border-left: 4px solid #bbb; }
        .ynow-shen-card-h {
          display: flex; justify-content: space-between; align-items: center;
          gap: 8px; margin-bottom: 4px;
        }
        .ynow-shen-code { font-weight: 700; font-size: 13px; color: #222; }
        .ynow-shen-why { margin: 0 0 4px 0; font-size: 12.5px; color: #444; line-height: 1.45; }
        .ynow-shen-metrics { margin: 0; font-size: 12px; color: #555; line-height: 1.4; }
        .ynow-shen-sep { color: #bbb; padding: 0 4px; }
        .ynow-shen-ok { font-size: 13px; color: #1e7a45; margin: 0 0 8px 0; }
        .ynow-shen-fold { margin: 8px 0 0 0; font-size: 12.5px; color: #666; }
        .ynow-shen-fold summary { cursor: pointer; font-weight: 600; }

        /* YNOW：F-Score quality-screen cards (default visible, like Statement alerts) */
        .ynow-fscore-wrap { margin: 0 0 16px 0; }
        .ynow-fscore-count { display: flex; flex-wrap: wrap; gap: 8px; margin: 0 0 12px 0; }
        .ynow-fscore-st {
          display: inline-block; font-size: 11.5px; font-weight: 700;
          padding: 2px 8px; border-radius: 4px;
        }
        .ynow-fscore-pass { background: #e8f6ee; color: #1e7a45; }
        .ynow-fscore-fail { background: #faf0ef; color: #c0392b; }
        .ynow-fscore-waiting { color: #777; font-size: 13px; margin: 0; }
        .ynow-fscore-grid {
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 8px;
        }
        @media (max-width: 767px) {
          .ynow-fscore-grid { grid-template-columns: 1fr; }
        }
        .ynow-fscore-card {
          border: 1px solid #e4e8ec; border-radius: 6px;
          padding: 8px 10px; background: #fff; margin: 0;
        }
        .ynow-fscore-card.ynow-fscore-pass { border-left: 4px solid #27ae60; }
        .ynow-fscore-card.ynow-fscore-fail { border-left: 4px solid #c0392b; }
        .ynow-fscore-card-h {
          display: flex; justify-content: space-between; align-items: flex-start;
          gap: 8px;
        }
        .ynow-fscore-item { font-weight: 600; font-size: 12.5px; color: #222; line-height: 1.4; }

        /* YNOW：Quality of Earnings dashboard + automated risk matrix */
        /* Beneish M-Score control-point widget */
        .ynow-mscore-widget {
          margin: 0 0 16px 0;
          padding: 14px 16px;
          border-radius: 8px;
          border: 1px solid #dee2e6;
          background: #f8f9fa;
        }
        .ynow-mscore-widget--danger {
          border-color: #e0b4b0;
          background: linear-gradient(180deg, #fdf4f3 0%, #fff 70%);
          box-shadow: 0 0 0 1px rgba(192, 57, 43, 0.08);
        }
        .ynow-mscore-widget--watch {
          border-color: #e6d6a8;
          background: linear-gradient(180deg, #fffbf0 0%, #fff 70%);
        }
        .ynow-mscore-widget--pass {
          border-color: #b8dfc8;
          background: linear-gradient(180deg, #eef8f1 0%, #fff 70%);
        }
        .ynow-mscore-widget--na {
          border-style: dashed;
          color: #666;
        }
        .ynow-mscore-widget__head {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          justify-content: space-between;
          gap: 8px;
          margin: 0 0 10px 0;
        }
        .ynow-mscore-widget__title {
          font-size: 14px;
          font-weight: 700;
          color: #1a1a1a;
        }
        .ynow-mscore-widget__badge {
          display: inline-block;
          padding: 2px 8px;
          border-radius: 4px;
          font-size: 11px;
          font-weight: 700;
          background: #f0f2f4;
          color: #555;
        }
        .ynow-mscore-widget__badge--danger { background: #faf0ef; color: #c0392b; }
        .ynow-mscore-widget__badge--watch { background: #fff6e0; color: #9a7b1a; }
        .ynow-mscore-widget__badge--pass { background: #e8f6ee; color: #1e7a45; }
        .ynow-aml-mark-row {
          display: flex;
          align-items: center;
          gap: 0.35rem;
          margin: 0 0 0.4rem;
        }
        .ynow-aml-mark {
          display: inline-block;
          padding: 1px 6px;
          border-radius: 3px;
          font-size: 10px;
          font-weight: 800;
          letter-spacing: 0.04em;
          line-height: 1.35;
          vertical-align: middle;
          white-space: nowrap;
          cursor: help;
        }
        .ynow-aml-mark--alert {
          background: #faf0ef;
          color: #c0392b;
          border: 1px solid rgba(192, 57, 43, 0.35);
        }
        .ynow-aml-mark--watch {
          background: #fff6e0;
          color: #9a7b1a;
          border: 1px solid rgba(154, 123, 26, 0.35);
        }
        .small-box .ynow-aml-mark {
          margin-left: 0.25rem;
        }
        .ynow-mscore-widget__body {
          display: grid;
          grid-template-columns: minmax(88px, 120px) 1fr;
          gap: 12px;
          align-items: center;
        }
        @media (max-width: 640px) {
          .ynow-mscore-widget__body { grid-template-columns: 1fr; }
        }
        .ynow-mscore-widget__value {
          font-size: clamp(26px, 5.5vw, 38px);
          font-weight: 800;
          line-height: 1.05;
          font-variant-numeric: tabular-nums;
          color: #0C5484;
        }
        .ynow-mscore-widget--danger .ynow-mscore-widget__value { color: #c0392b; }
        .ynow-mscore-widget--watch .ynow-mscore-widget__value { color: #9a7b1a; }
        .ynow-mscore-widget--pass .ynow-mscore-widget__value { color: #1e7a45; }
        .ynow-mscore-widget__msg {
          margin: 0 0 4px 0;
          font-size: 13.5px;
          font-weight: 600;
          line-height: 1.45;
          color: #222;
        }
        .ynow-mscore-widget__hint {
          margin: 0;
          font-size: 12px;
          color: #666;
          line-height: 1.45;
        }
        .ynow-mscore-widget__nuance {
          margin: 10px 0 0 0;
          padding: 8px 10px;
          border-left: 3px solid #d68910;
          background: #fff8e8;
          font-size: 12.5px;
          line-height: 1.5;
          color: #5c4a12;
        }
        .ynow-mscore-widget__details {
          margin: 10px 0 0 0;
          font-size: 12.5px;
          color: #444;
        }
        .ynow-mscore-widget__details > summary {
          cursor: pointer;
          font-weight: 600;
          color: #0C5484;
        }
        .ynow-mscore-widget__list {
          margin: 8px 0 0 0;
          padding-left: 18px;
          line-height: 1.55;
        }

        .ynow-eq-wrap, .ynow-eq-matrix-wrap { margin: 0 0 18px 0; }
        .ynow-eq-waiting { color: #777; font-size: 13px; margin: 0; }
        .ynow-eq-head { margin: 0 0 12px 0; }
        .ynow-eq-title { margin: 0 0 4px 0; font-size: 15px; font-weight: 700; color: #1a1a1a; }
        .ynow-eq-sub { margin: 0; font-size: 12.5px; color: #666; line-height: 1.45; }
        .ynow-eq-scorecard {
          display: grid;
          grid-template-columns: minmax(140px, 180px) 1fr;
          gap: 12px;
          margin: 0 0 12px 0;
          align-items: stretch;
        }
        @media (max-width: 767px) {
          .ynow-eq-scorecard { grid-template-columns: 1fr; }
        }
        .ynow-eq-score-pill {
          border-radius: 8px; padding: 14px 12px; text-align: center;
          border: 1px solid #d5e3ec; background: #f7fbf9;
        }
        .ynow-eq-score-pill.ynow-eq-alert { border-color: #e0b4b0; background: #fdf4f3; }
        .ynow-eq-score-pill.ynow-eq-watch { border-color: #e6d6a8; background: #fffbf0; }
        .ynow-eq-score-pill.ynow-eq-pass { border-color: #b8dfc8; background: #eef8f1; }
        .ynow-eq-score-num {
          font-size: clamp(28px, 6vw, 40px); font-weight: 800; line-height: 1.1;
          color: #0C5484; letter-spacing: -0.02em;
          font-variant-numeric: tabular-nums;
        }
        .ynow-eq-score-meta {
          display: flex; justify-content: center; gap: 8px; flex-wrap: wrap;
          margin-top: 6px; font-size: 12px;
        }
        .ynow-eq-grade { font-weight: 700; color: #333; }
        .ynow-eq-flag {
          display: inline-block; font-size: 11px; font-weight: 700;
          padding: 1px 7px; border-radius: 3px;
        }
        .ynow-eq-flag.ynow-eq-alert { background: #faf0ef; color: #c0392b; }
        .ynow-eq-flag.ynow-eq-watch { background: #fff6e0; color: #9a7b1a; }
        .ynow-eq-flag.ynow-eq-pass { background: #e8f6ee; color: #1e7a45; }
        .ynow-eq-flag.ynow-eq-na { background: #f0f2f4; color: #777; }
        .ynow-eq-kpis {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 8px;
        }
        .ynow-eq-kpi {
          border: 1px solid #e4e8ec; border-radius: 6px;
          padding: 8px 10px; background: #fff;
        }
        .ynow-eq-kpi-k { display: block; font-size: 11px; color: #777; margin-bottom: 2px; }
        .ynow-eq-kpi-v { display: block; font-size: 15px; font-weight: 700; color: #0C5484; }
        .ynow-eq-bars { margin: 4px 0 0 0; }
        .ynow-eq-bar-row {
          display: grid;
          grid-template-columns: minmax(90px, 140px) 1fr 42px;
          gap: 8px; align-items: center;
          margin: 0 0 6px 0; font-size: 12px;
        }
        .ynow-eq-bar-lab { color: #444; }
        .ynow-eq-bar-track {
          height: 8px; border-radius: 4px; background: #eef1f4; overflow: hidden;
        }
        .ynow-eq-bar-fill { height: 100%; border-radius: 4px; }
        .ynow-eq-bar-pos { background: #249C60; }
        .ynow-eq-bar-neg { background: #c0392b; }
        .ynow-eq-bar-pts { text-align: right; font-weight: 700; color: #333; font-variant-numeric: tabular-nums; }
        .ynow-eq-level {
          display: inline-flex; align-items: center; gap: 8px;
          margin-top: 8px; padding: 4px 10px; border-radius: 4px;
          font-size: 12.5px; font-weight: 700;
        }
        .ynow-eq-level.ynow-eq-alert { background: #faf0ef; color: #c0392b; }
        .ynow-eq-level.ynow-eq-watch { background: #fff6e0; color: #9a7b1a; }
        .ynow-eq-level.ynow-eq-pass { background: #e8f6ee; color: #1e7a45; }
        .ynow-eq-level.ynow-eq-na { background: #f0f2f4; color: #777; }
        .ynow-eq-level-k { opacity: 0.85; font-weight: 600; }
        .ynow-eq-matrix-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 8px;
          margin: 0 0 12px 0;
        }
        @media (max-width: 767px) {
          .ynow-eq-matrix-grid { grid-template-columns: 1fr; }
        }
        .ynow-eq-matrix-card {
          border: 1px solid #e4e8ec; border-radius: 6px;
          padding: 10px 12px; background: #fff;
        }
        .ynow-eq-matrix-card.ynow-eq-alert { border-left: 4px solid #c0392b; }
        .ynow-eq-matrix-card.ynow-eq-watch { border-left: 4px solid #d4a017; }
        .ynow-eq-matrix-card.ynow-eq-pass { border-left: 4px solid #27ae60; }
        .ynow-eq-matrix-card.ynow-eq-na { border-left: 4px solid #adb5bd; }
        .ynow-eq-matrix-h {
          display: flex; justify-content: space-between; align-items: flex-start;
          gap: 8px; margin-bottom: 4px;
        }
        .ynow-eq-matrix-metric { font-weight: 700; font-size: 12.5px; color: #222; }
        .ynow-eq-matrix-val { font-size: 16px; font-weight: 800; color: #0C5484; margin: 2px 0 4px; }
        .ynow-eq-matrix-note { margin: 0; font-size: 11.5px; color: #666; line-height: 1.4; }
        .ynow-eq-redflags {
          border: 1px solid #e0b4b0; border-radius: 6px;
          background: #fdf6f5; padding: 10px 12px 6px;
        }
        .ynow-eq-redflags h5 { margin: 0 0 6px 0; color: #c0392b; font-size: 13px; }
        .ynow-eq-redflags ul { margin: 0 0 6px 0; padding-left: 18px; font-size: 12.5px; color: #444; }

        /* Backtest：績效指標卡片（軟色調 + 左側色條，避免實心色塊） */
        .ynow-metric-grid {
          --ynow-metric-green: #2d8a57;
          --ynow-metric-green-tint: #eef7f1;
          --ynow-metric-red: #c0392b;
          --ynow-metric-red-tint: #faf0ef;
          --ynow-metric-violet: #5c5a8a;
          --ynow-metric-violet-tint: #f3f2f8;
          --ynow-metric-blue: #333333;
          --ynow-metric-blue-tint: #f2f2f2;
          --ynow-metric-amber: #b7791f;
          --ynow-metric-amber-tint: #faf6ee;
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 14px;
          margin: 0 0 4px 0;
        }
        @media (max-width: 992px) {
          .ynow-metric-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        }
        @media (max-width: 767px) {
          /* 手機兩欄（勿三欄過窄；與 FS／KPI grid 一致） */
          .ynow-metric-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 8px; }
          .ynow-metric-card__body { padding: 8px 8px 8px 8px; gap: 4px; }
          .ynow-metric-card__icon { width: 22px; height: 22px; font-size: 11px; border-radius: 5px; }
          .ynow-metric-card__value { font-size: clamp(11px, 3.4vw, 16px); }
          .ynow-metric-card__label { font-size: 9px; }
          .ynow-metric-card__caption { font-size: 8px; }
        }
        .ynow-metric-card {
          background: #ffffff;
          border: 1px solid #e6e8eb;
          border-radius: 10px;
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.06);
          overflow: hidden;
          display: flex;
          flex-direction: column;
          min-height: 0;
          transition: box-shadow 0.15s ease, border-color 0.15s ease;
        }
        .ynow-metric-card:hover {
          box-shadow: 0 3px 10px rgba(0, 0, 0, 0.08);
          border-color: #d5d9de;
        }
        .ynow-metric-card--green {
          border-left: 4px solid var(--ynow-metric-green);
          background: linear-gradient(180deg, var(--ynow-metric-green-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--red {
          border-left: 4px solid var(--ynow-metric-red);
          background: linear-gradient(180deg, var(--ynow-metric-red-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--violet {
          border-left: 4px solid var(--ynow-metric-violet);
          background: linear-gradient(180deg, var(--ynow-metric-violet-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--blue {
          border-left: 4px solid var(--ynow-metric-blue);
          background: linear-gradient(180deg, var(--ynow-metric-blue-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--amber {
          border-left: 4px solid var(--ynow-metric-amber);
          background: linear-gradient(180deg, var(--ynow-metric-amber-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--blue .ynow-metric-card__icon { background: var(--ynow-metric-blue); }
        .ynow-metric-card--amber .ynow-metric-card__icon { background: var(--ynow-metric-amber); }
        .ynow-metric-card--blue .ynow-metric-card__value { color: #1a1a1a; }
        .ynow-metric-card--amber .ynow-metric-card__value { color: #8a5a12; }
        /* 執行面板：避免 btn-block 蓋住下方說明文字 */
        .ynow-bt-run-panel .btn-block { margin-left: 0; margin-right: 0; }
        .ynow-metric-card__body {
          padding: 14px 16px 12px 16px;
          display: flex;
          flex-direction: column;
          gap: 6px;
        }
        .ynow-metric-card__top {
          display: flex;
          align-items: center;
          gap: 10px;
        }
        .ynow-metric-card__icon {
          flex: 0 0 auto;
          width: 34px;
          height: 34px;
          border-radius: 8px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          font-size: 15px;
          color: #ffffff;
        }
        .ynow-metric-card--green .ynow-metric-card__icon { background: var(--ynow-metric-green); }
        .ynow-metric-card--red .ynow-metric-card__icon { background: var(--ynow-metric-red); }
        .ynow-metric-card--violet .ynow-metric-card__icon { background: var(--ynow-metric-violet); }
        .ynow-metric-card__label {
          font-size: 10px;
          font-weight: 600;
          color: #555555;
          line-height: 1.3;
          margin: 0;
        }
        .ynow-metric-card__value {
          font-size: clamp(22px, 4.6vw, 36px);
          font-weight: 800;
          font-variant-numeric: tabular-nums;
          letter-spacing: -0.02em;
          line-height: 1.1;
          margin: 0;
          color: #1a1a1a;
          overflow-wrap: anywhere;
          word-break: break-word;
        }
        .ynow-metric-card--green .ynow-metric-card__value { color: #1f5c3a; }
        .ynow-metric-card--red .ynow-metric-card__value { color: #8e2a20; }
        .ynow-metric-card--violet .ynow-metric-card__value { color: #3f3d62; }
        .ynow-metric-card__caption {
          margin: 0;
          font-size: 10px;
          color: #6b7280;
          line-height: 1.35;
        }

        /* 頂欄最右側：小圓標（無 wordmark）；幣別浮在 logo 正下方 */
        .main-header .navbar-custom-menu .navbar-nav > li#ynow-header-logo.ynow-header-logo,
        .main-header .navbar > #ynow-header-logo.ynow-header-logo {
          position: relative !important;
          height: 50px;
          display: flex !important;
          align-items: center;
          justify-content: center;
          float: left;
          list-style: none;
          padding: 0 14px 0 8px;
          margin: 0;
          overflow: visible !important;
        }
        .ynow-header-logo-mark {
          width: 36px;
          height: 36px;
          object-fit: contain;
          display: block;
          cursor: pointer;
          transform-origin: 50% 50%;
        }
        .ynow-header-logo-mark:focus {
          outline: 2px solid rgba(255, 215, 0, 0.85);
          outline-offset: 2px;
        }
        @keyframes ynow-logo-spin-cw {
          from { transform: rotate(0deg); }
          to { transform: rotate(360deg); }
        }
        .ynow-header-logo-mark.ynow-logo-spinning {
          --ynow-logo-spin-dur: 2.8s;
          animation: ynow-logo-spin-cw var(--ynow-logo-spin-dur) linear infinite;
        }
        .ynow-header-logo-mark.ynow-logo-spinning.ynow-logo-spin-fast {
          --ynow-logo-spin-dur: 0.7s;
        }
        @media (prefers-reduced-motion: reduce) {
          .ynow-header-logo-mark.ynow-logo-spinning,
          .ynow-header-logo-mark.ynow-logo-spinning.ynow-logo-spin-fast {
            --ynow-logo-spin-dur: 6s;
          }
        }
        body.ynow-market-tw .ynow-header-logo-mark {
          filter: drop-shadow(0 0 2px rgba(0,0,0,0.35));
        }

        /* 手機：單列頁首 — 標題與右上小 logo 垂直置中，夾在美股／台股與 logo 之間
           （黑色頁首置頂固定見全域 .main-header） */
        @media (max-width: 767px) {
          .main-header {
            min-height: 50px !important;
            max-height: 50px !important;
            height: 50px !important;
          }
          /* US + TW mobile: solid black bar so transparent navbar does not
             hide the centered title / progress fill. Desktop TW keeps flag. */
          body:not(.ynow-market-tw) .main-header,
          body:not(.ynow-market-tw).skin-black .main-header,
          body.ynow-market-tw .main-header,
          body.ynow-market-tw.skin-black .main-header {
            background-color: var(--ynow-ink) !important;
          }
          body.ynow-market-tw .main-header::before,
          body.ynow-market-tw .main-header::after {
            display: none !important;
            content: none !important;
            background: none !important;
            background-image: none !important;
          }
          body.ynow-market-tw .main-header .logo .ynow-app-title-fill-inner {
            text-shadow: none;
          }
          body.ynow-market-tw .main-header .navbar .nav > li > a,
          body.ynow-market-tw .ynow-market-header,
          body.ynow-market-tw .ynow-lang-header {
            text-shadow: none;
          }
          /* Collapse AdminLTE stacked logo+navbar (was ~100px) into one 50px bar.
             Title is full-bleed + viewport-centered; JS only sets title max-width
             so glyphs clear 美股／台股 and 繁中／EN (controls sit above via z-index). */
          .main-header .logo,
          .skin-black .main-header .logo,
          .skin-black .main-header .logo:hover {
            position: absolute !important;
            top: 0 !important;
            /* Full-bleed so title centers on the viewport; controls sit above via z-index */
            left: 0 !important;
            right: 0 !important;
            width: auto !important;
            max-width: none !important;
            height: 50px !important;
            min-height: 50px !important;
            line-height: 50px !important;
            padding: 0 8px !important;
            margin: 0 !important;
            float: none !important;
            display: flex !important;
            align-items: center !important;
            justify-content: center !important;
            text-align: center !important;
            background-color: transparent !important;
            background-image: none !important;
            /* Under transparent navbar so hamburger / 美股台股 / 繁中EN / mark stay on top */
            z-index: 1040 !important;
            overflow: visible !important;
            visibility: visible !important;
            opacity: 1 !important;
            pointer-events: none;
          }
          /* Opaque ::before on full-bleed title was painting over hamburger / 繁中／EN / mark */
          .skin-black .main-header .logo::before,
          body.ynow-market-tw .skin-black .main-header .logo::before {
            display: none !important;
            content: none !important;
            background: none !important;
          }
          /* Transparent navbar so title + gold progress fill show through the center gap.
             Interactive chrome (toggle / market / lang / mark) stays above via child z-index. */
          .main-header .navbar,
          .skin-black .main-header .navbar {
            position: relative !important;
            z-index: 1060 !important;
            background-color: transparent !important;
            background-image: none !important;
          }
          .main-header .logo .ynow-app-title {
            /* JS scales font into the center gap; clip so title cannot paint over pts/lang */
            position: relative;
            font-size: 16px;
            line-height: 1.15 !important;
            white-space: nowrap;
            overflow: hidden !important;
            text-overflow: clip;
            max-width: min(100%, calc(100vw - 200px));
            height: auto !important;
            visibility: visible !important;
            opacity: 1 !important;
          }
          .main-header .logo .ynow-app-title-base,
          .main-header .logo .ynow-app-title-fill,
          .main-header .logo .ynow-app-title-fill-inner {
            font-size: inherit;
            line-height: inherit;
            visibility: visible !important;
            opacity: 1 !important;
          }
          /* Keep progress-fill clip layer aligned while font is JS-scaled */
          .main-header .logo .ynow-app-title-fill {
            position: absolute;
            left: 0;
            top: 0;
            bottom: 0;
            width: var(--ynow-load-pct, 0%);
            max-width: 100%;
            overflow: hidden;
            white-space: nowrap;
            pointer-events: none;
          }
          /* Keep 繁中／EN / FYN·PYN / mark / toggle above the absolute title band */
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-auth-header,
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header,
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header,
          .main-header .navbar-custom-menu .navbar-nav > li#ynow-header-logo.ynow-header-logo {
            position: relative !important;
            z-index: 1062 !important;
            flex-shrink: 0 !important;
          }
          .main-header .navbar > .sidebar-toggle,
          .skin-black .main-header .navbar .sidebar-toggle {
            z-index: 1063 !important;
          }
          .main-header .navbar #ynow-market-header.ynow-market-header {
            z-index: 1062 !important;
          }
          /* Slightly compress 繁中／EN to free width for the centered title */
          .ynow-lang-btn {
            min-width: 28px !important;
            padding-left: 4px !important;
            padding-right: 4px !important;
            font-size: 10px !important;
            height: 26px !important;
            line-height: 24px !important;
          }
          .ynow-lang-stack {
            height: 26px !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-auth-header {
            padding: 0 6px 0 4px !important;
            margin-left: 4px !important;
            max-width: 42vw;
          }
          .ynow-auth-links--sidebar .ynow-auth-pair {
            gap: 4px !important;
          }
          .ynow-auth-links--sidebar .ynow-auth-pair a.ynow-auth-btn {
            height: 26px !important;
            min-height: 26px !important;
            padding: 0 6px !important;
            font-size: 11px !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header {
            padding: 0 4px 0 0 !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header {
            padding: 0 6px 0 2px !important;
          }
          .ynow-pts-banner {
            font-size: 10px !important;
            gap: 0 !important;
            min-width: 0 !important;
            flex-shrink: 0 !important;
            line-height: 1.1 !important;
          }
          .main-header .navbar {
            min-height: 50px !important;
            height: 50px;
            width: 100% !important;
            margin-left: 0 !important;
            display: flex !important;
            align-items: center !important;
            overflow: visible !important;
          }
          .main-header .navbar > .sidebar-toggle,
          .skin-black .main-header .navbar .sidebar-toggle {
            height: 50px !important;
            min-height: 50px !important;
            max-height: 50px !important;
            line-height: 0 !important;
            padding: 0 !important;
            width: 44px !important;
            display: inline-flex !important;
            align-items: center !important;
            justify-content: center !important;
            float: none !important;
            margin: 0 !important;
          }
          /* Mobile: flags sit in the flex row immediately after the hamburger
             (leftmost chrome on the black bar) — no absolute left drift. */
          .main-header .navbar #ynow-market-header.ynow-market-header,
          .main-header .navbar-custom-menu #ynow-market-header.ynow-market-header,
          .main-header .navbar-custom-menu .navbar-nav > li#ynow-market-header.ynow-market-header {
            position: static !important;
            left: auto !important;
            top: auto !important;
            right: auto !important;
            float: none !important;
            align-self: center !important;
            flex: 0 0 auto !important;
            margin: 0 !important;
            padding: 0 2px 0 0 !important;
            width: auto !important;
            height: 50px !important;
            min-height: 50px !important;
            display: flex !important;
            align-items: center !important;
            z-index: 1063 !important;
            border: none !important;
            background: transparent !important;
            box-shadow: none !important;
            outline: none !important;
          }
          .main-header .navbar #ynow-market-header.ynow-market-header::before,
          .main-header .navbar #ynow-market-header.ynow-market-header::after,
          #ynow-market-header.ynow-market-header::before,
          #ynow-market-header.ynow-market-header::after {
            content: none !important;
            display: none !important;
            border: none !important;
            background: none !important;
          }
          #ynow-market-header .ynow-market-stack {
            /* Shared length; US+TW hoist sum = 50px (same math as desktop). */
            width: calc(50px * 57 / 68) !important;
            height: 50px !important;
            min-height: 50px !important;
            flex-direction: column;
            justify-content: stretch;
            gap: 0 !important;
            margin: 0 !important;
            padding: 0 !important;
            border: none !important;
            background: transparent !important;
            box-shadow: none !important;
            overflow: hidden !important;
          }
          /* Bare flags only — no frame / seam; same length, native aspect, full stack height. */
          #ynow-market-header .ynow-mkt-btn,
          #ynow-market-header .ynow-mkt-btn.active,
          #ynow-market-header .ynow-mkt-btn:hover,
          #ynow-market-header .ynow-mkt-btn:focus,
          #ynow-market-header .ynow-mkt-btn:active {
            -webkit-appearance: none !important;
            appearance: none !important;
            border: 0 !important;
            border-width: 0 !important;
            border-style: none !important;
            border-color: transparent !important;
            border-radius: 0 !important;
            background: transparent !important;
            background-color: transparent !important;
            background-image: none !important;
            box-shadow: none !important;
            -webkit-box-shadow: none !important;
            outline: none !important;
            -webkit-tap-highlight-color: transparent !important;
            width: 100% !important;
            height: auto !important;
            min-height: 0 !important;
            max-height: none !important;
            padding: 0 !important;
            margin: 0 !important;
            align-items: stretch !important;
            justify-content: stretch !important;
            overflow: hidden !important;
          }
          #ynow-market-header #ynow_mkt_btn_us {
            flex: 15 0 0 !important;
          }
          #ynow-market-header #ynow_mkt_btn_tw {
            flex: 19 0 0 !important;
          }
          #ynow-market-header .ynow-mkt-btn {
            opacity: 1;
          }
          #ynow-market-header .ynow-mkt-btn + .ynow-mkt-btn {
            border: 0 !important;
            border-top-width: 0 !important;
            margin-top: 0 !important;
          }
          #ynow-market-header .ynow-mkt-btn.active {
            opacity: 1;
          }
          #ynow-market-header .ynow-mkt-btn .ynow-mkt-flag {
            width: 100% !important;
            height: 100% !important;
            min-width: 0 !important;
            min-height: 0 !important;
            max-width: none !important;
            max-height: none !important;
            border: none !important;
            box-shadow: none !important;
            outline: none !important;
            filter: grayscale(1) !important;
            -webkit-filter: grayscale(1) !important;
          }
          #ynow-market-header .ynow-mkt-btn.active .ynow-mkt-flag {
            filter: none !important;
            -webkit-filter: none !important;
          }
          .main-header .navbar-custom-menu {
            float: none !important;
            margin-left: auto !important;
            height: 50px !important;
            display: flex !important;
            align-items: center !important;
            overflow: visible !important;
            position: relative !important;
            z-index: 1063 !important;
          }
          .main-header .navbar-custom-menu > .navbar-nav {
            display: flex !important;
            flex-direction: row;
            align-items: center !important;
            height: 50px !important;
            margin: 0 !important;
            overflow: visible !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header,
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header,
          .main-header .navbar-custom-menu .navbar-nav > li#ynow-header-logo.ynow-header-logo {
            float: none !important;
            height: 50px !important;
            min-height: 50px !important;
            display: flex !important;
            align-items: center !important;
            justify-content: center !important;
            margin: 0 !important;
            padding-top: 0 !important;
            padding-bottom: 0 !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header {
            padding-left: 0 !important;
            padding-right: 4px !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header {
            padding-left: 2px !important;
            padding-right: 6px !important;
            overflow: visible !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li#ynow-header-logo.ynow-header-logo {
            position: relative !important;
            overflow: visible !important;
          }
          /* Mobile: one auth surface — sidebar date-row text links, not top-banner button. */
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-auth-header,
          body.ynow-lite .main-header .navbar-custom-menu .navbar-nav > li.ynow-auth-header,
          #ynow-auth-sidebar,
          .ynow-auth-links--sidebar {
            display: none !important;
            visibility: hidden !important;
            opacity: 0 !important;
            width: 0 !important;
            min-width: 0 !important;
            max-width: 0 !important;
            padding: 0 !important;
            margin: 0 !important;
            overflow: hidden !important;
            pointer-events: none !important;
          }
          /* Mobile override: points live in the sidebar (right of login date), not header. */
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header,
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header:not(.ynow-pts-hidden),
          body.ynow-lite .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header,
          body.ynow-lite .main-header .navbar-custom-menu .navbar-nav > li.ynow-pts-header:not(.ynow-pts-hidden),
          body.ynow-lite #ynow-pts-header,
          body.ynow-lite #ynow-pts-header:not(.ynow-pts-hidden),
          #ynow-pts-header,
          #ynow-pts-header:not(.ynow-pts-hidden) {
            display: none !important;
            visibility: hidden !important;
            opacity: 0 !important;
            width: 0 !important;
            min-width: 0 !important;
            max-width: 0 !important;
            padding: 0 !important;
            margin: 0 !important;
            overflow: hidden !important;
            pointer-events: none !important;
          }
          .ynow-ccy-float {
            right: 8px;
            margin-top: 8px;
            max-width: min(240px, calc(100vw - 16px));
          }
          .ynow-ccy-float .ynow-hdr-ccy-status,
          .ynow-ccy-float .ynow-hdr-ccy-status .shiny-text-output {
            max-width: min(200px, calc(100vw - 20px));
            white-space: normal;
            overflow-wrap: anywhere;
            word-break: break-word;
            line-height: 1.25;
            font-size: 9px;
          }
        }

        /* About：完整 LOGO（含文字）置頂品牌區 */
        .ynow-about-brand {
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          margin: 4px 0 18px 0;
          text-align: center;
        }
        .ynow-about-logo-full {
          width: min(240px, 55vw);
          height: auto;
          display: block;
        }

        /* About：中英左右對照簡介 */
        .ynow-about-bilingual {
          margin: 0 0 8px 0;
        }
        .ynow-about-col {
          padding-bottom: 8px;
        }
        .ynow-about-col--zh {
          border-right: 1px solid #e5e8eb;
        }
        @media (max-width: 991px) {
          .ynow-about-col--zh {
            border-right: none;
            border-bottom: 1px solid #e5e8eb;
            margin-bottom: 16px;
            padding-bottom: 16px;
          }
        }
        .ynow-about-title {
          margin: 0 0 12px 0;
          font-size: 22px;
          line-height: 1.3;
          color: #1a1a1a;
        }
        .ynow-about-lead,
        .ynow-about-method {
          font-size: 13.5px;
          line-height: 1.65;
          color: #333;
          margin: 0 0 12px 0;
        }
        .ynow-about-method {
          padding: 10px 12px;
          background: #f5f5f5;
          border-left: 4px solid #222222;
          border-radius: 0 4px 4px 0;
        }
        .ynow-about-feat-h {
          margin: 16px 0 8px 0;
          font-size: 15px;
          color: #222;
        }
        .ynow-about-feat {
          margin: 0;
          padding-left: 18px;
          font-size: 13px;
          line-height: 1.6;
          color: #444;
        }
        .ynow-about-feat > li {
          margin-bottom: 10px;
        }
        .ynow-about-feat > li > b {
          color: #1a1a1a;
        }
        .ynow-about-github {
          margin: 14px 0 0 0;
          font-size: 13px;
          line-height: 1.5;
          color: #444;
        }
        .ynow-about-github a {
          color: #0b57d0;
          word-break: break-all;
        }
        .ynow-about-data-sources,
        .ynow-legal-notices {
          margin: 20px 0 8px 0;
          padding-top: 8px;
        }
        .ynow-about-data-sources .ynow-about-section-title,
        .ynow-legal-notices .ynow-about-section-title {
          margin: 0 0 14px 0;
          font-size: 18px;
          line-height: 1.35;
          color: #1a1a1a;
        }
        .ynow-about-data-sources .ynow-about-section-lead {
          margin: 0 0 14px 0;
          font-size: 13px;
          line-height: 1.6;
          color: #444;
        }
        .ynow-legal-row {
          margin: 0 0 12px 0;
        }
        .ynow-legal-h {
          margin: 0 0 6px 0;
          font-size: 14px;
          font-weight: 650;
          color: #222;
        }
        .ynow-legal-body {
          margin: 0 0 10px 0;
          font-size: 12.5px;
          line-height: 1.6;
          color: #555;
        }
        /* About 後續區塊：標題／內文與「關於 The YNow App」同左緣 */
        #shiny-tab-about .ynow-about-section-title,
        #shiny-tab-about .ynow-about-section-lead {
          margin-left: 0;
          padding-left: 0;
        }
        #shiny-tab-about .ynow-about-section-title {
          margin-top: 0;
        }
        #shiny-tab-about .ynow-about-section > ul {
          margin-left: 0;
        }

        /* Backtest：策略參數 tabBox 輕量潤飾 */
        .ynow-bt-params .nav-tabs-custom > .nav-tabs {
          border-bottom-color: #e5e8eb;
        }
        .ynow-bt-params .nav-tabs-custom > .nav-tabs > li > a {
          border-radius: 6px 6px 0 0;
          font-size: 12.5px;
          font-weight: 600;
        }
        .ynow-bt-params .nav-tabs-custom > .tab-content {
          background: #fafbfc;
          border: 1px solid #e8ecef;
          border-top: 0;
          border-radius: 0 0 8px 8px;
          padding: 14px 16px 10px;
        }
        .ynow-bt-params .form-group {
          background: #ffffff;
          border: 1px solid #e8ecef;
          border-radius: 8px;
          padding: 10px 12px 6px;
          margin-bottom: 10px;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04);
        }
        .ynow-bt-params .form-group > label {
          font-size: 12.5px;
          font-weight: 600;
          color: #333;
        }

        /* 僅美化：情緒策略參數（寬螢幕四參數一列；按鈕獨立列） */
        .ynow-bt-mode-b .ynow-bt-mode-b-grid > .col-sm-3,
        .ynow-bt-mode-b .ynow-bt-mode-b-grid > .col-sm-4,
        .ynow-bt-mode-b .ynow-bt-mode-b-grid > .col-sm-6,
        .ynow-bt-mode-b .ynow-bt-mode-b-grid > .col-sm-12,
        .ynow-bt-mode-b .ynow-bt-mode-b-grid > .col-xs-12 {
          margin-bottom: 8px;
        }
        .ynow-bt-mode-b .ynow-bt-fit-row {
          clear: both;
          margin-top: 4px;
          padding-top: 12px;
          border-top: 1px solid #eee0b8;
        }
        .ynow-bt-mode-b .ynow-bt-fit-panel {
          padding: 12px 14px;
          background: #fcf8e3;
          border: 1px solid #f0e6b2;
          border-radius: 5px;
          font-size: 12px;
          color: #8a6d3b;
          line-height: 1.5;
        }
        .ynow-bt-mode-b .ynow-bt-fit-panel .btn {
          max-width: 320px;
        }

        /* 僅美化：回測驗證（寬螢幕三列） */
        .ynow-bt-validate .ynow-bt-validate-col {
          margin-bottom: 12px;
        }
        .ynow-bt-validate .ynow-bt-validate-col > h5 {
          margin: 0 0 6px 0;
          font-size: 13px;
        }
        .ynow-bt-validate .ynow-bt-validate-panel {
          height: 100%;
          padding: 10px 12px;
          background: #fafbfc;
          border: 1px solid #e8ecef;
          border-radius: 6px;
        }
        .ynow-bt-validate .ynow-bt-validate-panel .table {
          margin-bottom: 0;
          font-size: 12px;
        }

        /* Decision SOP — compact coach on Decision Checklist */
        .ynow-dc-sop--compact {
          margin: 0 0 14px 0;
          padding: 12px 14px;
          background: #fafbfc;
          border: 1px solid #e5e7eb;
          border-radius: 8px;
        }
        .ynow-dc-sop-head {
          margin: 0 0 10px 0;
        }
        .ynow-dc-sop-title {
          margin: 0 0 4px 0;
          font-size: 15px;
          font-weight: 700;
          color: #1f2937;
        }
        .ynow-dc-sop-intro {
          font-size: 12.5px;
          line-height: 1.45;
          color: #6b7280;
          margin: 0;
        }
        .ynow-dc-sop-reloc-note {
          font-size: 12.5px;
          line-height: 1.45;
          color: #6b7280;
          margin: 0 0 14px 0;
          padding: 8px 10px;
          background: #f8fafc;
          border-left: 3px solid #94a3b8;
          border-radius: 4px;
        }
        .ynow-dc-sop-steps {
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 10px;
          margin: 0;
          padding: 0;
        }
        @media (max-width: 991px) {
          .ynow-dc-sop-steps {
            grid-template-columns: 1fr;
          }
        }
        .ynow-dc-sop-step {
          margin: 0;
          padding: 10px 12px;
          background: #fff;
          border: 1px solid #e5e7eb;
          border-radius: 6px;
          list-style: none;
        }
        .ynow-dc-sop-step-top {
          display: flex;
          align-items: center;
          gap: 8px;
          margin: 0 0 8px 0;
        }
        .ynow-dc-sop-step-num {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 20px;
          height: 20px;
          border-radius: 50%;
          background: #1f2937;
          color: #fff;
          font-size: 11px;
          font-weight: 700;
          flex: 0 0 auto;
        }
        .ynow-dc-sop-step-title {
          font-size: 13px;
          font-weight: 700;
          color: #111827;
          line-height: 1.3;
        }
        .ynow-dc-sop-step-body {
          margin: 0 0 8px 0;
        }
        .ynow-dc-sop-chips {
          display: flex;
          flex-wrap: wrap;
          gap: 6px;
        }
        .ynow-dc-sop-chip {
          display: inline-block;
          font-size: 11.5px;
          line-height: 1.3;
          color: #374151;
          background: #f3f4f6;
          border-radius: 999px;
          padding: 3px 8px;
        }
        .ynow-dc-sop-step .checkbox {
          margin: 0;
        }
        .ynow-dc-sop-step .checkbox label {
          font-size: 12.5px;
          font-weight: 500;
          color: #1f2937;
        }
        .ynow-dc-sop-banner {
          margin-top: 10px;
          padding: 8px 10px;
          border-radius: 6px;
          font-size: 12.5px;
          font-weight: 600;
          display: flex;
          align-items: center;
          gap: 8px;
        }
        .ynow-dc-sop-banner.is-locked {
          background: #fef2f2;
          color: #b91c1c;
          border-left: 3px solid #b91c1c;
        }
        .ynow-dc-sop-banner.is-unlocked {
          background: #ecfdf5;
          color: #047857;
          border-left: 3px solid #047857;
        }
        .ynow-sop-verdict-locked {
          border-left: 4px solid #6c757d;
          margin-top: 0;
        }

        /* Decision Checklist — summary on top; theme items side-by-side */
        .ynow-dc-summary-top {
          width: 100%;
        }
        .ynow-dc-summary-top .ynow-dc-live {
          width: 100%;
        }
        .ynow-dc-summary-grid {
          display: flex;
          flex-wrap: wrap;
          gap: 10px;
          width: 100%;
          align-items: stretch;
        }
        .ynow-dc-summary-grid > .ynow-dc-row,
        .ynow-dc-summary-grid > .ynow-dc-row-skip {
          flex: 1 1 calc(50% - 10px);
          min-width: 260px;
          box-sizing: border-box;
        }
        .ynow-dc-section {
          margin: 0 0 18px 0;
        }
        .ynow-dc-section:last-child {
          margin-bottom: 0;
        }
        .ynow-dc-section-title {
          margin: 0 0 10px 0;
          padding: 0 0 6px 0;
          border-bottom: 1px solid #dee2e6;
          font-size: 13px;
          font-weight: 700;
          color: #1a1a1a;
          letter-spacing: 0.02em;
        }
        .ynow-dc-section-items {
          display: flex;
          flex-wrap: wrap;
          gap: 12px;
          align-items: stretch;
          width: 100%;
        }
        .ynow-dc-item {
          flex: 1 1 calc(50% - 12px);
          min-width: 260px;
          margin: 0;
          padding: 10px 12px;
          border: 1px solid #e9ecef;
          border-radius: 4px;
          background: #fff;
          box-sizing: border-box;
        }
        .ynow-dc-item--default {
          border-color: #b8dae0;
          background: #f7fbfc;
        }
        .ynow-dc-panel-hint {
          margin: 0 0 14px 0;
          font-size: 12.5px;
          color: #555;
          line-height: 1.5;
        }
        .ynow-dc-hint {
          margin: 2px 0 0 26px;
          font-size: 11.5px;
          color: #6c757d;
          line-height: 1.45;
        }
        .ynow-dc-cond-hint {
          margin: -4px 0 10px 0;
          font-size: 11px;
          color: #6c757d;
          line-height: 1.4;
        }
        .ynow-dc-conds .form-group {
          margin-bottom: 6px;
        }
        .ynow-dc-badge {
          margin-left: 6px;
          padding: 1px 6px;
          font-size: 10px;
          font-weight: 700;
          color: #0c5460;
          background: #d1ecf1;
          border-radius: 3px;
          white-space: nowrap;
        }
        .ynow-dc-conds {
          margin: 8px 0 0 26px;
          max-width: 100%;
        }
        .ynow-dc-label-wrap {
          display: inline;
        }
        @media (max-width: 767px) {
          .ynow-dc-item,
          .ynow-dc-summary-grid > .ynow-dc-row,
          .ynow-dc-summary-grid > .ynow-dc-row-skip {
            flex-basis: 100%;
            min-width: 0;
          }
          .ynow-dc-conds { margin-left: 0; max-width: 100%; }
          .ynow-dc-hint { margin-left: 0; }
          .ynow-dc-cond-hint { margin-left: 0; }
        }

        .ynow-bt-hfv-wrap {
          position: relative;
        }
        .ynow-bt-hfv-controls {
          position: static;
          display: flex;
          flex-wrap: wrap;
          align-items: flex-end;
          gap: 8px 28px;
          width: 100%;
          max-width: none;
          min-width: 0;
          margin: 0 0 12px 0;
          padding: 8px 12px 10px 12px;
          background: #f5f5f5;
          border: 1px solid #e4eaef;
          border-radius: 6px;
          box-shadow: none;
          font-size: 12px;
          box-sizing: border-box;
          overflow: visible;
        }
        .ynow-bt-hfv-bench {
          flex: 0 0 auto;
          padding-bottom: 2px;
        }
        .ynow-bt-hfv-models {
          flex: 1 1 280px;
          min-width: 0;
        }
        .ynow-bt-hfv-controls .form-group {
          margin-bottom: 0;
        }
        .ynow-bt-hfv-controls .control-label {
          display: block;
          font-size: 12px;
          font-weight: 600;
          margin-top: 0;
          margin-bottom: 6px;
          line-height: 1.4;
          white-space: normal;
        }
        .ynow-bt-hfv-controls .shiny-options-group {
          margin-top: 0;
          clear: none;
          display: flex;
          flex-wrap: wrap;
          column-gap: 18px;
          row-gap: 4px;
          align-items: center;
          padding-left: 0;
        }
        .ynow-bt-hfv-controls .radio {
          margin-top: 0;
          margin-bottom: 4px;
        }
        .ynow-bt-hfv-controls .radio label {
          font-size: 12px;
          font-weight: normal;
          line-height: 1.35;
        }
        .ynow-bt-hfv-controls .checkbox,
        .ynow-bt-hfv-controls .checkbox-inline {
          margin-top: 0;
          margin-bottom: 0;
          margin-left: 0 !important;
          min-height: 18px;
          padding-left: 0;
        }
        .ynow-bt-hfv-controls .checkbox label,
        .ynow-bt-hfv-controls .checkbox-inline {
          font-size: 12px;
          font-weight: normal;
          line-height: 1.35;
          padding-left: 20px;
          display: inline-block;
          min-height: 18px;
        }
        .ynow-bt-hfv-controls .checkbox input[type='checkbox'],
        .ynow-bt-hfv-controls .checkbox-inline input[type='checkbox'] {
          position: absolute;
          margin-left: -20px;
          margin-top: 2px;
        }
        @media (max-width: 767px) {
          .ynow-bt-hfv-controls {
            position: static;
            max-width: none;
            margin-top: 0;
          }
        }
        @media (max-width: 991px) {
          .ynow-bt-mode-b .ynow-bt-fit-panel .btn {
            max-width: none;
            width: 100%;
          }
        }
        
        /* 針對 search_results (產業資訊) 進行黑白主題與字體縮小；滿寬 */
        #search_results {
          width: 100% !important;
          display: block !important;
          box-sizing: border-box !important;
          white-space: pre-wrap !important;
          background-color: #1e1e1e !important;  /* 深黑色背景 */
          color: #eeeeee !important;             /* 淺白色文字 */
          font-size: 12px !important;            /* 縮小字體 */
          border: 1px solid #444444 !important;  /* 加上細緻的暗色邊框 */
          padding: 8px 12px !important;          /* 調整內邊距讓它扁平一點 */
          border-radius: 4px !important;         /* 圓角 */
          font-weight: 500 !important;
          line-height: 1.2 !important;
        }
        #search_results pre {
          width: 100% !important;
          display: block !important;
          margin: 0 !important;
          white-space: pre-wrap !important;
          background: transparent !important;
          border: none !important;
          color: inherit !important;
        }
      ")),
      # Custom screen DnD assets (Blue Chip lazy tab mounts UI later)
      tags$link(rel = "stylesheet", type = "text/css", href = "ynow_screener_dnd.css"),
      tags$script(src = "vendor/Sortable.min.js"),
      tags$script(src = "ynow_screener_dnd.js")
    ),
    
    # ==========================================
    # 獨立的 sc 搜尋輸入框與按鈕區塊
    # ==========================================
    # Author credit: same visual row as USD/TWD (document flow, not fixed/sticky)
    tags$div(
      class = "ynow-hdr-subband",
      tags$div(
        class = "ynow-credit-flow",
        tags$span(class = "ynow-credit-text", "a lawrence kuo shiny app")
      )
    ),
    # Ticker search + Yahoo industry chrome: hide on Home, About, Macro, Blue Chip,
    # Snapshot, Testing, and account maintenance (gate / 會員管理 / points). Guest
    # Sign-in-required surfaces (Lab / Checklist / Value Alerts) hide the same chrome
    # via CSS while gated. Snapshot is parameter audit / restore only — no Ticker /
    # Stock Code or Yahoo industry titles. (Business Breakdown shares global Ticker /
    # Stock Code chrome; Blue Chip uses its own universe Search, not this chrome).
    conditionalPanel(
      condition = paste(
        "input.sidebar_tabs != 'about' &&",
        "input.sidebar_tabs != 'macro_market' &&",
        "input.sidebar_tabs != 'asset_transmission' &&",
        "input.sidebar_tabs != 'bluechip' &&",
        "input.sidebar_tabs != 'snapshot' &&",
        "input.sidebar_tabs != 'testing' &&",
        "input.sidebar_tabs != 'home' &&",
        "input.sidebar_tabs != 'account_admin' &&",
        "input.sidebar_tabs != 'account_points' &&",
        "input.sidebar_tabs != 'settings'"
      ),
      tags$div(
        class = "ynow-stock-chrome",
        fluidRow(
          column(width = 12,
                 div(
                   class = "ynow-sc-row",
                   div(
                     class = "ynow-sc-wrap",
                     textInput(
                       "sc",
                       "Ticker / Stock Code",
                       value = "",
                       placeholder = "e.g. AAPL / TSM"
                     ),
                     uiOutput("sc_ticker_suggest_ui")
                   ),
                   actionButton(
                     "search",
                     "Search",
                     icon = icon("search"),
                     class = "ynow-search-btn"
                   )
                 )
          )
        ),
        fluidRow(
          column(
            width = 12,
            h2(uiOutput("txt_corpname", inline = TRUE), class = "ynow-corpname")
          )
        ),
        fluidRow(
          column(
            width = 12,
            uiOutput("ynow_data_gap_banner")
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$div(
              style = "width: 100%; text-align: left; margin-top: 8px;",
              tags$p(
                id = "ynow_industry_info_yahoo",
                "industry info from Yahoo",
                style = "font-size: 12px; color: #888; margin: 0 0 4px 0; font-weight: bold;"
              ),
              verbatimTextOutput("search_results")
            )
          )
        ),
        br()
      )
    ),
    # Shared ticker typeahead (Ticker/Stock Code + Radar focus); always loaded
    tags$script(HTML("
      (function() {
        if (document.documentElement.getAttribute('data-ynow-ticker-typeahead') === '1') return;
        document.documentElement.setAttribute('data-ynow-ticker-typeahead', '1');
        var typingOpen = {};

        function suggestIdForInput(inputId) {
          if (inputId === 'lab_cluster_focus') return 'lab_cluster_focus_suggest';
          return 'sc_ticker_suggest';
        }

        function inputValue(inputId) {
          var inp = document.getElementById(inputId);
          return inp ? (inp.value || '') : '';
        }

        function showSuggest(inputId) {
          var el = document.getElementById(suggestIdForInput(inputId));
          if (!el) return;
          var q = inputValue(inputId).trim();
          if (typingOpen[inputId] && q.length > 0 && el.children.length) {
            el.style.display = 'block';
          } else {
            el.style.display = 'none';
          }
        }

        /* If suggest DOM is still mounting (tab just shown), re-try briefly. */
        function showSuggestWhenReady(inputId, tries) {
          tries = (typeof tries === 'number') ? tries : 8;
          var el = document.getElementById(suggestIdForInput(inputId));
          if (el && el.children.length) {
            showSuggest(inputId);
            return;
          }
          if (tries <= 0) return;
          setTimeout(function() { showSuggestWhenReady(inputId, tries - 1); }, 60);
        }

        function hideSuggest(inputId) {
          if (inputId) {
            typingOpen[inputId] = false;
            var el = document.getElementById(suggestIdForInput(inputId));
            if (el) el.style.display = 'none';
            return;
          }
          ['sc', 'lab_cluster_focus'].forEach(hideSuggest);
        }

        function bindTypeahead(inputId) {
          $(document).on('input', '#' + inputId, function() {
            var v = $(this).val() || '';
            Shiny.setInputValue('ticker_typeahead', v, {priority: 'event'});
            typingOpen[inputId] = v.trim().length > 0;
            showSuggest(inputId);
          });

          $(document).on('blur', '#' + inputId, function(e) {
            var rt = e.relatedTarget;
            var el = document.getElementById(suggestIdForInput(inputId));
            if (el && rt && el.contains(rt)) return;
            hideSuggest(inputId);
          });

          $(document).on('keydown', '#' + inputId, function(e) {
            if (e.key === 'Enter' || e.keyCode === 13) hideSuggest(inputId);
          });
        }

        bindTypeahead('sc');
        bindTypeahead('lab_cluster_focus');

        $(document).on('click', '#search', function() {
          hideSuggest('sc');
        });

        // Recent / pinned tickers: click → reload; long-press 2s → pin/unpin (max 12).
        // Tab long-press is NOT pin — sibling customize+drag reorder owns that.
        (function() {
          var LONG_MS = 2000;
          var GUEST_KEY = 'ynow_pinned_recent_v1';
          var MAX_PINS = 12;
          var pressTimer = null;
          var pressEl = null;
          var longFired = false;
          var suppressClickUntil = 0;

          function guestRead() {
            try {
              var raw = localStorage.getItem(GUEST_KEY);
              if (!raw) return [];
              var arr = JSON.parse(raw);
              if (!Array.isArray(arr)) return [];
              return arr.map(function(x) { return String(x || '').trim().toUpperCase(); })
                .filter(function(x) { return !!x; })
                .slice(0, MAX_PINS);
            } catch (e) { return []; }
          }
          function guestWrite(arr) {
            try {
              localStorage.setItem(GUEST_KEY, JSON.stringify((arr || []).slice(0, MAX_PINS)));
            } catch (e) {}
          }
          window.ynowGuestPinnedRecent = {
            read: guestRead,
            write: guestWrite,
            clear: function() { try { localStorage.removeItem(GUEST_KEY); } catch (e) {} }
          };

          function clearPress() {
            if (pressTimer) { clearTimeout(pressTimer); pressTimer = null; }
            pressEl = null;
          }

          function fireToggle(el) {
            var tk = el.getAttribute('data-ticker') || '';
            if (!tk || !(window.Shiny && Shiny.setInputValue)) return;
            longFired = true;
            suppressClickUntil = Date.now() + 400;
            try { el.classList.add('ynow-chip-longpress'); } catch (e) {}
            setTimeout(function() {
              try { el.classList.remove('ynow-chip-longpress'); } catch (e2) {}
            }, 280);
            Shiny.setInputValue('pinned_search_toggle', tk + '|' + Date.now(), {priority: 'event'});
          }

          $(document).on('pointerdown', '.ynow-recent-chip, .ynow-pinned-chip', function(e) {
            if (e.pointerType === 'mouse' && e.button !== 0) return;
            clearPress();
            longFired = false;
            pressEl = this;
            var el = this;
            pressTimer = setTimeout(function() {
              if (pressEl === el) fireToggle(el);
            }, LONG_MS);
          });
          $(document).on('pointerup pointercancel pointerleave', '.ynow-recent-chip, .ynow-pinned-chip', function() {
            clearPress();
          });
          $(document).on('click keydown', '.ynow-recent-chip, .ynow-pinned-chip', function(e) {
            if (e.type === 'keydown' && e.key !== 'Enter' && e.key !== ' ') return;
            if (e.type === 'click' && (longFired || Date.now() < suppressClickUntil)) {
              e.preventDefault();
              e.stopImmediatePropagation();
              longFired = false;
              return;
            }
            e.preventDefault();
            var tk = this.getAttribute('data-ticker') || '';
            if (!tk || !(window.Shiny && Shiny.setInputValue)) return;
            Shiny.setInputValue('recent_search_pick', tk, {priority: 'event'});
          });

          // Guest: hydrate session from localStorage after Shiny connects.
          $(document).on('shiny:connected', function() {
            var pins = guestRead();
            if (pins.length && window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('pinned_search_guest_load', pins.join(',') + '|' + Date.now(), {priority: 'event'});
            }
          });
          Shiny.addCustomMessageHandler('ynowPinnedGuestSync', function(msg) {
            if (!msg) return;
            if (msg.clear) { guestWrite([]); return; }
            if (Array.isArray(msg.tickers)) guestWrite(msg.tickers);
          });
          Shiny.addCustomMessageHandler('ynowRecentSearchLabel', function(msg) {
            var recent = document.getElementById('ynow_recent_search_label');
            if (!recent || !msg) return;
            if (msg.hint) {
              recent.setAttribute('data-ynow-signed-in', '1');
              recent.setAttribute('title', String(msg.title || msg.hint));
            } else {
              recent.removeAttribute('data-ynow-signed-in');
              recent.removeAttribute('title');
            }
          });
        })();

        // Legacy page-pin UX disabled (v25.93 misread). Stock chips keep long-press;
        // menu long-press is edit-mode drag-reorder only. Stubs keep old handlers safe.
        (function() {
          var GUEST_KEY = 'ynow_pinned_sidebar_v1';
          window.ynowGuestPinnedSidebar = {
            read: function() { return []; },
            write: function() {},
            clear: function() { try { localStorage.removeItem(GUEST_KEY); } catch (e) {} }
          };
          window.ynowMarkSidebarMenuPinned = function() {};
          window.ynowSyncSidebarPinLocks = function() {};
          Shiny.addCustomMessageHandler('ynowPinnedSidebarGuestSync', function(msg) {
            if (msg && msg.clear) {
              try { localStorage.removeItem(GUEST_KEY); } catch (e) {}
            }
          });
          /* Shiny 1.14+ requires handler.length === 1; arity-0 throws and aborts
             the rest of this script (including the reorder IIFE below). */
          Shiny.addCustomMessageHandler('ynowPinnedSidebarMark', function(_msg) {});
        })();

        // Signed-in sidebar tab reorder: customize button → long-press drag → Done persists.
        (function() {
          var LONG_MS = 420;
          var editOn = false;
          var pressTimer = null;
          var pressLi = null;
          var dragging = false;
          var dragLi = null;
          var startY = 0;
          var suppressClickUntil = 0;
          var pendingOrder = null;

          function locStr(key, fallback) {
            var s = window.__ynowUiLocaleStrings || {};
            if (s[key]) return String(s[key]);
            return fallback;
          }

          function syncBtnLocale(s) {
            var full = window.__ynowUiLocaleStrings || {};
            s = s || full;
            var btn = document.getElementById('ynow_sidebar_reorder_btn');
            var lab = document.getElementById('ynow_sidebar_reorder_btn_label');
            if (!btn) return;
            var editing = !!(document.body && document.body.classList.contains('ynow-sidebar-reorder-edit'));
            var label = editing
              ? (s.sidebar_reorder_btn_done || full.sidebar_reorder_btn_done || 'Done')
              : (s.sidebar_reorder_btn || full.sidebar_reorder_btn || 'Rearrange');
            var title = editing
              ? (s.sidebar_reorder_btn_done_title || full.sidebar_reorder_btn_done_title ||
                 'Save order and exit edit mode')
              : (s.sidebar_reorder_btn_title || full.sidebar_reorder_btn_title ||
                 'Rearrange sidebar tab order');
            if (lab) lab.textContent = label;
            btn.setAttribute('title', title);
            btn.setAttribute('aria-label', title);
            btn.setAttribute('aria-pressed', editing ? 'true' : 'false');
          }
          window.ynowSyncSidebarReorderBtnLocale = syncBtnLocale;

          function menuRoot() {
            /* shinydashboard puts id=sidebar_tabs on a hidden DIV
               (.sidebarMenuSelectedTabItem), not the UL. Prefer the real menu. */
            return document.querySelector('.main-sidebar ul.sidebar-menu') ||
              document.querySelector('ul.sidebar-menu');
          }

          function isVisibleLi(li) {
            if (!li || li.tagName !== 'LI') return false;
            try {
              var st = window.getComputedStyle(li);
              if (st && (st.display === 'none' || st.visibility === 'hidden')) return false;
            } catch (e) {}
            return true;
          }

          function orderIdForLi(li) {
            if (!li) return '';
            var existing = li.getAttribute('data-ynow-order-id');
            if (existing) return existing;
            var a = li.querySelector(':scope > a') || li.firstElementChild;
            if (!a) return '';
            var dv = a.getAttribute('data-value') || '';
            if (dv) {
              li.setAttribute('data-ynow-order-id', dv);
              return dv;
            }
            var cat = a.querySelector('[id^=\"ynow_menu_cat_\"]');
            if (cat && cat.id) {
              var id = String(cat.id).replace(/^ynow_menu_/, '');
              li.setAttribute('data-ynow-order-id', id);
              return id;
            }
            var admin = a.querySelector('#ynow_menu_admin');
            if (admin) {
              li.setAttribute('data-ynow-order-id', 'account_admin');
              return 'account_admin';
            }
            return '';
          }

          function tagMenuItems() {
            var ul = menuRoot();
            if (!ul) return;
            Array.prototype.forEach.call(ul.children, function(li) {
              if (li.tagName === 'LI') orderIdForLi(li);
            });
          }

          function readOrderFromDom() {
            var ul = menuRoot();
            if (!ul) return [];
            var out = [];
            Array.prototype.forEach.call(ul.children, function(li) {
              if (li.tagName !== 'LI') return;
              var id = orderIdForLi(li);
              if (id) out.push(id);
            });
            return out;
          }

          function applyOrder(order) {
            var ul = menuRoot();
            if (!ul || !order || !order.length) return;
            tagMenuItems();
            var map = {};
            Array.prototype.forEach.call(ul.children, function(li) {
              if (li.tagName !== 'LI') return;
              var id = orderIdForLi(li);
              if (id) map[id] = li;
            });
            var used = {};
            order.forEach(function(id) {
              var li = map[id];
              if (!li || used[id]) return;
              used[id] = true;
              ul.appendChild(li);
            });
            Object.keys(map).forEach(function(id) {
              if (!used[id]) ul.appendChild(map[id]);
            });
          }
          window.ynowApplySidebarTabOrder = applyOrder;

          function setEditMode(on) {
            editOn = !!on;
            if (document.body) {
              document.body.classList.toggle('ynow-sidebar-reorder-edit', editOn);
            }
            syncBtnLocale();
            if (editOn) tagMenuItems();
          }

          function clearPress() {
            if (pressTimer) { clearTimeout(pressTimer); pressTimer = null; }
            pressLi = null;
          }

          function clearDragClasses() {
            document.querySelectorAll('.sidebar-menu > li.ynow-sidebar-reorder-dragging, .sidebar-menu > li.ynow-sidebar-reorder-drop-target')
              .forEach(function(li) {
                li.classList.remove('ynow-sidebar-reorder-dragging');
                li.classList.remove('ynow-sidebar-reorder-drop-target');
              });
          }

          function endDrag(commit) {
            clearPress();
            if (dragging && dragLi) {
              suppressClickUntil = Date.now() + 350;
            }
            dragging = false;
            dragLi = null;
            clearDragClasses();
            if (commit) pendingOrder = readOrderFromDom();
          }

          function topLiFromEventTarget(t) {
            if (!t || !t.closest) return null;
            var a = t.closest('.sidebar-menu > li > a');
            if (!a) return null;
            /* Submenu leaves live under .treeview-menu — not top-level reorder targets. */
            if (a.closest('.treeview-menu')) return null;
            var li = a.parentElement;
            if (!li || li.tagName !== 'LI') return null;
            if (!isVisibleLi(li)) return null;
            return li;
          }

          function liAtPoint(x, y) {
            var el = document.elementFromPoint(x, y);
            return topLiFromEventTarget(el);
          }

          $(document).on('click', '#ynow_sidebar_reorder_btn', function(e) {
            e.preventDefault();
            e.stopPropagation();
            var authed = !!(document.body && document.body.classList.contains('ynow-authed'));
            if (!authed) {
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('sidebar_tab_order_guest', Date.now(), {priority: 'event'});
              }
              return;
            }
            if (!editOn) {
              setEditMode(true);
              pendingOrder = readOrderFromDom();
              return;
            }
            /* Confirm + exit */
            var order = readOrderFromDom();
            setEditMode(false);
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue(
                'sidebar_tab_order_save',
                order.join(',') + '|' + Date.now(),
                {priority: 'event'}
              );
            }
          });

          $(document).on('pointerdown', '.sidebar-menu > li > a', function(e) {
            if (!editOn) return;
            if (e.pointerType === 'mouse' && e.button !== 0) return;
            if (this.closest && this.closest('.treeview-menu')) return;
            var li = this.parentElement;
            if (!li || li.tagName !== 'LI' || !isVisibleLi(li)) return;
            clearPress();
            pressLi = li;
            startY = e.clientY || 0;
            var captured = li;
            pressTimer = setTimeout(function() {
              if (pressLi !== captured) return;
              dragging = true;
              dragLi = captured;
              captured.classList.add('ynow-sidebar-reorder-dragging');
              try { captured.setPointerCapture && e.pointerId != null && captured.setPointerCapture(e.pointerId); } catch (eCap) {}
            }, LONG_MS);
          });

          $(document).on('pointermove', function(e) {
            if (!editOn) return;
            if (!dragging && pressLi) {
              var dy = Math.abs((e.clientY || 0) - startY);
              if (dy > 10) clearPress();
              return;
            }
            if (!dragging || !dragLi) return;
            e.preventDefault();
            var over = liAtPoint(e.clientX, e.clientY);
            document.querySelectorAll('.sidebar-menu > li.ynow-sidebar-reorder-drop-target')
              .forEach(function(n) { n.classList.remove('ynow-sidebar-reorder-drop-target'); });
            if (!over || over === dragLi) return;
            over.classList.add('ynow-sidebar-reorder-drop-target');
            var ul = menuRoot();
            if (!ul) return;
            var mid = over.getBoundingClientRect().top + over.getBoundingClientRect().height / 2;
            if ((e.clientY || 0) < mid) {
              ul.insertBefore(dragLi, over);
            } else if (over.nextSibling) {
              ul.insertBefore(dragLi, over.nextSibling);
            } else {
              ul.appendChild(dragLi);
            }
          });

          $(document).on('pointerup pointercancel', function() {
            if (!editOn) { clearPress(); return; }
            if (dragging) endDrag(true);
            else clearPress();
          });

          $(document).on('click', '.sidebar-menu > li > a', function(e) {
            if (!editOn) return;
            if (dragging || Date.now() < suppressClickUntil) {
              e.preventDefault();
              e.stopImmediatePropagation();
            }
          });

          Shiny.addCustomMessageHandler('ynowSidebarTabOrder', function(msg) {
            if (!msg) return;
            var order = Array.isArray(msg.order) ? msg.order : [];
            applyOrder(order.map(function(x) { return String(x || ''); }).filter(Boolean));
          });

          $(document).on('shiny:connected', function() {
            tagMenuItems();
            syncBtnLocale();
          });

          /* Exit edit mode on sign-out */
          var _authState = null;
          try {
            /* Observe body class changes for ynow-authed removal */
            if (window.MutationObserver && document.body) {
              new MutationObserver(function() {
                var authed = document.body.classList.contains('ynow-authed');
                if (!authed && editOn) setEditMode(false);
                syncBtnLocale();
              }).observe(document.body, { attributes: true, attributeFilter: ['class'] });
            }
          } catch (eObs) {}
        })();

        $(document).on('mousedown', '#sc_ticker_suggest .ynow-suggest-item, #lab_cluster_focus_suggest .ynow-suggest-item', function(e) {
          e.preventDefault();
          var sym = $(this).data('symbol');
          var list = this.closest ? this.closest('.ynow-ticker-suggest, #sc_ticker_suggest, #lab_cluster_focus_suggest') : null;
          var inputId = (list && list.id === 'lab_cluster_focus_suggest') ? 'lab_cluster_focus' : 'sc';
          hideSuggest(inputId);
          if (sym) {
            $('#' + inputId).val(sym).trigger('change');
            Shiny.setInputValue(inputId, sym, {priority: 'event'});
          }
        });

        $(document).on('shiny:value', function(e) {
          if (e.name === 'sc_ticker_suggest_ui') setTimeout(function() { showSuggest('sc'); }, 0);
          if (e.name === 'lab_cluster_focus_suggest_ui') setTimeout(function() { showSuggest('lab_cluster_focus'); }, 0);
        });
        /* Leaving Home/Macro/Blue Chip remounts stock chrome; re-open if still typing. */
        $(document).on('shiny:inputchanged', function(e) {
          if (!e || e.name !== 'sidebar_tabs') return;
          var tabNow = String(e.value || '');
          if (tabNow && tabNow !== 'testing' && document.body &&
              document.body.classList.contains('ynow-authed') &&
              typeof window.ynowLastUiWrite === 'function') {
            try { window.ynowLastUiWrite(tabNow, null); } catch (eLastUi) {}
          }
          setTimeout(function() {
            if (typingOpen.sc) showSuggestWhenReady('sc');
            if (typingOpen.lab_cluster_focus) showSuggestWhenReady('lab_cluster_focus');
          }, 0);
        });
      })();
    ")),
    # Header KPIs: Company (dashboard) only (Last Price / Market Cap / EPS TTM).
    # Smart Analysis (Lite) uses its own fair-value cards — no quote KPI strip.
    # Shell matches Company Overview width (same --ynow-page-max as .tab-pane).
    conditionalPanel(
      condition = "input.sidebar_tabs == 'dashboard'",
      tags$div(
        class = "ynow-header-kpi-shell",
        fluidRow(
          class = "ynow-header-kpi-row",
          column(
            width = 4,
            class = "col-xs-4",
            uiOutput("ibx_stockprice")
          ),
          column(
            width = 4,
            class = "col-xs-4",
            infoBoxOutput("ibx_marketcap", width = NULL)
          ),
          column(
            width = 4,
            class = "col-xs-4",
            uiOutput("ibx_EPS")
          )
        )
      )
    ),
    # Model pages + Smart Analysis: full composite valuation block (shared output) + forecast years
    # Lite Smart Analysis: explanatory blurb sits immediately above composite status (no page heading).
    conditionalPanel(
      condition = paste(
        "input.sidebar_tabs == 'smart_analysis' ||",
        "input.sidebar_tabs == 'dcf_calculator' ||",
        "input.sidebar_tabs == 'ddm_calculator' ||",
        "input.sidebar_tabs == 'pb_calculator' ||",
        "input.sidebar_tabs == 'rel_mult_ev' ||",
        "input.sidebar_tabs == 'rel_multiples_calculator' ||",
        "input.sidebar_tabs == 'rel_mult_rev' ||",
        "input.sidebar_tabs == 'sotp_calculator' ||",
        "input.sidebar_tabs == 'ri_calculator' ||",
        "input.sidebar_tabs == 'nav_calculator'"
      ),
      conditionalPanel(
        condition = "input.sidebar_tabs == 'smart_analysis'",
        tags$div(
          class = "ynow-lite-only ynow-smart-lite-blurb-wrap",
          tags$p(
            id = "ynow_smart_page_sub",
            class = "ynow-smart-lite-blurb",
            paste0(
              "Auto-selects primary and secondary valuation models from the ticker profile, ",
              "detects the best parameter scenario (Two-Stage vs Gordon, SGR method, claim), ",
              "applies those defaults, and shows fair-value charts. No manual model settings."
            )
          )
        )
      ),
      fluidRow(
        class = "ynow-header-composite-row",
        column(
          width = 12,
          class = "col-xs-12",
          decision_valuation_compare_ui("main_decision")
        )
      ),
      # DCF-Model：模型選擇／採用現金流 與下方 預測年數 n／建議 同為 6+6 靠左對齊
      conditionalPanel(
        condition = "input.sidebar_tabs == 'dcf_calculator'",
        fluidRow(
          class = "ynow-dcf-mode-row",
          column(
            width = 6,
            class = "col-xs-12 col-sm-6",
            tags$div(
              class = "ynow-dcf-mode-col",
              radioButtons(
                "dcf_mode", ui_i18n("dcf_mode_label"),
                choiceNames = list(
                  ui_i18n("dcf_mode_gordon"),
                  ui_i18n("dcf_mode_two_stage")
                ),
                choiceValues = list("gordon", "two_stage"),
                selected = APP_DEFAULTS$dcf_mode
              )
            )
          ),
          column(
            width = 6,
            class = "col-xs-12 col-sm-6",
            tags$div(
              class = "ynow-dcf-claim-col",
              radioButtons(
                "dcf_claim",
                ui_i18n("dcf_claim_label"),
                choiceNames = list(
                  ui_i18n("dcf_claim_fcff"),
                  ui_i18n("dcf_claim_fcfe")
                ),
                choiceValues = list("fcff", "fcfe"),
                selected = APP_DEFAULTS$dcf_claim,
                inline = TRUE
              ),
              helpText(ui_i18n("dcf_claim_help"))
            )
          )
        )
      ),
      # 預測年數 n：僅 DCF 明確預測期使用。
      # DDM／RI 各自有 n1／ri_years；P/B／NAV／Multiples／SOTP 不涉及年數假設。
      # Lite Smart Analysis：不顯示手動模型參數（含預測年數）
      conditionalPanel(
        condition = "input.sidebar_tabs == 'dcf_calculator'",
        fluidRow(
          class = "ynow-header-years-suggest-row",
          column(
            width = 6,
            class = "col-xs-12 col-sm-6",
            tags$div(
              class = "ynow-header-years",
              numericInput(
                "years", ui_i18n("dcf_years_label"),
                value = APP_DEFAULTS$years, min = 1, max = 30
              )
            )
          ),
          column(
            width = 6,
            class = "col-xs-12 col-sm-6",
            tags$div(
              class = "ynow-dcf-claim-suggest-wrap",
              uiOutput("dcf_claim_suggest")
            )
          )
        )
      )
    ),

    .ynow_tab_items(
      tabItem(
        tabName = "home",
        .home_brand_ui()
      ),
      tabItem(
        tabName = "get_started",
        tags$div(
          id = "ynow_explain_core_callout",
          class = "ynow-explain-core-callout ynow-full-only ynow-admin-dev-explain",
          style = paste0(
            "margin:0 0 14px 0; padding:10px 14px; ",
            "border:1px solid #c5d8ea; border-left:4px solid #0C5484; ",
            "border-radius:4px; background:#f3f8fc; font-size:12.5px; line-height:1.5; color:#333;"
          ),
          tags$b(id = "ynow_explain_core_callout_title", style = "color:#0C5484;", "可解釋性（核心）"),
          tags$span(id = "ynow_explain_core_callout_body", paste0(
            "：每個估值結果旁應能回答「為何是這個合理價」。",
            "DCF Overview 的 KPI 下方已顯示公式路徑／現值橋接／終值驅動；",
            "各模型「公式參數貢獻與敏感度」預設展開。完整 Decision Matrix 見本頁下方。"
          ))
        ),
        # Full-mode primary workflow: same Why + gate surface as Smart Analysis
        uiOutput("dashboard_value_why"),
        fluidRow(
          box(
            title = tagList(
              icon("route"),
              tags$span(id = "ynow_gs_model_selector_title", "基本面評價模型")
            ),
            width = 12, status = "primary", solidHeader = TRUE,
            uiOutput("get_started_model_selector"),
            .model_selector_dimensions_annotation_ui()
          )
        ),
        # SGR 與 BETA 同層、同 col-sm-12（勿只對 SGR 外包 fluidRow，否則欄寬會不一致）
        # Order: SGR → BETA → shared Cash/Debt/Shares/Revenue (plain-language shared inputs)
        fluidRow(
          .dcf_core_params_box()
        ),
        fluidRow(
          tags$div(
            `data-ynow-lock` = "section:beta_box",
            tabBox(
              title = "BETA",
              width = 12,
              tabPanel(
                "Beta Overview",
                icon = icon("th-large"),
                helpText(
                  id = "ynow_gs_beta_overview_help",
                  paste0(
                    "Intrinsic-value path: Summary β is written into CAPM by default; ",
                    "you can switch to industry / Bottom-Up / unlevered / manual. ",
                    "Rolling estimates are for cross-check only and are not written into CAPM."
                  )
                ),
                beta_overview_section_ui()
              ),
              tabPanel(
                "Peer Unlever",
                value = "peer_unlever",
                icon = icon("users"),
                helpText(
                  id = "ynow_gs_peer_unlever_help",
                  paste0(
                    "Left: enter peers, unlever, then average (Bottom-Up). ",
                    "Right: this firm's Hamada unlever and manual βe. ",
                    "To write into CAPM: pick the matching β source on Beta Overview or DCF → Beta."
                  )
                ),
                beta_unlever_section_ui()
              ),
              tabPanel(
                "Rolling β",
                icon = icon("chart-area"),
                helpText(
                  id = "ynow_gs_rolling_help",
                  paste0(
                    "Use Rolling β to cross-check valuation β (includes sentiment / event noise). ",
                    "It is not written into CAPM; if it diverges sharply from Bottom-Up βᵤ, ",
                    "review peers, capital structure, events, and liquidity."
                  )
                ),
                beta_rolling_section_ui()
              )
            )
          )
        ),
        fluidRow(
          .shared_bridge_box()
        ),
        # Valuation Methodology at bottom of Basic Setup (Full only; Lite hides get_started)
        tags$div(
          class = "ynow-full-only",
          style = "margin-top: 20px;",
          tags$hr(style = "margin: 8px 0 20px 0; border-color: #e5e8eb;"),
          .valuation_methodology_section_ui(collapsible = FALSE, collapsed = FALSE)
        )
      ),

      tabItem(
        tabName = "snapshot",
        h2(tags$span(id = "ynow_snapshot_page_title", "Snapshot")),
        tags$p(
          id = "ynow_snapshot_page_help",
          class = "help-block ynow-full-only",
          paste0(
            "Three tabs: manual adjustments vs the post-Search baseline (with annotated PDF); ",
            "current live parameters (download / upload restore CSV); and APP_DEFAULTS ",
            "(that tab shows a lock until System admin unlock; the defaults table appears only after unlock)."
          )
        ),
        tags$p(
          id = "ynow_snapshot_page_help_lite",
          class = "help-block ynow-lite-only",
          paste0(
            "Lite can show System defaults (APP_DEFAULTS) for parameters you can set in Lite ",
            "(Dashboard industry / currency / default ticker, Blue Chip ranking and Clustering) ",
            "after System admin unlock. Manual audit and live snapshot restore stay in Full mode."
          )
        ),
        tags$div(
          id = "ynow_inner_tab_lock_promo_tpl",
          class = "ynow-inner-tab-lock-tpl ynow-gate-callout",
          hidden = NA,
          `data-ynow-gate-feature` = "inner_lock",
          `data-ynow-lock-scope` = "ops_acl",
          tags$p(
            class = "ynow-gate-body",
            "Sign in to open this locked tab. If you are already signed in, a system admin must clear the restriction."
          ),
          if (exists("ynow_gate_promo_faq_ui", mode = "function")) {
            ynow_gate_promo_faq_ui(
              feature = "inner_lock",
              include_faq = FALSE,
              compact = FALSE
            )
          } else {
            NULL
          },
          tags$div(
            class = "ynow-auth-pair ynow-auth-pair--gate",
            tags$button(
              type = "button",
              class = "btn btn-primary ynow-auth-open ynow-auth-btn",
              tags$span(class = "ynow-member-gate-signin-label", "Sign in")
            ),
            tags$button(
              type = "button",
              class = "btn btn-default ynow-auth-register-open ynow-auth-btn",
              tags$span(class = "ynow-auth-register-label", "Register")
            )
          )
        ),
        tags$div(
          id = "ynow_atx_lock_promo_tpl",
          class = "ynow-atx-lock-tpl ynow-member-gate-surface ynow-member-gate-surface--checklist ynow-member-gate-surface--locked",
          hidden = NA,
          `data-ynow-gate-feature` = "atx",
          if (exists("ynow_member_gate_fade_backdrop_ui", mode = "function")) {
            ynow_member_gate_fade_backdrop_ui("atx")
          } else {
            tags$div(class = "ynow-member-gate-fade-backdrop", `aria-hidden` = "true")
          },
          tags$section(
            class = "ynow-hfv-chapter ynow-member-gate ynow-gate-box ynow-member-gate--surface-overlay",
            `data-ynow-gate-feature` = "atx",
            `data-ynow-lock-scope` = "ops_acl",
            tags$div(
              class = "ynow-hfv-chapter__head",
              tags$span(class = "ynow-hfv-chapter__kicker ynow-gate-section-kicker", "Member access"),
              tags$h3(
                class = "ynow-hfv-chapter__title",
                tags$span(class = "ynow-auth-gate-title", "Sign in required")
              )
            ),
            tags$div(
              class = "ynow-hfv-chapter__body",
              tags$p(
                class = "ynow-gate-body",
                "Asset Transmission Root is locked. Sign in to open this page. If you are already signed in, a system admin must clear the restriction."
              ),
              if (exists("ynow_gate_promo_faq_ui", mode = "function")) {
                ynow_gate_promo_faq_ui(
                  feature = "atx",
                  include_faq = TRUE,
                  compact = FALSE
                )
              } else {
                NULL
              },
              tags$div(
                class = "ynow-auth-pair ynow-auth-pair--gate",
                tags$button(
                  type = "button",
                  class = "btn btn-primary ynow-auth-open ynow-auth-btn",
                  tags$span(class = "ynow-member-gate-signin-label", "Sign in")
                ),
                tags$button(
                  type = "button",
                  class = "btn btn-default ynow-auth-register-open ynow-auth-btn",
                  tags$span(class = "ynow-auth-register-label", "Register")
                )
              )
            )
          )
        ),
        tabBox(
          # No right-side chrome title (page h2 already says Snapshot; avoids wrong remaps)
          id = "snapshot_report",
          width = "auto",

          tabPanel(
            title = tagList(
              icon("user-edit"),
              tags$span(id = "ynow_snapshot_tab_audit", "Manual adjustments (vs post-Search baseline)")
            ),
            value = "snap_audit",
            tags$div(
              `data-ynow-lock` = "section:snap_audit",
              tags$p(
                id = "ynow_param_audit_help",
                style = "font-size:12.5px; color:#666; line-height:1.45; margin:0 0 10px 0;",
                paste0(
                  "Baseline locks after Search and statement auto-fill. ",
                  "Later manual overrides are listed by page. ",
                  "Use Go & highlight to jump and frame the input. ",
                  "Below: select pages and generate an annotated screenshot PDF."
                )
              ),
              uiOutput("param_audit_report"),
              tags$hr(style = "margin:14px 0;"),
              tags$h4(
                id = "ynow_param_audit_pdf_title",
                style = "margin:0 0 8px 0; font-size:15px; font-weight:700;",
                "Annotated page screenshots (PDF)"
              ),
              tags$p(
                id = "ynow_param_audit_pdf_help",
                style = "font-size:12.5px; color:#666; line-height:1.45; margin:0 0 10px 0;",
                paste0(
                  "Select pages by category. The app switches to each page (and statement block when chosen), ",
                  "captures the live layout, draws boxes on inputs you changed vs the post-Search baseline, ",
                  "and downloads one PDF."
                )
              ),
              if (exists("ynow_param_audit_pdf_pages_ui", mode = "function")) {
                ynow_param_audit_pdf_pages_ui()
              } else {
                checkboxGroupInput(
                  "param_audit_pdf_pages",
                  label = tags$span(id = "ynow_param_audit_pdf_pages_label", "Pages to capture"),
                  choiceNames = list(
                    tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_basic",
                              "Model Dashboard (SGR / CAPM / WACC / Beta)"),
                    tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_dcf", "DCF"),
                    tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_ddm", "DDM"),
                    tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_ri", "RI"),
                    tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_pb", "P/B"),
                    tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_nav", "NAV")
                  ),
                  choiceValues = c(
                    "get_started", "dcf_calculator", "ddm_calculator",
                    "ri_calculator", "pb_calculator", "nav_calculator"
                  ),
                  selected = c(
                    "get_started", "dcf_calculator", "ddm_calculator",
                    "ri_calculator", "pb_calculator", "nav_calculator"
                  )
                )
              },
              div(
                style = "display:flex; flex-wrap:wrap; gap:10px; align-items:center; margin-top:8px;",
                actionButton(
                  "param_audit_pdf_go",
                  tagList(icon("file-pdf"), tags$span(id = "ynow_param_audit_pdf_btn", "Generate annotated PDF")),
                  class = "btn-success"
                ),
                tags$span(
                  id = "ynow_param_audit_pdf_status",
                  style = "font-size:12.5px; color:#555;"
                )
              )
            )
          ),

          tabPanel(
            title = tagList(
              icon("camera"),
              tags$span(id = "ynow_snapshot_tab_current", "Current App Parameter Snapshot")
            ),
            value = "snap_current",
            tags$p(
              id = "ynow_param_restore_help",
              style = "font-size:12.5px; color:#666; line-height:1.45; margin:0 0 10px 0;",
              paste0(
                "Download a restore CSV to save your current valuation inputs. ",
                "Later, upload that file and click Restore to write the values back into the App, ",
                "then continue DCF / DDM / RI / P/B / NAV analysis. ",
                "Prefer Search (load statements) first when the ticker differs."
              )
            ),
            div(
              style = "display:flex; flex-wrap:wrap; justify-content:space-between; align-items:center; gap:12px; margin-bottom:10px;",
              uiOutput("snapshot_timestamp"),
              div(
                style = "display:flex; flex-wrap:wrap; gap:8px; align-items:center;",
                downloadButton(
                  "download_snapshot",
                  tags$span(id = "ynow_download_snapshot_btn", "Download Snapshot CSV"),
                  icon = icon("download")
                ),
                downloadButton(
                  "download_param_restore",
                  tags$span(id = "ynow_download_param_restore_btn", "Download restore CSV"),
                  icon = icon("save"),
                  class = "btn-primary"
                )
              )
            ),
            tags$p(
              id = "ynow_snapshot_download_quota",
              class = "ynow-snapshot-download-quota",
              role = "status",
              `aria-live` = "polite",
              "Sign-in required. Snapshot / restore CSV downloads are free after sign-in (no FYN)."
            ),
            tags$div(
              class = "ynow-param-restore-box",
              `data-ynow-lock` = "section:snapshot_restore",
              style = "margin:0 0 14px 0; padding:12px 14px; background:#f7f9fc; border:1px solid #e2e8f0; border-radius:6px; border-left:4px solid #3c8dbc;",
              tags$h4(
                id = "ynow_param_restore_title",
                style = "margin:0 0 8px 0; font-size:15px; font-weight:700;",
                "Restore parameters from file"
              ),
              fluidRow(
                column(
                  width = 7,
                  fileInput(
                    "param_restore_file",
                    label = tags$span(id = "ynow_param_restore_file_label", "Upload restore CSV"),
                    accept = c(".csv", "text/csv", "text/comma-separated-values"),
                    buttonLabel = "Browse…",
                    placeholder = "No file selected"
                  )
                ),
                column(
                  width = 5,
                  tags$div(
                    style = "margin-top:24px;",
                    actionButton(
                      "param_restore_go",
                      tagList(
                        icon("upload"),
                        tags$span(id = "ynow_param_restore_btn", "Restore parameters")
                      ),
                      class = "btn-success"
                    )
                  )
                )
              ),
              tags$span(
                id = "ynow_param_restore_status",
                style = "font-size:12.5px; color:#555;"
              ),
              uiOutput("param_restore_status_ui")
            ),
            # Bear／Base／Bull scenario rules for every valuation engine
            # (moved from Model Dashboard SGR box — Snapshot is the review surface).
            tags$div(
              class = "ynow-scenario-audit-wrap",
              style = "margin:0 0 16px 0; padding:12px 14px; background:#fafbfc; border:1px solid #e5e7eb; border-radius:6px;",
              uiOutput("txt_scenario_audit")
            ),
            dataTableOutput("snapshot_table")
          ),

          tabPanel(
            title = tagList(
              icon("lock", class = "ynow-snap-tab-lock"),
              icon("sliders-h"),
              tags$span(id = "ynow_snapshot_tab_defaults", "System defaults (APP_DEFAULTS)")
            ),
            value = "snap_defaults",
            tags$div(
              id = "ynow_snapshot_defaults_locked",
              class = "ynow-snap-defaults-locked ynow-gate-callout",
              `data-ynow-gate-feature` = "snap_defaults",
              `data-ynow-lock-scope` = "ops_sysadmin",
              tags$span(
                id = "ynow_snapshot_defaults_locked_msg",
                class = "ynow-gate-body",
                paste0(
                  "System defaults (APP_DEFAULTS) require System admin unlock ",
                  "(same session as Points → System admin). ",
                  "After Exit system admin, this tab locks again."
                )
              ),
              if (exists("ynow_gate_promo_faq_ui", mode = "function")) {
                ynow_gate_promo_faq_ui(
                  feature = "snap_defaults",
                  include_faq = FALSE,
                  compact = FALSE
                )
              } else {
                NULL
              }
            ),
            div(
              class = "ynow-snap-defaults-body",
              div(
                style = "display:flex; justify-content:space-between; align-items:center; gap:12px; margin-bottom:10px; flex-wrap:wrap;",
                tags$span(
                  id = "ynow_snapshot_defaults_help",
                  class = "ynow-full-only",
                  style = "font-size:12.5px; color:#666; line-height:1.45;",
                  paste0(
                    "Defaults written at App start (including items estimated from the default industry / Rf). ",
                    "May differ from Current App Parameter Snapshot; fields can still be overridden on each page."
                  )
                ),
                tags$span(
                  id = "ynow_snapshot_defaults_help_lite",
                  class = "ynow-lite-only",
                  style = "font-size:12.5px; color:#666; line-height:1.45;",
                  paste0(
                    "Only parameters Lite can set (Dashboard industry / currency / default ticker, ",
                    "plus Blue Chip ranking and Clustering). Smart Analysis engine seeds and Full-only ",
                    "model parameters are hidden here."
                  )
                ),
                downloadButton(
                  "download_defaults",
                  tags$span(id = "ynow_download_defaults_btn", "Download Defaults CSV"),
                  icon = icon("download")
                )
              ),
              dataTableOutput("defaults_table")
            )
          )
        )
      ),

      tabItem(
        tabName = "macro_market",
        uiOutput("ynow_lazy_host_macro_market")
      ),

      tabItem(tabName = "dashboard",

              uiOutput("company_overview_ui"),
              uiOutput("ynow_news_thermometer"),

              tags$div(
                class = "ynow-full-only",
              tabBox(title = "FINANCIAL REPORT",
                     id = "dashboard_fin_report",
                     width = "auto",
                     
                     tabPanel("Finance Summary",
                              p("This section imports Finance Summaries from Yahoo Finance",
                                style = "margin-bottom: 12px; color: #666; font-size: 13px;"),
                              uiOutput("fs_summary_ui"),
                              downloadButton('FS_download', "Download Finance Summary")
                     ),
                     
                     tabPanel(
                       "Income Statement",
                       icon = icon("chart-line"),
                       p("This section imports Income Statements from Yahoo Finance"),
                       
                       # 🌟 新增：Income Statement 下拉選單與互動圖表
                       selectInput("is_type", "Select Income Statement Metric",
                                   choices = c("Total Revenue", "Gross Profit", "EBITDA")),
                       tags$div(
                         class = "ynow-fs-chart ynow-fs-chart-is",
                         tags$div(
                           class = "ynow-fs-chart-heading",
                           icon("chart-line"),
                           tags$span(id = "ynow_fs_is_chart_heading", "Income Statement")
                         ),
                         plotlyOutput("is_plot")
                       ),
                       tags$hr(),
                       
                       DT::dataTableOutput("tbIncomeStatement"), 
                       downloadButton('IS_download', "Download Income Statement")
                     ),
                     
                     tabPanel("Balance Sheet",
                              p("This section imports Balance Sheets from Yahoo Finance"),
                              uiOutput("bs_equity_charts_ui"),
                              tags$hr(),
                              DT::dataTableOutput("tbBalanceSheet"),
                              downloadButton('BS_download', "Download Balance Sheet")
                     ),
                     
                     tabPanel(
                       "Cash Flow",
                       icon = icon("money-bill-wave"),
                       p("This section imports Cash Flow data from Yahoo Finance"),
                       tags$div(
                         class = "ynow-fs-chart ynow-fs-chart-cf",
                         tags$div(
                           class = "ynow-fs-chart-heading",
                           icon("money-bill-wave"),
                           tags$span(id = "ynow_fs_cf_chart_heading", "Cash Flow")
                         ),
                         plotlyOutput("cf_plot", height = "460px") %>% withSpinner()
                       ),
                       tags$hr(),
                       DT::dataTableOutput("tbCashFlow"),
                       downloadButton('CF_download', "Download Cash Flow Data")
                     ),

                     # 董監改選／可轉債稀釋（Search 代號勾稽；台股 MOPS／TPEx，美股 SEC 連結）
                     tabPanel(
                       title = "Board / CB",
                       value = "corp_events",
                       icon = icon("landmark"),
                       id = "corp_events_panel",
                       uiOutput("corp_events_ui")
                     ),

                     # 財報附註擷取 (SEC EDGAR) — 僅美股模式顯示（server 以 shinyjs 控制）
                     tabPanel(
                       title = ui_i18n("tab_sec_notes"),
                       value = "sec_notes",
                       icon = icon("file-alt"),
                       id = "sec_notes_panel",
                       p(
                         ui_i18n("sec_notes_lead"),
                         " ",
                         tags$b(ui_i18n("sec_notes_forms")),
                         tags$br(),
                         tags$span(
                           style = "color:#888;",
                           ui_i18n("sec_notes_source")
                         )
                       ),
                       tags$hr(),
                       fluidRow(
                         column(
                           width = 4,
                           box(
                             width = 12, status = "primary", solidHeader = TRUE,
                             title = ui_i18n("sec_query_title"),
                             tags$div(
                               style = "margin-bottom:10px;",
                               tags$label(ui_i18n("sec_ticker_label"), class = "control-label"),
                               uiOutput("lab_sec_ticker_display"),
                               tags$span(
                                 style = "color:#888; font-size:12px;",
                                 ui_i18n("sec_ticker_hint")
                               )
                             ),
                             radioButtons(
                               "lab_sec_form", ui_i18n("sec_form_label"),
                               choiceNames = list(
                                 ui_i18n("sec_form_10k"),
                                 ui_i18n("sec_form_10q"),
                                 ui_i18n("sec_form_8k")
                               ),
                               choiceValues = list("10-K", "10-Q", "8-K"),
                               selected = "10-K", inline = TRUE
                             ),
                             checkboxInput(
                               "lab_sec_important_only", ui_i18n("sec_important_only"), value = isTRUE(APP_DEFAULTS$lab_sec_important_only)
                             ),
                             textInput(
                               "lab_sec_keyword", ui_i18n("sec_keyword_label"),
                               value = "",
                               placeholder = ui_str("sec_keyword_ph", "en")
                             ) %>% tagAppendAttributes(`data-i18n-ph` = "sec_keyword_ph"),
                             tags$span(
                               style = "color:#888; font-size:12px; display:block; margin:-6px 0 10px 0;",
                               ui_i18n("sec_keyword_hint")
                             ),
                             actionButton(
                               "lab_sec_fetch", ui_i18n("sec_fetch_btn"),
                               class = "btn-success btn-block",
                               icon = icon("download"),
                               style = "font-weight:bold;"
                             ),
                             tags$p(
                               style = "margin-top:10px; color:#888; font-size:12px;",
                               ui_i18n("sec_fetch_hint")
                             )
                           ),
                           uiOutput("lab_sec_meta")
                         ),
                         column(
                           width = 8,
                           box(
                             width = 12, status = "info", solidHeader = TRUE,
                             title = ui_i18n("sec_index_title"),
                             uiOutput("lab_sec_index")
                           ),
                           box(
                             width = 12, status = "info", solidHeader = TRUE,
                             title = ui_i18n("sec_body_title"),
                             uiOutput("lab_sec_notes")
                           )
                         )
                       )
                     )
              )
              ),
              
              # Ownership Structure Analysis — Lite + Full (outside ynow-full-only)
              tabBox(
                title = "OWNERSHIP STRUCTURE",
                id = "dashboard_ownership",
                width = "auto",
                tabPanel(
                  title = ui_i18n("tab_ownership_structure"),
                  value = "ownership_structure",
                  icon = icon("sitemap"),
                  id = "ownership_structure_panel",
                  uiOutput("ownership_structure_top_ui"),
                  plotly::plotlyOutput("own_volume_plot", height = "320px"),
                  uiOutput("ownership_structure_holders_ui")
                )
              ),

              tags$div(
                class = "ynow-ind-overview-block",
                tags$div(
                  class = "ynow-ind-overview-title",
                  id = "ynow_ind_overview_title",
                  "目前產業標準快覽"
                ),
                tags$div(
                  class = "ynow-ind-overview-row",
                  tags$div(
                    class = "ynow-ind-overview-picker",
                    pickerInput(
                      inputId = "industry_choice",
                      label = NULL,
                      choices = industry_picker_choices(),
                      selected = APP_DEFAULTS$industry_choice,
                      # container=body: dropdown is portaled out of the picker cell so
                      # data-size menu height cannot create a huge in-flow white gap on mobile.
                      options = list(
                        `live-search` = TRUE,
                        size = 12,
                        container = "body"
                      )
                    )
                  ),
                  tags$div(
                    class = "ynow-ind-overview-summary",
                    uiOutput("dashboard_selected_industry")
                  )
                ),
                tags$div(
                  class = "ynow-ind-overview-metrics",
                  uiOutput("dashboard_industry_metric_chips")
                )
              ),
              .kpi_band_color_legend_ui(),
              
              tabBox(title = "PERFORMANCE",
                     width = "auto",
                     
                     tabPanel("KPI by Sheet", fluidRow(
                       column(width = 12,
                              tags$h4("Balance Sheet KPI", class = "ynow-kpi-section-title"),
                              div(class = "ynow-kpi-grid",
                                  uiOutput(NS("kpi", "vbx_eqt_multiplier"))
                              ),
                              tags$h4("Income Statement KPI", class = "ynow-kpi-section-title"),
                              div(class = "ynow-kpi-grid",
                                  uiOutput(NS("kpi", "vbx_net_profit_margin")),
                                  uiOutput(NS("kpi", "vbx_gross_profit_margin")),
                                  uiOutput(NS("kpi", "vbx_opex_ratio")),
                                  uiOutput(NS("kpi", "vbx_rev_growth")),
                                  uiOutput(NS("kpi", "vbx_gross_profit_growth"))
                              ),
                              tags$h4("Cash Flow KPI", class = "ynow-kpi-section-title"),
                              div(class = "ynow-kpi-grid",
                                  uiOutput(NS("kpi", "vbx_op_cash_flow_growth")),
                                  uiOutput(NS("kpi", "vbx_inv_cash_flow_growth")),
                                  uiOutput(NS("kpi", "vbx_fin_cash_flow_growth"))
                              )
                       )
                     )),
                     
                     tabPanel("Crossover KPIs", fluidRow(
                       column(width = 12,
                              div(class = "ynow-kpi-grid",
                                  uiOutput(NS("kpi", "vbx_ROA")),
                                  uiOutput(NS("kpi", "vbx_ROE")),
                                  uiOutput(NS("kpi", "vbx_asset_turnover")),
                                  uiOutput(NS("kpi", "vbx_ocf_net_income"))
                              )
                       )
                     )),
                     
                     tabPanel(
                       title = tags$span(id = "ynow_dash_annotation_tab", "Annotation"),
                       fluidRow(
                         column(
                           width = 12,
                           tags$p(
                             class = "ynow-ann-note",
                             ui_i18n("ann_kpi_purpose")
                           ),
                           tags$p(
                             style = "margin:-4px 0 12px 0; font-size:12px; color:#777; line-height:1.45;",
                             ui_i18n("ann_kpi_rules")
                           ),
                           tags$h4(ui_i18n("ann_kpi_calc_title"), class = "ynow-ann-h"),
                           DT::dataTableOutput("annotation_kpi_guide"),
                           tags$h4(ui_i18n("ann_kpi_stability_title"), class = "ynow-ann-h"),
                           tableOutput("annotation_stability_table"),
                           tags$p(
                             id = "ynow_ann_fscore_crossref",
                             style = "margin-top:10px; font-size:12px; color:#888;",
                             "提示：資產週轉率、OCF／淨利無同業區間時固定為白；",
                             "現金品質另可對照 YNOW／F-Score 品質檢核中的盈餘品質項。"
                           )
                         )
                       )
                     )
              ),
      ),

      # ==========================================
      # Business Breakdown（Full-only；tab id 仍為 company_advance）
      # ==========================================
      tabItem(
        tabName = "company_advance",
        uiOutput("ynow_lazy_host_company_advance")
      ),

      # ==========================================
      # Smart Analysis（Lite）：自動主／副模型＋預設參數試算與圖表
      # ==========================================
      tabItem(
        tabName = "smart_analysis",
        uiOutput("ynow_lazy_host_smart_analysis")
      ),
      
      # ==========================================
      # DDM：版面節奏對齊 DCF（模式列 → Overview → 參數 → 敏感度）
      # ==========================================
      tabItem(tabName = "ddm_calculator",
              tabBox(width = "auto",
                     tabPanel("",
                              fluidRow(
                                column(
                                  width = 6,
                                  radioButtons(
                                    "mod_ddm-ddm_mode",
                                    "選擇 DDM 估值模型：",
                                    choices = list(
                                      "Gordon 永續成長 (GGM)" = "gordon",
                                      "SPM 永續和 (Sum of Perpetuities)" = "spm",
                                      "二階段成長法 (Two-Stage Model)" = "two_stage"
                                    ),
                                    selected = APP_DEFAULTS$ddm_mode
                                  ),
                                  tags$p(
                                    id = "ynow_ddm_mode_help",
                                    class = "help-block",
                                    "Gordon (GGM)：V0 = D1/(r−g)。SPM：定額股利永續 + 保留盈餘成長（V0 = E·g/r² + D/r）。二階段：g1 後接 Gordon 終值 Pn。折現率 r = Ke（CAPM）。"
                                  )
                                ),
                                column(
                                  width = 6,
                                  numericInput(
                                    "mod_ddm-d0",
                                    "剛配股利 D0",
                                    value = APP_DEFAULTS$ddm_d0
                                  ),
                                  tags$p(
                                    id = "ynow_ddm_d0_help",
                                    class = "help-block",
                                    "由財報自動帶入：現金股利（現金流量表）÷ 股數；若無則顯示 0。可手動覆寫，或至 D0 以配息率／歷史平均覆寫。SPM 模式下此值為定額永續股利 D。"
                                  ),
                                  conditionalPanel(
                                    condition = "input['mod_ddm-ddm_mode'] == 'spm'",
                                    tags$div(
                                      id = "ynow_ddm_spm_tip",
                                      style = "margin-top: 8px; padding: 8px 10px; background: #FFF8E7; border-left: 3px solid #E0A800; border-radius: 4px; font-size: 13px;",
                                      tags$b("SPM："), " 請至下方 D0 分頁確認「預估／最新 EPS」；公式使用 E（EPS）與 D（上方股利）。"
                                    )
                                  )
                                )
                              )
                     )
              ),

              tabBox(title = "DIVIDEND DISCOUNT", width = "auto",
                     tabPanel(
                       "DDM Overview",
                       icon = icon("calculator"),
                       fluidRow(
                         column(
                           width = 12,
                           fluidRow(
                             class = "ynow-model-kpi-row",
                             column(6, class = "ynow-model-kpi-result ynow-model-kpi-outline",
                                    `data-ynow-lock` = "output:ddm_fair_value",
                                    infoBoxOutput("mod_ddm-ibx_ddm_price", width = NULL)),
                             column(6, class = "ynow-model-kpi-tone-2",
                                    infoBoxOutput("mod_ddm-ibx_ddm_d1", width = NULL))
                           )
                         )
                       ),
                       uiOutput("ddm_value_why"),
                       fluidRow(
                         column(
                           width = 12,
                           .ddm_formula_banner(),
                           uiOutput("mod_ddm-ui_ddm_result"),
                           h6(helpText(id = "ynow_ddm_overview_hint", ui_str("ddm_overview_hint", "en"))),
                           fluidRow(
                             column(
                               width = 6,
                               ynow_calc_btn("mod_ddm-btn_calc_ddm", "試算 DDM")
                             ),
                             column(
                               width = 6,
                               ynow_reset_defaults_btn("mod_ddm-reset_ddm")
                             )
                           ),
                           tags$div(style = "margin-top: 10px;", htmlOutput("mod_ddm-vtxt_ddm_setting_details"))
                         )
                       )
                     ),
                     tabPanel(
                       "DDM Calculation Details",
                       fluidRow(
                         infoBoxOutput("mod_ddm-ibx_d0_scraped", width = 4),
                         infoBoxOutput("mod_ddm-ibx_d0_eps", width = 4),
                         infoBoxOutput("mod_ddm-ibx_d0_payout", width = 4)
                       ),
                       fluidRow(
                         column(
                           width = 12,
                           div(
                             ui_i18n("ddm_details_smooth_note"),
                             style = "font-size: 15px; font-weight: bold; color: #2C3E50; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
                           )
                         )
                       )
                     )
              ),

              tabBox(width = "auto",
                     tabPanel(
                       "Overview",
                       fluidRow(
                         column(
                           width = 12,
                           numericInput(
                             "mod_ddm-g",
                             ui_i18n("ddm_g_label"),
                             value = APP_DEFAULTS$ddm_g
                           ),
                           checkboxInput(
                             "mod_ddm-sync_g",
                             ui_i18n("ddm_sync_g_label"),
                             value = isTRUE(APP_DEFAULTS$ddm_sync_central_g)
                           ),
                           tags$p(
                             id = "ynow_ddm_g_sync_help",
                             class = "help-block",
                             "勾選時跟隨中央 SGR；取消勾選後可單獨覆寫股利成長率（不必等於 FCFF 終值 g）。二階段時此值即 g₂；SPM 時為公式中的盈餘成長 g。"
                           ),
                           tags$div(style = "margin-top: 12px;", .ddm_two_stage_params_box()),
                           conditionalPanel(
                             condition = "input['mod_ddm-ddm_mode'] == 'spm'",
                             tags$div(
                               style = "margin-top: 12px;",
                               tags$p(
                                 id = "ynow_ddm_spm_g_note",
                                 class = "help-block",
                                 "SPM 另需 EPS（E）：請至 D0 分頁確認「預估／最新 EPS」。成長 g 常用 ROE×保留率或 ROA×保留率。"
                               )
                             )
                           )
                         )
                       )
                     ),
                     tabPanel(
                       "D0",
                       icon = icon("cogs"),
                       fluidRow(
                         column(
                           width = 12,
                           div(
                             id = "ynow_ddm_d0_banner",
                             "D0 = 現金股利（現金流量表）÷ 股數（或 EPS × 配息率）　｜　D1 = D0 × (1 + g)",
                             style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
                           )
                         ),
                         box(
                           h4(tags$b(ui_i18n("ddm_d0_m1_title"))),
                           p(helpText(ui_i18n("ddm_d0_m1_help"))),
                           div(
                             ui_i18n("ddm_d0_formula"),
                             style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
                           ),
                           numericInput("mod_ddm-est_eps", ui_i18n("ddm_est_eps_label"), value = NA, step = 0.01),
                           numericInput("mod_ddm-est_payout", ui_i18n("ddm_est_payout_label"), value = NA, min = 0, max = 100, step = 0.01),
                           actionButton("mod_ddm-calc_d0_payout", ui_i18n("btn_calc_d0_payout"), class = "btn-primary"),
                           tags$br(),
                           htmlOutput("mod_ddm-txt_d0_payout_res")
                         ),
                         box(
                           h4(tags$b(ui_i18n("ddm_d0_m2_title"))),
                           p(helpText(ui_i18n("ddm_d0_m2_help"))),
                           numericInput("mod_ddm-cycle_years", ui_i18n("ddm_cycle_years_label"), value = 5, min = 1, max = 10, step = 0.01),
                           actionButton("mod_ddm-calc_d0_average", ui_i18n("btn_calc_d0_avg"), class = "btn-primary"),
                           tags$br(),
                           htmlOutput("mod_ddm-txt_d0_avg_res")
                         )
                       )
                     ),
                     # Ke ↔ Beta 左右對調：Ke 在前（對齊 DCF 的 WACC → Beta 節奏）
                     tabPanel(
                       "Ke",
                       icon = icon("balance-scale"),
                       uiOutput("ddm_ke_tab_note"),
                       fluidRow(
                         infoBoxOutput("ibx_ddm_ke", width = 4),
                         infoBoxOutput("ibx_ddm_beta", width = 4),
                         infoBoxOutput("ibx_ddm_erp", width = 4)
                       ),
                       div(
                         id = "ynow_ddm_ke_formula_banner",
                         "Ke = Rf + β × (Rm − Rf)",
                         style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
                       ),
                       # 上列 100%：Ke 估算（對齊 WACC 估算盒）；下列左說明、右 CAPM
                       fluidRow(
                         box(
                           width = 12,
                           h4("Ke 估算", id = "ynow_ddm_ke_box_title"),
                           fluidRow(
                             column(
                               4,
                               numericInput(
                                 "mod_ddm-ke", ui_i18n("ddm_r_ke_label"),
                                 value = APP_DEFAULTS$ddm_ke, min = 0, step = 0.01
                               )
                             ),
                             column(
                               4,
                               tags$div(
                                 style = "margin-top: 25px;",
                                 htmlOutput("ddm_ke_erp_summary")
                               )
                             ),
                             column(
                               4,
                               tags$div(
                                 style = "margin-top: 25px;",
                                 htmlOutput("ddm_ke_source_chip")
                               )
                             )
                           ),
                           checkboxInput(
                             "ddm_use_estimated_re",
                             tags$span(
                               id = "ynow_ddm_use_estimated_ke_label",
                               "Use estimated Ke (from CAPM)"
                             ),
                             value = isTRUE(APP_DEFAULTS$use_est_re)
                           ),
                           tags$p(
                             id = "ynow_ddm_ke_help",
                             style = "margin:0 0 8px 0;color:#666;font-size:12px;",
                             paste0(
                               "Ke = Rf + β × (Rm − Rf). Bidirectionally synced with DCF→WACC \"Use estimated rₑ\" and the rₑ value; ",
                               "when checked, Ke follows CAPM; uncheck to override manually."
                             )
                           ),
                           actionButton("calc_ddm_ke", "計算 Ke（CAPM）", class = "btn-primary"),
                           tags$br(), htmlOutput("ddm_beta_ke_status")
                         )
                       ),
                       fluidRow(
                         ddm_ke_bridge_settings_ui(width = 6),
                         capm_beta_settings_ui(
                           title = "CAPM Estimate Ke",
                           calc_id = "calc_ddm_capm",
                           result_id = "ddm_capm_result",
                           width = 6,
                           id_prefix = "ddm_",
                           box_title_id = "ynow_ddm_capm_box_title",
                           sync_label_id = "ynow_ddm_sync_gs_beta_label",
                           rf_note_id = "ddm_capm_rf_source_note",
                           btn_label = "Estimate Ke (CAPM)"
                         )
                       )
                     ),
                     tabPanel(
                       "Beta (β)",
                       icon = icon("chart-line"),
                       .beta_model_source_section_ui(
                         "ddm_beta_u_apply_source",
                         "ddm_apply_beta_u_selected"
                       )
                     )
              ),

              tabBox(title = "SENSITIVITY", width = "auto",
                     fluidRow(
                       column(
                         width = 12,
                         h4(ui_i18n("sens_matrix_title")),
                         p(helpText(ui_i18n("ddm_sens_help")))
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         div(
                           style = "width: 100%; overflow-x: auto;",
                           tags$style(HTML("#ddm_sensitivity_table table { width: 100% !important; table-layout: fixed; }")),
                           tableOutput("ddm_sensitivity_table")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         uiOutput("ddm_sensitivity_analysis_panel")
                       )
                     )
              ),
              .model_param_sensitivity_box(
                ui_i18n("ddm_formula_contrib_title"),
                "mod_ddm-param_sensitivity_table"
              )
      ),
      
      # ==========================================
      # DCF Calculator 分頁
      # ==========================================
      tabItem(tabName = "dcf_calculator",
              # dcf_mode／dcf_claim 已上移至 shared header（預測年數 n 正上方）
              # WACC 改由 DCF → WACC 分頁／CAPM 同步；此處隱藏保留 input$id 供計算鏈使用
              tags$div(
                style = "display:none;",
                numericInput(
                  "wacc_gordon", ui_i18n("wacc_rate_label"),
                  value = APP_DEFAULTS$wacc_gordon, step = 0.01
                )
              ),
              
              tabBox(title = "DISCOUNTED CASH FLOW", width = "auto",
                     tabPanel("DCF Overview",
                              fluidRow(
                                column(width = 12,
                                       fluidRow(
                                         class = "ynow-model-kpi-row",
                                         column(6, class = "ynow-model-kpi-result ynow-model-kpi-outline",
                                                `data-ynow-lock` = "output:dcf_fair_value",
                                                infoBoxOutput("ibx_stock_value_dcf", width = NULL)),
                                         column(6, class = "ynow-model-kpi-tone-2",
                                                infoBoxOutput("ibx_enterprise_value_dcf", width = NULL))
                                       )
                                )
                              ),
                              # Core explainability: Why this Fair Value (not appendix)
                              uiOutput("dcf_value_why"),
                              fluidRow(
                                column(width = 12,
                                       plotOutput("plt_dcf_trajectory", height = "420px"),
                                       h6(uiOutput("dcf_chart_help")),
                                       fluidRow(
                                         column(width = 6, ynow_calc_btn("calc", "試算 DCF")),
                                         column(width = 6, ynow_reset_defaults_btn("reset_dcf"))
                                       ),
                                       tags$div(style = "margin-top: 10px;", htmlOutput("vtxt_dcf_setting_details"))
                                )
                              )
                     ),
                     
                     tabPanel("DCF Calculation Details",
                              fluidRow(
                                column(width = 12,
                                       plotOutput("mod_fcf-fcf_plot", height = "350px")
                                ),
                                uiOutput("ui_data_validation") 
                              )
                     )
              ),
              
              tabBox(width = "auto",
                     tabPanel("Overview",
                              fluidRow(
                                infoBoxOutput("ibx_estimated_g", width = 6),
                                infoBoxOutput("ibx_sgr", width = 6),
                                
                                column(width = 12,
                                       plotOutput("plt_fcf_trend", height = "350px")
                                ),
                                column(
                                  width = 12,
                                  tags$div(style = "margin-top: 12px;", .dcf_two_stage_params_box())
                                )
                              )
                     ),
                     
                     # 在 ui.R 的某個 tabBox 或 navbarMenu 中：
                     fcf_projection_module_ui(id = "mod_fcf"),
                     
                     tabPanel(
                       "WACC",
                       icon = icon("balance-scale"),
                       tags$div(
                         `data-ynow-lock` = "section:wacc_box",
                         uiOutput("dcf_wacc_fcfe_note"),
                         fluidRow(
                           valueBoxOutput("vbx_equity_val", width = 4), # 股權市值 (E)
                           valueBoxOutput("vbx_debt_val", width = 4),   # 總負債 (D)
                           valueBoxOutput("vbx_tax_rate", width = 4)    # 有效稅率 (T)
                         ),
                         fluidRow(
                           tags$div(`data-ynow-lock` = "output:wacc", infoBoxOutput("ibx_wacc", width = 4)),
                           infoBoxOutput("ibx_rd", width = 4),
                           infoBoxOutput("ibx_re", width = 4)
                         ),
                         uiOutput("dcf_disc_formula_banner"),
                         # 上列 100%：WACC 估算；下列左 50% rᵈ、右 50% CAPM
                         fluidRow(
                           box(
                             width = 12,
                             h4("WACC 估算", id = "ynow_wacc_box_title"),
                             fluidRow(
                               column(4, numericInput(
                                 "wacc_re", ui_i18n("wacc_re_label"),
                                 value = APP_DEFAULTS$wacc_re, min = 0, step = 0.01
                               )),
                               column(4, numericInput(
                                 "wacc_rd", ui_i18n("wacc_rd_label"),
                                 value = APP_DEFAULTS$wacc_rd, min = 0, step = 0.01
                               )),
                               column(4, numericInput(
                                 "wacc_tax", ui_i18n("wacc_tax_label"),
                                 value = APP_DEFAULTS$wacc_tax, min = 0, max = 100, step = 0.01
                               ))
                             ),
                             checkboxInput(
                               "use_estimated_re",
                               tags$span(id = "ynow_use_estimated_re_label", "Use estimated rₑ (from CAPM)"),
                               value = APP_DEFAULTS$use_est_re
                             ),
                             uiOutput("wacc_tax_source_note"),
                             tags$p(
                               id = "ynow_wacc_help",
                               style = "margin:0 0 8px 0;color:#666;font-size:12px;",
                               "WACC = We×rₑ + Wd×rᵈ×(1−T)。rᵈ 可由下方「估算 rᵈ」以利息費用／有息負債推估（稅前），再於此套用稅盾。"
                             ),
                             actionButton("calc_wacc", "計算 WACC", class = "btn-primary"),
                             tags$br(), htmlOutput("wacc_result")
                           )
                         ),
                         fluidRow(
                           rd_estimate_settings_ui(width = 6),
                           capm_beta_settings_ui(
                             title = "CAPM Estimate rₑ",
                             calc_id = "calc_capm",
                             result_id = "capm_result",
                             width = 6
                           )
                         )
                       )
                     ),

                     tabPanel(
                       "Beta (β)",
                       icon = icon("chart-line"),
                       .beta_model_source_section_ui(
                         "dcf_beta_u_apply_source",
                         "dcf_apply_beta_u_selected"
                       )
                     )
              ),
              tabBox(title = "SENSITIVITY", width = "auto",
                     fluidRow(
                       column(
                         width = 12,
                         h4(ui_i18n("sens_matrix_title")),
                         uiOutput("dcf_sens_help")
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         div(
                           style = "width: 100%; overflow-x: auto;",
                           tags$style(HTML("#dcf_sensitivity_table table { width: 100% !important; table-layout: fixed; }")),
                           tableOutput("dcf_sensitivity_table")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         uiOutput("sensitivity_analysis_panel")
                       )
                     )
              ),
              .model_param_sensitivity_box(
                ui_i18n("dcf_formula_contrib_title"),
                "dcf_param_sensitivity_table"
              ),
              uiOutput("txt_dcf_investment_checks")
      ),
      
      # 🌟 呼叫 RI 模型分頁介面
      ri_module_ui("mod_ri"),
      
      # 🌟 呼叫 P/B 相對估值分頁介面
      pb_asset_module_ui("mod_pb"),

      # Relative multiples: enterprise value, earnings, revenue (book value is P/B above)
      relative_multiples_module_ui("mod_rel"),

      # SOTP — structural Sum-of-the-Parts (separate sidebar)
      sotp_module_ui("mod_sotp"),

      # 🌟 呼叫純 NAV 分頁介面
      nav_module_ui("mod_nav"),
      
      tabItem(
        tabName = "sensitivity",
        uiOutput("ynow_lazy_host_sensitivity")
      ),

      # Blue Chip：美股績優篩選（版面節奏對齊 Dashboard FINANCIAL REPORT）
      tabItem(
        tabName = "bluechip",
        uiOutput("ynow_lazy_host_bluechip")
      ),
      
      # ==========================================
      # 歷史基本面驗證（HFV）：報告式版面 — 理論估值 vs 實際市值（非策略回測）
      # ==========================================
      tabItem(
        tabName = "hfv",
        uiOutput("ynow_lazy_host_hfv")
      ),

      # ==========================================
      # 決策檢核（投資決策通過檢核表）
      # ==========================================
      decision_checklist_tab_ui(),
      mos_assumptions_tab_ui(),
      price_alert_tab_ui(),
      if (exists("ynow_admin_tab_ui", mode = "function")) ynow_admin_tab_ui() else NULL,

      # ==========================================
      # Quant Backtest Lab（報告式版面；章節內容後接預設收合之控制區）
      # ==========================================
      tabItem(
        tabName = "lab_notes",
        {
          lab_gate <- if (exists("ynow_member_gate_ui", mode = "function")) {
            ynow_member_gate_ui(
              "ynow_lab_guest_gate",
              "ynow_lab_gate_title",
              "ynow_lab_gate_body",
              "ynow_lab_gate_signin",
              feature = "lab"
            )
          }
          lab_body <- tags$div(
            id = "ynow_lab_body",
            class = "ynow-member-body",
            `data-ynow-lock` = "section:backtest_body",
            uiOutput("ynow_lazy_host_lab_notes")
          )
          if (exists("ynow_member_gate_surface_ui", mode = "function")) {
            ynow_member_gate_surface_ui(feature = "lab", gate = lab_gate, body = lab_body)
          } else {
            tagList(lab_gate, lab_body)
          }
        }
      ),

      # ==========================================
      # Asset Transmission Root（Full guest lock；Lite 側欄省略）
      # ==========================================
      tabItem(
        tabName = "asset_transmission",
        {
          atx_gate <- if (exists("ynow_member_gate_ui", mode = "function")) {
            ynow_member_gate_ui(
              "ynow_atx_guest_gate",
              "ynow_atx_gate_title",
              "ynow_atx_gate_body",
              "ynow_atx_gate_signin",
              feature = "atx"
            )
          }
          atx_body <- tags$div(
            id = "ynow_atx_body",
            class = "ynow-member-body",
            uiOutput("ynow_lazy_host_asset_transmission")
          )
          if (exists("ynow_member_gate_surface_ui", mode = "function")) {
            ynow_member_gate_surface_ui(feature = "atx", gate = atx_gate, body = atx_body)
          } else {
            tagList(atx_gate, atx_body)
          }
        }
      ),

      # ==========================================
      # System admin portal（完整版側邊欄入口；內部 tabName = testing）
      # ==========================================
      tabItem(
        tabName = "testing",
        uiOutput("ynow_lazy_host_testing")
      ),

      # ==========================================
      # 💬 意見區：使用者回饋 → GitHub Issues
      # ==========================================
      tabItem(
        tabName = "feedback",
        uiOutput("ynow_lazy_host_feedback")
      ),

      # ==========================================
      # ℹ️ About 分頁 (系統介紹／Lite 簡介；完整評價方法論在 Basic Setup 最下方)
      # ==========================================
      tabItem(
        tabName = "about",
        uiOutput("ynow_lazy_host_about")
      )
    )
  )
)

# Fail-closed Full guest locks on first HTML paint (before Shiny auth websocket).
# See ynow_stamp_auth_feature_body() / lock-first-paint grounding.
if (exists("ynow_stamp_auth_feature_body", mode = "function")) {
  ui <- ynow_stamp_auth_feature_body(ui)
}
