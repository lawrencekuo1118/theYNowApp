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
  "v28.47"
}
.YNOW_BUILD_VERSION <- .ynow_read_build_version()

#' Major segment of a display version (`v28.47` → 25; `v26` → 26).
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
          tags$p(id = "ynow_home_bluechip_k", class = "ynow-home-card-k", "Stock Prospecting"),
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
        # Divider is the top border. Disclaimer sits under it, then the link row.
        tags$p(
          id = "ynow_home_legal",
          class = "ynow-home-legal-text ynow-home-disclaimer",
          paste0(
            "Research and education only — not investment advice. ",
            "You alone bear all investment decisions and losses."
          )
        ),
        tags$div(
          class = "ynow-home-legal-links",
          if (exists("ynow_auth_home_ui", mode = "function")) ynow_auth_home_ui(),
          tags$span(class = "ynow-home-legal-dot", `aria-hidden` = "true", "·"),
          tags$button(
            type = "button",
            id = "ynow_home_legal_about",
            class = "ynow-home-legal-link",
            `data-tab` = "about",
            "About · Privacy"
          )
        ),
        if (exists("ynow_inspiration_home_shell_ui", mode = "function")) {
          ynow_inspiration_home_shell_ui()
        }
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

#' Focus-KPI gold-frame legend note (above FINANCIAL REPORT; right-aligned).
.ynow_focus_metric_legend_note_ui <- function() {
  tags$p(
    class = "ynow-focus-metric-note ynow-focus-metric-note--above-fs",
    tags$span(
      class = "ynow-focus-metric-swatch ynow-focus-metric-swatch--legend",
      `aria-hidden` = "true"
    ),
    tags$span(id = "ynow_kpi_legend_focus", "藤蔓 · 本財報屬性關鍵指標")
  )
}

#' KPI 色碼圖例：藍→紅→黑→白（金色屬性重視註解改置財務報表區上方）
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
    )
  )
}

# Shared gray「回復預設」／試算 helpers live in setup.R
# (ynow_reset_defaults_btn, ynow_calc_btn, ynow_calc_reset_row).

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

#' Decision Matrix tables for Model Selection Guide (tight bilingual cells).
.model_selection_guide_matrix_body <- function() {
  .method_table_fv_en <- HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #2C3E50; color: white;'>
                                   <tr>
                                     <th>Item</th>
                                     <th>DDM</th>
                                     <th>DCF</th>
                                     <th>RI</th>
                                     <th>P/B</th>
                                     <th>NAV</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>Anchor</b></td>
                                     <td>DPS (equity)</td>
                                     <td>FCFF / FCFE</td>
                                     <td>Book + RI</td>
                                     <td>BVPS / TBVPS / NAVPS × P/B</td>
                                     <td>NAVPS × multiple</td>
                                   </tr>
                                   <tr>
                                     <td><b>Rate / multiple</b></td>
                                     <td>Ke</td>
                                     <td>WACC / Ke</td>
                                     <td>Ke</td>
                                     <td>Justified / industry / history</td>
                                     <td>Disc. / prem. multiple</td>
                                   </tr>
                                   <tr>
                                     <td><b>g / years</b></td>
                                     <td>Perpetual g; SPM g; 2-stage g₁/n₁</td>
                                     <td>Forecast n; terminal SGR</td>
                                     <td>Horizon n; terminal g</td>
                                     <td>Justified needs g</td>
                                     <td>—</td>
                                   </tr>
                                   <tr>
                                     <td><b>Formula</b></td>
                                     <td>Gordon / SPM / 2-stage ΣPV(D)+PV(TV)</td>
                                     <td>ΣPV(FCFF/FCFE)+PV(TV)</td>
                                     <td>B₀+ΣPV(RI)+PV(TV_RI)</td>
                                     <td>(BVPS / TBVPS / NAVPS)×P/B</td>
                                     <td>NAVPS×multiple</td>
                                   </tr>
                                   <tr>
                                     <td><b>Role</b></td>
                                     <td>Fair Value</td>
                                     <td>Fair Value (EV bridge)</td>
                                     <td>Fair Value</td>
                                     <td>Bear / Base / Bull</td>
                                     <td>Fair Value (HoldCo)</td>
                                   </tr>
                                 </tbody>
                               </table>")
  .method_table_fv_zh <- HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #2C3E50; color: white;'>
                                   <tr>
                                     <th>項目</th>
                                     <th>DDM</th>
                                     <th>DCF</th>
                                     <th>RI</th>
                                     <th>P/B</th>
                                     <th>NAV</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>錨定</b></td>
                                     <td>DPS（股權）</td>
                                     <td>FCFF／FCFE</td>
                                     <td>帳面＋RI</td>
                                     <td>BVPS／TBVPS／NAVPS × P/B</td>
                                     <td>NAVPS × 倍數</td>
                                   </tr>
                                   <tr>
                                     <td><b>折現／倍數</b></td>
                                     <td>Ke</td>
                                     <td>WACC／Ke</td>
                                     <td>Ke</td>
                                     <td>Justified／產業／歷史</td>
                                     <td>折價／溢價倍數</td>
                                   </tr>
                                   <tr>
                                     <td><b>g／年數</b></td>
                                     <td>永續 g；SPM g；二階段 g₁／n₁</td>
                                     <td>預測 n；終值 SGR</td>
                                     <td>預測 n；終值 g</td>
                                     <td>Justified 需 g</td>
                                     <td>—</td>
                                   </tr>
                                   <tr>
                                     <td><b>公式</b></td>
                                     <td>Gordon／SPM／二階段 ΣPV(D)+PV(TV)</td>
                                     <td>ΣPV(FCFF／FCFE)+PV(TV)</td>
                                     <td>B₀+ΣPV(RI)+PV(TV_RI)</td>
                                     <td>(BVPS／TBVPS／NAVPS)×P/B</td>
                                     <td>NAVPS×倍數</td>
                                   </tr>
                                   <tr>
                                     <td><b>角色</b></td>
                                     <td>Fair Value</td>
                                     <td>Fair Value（EV 橋接）</td>
                                     <td>Fair Value</td>
                                     <td>Bear／Base／Bull</td>
                                     <td>Fair Value（控股）</td>
                                   </tr>
                                 </tbody>
                               </table>")
  .method_table_rel_en <- HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #34495e; color: white;'>
                                   <tr>
                                     <th>Item</th>
                                     <th>Multiples</th>
                                     <th>SOTP</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>Anchor</b></td>
                                     <td>EPS / EBIT / EBITDA / FCFF / Rev / ARR × multiple</td>
                                     <td>≥2 seg. rev × EV/Sales (+ non-op)</td>
                                   </tr>
                                   <tr>
                                     <td><b>Bridge</b></td>
                                     <td>EV→Equity＝EV+Cash−Debt; P/S equity-side</td>
                                     <td>EV→Equity＝EV+Cash−Debt</td>
                                   </tr>
                                   <tr>
                                     <td><b>g / years</b></td>
                                     <td>No n; PEG uses growth %</td>
                                     <td>—</td>
                                   </tr>
                                   <tr>
                                     <td><b>Formula</b></td>
                                     <td>Price＝EPS×P/E; or Equity＝(Metric×Mult+Cash−Debt)÷Shares</td>
                                     <td>EV＝Σ(Seg Rev×EV/Sales)+Non-op</td>
                                   </tr>
                                   <tr>
                                     <td><b>Role</b></td>
                                     <td>Market Reference (not Fair Value)</td>
                                     <td>Meta-Framework (often + NAV)</td>
                                   </tr>
                                 </tbody>
                               </table>")
  .method_table_rel_zh <- HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #34495e; color: white;'>
                                   <tr>
                                     <th>項目</th>
                                     <th>Multiples</th>
                                     <th>SOTP</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>錨定</b></td>
                                     <td>EPS／EBIT／EBITDA／FCFF／營收／ARR × 倍數</td>
                                     <td>≥2 部門營收 × EV/Sales（＋非營業）</td>
                                   </tr>
                                   <tr>
                                     <td><b>橋接</b></td>
                                     <td>EV→Equity＝EV+Cash−Debt；P/S 股權面</td>
                                     <td>EV→Equity＝EV+Cash−Debt</td>
                                   </tr>
                                   <tr>
                                     <td><b>g／年數</b></td>
                                     <td>無 n；PEG 用成長％</td>
                                     <td>—</td>
                                   </tr>
                                   <tr>
                                     <td><b>公式</b></td>
                                     <td>Price＝EPS×P/E；或 Equity＝(Metric×Mult+Cash−Debt)÷Shares</td>
                                     <td>EV＝Σ(Seg Rev×EV/Sales)+Non-op</td>
                                   </tr>
                                   <tr>
                                     <td><b>角色</b></td>
                                     <td>市場參照（非 Fair Value）</td>
                                     <td>Meta-Framework（常配 NAV）</td>
                                   </tr>
                                 </tbody>
                               </table>")
  tagList(
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
      "Multiples = market Reference; SOTP = Meta-Framework — not Fair Value primaries."
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
  )
}

#' Collapsible Model Selection Guide (Decision Matrix only; default closed).
#' Mounted under the 基本面評價模型 position chart — not with formula sibling tabs.
.model_selection_guide_panel_ui <- function() {
  tags$details(
    id = "ynow_ms_guide_details",
    class = "ynow-notes ynow-ms-guide",
    tags$summary(
      class = "ynow-notes__summary",
      id = "ynow_ms_guide_summary",
      title = "Show or hide Model Selection Guide",
      `aria-label` = "Show or hide Model Selection Guide",
      tags$span(
        class = "ynow-ms-guide__title",
        id = "ynow_ms_guide_title",
        "Model Selection Guide"
      )
    ),
    tags$div(
      class = "ynow-notes__body",
      .model_selection_guide_matrix_body()
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

#' Valuation methodology guide (model formula tabs).
#' Mounted at the bottom of Basic Setup (Full only; Lite hides Basic Setup).
#' Section title uses column(12); tabBox keeps width=12 (shinydashboard always emits col-sm-N).
#' Locale: `.ynow-loc-en` / `.ynow-loc-zh` toggle via `html[lang]` (see CSS + applyUiLocale).
.valuation_methodology_section_ui <- function(collapsible = TRUE, collapsed = FALSE) {
  # collapsible/collapsed kept for call-site compatibility (no outer box to collapse)
  # Decision Matrix (Model Selection Guide) lives under the Fundamental Valuation
  # Models chart as a collapsed panel — see .model_selection_guide_panel_ui().
  # This section keeps formula sibling tabs only.
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
        )
      )
    ),
    fluidRow(
      tabBox(
        title = tags$span(id = "ynow_method_tabbox_title", "Model Formulas"),
        width = 12,
        side = "left",

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
      tags$span(id = "ynow_shared_bridge_title", "模型共用參數")
    ),
    width = 12, status = "primary", solidHeader = TRUE,
    tags$p(
      id = "ynow_shared_bridge_help",
      class = "help-block",
      paste0(
        "Cash、Total Debt、股數、總營收與其他權益請求權請只在這裡填一次。",
        "需要這些數字的模型——DCF、WACC 權重、Enterprise Multiples、P/S、SOTP——都會讀這裡。",
        "終值 g 與 Ke 仍在 SGR 與 CAPM 調整。",
        "SOTP 部門營收、股利 g、近端營收成長各自設定。",
        "USD/TWD 匯率由系統自動擷取。"
      )
    ),
    fluidRow(
      column(3, numericInput("bridge_cash", "Cash", value = NA, step = 1)),
      column(3, numericInput("bridge_debt", "Total Debt", value = NA, step = 1)),
      column(3, numericInput("bridge_shares", "股數", value = NA, step = 1)),
      column(3, numericInput("bridge_revenue", "總營收", value = NA, step = 1)),
      column(12, numericInput("bridge_other_claims", ui_i18n("param_other_claims_label"), value = NA, step = 1))
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
        # SGR | near-term g: always 1:1 side-by-side (desktop + mobile).
        # col-xs-6 so BS3 does not stack below 768px; lock on the column host;
        # bare uiOutput + width=NULL small-box (same as Summary β host pattern).
        # CSS .ynow-sgr-kpi-row enforces flex 1:1 equal height and is excluded
        # from the mobile “KPI → full-width stack” rule.
        fluidRow(
          class = "ynow-sgr-kpi-row",
          tags$div(
            class = "col-xs-6",
            `data-ynow-lock` = "output:sgr_pct",
            shiny::uiOutput("vbx_sgr_pct")
          ),
          tags$div(
            class = "col-xs-6",
            shiny::uiOutput("vbx_session_g")
          )
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
  # Collapsed by default; user expands when reviewing |ε| / contribution.
  fluidRow(
    box(
      title = tagList(icon("percentage"), title),
      width = 12, status = "info", solidHeader = TRUE,
      collapsible = TRUE, collapsed = TRUE,
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
                    "開啟時先顯示預設快照；閒置後才背景重算。按「更新排名」可立即手動重算並寫入預設快照。"
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
                    paste0(
                      "面積＝產業鏈總市值；顏色＝市值加權年化估值漲幅（美股漲綠／台股漲紅）。",
                      "字級依｜漲幅｜絕對值放大（與區塊面積無關，文字可溢出顯示）。納入全部產業鏈。"
                    )
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
            title = "STOCK PROSPECTING",
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
                        "產業市值加權漲幅（全部產業鏈）在上方獨立區塊（預設快照優先；閒置後背景重算，或按「更新排名」）。",
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
                    tags$div(
                      class = "ynow-lab-im-query-title-right",
                      tags$div(
                        class = "ynow-lab-im-boards ynow-tw-only",
                        title = paste0(
                          "複選上市／上櫃／興櫃；預設上市＋上櫃。",
                          "興櫃資料覆蓋較不穩，勾選後才納入績優評估池。"
                        ),
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
                          class = "ynow-lab-im-quality-hint ynow-lab-im-header-hint",
                          "複選上市／上櫃／興櫃；預設上市＋上櫃。興櫃資料覆蓋較不穩，勾選後才納入績優評估池。"
                        )
                      ),
                      tags$div(
                        class = "ynow-lab-im-us-listed ynow-us-only",
                        title = "美股績優池僅含主要上市（Nasdaq／NYSE／NYSE American）。",
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
                          class = "ynow-lab-im-quality-hint ynow-lab-im-header-hint",
                          "美股績優池僅含主要上市（Nasdaq／NYSE／NYSE American）。"
                        )
                      ),
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
                    )
                  ),
                  tags$div(
                    class = "ynow-lab-im-universe-row",
                    uiOutput("lab_im_universe_meta")
                  ),
                  # Responsive QueryConditions module (visual). Hidden bridge inputs
                  # keep the existing lab_im_* engine / lock / picker observers intact.
                  if (exists("ynq_query_ui", mode = "function") &&
                      exists("ynq_lab_catalog", mode = "function")) {
                    ynq_query_ui(
                      "lab_ynq",
                      catalog = ynq_lab_catalog(APP_DEFAULTS$lab_im_methods, locale = .bc_loc),
                      embed = TRUE,
                      locale = .bc_loc
                    )
                  } else {
                    tags$p(id = "ynow_ynq_missing", "Query conditions module unavailable.")
                  },
                  # Bridge inputs (engine source of truth). Visually hidden; synced from lab_ynq.
                  tags$div(
                    id = "ynow_lab_im_ynq_bridge",
                    class = "ynow-lab-im-ynq-bridge",
                    style = "position:absolute;width:1px;height:1px;overflow:hidden;clip:rect(0,0,0,0);",
                    `aria-hidden` = "true",
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
                    tags$span(id = "ynow_lab_im_chains_hint", class = "ynow-lab-im-quality-hint", " "),
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
                    tags$span(id = "ynow_lab_im_industries_hint", class = "ynow-lab-im-quality-hint", " "),
                    tags$div(
                      class = "ynow-lab-im-methods",
                      # Absolute vs Relative taxonomy retained for lock JS / tests.
                      .lab_im_methods_filter_ui(APP_DEFAULTS$lab_im_methods)
                    ),
                    checkboxInput(
                      "lab_im_eq_only",
                      tags$span("盈餘品質"),
                      value = isTRUE(APP_DEFAULTS$lab_im_eq_only)
                    ),
                    checkboxInput(
                      "lab_im_gate_only",
                      tags$span("Piotroski 高門檻"),
                      value = isTRUE(APP_DEFAULTS$lab_im_gate_only)
                    ),
                    checkboxInput(
                      "lab_im_no_alert",
                      tags$span("無財報警訊"),
                      value = isTRUE(APP_DEFAULTS$lab_im_no_alert)
                    ),
                    checkboxInput(
                      "lab_im_include_adr",
                      tags$span("含 ADR"),
                      value = isTRUE(APP_DEFAULTS$lab_im_include_adr)
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
              tags$p(
                id = "ynow_lab_im_detail_virt_hint",
                class = "ynow-lab-im-detail-virt-hint",
                style = "margin:0 0 10px 0; color:#888; font-size:12px;",
                "明細列數較多時（例如 N＝500／全部）每次只顯示 25 筆，畫面外的列不會留在行動裝置 DOM 中。"
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  title = tags$span(
                    id = "ynow_lab_im_detail_box_title",
                    "明細（按年化估值漲幅排序）"
                  ),
                  DT::dataTableOutput("lab_im_table") %>% shinycssloaders::withSpinner()
                )
              )
            ),

            # Clustering + Custom Screen: nested lazy hosts (mount on subtab select).
            tabPanel(
              title = "分群",
              value = "im_cluster",
              icon = icon("project-diagram"),
              tags$div(
                class = "ynow-bc-subtab-shell ynow-bc-cluster-shell",
                tags$p(
                  id = "ynow_bc_cluster_shell_hint",
                  class = "ynow-bc-subtab-shell-hint text-muted",
                  style = "color:#888; font-size:12.5px; margin:8px 0;",
                  "Loading Clustering…"
                ),
                uiOutput("ynow_lazy_host_bc_cluster")
              )
            ),

            tabPanel(
              title = "自訂選股",
              value = "im_screener",
              icon = icon("filter"),
              tags$div(
                class = "ynow-bc-subtab-shell ynow-bc-screener-shell",
                tags$p(
                  id = "ynow_bc_screener_shell_hint",
                  class = "ynow-bc-subtab-shell-hint text-muted",
                  style = "color:#888; font-size:12.5px; margin:8px 0;",
                  "Loading Custom Screen…"
                ),
                uiOutput("ynow_lazy_host_bc_screener")
              )
            )
          )
  )
}

#' Nested lazy body for Stock Prospecting → Clustering (once per session).
.ynow_page_ui_bluechip_cluster <- function() {
  tagList(
    # Light-blue Research Clustering Lab intro: blurb + disclaimer | Radar focus
    tags$div(
      class = "ynow-lab-cluster-intro",
      tags$p(
        id = "ynow_lab_cluster_blurb",
        paste0(
          "研究用分群 Lab：僅以比率／成長率做 K-Means（不把金額放入模型），",
          "降低公司規模對距離的干擾。語意標籤為描述性啟發式，非買進／賣出訊號。"
        )
      ),
      tags$div(
        class = "ynow-lab-cluster-intro-footer",
        tags$p(
          id = "ynow_lab_cluster_disclaimer",
          "僅供研究／教育，非投資建議，亦非買進訊號。"
        ),
        tags$div(
          class = "ynow-lab-cluster-focus-run",
          tags$div(
            class = "ynow-sc-wrap ynow-ticker-typeahead ynow-lab-cluster-focus-wrap",
            textInput(
              "lab_cluster_focus",
              tags$span(id = "ynow_lab_cluster_focus_label", "Radar focus ticker"),
              value = "",
              width = "100%",
              placeholder = "e.g. AAPL / 2330"
            ),
            uiOutput("lab_cluster_focus_suggest_ui")
          ),
          tags$div(
            class = "ynow-lab-cluster-run-wrap",
            actionButton(
              "lab_cluster_run",
              tags$span(id = "ynow_lab_cluster_run_label", "執行分群"),
              icon = icon("object-ungroup"),
              class = "btn-success"
            )
          )
        )
      )
    ),
    tags$hr(class = "ynow-lab-cluster-controls-hr"),
    fluidRow(
      class = "ynow-lab-cluster-controls",
      column(
        width = 3,
        selectInput(
          "lab_cluster_mode",
          tags$span(id = "ynow_lab_cluster_mode_label", "分群模式"),
          choices = if (exists("lab_cluster_mode_choices", mode = "function")) {
            lab_cluster_mode_choices("zh-TW")
          } else {
            c(
              "K-Means（比率／成長率）" = "kmeans",
              "依 Beta（槓桿股權 β_L）" = "beta"
            )
          },
          selected = {
            m0 <- as.character(APP_DEFAULTS$lab_cluster_mode %||% "kmeans")[1]
            if (m0 %in% c("kmeans", "beta")) m0 else "kmeans"
          },
          width = "100%"
        )
      ),
      column(
        width = 2,
        conditionalPanel(
          condition = "input.lab_cluster_mode != 'beta'",
          numericInput(
            "lab_cluster_k",
            tags$span(id = "ynow_lab_cluster_k_label", "群數（k）"),
            value = APP_DEFAULTS$lab_cluster_k, min = 2, max = 8, step = 1, width = "100%"
          )
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.lab_cluster_mode != 'beta'",
          selectInput(
            "lab_cluster_x",
            tags$span(id = "ynow_lab_cluster_x_label", "散點 X"),
            choices = if (exists("lab_cluster_axis_choices", mode = "function")) {
              lab_cluster_axis_choices("zh-TW")
            } else {
              c(
                "ROE" = "ROE",
                "Operating Margin" = "Operating_Margin",
                "Rev YoY" = "Rev_YoY",
                "OpInc YoY" = "OpInc_YoY",
                "Debt Ratio" = "Debt_Ratio",
                "Trailing P/E" = "PE_Ratio",
                "P/B" = "PB_Ratio",
                "Beta（槓桿 β_L）" = "Beta"
              )
            },
            selected = APP_DEFAULTS$lab_cluster_x,
            width = "100%"
          )
        ),
        conditionalPanel(
          condition = "input.lab_cluster_mode == 'beta'",
          selectInput(
            "lab_cluster_beta_membership",
            tags$span(id = "ynow_lab_cluster_beta_membership_label", "群組成員"),
            choices = if (exists("lab_cluster_beta_membership_choices", mode = "function")) {
              lab_cluster_beta_membership_choices("zh-TW")
            } else {
              c(
                "固定群組（截止月）" = "fixed",
                "逐期重分（當期分位）" = "dynamic"
              )
            },
            selected = {
              m0 <- as.character(APP_DEFAULTS$lab_cluster_beta_membership %||% "fixed")[1]
              if (m0 %in% c("fixed", "dynamic")) m0 else "fixed"
            },
            width = "100%"
          )
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.lab_cluster_mode != 'beta'",
          selectInput(
            "lab_cluster_y",
            tags$span(id = "ynow_lab_cluster_y_label", "散點 Y"),
            choices = if (exists("lab_cluster_axis_choices", mode = "function")) {
              lab_cluster_axis_choices("zh-TW")
            } else {
              c(
                "ROE" = "ROE",
                "Operating Margin" = "Operating_Margin",
                "Rev YoY" = "Rev_YoY",
                "OpInc YoY" = "OpInc_YoY",
                "Debt Ratio" = "Debt_Ratio",
                "Trailing P/E" = "PE_Ratio",
                "P/B" = "PB_Ratio",
                "Beta（槓桿 β_L）" = "Beta"
              )
            },
            selected = APP_DEFAULTS$lab_cluster_y,
            width = "100%"
          )
        ),
        conditionalPanel(
          condition = "input.lab_cluster_mode == 'beta'",
          checkboxInput(
            "lab_cluster_beta_show_iqr",
            tags$span(id = "ynow_lab_cluster_beta_show_iqr_label", "顯示 Q25–Q75 帶狀"),
            value = isTRUE(APP_DEFAULTS$lab_cluster_beta_show_iqr)
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        conditionalPanel(
          condition = "input.lab_cluster_mode == 'beta'",
          tags$p(
            id = "ynow_lab_cluster_beta_bins_help",
            style = "color:#666; font-size:12px; margin-top:-4px;",
            paste0(
              "Beta 模式（level）：K=3 等人數分位（低／中／高），非絕對切點。",
              "優先以 24 個月月報酬 OLS 估計槓桿 β_L；若月報酬不足則退回 Yahoo Summary「Beta（五年月資料）」。",
              "固定群組＝截止月成員回溯；逐期重分＝每月重排分位。非去槓桿 βᵤ，亦非可交易回測。"
            )
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
  )
}

#' Nested lazy body for Stock Prospecting → Custom Screen (once per session).
.ynow_page_ui_bluechip_screener <- function() {
  if (exists("screener_panel_ui", mode = "function")) {
    screener_panel_ui(embed = "bluechip")
  } else {
    tags$p(id = "ynow_scr_missing", "Custom Screen unavailable.")
  }
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
          style = "border:0;box-shadow:none;margin:0;",
          tags$div(
            class = "box-body",
            style = "padding-top:8px;",
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

#' Lazy page body for standalone Global Prediction (Economic / Finance / Company).
.ynow_page_ui_global_prediction <- function(locale = "en") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    "en"
  }
  .gp_s <- function(key) {
    if (exists("ui_str", mode = "function")) ui_str(key, loc) else key
  }
  tagList(
    fluidRow(
      column(
        width = 12,
        h2(tags$b(id = "ynow_gp_page_title", .gp_s("gp_page_title"))),
        tags$hr()
      )
    ),
    fluidRow(
      column(
        width = 12,
        global_prediction_ui("global_prediction", locale = loc)
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
        title = tags$span(id = "ynow_smart_chart_title", "Valuation comparison"),
        plotlyOutput("smart_analysis_chart", height = "360px") %>% shinycssloaders::withSpinner()
      )
    ),
    uiOutput("smart_analysis_reason"),
    # Lite applied-assumption disclosures: page-footer footnotes (not mid-page alerts).
    uiOutput("smart_analysis_assumption_notes")
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

#' Lazy Company body. industry_choice stays in the tabItem shell.
.ynow_page_ui_dashboard <- function() {
  tagList(

              uiOutput("company_overview_ui"),
              uiOutput("ynow_news_thermometer"),

              # FINANCIAL REPORT: Board / CB (+ Ownership) stays Lite+Full; other FS tabs Full-only via CSS.
              tags$div(
                class = "ynow-dashboard-fin-report-wrap",
              .ynow_focus_metric_legend_note_ui(),
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

                     # Board / CB + Ownership Structure Analysis（Lite + Full；見下方 ynow-full-only / Lite CSS）
                     tabPanel(
                       title = "Board / CB",
                       value = "corp_events",
                       icon = icon("landmark"),
                       id = "corp_events_panel",
                       uiOutput("corp_events_ui"),
                       # Ownership Structure Analysis — immediately below Convertible bonds — dilution
                       tags$hr(class = "ynow-own-under-cb-hr"),
                       tags$div(
                         id = "ownership_structure_panel",
                         class = "ynow-ownership-under-cb",
                         tags$h4(
                           class = "ynow-corp-events-h",
                           ui_i18n("tab_ownership_structure")
                         ),
                         uiOutput("ownership_structure_top_ui"),
                         plotly::plotlyOutput("own_volume_plot", height = "320px"),
                         uiOutput("ownership_structure_holders_ui")
                       )
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
  )
}

#' Lazy page body for tab `snapshot` (live parameters, manual audit, APP_DEFAULTS).
.ynow_page_ui_snapshot <- function() {
  tagList(
        h2(tags$span(id = "ynow_snapshot_page_title", "Snapshot")),
        tags$p(
          id = "ynow_snapshot_page_help",
          class = "help-block ynow-full-only",
          paste0(
            "Three tabs: current live parameters (download / upload restore CSV); ",
            "manual adjustments vs the post-Search baseline (with annotated PDF); and APP_DEFAULTS ",
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
        tabBox(
          # No right-side chrome title (page h2 already says Snapshot; avoids wrong remaps)
          # LTR order: Current App Parameter Snapshot (snap_current) before Manual adjustments.
          id = "snapshot_report",
          width = "auto",
          selected = "snap_current",

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
            # Enter leaves self-view cleared (no Exit). Exit self-view shows only when a
            # mode is active (CSS); top-of-screen mode reminder is separate.
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
                    "ynow_pts_sysadmin_self_signed_in",
                    label = tags$span(
                      id = "ynow_pts_sysadmin_self_signed_in_label",
                      "Signed-in user mode"
                    ),
                    class = "btn-default btn-sm",
                    title = "Quick apply all-user member restrictions"
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
                  '<span class="ynow-app-title-base" aria-hidden="true">The YNow App v28.47</span>',
                  '<span class="ynow-app-title-fill" aria-hidden="true">',
                  '<span class="ynow-app-title-fill-inner">The YNow App v28.47</span>',
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
             menuItem(
               text = if (exists("ynow_sidebar_guest_lock_label", mode = "function")) {
                 ynow_sidebar_guest_lock_label(
                   "global_prediction", "ynow_menu_global_prediction", "Global Prediction"
                 )
               } else {
                 tags$span(id = "ynow_menu_global_prediction", "Global Prediction")
               },
               tabName = "global_prediction",
               icon = icon("chart-pie")
             ),
             menuItem("Stock Prospecting", tabName = "bluechip", icon = icon("star")),
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
            onclick = "if (document.body && document.body.classList.contains('ynow-authed') && !document.body.classList.contains('ynow-sysadmin-on')) { return false; } if (window.Shiny) { Shiny.setInputValue('sidebar_test_click', (window.__ynowTestClicks=(window.__ynowTestClicks||0)+1), {priority: 'event'}); } if (window.ynowShowSidebarFootTab) { window.ynowShowSidebarFootTab('testing'); } else if (window.Shiny) { Shiny.setInputValue('sidebar_tabs', 'testing', {priority: 'event'}); } return false;",
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
    # Persistent top reminder while a view mode is active (Exit on this strip).
    # Sibling of page chrome — not inside .ynow-pts-sysadmin-box (keeps portal usable).
    tags$div(
      id = "ynow_admin_mode_reminder",
      class = "ynow-admin-mode-reminder",
      role = "status",
      `aria-live` = "polite",
      `aria-label` = "Active system admin view mode",
      hidden = NA,
      tags$div(
        class = "ynow-admin-mode-reminder__inner",
        tags$strong(
          id = "ynow_admin_mode_reminder_title",
          class = "ynow-admin-mode-reminder__title",
          ""
        ),
        tags$span(
          id = "ynow_admin_mode_reminder_body",
          class = "ynow-admin-mode-reminder__body",
          ""
        ),
        tags$button(
          id = "ynow_admin_mode_reminder_exit",
          type = "button",
          class = "btn btn-default btn-sm ynow-admin-mode-reminder__exit",
          tags$span(
            id = "ynow_admin_mode_reminder_exit_label",
            "Exit self view"
          )
        )
      )
    ),
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
      tags$script(src = "parameter_completeness.js"),
      # Ticker / Stock Code: case-insensitive entry → always uppercase display+value.
      tags$script(src = "ynow_ticker_uppercase.js"),
      # First-load: hide shinyapps ReconnectUI (#ss-connect-dialog "Disconnected
      # from the server.") while SockJS is still negotiating after HTML paint on
      # a cold worker. Cleared on shiny:connected / sessioninitialized /
      # reconnected, or after 20s so real mid-session drops still surface.
      tags$style(HTML(paste(
        "html.ynow-boot-connecting #ss-connect-dialog,",
        "html.ynow-boot-connecting #ss-overlay.ss-gray-out,",
        "html.ynow-boot-connecting #ss-overlay {",
        "  display: none !important;",
        "}"
      ))),
      tags$script(HTML("
        (function () {
          var root = document.documentElement;
          if (!root) return;
          root.classList.add('ynow-boot-connecting');
          var cleared = false;
          function clearBootCloak() {
            if (cleared) return;
            cleared = true;
            try { root.classList.remove('ynow-boot-connecting'); } catch (e0) {}
          }
          document.addEventListener('shiny:connected', clearBootCloak);
          document.addEventListener('shiny:sessioninitialized', clearBootCloak);
          document.addEventListener('shiny:reconnected', clearBootCloak);
          setTimeout(clearBootCloak, 20000);
        })();
      ")),
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
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0("ynow_chrome.css?v=", utils::URLencode(as.character(.YNOW_BUILD_VERSION)[1], reserved = TRUE))
      ),
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
          "window.__YNOW_SIDEBAR_GUEST_LOCK_TABS = {lab_notes:true,decision_checklist:true,mos_assumptions:true,price_alerts:true,asset_transmission:true,global_prediction:true};"
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
      tags$script(
        src = paste0("ynow_app.js?v=", utils::URLencode(as.character(.YNOW_BUILD_VERSION)[1], reserved = TRUE))
      ),

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
      
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0("ynow_widgets.css?v=", utils::URLencode(as.character(.YNOW_BUILD_VERSION)[1], reserved = TRUE))
      ),
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
    # Snapshot, Global Prediction, Testing, and account maintenance (gate / 會員管理 /
    # points). Guest Sign-in-required surfaces (Lab / Checklist / Value Alerts) hide
    # the same chrome via CSS while gated. Snapshot and Global Prediction are not
    # ticker-search surfaces — no Ticker / Stock Code or Yahoo industry titles.
    # (Business Breakdown shares global Ticker / Stock Code chrome; Blue Chip uses
    # its own universe Search, not this chrome).
    conditionalPanel(
      condition = paste(
        "input.sidebar_tabs != 'about' &&",
        "input.sidebar_tabs != 'macro_market' &&",
        "input.sidebar_tabs != 'asset_transmission' &&",
        "input.sidebar_tabs != 'global_prediction' &&",
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
            var el = this;
            var v = el.value || '';
            var up = v.toUpperCase();
            if (up !== v) {
              var start = el.selectionStart, end = el.selectionEnd;
              el.value = up;
              try {
                if (typeof start === 'number' && typeof end === 'number') {
                  el.setSelectionRange(start, end);
                }
              } catch (eCaret) {}
              v = up;
            }
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
        // Long-press must survive pointerleave on tiny chips: capture the pointer,
        // ignore leave, abort only on move past MOVE_PX or pointercancel.
        (function() {
          var LONG_MS = 2000;
          var MOVE_PX = 12;
          var GUEST_KEY = 'ynow_pinned_recent_v1';
          var MAX_PINS = 12;
          var pressTimer = null;
          var pressEl = null;
          var pressId = null;
          var startX = 0;
          var startY = 0;
          var pressMoved = false;
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

          function releaseCapture(el) {
            if (!el || pressId == null) return;
            try {
              if (el.releasePointerCapture) el.releasePointerCapture(pressId);
            } catch (eRel) {}
            pressId = null;
          }

          function clearPress(el) {
            if (pressTimer) { clearTimeout(pressTimer); pressTimer = null; }
            releaseCapture(el || pressEl);
            pressEl = null;
            pressMoved = false;
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
            clearPress(this);
            longFired = false;
            pressMoved = false;
            pressEl = this;
            startX = e.clientX || 0;
            startY = e.clientY || 0;
            pressId = e.pointerId;
            try {
              if (this.setPointerCapture && e.pointerId != null) {
                this.setPointerCapture(e.pointerId);
              }
            } catch (eCap) {}
            var el = this;
            pressTimer = setTimeout(function() {
              if (pressEl === el && !pressMoved) fireToggle(el);
            }, LONG_MS);
          });
          $(document).on('pointermove', '.ynow-recent-chip, .ynow-pinned-chip', function(e) {
            if (pressEl !== this || pressMoved) return;
            var dx = Math.abs((e.clientX || 0) - startX);
            var dy = Math.abs((e.clientY || 0) - startY);
            if (dx > MOVE_PX || dy > MOVE_PX) {
              pressMoved = true;
              clearPress(this);
            }
          });
          $(document).on('pointerup pointercancel', '.ynow-recent-chip, .ynow-pinned-chip', function() {
            clearPress(this);
          });
          $(document).on('contextmenu', '.ynow-recent-chip, .ynow-pinned-chip', function(e) {
            e.preventDefault();
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
            document.querySelectorAll('.sidebar-menu > li > a').forEach(function(a) {
              if (editOn) a.setAttribute('draggable', 'false');
              else a.removeAttribute('draggable');
            });
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

          $(document).on('dragstart', '.sidebar-menu > li > a', function(e) {
            if (!editOn) return;
            e.preventDefault();
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
    # Navigation timing: measure click -> visible pane paint and lazy host value.
    # Marks stay in the browser Performance timeline for field diagnostics; the
    # latest samples are also exposed on window without adding server traffic.
    tags$script(HTML("
      (function() {
        if (window.__ynowNavTimingBound) return;
        window.__ynowNavTimingBound = true;
        window.__ynowNavTimings = window.__ynowNavTimings || [];
        var pending = null;
        function now() { return window.performance ? performance.now() : Date.now(); }
        function keep(kind, value, phase) {
          if (!pending || pending.kind !== kind || String(pending.value) !== String(value)) return;
          var sample = {kind: kind, value: String(value), phase: phase,
                        ms: Math.round((now() - pending.at) * 10) / 10,
                        at: Date.now()};
          window.__ynowNavTimings.push(sample);
          if (window.__ynowNavTimings.length > 80) window.__ynowNavTimings.shift();
        }
        document.addEventListener('click', function(ev) {
          var tab = ev.target && ev.target.closest ?
            ev.target.closest('.main-sidebar a[data-value], .sidebar-menu a[data-value]') : null;
          var market = ev.target && ev.target.closest ? ev.target.closest('.ynow-mkt-btn') : null;
          if (tab && tab.getAttribute('data-value')) {
            pending = {kind: 'tab', value: tab.getAttribute('data-value'), at: now()};
          } else if (market && market.getAttribute('data-value')) {
            pending = {kind: 'market', value: market.getAttribute('data-value'), at: now()};
          }
        }, true);
        document.addEventListener('shiny:inputchanged', function(ev) {
          if (!pending || !ev) return;
          var matches = (pending.kind === 'tab' && ev.name === 'sidebar_tabs') ||
                        (pending.kind === 'market' && ev.name === 'market_mode_pick');
          if (!matches || String(ev.value) !== String(pending.value)) return;
          requestAnimationFrame(function() {
            requestAnimationFrame(function() { keep(pending.kind, pending.value, 'paint'); });
          });
        });
        document.addEventListener('shiny:value', function(ev) {
          if (!pending || pending.kind !== 'tab' || !ev || !ev.name) return;
          if (ev.name === 'ynow_lazy_host_' + pending.value) {
            keep('tab', pending.value, 'lazy-ready');
          }
        });
        if (window.jQuery) {
          jQuery(document).on('shiny:idle', function() {
            if (pending) keep(pending.kind, pending.value, 'idle');
          });
        }
      })();
    ")),
    # Ownership top-10 holders box: report AdminLTE collapse so server can keep
    # expand state across institution-detail remounts (reset on ticker/search).
    tags$script(HTML("
      (function() {
        if (document.documentElement.getAttribute('data-ynow-own-holders-collapse') === '1') return;
        document.documentElement.setAttribute('data-ynow-own-holders-collapse', '1');
        function reportOwnHoldersCollapsed() {
          var box = document.getElementById('own_holders_box');
          if (!box || typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
          var collapsed = box.classList.contains('collapsed-box');
          Shiny.setInputValue('own_holders_box_collapsed', collapsed, {priority: 'event'});
        }
        $(document).on(
          'click',
          '#own_holders_box > .box-header .btn-box-tool[data-widget=\"collapse\"]',
          function() {
            setTimeout(reportOwnHoldersCollapsed, 0);
          }
        );
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

    # Lock promo templates stay in the first HTML. Client lock script clones
    # them by id before the Snapshot tab mounts.
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
            "各模型「公式參數貢獻與敏感度」預設收合。完整 Decision Matrix 見基本面評價模型圖表下方（預設收合）。"
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
            # Model Selection Guide (Decision Matrix only) — under chart, collapsed
            .model_selection_guide_panel_ui(),
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
        uiOutput("ynow_lazy_host_snapshot")
      ),

      tabItem(
        tabName = "macro_market",
        uiOutput("ynow_lazy_host_macro_market")
      ),

      tabItem(
        tabName = "dashboard",
        # Search calls updatePickerInput("industry_choice") at connect. The id has to
        # be in the first HTML or Shiny drops the message.
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
        uiOutput("ynow_lazy_host_dashboard")
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
                           ynow_calc_reset_row(
                             ynow_calc_btn("mod_ddm-btn_calc_ddm", "試算 DDM"),
                             ynow_reset_defaults_btn("mod_ddm-reset_ddm")
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
                         plotlyOutput("ddm_sensitivity_heatmap", height = "420px") %>% withSpinner()
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         tags$details(
                           tags$summary(ui_i18n("sens_numeric_table_summary")),
                           div(
                             style = "width: 100%; overflow-x: auto; margin-top: 8px;",
                             tags$style(HTML("#ddm_sensitivity_table table { width: 100% !important; table-layout: fixed; }")),
                             tableOutput("ddm_sensitivity_table")
                           )
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
                                       ynow_calc_reset_row(
                                         ynow_calc_btn("calc", "試算 DCF"),
                                         ynow_reset_defaults_btn("reset_dcf")
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
                         # Single Bootstrap row: desktop wraps 4+4+4 / 4+4+4.
                         # Phone ≤767px: .ynow-wacc-kpi-grid → CSS 2×3 equal cells.
                         # One .row avoids Bootstrap clearfix + display:contents bugs.
                         fluidRow(
                           class = "ynow-wacc-kpi-grid",
                           valueBoxOutput("vbx_equity_val", width = 4), # 股權市值 (E)
                           valueBoxOutput("vbx_debt_val", width = 4),   # 總負債 (D)
                           valueBoxOutput("vbx_tax_rate", width = 4),   # 有效稅率 (T)
                           # Lock on col-sm-4 host (SGR pattern) so grid cells stay equal.
                           tags$div(
                             class = "col-sm-4",
                             `data-ynow-lock` = "output:wacc",
                             # bare uiOutput (SGR pattern); server infoBox width=NULL
                             shiny::uiOutput("ibx_wacc")
                           ),
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
                         plotlyOutput("dcf_sensitivity_heatmap", height = "420px") %>% withSpinner()
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         tags$details(
                           tags$summary(ui_i18n("sens_numeric_table_summary")),
                           div(
                             style = "width: 100%; overflow-x: auto; margin-top: 8px;",
                             tags$style(HTML("#dcf_sensitivity_table table { width: 100% !important; table-layout: fixed; }")),
                             tableOutput("dcf_sensitivity_table")
                           )
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
      # Global Prediction（Full guest lock；Lite 側欄省略；Economic / Finance / Company）
      # ==========================================
      tabItem(
        tabName = "global_prediction",
        {
          gp_gate <- if (exists("ynow_member_gate_ui", mode = "function")) {
            ynow_member_gate_ui(
              "ynow_gp_guest_gate",
              "ynow_gp_gate_title",
              "ynow_gp_gate_body",
              "ynow_gp_gate_signin",
              feature = "gp"
            )
          }
          gp_body <- tags$div(
            id = "ynow_gp_body",
            class = "ynow-member-body",
            uiOutput("ynow_lazy_host_global_prediction")
          )
          if (exists("ynow_member_gate_surface_ui", mode = "function")) {
            ynow_member_gate_surface_ui(
              feature = "gp", gate = gp_gate, body = gp_body
            )
          } else {
            tagList(gp_gate, gp_body)
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
