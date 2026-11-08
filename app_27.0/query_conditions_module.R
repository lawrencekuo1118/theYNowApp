# TheYNowApp — responsive query-conditions Shiny module (Lab 「查詢條件」)
#
# Owns UI layout + input normalization ONLY. Does not calculate eligible stocks,
# model applicability, financial quality, or snapshot counts — the existing Lab
# engine (lab_im_*) remains the sole filter.
#
# Production model ids match LAB_METHOD_ABS_KEYS / LAB_METHOD_REL_KEYS:
#   absolute: nav, pb, dcf, ddm, ri
#   relative: enterprise, earnings, revenue, book
# Visual ui_group (intrinsic / justified / multiples) is independent of
# legacy_group; Justified P/B stays on the absolute path.
#
# Preview: source this file then shiny::shinyApp(ynq_demo_ui, ynq_demo_server)

ynq_all <- "__ALL__"

ynq_or <- function(x, y) if (is.null(x)) y else x

ynq_bi <- function(key, fallback = key, locale = NULL) {
  if (exists("ui_str", mode = "function")) {
    hit <- tryCatch({
      if (!is.null(locale) && nzchar(as.character(locale)[1])) {
        ui_str(key, locale)
      } else {
        ui_str(key)
      }
    }, error = function(e) NULL)
    if (!is.null(hit) && nzchar(as.character(hit)[1])) return(as.character(hit)[1])
  }
  as.character(fallback)[1]
}

#' Production catalog for Blue Chip Lab (ids = existing method keys).
ynq_lab_catalog <- function(selected = NULL, locale = NULL) {
  if (is.null(selected)) {
    selected <- if (exists("APP_DEFAULTS") && !is.null(APP_DEFAULTS$lab_im_methods)) {
      as.character(APP_DEFAULTS$lab_im_methods)
    } else {
      c("nav", "pb", "dcf", "ddm", "ri")
    }
  }
  selected <- as.character(selected %||% character(0))
  ids <- c(
    "nav", "dcf", "ddm", "ri", "pb",
    "enterprise", "earnings", "revenue", "book"
  )
  labels <- c(
    ynq_bi("lab_im_method_nav", "NAV", locale = locale),
    ynq_bi("lab_im_method_dcf", "DCF", locale = locale),
    ynq_bi("lab_im_method_ddm", "DDM", locale = locale),
    ynq_bi("lab_im_method_ri", "RI", locale = locale),
    ynq_bi("lab_im_method_pb", "Justified P/B", locale = locale),
    ynq_bi("lab_im_method_enterprise", "Enterprise value", locale = locale),
    ynq_bi("lab_im_method_earnings", "Earnings", locale = locale),
    ynq_bi("lab_im_method_revenue", "Revenue", locale = locale),
    ynq_bi("lab_im_method_book", "Book multiples", locale = locale)
  )
  data.frame(
    id = ids,
    label = labels,
    ui_group = c(rep("intrinsic", 4L), "justified", rep("multiples", 4L)),
    legacy_group = c(rep("absolute", 5L), rep("relative", 4L)),
    selected = ids %in% selected,
    stringsAsFactors = FALSE
  )
}

#' Example catalog from the uploaded prototype (NOT production keys).
ynq_default_catalog <- function() {
  ynq_lab_catalog()
}

ynq_validate_catalog <- function(catalog) {
  required <- c("id", "label", "ui_group", "legacy_group", "selected")
  stopifnot(is.data.frame(catalog), all(required %in% names(catalog)))
  stopifnot(!anyNA(catalog[, required, drop = FALSE]))
  stopifnot(!anyDuplicated(catalog$id))
  stopifnot(all(catalog$ui_group %in% c("intrinsic", "justified", "multiples")))
  stopifnot(all(catalog$legacy_group %in% c("absolute", "relative")))
  stopifnot(is.logical(catalog$selected))
  invisible(TRUE)
}

ynq_section_title <- function(index, title, title_id = NULL) {
  h <- if (!is.null(title_id)) {
    tags$h3(id = title_id, title)
  } else {
    tags$h3(title)
  }
  tags$div(
    class = "ynq-section-heading",
    tags$span(class = "ynq-index", sprintf("%02d", as.integer(index))),
    h
  )
}

ynq_model_choices <- function(catalog, group) {
  rows <- catalog[catalog$ui_group == group, , drop = FALSE]
  stats::setNames(rows$id, rows$label)
}

ynq_model_defaults <- function(catalog, group) {
  rows <- catalog[catalog$ui_group == group, , drop = FALSE]
  rows$id[rows$selected]
}

#' Named choices for the Industry chain select (label → chain_id), plus __ALL__.
ynq_chain_select_choices <- function(locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    tryCatch(normalize_ui_locale(locale), error = function(e) "zh-TW")
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  all_lab <- ynq_bi("lab_im_chains_none", "全部產業鏈", locale = loc)
  out <- stats::setNames(ynq_all, all_lab)
  if (!exists("lab_industry_chain_picker_choices", mode = "function")) return(out)
  ch <- tryCatch(
    lab_industry_chain_picker_choices(loc),
    error = function(e) stats::setNames(character(0), character(0))
  )
  if (!length(ch)) return(out)
  c(out, ch)
}

#' Named choices for the Sub-industry select (label → industry_key), plus __ALL__.
ynq_sub_select_choices <- function(chain_ids = character(0), locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    tryCatch(normalize_ui_locale(locale), error = function(e) "zh-TW")
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  all_lab <- ynq_bi("lab_im_industries_none", "全部子產業", locale = loc)
  out <- stats::setNames(ynq_all, all_lab)
  if (!exists("lab_industry_sub_picker_choices", mode = "function")) return(out)
  chain_ids <- unique(as.character(chain_ids %||% character(0)))
  chain_ids <- chain_ids[nzchar(chain_ids) & !is.na(chain_ids) &
                           !identical(chain_ids, ynq_all)]
  subs <- tryCatch(
    lab_industry_sub_picker_choices(chain_ids, loc),
    error = function(e) stats::setNames(character(0), character(0))
  )
  if (!length(subs)) return(out)
  c(out, subs)
}

#' Build chain×subindustry tree for the module from Lab picker helpers.
ynq_lab_industry_tree <- function(locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    tryCatch(normalize_ui_locale(locale), error = function(e) "zh-TW")
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  if (!exists("lab_industry_chain_picker_choices", mode = "function") ||
      !exists("lab_industry_sub_picker_choices", mode = "function")) {
    return(data.frame(chain = character(0), subindustry = character(0),
                      stringsAsFactors = FALSE))
  }
  chains <- lab_industry_chain_picker_choices(loc)
  chain_ids <- unname(as.character(chains))
  chain_ids <- chain_ids[nzchar(chain_ids) & !is.na(chain_ids)]
  if (!length(chain_ids)) {
    return(data.frame(chain = character(0), subindustry = character(0),
                      stringsAsFactors = FALSE))
  }
  rows <- lapply(chain_ids, function(ch) {
    subs <- unname(as.character(lab_industry_sub_picker_choices(ch, loc)))
    subs <- subs[nzchar(subs) & !is.na(subs)]
    if (!length(subs)) return(NULL)
    data.frame(chain = ch, subindustry = subs, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) {
    data.frame(chain = character(0), subindustry = character(0),
               stringsAsFactors = FALSE)
  } else {
    out
  }
}

#' Map module filters() → values for existing lab_im_* inputs / resolvers.
ynq_filters_to_lab_bridge <- function(p, locale = "zh-TW") {
  p <- as.list(p %||% list())
  loc <- as.character(locale %||% "zh-TW")[1]
  all_chains <- character(0)
  if (exists("lab_industry_chain_picker_choices", mode = "function")) {
    all_chains <- unname(as.character(lab_industry_chain_picker_choices(loc)))
  }
  chain <- as.character(p$chain %||% ynq_all)[1]
  if (!nzchar(chain) || identical(chain, ynq_all)) {
    chains <- all_chains
  } else {
    chains <- chain
  }
  sub <- as.character(p$subindustry %||% ynq_all)[1]
  if (!nzchar(sub) || identical(sub, ynq_all)) {
    industries <- if (exists("lab_industry_sub_picker_choices", mode = "function")) {
      unname(as.character(lab_industry_sub_picker_choices(chains, loc)))
    } else {
      character(0)
    }
  } else {
    industries <- sub
  }
  methods <- as.character(p$selected_models %||% character(0))
  methods <- methods[nzchar(methods)]
  # Lazy mount can emit filters before model checkboxes bind. Never wipe the
  # hidden lab_im_methods bridge to empty — fall back to App defaults.
  if (!length(methods)) {
    methods <- if (exists("APP_DEFAULTS") && !is.null(APP_DEFAULTS$lab_im_methods)) {
      as.character(APP_DEFAULTS$lab_im_methods)
    } else {
      c("nav", "pb", "dcf", "ddm", "ri")
    }
  }
  list(
    lab_im_chains = chains,
    lab_im_industries = industries,
    lab_im_methods = methods,
    lab_im_eq_only = isTRUE(p$earnings_quality),
    lab_im_gate_only = isTRUE(p$piotroski),
    lab_im_no_alert = isTRUE(p$no_warning),
    lab_im_include_adr = isTRUE(p$include_adr)
  )
}

ynq_query_ui <- function(id, catalog = ynq_lab_catalog(), embed = FALSE,
                         locale = "zh-TW") {
  ynq_validate_catalog(catalog)
  ns <- NS(id)
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    tryCatch(normalize_ui_locale(locale), error = function(e) "zh-TW")
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  # Bind locale so ui_str does not fall back to its default "en" during bake.
  .t <- function(key, fallback = key) ynq_bi(key, fallback, locale = loc)
  # Bake chain/sub choices at UI render (lazy Blue Chip mount). Server
  # updateSelectInput often runs before the host exists and would otherwise
  # leave only the __ALL__ stub — looks like Industry chain "disappeared".
  chain_choices <- ynq_chain_select_choices(loc)
  sub_choices <- ynq_sub_select_choices(character(0), loc)

  css <- "
    .ynq-wrap{max-width:1080px;margin:0 auto;min-width:0;padding:4px 0 8px;color:#263444}
    .ynq-wrap *{box-sizing:border-box}
    .ynq-header{display:flex;align-items:center;justify-content:space-between;gap:10px;flex-wrap:wrap;margin-bottom:14px}
    .ynq-header h2{font-size:1.45rem;font-weight:750;margin:0}
    .ynq-muted{font-size:.88rem;color:#64748b;line-height:1.55}
    .ynq-card{border:1px solid #dbe4ee;border-radius:14px;background:#fff;padding:17px;margin:0 0 12px;min-width:0}
    .ynq-section-heading{display:flex;align-items:center;gap:10px;margin-bottom:14px}
    .ynq-section-heading h3{margin:0;font-size:1.13rem;font-weight:700}
    .ynq-index{border-radius:8px;padding:4px 9px;background:#e6f2fb;color:#1269a3;font-size:.78rem;font-weight:800}
    .ynq-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px}
    /* Valuation models + Fundamental quality gates: multi-select chip buttons
       matching HFV Chart Overlay Models (.ynow-hfv-toolbar). Native boxes are
       visually hidden; selection = filled dark pill. Topics keep one equal-height
       auto-fit row that wraps on narrow without clipping. */
    .ynq-optgroup .shiny-options-group{
      display:grid;
      grid-template-columns:repeat(auto-fit,minmax(min(100%,7.5rem),1fr));
      gap:6px 8px;
      align-items:stretch;
      width:100%;
      min-width:0;
    }
    .ynq-chip-toggles{
      display:flex;
      flex-wrap:wrap;
      align-items:stretch;
      gap:6px 8px;
      width:100%;
      min-width:0;
    }
    .ynq-optgroup .form-group,
    .ynq-chip-toggles .form-group{margin:0}
    .ynq-optgroup .checkbox,
    .ynq-optgroup .checkbox-inline,
    .ynq-chip-toggles .checkbox,
    .ynq-chip-toggles .checkbox-inline{
      float:none !important;
      position:relative;
      margin:0 !important;
      padding:0 !important;
      min-width:0;
      max-width:100%;
      height:100%;
      min-height:0;
      display:flex !important;
      align-items:stretch;
      width:100%;
    }
    .ynq-chip-toggles .checkbox,
    .ynq-chip-toggles .checkbox-inline{
      width:auto;
      flex:0 1 auto;
    }
    .ynq-optgroup .checkbox > label,
    .ynq-optgroup .checkbox-inline,
    .ynq-optgroup .checkbox-inline > label,
    .ynq-chip-toggles .checkbox > label,
    .ynq-chip-toggles .checkbox-inline,
    .ynq-chip-toggles .checkbox-inline > label{
      position:relative;
      margin:0 !important;
      padding:8px 12px !important;
      border:1px solid #d0d0d0;
      border-radius:999px;
      background:#fff;
      font-size:12.5px;
      font-weight:500;
      line-height:1.25;
      color:#222;
      white-space:normal;
      overflow-wrap:anywhere;
      word-break:break-word;
      display:flex !important;
      align-items:center;
      justify-content:center;
      text-align:center;
      gap:0;
      width:100%;
      min-width:0;
      min-height:36px;
      height:100%;
      box-sizing:border-box;
      cursor:pointer;
      transition:border-color .12s ease,background .12s ease,box-shadow .12s ease;
    }
    .ynq-chip-toggles .checkbox > label,
    .ynq-chip-toggles .checkbox-inline,
    .ynq-chip-toggles .checkbox-inline > label{
      width:auto;
      white-space:nowrap;
      overflow-wrap:normal;
      word-break:normal;
    }
    /* Hide native checkbox; selection shown by filled chip. */
    .ynq-optgroup .checkbox input[type='checkbox'],
    .ynq-optgroup .checkbox-inline input[type='checkbox'],
    .ynq-chip-toggles .checkbox input[type='checkbox'],
    .ynq-chip-toggles .checkbox-inline input[type='checkbox']{
      position:absolute !important;
      left:0 !important;
      top:0 !important;
      width:1px !important;
      height:1px !important;
      margin:0 !important;
      padding:0 !important;
      opacity:0 !important;
      clip:rect(0,0,0,0);
      pointer-events:none;
      flex-shrink:0;
    }
    .ynq-optgroup .checkbox > label:hover,
    .ynq-optgroup .checkbox-inline:hover,
    .ynq-chip-toggles .checkbox > label:hover,
    .ynq-chip-toggles .checkbox-inline:hover{
      border-color:#999;
      background:#fff;
    }
    .ynq-optgroup .checkbox:has(input:checked) > label,
    .ynq-optgroup .checkbox-inline:has(input:checked),
    .ynq-chip-toggles .checkbox:has(input:checked) > label,
    .ynq-chip-toggles .checkbox-inline:has(input:checked){
      border-color:#1a1a1a;
      background:#1a1a1a;
      color:#fff;
      box-shadow:0 1px 2px rgba(0,0,0,.12);
    }
    .ynq-label{font-size:.9rem;font-weight:700;margin:6px 0 10px}
    .ynq-subgroup+.ynq-subgroup{border-top:1px solid #edf1f5;margin-top:12px;padding-top:12px}
    .ynq-flag{display:inline-flex;align-items:center;padding:5px 10px;border-radius:999px;background:#e9f5ef;color:#26744f;font-size:.83rem}
    .ynq-card .form-control{max-width:100%;width:100%}
    .ynq-card .checkbox label{line-height:1.45}
    .ynq-metric-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:10px}
    .ynq-metric{border-radius:10px;background:#f2f7fb;padding:13px 10px;min-width:0}
    .ynq-metric .ynq-num{font-size:1.3rem;font-weight:800;color:#0a6097;font-variant-numeric:tabular-nums}
    .ynq-metric .ynq-muted{font-size:.77rem}
    .ynq-help{margin-top:11px;border-top:1px solid #edf1f5;padding-top:10px;color:#64748b;font-size:.86rem}
    .ynq-help summary{cursor:pointer;color:#375c79;font-weight:600}
    .ynq-help p{margin:8px 0 0;line-height:1.6}
    .ynq-card .selectize-control{width:100%}
    .ynq-optgroup label.ynow-lab-im-method--locked{
      opacity:.45;
      cursor:not-allowed;
      pointer-events:none;
    }
    @media(max-width:640px){
      .ynq-wrap{padding:8px 2px 12px}
      .ynq-card{padding:14px 12px;border-radius:12px;margin-bottom:10px}
      .ynq-grid{grid-template-columns:minmax(0,1fr);gap:3px}
      .ynq-metric-grid{grid-template-columns:repeat(2,minmax(0,1fr))}
      .ynq-optgroup .shiny-options-group{
        grid-template-columns:repeat(auto-fit,minmax(min(100%,6.5rem),1fr));
        gap:8px;
      }
      .ynq-chip-toggles .checkbox > label,
      .ynq-chip-toggles .checkbox-inline,
      .ynq-chip-toggles .checkbox-inline > label{
        white-space:normal;
      }
      .ynq-header h2{font-size:1.32rem}
      .ynq-metric .ynq-num{font-size:1.14rem}
      .ynq-card select,.ynq-card input{max-width:100%}
    }
    @media(max-width:360px){
      .ynq-optgroup .shiny-options-group{
        grid-template-columns:minmax(0,1fr);
      }
    }
  "

  group_input <- function(group) {
    tags$div(
      class = "ynq-optgroup",
      checkboxGroupInput(
        ns(paste0("models_", group)),
        label = NULL,
        inline = TRUE,
        choices = ynq_model_choices(catalog, group),
        selected = ynq_model_defaults(catalog, group)
      )
    )
  }

  header <- if (isTRUE(embed)) {
    NULL
  } else {
    tags$div(
      class = "ynq-header",
      tags$div(
        tags$h2(id = ns("title"), .t("lab_im_query_title", "查詢條件")),
        tags$div(
          id = ns("subtitle"),
          class = "ynq-muted",
          .t("ynq_subtitle", "美股估值與基本面篩選")
        )
      ),
      tags$div(
        id = ns("pool_flag"),
        class = "ynq-flag",
        .t("ynq_pool_flag", "主要上市股票池")
      )
    )
  }

  tagList(
    tags$style(HTML(css)),
    tags$div(
      class = "ynq-wrap",
      id = ns("wrap"),
      header,

      tags$section(
        class = "ynq-card ynow-full-only",
        ynq_section_title(1, .t("ynq_sec_pool", "股票池設定"), ns("sec_pool")),
        tags$div(
          class = "ynq-grid",
          selectInput(
            ns("chain"),
            tags$span(id = ns("chain_label"), .t("lab_im_chains_label", "產業鏈")),
            choices = chain_choices,
            selected = ynq_all
          ),
          selectInput(
            ns("subindustry"),
            tags$span(id = ns("sub_label"), .t("lab_im_industries_label", "子產業")),
            choices = sub_choices,
            selected = ynq_all
          )
        ),
        tags$div(
          class = "ynow-us-only",
          checkboxInput(
            ns("include_adr"),
            tags$span(
              id = ns("adr_label"),
              .t("lab_im_include_adr_label", "含 ADR")
            ),
            value = TRUE
          )
        ),
        tags$details(
          class = "ynq-help ynow-full-only",
          tags$summary(id = ns("help_pool_sum"), .t("ynq_help_pool_sum", "市場與產業篩選說明")),
          tags$p(id = ns("help_pool_body"), .t(
            "ynq_help_pool_body",
            "主要上市的限制由既有資料來源及 Server 規則維持。子產業依產業鏈提供選項；未選表示不額外限制。"
          ))
        )
      ),

      tags$section(
        class = "ynq-card ynow-full-only",
        ynq_section_title(2, .t("ynq_sec_models", "估值模型"), ns("sec_models")),
        tags$div(
          class = "ynq-subgroup",
          tags$div(
            id = ns("grp_intrinsic"),
            class = "ynq-label",
            .t("ynq_grp_intrinsic", "內在價值")
          ),
          group_input("intrinsic")
        ),
        tags$div(
          class = "ynq-subgroup",
          tags$div(
            id = ns("grp_justified"),
            class = "ynq-label",
            .t("ynq_grp_justified", "理論倍數")
          ),
          group_input("justified")
        ),
        tags$div(
          class = "ynq-subgroup",
          tags$div(
            id = ns("grp_multiples"),
            class = "ynq-label",
            .t("ynq_grp_multiples", "市場相對估值")
          ),
          group_input("multiples")
        ),
        tags$span(
          id = "ynow_lab_im_methods_locked_hint",
          class = "ynow-lab-im-quality-hint ynow-full-only",
          style = "display:block; color:#888; font-size:12px; margin-top:8px;",
          .t(
            "lab_im_methods_locked_hint",
            paste0(
              "若某評價模型未被任何個股列為主模型，則維持不勾選並鎖定。",
              "相對估值的企業價值、盈餘、營收、淨值，在該類被當成 Multiples 參照時才可勾選。"
            )
          )
        ),
        tags$details(
          class = "ynq-help",
          tags$summary(id = ns("help_models_sum"), .t("ynq_help_models_sum", "模型適用條件與篩選規則")),
          tags$p(id = ns("help_models_body"), .t(
            "ynq_help_models_body",
            "本介面只改變顯示分類，不改變任何模型的舊有篩選群組、主／副模型判斷、資料需求或 OR／AND 規則。"
          ))
        )
      ),

      tags$section(
        class = "ynq-card",
        ynq_section_title(3, .t("ynq_sec_quality", "基本面品質門檻"), ns("sec_quality")),
        tags$div(
          class = "ynq-chip-toggles ynq-chip-toggles--quality",
          role = "group",
          `aria-label` = .t("ynq_sec_quality", "基本面品質門檻"),
          checkboxInput(
            ns("earnings_quality"),
            tags$span(id = "ynow_lab_im_eq_label", .t("lab_im_eq_label", "盈餘品質")),
            value = FALSE
          ),
          checkboxInput(
            ns("piotroski"),
            tags$span(id = "ynow_lab_im_gate_label", .t("lab_im_gate_label", "Piotroski 高門檻")),
            value = FALSE
          ),
          checkboxInput(
            ns("no_warning"),
            tags$span(id = "ynow_lab_im_no_alert_label", .t("lab_im_no_alert_label", "無財報警訊")),
            value = FALSE
          )
        ),
        tags$details(
          class = "ynq-help ynow-full-only",
          tags$summary(id = ns("help_quality_sum"), .t("ynq_help_quality_sum", "門檻定義與快照限制")),
          tags$p(id = ns("help_quality_body"), .t(
            "ynq_help_quality_body",
            "例如既有 Piotroski F-Score ≥ 7、盈餘品質、Schilit／財報警訊及樣本數不足時不湊滿等規則，全部仍由原 Server 執行。未更新的品質快照不可視為通過。"
          ))
        )
      ),

      tags$section(
        class = "ynq-card ynow-full-only",
        ynq_section_title(4, .t("ynq_sec_results", "篩選結果"), ns("sec_results")),
        uiOutput(ns("counts")),
        textOutput(ns("snapshot_note")),
        tags$details(
          class = "ynq-help",
          tags$summary(id = ns("help_stats_sum"), .t("ynq_help_stats_sum", "統計範圍說明")),
          tags$p(id = ns("help_stats_body"), .t(
            "ynq_help_stats_body",
            "本模組不以 UI 選項自行估算檔數。請由既有股票池、模型適用性與品質快照流程傳入同一基準的統計值。"
          ))
        )
      )
    )
  )
}

ynq_query_server <- function(id, industry_tree, stats = NULL,
                             catalog = ynq_lab_catalog(),
                             refresh_token = NULL,
                             locale = NULL) {
  ynq_validate_catalog(catalog)
  stopifnot(is.function(industry_tree))

  moduleServer(id, function(input, output, session) {
    if (is.null(stats)) stats <- reactive(list())
    stopifnot(is.function(stats))

    .ynq_locale <- function() {
      if (is.function(locale)) {
        tryCatch(normalize_ui_locale(locale()), error = function(e) "zh-TW")
      } else if (!is.null(locale)) {
        tryCatch(normalize_ui_locale(locale), error = function(e) "zh-TW")
      } else {
        "zh-TW"
      }
    }

    safe_tree <- reactive({
      # Re-run after lazy Blue Chip mount so updateSelectInput hits a live input.
      if (is.function(refresh_token)) refresh_token()
      tree <- industry_tree()
      req(is.data.frame(tree), all(c("chain", "subindustry") %in% names(tree)))
      tree <- tree[, c("chain", "subindustry"), drop = FALSE]
      tree$chain <- as.character(tree$chain)
      tree$subindustry <- as.character(tree$subindustry)
      tree <- tree[!is.na(tree$chain) & !is.na(tree$subindustry) &
                     nzchar(tree$chain) & nzchar(tree$subindustry), , drop = FALSE]
      tree
    })

    observe({
      tree <- safe_tree()
      loc <- .ynq_locale()
      chains <- unique(tree$chain)
      chosen <- ynq_or(isolate(input$chain), ynq_all)
      if (!(chosen %in% chains)) chosen <- ynq_all
      choices <- ynq_chain_select_choices(loc)
      # Keep only chains present in the tree (+ __ALL__).
      keep <- unname(choices) == ynq_all | unname(choices) %in% chains
      choices <- choices[keep]
      updateSelectInput(
        session, "chain",
        choices = choices,
        selected = chosen
      )
    })

    observe({
      tree <- safe_tree()
      loc <- .ynq_locale()
      chain <- ynq_or(input$chain, ynq_all)
      chain_ids <- if (identical(chain, ynq_all)) character(0) else chain
      if (!identical(chain, ynq_all)) {
        tree <- tree[tree$chain == chain, , drop = FALSE]
      }
      subs <- unique(tree$subindustry)
      current <- ynq_or(isolate(input$subindustry), ynq_all)
      if (!(current %in% subs)) current <- ynq_all
      choices <- ynq_sub_select_choices(chain_ids, loc)
      keep <- unname(choices) == ynq_all | unname(choices) %in% subs
      choices <- choices[keep]
      updateSelectInput(
        session, "subindustry",
        choices = choices,
        selected = current
      )
    })

    filters <- reactive({
      tree <- safe_tree()
      chain <- ynq_or(input$chain, ynq_all)
      valid_chains <- unique(tree$chain)
      if (!(chain %in% valid_chains)) chain <- ynq_all

      visible <- if (identical(chain, ynq_all)) tree else
        tree[tree$chain == chain, , drop = FALSE]
      sub <- ynq_or(input$subindustry, ynq_all)
      if (!(sub %in% visible$subindustry)) sub <- ynq_all

      picked <- unique(c(
        ynq_or(input$models_intrinsic, character()),
        ynq_or(input$models_justified, character()),
        ynq_or(input$models_multiples, character())
      ))
      picked <- catalog$id[catalog$id %in% picked]

      list(
        chain = chain,
        subindustry = sub,
        include_adr = isTRUE(input$include_adr),
        listed_only = TRUE,
        selected_models = picked,
        selected_absolute = picked[picked %in%
          catalog$id[catalog$legacy_group == "absolute"]],
        selected_relative = picked[picked %in%
          catalog$id[catalog$legacy_group == "relative"]],
        earnings_quality = isTRUE(input$earnings_quality),
        piotroski = isTRUE(input$piotroski),
        no_warning = isTRUE(input$no_warning)
      )
    })

    fmt_n <- function(x) {
      if (is.null(x) || length(x) != 1L || !is.finite(suppressWarnings(as.numeric(x))))
        return("—")
      format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
    }
    count_tile <- function(label, value) {
      tags$div(
        class = "ynq-metric",
        tags$div(class = "ynq-num", fmt_n(value)),
        tags$div(class = "ynq-muted", label)
      )
    }

    output$counts <- renderUI({
      s <- ynq_or(stats(), list())
      tags$div(
        class = "ynq-metric-grid",
        count_tile(ynq_bi("ynq_metric_universe", "原始股票池"), s$universe_n),
        count_tile(ynq_bi("ynq_metric_market", "產業與市場後"), s$market_n),
        count_tile(ynq_bi("ynq_metric_model", "模型適用後"), s$model_n),
        count_tile(
          ynq_bi("ynq_metric_quality", "品質門檻後"),
          if (isTRUE(s$snapshot_ready)) s$quality_n else NULL
        )
      )
    })

    output$snapshot_note <- renderText({
      s <- ynq_or(stats(), list())
      if (!isTRUE(s$snapshot_ready)) {
        ynq_bi(
          "ynq_snapshot_pending",
          "品質快照：待更新／未確認涵蓋率。品質門檻後的檔數暫不顯示。"
        )
      } else {
        paste0(
          ynq_bi("ynq_snapshot_ready", "品質快照：已更新"),
          if (!is.null(s$snapshot_at)) paste0("（", s$snapshot_at, "）") else "",
          if (!is.null(s$coverage)) {
            paste0("；", ynq_bi("ynq_snapshot_coverage", "涵蓋率"), " ", s$coverage)
          } else {
            ""
          }
        )
      }
    })

    filters
  })
}

#' Compare candidate id sets from old vs adapted paths (UI-only rollout gate).
ynq_assert_same_candidates <- function(old_result, new_result, id_column = "symbol") {
  stopifnot(is.data.frame(old_result), is.data.frame(new_result))
  stopifnot(id_column %in% names(old_result), id_column %in% names(new_result))
  old_ids <- sort(as.character(old_result[[id_column]]), na.last = TRUE)
  new_ids <- sort(as.character(new_result[[id_column]]), na.last = TRUE)
  if (!identical(old_ids, new_ids)) {
    stop(sprintf(
      "Candidate mismatch: old=%d rows, new=%d rows",
      nrow(old_result), nrow(new_result)
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# --------- Standalone UI preview (no stock-screening engine) ---------
ynq_demo_tree <- data.frame(
  chain = c("半導體", "半導體", "科技", "金融"),
  subindustry = c("IC 設計", "晶圓代工", "軟體", "銀行"),
  stringsAsFactors = FALSE
)

# Built lazily so offline unit tests can source this file without attaching shiny.
ynq_demo_ui <- NULL
ynq_demo_server <- function(input, output, session) {
  query <- ynq_query_server(
    "query",
    industry_tree = reactive(ynq_demo_tree),
    stats = reactive(list(snapshot_ready = FALSE))
  )
  output$selected_filters <- renderPrint(str(query()))
}

ynq_build_demo_ui <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny is required for ynq_build_demo_ui()", call. = FALSE)
  }
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),
    ynq_query_ui("query"),
    shiny::tags$div(
      style = "max-width:1080px;margin:20px auto;padding:0 8px",
      shiny::tags$h4("開發預覽：正規化條件（不代表真實股票篩選結果）"),
      shiny::verbatimTextOutput("selected_filters")
    )
  )
}
