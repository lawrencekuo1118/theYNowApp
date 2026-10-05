# Business Breakdown — sidebar report module (tab id: company_advance).
# Shares the global Ticker / Stock Code field; not a valuation engine.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.bblab_ui <- function(key, locale = "en") {
  if (exists("ui_str", mode = "function")) {
    tryCatch(ui_str(key, locale), error = function(e) key)
  } else key
}

.bblab_history_visible <- function(res) {
  if (is.null(res) || !is.list(res)) return(FALSE)
  hist <- res$history
  isTRUE(hist$eligible) && length(hist$years) >= 2L && length(hist$series) >= 1L
}

.bblab_fmt_amt <- function(x, digits = 0) {
  num <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(num)) return("—")
  prettyNum(round(num, digits), big.mark = ",", scientific = FALSE)
}

.bblab_fmt_pct <- function(x) {
  num <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(num)) return("—")
  if (abs(num) <= 1.0000001) num <- num * 100
  sprintf("%.1f%%", num)
}

.bblab_snapshot_is_html <- function(cons, locale = "en",
                                    statement_currency = NA_character_,
                                    period = NA_character_) {
  if (is.null(cons) || !is.list(cons)) return(NULL)
  msg <- function(k) .bblab_ui(k, locale)
  gm <- if (.bblab_finite(cons$revenue) && .bblab_finite(cons$gp) && cons$revenue != 0) {
    cons$gp / cons$revenue
  } else NA_real_
  ccy <- cons$currency %||% statement_currency
  per <- cons$period %||% period
  if (!nzchar(.bblab_chr(ccy))) ccy <- "—"
  if (!nzchar(.bblab_chr(per))) per <- "—"
  line <- function(op, lab, amt, formula = NULL, extra = NULL) {
    tags$tr(
      class = paste(c("ynow-bblab-is__row", extra), collapse = " "),
      tags$td(class = "ynow-bblab-is__op", op),
      tags$td(class = "ynow-bblab-is__lab", lab),
      tags$td(class = "ynow-bblab-is__amt", amt),
      tags$td(class = "ynow-bblab-is__fml", if (nzchar(formula %||% "")) formula else "")
    )
  }
  rows <- list(
    line("", msg("bblab_kpi_revenue"), .bblab_fmt_amt(cons$revenue)),
    line("\u2212", msg("bblab_kpi_cor"), .bblab_fmt_amt(cons$cor)),
    line("=", msg("bblab_kpi_gp"), .bblab_fmt_amt(cons$gp),
         msg("bblab_formula_gp"), "ynow-bblab-is__row--total"),
    line("", msg("bblab_kpi_gm"), .bblab_fmt_pct(gm),
         msg("bblab_formula_gm"), "ynow-bblab-is__row--ratio")
  )
  if (.bblab_finite(cons$ni)) {
    rows[[length(rows) + 1L]] <- line(
      "", msg("bblab_kpi_ni"), .bblab_fmt_amt(cons$ni), extra = "ynow-bblab-is__row--ni"
    )
  }
  tags$div(
    class = "ynow-bblab-is",
    tags$div(
      class = "ynow-bblab-is__meta",
      tags$span(
        class = "ynow-bblab-is__meta-item",
        tags$span(class = "ynow-bblab-is__meta-lab", msg("bblab_period_label")),
        tags$span(class = "ynow-bblab-is__meta-val", per)
      ),
      tags$span(
        class = "ynow-bblab-is__meta-item",
        tags$span(class = "ynow-bblab-is__meta-lab", msg("bblab_statement_ccy_label")),
        tags$span(class = "ynow-bblab-is__meta-val", ccy)
      )
    ),
    tags$table(
      class = "ynow-bblab-is__table",
      tags$tbody(rows)
    )
  )
}

.bblab_neutral_class <- function(classification) {
  classification %in% c("OTHER", "UNALLOCATED", "RECONCILIATION", "ROUNDING")
}

.bblab_structure_panel_html <- function(sa, locale = "en") {
  msg <- function(k) .bblab_ui(k, locale)
  if (is.null(sa) || !is.list(sa)) {
    return(tags$p(class = "help-block", msg("bblab_waiting")))
  }
  nd <- msg("bblab_struct_not_disclosed")
  fmt_oi <- function(row) {
    if (!isTRUE(row$operating_income_disclosed)) return(nd)
    .bblab_fmt_amt(row$operating_income)
  }
  fmt_margin <- function(row) {
    if (!isTRUE(row$operating_income_disclosed)) return(nd)
    .bblab_fmt_pct(row$margin)
  }
  fmt_pc <- function(row) {
    if (!isTRUE(row$operating_income_disclosed) || !.bblab_finite(row$profit_contribution)) {
      return(nd)
    }
    .bblab_fmt_pct(row$profit_contribution)
  }
  biz_rows <- sa$businesses %||% list()
  adj_rows <- sa$adjustments %||% list()
  biz_table <- if (!length(biz_rows)) {
    tags$p(class = "help-block", msg("bblab_struct_no_business"))
  } else {
    tags$table(
      class = "table table-condensed table-striped ynow-bblab-struct-table",
      tags$thead(tags$tr(
        tags$th(msg("bblab_struct_col_business")),
        tags$th(msg("bblab_struct_col_revenue")),
        tags$th(msg("bblab_struct_col_rev_pct")),
        tags$th(msg("bblab_struct_col_oi")),
        tags$th(msg("bblab_struct_col_margin")),
        tags$th(msg("bblab_struct_col_profit_pct"))
      )),
      tags$tbody(lapply(biz_rows, function(r) {
        tags$tr(
          tags$td(r$business),
          tags$td(.bblab_fmt_amt(r$revenue)),
          tags$td(.bblab_fmt_pct(r$revenue_pct)),
          tags$td(fmt_oi(r)),
          tags$td(fmt_margin(r)),
          tags$td(fmt_pc(r))
        )
      }))
    )
  }
  adj_table <- if (!length(adj_rows)) {
    tags$p(class = "help-block", msg("bblab_struct_no_adjustments"))
  } else {
    tags$table(
      class = "table table-condensed table-striped ynow-bblab-struct-table",
      tags$thead(tags$tr(
        tags$th(msg("bblab_struct_col_adjustment")),
        tags$th(msg("bblab_struct_col_amount")),
        tags$th(msg("bblab_struct_col_type")),
        tags$th(msg("bblab_struct_col_expl"))
      )),
      tags$tbody(lapply(adj_rows, function(r) {
        tags$tr(
          tags$td(r$adjustment),
          tags$td(.bblab_fmt_amt(r$amount)),
          tags$td(r$type_label %||% r$type),
          tags$td(r$explanation %||% "")
        )
      }))
    )
  }
  tags$div(
    class = "ynow-bblab-struct ynow-bblab-struct--tables",
    tags$h4(id = "ynow_bblab_struct_biz_h", class = "ynow-bblab-subhead",
            msg("bblab_struct_biz_h")),
    biz_table,
    tags$h4(id = "ynow_bblab_struct_adj_h", class = "ynow-bblab-subhead",
            msg("bblab_struct_adj_h")),
    adj_table,
    if (!is.null(sa$bridge_note) && nzchar(.bblab_chr(sa$bridge_note))) {
      tags$p(class = "help-block", sa$bridge_note)
    } else NULL,
    if (length(sa$missing)) {
      tags$p(class = "help-block", paste(sa$missing, collapse = " "))
    } else NULL
  )
}

.bblab_structure_conclusions_html <- function(sa, locale = "en") {
  if (is.null(sa)) return(NULL)
  msg <- function(key) ui_str(key, locale)
  nd <- msg("bblab_struct_not_disclosed")
  conc <- sa$conclusions %||% list()
  conc_list <- tags$ol(
    class = "ynow-bblab-struct-conclusions",
    tags$li(tags$b(msg("bblab_struct_c1")), " ", conc$primary_revenue %||% nd),
    tags$li(tags$b(msg("bblab_struct_c2")), " ", conc$primary_profit %||% nd),
    tags$li(tags$b(msg("bblab_struct_c3")), " ", conc$high_revenue_low_profit %||% nd),
    tags$li(tags$b(msg("bblab_struct_c4")), " ", conc$high_profit_low_revenue %||% nd),
    tags$li(tags$b(msg("bblab_struct_c5")), " ", conc$corporate_consolidation %||% nd),
    tags$li(tags$b(msg("bblab_struct_c6")), " ", conc$non_operating %||% nd)
  )
  tags$div(
    class = "ynow-bblab-struct ynow-bblab-struct--conclusions",
    tags$h4(id = "ynow_bblab_struct_conc_h", class = "ynow-bblab-subhead",
            msg("bblab_struct_conc_h")),
    conc_list,
    tags$p(
      class = "ynow-bblab-struct-summary",
      tags$em(sa$summary_sentence %||% "")
    )
  )
}

#' TRUE only when Search input is clearly not a listed Taiwan or U.S. name.
#' Ambiguous symbols stay FALSE so valid TW/US listings are never blocked.
#' Company-agnostic: no ticker-specific formula branches.
bblab_clearly_not_listed_tw_us <- function(ticker) {
  raw <- toupper(trimws(as.character(ticker %||% "")[1]))
  raw <- sub("\\s+[—\\-–].*$", "", raw)
  raw <- gsub("\\s+", "", raw)
  if (!nzchar(raw) || identical(raw, "NA")) return(FALSE)
  if (grepl("^\\^", raw)) return(TRUE)
  if (grepl("=(X|F)$", raw)) return(TRUE)
  if (grepl("-(USD|USDT|USDC|BTC|ETH)$", raw)) return(TRUE)
  # TW listed: 4–6 digit codes, optional board letter, .TW / .TWO
  if (grepl("^[0-9]{4,6}[A-Z]?(\\.(TW|TWO))?$", raw)) return(FALSE)
  # US listed: 1–5 letters, optional share class (BRK.B / BRK-B)
  if (grepl("^[A-Z]{1,5}([.-][A-Z])?$", raw)) return(FALSE)
  if (grepl("\\.(HK|L|T|SS|SZ|KS|AX|TO|PA|DE|F|SW|MI|MC|SA|MX|NS|BO|JK|SI|NZ|OL|ST|CO|HE|BR|IR|IC|AS|VI|PK|OB)$", raw)) {
    return(TRUE)
  }
  FALSE
}

.bblab_report_section <- function(num, title_id, title_default, help_id = NULL,
                                  help_default = NULL, chapter = NULL, ...) {
  tags$section(
    class = "ynow-bblab-report__section",
    `data-bblab-chapter` = chapter %||% num,
    tags$header(
      class = "ynow-bblab-report__section-head",
      tags$span(class = "ynow-bblab-report__section-num", num),
      tags$h3(id = title_id, class = "ynow-bblab-report__section-title", title_default)
    ),
    if (!is.null(help_id)) {
      tags$p(id = help_id, class = "ynow-bblab-report__section-help", help_default %||% "")
    } else NULL,
    tags$div(class = "ynow-bblab-report__section-body", ...)
  )
}

business_breakdown_lab_ui <- function(id = "bblab") {
  ns <- NS(id)
  tags$div(
    class = "ynow-bblab ynow-bblab--report ynow-full-only",
    tags$article(
      class = "ynow-bblab-report",
      tags$header(
        class = "ynow-bblab-report__cover",
        tags$p(
          id = "ynow_bblab_report_kicker",
          class = "ynow-bblab-report__kicker",
          "Business Breakdown"
        ),
        tags$h1(
          id = "ynow_bblab_page_title",
          class = "ynow-bblab-report__title",
          "Business Breakdown"
        ),
        tags$p(
          id = "ynow_bblab_page_sub",
          class = "ynow-bblab-report__deck",
          paste0(
            "Investment-report view of the filer's business and profitability structure: ",
            "reportable segments, revenue and operating-income mix, accounting / ",
            "consolidation adjustments kept separate, and per-business cards. ",
            "Uses the global Ticker / Stock Code field. Not a valuation engine."
          )
        ),
        tags$style(HTML(paste(
          ".ynow-bblab-struct--tables{width:100%;margin:0 0 16px 0;}",
          ".ynow-bblab-struct--tables .ynow-bblab-struct-table{width:100%;}",
          ".ynow-bblab-struct-mix{display:flex;flex-wrap:wrap;gap:16px;align-items:flex-start;width:100%;}",
          ".ynow-bblab-struct-mix__conc{flex:1 1 48%;min-width:280px;}",
          ".ynow-bblab-struct-mix__chart{flex:1 1 44%;min-width:300px;max-width:560px;overflow:visible;}",
          ".ynow-bblab-struct-mix__chart .plotly,",
          ".ynow-bblab-struct-mix__chart .js-plotly-plot,",
          ".ynow-bblab-struct-mix__chart .plot-container,",
          ".ynow-bblab-struct-mix__chart .svg-container{width:100% !important;overflow:visible !important;}",
          ".ynow-bblab-struct-table{table-layout:fixed;width:100%;}",
          ".ynow-bblab-struct-table thead th{",
          "vertical-align:bottom;min-height:3.25em;height:3.25em;",
          "line-height:1.2;padding:8px 6px;white-space:normal;hyphens:auto;",
          "}",
          ".ynow-bblab-struct-mix__controls{margin:0 0 8px 0;}",
          ".ynow-bblab-struct-mix__controls .shiny-input-container{margin-bottom:6px;}",
          ".ynow-bblab-struct-mix__exports{margin-top:4px;}",
          ".ynow-bblab-struct-mix__exports .btn{margin:0 6px 6px 0;}",
          sep = ""
        ))),
        tags$p(
          id = "ynow_bblab_listed_only_notice",
          class = "ynow-bblab__listed-notice",
          role = "note",
          "Listed stocks only (Taiwan and U.S. exchanges)."
        ),
        tags$div(
          class = "ynow-bblab-report__meta",
          tags$div(
            class = "ynow-bblab-report__meta-item",
            tags$span(id = "ynow_bblab_company_label", class = "ynow-bblab-report__meta-lab", "Company"),
            uiOutput(ns("company_name"))
          ),
          tags$div(
            class = "ynow-bblab-report__meta-item",
            uiOutput(ns("statement_ccy"))
          ),
          tags$div(
            class = "ynow-bblab-report__meta-item ynow-bblab-report__meta-item--grow",
            uiOutput(ns("source_status"))
          )
        ),
        uiOutput(ns("listed_scope"))
      ),

      tags$div(
        class = "ynow-bblab-report__toolbar",
        fluidRow(
          column(
            width = 3,
            selectInput(
              ns("frequency"),
              label = tags$span(id = "ynow_bblab_period_label", "Period"),
              choices = c("Annual" = "annual", "Quarter" = "quarter"),
              selected = "annual"
            )
          ),
          column(
            width = 9,
            tags$div(
              class = "ynow-bblab-report__toolbar-check",
              checkboxInput(
                ns("use_gm_fallback"),
                label = tags$span(
                  id = "ynow_bblab_fallback_gm_label",
                  "Use consolidated Gross Margin as low-confidence fallback"
                ),
                value = FALSE
              )
            )
          )
        ),
        tags$p(
          id = "ynow_bblab_shared_ticker_hint",
          class = "ynow-bblab-report__hint",
          paste0(
            "Uses the header Ticker / Stock Code. Search there to load or refresh this report."
          )
        )
      ),

      uiOutput(ns("toasts_slot")),

      .bblab_report_section(
        "I", "ynow_bblab_ch1_title", "Consolidated statement snapshot",
        "ynow_bblab_ch1_help",
        paste0(
          "Reported consolidated Income Statement totals in statement currency. ",
          "Gross Profit = Revenue − Cost of Revenue. This is the whole firm, before any business split."
        ),
        chapter = "1",
        uiOutput(ns("snapshot"))
      ),

      .bblab_report_section(
        "II", "ynow_bblab_ch2_title", "How the statements split the business",
        "ynow_bblab_ch2_help",
        paste0(
          "One primary reporting dimension is selected. Geography that only describes ",
          "customer location is never the business split. Overlapping dimensions are never added together."
        ),
        chapter = "2",
        uiOutput(ns("summary"))
      ),

      .bblab_report_section(
        "III", "ynow_bblab_struct_title", "Business & profitability structure",
        "ynow_bblab_struct_help",
        paste0(
          "Main businesses come from the filer's reportable segments. ",
          "Corporate, eliminations, reconciliation, non-operating, and accounting ",
          "adjustments are kept separate and are never treated as operating businesses. ",
          "Segment Operating Income is shown only when disclosed — never estimated. ",
          "Main businesses and accounting adjustments span the full width; ",
          "profit-structure conclusions sit beside the revenue-mix chart. ",
          "Revenue-mix slices are shares of reported consolidated revenue."
        ),
        chapter = "3",
        uiOutput(ns("structure_panel")),
        tags$div(
          class = "ynow-bblab-struct-mix",
          tags$div(
            class = "ynow-bblab-struct-mix__conc",
            uiOutput(ns("structure_conclusions"))
          ),
          tags$div(
            class = "ynow-bblab-struct-mix__chart",
            tags$h4(
              class = "ynow-bblab-subhead",
              id = "ynow_bblab_ch3_title",
              "Revenue mix"
            ),
            tags$p(
              id = "ynow_bblab_ch3_help",
              class = "help-block",
              "Current-period slices are shares of reported consolidated revenue."
            ),
            tags$h5(
              class = "ynow-bblab-subhead",
              id = "ynow_bblab_ch3_current_label",
              "Current period"
            ),
            tags$div(
              class = "ynow-bblab-struct-mix__controls",
              radioButtons(ns("chart_mode"), NULL,
                           choices = c("Share %" = "pct", "Amount" = "amount"),
                           selected = "pct", inline = TRUE),
              radioButtons(ns("view_mode"), NULL,
                           choices = c("Reported" = "reported", "Adjusted" = "adjusted"),
                           selected = "reported", inline = TRUE),
              checkboxInput(ns("expand_other"),
                            label = tags$span(id = "ynow_bblab_expand_other", "Expand Other"),
                            value = FALSE),
              tags$div(
                class = "ynow-bblab-struct-mix__exports",
                downloadButton(ns("export_data"), "Export data", class = "btn-sm"),
                downloadButton(ns("export_chart"), "Export chart", class = "btn-sm")
              )
            ),
            uiOutput(ns("chart_status")),
            plotly::plotlyOutput(ns("donut"), height = "360px")
          )
        ),
        uiOutput(ns("history_panel"))
      ),

      .bblab_report_section(
        "IV", "ynow_bblab_ch5_title", "Business cards",
        "ynow_bblab_ch5_help",
        paste0(
          "One card per supportable business: reported Revenue, Cost of Revenue, ",
          "Gross Profit, and Gross Margin when disclosed or derived. Click a donut slice to focus a card."
        ),
        chapter = "4",
        uiOutput(ns("cards"))
      ),

      .bblab_report_section(
        "V", "ynow_bblab_ch6_title", "Reconciliation",
        chapter = "5",
        uiOutput(ns("recon"))
      ),

      tags$section(
        class = "ynow-bblab-report__section ynow-bblab-report__section--muted",
        tags$header(
          class = "ynow-bblab-report__section-head",
          tags$h3(
            id = "ynow_bblab_shared_title",
            class = "ynow-bblab-report__section-title",
            "Shared and Corporate Items"
          )
        ),
        tags$div(class = "ynow-bblab-report__section-body", uiOutput(ns("shared")))
      ),

      .bblab_report_section(
        "VI", "ynow_bblab_ch7_title", "Sources",
        chapter = "6",
        tags$p(
          id = "ynow_bblab_sources_chrome",
          class = "ynow-bblab-report__section-help",
          paste0(
            "Disclosure priority: operating segments → segment notes → product/service revenue → ",
            "revenue disaggregation → MD&A → earnings → IR decks → official descriptions. ",
            "Filed / audited sources are preferred. This page does not write into valuation."
          )
        ),
        uiOutput(ns("notes"))
      )
    )
  )
}

.bblab_card_html <- function(comp, locale = "en", focused = FALSE, cons_rev = NA_real_) {
  if (is.null(comp)) return(NULL)
  share <- if (.bblab_finite(comp$revenue_pct)) comp$revenue_pct else
    if (.bblab_finite(comp$revenue) && .bblab_finite(cons_rev) && cons_rev != 0) comp$revenue / cons_rev else NA_real_
  gp_txt <- if (!is.null(comp$gp_display) && nzchar(comp$gp_display)) comp$gp_display else .bblab_fmt_amt(comp$gp)
  gm_txt <- if (!is.null(comp$gm_display) && nzchar(comp$gm_display)) comp$gm_display else .bblab_fmt_pct(comp$gm)
  reval <- comp$revaluation
  reval_txt <- if (is.null(reval) || identical(reval$status, "UNAVAILABLE") || !is.finite(reval$revaluationRatio)) {
    .bblab_ui("bblab_reval_unavailable", locale)
  } else {
    sprintf("%s  %.3f  (%s)", reval$status, reval$revaluationRatio, .bblab_fmt_pct(reval$revaluationPercentage))
  }
  cls <- c("ynow-bblab-card", paste0("ynow-bblab-card--", tolower(comp$classification %||% "major")))
  if (isTRUE(focused)) cls <- c(cls, "ynow-bblab-card--focus")
  if (.bblab_neutral_class(comp$classification)) cls <- c(cls, "ynow-bblab-card--neutral")
  tags$div(
    class = paste(cls, collapse = " "),
    `data-component-id` = comp$id,
    tags$h4(comp$name),
    tags$p(class = "ynow-bblab-card__class", paste(comp$classification, "·",
                                                   comp$revenue_evidence$confidence %||% "UNAVAILABLE")),
    tags$ul(
      class = "ynow-bblab-card__metrics",
      tags$li(tags$b("Revenue: "), .bblab_fmt_amt(comp$revenue),
              "  ", tags$span(class = "muted", paste0("(", .bblab_fmt_pct(share), ")"))),
      tags$li(tags$b("Cost of Revenue: "),
              if (.bblab_finite(comp$cor)) .bblab_fmt_amt(comp$cor) else .bblab_ui("bblab_gm_unestimable", locale),
              if (!is.null(comp$cor_label) && !identical(comp$cor_label, "REPORTED"))
                tags$span(class = "ynow-bblab-tag", comp$cor_label) else NULL),
      tags$li(tags$b("Gross Profit: "), gp_txt),
      tags$li(tags$b("Gross Margin: "), gm_txt),
      if (.bblab_finite(comp$operating_income)) {
        tags$li(tags$b("Operating income: "), .bblab_fmt_amt(comp$operating_income))
      } else NULL,
      tags$li(tags$b(.bblab_ui("bblab_reval_label", locale), ": "), reval_txt)
    ),
    if (isTRUE(comp$qualitative_only) && nzchar(comp$description %||% "")) {
      tags$p(class = "muted", comp$description)
    } else NULL
  )
}

business_breakdown_lab_server <- function(id = "bblab",
                                          market_mode_rv = NULL,
                                          ui_locale_rv = NULL,
                                          current_ticker_rv = NULL,
                                          disclosures_provider = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lab_ticker <- reactiveVal(NULL)
    lab_entity <- reactiveVal("")
    lab_payload <- reactiveVal(NULL)
    lab_result <- reactiveVal(NULL)
    lab_codes <- reactiveVal(character(0))
    focus_id <- reactiveVal(NULL)
    source_status <- reactiveVal("idle")
    listed_scope_on <- reactiveVal(FALSE)

    loc <- function() {
      if (is.reactive(ui_locale_rv)) {
        tryCatch(normalize_ui_locale(ui_locale_rv()), error = function(e) "en")
      } else "en"
    }
    ui_msg <- function(key, ...) {
      msg <- .bblab_ui(key, loc())
      dots <- list(...)
      if (length(dots)) {
        for (nm in names(dots)) {
          msg <- gsub(paste0("{", nm, "}"), as.character(dots[[nm]] %||% ""), msg, fixed = TRUE)
        }
      }
      msg
    }

    # Shared global Ticker / Stock Code (current_ticker_rv) — no in-page Search.
    # Heavy scrape only while Business Breakdown tab is active (avoids Home/DCF double-load).
    observeEvent(
      list(
        if (is.reactive(current_ticker_rv)) current_ticker_rv() else NULL,
        input$frequency,
        tryCatch(session$rootScope()$input$sidebar_tabs, error = function(e) NULL)
      ),
      {
      # NULL ticker → character(0); if (!nzchar(raw)) then crashes ("argument is of length zero").
      raw <- tryCatch({
        v <- if (is.reactive(current_ticker_rv)) current_ticker_rv() else NULL
        if (is.null(v) || length(v) < 1L || (length(v) == 1L && is.na(v))) {
          ""
        } else {
          trimws(as.character(v[[1]]))
        }
      }, error = function(e) "")
      if (length(raw) != 1L || is.na(raw)) raw <- ""
      tab_now <- tryCatch(
        as.character(isolate(session$rootScope()$input$sidebar_tabs %||% ""))[1],
        error = function(e) ""
      )
      if (length(tab_now) != 1L || is.na(tab_now)) tab_now <- ""
      if (!nzchar(raw)) {
        lab_ticker(NULL)
        lab_entity("")
        lab_payload(NULL)
        lab_result(NULL)
        lab_codes(character(0))
        source_status("idle")
        listed_scope_on(FALSE)
        return(invisible(NULL))
      }
      # Defer Yahoo/SEC heavy path until the Business Breakdown tab is visible.
      if (!identical(tab_now, "company_advance") &&
          !identical(tab_now, "business_breakdown_lab")) {
        return(invisible(NULL))
      }
      mode <- if (is.reactive(market_mode_rv)) {
        tryCatch(market_mode_rv(), error = function(e) "US")
      } else "US"
      tk <- if (exists("normalize_ticker_for_market", mode = "function")) {
        normalize_ticker_for_market(raw, mode)
      } else raw
      listed_scope_on(
        isTRUE(bblab_clearly_not_listed_tw_us(raw)) ||
          isTRUE(bblab_clearly_not_listed_tw_us(tk))
      )
      lab_ticker(tk)
      lab_result(NULL)
      lab_codes(character(0))
      source_status("running")
      focus_id(NULL)

      tryCatch({
      withProgress(message = ui_msg("bblab_progress_running"), value = 0, {
        incProgress(0.1, detail = ui_msg("bblab_stage_resolve"))
        entity_name <- tk
        f_ccy <- NA_character_
        q_ccy <- NA_character_
        usd_twd <- NA_real_
        inst <- "ordinary"
        adr <- NA_real_
        d_is <- NULL
        sum_df <- NULL

        incProgress(0.25, detail = ui_msg("bblab_stage_retrieve"))
        if (exists("cached_get_summary_data", mode = "function")) {
          sum_df <- tryCatch(cached_get_summary_data(tk), error = function(e) NULL)
        } else if (exists("get_summary_data", mode = "function")) {
          sum_df <- tryCatch(get_summary_data(tk), error = function(e) NULL)
        }
        if (is.data.frame(sum_df) && nrow(sum_df) > 0) {
          entity_name <- as.character(sum_df$Name[1] %||% sum_df$shortName[1] %||% tk)
          if (exists("normalize_ccy", mode = "function")) {
            q_ccy <- normalize_ccy(attr(sum_df, "currency") %||% "")
            f_ccy <- normalize_ccy(attr(sum_df, "financialCurrency") %||% "")
          }
        }
        if (exists("cached_get_yahoo_industry", mode = "function")) {
          inf <- tryCatch(cached_get_yahoo_industry(tk), error = function(e) NULL)
          if (!is.null(inf) && nzchar(as.character(inf$company_name %||% "")[1])) {
            entity_name <- as.character(inf$company_name)[1]
          }
        } else if (exists("get_yahoo_industry", mode = "function")) {
          inf <- tryCatch(get_yahoo_industry(tk), error = function(e) NULL)
          if (!is.null(inf) && nzchar(as.character(inf$company_name %||% "")[1])) {
            entity_name <- as.character(inf$company_name)[1]
          }
        }
        lab_entity(entity_name)
        if (exists("cached_scrape_financials", mode = "function")) {
          # cached_scrape_financials already normalizes — avoid double work on cache hits
          res <- tryCatch(cached_scrape_financials(tk), error = function(e) NULL)
          if (!is.null(res) && !is.null(res[["Income Statement"]])) {
            d_is <- tryCatch({
              exp <- res[["Income Statement"]]$expanded
              if (exists("reorder_financial_columns", mode = "function")) {
                reorder_financial_columns(exp)
              } else if (exists("coerce_financial_df", mode = "function")) {
                coerce_financial_df(exp)
              } else exp
            }, error = function(e) NULL)
            if (!is.null(d_is) && !is.data.frame(d_is)) d_is <- NULL
            meta_fc <- attr(res, "financialCurrency")
            if (!is.null(meta_fc) && exists("normalize_ccy", mode = "function")) {
              mc <- normalize_ccy(meta_fc)
              if (!is.na(mc)) f_ccy <- mc
            }
          }
        }
        if (is.na(f_ccy) || !nzchar(.bblab_chr(f_ccy))) {
          if (grepl("\\.(TW|TWO)$", tk, ignore.case = TRUE)) f_ccy <- "TWD"
        }
        if (exists("cached_get_usd_twd_fx", mode = "function")) {
          usd_twd <- tryCatch(cached_get_usd_twd_fx(), error = function(e) NA_real_)
        }

        incProgress(0.45, detail = ui_msg("bblab_stage_parse"))
        extra_disc <- NULL
        if (is.function(disclosures_provider)) {
          extra_disc <- tryCatch(disclosures_provider(tk, d_is), error = function(e) NULL)
        }
        notes <- NULL
        if (is.null(extra_disc) || !length(extra_disc)) {
          us_edgar <- !grepl("\\.(TW|TWO)$", tk, ignore.case = TRUE)
          if (isTRUE(us_edgar) && exists("cached_fetch_sec_segment_notes", mode = "function")) {
            notes <- tryCatch(cached_fetch_sec_segment_notes(tk, "10-K"), error = function(e) NULL)
          }
        }
        payload <- bblab_payload_from_statements(
          d_is, ticker = tk, entity_name = entity_name,
          statement_currency = f_ccy, period = NA_character_,
          frequency = isolate(input$frequency) %||% "annual",
          disclosures = extra_disc,
          notes = notes,
          instrument_type = inst, adr_ratio = adr, usd_twd = usd_twd,
          display_currency = f_ccy
        )
        lab_payload(payload)
        extracted <- length(payload$dimensions %||% list()) >= 1L &&
          !all(vapply(payload$dimensions, function(d) {
            identical(d$kind %||% "", "official_description")
          }, logical(1)))
        if (is.null(d_is) && !length(extra_disc) && !.bblab_finite(payload$consolidated$revenue)) {
          source_status("statements_unavailable")
        } else if (isTRUE(extracted)) {
          source_status("ok")
        } else {
          source_status("statements_no_segment")
        }

        incProgress(0.7, detail = ui_msg("bblab_stage_analyze"))
        result <- bblab_analyze(
          payload,
          options = list(
            use_consolidated_gm_fallback = isTRUE(isolate(input$use_gm_fallback)),
            view = isolate(input$view_mode) %||% "reported",
            usd_twd = usd_twd,
            display_currency = f_ccy,
            per_adr_display = FALSE
          )
        )
        lab_result(result)
        lab_codes(result$codes %||% character(0))
        incProgress(1, detail = ui_msg("bblab_stage_done"))

        for (tst in result$toasts %||% list()) {
          # Scoped degradation: missing reval / overlapping dims are not Search errors.
          if (identical(tst$code, "BUSINESS_REVALUATION_UNAVAILABLE") ||
              identical(tst$code, "BUSINESS_OVERLAPPING_DIMENSIONS_BLOCKED") ||
              identical(tst$code, "BUSINESS_HISTORY_INSUFFICIENT_YEARS")) {
            next
          }
          msg <- ui_msg(paste0("notif_bblab_", tolower(tst$code)))
          if (identical(msg, paste0("notif_bblab_", tolower(tst$code)))) {
            msg <- paste0(
              tst$code, " — blocked: ",
              paste(tst$blocked_outputs, collapse = ", "),
              "; remaining: ", paste(tst$remaining_outputs, collapse = ", "),
              "; recon ", if (isTRUE(tst$recon_pass)) "pass" else "fail"
            )
          }
          is_page <- identical(tst$code, "BUSINESS_STATEMENTS_UNAVAILABLE") ||
            identical(tst$code, "BUSINESS_ISSUER_UNRESOLVED")
          showNotification(
            msg,
            type = if (isTRUE(is_page) || grepl("FAIL|MISSING", tst$code)) "warning" else "message",
            duration = 8
          )
        }
      })
      }, error = function(e) {
        pl <- tryCatch(isolate(lab_payload()), error = function(e2) NULL)
        has_stmt <- is.list(pl) && .bblab_finite(pl$consolidated$revenue)
        if (isTRUE(has_stmt)) {
          extracted <- length(pl$dimensions %||% list()) >= 1L &&
            !all(vapply(pl$dimensions, function(d) {
              identical(d$kind %||% "", "official_description")
            }, logical(1)))
          source_status(if (isTRUE(extracted)) "ok" else "statements_no_segment")
        } else {
          source_status("statements_unavailable")
          showNotification(
            ui_msg("bblab_source_unavailable"),
            type = "warning",
            duration = 8
          )
        }
      })
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    observeEvent(list(input$use_gm_fallback, input$view_mode), {
      payload <- lab_payload()
      if (is.null(payload)) return()
      result <- bblab_analyze(
        payload,
        options = list(
          use_consolidated_gm_fallback = isTRUE(input$use_gm_fallback),
          view = input$view_mode %||% "reported",
          usd_twd = payload$usd_twd,
          display_currency = payload$display_currency,
          per_adr_display = FALSE
        )
      )
      lab_result(result)
      lab_codes(result$codes %||% character(0))
    }, ignoreInit = TRUE)

    output$company_name <- renderUI({
      nm <- lab_entity()
      tk <- lab_ticker()
      label <- if (nzchar(nm)) nm else "—"
      if (nzchar(.bblab_chr(tk)) && !identical(label, tk)) {
        label <- paste0(label, "  ·  ", tk)
      } else if (nzchar(.bblab_chr(tk)) && identical(label, "—")) {
        label <- tk
      }
      tags$div(class = "ynow-bblab-report__meta-val", label)
    })
    output$statement_ccy <- renderUI({
      payload <- lab_payload()
      ccy <- payload$statement_currency %||% "—"
      tagList(
        tags$span(id = "ynow_bblab_statement_ccy_label", class = "ynow-bblab-report__meta-lab",
                  ui_msg("bblab_statement_ccy_label")),
        tags$div(class = "ynow-bblab-report__meta-val", ccy)
      )
    })
    output$source_status <- renderUI({
      st <- source_status()
      key <- switch(st,
                    idle = "bblab_source_idle",
                    running = "bblab_source_running",
                    ok = "bblab_source_ok",
                    statements_no_segment = "bblab_source_no_segment",
                    statements_unavailable = "bblab_source_unavailable",
                    "bblab_source_idle")
      tags$div(
        class = "ynow-bblab-report__meta-item",
        tags$span(class = "ynow-bblab-report__meta-lab", ui_msg("bblab_source_status_label")),
        tags$div(class = "ynow-bblab-report__meta-val", id = "ynow_bblab_source_status", ui_msg(key))
      )
    })
    output$listed_scope <- renderUI({
      if (!isTRUE(listed_scope_on())) return(NULL)
      tags$p(
        id = "ynow_bblab_listed_only_scope",
        class = "ynow-bblab__listed-scope help-block",
        ui_msg("bblab_listed_only_scope")
      )
    })

    output$snapshot <- renderUI({
      res <- lab_result()
      if (is.null(res)) {
        return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      }
      .bblab_snapshot_is_html(
        res$consolidated %||% list(),
        locale = loc(),
        statement_currency = res$statement_currency,
        period = res$period
      )
    })

    output$summary <- renderUI({
      res <- lab_result()
      if (is.null(res)) {
        return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      }
      kind <- res$primary_dimension$kind %||% "—"
      dim_key <- paste0("bblab_dim_", gsub("[^a-z_]", "", kind))
      dim_lab <- {
        mapped <- ui_msg(dim_key)
        if (identical(mapped, dim_key)) kind else mapped
      }
      n_biz <- length(res$businesses %||% list())
      recon <- res$reconciliation
      codes <- res$codes %||% character(0)
      tags$div(
        class = "ynow-bblab-summary",
        tags$ul(
          class = "ynow-bblab-split",
          tags$li(tags$b(ui_msg("bblab_dimension_label"), ": "), dim_lab),
          tags$li(tags$b(ui_msg("bblab_count_label"), ": "), n_biz),
          tags$li(tags$b(ui_msg("bblab_level_label"), ": "), res$level %||% "—"),
          tags$li(tags$b(ui_msg("bblab_confidence_label"), ": "), res$confidence$overall %||% "UNAVAILABLE"),
          tags$li(tags$b(ui_msg("bblab_rev_recon_label"), ": "),
                  if (isTRUE(recon$revenue$pass)) ui_msg("bblab_pass") else ui_msg("bblab_fail")),
          tags$li(tags$b(ui_msg("bblab_cor_recon_label"), ": "),
                  if (isTRUE(recon$cor$pass) || is.null(recon$cor)) ui_msg("bblab_pass") else ui_msg("bblab_fail")),
          tags$li(tags$b(ui_msg("bblab_reval_avail_label"), ": "),
                  if (isTRUE(res$revaluation_available)) ui_msg("bblab_available") else ui_msg("bblab_reval_unavailable")),
          tags$li(tags$b(ui_msg("bblab_limitations_label"), ": "),
                  if (length(res$limitations)) paste(res$limitations, collapse = "; ") else "—")
        ),
        if ("BUSINESS_GEOGRAPHY_CUSTOMER_LOCATION_ONLY" %in% codes) {
          tags$p(id = "ynow_bblab_geo_veto_why", class = "help-block", ui_msg("bblab_geo_veto_why"))
        } else NULL,
        if ("BUSINESS_OVERLAPPING_DIMENSIONS_BLOCKED" %in% codes) {
          tags$p(id = "ynow_bblab_overlap_why", class = "help-block", ui_msg("bblab_overlap_why"))
        } else NULL,
        if ("no_multi_business_split" %in% (res$limitations %||% character(0))) {
          tags$p(class = "help-block", ui_msg("bblab_single_business_note"))
        } else NULL
      )
    })

    .structure_analysis_sa <- function() {
      res <- lab_result()
      if (is.null(res)) return(NULL)
      sa <- res$structure_analysis
      if (is.null(sa) && exists("bblab_build_structure_analysis", mode = "function")) {
        sa <- bblab_build_structure_analysis(res, locale = loc())
      }
      sa
    }

    output$structure_panel <- renderUI({
      res <- lab_result()
      if (is.null(res)) return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      .bblab_structure_panel_html(.structure_analysis_sa(), loc())
    })

    output$structure_conclusions <- renderUI({
      res <- lab_result()
      if (is.null(res)) return(NULL)
      .bblab_structure_conclusions_html(.structure_analysis_sa(), loc())
    })

    output$chart_status <- renderUI({
      res <- lab_result()
      if (is.null(res) || isTRUE(res$chart$eligible)) return(NULL)
      tags$p(class = "help-block", paste(res$chart$codes, collapse = "; "))
    })

    output$donut <- plotly::renderPlotly({
      res <- lab_result()
      empty <- plotly::layout(plotly::plot_ly(type = "pie"), showlegend = FALSE, title = NULL)
      if (is.null(res) || !isTRUE(res$chart$eligible) || !length(res$chart$slices)) {
        return(empty)
      }
      slices <- res$chart$slices
      if (isTRUE(input$expand_other) && !is.null(res$other$members)) {
        slices <- Filter(function(s) !identical(s$id, "other_businesses") && !isTRUE(s$display_group), slices)
        for (m in res$other$members) {
          slices[[length(slices) + 1L]] <- list(
            id = m$id, name = m$name, revenue = m$revenue,
            share = m$revenue_pct, classification = m$classification %||% "OTHER",
            confidence = m$revenue_evidence$confidence %||% "MEDIUM",
            period = res$period, currency = res$statement_currency
          )
        }
      }
      labels <- vapply(slices, function(s) s$name, character(1))
      values <- vapply(slices, function(s) {
        if (identical(input$chart_mode, "amount")) .bblab_num(s$revenue, 0) else {
          sh <- .bblab_num(s$share, 0)
          if (is.finite(sh)) abs(sh) else 0
        }
      }, numeric(1))
      pal <- c("#0C5484", "#1AA8B8", "#249C60", "#E8A838", "#8E6BB5", "#D96B5F")
      col <- vapply(seq_along(slices), function(i) {
        if (.bblab_neutral_class(slices[[i]]$classification)) "#9AA3AB"
        else pal[((i - 1L) %% length(pal)) + 1L]
      }, character(1))
      hover <- vapply(slices, function(s) {
        paste0(
          s$name, "<br>Revenue: ", .bblab_fmt_amt(s$revenue),
          "<br>Share: ", .bblab_fmt_pct(s$share),
          "<br>", s$classification, " · ", s$confidence %||% "",
          "<br>", res$period %||% "", " · ", res$statement_currency %||% "",
          "<br>Recon: ", if (isTRUE(res$reconciliation$pass)) "pass" else "fail"
        )
      }, character(1))
      # Outside labels = percent only; full names stay in legend / hover (narrow side column).
      fig <- plotly::plot_ly(
        labels = labels, values = pmax(values, 0), type = "pie", hole = 0.45,
        customdata = vapply(slices, function(s) s$id, character(1)),
        textinfo = "percent",
        textposition = "outside",
        hovertext = hover, hoverinfo = "text",
        marker = list(colors = col, line = list(color = "#ffffff", width = 1)),
        domain = list(x = c(0.15, 0.85), y = c(0.1, 0.85)),
        source = ns("donut")
      )
      if (!is.null(res$chart$eliminations_legend)) {
        el <- res$chart$eliminations_legend
        fig <- plotly::add_annotations(
          fig,
          text = paste0(el$name, ": ", .bblab_fmt_amt(el$revenue), " (not a pie slice)"),
          x = 0.5, y = -0.12, showarrow = FALSE, xref = "paper", yref = "paper"
        )
      }
      plotly::layout(
        fig,
        showlegend = TRUE,
        margin = list(l = 70, r = 70, t = 50, b = 60),
        legend = list(orientation = "h", y = 1.12, x = 0.5, xanchor = "center"),
        uniformtext = list(minsize = 10, mode = "hide")
      )
    })

    output$history_panel <- renderUI({
      res <- lab_result()
      if (!isTRUE(.bblab_history_visible(res))) return(NULL)
      tagList(
        tags$h4(
          class = "ynow-bblab-subhead",
          id = "ynow_bblab_ch4_title",
          ui_msg("bblab_ch4_title")
        ),
        tags$p(
          id = "ynow_bblab_ch4_help",
          class = "help-block",
          ui_msg("bblab_ch4_help")
        ),
        plotly::plotlyOutput(ns("history"), height = "420px")
      )
    })

    output$history <- plotly::renderPlotly({
      empty <- plotly::layout(
        plotly::plot_ly(type = "bar"),
        showlegend = FALSE, title = NULL,
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      )
      res <- lab_result()
      hist <- res$history
      if (!isTRUE(.bblab_history_visible(res))) {
        return(empty)
      }
      years <- as.character(hist$years)
      pal <- c("#0C5484", "#1AA8B8", "#249C60", "#E8A838", "#8E6BB5", "#D96B5F")
      fig <- plotly::plot_ly()
      for (i in seq_along(hist$series)) {
        s <- hist$series[[i]]
        y <- vapply(years, function(ys) {
          v <- s$shares[[ys]]
          if (.bblab_finite(v) && v > 0) as.numeric(v)[1] * 100 else NA_real_
        }, numeric(1))
        col <- if (.bblab_neutral_class(s$classification)) "#9AA3AB"
        else pal[((i - 1L) %% length(pal)) + 1L]
        hover <- vapply(seq_along(years), function(j) {
          sh <- s$shares[[years[[j]]]]
          rv <- s$revenues[[years[[j]]]]
          paste0(
            s$name, "<br>", years[[j]],
            "<br>Share: ", .bblab_fmt_pct(sh),
            "<br>Revenue: ", .bblab_fmt_amt(rv),
            "<br>Denom: ", .bblab_fmt_amt(hist$denominator[[years[[j]]]])
          )
        }, character(1))
        fig <- plotly::add_trace(
          fig,
          x = years, y = y, name = s$name, type = "bar",
          marker = list(color = col),
          hovertext = hover, hoverinfo = "text"
        )
      }
      plotly::layout(
        fig,
        barmode = "stack",
        showlegend = TRUE,
        xaxis = list(title = "", type = "category"),
        yaxis = list(
          title = ui_msg("bblab_history_yaxis"),
          range = c(0, 100), ticksuffix = "%"
        ),
        margin = list(b = 40)
      )
    })

    observeEvent(plotly::event_data("plotly_click", source = ns("donut")), {
      ev <- plotly::event_data("plotly_click", source = ns("donut"))
      if (!is.null(ev) && !is.null(ev$customdata)) focus_id(as.character(ev$customdata)[1])
    }, ignoreNULL = TRUE)

    output$cards <- renderUI({
      res <- lab_result()
      if (is.null(res)) return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      # Missing revaluation never withholds reported / derived Rev, CoR, GP cards.
      biz <- res$businesses %||% list()
      cons_rev <- .bblab_num(res$consolidated$revenue)
      cards <- lapply(biz, function(c) {
        column(width = 4, .bblab_card_html(c, loc(), identical(focus_id(), c$id), cons_rev))
      })
      extra <- list()
      if (!is.null(res$other)) {
        extra[[length(extra) + 1L]] <- column(
          width = 4, .bblab_card_html(res$other, loc(), FALSE, cons_rev)
        )
      }
      if (!length(cards) && !length(extra)) {
        return(tags$p(class = "help-block", ui_msg("bblab_cards_empty")))
      }
      tagList(fluidRow(cards), if (length(extra)) fluidRow(extra) else NULL)
    })

    output$recon <- renderUI({
      res <- lab_result()
      if (is.null(res) || is.null(res$reconciliation)) {
        return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      }
      rec <- res$reconciliation
      badge <- if (isTRUE(rec$pass)) {
        tags$span(class = "ynow-bblab-recon ynow-bblab-recon--pass", ui_msg("bblab_pass"))
      } else {
        tags$span(class = "ynow-bblab-recon ynow-bblab-recon--fail", ui_msg("bblab_fail"))
      }
      row <- function(label, chk) {
        if (is.null(chk)) return(NULL)
        tags$tr(
          tags$td(label),
          tags$td(.bblab_fmt_amt(chk$reported)),
          tags$td(.bblab_fmt_amt(chk$recast)),
          tags$td(.bblab_fmt_amt(chk$leftover)),
          tags$td(if (isTRUE(chk$pass)) ui_msg("bblab_pass") else ui_msg("bblab_fail"))
        )
      }
      tags$div(
        badge,
        tags$table(
          class = "table table-condensed ynow-bblab-recon-table",
          tags$thead(tags$tr(tags$th(""), tags$th("Reported"), tags$th("Recast"),
                             tags$th("Leftover"), tags$th("Status"))),
          tags$tbody(
            row("Revenue", rec$revenue),
            row("Cost of Revenue", rec$cor),
            row("Gross Profit", rec$gp)
          )
        )
      )
    })

    output$shared <- renderUI({
      res <- lab_result()
      items <- res$shared_corporate %||% list()
      if (!length(items)) return(tags$p(class = "help-block", ui_msg("bblab_shared_empty")))
      tags$ul(lapply(names(items), function(nm) tags$li(tags$b(nm), ": ", .bblab_fmt_amt(items[[nm]]))))
    })

    output$notes <- renderUI({
      res <- lab_result()
      body <- tagList(
        tags$p(ui_msg("bblab_notes_body")),
        if (!is.null(res) && length(res$codes)) tags$p(paste("Codes:", paste(res$codes, collapse = ", "))) else NULL,
        if (!is.null(res) && "rev_share_cost" %in% (res$cost_notices %||% character(0))) {
          tags$p(ui_msg("bblab_rev_share_cost_notice"))
        } else NULL
      )
      if (exists("ynow_notes_block", mode = "function")) {
        ynow_notes_block(body, locale = loc())
      } else body
    })

    output$export_data <- downloadHandler(
      filename = function() {
        paste0("business_breakdown_", lab_ticker() %||% "lab", "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        res <- lab_result()
        rows <- lapply(res$businesses %||% list(), function(c) {
          data.frame(
            id = c$id, name = c$name, classification = c$classification,
            revenue = c$revenue %||% NA_real_, cor = c$cor %||% NA_real_,
            gp = c$gp %||% NA_real_, gm = c$gm %||% NA_real_,
            status_rev = c$revenue_evidence$status %||% NA_character_,
            status_cor = c$cor_evidence$status %||% NA_character_,
            stringsAsFactors = FALSE
          )
        })
        df <- if (length(rows)) do.call(rbind, rows) else data.frame()
        utils::write.csv(df, file, row.names = FALSE)
      }
    )
    output$export_chart <- downloadHandler(
      filename = function() paste0("business_breakdown_chart_", Sys.Date(), ".html"),
      content = function(file) {
        res <- lab_result()
        if (is.null(res) || !isTRUE(res$chart$eligible)) {
          writeLines("<html><body>Chart not eligible</body></html>", file)
          return()
        }
        htmlwidgets::saveWidget(plotly::last_plot(), file, selfcontained = TRUE)
      }
    )
  })
}
