# Business Breakdown Lab — standalone experimental Shiny module.
# Not wired into valuation, CV, company overview, or production FS pages.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.bblab_ui <- function(key, locale = "en") {
  if (exists("ui_str", mode = "function")) {
    tryCatch(ui_str(key, locale), error = function(e) key)
  } else key
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

.bblab_neutral_class <- function(classification) {
  classification %in% c("OTHER", "UNALLOCATED", "RECONCILIATION", "ROUNDING")
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

business_breakdown_lab_ui <- function(id = "bblab") {
  ns <- NS(id)
  tags$div(
    class = "ynow-bblab ynow-full-only",
    tags$div(
      class = "ynow-bblab__masthead",
      tags$div(
        class = "ynow-bblab__title-row",
        h2(tags$b(id = "ynow_bblab_page_title", "Business Breakdown Lab")),
        tags$span(
          id = "ynow_bblab_experimental_badge",
          class = "ynow-bblab-badge",
          "Experimental Feature"
        )
      ),
      tags$p(
        id = "ynow_bblab_page_sub",
        class = "ynow-bblab__lead",
        paste0(
          "Walk through the company's financial structure from the statement viewpoint: ",
          "consolidated totals, how the filer splits the business, current mix, ",
          "five-year share evolution, and per-business cards. Experimental; not a valuation engine."
        )
      ),
      tags$p(
        id = "ynow_bblab_listed_only_notice",
        class = "ynow-bblab__listed-notice",
        role = "note",
        "Listed stocks only (Taiwan and U.S. exchanges)."
      )
    ),

    # 1 Search — div (not <form>); actionButton type=button so click/Enter never native-submits.
    fluidRow(
      box(
        title = tagList(icon("search"), tags$span(id = "ynow_bblab_search_title", "Search")),
        width = 12, status = "primary", solidHeader = TRUE,
        tags$div(
          class = "ynow-bblab-search-controls",
          fluidRow(
          column(
            width = 3,
            textInput(ns("ticker"), label = tags$span(id = "ynow_bblab_ticker_label", "Ticker"),
                      placeholder = "AAPL / 2330")
          ),
          column(
            width = 2, style = "padding-top: 25px;",
            actionButton(ns("search"), "Search", icon = icon("search"),
                         class = "btn-primary ynow-bblab-search-btn", width = "100%")
          ),
          column(
            width = 3,
            tags$div(class = "ynow-bblab-meta",
                     tags$div(id = "ynow_bblab_company_label", class = "ynow-bblab-meta__lab", "Company"),
                     uiOutput(ns("company_name")))
          ),
          column(
            width = 2,
            selectInput(ns("frequency"), label = tags$span(id = "ynow_bblab_period_label", "Period"),
                        choices = c("Annual" = "annual", "Quarter" = "quarter"),
                        selected = "annual")
          ),
          column(
            width = 2,
            uiOutput(ns("statement_ccy"))
          )
        )
        ),
        tags$script(HTML(sprintf(
          paste0(
            "(function(){",
            "if(window.__ynowBblabSearchGuard) return; window.__ynowBblabSearchGuard=1;",
            "var tickerId=%s, btnId=%s;",
            "document.addEventListener('keydown',function(ev){",
            "var t=ev.target; if(!t||t.id!==tickerId) return;",
            "if(ev.key!=='Enter'&&ev.keyCode!==13) return;",
            "ev.preventDefault(); ev.stopPropagation();",
            "var btn=document.getElementById(btnId);",
            "if(btn&&typeof btn.click==='function') btn.click();",
            "},true);",
            "document.addEventListener('submit',function(ev){",
            "var root=document.querySelector('.ynow-bblab');",
            "if(!root||!ev.target) return;",
            "if(root===ev.target||root.contains(ev.target)){",
            "ev.preventDefault(); ev.stopPropagation();",
            "}",
            "},true);",
            "})();"
          ),
          paste0('"', ns("ticker"), '"'),
          paste0('"', ns("search"), '"')
        ))),
        uiOutput(ns("source_status")),
        uiOutput(ns("listed_scope")),
        tags$div(
          style = "margin-top:8px;",
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

    uiOutput(ns("toasts_slot")),

    # 1 Consolidated statement snapshot
    fluidRow(
      box(
        title = tagList(
          tags$span(class = "ynow-bblab-chapter__num", "1"),
            icon("building"),
          tags$span(id = "ynow_bblab_ch1_title", "Consolidated statement snapshot")
        ),
        width = 12, status = "info", solidHeader = TRUE,
        `data-bblab-chapter` = "1",
        tags$p(id = "ynow_bblab_ch1_help", class = "help-block",
               "Reported consolidated totals in statement currency. This is the whole firm, before any business split."),
        uiOutput(ns("snapshot"))
      )
    ),

    # 2 How the statements split the business
    fluidRow(
      box(
        title = tagList(
          tags$span(class = "ynow-bblab-chapter__num", "2"),
          icon("sitemap"),
          tags$span(id = "ynow_bblab_ch2_title", "How the statements split the business")
        ),
        width = 12, status = "info", solidHeader = TRUE,
        `data-bblab-chapter` = "2",
        tags$p(id = "ynow_bblab_ch2_help", class = "help-block",
               paste0(
                 "One primary reporting dimension is selected. Geography that only describes ",
                 "customer location is never the business split. Overlapping dimensions are never added together."
               )),
        uiOutput(ns("summary"))
      )
    ),

    # 3 Revenue mix (current donut + five-year evolution)
    fluidRow(
      box(
        title = tagList(
          tags$span(class = "ynow-bblab-chapter__num", "3"),
          icon("chart-pie"),
          tags$span(id = "ynow_bblab_ch3_title", "Revenue mix")
        ),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
        `data-bblab-chapter` = "3",
        tags$p(id = "ynow_bblab_ch3_help", class = "help-block",
               paste0(
                 "Current-period slices are shares of reported consolidated revenue. ",
                 "Below, revenue share by business for up to five fiscal years on the same reporting dimension. ",
                 "Years that cannot be mapped are omitted; shares are never fabricated or filled with 0."
               )),
        tags$h4(
          class = "ynow-bblab-subhead",
          id = "ynow_bblab_ch3_current_label",
          "Current period"
        ),
        fluidRow(
          column(width = 3, radioButtons(ns("chart_mode"), NULL,
                                         choices = c("Share %" = "pct", "Amount" = "amount"),
                                         selected = "pct", inline = TRUE)),
          column(width = 3, radioButtons(ns("view_mode"), NULL,
                                         choices = c("Reported" = "reported", "Adjusted" = "adjusted"),
                                         selected = "reported", inline = TRUE)),
          column(width = 3, checkboxInput(ns("expand_other"),
                                          label = tags$span(id = "ynow_bblab_expand_other", "Expand Other"),
                                          value = FALSE)),
          column(
            width = 3,
            downloadButton(ns("export_data"), "Export data", class = "btn-sm"),
            downloadButton(ns("export_chart"), "Export chart", class = "btn-sm")
          )
        ),
        uiOutput(ns("chart_status")),
        plotly::plotlyOutput(ns("donut"), height = "420px"),
        tags$h4(
          class = "ynow-bblab-subhead",
          id = "ynow_bblab_ch4_title",
          "Five-year mix evolution"
        ),
        tags$p(id = "ynow_bblab_ch4_help", class = "help-block",
               paste0(
                 "Revenue share by business for up to five fiscal years, using the same reporting dimension. ",
                 "Years that cannot be mapped are omitted; shares are never fabricated or filled with 0."
               )),
        uiOutput(ns("history_status")),
        plotly::plotlyOutput(ns("history"), height = "420px")
      )
    ),

    # 4 Business cards
    fluidRow(
      box(
        title = tagList(
          tags$span(class = "ynow-bblab-chapter__num", "4"),
          icon("th-large"),
          tags$span(id = "ynow_bblab_ch5_title", "Business cards")
        ),
        width = 12, status = "primary", solidHeader = TRUE,
        `data-bblab-chapter` = "4",
        uiOutput(ns("cards"))
      )
    ),

    # 5 Reconciliation
    fluidRow(
      box(
        title = tagList(
          tags$span(class = "ynow-bblab-chapter__num", "5"),
          icon("balance-scale"),
          tags$span(id = "ynow_bblab_ch6_title", "Reconciliation")
        ),
        width = 12, status = "warning", solidHeader = TRUE,
        `data-bblab-chapter` = "5",
        uiOutput(ns("recon"))
      )
    ),

    # Shared / corporate (collapsed; not a numbered chapter)
    fluidRow(
      box(
        title = tagList(icon("sitemap"), tags$span(id = "ynow_bblab_shared_title", "Shared and Corporate Items")),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        uiOutput(ns("shared"))
      )
    ),

    # 6 Sources — Notes collapsed; chapter header stays visible
    fluidRow(
      box(
        title = tagList(
          tags$span(class = "ynow-bblab-chapter__num", "6"),
          icon("book"),
          tags$span(id = "ynow_bblab_ch7_title", "Sources")
        ),
        width = 12, status = "info", solidHeader = TRUE,
        `data-bblab-chapter` = "6",
        tags$p(
          id = "ynow_bblab_sources_chrome",
          class = "help-block",
          paste0(
            "Disclosure priority: operating segments → segment notes → product/service revenue → ",
            "revenue disaggregation → MD&A → earnings → IR decks → official descriptions. ",
            "Filed / audited sources are preferred. This page is experimental and does not write into valuation."
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

    observe({
      if (is.reactive(current_ticker_rv)) {
        tk <- tryCatch(current_ticker_rv(), error = function(e) NULL)
        if (!is.null(tk) && nzchar(as.character(tk)[1]) &&
            (is.null(isolate(input$ticker)) || !nzchar(isolate(input$ticker)))) {
          tryCatch(updateTextInput(session, "ticker", value = as.character(tk)[1]), error = function(e) NULL)
        }
      }
    })

    # In-session only: Search updates reactives; do not remount the page.
    observeEvent(input$search, {
      raw <- trimws(as.character(input$ticker %||% "")[1])
      req(nzchar(raw))
      mode <- if (is.reactive(market_mode_rv)) {
        tryCatch(market_mode_rv(), error = function(e) "US")
      } else "US"
      tk <- if (exists("normalize_ticker_for_market", mode = "function")) {
        normalize_ticker_for_market(raw, mode)
      } else raw
      # Scoped notice only; never req-stop Search for valid TW/US listings.
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
        if (exists("get_summary_data", mode = "function")) {
          sum_df <- tryCatch(get_summary_data(tk), error = function(e) NULL)
        }
        if (is.data.frame(sum_df) && nrow(sum_df) > 0) {
          entity_name <- as.character(sum_df$Name[1] %||% sum_df$shortName[1] %||% tk)
          if (exists("normalize_ccy", mode = "function")) {
            q_ccy <- normalize_ccy(attr(sum_df, "currency") %||% "")
            f_ccy <- normalize_ccy(attr(sum_df, "financialCurrency") %||% "")
          }
        }
        if (exists("get_yahoo_industry", mode = "function")) {
          inf <- tryCatch(get_yahoo_industry(tk), error = function(e) NULL)
          if (!is.null(inf) && nzchar(as.character(inf$company_name %||% "")[1])) {
            entity_name <- as.character(inf$company_name)[1]
          }
        }
        lab_entity(entity_name)
        if (exists("cached_scrape_financials", mode = "function")) {
          res <- tryCatch({
            raw_fs <- cached_scrape_financials(tk)
            if (exists("normalize_all_financials", mode = "function")) normalize_all_financials(raw_fs) else raw_fs
          }, error = function(e) NULL)
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
        source_status("statements_unavailable")
        showNotification(
          ui_msg("bblab_source_unavailable"),
          type = "warning",
          duration = 8
        )
      })
    })

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
      tags$div(class = "ynow-bblab-meta__val", if (nzchar(nm)) nm else "—")
    })
    output$statement_ccy <- renderUI({
      payload <- lab_payload()
      ccy <- payload$statement_currency %||% "—"
      tagList(
        tags$label(id = "ynow_bblab_statement_ccy_label", class = "control-label",
                   ui_msg("bblab_statement_ccy_label")),
        tags$div(class = "ynow-bblab-meta__val", ccy)
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
      tags$p(class = "help-block", id = "ynow_bblab_source_status", ui_msg(key))
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
      cons <- res$consolidated %||% list()
      gm <- if (.bblab_finite(cons$revenue) && .bblab_finite(cons$gp) && cons$revenue != 0) {
        cons$gp / cons$revenue
      } else NA_real_
      kpi <- function(lab, val, unit = NULL) {
        tags$div(
          class = "ynow-bblab-kpi",
          tags$span(class = "ynow-bblab-kpi__lab", lab),
          tags$span(class = "ynow-bblab-kpi__val", val),
          if (!is.null(unit)) tags$span(class = "ynow-bblab-kpi__unit", unit) else NULL
        )
      }
      tags$div(
        class = "ynow-bblab-kpis",
        kpi(ui_msg("bblab_kpi_revenue"), .bblab_fmt_amt(cons$revenue), cons$currency %||% res$statement_currency),
        kpi(ui_msg("bblab_kpi_cor"), .bblab_fmt_amt(cons$cor), cons$currency %||% res$statement_currency),
        kpi(ui_msg("bblab_kpi_gp"), .bblab_fmt_amt(cons$gp), cons$currency %||% res$statement_currency),
        kpi(ui_msg("bblab_kpi_gm"), .bblab_fmt_pct(gm)),
        kpi(ui_msg("bblab_period_label"), cons$period %||% res$period %||% "—"),
        kpi(ui_msg("bblab_statement_ccy_label"), cons$currency %||% res$statement_currency %||% "—")
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
      fig <- plotly::plot_ly(
        labels = labels, values = pmax(values, 0), type = "pie", hole = 0.45,
        customdata = vapply(slices, function(s) s$id, character(1)),
        textinfo = "label+percent",
        hovertext = hover, hoverinfo = "text",
        marker = list(colors = col, line = list(color = "#ffffff", width = 1)),
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
      plotly::layout(fig, showlegend = TRUE, margin = list(b = 60))
    })

    output$history_status <- renderUI({
      res <- lab_result()
      if (is.null(res)) return(NULL)
      hist <- res$history
      if (isTRUE(hist$eligible) && length(hist$years) >= 2L) return(NULL)
      tags$p(
        id = "ynow_bblab_ch4_limited",
        class = "help-block",
        ui_msg("bblab_ch4_limited")
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
      if (is.null(res) || !isTRUE(hist$eligible) || length(hist$years) < 2L ||
          !length(hist$series)) {
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
      cons_rev <- .bblab_num(res$consolidated$revenue)
      cards <- lapply(res$businesses %||% list(), function(c) {
        column(width = 4, .bblab_card_html(c, loc(), identical(focus_id(), c$id), cons_rev))
      })
      extra <- list()
      if (!is.null(res$other)) extra[[length(extra) + 1L]] <- column(width = 4, .bblab_card_html(res$other, loc(), FALSE, cons_rev))
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
