# financial_narrative_module.R — Shiny UI/server for Financial Narrative Analysis
# Tab id: financial_narrative — Lite + Full free (no lock / no ynow-full-only).
# Analysis runs only when the user presses Run (no Workers AI / conversion on page load).

.fni_ui <- function(key, locale = "en") {
  if (exists("ui_str", mode = "function")) {
    return(ui_str(key, locale = locale))
  }
  as.character(key)[1]
}

financial_narrative_ui <- function(id = "fni") {
  ns <- NS(id)
  tags$div(
    class = "ynow-fni",
    tags$style(HTML(paste(
      ".ynow-fni{max-width:1100px;margin:0 auto 24px auto;padding:0 8px;}",
      ".ynow-fni__kicker{font-size:12px;letter-spacing:.04em;text-transform:uppercase;color:#666;margin:0 0 4px 0;}",
      ".ynow-fni__title{font-size:28px;font-weight:700;margin:0 0 8px 0;}",
      ".ynow-fni__deck{color:#444;margin:0 0 14px 0;line-height:1.45;}",
      ".ynow-fni__banner{padding:10px 12px;border:1px solid #e5e5e5;border-radius:6px;background:#fafafa;margin:0 0 14px 0;}",
      ".ynow-fni__banner--warn{border-color:#d9a441;background:#fff8e8;}",
      ".ynow-fni__banner--ok{border-color:#8bbf9a;background:#f3faf5;}",
      ".ynow-fni__panel{border:1px solid #e8e8e8;border-radius:6px;padding:12px 14px;margin:0 0 14px 0;background:#fff;}",
      ".ynow-fni__panel h3{margin:0 0 8px 0;font-size:16px;}",
      ".ynow-fni__row{display:flex;flex-wrap:wrap;gap:12px;align-items:flex-end;}",
      ".ynow-fni__row .shiny-input-container{margin-bottom:0;}",
      ".ynow-fni__finding{border-left:3px solid #0C5484;padding:8px 10px;margin:8px 0;background:#f7fafc;}",
      ".ynow-fni__quote{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:12px;color:#333;white-space:pre-wrap;}",
      ".ynow-fni__meta{font-size:12px;color:#666;}",
      ".ynow-fni__busy .btn{opacity:.85;}",
      sep = ""
    ))),
    tags$header(
      class = "ynow-fni__header",
      tags$p(
        id = "ynow_fni_kicker",
        class = "ynow-fni__kicker",
        "Financial Narrative Analysis"
      ),
      tags$h1(
        id = "ynow_fni_page_title",
        class = "ynow-fni__title",
        "Financial Narrative Analysis"
      ),
      tags$p(
        id = "ynow_fni_page_sub",
        class = "ynow-fni__deck",
        paste0(
          "Upload or select filings, transcripts, or decks; run grounded qualitative analysis ",
          "across six dimensions with evidence quotes. Open on Lite and Full (no membership lock) ",
          "when Cloudflare Workers AI is configured. If credentials are missing, this page shows ",
          "an offline banner and does not call the model. Does not auto-overwrite WACC, Ke, g, or price."
        )
      )
    ),
    uiOutput(ns("service_banner")),
    tags$div(
      class = "ynow-fni__panel",
      tags$h3(id = "ynow_fni_sources_title", "Sources"),
      tags$p(
        id = "ynow_fni_sources_help",
        class = "ynow-fni__meta",
        "Paste text, upload a file, and/or pull from the earnings-call catalog. Analysis starts only when you press Run."
      ),
      tags$div(
        class = "ynow-fni__row",
        textInput(ns("period"), label = tags$span(id = "ynow_fni_period_label", "Period"),
                  value = "", placeholder = "2024Q3"),
        textInput(ns("period_b"), label = tags$span(id = "ynow_fni_period_b_label", "Compare period (optional)"),
                  value = "", placeholder = "2024Q2"),
        selectInput(
          ns("doc_lang"),
          label = tags$span(id = "ynow_fni_lang_label", "Document language"),
          choices = c("en-US" = "en", "zh-TW" = "zh-TW"),
          selected = "en"
        )
      ),
      textAreaInput(
        ns("paste_text"),
        label = tags$span(id = "ynow_fni_paste_label", "Paste transcript / excerpt"),
        value = "",
        rows = 6,
        width = "100%",
        placeholder = "Paste filing, transcript, or deck text…"
      ),
      textAreaInput(
        ns("paste_text_b"),
        label = tags$span(id = "ynow_fni_paste_b_label", "Second period paste (optional)"),
        value = "",
        rows = 4,
        width = "100%"
      ),
      fileInput(
        ns("upload"),
        label = tags$span(id = "ynow_fni_upload_label", "Upload file (PDF / TXT / MD)"),
        accept = c(".pdf", ".txt", ".md", ".docx", ".pptx", ".html", ".htm"),
        multiple = FALSE
      ),
      tags$div(
        class = "ynow-fni__row",
        actionButton(
          ns("refresh_catalog"),
          label = tags$span(id = "ynow_fni_catalog_btn", "Refresh catalog"),
          class = "btn-default"
        ),
        uiOutput(ns("catalog_picker"))
      ),
      tags$div(
        class = "ynow-fni__row",
        style = "margin-top:12px;",
        actionButton(
          ns("run"),
          label = tags$span(id = "ynow_fni_run_btn", "Run analysis"),
          class = "btn-primary"
        ),
        checkboxInput(
          ns("force"),
          label = tags$span(id = "ynow_fni_force_label", "Bypass cache"),
          value = FALSE
        )
      ),
      uiOutput(ns("run_status"))
    ),
    tags$div(
      class = "ynow-fni__panel",
      tags$h3(id = "ynow_fni_summary_title", "Summary"),
      uiOutput(ns("summary_ui"))
    ),
    tags$div(
      class = "ynow-fni__panel",
      tags$h3(id = "ynow_fni_guidance_title", "Management Guidance"),
      uiOutput(ns("guidance_ui"))
    ),
    tags$div(
      class = "ynow-fni__panel",
      tags$h3(id = "ynow_fni_findings_title", "Findings by dimension"),
      uiOutput(ns("findings_ui"))
    ),
    tags$div(
      class = "ynow-fni__panel",
      tags$h3(id = "ynow_fni_compare_title", "Cross-quarter compare"),
      uiOutput(ns("compare_ui"))
    ),
    tags$div(
      class = "ynow-fni__panel",
      tags$h3(id = "ynow_fni_evidence_title", "Evidence drill-down"),
      uiOutput(ns("evidence_ui")),
      tags$div(
        class = "ynow-fni__row",
        downloadButton(ns("dl_csv"), label = NULL),
        tags$span(id = "ynow_fni_dl_csv_label", class = "ynow-fni__meta", "Download CSV"),
        downloadButton(ns("dl_json"), label = NULL),
        tags$span(id = "ynow_fni_dl_json_label", class = "ynow-fni__meta", "Download JSON")
      )
    )
  )
}

financial_narrative_server <- function(id = "fni",
                                       market_mode_rv = NULL,
                                       ui_locale_rv = NULL,
                                       current_ticker_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactive({
      raw <- tryCatch({
        if (is.reactive(ui_locale_rv)) ui_locale_rv() else ui_locale_rv
      }, error = function(e) "en")
      if (exists("normalize_ui_locale", mode = "function")) {
        normalize_ui_locale(raw)
      } else if (identical(as.character(raw)[1], "zh-TW")) {
        "zh-TW"
      } else {
        "en"
      }
    })
    ticker <- reactive({
      raw <- tryCatch({
        if (is.reactive(current_ticker_rv)) current_ticker_rv() else current_ticker_rv
      }, error = function(e) "")
      toupper(trimws(as.character(raw %||% "")[1]))
    })
    market <- reactive({
      raw <- tryCatch({
        if (is.reactive(market_mode_rv)) market_mode_rv() else market_mode_rv
      }, error = function(e) "")
      as.character(raw %||% "")[1]
    })

    catalog_rv <- reactiveVal(NULL)
    result_rv <- reactiveVal(NULL)
    busy_rv <- reactiveVal(FALSE)
    # Latest-press-wins: bump on each Run; ignore stale ExtendedTask completions.
    fni_run_gen <- reactiveVal(0L)
    fni_inflight_gen <- reactiveVal(0L)
    fni_pending <- reactiveVal(NULL)
    fni_task <- NULL
    if (exists(".ynow_make_extended_task", mode = "function") &&
        exists("ynow_fni_run_analysis_job", mode = "function") &&
        exists(".ynow_async_available", mode = "function") &&
        isTRUE(.ynow_async_available())) {
      fn <- ynow_fni_run_analysis_job
      attr(fn, "ynow_job_name") <- "ynow_fni_run_analysis_job"
      fni_task <- tryCatch(.ynow_make_extended_task(fn), error = function(e) NULL)
    }

    output$service_banner <- renderUI({
      st <- tryCatch(ynow_fni_service_status(), error = function(e) {
        list(ready = FALSE, message = "AI service not configured")
      })
      cls <- if (isTRUE(st$ready)) "ynow-fni__banner ynow-fni__banner--ok" else "ynow-fni__banner ynow-fni__banner--warn"
      title <- if (isTRUE(st$ready)) {
        .fni_ui("fni_service_ready", loc())
      } else {
        .fni_ui("fni_service_not_configured", loc())
      }
      detail <- st$message %||% ""
      if (!isTRUE(st$ready) && grepl("not configured", detail, ignore.case = TRUE)) {
        detail <- .fni_ui("fni_service_offline_detail", loc())
      }
      tags$div(
        class = cls,
        role = "status",
        tags$strong(title),
        tags$p(class = "ynow-fni__meta", style = "margin:4px 0 0 0;", detail),
        tags$p(
          class = "ynow-fni__meta",
          style = "margin:4px 0 0 0;",
          sprintf(
            "model=%s · prompt=%s · parse=%s · llm=%s",
            st$model %||% "",
            st$prompt_version %||% "",
            (st$document %||% st$docling)$status %||% "",
            (st$llm %||% st$ollama)$status %||% ""
          )
        )
      )
    })

    observeEvent(input$refresh_catalog, {
      tk <- ticker()
      if (!nzchar(tk)) {
        catalog_rv(NULL)
        return()
      }
      if (!exists("ynow_earnings_call_catalog", mode = "function")) {
        catalog_rv(NULL)
        return()
      }
      cat <- tryCatch(ynow_earnings_call_catalog(tk, max_rows = 12L), error = function(e) NULL)
      catalog_rv(cat)
    }, ignoreInit = TRUE)

    output$catalog_picker <- renderUI({
      cat <- catalog_rv()
      if (!is.list(cat) || !length(cat$rows)) {
        return(tags$p(
          class = "ynow-fni__meta",
          .fni_ui("fni_catalog_empty", loc())
        ))
      }
      choices <- if (exists("ynow_ec_catalog_choice_labels", mode = "function")) {
        tryCatch(ynow_ec_catalog_choice_labels(cat, locale = loc()), error = function(e) NULL)
      } else {
        NULL
      }
      if (is.null(choices) || !length(choices)) {
        ids <- vapply(cat$rows, function(r) as.character(r$id %||% "")[1], character(1))
        labs <- vapply(cat$rows, function(r) {
          paste(r$published %||% "", r$title %||% r$label %||% r$id %||% "", sep = " · ")
        }, character(1))
        choices <- setNames(ids, labs)
      }
      selectizeInput(
        ns("catalog_ids"),
        label = .fni_ui("fni_catalog_label", loc()),
        choices = choices,
        selected = cat$default_id %||% names(choices)[1],
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    .fni_build_sources <- function() {
      sources <- list()
      tk <- ticker()
      lang <- as.character(input$doc_lang %||% "en")[1]
      period_a <- trimws(as.character(input$period %||% "")[1])
      period_b <- trimws(as.character(input$period_b %||% "")[1])
      paste_a <- as.character(input$paste_text %||% "")[1]
      paste_b <- as.character(input$paste_text_b %||% "")[1]
      if (nzchar(trimws(paste_a))) {
        sources[[length(sources) + 1L]] <- list(
          kind = "paste",
          text = paste_a,
          ticker = tk,
          period = if (nzchar(period_a)) period_a else "P1",
          language = lang,
          filename = "paste",
          market = market()
        )
      }
      if (nzchar(trimws(paste_b))) {
        sources[[length(sources) + 1L]] <- list(
          kind = "paste",
          text = paste_b,
          ticker = tk,
          period = if (nzchar(period_b)) period_b else "P2",
          language = lang,
          filename = "paste_b",
          market = market()
        )
      }
      up <- input$upload
      if (is.data.frame(up) && nrow(up) >= 1L && nzchar(up$datapath[1])) {
        sources[[length(sources) + 1L]] <- list(
          kind = "upload",
          path = up$datapath[1],
          filename = up$name[1],
          ticker = tk,
          period = if (nzchar(period_a)) period_a else "upload",
          language = lang,
          market = market()
        )
      }
      # Catalog pull (optional add)
      ids <- input$catalog_ids
      if (length(ids) && nzchar(tk) && exists("ynow_fni_fetch_catalog_source", mode = "function")) {
        fetched <- tryCatch(
          ynow_fni_fetch_catalog_source(
            tk,
            selected_ids = ids,
            period = if (nzchar(period_a)) period_a else "catalog",
            language = lang
          ),
          error = function(e) NULL
        )
        if (is.list(fetched) && isTRUE(fetched$ok) && length(fetched$sources)) {
          sources <- c(sources, fetched$sources)
        }
      }
      sources
    }

    .fni_set_busy_label <- function(busy) {
      busy_rv(isTRUE(busy))
      label <- if (isTRUE(busy)) {
        .fni_ui("fni_run_busy", loc())
      } else {
        .fni_ui("fni_run_btn", loc())
      }
      tryCatch(
        updateActionButton(session, "run", label = label),
        error = function(e) NULL
      )
    }

    .fni_task_running <- function() {
      if (is.null(fni_task)) return(FALSE)
      st <- tryCatch(fni_task$status(), error = function(e) "")
      identical(as.character(st)[1], "running")
    }

    .fni_try_start_async <- function() {
      if (is.null(fni_task)) return(invisible(FALSE))
      pending <- isolate(fni_pending())
      if (!is.list(pending)) return(invisible(FALSE))
      if (isTRUE(.fni_task_running())) {
        # Prior job still in flight — keep pending; restart when it finishes.
        return(invisible(FALSE))
      }
      gen <- as.integer(pending$.gen %||% 0L)[1]
      tryCatch(
        {
          fni_task$invoke(
            sources_json = as.character(pending$sources_json %||% "[]")[1],
            locale = as.character(pending$locale %||% "en")[1],
            force = isTRUE(pending$force),
            key = paste0("fni-", gen)
          )
          fni_inflight_gen(gen)
          fni_pending(NULL)
          invisible(TRUE)
        },
        error = function(e) {
          # Never fall through to sync analyze while async is available.
          # If still running (re-click race), keep pending + busy for restart.
          if (isTRUE(.fni_task_running())) {
            return(invisible(FALSE))
          }
          # Idle invoke failure: surface error; do not block session with sync work.
          result_rv(list(
            ok = FALSE,
            error = "task_invoke_failed",
            message = as.character(conditionMessage(e))[1],
            results = list(),
            compare = NULL
          ))
          fni_pending(NULL)
          .fni_set_busy_label(FALSE)
          invisible(FALSE)
        }
      )
    }

    .fni_finish_async <- function(had_error = FALSE, out = NULL, err_msg = "") {
      latest <- isolate(as.integer(fni_run_gen()) %||% 0L)[1]
      inflight <- isolate(as.integer(fni_inflight_gen()) %||% 0L)[1]
      pending <- isolate(fni_pending())
      # Newer press waiting: discard this completion and start the latest payload.
      if (is.list(pending)) {
        .fni_try_start_async()
        return(invisible(FALSE))
      }
      # Stale completion from a superseded generation (no newer pending left).
      if (!identical(inflight, latest)) {
        .fni_set_busy_label(FALSE)
        return(invisible(FALSE))
      }
      if (isTRUE(had_error)) {
        result_rv(list(
          ok = FALSE,
          error = "task_error",
          message = as.character(err_msg)[1],
          results = list(),
          compare = NULL
        ))
      } else if (is.list(out)) {
        result_rv(out)
      }
      .fni_set_busy_label(FALSE)
      invisible(TRUE)
    }

    observeEvent(input$run, {
      sources <- .fni_build_sources()
      if (!length(sources)) {
        result_rv(list(
          ok = FALSE,
          error = "no_sources",
          message = .fni_ui("fni_no_sources", loc()),
          results = list(),
          compare = NULL
        ))
        return()
      }
      # Block until services (and model) are ready, unless mock mode.
      st <- tryCatch(ynow_fni_service_status(), error = function(e) {
        list(ready = FALSE, message = "AI service not configured")
      })
      mock_on <- exists(".ynow_fni_env_flag", mode = "function") &&
        isTRUE(.ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0"))
      if (!isTRUE(st$ready) && !isTRUE(mock_on)) {
        result_rv(list(
          ok = FALSE,
          error = "ai_service_not_configured",
          message = st$message %||% .fni_ui("fni_service_not_configured", loc()),
          results = list(),
          compare = NULL
        ))
        return()
      }
      force <- isTRUE(input$force)
      locale_now <- loc()
      if (!is.null(fni_task)) {
        # Latest press wins: queue newest args; never sync-fallback on re-click.
        gen <- as.integer(isolate(fni_run_gen()) %||% 0L)[1] + 1L
        fni_run_gen(gen)
        payload <- jsonlite::toJSON(sources, auto_unbox = TRUE, null = "null")
        fni_pending(list(
          sources_json = as.character(payload)[1],
          locale = locale_now,
          force = force,
          .gen = gen
        ))
        .fni_set_busy_label(TRUE)
        .fni_try_start_async()
      } else {
        .fni_set_busy_label(TRUE)
        out <- ynow_fni_analyze_and_compare(sources, locale = locale_now, force = force)
        result_rv(out)
        .fni_set_busy_label(FALSE)
      }
    }, ignoreInit = TRUE)

    if (!is.null(fni_task)) {
      observeEvent(fni_task$status(), {
        st <- fni_task$status()
        if (identical(st, "error")) {
          err <- tryCatch(fni_task$result(), error = function(e) conditionMessage(e))
          .fni_finish_async(had_error = TRUE, err_msg = as.character(err)[1])
        } else if (identical(st, "success")) {
          out <- tryCatch(fni_task$result(), error = function(e) NULL)
          .fni_finish_async(had_error = FALSE, out = out)
        }
      }, ignoreInit = TRUE)
    }

    output$run_status <- renderUI({
      if (isTRUE(busy_rv())) {
        return(tags$p(class = "ynow-fni__meta", .fni_ui("fni_run_busy", loc())))
      }
      res <- result_rv()
      if (is.null(res)) return(NULL)
      if (!isTRUE(res$ok)) {
        return(tags$p(class = "ynow-fni__meta", res$message %||% res$error %||% "Error"))
      }
      hits <- vapply(res$results %||% list(), function(r) isTRUE(r$cache_hit), logical(1))
      tags$p(
        class = "ynow-fni__meta",
        sprintf(
          "%s · cache_hit=%s",
          .fni_ui("fni_run_done", loc()),
          paste(hits, collapse = ",")
        )
      )
    })

    .fni_primary_result <- reactive({
      res <- result_rv()
      if (!is.list(res) || !length(res$results)) return(NULL)
      for (nm in names(res$results)) {
        r <- res$results[[nm]]
        if (isTRUE(r$ok) && is.list(r$result)) return(r)
      }
      NULL
    })

    output$summary_ui <- renderUI({
      primary <- .fni_primary_result()
      if (is.null(primary)) {
        return(tags$p(class = "ynow-fni__meta", .fni_ui("fni_empty_results", loc())))
      }
      tags$p(primary$result$summary %||% "")
    })

    output$guidance_ui <- renderUI({
      primary <- .fni_primary_result()
      if (is.null(primary)) return(NULL)
      finds <- Filter(function(f) {
        identical(as.character(f$dimension %||% "")[1], "management_guidance")
      }, primary$result$findings %||% list())
      if (!length(finds)) {
        return(tags$p(class = "ynow-fni__meta", .fni_ui("fni_no_guidance", loc())))
      }
      tagList(lapply(finds, function(f) {
        tags$div(
          class = "ynow-fni__finding",
          tags$p(tags$strong(f$claim %||% "")),
          tags$blockquote(class = "ynow-fni__quote", f$evidence_quote %||% "")
        )
      }))
    })

    output$findings_ui <- renderUI({
      primary <- .fni_primary_result()
      if (is.null(primary)) return(NULL)
      finds <- primary$result$findings %||% list()
      if (!length(finds)) {
        return(tags$p(class = "ynow-fni__meta", .fni_ui("fni_empty_findings", loc())))
      }
      tagList(lapply(.YNOW_FNI_DIMENSIONS, function(dim) {
        subset <- Filter(function(f) identical(f$dimension %||% "", dim), finds)
        if (!length(subset)) return(NULL)
        tags$div(
          tags$h4(ynow_fni_dimension_label(dim, loc())),
          lapply(subset, function(f) {
            tags$div(
              class = "ynow-fni__finding",
              tags$p(tags$strong(f$claim %||% ""),
                     tags$span(class = "ynow-fni__meta",
                               sprintf(" · %s · conf=%.2f", f$polarity %||% "",
                                       as.numeric(f$confidence %||% 0)))),
              tags$blockquote(class = "ynow-fni__quote", f$evidence_quote %||% ""),
              if (nzchar(as.character(f$evidence_source %||% "")[1])) {
                tags$p(class = "ynow-fni__meta", f$evidence_source)
              }
            )
          })
        )
      }))
    })

    output$compare_ui <- renderUI({
      res <- result_rv()
      cmp <- res$compare
      if (!is.list(cmp) || !isTRUE(cmp$ok)) {
        return(tags$p(class = "ynow-fni__meta", .fni_ui("fni_compare_need_two", loc())))
      }
      tagList(
        tags$p(
          class = "ynow-fni__meta",
          paste("periods:", paste(cmp$period_ids %||% character(), collapse = ", "))
        ),
        lapply(cmp$changes %||% list(), function(ch) {
          tags$div(
            class = "ynow-fni__finding",
            tags$p(
              tags$strong(ynow_fni_dimension_label(ch$dimension, loc())),
              tags$span(class = "ynow-fni__meta", paste0(" · ", ch$status %||% ""))
            ),
            if (nzchar(as.character(ch$note %||% "")[1])) {
              tags$p(class = "ynow-fni__meta", ch$note)
            }
          )
        })
      )
    })

    output$evidence_ui <- renderUI({
      primary <- .fni_primary_result()
      if (is.null(primary)) return(NULL)
      finds <- primary$result$findings %||% list()
      if (!length(finds)) return(NULL)
      tags$ol(lapply(finds, function(f) {
        tags$li(
          tags$p(tags$strong(ynow_fni_dimension_label(f$dimension, loc())), " — ", f$claim %||% ""),
          tags$blockquote(class = "ynow-fni__quote", f$evidence_quote %||% ""),
          tags$p(
            class = "ynow-fni__meta",
            paste(
              "source=", f$evidence_source %||% "",
              " locator=", f$evidence_locator %||% "(none)",
              sep = ""
            )
          )
        )
      }))
    })

    output$dl_csv <- downloadHandler(
      filename = function() {
        paste0("fni_", ticker() %||% "na", "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        primary <- .fni_primary_result()
        df <- ynow_fni_findings_to_df(primary$result$findings %||% list())
        utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    output$dl_json <- downloadHandler(
      filename = function() {
        paste0("fni_", ticker() %||% "na", "_", Sys.Date(), ".json")
      },
      content = function(file) {
        res <- result_rv()
        writeLines(
          jsonlite::toJSON(res, auto_unbox = TRUE, pretty = TRUE, null = "null", force = TRUE),
          file,
          useBytes = TRUE
        )
      }
    )
  })
}
