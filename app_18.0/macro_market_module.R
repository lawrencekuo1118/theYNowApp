# ==========================================
# macro_market_module.R — 總體經濟與大盤趨勢
#
# 訂閱全域 market_mode（US／TW），不另建市場開關。
# 指數／板塊歷史序列維持 Yahoo 原始報價幣別，不做歷史 FX 換算。
# 本頁 Rolling β 僅供交叉檢驗，絕不可寫入 CAPM／Ke／WACC。
# ==========================================

# ---- Catalogs (no FX; Yahoo native quotes) ----

.MACRO_US_INDICES <- c(
  "^GSPC" = "S&P 500",
  "^IXIC" = "Nasdaq",
  "^DJI" = "Dow Jones",
  "^SOX" = "SOX (semis)"
)

.MACRO_TW_INDICES <- c(
  "^TWII" = "TAIEX",
  "^TWOII" = "TPEx",
  "0050.TW" = "0050"
)

# GICS-ish US sector ETFs (11 SPDR sectors)
.MACRO_US_GICS <- c(
  gics_xlk = "XLK",
  gics_xlv = "XLV",
  gics_xlf = "XLF",
  gics_xle = "XLE",
  gics_xli = "XLI",
  gics_xly = "XLY",
  gics_xlp = "XLP",
  gics_xlu = "XLU",
  gics_xlb = "XLB",
  gics_xlre = "XLRE",
  gics_xlc = "XLC"
)

.MACRO_US_GICS_LABELS_EN <- c(
  gics_xlk = "Technology (XLK)",
  gics_xlv = "Health Care (XLV)",
  gics_xlf = "Financials (XLF)",
  gics_xle = "Energy (XLE)",
  gics_xli = "Industrials (XLI)",
  gics_xly = "Consumer Discretionary (XLY)",
  gics_xlp = "Consumer Staples (XLP)",
  gics_xlu = "Utilities (XLU)",
  gics_xlb = "Materials (XLB)",
  gics_xlre = "Real Estate (XLRE)",
  gics_xlc = "Communication Services (XLC)"
)

.MACRO_US_GICS_LABELS_ZH <- c(
  gics_xlk = "科技（XLK）",
  gics_xlv = "醫療保健（XLV）",
  gics_xlf = "金融（XLF）",
  gics_xle = "能源（XLE）",
  gics_xli = "工業（XLI）",
  gics_xly = "非必需消費（XLY）",
  gics_xlp = "必需消費（XLP）",
  gics_xlu = "公用事業（XLU）",
  gics_xlb = "原物料（XLB）",
  gics_xlre = "不動產（XLRE）",
  gics_xlc = "通訊服務（XLC）"
)

macro_bench_ticker <- function(mode = get_market_mode()) {
  if (identical(normalize_market_mode(mode), "TW")) "^TWII" else "^GSPC"
}

macro_index_specs <- function(mode = get_market_mode()) {
  if (identical(normalize_market_mode(mode), "TW")) .MACRO_TW_INDICES else .MACRO_US_INDICES
}

#' Theme picker choices: GICS ETFs + concept groups for US; TW concept groups.
macro_theme_choices <- function(mode = get_market_mode(), locale = "en") {
  mode <- normalize_market_mode(mode)
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale)[1]
  }
  is_zh <- grepl("^zh", tolower(loc), perl = TRUE)

  out <- character(0)
  if (identical(mode, "US")) {
    labs <- if (is_zh) .MACRO_US_GICS_LABELS_ZH else .MACRO_US_GICS_LABELS_EN
    gics <- setNames(names(.MACRO_US_GICS), as.character(labs[names(.MACRO_US_GICS)]))
    out <- c(out, gics)
  }

  if (exists("LAB_CONCEPT_GROUPS", inherits = TRUE) &&
      !is.null(LAB_CONCEPT_GROUPS[[mode]])) {
    keys <- names(LAB_CONCEPT_GROUPS[[mode]])
    for (k in keys) {
      lab <- if (exists("lab_concept_group_label", mode = "function")) {
        lab_concept_group_label(k, market = mode, locale = loc)
      } else {
        k
      }
      prefix <- if (identical(mode, "US")) "concept_" else "tw_"
      out <- c(out, setNames(paste0(prefix, k), lab))
    }
  }
  out
}

#' Resolve theme key → Yahoo tickers (equal-weight basket or single ETF).
macro_theme_tickers <- function(theme_key, mode = get_market_mode()) {
  mode <- normalize_market_mode(mode)
  key <- as.character(theme_key %||% "")[1]
  if (!nzchar(key)) return(character(0))

  if (startsWith(key, "gics_") && key %in% names(.MACRO_US_GICS)) {
    return(as.character(.MACRO_US_GICS[[key]]))
  }
  cg_key <- sub("^(concept_|tw_)", "", key)
  if (exists("LAB_CONCEPT_GROUPS", inherits = TRUE) &&
      !is.null(LAB_CONCEPT_GROUPS[[mode]]) &&
      cg_key %in% names(LAB_CONCEPT_GROUPS[[mode]])) {
    return(as.character(LAB_CONCEPT_GROUPS[[mode]][[cg_key]]))
  }
  character(0)
}

#' Equal-weight rebased series (start = 100). No FX conversion.
macro_equal_weight_rebased <- function(tickers, period = "1y", max_n = 10L) {
  tickers <- unique(as.character(tickers))
  tickers <- tickers[nzchar(tickers)]
  if (!length(tickers)) return(NULL)
  tickers <- head(tickers, as.integer(max_n))

  series_list <- list()
  for (tk in tickers) {
    df <- tryCatch(fetch_price_history_df(tk, period), error = function(e) NULL)
    if (is.null(df) || nrow(df) < 20L) next
    df <- df[order(df$Date), , drop = FALSE]
    base <- df$Close[which(is.finite(df$Close))[1]]
    if (!is.finite(base) || base <= 0) next
    df$rebased <- 100 * df$Close / base
    series_list[[tk]] <- df[, c("Date", "rebased")]
  }
  if (!length(series_list)) return(NULL)

  # Outer join on Date, rowMeans of available rebased legs
  all_dates <- sort(unique(do.call(c, lapply(series_list, function(x) x$Date))))
  mat <- matrix(NA_real_, nrow = length(all_dates), ncol = length(series_list))
  colnames(mat) <- names(series_list)
  for (i in seq_along(series_list)) {
    s <- series_list[[i]]
    idx <- match(s$Date, all_dates)
    mat[idx, i] <- s$rebased
  }
  avg <- apply(mat, 1L, function(r) {
    r <- r[is.finite(r)]
    if (!length(r)) return(NA_real_)
    mean(r)
  })
  out <- data.frame(Date = all_dates, Theme = as.numeric(avg), stringsAsFactors = FALSE)
  out <- out[is.finite(out$Theme), , drop = FALSE]
  if (nrow(out) < 10L) return(NULL)
  out
}

#' Rolling β path (month-end) for theme vs bench. Display-only; never writes CAPM.
macro_rolling_beta_path <- function(theme_df, bench_df, lookback_months = 36L) {
  if (is.null(theme_df) || is.null(bench_df)) return(NULL)
  if (!all(c("Date", "Close") %in% names(theme_df))) {
    if ("Theme" %in% names(theme_df)) {
      theme_df <- data.frame(Date = theme_df$Date, Close = theme_df$Theme)
    } else {
      return(NULL)
    }
  }
  if (!all(c("Date", "Close") %in% names(bench_df))) return(NULL)

  merged <- merge(
    theme_df[, c("Date", "Close")],
    bench_df[, c("Date", "Close")],
    by = "Date",
    suffixes = c("_s", "_m")
  )
  names(merged) <- c("Date", "S", "M")
  merged <- merged[is.finite(merged$S) & is.finite(merged$M), , drop = FALSE]
  if (nrow(merged) < 60L) return(NULL)
  merged <- merged[order(merged$Date), , drop = FALSE]

  ym <- format(merged$Date, "%Y-%m")
  ends <- merged[!duplicated(ym, fromLast = TRUE), , drop = FALSE]
  if (nrow(ends) < 18L) return(NULL)

  betas <- rep(NA_real_, nrow(ends))
  for (i in seq_len(nrow(ends))) {
    as_of <- ends$Date[i]
    betas[i] <- estimate_rolling_beta(
      merged$S, merged$M, merged$Date, as_of,
      lookback_months = lookback_months, min_obs = 18L
    )
  }
  data.frame(Date = ends$Date, Beta = betas, stringsAsFactors = FALSE)
}

# ---- UI ----

macro_market_ui <- function(id = "macro") {
  ns <- NS(id)
  tags$div(
    class = "ynow-macro-report",
    tags$div(
      class = "ynow-macro-report__masthead",
      h2(tags$b(id = "ynow_macro_page_title", "Macro & Market Trends")),
      p(
        id = "ynow_macro_page_sub",
        class = "ynow-macro-report__lead",
        paste0(
          "Follows the global US / TW market toggle. Index and theme price series stay in Yahoo’s native quote currency—",
          "no historical FX conversion. Rolling β here is a cross-check only and never feeds CAPM / Ke / WACC."
        )
      )
    ),
    tags$div(
      class = "ynow-macro-callout ynow-macro-callout--mode",
      tags$span(id = "ynow_macro_mode_label", class = "ynow-macro-kicker", "Market mode"),
      uiOutput(ns("mode_badge"))
    ),
    fluidRow(
      column(width = 12, uiOutput(ns("index_kpi_row")))
    ),
    fluidRow(
      column(
        width = 4,
        tags$div(
          class = "ynow-macro-card",
          tags$h4(id = "ynow_macro_rf_title", "Risk-free rate Rf (10Y)"),
          uiOutput(ns("rf_box")),
          tags$p(
            id = "ynow_macro_rf_note",
            class = "ynow-macro-hint",
            "Same live Rf path as CAPM (US: Yahoo ^TNX; TW: TPEx 10Y curve). Display only on this page."
          )
        )
      ),
      column(
        width = 8,
        tags$div(
          class = "ynow-macro-card",
          tags$h4(id = "ynow_macro_tw_signal_title", "TW business-cycle signal"),
          uiOutput(ns("tw_signal_box"))
        )
      )
    ),
    tags$section(
      class = "ynow-macro-chapter",
      tags$h3(id = "ynow_macro_theme_title", "Industry / concept vs benchmark"),
      tags$p(
        id = "ynow_macro_theme_help",
        class = "ynow-macro-hint",
        "Pick a GICS sector ETF (US) or a concept basket. Benchmark is gray dashed on the right axis (rebased = 100 at window start; native currency, no FX)."
      ),
      fluidRow(
        column(
          width = 5,
          selectInput(
            ns("theme_key"),
            label = tags$span(id = "ynow_macro_theme_label", "Theme"),
            choices = c("—" = ""),
            selected = ""
          )
        ),
        column(
          width = 3,
          selectInput(
            ns("hist_period"),
            label = tags$span(id = "ynow_macro_period_label", "Window"),
            choices = c("6M" = "6mo", "1Y" = "1y", "3Y" = "3y", "5Y" = "5y"),
            selected = "1y"
          )
        ),
        column(
          width = 4,
          tags$div(
            style = "margin-top: 24px;",
            actionButton(
              ns("refresh"),
              "Refresh",
              icon = icon("sync"),
              class = "btn-default"
            )
          )
        )
      ),
      plotlyOutput(ns("overlay_plot"), height = "380px") %>% shinycssloaders::withSpinner(),
      tags$p(
        id = "ynow_macro_fx_lock",
        class = "ynow-macro-hint",
        "Currency lock: historical index / theme series are never converted by the session USD⇄TWD toggle."
      )
    ),
    tags$section(
      class = "ynow-macro-chapter",
      tags$h3(id = "ynow_macro_beta_title", "Theme Rolling β vs benchmark"),
      tags$div(
        class = "ynow-macro-callout ynow-macro-callout--warn",
        tags$b(id = "ynow_macro_beta_warn_title", "Cross-check only — not a CAPM input"),
        tags$p(
          id = "ynow_macro_beta_warn_body",
          paste0(
            "Rolling β embeds market sentiment and event noise. Use it to sanity-check sensitivity; ",
            "do not paste it into DCF / RI discount rates. Core Ke / WACC keep Bottom-Up or manual β."
          )
        )
      ),
      fluidRow(
        column(width = 3, uiOutput(ns("beta_kpi"))),
        column(width = 9, plotlyOutput(ns("beta_plot"), height = "280px") %>% shinycssloaders::withSpinner())
      )
    )
  )
}

# ---- Server ----

#' @param market_mode_rv reactiveVal / reactive returning "US" | "TW"
#' @param ui_locale_rv reactive returning locale id
macro_market_server <- function(id = "macro",
                                market_mode_rv,
                                ui_locale_rv = reactive("en")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a)) b else a

    .loc <- function() {
      tryCatch(normalize_ui_locale(ui_locale_rv()), error = function(e) "en")
    }
    .mode <- function() {
      tryCatch(normalize_market_mode(market_mode_rv()), error = function(e) "US")
    }
    .ui <- function(key, ...) {
      if (exists("ui_str", mode = "function")) {
        tryCatch(ui_str(key, .loc(), ...), error = function(e) key)
      } else {
        key
      }
    }

    refresh_token <- reactiveVal(0L)
    observeEvent(input$refresh, {
      refresh_token(isolate(refresh_token()) + 1L)
    }, ignoreInit = TRUE)

    # Theme menu follows market + locale
    observe({
      mode <- .mode()
      loc <- .loc()
      ch <- macro_theme_choices(mode, loc)
      if (!length(ch)) {
        updateSelectInput(session, "theme_key", choices = c("—" = ""), selected = "")
        return()
      }
      sel <- isolate(input$theme_key)
      if (is.null(sel) || !nzchar(sel) || !(sel %in% unname(ch))) {
        sel <- unname(ch)[1]
      }
      updateSelectInput(session, "theme_key", choices = ch, selected = sel)
    })

    output$mode_badge <- renderUI({
      mode <- .mode()
      lab <- if (identical(mode, "TW")) .ui("macro_mode_tw") else .ui("macro_mode_us")
      tags$span(
        class = paste0("ynow-macro-badge ynow-macro-badge--", tolower(mode)),
        lab
      )
    })

    index_quotes <- reactive({
      refresh_token()
      mode <- .mode()
      specs <- macro_index_specs(mode)
      rows <- lapply(names(specs), function(sym) {
        df <- tryCatch(fetch_price_history_df(sym, "5d"), error = function(e) NULL)
        last <- if (!is.null(df) && nrow(df)) {
          tail(df$Close[is.finite(df$Close)], 1)
        } else {
          NA_real_
        }
        chg <- NA_real_
        if (!is.null(df) && sum(is.finite(df$Close)) >= 2L) {
          cc <- df$Close[is.finite(df$Close)]
          chg <- 100 * (tail(cc, 1) / cc[length(cc) - 1L] - 1)
        }
        list(symbol = sym, label = unname(specs[[sym]]), last = last, chg_pct = chg)
      })
      rows
    })

    output$index_kpi_row <- renderUI({
      rows <- index_quotes()
      cols <- lapply(rows, function(r) {
        last_txt <- if (is.finite(r$last)) format(round(r$last, 2), big.mark = ",") else "—"
        chg_txt <- if (is.finite(r$chg_pct)) sprintf("%+.2f%%", r$chg_pct) else "—"
        col_cls <- if (is.finite(r$chg_pct) && r$chg_pct >= 0) "ynow-macro-up" else "ynow-macro-down"
        column(
          width = 3,
          tags$div(
            class = "ynow-macro-kpi",
            tags$div(class = "ynow-macro-kpi__label", r$label),
            tags$div(class = "ynow-macro-kpi__value", last_txt),
            tags$div(class = paste("ynow-macro-kpi__chg", col_cls), chg_txt),
            tags$div(class = "ynow-macro-kpi__sym", r$symbol)
          )
        )
      })
      do.call(fluidRow, cols)
    })

    output$rf_box <- renderUI({
      refresh_token()
      mode <- .mode()
      det <- tryCatch({
        if (exists("cached_get_risk_free_rate_detail", mode = "function")) {
          cached_get_risk_free_rate_detail(mode)
        } else {
          list(rf_pct = cached_get_risk_free_rate(mode), source = "")
        }
      }, error = function(e) list(rf_pct = NA_real_, source = e$message))
      rf <- suppressWarnings(as.numeric(det$rf_pct %||% NA_real_)[1])
      src <- as.character(det$source %||% "")[1]
      lab <- as.character(det$label %||% "")[1]
      src_lab <- switch(
        src,
        live = .ui("macro_rf_src_live"),
        last_known = .ui("macro_rf_src_last"),
        fallback = .ui("macro_rf_src_fallback"),
        .ui("macro_rf_source_fallback")
      )
      tags$div(
        class = "ynow-macro-rf",
        tags$div(
          class = "ynow-macro-rf__value",
          if (is.finite(rf)) sprintf("%.2f%%", rf) else "—"
        ),
        tags$div(
          class = "ynow-macro-hint",
          paste(lab, "·", src_lab)
        )
      )
    })

    output$tw_signal_box <- renderUI({
      mode <- .mode()
      if (!identical(mode, "TW")) {
        return(tags$p(
          class = "ynow-macro-hint",
          id = "ynow_macro_tw_signal_us_note",
          .ui("macro_tw_signal_us_only")
        ))
      }
      tags$div(
        class = "ynow-macro-hint",
        tags$p(.ui("macro_tw_signal_body")),
        tags$p(
          tags$a(
            href = "https://index.ndc.gov.tw/",
            target = "_blank",
            rel = "noopener noreferrer",
            .ui("macro_tw_signal_link")
          )
        )
      )
    })

    overlay_data <- reactive({
      refresh_token()
      mode <- .mode()
      period <- as.character(input$hist_period %||% "1y")[1]
      theme_key <- as.character(input$theme_key %||% "")[1]
      req(nzchar(theme_key))

      tickers <- macro_theme_tickers(theme_key, mode)
      theme <- NULL
      if (length(tickers) == 1L) {
        raw <- tryCatch(fetch_price_history_df(tickers[[1]], period), error = function(e) NULL)
        if (!is.null(raw) && nrow(raw) >= 20L) {
          raw <- raw[order(raw$Date), , drop = FALSE]
          base <- raw$Close[which(is.finite(raw$Close))[1]]
          if (is.finite(base) && base > 0) {
            theme <- data.frame(
              Date = raw$Date,
              Theme = 100 * raw$Close / base,
              Close = raw$Close,
              stringsAsFactors = FALSE
            )
          }
        }
      } else {
        ew <- macro_equal_weight_rebased(tickers, period = period, max_n = 10L)
        if (!is.null(ew)) {
          theme <- data.frame(
            Date = ew$Date, Theme = ew$Theme, Close = ew$Theme,
            stringsAsFactors = FALSE
          )
        }
      }

      bench_tk <- macro_bench_ticker(mode)
      bench_raw <- tryCatch(fetch_price_history_df(bench_tk, period), error = function(e) NULL)
      bench <- NULL
      if (!is.null(bench_raw) && nrow(bench_raw) >= 20L) {
        bench_raw <- bench_raw[order(bench_raw$Date), , drop = FALSE]
        base_b <- bench_raw$Close[which(is.finite(bench_raw$Close))[1]]
        if (is.finite(base_b) && base_b > 0) {
          bench <- data.frame(
            Date = bench_raw$Date,
            Bench = 100 * bench_raw$Close / base_b,
            Close = bench_raw$Close,
            stringsAsFactors = FALSE
          )
        }
      }
      list(
        theme = theme,
        bench = bench,
        bench_ticker = bench_tk,
        theme_key = theme_key,
        tickers = tickers
      )
    })

    output$overlay_plot <- plotly::renderPlotly({
      od <- overlay_data()
      shiny::validate(shiny::need(!is.null(od$theme), .ui("macro_plot_need_theme")))
      shiny::validate(shiny::need(!is.null(od$bench), .ui("macro_plot_need_bench")))

      th <- od$theme
      bh <- od$bench
      # Align dates for dual-axis readability
      common <- intersect(th$Date, bh$Date)
      th <- th[th$Date %in% common, , drop = FALSE]
      bh <- bh[bh$Date %in% common, , drop = FALSE]

      theme_lab <- .ui("macro_series_theme")
      bench_lab <- paste0(.ui("macro_series_bench"), " (", od$bench_ticker, ")")

      fig <- plotly::plot_ly()
      fig <- plotly::add_trace(
        fig,
        x = th$Date, y = th$Theme,
        type = "scatter", mode = "lines",
        name = theme_lab,
        line = list(color = "#e67e22", width = 2),
        yaxis = "y"
      )
      fig <- plotly::add_trace(
        fig,
        x = bh$Date, y = bh$Bench,
        type = "scatter", mode = "lines",
        name = bench_lab,
        line = list(color = "#888888", width = 1.5, dash = "dash"),
        yaxis = "y2"
      )
      plotly::layout(
        fig,
        title = list(text = .ui("macro_overlay_title"), font = list(size = 14)),
        xaxis = list(title = ""),
        yaxis = list(title = theme_lab, side = "left", showgrid = TRUE),
        yaxis2 = list(
          title = bench_lab,
          overlaying = "y",
          side = "right",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        legend = list(orientation = "h", y = 1.12),
        margin = list(l = 50, r = 60, t = 50, b = 40),
        hovermode = "x unified"
      )
    })

    beta_path <- reactive({
      od <- overlay_data()
      req(!is.null(od$theme), !is.null(od$bench))
      th <- data.frame(Date = od$theme$Date, Close = od$theme$Close)
      bh <- data.frame(Date = od$bench$Date, Close = od$bench$Close)
      macro_rolling_beta_path(th, bh, lookback_months = 36L)
    })

    output$beta_kpi <- renderUI({
      bp <- beta_path()
      last_b <- if (!is.null(bp) && nrow(bp)) {
        tail(bp$Beta[is.finite(bp$Beta)], 1)
      } else {
        NA_real_
      }
      tags$div(
        class = "ynow-macro-kpi ynow-macro-kpi--beta",
        tags$div(class = "ynow-macro-kpi__label", .ui("macro_beta_latest")),
        tags$div(
          class = "ynow-macro-kpi__value",
          if (is.finite(last_b)) sprintf("%.2f", last_b) else "—"
        ),
        tags$div(
          class = "ynow-macro-hint",
          .ui("macro_beta_kpi_hint")
        )
      )
    })

    output$beta_plot <- plotly::renderPlotly({
      bp <- beta_path()
      shiny::validate(shiny::need(!is.null(bp) && nrow(bp) > 2L, .ui("macro_beta_need_data")))
      fig <- plotly::plot_ly(
        bp, x = ~Date, y = ~Beta,
        type = "scatter", mode = "lines",
        name = "Rolling β",
        line = list(color = "#2980b9", width = 2)
      )
      plotly::layout(
        fig,
        title = list(text = .ui("macro_beta_chart_title"), font = list(size = 13)),
        xaxis = list(title = ""),
        yaxis = list(title = "β", zeroline = TRUE),
        margin = list(l = 50, r = 20, t = 40, b = 40),
        showlegend = FALSE
      )
    })
  })
}
