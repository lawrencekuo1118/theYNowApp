# ==========================================
# macro_bubble_indicators.R — 動態產業泡沫與權重集中度
#
# 獨立於 CAPM／Ke／WACC／個股估值引擎。
# 訂閱全域 market_mode 與 Macro 頁 theme_key；不寫回任何折現率輸入。
# ==========================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.MACRO_BUBBLE_GICS_SECTOR <- c(
  gics_xlk = "Information Technology",
  gics_xlv = "Health Care",
  gics_xlf = "Financials",
  gics_xle = "Energy",
  gics_xli = "Industrials",
  gics_xly = "Consumer Discretionary",
  gics_xlp = "Consumer Staples",
  gics_xlu = "Utilities",
  gics_xlb = "Materials",
  gics_xlre = "Real Estate",
  gics_xlc = "Communication Services"
)

.MACRO_BUBBLE_MAX_POOL <- 40L
.MACRO_BUBBLE_ENV <- new.env(parent = emptyenv())

macro_bubble_buffett_paths <- function(mode = "US") {
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(mode)
  } else {
    toupper(as.character(mode)[1])
  }
  rel <- if (identical(mode, "TW")) {
    file.path("data", "macro_buffett_tw.csv")
  } else {
    file.path("data", "macro_buffett_us.csv")
  }
  unique(c(rel, file.path(getwd(), rel), file.path("app_18.0", rel)))
}

#' Resolve theme_key → ticker universe for concentration/attribution.
macro_bubble_resolve_universe <- function(theme_key, mode = get_market_mode()) {
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(mode)
  } else {
    toupper(as.character(mode)[1])
  }
  key <- as.character(theme_key %||% "")[1]
  if (!nzchar(key)) {
    return(list(tickers = character(0), source = "empty", label = ""))
  }

  # Concept / TW baskets already expand via macro_theme_tickers
  if (exists("macro_theme_tickers", mode = "function") &&
      (startsWith(key, "concept_") || startsWith(key, "tw_"))) {
    tks <- macro_theme_tickers(key, mode)
    return(list(
      tickers = unique(as.character(tks)),
      source = "concept",
      label = key
    ))
  }

  # GICS ETF → S&P 500 sector constituents
  if (startsWith(key, "gics_") && key %in% names(.MACRO_BUBBLE_GICS_SECTOR)) {
    sector <- unname(.MACRO_BUBBLE_GICS_SECTOR[[key]])
    sp <- tryCatch({
      if (exists("lab_get_sp500_universe", mode = "function")) {
        lab_get_sp500_universe(FALSE)
      } else {
        NULL
      }
    }, error = function(e) NULL)
    if (is.data.frame(sp) && nrow(sp) > 0L &&
        all(c("ticker", "gics_sector") %in% names(sp))) {
      keep <- toupper(trimws(as.character(sp$gics_sector))) == toupper(sector)
      tks <- unique(as.character(sp$ticker[keep]))
      tks <- tks[nzchar(tks)]
      return(list(tickers = tks, source = "gics_sp500", label = sector))
    }
    # Fallback: single ETF ticker
    if (exists("macro_theme_tickers", mode = "function")) {
      tks <- macro_theme_tickers(key, mode)
      return(list(tickers = unique(as.character(tks)), source = "gics_etf", label = sector))
    }
  }

  if (exists("macro_theme_tickers", mode = "function")) {
    tks <- macro_theme_tickers(key, mode)
    return(list(tickers = unique(as.character(tks)), source = "theme", label = key))
  }
  list(tickers = character(0), source = "empty", label = key)
}

#' Attach market caps; keep top max_n by mcap.
macro_bubble_attach_caps <- function(tickers, max_n = .MACRO_BUBBLE_MAX_POOL) {
  tks <- unique(as.character(tickers))
  tks <- tks[nzchar(tks)]
  if (!length(tks)) {
    return(data.frame(
      ticker = character(0), market_cap = numeric(0), weight = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  caps <- rep(NA_real_, length(tks))
  names(caps) <- tks
  if (exists("lab_fetch_market_caps_usd", mode = "function")) {
    got <- tryCatch(lab_fetch_market_caps_usd(tks), error = function(e) NULL)
    if (is.numeric(got) && length(got)) {
      nm <- toupper(trimws(names(got)))
      idx <- match(toupper(trimws(tks)), nm)
      hit <- which(!is.na(idx))
      if (length(hit)) caps[hit] <- as.numeric(got[idx[hit]])
    } else if (is.data.frame(got) && all(c("ticker", "market_cap") %in% names(got))) {
      idx <- match(toupper(trimws(tks)), toupper(trimws(as.character(got$ticker))))
      hit <- which(!is.na(idx))
      if (length(hit)) caps[hit] <- suppressWarnings(as.numeric(got$market_cap[idx[hit]]))
    }
  }
  if (!any(is.finite(caps)) && exists("lab_load_universe_metrics_snapshot", mode = "function")) {
    snap <- tryCatch(lab_load_universe_metrics_snapshot(), error = function(e) NULL)
    if (is.data.frame(snap) && all(c("ticker", "market_cap") %in% names(snap))) {
      idx <- match(toupper(trimws(tks)), toupper(trimws(as.character(snap$ticker))))
      hit <- which(!is.na(idx))
      if (length(hit)) caps[hit] <- suppressWarnings(as.numeric(snap$market_cap[idx[hit]]))
    }
  }
  # Fallback: snapshot CSV direct read
  if (!any(is.finite(caps))) {
    p <- c(
      file.path("data", "universe_metrics_snapshot.csv"),
      file.path(getwd(), "data", "universe_metrics_snapshot.csv")
    )
    p <- p[file.exists(p)][1]
    if (!is.na(p) && nzchar(p)) {
      snap <- tryCatch(
        utils::read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8"),
        error = function(e) NULL
      )
      if (is.data.frame(snap) && all(c("ticker", "market_cap") %in% names(snap))) {
        idx <- match(toupper(trimws(tks)), toupper(trimws(as.character(snap$ticker))))
        hit <- which(!is.na(idx))
        if (length(hit)) caps[hit] <- suppressWarnings(as.numeric(snap$market_cap[idx[hit]]))
      }
    }
  }

  df <- data.frame(
    ticker = tks,
    market_cap = as.numeric(caps),
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$market_cap) & df$market_cap > 0, , drop = FALSE]
  if (!nrow(df)) return(df)
  df <- df[order(-df$market_cap, df$ticker), , drop = FALSE]
  max_n <- max(5L, as.integer(max_n)[1])
  df <- utils::head(df, max_n)
  tot <- sum(df$market_cap)
  df$weight <- if (is.finite(tot) && tot > 0) df$market_cap / tot else NA_real_
  df
}

#' Period total return from price history (fraction).
macro_bubble_ticker_return <- function(ticker, period = "1y") {
  period <- as.character(period %||% "1y")[1]
  df <- tryCatch({
    if (exists("fetch_price_history_df", mode = "function")) {
      fetch_price_history_df(ticker, period)
    } else {
      NULL
    }
  }, error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 2L) return(NA_real_)
  if (!all(c("Date", "Close") %in% names(df))) return(NA_real_)
  df <- df[order(df$Date), , drop = FALSE]
  px <- df$Close[is.finite(df$Close)]
  if (length(px) < 2L) return(NA_real_)
  as.numeric(tail(px, 1) / px[[1]] - 1)
}

#' Concentration + return attribution for a theme universe.
#' @return list with weights table, alerts, attribution
macro_bubble_concentration <- function(theme_key,
                                       mode = get_market_mode(),
                                       top_n = 5L,
                                       attr_period = "1y") {
  top_n <- max(1L, min(10L, as.integer(top_n)[1]))
  attr_period <- as.character(attr_period %||% "1y")[1]
  if (!attr_period %in% c("1mo", "3mo", "1y")) attr_period <- "1y"

  uni <- macro_bubble_resolve_universe(theme_key, mode)
  pool <- macro_bubble_attach_caps(uni$tickers, max_n = .MACRO_BUBBLE_MAX_POOL)
  empty <- list(
    universe = uni,
    pool = pool,
    top_n = top_n,
    top_share = NA_real_,
    top1_weight = NA_real_,
    alerts = character(0),
    attribution = list(
      period = attr_period,
      basket_ret = NA_real_,
      top_contrib = NA_real_,
      rest_contrib = NA_real_,
      top_share_of_upside = NA_real_,
      breadth_warn = FALSE
    )
  )
  if (!nrow(pool)) return(empty)

  top <- utils::head(pool, top_n)
  top_share <- sum(top$weight)
  top1 <- pool$weight[[1]]
  alerts <- character(0)
  if (is.finite(top1) && top1 >= 0.50) alerts <- c(alerts, "top1_gt_50")
  if (is.finite(top_share) && top_share >= 0.70) alerts <- c(alerts, "topn_gt_70")

  # Returns for attribution (cap-weighted)
  rets <- vapply(pool$ticker, macro_bubble_ticker_return, numeric(1), period = attr_period)
  pool$ret <- as.numeric(rets)
  ok <- is.finite(pool$weight) & is.finite(pool$ret)
  basket_ret <- if (any(ok)) sum(pool$weight[ok] * pool$ret[ok]) else NA_real_
  top_idx <- seq_len(min(top_n, nrow(pool)))
  top_ok <- ok[top_idx]
  top_contrib <- if (any(top_ok)) {
    sum(pool$weight[top_idx][top_ok] * pool$ret[top_idx][top_ok])
  } else {
    NA_real_
  }
  rest_contrib <- if (is.finite(basket_ret) && is.finite(top_contrib)) {
    basket_ret - top_contrib
  } else {
    NA_real_
  }

  breadth_warn <- FALSE
  top_share_up <- NA_real_
  if (is.finite(basket_ret) && basket_ret > 0 && is.finite(top_contrib)) {
    # Share of positive basket return coming from Top-N
    pos_top <- max(top_contrib, 0)
    top_share_up <- pos_top / basket_ret
    if (is.finite(top_share_up) && top_share_up >= 0.80) {
      breadth_warn <- TRUE
      alerts <- c(alerts, "narrow_breadth")
    }
  }

  list(
    universe = uni,
    pool = pool,
    top = top,
    top_n = top_n,
    top_share = top_share,
    top1_weight = top1,
    alerts = unique(alerts),
    attribution = list(
      period = attr_period,
      basket_ret = basket_ret,
      top_contrib = top_contrib,
      rest_contrib = rest_contrib,
      top_share_of_upside = top_share_up,
      breadth_warn = breadth_warn
    )
  )
}

macro_bubble_read_buffett_csv <- function(mode = "US") {
  paths <- macro_bubble_buffett_paths(mode)
  hit <- paths[file.exists(paths)]
  if (!length(hit)) return(NULL)
  d <- tryCatch(
    utils::read.csv(hit[[1]], stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(d) || !nrow(d)) return(NULL)
  if (!("date" %in% names(d)) || !("ratio_pct" %in% names(d))) return(NULL)
  d$date <- as.Date(d$date)
  d$ratio_pct <- suppressWarnings(as.numeric(d$ratio_pct))
  d <- d[is.finite(d$ratio_pct) & !is.na(d$date), , drop = FALSE]
  d <- d[order(d$date), , drop = FALSE]
  if (!("source" %in% names(d))) d$source <- "csv"
  d
}

#' Try World Bank CM.MKT.LCAP.GD.ZS; fall back to shipped CSV.
macro_bubble_fetch_buffett_worldbank <- function(mode = "US", timeout_sec = 20) {
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(mode)
  } else {
    toupper(as.character(mode)[1])
  }
  code <- if (identical(mode, "TW")) "TWN" else "USA"
  cache_key <- paste0("wb_", code)
  if (is.data.frame(.MACRO_BUBBLE_ENV[[cache_key]])) {
    return(.MACRO_BUBBLE_ENV[[cache_key]])
  }
  url <- sprintf(
    "https://api.worldbank.org/v2/country/%s/indicator/CM.MKT.LCAP.GD.ZS?format=json&per_page=120",
    code
  )
  raw <- tryCatch({
    if (requireNamespace("httr", quietly = TRUE)) {
      resp <- httr::GET(url, httr::timeout(timeout_sec))
      if (httr::status_code(resp) >= 400L) return(NULL)
      httr::content(resp, as = "text", encoding = "UTF-8")
    } else {
      con <- url(url, open = "rb")
      on.exit(close(con), add = TRUE)
      paste(readLines(con, warn = FALSE), collapse = "\n")
    }
  }, error = function(e) NULL)
  if (!nzchar(raw %||% "")) return(NULL)
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(parsed) || length(parsed) < 2L) return(NULL)
  rows <- parsed[[2]]
  if (!is.list(rows) || !length(rows)) return(NULL)
  out <- lapply(rows, function(r) {
    if (!is.list(r)) return(NULL)
    yr <- suppressWarnings(as.integer(r$date %||% NA))
    val <- suppressWarnings(as.numeric(r$value %||% NA))
    if (!is.finite(yr) || !is.finite(val)) return(NULL)
    data.frame(
      date = as.Date(sprintf("%d-12-31", yr)),
      ratio_pct = val,
      source = "worldbank",
      stringsAsFactors = FALSE
    )
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(NULL)
  df <- do.call(rbind, out)
  df <- df[order(df$date), , drop = FALSE]
  .MACRO_BUBBLE_ENV[[cache_key]] <- df
  df
}

macro_bubble_buffett_series <- function(mode = get_market_mode()) {
  live <- tryCatch(macro_bubble_fetch_buffett_worldbank(mode), error = function(e) NULL)
  if (is.data.frame(live) && nrow(live) >= 8L) return(live)
  csv <- macro_bubble_read_buffett_csv(mode)
  if (is.data.frame(csv) && nrow(csv) >= 1L) return(csv)
  live
}

#' Traffic light from series mean ± 0.75·sd
macro_bubble_buffett_light <- function(series) {
  if (is.null(series) || !is.data.frame(series) || nrow(series) < 3L) {
    return(list(
      level = "unknown",
      label_key = "macro_bubble_buffett_unknown",
      color = "#95a5a6",
      current = NA_real_,
      mean = NA_real_,
      sd = NA_real_,
      as_of = as.Date(NA)
    ))
  }
  x <- series$ratio_pct[is.finite(series$ratio_pct)]
  mu <- mean(x)
  sdv <- stats::sd(x)
  if (!is.finite(sdv) || sdv <= 0) sdv <- abs(mu) * 0.05 + 1
  cur_row <- series[nrow(series), , drop = FALSE]
  cur <- as.numeric(cur_row$ratio_pct[[1]])
  lo <- mu - 0.75 * sdv
  hi <- mu + 0.75 * sdv
  level <- if (!is.finite(cur)) {
    "unknown"
  } else if (cur >= hi) {
    "overvalued"
  } else if (cur <= lo) {
    "undervalued"
  } else {
    "fair"
  }
  label_key <- switch(
    level,
    overvalued = "macro_bubble_buffett_over",
    undervalued = "macro_bubble_buffett_under",
    fair = "macro_bubble_buffett_fair",
    "macro_bubble_buffett_unknown"
  )
  color <- switch(
    level,
    overvalued = "#d9534f",
    undervalued = "#00a65a",
    fair = "#f39c12",
    "#95a5a6"
  )
  list(
    level = level,
    label_key = label_key,
    color = color,
    current = cur,
    mean = mu,
    sd = sdv,
    lo = lo,
    hi = hi,
    as_of = as.Date(cur_row$date[[1]])
  )
}

#' UI body: bubble & concentration (mounted on the YNOW tab, not Macro).
#' Parent supplies chapter kicker + title; KPIs sit above charts.
macro_bubble_chapter_ui <- function(ns) {
  tags$div(
    class = "ynow-macro-bubble ynow-funnel-bubble-body",
    tags$span(id = "ynow_macro_bubble_title", style = "display:none;", "Dynamic industry bubble & weight concentration"),
    tags$span(id = "ynow_macro_bubble_sub", style = "display:none;", ""),
    tags$div(
      class = "ynow-funnel-toolbar",
      role = "group",
      `aria-label` = "YNOW bubble controls",
      fluidRow(
        column(
          width = 3,
          class = "col-xs-12 col-sm-6 col-md-3",
          selectInput(
            ns("bubble_theme_key"),
            label = tags$span(id = "ynow_macro_bubble_theme_label", "Theme"),
            choices = c("—" = ""),
            selected = ""
          )
        ),
        column(
          width = 3,
          class = "col-xs-12 col-sm-6 col-md-3",
          selectInput(
            ns("bubble_top_n"),
            label = tags$span(id = "ynow_macro_bubble_topn_label", "Top N by market cap"),
            choices = c("Top 3" = "3", "Top 5" = "5"),
            selected = "5"
          )
        ),
        column(
          width = 3,
          class = "col-xs-12 col-sm-6 col-md-3",
          selectInput(
            ns("bubble_attr_period"),
            label = tags$span(id = "ynow_macro_bubble_attr_label", "Attribution window"),
            choices = c("1M" = "1mo", "3M" = "3mo", "1Y" = "1y"),
            selected = "1y"
          )
        ),
        column(
          width = 3,
          class = "col-xs-12 col-sm-6 col-md-3",
          tags$div(
            style = "margin-top: 24px;",
            actionButton(
              ns("bubble_refresh"),
              label = tags$span(id = "ynow_macro_bubble_refresh", "Refresh"),
              icon = icon("sync"),
              class = "btn-default"
            )
          )
        )
      )
    ),
    uiOutput(ns("bubble_alert_box")),
    fluidRow(
      class = "ynow-macro-chart-row",
      column(
        width = 6,
        class = "col-xs-12 col-sm-12 col-md-6",
        tags$h4(id = "ynow_macro_bubble_conc_title", "Market-cap concentration"),
        uiOutput(ns("bubble_conc_kpi")),
        plotlyOutput(ns("bubble_conc_plot"), height = "300px") %>%
          shinycssloaders::withSpinner()
      ),
      column(
        width = 6,
        class = "col-xs-12 col-sm-12 col-md-6",
        tags$h4(id = "ynow_macro_bubble_attr_title", "Return attribution"),
        uiOutput(ns("bubble_attr_kpi")),
        plotlyOutput(ns("bubble_attr_plot"), height = "300px") %>%
          shinycssloaders::withSpinner()
      )
    ),
    tags$hr(),
    tags$h4(id = "ynow_macro_bubble_buffett_title", "Buffett Indicator (market cap / GDP)"),
    fluidRow(
      class = "ynow-macro-kpi-row ynow-funnel-kpi-row",
      column(
        width = 3,
        class = "col-xs-12 col-sm-4 col-md-3",
        uiOutput(ns("bubble_buffett_light"))
      ),
      column(
        width = 5,
        class = "col-xs-12 col-sm-8 col-md-5",
        sliderInput(
          ns("bubble_buffett_asof"),
          label = tags$span(id = "ynow_macro_bubble_asof_label", "As-of year (playback)"),
          min = 1995,
          max = as.integer(format(Sys.Date(), "%Y")),
          value = as.integer(format(Sys.Date(), "%Y")),
          step = 1,
          sep = "",
          width = "100%"
        )
      ),
      column(
        width = 4,
        class = "col-xs-12 col-sm-12 col-md-4",
        tags$div(
          style = "margin-top: 24px;",
          actionButton(
            ns("bubble_buffett_play"),
            label = tags$span(id = "ynow_macro_bubble_play", "Play"),
            icon = icon("play"),
            class = "btn-default"
          ),
          actionButton(
            ns("bubble_buffett_pause"),
            label = tags$span(id = "ynow_macro_bubble_pause", "Pause"),
            icon = icon("pause"),
            class = "btn-default"
          )
        )
      )
    ),
    plotlyOutput(ns("bubble_buffett_plot"), height = "320px") %>%
      shinycssloaders::withSpinner(),
    tags$p(
      id = "ynow_macro_bubble_buffett_note",
      class = "ynow-macro-hint",
      paste0(
        "Series: World Bank market capitalization of listed domestic companies (% of GDP) when reachable; ",
        "otherwise the bundled CSV snapshot. Traffic light uses each market’s own mean ± 0.75·sd."
      )
    )
  )
}
