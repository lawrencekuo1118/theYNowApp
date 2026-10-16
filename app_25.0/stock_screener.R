# Custom screen on fields already stored for the US / Taiwan universes.
# Results can be written into the existing value-alert store (price_alerts.json)
# with screen_ref set, so the alert list can show which screen created the rule.

.SCR_N_COND <- 4L

screener_field_spec <- function() {
  data.frame(
    id = c(
      "market_cap_bn", "ret_1y_pct", "roe", "op_margin", "rev_yoy",
      "opinc_yoy", "debt_ratio", "pe", "pb"
    ),
    stringsAsFactors = FALSE
  )
}

screener_field_ids <- function() {
  screener_field_spec()$id
}

screener_field_choices <- function(locale = "en") {
  ids <- screener_field_ids()
  labs <- vapply(ids, function(id) {
    key <- paste0("scr_field_", id)
    if (exists("ui_str", mode = "function")) ui_str(key, locale) else id
  }, character(1))
  stats::setNames(ids, labs)
}

screener_empty_pool <- function() {
  data.frame(
    ticker = character(0),
    name = character(0),
    market_cap_bn = numeric(0),
    ret_1y_pct = numeric(0),
    roe = numeric(0),
    op_margin = numeric(0),
    rev_yoy = numeric(0),
    opinc_yoy = numeric(0),
    debt_ratio = numeric(0),
    pe = numeric(0),
    pb = numeric(0),
    stringsAsFactors = FALSE
  )
}

.screener_pull <- function(df, idx, col) {
  out <- rep(NA_real_, length(idx))
  if (is.null(df) || !is.data.frame(df) || !nrow(df) || !col %in% names(df)) return(out)
  v <- suppressWarnings(as.numeric(df[[col]]))
  ok <- !is.na(idx) & idx >= 1L & idx <= length(v)
  out[ok] <- v[idx[ok]]
  out
}

#' Join universe membership with snapshot metrics. Values are scaled for the form
#' (market cap in USD billions, 1-year return in percent).
screener_assemble <- function(universe, features = NULL, metrics = NULL, market = "US") {
  market <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(market)
  } else if (identical(toupper(trimws(as.character(market)[1])), "TW")) {
    "TW"
  } else {
    "US"
  }
  empty <- screener_empty_pool()
  if (is.null(universe) || !is.data.frame(universe) || !nrow(universe) || !"ticker" %in% names(universe)) {
    return(empty)
  }
  ticker <- toupper(trimws(as.character(universe$ticker)))
  name <- if ("name" %in% names(universe)) as.character(universe$name) else ticker
  name[is.na(name) | !nzchar(name)] <- ticker[is.na(name) | !nzchar(name)]
  is_tw <- grepl("\\.(TW|TWO)$", ticker)
  keep <- if (identical(market, "TW")) is_tw else !is_tw
  ticker <- ticker[keep]
  name <- name[keep]
  ok_tk <- nzchar(ticker) & !is.na(ticker)
  ticker <- ticker[ok_tk]
  name <- name[ok_tk]
  if (!length(ticker)) return(empty)
  dup <- duplicated(ticker)
  ticker <- ticker[!dup]
  name <- name[!dup]
  feat <- features
  met <- metrics
  if (is.data.frame(feat) && nrow(feat) && "ticker" %in% names(feat)) {
    feat$ticker <- toupper(trimws(as.character(feat$ticker)))
  }
  if (is.data.frame(met) && nrow(met) && "ticker" %in% names(met)) {
    met$ticker <- toupper(trimws(as.character(met$ticker)))
  }
  fi <- if (is.data.frame(feat) && nrow(feat) && "ticker" %in% names(feat)) {
    match(ticker, feat$ticker)
  } else {
    rep(NA_integer_, length(ticker))
  }
  mi <- if (is.data.frame(met) && nrow(met) && "ticker" %in% names(met)) {
    match(ticker, met$ticker)
  } else {
    rep(NA_integer_, length(ticker))
  }
  mcap <- .screener_pull(feat, fi, "market_cap")
  mcap_m <- .screener_pull(met, mi, "market_cap")
  mcap[!is.finite(mcap)] <- mcap_m[!is.finite(mcap)]
  data.frame(
    ticker = ticker,
    name = name,
    market_cap_bn = mcap * 1e-9,
    ret_1y_pct = .screener_pull(met, mi, "ret_1y") * 100,
    roe = .screener_pull(feat, fi, "ROE"),
    op_margin = .screener_pull(feat, fi, "Operating_Margin"),
    rev_yoy = .screener_pull(feat, fi, "Rev_YoY"),
    opinc_yoy = .screener_pull(feat, fi, "OpInc_YoY"),
    debt_ratio = .screener_pull(feat, fi, "Debt_Ratio"),
    pe = .screener_pull(feat, fi, "PE_Ratio"),
    pb = .screener_pull(feat, fi, "PB_Ratio"),
    stringsAsFactors = FALSE
  )
}

screener_active_conditions <- function(conditions) {
  blank <- data.frame(
    field = character(0), op = character(0), value = numeric(0),
    stringsAsFactors = FALSE
  )
  if (is.null(conditions) || !is.data.frame(conditions) || !nrow(conditions)) return(blank)
  use <- if ("use" %in% names(conditions)) as.logical(conditions$use) else rep(TRUE, nrow(conditions))
  use[is.na(use)] <- FALSE
  sub <- conditions[use, , drop = FALSE]
  if (!nrow(sub)) return(blank)
  data.frame(
    field = as.character(sub$field),
    op = as.character(sub$op),
    value = suppressWarnings(as.numeric(sub$value)),
    stringsAsFactors = FALSE
  )
}

#' AND-filter. A missing field value never passes.
screener_apply <- function(pool, conditions) {
  if (is.null(pool) || !is.data.frame(pool)) pool <- screener_empty_pool()
  empty <- pool[0, , drop = FALSE]
  conds <- screener_active_conditions(conditions)
  if (!nrow(conds)) {
    return(list(ok = FALSE, reason = "no_conditions", df = empty, conditions = conds))
  }
  if (any(!conds$field %in% screener_field_ids()) || any(!conds$field %in% names(pool))) {
    return(list(ok = FALSE, reason = "bad_field", df = empty, conditions = conds))
  }
  if (any(!conds$op %in% c("gte", "lte"))) {
    return(list(ok = FALSE, reason = "bad_field", df = empty, conditions = conds))
  }
  if (any(!is.finite(conds$value))) {
    return(list(ok = FALSE, reason = "bad_value", df = empty, conditions = conds))
  }
  keep <- rep(TRUE, nrow(pool))
  for (i in seq_len(nrow(conds))) {
    x <- suppressWarnings(as.numeric(pool[[conds$field[[i]]]]))
    v <- conds$value[[i]]
    hit <- if (identical(conds$op[[i]], "gte")) {
      is.finite(x) & x >= v
    } else {
      is.finite(x) & x <= v
    }
    keep <- keep & hit
  }
  list(ok = TRUE, reason = "ok", df = pool[keep, , drop = FALSE], conditions = conds)
}

screener_condition_label <- function(conditions, locale = "en") {
  conds <- screener_active_conditions(conditions)
  if (!nrow(conds)) return("")
  parts <- vapply(seq_len(nrow(conds)), function(i) {
    key <- paste0("scr_field_", conds$field[[i]])
    lab <- if (exists("ui_str", mode = "function")) ui_str(key, locale) else conds$field[[i]]
    op <- if (identical(conds$op[[i]], "gte")) "≥" else "≤"
    paste0(lab, " ", op, " ", format(conds$value[[i]], trim = TRUE, scientific = FALSE))
  }, character(1))
  paste(parts, collapse = "; ")
}

screener_make_ref <- function(label) {
  label <- gsub("[|\\r\\n]+", " ", as.character(label %||% "")[1])
  label <- trimws(label)
  if (!nzchar(label)) label <- "screen"
  if (nchar(label) > 180L) label <- paste0(substr(label, 1L, 177L), "...")
  paste0(
    "scr_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
    sprintf("%04d", sample.int(9999L, 1L)), "|", label
  )
}

screener_mark_alerts <- function(df, alerts) {
  if (is.null(df) || !is.data.frame(df) || !"ticker" %in% names(df)) return(df)
  tks <- character(0)
  if (is.data.frame(alerts) && nrow(alerts) && "ticker" %in% names(alerts)) {
    tks <- unique(toupper(trimws(as.character(alerts$ticker))))
    tks <- tks[nzchar(tks)]
  }
  df$has_alert <- toupper(trimws(as.character(df$ticker))) %in% tks
  df
}

screener_alerts_for_ref <- function(alerts, screen_ref) {
  blank <- if (exists("price_alert_empty", mode = "function")) price_alert_empty() else alerts[0, , drop = FALSE]
  if (!is.data.frame(alerts) || !nrow(alerts) || !"screen_ref" %in% names(alerts)) return(blank)
  ref <- as.character(screen_ref)[1]
  alerts[!is.na(alerts$screen_ref) & alerts$screen_ref == ref, , drop = FALSE]
}

screener_load_pool <- function(market = "US", app_dir = getwd()) {
  market <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(market)
  } else {
    "US"
  }
  uni_file <- if (identical(market, "TW")) "tw_universe.csv" else "us_universe.csv"
  read_csv <- function(path) {
    if (!file.exists(path)) return(NULL)
    utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = TRUE)
  }
  data_dir <- file.path(app_dir, "data")
  screener_assemble(
    read_csv(file.path(data_dir, uni_file)),
    read_csv(file.path(data_dir, "cluster_features_snapshot.csv")),
    read_csv(file.path(data_dir, "universe_metrics_snapshot.csv")),
    market = market
  )
}

.screener_bi <- function(key) {
  if (exists(".pa_bi", mode = "function")) return(.pa_bi(key))
  key
}

screener_panel_ui <- function() {
  defaults <- data.frame(
    field = c("roe", "pe", "debt_ratio", "pb"),
    op = c("gte", "lte", "lte", "lte"),
    value = c(15, 20, 60, 3),
    use = c(TRUE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(.SCR_N_COND), function(i) {
    fluidRow(
      column(2, checkboxInput(
        paste0("scr_use_", i), .screener_bi("scr_use"), value = isTRUE(defaults$use[[i]])
      )),
      column(4, selectInput(
        paste0("scr_field_", i), .screener_bi("scr_field"),
        choices = screener_field_choices("en"),
        selected = defaults$field[[i]], width = "100%"
      )),
      column(2, selectInput(
        paste0("scr_op_", i), .screener_bi("scr_op"),
        choices = c("≥" = "gte", "≤" = "lte"),
        selected = defaults$op[[i]], width = "100%"
      )),
      column(4, numericInput(
        paste0("scr_value_", i), .screener_bi("scr_value"),
        value = defaults$value[[i]], width = "100%"
      ))
    )
  })
  fluidRow(
    box(
      title = .screener_bi("scr_title"),
      id = "ynow_screener_box",
      width = 12,
      status = "warning",
      solidHeader = TRUE,
      tags$p(.screener_bi("scr_help")),
      fluidRow(
        column(4, selectInput(
          "scr_market", .screener_bi("scr_market"),
          choices = c("US" = "US", "Taiwan" = "TW"),
          selected = "US", width = "100%"
        ))
      ),
      do.call(tagList, rows),
      actionButton("scr_run", .screener_bi("scr_run"), class = "btn-warning"),
      tags$div(style = "margin-top:12px;", DT::DTOutput("scr_table")),
      fluidRow(
        style = "margin-top:8px;",
        column(4, selectInput(
          "scr_alert_kind", .screener_bi("scr_alert_kind"),
          choices = c(
            "Last price ≤ target" = "price_lte",
            "Last price ≥ target" = "price_gte"
          ),
          selected = "price_lte", width = "100%"
        )),
        column(3, numericInput(
          "scr_alert_level", .screener_bi("scr_alert_level"), value = NA, width = "100%"
        )),
        column(5, tags$div(
          style = "margin-top:25px;",
          actionButton("scr_add", .screener_bi("scr_add"), class = "btn-primary")
        ))
      )
    )
  )
}

screener_server <- function(input, output, session, ui_locale_rv = NULL,
                            say = NULL, loc = NULL, pa_refresh = NULL) {
  locale_now <- function() {
    if (!is.null(ui_locale_rv)) {
      hit <- tryCatch(normalize_ui_locale(ui_locale_rv()), error = function(e) NULL)
      if (!is.null(hit)) return(hit)
    }
    if (is.function(loc)) {
      hit <- tryCatch(loc(), error = function(e) NULL)
      if (!is.null(hit)) return(hit)
    }
    "en"
  }
  say_now <- function(key) {
    if (exists("ui_str", mode = "function")) return(ui_str(key, locale_now()))
    if (is.function(say)) return(say(key))
    key
  }
  if (is.null(pa_refresh)) pa_refresh <- reactiveVal(0L)
  scr_result <- reactiveVal(screener_empty_pool())
  scr_meta <- reactiveVal(list(ok = FALSE, reason = "idle", label = "", fields = character(0)))

  .conds_from_input <- function() {
    rows <- lapply(seq_len(.SCR_N_COND), function(i) {
      data.frame(
        use = isTRUE(input[[paste0("scr_use_", i)]]),
        field = as.character(input[[paste0("scr_field_", i)]] %||% "")[1],
        op = as.character(input[[paste0("scr_op_", i)]] %||% "")[1],
        value = suppressWarnings(as.numeric(input[[paste0("scr_value_", i)]])[1]),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }

  observe({
    locale <- if (is.null(ui_locale_rv)) "en" else {
      tryCatch(normalize_ui_locale(ui_locale_rv()), error = function(e) "en")
    }
    ch <- screener_field_choices(locale)
    ops <- stats::setNames(c("gte", "lte"), c(ui_str("scr_op_gte", locale), ui_str("scr_op_lte", locale)))
    mkts <- stats::setNames(c("US", "TW"), c(ui_str("scr_market_us", locale), ui_str("scr_market_tw", locale)))
    kinds <- stats::setNames(
      c("price_lte", "price_gte", "day_chg_lte", "day_chg_gte"),
      vapply(
        c("pa_kind_price_lte", "pa_kind_price_gte", "pa_kind_day_chg_lte", "pa_kind_day_chg_gte"),
        function(k) ui_str(k, locale),
        character(1)
      )
    )
    field_defaults <- c("roe", "pe", "debt_ratio", "pb")
    op_defaults <- c("gte", "lte", "lte", "lte")
    for (i in seq_len(.SCR_N_COND)) {
      cur <- isolate(input[[paste0("scr_field_", i)]])
      if (is.null(cur) || !cur %in% unname(ch)) cur <- field_defaults[[i]]
      updateSelectInput(session, paste0("scr_field_", i), choices = ch, selected = cur)
      cur_op <- isolate(input[[paste0("scr_op_", i)]])
      if (is.null(cur_op) || !cur_op %in% c("gte", "lte")) cur_op <- op_defaults[[i]]
      updateSelectInput(session, paste0("scr_op_", i), choices = ops, selected = cur_op)
    }
    cur_m <- isolate(input$scr_market)
    if (is.null(cur_m) || !cur_m %in% c("US", "TW")) cur_m <- "US"
    updateSelectInput(session, "scr_market", choices = mkts, selected = cur_m)
    cur_k <- isolate(input$scr_alert_kind)
    if (is.null(cur_k) || !cur_k %in% names(kinds) && !cur_k %in% unname(kinds)) cur_k <- "price_lte"
    updateSelectInput(session, "scr_alert_kind", choices = kinds, selected = cur_k)
  })

  observeEvent(input$market_mode_pick, {
    mode <- if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(input$market_mode_pick)
    } else {
      "US"
    }
    updateSelectInput(session, "scr_market", selected = mode)
  }, ignoreInit = TRUE)

  observeEvent(input$scr_run, {
    pool <- tryCatch(screener_load_pool(input$scr_market %||% "US"), error = function(e) NULL)
    conds <- .conds_from_input()
    if (is.null(pool)) {
      showNotification(say_now("scr_bad_field"), type = "warning")
      return()
    }
    res <- screener_apply(pool, conds)
    if (!isTRUE(res$ok)) {
      key <- switch(
        res$reason,
        no_conditions = "scr_no_conditions",
        bad_value = "scr_bad_value",
        "scr_bad_field"
      )
      showNotification(say_now(key), type = "warning")
      scr_meta(list(ok = FALSE, reason = res$reason, label = "", fields = character(0)))
      scr_result(res$df)
      return()
    }
    label <- screener_condition_label(res$conditions, locale_now())
    ordered <- res$df[order(res$df$ticker), , drop = FALSE]
    ordered <- screener_mark_alerts(ordered, price_alert_read())
    scr_meta(list(
      ok = TRUE, reason = "ok", label = label,
      fields = unique(as.character(res$conditions$field))
    ))
    scr_result(ordered)
    if (!nrow(ordered)) showNotification(say_now("scr_no_match"), type = "message")
  })

  observeEvent(pa_refresh(), {
    cur <- scr_result()
    if (!is.data.frame(cur) || !nrow(cur)) return()
    scr_result(screener_mark_alerts(cur, price_alert_read()))
  }, ignoreInit = TRUE)

  output$scr_table <- DT::renderDT({
    if (!is.null(ui_locale_rv)) ui_locale_rv()
    pa_refresh()
    df <- scr_result()
    meta <- scr_meta()
    locale <- locale_now()
    if (!is.data.frame(df) || !nrow(df) || !isTRUE(meta$ok)) {
      note <- if (identical(meta$reason, "idle")) say_now("scr_idle") else say_now("scr_no_match")
      return(DT::datatable(
        data.frame(Note = note, stringsAsFactors = FALSE),
        rownames = FALSE, selection = "none",
        options = list(dom = "t", ordering = FALSE)
      ))
    }
    df <- screener_mark_alerts(df, price_alert_read())
    cols <- unique(c("ticker", "name", meta$fields, "market_cap_bn", "roe", "pe", "pb"))
    cols <- cols[cols %in% names(df)]
    show <- df[, cols, drop = FALSE]
    for (nm in names(show)) {
      if (is.numeric(show[[nm]])) show[[nm]] <- round(show[[nm]], 2)
    }
    show$alert_flag <- as.integer(df$has_alert %in% TRUE)
    show$Alert <- ifelse(show$alert_flag == 1L, ui_str("scr_alert_yes", locale), ui_str("scr_alert_no", locale))
    lab_of <- function(id) {
      if (identical(id, "ticker")) return(ui_str("pa_col_ticker", locale))
      if (identical(id, "name")) return(ui_str("scr_col_name", locale))
      ui_str(paste0("scr_field_", id), locale)
    }
    names(show) <- c(vapply(cols, lab_of, character(1)), "alert_flag", ui_str("scr_col_alert", locale))
    # alert_flag must keep a stable name for formatStyle
    names(show)[names(show) == "alert_flag"] <- "alert_flag"
    DT::datatable(
      show,
      rownames = FALSE,
      selection = "multiple",
      options = list(
        pageLength = 12,
        dom = "tp",
        scrollX = TRUE,
        columnDefs = list(list(visible = FALSE, targets = which(names(show) == "alert_flag") - 1L))
      )
    ) |>
      DT::formatStyle(
        "alert_flag",
        target = "row",
        backgroundColor = DT::styleEqual(1L, "#fff4d6")
      )
  }, server = TRUE)

  observeEvent(input$scr_add, {
    # Full + auth: Value Alerts require sign-in (Lite exempt)
    lite <- isTRUE(tryCatch(isolate(input$ynow_lite_mode), error = function(e) FALSE))
    auth_on <- exists("ynow_auth_enabled", mode = "function") && isTRUE(ynow_auth_enabled())
    if (!lite && auth_on) {
      auth <- tryCatch(session$userData$ynow_auth, error = function(e) NULL)
      logged <- isTRUE(tryCatch(auth$is_logged_in(), error = function(e) FALSE))
      if (!logged) {
        showNotification(say_now("auth_pa_gate_title"), type = "warning")
        tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "open"), error = function(e) NULL)
        return()
      }
    }
    res_df <- isolate(scr_result())
    meta <- isolate(scr_meta())
    idx <- input$scr_table_rows_selected
    if (!isTRUE(meta$ok) || !nzchar(meta$label %||% "")) {
      showNotification(say_now("scr_no_conditions"), type = "warning")
      return()
    }
    if (!length(idx)) {
      showNotification(say_now("scr_none_selected"), type = "warning")
      return()
    }
    level <- suppressWarnings(as.numeric(input$scr_alert_level)[1])
    if (!is.finite(level)) {
      showNotification(say_now("pa_bad_level"), type = "warning")
      return()
    }
    kind <- as.character(input$scr_alert_kind %||% "price_lte")[1]
    if (!kind %in% c("price_lte", "price_gte", "day_chg_lte", "day_chg_gte")) kind <- "price_lte"
    ref <- screener_make_ref(meta$label)
    alerts <- price_alert_read()
    n_ok <- 0L
    last_reason <- "saved"
    notify_app <- if (is.null(input$pa_notify_app)) TRUE else isTRUE(input$pa_notify_app)
    notify_email <- isTRUE(input$pa_notify_email)
    email_val <- as.character(input$pa_email %||% "")[1]
    if (exists("ynow_auth_session_email", mode = "function")) {
      acct <- ynow_auth_session_email(tryCatch(session$userData$ynow_auth, error = function(e) NULL))
      if (nzchar(acct)) email_val <- acct
    }
    for (i in idx) {
      if (is.na(i) || i < 1L || i > nrow(res_df)) next
      # Points: one alert_slot per ticker when enforcement is on (Lite skips).
      if (!lite &&
          exists("ynow_points_enabled", mode = "function") && isTRUE(ynow_points_enabled()) &&
          exists("ynow_points_gate", mode = "function")) {
        auth_u <- tryCatch({
          a <- session$userData$ynow_auth
          a$user()
        }, error = function(e) NULL)
        day <- format(Sys.Date(), "%Y-%m-%d")
        pref <- paste(
          "alert",
          as.character(auth_u$id %||% "anon")[1],
          toupper(trimws(as.character(res_df$ticker[[i]])[1])),
          kind,
          as.character(level)[1],
          day,
          sep = ":"
        )
        loc_now <- tryCatch({
          if (is.null(ui_locale_rv)) "en" else normalize_ui_locale(isolate(ui_locale_rv()))
        }, error = function(e) "en")
        if (!isTRUE(ynow_points_gate(session, "alert_slot", pref, user = auth_u, locale = loc_now))) {
          last_reason <- "points"
          break
        }
      }
      added <- price_alert_add(
        alerts,
        email = email_val,
        ticker = res_df$ticker[[i]],
        kind = kind,
        level = level,
        mos_pct = 0,
        note = input$pa_note %||% "",
        notify_app = notify_app,
        notify_email = notify_email,
        source = "screener",
        metric = "",
        screen_ref = ref
      )
      if (!isTRUE(added$ok)) {
        last_reason <- added$reason
        break
      }
      alerts <- added$df
      n_ok <- n_ok + 1L
    }
    if (n_ok > 0L) price_alert_write(alerts)
    scr_result(screener_mark_alerts(res_df, if (n_ok > 0L) alerts else price_alert_read()))
    pa_refresh(isolate(pa_refresh()) + 1L)
    if (n_ok > 0L && n_ok == length(idx)) {
      showNotification(sprintf(say_now("scr_added"), n_ok), type = "message")
    } else if (n_ok > 0L) {
      showNotification(sprintf(say_now("scr_partial"), n_ok), type = "warning")
    } else {
      key <- switch(
        last_reason,
        bad_email = "pa_bad_email",
        bad_ticker = "pa_bad_ticker",
        bad_level = "pa_bad_level",
        no_channel = "pa_no_channel",
        full = "scr_partial",
        "pa_bad_level"
      )
      msg <- say_now(key)
      if (identical(key, "scr_partial")) msg <- sprintf(msg, 0L)
      showNotification(msg, type = "warning")
    }
  })
}
