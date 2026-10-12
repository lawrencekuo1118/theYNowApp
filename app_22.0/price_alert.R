if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

# Price / condition email alerts.
# Recipients must be on YNOW_ALERT_TO or YNOW_ALERT_ALLOWLIST.
# SMTP: YNOW_SMTP_USER, YNOW_SMTP_PASSWORD, optional YNOW_SMTP_SERVER / YNOW_ALERT_FROM.

.PA_KINDS <- c("price_gte", "price_lte", "day_chg_gte", "day_chg_lte", "below_fair")

price_alert_store_path <- function() {
  override <- Sys.getenv("YNOW_ALERT_STORE", unset = "")
  if (nzchar(override)) return(override)
  file.path("data", "price_alerts.json")
}

price_alert_empty <- function() {
  data.frame(
    id = character(0),
    email = character(0),
    ticker = character(0),
    kind = character(0),
    level = numeric(0),
    mos_pct = numeric(0),
    note = character(0),
    armed = logical(0),
    status = character(0),
    last_price = numeric(0),
    last_checked = character(0),
    last_sent = character(0),
    last_error = character(0),
    stringsAsFactors = FALSE
  )
}

price_alert_read <- function(path = price_alert_store_path()) {
  empty <- price_alert_empty()
  if (!file.exists(path)) return(empty)
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0L) return(empty)
  for (nm in names(empty)) {
    if (!nm %in% names(raw)) raw[[nm]] <- empty[[nm]][NA_integer_][seq_len(nrow(raw))]
  }
  raw <- raw[, names(empty), drop = FALSE]
  raw$email <- tolower(trimws(as.character(raw$email)))
  raw$ticker <- toupper(trimws(as.character(raw$ticker)))
  raw$kind <- as.character(raw$kind)
  raw$level <- suppressWarnings(as.numeric(raw$level))
  raw$mos_pct <- suppressWarnings(as.numeric(raw$mos_pct))
  raw$mos_pct[!is.finite(raw$mos_pct)] <- 0
  raw$armed <- as.logical(raw$armed)
  raw$armed[is.na(raw$armed)] <- FALSE
  raw$last_price <- suppressWarnings(as.numeric(raw$last_price))
  raw$note <- as.character(raw$note)
  raw$status <- as.character(raw$status)
  raw$last_checked <- as.character(raw$last_checked)
  raw$last_sent <- as.character(raw$last_sent)
  raw$last_error <- as.character(raw$last_error)
  raw
}

price_alert_write <- function(df, path = price_alert_store_path()) {
  if (!is.data.frame(df)) df <- price_alert_empty()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(df, path, dataframe = "rows", pretty = TRUE, auto_unbox = TRUE, na = "null")
  invisible(path)
}

price_alert_valid_email <- function(email) {
  email <- tolower(trimws(as.character(email)[1]))
  grepl("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$", email, ignore.case = TRUE)
}

price_alert_allowlist <- function() {
  chunks <- c(
    Sys.getenv("YNOW_ALERT_TO", unset = ""),
    Sys.getenv("YNOW_ALERT_ALLOWLIST", unset = "")
  )
  parts <- unlist(strsplit(chunks, "[,;[:space:]]+"), use.names = FALSE)
  parts <- tolower(trimws(parts))
  unique(parts[nzchar(parts)])
}

price_alert_email_allowed <- function(email) {
  email <- tolower(trimws(as.character(email)[1]))
  if (!price_alert_valid_email(email)) return(FALSE)
  allowed <- price_alert_allowlist()
  if (!length(allowed)) return(FALSE)
  email %in% allowed
}

price_alert_valid_ticker <- function(ticker) {
  ticker <- toupper(trimws(as.character(ticker)[1]))
  grepl("^[A-Z0-9.^=-]{1,16}$", ticker)
}

price_alert_condition_met <- function(kind, level, price, prev = NA_real_, mos_pct = 0) {
  kind <- as.character(kind)[1]
  level <- suppressWarnings(as.numeric(level)[1])
  price <- suppressWarnings(as.numeric(price)[1])
  prev <- suppressWarnings(as.numeric(prev)[1])
  mos_pct <- suppressWarnings(as.numeric(mos_pct)[1])
  if (!is.finite(mos_pct)) mos_pct <- 0
  if (!kind %in% .PA_KINDS || !is.finite(level) || !is.finite(price) || price <= 0) {
    return(FALSE)
  }
  chg <- if (is.finite(prev) && abs(prev) > 0) (price - prev) / abs(prev) * 100 else NA_real_
  if (identical(kind, "price_gte")) return(price >= level)
  if (identical(kind, "price_lte")) return(price <= level)
  if (identical(kind, "day_chg_gte")) return(is.finite(chg) && chg >= level)
  if (identical(kind, "day_chg_lte")) return(is.finite(chg) && chg <= level)
  if (identical(kind, "below_fair")) {
    hurdle <- level * (1 - mos_pct / 100)
    return(is.finite(hurdle) && hurdle > 0 && price <= hurdle)
  }
  FALSE
}

price_alert_new_id <- function() {
  paste0("pa_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sprintf("%04d", sample.int(9999L, 1L)))
}

price_alert_add <- function(df, email, ticker, kind, level, mos_pct = 0, note = "") {
  if (is.null(df) || !is.data.frame(df)) df <- price_alert_empty()
  email <- tolower(trimws(as.character(email)[1]))
  ticker <- toupper(trimws(as.character(ticker)[1]))
  kind <- as.character(kind)[1]
  level <- suppressWarnings(as.numeric(level)[1])
  mos_pct <- suppressWarnings(as.numeric(mos_pct)[1])
  if (!is.finite(mos_pct)) mos_pct <- 0
  if (!price_alert_email_allowed(email)) return(list(ok = FALSE, reason = "bad_email", df = df))
  if (!price_alert_valid_ticker(ticker)) return(list(ok = FALSE, reason = "bad_ticker", df = df))
  if (!kind %in% .PA_KINDS) return(list(ok = FALSE, reason = "bad_kind", df = df))
  if (!is.finite(level)) return(list(ok = FALSE, reason = "bad_level", df = df))
  if (nrow(df) >= 30L) return(list(ok = FALSE, reason = "full", df = df))
  row <- price_alert_empty()
  row[1, ] <- list(
    price_alert_new_id(), email, ticker, kind, level, mos_pct,
    as.character(note)[1], TRUE, "armed", NA_real_, "", "", ""
  )
  list(ok = TRUE, reason = "saved", df = rbind(df, row))
}

price_alert_smtp_config <- function() {
  user <- trimws(Sys.getenv("YNOW_SMTP_USER", unset = ""))
  password <- Sys.getenv("YNOW_SMTP_PASSWORD", unset = "")
  server <- trimws(Sys.getenv("YNOW_SMTP_SERVER", unset = "smtps://smtp.gmail.com:465"))
  from <- trimws(Sys.getenv("YNOW_ALERT_FROM", unset = user))
  if (!nzchar(user) || !nzchar(password) || !nzchar(from)) {
    return(list(ok = FALSE, reason = "smtp_missing"))
  }
  list(ok = TRUE, user = user, password = password, server = server, from = from)
}

price_alert_compose <- function(row, price) {
  kind <- as.character(row$kind)[1]
  ticker <- as.character(row$ticker)[1]
  level <- suppressWarnings(as.numeric(row$level)[1])
  mos <- suppressWarnings(as.numeric(row$mos_pct)[1])
  subject <- sprintf("YNow alert: %s %s", ticker, kind)
  body <- paste(
    sprintf("Ticker: %s", ticker),
    sprintf("Condition: %s", kind),
    sprintf("Level: %s", format(level, trim = TRUE, scientific = FALSE)),
    sprintf("MOS %%: %s", format(mos, trim = TRUE, scientific = FALSE)),
    sprintf("Last price: %s", format(price, trim = TRUE, scientific = FALSE)),
    sprintf("Note: %s", as.character(row$note)[1]),
    "This rule is now disarmed. Arm it again in the app to be notified next time.",
    sep = "\n"
  )
  list(subject = subject, body = body)
}

price_alert_send <- function(to, subject, body) {
  cfg <- price_alert_smtp_config()
  if (!isTRUE(cfg$ok)) return(list(ok = FALSE, reason = "smtp_missing"))
  if (!price_alert_email_allowed(to)) return(list(ok = FALSE, reason = "bad_email"))
  if (!requireNamespace("curl", quietly = TRUE)) return(list(ok = FALSE, reason = "curl_missing"))
  msg <- paste0(
    "From: ", cfg$from, "\r\n",
    "To: ", to, "\r\n",
    "Subject: ", subject, "\r\n",
    "Content-Type: text/plain; charset=UTF-8\r\n",
    "\r\n",
    body
  )
  sent <- tryCatch({
    curl::send_mail(
      mail_from = cfg$from,
      mail_rcpt = to,
      message = msg,
      smtp_server = cfg$server,
      use_ssl = "force",
      verbose = FALSE,
      username = cfg$user,
      password = cfg$password
    )
    TRUE
  }, error = function(e) conditionMessage(e))
  if (isTRUE(sent)) return(list(ok = TRUE, reason = "sent"))
  list(ok = FALSE, reason = "send_failed", detail = as.character(sent)[1])
}

price_alert_quote <- function(ticker) {
  if (!exists("fetch_price_history_df", mode = "function")) {
    return(list(ok = FALSE, price = NA_real_, prev = NA_real_, reason = "no_quote_fn"))
  }
  df <- tryCatch(fetch_price_history_df(ticker, "5d"), error = function(e) NULL)
  if (!is.data.frame(df) || !nrow(df) || !"Close" %in% names(df)) {
    return(list(ok = FALSE, price = NA_real_, prev = NA_real_, reason = "no_quote"))
  }
  px <- suppressWarnings(as.numeric(df$Close))
  px <- px[is.finite(px)]
  if (!length(px)) return(list(ok = FALSE, price = NA_real_, prev = NA_real_, reason = "no_quote"))
  prev <- if (length(px) >= 2L) px[[length(px) - 1L]] else NA_real_
  list(ok = TRUE, price = px[[length(px)]], prev = prev, reason = "ok")
}

price_alert_apply_checks <- function(df, quotes, send_fn = price_alert_send, now = Sys.time()) {
  if (is.null(df) || !nrow(df)) {
    return(list(df = price_alert_empty(), sent = 0L, met = 0L))
  }
  sent <- 0L
  met <- 0L
  stamp <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  for (i in seq_len(nrow(df))) {
    tk <- df$ticker[[i]]
    q <- quotes[[tk]]
    if (is.null(q)) next
    df$last_checked[[i]] <- stamp
    if (isTRUE(q$ok)) df$last_price[[i]] <- q$price
    hit <- isTRUE(df$armed[[i]]) && isTRUE(price_alert_condition_met(
      df$kind[[i]], df$level[[i]], q$price, q$prev, df$mos_pct[[i]]
    ))
    if (!hit) next
    met <- met + 1L
    mail <- price_alert_compose(df[i, , drop = FALSE], q$price)
    res <- tryCatch(
      send_fn(df$email[[i]], mail$subject, mail$body),
      error = function(e) list(ok = FALSE, reason = "send_failed", detail = conditionMessage(e))
    )
    if (isTRUE(res$ok)) {
      df$armed[[i]] <- FALSE
      df$status[[i]] <- "sent"
      df$last_sent[[i]] <- stamp
      df$last_error[[i]] <- ""
      sent <- sent + 1L
    } else {
      df$status[[i]] <- as.character(res$reason %||% "send_failed")[1]
      df$last_error[[i]] <- as.character(res$detail %||% res$reason)[1]
    }
  }
  list(df = df, sent = sent, met = met)
}

price_alert_check_store <- function(path = price_alert_store_path(), send_fn = price_alert_send) {
  df <- price_alert_read(path)
  if (!nrow(df) || !any(df$armed %in% TRUE)) return(list(df = df, sent = 0L, met = 0L))
  lock <- paste0(path, ".lock")
  if (file.exists(lock)) {
    age <- difftime(Sys.time(), file.info(lock)$mtime, units = "secs")
    if (is.finite(age) && age < 90) return(list(df = df, sent = 0L, met = 0L, skipped = "locked"))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines("1", lock)
  on.exit(unlink(lock), add = TRUE)
  quotes <- list()
  for (tk in unique(df$ticker[df$armed])) quotes[[tk]] <- price_alert_quote(tk)
  out <- price_alert_apply_checks(df, quotes, send_fn = send_fn)
  price_alert_write(out$df, path)
  out
}

price_alert_tab_ui <- function() {
  en <- .UI_STRINGS$en
  zh <- .UI_STRINGS$`zh-TW`
  bi <- function(key) {
    tags$span(
      tags$span(class = "ynow-loc-en", en[[key]]),
      tags$span(class = "ynow-loc-zh", zh[[key]])
    )
  }
  tabItem(
    tabName = "price_alerts",
    fluidRow(
      box(
        title = bi("pa_title"),
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        tags$p(bi("pa_help")),
        fluidRow(
          column(4, textInput("pa_email", bi("pa_email"), value = Sys.getenv("YNOW_ALERT_TO", ""))),
          column(3, textInput("pa_ticker", bi("pa_ticker"), value = "")),
          column(5, selectInput(
            "pa_kind", bi("pa_kind"),
            choices = c(
              "Last price ≥ target / 最新價 ≥ 目標價" = "price_gte",
              "Last price ≤ target / 最新價 ≤ 目標價" = "price_lte",
              "Daily change ≥ % / 日漲跌幅 ≥ %" = "day_chg_gte",
              "Daily change ≤ % / 日漲跌幅 ≤ %" = "day_chg_lte",
              "Price ≤ fair × (1 − MOS) / 最新價 ≤ 公允價值 × (1 − MOS)" = "below_fair"
            ),
            width = "100%"
          ))
        ),
        fluidRow(
          column(3, numericInput("pa_level", bi("pa_level"), value = NA, width = "100%")),
          column(3, numericInput("pa_mos", bi("pa_mos"), value = 0, min = 0, max = 90, width = "100%")),
          column(6, textInput("pa_note", bi("pa_note"), value = ""))
        ),
        actionButton("pa_save", bi("pa_save"), class = "btn-primary"),
        actionButton("pa_check", bi("pa_check"), class = "btn-default"),
        actionButton("pa_rearm", bi("pa_rearm"), class = "btn-default"),
        actionButton("pa_delete", bi("pa_delete"), class = "btn-danger"),
        tags$div(style = "margin-top:12px;", DT::DTOutput("pa_table"))
      )
    )
  )
}

price_alert_server <- function(input, output, session, ui_locale_rv = NULL) {
  loc <- function() {
    if (is.null(ui_locale_rv)) return("en")
    tryCatch(normalize_ui_locale(isolate(ui_locale_rv())), error = function(e) "en")
  }
  say <- function(key) ui_str(key, loc())
  pa_refresh <- reactiveVal(0L)

  output$pa_table <- DT::renderDT({
    pa_refresh()
    df <- price_alert_read()
    if (!nrow(df)) {
      show <- data.frame(Note = say("pa_empty"), stringsAsFactors = FALSE)
    } else {
      show <- data.frame(
        Ticker = df$ticker,
        Condition = df$kind,
        Level = df$level,
        Email = df$email,
        Status = df$status,
        Price = df$last_price,
        Sent = df$last_sent,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      names(show) <- c(
        say("pa_col_ticker"), say("pa_col_kind"), say("pa_col_level"),
        say("pa_col_email"), say("pa_col_status"), say("pa_col_price"),
        say("pa_col_sent")
      )
    }
    DT::datatable(
      show,
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 10, dom = "tp", scrollX = TRUE)
    )
  }, server = TRUE)

  observeEvent(input$pa_save, {
    res <- price_alert_add(
      price_alert_read(),
      input$pa_email, input$pa_ticker, input$pa_kind,
      input$pa_level, input$pa_mos, input$pa_note
    )
    if (isTRUE(res$ok)) {
      price_alert_write(res$df)
      showNotification(say("pa_saved"), type = "message")
    } else {
      key <- switch(
        res$reason,
        bad_email = "pa_bad_email",
        bad_ticker = "pa_bad_ticker",
        bad_level = "pa_bad_level",
        "pa_bad_level"
      )
      showNotification(say(key), type = "warning")
    }
    pa_refresh(isolate(pa_refresh()) + 1L)
  })

  observeEvent(input$pa_check, {
    out <- price_alert_check_store()
    msg <- say("pa_checked")
    if (out$met > 0L && out$sent == 0L) msg <- paste(msg, say("pa_smtp_missing"))
    showNotification(msg, type = if (out$sent > 0L) "message" else "warning")
    pa_refresh(isolate(pa_refresh()) + 1L)
  })

  .selected_id <- function() {
    df <- price_alert_read()
    if (!nrow(df)) return(NA_character_)
    idx <- input$pa_table_rows_selected
    if (length(idx) != 1L || is.na(idx) || idx < 1L || idx > nrow(df)) return(NA_character_)
    df$id[[idx]]
  }

  observeEvent(input$pa_delete, {
    id <- .selected_id()
    if (is.na(id)) {
      showNotification(say("pa_need_row"), type = "warning")
      return()
    }
    df <- price_alert_read()
    price_alert_write(df[df$id != id, , drop = FALSE])
    pa_refresh(isolate(pa_refresh()) + 1L)
  })

  observeEvent(input$pa_rearm, {
    id <- .selected_id()
    if (is.na(id)) {
      showNotification(say("pa_need_row"), type = "warning")
      return()
    }
    df <- price_alert_read()
    hit <- df$id == id
    df$armed[hit] <- TRUE
    df$status[hit] <- "armed"
    price_alert_write(df)
    pa_refresh(isolate(pa_refresh()) + 1L)
  })

  observe({
    invalidateLater(15 * 60 * 1000, session)
    df <- price_alert_read()
    if (!nrow(df) || !any(df$armed %in% TRUE)) return()
    price_alert_check_store()
    pa_refresh(isolate(pa_refresh()) + 1L)
  })
}
