if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

# Value alerts（到值通知）: price / metric / grade conditions with trigger history.
# Recipients must be on YNOW_ALERT_TO or YNOW_ALERT_ALLOWLIST when email is on.
# SMTP: YNOW_SMTP_USER, YNOW_SMTP_PASSWORD, optional YNOW_SMTP_SERVER / YNOW_ALERT_FROM.

.PA_KINDS <- c(
  "price_gte", "price_lte", "day_chg_gte", "day_chg_lte", "below_fair",
  "value_gte", "value_lte", "grade_lte", "grade_gte"
)
.PA_PRICE_KINDS <- c("price_gte", "price_lte", "day_chg_gte", "day_chg_lte", "below_fair")
.PA_VALUE_KINDS <- c("value_gte", "value_lte")
.PA_GRADE_KINDS <- c("grade_lte", "grade_gte")
.PA_GRADE_ORD <- c(A = 5, B = 4, C = 3, D = 2, F = 1)

price_alert_grade_to_ord <- function(x) {
  if (is.numeric(x) && length(x) >= 1L && is.finite(x[1])) {
    v <- as.numeric(x[1])
    if (v >= 1 && v <= 5) return(v)
  }
  g <- toupper(trimws(as.character(x)[1]))
  if (g %in% names(.PA_GRADE_ORD)) return(unname(.PA_GRADE_ORD[[g]]))
  NA_real_
}

price_alert_ord_to_grade <- function(x) {
  v <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(v)) return(NA_character_)
  hit <- names(.PA_GRADE_ORD)[.PA_GRADE_ORD == as.integer(round(v))]
  if (!length(hit)) return(NA_character_)
  hit[[1]]
}

price_alert_store_path <- function() {
  override <- Sys.getenv("YNOW_ALERT_STORE", unset = "")
  if (nzchar(override)) return(override)
  file.path("data", "price_alerts.json")
}

price_alert_history_path <- function() {
  override <- Sys.getenv("YNOW_ALERT_HISTORY", unset = "")
  if (nzchar(override)) return(override)
  file.path("data", "price_alert_history.json")
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
    notify_app = logical(0),
    notify_email = logical(0),
    source = character(0),
    metric = character(0),
    screen_ref = character(0),
    stringsAsFactors = FALSE
  )
}

price_alert_history_empty <- function() {
  data.frame(
    id = character(0),
    alert_id = character(0),
    ticker = character(0),
    kind = character(0),
    level = numeric(0),
    price = numeric(0),
    channel = character(0),
    message = character(0),
    triggered_at = character(0),
    stringsAsFactors = FALSE
  )
}

.pa_fill_cols <- function(raw, empty) {
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0L) return(empty)
  for (nm in names(empty)) {
    if (!nm %in% names(raw)) raw[[nm]] <- empty[[nm]][NA_integer_][seq_len(nrow(raw))]
  }
  raw[, names(empty), drop = FALSE]
}

price_alert_read <- function(path = price_alert_store_path()) {
  empty <- price_alert_empty()
  if (!file.exists(path)) return(empty)
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyDataFrame = TRUE), error = function(e) NULL)
  raw <- .pa_fill_cols(raw, empty)
  if (!nrow(raw)) return(empty)
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
  raw$notify_app <- as.logical(raw$notify_app)
  raw$notify_email <- as.logical(raw$notify_email)
  # Legacy rows (pre-channel columns): email-only
  legacy_na_app <- is.na(raw$notify_app)
  legacy_na_email <- is.na(raw$notify_email)
  raw$notify_app[legacy_na_app] <- FALSE
  raw$notify_email[legacy_na_email] <- TRUE
  raw$source <- as.character(raw$source)
  raw$source[!nzchar(raw$source) | is.na(raw$source)] <- "form"
  raw$metric <- as.character(raw$metric)
  raw$metric[is.na(raw$metric)] <- ""
  raw$screen_ref <- as.character(raw$screen_ref)
  raw$screen_ref[is.na(raw$screen_ref)] <- ""
  raw
}

price_alert_screen_text <- function(screen_ref) {
  x <- as.character(screen_ref)
  if (!length(x)) return(character(0))
  x[is.na(x)] <- ""
  sub("^scr_[^|]*\\|", "", x)
}

price_alert_write <- function(df, path = price_alert_store_path()) {
  if (!is.data.frame(df)) df <- price_alert_empty()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(df, path, dataframe = "rows", pretty = TRUE, auto_unbox = TRUE, na = "null")
  invisible(path)
}

price_alert_history_read <- function(path = price_alert_history_path()) {
  empty <- price_alert_history_empty()
  if (!file.exists(path)) return(empty)
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyDataFrame = TRUE), error = function(e) NULL)
  raw <- .pa_fill_cols(raw, empty)
  if (!nrow(raw)) return(empty)
  raw$id <- as.character(raw$id)
  raw$alert_id <- as.character(raw$alert_id)
  raw$ticker <- toupper(trimws(as.character(raw$ticker)))
  raw$kind <- as.character(raw$kind)
  raw$level <- suppressWarnings(as.numeric(raw$level))
  raw$price <- suppressWarnings(as.numeric(raw$price))
  raw$channel <- as.character(raw$channel)
  raw$message <- as.character(raw$message)
  raw$triggered_at <- as.character(raw$triggered_at)
  raw
}

price_alert_history_write <- function(df, path = price_alert_history_path()) {
  if (!is.data.frame(df)) df <- price_alert_history_empty()
  # Cap growth
  if (nrow(df) > 200L) df <- utils::tail(df, 200L)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(df, path, dataframe = "rows", pretty = TRUE, auto_unbox = TRUE, na = "null")
  invisible(path)
}

price_alert_history_append <- function(row, path = price_alert_history_path()) {
  hist <- price_alert_history_read(path)
  add <- price_alert_history_empty()
  add[1, ] <- list(
    as.character(row$id %||% price_alert_new_id())[1],
    as.character(row$alert_id %||% "")[1],
    toupper(trimws(as.character(row$ticker %||% "")[1])),
    as.character(row$kind %||% "")[1],
    suppressWarnings(as.numeric(row$level %||% NA_real_)[1]),
    suppressWarnings(as.numeric(row$price %||% NA_real_)[1]),
    as.character(row$channel %||% "")[1],
    as.character(row$message %||% "")[1],
    as.character(row$triggered_at %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))[1]
  )
  price_alert_history_write(rbind(hist, add), path)
  invisible(add)
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
  if (!kind %in% .PA_KINDS || !is.finite(level) || !is.finite(price)) {
    return(FALSE)
  }
  # Price / fair paths require a positive spot; value/grade may be ≤ 0 (e.g. M-Score, MOS).
  if (kind %in% .PA_PRICE_KINDS && !(is.finite(price) && price > 0)) {
    return(FALSE)
  }
  chg <- if (is.finite(prev) && abs(prev) > 0) (price - prev) / abs(prev) * 100 else NA_real_
  if (identical(kind, "price_gte") || identical(kind, "value_gte")) return(price >= level)
  if (identical(kind, "price_lte") || identical(kind, "value_lte")) return(price <= level)
  if (identical(kind, "day_chg_gte")) return(is.finite(chg) && chg >= level)
  if (identical(kind, "day_chg_lte")) return(is.finite(chg) && chg <= level)
  if (identical(kind, "grade_gte")) return(price >= level)
  if (identical(kind, "grade_lte")) return(price <= level)
  if (identical(kind, "below_fair")) {
    hurdle <- level * (1 - mos_pct / 100)
    return(is.finite(hurdle) && hurdle > 0 && price <= hurdle)
  }
  FALSE
}

price_alert_new_id <- function() {
  paste0("pa_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sprintf("%04d", sample.int(9999L, 1L)))
}

price_alert_add <- function(
  df, email, ticker, kind, level, mos_pct = 0, note = "",
  notify_app = TRUE, notify_email = FALSE, source = "form", metric = "",
  screen_ref = ""
) {
  if (is.null(df) || !is.data.frame(df)) df <- price_alert_empty()
  df <- .pa_fill_cols(df, price_alert_empty())
  email <- tolower(trimws(as.character(email)[1]))
  ticker <- toupper(trimws(as.character(ticker)[1]))
  kind <- as.character(kind)[1]
  level <- suppressWarnings(as.numeric(level)[1])
  mos_pct <- suppressWarnings(as.numeric(mos_pct)[1])
  if (!is.finite(mos_pct)) mos_pct <- 0
  notify_app <- isTRUE(notify_app)
  notify_email <- isTRUE(notify_email)
  if (!notify_app && !notify_email) {
    return(list(ok = FALSE, reason = "no_channel", df = df))
  }
  if (notify_email) {
    if (!price_alert_email_allowed(email)) {
      return(list(ok = FALSE, reason = "bad_email", df = df))
    }
  } else {
    email <- ""
  }
  if (!price_alert_valid_ticker(ticker)) return(list(ok = FALSE, reason = "bad_ticker", df = df))
  if (!kind %in% .PA_KINDS) return(list(ok = FALSE, reason = "bad_kind", df = df))
  if (!is.finite(level)) return(list(ok = FALSE, reason = "bad_level", df = df))
  if (nrow(df) >= 30L) return(list(ok = FALSE, reason = "full", df = df))
  row <- price_alert_empty()
  row[1, ] <- list(
    price_alert_new_id(), email, ticker, kind, level, mos_pct,
    as.character(note)[1], TRUE, "armed", NA_real_, "", "", "",
    notify_app, notify_email,
    as.character(source)[1], as.character(metric)[1],
    as.character(screen_ref)[1]
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
    sprintf("Target Value: %s", format(level, trim = TRUE, scientific = FALSE)),
    sprintf("MOS %%: %s", format(mos, trim = TRUE, scientific = FALSE)),
    sprintf("Last price: %s", format(price, trim = TRUE, scientific = FALSE)),
    sprintf("Note: %s", as.character(row$note)[1]),
    "This rule is now disarmed. Arm it again in the app to be notified next time.",
    sep = "\n"
  )
  list(subject = subject, body = body)
}

price_alert_toast_message <- function(row, price, locale = "en") {
  ticker <- as.character(row$ticker)[1]
  kind <- as.character(row$kind)[1]
  level <- suppressWarnings(as.numeric(row$level)[1])
  px <- suppressWarnings(as.numeric(price)[1])
  if (exists("ui_str", mode = "function")) {
    tryCatch({
      return(sprintf(
        "%s — %s @ %s (level %s)",
        ticker,
        ui_str(paste0("pa_kind_", kind), locale),
        format(px, trim = TRUE, scientific = FALSE),
        format(level, trim = TRUE, scientific = FALSE)
      ))
    }, error = function(e) NULL)
  }
  sprintf("%s %s hit @ %s", ticker, kind, format(px, trim = TRUE, scientific = FALSE))
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

price_alert_apply_checks <- function(df, quotes, send_fn = price_alert_send, now = Sys.time(),
                                     history_path = price_alert_history_path()) {
  if (is.null(df) || !nrow(df)) {
    return(list(df = price_alert_empty(), sent = 0L, met = 0L, app_toasts = list()))
  }
  sent <- 0L
  met <- 0L
  app_toasts <- list()
  stamp <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  for (i in seq_len(nrow(df))) {
    tk <- df$ticker[[i]]
    q <- quotes[[tk]]
    if (is.null(q)) next
    df$last_checked[[i]] <- stamp
    kind_i <- as.character(df$kind[[i]])[1]
    metric_i <- as.character(df$metric[[i]] %||% "")[1]
    cur <- NA_real_
    prev <- suppressWarnings(as.numeric(q$prev %||% NA_real_)[1])
    if (kind_i %in% c(.PA_VALUE_KINDS, .PA_GRADE_KINDS)) {
      mets <- q$metrics
      if (is.list(mets) && nzchar(metric_i) && !is.null(mets[[metric_i]])) {
        cur <- suppressWarnings(as.numeric(mets[[metric_i]])[1])
      }
      if (isTRUE(q$ok) && is.finite(suppressWarnings(as.numeric(q$price)[1]))) {
        df$last_price[[i]] <- suppressWarnings(as.numeric(q$price)[1])
      }
      if (is.finite(cur)) df$last_price[[i]] <- cur
    } else {
      if (isTRUE(q$ok)) df$last_price[[i]] <- q$price
      cur <- suppressWarnings(as.numeric(q$price %||% NA_real_)[1])
    }
    hit <- isTRUE(df$armed[[i]]) && isTRUE(price_alert_condition_met(
      kind_i, df$level[[i]], cur, prev, df$mos_pct[[i]]
    ))
    if (!hit) next
    met <- met + 1L
    want_app <- isTRUE(df$notify_app[[i]])
    want_email <- isTRUE(df$notify_email[[i]])
    # Legacy: neither flag stored → email only
    if (!want_app && !want_email) want_email <- TRUE

    email_ok <- FALSE
    email_reason <- ""
    if (want_email) {
      mail <- price_alert_compose(df[i, , drop = FALSE], cur)
      res <- tryCatch(
        send_fn(df$email[[i]], mail$subject, mail$body),
        error = function(e) list(ok = FALSE, reason = "send_failed", detail = conditionMessage(e))
      )
      email_ok <- isTRUE(res$ok)
      email_reason <- as.character(res$reason %||% "send_failed")[1]
      if (email_ok) sent <- sent + 1L
    }

    toast_msg <- price_alert_toast_message(df[i, , drop = FALSE], cur)
    channels <- character(0)
    if (want_app) channels <- c(channels, "app")
    if (want_email && email_ok) channels <- c(channels, "email")

    # Disarm when: app-only notified, or email succeeded, or email not requested
    can_disarm <- (!want_email && want_app) || (want_email && email_ok) || (!want_email && !want_app)
    if (want_email && !email_ok && !want_app) {
      can_disarm <- FALSE
    }
    if (want_email && !email_ok && want_app) {
      # App toast delivered; keep armed only if user insisted on email — disarm app side done via history
      can_disarm <- FALSE
      df$status[[i]] <- email_reason
      df$last_error[[i]] <- email_reason
    }

    if (length(channels)) {
      price_alert_history_append(list(
        id = price_alert_new_id(),
        alert_id = df$id[[i]],
        ticker = tk,
        kind = df$kind[[i]],
        level = df$level[[i]],
        price = cur,
        channel = paste(channels, collapse = "+"),
        message = toast_msg,
        triggered_at = stamp
      ), path = history_path)
    }

    if (want_app) {
      app_toasts[[length(app_toasts) + 1L]] <- list(
        alert_id = df$id[[i]],
        ticker = tk,
        message = toast_msg,
        price = cur
      )
    }

    if (isTRUE(can_disarm) || (want_app && !want_email)) {
      df$armed[[i]] <- FALSE
      df$status[[i]] <- if (want_email && email_ok) "sent" else if (want_app) "notified_app" else "sent"
      df$last_sent[[i]] <- stamp
      df$last_error[[i]] <- ""
    } else if (want_email && !email_ok) {
      df$status[[i]] <- email_reason
      df$last_error[[i]] <- email_reason
    }
  }
  list(df = df, sent = sent, met = met, app_toasts = app_toasts)
}

price_alert_check_store <- function(path = price_alert_store_path(), send_fn = price_alert_send,
                                    history_path = price_alert_history_path(),
                                    metrics_by_ticker = NULL) {
  df <- price_alert_read(path)
  if (!nrow(df) || !any(df$armed %in% TRUE)) {
    return(list(df = df, sent = 0L, met = 0L, app_toasts = list()))
  }
  lock <- paste0(path, ".lock")
  if (file.exists(lock)) {
    age <- difftime(Sys.time(), file.info(lock)$mtime, units = "secs")
    if (is.finite(age) && age < 90) {
      return(list(df = df, sent = 0L, met = 0L, skipped = "locked", app_toasts = list()))
    }
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines("1", lock)
  on.exit(unlink(lock), add = TRUE)
  quotes <- list()
  for (tk in unique(df$ticker[df$armed])) {
    q <- price_alert_quote(tk)
    mets <- NULL
    if (is.list(metrics_by_ticker) && !is.null(metrics_by_ticker[[tk]])) {
      mets <- metrics_by_ticker[[tk]]
    }
    if (is.list(mets) && length(mets)) q$metrics <- mets
    # Value/grade-only rules can still evaluate from session metrics without a live quote.
    if (!isTRUE(q$ok) && is.list(mets) && length(mets)) {
      q$ok <- TRUE
      if (!is.finite(suppressWarnings(as.numeric(q$price)[1]))) q$price <- NA_real_
    }
    quotes[[tk]] <- q
  }
  out <- price_alert_apply_checks(df, quotes, send_fn = send_fn, history_path = history_path)
  price_alert_write(out$df, path)
  out
}

#' Wrap a KPI / card so right-click / long-press can open the quick alert modal.
#' Set ready=FALSE when the value needs Search/Run first.
#' A small translucent bell sits at the inner top-right of ready/blocked targets.
ynow_alertable <- function(
  ...,
  metric,
  ticker = "",
  value = NA_real_,
  label = "",
  ready = FALSE,
  default_kind = "price_gte",
  fair_value = NA_real_
) {
  ready_flag <- if (isTRUE(ready) && is.finite(suppressWarnings(as.numeric(value)[1]))) "1" else "0"
  tags$div(
    class = paste(
      "ynow-alertable",
      if (identical(ready_flag, "1")) "ynow-alertable--ready" else "ynow-alertable--blocked"
    ),
    `data-ynow-alert` = "1",
    `data-ynow-alert-ready` = ready_flag,
    `data-ynow-alert-metric` = as.character(metric)[1],
    `data-ynow-alert-ticker` = toupper(trimws(as.character(ticker)[1])),
    `data-ynow-alert-value` = if (is.finite(suppressWarnings(as.numeric(value)[1]))) {
      format(suppressWarnings(as.numeric(value)[1]), scientific = FALSE, trim = TRUE)
    } else {
      ""
    },
    `data-ynow-alert-label` = as.character(label)[1],
    `data-ynow-alert-kind` = as.character(default_kind)[1],
    `data-ynow-alert-fair` = if (is.finite(suppressWarnings(as.numeric(fair_value)[1]))) {
      format(suppressWarnings(as.numeric(fair_value)[1]), scientific = FALSE, trim = TRUE)
    } else {
      ""
    },
    tags$span(
      class = "ynow-alert-bell",
      `aria-hidden` = "true",
      HTML(
        paste0(
          '<svg viewBox="0 0 24 24" width="14" height="14" focusable="false">',
          '<path fill="currentColor" d="M12 22a2.2 2.2 0 0 0 2.2-2.2h-4.4A2.2 2.2 0 0 0 12 22zm6.4-6.2V11a6.4 6.4 0 0 0-5.1-6.3V4a1.3 1.3 0 1 0-2.6 0v.7A6.4 6.4 0 0 0 5.6 11v4.8L4 17.4V18h16v-.6l-1.6-1.6z"/>',
          "</svg>"
        )
      )
    ),
    ...
  )
}

.pa_bi <- function(key) {
  en <- .UI_STRINGS$en
  zh <- .UI_STRINGS$`zh-TW`
  tags$span(
    tags$span(class = "ynow-loc-en", en[[key]] %||% key),
    tags$span(class = "ynow-loc-zh", zh[[key]] %||% key)
  )
}

price_alert_tab_ui <- function() {
  tabItem(
    tabName = "price_alerts",
    # Full + auth-on + guest gate (CSS: body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed))
    if (exists("ynow_pa_guest_gate_ui", mode = "function")) {
      ynow_pa_guest_gate_ui()
    } else {
      tags$div(id = "ynow_pa_guest_gate", style = "display:none;")
    },
    tags$div(
      id = "ynow_pa_body",
      class = "ynow-pa-tabs-wrap",
      tabBox(
        title = .pa_bi("menu_price_alert"),
        id = "ynow_pa_report",
        width = 12,
        tabPanel(
          title = .pa_bi("pa_title"),
          value = "pa_alerts",
          icon = icon("bell"),
          tags$p(.pa_bi("pa_help")),
          tags$p(class = "help-block", .pa_bi("pa_email_account_help")),
          fluidRow(
            column(4, textInput("pa_email", .pa_bi("pa_email"), value = Sys.getenv("YNOW_ALERT_TO", ""))),
            column(3, textInput("pa_ticker", .pa_bi("pa_ticker"), value = "")),
            column(5, selectInput(
              "pa_kind", .pa_bi("pa_kind"),
              choices = c(
                "Last price ≥ target / 最新價 ≥ 目標價" = "price_gte",
                "Last price ≤ target / 最新價 ≤ 目標價" = "price_lte",
                "Daily change ≥ % / 日漲跌幅 ≥ %" = "day_chg_gte",
                "Daily change ≤ % / 日漲跌幅 ≤ %" = "day_chg_lte",
                "Price ≤ fair × (1 − MOS) / 最新價 ≤ 公允價值 × (1 − MOS)" = "below_fair",
                "Value ≥ level / 數值 ≥ 水準" = "value_gte",
                "Value ≤ level / 數值 ≤ 水準" = "value_lte",
                "Grade ≥ / 評等 ≥" = "grade_gte",
                "Grade ≤ / 評等 ≤" = "grade_lte"
              ),
              width = "100%"
            ))
          ),
          fluidRow(
            column(3, numericInput("pa_level", .pa_bi("pa_level"), value = NA, width = "100%")),
            column(3, numericInput("pa_mos", .pa_bi("pa_mos"), value = 0, min = 0, max = 90, width = "100%")),
            column(6, textInput("pa_note", .pa_bi("pa_note"), value = ""))
          ),
          fluidRow(
            column(3, checkboxInput("pa_notify_app", .pa_bi("pa_notify_app"), value = TRUE)),
            column(3, checkboxInput("pa_notify_email", .pa_bi("pa_notify_email"), value = FALSE))
          ),
          actionButton("pa_save", .pa_bi("pa_save"), class = "btn-primary"),
          actionButton("pa_check", .pa_bi("pa_check"), class = "btn-default"),
          actionButton("pa_rearm", .pa_bi("pa_rearm"), class = "btn-default"),
          actionButton("pa_delete", .pa_bi("pa_delete"), class = "btn-danger"),
          tags$div(style = "margin-top:12px;", DT::DTOutput("pa_table"))
        ),
        tabPanel(
          title = .pa_bi("pa_history_title"),
          value = "pa_history",
          icon = icon("history"),
          tags$p(.pa_bi("pa_history_help")),
          actionButton("pa_history_clear", .pa_bi("pa_history_clear"), class = "btn-default"),
          tags$div(style = "margin-top:12px;", DT::DTOutput("pa_history_table"))
        )
      )
    ) # /#ynow_pa_body
  )
}

.pa_metric_family <- function(metric) {
  m <- as.character(metric %||% "")[1]
  if (m %in% c("last_price", "fair_value", "fair_value_secondary", "mos_pct")) return("price")
  if (identical(m, "qoe_grade")) return("grade")
  "value"
}

price_alert_quick_modal <- function(payload, locale = "en") {
  say <- function(key) {
    if (exists("ui_str", mode = "function")) ui_str(key, locale) else key
  }
  ticker <- toupper(trimws(as.character(payload$ticker %||% "")[1]))
  metric <- as.character(payload$metric %||% "last_price")[1]
  label <- as.character(payload$label %||% metric)[1]
  value <- suppressWarnings(as.numeric(payload$value %||% NA_real_)[1])
  fair <- suppressWarnings(as.numeric(payload$fair %||% NA_real_)[1])
  def_kind <- as.character(payload$kind %||% "price_gte")[1]
  if (!def_kind %in% .PA_KINDS) def_kind <- "price_gte"
  family <- .pa_metric_family(metric)

  level_default <- value
  mos_default <- 0
  show_mos <- FALSE
  if (identical(metric, "mos_pct") && is.finite(fair) && fair > 0 && is.finite(value)) {
    def_kind <- "below_fair"
    level_default <- fair
    mos_default <- max(0, min(90, value))
    family <- "price"
    show_mos <- TRUE
  } else if (metric %in% c("fair_value", "fair_value_secondary") && is.finite(value)) {
    def_kind <- "below_fair"
    level_default <- value
    mos_default <- 0
    family <- "price"
    show_mos <- TRUE
  } else if (identical(family, "grade")) {
    if (!def_kind %in% .PA_GRADE_KINDS) def_kind <- "grade_lte"
    if (!is.finite(level_default)) level_default <- 3
  } else if (identical(family, "value")) {
    if (!def_kind %in% .PA_VALUE_KINDS) {
      def_kind <- if (metric %in% c("beneish_m_score", "statement_alert_count")) "value_gte" else "value_lte"
    }
  } else {
    show_mos <- identical(def_kind, "below_fair") || metric %in% c("last_price", "fair_value", "mos_pct")
    if (!def_kind %in% c("price_gte", "price_lte", "below_fair")) def_kind <- "price_lte"
  }

  kind_choices <- if (identical(family, "grade")) {
    c(
      setNames("grade_gte", say("pa_kind_grade_gte")),
      setNames("grade_lte", say("pa_kind_grade_lte"))
    )
  } else if (identical(family, "value")) {
    c(
      setNames("value_gte", say("pa_kind_value_gte")),
      setNames("value_lte", say("pa_kind_value_lte"))
    )
  } else {
    c(
      setNames("price_gte", say("pa_kind_price_gte")),
      setNames("price_lte", say("pa_kind_price_lte")),
      setNames("below_fair", say("pa_kind_below_fair"))
    )
  }

  disp_val <- if (identical(family, "grade") && is.finite(value)) {
    price_alert_ord_to_grade(value) %||% format(value, trim = TRUE, scientific = FALSE)
  } else if (is.finite(value)) {
    format(value, trim = TRUE, scientific = FALSE)
  } else {
    "—"
  }

  level_input <- if (identical(family, "grade")) {
    grade_choices <- c(
      setNames(5, "A"), setNames(4, "B"), setNames(3, "C"),
      setNames(2, "D"), setNames(1, "F")
    )
    sel <- if (is.finite(level_default)) as.character(as.integer(round(level_default))) else "3"
    selectInput("pa_quick_level", say("pa_level_grade"), choices = grade_choices, selected = sel)
  } else {
    numericInput("pa_quick_level", say("pa_level"), value = level_default, width = "100%")
  }

  modalDialog(
    title = NULL,
    easyClose = TRUE,
    fade = TRUE,
    size = "m",
    tags$div(
      class = "ynow-pa-modal",
      tags$div(
        class = "ynow-pa-modal__hero",
        tags$div(
          class = "ynow-pa-modal__eyebrow",
          tags$span(class = "ynow-pa-modal__bell", HTML(
            paste0(
              '<svg viewBox="0 0 24 24" width="16" height="16" focusable="false">',
              '<path fill="currentColor" d="M12 22a2.2 2.2 0 0 0 2.2-2.2h-4.4A2.2 2.2 0 0 0 12 22zm6.4-6.2V11a6.4 6.4 0 0 0-5.1-6.3V4a1.3 1.3 0 1 0-2.6 0v.7A6.4 6.4 0 0 0 5.6 11v4.8L4 17.4V18h16v-.6l-1.6-1.6z"/>',
              "</svg>"
            )
          )),
          say("pa_quick_title")
        ),
        tags$div(class = "ynow-pa-modal__ticker", ticker),
        tags$div(
          class = "ynow-pa-modal__metric",
          tags$span(class = "ynow-pa-modal__metric-label", label),
          tags$span(class = "ynow-pa-modal__metric-value", disp_val)
        ),
        tags$p(class = "ynow-pa-modal__hint", say("pa_quick_hint"))
      ),
      tags$div(
        class = "ynow-pa-modal__body",
        selectInput("pa_quick_kind", say("pa_kind"), choices = kind_choices, selected = def_kind),
        level_input,
        if (isTRUE(show_mos)) {
          numericInput("pa_quick_mos", say("pa_mos"), value = mos_default, min = 0, max = 90, width = "100%")
        } else {
          tags$div(
            style = "display:none;",
            numericInput("pa_quick_mos", NULL, value = 0)
          )
        },
        tags$div(
          class = "ynow-pa-modal__channels",
          tags$p(class = "ynow-pa-modal__section", say("pa_quick_channels")),
          checkboxInput("pa_quick_notify_app", say("pa_notify_app"), value = TRUE),
          checkboxInput("pa_quick_notify_email", say("pa_notify_email"), value = FALSE),
          conditionalPanel(
            condition = "input.pa_quick_notify_email == true",
            textInput("pa_quick_email", say("pa_email"), value = Sys.getenv("YNOW_ALERT_TO", ""))
          )
        ),
        textInput("pa_quick_note", say("pa_note"), value = ""),
        tags$div(
          style = "display:none;",
          textInput("pa_quick_ticker", NULL, value = ticker),
          textInput("pa_quick_metric", NULL, value = metric)
        )
      )
    ),
    footer = tagList(
      modalButton(say("pa_quick_cancel")),
      actionButton("pa_quick_save", say("pa_quick_save"), class = "btn-primary ynow-pa-modal__save")
    )
  )
}

price_alert_server <- function(input, output, session, ui_locale_rv = NULL,
                               current_ticker_rv = NULL,
                               metric_snapshot_rv = NULL) {
  loc <- function() {
    if (is.null(ui_locale_rv)) return("en")
    tryCatch(normalize_ui_locale(isolate(ui_locale_rv())), error = function(e) "en")
  }
  say <- function(key) ui_str(key, loc())
  pa_refresh <- reactiveVal(0L)
  hist_refresh <- reactiveVal(0L)
  screener_server(
    input, output, session,
    ui_locale_rv = ui_locale_rv,
    say = say,
    loc = loc,
    pa_refresh = pa_refresh
  )
  .metrics_by_ticker <- function() {
    from_rv <- if (is.null(metric_snapshot_rv)) {
      NULL
    } else {
      tryCatch(metric_snapshot_rv(), error = function(e) NULL)
    }
    from_ud <- tryCatch(session$userData$value_alert_metrics, error = function(e) NULL)
    if (!is.list(from_rv) && !is.list(from_ud)) return(NULL)
    out <- list()
    for (tk in unique(c(names(from_rv %||% list()), names(from_ud %||% list())))) {
      out[[tk]] <- modifyList(
        as.list((from_rv %||% list())[[tk]] %||% list()),
        as.list((from_ud %||% list())[[tk]] %||% list())
      )
    }
    out
  }

  .deliver_app_toasts <- function(toasts) {
    if (!length(toasts)) return()
    for (t in toasts) {
      showNotification(
        as.character(t$message %||% say("pa_app_triggered"))[1],
        type = "warning",
        duration = 8
      )
    }
  }

  output$pa_table <- DT::renderDT({
    pa_refresh()
    df <- price_alert_read()
    if (!nrow(df)) {
      show <- data.frame(Note = say("pa_empty"), stringsAsFactors = FALSE)
    } else {
      ch <- ifelse(
        isTRUE(df$notify_app) & isTRUE(df$notify_email), "app+email",
        ifelse(isTRUE(df$notify_app), "app",
               ifelse(isTRUE(df$notify_email), "email", "—"))
      )
      show <- data.frame(
        Ticker = df$ticker,
        Screen = price_alert_screen_text(df$screen_ref),
        Condition = df$kind,
        Level = df$level,
        Channel = ch,
        Email = df$email,
        Status = df$status,
        Price = df$last_price,
        Sent = df$last_sent,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      names(show) <- c(
        say("pa_col_ticker"), say("pa_col_screen"), say("pa_col_kind"), say("pa_col_level"),
        say("pa_col_channel"), say("pa_col_email"), say("pa_col_status"),
        say("pa_col_price"), say("pa_col_sent")
      )
    }
    DT::datatable(
      show,
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 10, dom = "tp", scrollX = TRUE)
    )
  }, server = TRUE)

  output$pa_history_table <- DT::renderDT({
    hist_refresh()
    pa_refresh()
    df <- price_alert_history_read()
    if (!nrow(df)) {
      show <- data.frame(Note = say("pa_history_empty"), stringsAsFactors = FALSE)
    } else {
      df <- df[order(df$triggered_at, decreasing = TRUE), , drop = FALSE]
      show <- data.frame(
        When = df$triggered_at,
        Ticker = df$ticker,
        Condition = df$kind,
        Level = df$level,
        Price = df$price,
        Channel = df$channel,
        Message = df$message,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      names(show) <- c(
        say("pa_hist_col_when"), say("pa_col_ticker"), say("pa_col_kind"),
        say("pa_col_level"), say("pa_col_price"), say("pa_col_channel"),
        say("pa_hist_col_msg")
      )
    }
    DT::datatable(
      show,
      rownames = FALSE,
      selection = "none",
      options = list(pageLength = 10, dom = "tp", scrollX = TRUE)
    )
  }, server = TRUE)

  .pa_login_ok <- function() {
    # Lite never requires an account (fully usable without sign-in).
    if (isTRUE(tryCatch(isolate(input$ynow_lite_mode), error = function(e) FALSE))) {
      return(TRUE)
    }
    # Full: Value Alerts require a signed-in account when auth is enabled.
    if (!exists("ynow_auth_enabled", mode = "function") || !isTRUE(ynow_auth_enabled())) {
      return(TRUE)
    }
    auth <- tryCatch(session$userData$ynow_auth, error = function(e) NULL)
    isTRUE(tryCatch(auth$is_logged_in(), error = function(e) FALSE))
  }

  .pa_registered_email <- function() {
    if (!exists("ynow_auth_session_email", mode = "function")) return("")
    ynow_auth_session_email(tryCatch(session$userData$ynow_auth, error = function(e) NULL))
  }

  #' Prefer registered account mailbox for email notify (Full, signed-in).
  .pa_resolve_email <- function(form_email) {
    acct <- .pa_registered_email()
    if (nzchar(acct)) return(acct)
    trimws(as.character(form_email %||% "")[1])
  }

  .pa_require_login_or_notify <- function() {
    if (isTRUE(.pa_login_ok())) return(TRUE)
    showNotification(say("auth_pa_gate_title"), type = "warning")
    tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "open"), error = function(e) NULL)
    FALSE
  }

  .pa_points_gate_alert <- function(ticker, kind, level) {
    # Lite / points-off: no-op allow.
    if (isTRUE(tryCatch(isolate(input$ynow_lite_mode), error = function(e) FALSE))) {
      return(TRUE)
    }
    if (!exists("ynow_points_enabled", mode = "function") || !isTRUE(ynow_points_enabled())) {
      return(TRUE)
    }
    if (!exists("ynow_points_gate", mode = "function")) return(TRUE)
    auth <- tryCatch(session$userData$ynow_auth, error = function(e) NULL)
    u <- tryCatch(auth$user(), error = function(e) NULL)
    day <- format(Sys.Date(), "%Y-%m-%d")
    ref <- paste(
      "alert",
      as.character(u$id %||% "anon")[1],
      toupper(trimws(as.character(ticker %||% "")[1])),
      as.character(kind %||% "")[1],
      as.character(level %||% "")[1],
      day,
      sep = ":"
    )
    ynow_points_gate(session, "alert_slot", ref, user = u, locale = loc())
  }

  observeEvent(input$pa_save, {
    if (!isTRUE(.pa_require_login_or_notify())) return()
    if (!isTRUE(.pa_points_gate_alert(input$pa_ticker, input$pa_kind, input$pa_level))) return()
    email_val <- .pa_resolve_email(input$pa_email)
    res <- price_alert_add(
      price_alert_read(),
      email_val, input$pa_ticker, input$pa_kind,
      input$pa_level, input$pa_mos, input$pa_note,
      notify_app = isTRUE(input$pa_notify_app),
      notify_email = isTRUE(input$pa_notify_email),
      source = "form"
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
        no_channel = "pa_no_channel",
        "pa_bad_level"
      )
      showNotification(say(key), type = "warning")
    }
    pa_refresh(isolate(pa_refresh()) + 1L)
  })

  observeEvent(input$pa_check, {
    out <- price_alert_check_store(metrics_by_ticker = .metrics_by_ticker())
    .deliver_app_toasts(out$app_toasts %||% list())
    msg <- say("pa_checked")
    if (out$met > 0L && out$sent == 0L && any(vapply(out$app_toasts %||% list(), function(x) TRUE, logical(1))) == FALSE) {
      # met but neither email nor app delivered meaningfully
      if (!length(out$app_toasts)) msg <- paste(msg, say("pa_smtp_missing"))
    }
    showNotification(msg, type = if (out$sent > 0L || length(out$app_toasts)) "message" else "warning")
    pa_refresh(isolate(pa_refresh()) + 1L)
    hist_refresh(isolate(hist_refresh()) + 1L)
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

  observeEvent(input$pa_history_clear, {
    price_alert_history_write(price_alert_history_empty())
    hist_refresh(isolate(hist_refresh()) + 1L)
    showNotification(say("pa_history_cleared"), type = "message")
  })

  # Quick-set from KPI gesture
  observeEvent(input$ynow_kpi_alert_open, {
    payload <- input$ynow_kpi_alert_open
    if (is.null(payload) || !is.list(payload)) return()
    if (!isTRUE(.pa_require_login_or_notify())) return()
    ready <- as.character(payload$ready %||% "0")[1]
    if (!identical(ready, "1")) {
      showNotification(say("pa_quick_not_ready"), type = "warning", duration = 5)
      return()
    }
    value <- suppressWarnings(as.numeric(payload$value %||% NA_real_)[1])
    if (!is.finite(value)) {
      showNotification(say("pa_quick_not_ready"), type = "warning", duration = 5)
      return()
    }
    showModal(price_alert_quick_modal(payload, locale = loc()))
    # Prefill / lock to registered mailbox when signed in
    acct <- .pa_registered_email()
    if (nzchar(acct)) {
      tryCatch(updateTextInput(session, "pa_quick_email", value = acct), error = function(e) NULL)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_kpi_alert_blocked, {
    if (!isTRUE(.pa_login_ok())) {
      showNotification(say("auth_pa_gate_title"), type = "warning", duration = 5)
      tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "open"), error = function(e) NULL)
      return()
    }
    showNotification(say("pa_quick_not_ready"), type = "warning", duration = 5)
  }, ignoreInit = TRUE)

  observeEvent(input$pa_quick_save, {
    if (!isTRUE(.pa_require_login_or_notify())) return()
    if (!isTRUE(.pa_points_gate_alert(
      input$pa_quick_ticker, input$pa_quick_kind, input$pa_quick_level
    ))) {
      return()
    }
    res <- price_alert_add(
      price_alert_read(),
      email = .pa_resolve_email(input$pa_quick_email %||% ""),
      ticker = input$pa_quick_ticker,
      kind = input$pa_quick_kind,
      level = input$pa_quick_level,
      mos_pct = input$pa_quick_mos %||% 0,
      note = input$pa_quick_note %||% "",
      notify_app = isTRUE(input$pa_quick_notify_app),
      notify_email = isTRUE(input$pa_quick_notify_email),
      source = "kpi_quick",
      metric = as.character(input$pa_quick_metric %||% "")[1]
    )
    if (isTRUE(res$ok)) {
      price_alert_write(res$df)
      removeModal()
      showNotification(say("pa_saved"), type = "message")
    } else {
      key <- switch(
        res$reason,
        bad_email = "pa_bad_email",
        bad_ticker = "pa_bad_ticker",
        bad_level = "pa_bad_level",
        no_channel = "pa_no_channel",
        "pa_bad_level"
      )
      showNotification(say(key), type = "warning")
    }
    pa_refresh(isolate(pa_refresh()) + 1L)
  })

  # Only keep a 15-minute timer while at least one alert is armed — avoids
  # waking every idle Starter session every quarter hour.
  observe({
    df <- price_alert_read()
    armed <- isTRUE(nrow(df) > 0L) && any(df$armed %in% TRUE)
    if (!armed) {
      # Re-check when store mutates via pa_refresh; no standing invalidateLater.
      pa_refresh()
      return()
    }
    invalidateLater(15 * 60 * 1000, session)
    out <- price_alert_check_store(metrics_by_ticker = .metrics_by_ticker())
    .deliver_app_toasts(out$app_toasts %||% list())
    pa_refresh(isolate(pa_refresh()) + 1L)
    hist_refresh(isolate(hist_refresh()) + 1L)
  })
}
