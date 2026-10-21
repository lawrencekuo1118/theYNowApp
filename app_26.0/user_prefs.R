# ==========================================
# Durable per-user preferences (Supabase Postgres)
# ==========================================
# Primary store: table `ynow_user_prefs` via YNOW_DB_* (same backend as ynow_users).
# Fallback when DB is off: in-process memory + local JSON file (same pattern as
# access_locks) so demo / memory-auth users still keep pins across logout/login
# within one deploy host. Guests never write here — client localStorage only.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

# Pinned tickers from Recent Search: max 12 → 4×3 grid.
# Page-pin UX removed (#355 misread); tab long-press is reorder-edit only.
YNOW_PREF_PINNED_RECENT <- "pinned_recent_search"
.YNOW_PINNED_RECENT_MAX <- 12L

# Signed-in rolling search history (ticker lookups). Separate key from:
# - pinned_recent_search (stock chips long-press pin)
# - sidebar_tab_order (signed-in menu reorder)
# Additive only on ynow_user_prefs.
YNOW_PREF_RECENT_SEARCH_24H <- "recent_search_history_24h"
.YNOW_RECENT_SEARCH_24H_MAX <- 20L
.YNOW_RECENT_SEARCH_24H_SECS <- 24L * 3600L

# Sidebar menu tab pins (leaf tabNames): max 12 → 3-column tray.
# Page-pin UX is disabled (misread vs stock pins); API retained for stored prefs.
YNOW_PREF_PINNED_SIDEBAR <- "pinned_sidebar_tabs"
.YNOW_PINNED_SIDEBAR_MAX <- 12L

# Signed-in top-level sidebar menu order (edit mode + long-press drag).
YNOW_PREF_SIDEBAR_TAB_ORDER <- "sidebar_tab_order"

# Signed-in Settings tab (hide locked items, locale/market prefs, density).
# Single JSON object under ynow_user_prefs; guests never write here.
YNOW_PREF_USER_SETTINGS <- "user_settings"

.ynow_prefs_mem <- new.env(parent = emptyenv())
.ynow_prefs_mem$by_user <- list()

.ynow_prefs_db_on <- function() {
  exists("ynow_db_ready", mode = "function") &&
    isTRUE(ynow_db_ready()) &&
    exists("ynow_db_with", mode = "function") &&
    !identical(Sys.getenv("YNOW_USER_PREFS_FORCE_FILE", ""), "1")
}

.ynow_prefs_file <- function() {
  root <- tryCatch(
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    error = function(e) tempdir()
  )
  file.path(root, "ynow_data", "user_prefs.json")
}

.ynow_prefs_read_file <- function() {
  path <- .ynow_prefs_file()
  if (!file.exists(path)) return(list())
  raw <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.list(raw)) return(list())
  raw
}

.ynow_prefs_write_file <- function(store) {
  path <- .ynow_prefs_file()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(store, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

#' Normalize a ticker list for pinned recent search.
ynow_prefs_normalize_tickers <- function(tickers, max_n = .YNOW_PINNED_RECENT_MAX) {
  raw <- unique(toupper(trimws(as.character(tickers %||% character(0)))))
  raw <- raw[nzchar(raw) & !is.na(raw)]
  # Reject pathological tokens (spaces, control chars).
  raw <- raw[grepl("^[A-Z0-9.\\-]{1,24}$", raw)]
  head(raw, as.integer(max_n)[1])
}

#' Leaf sidebar tabNames that may be pinned (order not significant).
#' Categories / treeview parents without a tabName are excluded.
ynow_sidebar_pinnable_tabs <- function() {
  c(
    "home", "macro_market", "asset_transmission", "bluechip", "dashboard",
    "company_advance", "sensitivity", "smart_analysis", "get_started",
    "nav_calculator", "pb_calculator", "dcf_calculator", "ddm_calculator",
    "ri_calculator", "rel_mult_ev", "rel_multiples_calculator", "rel_mult_rev",
    "sotp_calculator", "hfv", "lab_notes", "decision_checklist", "price_alerts",
    "about", "settings", "snapshot", "feedback", "account_admin", "account_points"
  )
}

#' Locale key + FA icon for one pinnable sidebar tab.
ynow_sidebar_pin_meta <- function(tab) {
  t <- as.character(tab %||% "")[1]
  meta <- list(
    home = list(locale = "menu_home", icon = "home", short_en = "Home", short_zh = "首頁"),
    macro_market = list(locale = "menu_macro_market", icon = "globe-asia", short_en = "Macro", short_zh = "總體"),
    asset_transmission = list(locale = "menu_asset_transmission", icon = "project-diagram", short_en = "ATX", short_zh = "傳導"),
    bluechip = list(locale = "menu_bluechip", icon = "star", short_en = "Blue Chip", short_zh = "績優"),
    dashboard = list(locale = "menu_dashboard", icon = "chart-line", short_en = "Company", short_zh = "個股"),
    company_advance = list(locale = "menu_company_advance", icon = "layer-group", short_en = "BB", short_zh = "拆解"),
    sensitivity = list(locale = "menu_ynow", icon = "clock", short_en = "YNOW", short_zh = "YNOW"),
    smart_analysis = list(locale = "menu_smart_analysis", icon = "magic", short_en = "Smart", short_zh = "智慧"),
    get_started = list(locale = "menu_get_started", icon = "play-circle", short_en = "Models", short_zh = "儀表板"),
    nav_calculator = list(locale = "menu_nav", icon = "sitemap", short_en = "NAV", short_zh = "NAV"),
    pb_calculator = list(locale = "menu_pb", icon = "landmark", short_en = "P/B", short_zh = "P/B"),
    dcf_calculator = list(locale = "menu_dcf", icon = "calculator", short_en = "DCF", short_zh = "DCF"),
    ddm_calculator = list(locale = "menu_ddm", icon = "hand-holding-usd", short_en = "DDM", short_zh = "DDM"),
    ri_calculator = list(locale = "menu_ri", icon = "gem", short_en = "RI", short_zh = "RI"),
    rel_mult_ev = list(locale = "menu_rel_ev", icon = "building", short_en = "EV", short_zh = "EV"),
    rel_multiples_calculator = list(locale = "menu_rel_earn", icon = "chart-line", short_en = "Earn", short_zh = "盈餘"),
    rel_mult_rev = list(locale = "menu_rel_rev", icon = "shopping-cart", short_en = "Rev", short_zh = "營收"),
    sotp_calculator = list(locale = "menu_sotp", icon = "puzzle-piece", short_en = "SOTP", short_zh = "SOTP"),
    hfv = list(locale = "menu_hfv", icon = "balance-scale", short_en = "HFV", short_zh = "HFV"),
    lab_notes = list(locale = "menu_backtest", icon = "flask", short_en = "Lab", short_zh = "回測"),
    decision_checklist = list(locale = "menu_decision_checklist", icon = "clipboard-check", short_en = "Check", short_zh = "檢核"),
    price_alerts = list(locale = "menu_price_alert", icon = "bell", short_en = "Alerts", short_zh = "通知"),
    about = list(locale = "menu_about", icon = "info-circle", short_en = "About", short_zh = "關於"),
    settings = list(locale = "menu_settings", icon = "cog", short_en = "Settings", short_zh = "設定"),
    snapshot = list(locale = "snapshot_link", icon = "camera", short_en = "Snap", short_zh = "快照"),
    feedback = list(locale = "feedback_link", icon = "comment-dots", short_en = "Feedback", short_zh = "意見"),
    account_admin = list(locale = "menu_admin", icon = "user-cog", short_en = "Account", short_zh = "帳號"),
    account_points = list(locale = "menu_pts_ledger", icon = "coins", short_en = "Points", short_zh = "點數")
  )
  meta[[t]] %||% list(locale = "", icon = "thumbtack", short_en = t, short_zh = t)
}

#' Normalize sidebar tab pin list (allowed leaf tabs only; preserve order; max 12).
ynow_prefs_normalize_sidebar_tabs <- function(tabs, max_n = .YNOW_PINNED_SIDEBAR_MAX) {
  allow <- ynow_sidebar_pinnable_tabs()
  raw <- as.character(tabs %||% character(0))
  raw <- trimws(raw)
  raw <- raw[nzchar(raw) & !is.na(raw)]
  raw <- raw[raw %in% allow]
  # Unique preserving first occurrence (pin order).
  raw <- raw[!duplicated(raw)]
  head(raw, as.integer(max_n)[1])
}

#' Default top-level sidebar order ids (leaves + category treeviews).
#' Category parents without a tabName use synthetic ids (`cat_asset`, …).
ynow_sidebar_default_order <- function() {
  c(
    "home", "macro_market", "asset_transmission", "bluechip", "dashboard",
    "company_advance", "sensitivity", "smart_analysis", "get_started",
    "cat_asset", "cat_income", "cat_relative", "cat_meta",
    "hfv", "lab_notes", "decision_checklist", "price_alerts", "about",
    "settings", "account_admin"
  )
}

#' Allowed order-id vocabulary for sidebar_tab_order prefs.
ynow_sidebar_orderable_ids <- function() {
  ynow_sidebar_default_order()
}

#' Normalize a custom sidebar order: known ids only, unique, defaults fill gaps.
ynow_prefs_normalize_sidebar_order <- function(ids, fill_defaults = TRUE) {
  allow <- ynow_sidebar_orderable_ids()
  raw <- as.character(ids %||% character(0))
  raw <- trimws(raw)
  raw <- raw[nzchar(raw) & !is.na(raw)]
  raw <- raw[raw %in% allow]
  raw <- raw[!duplicated(raw)]
  if (!isTRUE(fill_defaults)) return(raw)
  c(raw, setdiff(ynow_sidebar_default_order(), raw))
}

ynow_prefs_ensure_schema <- function(conn = NULL) {
  run <- function(con) {
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS ynow_user_prefs (
        user_id TEXT NOT NULL,
        pref_key TEXT NOT NULL,
        pref_value JSONB NOT NULL DEFAULT '[]'::jsonb,
        updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        PRIMARY KEY (user_id, pref_key)
      )
    ")
    DBI::dbExecute(con, "
      CREATE INDEX IF NOT EXISTS ynow_user_prefs_user_idx
        ON ynow_user_prefs (user_id)
    ")
    if (exists(".ynow_db_rls_fail_closed", mode = "function")) {
      tryCatch(.ynow_db_rls_fail_closed(con, "ynow_user_prefs"), error = function(e) NULL)
    }
    TRUE
  }
  if (!is.null(conn)) return(run(conn))
  if (!.ynow_prefs_db_on()) return(invisible(FALSE))
  out <- ynow_db_with(run)
  invisible(isTRUE(out))
}

.ynow_prefs_parse_json_tickers <- function(raw) {
  if (is.null(raw)) return(character(0))
  if (is.character(raw) && length(raw) == 1L) {
    parsed <- tryCatch(
      jsonlite::fromJSON(raw, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed)) return(character(0))
    return(ynow_prefs_normalize_tickers(parsed))
  }
  if (is.list(raw) || is.character(raw)) {
    return(ynow_prefs_normalize_tickers(unlist(raw, use.names = FALSE)))
  }
  character(0)
}

#' Read pinned recent-search tickers for a signed-in user id.
ynow_prefs_get_pinned_recent <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character(0))
  if (.ynow_prefs_db_on()) {
    ynow_prefs_ensure_schema()
    df <- ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT pref_value::text AS pref_value",
          "FROM ynow_user_prefs",
          "WHERE user_id = $1 AND pref_key = $2",
          "LIMIT 1"
        ),
        params = list(uid, YNOW_PREF_PINNED_RECENT)
      )
    })
    if (is.data.frame(df) && nrow(df) >= 1L) {
      return(.ynow_prefs_parse_json_tickers(df$pref_value[[1]]))
    }
    return(character(0))
  }
  mem <- .ynow_prefs_mem$by_user[[uid]]
  if (is.null(mem)) {
    store <- .ynow_prefs_read_file()
    mem <- store[[uid]]
  }
  if (is.list(mem) && !is.null(mem[[YNOW_PREF_PINNED_RECENT]])) {
    return(ynow_prefs_normalize_tickers(mem[[YNOW_PREF_PINNED_RECENT]]))
  }
  if (is.character(mem)) return(ynow_prefs_normalize_tickers(mem))
  character(0)
}

#' Persist pinned recent-search tickers for a signed-in user id.
ynow_prefs_set_pinned_recent <- function(user_id, tickers) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character(0))
  pins <- ynow_prefs_normalize_tickers(tickers)
  payload <- jsonlite::toJSON(as.list(pins), auto_unbox = TRUE)
  if (.ynow_prefs_db_on()) {
    ynow_prefs_ensure_schema()
    ynow_db_with(function(con) {
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO ynow_user_prefs (user_id, pref_key, pref_value, updated_at)",
          "VALUES ($1, $2, $3::jsonb, NOW())",
          "ON CONFLICT (user_id, pref_key) DO UPDATE",
          "SET pref_value = EXCLUDED.pref_value, updated_at = NOW()"
        ),
        params = list(uid, YNOW_PREF_PINNED_RECENT, as.character(payload)[1])
      )
    })
    return(pins)
  }
  store <- .ynow_prefs_read_file()
  cur <- .ynow_prefs_mem$by_user[[uid]]
  if (!is.list(cur)) cur <- store[[uid]]
  if (!is.list(cur)) cur <- list()
  cur[[YNOW_PREF_PINNED_RECENT]] <- as.list(pins)
  .ynow_prefs_mem$by_user[[uid]] <- cur
  store[[uid]] <- cur
  .ynow_prefs_write_file(store)
  pins
}

#' Toggle one ticker in the pinned list. Returns the new pinned vector.
#' When already at max (12) and adding a new ticker, returns current list
#' unchanged; attribute `at_cap` is TRUE in that case.
ynow_prefs_toggle_pinned_recent <- function(user_id, ticker) {
  tk <- ynow_prefs_normalize_tickers(ticker, max_n = 1L)
  if (!length(tk)) {
    out <- ynow_prefs_get_pinned_recent(user_id)
    attr(out, "at_cap") <- FALSE
    return(out)
  }
  cur <- ynow_prefs_get_pinned_recent(user_id)
  if (tk[[1]] %in% cur) {
    nxt <- ynow_prefs_set_pinned_recent(user_id, setdiff(cur, tk[[1]]))
    attr(nxt, "at_cap") <- FALSE
    return(nxt)
  }
  if (length(cur) >= .YNOW_PINNED_RECENT_MAX) {
    attr(cur, "at_cap") <- TRUE
    return(cur)
  }
  nxt <- ynow_prefs_set_pinned_recent(
    user_id,
    c(tk[[1]], setdiff(cur, tk[[1]]))
  )
  attr(nxt, "at_cap") <- FALSE
  nxt
}

.ynow_prefs_parse_json_tabs <- function(raw) {
  if (is.null(raw)) return(character(0))
  if (is.character(raw) && length(raw) == 1L) {
    parsed <- tryCatch(
      jsonlite::fromJSON(raw, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed)) return(character(0))
    return(ynow_prefs_normalize_sidebar_tabs(parsed))
  }
  if (is.list(raw) || is.character(raw)) {
    return(ynow_prefs_normalize_sidebar_tabs(unlist(raw, use.names = FALSE)))
  }
  character(0)
}

#' Read pinned sidebar tabNames for a signed-in user id.
ynow_prefs_get_pinned_sidebar <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character(0))
  if (.ynow_prefs_db_on()) {
    ynow_prefs_ensure_schema()
    df <- ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT pref_value::text AS pref_value",
          "FROM ynow_user_prefs",
          "WHERE user_id = $1 AND pref_key = $2",
          "LIMIT 1"
        ),
        params = list(uid, YNOW_PREF_PINNED_SIDEBAR)
      )
    })
    if (is.data.frame(df) && nrow(df) >= 1L) {
      return(.ynow_prefs_parse_json_tabs(df$pref_value[[1]]))
    }
    return(character(0))
  }
  mem <- .ynow_prefs_mem$by_user[[uid]]
  if (is.null(mem)) {
    store <- .ynow_prefs_read_file()
    mem <- store[[uid]]
  }
  if (is.list(mem) && !is.null(mem[[YNOW_PREF_PINNED_SIDEBAR]])) {
    return(ynow_prefs_normalize_sidebar_tabs(mem[[YNOW_PREF_PINNED_SIDEBAR]]))
  }
  character(0)
}

#' Persist pinned sidebar tabNames for a signed-in user id.
ynow_prefs_set_pinned_sidebar <- function(user_id, tabs) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character(0))
  pins <- ynow_prefs_normalize_sidebar_tabs(tabs)
  payload <- jsonlite::toJSON(as.list(pins), auto_unbox = TRUE)
  if (.ynow_prefs_db_on()) {
    ynow_prefs_ensure_schema()
    ynow_db_with(function(con) {
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO ynow_user_prefs (user_id, pref_key, pref_value, updated_at)",
          "VALUES ($1, $2, $3::jsonb, NOW())",
          "ON CONFLICT (user_id, pref_key) DO UPDATE",
          "SET pref_value = EXCLUDED.pref_value, updated_at = NOW()"
        ),
        params = list(uid, YNOW_PREF_PINNED_SIDEBAR, as.character(payload)[1])
      )
    })
    return(pins)
  }
  store <- .ynow_prefs_read_file()
  cur <- .ynow_prefs_mem$by_user[[uid]]
  if (!is.list(cur)) cur <- store[[uid]]
  if (!is.list(cur)) cur <- list()
  cur[[YNOW_PREF_PINNED_SIDEBAR]] <- as.list(pins)
  .ynow_prefs_mem$by_user[[uid]] <- cur
  store[[uid]] <- cur
  .ynow_prefs_write_file(store)
  pins
}

#' Toggle one sidebar tab in the pinned list. Returns the new pinned vector.
#' When already at max and adding a new tab, returns current list unchanged
#' (caller may toast); attribute `at_cap` is TRUE in that case.
ynow_prefs_toggle_pinned_sidebar <- function(user_id, tab) {
  tk <- ynow_prefs_normalize_sidebar_tabs(tab, max_n = 1L)
  if (!length(tk)) {
    out <- ynow_prefs_get_pinned_sidebar(user_id)
    attr(out, "at_cap") <- FALSE
    return(out)
  }
  cur <- ynow_prefs_get_pinned_sidebar(user_id)
  if (tk[[1]] %in% cur) {
    nxt <- ynow_prefs_set_pinned_sidebar(user_id, setdiff(cur, tk[[1]]))
    attr(nxt, "at_cap") <- FALSE
    return(nxt)
  }
  if (length(cur) >= .YNOW_PINNED_SIDEBAR_MAX) {
    attr(cur, "at_cap") <- TRUE
    return(cur)
  }
  nxt <- ynow_prefs_set_pinned_sidebar(
    user_id,
    c(tk[[1]], setdiff(cur, tk[[1]]))
  )
  attr(nxt, "at_cap") <- FALSE
  nxt
}

.ynow_prefs_parse_json_order <- function(raw) {
  if (is.null(raw)) return(character(0))
  if (is.character(raw) && length(raw) == 1L) {
    parsed <- tryCatch(
      jsonlite::fromJSON(raw, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed)) return(character(0))
    return(ynow_prefs_normalize_sidebar_order(parsed, fill_defaults = TRUE))
  }
  if (is.list(raw) || is.character(raw)) {
    return(ynow_prefs_normalize_sidebar_order(
      unlist(raw, use.names = FALSE),
      fill_defaults = TRUE
    ))
  }
  character(0)
}

#' Read custom sidebar tab order for a signed-in user (defaults filled).
ynow_prefs_get_sidebar_tab_order <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(ynow_sidebar_default_order())
  if (.ynow_prefs_db_on()) {
    ynow_prefs_ensure_schema()
    df <- ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT pref_value::text AS pref_value",
          "FROM ynow_user_prefs",
          "WHERE user_id = $1 AND pref_key = $2",
          "LIMIT 1"
        ),
        params = list(uid, YNOW_PREF_SIDEBAR_TAB_ORDER)
      )
    })
    if (is.data.frame(df) && nrow(df) >= 1L) {
      return(.ynow_prefs_parse_json_order(df$pref_value[[1]]))
    }
    return(ynow_sidebar_default_order())
  }
  mem <- .ynow_prefs_mem$by_user[[uid]]
  if (is.null(mem)) {
    store <- .ynow_prefs_read_file()
    mem <- store[[uid]]
  }
  if (is.list(mem) && !is.null(mem[[YNOW_PREF_SIDEBAR_TAB_ORDER]])) {
    return(ynow_prefs_normalize_sidebar_order(mem[[YNOW_PREF_SIDEBAR_TAB_ORDER]]))
  }
  ynow_sidebar_default_order()
}

#' Persist custom sidebar tab order for a signed-in user id.
ynow_prefs_set_sidebar_tab_order <- function(user_id, ids) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character(0))
  order_ids <- ynow_prefs_normalize_sidebar_order(ids, fill_defaults = TRUE)
  payload <- jsonlite::toJSON(as.list(order_ids), auto_unbox = TRUE)
  if (.ynow_prefs_db_on()) {
    ynow_prefs_ensure_schema()
    ynow_db_with(function(con) {
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO ynow_user_prefs (user_id, pref_key, pref_value, updated_at)",
          "VALUES ($1, $2, $3::jsonb, NOW())",
          "ON CONFLICT (user_id, pref_key) DO UPDATE",
          "SET pref_value = EXCLUDED.pref_value, updated_at = NOW()"
        ),
        params = list(uid, YNOW_PREF_SIDEBAR_TAB_ORDER, as.character(payload)[1])
      )
    })
    return(order_ids)
  }
  store <- .ynow_prefs_read_file()
  cur <- .ynow_prefs_mem$by_user[[uid]]
  if (!is.list(cur)) cur <- store[[uid]]
  if (!is.list(cur)) cur <- list()
  cur[[YNOW_PREF_SIDEBAR_TAB_ORDER]] <- as.list(order_ids)
  .ynow_prefs_mem$by_user[[uid]] <- cur
  store[[uid]] <- cur
  .ynow_prefs_write_file(store)
  order_ids
}

# ---- Recent search history (signed-in, rolling 24h) ----

.ynow_prefs_now_posix <- function() {
  as.POSIXct(Sys.time(), tz = "UTC")
}

.ynow_prefs_parse_iso_time <- function(x) {
  raw <- as.character(x %||% "")[1]
  if (!nzchar(raw)) return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  # Accept ISO-8601 with trailing Z.
  raw <- sub("Z$", "", raw, ignore.case = TRUE)
  parsed <- tryCatch(
    as.POSIXct(raw, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
    error = function(e) as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  )
  if (is.na(parsed)) {
    parsed <- tryCatch(
      as.POSIXct(raw, tz = "UTC"),
      error = function(e) as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
    )
  }
  parsed
}

.ynow_prefs_format_iso_time <- function(t) {
  format(as.POSIXct(t, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Normalize timed recent-search entries; drop older than 24h; newest first; dedupe.
#'
#' @param entries list of lists with `ticker` / `at`, or a character ticker vector
#'   (treated as "now").
#' @param now POSIXct reference time (UTC).
#' @return list of list(ticker=, at=) within the rolling window.
ynow_prefs_normalize_recent_entries <- function(entries,
                                                now = .ynow_prefs_now_posix(),
                                                max_n = .YNOW_RECENT_SEARCH_24H_MAX,
                                                window_secs = .YNOW_RECENT_SEARCH_24H_SECS) {
  now <- as.POSIXct(now, tz = "UTC")
  cutoff <- now - as.numeric(window_secs)[1]
  out <- list()
  if (is.character(entries)) {
    tickers <- ynow_prefs_normalize_tickers(entries, max_n = max_n)
    if (!length(tickers)) return(list())
    at <- .ynow_prefs_format_iso_time(now)
    return(lapply(tickers, function(tk) list(ticker = tk, at = at)))
  }
  if (!is.list(entries) || !length(entries)) return(list())
  for (el in entries) {
    if (is.character(el) && length(el) >= 1L) {
      tk <- ynow_prefs_normalize_tickers(el[[1]], max_n = 1L)
      if (!length(tk)) next
      out[[length(out) + 1L]] <- list(
        ticker = tk[[1]],
        at = .ynow_prefs_format_iso_time(now)
      )
      next
    }
    if (!is.list(el)) next
    tk <- ynow_prefs_normalize_tickers(el$ticker %||% el[[1]], max_n = 1L)
    if (!length(tk)) next
    at_raw <- el$at %||% el$ts %||% el$time %||% NA
    at <- .ynow_prefs_parse_iso_time(at_raw)
    if (is.na(at)) at <- now
    if (at < cutoff) next
    out[[length(out) + 1L]] <- list(
      ticker = tk[[1]],
      at = .ynow_prefs_format_iso_time(at)
    )
  }
  if (!length(out)) return(list())
  # Newest first; keep first occurrence per ticker (most recent).
  ords <- order(
    vapply(out, function(x) as.numeric(.ynow_prefs_parse_iso_time(x$at)), numeric(1)),
    decreasing = TRUE
  )
  out <- out[ords]
  seen <- character(0)
  kept <- list()
  for (el in out) {
    tk <- el$ticker
    if (tk %in% seen) next
    seen <- c(seen, tk)
    kept[[length(kept) + 1L]] <- el
    if (length(kept) >= as.integer(max_n)[1]) break
  }
  kept
}

ynow_prefs_recent_entries_tickers <- function(entries) {
  ents <- ynow_prefs_normalize_recent_entries(entries)
  if (!length(ents)) return(character(0))
  vapply(ents, function(x) as.character(x$ticker)[1], character(1))
}

.ynow_prefs_parse_json_recent_entries <- function(raw) {
  if (is.null(raw)) return(list())
  if (is.character(raw) && length(raw) == 1L) {
    parsed <- tryCatch(
      jsonlite::fromJSON(raw, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) return(list())
    return(ynow_prefs_normalize_recent_entries(parsed))
  }
  if (is.list(raw)) return(ynow_prefs_normalize_recent_entries(raw))
  if (is.character(raw)) return(ynow_prefs_normalize_recent_entries(raw))
  list()
}

.ynow_prefs_read_pref_raw <- function(user_id, pref_key) {
  uid <- as.character(user_id %||% "")[1]
  key <- as.character(pref_key %||% "")[1]
  if (!nzchar(uid) || !nzchar(key)) return(NULL)
  if (.ynow_prefs_db_on()) {
    ynow_prefs_ensure_schema()
    df <- ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT pref_value::text AS pref_value",
          "FROM ynow_user_prefs",
          "WHERE user_id = $1 AND pref_key = $2",
          "LIMIT 1"
        ),
        params = list(uid, key)
      )
    })
    if (is.data.frame(df) && nrow(df) >= 1L) return(df$pref_value[[1]])
    return(NULL)
  }
  mem <- .ynow_prefs_mem$by_user[[uid]]
  if (is.null(mem)) {
    store <- .ynow_prefs_read_file()
    mem <- store[[uid]]
  }
  if (is.list(mem) && !is.null(mem[[key]])) return(mem[[key]])
  NULL
}

.ynow_prefs_write_pref_json <- function(user_id, pref_key, value_obj) {
  uid <- as.character(user_id %||% "")[1]
  key <- as.character(pref_key %||% "")[1]
  if (!nzchar(uid) || !nzchar(key)) return(invisible(FALSE))
  payload <- jsonlite::toJSON(value_obj, auto_unbox = TRUE)
  if (.ynow_prefs_db_on()) {
    ynow_prefs_ensure_schema()
    ynow_db_with(function(con) {
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO ynow_user_prefs (user_id, pref_key, pref_value, updated_at)",
          "VALUES ($1, $2, $3::jsonb, NOW())",
          "ON CONFLICT (user_id, pref_key) DO UPDATE",
          "SET pref_value = EXCLUDED.pref_value, updated_at = NOW()"
        ),
        params = list(uid, key, as.character(payload)[1])
      )
    })
    return(invisible(TRUE))
  }
  store <- .ynow_prefs_read_file()
  cur <- .ynow_prefs_mem$by_user[[uid]]
  if (!is.list(cur)) cur <- store[[uid]]
  if (!is.list(cur)) cur <- list()
  cur[[key]] <- value_obj
  .ynow_prefs_mem$by_user[[uid]] <- cur
  store[[uid]] <- cur
  .ynow_prefs_write_file(store)
  invisible(TRUE)
}

#' Timed recent-search entries for a signed-in user (already pruned to 24h).
ynow_prefs_get_recent_search_entries <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(list())
  raw <- .ynow_prefs_read_pref_raw(uid, YNOW_PREF_RECENT_SEARCH_24H)
  .ynow_prefs_parse_json_recent_entries(raw)
}

#' Tickers only (newest first) within the rolling 24h window.
ynow_prefs_get_recent_search_24h <- function(user_id) {
  ynow_prefs_recent_entries_tickers(ynow_prefs_get_recent_search_entries(user_id))
}

#' Replace the signed-in 24h history with normalized entries / tickers.
ynow_prefs_set_recent_search_24h <- function(user_id, entries) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character(0))
  ents <- ynow_prefs_normalize_recent_entries(entries)
  .ynow_prefs_write_pref_json(uid, YNOW_PREF_RECENT_SEARCH_24H, ents)
  ynow_prefs_recent_entries_tickers(ents)
}

#' Record one successful ticker lookup for a signed-in user (bumps to front / now).
ynow_prefs_record_recent_search <- function(user_id, ticker, at = .ynow_prefs_now_posix()) {
  uid <- as.character(user_id %||% "")[1]
  tk <- ynow_prefs_normalize_tickers(ticker, max_n = 1L)
  if (!nzchar(uid) || !length(tk)) {
    return(ynow_prefs_get_recent_search_24h(uid))
  }
  cur <- ynow_prefs_get_recent_search_entries(uid)
  # Drop prior rows for this ticker so the new timestamp wins.
  cur <- Filter(function(el) !identical(el$ticker, tk[[1]]), cur)
  cur <- c(
    list(list(ticker = tk[[1]], at = .ynow_prefs_format_iso_time(at))),
    cur
  )
  ynow_prefs_set_recent_search_24h(uid, cur)
}

#' Merge guest/session tickers into durable 24h history (login handoff).
#' Guest tickers are stamped "now"; server entries keep their timestamps.
ynow_prefs_merge_recent_search_24h <- function(user_id, guest_tickers) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character(0))
  server <- ynow_prefs_get_recent_search_entries(uid)
  guest <- ynow_prefs_normalize_tickers(guest_tickers)
  if (!length(guest)) {
    return(ynow_prefs_recent_entries_tickers(server))
  }
  now_at <- .ynow_prefs_format_iso_time(.ynow_prefs_now_posix())
  guest_ents <- lapply(guest, function(tk) list(ticker = tk, at = now_at))
  # Guest session wins ordering for overlap (just searched this session).
  ynow_prefs_set_recent_search_24h(uid, c(guest_ents, server))
}

# ---- Signed-in Settings (user_settings JSON) ----

#' Default Settings values (hide-locked OFF so post-login lock chrome stays visible).
ynow_prefs_default_user_settings <- function() {
  list(
    hide_locked_sidebar = FALSE,
    preferred_ui_locale = "auto",
    preferred_market = "auto",
    sidebar_compact = FALSE
  )
}

#' Normalize a Settings object; unknown keys dropped; defaults fill gaps.
ynow_prefs_normalize_user_settings <- function(raw) {
  def <- ynow_prefs_default_user_settings()
  if (is.null(raw)) return(def)
  if (is.character(raw) && length(raw) == 1L) {
    raw <- tryCatch(
      jsonlite::fromJSON(raw, simplifyVector = FALSE),
      error = function(e) NULL
    )
  }
  if (!is.list(raw)) return(def)
  hide <- raw$hide_locked_sidebar %||% raw$hide_locked %||% def$hide_locked_sidebar
  hide <- isTRUE(hide) || identical(tolower(as.character(hide)[1]), "true") ||
    identical(as.character(hide)[1], "1")
  loc <- tolower(trimws(as.character(raw$preferred_ui_locale %||% def$preferred_ui_locale)[1]))
  if (loc %in% c("zh", "zh_tw", "zh-tw", "zhtw", "tw")) loc <- "zh-TW"
  else if (loc %in% c("en", "en-us", "en_us")) loc <- "en"
  else loc <- "auto"
  mkt <- toupper(trimws(as.character(raw$preferred_market %||% def$preferred_market)[1]))
  if (mkt %in% c("US", "TW")) {
    # keep
  } else {
    mkt <- "auto"
  }
  compact <- raw$sidebar_compact %||% def$sidebar_compact
  compact <- isTRUE(compact) || identical(tolower(as.character(compact)[1]), "true") ||
    identical(as.character(compact)[1], "1")
  list(
    hide_locked_sidebar = hide,
    preferred_ui_locale = loc,
    preferred_market = mkt,
    sidebar_compact = compact
  )
}

.ynow_prefs_parse_json_user_settings <- function(raw) {
  ynow_prefs_normalize_user_settings(raw)
}

#' Read Settings for a signed-in user (defaults when missing).
ynow_prefs_get_user_settings <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(ynow_prefs_default_user_settings())
  raw <- .ynow_prefs_read_pref_raw(uid, YNOW_PREF_USER_SETTINGS)
  ynow_prefs_normalize_user_settings(raw)
}

#' Persist Settings for a signed-in user. Returns the normalized object.
ynow_prefs_set_user_settings <- function(user_id, settings) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(ynow_prefs_default_user_settings())
  norm <- ynow_prefs_normalize_user_settings(settings)
  .ynow_prefs_write_pref_json(uid, YNOW_PREF_USER_SETTINGS, norm)
  norm
}

#' Test helper: clear in-process memory prefs.
ynow_prefs_reset_memory <- function() {
  .ynow_prefs_mem$by_user <- list()
  invisible(TRUE)
}

#' Remove one user's prefs from memory + file fallback (DB rows deleted in purge txn).
ynow_prefs_purge_user <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(invisible(FALSE))
  if (!is.null(.ynow_prefs_mem$by_user[[uid]])) {
    .ynow_prefs_mem$by_user[[uid]] <- NULL
  }
  store <- tryCatch(.ynow_prefs_read_file(), error = function(e) list())
  if (is.list(store) && !is.null(store[[uid]])) {
    store[[uid]] <- NULL
    tryCatch(.ynow_prefs_write_file(store), error = function(e) NULL)
  }
  invisible(TRUE)
}
