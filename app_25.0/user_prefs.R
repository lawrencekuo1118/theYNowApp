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

YNOW_PREF_PINNED_RECENT <- "pinned_recent_search"
.YNOW_PINNED_RECENT_MAX <- 8L

# Sidebar menu tab pins (leaf tabNames): max 12 → 3-column tray.
YNOW_PREF_PINNED_SIDEBAR <- "pinned_sidebar_tabs"
.YNOW_PINNED_SIDEBAR_MAX <- 12L

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
    "about", "snapshot", "feedback", "account_admin", "account_points"
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
ynow_prefs_toggle_pinned_recent <- function(user_id, ticker) {
  tk <- ynow_prefs_normalize_tickers(ticker, max_n = 1L)
  if (!length(tk)) {
    return(ynow_prefs_get_pinned_recent(user_id))
  }
  cur <- ynow_prefs_get_pinned_recent(user_id)
  if (tk[[1]] %in% cur) {
    nxt <- setdiff(cur, tk[[1]])
  } else {
    nxt <- head(c(tk[[1]], setdiff(cur, tk[[1]])), .YNOW_PINNED_RECENT_MAX)
  }
  ynow_prefs_set_pinned_recent(user_id, nxt)
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
