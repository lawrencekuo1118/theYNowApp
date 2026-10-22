# System admin maintenance windows (維護) — distinct from membership / ACL locks.
# item id = same catalog as access_locks.R (kind:target).
# Scope default = all users. While active (server clock in [start, end)), content is
# unusable like a lock, but sidebar/menu chrome is non-clickable (no gate navigation).
# When end is reached, restrictions auto-lift (no admin click); expired rows are
# marked inactive on read. Fail closed; no secrets in git.
#
# Do not merge with guest_menu_lock, Checklist gate overlay, or access_locks domains.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

# Ensure access catalog helpers (standalone source / tests).
if (!exists("ynow_access_catalog", mode = "function")) {
  .ynow_maint_al_ofile <- tryCatch(
    normalizePath(sys.frame(1)$ofile, mustWork = FALSE),
    error = function(e) ""
  )
  .ynow_maint_al_cands <- unique(c(
    if (is.character(.ynow_maint_al_ofile) && nzchar(.ynow_maint_al_ofile)) {
      file.path(dirname(.ynow_maint_al_ofile), "access_locks.R")
    } else {
      character()
    },
    "access_locks.R",
    file.path("app_26.0", "access_locks.R"),
    file.path("..", "access_locks.R")
  ))
  .ynow_maint_al_loaded <- FALSE
  for (.ynow_maint_al_p in .ynow_maint_al_cands) {
    if (is.character(.ynow_maint_al_p) && nzchar(.ynow_maint_al_p) &&
        file.exists(.ynow_maint_al_p)) {
      sys.source(.ynow_maint_al_p, envir = environment(), keep.source = TRUE)
      .ynow_maint_al_loaded <- exists("ynow_access_catalog", mode = "function")
      if (isTRUE(.ynow_maint_al_loaded)) break
    }
  }
  if (!isTRUE(.ynow_maint_al_loaded)) {
    warning("access_locks.R not found; maintenance catalog unavailable", call. = FALSE)
  }
  rm(list = intersect(
    c(
      ".ynow_maint_al_ofile", ".ynow_maint_al_cands",
      ".ynow_maint_al_p", ".ynow_maint_al_loaded"
    ),
    ls(all.names = TRUE)
  ))
}

.ynow_maint_mem <- new.env(parent = emptyenv())
.ynow_maint_mem$by_item <- list()
.ynow_maint_mem$now_override <- NULL

.ynow_maint_file <- function() {
  override <- Sys.getenv("YNOW_MAINTENANCE_PATH", "")
  if (nzchar(override)) return(override)
  file.path("data", "maintenance.json")
}

.ynow_maint_db_on <- function() {
  exists("ynow_db_ready", mode = "function") &&
    isTRUE(ynow_db_ready()) &&
    exists("ynow_db_with", mode = "function") &&
    !identical(Sys.getenv("YNOW_MAINTENANCE_FORCE_FILE", ""), "1")
}

#' Server-authoritative clock (injectable for tests).
ynow_maintenance_now <- function() {
  ov <- .ynow_maint_mem$now_override
  if (inherits(ov, "POSIXt") && length(ov) >= 1L && !is.na(ov[[1]])) {
    return(as.POSIXct(ov[[1]], tz = "UTC"))
  }
  env_now <- Sys.getenv("YNOW_MAINTENANCE_NOW", "")
  if (nzchar(env_now)) {
    parsed <- tryCatch(
      as.POSIXct(env_now, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      parsed <- tryCatch(
        as.POSIXct(env_now, tz = "UTC"),
        error = function(e) NULL
      )
    }
    if (inherits(parsed, "POSIXt") && length(parsed) >= 1L && !is.na(parsed[[1]])) {
      return(as.POSIXct(parsed[[1]], tz = "UTC"))
    }
  }
  as.POSIXct(Sys.time(), tz = "UTC")
}

ynow_maintenance_set_now_override <- function(t = NULL) {
  if (is.null(t) || (length(t) == 1L && is.na(t))) {
    .ynow_maint_mem$now_override <- NULL
  } else {
    .ynow_maint_mem$now_override <- ynow_maintenance_parse_time(t)
  }
  invisible(.ynow_maint_mem$now_override)
}

ynow_maintenance_reset_memory <- function() {
  .ynow_maint_mem$by_item <- list()
  .ynow_maint_mem$now_override <- NULL
  invisible(TRUE)
}

#' Parse admin / store timestamps to UTC POSIXct.
ynow_maintenance_parse_time <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x[[1]], tz = "UTC"))
  }
  s <- trimws(as.character(x %||% "")[1])
  if (!nzchar(s)) return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  # Shiny datetimeInput often yields "YYYY-MM-DD HH:MM:SS" local; treat as UTC wall.
  fmts <- c(
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y-%m-%dT%H:%M",
    "%Y-%m-%d"
  )
  for (fmt in fmts) {
    parsed <- tryCatch(
      as.POSIXct(s, tz = "UTC", format = fmt),
      error = function(e) NULL
    )
    if (inherits(parsed, "POSIXt") && length(parsed) >= 1L && !is.na(parsed[[1]])) {
      return(as.POSIXct(parsed[[1]], tz = "UTC"))
    }
  }
  parsed <- tryCatch(as.POSIXct(s, tz = "UTC"), error = function(e) NULL)
  if (inherits(parsed, "POSIXt") && length(parsed) >= 1L && !is.na(parsed[[1]])) {
    return(as.POSIXct(parsed[[1]], tz = "UTC"))
  }
  as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
}

#' Display form for window bounds (admin status + client copy).
ynow_maintenance_format_time <- function(t) {
  tt <- ynow_maintenance_parse_time(t)
  if (length(tt) < 1L || is.na(tt[[1]])) return("")
  format(as.POSIXct(tt[[1]], tz = "UTC"), "%Y-%m-%d %H:%M", tz = "UTC", usetz = FALSE)
}

ynow_maintenance_catalog <- function() {
  if (!exists("ynow_access_catalog", mode = "function")) {
    return(data.frame(
      id = character(), kind = character(),
      label_en = character(), label_zh = character(),
      stringsAsFactors = FALSE
    ))
  }
  ynow_access_catalog()
}

ynow_maintenance_label <- function(id, locale = "en") {
  if (exists("ynow_access_label", mode = "function")) {
    return(ynow_access_label(id, locale = locale))
  }
  as.character(id)[1]
}

#' Map of catalog item_id → display window "start-end" for configured
#' (active flag + not yet past end) maintenance rows. Empty character named vector when none.
ynow_maintenance_configured_window_map <- function(now = NULL) {
  now <- if (is.null(now)) ynow_maintenance_now() else ynow_maintenance_parse_time(now)
  df <- tryCatch(
    ynow_maintenance_expire_past(now = now),
    error = function(e) .ynow_maint_empty_df()
  )
  if (!is.data.frame(df) || nrow(df) < 1L) {
    return(stats::setNames(character(), character()))
  }
  keep <- as.logical(df$active) & !is.na(df$window_end) & df$window_end > now
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) {
    return(stats::setNames(character(), character()))
  }
  rng <- vapply(seq_len(nrow(df)), function(i) {
    ws <- ynow_maintenance_format_time(df$window_start[[i]])
    we <- ynow_maintenance_format_time(df$window_end[[i]])
    paste0(ws, "-", we)
  }, character(1))
  stats::setNames(rng, as.character(df$id))
}

#' Suffix for admin item picker when a maintenance window is configured.
#' en-US: " · Expected: start-end"; zh-TW: " · 預計時間: start-end"
ynow_maintenance_choice_window_suffix <- function(window_range, locale = "en") {
  rng <- as.character(window_range %||% "")[1]
  if (!nzchar(rng)) return("")
  zh <- identical(as.character(locale)[1], "zh-TW")
  prefix <- if (zh) "預計時間:" else "Expected:"
  paste0(" · ", prefix, " ", rng)
}

#' Annotate named choice vector labels with configured maintenance windows.
.ynow_maint_annotate_choice_labels <- function(named, locale = "en", now = NULL) {
  if (!length(named)) return(named)
  labs <- names(named)
  if (is.null(labs)) labs <- rep("", length(named))
  ids <- as.character(unname(named))
  win_map <- ynow_maintenance_configured_window_map(now = now)
  win_names <- names(win_map)
  new_labs <- vapply(seq_along(ids), function(i) {
    base <- as.character(labs[[i]] %||% "")[1]
    if (!nzchar(base)) base <- ids[[i]]
    id_i <- ids[[i]]
    rng <- if (nzchar(id_i) && length(win_names) && id_i %in% win_names) {
      as.character(win_map[[id_i]])[1]
    } else {
      ""
    }
    paste0(base, ynow_maintenance_choice_window_suffix(rng, locale = locale))
  }, character(1))
  stats::setNames(ids, new_labs)
}

ynow_maintenance_item_choice_list <- function(ids, locale = "en", group = TRUE,
                                              now = NULL) {
  if (!exists("ynow_access_item_choice_list", mode = "function")) {
    return(character())
  }
  base <- ynow_access_item_choice_list(ids, locale = locale, group = group)
  if (!length(base)) return(base)
  if (is.list(base) && !is.null(names(base))) {
    out <- lapply(base, function(named) {
      .ynow_maint_annotate_choice_labels(named, locale = locale, now = now)
    })
    # Drop empty groups if any became empty (should not).
    out <- out[vapply(out, length, integer(1)) > 0L]
    if (!length(out)) return(character())
    return(out)
  }
  .ynow_maint_annotate_choice_labels(base, locale = locale, now = now)
}

.ynow_maint_empty_df <- function() {
  data.frame(
    id = character(),
    window_start = as.POSIXct(character(), tz = "UTC"),
    window_end = as.POSIXct(character(), tz = "UTC"),
    active = logical(),
    stringsAsFactors = FALSE
  )
}

.ynow_maint_normalize_row <- function(raw = NULL,
                                      id = "",
                                      window_start = NA,
                                      window_end = NA,
                                      active = TRUE) {
  if (is.list(raw) && !is.null(raw$id)) {
    id <- raw$id
    window_start <- raw$window_start %||% raw$start %||% window_start
    window_end <- raw$window_end %||% raw$end %||% window_end
    active <- raw$active %||% active
  }
  list(
    id = as.character(id %||% "")[1],
    window_start = ynow_maintenance_parse_time(window_start),
    window_end = ynow_maintenance_parse_time(window_end),
    active = isTRUE(active)
  )
}

.ynow_maint_row_to_jsonable <- function(row) {
  e <- .ynow_maint_normalize_row(raw = row)
  list(
    id = e$id,
    window_start = format(e$window_start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    window_end = format(e$window_end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    active = isTRUE(e$active)
  )
}

.ynow_maint_df_from_list <- function(lst) {
  if (!length(lst)) return(.ynow_maint_empty_df())
  rows <- lapply(lst, function(v) .ynow_maint_normalize_row(raw = v))
  data.frame(
    id = vapply(rows, function(r) r$id, character(1)),
    window_start = do.call(c, lapply(rows, function(r) r$window_start)),
    window_end = do.call(c, lapply(rows, function(r) r$window_end)),
    active = vapply(rows, function(r) isTRUE(r$active), logical(1)),
    stringsAsFactors = FALSE
  )
}

ynow_maintenance_ensure_schema <- function() {
  if (!.ynow_maint_db_on()) return(invisible(FALSE))
  ynow_db_with(function(con) {
    DBI::dbExecute(con, paste(
      "CREATE TABLE IF NOT EXISTS ynow_maintenance (",
      "item_id TEXT NOT NULL PRIMARY KEY,",
      "window_start TIMESTAMPTZ NOT NULL,",
      "window_end TIMESTAMPTZ NOT NULL,",
      "active BOOLEAN NOT NULL DEFAULT TRUE,",
      "updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()",
      ")"
    ))
    cols <- tryCatch(
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT column_name FROM information_schema.columns",
          "WHERE table_name = 'ynow_maintenance'"
        )
      ),
      error = function(e) NULL
    )
    if (is.data.frame(cols) && !"active" %in% cols$column_name) {
      tryCatch(
        DBI::dbExecute(
          con,
          "ALTER TABLE ynow_maintenance ADD COLUMN IF NOT EXISTS active BOOLEAN NOT NULL DEFAULT TRUE"
        ),
        error = function(e) NULL
      )
    }
    if (exists(".ynow_db_rls_fail_closed", mode = "function")) {
      tryCatch(.ynow_db_rls_fail_closed(con, "ynow_maintenance"), error = function(e) NULL)
    }
  })
  invisible(TRUE)
}

.ynow_maint_read_file <- function() {
  path <- .ynow_maint_file()
  if (!file.exists(path)) return(list())
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(raw)) return(list())
  # Accept either {items: [...]} or named map by item_id
  if (!is.null(raw$items) && is.list(raw$items)) return(raw$items)
  if (!is.null(names(raw)) && any(nzchar(names(raw)))) {
    out <- lapply(names(raw), function(nm) {
      v <- raw[[nm]]
      if (is.list(v)) {
        if (is.null(v$id) || !nzchar(as.character(v$id %||% "")[1])) v$id <- nm
        v
      } else {
        NULL
      }
    })
    return(Filter(Negate(is.null), out))
  }
  if (is.list(raw) && length(raw) && is.null(names(raw))) return(raw)
  list()
}

.ynow_maint_write_file <- function(rows_list) {
  path <- .ynow_maint_file()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  payload <- list(items = lapply(rows_list, .ynow_maint_row_to_jsonable))
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

.ynow_maint_load_all_raw <- function() {
  if (.ynow_maint_db_on()) {
    ynow_maintenance_ensure_schema()
    df <- ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT item_id AS id, window_start, window_end,",
          "COALESCE(active, TRUE) AS active",
          "FROM ynow_maintenance",
          "ORDER BY item_id"
        )
      )
    })
    if (!is.data.frame(df) || nrow(df) < 1L) return(.ynow_maint_empty_df())
    df$id <- as.character(df$id)
    df$window_start <- as.POSIXct(df$window_start, tz = "UTC")
    df$window_end <- as.POSIXct(df$window_end, tz = "UTC")
    df$active <- as.logical(df$active)
    return(df)
  }
  mem <- .ynow_maint_mem$by_item
  if (length(mem)) {
    return(.ynow_maint_df_from_list(unname(mem)))
  }
  file_rows <- .ynow_maint_read_file()
  df <- .ynow_maint_df_from_list(file_rows)
  if (nrow(df)) {
    mp <- lapply(seq_len(nrow(df)), function(i) {
      list(
        id = df$id[[i]],
        window_start = df$window_start[[i]],
        window_end = df$window_end[[i]],
        active = isTRUE(df$active[[i]])
      )
    })
    names(mp) <- df$id
    .ynow_maint_mem$by_item <- mp
  }
  df
}

#' Mark expired rows inactive (server clock). Persist when any change.
#' @return data.frame of all rows after expiry pass
ynow_maintenance_expire_past <- function(now = NULL) {
  now <- if (is.null(now)) ynow_maintenance_now() else ynow_maintenance_parse_time(now)
  df <- .ynow_maint_load_all_raw()
  if (!nrow(df)) return(df)
  expired <- as.logical(df$active) & !is.na(df$window_end) & df$window_end <= now
  if (!any(expired, na.rm = TRUE)) return(df)
  ids <- as.character(df$id[expired])
  df$active[expired] <- FALSE
  if (.ynow_maint_db_on()) {
    ynow_maintenance_ensure_schema()
    ynow_db_with(function(con) {
      for (id in ids) {
        DBI::dbExecute(
          con,
          paste(
            "UPDATE ynow_maintenance",
            "SET active = FALSE, updated_at = NOW()",
            "WHERE item_id = $1 AND active IS TRUE"
          ),
          params = list(id)
        )
      }
    })
  } else {
    mp <- .ynow_maint_mem$by_item
    for (id in ids) {
      if (!is.null(mp[[id]])) mp[[id]]$active <- FALSE
    }
    .ynow_maint_mem$by_item <- mp
    .ynow_maint_write_file(unname(mp))
  }
  df
}

#' True when flag active and now in [start, end).
ynow_maintenance_window_active <- function(window_start, window_end,
                                           active = TRUE, now = NULL) {
  if (!isTRUE(active)) return(FALSE)
  now <- if (is.null(now)) ynow_maintenance_now() else ynow_maintenance_parse_time(now)
  ws <- ynow_maintenance_parse_time(window_start)
  we <- ynow_maintenance_parse_time(window_end)
  if (is.na(ws) || is.na(we) || is.na(now)) return(FALSE)
  now >= ws && now < we
}

#' All stored rows (after expiry pass). Includes inactive / future.
ynow_maintenance_store_entries <- function(now = NULL) {
  ynow_maintenance_expire_past(now = now)
}

#' Currently effective maintenance (active flag + inside window).
ynow_maintenance_active_entries <- function(now = NULL) {
  now <- if (is.null(now)) ynow_maintenance_now() else ynow_maintenance_parse_time(now)
  df <- ynow_maintenance_expire_past(now = now)
  if (!nrow(df)) return(.ynow_maint_empty_df())
  keep <- vapply(seq_len(nrow(df)), function(i) {
    ynow_maintenance_window_active(
      df$window_start[[i]], df$window_end[[i]],
      active = df$active[[i]], now = now
    )
  }, logical(1))
  df[keep, , drop = FALSE]
}

ynow_maintenance_is_active <- function(item_id, now = NULL) {
  id <- as.character(item_id %||% "")[1]
  if (!nzchar(id)) return(FALSE)
  ent <- ynow_maintenance_active_entries(now = now)
  id %in% as.character(ent$id)
}

ynow_maintenance_active_ids <- function(now = NULL) {
  ent <- ynow_maintenance_active_entries(now = now)
  if (!nrow(ent)) return(character())
  sort(unique(as.character(ent$id)))
}

#' Earliest future window_end among still-active (not yet expired) rows.
#' Used to schedule the next auto-lift push.
ynow_maintenance_next_end <- function(now = NULL) {
  now <- if (is.null(now)) ynow_maintenance_now() else ynow_maintenance_parse_time(now)
  df <- ynow_maintenance_expire_past(now = now)
  if (!nrow(df)) return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  cand <- df[as.logical(df$active) & !is.na(df$window_end) & df$window_end > now, , drop = FALSE]
  if (!nrow(cand)) return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  min(cand$window_end)
}

#' Bilingual maintenance body for panes / elements.
#' zh-TW: 維護中\\n預計時間: start-end
#' en-US: Under maintenance\\nExpected: start-end
ynow_maintenance_note <- function(window_start, window_end, locale = "en") {
  zh <- identical(as.character(locale)[1], "zh-TW")
  ws <- ynow_maintenance_format_time(window_start)
  we <- ynow_maintenance_format_time(window_end)
  rng <- paste0(ws, "-", we)
  if (zh) {
    paste0("維護中\n預計時間: ", rng)
  } else {
    paste0("Under maintenance\nExpected: ", rng)
  }
}

#' Admin status line for current store (active + scheduled).
ynow_maintenance_admin_status_text <- function(locale = "en",
                                               none_lab = NULL) {
  zh <- identical(as.character(locale)[1], "zh-TW")
  if (is.null(none_lab)) {
    none_lab <- if (zh) "目前沒有維護項目。" else "No maintenance entries."
  }
  now <- ynow_maintenance_now()
  df <- ynow_maintenance_expire_past(now = now)
  if (!nrow(df)) return(as.character(none_lab)[1])
  # Prefer active-flag rows that are not past end (active now or future scheduled).
  keep <- as.logical(df$active) & !is.na(df$window_end) & df$window_end > now
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) return(as.character(none_lab)[1])
  parts <- vapply(seq_len(nrow(df)), function(i) {
    lab <- ynow_maintenance_label(df$id[[i]], locale = locale)
    ws <- ynow_maintenance_format_time(df$window_start[[i]])
    we <- ynow_maintenance_format_time(df$window_end[[i]])
    live <- ynow_maintenance_window_active(
      df$window_start[[i]], df$window_end[[i]],
      active = TRUE, now = now
    )
    state <- if (live) {
      if (zh) "進行中" else "active"
    } else {
      if (zh) "排程中" else "scheduled"
    }
    paste0(lab, " [", state, "] ", ws, "-", we)
  }, character(1))
  paste(parts, collapse = "; ")
}

#' Set or clear a maintenance window for an item (all users).
#' @param locked TRUE = set/update window; FALSE = clear
ynow_maintenance_set <- function(item_id,
                                 locked = TRUE,
                                 window_start = NULL,
                                 window_end = NULL) {
  id <- as.character(item_id %||% "")[1]
  cat <- ynow_maintenance_catalog()
  if (!nzchar(id) || !id %in% cat$id) {
    return(list(ok = FALSE, message_key = "maint_bad_item"))
  }
  on <- isTRUE(locked)
  if (!on) {
    if (.ynow_maint_db_on()) {
      ynow_maintenance_ensure_schema()
      ynow_db_with(function(con) {
        DBI::dbExecute(
          con,
          "DELETE FROM ynow_maintenance WHERE item_id = $1",
          params = list(id)
        )
      })
    } else {
      mp <- .ynow_maint_mem$by_item
      mp <- mp[setdiff(names(mp), id)]
      .ynow_maint_mem$by_item <- mp
      .ynow_maint_write_file(unname(mp))
    }
    return(list(ok = TRUE, message_key = "maint_ok_clear", id = id))
  }
  ws <- ynow_maintenance_parse_time(window_start)
  we <- ynow_maintenance_parse_time(window_end)
  if (is.na(ws) || is.na(we)) {
    return(list(ok = FALSE, message_key = "maint_bad_window"))
  }
  if (!(we > ws)) {
    return(list(ok = FALSE, message_key = "maint_bad_window_order"))
  }
  row <- list(id = id, window_start = ws, window_end = we, active = TRUE)
  if (.ynow_maint_db_on()) {
    ynow_maintenance_ensure_schema()
    ynow_db_with(function(con) {
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO ynow_maintenance",
          "(item_id, window_start, window_end, active, updated_at)",
          "VALUES ($1, $2, $3, TRUE, NOW())",
          "ON CONFLICT (item_id) DO UPDATE SET",
          "window_start = EXCLUDED.window_start,",
          "window_end = EXCLUDED.window_end,",
          "active = TRUE,",
          "updated_at = NOW()"
        ),
        params = list(id, ws, we)
      )
    })
  } else {
    mp <- .ynow_maint_mem$by_item
    mp[[id]] <- row
    .ynow_maint_mem$by_item <- mp
    .ynow_maint_write_file(unname(mp))
  }
  list(
    ok = TRUE,
    message_key = "maint_ok_set",
    id = id,
    window_start = ws,
    window_end = we,
    active = TRUE
  )
}

#' Client payload rows: id + note + window bounds (ISO).
ynow_maintenance_client_payload <- function(locale = "en", now = NULL) {
  now <- if (is.null(now)) ynow_maintenance_now() else ynow_maintenance_parse_time(now)
  ent <- ynow_maintenance_active_entries(now = now)
  next_end <- ynow_maintenance_next_end(now = now)
  entries <- if (!nrow(ent)) {
    list()
  } else {
    lapply(seq_len(nrow(ent)), function(i) {
      list(
        id = as.character(ent$id[[i]]),
        note = ynow_maintenance_note(
          ent$window_start[[i]], ent$window_end[[i]], locale = locale
        ),
        window_start = format(
          as.POSIXct(ent$window_start[[i]], tz = "UTC"),
          "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
        ),
        window_end = format(
          as.POSIXct(ent$window_end[[i]], tz = "UTC"),
          "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
        )
      )
    })
  }
  list(
    entries = entries,
    ids = as.list(as.character(ent$id)),
    server_now = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    next_end = if (is.na(next_end)) {
      NA_character_
    } else {
      format(next_end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
  )
}
