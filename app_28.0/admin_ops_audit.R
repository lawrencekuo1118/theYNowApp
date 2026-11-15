# System-admin operations audit log (操作紀錄).
# Append-only from the operator’s perspective (no UI clear/delete).
# Each row is retained at most 24 hours (server-authoritative clock); older rows
# are purged on write/list. Prefer Supabase Postgres (same YNOW_DB_* store as
# auth / ACL / maintenance); file/memory fallback for offline tests.
# Fail closed: if persistence is unavailable, writes return ok=FALSE and reads
# return an empty list (never invent rows). No secrets in git.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

YNOW_ADMIN_OPS_AUDIT_TTL_SEC <- 24L * 3600L

# Action taxonomy (stable ids for store + UI labels).
YNOW_ADMIN_OPS_ACTIONS <- c(
  "sysadmin.enter",
  "sysadmin.exit",
  "acl.apply",
  "acl.remove",
  "maintenance.set",
  "maintenance.clear",
  "view_as.enter",
  "view_as.exit",
  "self_view.apply",
  "self_view.exit",
  "points.adjust",
  "insp.upload",
  "insp.delete",
  "insp.freq"
)

.ynow_admin_ops_mem <- new.env(parent = emptyenv())
.ynow_admin_ops_mem$entries <- list()
.ynow_admin_ops_mem$now_override <- NULL

# Process-once schema cache (same pattern as access / points / prefs).
.ynow_admin_ops_schema_cache <- new.env(parent = emptyenv())
.ynow_admin_ops_schema_cache$ready <- FALSE

ynow_admin_ops_schema_cache_reset <- function() {
  .ynow_admin_ops_schema_cache$ready <- FALSE
  invisible(TRUE)
}

.ynow_admin_ops_file <- function() {
  override <- Sys.getenv("YNOW_ADMIN_OPS_AUDIT_PATH", "")
  if (nzchar(override)) return(override)
  file.path("data", "admin_ops_audit.json")
}

.ynow_admin_ops_db_on <- function() {
  exists("ynow_db_ready", mode = "function") &&
    isTRUE(ynow_db_ready()) &&
    exists("ynow_db_with", mode = "function") &&
    !identical(Sys.getenv("YNOW_ADMIN_OPS_AUDIT_FORCE_FILE", ""), "1")
}

#' Server-authoritative clock (injectable for tests).
ynow_admin_ops_now <- function() {
  ov <- .ynow_admin_ops_mem$now_override
  if (inherits(ov, "POSIXt") && length(ov) >= 1L && !is.na(ov[[1]])) {
    return(as.POSIXct(ov[[1]], tz = "UTC"))
  }
  env_now <- Sys.getenv("YNOW_ADMIN_OPS_AUDIT_NOW", "")
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

ynow_admin_ops_set_now_override <- function(t = NULL) {
  if (is.null(t) || (length(t) == 1L && is.na(t))) {
    .ynow_admin_ops_mem$now_override <- NULL
  } else {
    .ynow_admin_ops_mem$now_override <- ynow_admin_ops_parse_time(t)
  }
  invisible(.ynow_admin_ops_mem$now_override)
}

ynow_admin_ops_reset_memory <- function() {
  .ynow_admin_ops_mem$entries <- list()
  .ynow_admin_ops_mem$now_override <- NULL
  invisible(TRUE)
}

ynow_admin_ops_parse_time <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x[[1]], tz = "UTC"))
  }
  s <- trimws(as.character(x %||% "")[1])
  if (!nzchar(s)) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  }
  fmts <- c(
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M"
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

ynow_admin_ops_normalize_action <- function(action) {
  a <- tolower(trimws(as.character(action %||% "")[1]))
  if (!nzchar(a)) return(NA_character_)
  if (a %in% YNOW_ADMIN_OPS_ACTIONS) a else a
}

.ynow_admin_ops_empty_df <- function() {
  data.frame(
    id = character(),
    created_at = as.POSIXct(character(), tz = "UTC"),
    actor = character(),
    action = character(),
    detail = character(),
    stringsAsFactors = FALSE
  )
}

.ynow_admin_ops_new_id <- function() {
  hex <- paste(
    sprintf("%02x", as.integer(runif(8L, 0, 255))),
    collapse = ""
  )
  paste0(
    format(ynow_admin_ops_now(), "%Y%m%d%H%M%S", tz = "UTC"),
    "-",
    hex
  )
}

.ynow_admin_ops_normalize_row <- function(raw = NULL,
                                          id = "",
                                          created_at = NA,
                                          actor = "",
                                          action = "",
                                          detail = "") {
  if (is.list(raw) && !is.null(raw$action)) {
    id <- raw$id %||% id
    created_at <- raw$created_at %||% raw$ts %||% raw$timestamp %||% created_at
    actor <- raw$actor %||% raw$admin %||% actor
    action <- raw$action %||% action
    detail <- raw$detail %||% raw$message %||% detail
  }
  list(
    id = as.character(id %||% "")[1],
    created_at = ynow_admin_ops_parse_time(created_at),
    actor = as.character(actor %||% "")[1],
    action = ynow_admin_ops_normalize_action(action),
    detail = as.character(detail %||% "")[1]
  )
}

.ynow_admin_ops_row_to_jsonable <- function(row) {
  e <- .ynow_admin_ops_normalize_row(raw = row)
  list(
    id = e$id,
    created_at = format(e$created_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    actor = e$actor,
    action = e$action,
    detail = e$detail
  )
}

.ynow_admin_ops_df_from_list <- function(lst) {
  if (!length(lst)) return(.ynow_admin_ops_empty_df())
  rows <- lapply(lst, function(v) .ynow_admin_ops_normalize_row(raw = v))
  data.frame(
    id = vapply(rows, function(r) r$id, character(1)),
    created_at = do.call(c, lapply(rows, function(r) r$created_at)),
    actor = vapply(rows, function(r) r$actor, character(1)),
    action = vapply(rows, function(r) as.character(r$action %||% ""), character(1)),
    detail = vapply(rows, function(r) r$detail, character(1)),
    stringsAsFactors = FALSE
  )
}

ynow_admin_ops_ensure_schema <- function(force = FALSE) {
  if (!isTRUE(force) && isTRUE(.ynow_admin_ops_schema_cache$ready)) {
    return(invisible(TRUE))
  }
  if (!.ynow_admin_ops_db_on()) return(invisible(FALSE))
  ynow_db_with(function(con) {
    DBI::dbExecute(con, paste(
      "CREATE TABLE IF NOT EXISTS ynow_admin_ops_audit (",
      "id TEXT NOT NULL PRIMARY KEY,",
      "created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),",
      "actor TEXT NOT NULL DEFAULT '',",
      "action TEXT NOT NULL,",
      "detail TEXT NOT NULL DEFAULT ''",
      ")"
    ))
    tryCatch(
      DBI::dbExecute(
        con,
        paste(
          "CREATE INDEX IF NOT EXISTS ynow_admin_ops_audit_created_idx",
          "ON ynow_admin_ops_audit (created_at DESC)"
        )
      ),
      error = function(e) NULL
    )
    if (exists(".ynow_db_rls_fail_closed", mode = "function")) {
      tryCatch(
        .ynow_db_rls_fail_closed(con, "ynow_admin_ops_audit"),
        error = function(e) NULL
      )
    }
  })
  .ynow_admin_ops_schema_cache$ready <- TRUE
  invisible(TRUE)
}

.ynow_admin_ops_read_file <- function() {
  path <- .ynow_admin_ops_file()
  if (!file.exists(path)) return(list())
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(raw)) return(list())
  if (!is.null(raw$entries) && is.list(raw$entries)) return(raw$entries)
  if (is.list(raw) && length(raw) && is.null(names(raw))) return(raw)
  list()
}

.ynow_admin_ops_write_file <- function(rows_list) {
  path <- .ynow_admin_ops_file()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  payload <- list(entries = lapply(rows_list, .ynow_admin_ops_row_to_jsonable))
  ok <- tryCatch({
    jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE)
    TRUE
  }, error = function(e) FALSE)
  invisible(isTRUE(ok))
}

.ynow_admin_ops_load_all_raw <- function() {
  if (.ynow_admin_ops_db_on()) {
    ynow_admin_ops_ensure_schema()
    df <- ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT id, created_at, actor, action, detail",
          "FROM ynow_admin_ops_audit",
          "ORDER BY created_at DESC"
        )
      )
    })
    if (!is.data.frame(df) || nrow(df) < 1L) return(.ynow_admin_ops_empty_df())
    df$id <- as.character(df$id)
    df$created_at <- as.POSIXct(df$created_at, tz = "UTC")
    df$actor <- as.character(df$actor %||% "")
    df$action <- as.character(df$action %||% "")
    df$detail <- as.character(df$detail %||% "")
    return(df)
  }
  mem <- .ynow_admin_ops_mem$entries
  if (length(mem)) {
    return(.ynow_admin_ops_df_from_list(mem))
  }
  file_rows <- .ynow_admin_ops_read_file()
  df <- .ynow_admin_ops_df_from_list(file_rows)
  if (nrow(df)) {
    .ynow_admin_ops_mem$entries <- lapply(seq_len(nrow(df)), function(i) {
      list(
        id = df$id[[i]],
        created_at = df$created_at[[i]],
        actor = df$actor[[i]],
        action = df$action[[i]],
        detail = df$detail[[i]]
      )
    })
  }
  df
}

#' Drop rows older than TTL (server clock). Called on write/list.
#' Postgres: DELETE by cutoff only (no full-table SELECT before write).
#' File/memory: filter in process, then rewrite.
#' @return data.frame remaining after purge (for list); empty df ok after DB-only purge
ynow_admin_ops_purge_expired <- function(now = NULL,
                                         ttl_sec = YNOW_ADMIN_OPS_AUDIT_TTL_SEC,
                                         return_rows = TRUE) {
  now <- if (is.null(now)) ynow_admin_ops_now() else ynow_admin_ops_parse_time(now)
  ttl <- suppressWarnings(as.integer(ttl_sec)[1])
  if (!is.finite(ttl) || ttl < 1L) ttl <- YNOW_ADMIN_OPS_AUDIT_TTL_SEC
  # At most 24h: drop when age >= TTL (created_at <= now - ttl).
  cutoff <- now - ttl
  if (.ynow_admin_ops_db_on()) {
    ynow_admin_ops_ensure_schema()
    tryCatch(
      ynow_db_with(function(con) {
        DBI::dbExecute(
          con,
          "DELETE FROM ynow_admin_ops_audit WHERE created_at <= $1",
          params = list(cutoff)
        )
      }),
      error = function(e) NULL
    )
    if (!isTRUE(return_rows)) return(.ynow_admin_ops_empty_df())
    return(.ynow_admin_ops_load_all_raw())
  }
  df <- .ynow_admin_ops_load_all_raw()
  if (!nrow(df)) return(df)
  keep <- !is.na(df$created_at) & df$created_at > cutoff
  drop_ids <- as.character(df$id[!keep])
  if (!length(drop_ids)) return(df[keep, , drop = FALSE])
  kept <- lapply(which(keep), function(i) {
    list(
      id = df$id[[i]],
      created_at = df$created_at[[i]],
      actor = df$actor[[i]],
      action = df$action[[i]],
      detail = df$detail[[i]]
    )
  })
  .ynow_admin_ops_mem$entries <- kept
  .ynow_admin_ops_write_file(kept)
  df[keep, , drop = FALSE]
}

#' Append one audit row. Fail closed on persistence errors.
#' @return list(ok=, id=, message_key=)
ynow_admin_ops_append <- function(action,
                                  detail = "",
                                  actor = "",
                                  created_at = NULL) {
  act <- ynow_admin_ops_normalize_action(action)
  if (is.na(act) || !nzchar(act)) {
    return(list(ok = FALSE, id = "", message_key = "admin_ops_bad_action"))
  }
  ts <- if (is.null(created_at)) {
    ynow_admin_ops_now()
  } else {
    ynow_admin_ops_parse_time(created_at)
  }
  if (is.na(ts)) {
    return(list(ok = FALSE, id = "", message_key = "admin_ops_bad_time"))
  }
  id <- .ynow_admin_ops_new_id()
  row <- list(
    id = id,
    created_at = ts,
    actor = as.character(actor %||% "")[1],
    action = act,
    detail = as.character(detail %||% "")[1]
  )
  # Purge first so append never rehydrates expired noise.
  # DB path: DELETE-only (return_rows=FALSE) — avoid SELECT-all before INSERT.
  tryCatch(
    ynow_admin_ops_purge_expired(now = ts, return_rows = FALSE),
    error = function(e) NULL
  )
  if (.ynow_admin_ops_db_on()) {
    ynow_admin_ops_ensure_schema()
    ok <- tryCatch({
      ynow_db_with(function(con) {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO ynow_admin_ops_audit",
            "(id, created_at, actor, action, detail)",
            "VALUES ($1, $2, $3, $4, $5)"
          ),
          params = list(
            row$id,
            row$created_at,
            row$actor,
            row$action,
            row$detail
          )
        )
      })
      TRUE
    }, error = function(e) {
      warning("ynow_admin_ops_append: ", conditionMessage(e), call. = FALSE)
      FALSE
    })
    if (!isTRUE(ok)) {
      return(list(ok = FALSE, id = "", message_key = "admin_ops_store_unavailable"))
    }
    return(list(ok = TRUE, id = id, message_key = "admin_ops_ok"))
  }
  # File / memory path (tests + offline).
  entries <- .ynow_admin_ops_mem$entries
  if (!length(entries)) {
    df0 <- .ynow_admin_ops_df_from_list(.ynow_admin_ops_read_file())
    if (nrow(df0)) {
      entries <- lapply(seq_len(nrow(df0)), function(i) {
        list(
          id = df0$id[[i]],
          created_at = df0$created_at[[i]],
          actor = df0$actor[[i]],
          action = df0$action[[i]],
          detail = df0$detail[[i]]
        )
      })
    }
  }
  entries <- c(list(row), entries)
  .ynow_admin_ops_mem$entries <- entries
  wrote <- .ynow_admin_ops_write_file(entries)
  if (!isTRUE(wrote) && identical(Sys.getenv("YNOW_ADMIN_OPS_AUDIT_REQUIRE_FILE", ""), "1")) {
    return(list(ok = FALSE, id = "", message_key = "admin_ops_store_unavailable"))
  }
  list(ok = TRUE, id = id, message_key = "admin_ops_ok")
}

#' List audit rows newest-first after 24h purge.
#' @return data.frame(id, created_at, actor, action, detail)
ynow_admin_ops_list <- function(now = NULL, limit = 500L) {
  df <- tryCatch(
    ynow_admin_ops_purge_expired(now = now),
    error = function(e) .ynow_admin_ops_empty_df()
  )
  if (!is.data.frame(df) || nrow(df) < 1L) return(.ynow_admin_ops_empty_df())
  ord <- order(df$created_at, decreasing = TRUE, na.last = TRUE)
  df <- df[ord, , drop = FALSE]
  lim <- suppressWarnings(as.integer(limit)[1])
  if (is.finite(lim) && lim > 0L && nrow(df) > lim) {
    df <- df[seq_len(lim), , drop = FALSE]
  }
  rownames(df) <- NULL
  df
}

#' Human label for action type (en-US / zh-TW).
ynow_admin_ops_action_label <- function(action, locale = "en") {
  zh <- identical(as.character(locale)[1], "zh-TW")
  a <- ynow_admin_ops_normalize_action(action)
  if (zh) {
    switch(
      as.character(a %||% ""),
      "sysadmin.enter" = "進入系統管理",
      "sysadmin.exit" = "離開系統管理",
      "acl.apply" = "套用項目限制",
      "acl.remove" = "解除項目限制",
      "maintenance.set" = "設定維護",
      "maintenance.clear" = "清除維護",
      "view_as.enter" = "切換使用者視角",
      "view_as.exit" = "結束使用者視角",
      "self_view.apply" = "套用自身視角",
      "self_view.exit" = "結束自身視角",
      "points.adjust" = "調整點數",
      "insp.upload" = "上傳靈感藝廊圖片",
      "insp.delete" = "移除靈感藝廊圖片",
      "insp.freq" = "更新靈感藝廊顯示頻率",
      as.character(a %||% "")
    )
  } else {
    switch(
      as.character(a %||% ""),
      "sysadmin.enter" = "Enter system admin",
      "sysadmin.exit" = "Exit system admin",
      "acl.apply" = "Apply item lock",
      "acl.remove" = "Remove item lock",
      "maintenance.set" = "Set maintenance",
      "maintenance.clear" = "Clear maintenance",
      "view_as.enter" = "Enter user view",
      "view_as.exit" = "Exit user view",
      "self_view.apply" = "Apply self view",
      "self_view.exit" = "Exit self view",
      "points.adjust" = "Adjust points",
      "insp.upload" = "Upload inspiration image",
      "insp.delete" = "Remove inspiration image",
      "insp.freq" = "Update inspiration frequency",
      as.character(a %||% "")
    )
  }
}

#' Format one row for the admin list UI.
ynow_admin_ops_format_row <- function(row, locale = "en") {
  if (is.data.frame(row) && nrow(row) >= 1L) {
    row <- as.list(row[1, , drop = FALSE])
  }
  e <- .ynow_admin_ops_normalize_row(raw = row)
  ts <- if (is.na(e$created_at)) {
    "—"
  } else {
    format(e$created_at, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
  }
  act_lab <- ynow_admin_ops_action_label(e$action, locale = locale)
  actor <- if (nzchar(e$actor)) e$actor else "—"
  detail <- if (nzchar(e$detail)) e$detail else "—"
  list(
    timestamp = ts,
    actor = actor,
    action = act_lab,
    action_id = e$action,
    detail = detail
  )
}
