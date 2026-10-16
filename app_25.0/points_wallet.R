# ==========================================
# Points wallet — usage unlock (v1)
# ==========================================
# Freemium usage points on Neon (ledger = source of truth).
# Env:
#   YNOW_POINTS_ENABLED=0|1   (default 0 — shadow/off; gate always allows)
#   YNOW_POINTS_SIGNUP_BONUS  (default 20)
#   YNOW_POINTS_PRICE_ALERT_SLOT / HFV_RUN / BT_RUN  (optional overrides)
#   YNOW_POINTS_BACKEND=memory  (tests / no Neon)
#
# SKUs (v1): alert_slot=1, hfv_run=3, bt_run=5
# Lite never spends points. Spend is server-side only (idempotent ref_id).

.ynow_points_mem <- new.env(parent = emptyenv())
.ynow_points_mem$users <- new.env(parent = emptyenv())
.ynow_points_mem$ledger <- list()

ynow_points_enabled <- function() {
  v <- tolower(trimws(Sys.getenv("YNOW_POINTS_ENABLED", unset = "0")))
  v %in% c("1", "true", "yes", "on")
}

ynow_points_signup_bonus <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("YNOW_POINTS_SIGNUP_BONUS", unset = "20")))
  if (!is.finite(n) || n < 0L) 20L else as.integer(n)
}

ynow_points_backend <- function() {
  b <- tolower(trimws(Sys.getenv("YNOW_POINTS_BACKEND", unset = "")))
  if (identical(b, "memory")) return("memory")
  if (exists("ynow_db_ready", mode = "function") && isTRUE(ynow_db_ready())) {
    return("neon")
  }
  "memory"
}

#' Default catalog (cost may be overridden by env).
ynow_points_catalog_defaults <- function() {
  env_cost <- function(key, default) {
    n <- suppressWarnings(as.integer(Sys.getenv(key, unset = "")))
    if (is.finite(n) && n >= 0L) as.integer(n) else as.integer(default)
  }
  data.frame(
    sku = c("alert_slot", "hfv_run", "bt_run"),
    title_en = c("Value Alert slot", "HFV full recalculation", "Backtest full run"),
    title_zh_tw = c("到值通知一筆", "HFV 完整重算", "回測完整執行"),
    cost = c(
      env_cost("YNOW_POINTS_PRICE_ALERT_SLOT", 1L),
      env_cost("YNOW_POINTS_PRICE_HFV_RUN", 3L),
      env_cost("YNOW_POINTS_PRICE_BT_RUN", 5L)
    ),
    enabled = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}

ynow_points_sku_cost <- function(sku) {
  sku <- as.character(sku %||% "")[1]
  cat <- ynow_points_catalog_defaults()
  en <- as.logical(cat$enabled)
  en[is.na(en)] <- FALSE
  hit <- cat[cat$sku == sku & en, , drop = FALSE]
  if (!nrow(hit)) return(NA_integer_)
  as.integer(hit$cost[[1]])
}

ynow_points_sku_title <- function(sku, locale = "en") {
  sku <- as.character(sku %||% "")[1]
  cat <- ynow_points_catalog_defaults()
  hit <- cat[cat$sku == sku, , drop = FALSE]
  if (!nrow(hit)) return(sku)
  loc <- as.character(locale %||% "en")[1]
  if (startsWith(tolower(loc), "zh")) {
    as.character(hit$title_zh_tw[[1]])
  } else {
    as.character(hit$title_en[[1]])
  }
}

# ----- memory backend (tests / offline) -----

.ynow_points_mem_reset <- function() {
  .ynow_points_mem$users <- new.env(parent = emptyenv())
  .ynow_points_mem$ledger <- list()
  invisible(TRUE)
}

.ynow_points_mem_ensure_user <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(NULL)
  if (is.null(.ynow_points_mem$users[[uid]])) {
    .ynow_points_mem$users[[uid]] <- list(balance = 0L)
  }
  uid
}

.ynow_points_mem_balance <- function(user_id) {
  uid <- .ynow_points_mem_ensure_user(user_id)
  if (is.null(uid)) return(NA_integer_)
  as.integer(.ynow_points_mem$users[[uid]]$balance %||% 0L)
}

.ynow_points_mem_apply <- function(user_id, delta, reason, sku = NULL, ref_id = NULL,
                                   meta = NULL) {
  uid <- .ynow_points_mem_ensure_user(user_id)
  if (is.null(uid)) {
    return(list(ok = FALSE, balance = NA_integer_, message_key = "pts_msg_no_user"))
  }
  ref <- as.character(ref_id %||% "")[1]
  if (!nzchar(ref)) {
    return(list(ok = FALSE, balance = .ynow_points_mem_balance(uid),
                message_key = "pts_msg_bad_ref"))
  }
  # Idempotent: same ref returns prior outcome
  for (row in .ynow_points_mem$ledger) {
    if (identical(row$user_id, uid) && identical(row$ref_id, ref)) {
      return(list(
        ok = TRUE,
        balance = as.integer(row$balance_after),
        message_key = "pts_msg_ok",
        idempotent = TRUE,
        delta = as.integer(row$delta)
      ))
    }
  }
  bal <- as.integer(.ynow_points_mem$users[[uid]]$balance %||% 0L)
  dlt <- as.integer(delta)[1]
  if (!is.finite(dlt)) {
    return(list(ok = FALSE, balance = bal, message_key = "pts_msg_bad_delta"))
  }
  if (dlt < 0L && bal + dlt < 0L) {
    return(list(ok = FALSE, balance = bal, message_key = "pts_msg_insufficient"))
  }
  new_bal <- bal + dlt
  .ynow_points_mem$users[[uid]]$balance <- new_bal
  entry <- list(
    id = paste0("mem-", length(.ynow_points_mem$ledger) + 1L),
    user_id = uid,
    delta = dlt,
    balance_after = new_bal,
    reason = as.character(reason %||% "adjust")[1],
    sku = if (is.null(sku) || !nzchar(as.character(sku)[1])) NA_character_ else as.character(sku)[1],
    ref_id = ref,
    meta = meta,
    created_at = as.character(Sys.time())
  )
  .ynow_points_mem$ledger <- c(.ynow_points_mem$ledger, list(entry))
  list(ok = TRUE, balance = new_bal, message_key = "pts_msg_ok", delta = dlt)
}

.ynow_points_mem_meta_note <- function(meta) {
  if (is.null(meta)) return("")
  if (is.list(meta) && !is.null(meta$note)) return(as.character(meta$note)[1])
  ""
}

.ynow_points_mem_ledger_tail <- function(user_id, n = NULL) {
  uid <- as.character(user_id %||% "")[1]
  empty <- data.frame(
    id = character(), created_at = character(), delta = integer(),
    balance_after = integer(), reason = character(), sku = character(),
    ref_id = character(), note = character(),
    stringsAsFactors = FALSE
  )
  rows <- Filter(function(r) identical(r$user_id, uid), .ynow_points_mem$ledger)
  if (!length(rows)) return(empty)
  rows <- rev(rows)
  if (!is.null(n) && is.finite(as.integer(n)[1]) && as.integer(n)[1] > 0L) {
    rows <- rows[seq_len(min(as.integer(n)[1], length(rows)))]
  }
  data.frame(
    id = vapply(rows, function(r) as.character(r$id %||% ""), character(1)),
    created_at = vapply(rows, function(r) as.character(r$created_at), character(1)),
    delta = vapply(rows, function(r) as.integer(r$delta), integer(1)),
    balance_after = vapply(rows, function(r) as.integer(r$balance_after), integer(1)),
    reason = vapply(rows, function(r) as.character(r$reason), character(1)),
    sku = vapply(rows, function(r) as.character(r$sku %||% ""), character(1)),
    ref_id = vapply(rows, function(r) as.character(r$ref_id), character(1)),
    note = vapply(rows, function(r) .ynow_points_mem_meta_note(r$meta), character(1)),
    stringsAsFactors = FALSE
  )
}

# ----- Neon schema helpers -----

ynow_points_ensure_schema <- function(conn = NULL) {
  run <- function(con) {
    # Cache column on users (idempotent ADD)
    cols <- tryCatch(
      DBI::dbGetQuery(
        con,
        "SELECT column_name FROM information_schema.columns
          WHERE table_name = 'ynow_users' AND column_name = 'points_balance'"
      ),
      error = function(e) data.frame()
    )
    if (!is.data.frame(cols) || nrow(cols) < 1L) {
      tryCatch(
        DBI::dbExecute(
          con,
          "ALTER TABLE ynow_users
             ADD COLUMN IF NOT EXISTS points_balance INTEGER NOT NULL DEFAULT 0"
        ),
        error = function(e) {
          # Older Postgres without IF NOT EXISTS on ADD COLUMN
          tryCatch(
            DBI::dbExecute(
              con,
              "ALTER TABLE ynow_users ADD COLUMN points_balance INTEGER NOT NULL DEFAULT 0"
            ),
            error = function(e2) NULL
          )
        }
      )
    }
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS ynow_points_ledger (
        id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        user_id UUID NOT NULL REFERENCES ynow_users(id),
        delta INTEGER NOT NULL,
        balance_after INTEGER NOT NULL,
        reason TEXT NOT NULL,
        sku TEXT,
        ref_id TEXT NOT NULL,
        meta JSONB,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        UNIQUE (user_id, ref_id)
      )
    ")
    DBI::dbExecute(con, "
      CREATE INDEX IF NOT EXISTS ynow_points_ledger_user_created_idx
        ON ynow_points_ledger (user_id, created_at DESC)
    ")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS ynow_points_catalog (
        sku TEXT PRIMARY KEY,
        title_en TEXT NOT NULL,
        title_zh_tw TEXT NOT NULL,
        cost INTEGER NOT NULL CHECK (cost >= 0),
        enabled BOOLEAN NOT NULL DEFAULT TRUE,
        mode TEXT NOT NULL DEFAULT 'per_use'
      )
    ")
    # Seed / upsert v1 rows
    defs <- ynow_points_catalog_defaults()
    for (i in seq_len(nrow(defs))) {
      DBI::dbExecute(
        con,
        "INSERT INTO ynow_points_catalog (sku, title_en, title_zh_tw, cost, enabled, mode)
         VALUES ($1, $2, $3, $4, $5, 'per_use')
         ON CONFLICT (sku) DO UPDATE SET
           title_en = EXCLUDED.title_en,
           title_zh_tw = EXCLUDED.title_zh_tw,
           cost = EXCLUDED.cost,
           enabled = EXCLUDED.enabled",
        params = list(
          defs$sku[[i]], defs$title_en[[i]], defs$title_zh_tw[[i]],
          as.integer(defs$cost[[i]]), isTRUE(defs$enabled[[i]])
        )
      )
    }
    TRUE
  }
  if (!is.null(conn)) return(run(conn))
  if (!identical(ynow_points_backend(), "neon")) return(TRUE)
  out <- ynow_db_with(run)
  isTRUE(out)
}

.ynow_points_neon_balance <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(NA_integer_)
  ynow_db_with(function(con) {
    ynow_points_ensure_schema(con)
    df <- DBI::dbGetQuery(
      con,
      "SELECT points_balance FROM ynow_users WHERE id = $1::uuid LIMIT 1",
      params = list(uid)
    )
    if (!is.data.frame(df) || nrow(df) < 1L) return(NA_integer_)
    as.integer(df$points_balance[[1]])
  }) %||% NA_integer_
}

.ynow_points_neon_apply <- function(user_id, delta, reason, sku = NULL, ref_id = NULL,
                                    meta = NULL) {
  uid <- as.character(user_id %||% "")[1]
  ref <- as.character(ref_id %||% "")[1]
  dlt <- as.integer(delta)[1]
  if (!nzchar(uid)) {
    return(list(ok = FALSE, balance = NA_integer_, message_key = "pts_msg_no_user"))
  }
  if (!nzchar(ref)) {
    return(list(ok = FALSE, balance = NA_integer_, message_key = "pts_msg_bad_ref"))
  }
  if (!is.finite(dlt)) {
    return(list(ok = FALSE, balance = NA_integer_, message_key = "pts_msg_bad_delta"))
  }
  sku_s <- as.character(sku %||% "")[1]
  if (!nzchar(sku_s)) sku_s <- NA_character_
  reason_s <- as.character(reason %||% "adjust")[1]
  meta_json <- tryCatch(
    jsonlite::toJSON(meta %||% list(), auto_unbox = TRUE, null = "null"),
    error = function(e) "{}"
  )

  ynow_db_with(function(con) {
    ynow_points_ensure_schema(con)
    # Existing ref → idempotent
    prev <- DBI::dbGetQuery(
      con,
      "SELECT delta, balance_after FROM ynow_points_ledger
        WHERE user_id = $1::uuid AND ref_id = $2 LIMIT 1",
      params = list(uid, ref)
    )
    if (is.data.frame(prev) && nrow(prev) >= 1L) {
      return(list(
        ok = TRUE,
        balance = as.integer(prev$balance_after[[1]]),
        message_key = "pts_msg_ok",
        idempotent = TRUE,
        delta = as.integer(prev$delta[[1]])
      ))
    }
    DBI::dbWithTransaction(con, {
      locked <- DBI::dbGetQuery(
        con,
        "SELECT points_balance FROM ynow_users WHERE id = $1::uuid FOR UPDATE",
        params = list(uid)
      )
      if (!is.data.frame(locked) || nrow(locked) < 1L) {
        return(list(ok = FALSE, balance = NA_integer_, message_key = "pts_msg_no_user"))
      }
      bal <- as.integer(locked$points_balance[[1]])
      if (dlt < 0L && bal + dlt < 0L) {
        return(list(ok = FALSE, balance = bal, message_key = "pts_msg_insufficient"))
      }
      new_bal <- bal + dlt
      DBI::dbExecute(
        con,
        "UPDATE ynow_users SET points_balance = $1 WHERE id = $2::uuid",
        params = list(new_bal, uid)
      )
      DBI::dbExecute(
        con,
        "INSERT INTO ynow_points_ledger
           (user_id, delta, balance_after, reason, sku, ref_id, meta)
         VALUES ($1::uuid, $2, $3, $4, $5, $6, $7::jsonb)",
        params = list(uid, dlt, new_bal, reason_s, sku_s, ref, meta_json)
      )
      list(ok = TRUE, balance = new_bal, message_key = "pts_msg_ok", delta = dlt)
    })
  }) %||% list(ok = FALSE, balance = NA_integer_, message_key = "pts_msg_backend")
}

.ynow_points_neon_ledger_empty <- function() {
  data.frame(
    id = character(), created_at = character(), delta = integer(),
    balance_after = integer(), reason = character(), sku = character(),
    ref_id = character(), note = character(),
    stringsAsFactors = FALSE
  )
}

.ynow_points_neon_ledger_tail <- function(user_id, n = NULL) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(.ynow_points_neon_ledger_empty())
  lim <- suppressWarnings(as.integer(n)[1])
  ynow_db_with(function(con) {
    ynow_points_ensure_schema(con)
    # All rows for the user (newest first). Optional LIMIT when n is set.
    sql <- paste0(
      "SELECT id::text AS id, created_at::text AS created_at, delta, balance_after, reason,",
      " COALESCE(sku, '') AS sku, ref_id,",
      " COALESCE(meta->>'note', '') AS note",
      " FROM ynow_points_ledger",
      " WHERE user_id = $1::uuid",
      " ORDER BY created_at DESC"
    )
    if (is.finite(lim) && lim > 0L) {
      sql <- paste0(sql, " LIMIT $2")
      DBI::dbGetQuery(con, sql, params = list(uid, lim))
    } else {
      DBI::dbGetQuery(con, sql, params = list(uid))
    }
  }) %||% .ynow_points_neon_ledger_empty()
}

# ----- public API -----

ynow_points_balance <- function(user_id) {
  if (identical(ynow_points_backend(), "neon")) {
    return(.ynow_points_neon_balance(user_id))
  }
  .ynow_points_mem_balance(user_id)
}

ynow_points_catalog <- function() {
  if (identical(ynow_points_backend(), "neon")) {
    df <- ynow_db_with(function(con) {
      ynow_points_ensure_schema(con)
      DBI::dbGetQuery(
        con,
        "SELECT sku, title_en, title_zh_tw, cost, enabled FROM ynow_points_catalog
          WHERE enabled = TRUE ORDER BY sku"
      )
    })
    if (is.data.frame(df) && nrow(df) >= 1L) return(df)
  }
  ynow_points_catalog_defaults()
}

ynow_points_credit <- function(user_id, delta, reason = "adjust", ref_id = NULL,
                               sku = NULL, meta = NULL) {
  dlt <- as.integer(delta)[1]
  if (!is.finite(dlt) || dlt == 0L) {
    return(list(ok = FALSE, balance = ynow_points_balance(user_id),
                message_key = "pts_msg_bad_delta"))
  }
  if (identical(ynow_points_backend(), "neon")) {
    return(.ynow_points_neon_apply(user_id, dlt, reason, sku = sku,
                                   ref_id = ref_id, meta = meta))
  }
  .ynow_points_mem_apply(user_id, dlt, reason, sku = sku, ref_id = ref_id, meta = meta)
}

#' Spend points for a SKU (negative delta = cost). Idempotent on ref_id.
ynow_points_try_spend <- function(user_id, sku, ref_id, meta = NULL) {
  cost <- ynow_points_sku_cost(sku)
  if (!is.finite(cost)) {
    return(list(ok = FALSE, balance = ynow_points_balance(user_id),
                message_key = "pts_msg_unknown_sku"))
  }
  if (cost <= 0L) {
    return(list(ok = TRUE, balance = ynow_points_balance(user_id),
                message_key = "pts_msg_ok", delta = 0L))
  }
  ynow_points_credit(
    user_id,
    delta = -as.integer(cost),
    reason = "spend_sku",
    ref_id = ref_id,
    sku = sku,
    meta = meta
  )
}

#' Ledger rows for one user (newest first).
#' @param n NULL = all rows (DB stores every mutation); or positive integer limit.
ynow_points_ledger_tail <- function(user_id, n = NULL) {
  if (identical(ynow_points_backend(), "neon")) {
    return(.ynow_points_neon_ledger_tail(user_id, n = n))
  }
  .ynow_points_mem_ledger_tail(user_id, n = n)
}

#' Alias: full history for one user (every points mutation).
ynow_points_ledger_all <- function(user_id) {
  ynow_points_ledger_tail(user_id, n = NULL)
}

#' Grant signup bonus once (idempotent). Safe to call on every new account path.
ynow_points_grant_signup_bonus <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) {
    return(list(ok = FALSE, balance = NA_integer_, message_key = "pts_msg_no_user"))
  }
  bonus <- ynow_points_signup_bonus()
  if (bonus <= 0L) {
    return(list(ok = TRUE, balance = ynow_points_balance(uid), message_key = "pts_msg_ok"))
  }
  ynow_points_credit(
    uid,
    delta = bonus,
    reason = "earn_signup",
    ref_id = paste0("signup:", uid),
    meta = list(bonus = bonus)
  )
}

#' Shiny gate helper. Returns TRUE if the action may proceed.
#' When points disabled → always TRUE (current behavior).
#' Lite callers should skip before calling.
ynow_points_gate <- function(session, sku, ref_id, user = NULL, locale = "en") {
  if (!isTRUE(ynow_points_enabled())) return(TRUE)

  u <- user
  if (is.null(u)) {
    auth <- tryCatch(session$userData$ynow_auth, error = function(e) NULL)
    u <- tryCatch(auth$user(), error = function(e) NULL)
  }
  uid <- as.character(u$id %||% "")[1]
  if (!nzchar(uid)) {
    tryCatch(
      showNotification(
        tryCatch(ui_str("pts_msg_no_user", locale), error = function(e) {
          "Points require a signed-in account with a stored user id."
        }),
        type = "warning",
        duration = 6
      ),
      error = function(e) NULL
    )
    return(FALSE)
  }

  res <- ynow_points_try_spend(uid, sku, ref_id, meta = list(sku = sku))
  if (isTRUE(res$ok)) return(TRUE)

  mk <- as.character(res$message_key %||% "pts_msg_insufficient")[1]
  cost <- ynow_points_sku_cost(sku)
  bal <- res$balance
  title <- ynow_points_sku_title(sku, locale = locale)
  msg <- tryCatch(
    {
      if (identical(mk, "pts_msg_insufficient")) {
        sprintf(
          tryCatch(ui_str("pts_msg_insufficient_fmt", locale), error = function(e) {
            "Not enough points (need %s for %s; balance %s)."
          }),
          as.character(cost), title, as.character(bal %||% 0)
        )
      } else {
        ui_str(mk, locale)
      }
    },
    error = function(e) mk
  )
  tryCatch(showNotification(msg, type = "warning", duration = 8), error = function(e) NULL)
  FALSE
}

#' Resolve user id from auth handle or list.
ynow_points_user_id <- function(auth_or_user) {
  if (is.null(auth_or_user)) return("")
  if (is.list(auth_or_user) && !is.null(auth_or_user$id)) {
    return(as.character(auth_or_user$id %||% "")[1])
  }
  u <- tryCatch(auth_or_user$user(), error = function(e) NULL)
  as.character(u$id %||% "")[1]
}
