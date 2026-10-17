# ==========================================
# Points wallet — FYN (free) + PYN (paid)
# ==========================================
# Freemium usage points on Neon (ledger = source of truth).
# Env:
#   YNOW_POINTS_ENABLED=0|1   (default 0 — shadow/off; gate always allows)
#   YNOW_POINTS_SIGNUP_BONUS  (default 100 FYN — first registration / first-login gift, once)
#   YNOW_POINTS_DAILY_CHECKIN (default 30 FYN — once per Asia/Taipei calendar day)
#   YNOW_POINTS_PRICE_ALERT_SLOT / HFV_RUN / BT_RUN  (optional overrides)
#   YNOW_POINTS_BACKEND=memory  (tests / no Neon)
#
# SKUs (v1): alert_slot=1, hfv_run=3, bt_run=5 — spent from FYN.
# Lite never spends points. Spend is server-side only (idempotent ref_id).
# Legacy single `points_balance` migrates into FYN (no paid evidence → FYN).

.ynow_points_mem <- new.env(parent = emptyenv())
.ynow_points_mem$users <- new.env(parent = emptyenv())
.ynow_points_mem$ledger <- list()
.ynow_points_mem$profiles <- new.env(parent = emptyenv())

ynow_points_enabled <- function() {
  v <- tolower(trimws(Sys.getenv("YNOW_POINTS_ENABLED", unset = "0")))
  v %in% c("1", "true", "yes", "on")
}

#' First-gift FYN for new users (once). Default 100.
ynow_points_signup_bonus <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("YNOW_POINTS_SIGNUP_BONUS", unset = "100")))
  if (!is.finite(n) || n < 0L) 100L else as.integer(n)
}

#' Daily check-in FYN grant (once per Asia/Taipei calendar day). Default 30.
ynow_points_daily_checkin_amount <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("YNOW_POINTS_DAILY_CHECKIN", unset = "30")))
  if (!is.finite(n) || n < 0L) 30L else as.integer(n)
}

#' @deprecated Kept for tests / env probes; login no longer resets to this value.
ynow_points_login_grant <- function() {
  ynow_points_signup_bonus()
}

ynow_points_tz <- function() {
  "Asia/Taipei"
}

#' Calendar day key in Asia/Taipei (YYYY-MM-DD).
ynow_points_taipei_day <- function(when = Sys.time()) {
  format(as.POSIXlt(when, tz = ynow_points_tz()), "%Y-%m-%d")
}

ynow_points_backend <- function() {
  b <- tolower(trimws(Sys.getenv("YNOW_POINTS_BACKEND", unset = "")))
  if (identical(b, "memory")) return("memory")
  # Legacy force "neon" meant "use Postgres account store" — map to postgres.
  if (identical(b, "neon") || identical(b, "postgres") || identical(b, "db")) {
    if (exists("ynow_db_ready", mode = "function") && isTRUE(ynow_db_ready())) {
      return("postgres")
    }
    return("memory")
  }
  if (exists("ynow_db_ready", mode = "function") && isTRUE(ynow_db_ready())) {
    return("postgres")
  }
  "memory"
}

#' TRUE when FYN/PYN use the account Postgres store (not in-process memory).
ynow_points_uses_db <- function() {
  identical(ynow_points_backend(), "postgres")
}

#' UI label for Testing admin: memory | supabase | neon | postgres (no secrets).
ynow_points_backend_label <- function() {
  if (!isTRUE(ynow_points_uses_db())) return("memory")
  if (exists("ynow_db_host_kind", mode = "function")) {
    k <- tryCatch(ynow_db_host_kind(), error = function(e) "postgres")
    if (identical(k, "none")) return("postgres")
    return(as.character(k)[1])
  }
  "postgres"
}

ynow_points_norm_wallet <- function(wallet = "fyn") {
  w <- tolower(trimws(as.character(wallet %||% "fyn")[1]))
  if (identical(w, "pyn")) "pyn" else "fyn"
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

#' Opaque stable public id for de-identified admin lists (個資法-minded).
ynow_points_public_id <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return("")
  dig <- if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(paste0("ynow-pts-mask:", uid), algo = "sha256", serialize = FALSE)
  } else {
    # Stable fallback without digest: fold chars (no raw email/name).
    raw <- charToRaw(paste0("ynow-pts-mask:", uid))
    paste(as.character(raw), collapse = "")
  }
  paste0("U-", substr(dig, 1L, 12L))
}

# ----- memory backend (tests / offline) -----

.ynow_points_mem_reset <- function() {
  .ynow_points_mem$users <- new.env(parent = emptyenv())
  .ynow_points_mem$ledger <- list()
  .ynow_points_mem$profiles <- new.env(parent = emptyenv())
  invisible(TRUE)
}

#' Register a memory-backend profile row (for admin list tests).
ynow_points_mem_register_profile <- function(user_id, role = "user", disabled = FALSE) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(invisible(FALSE))
  .ynow_points_mem_ensure_user(uid)
  .ynow_points_mem$profiles[[uid]] <- list(
    role = as.character(role %||% "user")[1],
    disabled = isTRUE(disabled)
  )
  invisible(TRUE)
}

.ynow_points_mem_ensure_user <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(NULL)
  if (is.null(.ynow_points_mem$users[[uid]])) {
    .ynow_points_mem$users[[uid]] <- list(fyn = 0L, pyn = 0L)
  } else {
    u <- .ynow_points_mem$users[[uid]]
    # Migrate legacy single-balance memory rows → FYN.
    if (is.null(u$fyn) && !is.null(u$balance)) {
      .ynow_points_mem$users[[uid]] <- list(
        fyn = as.integer(u$balance %||% 0L),
        pyn = 0L
      )
    } else {
      if (is.null(u$fyn)) u$fyn <- 0L
      if (is.null(u$pyn)) u$pyn <- 0L
      .ynow_points_mem$users[[uid]] <- u
    }
  }
  uid
}

.ynow_points_mem_balances <- function(user_id) {
  uid <- .ynow_points_mem_ensure_user(user_id)
  if (is.null(uid)) {
    return(list(fyn = NA_integer_, pyn = NA_integer_))
  }
  u <- .ynow_points_mem$users[[uid]]
  list(
    fyn = as.integer(u$fyn %||% 0L),
    pyn = as.integer(u$pyn %||% 0L)
  )
}

.ynow_points_mem_balance <- function(user_id, wallet = "fyn") {
  bals <- .ynow_points_mem_balances(user_id)
  w <- ynow_points_norm_wallet(wallet)
  as.integer(bals[[w]] %||% NA_integer_)
}

.ynow_points_mem_apply <- function(user_id, delta, reason, sku = NULL, ref_id = NULL,
                                   meta = NULL, wallet = "fyn") {
  uid <- .ynow_points_mem_ensure_user(user_id)
  if (is.null(uid)) {
    return(list(ok = FALSE, balance = NA_integer_, fyn = NA_integer_, pyn = NA_integer_,
                message_key = "pts_msg_no_user"))
  }
  w <- ynow_points_norm_wallet(wallet)
  ref <- as.character(ref_id %||% "")[1]
  if (!nzchar(ref)) {
    bals <- .ynow_points_mem_balances(uid)
    return(list(ok = FALSE, balance = bals[[w]], fyn = bals$fyn, pyn = bals$pyn,
                message_key = "pts_msg_bad_ref"))
  }
  for (row in .ynow_points_mem$ledger) {
    if (identical(row$user_id, uid) && identical(row$ref_id, ref)) {
      bals <- .ynow_points_mem_balances(uid)
      return(list(
        ok = TRUE,
        balance = as.integer(row$balance_after),
        fyn = bals$fyn,
        pyn = bals$pyn,
        wallet = as.character(row$wallet %||% w),
        message_key = "pts_msg_ok",
        idempotent = TRUE,
        delta = as.integer(row$delta)
      ))
    }
  }
  bals <- .ynow_points_mem_balances(uid)
  bal <- as.integer(bals[[w]] %||% 0L)
  dlt <- as.integer(delta)[1]
  if (!is.finite(dlt)) {
    return(list(ok = FALSE, balance = bal, fyn = bals$fyn, pyn = bals$pyn,
                message_key = "pts_msg_bad_delta"))
  }
  if (dlt < 0L && bal + dlt < 0L) {
    return(list(ok = FALSE, balance = bal, fyn = bals$fyn, pyn = bals$pyn,
                message_key = "pts_msg_insufficient"))
  }
  new_bal <- bal + dlt
  .ynow_points_mem$users[[uid]][[w]] <- new_bal
  bals2 <- .ynow_points_mem_balances(uid)
  entry <- list(
    id = paste0("mem-", length(.ynow_points_mem$ledger) + 1L),
    user_id = uid,
    delta = dlt,
    balance_after = new_bal,
    wallet = w,
    reason = as.character(reason %||% "adjust")[1],
    sku = if (is.null(sku) || !nzchar(as.character(sku)[1])) NA_character_ else as.character(sku)[1],
    ref_id = ref,
    meta = meta,
    created_at = as.character(Sys.time())
  )
  .ynow_points_mem$ledger <- c(.ynow_points_mem$ledger, list(entry))
  list(
    ok = TRUE, balance = new_bal, fyn = bals2$fyn, pyn = bals2$pyn,
    wallet = w, message_key = "pts_msg_ok", delta = dlt
  )
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
    balance_after = integer(), wallet = character(), reason = character(),
    sku = character(), ref_id = character(), note = character(),
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
    wallet = vapply(rows, function(r) as.character(r$wallet %||% "fyn"), character(1)),
    reason = vapply(rows, function(r) as.character(r$reason), character(1)),
    sku = vapply(rows, function(r) as.character(r$sku %||% ""), character(1)),
    ref_id = vapply(rows, function(r) as.character(r$ref_id), character(1)),
    note = vapply(rows, function(r) .ynow_points_mem_meta_note(r$meta), character(1)),
    stringsAsFactors = FALSE
  )
}

.ynow_points_mem_list_users <- function() {
  uids <- ls(.ynow_points_mem$users, all.names = TRUE)
  if (!length(uids)) {
    return(data.frame(
      public_id = character(), user_id = character(),
      fyn = integer(), pyn = integer(),
      role = character(), disabled = logical(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(uids, function(uid) {
    bals <- .ynow_points_mem_balances(uid)
    prof <- .ynow_points_mem$profiles[[uid]]
    data.frame(
      public_id = ynow_points_public_id(uid),
      user_id = uid,
      fyn = bals$fyn,
      pyn = bals$pyn,
      role = as.character(prof$role %||% "user")[1],
      disabled = isTRUE(prof$disabled),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# ----- Neon schema helpers -----

.ynow_points_neon_has_column <- function(con, table, column) {
  df <- tryCatch(
    DBI::dbGetQuery(
      con,
      "SELECT 1 AS ok FROM information_schema.columns
        WHERE table_name = $1 AND column_name = $2 LIMIT 1",
      params = list(as.character(table)[1], as.character(column)[1])
    ),
    error = function(e) data.frame()
  )
  is.data.frame(df) && nrow(df) >= 1L
}

.ynow_points_neon_add_column <- function(con, sql_if, sql_plain) {
  tryCatch(
    DBI::dbExecute(con, sql_if),
    error = function(e) {
      tryCatch(DBI::dbExecute(con, sql_plain), error = function(e2) NULL)
    }
  )
}

ynow_points_ensure_schema <- function(conn = NULL) {
  run <- function(con) {
    # Dual wallet columns
    if (!.ynow_points_neon_has_column(con, "ynow_users", "fyn_balance")) {
      .ynow_points_neon_add_column(
        con,
        "ALTER TABLE ynow_users ADD COLUMN IF NOT EXISTS fyn_balance INTEGER NOT NULL DEFAULT 0",
        "ALTER TABLE ynow_users ADD COLUMN fyn_balance INTEGER NOT NULL DEFAULT 0"
      )
    }
    if (!.ynow_points_neon_has_column(con, "ynow_users", "pyn_balance")) {
      .ynow_points_neon_add_column(
        con,
        "ALTER TABLE ynow_users ADD COLUMN IF NOT EXISTS pyn_balance INTEGER NOT NULL DEFAULT 0",
        "ALTER TABLE ynow_users ADD COLUMN pyn_balance INTEGER NOT NULL DEFAULT 0"
      )
    }
    # Keep legacy column for older readers; migrate into FYN once.
    if (!.ynow_points_neon_has_column(con, "ynow_users", "points_balance")) {
      .ynow_points_neon_add_column(
        con,
        "ALTER TABLE ynow_users ADD COLUMN IF NOT EXISTS points_balance INTEGER NOT NULL DEFAULT 0",
        "ALTER TABLE ynow_users ADD COLUMN points_balance INTEGER NOT NULL DEFAULT 0"
      )
    }
    # One-time migrate: legacy single balance → FYN when FYN still 0 and legacy > 0.
    tryCatch(
      DBI::dbExecute(
        con,
        "UPDATE ynow_users
            SET fyn_balance = points_balance
          WHERE COALESCE(fyn_balance, 0) = 0
            AND COALESCE(pyn_balance, 0) = 0
            AND COALESCE(points_balance, 0) > 0"
      ),
      error = function(e) NULL
    )
    # Keep points_balance mirrored to FYN for any legacy SELECT paths.
    tryCatch(
      DBI::dbExecute(
        con,
        "UPDATE ynow_users SET points_balance = COALESCE(fyn_balance, 0)
          WHERE points_balance IS DISTINCT FROM COALESCE(fyn_balance, 0)"
      ),
      error = function(e) NULL
    )

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
    if (!.ynow_points_neon_has_column(con, "ynow_points_ledger", "wallet")) {
      .ynow_points_neon_add_column(
        con,
        "ALTER TABLE ynow_points_ledger ADD COLUMN IF NOT EXISTS wallet TEXT NOT NULL DEFAULT 'fyn'",
        "ALTER TABLE ynow_points_ledger ADD COLUMN wallet TEXT NOT NULL DEFAULT 'fyn'"
      )
      tryCatch(
        DBI::dbExecute(
          con,
          "UPDATE ynow_points_ledger SET wallet = 'fyn' WHERE wallet IS NULL OR wallet = ''"
        ),
        error = function(e) NULL
      )
    }
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
  if (!ynow_points_uses_db()) return(TRUE)
  out <- ynow_db_with(run)
  isTRUE(out)
}

.ynow_points_neon_balances <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(list(fyn = NA_integer_, pyn = NA_integer_))
  ynow_db_with(function(con) {
    ynow_points_ensure_schema(con)
    df <- DBI::dbGetQuery(
      con,
      "SELECT COALESCE(fyn_balance, 0) AS fyn, COALESCE(pyn_balance, 0) AS pyn
         FROM ynow_users WHERE id = $1::uuid LIMIT 1",
      params = list(uid)
    )
    if (!is.data.frame(df) || nrow(df) < 1L) {
      return(list(fyn = NA_integer_, pyn = NA_integer_))
    }
    list(fyn = as.integer(df$fyn[[1]]), pyn = as.integer(df$pyn[[1]]))
  }) %||% list(fyn = NA_integer_, pyn = NA_integer_)
}

.ynow_points_neon_balance <- function(user_id, wallet = "fyn") {
  bals <- .ynow_points_neon_balances(user_id)
  w <- ynow_points_norm_wallet(wallet)
  as.integer(bals[[w]] %||% NA_integer_)
}

.ynow_points_neon_apply <- function(user_id, delta, reason, sku = NULL, ref_id = NULL,
                                    meta = NULL, wallet = "fyn") {
  uid <- as.character(user_id %||% "")[1]
  ref <- as.character(ref_id %||% "")[1]
  dlt <- as.integer(delta)[1]
  w <- ynow_points_norm_wallet(wallet)
  if (!nzchar(uid)) {
    return(list(ok = FALSE, balance = NA_integer_, fyn = NA_integer_, pyn = NA_integer_,
                message_key = "pts_msg_no_user"))
  }
  if (!nzchar(ref)) {
    return(list(ok = FALSE, balance = NA_integer_, fyn = NA_integer_, pyn = NA_integer_,
                message_key = "pts_msg_bad_ref"))
  }
  if (!is.finite(dlt)) {
    return(list(ok = FALSE, balance = NA_integer_, fyn = NA_integer_, pyn = NA_integer_,
                message_key = "pts_msg_bad_delta"))
  }
  sku_s <- as.character(sku %||% "")[1]
  if (!nzchar(sku_s)) sku_s <- NA_character_
  reason_s <- as.character(reason %||% "adjust")[1]
  meta_json <- tryCatch(
    jsonlite::toJSON(meta %||% list(), auto_unbox = TRUE, null = "null"),
    error = function(e) "{}"
  )
  bal_col <- if (identical(w, "pyn")) "pyn_balance" else "fyn_balance"

  ynow_db_with(function(con) {
    ynow_points_ensure_schema(con)
    prev <- DBI::dbGetQuery(
      con,
      "SELECT delta, balance_after, COALESCE(wallet, 'fyn') AS wallet
         FROM ynow_points_ledger
        WHERE user_id = $1::uuid AND ref_id = $2 LIMIT 1",
      params = list(uid, ref)
    )
    if (is.data.frame(prev) && nrow(prev) >= 1L) {
      bals <- .ynow_points_neon_balances(uid)
      return(list(
        ok = TRUE,
        balance = as.integer(prev$balance_after[[1]]),
        fyn = bals$fyn,
        pyn = bals$pyn,
        wallet = as.character(prev$wallet[[1]]),
        message_key = "pts_msg_ok",
        idempotent = TRUE,
        delta = as.integer(prev$delta[[1]])
      ))
    }
    DBI::dbWithTransaction(con, {
      locked <- DBI::dbGetQuery(
        con,
        "SELECT COALESCE(fyn_balance, 0) AS fyn, COALESCE(pyn_balance, 0) AS pyn
           FROM ynow_users WHERE id = $1::uuid FOR UPDATE",
        params = list(uid)
      )
      if (!is.data.frame(locked) || nrow(locked) < 1L) {
        return(list(ok = FALSE, balance = NA_integer_, fyn = NA_integer_, pyn = NA_integer_,
                    message_key = "pts_msg_no_user"))
      }
      fyn <- as.integer(locked$fyn[[1]])
      pyn <- as.integer(locked$pyn[[1]])
      bal <- if (identical(w, "pyn")) pyn else fyn
      if (dlt < 0L && bal + dlt < 0L) {
        return(list(ok = FALSE, balance = bal, fyn = fyn, pyn = pyn,
                    message_key = "pts_msg_insufficient"))
      }
      new_bal <- bal + dlt
      if (identical(w, "pyn")) {
        pyn <- new_bal
      } else {
        fyn <- new_bal
      }
      DBI::dbExecute(
        con,
        paste0(
          "UPDATE ynow_users SET ", bal_col, " = $1, points_balance = $2 ",
          "WHERE id = $3::uuid"
        ),
        params = list(new_bal, fyn, uid)
      )
      DBI::dbExecute(
        con,
        "INSERT INTO ynow_points_ledger
           (user_id, delta, balance_after, reason, sku, ref_id, meta, wallet)
         VALUES ($1::uuid, $2, $3, $4, $5, $6, $7::jsonb, $8)",
        params = list(uid, dlt, new_bal, reason_s, sku_s, ref, meta_json, w)
      )
      list(ok = TRUE, balance = new_bal, fyn = fyn, pyn = pyn, wallet = w,
           message_key = "pts_msg_ok", delta = dlt)
    })
  }) %||% list(ok = FALSE, balance = NA_integer_, fyn = NA_integer_, pyn = NA_integer_,
               message_key = "pts_msg_backend")
}

.ynow_points_neon_ledger_empty <- function() {
  data.frame(
    id = character(), created_at = character(), delta = integer(),
    balance_after = integer(), wallet = character(), reason = character(),
    sku = character(), ref_id = character(), note = character(),
    stringsAsFactors = FALSE
  )
}

.ynow_points_neon_ledger_tail <- function(user_id, n = NULL) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(.ynow_points_neon_ledger_empty())
  lim <- suppressWarnings(as.integer(n)[1])
  ynow_db_with(function(con) {
    ynow_points_ensure_schema(con)
    sql <- paste0(
      "SELECT id::text AS id, created_at::text AS created_at, delta, balance_after,",
      " COALESCE(wallet, 'fyn') AS wallet, reason,",
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

.ynow_points_neon_list_users <- function() {
  ynow_db_with(function(con) {
    ynow_points_ensure_schema(con)
    df <- DBI::dbGetQuery(
      con,
      "SELECT id::text AS user_id,
              COALESCE(fyn_balance, 0) AS fyn,
              COALESCE(pyn_balance, 0) AS pyn,
              COALESCE(role, 'user') AS role,
              COALESCE(disabled, FALSE) AS disabled
         FROM ynow_users
        ORDER BY created_at ASC NULLS LAST, id ASC"
    )
    if (!is.data.frame(df) || nrow(df) < 1L) {
      return(data.frame(
        public_id = character(), user_id = character(),
        fyn = integer(), pyn = integer(),
        role = character(), disabled = logical(),
        stringsAsFactors = FALSE
      ))
    }
    df$public_id <- vapply(df$user_id, ynow_points_public_id, character(1))
    df$disabled <- as.logical(df$disabled)
    df[, c("public_id", "user_id", "fyn", "pyn", "role", "disabled"), drop = FALSE]
  }) %||% data.frame(
    public_id = character(), user_id = character(),
    fyn = integer(), pyn = integer(),
    role = character(), disabled = logical(),
    stringsAsFactors = FALSE
  )
}

# ----- public API -----

#' Dual balances: list(fyn=, pyn=).
ynow_points_balances <- function(user_id) {
  if (ynow_points_uses_db()) {
    return(.ynow_points_neon_balances(user_id))
  }
  .ynow_points_mem_balances(user_id)
}

#' FYN balance (spendable free points). Alias of balances$fyn for gate/spend.
ynow_points_balance <- function(user_id, wallet = "fyn") {
  if (ynow_points_uses_db()) {
    return(.ynow_points_neon_balance(user_id, wallet = wallet))
  }
  .ynow_points_mem_balance(user_id, wallet = wallet)
}

ynow_points_catalog <- function() {
  if (ynow_points_uses_db()) {
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

#' Credit / debit a wallet (default FYN). Idempotent on ref_id.
ynow_points_credit <- function(user_id, delta, reason = "adjust", ref_id = NULL,
                               sku = NULL, meta = NULL, wallet = "fyn") {
  dlt <- as.integer(delta)[1]
  w <- ynow_points_norm_wallet(wallet)
  if (!is.finite(dlt) || dlt == 0L) {
    bals <- ynow_points_balances(user_id)
    return(list(
      ok = FALSE, balance = bals[[w]], fyn = bals$fyn, pyn = bals$pyn,
      wallet = w, message_key = "pts_msg_bad_delta"
    ))
  }
  if (ynow_points_uses_db()) {
    return(.ynow_points_neon_apply(user_id, dlt, reason, sku = sku,
                                   ref_id = ref_id, meta = meta, wallet = w))
  }
  .ynow_points_mem_apply(user_id, dlt, reason, sku = sku, ref_id = ref_id,
                         meta = meta, wallet = w)
}

#' Admin adjust: add or subtract FYN and/or PYN in one call (separate ledger refs).
ynow_points_admin_adjust <- function(user_id, fyn_delta = 0L, pyn_delta = 0L,
                                     note = "", by = "testing_admin") {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) {
    return(list(ok = FALSE, message_key = "pts_msg_no_user",
                fyn = NA_integer_, pyn = NA_integer_))
  }
  fd <- suppressWarnings(as.integer(fyn_delta)[1])
  pd <- suppressWarnings(as.integer(pyn_delta)[1])
  if (!is.finite(fd)) fd <- 0L
  if (!is.finite(pd)) pd <- 0L
  if (identical(fd, 0L) && identical(pd, 0L)) {
    bals <- ynow_points_balances(uid)
    return(list(ok = FALSE, message_key = "pts_msg_bad_delta",
                fyn = bals$fyn, pyn = bals$pyn))
  }
  stamp <- paste0(
    as.integer(as.numeric(Sys.time())), ":",
    paste(sample(c(letters, 0:9), 8L, replace = TRUE), collapse = "")
  )
  note_s <- as.character(note %||% "")[1]
  by_s <- as.character(by %||% "testing_admin")[1]
  res_fyn <- NULL
  res_pyn <- NULL
  if (!identical(fd, 0L)) {
    res_fyn <- ynow_points_credit(
      uid, delta = fd, reason = "earn_admin",
      ref_id = paste0("admin_adj:fyn:", uid, ":", stamp),
      meta = list(by = by_s, note = note_s, wallet = "fyn"),
      wallet = "fyn"
    )
    if (!isTRUE(res_fyn$ok)) {
      return(list(
        ok = FALSE,
        message_key = res_fyn$message_key %||% "pts_msg_backend",
        fyn = res_fyn$fyn, pyn = res_pyn$pyn %||% ynow_points_balances(uid)$pyn
      ))
    }
  }
  if (!identical(pd, 0L)) {
    res_pyn <- ynow_points_credit(
      uid, delta = pd, reason = "earn_admin",
      ref_id = paste0("admin_adj:pyn:", uid, ":", stamp),
      meta = list(by = by_s, note = note_s, wallet = "pyn"),
      wallet = "pyn"
    )
    if (!isTRUE(res_pyn$ok)) {
      bals <- ynow_points_balances(uid)
      return(list(
        ok = FALSE,
        message_key = res_pyn$message_key %||% "pts_msg_backend",
        fyn = bals$fyn, pyn = bals$pyn
      ))
    }
  }
  bals <- ynow_points_balances(uid)
  list(ok = TRUE, message_key = "pts_msg_ok", fyn = bals$fyn, pyn = bals$pyn,
       fyn_delta = fd, pyn_delta = pd)
}

#' Spend FYN for a SKU (negative delta = cost). Idempotent on ref_id.
ynow_points_try_spend <- function(user_id, sku, ref_id, meta = NULL) {
  cost <- ynow_points_sku_cost(sku)
  if (!is.finite(cost)) {
    bals <- ynow_points_balances(user_id)
    return(list(ok = FALSE, balance = bals$fyn, fyn = bals$fyn, pyn = bals$pyn,
                message_key = "pts_msg_unknown_sku"))
  }
  if (cost <= 0L) {
    bals <- ynow_points_balances(user_id)
    return(list(ok = TRUE, balance = bals$fyn, fyn = bals$fyn, pyn = bals$pyn,
                message_key = "pts_msg_ok", delta = 0L))
  }
  ynow_points_credit(
    user_id,
    delta = -as.integer(cost),
    reason = "spend_sku",
    ref_id = ref_id,
    sku = sku,
    meta = meta,
    wallet = "fyn"
  )
}

#' Ledger rows for one user (newest first).
ynow_points_ledger_tail <- function(user_id, n = NULL) {
  if (ynow_points_uses_db()) {
    return(.ynow_points_neon_ledger_tail(user_id, n = n))
  }
  .ynow_points_mem_ledger_tail(user_id, n = n)
}

ynow_points_ledger_all <- function(user_id) {
  ynow_points_ledger_tail(user_id, n = NULL)
}

#' De-identified user list for Testing-tab system admin.
ynow_points_admin_list_users <- function() {
  if (ynow_points_uses_db()) {
    return(.ynow_points_neon_list_users())
  }
  .ynow_points_mem_list_users()
}

#' Resolve internal user_id from opaque public_id (admin UI).
ynow_points_resolve_public_id <- function(public_id) {
  pid <- as.character(public_id %||% "")[1]
  if (!nzchar(pid)) return("")
  df <- ynow_points_admin_list_users()
  if (!is.data.frame(df) || !nrow(df)) return("")
  hit <- df[df$public_id == pid, , drop = FALSE]
  if (!nrow(hit)) return("")
  as.character(hit$user_id[[1]])
}

#' Grant first-registration / first-login gift once (idempotent). Default 100 FYN.
ynow_points_grant_signup_bonus <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) {
    return(list(ok = FALSE, balance = NA_integer_, fyn = NA_integer_, pyn = NA_integer_,
                message_key = "pts_msg_no_user"))
  }
  bonus <- ynow_points_signup_bonus()
  if (bonus <= 0L) {
    bals <- ynow_points_balances(uid)
    return(list(ok = TRUE, balance = bals$fyn, fyn = bals$fyn, pyn = bals$pyn,
                message_key = "pts_msg_ok", skipped = TRUE))
  }
  ynow_points_credit(
    uid,
    delta = bonus,
    reason = "earn_signup",
    ref_id = paste0("signup:", uid),
    meta = list(bonus = bonus, wallet = "fyn"),
    wallet = "fyn"
  )
}

#' Daily check-in: +N FYN once per Asia/Taipei calendar day (idempotent).
ynow_points_grant_daily_checkin <- function(user_id, when = Sys.time()) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) {
    return(list(ok = FALSE, balance = NA_integer_, fyn = NA_integer_, pyn = NA_integer_,
                message_key = "pts_msg_no_user"))
  }
  amt <- ynow_points_daily_checkin_amount()
  day <- ynow_points_taipei_day(when)
  if (amt <= 0L) {
    bals <- ynow_points_balances(uid)
    return(list(ok = TRUE, balance = bals$fyn, fyn = bals$fyn, pyn = bals$pyn,
                message_key = "pts_msg_ok", skipped = TRUE, day = day))
  }
  ynow_points_credit(
    uid,
    delta = amt,
    reason = "earn_daily_checkin",
    ref_id = paste0("daily_checkin:", uid, ":", day),
    meta = list(amount = amt, day = day, tz = ynow_points_tz(), wallet = "fyn"),
    wallet = "fyn"
  )
}

#' Login hook: first gift once + daily check-in once/day. Never resets balance to 100.
ynow_points_on_login <- function(user_id, when = Sys.time()) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) {
    return(list(ok = FALSE, balance = NA_integer_, fyn = NA_integer_, pyn = NA_integer_,
                message_key = "pts_msg_no_user"))
  }
  if (identical(ynow_points_backend(), "memory")) {
    .ynow_points_mem_ensure_user(uid)
  }
  gift <- ynow_points_grant_signup_bonus(uid)
  checkin <- ynow_points_grant_daily_checkin(uid, when = when)
  bals <- ynow_points_balances(uid)
  list(
    ok = TRUE,
    balance = bals$fyn,
    fyn = bals$fyn,
    pyn = bals$pyn,
    message_key = "pts_msg_ok",
    gift = gift,
    checkin = checkin,
    gift_granted = isTRUE(gift$ok) && !isTRUE(gift$idempotent) && !isTRUE(gift$skipped) &&
      isTRUE((gift$delta %||% 0L) > 0L),
    checkin_granted = isTRUE(checkin$ok) && !isTRUE(checkin$idempotent) &&
      !isTRUE(checkin$skipped) && isTRUE((checkin$delta %||% 0L) > 0L)
  )
}

#' @deprecated No-op kept so old callers do not reset balances.
#' Use ynow_points_on_login() instead.
ynow_points_reset_on_login <- function(user_id, target = NULL) {
  uid <- as.character(user_id %||% "")[1]
  bals <- if (nzchar(uid)) ynow_points_balances(uid) else list(fyn = NA_integer_, pyn = NA_integer_)
  list(
    ok = TRUE,
    balance = bals$fyn,
    fyn = bals$fyn,
    pyn = bals$pyn,
    message_key = "pts_msg_ok",
    delta = 0L,
    skipped = TRUE,
    deprecated = TRUE
  )
}

#' Shiny gate helper. Returns TRUE if the action may proceed.
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
  bal <- res$balance %||% res$fyn
  title <- ynow_points_sku_title(sku, locale = locale)
  msg <- tryCatch(
    {
      if (identical(mk, "pts_msg_insufficient")) {
        sprintf(
          tryCatch(ui_str("pts_msg_insufficient_fmt", locale), error = function(e) {
            "Not enough FYN points (need %s for %s; FYN balance %s)."
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
