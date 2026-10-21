# ==========================================
# Points wallet — FYN (free) + PYN (paid)
# ==========================================
# Freemium usage points on Supabase Postgres (ledger = source of truth).
# Env:
#   YNOW_POINTS_ENABLED=0|1   (default 0 — shadow/off; gate always allows)
#   YNOW_POINTS_SIGNUP_BONUS  (default 100 FYN — first registration / first-login gift, once)
#   YNOW_POINTS_DAILY_CHECKIN (default 30 FYN — once per Asia/Taipei calendar day)
#   YNOW_POINTS_PRICE_ALERT_SLOT / HFV_RUN / BT_RUN  (optional overrides)
#   YNOW_POINTS_BACKEND=memory  (tests / no Postgres account store)
#   YNOW_POINTS_REDEEM_SEED     (optional) CODE:wallet:amount[:max],… e.g. DEMO100:pyn:100:1
#
# SKUs (membership lock catalog tier B): alert_slot=1, bt_run=5, pdf_report=10 — spent from FYN.
# Retired: hfv_run (never wired to ynow_points_gate; see ynow_non_membership_lock_catalog).
# Lite never spends points except the valuation PDF (login, 3 free / Taipei day, then 10 FYN).
# Spend is server-side only (idempotent ref_id).
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

#' UI label for Testing admin: memory | supabase | neon | postgres via host_kind (no secrets).
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
#' SKUs come from membership_locks.R (kind=points_sku, all tier B).
ynow_points_catalog_defaults <- function() {
  env_cost <- function(key, default) {
    n <- suppressWarnings(as.integer(Sys.getenv(key, unset = "")))
    if (is.finite(n) && n >= 0L) as.integer(n) else as.integer(default)
  }
  defs <- if (exists("ynow_membership_points_sku_defaults", mode = "function")) {
    ynow_membership_points_sku_defaults()
  } else {
    data.frame(
      sku = c("alert_slot", "bt_run", "pdf_report"),
      title_en = c("Value Alert slot", "Backtest full run", "Valuation report (PDF)"),
      title_zh_tw = c("到值通知一筆", "回測完整執行", "估值報告（PDF）"),
      cost_default = c(1L, 5L, 10L),
      env_key = c(
        "YNOW_POINTS_PRICE_ALERT_SLOT",
        "YNOW_POINTS_PRICE_BT_RUN",
        "YNOW_POINTS_PRICE_PDF_REPORT"
      ),
      enabled = c(TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    )
  }
  # Guard: every points SKU must be classified B in the membership catalog.
  if (exists("ynow_membership_lock_tier", mode = "function")) {
    for (sku in defs$sku) {
      tier <- ynow_membership_lock_tier(sku)
      if (!identical(tier, "B")) {
        stop(
          "points SKU '", sku, "' must be membership lock tier B (got ",
          if (is.na(tier)) "NA" else tier, ")",
          call. = FALSE
        )
      }
    }
  }
  data.frame(
    sku = defs$sku,
    title_en = defs$title_en,
    title_zh_tw = defs$title_zh_tw,
    cost = vapply(
      seq_len(nrow(defs)),
      function(i) env_cost(defs$env_key[[i]], defs$cost_default[[i]]),
      integer(1)
    ),
    enabled = as.logical(defs$enabled),
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
  .ynow_points_mem$codes <- new.env(parent = emptyenv())
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

# ----- Postgres schema helpers -----

.ynow_points_pg_has_column <- function(con, table, column) {
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

.ynow_points_pg_add_column <- function(con, sql_if, sql_plain) {
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
    if (!.ynow_points_pg_has_column(con, "ynow_users", "fyn_balance")) {
      .ynow_points_pg_add_column(
        con,
        "ALTER TABLE ynow_users ADD COLUMN IF NOT EXISTS fyn_balance INTEGER NOT NULL DEFAULT 0",
        "ALTER TABLE ynow_users ADD COLUMN fyn_balance INTEGER NOT NULL DEFAULT 0"
      )
    }
    if (!.ynow_points_pg_has_column(con, "ynow_users", "pyn_balance")) {
      .ynow_points_pg_add_column(
        con,
        "ALTER TABLE ynow_users ADD COLUMN IF NOT EXISTS pyn_balance INTEGER NOT NULL DEFAULT 0",
        "ALTER TABLE ynow_users ADD COLUMN pyn_balance INTEGER NOT NULL DEFAULT 0"
      )
    }
    # Keep legacy column for older readers; migrate into FYN once.
    if (!.ynow_points_pg_has_column(con, "ynow_users", "points_balance")) {
      .ynow_points_pg_add_column(
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
    if (!.ynow_points_pg_has_column(con, "ynow_points_ledger", "wallet")) {
      .ynow_points_pg_add_column(
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
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS ynow_pdf_quota (
        user_id TEXT NOT NULL,
        day_key TEXT NOT NULL,
        free_used INTEGER NOT NULL DEFAULT 0,
        PRIMARY KEY (user_id, day_key)
      )
    ")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS ynow_pdf_repeat (
        user_id TEXT NOT NULL,
        fingerprint TEXT NOT NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        PRIMARY KEY (user_id, fingerprint)
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
    # Retired membership SKU (never wired): keep row if present but disable spend.
    tryCatch(
      DBI::dbExecute(
        con,
        "UPDATE ynow_points_catalog SET enabled = FALSE WHERE sku = 'hfv_run'"
      ),
      error = function(e) NULL
    )
    # Redeem codes for Account points top-up (no payment processor).
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS ynow_points_redeem_codes (
        code TEXT PRIMARY KEY,
        wallet TEXT NOT NULL DEFAULT 'pyn',
        amount INTEGER NOT NULL CHECK (amount > 0),
        max_redemptions INTEGER,
        redemption_count INTEGER NOT NULL DEFAULT 0,
        enabled BOOLEAN NOT NULL DEFAULT TRUE,
        expires_at TIMESTAMPTZ,
        note TEXT,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
      )
    ")
    tryCatch(ynow_points_redeem_seed_from_env(conn = con), error = function(e) NULL)
    TRUE
  }
  if (!is.null(conn)) return(run(conn))
  if (!ynow_points_uses_db()) return(TRUE)
  out <- ynow_db_with(run)
  isTRUE(out)
}

.ynow_points_pg_balances <- function(user_id) {
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

.ynow_points_pg_balance <- function(user_id, wallet = "fyn") {
  bals <- .ynow_points_pg_balances(user_id)
  w <- ynow_points_norm_wallet(wallet)
  as.integer(bals[[w]] %||% NA_integer_)
}

.ynow_points_pg_apply <- function(user_id, delta, reason, sku = NULL, ref_id = NULL,
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

  # con is a checked-out DBI connection (not Pool) — dbWithTransaction works.
  # Nested balances/idempotent reads reuse the same checkout via ynow_db_with stack.
  out <- ynow_db_with(function(con) {
    ynow_points_ensure_schema(con)
    prev <- DBI::dbGetQuery(
      con,
      "SELECT delta, balance_after, COALESCE(wallet, 'fyn') AS wallet
         FROM ynow_points_ledger
        WHERE user_id = $1::uuid AND ref_id = $2 LIMIT 1",
      params = list(uid, ref)
    )
    if (is.data.frame(prev) && nrow(prev) >= 1L) {
      # Same connection — avoid nested checkout / second pool slot.
      bals_df <- DBI::dbGetQuery(
        con,
        "SELECT COALESCE(fyn_balance, 0) AS fyn, COALESCE(pyn_balance, 0) AS pyn
           FROM ynow_users WHERE id = $1::uuid LIMIT 1",
        params = list(uid)
      )
      bals <- if (is.data.frame(bals_df) && nrow(bals_df) >= 1L) {
        list(fyn = as.integer(bals_df$fyn[[1]]), pyn = as.integer(bals_df$pyn[[1]]))
      } else {
        list(fyn = NA_integer_, pyn = NA_integer_)
      }
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
  })
  out %||% list(ok = FALSE, balance = NA_integer_, fyn = NA_integer_, pyn = NA_integer_,
                message_key = "pts_msg_backend")
}

.ynow_points_pg_ledger_empty <- function() {
  data.frame(
    id = character(), created_at = character(), delta = integer(),
    balance_after = integer(), wallet = character(), reason = character(),
    sku = character(), ref_id = character(), note = character(),
    stringsAsFactors = FALSE
  )
}

.ynow_points_pg_ledger_tail <- function(user_id, n = NULL) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(.ynow_points_pg_ledger_empty())
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
  }) %||% .ynow_points_pg_ledger_empty()
}

.ynow_points_pg_list_users <- function() {
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
    return(.ynow_points_pg_balances(user_id))
  }
  .ynow_points_mem_balances(user_id)
}

#' FYN balance (spendable free points). Alias of balances$fyn for gate/spend.
ynow_points_balance <- function(user_id, wallet = "fyn") {
  if (ynow_points_uses_db()) {
    return(.ynow_points_pg_balance(user_id, wallet = wallet))
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
    return(.ynow_points_pg_apply(user_id, dlt, reason, sku = sku,
                                   ref_id = ref_id, meta = meta, wallet = w))
  }
  .ynow_points_mem_apply(user_id, dlt, reason, sku = sku, ref_id = ref_id,
                         meta = meta, wallet = w)
}

#' Admin adjust: add or subtract FYN and/or PYN in one call (separate ledger refs).
#' Re-reads balances after credit; fails if expected deltas are not reflected (no silent OK).
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
  before <- ynow_points_balances(uid)
  if (identical(fd, 0L) && identical(pd, 0L)) {
    return(list(ok = FALSE, message_key = "pts_msg_bad_delta",
                fyn = before$fyn, pyn = before$pyn))
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
  # Post-write verify against the same backend the credit used (DB or memory).
  bals <- ynow_points_balances(uid)
  bf <- suppressWarnings(as.integer(before$fyn %||% NA_integer_)[1])
  bp <- suppressWarnings(as.integer(before$pyn %||% NA_integer_)[1])
  af <- suppressWarnings(as.integer(bals$fyn %||% NA_integer_)[1])
  ap <- suppressWarnings(as.integer(bals$pyn %||% NA_integer_)[1])
  expect_fyn <- if (is.finite(bf)) bf + fd else NA_integer_
  expect_pyn <- if (is.finite(bp)) bp + pd else NA_integer_
  ok_fyn <- identical(fd, 0L) || (is.finite(af) && is.finite(expect_fyn) && identical(af, expect_fyn))
  ok_pyn <- identical(pd, 0L) || (is.finite(ap) && is.finite(expect_pyn) && identical(ap, expect_pyn))
  if (!isTRUE(ok_fyn) || !isTRUE(ok_pyn)) {
    return(list(
      ok = FALSE,
      message_key = "pts_msg_backend",
      fyn = bals$fyn,
      pyn = bals$pyn,
      verified = FALSE,
      fyn_delta = fd,
      pyn_delta = pd
    ))
  }
  list(
    ok = TRUE, message_key = "pts_msg_ok", fyn = bals$fyn, pyn = bals$pyn,
    fyn_delta = fd, pyn_delta = pd, verified = TRUE
  )
}

# ----- redeem codes (Account 點數儲值 / points top-up) -----

ynow_points_norm_redeem_code <- function(code) {
  c0 <- toupper(trimws(as.character(code %||% "")[1]))
  c0 <- gsub("[^A-Z0-9_-]", "", c0)
  if (!nzchar(c0) || nchar(c0) < 4L || nchar(c0) > 64L) "" else c0
}

#' Upsert a redeem code (tests / ops / env seed). Does not invent payment.
ynow_points_redeem_code_upsert <- function(code, amount, wallet = "pyn",
                                           max_redemptions = 1L,
                                           enabled = TRUE,
                                           expires_at = NULL,
                                           note = "") {
  c0 <- ynow_points_norm_redeem_code(code)
  amt <- suppressWarnings(as.integer(amount)[1])
  w <- ynow_points_norm_wallet(wallet)
  mx <- suppressWarnings(as.integer(max_redemptions)[1])
  if (!nzchar(c0) || !is.finite(amt) || amt < 1L) {
    return(list(ok = FALSE, message_key = "pts_topup_bad_code"))
  }
  if (!is.finite(mx) || mx < 1L) mx <- NA_integer_
  note_s <- as.character(note %||% "")[1]
  exp_s <- if (is.null(expires_at) || !nzchar(as.character(expires_at)[1])) {
    NA_character_
  } else {
    as.character(expires_at)[1]
  }
  if (ynow_points_uses_db()) {
    out <- ynow_db_with(function(con) {
      ynow_points_ensure_schema(con)
      DBI::dbExecute(
        con,
        "INSERT INTO ynow_points_redeem_codes
           (code, wallet, amount, max_redemptions, enabled, expires_at, note)
         VALUES ($1, $2, $3, $4, $5, $6::timestamptz, $7)
         ON CONFLICT (code) DO UPDATE SET
           wallet = EXCLUDED.wallet,
           amount = EXCLUDED.amount,
           max_redemptions = EXCLUDED.max_redemptions,
           enabled = EXCLUDED.enabled,
           expires_at = EXCLUDED.expires_at,
           note = EXCLUDED.note",
        params = list(
          c0, w, amt,
          if (is.na(mx)) NA else mx,
          isTRUE(enabled),
          if (is.na(exp_s)) NA else exp_s,
          if (nzchar(note_s)) note_s else NA_character_
        )
      )
      TRUE
    })
    return(list(ok = isTRUE(out), code = c0, wallet = w, amount = amt))
  }
  if (is.null(.ynow_points_mem$codes)) {
    .ynow_points_mem$codes <- new.env(parent = emptyenv())
  }
  prev <- .ynow_points_mem$codes[[c0]]
  .ynow_points_mem$codes[[c0]] <- list(
    code = c0,
    wallet = w,
    amount = amt,
    max_redemptions = if (is.na(mx)) NA_integer_ else mx,
    redemption_count = as.integer(prev$redemption_count %||% 0L),
    enabled = isTRUE(enabled),
    expires_at = exp_s,
    note = note_s
  )
  list(ok = TRUE, code = c0, wallet = w, amount = amt)
}

#' Seed redeem codes from YNOW_POINTS_REDEEM_SEED (CODE:wallet:amount[:max],…).
#' When `conn` is set (inside ensure_schema), writes with that connection only —
#' never re-enter ensure_schema / upsert to avoid recursion.
ynow_points_redeem_seed_from_env <- function(conn = NULL) {
  raw <- trimws(Sys.getenv("YNOW_POINTS_REDEEM_SEED", unset = ""))
  if (!nzchar(raw)) return(invisible(0L))
  parts <- strsplit(raw, "[,;]+")[[1]]
  n <- 0L
  for (p in parts) {
    p <- trimws(p)
    if (!nzchar(p)) next
    bits <- strsplit(p, ":", fixed = TRUE)[[1]]
    if (length(bits) < 3L) next
    code <- bits[[1]]
    wallet <- bits[[2]]
    amount <- bits[[3]]
    mx <- if (length(bits) >= 4L) bits[[4]] else "1"
    if (!is.null(conn)) {
      c0 <- ynow_points_norm_redeem_code(code)
      amt <- suppressWarnings(as.integer(amount)[1])
      w <- ynow_points_norm_wallet(wallet)
      mx_i <- suppressWarnings(as.integer(mx)[1])
      if (!nzchar(c0) || !is.finite(amt) || amt < 1L) next
      if (!is.finite(mx_i) || mx_i < 1L) mx_i <- NA_integer_
      ok <- tryCatch({
        DBI::dbExecute(
          conn,
          "INSERT INTO ynow_points_redeem_codes
             (code, wallet, amount, max_redemptions, enabled, note)
           VALUES ($1, $2, $3, $4, TRUE, 'env_seed')
           ON CONFLICT (code) DO UPDATE SET
             wallet = EXCLUDED.wallet,
             amount = EXCLUDED.amount,
             max_redemptions = EXCLUDED.max_redemptions,
             enabled = TRUE,
             note = EXCLUDED.note",
          params = list(c0, w, amt, if (is.na(mx_i)) NA else mx_i)
        )
        TRUE
      }, error = function(e) FALSE)
      if (isTRUE(ok)) n <- n + 1L
      next
    }
    res <- ynow_points_redeem_code_upsert(
      code, amount = amount, wallet = wallet, max_redemptions = mx,
      note = "env_seed"
    )
    if (isTRUE(res$ok)) n <- n + 1L
  }
  invisible(n)
}

.ynow_points_redeem_lookup <- function(code) {
  c0 <- ynow_points_norm_redeem_code(code)
  if (!nzchar(c0)) return(NULL)
  if (ynow_points_uses_db()) {
    return(ynow_db_with(function(con) {
      ynow_points_ensure_schema(con)
      df <- DBI::dbGetQuery(
        con,
        "SELECT code, wallet, amount, max_redemptions, redemption_count,
                enabled, expires_at, note
           FROM ynow_points_redeem_codes WHERE code = $1 LIMIT 1",
        params = list(c0)
      )
      if (!is.data.frame(df) || nrow(df) < 1L) return(NULL)
      list(
        code = as.character(df$code[[1]]),
        wallet = ynow_points_norm_wallet(df$wallet[[1]]),
        amount = as.integer(df$amount[[1]]),
        max_redemptions = suppressWarnings(as.integer(df$max_redemptions[[1]])),
        redemption_count = as.integer(df$redemption_count[[1]] %||% 0L),
        enabled = isTRUE(df$enabled[[1]]),
        expires_at = as.character(df$expires_at[[1]] %||% ""),
        note = as.character(df$note[[1]] %||% "")
      )
    }))
  }
  if (is.null(.ynow_points_mem$codes)) return(NULL)
  .ynow_points_mem$codes[[c0]]
}

.ynow_points_redeem_bump_count <- function(code) {
  c0 <- ynow_points_norm_redeem_code(code)
  if (!nzchar(c0)) return(FALSE)
  if (ynow_points_uses_db()) {
    out <- ynow_db_with(function(con) {
      DBI::dbExecute(
        con,
        "UPDATE ynow_points_redeem_codes
            SET redemption_count = COALESCE(redemption_count, 0) + 1
          WHERE code = $1",
        params = list(c0)
      )
      TRUE
    })
    return(isTRUE(out))
  }
  row <- .ynow_points_mem$codes[[c0]]
  if (is.null(row)) return(FALSE)
  row$redemption_count <- as.integer(row$redemption_count %||% 0L) + 1L
  .ynow_points_mem$codes[[c0]] <- row
  TRUE
}

#' Redeem a top-up code into the signed-in user's ledger (FYN or PYN).
#' Idempotent per user+code via ref_id. No payment processor.
ynow_points_redeem <- function(user_id, code) {
  uid <- as.character(user_id %||% "")[1]
  c0 <- ynow_points_norm_redeem_code(code)
  empty <- list(
    ok = FALSE, fyn = NA_integer_, pyn = NA_integer_,
    amount = NA_integer_, wallet = NA_character_,
    message_key = "pts_topup_bad_code"
  )
  if (!nzchar(uid)) {
    empty$message_key <- "pts_topup_no_user"
    return(empty)
  }
  if (!nzchar(c0)) return(empty)
  row <- .ynow_points_redeem_lookup(c0)
  if (is.null(row)) {
    empty$message_key <- "pts_topup_bad_code"
    bals <- ynow_points_balances(uid)
    empty$fyn <- bals$fyn
    empty$pyn <- bals$pyn
    return(empty)
  }
  bals0 <- ynow_points_balances(uid)
  if (!isTRUE(row$enabled)) {
    return(list(
      ok = FALSE, message_key = "pts_topup_disabled",
      fyn = bals0$fyn, pyn = bals0$pyn,
      amount = row$amount, wallet = row$wallet
    ))
  }
  exp <- as.character(row$expires_at %||% "")[1]
  if (nzchar(exp) && !identical(exp, "NA")) {
    exp_t <- tryCatch(as.POSIXct(exp, tz = "UTC"), error = function(e) NA)
    if (is.finite(as.numeric(exp_t)) && exp_t < Sys.time()) {
      return(list(
        ok = FALSE, message_key = "pts_topup_expired",
        fyn = bals0$fyn, pyn = bals0$pyn,
        amount = row$amount, wallet = row$wallet
      ))
    }
  }
  mx <- suppressWarnings(as.integer(row$max_redemptions)[1])
  used <- suppressWarnings(as.integer(row$redemption_count)[1])
  if (!is.finite(used)) used <- 0L
  if (is.finite(mx) && mx > 0L && used >= mx) {
    return(list(
      ok = FALSE, message_key = "pts_topup_exhausted",
      fyn = bals0$fyn, pyn = bals0$pyn,
      amount = row$amount, wallet = row$wallet
    ))
  }
  # Per-user uniqueness: same code cannot credit the same user twice (ledger ref_id).
  ref <- paste0("redeem:", c0, ":", uid)
  before <- bals0
  res <- ynow_points_credit(
    uid,
    delta = as.integer(row$amount),
    reason = "earn_redeem",
    ref_id = ref,
    sku = NULL,
    meta = list(code = c0, source = "redeem"),
    wallet = row$wallet
  )
  w <- ynow_points_norm_wallet(row$wallet)
  if (isTRUE(res$idempotent)) {
    return(list(
      ok = FALSE, message_key = "pts_topup_used",
      fyn = res$fyn %||% before$fyn, pyn = res$pyn %||% before$pyn,
      amount = row$amount, wallet = w
    ))
  }
  if (!isTRUE(res$ok)) {
    return(list(
      ok = FALSE,
      message_key = res$message_key %||% "pts_msg_backend",
      fyn = res$fyn %||% before$fyn, pyn = res$pyn %||% before$pyn,
      amount = row$amount, wallet = w
    ))
  }
  .ynow_points_redeem_bump_count(c0)
  list(
    ok = TRUE,
    message_key = "pts_topup_ok",
    fyn = res$fyn, pyn = res$pyn,
    amount = as.integer(row$amount),
    wallet = w,
    code = c0
  )
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
    return(.ynow_points_pg_ledger_tail(user_id, n = n))
  }
  .ynow_points_mem_ledger_tail(user_id, n = n)
}

ynow_points_ledger_all <- function(user_id) {
  ynow_points_ledger_tail(user_id, n = NULL)
}

#' De-identified user list for system-admin portal.
ynow_points_admin_list_users <- function() {
  if (ynow_points_uses_db()) {
    return(.ynow_points_pg_list_users())
  }
  .ynow_points_mem_list_users()
}

#' Expected system-admin portal password for `when` (Asia/Taipei YYYYMMDD).
#' Dynamic — never a fixed hardcoded secret. Same TZ as daily check-in.
ynow_sysadmin_expected_password <- function(when = Sys.time()) {
  format(as.POSIXlt(when, tz = ynow_points_tz()), "%Y%m%d")
}

#' Validate dedicated sysadmin portal credentials (not end-user Email login).
#' Username must be `ynow`; password = Asia/Taipei calendar date as YYYYMMDD.
#' Never logs the password. Returns list(ok=TRUE/FALSE) only.
ynow_sysadmin_validate_login <- function(username, password, when = Sys.time()) {
  u <- tolower(trimws(as.character(username %||% "")[1]))
  p <- as.character(password %||% "")[1]
  user_ok <- identical(u, "ynow")
  expect <- ynow_sysadmin_expected_password(when)
  pass_ok <- identical(p, expect)
  list(ok = isTRUE(user_ok) && isTRUE(pass_ok))
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
#' SKU must be a membership lock tier B points_sku when the catalog is loaded.
ynow_points_gate <- function(session, sku, ref_id, user = NULL, locale = "en") {
  if (!isTRUE(ynow_points_enabled())) return(TRUE)

  sku <- as.character(sku %||% "")[1]
  if (exists("ynow_membership_lock_tier", mode = "function") && nzchar(sku)) {
    tier <- ynow_membership_lock_tier(sku)
    if (!identical(tier, "B")) {
      warning(
        "ynow_points_gate: SKU '", sku, "' is not membership tier B; deny spend.",
        call. = FALSE
      )
      return(FALSE)
    }
  }

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

# Valuation PDF: 3 free downloads per Asia/Taipei day, then sku pdf_report (10 FYN).
# Charged even when YNOW_POINTS_ENABLED is off. Login is enforced by the download handler.

.ynow_pdf_quota <- new.env(parent = emptyenv())

ynow_pdf_free_per_day <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("YNOW_PDF_FREE_PER_DAY", unset = "3")))
  if (!is.finite(n) || n < 0L) 3L else as.integer(n)
}

#' Read-only PDF free-quota status (does not consume a slot).
#' Source of truth: `ynow_pdf_quota` DB row when connected, else in-process mem.
ynow_pdf_quota_status <- function(user_id = "", day = NULL) {
  uid <- as.character(user_id %||% "")[1]
  limit <- ynow_pdf_free_per_day()
  cost <- if (exists("ynow_points_sku_cost", mode = "function")) {
    suppressWarnings(as.integer(ynow_points_sku_cost("pdf_report"))[1])
  } else {
    10L
  }
  if (!is.finite(cost) || cost < 0L) cost <- 10L
  if (is.null(day) || !nzchar(as.character(day)[1])) {
    day <- if (exists("ynow_points_taipei_day", mode = "function")) {
      ynow_points_taipei_day()
    } else {
      format(Sys.Date(), "%Y-%m-%d")
    }
  }
  day <- as.character(day)[1]
  if (!nzchar(uid)) {
    return(list(
      ok = TRUE, logged_in = FALSE, used = 0L, left = as.integer(limit),
      limit = as.integer(limit), day = day, cost = as.integer(cost)
    ))
  }
  used <- 0L
  if (isTRUE(ynow_points_uses_db()) && exists("ynow_db_with", mode = "function")) {
    df <- tryCatch(ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        "SELECT free_used FROM ynow_pdf_quota WHERE user_id = $1 AND day_key = $2 LIMIT 1",
        params = list(uid, day)
      )
    }), error = function(e) NULL)
    if (is.data.frame(df) && nrow(df) >= 1L) {
      used <- suppressWarnings(as.integer(df$free_used[[1]])[1])
    }
  } else {
    used <- .ynow_pdf_mem_used(uid, day)
  }
  if (!is.finite(used) || used < 0L) used <- 0L
  used <- as.integer(used)
  list(
    ok = TRUE,
    logged_in = TRUE,
    used = used,
    left = as.integer(max(0L, limit - used)),
    limit = as.integer(limit),
    day = day,
    cost = as.integer(cost)
  )
}

ynow_pdf_reset_memory <- function() {
  .ynow_pdf_quota$used <- new.env(parent = emptyenv())
  .ynow_pdf_quota$repeats <- new.env(parent = emptyenv())
  invisible(TRUE)
}
ynow_pdf_reset_memory()

#' Drop one user's in-process points + PDF quota/repeat (account delete).
ynow_points_purge_user_local <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(invisible(FALSE))
  if (!is.null(.ynow_points_mem$users) && exists(uid, envir = .ynow_points_mem$users, inherits = FALSE)) {
    tryCatch(rm(list = uid, envir = .ynow_points_mem$users), error = function(e) NULL)
  }
  if (!is.null(.ynow_points_mem$profiles) &&
        exists(uid, envir = .ynow_points_mem$profiles, inherits = FALSE)) {
    tryCatch(rm(list = uid, envir = .ynow_points_mem$profiles), error = function(e) NULL)
  }
  if (is.list(.ynow_points_mem$ledger) && length(.ynow_points_mem$ledger)) {
    .ynow_points_mem$ledger <- Filter(
      function(r) !identical(as.character(r$user_id %||% "")[1], uid),
      .ynow_points_mem$ledger
    )
  }
  # PDF free-quota keys are "uid|day"; repeats keyed by uid.
  if (!is.null(.ynow_pdf_quota$used)) {
    keys <- ls(.ynow_pdf_quota$used, all.names = TRUE)
    drop <- keys[startsWith(keys, paste0(uid, "|"))]
    if (length(drop)) {
      tryCatch(rm(list = drop, envir = .ynow_pdf_quota$used), error = function(e) NULL)
    }
  }
  if (!is.null(.ynow_pdf_quota$repeats) &&
        exists(uid, envir = .ynow_pdf_quota$repeats, inherits = FALSE)) {
    tryCatch(rm(list = uid, envir = .ynow_pdf_quota$repeats), error = function(e) NULL)
  }
  invisible(TRUE)
}

.ynow_pdf_key <- function(user_id, day) {
  paste0(as.character(user_id)[1], "|", as.character(day)[1])
}

.ynow_pdf_mem_used <- function(user_id, day) {
  key <- .ynow_pdf_key(user_id, day)
  n <- .ynow_pdf_quota$used[[key]]
  if (is.null(n) || !is.finite(n)) 0L else as.integer(n)
}

.ynow_pdf_try_free <- function(user_id, day, limit) {
  uid <- as.character(user_id %||% "")[1]
  day <- as.character(day)[1]
  limit <- as.integer(limit)[1]
  if (isTRUE(ynow_points_uses_db()) && exists("ynow_db_with", mode = "function")) {
    df <- ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        paste(
          "INSERT INTO ynow_pdf_quota (user_id, day_key, free_used)",
          "VALUES ($1, $2, 1)",
          "ON CONFLICT (user_id, day_key) DO UPDATE",
          "SET free_used = ynow_pdf_quota.free_used + 1",
          "WHERE ynow_pdf_quota.free_used < $3",
          "RETURNING free_used"
        ),
        params = list(uid, day, limit)
      )
    })
    if (is.data.frame(df) && nrow(df) >= 1L) {
      used <- as.integer(df$free_used[[1]])
      return(list(ok = TRUE, used = used, left = max(0L, limit - used)))
    }
    return(list(ok = FALSE, used = limit, left = 0L))
  }
  used <- .ynow_pdf_mem_used(uid, day)
  if (used >= limit) return(list(ok = FALSE, used = used, left = 0L))
  used <- used + 1L
  .ynow_pdf_quota$used[[.ynow_pdf_key(uid, day)]] <- used
  list(ok = TRUE, used = used, left = max(0L, limit - used))
}

.ynow_pdf_release_free <- function(user_id, day) {
  uid <- as.character(user_id %||% "")[1]
  day <- as.character(day)[1]
  if (isTRUE(ynow_points_uses_db()) && exists("ynow_db_with", mode = "function")) {
    ynow_db_with(function(con) {
      DBI::dbExecute(
        con,
        "UPDATE ynow_pdf_quota SET free_used = GREATEST(free_used - 1, 0)
          WHERE user_id = $1 AND day_key = $2",
        params = list(uid, day)
      )
    })
    return(invisible(TRUE))
  }
  used <- .ynow_pdf_mem_used(uid, day)
  .ynow_pdf_quota$used[[.ynow_pdf_key(uid, day)]] <- max(0L, used - 1L)
  invisible(TRUE)
}

#' Stable key for one ticker plus its valuation settings. Empty when there is no ticker.
ynow_pdf_settings_key <- function(ticker, settings = list()) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(tk) || identical(tk, "NA")) return("")
  if (!is.list(settings)) settings <- list()
  nms <- sort(unique(names(settings)))
  parts <- vapply(nms, function(nm) {
    paste0(nm, "=", trimws(as.character(settings[[nm]] %||% "")[1]))
  }, character(1))
  paste(c(paste0("ticker=", tk), parts), collapse = "|")
}

ynow_pdf_repeat_seen <- function(user_id, fingerprint) {
  uid <- as.character(user_id %||% "")[1]
  fp <- trimws(as.character(fingerprint %||% "")[1])
  if (!nzchar(uid) || !nzchar(fp)) return(FALSE)
  if (isTRUE(ynow_points_uses_db()) && exists("ynow_db_with", mode = "function")) {
    hit <- tryCatch(ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        "SELECT 1 AS hit FROM ynow_pdf_repeat WHERE user_id = $1 AND fingerprint = $2 LIMIT 1",
        params = list(uid, fp)
      )
    }), error = function(e) NULL)
    if (is.data.frame(hit) && nrow(hit) >= 1L) return(TRUE)
  }
  remembered <- .ynow_pdf_quota$repeats[[uid]]
  is.character(remembered) && fp %in% remembered
}

ynow_pdf_repeat_mark <- function(user_id, fingerprint) {
  uid <- as.character(user_id %||% "")[1]
  fp <- trimws(as.character(fingerprint %||% "")[1])
  if (!nzchar(uid) || !nzchar(fp)) return(invisible(FALSE))
  remembered <- .ynow_pdf_quota$repeats[[uid]]
  if (is.null(remembered)) remembered <- character()
  .ynow_pdf_quota$repeats[[uid]] <- unique(c(remembered, fp))
  if (isTRUE(ynow_points_uses_db()) && exists("ynow_db_with", mode = "function")) {
    tryCatch(ynow_db_with(function(con) {
      DBI::dbExecute(
        con,
        "INSERT INTO ynow_pdf_repeat (user_id, fingerprint) VALUES ($1, $2)
         ON CONFLICT (user_id, fingerprint) DO NOTHING",
        params = list(uid, fp)
      )
    }), error = function(e) NULL)
  }
  invisible(TRUE)
}

#' Reserve one PDF download: free slot, else spend pdf_report.
#' A non-empty `fingerprint` that was already marked is a free repeat.
#' `day` is Asia/Taipei YYYY-MM-DD (tests may pass a fixed day).
ynow_pdf_claim <- function(user_id, day = NULL, fingerprint = NULL) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) {
    return(list(ok = FALSE, kind = "login", message_key = "pdf_need_login"))
  }
  fp <- trimws(as.character(fingerprint %||% "")[1])
  if (is.null(day) || !nzchar(as.character(day)[1])) day <- ynow_points_taipei_day()
  day <- as.character(day)[1]
  if (nzchar(fp) && isTRUE(ynow_pdf_repeat_seen(uid, fp))) {
    return(list(
      ok = TRUE, kind = "repeat", user_id = uid, day = day, fingerprint = fp,
      message_key = "pdf_repeat_free"
    ))
  }
  limit <- ynow_pdf_free_per_day()
  if (isTRUE(ynow_points_uses_db())) {
    tryCatch(ynow_points_ensure_schema(), error = function(e) NULL)
  }
  free <- .ynow_pdf_try_free(uid, day, limit)
  if (isTRUE(free$ok)) {
    return(list(
      ok = TRUE, kind = "free", user_id = uid, day = day, fingerprint = fp,
      used = free$used, limit = limit, left = free$left,
      message_key = "pdf_free_ok_fmt"
    ))
  }
  ref <- paste0(
    "pdf:", uid, ":", day, ":",
    format(Sys.time(), "%Y%m%d%H%M%OS3"), ":",
    paste(sample(c(letters, 0:9), 4L, replace = TRUE), collapse = "")
  )
  res <- ynow_points_try_spend(uid, "pdf_report", ref, meta = list(day = day))
  if (isTRUE(res$ok)) {
    return(list(
      ok = TRUE, kind = "paid", user_id = uid, day = day, ref_id = ref, fingerprint = fp,
      cost = ynow_points_sku_cost("pdf_report"),
      fyn = res$fyn %||% res$balance,
      message_key = "pdf_paid_ok_fmt"
    ))
  }
  list(
    ok = FALSE, kind = "paid", user_id = uid, day = day,
    message_key = res$message_key %||% "pts_msg_insufficient",
    fyn = res$fyn %||% res$balance,
    cost = ynow_points_sku_cost("pdf_report")
  )
}

#' Give back a free slot or refund FYN when PDF rendering fails.
ynow_pdf_release <- function(ticket) {
  if (!is.list(ticket) || !isTRUE(ticket$ok)) return(invisible(FALSE))
  if (identical(ticket$kind, "free")) {
    .ynow_pdf_release_free(ticket$user_id, ticket$day)
    return(invisible(TRUE))
  }
  if (identical(ticket$kind, "paid") && nzchar(as.character(ticket$ref_id %||% "")[1])) {
    cost <- as.integer(ticket$cost %||% ynow_points_sku_cost("pdf_report"))
    if (is.finite(cost) && cost > 0L) {
      ynow_points_credit(
        ticket$user_id,
        delta = cost,
        reason = "pdf_refund",
        ref_id = paste0("refund:", ticket$ref_id),
        sku = "pdf_report",
        wallet = "fyn"
      )
    }
  }
  invisible(TRUE)
}
