# Delete account: confirm gate + purge helper (memory / local stores)
# Run: Rscript app_25.0/tests/test_delete_account.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_delete_account.R", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

pass <- 0L
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  if (ok) {
    pass <<- pass + 1L
    cat("OK", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL", label, "\n")
  }
  invisible(ok)
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

src_locale <- file.path(app_dir, "ui_locale.R")
src_auth <- file.path(app_dir, "auth_account.R")
src_db <- file.path(app_dir, "auth_db.R")
src_oauth <- file.path(app_dir, "auth_oauth.R")
src_points <- file.path(app_dir, "points_wallet.R")
src_prefs <- file.path(app_dir, "user_prefs.R")
src_access <- file.path(app_dir, "access_locks.R")
src_alerts <- file.path(app_dir, "price_alert.R")

suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite required")
  if (!requireNamespace("digest", quietly = TRUE)) stop("digest required")
  if (!requireNamespace("openssl", quietly = TRUE)) stop("openssl required")
  if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny required")
  library(shiny)
  library(htmltools)
})

source(src_locale, local = FALSE, encoding = "UTF-8")
source(src_auth, local = FALSE, encoding = "UTF-8")
source(src_db, local = FALSE, encoding = "UTF-8")
source(src_oauth, local = FALSE, encoding = "UTF-8")
source(src_points, local = FALSE, encoding = "UTF-8")
source(src_prefs, local = FALSE, encoding = "UTF-8")
source(src_access, local = FALSE, encoding = "UTF-8")
source(src_alerts, local = FALSE, encoding = "UTF-8")

# Isolate from live YNOW_DB_* / Renviron for unit paths.
.db_env_keys <- c(
  "YNOW_DB_HOST", "YNOW_DB_USER", "YNOW_DB_PASSWORD", "YNOW_DB_NAME",
  "YNOW_DB_PORT", "YNOW_DB_SSLMODE", "YNOW_DATABASE_URL", "DATABASE_URL",
  "YNOW_POINTS_BACKEND", "YNOW_USER_PREFS_FORCE_FILE", "YNOW_ACCESS_LOCKS_PATH",
  "YNOW_ALERT_STORE", "YNOW_ALERT_HISTORY", "YNOW_SUPABASE_SERVICE_ROLE"
)
.old_db <- setNames(lapply(.db_env_keys, function(k) Sys.getenv(k, unset = NA_character_)), .db_env_keys)
on.exit({
  for (k in names(.old_db)) {
    v <- .old_db[[k]]
    if (is.na(v)) Sys.unsetenv(k) else do.call(Sys.setenv, setNames(list(v), k))
  }
}, add = TRUE)
for (k in .db_env_keys) Sys.unsetenv(k)
Sys.setenv(YNOW_POINTS_BACKEND = "memory", YNOW_USER_PREFS_FORCE_FILE = "1")

tmp_root <- tempfile("ynow-delete-")
dir.create(tmp_root, recursive = TRUE)
Sys.setenv(
  YNOW_ACCESS_LOCKS_PATH = file.path(tmp_root, "access_locks.json"),
  YNOW_ALERT_STORE = file.path(tmp_root, "price_alerts.json"),
  YNOW_ALERT_HISTORY = file.path(tmp_root, "price_alert_history.json")
)
# Prefs file root follows getwd(); keep cwd at app_dir for relative data paths.
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(app_dir)
prefs_dir <- file.path(app_dir, "ynow_data")
prefs_bak <- file.path(tmp_root, "user_prefs.json.bak")
prefs_path <- file.path(prefs_dir, "user_prefs.json")
if (file.exists(prefs_path)) {
  file.copy(prefs_path, prefs_bak, overwrite = TRUE)
}
on.exit({
  if (file.exists(prefs_bak)) {
    dir.create(prefs_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(prefs_bak, prefs_path, overwrite = TRUE)
  } else if (file.exists(prefs_path)) {
    unlink(prefs_path)
  }
}, add = TRUE)

check("delete confirm token is DELETE", {
  identical(ynow_auth_delete_confirm_token(), "DELETE")
})
check("confirm gate rejects empty / wrong case / partial", {
  !ynow_auth_delete_confirm_ok("") &&
    !ynow_auth_delete_confirm_ok(NULL) &&
    !ynow_auth_delete_confirm_ok("delete") &&
    !ynow_auth_delete_confirm_ok("Delete") &&
    !ynow_auth_delete_confirm_ok("DELET") &&
    !ynow_auth_delete_confirm_ok("DELETE!")
})
check("confirm gate accepts exact DELETE (trim)", {
  ynow_auth_delete_confirm_ok("DELETE") &&
    ynow_auth_delete_confirm_ok(" DELETE ")
})

check("purge rejects without DELETE", {
  res <- ynow_db_purge_user("mem:test-purge", email = "purge@example.com", confirm = "nope")
  isTRUE(!isTRUE(res$ok)) && identical(res$message_key, "auth_delete_msg_confirm")
})
check("purge rejects missing user id", {
  res <- ynow_db_purge_user("", email = "", confirm = "DELETE")
  isTRUE(!isTRUE(res$ok)) && identical(res$message_key, "auth_delete_msg_no_user")
})

uid <- "mem:delete-account-unit"
em <- "delete.unit@example.com"
ynow_prefs_reset_memory()
ynow_access_reset_memory()
.ynow_points_mem_reset()
ynow_pdf_reset_memory()

# Seed local stores
ynow_points_mem_register_profile(uid, role = "user")
ynow_points_credit(uid, 50L, reason = "unit", ref_id = "ref-delete-1", wallet = "fyn")
ynow_prefs_set_pinned_recent(uid, c("AAPL", "2330.TW"))
ynow_access_set(uid, "tab:price_alerts", locked = TRUE, effect = "hide")
.ynow_pdf_quota$used[[paste0(uid, "|2026-10-20")]] <- 2L
.ynow_pdf_quota$repeats[[uid]] <- c("fp1")

alert_df <- price_alert_empty()
alert_df[1, ] <- list(
  "al-del-1", em, "AAPL", "price_lte", 100, 0, "", TRUE, "armed",
  NA_real_, "", "", "", TRUE, FALSE, "manual", "", "", FALSE
)
alert_df[2, ] <- list(
  "al-keep-1", "other@example.com", "MSFT", "price_gte", 200, 0, "", TRUE, "armed",
  NA_real_, "", "", "", TRUE, FALSE, "manual", "", "", FALSE
)
price_alert_write(alert_df)
hist <- price_alert_history_empty()
hist[1, ] <- list("h1", "al-del-1", "AAPL", "price_lte", 100, 99, "app", "hit", "2026-10-20")
hist[2, ] <- list("h2", "al-keep-1", "MSFT", "price_gte", 200, 201, "app", "hit", "2026-10-20")
price_alert_history_write(hist)

check("seed: points / prefs / locks / alerts present", {
  bals <- ynow_points_balances(uid)
  pins <- ynow_prefs_get_pinned_recent(uid)
  locks <- ynow_access_locked_ids(uid)
  alerts <- price_alert_read()
  isTRUE(as.integer(bals$fyn)[1] >= 50L) &&
    identical(pins, c("AAPL", "2330.TW")) &&
    "tab:price_alerts" %in% locks &&
    any(alerts$email == em)
})

res <- ynow_db_purge_user(uid, email = em, confirm = "DELETE")
check("purge ok for memory user with DELETE", isTRUE(res$ok))
check("purge message key ok", identical(res$message_key, "auth_delete_msg_ok"))

check("points mem cleared for user", {
  led <- .ynow_points_mem_ledger_tail(uid, n = 100L)
  isTRUE(nrow(led) == 0L) && is.null(.ynow_points_mem$profiles[[uid]])
})
check("prefs cleared", identical(ynow_prefs_get_pinned_recent(uid), character(0)))
check("access locks cleared for user (not __all__)", {
  !"tab:price_alerts" %in% ynow_access_locked_ids(uid)
})
check("pdf mem cleared", {
  is.null(.ynow_pdf_quota$used[[paste0(uid, "|2026-10-20")]]) &&
    is.null(.ynow_pdf_quota$repeats[[uid]])
})
check("alerts for email removed; other mailbox kept", {
  alerts <- price_alert_read()
  hist2 <- price_alert_history_read()
  !any(alerts$email == em) &&
    any(alerts$email == "other@example.com") &&
    !any(hist2$alert_id == "al-del-1") &&
    any(hist2$alert_id == "al-keep-1")
})

# UI surface
Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
panel <- ynow_admin_panel_ui(
  list(email = em, logged_in_at = "2026-10-20", id = uid),
  locale = "en"
)
panel_html <- paste(as.character(panel), collapse = "\n")
check("Account overview has Delete account next to Sign out", {
  grepl("ynow_auth_delete_account", panel_html, fixed = TRUE) &&
    grepl("ynow-maint-actions-auth", panel_html, fixed = TRUE) &&
    grepl("ynow-maint-btn-danger", panel_html, fixed = TRUE) &&
    grepl("ynow_auth_sign_out_admin", panel_html, fixed = TRUE) &&
    grepl("Delete account", panel_html, fixed = TRUE)
})
check("danger button CSS present in auth chrome", {
  css_ui <- ynow_auth_css()
  css <- paste(as.character(css_ui), collapse = "\n")
  grepl("ynow-maint-btn-danger", css, fixed = TRUE) &&
    grepl("ynow-maint-actions-auth", css, fixed = TRUE) &&
    grepl("#c0392b", css, fixed = TRUE)
})

modal <- ynow_delete_account_modal("en")
modal_html <- paste(as.character(modal), collapse = "\n")
check("modal warns + requires DELETE token in JS gate", {
  grepl("ynow_auth_delete_confirm_text", modal_html, fixed = TRUE) &&
    grepl("ynow_auth_delete_confirm", modal_html, fixed = TRUE) &&
    grepl("disabled", modal_html, fixed = TRUE) &&
    grepl("DELETE", modal_html, fixed = TRUE) &&
    grepl("permanently", modal_html, ignore.case = TRUE)
})
modal_zh <- paste(as.character(ynow_delete_account_modal("zh-TW")), collapse = "\n")
check("zh-TW modal uses Taiwan wording", {
  grepl("永久刪除", modal_zh, fixed = TRUE) &&
    grepl("到值通知", modal_zh, fixed = TRUE) &&
    !grepl("数据", modal_zh, fixed = TRUE) &&
    !grepl("用户", modal_zh, fixed = TRUE)
})

del_keys <- c(
  "auth_delete_account", "auth_delete_modal_title", "auth_delete_modal_warn",
  "auth_delete_modal_type", "auth_delete_modal_confirm", "auth_delete_modal_cancel",
  "auth_delete_msg_confirm", "auth_delete_msg_ok", "auth_delete_msg_ok_auth_skipped",
  "auth_delete_msg_ok_auth_failed", "auth_delete_msg_fail", "auth_delete_msg_no_user"
)
check("locale keys en + zh-TW", {
  all(vapply(del_keys, function(k) {
    en <- tryCatch(ui_str(k, "en"), error = function(e) "")
    zh <- tryCatch(ui_str(k, "zh-TW"), error = function(e) "")
    nzchar(en) && nzchar(zh)
  }, logical(1)))
})

check("service-role delete skips when unset (needed=TRUE)", {
  old_url <- Sys.getenv("YNOW_SUPABASE_URL", unset = NA_character_)
  Sys.setenv(YNOW_SUPABASE_URL = "https://example.supabase.co")
  on.exit({
    if (is.na(old_url)) Sys.unsetenv("YNOW_SUPABASE_URL") else
      Sys.setenv(YNOW_SUPABASE_URL = old_url)
  }, add = TRUE)
  Sys.unsetenv("YNOW_SUPABASE_SERVICE_ROLE")
  res <- ynow_oauth_delete_supabase_user("00000000-0000-0000-0000-000000000001")
  isTRUE(isTRUE(res$skipped)) &&
    isTRUE(isTRUE(res$needed)) &&
    identical(res$reason, "no_service_role")
})

check("non-UUID provider id is not treated as Supabase Auth uid", {
  res <- ynow_oauth_delete_supabase_user("google-sub-12345")
  isTRUE(isTRUE(res$skipped)) &&
    isTRUE(!isTRUE(res$needed)) &&
    identical(res$reason, "not_supabase_uid")
})

check("message key: full ok when Auth not needed", {
  identical(
    ynow_auth_delete_message_key(list(ok = FALSE, skipped = TRUE, needed = FALSE)),
    "auth_delete_msg_ok"
  )
})
check("message key: skipped service role → auth_skipped", {
  identical(
    ynow_auth_delete_message_key(list(
      ok = FALSE, skipped = TRUE, needed = TRUE, reason = "no_service_role"
    )),
    "auth_delete_msg_ok_auth_skipped"
  )
})
check("message key: Auth HTTP failure → auth_failed", {
  identical(
    ynow_auth_delete_message_key(list(
      ok = FALSE, skipped = FALSE, needed = TRUE, status = 401L
    )),
    "auth_delete_msg_ok_auth_failed"
  )
})
check("message key: Auth deleted → ok", {
  identical(
    ynow_auth_delete_message_key(list(ok = TRUE, skipped = FALSE, needed = TRUE, status = 200L)),
    "auth_delete_msg_ok"
  )
})

# Mock Auth Admin delete success path (no real network).
.orig_oauth_del <- ynow_oauth_delete_supabase_user
assign(
  "ynow_oauth_delete_supabase_user",
  function(provider_uid) {
    list(
      ok = TRUE,
      skipped = FALSE,
      needed = TRUE,
      status = 200L,
      reason = "deleted",
      mocked = TRUE,
      provider_uid = as.character(provider_uid %||% "")[1]
    )
  },
  envir = globalenv()
)
on.exit({
  assign("ynow_oauth_delete_supabase_user", .orig_oauth_del, envir = globalenv())
}, add = TRUE)

uid2 <- "mem:delete-account-auth-mock"
em2 <- "delete.auth.mock@example.com"
ynow_prefs_reset_memory()
ynow_access_reset_memory()
.ynow_points_mem_reset()
ynow_pdf_reset_memory()
ynow_points_mem_register_profile(uid2, role = "user")
res_auth <- ynow_db_purge_user(
  uid2,
  email = em2,
  confirm = "DELETE",
  provider_uid = "11111111-2222-3333-4444-555555555555"
)
check("purge with mocked Auth delete reports full ok", {
  isTRUE(res_auth$ok) &&
    identical(res_auth$message_key, "auth_delete_msg_ok") &&
    isTRUE(res_auth$auth_idp$ok) &&
    isTRUE(res_auth$auth_idp$mocked)
})

# Mock skipped service-role path through purge + session provider_uid.
assign(
  "ynow_oauth_delete_supabase_user",
  function(provider_uid) {
    list(
      ok = FALSE,
      skipped = TRUE,
      needed = TRUE,
      reason = "no_service_role",
      mocked = TRUE
    )
  },
  envir = globalenv()
)
uid3 <- "mem:delete-account-auth-skip"
em3 <- "delete.auth.skip@example.com"
ynow_points_mem_register_profile(uid3, role = "user")
res_skip <- ynow_db_purge_user(
  uid3,
  email = em3,
  confirm = "DELETE",
  provider_uid = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
)
check("purge still ok when Auth delete skipped; message reports Auth pending", {
  isTRUE(res_skip$ok) &&
    identical(res_skip$message_key, "auth_delete_msg_ok_auth_skipped") &&
    isTRUE(res_skip$auth_idp$skipped)
})

# Confirm DELETE gate still blocks before any Auth call.
blocked <- ynow_db_purge_user(
  uid3,
  email = em3,
  confirm = "delete",
  provider_uid = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
)
check("DELETE confirm still required before Auth path", {
  isTRUE(!isTRUE(blocked$ok)) &&
    identical(blocked$message_key, "auth_delete_msg_confirm")
})

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1L)
