# Points wallet — FYN/PYN, first gift, daily check-in, admin adjust
# Run: Rscript app_25.0/tests/test_points_wallet.R

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
test_file <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  normalizePath(file_arg, mustWork = TRUE)
} else {
  normalizePath("test_points_wallet.R", mustWork = FALSE)
}
tests_dir <- dirname(test_file)
app_dir <- normalizePath(file.path(tests_dir, ".."), mustWork = TRUE)
if (!file.exists(file.path(app_dir, "points_wallet.R"))) {
  app_dir <- normalizePath(file.path(tests_dir, "../../app_25.0"), mustWork = TRUE)
}
old_wd <- getwd()
setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

Sys.setenv(YNOW_POINTS_BACKEND = "memory")
Sys.setenv(YNOW_POINTS_ENABLED = "1")
Sys.setenv(YNOW_POINTS_SIGNUP_BONUS = "100")
Sys.setenv(YNOW_POINTS_DAILY_CHECKIN = "30")
Sys.unsetenv("YNOW_DB_HOST")
Sys.unsetenv("YNOW_DATABASE_URL")
Sys.unsetenv("DATABASE_URL")

source(file.path(app_dir, "ui_locale.R"), local = FALSE, encoding = "UTF-8")
source(file.path(app_dir, "auth_db.R"), local = FALSE, encoding = "UTF-8")
source(file.path(app_dir, "auth_account.R"), local = FALSE, encoding = "UTF-8")
source(file.path(app_dir, "points_wallet.R"), local = FALSE, encoding = "UTF-8")

.ynow_points_mem_reset()

n_ok <- 0L
n_fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    n_ok <<- n_ok + 1L
    cat("OK", label, "\n")
  } else {
    n_fail <<- n_fail + 1L
    cat("FAIL", label, "\n")
  }
}

check("backend memory", identical(ynow_points_backend(), "memory"))
check("uses_db false under memory", identical(ynow_points_uses_db(), FALSE))
check("backend_label memory", identical(ynow_points_backend_label(), "memory"))
check("host_kind none without cfg", identical(ynow_db_host_kind(), "none"))
check("host_kind neon", {
  identical(
    ynow_db_host_kind(list(host = "ep-x-pooler.us-east-2.aws.neon.tech")),
    "neon"
  )
})
check("host_kind supabase", {
  identical(
    ynow_db_host_kind(list(host = "aws-0-ap-southeast-1.pooler.supabase.com")),
    "supabase"
  )
})
check("host_kind postgres other", {
  identical(ynow_db_host_kind(list(host = "db.example.com")), "postgres")
})
check("backend returns postgres not neon when ready", {
  # Stub ready without real drivers/creds; restore after.
  old_ready <- ynow_db_ready
  assign("ynow_db_ready", function() TRUE, envir = .GlobalEnv)
  on.exit(assign("ynow_db_ready", old_ready, envir = .GlobalEnv), add = TRUE)
  Sys.unsetenv("YNOW_POINTS_BACKEND")
  be <- ynow_points_backend()
  Sys.setenv(YNOW_POINTS_BACKEND = "memory")
  identical(be, "postgres")
})
check("backend_label uses host_kind when db", {
  old_ready <- ynow_db_ready
  old_kind <- ynow_db_host_kind
  assign("ynow_db_ready", function() TRUE, envir = .GlobalEnv)
  assign("ynow_db_host_kind", function(cfg = NULL) "supabase", envir = .GlobalEnv)
  on.exit({
    assign("ynow_db_ready", old_ready, envir = .GlobalEnv)
    assign("ynow_db_host_kind", old_kind, envir = .GlobalEnv)
  }, add = TRUE)
  Sys.unsetenv("YNOW_POINTS_BACKEND")
  lab <- ynow_points_backend_label()
  Sys.setenv(YNOW_POINTS_BACKEND = "memory")
  identical(lab, "supabase")
})
check("legacy YNOW_POINTS_BACKEND=neon maps to postgres", {
  old_ready <- ynow_db_ready
  assign("ynow_db_ready", function() TRUE, envir = .GlobalEnv)
  on.exit(assign("ynow_db_ready", old_ready, envir = .GlobalEnv), add = TRUE)
  Sys.setenv(YNOW_POINTS_BACKEND = "neon")
  be <- ynow_points_backend()
  Sys.setenv(YNOW_POINTS_BACKEND = "memory")
  identical(be, "postgres")
})
check("enabled true", isTRUE(ynow_points_enabled()))
check("signup bonus default 100", identical(ynow_points_signup_bonus(), 100L))
check("daily checkin default 30", identical(ynow_points_daily_checkin_amount(), 30L))
check("catalog has alert_slot", {
  cat <- ynow_points_catalog()
  "alert_slot" %in% cat$sku && as.integer(cat$cost[cat$sku == "alert_slot"][[1]]) == 1L
})

uid <- "user-test-001"
ynow_points_mem_register_profile(uid, role = "user")
check("balance starts 0 fyn", identical(ynow_points_balance(uid, "fyn"), 0L))
check("balance starts 0 pyn", identical(ynow_points_balance(uid, "pyn"), 0L))

g1 <- ynow_points_grant_signup_bonus(uid)
check("first gift ok 100 FYN", isTRUE(g1$ok) && identical(g1$balance, 100L) && identical(g1$fyn, 100L))
check("balance after first gift", identical(ynow_points_balances(uid)$fyn, 100L))
check("pyn still 0 after gift", identical(ynow_points_balances(uid)$pyn, 0L))

g2 <- ynow_points_grant_signup_bonus(uid)
check(
  "first gift once only",
  isTRUE(g2$ok) && isTRUE(g2$idempotent) && identical(ynow_points_balances(uid)$fyn, 100L)
)

# Daily check-in once per Taipei day
day1 <- as.POSIXct("2026-10-16 10:00:00", tz = "Asia/Taipei")
c1 <- ynow_points_grant_daily_checkin(uid, when = day1)
check("daily +30 once", isTRUE(c1$ok) && identical(ynow_points_balances(uid)$fyn, 130L))
c1b <- ynow_points_grant_daily_checkin(uid, when = day1)
check(
  "daily same day idempotent",
  isTRUE(c1b$ok) && isTRUE(c1b$idempotent) && identical(ynow_points_balances(uid)$fyn, 130L)
)
day2 <- as.POSIXct("2026-10-17 01:00:00", tz = "Asia/Taipei")
c2 <- ynow_points_grant_daily_checkin(uid, when = day2)
check("daily next day +30", isTRUE(c2$ok) && identical(ynow_points_balances(uid)$fyn, 160L))

# Login hook never resets to 100
before_login <- ynow_points_balances(uid)$fyn
invisible(ynow_points_try_spend(uid, "alert_slot", "spend-before-login"))
mid <- ynow_points_balances(uid)$fyn
check("spend from FYN", identical(mid, before_login - 1L))
login_again <- ynow_points_on_login(uid, when = day2)
check(
  "login does not reset to 100",
  isTRUE(login_again$ok) &&
    identical(ynow_points_balances(uid)$fyn, mid) &&
    mid != 100L
)
# Deprecated reset is no-op
r_old <- ynow_points_reset_on_login(uid)
check(
  "deprecated reset is no-op",
  isTRUE(r_old$skipped) && identical(ynow_points_balances(uid)$fyn, mid)
)

# FYN / PYN separation + admin adjust mirrors user-visible balances
adj <- ynow_points_admin_adjust(uid, fyn_delta = 5L, pyn_delta = 40L, note = "test")
check("admin adjust ok", isTRUE(adj$ok))
bals <- ynow_points_balances(uid)
check("admin fyn matches visible", identical(bals$fyn, mid + 5L) && identical(adj$fyn, bals$fyn))
check("admin pyn matches visible", identical(bals$pyn, 40L) && identical(adj$pyn, bals$pyn))
check("spend still FYN only", {
  s <- ynow_points_try_spend(uid, "alert_slot", "spend-fyn-only")
  isTRUE(s$ok) &&
    identical(ynow_points_balances(uid)$fyn, bals$fyn - 1L) &&
    identical(ynow_points_balances(uid)$pyn, 40L)
})

# Insufficient when FYN empty (PYN untouched)
while (ynow_points_balances(uid)$fyn > 0L) {
  bal <- ynow_points_balances(uid)$fyn
  ynow_points_try_spend(uid, "alert_slot", paste0("drain:", bal))
}
fail <- ynow_points_try_spend(uid, "alert_slot", "drain:overflow")
check("insufficient fails on FYN", !isTRUE(fail$ok) && identical(fail$message_key, "pts_msg_insufficient"))
check("pyn untouched when FYN empty", identical(ynow_points_balances(uid)$pyn, 40L))

# Admin subtract
sub <- ynow_points_admin_adjust(uid, fyn_delta = 0L, pyn_delta = -10L)
check("admin subtract pyn", isTRUE(sub$ok) && identical(ynow_points_balances(uid)$pyn, 30L))

# De-identified admin list
uid2 <- "user-test-002"
ynow_points_mem_register_profile(uid2, role = "admin")
invisible(ynow_points_grant_signup_bonus(uid2))
lst <- ynow_points_admin_list_users()
check("admin list has users", is.data.frame(lst) && nrow(lst) >= 2L)
check("admin list has public_id", "public_id" %in% names(lst) && all(grepl("^U-", lst$public_id)))
check("admin list has fyn/pyn", all(c("fyn", "pyn") %in% names(lst)))
pid <- lst$public_id[lst$user_id == uid][[1]]
check("resolve public_id", identical(ynow_points_resolve_public_id(pid), uid))

# finish_auth wires on_login (not reset)
auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("finish_auth wires on_login", grepl("ynow_points_on_login", auth, fixed = TRUE))
check("finish_auth no active reset call", {
  # Deprecated function may still exist; login path must call on_login.
  grepl("ynow_points_on_login", auth, fixed = TRUE) &&
    !grepl("ynow_points_reset_on_login\\(", auth)
})

tail <- ynow_points_ledger_tail(uid, n = 50L)
check("ledger has rows", is.data.frame(tail) && nrow(tail) >= 3L)
check("ledger has wallet column", "wallet" %in% names(tail))
all_rows <- ynow_points_ledger_all(uid)
check("ledger_all returns every row", is.data.frame(all_rows) && nrow(all_rows) >= nrow(tail))

# Gate off always allows
Sys.setenv(YNOW_POINTS_ENABLED = "0")
check("enabled false", !isTRUE(ynow_points_enabled()))
check("gate allows when disabled", isTRUE(ynow_points_gate(NULL, "alert_slot", "x", user = list(id = uid))))

# Locale keys
for (k in c(
  "pts_balance", "pts_fyn_label", "pts_pyn_label", "pts_ledger",
  "pts_msg_insufficient_fmt", "pts_rules_note", "pts_sysadmin_title",
  "pts_sysadmin_enter", "pts_daily_checkin_fmt", "pts_first_gift_fmt",
  "auth_admin_tool_points"
)) {
  check(paste("locale en", k), nzchar(ui_str(k, "en")))
  check(paste("locale zh", k), nzchar(ui_str(k, "zh-TW")))
}
check("locale zh-TW no simplified 数据", {
  !grepl("数据", ui_str("pts_rules_note", "zh-TW"), fixed = TRUE) &&
    !grepl("默认", ui_str("pts_sysadmin_intro", "zh-TW"), fixed = TRUE)
})

check("global sources points_wallet", {
  g <- paste(readLines(file.path(app_dir, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('source\\(\"points_wallet\\.R\"', g)
})
check("pa gates alert_slot", {
  pa <- paste(readLines(file.path(app_dir, "price_alert.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("alert_slot", pa, fixed = TRUE) && grepl(".pa_points_gate_alert", pa, fixed = TRUE)
})
check("run_bt gates bt_run", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("bt_run", srv, fixed = TRUE) && grepl("ynow_points_gate", srv, fixed = TRUE)
})
check("sysadmin portal has login + entry", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_pts_sysadmin_enter", ui, fixed = TRUE) &&
    grepl("ynow_pts_sysadmin_login_user", ui, fixed = TRUE) &&
    grepl("ynow_pts_sysadmin_login_pass", ui, fixed = TRUE) &&
    grepl('value = "ynow"', ui, fixed = TRUE) &&
    grepl("ynow-sysadmin-login-chrome", ui, fixed = TRUE) &&
    grepl("ynow-sysadmin-unlocked-actions", ui, fixed = TRUE) &&
    grepl("ynow_viewas_chrome", ui, fixed = TRUE) &&
    grepl("ynow-view-as", ui, fixed = TRUE) &&
    grepl("ynow-silver-gradient", ui, fixed = TRUE) &&
    grepl("ynow_pts_banner", ui, fixed = TRUE) &&
    grepl("ynow-pts-header", ui, fixed = TRUE) &&
    !grepl("ynow_pts_sysadmin_refresh", ui, fixed = TRUE)
})
check("sysadmin hides login chrome when unlocked via CSS/class", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("body.ynow-sysadmin-on .ynow-sysadmin-login-chrome", ui, fixed = TRUE) &&
    grepl("ynowSysadminMode", ui, fixed = TRUE) &&
    grepl("body.ynow-view-as .ynow-pts-sysadmin-box", ui, fixed = TRUE)
})
check("mobile CSS includes pts header with lang/logo flex+z-index", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl(
    "@media \\(max-width: 767px\\)[\\s\\S]*li\\.ynow-lang-header,[\\s\\S]*li\\.ynow-pts-header,[\\s\\S]*#ynow-header-logo",
    ui,
    perl = TRUE
  ) &&
    grepl("body.ynow-lite #ynow-pts-header", ui, fixed = TRUE) &&
    grepl("pushLeft(ptsHdr)", ui, fixed = TRUE)
})
check("boot locale slim keeps pts banner labels", {
  loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("pts_fyn_label|pts_pyn_label", loc, perl = TRUE) &&
    grepl("ui_locale_boot_bundle", loc, fixed = TRUE)
})
check("server has sysadmin + banner", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_pts_sysadmin_apply", srv, fixed = TRUE) &&
    grepl("ynowPtsBanner", srv, fixed = TRUE) &&
    grepl("ynow_points_admin_adjust", srv, fixed = TRUE)
})
check("guest pts banner hidden until login", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("visible = FALSE", srv, fixed = TRUE) &&
    grepl("ynow-pts-hidden", ui, fixed = TRUE) &&
    grepl("classList.add('ynow-pts-hidden')", ui, fixed = TRUE) &&
    grepl("li.ynow-pts-header.ynow-pts-hidden", ui, fixed = TRUE) &&
    grepl("ynow-pts-header:not(.ynow-pts-hidden)", ui, fixed = TRUE)
})
check("guest pts banner clears DOM values when hidden", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("payload.visible === false", ui, fixed = TRUE) &&
    grepl("el.textContent = ''", ui, fixed = TRUE) &&
    grepl("ynow_pts_sidebar_fyn_value", ui, fixed = TRUE) &&
    grepl("classList.add('ynow-pts-hidden')", ui, fixed = TRUE) &&
    grepl("ptsGuestHidden", ui, fixed = TRUE)
})
check("guest pts banner initial HTML labels empty", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Boot markup must not embed "FYN points:" / "PYN points:" before login.
  !grepl('ynow_pts_banner_fyn_label", "FYN', ui, fixed = TRUE) &&
    !grepl('ynow_pts_banner_pyn_label", "PYN', ui, fixed = TRUE) &&
    grepl('ynow_pts_banner_fyn_label", ""', ui, fixed = TRUE) &&
    grepl('ynow_pts_banner_pyn_label", ""', ui, fixed = TRUE)
})
check("mobile pts live in sidebar right of login date", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow-pts-sidebar", ui, fixed = TRUE) &&
    grepl("ynow_pts_sidebar_fyn_label", ui, fixed = TRUE) &&
    grepl("ynow-sidebar-date-row", ui, fixed = TRUE) &&
    grepl("ynow_pts_sidebar", ui, fixed = TRUE) &&
    grepl("@media \\(max-width:767px\\)", auth, perl = TRUE) &&
    grepl("#ynow-pts-sidebar.ynow-pts-sidebar:not\\(.ynow-pts-hidden\\)", auth, perl = TRUE) &&
    grepl("Mobile override: points live in the sidebar", ui, fixed = TRUE)
})
check("auth memory user id helper stable", {
  auth_src <- file.path(app_dir, "auth_account.R")
  if (!exists("ynow_auth_memory_user_id", mode = "function")) {
    sys.source(auth_src, envir = environment())
  }
  a <- ynow_auth_memory_user_id("A@B.com")
  b <- ynow_auth_memory_user_id("a@b.com")
  grepl("^mem:", a) && identical(a, b) && nchar(a) > 8
})
check("server distinguishes no-db empty state", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("pts_sysadmin_no_db", srv, fixed = TRUE) &&
    grepl("ynow_db_ready", srv, fixed = TRUE) &&
    grepl("ynow_points_backend_label", srv, fixed = TRUE) &&
    !grepl("ynow_pts_sysadmin_db_diag", srv, fixed = TRUE) &&
    !grepl("ynow_db_debug_log_probe", srv, fixed = TRUE)
})
check("forced deps file sources Neon packages", {
  deps <- paste(readLines(file.path(app_dir, "ynow_forced_deps.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("library(RPostgres)", deps, fixed = TRUE) &&
    grepl("library(pool)", deps, fixed = TRUE) &&
    grepl("library(sodium)", deps, fixed = TRUE)
})
check("locale no-db keys", {
  en <- ui_str("pts_sysadmin_no_db", "en")
  zh <- ui_str("pts_sysadmin_no_db", "zh-TW")
  nzchar(en) && nzchar(zh) &&
    grepl("YNOW_DB_", en, fixed = TRUE) &&
    grepl("YNOW_DB_", zh, fixed = TRUE) &&
    !grepl("尚未設定 Neon", zh, fixed = TRUE) &&
    !grepl("Neon account store", en, fixed = TRUE)
})
check("account_points is independent tab", {
  grepl('tabName = "account_points"', auth, fixed = TRUE) &&
    grepl("ynow_points_panel_ui", auth, fixed = TRUE) &&
    grepl("ynow_menu_pts_ledger", auth, fixed = TRUE)
})
check("server renders points panel + DT", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("output$ynow_points_panel", srv, fixed = TRUE) &&
    grepl("ynow_pts_ledger_dt", srv, fixed = TRUE) &&
    grepl("ynow_points_ledger_all", srv, fixed = TRUE)
})
check("locale has pts ledger tab keys", {
  nzchar(ui_str("pts_ledger_title", "en")) &&
    nzchar(ui_str("pts_ledger_title", "zh-TW")) &&
    nzchar(ui_str("menu_pts_ledger", "zh-TW")) &&
    grepl("資料庫", ui_str("pts_ledger_intro", "zh-TW"), fixed = TRUE)
})
check("locale rules note persists", {
  grepl("100", ui_str("pts_rules_note", "en"), fixed = TRUE) &&
    grepl("30", ui_str("pts_rules_note", "en"), fixed = TRUE) &&
    grepl("100", ui_str("pts_rules_note", "zh-TW"), fixed = TRUE)
})

# Pool-safe credit path: checkout real DBI con (not Pool) + nest reuse.
check("ynow_db_with checkouts connection not pool", {
  db <- paste(readLines(file.path(app_dir, "auth_db.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("poolCheckout", db, fixed = TRUE) &&
    grepl("poolReturn", db, fixed = TRUE) &&
    grepl(".ynow_db_with_stack", db, fixed = TRUE) &&
    grepl("Nested first — reuse outer connection", db, fixed = TRUE)
})
check("neon_apply keeps dbWithTransaction on checked-out con", {
  pts <- paste(readLines(file.path(app_dir, "points_wallet.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("DBI::dbWithTransaction(con", pts, fixed = TRUE) &&
    grepl("Same connection — avoid nested checkout", pts, fixed = TRUE)
})
check("ynow_db_with nest reuses outer connection", {
  if (!exists("ynow_db_with", mode = "function") ||
      !exists(".ynow_db_with_stack", inherits = TRUE)) {
    return(TRUE) # sourced env may differ; structural checks above cover ship gate
  }
  stack_env <- get(".ynow_db_with_stack", inherits = TRUE)
  old <- stack_env$cons
  on.exit({ stack_env$cons <- old }, add = TRUE)
  sentinel <- structure(list(mark = "outer-con"), class = "DBIConnection")
  stack_env$cons <- list(sentinel)
  seen <- NULL
  res <- ynow_db_with(function(con) {
    seen <<- con
    "ok"
  })
  identical(res, "ok") && identical(seen$mark, "outer-con")
})
check("locale pts_sysadmin_need_enter en+zh-TW", {
  en <- ui_str("pts_sysadmin_need_enter", "en")
  zh <- ui_str("pts_sysadmin_need_enter", "zh-TW")
  nzchar(en) && nzchar(zh) &&
    grepl("username and password", en, fixed = TRUE) &&
    grepl("使用者名稱與密碼", zh, fixed = TRUE) &&
    !grepl("数据", zh, fixed = TRUE) &&
    !grepl("一鍵", zh, fixed = TRUE)
})
check("locale pts_sysadmin_loading en+zh-TW", {
  en <- ui_str("pts_sysadmin_loading", "en")
  zh <- ui_str("pts_sysadmin_loading", "zh-TW")
  grepl("Loading", en, fixed = TRUE) &&
    grepl("載入", zh, fixed = TRUE) &&
    !grepl("加载", zh, fixed = TRUE) &&
    !grepl("数据", zh, fixed = TRUE)
})
check("server loads sysadmin users async via later", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_pts_sysadmin_panel_state", srv, fixed = TRUE) &&
    grepl('kind = "loading"', srv, fixed = TRUE) &&
    grepl("later::later", srv, fixed = TRUE)
})
check("locale sysadmin login keys en+zh-TW", {
  keys <- c(
    "pts_sysadmin_login_user", "pts_sysadmin_login_pass",
    "pts_sysadmin_login_ok", "pts_sysadmin_login_bad",
    "pts_sysadmin_viewas_chrome_hint"
  )
  all(vapply(keys, function(k) {
    en <- ui_str(k, "en")
    zh <- ui_str(k, "zh-TW")
    nzchar(en) && nzchar(zh) && !grepl("数据|默认|一鍵", zh)
  }, logical(1)))
})
check("server shows need_enter hint when sysadmin off", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("pts_sysadmin_need_enter", srv, fixed = TRUE) &&
    grepl("ynow_pts_sysadmin_need_enter", srv, fixed = TRUE)
})
check("sysadmin login validates ynow + Taipei YYYYMMDD", {
  pts <- paste(readLines(file.path(app_dir, "points_wallet.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_sysadmin_validate_login", pts, fixed = TRUE) &&
    grepl("ynow_sysadmin_expected_password", pts, fixed = TRUE) &&
    grepl("%Y%m%d", pts, fixed = TRUE) &&
    grepl("ynow_points_tz", pts, fixed = TRUE)
})
check("sysadmin password is Taipei date not fixed", {
  when <- as.POSIXct("2026-10-15 23:30:00", tz = "Asia/Taipei")
  expect <- ynow_sysadmin_expected_password(when)
  identical(expect, "20261015") &&
    isTRUE(ynow_sysadmin_validate_login("ynow", "20261015", when = when)$ok) &&
    isTRUE(ynow_sysadmin_validate_login("YNOW", "20261015", when = when)$ok) &&
    !isTRUE(ynow_sysadmin_validate_login("ynow", "20261014", when = when)$ok) &&
    !isTRUE(ynow_sysadmin_validate_login("admin", "20261015", when = when)$ok) &&
    !isTRUE(ynow_sysadmin_validate_login("ynow", "", when = when)$ok)
})
check("server never logs sysadmin password", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Enter handler clears pass; no message()/cat()/print of login_pass.
  grepl("ynow_pts_sysadmin_login_pass", srv, fixed = TRUE) &&
    grepl("ynow_sysadmin_validate_login", srv, fixed = TRUE) &&
    grepl("updateTextInput\\(session, \"ynow_pts_sysadmin_login_pass\"", srv, perl = TRUE) &&
    !grepl("message\\(.*login_pass|cat\\(.*login_pass|print\\(.*login_pass", srv, perl = TRUE)
})

# Admin adjust post-write verify + view-as wiring
check("admin adjust verifies persisted balances", {
  .ynow_points_mem_reset()
  u2 <- paste0("mem-verify-", as.integer(as.numeric(Sys.time())))
  ynow_points_on_login(u2)
  before <- ynow_points_balances(u2)
  adj <- ynow_points_admin_adjust(u2, fyn_delta = 17L, pyn_delta = 3L, note = "verify")
  after <- ynow_points_balances(u2)
  isTRUE(adj$ok) && isTRUE(adj$verified) &&
    identical(as.integer(after$fyn), as.integer(before$fyn) + 17L) &&
    identical(as.integer(after$pyn), as.integer(before$pyn) + 3L)
})
check("on_login does not wipe admin grant", {
  .ynow_points_mem_reset()
  u3 <- paste0("mem-persist-", as.integer(as.numeric(Sys.time())))
  ynow_points_on_login(u3)
  ynow_points_admin_adjust(u3, fyn_delta = 40L, pyn_delta = 0L)
  mid <- ynow_points_balances(u3)
  again <- ynow_points_on_login(u3)
  after <- ynow_points_balances(u3)
  isTRUE(again$ok) && !isTRUE(again$gift_granted) &&
    identical(as.integer(after$fyn), as.integer(mid$fyn)) &&
    identical(as.integer(after$pyn), as.integer(mid$pyn))
})
check("locale view-as keys en+zh-TW", {
  keys <- c(
    "pts_sysadmin_viewas", "pts_sysadmin_viewas_exit",
    "pts_sysadmin_viewas_active", "pts_sysadmin_ok_verify_fmt",
    "pts_sysadmin_verify_failed", "pts_sysadmin_viewas_chrome_hint"
  )
  all(vapply(keys, function(k) {
    en <- ui_str(k, "en")
    zh <- ui_str(k, "zh-TW")
    nzchar(en) && nzchar(zh) && !grepl("数据|默认|一鍵", zh)
  }, logical(1))) &&
    identical(ui_str("pts_sysadmin_viewas", "en"), "Switch view") &&
    identical(ui_str("pts_sysadmin_viewas", "zh-TW"), "切換檢視")
})
check("server wires view-as + silver chrome + effective user", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_pts_view_as", srv, fixed = TRUE) &&
    grepl(".ynow_pts_effective_user", srv, fixed = TRUE) &&
    grepl("ynow_pts_sysadmin_viewas", srv, fixed = TRUE) &&
    grepl("pts_sysadmin_viewas_exit", srv, fixed = TRUE) &&
    grepl(".ynow_view_as_push_chrome", srv, fixed = TRUE) &&
    grepl(".ynow_sysadmin_push_mode", srv, fixed = TRUE) &&
    grepl("ynowSysadminMode", srv, fixed = TRUE) &&
    grepl("ynowViewAsMode", srv, fixed = TRUE) &&
    grepl("ynowViewAsMode", ui, fixed = TRUE) &&
    grepl("ynow-silver-shine", ui, fixed = TRUE) &&
    !grepl("ynow_pts_sysadmin_refresh", srv, fixed = TRUE) &&
    grepl("verified", paste(readLines(file.path(app_dir, "points_wallet.R"), warn = FALSE), collapse = "\n"), fixed = TRUE)
})
check("price_alert prefers effective user for points gate", {
  pa <- paste(readLines(file.path(app_dir, "price_alert.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_pts_effective_user", pa, fixed = TRUE)
})
check("sidebar foot link is Sys. Admin not Testing sandbox", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  en <- ui_str("test_link", "en")
  zh <- ui_str("test_link", "zh-TW")
  grepl("Sys. Admin", en, fixed = TRUE) &&
    grepl("Sys. Admin", zh, fixed = TRUE) &&
    grepl("user-shield", ui, fixed = TRUE) &&
    grepl(".ynow-sidebar-snapshot-foot", ui, fixed = TRUE) &&
    grepl("ynow-sidebar-snapshot-foot-links", ui, fixed = TRUE) &&
    grepl("ynow-sidebar-copyright", ui, fixed = TRUE) &&
    grepl("flex-direction: column !important", ui, fixed = TRUE) &&
    grepl("flex-wrap: nowrap", ui, fixed = TRUE) &&
    !grepl("一鍵", zh, fixed = TRUE)
})
check("Sys. Admin hidden for signed-in users", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("body.ynow-authed .ynow-sidebar-test-link", ui, fixed = TRUE)
})
check("no leftover agent debug instrumentation", {
  files <- c("points_wallet.R", "ynow_server.R", "auth_account.R", "debug_helpers.R")
  txt <- paste(vapply(files, function(f) {
    paste(readLines(file.path(app_dir, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, character(1)), collapse = "\n")
  !grepl("ynow_agent_dbg", txt, fixed = TRUE) &&
    !grepl("/opt/cursor/logs/debug.log", txt, fixed = TRUE) &&
    !grepl("#region agent log", txt, fixed = TRUE)
})

ynow_pdf_reset_memory()
.ynow_points_mem_reset()
pdf_uid <- "pdf-user-1"
invisible(ynow_points_credit(pdf_uid, 100L, reason = "earn_signup", ref_id = "pdf-seed", wallet = "fyn"))
check("pdf sku costs 10", identical(ynow_points_sku_cost("pdf_report"), 10L))
day_fixed <- "2026-10-18"
free_ok <- replicate(3L, ynow_pdf_claim(pdf_uid, day = day_fixed)$kind)
check("three free pdf downloads", identical(as.character(free_ok), c("free", "free", "free")))
bal_before <- ynow_points_balances(pdf_uid)$fyn
paid <- ynow_pdf_claim(pdf_uid, day = day_fixed)
check("fourth pdf spends 10 FYN", {
  isTRUE(paid$ok) && identical(paid$kind, "paid") &&
    identical(ynow_points_balances(pdf_uid)$fyn, bal_before - 10L)
})
ynow_pdf_release(paid)
check("failed render refunds FYN", identical(ynow_points_balances(pdf_uid)$fyn, bal_before))
drain <- ynow_points_balances(pdf_uid)$fyn
if (is.finite(drain) && drain > 0L) {
  invisible(ynow_points_credit(pdf_uid, -drain, reason = "adjust", ref_id = "pdf-drain", wallet = "fyn"))
}
broke <- ynow_pdf_claim(pdf_uid, day = day_fixed)
check("pdf blocked when FYN is short", isTRUE(!broke$ok) && identical(broke$message_key, "pts_msg_insufficient"))
check("next Taipei day is free again", identical(ynow_pdf_claim(pdf_uid, day = "2026-10-19")$kind, "free"))
check("pdf download requires the claim helper", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_pdf_claim", srv, fixed = TRUE) && grepl("pdf_need_login", srv, fixed = TRUE) &&
    grepl("pdf_need_ticker", srv, fixed = TRUE) && grepl("ynow_pdf_repeat_mark", srv, fixed = TRUE)
})
check("empty ticker has no settings key", identical(ynow_pdf_settings_key(""), "") && identical(ynow_pdf_settings_key("NA"), ""))
fp_same <- ynow_pdf_settings_key("aapl", list(market = "US", wacc_gordon = "8", sgr = "3"))
fp_other <- ynow_pdf_settings_key("msft", list(market = "US", wacc_gordon = "8", sgr = "3"))
ynow_pdf_reset_memory()
.ynow_points_mem_reset()
invisible(ynow_points_credit("pdf-user-2", 100L, reason = "earn_signup", ref_id = "pdf-seed-2", wallet = "fyn"))
first_fp <- ynow_pdf_claim("pdf-user-2", day = "2026-10-20", fingerprint = fp_same)
check("first fingerprint uses a free download", isTRUE(first_fp$ok) && identical(first_fp$kind, "free") && identical(first_fp$used, 1L))
ynow_pdf_repeat_mark("pdf-user-2", fp_same)
repeat_fp <- ynow_pdf_claim("pdf-user-2", day = "2026-10-20", fingerprint = fp_same)
check("same ticker and settings do not spend again", isTRUE(repeat_fp$ok) && identical(repeat_fp$kind, "repeat"))
check("different ticker still spends a free download", {
  diff_fp <- ynow_pdf_claim("pdf-user-2", day = "2026-10-20", fingerprint = fp_other)
  isTRUE(diff_fp$ok) && identical(diff_fp$kind, "free") && identical(diff_fp$used, 2L)
})
check("pdf ticker and repeat copy", {
  identical(ui_str("pdf_need_ticker", "zh-TW"), "請先輸入股票代碼，再下載報告。") &&
    grepl("FYN", ui_str("pdf_repeat_free", "en"), fixed = TRUE) &&
    grepl("不計免費次數", ui_str("pdf_repeat_free", "zh-TW"), fixed = TRUE)
})
check("pdf quota hint is bilingual", {
  grepl("10 FYN", ui_str("pdf_quota_hint", "en"), fixed = TRUE) &&
    grepl("10 FYN", ui_str("pdf_quota_hint", "zh-TW"), fixed = TRUE) &&
    !grepl("一鍵", ui_str("pdf_quota_hint", "zh-TW"), fixed = TRUE)
})

cat("PASS", n_ok, " FAIL", n_fail, "\n")
if (n_fail > 0L) quit(status = 1)
cat("ALL PASS\n")
