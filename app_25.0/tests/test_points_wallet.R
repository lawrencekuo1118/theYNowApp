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
check("testing tab has sysadmin entry", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_pts_sysadmin_enter", ui, fixed = TRUE) &&
    grepl("ynow_pts_banner", ui, fixed = TRUE) &&
    grepl("ynow-pts-header", ui, fixed = TRUE)
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
    grepl("fynVal.textContent = ''", ui, fixed = TRUE) &&
    grepl("pynVal.textContent = ''", ui, fixed = TRUE) &&
    grepl("ptsGuestHidden", ui, fixed = TRUE)
})
check("server distinguishes no-db empty state", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("pts_sysadmin_no_db", srv, fixed = TRUE) &&
    grepl("ynow_db_ready", srv, fixed = TRUE)
})
check("forced deps file sources Neon packages", {
  deps <- paste(readLines(file.path(app_dir, "ynow_forced_deps.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("library(RPostgres)", deps, fixed = TRUE) &&
    grepl("library(pool)", deps, fixed = TRUE) &&
    grepl("library(sodium)", deps, fixed = TRUE)
})
check("locale no-db keys", {
  nzchar(ui_str("pts_sysadmin_no_db", "en")) &&
    nzchar(ui_str("pts_sysadmin_no_db", "zh-TW")) &&
    grepl("Neon", ui_str("pts_sysadmin_no_db", "en"), fixed = TRUE) &&
    grepl("Neon", ui_str("pts_sysadmin_no_db", "zh-TW"), fixed = TRUE)
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

cat("PASS", n_ok, " FAIL", n_fail, "\n")
if (n_fail > 0L) quit(status = 1)
cat("ALL PASS\n")
