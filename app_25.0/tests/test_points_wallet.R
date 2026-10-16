# Points wallet v1 — memory backend unit tests
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
Sys.setenv(YNOW_POINTS_SIGNUP_BONUS = "20")
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
check("catalog has alert_slot", {
  cat <- ynow_points_catalog()
  "alert_slot" %in% cat$sku && as.integer(cat$cost[cat$sku == "alert_slot"][[1]]) == 1L
})
check("catalog bt_run=5", {
  cat <- ynow_points_catalog()
  as.integer(cat$cost[cat$sku == "bt_run"][[1]]) == 5L
})
check("catalog hfv_run=3", {
  cat <- ynow_points_catalog()
  as.integer(cat$cost[cat$sku == "hfv_run"][[1]]) == 3L
})

uid <- "user-test-001"
check("balance starts 0", identical(ynow_points_balance(uid), 0L))

g1 <- ynow_points_grant_signup_bonus(uid)
check("signup bonus ok", isTRUE(g1$ok) && identical(g1$balance, 20L))
check("balance after signup 20", identical(ynow_points_balance(uid), 20L))

g2 <- ynow_points_grant_signup_bonus(uid)
check("signup bonus idempotent", isTRUE(g2$ok) && isTRUE(g2$idempotent) && identical(ynow_points_balance(uid), 20L))

s1 <- ynow_points_try_spend(uid, "alert_slot", "alert:u:AAPL:price_lte:100:2026-10-16")
check("spend alert -1", isTRUE(s1$ok) && identical(ynow_points_balance(uid), 19L))

s1b <- ynow_points_try_spend(uid, "alert_slot", "alert:u:AAPL:price_lte:100:2026-10-16")
check("spend alert idempotent", isTRUE(s1b$ok) && isTRUE(s1b$idempotent) && identical(ynow_points_balance(uid), 19L))

s2 <- ynow_points_try_spend(uid, "bt_run", "bt:1")
check("spend bt -5", isTRUE(s2$ok) && identical(ynow_points_balance(uid), 14L))

s3 <- ynow_points_try_spend(uid, "hfv_run", "hfv:1")
check("spend hfv -3", isTRUE(s3$ok) && identical(ynow_points_balance(uid), 11L))

# Drain to zero then fail
while (ynow_points_balance(uid) > 0L) {
  bal <- ynow_points_balance(uid)
  ynow_points_try_spend(uid, "alert_slot", paste0("drain:", bal))
}
fail <- ynow_points_try_spend(uid, "alert_slot", "drain:overflow")
check("insufficient fails", !isTRUE(fail$ok) && identical(fail$message_key, "pts_msg_insufficient"))
check("balance stays 0", identical(ynow_points_balance(uid), 0L))

cred <- ynow_points_credit(uid, 5L, reason = "earn_admin", ref_id = "admin:1")
check("admin credit +5", isTRUE(cred$ok) && identical(ynow_points_balance(uid), 5L))

tail <- ynow_points_ledger_tail(uid, n = 50L)
check("ledger has rows", is.data.frame(tail) && nrow(tail) >= 3L)
all_rows <- ynow_points_ledger_all(uid)
check("ledger_all returns every row", is.data.frame(all_rows) && nrow(all_rows) >= nrow(tail))
check("ledger has note column", "note" %in% names(all_rows))
check("ledger has ref_id column", "ref_id" %in% names(all_rows))

# Gate off always allows
Sys.setenv(YNOW_POINTS_ENABLED = "0")
check("enabled false", !isTRUE(ynow_points_enabled()))
# Fake session-less gate: when disabled, returns TRUE even with NULL session
check("gate allows when disabled", isTRUE(ynow_points_gate(NULL, "alert_slot", "x", user = list(id = uid))))

# Locale keys present
for (k in c(
  "pts_balance", "pts_ledger", "pts_msg_insufficient_fmt", "pts_admin_credit",
  "auth_admin_tool_points"
)) {
  check(paste("locale en", k), nzchar(ui_str(k, "en")))
  check(paste("locale zh", k), nzchar(ui_str(k, "zh-TW")))
}

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
check("admin panel shows points", {
  auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("points_balance", auth, fixed = TRUE) && grepl("ynow_pts_admin_submit", auth, fixed = TRUE)
})
check("account_points is independent tab", {
  auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
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

cat("PASS", n_ok, " FAIL", n_fail, "\n")
if (n_fail > 0L) quit(status = 1)
cat("ALL PASS\n")
