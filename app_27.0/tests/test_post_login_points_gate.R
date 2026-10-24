# Post-login points gate: spend / top-up chrome stays visible after sign-in.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_post_login_points_gate.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

ok <- 0L
bad <- 0L
check <- function(name, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    message("OK ", name)
  } else {
    bad <<- bad + 1L
    message("FAIL ", name)
  }
}

app_dir <- normalizePath("..", mustWork = TRUE)
if (!grepl("app_27.0$", app_dir)) {
  app_dir <- normalizePath(".", mustWork = TRUE)
}

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("templates expose three chrome slots", {
  grepl("ynow_atx_lock_promo_tpl", ui, fixed = TRUE) &&
    grepl("ynow_inner_tab_lock_promo_tpl", ui, fixed = TRUE) &&
    grepl('data-ynow-gate-chrome` = "guest_auth"', ui, fixed = TRUE) &&
    grepl('data-ynow-gate-chrome` = "spend_fyn"', ui, fixed = TRUE) &&
    grepl('data-ynow-gate-chrome` = "topup_promo"', ui, fixed = TRUE)
})

check("bind runs after each cloner and apply", {
  grepl("function ynowBindGateModes()", ui, fixed = TRUE) &&
    grepl("function ynowBindGateMode(", ui, fixed = TRUE) &&
    grepl("function ynowResolve(", ui, fixed = TRUE) &&
    grepl("function ynowCoerceGateMode(", ui, fixed = TRUE) &&
    grepl("ynowBindGateModes();", ui, fixed = TRUE)
})

check("spend and top-up are not auth-open", {
  grepl("ynow-gate-spend", ui, fixed = TRUE) &&
    grepl("ynow-gate-topup", ui, fixed = TRUE) &&
    grepl("ynow_gate_spend", ui, fixed = TRUE) &&
    !grepl("ynow-gate-spend ynow-auth-open", ui, fixed = TRUE) &&
    !grepl("ynow-gate-topup ynow-auth-open", ui, fixed = TRUE)
})

check("authed css keeps spend and top-up visible", {
  grepl("body.ynow-authed .ynow-gate-spend", ui, fixed = TRUE) &&
    grepl("body.ynow-authed .ynow-gate-topup", ui, fixed = TRUE) &&
    grepl("display: inline-block !important", ui, fixed = TRUE) &&
    grepl("body.ynow-authed .ynow-gate-spend", auth, fixed = TRUE) &&
    grepl("body.ynow-authed .ynow-gate-topup", auth, fixed = TRUE) &&
    grepl("pointer-events:auto !important", auth, fixed = TRUE)
})

check("top-up opens account overview slot", {
  grepl("ynowGotoTab('account_admin')", ui, fixed = TRUE) &&
    grepl("ynow_pts_topup_slot", ui, fixed = TRUE)
})

check("server debits access cost, not a catalog sku", {
  grepl("observeEvent(input$ynow_gate_spend", srv, fixed = TRUE) &&
    grepl('reason = "access_unlock"', srv, fixed = TRUE) &&
    grepl('ref_id = paste0("acl-unlock:", id)', srv, fixed = TRUE) &&
    grepl("ynow_access_mark_points_unlocked", srv, fixed = TRUE) &&
    grepl("ynowGateSpendResult", srv, fixed = TRUE) &&
    grepl("gate_mode_preview_no_spend", srv, fixed = TRUE) &&
    !grepl("ynow_points_try_spend", srv, fixed = TRUE)
})

check("locale has gate_mode copy in en and zh-TW", {
  grepl('gate_mode_spend_cta = "Spend %s FYN"', loc, fixed = TRUE) &&
    grepl('gate_mode_spend_cta = "花費 %s FYN 解鎖"', loc, fixed = TRUE) &&
    grepl('gate_mode_topup_cta = "前往點數儲值"', loc, fixed = TRUE) &&
    grepl("gate_mode_spend_short", loc, fixed = TRUE) &&
    !grepl("一鍵", loc, fixed = TRUE) &&
    !grepl("充值", loc, fixed = TRUE)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
