# Gain/loss color scheme prefs + CSS wiring.
# Run: cd app_27.0 && Rscript tests/test_gain_loss_scheme.R
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK" else "FAIL", ":", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

root <- normalizePath("..", mustWork = TRUE)
if (!file.exists(file.path(root, "user_prefs.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

source(file.path(root, "user_prefs.R"), local = FALSE, encoding = "UTF-8")

check("scheme ids include auto + four pairs", {
  all(c("auto", "green_red", "red_green", "blue_red", "red_blue") %in% YNOW_GAIN_LOSS_SCHEMES)
})

check("normalize aliases", {
  identical(ynow_prefs_normalize_gain_loss_scheme("Blue Up Red Down"), "blue_red") &&
    identical(ynow_prefs_normalize_gain_loss_scheme("tw"), "red_green") &&
    identical(ynow_prefs_normalize_gain_loss_scheme("us"), "green_red") &&
    identical(ynow_prefs_normalize_gain_loss_scheme("nope"), "auto")
})

check("auto US = green up", {
  c <- ynow_gain_loss_colors("auto", "US")
  identical(c$up, "#1e7a46") && identical(c$down, "#c0392b") &&
    identical(c$scheme, "green_red")
})
check("auto TW = red up", {
  c <- ynow_gain_loss_colors("auto", "TW")
  identical(c$up, "#c0392b") && identical(c$down, "#1e7a46") &&
    identical(c$scheme, "red_green")
})
check("blue_red pair", {
  c <- ynow_gain_loss_colors("blue_red", "TW")
  identical(c$up, "#0C5484") && identical(c$down, "#c0392b") &&
    identical(c$scheme, "blue_red")
})
check("red_blue swapped", {
  c <- ynow_gain_loss_colors("red_blue", "US")
  identical(c$up, "#c0392b") && identical(c$down, "#0C5484")
})

check("treemap colorscale ends with up color", {
  us <- ynow_gain_loss_treemap_colorscale("auto", "US")
  tw <- ynow_gain_loss_treemap_colorscale("auto", "TW")
  br <- ynow_gain_loss_treemap_colorscale("blue_red", "US")
  identical(tolower(us[[length(us)]][[2]]), "#1e7a46") &&
    identical(tolower(tw[[length(tw)]][[2]]), "#c0392b") &&
    identical(tolower(br[[length(br)]][[2]]), "#0c5484")
})

atx <- paste(
  readLines(file.path(root, "asset_transmission_module.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
check("asset_tx_move_colors wires ynow_gain_loss_colors", {
  grepl("ynow_gain_loss_colors", atx, fixed = TRUE) &&
    grepl("gain_scheme = \"auto\"", atx, fixed = TRUE) &&
    grepl("gain_scheme_rv", atx, fixed = TRUE)
})

ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines(file.path(root, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(root, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("CSS vars + scheme attributes", {
  grepl("--ynow-chg-up", ui, fixed = TRUE) &&
    grepl("data-ynow-gain-scheme=\"blue_red\"", ui, fixed = TRUE) &&
    grepl("data-ynow-gain-scheme=\"red_blue\"", ui, fixed = TRUE) &&
    grepl("gain_loss_scheme", ui, fixed = TRUE)
})
check("HFV uses CSS vars", {
  grepl("\\.ynow-hfv-bull \\{ color: var\\(--ynow-chg-up\\)", ui, perl = TRUE) &&
    grepl("\\.ynow-hfv-bear \\{ color: var\\(--ynow-chg-down\\)", ui, perl = TRUE)
})
check("Account top-up redeem path (no payment processor)", {
  wallet <- paste(readLines(file.path(root, "points_wallet.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_pts_topup_slot", auth, fixed = TRUE) &&
    grepl("pts_topup_title", auth, fixed = TRUE) &&
    grepl("ynow_pts_redeem_code", auth, fixed = TRUE) &&
    grepl("ynow_pts_redeem_submit", auth, fixed = TRUE) &&
    grepl("ynow_points_redeem", wallet, fixed = TRUE) &&
    !grepl("stripe|paypal|chargeCard|paymentIntent", auth, ignore.case = TRUE)
})
check("no Mainland 一鍵 wording", {
  !grepl("一鍵", loc, fixed = TRUE) &&
    grepl("settings_gain_loss = \"漲跌底色\"", loc, fixed = TRUE) &&
    grepl("pts_topup_title = \"點數儲值\"", loc, fixed = TRUE)
})

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
