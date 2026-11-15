# Stock pins from Recent Search: max 12, 4×3 grid UI; no page-pin chrome
# (tab long-press reserved for sibling customize + drag reorder).
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

Sys.setenv(YNOW_USER_PREFS_FORCE_FILE = "1")
tmpdir <- tempfile("ynow_stock_pins_")
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
old_wd <- getwd()
on.exit({
  setwd(old_wd)
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  Sys.unsetenv("YNOW_USER_PREFS_FORCE_FILE")
}, add = TRUE)
setwd(tmpdir)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("SKIP: jsonlite not installed\n")
  quit(status = 0L)
}

source(file.path(root, "user_prefs.R"), local = FALSE, encoding = "UTF-8")
ynow_prefs_reset_memory()

check(".YNOW_PINNED_RECENT_MAX is 12", identical(.YNOW_PINNED_RECENT_MAX, 12L))

uid <- "test-user-stock-pins-1"
check("empty get", identical(ynow_prefs_get_pinned_recent(uid), character(0)))

a <- ynow_prefs_toggle_pinned_recent(uid, "AAPL")
check("pin AAPL", identical(as.character(a), "AAPL") && !isTRUE(attr(a, "at_cap")))
b <- ynow_prefs_toggle_pinned_recent(uid, "msft")
check("pin MSFT front", identical(as.character(b), c("MSFT", "AAPL")))
c <- ynow_prefs_toggle_pinned_recent(uid, "AAPL")
check("unpin AAPL", identical(as.character(c), "MSFT"))

ynow_prefs_reset_memory()
check(
  "file reload after memory clear",
  identical(ynow_prefs_get_pinned_recent(uid), "MSFT")
)

many <- paste0("T", seq_len(12L))
invisible(ynow_prefs_set_pinned_recent(uid, many))
got <- ynow_prefs_get_pinned_recent(uid)
check("set accepts 12", length(got) == 12L)

cap <- ynow_prefs_toggle_pinned_recent(uid, "ZZZZ")
check(
  "at cap refuses 13th",
  identical(as.character(cap), got) && isTRUE(attr(cap, "at_cap"))
)

# Coexist with sidebar tab order prefs (sibling reorder).
if (exists("ynow_prefs_set_sidebar_tab_order", mode = "function")) {
  invisible(ynow_prefs_set_sidebar_tab_order(uid, c("dashboard", "home")))
  ynow_prefs_reset_memory()
  check(
    "stock pins survive sidebar order write + reload",
    identical(ynow_prefs_get_pinned_recent(uid), got)
  )
}

srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(root, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines(file.path(root, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("uiOutput pinned_recent_search", grepl('uiOutput\\(\"pinned_recent_search\"\\)', ui, perl = TRUE))
check("no uiOutput pinned_sidebar_tabs in layout", !grepl('uiOutput\\(\"pinned_sidebar_tabs\"\\)', ui, perl = TRUE))
check("CSS 3-column grid on pinned search", grepl("grid-template-columns: repeat\\(3,", ui, perl = TRUE))
check("CSS class ynow-pinned-search", grepl("\\.ynow-pinned-search", ui, perl = TRUE))
check("no page-pin tile CSS", !grepl("ynow-sidebar-pin-tile", ui, fixed = TRUE))
check("JS MAX_PINS 12", grepl("MAX_PINS = 12", ui, fixed = TRUE))
check("JS pinned_search_toggle", grepl("pinned_search_toggle", ui, fixed = TRUE))
check(
  "long-press only on recent/pinned chips",
  grepl("\\.ynow-recent-chip, \\.ynow-pinned-chip", ui, perl = TRUE)
)
# Long-press must not abort when the pointer leaves the tiny chip bounds.
check("long-press uses setPointerCapture", grepl("setPointerCapture", ui, fixed = TRUE))
check("long-press MOVE_PX abort threshold", grepl("MOVE_PX = 12", ui, fixed = TRUE))
check(
  "long-press does not clear on pointerleave",
  !grepl(
    "pointerup pointercancel pointerleave', '\\.ynow-recent-chip, \\.ynow-pinned-chip'",
    ui,
    perl = TRUE
  )
)
check(
  "long-press blocks chip contextmenu",
  grepl("contextmenu', '\\.ynow-recent-chip, \\.ynow-pinned-chip'", ui, perl = TRUE)
)
check("server observes stock pin toggle", grepl("observeEvent\\(input\\$pinned_search_toggle", srv, perl = TRUE))
check("server at_cap toast key", grepl("recent_search_max_toast", srv, fixed = TRUE))
check("en max toast", grepl("recent_search_max_toast", loc, fixed = TRUE))
check("zh max toast TW", grepl("最多可釘選 12 檔股票", loc, fixed = TRUE))
check("page-pin locale keys removed", !grepl("sidebar_pins_max_toast", loc, fixed = TRUE))
check("reorder locale kept", grepl("sidebar_reorder_btn", loc, fixed = TRUE))
check("Lite omit no pin-tile selectors", !grepl("ynow-sidebar-pin-tile", auth, fixed = TRUE))
check(
  "page-pin toggle is noop / disabled",
  grepl("Page-pin toggle disabled", srv, fixed = TRUE) ||
    grepl("observeEvent\\(input\\$pinned_sidebar_toggle,[\\s\\S]*?invisible\\(NULL\\)", srv, perl = TRUE)
)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
