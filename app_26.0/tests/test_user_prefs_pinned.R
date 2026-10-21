# Pinned recent-search prefs: normalize + file/memory toggle persist
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

# Force file/memory path (no live DB required).
Sys.setenv(YNOW_USER_PREFS_FORCE_FILE = "1")
tmpdir <- tempfile("ynow_prefs_")
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
old_wd <- getwd()
on.exit({
  setwd(old_wd)
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  Sys.unsetenv("YNOW_USER_PREFS_FORCE_FILE")
}, add = TRUE)
setwd(tmpdir)

# Minimal stubs if auth_db helpers are absent.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("SKIP: jsonlite not installed\n")
  quit(status = 0L)
}

source(file.path(root, "user_prefs.R"), local = FALSE, encoding = "UTF-8")
ynow_prefs_reset_memory()

check(
  "normalize uppercases + dedupes",
  identical(
    ynow_prefs_normalize_tickers(c("aapl", "AAPL", " msft ", "", "BAD SPACE")),
    c("AAPL", "MSFT")
  )
)
check(
  "normalize rejects junk",
  identical(ynow_prefs_normalize_tickers(c("OK", "!!!", "too_long_ticker_name_xxxxx")), c("OK"))
)

uid <- "test-user-pinned-1"
check("empty get", identical(ynow_prefs_get_pinned_recent(uid), character(0)))

a <- ynow_prefs_toggle_pinned_recent(uid, "AAPL")
check("pin AAPL", identical(as.character(a), "AAPL") && !isTRUE(attr(a, "at_cap")))
check("get after pin", identical(ynow_prefs_get_pinned_recent(uid), "AAPL"))

b <- ynow_prefs_toggle_pinned_recent(uid, "msft")
check("pin MSFT front", identical(as.character(b), c("MSFT", "AAPL")))

c <- ynow_prefs_toggle_pinned_recent(uid, "AAPL")
check("unpin AAPL", identical(as.character(c), "MSFT"))

# Survive "logout" in-process: clear memory cache, reload from file.
ynow_prefs_reset_memory()
check(
  "file reload after memory clear",
  identical(ynow_prefs_get_pinned_recent(uid), "MSFT")
)

# Cap at 12 (4×3 grid)
many <- paste0("T", seq_len(15L))
invisible(ynow_prefs_set_pinned_recent(uid, many))
got <- ynow_prefs_get_pinned_recent(uid)
check("max 12 pins", length(got) == 12L)

cap <- ynow_prefs_toggle_pinned_recent(uid, "ZZZZ")
check(
  "toggle at cap refuses 13th",
  identical(as.character(cap), got) && isTRUE(attr(cap, "at_cap"))
)

# Wiring smoke (source text).
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(root, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
gbl <- paste(readLines(file.path(root, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("global sources user_prefs.R", grepl('source\\(\"user_prefs\\.R\"', gbl, perl = TRUE))
check("uiOutput pinned_recent_search", grepl('uiOutput\\(\"pinned_recent_search\"\\)', ui, perl = TRUE))
check("JS long-press 2000", grepl("LONG_MS = 2000", ui, fixed = TRUE))
check("JS pinned_search_toggle", grepl("pinned_search_toggle", ui, fixed = TRUE))
check("server observes toggle", grepl("observeEvent\\(input\\$pinned_search_toggle", srv, perl = TRUE))
check("server prefs get", grepl("ynow_prefs_get_pinned_recent", srv, fixed = TRUE))
check("pinned prefs prefer view-as uid when active", {
  grepl("ynow_pts_view_as", srv, fixed = TRUE) &&
    grepl("va <- tryCatch\\(isolate\\(ynow_pts_view_as\\(\\)\\)", srv, perl = TRUE) &&
    grepl("view_as = tryCatch\\(ynow_pts_view_as\\(\\)", srv, perl = TRUE)
})
check("en pin hint", grepl("recent_search_pin_hint", loc, fixed = TRUE))
check("en max toast", grepl("recent_search_max_toast", loc, fixed = TRUE))
check("zh pin hint", grepl("長按 2 秒釘選", loc, fixed = TRUE))
check("zh unpin hint", grepl("長按 2 秒取消釘選", loc, fixed = TRUE))
check("zh max toast", grepl("最多可釘選 12 檔股票", loc, fixed = TRUE))
check("page-pin locale keys removed", !grepl("sidebar_pins_max_toast", loc, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
