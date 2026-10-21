# Sidebar tab pins: max 12, normalize, file persist, 3-col UI wiring, Lite/ACL respect
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
tmpdir <- tempfile("ynow_sidebar_pins_")
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

check(
  "normalize keeps order + drops junk",
  identical(
    ynow_prefs_normalize_sidebar_tabs(c("home", "HOME", "nope", "dashboard", "home")),
    c("home", "dashboard")
  )
)
check(
  "pinnable includes core + models",
  all(c("home", "dcf_calculator", "lab_notes", "snapshot") %in% ynow_sidebar_pinnable_tabs())
)
check(
  "pin meta has icon + short labels",
  {
    m <- ynow_sidebar_pin_meta("decision_checklist")
    nzchar(m$icon) && nzchar(m$short_en) && nzchar(m$short_zh)
  }
)

uid <- "test-user-sidebar-pins-1"
check("empty get", identical(ynow_prefs_get_pinned_sidebar(uid), character(0)))

a <- ynow_prefs_toggle_pinned_sidebar(uid, "home")
check("pin home", identical(as.character(a), "home"))
check("get after pin", identical(ynow_prefs_get_pinned_sidebar(uid), "home"))

b <- ynow_prefs_toggle_pinned_sidebar(uid, "dashboard")
check("pin dashboard front", identical(as.character(b), c("dashboard", "home")))

c <- ynow_prefs_toggle_pinned_sidebar(uid, "home")
check("unpin home", identical(as.character(c), "dashboard"))

# Survive memory clear → file reload (logout/login within host).
ynow_prefs_reset_memory()
check(
  "file reload after memory clear",
  identical(ynow_prefs_get_pinned_sidebar(uid), "dashboard")
)

# Cap at 12 — do not evict when full.
many <- c(
  "home", "macro_market", "bluechip", "dashboard", "sensitivity",
  "smart_analysis", "about", "snapshot", "feedback", "hfv",
  "dcf_calculator", "ddm_calculator"
)
invisible(ynow_prefs_set_pinned_sidebar(uid, many))
got <- ynow_prefs_get_pinned_sidebar(uid)
check("set accepts 12", length(got) == 12L)

cap <- ynow_prefs_toggle_pinned_sidebar(uid, "ri_calculator")
check("at cap refuses 13th", identical(as.character(cap), got) && isTRUE(attr(cap, "at_cap")))

# Coexist with recent-search pins under same user prefs file.
invisible(ynow_prefs_set_pinned_recent(uid, c("AAPL")))
ynow_prefs_reset_memory()
check(
  "sidebar pins survive recent-search write + reload",
  identical(ynow_prefs_get_pinned_sidebar(uid), got)
)
check(
  "recent pins still present",
  identical(ynow_prefs_get_pinned_recent(uid), "AAPL")
)

# Wiring smoke: prefs API remains; page-pin UX disabled in favor of tab reorder.
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(root, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines(file.path(root, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("uiOutput pinned_sidebar_tabs still mounted", grepl('uiOutput\\(\"pinned_sidebar_tabs\"\\)', ui, perl = TRUE))
check("CSS 3-column grid retained (legacy)", grepl("grid-template-columns: repeat\\(3,", ui, perl = TRUE))
check("JS guest localStorage key stub", grepl("ynow_pinned_sidebar_v1", ui, fixed = TRUE))
check("page-pin UX disabled comment", grepl("page-pin UX disabled", ui, ignore.case = TRUE))
check("no menu long-press pin toggle", {
  !grepl("Shiny.setInputValue\\('pinned_sidebar_toggle'", ui, perl = TRUE)
})
check("server observes sidebar toggle (no-op)", grepl("observeEvent\\(input\\$pinned_sidebar_toggle", srv, perl = TRUE))
check("server prefs get sidebar retained", grepl("ynow_prefs_get_pinned_sidebar", srv, fixed = TRUE))
check("server pin tray render disabled", {
  grepl("output\\$pinned_sidebar_tabs", srv, perl = TRUE) &&
    grepl("Page-pin tray disabled", srv, fixed = TRUE)
})
check("en pin max toast locale kept", grepl("sidebar_pins_max_toast", loc, fixed = TRUE))
check("zh pin label locale kept", grepl("sidebar_pins_label = \"釘選\"", loc, fixed = TRUE))
check("zh max toast TW locale kept", grepl("最多可釘選 12 個頁面", loc, fixed = TRUE))
check("Lite omit pin selectors retained", {
  grepl("ynow-sidebar-pin-tile\\[data-tab=", auth, perl = TRUE) &&
    grepl("Lite: pinned tray must not surface omitted tabs", auth, fixed = TRUE)
})
check("legacy pin tray CSS hidden", {
  grepl(".ynow-sidebar-pins { display:none !important; }", auth, fixed = TRUE)
})

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
