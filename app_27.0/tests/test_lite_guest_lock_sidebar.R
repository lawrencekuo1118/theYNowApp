# Lite sidebar must omit every Full guest-locked tab (no grayed locks).
# Source of truth: ynow_sidebar_guest_lock_tabs() in auth_account.R
# Run: Rscript app_27.0/tests/test_lite_guest_lock_sidebar.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_lite_guest_lock_sidebar.R", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

ok <- 0L
bad <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    bad <<- bad + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
  invisible(isTRUE(cond))
}

source(file.path(app_dir, "auth_account.R"), local = TRUE, encoding = "UTF-8")
ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

tabs <- ynow_sidebar_guest_lock_tabs()
css <- ynow_sidebar_guest_lock_lite_css()
js_map <- ynow_sidebar_guest_lock_js_map()

check(
  "guest-lock tab set is Lab + Checklist + MOS Assumptions + Value Alerts + ATX",
  identical(
    sort(tabs),
    sort(c(
      "lab_notes", "decision_checklist", "mos_assumptions",
      "price_alerts", "asset_transmission", "global_prediction"
    ))
  )
)
check("asset_transmission is guest-locked", isTRUE(ynow_sidebar_tab_guest_locked("asset_transmission")))
check("global_prediction is guest-locked", isTRUE(ynow_sidebar_tab_guest_locked("global_prediction")))
check("price_alerts is guest-locked", isTRUE(ynow_sidebar_tab_guest_locked("price_alerts")))
check("home is not guest-locked", !isTRUE(ynow_sidebar_tab_guest_locked("home")))
check("about is not guest-locked", !isTRUE(ynow_sidebar_tab_guest_locked("about")))

css <- ynow_sidebar_lite_omit_css()
check("Lite CSS hides lab_notes", grepl('data-value="lab_notes"', css, fixed = TRUE))
check("Lite CSS hides decision_checklist", grepl('data-value="decision_checklist"', css, fixed = TRUE))
check("Lite CSS hides mos_assumptions", grepl('data-value="mos_assumptions"', css, fixed = TRUE))
check("Lite CSS hides price_alerts", grepl('data-value="price_alerts"', css, fixed = TRUE))
check("Lite CSS uses display none", grepl("display: none !important", css, fixed = TRUE))
check(
  "every guest-lock tab has a + li selectors",
  all(vapply(tabs, function(t) {
    grepl(sprintf('body.ynow-lite .sidebar-menu a[data-value="%s"]', t), css, fixed = TRUE) &&
      grepl(sprintf('body.ynow-lite .sidebar-menu li:has(> a[data-value="%s"])', t), css, fixed = TRUE)
  }, logical(1)))
)

check(
  "ynow_ui injects Lite omit CSS helper",
  grepl("ynow_sidebar_lite_omit_css", ui, fixed = TRUE) ||
    grepl("ynow_sidebar_guest_lock_lite_css", ui, fixed = TRUE)
)
check("ynow_ui injects JS lock map", grepl("__YNOW_SIDEBAR_GUEST_LOCK_TABS", ui, fixed = TRUE))
check("ynow_ui uses guest lock label helper", grepl("ynow_sidebar_guest_lock_label", ui, fixed = TRUE))
check("JS map helper referenced", grepl("ynow_sidebar_guest_lock_js_map", ui, fixed = TRUE))
check("Lite omit list includes price_alerts", "price_alerts" %in% ynow_sidebar_lite_omit_tabs())
check("fallback loginTabs keep lab_notes", grepl("lab_notes: true", ui, fixed = TRUE))
check("fallback loginTabs keep decision_checklist", grepl("decision_checklist: true", ui, fixed = TRUE))
check("fallback loginTabs keep mos_assumptions", grepl("mos_assumptions: true", ui, fixed = TRUE))
check("fallback loginTabs keep price_alerts", grepl("price_alerts: true", ui, fixed = TRUE))
check("fallback loginTabs keep asset_transmission", grepl("asset_transmission: true", ui, fixed = TRUE))
check("fallback loginTabs keep global_prediction", grepl("global_prediction: true", ui, fixed = TRUE))

check(
  "JS map keys match guest-lock tab set",
  identical(sort(names(js_map)), sort(tabs)) && all(vapply(js_map, isTRUE, logical(1)))
)
check("auth documents Lite omit rule", grepl("Lite must omit these entirely", auth, fixed = TRUE))

# Static label helper still emits lock glyph for locked tabs (needs htmltools/shiny)
if (requireNamespace("htmltools", quietly = TRUE)) {
  suppressPackageStartupMessages({
    if (requireNamespace("shiny", quietly = TRUE)) library(shiny)
    else library(htmltools)
  })
  lab <- as.character(
    ynow_sidebar_guest_lock_label("price_alerts", "ynow_menu_price_alert", "Value Alerts")
  )
  check("label helper emits ynow-menu-lock", grepl("ynow-menu-lock", lab, fixed = TRUE))
  home_lab <- as.character(
    ynow_sidebar_guest_lock_label("home", "ynow_menu_home", "Home")
  )
  check("label helper skips lock for unlocked tabs", !grepl("ynow-menu-lock", home_lab, fixed = TRUE))
} else {
  check("label helper emits ynow-menu-lock (skipped: no htmltools)", TRUE)
  check("label helper skips lock for unlocked tabs (skipped: no htmltools)", TRUE)
}

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
invisible(NULL)
