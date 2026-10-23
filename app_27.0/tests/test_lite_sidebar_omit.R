# Lite sidebar omit set = guest-locked ∪ Full-only ∪ Account (preferences).
# Snapshot foot stays free; only snap_defaults is inner-hidden.
# Run: Rscript app_27.0/tests/test_lite_sidebar_omit.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_lite_sidebar_omit.R", mustWork = FALSE)
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
css <- ynow_sidebar_lite_omit_css()
omit <- ynow_sidebar_lite_omit_tabs()
guest <- ynow_sidebar_guest_lock_tabs()
full_only <- ynow_sidebar_full_only_tabs()

check(
  "guest locks are Lab + Checklist + Value Alerts + ATX",
  identical(
    sort(guest),
    sort(c("lab_notes", "decision_checklist", "price_alerts", "asset_transmission"))
  )
)
check(
  "omit includes every guest lock",
  all(guest %in% omit)
)
check(
  "omit includes Full-only models + company_advance + ATX",
  all(c("get_started", "dcf_calculator", "ddm_calculator", "ri_calculator",
        "hfv", "company_advance", "sotp_calculator", "asset_transmission") %in% omit)
)
check(
  "omit includes Account workspace",
  all(c("account_admin", "account_points", "settings") %in% omit)
)
check(
  "omit includes Asset Transmission Root via guest lock, not full_only",
  "asset_transmission" %in% omit &&
    "asset_transmission" %in% guest &&
    !"asset_transmission" %in% full_only
)
check(
  "omit does not include free Lite core tabs",
  !any(c("home", "smart_analysis", "macro_market", "bluechip", "dashboard",
         "sensitivity", "about") %in% omit)
)
check(
  "omit does not include Snapshot page (foot stays free)",
  !"snapshot" %in% omit
)

for (t in c("lab_notes", "price_alerts", "ddm_calculator", "ri_calculator",
            "get_started", "asset_transmission")) {
  check(
    paste0("Lite CSS hides ", t),
    grepl(sprintf('data-value="%s"', t), css, fixed = TRUE)
  )
}
check(
  "Lite omit CSS has no page-pin tile selectors",
  !grepl("ynow-sidebar-pin-tile", css, fixed = TRUE)
)

check("ynow_ui injects lite omit CSS", grepl("ynow_sidebar_lite_omit_css", ui, fixed = TRUE))
check("ynow_ui injects __YNOW_LITE_OMIT_TABS", grepl("__YNOW_LITE_OMIT_TABS", ui, fixed = TRUE))
check(
  "FULL_ONLY_TABS reads omit list (price_alerts covered)",
  grepl("__YNOW_LITE_OMIT_TABS", ui, fixed = TRUE) &&
    grepl("price_alerts", ui, fixed = TRUE)
)
check(
  "hardcoded Lite model-hide block removed (get_started list)",
  !grepl(
    'body.ynow-lite .sidebar-menu a\\[data-value="get_started"\\],\\s*\n\\s*body.ynow-lite .sidebar-menu li:has\\(> a\\[data-value="get_started"\\]\\),\\s*\n\\s*body.ynow-lite .sidebar-menu li:has\\(a\\[data-value="nav_calculator"\\]\\)',
    ui
  )
)
check(
  "Lite still shows Smart Analysis",
  grepl(
    'body.ynow-lite .sidebar-menu a[data-value="smart_analysis"]',
    ui,
    fixed = TRUE
  ) && grepl("display: block !important", ui, fixed = TRUE)
)
check(
  "Lite Snapshot only hides snap_defaults (not audit/current)",
  grepl('data-value="snap_defaults"', ui, fixed = TRUE) &&
    !grepl(
      'body.ynow-lite #snapshot_report > li:has(> a[data-value="snap_audit"])',
      ui,
      fixed = TRUE
    ) &&
    !grepl(
      'body.ynow-lite #snapshot_report > li:has(> a[data-value="snap_current"])',
      ui,
      fixed = TRUE
    )
)

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
invisible(NULL)
