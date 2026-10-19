# Static HTML fixture: guest vs signed-in sidebar lock styling (no full Shiny).
# Writes under /opt/cursor/artifacts/sidebar-lock-fixtures/
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1", YNOW_ENABLE_ACCOUNT_LOGIN = "1")
suppressPackageStartupMessages({
  library(htmltools)
  library(shiny)
})

app_dir <- normalizePath("..", winslash = "/", mustWork = TRUE)
source(file.path(app_dir, "auth_account.R"), local = TRUE)

out_dir <- "/opt/cursor/artifacts/sidebar-lock-fixtures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Extract locked-item CSS rules from ynow_ui.R (guest gray + PDF mute + menu-lock)
ui_txt <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth_css <- paste(as.character(ynow_auth_css()), collapse = "\n")

# Pull a bounded CSS slice for sidebar locks / download
css_bits <- character()
for (pat in c(
  "Locked sidebar items stay visible",
  "Guest PDF download: muted",
  "Guest: sidebar PDF control shows a small lock",
  ".sidebar-menu .ynow-menu-lock",
  "ynow-sidebar-download-wrap"
)) {
  i <- regexpr(pat, ui_txt, fixed = TRUE)
  if (i > 0) {
    chunk <- substr(ui_txt, i, min(nchar(ui_txt), i + 2200L))
    # trim to next major comment if huge
    css_bits <- c(css_bits, chunk)
  }
}

page <- function(title, body_class, extra_note = "") {
  sprintf(
    paste0(
      "<!DOCTYPE html><html><head><meta charset='utf-8'><title>%s</title>",
      "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css'/>",
      "<style>",
      "body{margin:0;font-family:Helvetica,Arial,sans-serif;background:#f4f4f4;}",
      ".skin-black .main-sidebar{width:250px;min-height:100vh;background:#1a1a1a;color:#fff;padding:12px 0;}",
      ".sidebar-menu{list-style:none;margin:0;padding:8px 0;}",
      ".sidebar-menu>li>a,.sidebar-menu .treeview-menu>li>a{",
      "display:flex;align-items:center;gap:6px;padding:10px 14px;color:rgba(255,255,255,0.92);",
      "text-decoration:none;font-size:13px;}",
      ".skin-black .main-sidebar a{color:rgba(255,255,255,0.92)!important;}",
      ".treeview-menu{list-style:none;margin:0;padding:0 0 0 18px;display:block;}",
      ".ynow-menu-lock{display:none;font-size:11px;margin:0 4px 0 2px;}",
      "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu .ynow-menu-lock{display:inline-block;}",
      "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a:has(.ynow-menu-lock) > i:not(.ynow-menu-lock):not(.pull-right){display:none!important;}",
      ".ynow-sidebar-download-wrap{padding:10px;text-align:center;}",
      ".ynow-sidebar-download-btn{display:inline-block;width:100%%;box-sizing:border-box;",
      "background:#1a1a1a;color:#fff;border:1px solid #000;padding:8px 10px;font-size:13px;font-weight:bold;text-decoration:none;}",
      "body.ynow-auth-feature:not(.ynow-authed) .ynow-sidebar-download-btn{",
      "background-color:#2e2e2e!important;color:#a3a3a3!important;border-color:#666!important;}",
      "body.ynow-auth-feature:not(.ynow-authed) .ynow-download-report-quota{color:#a3a3a3;}",
      ".ynow-download-report-quota{margin:6px 0 0;font-size:11px;color:#d9d9d9;}",
      ".note{margin:16px;max-width:520px;font-size:13px;color:#333;}",
      "</style></head><body class='%s'><div class='skin-black'><aside class='main-sidebar'>",
      "<ul class='sidebar-menu'>",
      "<li><a data-value='home' href='#'><i class='fas fa-home'></i><span> Home</span></a></li>",
      "<li><a data-value='dcf_calculator' href='#'><i class='fas fa-calculator'></i><span> DCF</span></a></li>",
      "<li><a data-value='lab_notes' href='#'><i class='fas fa-flask'></i>",
      "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i><span> Quant Backtest Lab</span></a></li>",
      "<li><a data-value='decision_checklist' href='#'><i class='fas fa-clipboard-check'></i>",
      "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i><span> Decision Checklist</span></a></li>",
      "<li><a data-value='price_alerts' href='#'><i class='fas fa-bell'></i>",
      "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i><span> Value Alerts</span></a></li>",
      "<li class='treeview'><a href='#'><i class='fas fa-toolbox'></i>",
      "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i><span> Account</span></a>",
      "<ul class='treeview-menu'>",
      "<li><a data-value='account_admin' href='#'><i class='fas fa-user-cog'></i>",
      "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i><span> Overview</span></a></li>",
      "<li><a data-value='account_points' href='#'><i class='fas fa-list-alt'></i>",
      "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i><span> Points ledger</span></a></li>",
      "</ul></li>",
      "<li><a data-value='about' href='#'><i class='fas fa-info-circle'></i><span> About</span></a></li>",
      "</ul>",
      "<div class='ynow-sidebar-download-wrap'>",
      "<a id='download_report' class='ynow-sidebar-download-btn' href='#'>",
      "<i class='%s' role='presentation'></i> Download Report (PDF)</a>",
      "<p class='ynow-download-report-quota'>Sign-in required. Three free downloads each Asia/Taipei day.</p>",
      "</div></aside></div>",
      "<p class='note'>%s%s</p></body></html>"
    ),
    title,
    body_class,
    if (grepl("ynow-authed", body_class, fixed = TRUE)) "fas fa-download" else "fas fa-lock ynow-sidebar-dl-lock",
    extra_note,
    ""
  )
}

writeLines(
  page(
    "Guest sidebar locks",
    "ynow-auth-feature",
    "GUEST: Lab/Checklist/Alerts keep the same title color as Asset Transmission Root; lock replaces the glyph. PDF stays muted."
  ),
  file.path(out_dir, "guest-sidebar-locks.html"),
  useBytes = TRUE
)
writeLines(
  page(
    "Signed-in sidebar",
    "ynow-auth-feature ynow-authed",
    "SIGNED-IN: locks hidden; PDF download icon; normal white labels."
  ),
  file.path(out_dir, "authed-sidebar-unlocked.html"),
  useBytes = TRUE
)

# Assert source still has the live rules we care about
stopifnot(grepl("ynowSyncSidebarDownloadIcon", ui_txt, fixed = TRUE))
stopifnot(grepl("color: #a3a3a3 !important", ui_txt, fixed = TRUE))
stopifnot(!grepl("account_admin: true", ui_txt, fixed = TRUE))
stopifnot(!grepl(
  "body.skin-black.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .main-sidebar .sidebar-menu a:has(.ynow-menu-lock)",
  ui_txt,
  fixed = TRUE
))
stopifnot(grepl("treeview:has(a[data-value=\\\"account_admin\\\"])", auth_css, fixed = TRUE))
stopifnot(grepl("display:none !important;", auth_css, fixed = TRUE))

cat("wrote", out_dir, "\n")
cat("fixture ok\n")
