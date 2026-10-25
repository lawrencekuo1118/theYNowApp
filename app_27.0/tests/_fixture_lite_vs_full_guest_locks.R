# Static HTML fixture: Full guest (locked tabs visible) vs Lite (those tabs omitted).
# Tab ids from ynow_sidebar_guest_lock_tabs(): lab_notes, decision_checklist, price_alerts
# Writes under /opt/cursor/artifacts/ and Project media/
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1", YNOW_ENABLE_ACCOUNT_LOGIN = "1")
suppressPackageStartupMessages({
  library(htmltools)
  library(shiny)
})

app_dir <- normalizePath("..", winslash = "/", mustWork = TRUE)
source(file.path(app_dir, "auth_account.R"), local = TRUE)

tabs <- ynow_sidebar_guest_lock_tabs()
lite_css <- ynow_sidebar_guest_lock_lite_css()

out_dirs <- c(
  "/opt/cursor/artifacts/lite-vs-full-guest-locks",
  "/cursor/stores/bc-ee52ce88-0754-4bd3-9466-84bb18df73aa/media"
)
for (d in out_dirs) dir.create(d, showWarnings = FALSE, recursive = TRUE)

lock_items <- paste0(
  "<li><a data-value='lab_notes' href='#'><i class='fas fa-flask'></i>",
  "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i><span> Quant Backtest Lab</span></a></li>",
  "<li><a data-value='decision_checklist' href='#'><i class='fas fa-clipboard-check'></i>",
  "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i><span> Decision Checklist</span></a></li>",
  "<li><a data-value='price_alerts' href='#'><i class='fas fa-bell'></i>",
  "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i><span> Value Alerts</span></a></li>"
)

page <- function(title, body_class, note) {
  sprintf(
    paste0(
      "<!DOCTYPE html><html><head><meta charset='utf-8'><title>%s</title>",
      "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css'/>",
      "<style>",
      "body{margin:0;font-family:Helvetica,Arial,sans-serif;background:#f4f4f4;}",
      ".skin-black .main-sidebar{width:260px;min-height:100vh;background:#1a1a1a;color:#fff;padding:12px 0;}",
      ".sidebar-menu{list-style:none;margin:0;padding:8px 0;}",
      ".sidebar-menu>li>a{display:flex;align-items:center;gap:6px;padding:10px 14px;",
      "color:rgba(255,255,255,0.92);text-decoration:none;font-size:13px;}",
      ".ynow-menu-lock{display:none;font-size:11px;margin:0 4px 0 2px;}",
      "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu .ynow-menu-lock{display:inline-block;}",
      "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a:has(.ynow-menu-lock) > i:not(.ynow-menu-lock):not(.pull-right){display:none!important;}",
      "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a:has(> .ynow-menu-lock){opacity:0.55;}",
      "body.ynow-lite .ynow-menu-lock{display:none!important;}",
      "%s",
      ".badge{display:inline-block;margin:12px 16px;padding:4px 10px;border-radius:4px;",
      "background:#249C60;color:#fff;font-size:12px;font-weight:bold;}",
      "body:not(.ynow-lite) .badge-lite{display:none;}",
      "body.ynow-lite .badge-full{display:none;}",
      ".note{margin:16px;max-width:560px;font-size:13px;color:#333;line-height:1.45;}",
      ".ids{font-family:ui-monospace,monospace;font-size:12px;}",
      "</style></head><body class='%s'><div class='skin-black'><aside class='main-sidebar'>",
      "<span class='badge badge-full'>FULL · guest</span>",
      "<span class='badge badge-lite'>LITE</span>",
      "<ul class='sidebar-menu'>",
      "<li><a data-value='home' href='#'><i class='fas fa-home'></i><span> Home</span></a></li>",
      "<li><a data-value='smart_analysis' href='#'><i class='fas fa-lightbulb'></i><span> Smart Analysis</span></a></li>",
      "%s",
      "<li><a data-value='about' href='#'><i class='fas fa-info-circle'></i><span> About</span></a></li>",
      "</ul></aside></div>",
      "<p class='note'>%s<br><span class='ids'>Hidden in Lite: %s</span></p></body></html>"
    ),
    title,
    lite_css,
    body_class,
    lock_items,
    note,
    paste(tabs, collapse = ", ")
  )
}

full_html <- page(
  "Full guest — locked tabs visible",
  "ynow-auth-feature",
  "FULL guest: Lab / Decision Checklist / Value Alerts show lock glyphs (same set as ynow_sidebar_guest_lock_tabs)."
)
lite_html <- page(
  "Lite — guest-locked tabs omitted",
  "ynow-auth-feature ynow-lite",
  "LITE: those locked tabs are not rendered (CSS display:none from the shared lock set)."
)

for (d in out_dirs) {
  writeLines(full_html, file.path(d, "full-guest-sidebar-locked-tabs.html"), useBytes = TRUE)
  writeLines(lite_html, file.path(d, "lite-sidebar-locked-tabs-hidden.html"), useBytes = TRUE)
}

stopifnot(identical(
  sort(tabs),
  sort(c("lab_notes", "decision_checklist", "mos_assumptions", "price_alerts", "asset_transmission", "global_prediction"))
))
stopifnot(grepl('data-value="price_alerts"', lite_css, fixed = TRUE))
cat("wrote fixtures for Full vs Lite guest-lock compare\n")
cat("tabs:", paste(tabs, collapse = ", "), "\n")
