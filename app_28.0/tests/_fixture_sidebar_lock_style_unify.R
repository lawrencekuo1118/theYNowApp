# Static HTML fixture: ACL-locked ATX vs Full guest locks vs Download.
# Before unify: ATX (ynow-access-locked-nav) is dimmed; guest locks are not.
# After unify: shared .ynow-sidebar-locked chrome dims all locked menu items.
# Writes under /opt/cursor/artifacts/sidebar-lock-style/ and optional MEDIA_OUT.
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1", YNOW_ENABLE_ACCOUNT_LOGIN = "1")
suppressPackageStartupMessages({
  library(htmltools)
  library(shiny)
})

app_dir <- if (file.exists("ynow_ui.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../ynow_ui.R")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot locate ynow_ui.R")
}
source(file.path(app_dir, "auth_account.R"), local = TRUE)

out_dir <- Sys.getenv("YNOW_SIDEBAR_LOCK_FIXTURE_DIR", unset = "/opt/cursor/artifacts/sidebar-lock-style")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ui_txt <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
unified <- grepl("ynow-sidebar-locked", ui_txt, fixed = TRUE) &&
  grepl(
    "Unified locked sidebar",
    ui_txt,
    fixed = TRUE
  )

# Live CSS slices used by the app (keep in sync with ynow_ui.R)
acl_dim <- paste0(
  ".sidebar-menu li.ynow-access-locked-nav > a { opacity: 0.55; }\n",
  ".sidebar-menu li.ynow-access-locked-nav .ynow-menu-lock { display: inline-block; }\n",
  ".sidebar-menu li.ynow-access-locked-nav > a > i:not(.ynow-menu-lock):not(.pull-right) { display: none !important; }\n"
)

guest_lock_glyph <- paste0(
  ".ynow-menu-lock{display:none;font-size:11px;margin:0 4px 0 2px;opacity:0.95;vertical-align:-1px;}\n",
  "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu .ynow-menu-lock{display:inline-block;}\n",
  "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a:has(.ynow-menu-lock) > i:not(.ynow-menu-lock):not(.pull-right){display:none!important;}\n"
)

# Post-unify shared rules (also applied when source already has them)
unified_css <- paste0(
  "/* Unified locked sidebar nav chrome */\n",
  ".sidebar-menu li.ynow-sidebar-locked > a,\n",
  ".sidebar-menu li.ynow-access-locked-nav > a {\n",
  "  opacity: 0.55;\n",
  "}\n",
  ".sidebar-menu li.ynow-sidebar-locked .ynow-menu-lock,\n",
  ".sidebar-menu li.ynow-access-locked-nav .ynow-menu-lock,\n",
  "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu .ynow-menu-lock {\n",
  "  display: inline-block;\n",
  "}\n",
  ".sidebar-menu li.ynow-sidebar-locked > a > i:not(.ynow-menu-lock):not(.pull-right),\n",
  ".sidebar-menu li.ynow-access-locked-nav > a > i:not(.ynow-menu-lock):not(.pull-right),\n",
  "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a:has(.ynow-menu-lock) > i:not(.ynow-menu-lock):not(.pull-right) {\n",
  "  display: none !important;\n",
  "}\n",
  "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu li:has(> a > .ynow-menu-lock) {\n",
  "  /* marker class mirrored in DOM for screenshot parity */\n",
  "}\n",
  "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a:has(> .ynow-menu-lock) {\n",
  "  opacity: 0.55;\n",
  "}\n"
)

pdf_mute <- paste0(
  ".ynow-sidebar-download-wrap{padding:10px;text-align:center;}\n",
  ".ynow-sidebar-download-btn{display:inline-block;width:100%;box-sizing:border-box;",
  "background:#1a1a1a;color:#fff;border:1px solid #000;padding:8px 10px;font-size:13px;font-weight:bold;text-decoration:none;}\n",
  "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-sidebar-download-btn{",
  "background-color:#2e2e2e!important;color:#a3a3a3!important;border-color:#666!important;}\n",
  ".ynow-sidebar-dl-lock{font-size:0.92em;margin-right:2px;opacity:0.95;}\n"
)

base_chrome <- paste0(
  "body{margin:0;font-family:Helvetica,Arial,sans-serif;background:#f4f4f4;}\n",
  ".skin-black .main-sidebar{width:260px;min-height:100vh;background:#1a1a1a;color:#fff;padding:12px 0;}\n",
  ".sidebar-menu{list-style:none;margin:0;padding:8px 0;}\n",
  ".sidebar-menu>li>a{display:flex;align-items:center;gap:6px;padding:10px 14px;",
  "color:rgba(255,255,255,0.92);text-decoration:none;font-size:13px;}\n",
  ".skin-black .main-sidebar a{color:rgba(255,255,255,0.92)!important;}\n",
  ".note{margin:16px;max-width:640px;font-size:13px;color:#333;line-height:1.45;}\n",
  ".badge{display:inline-block;margin-left:8px;padding:2px 6px;border-radius:3px;",
  "font-size:10px;background:#444;color:#ddd;}\n"
)

lock_css <- if (isTRUE(unified)) {
  paste0(guest_lock_glyph, unified_css, pdf_mute)
} else {
  # Broken / pre-unify: ACL dim only; guest glyph without opacity
  paste0(acl_dim, guest_lock_glyph, pdf_mute)
}

guest_li_class <- if (isTRUE(unified)) "ynow-sidebar-locked" else ""
atx_li_class <- if (isTRUE(unified)) "ynow-access-locked-nav ynow-sidebar-locked" else "ynow-access-locked-nav"

page <- sprintf(
  paste0(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<title>Sidebar lock style %s</title>",
    "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css'/>",
    "<style>%s%s</style></head>",
    "<body class='ynow-auth-feature'><div class='skin-black'><aside class='main-sidebar'>",
    "<ul class='sidebar-menu'>",
    "<li><a data-value='home' href='#'><i class='fas fa-home'></i><span> Home</span></a></li>",
    "<li class='%s'><a data-value='asset_transmission' href='#'>",
    "<i class='fas fa-project-diagram'></i>",
    "<i class='fas fa-lock ynow-menu-lock ynow-access-lock' aria-hidden='true'></i>",
    "<span> Asset Transmission Root</span>",
    "<span class='badge'>ACL lock</span></a></li>",
    "<li class='%s'><a data-value='lab_notes' href='#'>",
    "<i class='fas fa-flask'></i>",
    "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i>",
    "<span> Quant Backtest Lab</span>",
    "<span class='badge'>guest lock</span></a></li>",
    "<li class='%s'><a data-value='decision_checklist' href='#'>",
    "<i class='fas fa-clipboard-check'></i>",
    "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i>",
    "<span> Decision Checklist</span>",
    "<span class='badge'>guest lock</span></a></li>",
    "<li class='%s'><a data-value='price_alerts' href='#'>",
    "<i class='fas fa-bell'></i>",
    "<i class='fas fa-lock ynow-menu-lock' aria-hidden='true'></i>",
    "<span> Value Alerts</span>",
    "<span class='badge'>guest lock</span></a></li>",
    "<li><a data-value='about' href='#'><i class='fas fa-info-circle'></i><span> About</span></a></li>",
    "</ul>",
    "<div class='ynow-sidebar-download-wrap'>",
    "<a id='download_report' class='ynow-sidebar-download-btn' href='#'>",
    "<i class='fas fa-lock ynow-sidebar-dl-lock' role='presentation'></i> Download Report (PDF)</a>",
    "</div></aside></div>",
    "<p class='note'><strong>%s</strong><br>%s</p></body></html>"
  ),
  if (unified) "AFTER" else "BEFORE",
  base_chrome,
  lock_css,
  atx_li_class,
  guest_li_class,
  guest_li_class,
  guest_li_class,
  if (unified) "AFTER unify" else "BEFORE unify (broken)",
  if (unified) {
    "ATX ACL lock and Full guest locks share the same dimmed locked chrome (opacity 0.55 + lock glyph). Download Report keeps its muted button styling."
  } else {
    "ATX (ACL) is dimmed via ynow-access-locked-nav. Lab / Decision Checklist / Value Alerts only swap the glyph — labels stay full-bright white."
  }
)

fname <- if (unified) "after-unified-locked-sidebar.html" else "before-divergent-locked-sidebar.html"
out_path <- file.path(out_dir, fname)
writeLines(page, out_path, useBytes = TRUE)

media_out <- Sys.getenv("YNOW_SIDEBAR_LOCK_MEDIA_DIR", unset = "")
if (nzchar(media_out)) {
  dir.create(media_out, showWarnings = FALSE, recursive = TRUE)
  file.copy(out_path, file.path(media_out, fname), overwrite = TRUE)
}

cat("unified_source=", unified, "\n", sep = "")
cat("wrote ", out_path, "\n", sep = "")
cat("fixture ok\n")
