# Static HTML: Lite sidebar without Asset Transmission Root.
# Writes under /opt/cursor/artifacts/ and Project media/
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
suppressPackageStartupMessages({
  library(htmltools)
  library(shiny)
})

app_dir <- normalizePath("..", winslash = "/", mustWork = TRUE)
source(file.path(app_dir, "auth_account.R"), local = TRUE)

omit <- ynow_sidebar_lite_omit_tabs()
stopifnot("asset_transmission" %in% omit)
lite_css <- ynow_sidebar_lite_omit_css()

out_dirs <- c(
  "/opt/cursor/artifacts/lite-sidebar-no-atx",
  "/cursor/stores/bc-ee52ce88-0754-4bd3-9466-84bb18df73aa/media"
)
for (d in out_dirs) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Menu includes ATX in DOM (as live app does) — Lite CSS must hide it.
html <- sprintf(
  paste0(
    "<!DOCTYPE html><html><head><meta charset='utf-8'><title>Lite sidebar — no ATX</title>",
    "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css'/>",
    "<style>",
    "body{margin:0;font-family:Helvetica,Arial,sans-serif;background:#f4f4f4;}",
    ".skin-black .main-sidebar{width:260px;min-height:100vh;background:#1a1a1a;color:#fff;padding:12px 0;}",
    ".sidebar-menu{list-style:none;margin:0;padding:8px 0;}",
    ".sidebar-menu>li>a{display:flex;align-items:center;gap:6px;padding:10px 14px;",
    "color:rgba(255,255,255,0.92);text-decoration:none;font-size:13px;}",
    ".badge{display:inline-block;margin:12px 16px;padding:4px 10px;border-radius:4px;",
    "background:#249C60;color:#fff;font-size:12px;font-weight:bold;}",
    ".note{margin:16px;max-width:560px;font-size:13px;color:#333;line-height:1.45;}",
    "%s",
    "</style></head>",
    "<body class='ynow-auth-feature ynow-lite'><div class='skin-black'><aside class='main-sidebar'>",
    "<span class='badge'>LITE</span>",
    "<ul class='sidebar-menu'>",
    "<li><a data-value='home' href='#'><i class='fas fa-home'></i><span> Home</span></a></li>",
    "<li><a data-value='macro_market' href='#'><i class='fas fa-globe-asia'></i><span> Macro & Market Trends</span></a></li>",
    "<li><a data-value='asset_transmission' href='#'><i class='fas fa-project-diagram'></i><span> Asset Transmission Root</span></a></li>",
    "<li><a data-value='bluechip' href='#'><i class='fas fa-star'></i><span> Blue Chip Leaderboard</span></a></li>",
    "<li><a data-value='dashboard' href='#'><i class='fas fa-chart-line'></i><span> Company</span></a></li>",
    "<li><a data-value='sensitivity' href='#'><i class='fas fa-clock'></i><span> YNOW</span></a></li>",
    "<li><a data-value='smart_analysis' href='#'><i class='fas fa-magic'></i><span> Smart Analysis</span></a></li>",
    "<li><a data-value='about' href='#'><i class='fas fa-info-circle'></i><span> About</span></a></li>",
    "</ul></aside></div>",
    "<p class='note'>Lite omits <code>asset_transmission</code> (and other Full-only / guest-locked tabs). ",
    "Visible: Home · Macro · Blue Chip · Company · YNOW · Smart Analysis · About.</p>",
    "</body></html>"
  ),
  lite_css
)

for (d in out_dirs) {
  writeLines(html, file.path(d, "lite-sidebar-no-atx.html"), useBytes = TRUE)
}
cat("wrote lite-sidebar-no-atx.html; omit has ATX=", "asset_transmission" %in% omit, "\n")
