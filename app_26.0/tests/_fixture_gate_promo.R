#!/usr/bin/env Rscript
# Build static HTML fixtures for per-feature Sign-in gate promo + demo screenshots.
suppressPackageStartupMessages({
  library(shiny)
  library(htmltools)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]))
} else {
  normalizePath("tests/_fixture_gate_promo.R")
}
app_dir <- normalizePath(file.path(dirname(script_path), ".."))
setwd(app_dir)

source("ui_locale.R", encoding = "UTF-8")
ynow_auth_enabled <- function() TRUE
suppressWarnings(source("auth_account.R", encoding = "UTF-8", local = FALSE))

fixture_css <- paste(
  ":root{--ynow-gate-bg:#f5f5f5;--ynow-gate-border:#d0d0d0;--ynow-gate-accent:#1a1a1a;--ynow-gate-ink:#1a1a1a;--ynow-gate-ink-soft:#333333;--ynow-logo-blue:#0C5484;--ynow-logo-green:#249C60;}",
  "body{font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",Helvetica,Arial,sans-serif;margin:0;background:#ececec;color:#333;}",
  ".content{padding:16px;}",
  ".ynow-hfv-chapter{margin:0 0 18px 0;padding:0;background:#fff;border:1px solid #e6e6e6;border-radius:8px;overflow:hidden;}",
  ".ynow-hfv-chapter__head{display:flex;flex-wrap:wrap;align-items:baseline;gap:6px 12px;padding:12px 16px;background:#fff;border-bottom:1px solid #ececec;}",
  ".ynow-hfv-chapter__kicker{font-size:11px;font-weight:700;letter-spacing:.06em;text-transform:uppercase;color:#888;}",
  ".ynow-hfv-chapter__title{margin:0;font-size:16px;font-weight:700;color:#1a1a1a;line-height:1.35;}",
  ".ynow-hfv-chapter__body{padding:12px 16px 16px 16px;}",
  ".btn{display:inline-flex;align-items:center;justify-content:center;padding:8px 14px;border-radius:3px;cursor:pointer;font-size:14px;}",
  # Shiny actionButton emits btn-default + btn-primary together — beat default.
  ".ynow-auth-pair--gate>.btn.ynow-auth-open,.btn.btn-default.btn-primary.ynow-auth-open{background:var(--ynow-logo-blue)!important;border:1px solid #0a446c!important;color:#fff!important;}",
  ".ynow-auth-pair--gate>.btn.ynow-auth-register-open,.btn.ynow-auth-register-open{background:#fff!important;border:1px solid var(--ynow-logo-green)!important;color:var(--ynow-logo-green)!important;}",
  ".ynow-auth-pair--gate{display:flex;gap:8px;margin-top:12px;max-width:420px;}",
  ".ynow-auth-pair--gate>.btn{flex:1 1 0;height:40px;}",
  ".ynow-gate-extras{margin:12px 0 16px 0;}",
  ".ynow-gate-promo{margin:0 0 10px 0;border:1px solid #e6e6e6;border-radius:8px;overflow:hidden;background:#fff;}",
  ".ynow-gate-promo__banner{display:flex;align-items:center;gap:10px;padding:12px 16px;background:#fff;color:#1a1a1a;border-bottom:1px solid #ececec;}",
  ".ynow-gate-promo__badge{display:inline-block;padding:2px 8px;border:1px solid #d0d0d0;border-radius:2px;font-size:11px;font-weight:700;letter-spacing:.08em;color:#888;}",
  ".ynow-gate-promo-kicker{font-size:13px;font-weight:600;color:#1a1a1a;}",
  ".ynow-gate-promo-lead{margin:10px 12px 6px 12px;font-size:13px;line-height:1.45;color:var(--ynow-gate-ink-soft);}",
  ".ynow-gate-promo__list{margin:0 12px 12px 12px;padding:0 0 0 18px;font-size:12.5px;line-height:1.45;color:var(--ynow-gate-ink-soft);}",
  ".ynow-gate-demo{margin:0 0 10px 0;padding:8px;border:1px solid var(--ynow-gate-border);border-radius:4px;background:#fff;}",
  ".ynow-gate-demo-caption{margin:0 0 6px 0;font-size:12px;font-weight:600;}",
  ".ynow-gate-demo-video{display:block;width:100%;max-height:220px;background:#1a1a1a;border:1px solid #000;}",
  ".ynow-gate-faq{margin:0;border:1px solid var(--ynow-gate-border);border-left:4px solid var(--ynow-gate-accent);border-radius:4px;background:#fff;}",
  ".ynow-gate-faq>.ynow-gate-faq__summary{list-style:none;cursor:pointer;padding:10px 12px;font-size:13px;font-weight:600;}",
  ".ynow-gate-faq>.ynow-gate-faq__summary::-webkit-details-marker{display:none;}",
  ".ynow-gate-faq>.ynow-gate-faq__summary::before{content:\"+\";display:inline-block;width:1.1em;margin-right:6px;font-weight:700;}",
  ".ynow-gate-faq[open]>.ynow-gate-faq__summary::before{content:\"–\";}",
  ".ynow-gate-faq__body{padding:0 12px 10px 12px;border-top:1px solid var(--ynow-gate-border);}",
  ".ynow-gate-faq-item{margin:10px 0 0 0;}",
  ".ynow-gate-faq-item>.ynow-gate-faq-q{margin:0 0 2px 0;font-size:12.5px;font-weight:600;}",
  ".ynow-gate-faq-item>.ynow-gate-faq-a{margin:0;font-size:12.5px;line-height:1.45;color:var(--ynow-gate-ink-soft);}",
  ".ynow-hfv-chapter.ynow-gate-box,.ynow-hfv-chapter.ynow-member-gate{border:1px solid #e6e6e6!important;background:#fff!important;}",
  "@media (max-width:767px){.ynow-member-gate,.ynow-gate-box{max-width:100%!important;margin-left:8px!important;margin-right:8px!important;}}",
  sep = "\n"
)

page <- function(title, body) {
  paste0(
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<title>", title, "</title><style>\n", fixture_css, "\n</style></head>",
    "<body><div class=\"content\">", body, "</div></body></html>"
  )
}

rewrite_demo_src <- function(html) {
  # Point video sources at absolute file paths for headless chrome file:// loads.
  gsub(
    'src="demos/',
    paste0('src="file://', app_dir, '/www/demos/'),
    html,
    fixed = TRUE
  )
}

open_faq <- function(html) {
  gsub(
    '<details class="ynow-gate-faq">',
    '<details class="ynow-gate-faq" open>',
    html,
    fixed = TRUE
  )
}

out_dir <- "/opt/cursor/artifacts/gate-fixtures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

features <- c("lab", "checklist", "alerts", "account", "pdf")
for (feat in features) {
  gate <- ynow_member_gate_ui(
    paste0("g_", feat), paste0("t_", feat), paste0("b_", feat),
    paste0("btn_", feat), feature = feat
  )
  gate$attribs$style <- "display:block; max-width:760px; margin:18px auto;"
  html <- open_faq(rewrite_demo_src(as.character(gate)))
  writeLines(page(paste("Gate", feat, "EN"), html),
             file.path(out_dir, paste0("gate-", feat, "-en.html")), useBytes = TRUE)
}

# zh-TW lab example via locale overlay on built HTML
lab_zh_keys <- ynow_gate_promo_keys("lab")
# Rebuild lab gate then swap text via ui_str zh — simpler: build from helper after temporarily
# monkey-patching .ynow_gate_str is hard; write manual zh page for lab + checklist.
esc <- function(x) htmltools::htmlEscape(as.character(x %||% "")[1])
loc <- function(key) ui_str(key, "zh-TW")
zh_gate <- function(feature) {
  keys <- ynow_gate_promo_keys(feature)
  sprintf(
    paste0(
      '<section class="ynow-hfv-chapter ynow-member-gate ynow-gate-box" style="display:block;max-width:760px;margin:18px auto;">',
      '<div class="ynow-hfv-chapter__head">',
      '<span class="ynow-hfv-chapter__kicker ynow-gate-section-kicker">%s</span>',
      '<h3 class="ynow-hfv-chapter__title"><span>%s</span></h3></div>',
      '<div class="ynow-hfv-chapter__body"><p class="ynow-gate-body">%s</p>',
      '<div class="ynow-gate-extras" data-ynow-gate-feature="%s">',
      '<div class="ynow-gate-promo"><div class="ynow-gate-promo__banner">',
      '<span class="ynow-gate-promo__badge">APP</span>',
      '<span class="ynow-gate-promo-kicker">%s</span></div>',
      '<p class="ynow-gate-promo-lead">%s</p>',
      '<ul class="ynow-gate-promo__list">',
      '<li><span class="ynow-gate-promo-b1">%s</span></li>',
      '<li><span class="ynow-gate-promo-b2">%s</span></li>',
      '<li><span class="ynow-gate-promo-b3">%s</span></li></ul></div>',
      '<div class="ynow-gate-demo"><p class="ynow-gate-demo-caption">%s</p>',
      '<video class="ynow-gate-demo-video" controls muted playsinline preload="none" loop>',
      '<source src="file://%s/www/demos/%s.webm" type="video/webm">',
      '<source src="file://%s/www/demos/%s.mp4" type="video/mp4"></video></div>',
      '<details class="ynow-gate-faq" open><summary class="ynow-gate-faq__summary">',
      '<span class="ynow-gate-faq-title">%s</span></summary>',
      '<div class="ynow-gate-faq__body">',
      '<div class="ynow-gate-faq-item"><p class="ynow-gate-faq-q">%s</p><p class="ynow-gate-faq-a">%s</p></div>',
      '<div class="ynow-gate-faq-item"><p class="ynow-gate-faq-q">%s</p><p class="ynow-gate-faq-a">%s</p></div>',
      '</div></details></div>',
      '<div class="ynow-auth-pair ynow-auth-pair--gate">',
      '<button type="button" class="btn btn-primary ynow-auth-open">%s</button>',
      '<button type="button" class="btn btn-default ynow-auth-register-open">%s</button>',
      '</div></div></section>'
    ),
    esc(loc("auth_gate_section_kicker")),
    esc(loc("auth_pa_gate_title")),
    esc(loc(keys$body)),
    feature,
    esc(loc(keys$kicker)),
    esc(loc(keys$lead)),
    esc(loc(keys$b1)),
    esc(loc(keys$b2)),
    esc(loc(keys$b3)),
    esc(loc(keys$demo_caption)),
    app_dir, feature, app_dir, feature,
    esc(loc("auth_gate_faq_title")),
    esc(loc("auth_gate_faq_q1")), esc(loc("auth_gate_faq_a1")),
    esc(loc("auth_gate_faq_q2")), esc(loc("auth_gate_faq_a2")),
    esc(loc("auth_sign_in")),
    esc(loc("auth_register_btn"))
  )
}
writeLines(page("Gate lab ZH", zh_gate("lab")),
           file.path(out_dir, "gate-lab-zh.html"), useBytes = TRUE)
writeLines(page("Gate checklist ZH", zh_gate("checklist")),
           file.path(out_dir, "gate-checklist-zh.html"), useBytes = TRUE)

cat("wrote fixtures to", out_dir, "\n")
stopifnot(file.exists(file.path(out_dir, "gate-lab-en.html")))
cat("fixture ok\n")
