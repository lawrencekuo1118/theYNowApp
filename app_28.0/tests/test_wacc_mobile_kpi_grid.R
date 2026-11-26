# WACC tab summary KPI cards: phone ≤767px → 2×3 equal cells (no ellipsis labels).
# Desktop/tablet Bootstrap rows unchanged (col-sm-4 × 3 per row).
# Run:
#   cd app_28.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_wacc_mobile_kpi_grid.R

root <- if (file.exists("ynow_ui.R")) {
  getwd()
} else if (file.exists("../ynow_ui.R")) {
  normalizePath("..")
} else if (file.exists("app_28.0/ynow_ui.R")) {
  file.path(getwd(), "app_28.0")
} else {
  stop("Cannot locate app_28.0 root")
}
setwd(root)

check <- function(label, cond) {
  if (!isTRUE(cond)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  cat("OK:", label, "\n")
}

ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_src <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc_src <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
css_src <- paste(readLines("www/ynow_widgets.css", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
chrome_src <- paste(readLines("www/ynow_chrome.css", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "WACC KPI grid is a single fluidRow class on WACC tab",
  grepl('fluidRow\\(\\s*class\\s*=\\s*"ynow-wacc-kpi-grid ynow-model-kpi-row"', ui_src, perl = TRUE)
)

# Markup order: E, D, T then WACC, rd, re inside the grid (WACC shell tab, not body).
dcf_shell <- sub(
  "(?s).*\\.ynow_dcf_shell_ui\\s*<-\\s*function\\s*\\(\\s*\\)\\s*\\{",
  "",
  ui_src,
  perl = TRUE
)
dcf_shell <- sub("(?s)\\n\\.ynow_dcf_body_ui\\s*<-\\s*function.*", "", dcf_shell, perl = TRUE)
dcf_body <- sub(
  "(?s).*\\.ynow_dcf_body_ui\\s*<-\\s*function\\s*\\(\\s*\\)\\s*\\{",
  "",
  ui_src,
  perl = TRUE
)
dcf_body <- sub("(?s)\\nui <- dashboardPage.*", "", dcf_body, perl = TRUE)
check("WACC KPIs left the DCF body", !grepl("ynow-wacc-kpi-grid", dcf_body, fixed = TRUE))
check("WACC KPIs live in DCF shell", grepl("ynow-wacc-kpi-grid", dcf_shell, fixed = TRUE))

grid_pos <- regexpr('class = "ynow-wacc-kpi-grid ynow-model-kpi-row"', ui_src, fixed = TRUE)[1]
check("grid wrapper located", is.finite(grid_pos) && grid_pos > 0)
grid_chunk <- substr(ui_src, grid_pos, min(nchar(ui_src), grid_pos + 2200L))
pos_e <- regexpr('vbx_equity_val', grid_chunk, fixed = TRUE)[1]
pos_d <- regexpr('vbx_debt_val', grid_chunk, fixed = TRUE)[1]
pos_t <- regexpr('vbx_tax_rate', grid_chunk, fixed = TRUE)[1]
pos_w <- regexpr('ibx_wacc', grid_chunk, fixed = TRUE)[1]
pos_rd <- regexpr('ibx_rd', grid_chunk, fixed = TRUE)[1]
pos_re <- regexpr('ibx_re', grid_chunk, fixed = TRUE)[1]
check(
  "semantic order E → D → T → WACC → rd → re",
  all(c(pos_e, pos_d, pos_t, pos_w, pos_rd, pos_re) > 0) &&
    pos_e < pos_d && pos_d < pos_t && pos_t < pos_w && pos_w < pos_rd && pos_rd < pos_re
)

check(
  "WACC result uses outline tone host with bare uiOutput",
  grepl(
    'ynow-model-kpi-result ynow-model-kpi-outline"[\\s\\S]{0,160}`data-ynow-lock` = "output:wacc"[\\s\\S]{0,160}uiOutput\\("ibx_wacc"\\)',
    grid_chunk,
    perl = TRUE
  )
)
check(
  "Rd/Re use tone-2 (no neon lime host)",
  grepl("ynow-model-kpi-tone-2", grid_chunk, fixed = TRUE) &&
    grepl('infoBoxOutput("ibx_rd"', grid_chunk, fixed = TRUE) &&
    grepl('infoBoxOutput("ibx_re"', grid_chunk, fixed = TRUE)
)
# Follow-up to #819: square .info-box-icon must share the same tone fill as the
# rectangular value area (beat .content-wrapper .info-box.bg-* muted + AdminLTE overlay).
.tone_block <- function(n) {
  pat <- sprintf(
    "(?s)body\\.ynow-theme-model \\.content-wrapper \\.ynow-model-kpi-tone-%d \\.small-box,.*?background-image:\\s*none\\s*!important;",
    as.integer(n)
  )
  m <- regexpr(pat, chrome_src, perl = TRUE)
  if (m[1] < 0) return("")
  substr(chrome_src, m[1], m[1] + attr(m, "match.length") - 1L)
}
t2 <- .tone_block(2L)
t3 <- .tone_block(3L)
check(
  "tone-2 icon + box share --ynow-model-kpi-s2 (beats muted bg-*)",
  grepl(".content-wrapper .ynow-model-kpi-tone-2 .info-box.bg-aqua", t2, fixed = TRUE) &&
    grepl(".content-wrapper .ynow-model-kpi-tone-2 .info-box .info-box-icon", t2, fixed = TRUE) &&
    grepl("background-color: var(--ynow-model-kpi-s2)", t2, fixed = TRUE) &&
    grepl("background-image: none !important;", t2, fixed = TRUE)
)
check(
  "tone-3 icon + box share --ynow-model-kpi-s3 (SGR / tax)",
  grepl(".content-wrapper .ynow-model-kpi-tone-3 .info-box.bg-aqua", t3, fixed = TRUE) &&
    grepl(".content-wrapper .ynow-model-kpi-tone-3 .info-box .info-box-icon", t3, fixed = TRUE) &&
    grepl("background-color: var(--ynow-model-kpi-s3)", t3, fixed = TRUE) &&
    grepl("background-image: none !important;", t3, fixed = TRUE)
)
check(
  "ibx_rd render avoids AdminLTE lime",
  grepl(
    "output\\$ibx_rd\\s*<-\\s*renderInfoBox\\([\\s\\S]{0,500}?color\\s*=\\s*\"aqua\"",
    srv_src,
    perl = TRUE
  ) &&
    !grepl(
      "output\\$ibx_rd\\s*<-\\s*renderInfoBox\\([\\s\\S]{0,500}?color\\s*=\\s*\"lime\"",
      srv_src,
      perl = TRUE
    )
)

check(
  "ibx_wacc render uses width=NULL",
  grepl(
    "output\\$ibx_wacc\\s*<-\\s*renderInfoBox\\([\\s\\S]{0,500}?width\\s*=\\s*NULL",
    srv_src,
    perl = TRUE
  )
)

check(
  "mobile media query targets ynow-wacc-kpi-grid",
  grepl(
    "@media\\s*\\(max-width:\\s*767px\\)\\s*\\{[\\s\\S]*?\\.ynow-wacc-kpi-grid",
    css_src,
    perl = TRUE
  )
)
check(
  "mobile CSS 3 equal columns",
  grepl(
    "\\.ynow-wacc-kpi-grid[\\s\\S]{0,400}?grid-template-columns:\\s*repeat\\(3,\\s*minmax\\(0,\\s*1fr\\)\\)",
    css_src,
    perl = TRUE
  )
)
check(
  "mobile CSS equal row tracks sized to content",
  grepl(
    "\\.ynow-wacc-kpi-grid[\\s\\S]{0,500}?grid-auto-rows:\\s*minmax\\(min-content,\\s*1fr\\)",
    css_src,
    perl = TRUE
  )
)
check(
  "mobile CSS disables Bootstrap clearfix",
  grepl(
    "\\.row\\.ynow-wacc-kpi-grid::before[\\s\\S]{0,200}?content:\\s*none\\s*!important",
    css_src,
    perl = TRUE
  )
)
check(
  "mobile CSS labels wrap without ellipsis",
  grepl(
    "\\.ynow-wacc-kpi-grid \\.small-box \\.inner p[\\s\\S]{0,250}?white-space:\\s*normal\\s*!important[\\s\\S]{0,120}?text-overflow:\\s*unset",
    css_src,
    perl = TRUE
  ) &&
    grepl(
      "\\.ynow-wacc-kpi-grid \\.info-box \\.info-box-text[\\s\\S]{0,250}?white-space:\\s*normal\\s*!important[\\s\\S]{0,120}?text-overflow:\\s*unset",
      css_src,
      perl = TRUE
    )
)
check(
  "mobile CSS prefers overflow-wrap:break-word in WACC grid labels",
  grepl(
    "ynow-wacc-kpi-grid .small-box .inner p",
    css_src,
    fixed = TRUE
  ) &&
    grepl(
      "Prefer whole-word wrap; avoid mid-glyph cuts from overflow-wrap:anywhere",
      css_src,
      fixed = TRUE
    ) &&
    grepl("overflow-wrap: break-word", css_src, fixed = TRUE)
)

check(
  "tax KPI uses short locale subtitle key",
  grepl('ui_str\\("vbx_wacc_tax_subtitle"', srv_src, perl = TRUE) &&
    !grepl("WACC 用使用者／enacted T", srv_src, fixed = TRUE)
)
check(
  "en-US short tax subtitle",
  grepl('vbx_wacc_tax_subtitle = "Effective Tax Rate \\(T\\)"', loc_src, perl = TRUE)
)
check(
  "zh-TW short tax subtitle",
  grepl('vbx_wacc_tax_subtitle = "有效稅率 \\(T\\)"', loc_src, perl = TRUE)
)
check(
  "en + zh-TW equity/debt subtitle keys present",
  grepl('vbx_wacc_equity_subtitle = "Market Equity \\(E\\)"', loc_src, perl = TRUE) &&
    grepl('vbx_wacc_debt_subtitle = "Total Debt \\(D\\)"', loc_src, perl = TRUE) &&
    grepl('vbx_wacc_equity_subtitle = "股權市值 \\(E\\)"', loc_src, perl = TRUE) &&
    grepl('vbx_wacc_debt_subtitle = "總負債 \\(D\\)"', loc_src, perl = TRUE)
)

cat("\nAll WACC mobile KPI grid checks passed.\n")
