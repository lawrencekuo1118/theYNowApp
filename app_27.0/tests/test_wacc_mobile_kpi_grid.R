# WACC tab summary KPI cards: phone ≤767px → 2×3 equal cells (no ellipsis labels).
# Desktop/tablet Bootstrap rows unchanged (col-sm-4 × 3 per row).
# Run:
#   cd app_27.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_wacc_mobile_kpi_grid.R

root <- if (file.exists("ynow_ui.R")) {
  getwd()
} else if (file.exists("../ynow_ui.R")) {
  normalizePath("..")
} else if (file.exists("app_27.0/ynow_ui.R")) {
  file.path(getwd(), "app_27.0")
} else {
  stop("Cannot locate app_27.0 root")
}
setwd(root)

check <- function(label, cond) {
  if (!isTRUE(cond)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  cat("OK:", label, "\n")
}

ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_src <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "WACC KPI grid is a single fluidRow class",
  grepl('fluidRow\\(\\s*class\\s*=\\s*"ynow-wacc-kpi-grid"', ui_src, perl = TRUE)
)

# Markup order: E, D, T then WACC, rd, re inside the grid.
grid_pos <- regexpr('class = "ynow-wacc-kpi-grid"', ui_src, fixed = TRUE)[1]
check("grid wrapper located", is.finite(grid_pos) && grid_pos > 0)
grid_chunk <- substr(ui_src, grid_pos, min(nchar(ui_src), grid_pos + 1800L))
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
  "WACC lock on col-sm-4 host with bare uiOutput",
  grepl(
    'class = "col-sm-4"[\\s\\S]{0,160}`data-ynow-lock` = "output:wacc"[\\s\\S]{0,160}uiOutput\\("ibx_wacc"\\)',
    grid_chunk,
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
    ui_src,
    perl = TRUE
  )
)
check(
  "mobile CSS 3 equal columns",
  grepl(
    "\\.ynow-wacc-kpi-grid[\\s\\S]{0,400}?grid-template-columns:\\s*repeat\\(3,\\s*minmax\\(0,\\s*1fr\\)\\)",
    ui_src,
    perl = TRUE
  )
)
check(
  "mobile CSS equal row tracks",
  grepl(
    "\\.ynow-wacc-kpi-grid[\\s\\S]{0,400}?grid-auto-rows:\\s*1fr",
    ui_src,
    perl = TRUE
  )
)
check(
  "mobile CSS disables Bootstrap clearfix",
  grepl(
    "\\.row\\.ynow-wacc-kpi-grid::before[\\s\\S]{0,200}?content:\\s*none\\s*!important",
    ui_src,
    perl = TRUE
  )
)
check(
  "mobile CSS labels wrap without ellipsis",
  grepl(
    "\\.ynow-wacc-kpi-grid \\.small-box \\.inner p[\\s\\S]{0,250}?white-space:\\s*normal\\s*!important[\\s\\S]{0,120}?text-overflow:\\s*unset",
    ui_src,
    perl = TRUE
  ) &&
    grepl(
      "\\.ynow-wacc-kpi-grid \\.info-box \\.info-box-text[\\s\\S]{0,250}?white-space:\\s*normal\\s*!important[\\s\\S]{0,120}?text-overflow:\\s*unset",
      ui_src,
      perl = TRUE
    )
)

cat("\nAll WACC mobile KPI grid checks passed.\n")
