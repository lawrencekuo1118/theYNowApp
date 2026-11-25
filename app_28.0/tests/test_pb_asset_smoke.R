#!/usr/bin/env Rscript
# Offline smoke: Book/Asset P/B hot path (formula + justified targets + module wiring).
# Asserts current outputs — does not change valuation formulas.
# Run: cd app_28.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_pb_asset_smoke.R

local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  cands <- character()
  if (length(f) == 1L && nzchar(f[[1]])) {
    cands <- c(
      cands,
      file.path(dirname(normalizePath(f[[1]], mustWork = FALSE)), "ynow_test_app_dir.R")
    )
  }
  wd <- normalizePath(getwd(), mustWork = FALSE)
  cands <- c(
    cands,
    file.path(wd, "ynow_test_app_dir.R"),
    file.path(wd, "tests", "ynow_test_app_dir.R"),
    file.path(wd, "app_28.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("setup.R")
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

source("setup.R", local = FALSE)
source("pb_asset_module.R", local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}
approx_eq <- function(a, b, tol = 1e-8) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

check("pb_asset_module_server exported", exists("pb_asset_module_server", mode = "function"))
check("pb_asset_module_ui exported", exists("pb_asset_module_ui", mode = "function"))
check(".pb_formula_p exported", exists(".pb_formula_p", mode = "function"))
check("derive_pb_targets exported", exists("derive_pb_targets", mode = "function"))

# Fair Value = basis × target P/B
check("P/B linear BVPS×1.5", approx_eq(.pb_formula_p(12, 1.5), 18))
check("P/B TBVPS path", approx_eq(.pb_formula_p(10, 2.0), 20))
check("P/B NA when basis<=0", !is.finite(.pb_formula_p(0, 1.5)) && !is.finite(.pb_formula_p(-2, 1.5)))
check("P/B NA when pb<=0", !is.finite(.pb_formula_p(12, 0)) && !is.finite(.pb_formula_p(12, -1)))

# Book metric parts → per-share (settings sections)
check(".pb_bvps_from_parts exported", exists(".pb_bvps_from_parts", mode = "function"))
check(".pb_tbvps_from_parts exported", exists(".pb_tbvps_from_parts", mode = "function"))
check(".pb_navps_from_parts exported", exists(".pb_navps_from_parts", mode = "function"))
check("BVPS = equity÷shares", approx_eq(.pb_bvps_from_parts(1200, 100), 12))
check("BVPS NA when shares<=0", !is.finite(.pb_bvps_from_parts(100, 0)))
check(
  "TBVPS deducts goodwill+intangibles",
  approx_eq(.pb_tbvps_from_parts(1200, goodwill = 100, intangibles = 100, shares = 100), 10)
)
check(
  "TBVPS floors at 0",
  approx_eq(.pb_tbvps_from_parts(50, goodwill = 80, intangibles = 0, shares = 10), 0)
)
check(
  "NAVPS with holdco discount",
  approx_eq(.pb_navps_from_parts(1000, investments = 200, holdco_discount = 0.20, shares = 100), 9.6)
)
check(
  "NAVPS clamps discount to 50%",
  approx_eq(.pb_navps_from_parts(1000, investments = 200, holdco_discount = 0.90, shares = 100), 9)
)
check(
  "Fair Price BVPS×P/B from parts",
  approx_eq(.pb_formula_p(.pb_bvps_from_parts(1200, 100), 1.5), 18)
)

# Justified P/B = (ROE − g) / (Ke − g) with pct inputs
just_closed <- (0.15 - 0.03) / (0.10 - 0.03)
tgt_j <- derive_pb_targets(
  roe_pct = 15, ke_pct = 10, g_pct = 3,
  include_justified = TRUE, include_industry = FALSE, include_history = FALSE
)
check("justified P/B closed form", approx_eq(tgt_j$justified, just_closed, 1e-8))
check("justified usable mid", is.finite(tgt_j$mid) && approx_eq(tgt_j$mid, just_closed, 1e-6))

# Fair price from justified multiple on BVPS (module Overview path)
fair <- .pb_formula_p(basis = 25, pb = tgt_j$justified)
check("BVPS×justified fair", approx_eq(fair, 25 * just_closed, 1e-8))

tgt_m <- derive_pb_targets(
  roe_pct = 15, ke_pct = 10, g_pct = 3,
  industry_band = list(low = 1.0, mid = 1.4, high = 1.8),
  include_justified = FALSE, include_industry = TRUE, include_history = FALSE
)
check("multiples mode skips justified", !is.finite(tgt_m$justified) && approx_eq(tgt_m$mid, 1.4, 1e-6))

tgt_ke_g <- derive_pb_targets(
  roe_pct = 15, ke_pct = 3, g_pct = 5,
  include_justified = TRUE, include_industry = FALSE, include_history = FALSE
)
check("justified NA when Ke<=g", !is.finite(tgt_ke_g$justified))

tgt_neg_eq <- derive_pb_targets(
  roe_pct = 15, ke_pct = 10, g_pct = 3,
  book_equity = -100,
  include_justified = TRUE, include_industry = FALSE, include_history = FALSE
)
check("neg equity sets pb_blocked", isTRUE(tgt_neg_eq$pb_blocked))
check("neg equity mid is NA", !is.finite(tgt_neg_eq$mid))

src <- paste(readLines("pb_asset_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("module uses .pb_formula_p", grepl(".pb_formula_p(", src, fixed = TRUE))
check("module uses derive_pb_targets", grepl("derive_pb_targets(", src, fixed = TRUE) ||
  grepl("pb_targets_derived", src, fixed = TRUE))
check("module has justified target mode", grepl('"justified"', src, fixed = TRUE))
check("module tab is pb_calculator", grepl('"pb_calculator"', src, fixed = TRUE))

# Overview is a left sub-tab under the Settings shell tabBox (not a body sibling).
shell <- sub(
  "(?s)^.*pb_asset_module_shell_ui\\s*<-\\s*function.*?\\{(.*)\\}\\s*pb_asset_module_body_ui\\s*<-\\s*function.*$",
  "\\1",
  src,
  perl = TRUE
)
body <- sub(
  "(?s)^.*pb_asset_module_body_ui\\s*<-\\s*function.*?\\{(.*)\\}\\s*pb_asset_module_ui\\s*<-\\s*function.*$",
  "\\1",
  src,
  perl = TRUE
)
check(
  "Overview tab lives in shell",
  grepl('"P/B Overview"', shell, fixed = TRUE) &&
    grepl('value = "pb_overview"', shell, fixed = TRUE)
)
check(
  "Settings + Target tabs stay in shell",
  grepl('"P/B Settings"', shell, fixed = TRUE) &&
    grepl('value = "pb_settings"', shell, fixed = TRUE) &&
    grepl('value = "target_pb"', shell, fixed = TRUE)
)
check(
  "Settings uses DDM-style radio for book metric",
  grepl("radioButtons(", shell, fixed = TRUE) &&
    grepl('ns("basis")', shell, fixed = TRUE) &&
    grepl("pb_basis_label", shell, fixed = TRUE)
)
check(
  "Settings shows one module via conditionalPanel",
  grepl("conditionalPanel(", shell, fixed = TRUE) &&
    grepl("== 'bvps'", shell, fixed = TRUE) &&
    grepl("== 'tbvps'", shell, fixed = TRUE) &&
    grepl("== 'navps'", shell, fixed = TRUE)
)
check(
  "Settings has three BVPS/TBVPS/NAVPS modules",
  grepl('data-ynow-pb-section` = "bvps"', shell, fixed = TRUE) &&
    grepl('data-ynow-pb-section` = "tbvps"', shell, fixed = TRUE) &&
    grepl('data-ynow-pb-section` = "navps"', shell, fixed = TRUE)
)
check(
  "Settings wires per-mode component inputs",
  grepl('ns("common_equity")', shell, fixed = TRUE) &&
    grepl('ns("shares")', shell, fixed = TRUE) &&
    grepl('ns("tb_common_equity")', shell, fixed = TRUE) &&
    grepl('ns("tb_shares")', shell, fixed = TRUE) &&
    grepl('ns("nav_common_equity")', shell, fixed = TRUE) &&
    grepl('ns("nav_shares")', shell, fixed = TRUE) &&
    grepl('ns("goodwill")', shell, fixed = TRUE) &&
    grepl('ns("intangibles")', shell, fixed = TRUE) &&
    grepl('ns("investments")', shell, fixed = TRUE) &&
    grepl('ns("holdco_discount")', shell, fixed = TRUE) &&
    grepl('ns("bvps")', shell, fixed = TRUE) &&
    grepl('ns("tbvps")', shell, fixed = TRUE) &&
    grepl('ns("navps")', shell, fixed = TRUE)
)
# Module markup order: BVPS → TBVPS → NAVPS (visibility gated by radio)
bv_pos <- regexpr('data-ynow-pb-section` = "bvps"', shell, fixed = TRUE)[1]
tb_pos <- regexpr('data-ynow-pb-section` = "tbvps"', shell, fixed = TRUE)[1]
nv_pos <- regexpr('data-ynow-pb-section` = "navps"', shell, fixed = TRUE)[1]
radio_pos <- regexpr("radioButtons(", shell, fixed = TRUE)[1]
check("Radio precedes BVPS module", radio_pos > 0 && bv_pos > radio_pos)
check("BVPS module precedes TBVPS", bv_pos > 0 && tb_pos > bv_pos)
check("TBVPS module precedes NAVPS", tb_pos > 0 && nv_pos > tb_pos)

# Bugbot #804: missing alert + recompute must not cross modes
server_src <- sub(
  "(?s)^.*pb_asset_module_server\\s*<-\\s*function.*?\\{(.*)\\}\\s*$",
  "\\1",
  src,
  perl = TRUE
)
check(
  "Missing alert scopes to selected basis",
  grepl("switch(", server_src, fixed = TRUE) &&
    grepl('list("TBVPS" = input$tbvps)', server_src, fixed = TRUE) &&
    grepl('list("NAVPS" = input$navps)', server_src, fixed = TRUE) &&
    grepl('list("BVPS" = input$bvps)', server_src, fixed = TRUE) &&
    !grepl(
      'check_list = list("BVPS" = input$bvps, "TBVPS" = input$tbvps, "NAVPS" = input$navps)',
      server_src,
      fixed = TRUE
    )
)
check(
  "Recompute is mode-scoped (no all-three rewrite)",
  grepl('.pb_recompute_from_parts("bvps")', server_src, fixed = TRUE) &&
    grepl('.pb_recompute_from_parts("tbvps")', server_src, fixed = TRUE) &&
    grepl('.pb_recompute_from_parts("navps")', server_src, fixed = TRUE) &&
    !grepl('.pb_recompute_from_parts()', server_src, fixed = TRUE)
)
check(
  "Overview not a separate body tabBox",
  !grepl('"P/B Overview"', body, fixed = TRUE) &&
    !grepl('valueBoxOutput(ns("vbx_fair")', body, fixed = TRUE)
)
check(
  "Overview precedes Settings in shell",
  regexpr('"P/B Overview"', shell, fixed = TRUE)[1] <
    regexpr('"P/B Settings"', shell, fixed = TRUE)[1]
)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All P/B asset smoke checks passed.\n")
