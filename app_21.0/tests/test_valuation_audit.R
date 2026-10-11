#!/usr/bin/env Rscript
# Terminal-growth identity, Gordon gate, and scenario shocks must be checkable.
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "setup.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "")
  else { cat("FAIL ", label, "\n", sep = ""); fail <<- fail + 1L }
}

# ASML FY2025: NI 9,609.4m, equity 19,612.2m, dividends -2,550.3m → g 35.99
id <- fundamental_sgr_identity(9609400000, 19612200000, -2550300000)
check("ASML fundamental g 35.99", isTRUE(id$ok) && abs(id$g_pct - 35.99) < 0.02)
check("ASML components rebuild g", isTRUE(id$matches_formula))
check("ASML ROE about 49", is.finite(id$roe_pct) && abs(id$roe_pct - 49.00) < 0.15)
check("ASML payout about 26.5", is.finite(id$payout) && abs(id$payout * 100 - 26.54) < 0.2)

blocked <- terminal_gordon_gate(35.99, 8.5)
check("g 35.99 vs WACC 8.5 blocked", isTRUE(!blocked$computable) && blocked$spread_pp < 0)
open <- terminal_gordon_gate(4.2, 8.5)
check("lifecycle g 4.2 vs 8.5 open", isTRUE(open$computable) && open$spread_pp > 0)
check("equal g is blocked", isTRUE(!terminal_gordon_gate(8.5, 8.5)$computable))

audit <- valuation_scenario_audit("en")
check("audit covers seven engines", setequal(unique(audit$model), c("dcf", "ddm", "ri", "pb", "nav", "multiples", "sotp")))
dcf_bear <- audit[audit$model == "dcf" & audit$case == "bear" & audit$parameter == "wacc_pp", , drop = FALSE]
check("DCF bear WACC +1pp", nrow(dcf_bear) == 1L && abs(dcf_bear$shock[[1]] - 1) < 1e-9)
ri_bull <- audit[audit$model == "ri" & audit$case == "bull" & audit$parameter == "roe_pp", , drop = FALSE]
check("RI bull ROE +2pp", nrow(ri_bull) == 1L && abs(ri_bull$shock[[1]] - 2) < 1e-9)
zh <- valuation_scenario_audit("zh-TW")
check("zh audit same shocks", identical(audit$shock, zh$shock) && !identical(audit$rule[[1]], zh$rule[[1]]))

px <- multiple_implied_price(12, 4, 2)
check("P/E identity 12 * 4 / 2 = 24", isTRUE(px$ok) && abs(px$price - 24) < 1e-9)
sotp <- sotp_implied_price(c(100, 50), cash = 20, debt = 50, shares = 10)
check("SOTP (150+20-50)/10 = 12", isTRUE(sotp$ok) && abs(sotp$price - 12) < 1e-9)

if (fail > 0L) quit(status = 1)
cat("ALL PASS\n")
