#!/usr/bin/env Rscript
# Snapshot must surface assumed / engine-default params (no fully hidden substitutes).
# Run: cd app_26.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_snapshot_assumption_surface.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

source(file.path(app_dir, "param_audit.R"), encoding = "UTF-8")
source(file.path(app_dir, "industry_standards.R"), encoding = "UTF-8")
source(file.path(app_dir, "default_config.R"), encoding = "UTF-8")
if (file.exists(file.path(app_dir, "backtest_validation.R"))) {
  source(file.path(app_dir, "backtest_validation.R"), encoding = "UTF-8")
}

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
    pass <<- pass + 1L
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

reg <- ynow_tracked_param_registry()
rel_ids <- c(
  "mod_rel-rel_mode", "mod_rel-pe_multiple", "mod_rel-fwd_pe_multiple",
  "mod_rel-ps_multiple", "mod_rel-ev_fcf_multiple", "mod_rel-ev_ebit_multiple",
  "mod_rel-ev_ebitda_multiple", "mod_rel-ev_sales_multiple", "mod_rel-ev_arr_multiple"
)
sotp_ids <- c("mod_sotp-default_ev_sales", "mod_sotp-nonop")
check("registry tracks Relative Multiples inputs", all(rel_ids %in% reg$input_id))
check("registry tracks SOTP default EV/Sales + non-op", all(sotp_ids %in% reg$input_id))
check("relative multiples are Full-only (not Lite)", {
  sub <- reg[reg$input_id %in% rel_ids, , drop = FALSE]
  nrow(sub) == length(rel_ids) && all(sub$full_only)
})

# APP_DEFAULTS seeds that feed Implied Price must exist for Defaults Snapshot.
def_keys <- c(
  "rel_pe_multiple", "rel_fwd_pe_multiple", "rel_ev_fcf_multiple",
  "rel_ev_ebit_multiple", "rel_ev_ebitda_multiple", "rel_ev_sales_multiple",
  "rel_ps_multiple", "rel_ev_arr_multiple", "capex_spike_prior_years", "g_stage2"
)
check("APP_DEFAULTS holds relative + CapEx prior + g_stage2", all(def_keys %in% names(APP_DEFAULTS)))

# HFV fixed thresholds must be readable for Snapshot extras.
thr <- hfv_scenario_threshold_defaults()
check("HFV thresholds list complete", {
  all(c(
    "momentum_flat_band", "price_strong_up", "mos_near",
    "mos_deep_cheap", "mos_deep_rich"
  ) %in% names(thr)) &&
    is.finite(thr$momentum_flat_band) &&
    is.finite(thr$mos_near)
})

# Snapshot page hosts scenario audit + current table (string presence).
ui_txt <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE), collapse = "\n")
srv_txt <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE), collapse = "\n")
check("UI Snapshot hosts txt_scenario_audit", grepl("txt_scenario_audit", ui_txt, fixed = TRUE))
check("Server Snapshot extras include HFV thresholds", {
  grepl("HFV\", \"Scenario", srv_txt, fixed = TRUE) ||
    grepl("Scenario |Δ|/prev flat band", srv_txt, fixed = TRUE) ||
    grepl("Scenario Price", srv_txt, fixed = TRUE)
})
check("Server Snapshot extras include CapEx prior years", {
  grepl("CapEx spike prior years", srv_txt, fixed = TRUE)
})
check("Server Snapshot extras include Stage-2 g2 sync note", {
  grepl("Stage-2 g2", srv_txt, fixed = TRUE)
})
check("Server Snapshot extras include statutory tax fallback", {
  grepl("Statutory tax fallback", srv_txt, fixed = TRUE)
})
check("Server Snapshot extras document SOTP conglomerate discount not applied", {
  grepl("Conglomerate discount", srv_txt, fixed = TRUE) &&
    grepl("not applied", srv_txt, fixed = TRUE)
})

# Mock live Snapshot rows include Multiples when inputs present.
inp <- as.environment(list(
  `mod_rel-pe_multiple` = 18,
  `mod_rel-ev_fcf_multiple` = 15,
  `mod_sotp-default_ev_sales` = 3
))
class(inp) <- c("reactivevalues", "environment")
# ynow_snapshot_registry_rows expects input[[id]] — use a simple list-like.
inp_obj <- lapply(setNames(as.list(rep(NA, nrow(reg))), reg$input_id), identity)
inp_obj[["mod_rel-pe_multiple"]] <- 18
inp_obj[["mod_rel-ev_fcf_multiple"]] <- 15
inp_obj[["mod_sotp-default_ev_sales"]] <- 3
snap <- ynow_snapshot_registry_rows(inp_obj, lite = FALSE)
check("live Snapshot rows include Multiples section", {
  any(snap$Section == "Multiples" & snap$Parameter == "Selected P/E")
})
check("live Snapshot rows include SOTP default EV/Sales", {
  any(snap$Section == "SOTP" & grepl("EV/Sales", snap$Parameter, fixed = TRUE))
})

cat("\n=== Summary ===\nPASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASSED\n")
