#!/usr/bin/env Rscript
# Aggregate Phase 2–4 audit logs → Audit_RCM_Report.md (Risk and Control Matrix).
# Invoked by scripts/run_audit.sh; can also be run standalone with log paths.
#
# Usage:
#   Rscript scripts/generate_audit_rcm.R \
#     --log-dir <dir> \
#     --out Audit_RCM_Report.md \
#     [--suite name=path.log ...]

args <- commandArgs(trailingOnly = TRUE)
`%||%` <- function(a, b) if (is.null(a) || length(a) < 1L || !nzchar(a[1])) b else a[1]

log_dir <- "audit_logs"
out_path <- "Audit_RCM_Report.md"
suite_args <- character(0)
i <- 1L
while (i <= length(args)) {
  if (identical(args[[i]], "--log-dir") && i < length(args)) {
    log_dir <- args[[i + 1L]]; i <- i + 2L
  } else if (identical(args[[i]], "--out") && i < length(args)) {
    out_path <- args[[i + 1L]]; i <- i + 2L
  } else if (grepl("=", args[[i]], fixed = TRUE)) {
    suite_args <- c(suite_args, args[[i]]); i <- i + 1L
  } else {
    i <- i + 1L
  }
}

# Canonical RCM control catalog (Phase 2–4)
catalog <- data.frame(
  suite = c(
    "data_integrity", "data_integrity", "data_integrity", "data_integrity",
    "parameters_boundaries", "parameters_boundaries", "parameters_boundaries", "parameters_boundaries",
    "formula_accuracy", "formula_accuracy", "formula_accuracy", "formula_accuracy"
  ),
  audit_item = c(
    "Offline universe / snapshot CSV schema & non-blank keys",
    "Macro Buffett time-series continuity",
    "Mock Yahoo payload → normalize_all_financials / metric extract",
    "Mock Summary quote / TW finalize / metrics snapshot loader",
    "APP_DEFAULTS & market Rf/tax / CapEx spike / EQ / YNOW lock",
    "Near-term g ceiling (−5%…25%) & CapEx spike edges",
    "DCF WACC≤g / shares≤0 / neg terminal CF fail-soft",
    "P/B neg equity, MOS near-zero, PE nonpositive EPS, WACC resolve",
    "Closed-form WACC / CAPM / DDM / Gordon TV / MOS / SGR / FCFE",
    "Three scenarios (base/bull/bear) DCF per-share / RI / MOS",
    "F-Score 9-signal decomposition (excellent/mixed/poor)",
    "Beneish 8-component + M-Score hand calc (base/bull/bear)"
  ),
  target = c(
    "lab_*_universe CSV; universe_metrics_snapshot; cluster_features_snapshot",
    "data/macro_buffett_*.csv",
    "setup.R::normalize_all_financials / select_clean_metric_row / parse_financial_number",
    "extract_quote_price_mcap; lab_finalize_tw_list; lab_load_universe_metrics_snapshot",
    "default_config.R::APP_DEFAULTS; market_profile.R; .EQ_THRESHOLDS; ynow_index.R",
    "ynow_server.R::.clamp_near_term_g_pct; setup.R::ratio_spike_vs_prior / fcf_sync_*",
    "valuation_guard.R::value_dcf_per_share / validate_stage_years / compute_wacc",
    "derive_pb_targets; mos_vs_fair_value; calc_pe_implied_price; resolve_wacc_input",
    "setup.R / valuation_guard.R / .ddm_formula_* / dcf_gordon_tv / fundamental_sgr_identity",
    "value_dcf_per_share; ri_module.R::compute_ri_valuation; mos_vs_fair_value",
    "setup.R::compute_report_f_score",
    "earnings_quality_module.R::calculate_m_score / .eq_beneish_components"
  ),
  control_objective = c(
    "Completeness", "Completeness", "Accuracy", "Accuracy",
    "Accuracy", "Accuracy", "Accuracy", "Accuracy",
    "Accuracy", "Accuracy", "Accuracy", "Accuracy"
  ),
  stringsAsFactors = FALSE
)

default_logs <- c(
  data_integrity = file.path(log_dir, "test_data_integrity.log"),
  parameters_boundaries = file.path(log_dir, "test_parameters_boundaries.log"),
  formula_accuracy = file.path(log_dir, "test_formula_accuracy.log")
)
if (length(suite_args)) {
  for (sa in suite_args) {
    parts <- strsplit(sa, "=", fixed = TRUE)[[1]]
    if (length(parts) >= 2L) default_logs[[parts[[1]]]] <- paste(parts[-1], collapse = "=")
  }
}

parse_log <- function(path) {
  if (!file.exists(path)) {
    return(list(
      exists = FALSE, pass = 0L, fail = 0L, exit_ok = FALSE,
      fail_lines = character(0), pass_lines = character(0), raw = ""
    ))
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  pass_lines <- grep("^PASS\\s", lines, value = TRUE)
  fail_lines <- grep("^FAIL\\s", lines, value = TRUE)
  # Also accept "OK " from older scripts if mixed in
  ok_lines <- grep("^OK\\s", lines, value = TRUE)
  pass_n <- length(pass_lines) + length(ok_lines)
  fail_n <- length(fail_lines)
  # Summary line PASS=n FAIL=m
  sum_line <- grep("^PASS=.*FAIL=", lines, value = TRUE)
  if (length(sum_line)) {
    m <- regmatches(sum_line[[1]], regexec("PASS=([0-9]+)\\s+FAIL=([0-9]+)", sum_line[[1]]))[[1]]
    if (length(m) >= 3L) {
      pass_n <- as.integer(m[[2]])
      fail_n <- as.integer(m[[3]])
    }
  }
  all_pass_marker <- any(grepl("ALL CONTROLS PASSED", lines, fixed = TRUE))
  list(
    exists = TRUE,
    pass = pass_n,
    fail = fail_n,
    exit_ok = isTRUE(fail_n == 0L) && (all_pass_marker || pass_n > 0L),
    fail_lines = sub("^FAIL\\s+", "", fail_lines),
    pass_lines = sub("^(PASS|OK)\\s+", "", c(pass_lines, ok_lines)),
    raw = paste(lines, collapse = "\n")
  )
}

suite_meta <- list(
  data_integrity = list(
    title = "Phase 2 — Data Integrity",
    script = "app_23.0/tests/test_data_integrity.R"
  ),
  parameters_boundaries = list(
    title = "Phase 3 — Parameter & Boundary",
    script = "app_23.0/tests/test_parameters_boundaries.R"
  ),
  formula_accuracy = list(
    title = "Phase 4 — Formula Accuracy",
    script = "app_23.0/tests/test_formula_accuracy.R"
  )
)

results <- lapply(names(default_logs), function(nm) {
  p <- parse_log(default_logs[[nm]])
  p$name <- nm
  p$path <- default_logs[[nm]]
  p
})
names(results) <- names(default_logs)

ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z", tz = "UTC")
git_sha <- tryCatch(
  system("git rev-parse --short HEAD", intern = TRUE)[1],
  error = function(e) "unknown"
)
overall_fail <- any(vapply(results, function(r) !isTRUE(r$exit_ok), logical(1)))

esc <- function(x) {
  x <- as.character(x)
  x <- gsub("\\|", "\\\\|", x)
  x
}

lines_out <- c(
  "# Audit RCM Report — theYNowApp (`app_23.0`)",
  "",
  "**Format:** Risk and Control Matrix (RCM)",
  paste0("**Generated at (UTC):** ", ts),
  paste0("**Git:** `", git_sha, "`"),
  paste0("**Overall result:** ", if (overall_fail) "**FAIL**" else "**PASS**"),
  "",
  "Mapped from Phase 1 `Audit_Mapping.md` controls; executed by `scripts/run_audit.sh`.",
  "",
  "---",
  "",
  "## Suite summary",
  "",
  "| Suite | Script | PASS | FAIL | Result |",
  "|---|---|---:|---:|---|"
)

for (nm in names(results)) {
  r <- results[[nm]]
  meta <- suite_meta[[nm]]
  res_lbl <- if (!r$exists) "MISSING" else if (r$exit_ok) "Pass" else "Fail"
  lines_out <- c(
    lines_out,
    sprintf(
      "| %s | `%s` | %s | %s | %s |",
      esc(meta$title %||% nm),
      esc(meta$script %||% nm),
      if (r$exists) r$pass else "—",
      if (r$exists) r$fail else "—",
      res_lbl
    )
  )
}

lines_out <- c(
  lines_out,
  "",
  "---",
  "",
  "## Risk and Control Matrix",
  "",
  "| Audit Item | Target Function / Module | Control Objective | Test Result | Findings / Remediation |",
  "|---|---|---|---|---|"
)

for (i in seq_len(nrow(catalog))) {
  row <- catalog[i, ]
  r <- results[[row$suite]]
  if (is.null(r) || !r$exists) {
    result <- "Fail"
    finding <- "Log missing — re-run `scripts/run_audit.sh`."
  } else if (r$exit_ok) {
    result <- "Pass"
    finding <- "—"
  } else {
    result <- "Fail"
    fl <- r$fail_lines
    if (!length(fl)) {
      finding <- sprintf("Suite failed (%d FAIL). Inspect `%s`.", r$fail, r$path)
    } else {
      # Attach first few fails; suite-level row shares findings
      finding <- paste(paste0("- ", head(fl, 5L)), collapse = "<br>")
      if (length(fl) > 5L) {
        finding <- paste0(finding, sprintf("<br>… +%d more (see log)", length(fl) - 5L))
      }
    }
  }
  lines_out <- c(
    lines_out,
    sprintf(
      "| %s | `%s` | %s | %s | %s |",
      esc(row$audit_item),
      esc(row$target),
      esc(row$control_objective),
      result,
      finding
    )
  )
}

# Detailed findings section
fail_any <- FALSE
detail <- c("", "---", "", "## Detailed findings", "")
for (nm in names(results)) {
  r <- results[[nm]]
  if (is.null(r) || !r$exists) {
    fail_any <- TRUE
    detail <- c(detail, paste0("### ", suite_meta[[nm]]$title), "", "- Log file missing.", "")
    next
  }
  if (r$exit_ok) next
  fail_any <- TRUE
  detail <- c(
    detail,
    paste0("### ", suite_meta[[nm]]$title),
    "",
    paste0("Log: `", r$path, "`"),
    ""
  )
  if (length(r$fail_lines)) {
    detail <- c(detail, paste0("- ", r$fail_lines), "")
  } else {
    detail <- c(detail, "- Suite reported FAIL without enumerated FAIL lines.", "")
  }
  detail <- c(
    detail,
    "**Remediation:** Fix the failing control in the target module, re-run the suite,",
    "`YNOW_DEBUG_SKIP_PY=1 Rscript <script>`, then `scripts/run_audit.sh`.",
    ""
  )
}
if (!fail_any) {
  detail <- c(detail, "No FAIL items. All Phase 2–4 controls passed.", "")
}
lines_out <- c(lines_out, detail)

lines_out <- c(
  lines_out,
  "---",
  "",
  "## How to reproduce",
  "",
  "```bash",
  "bash scripts/run_audit.sh",
  "# or:",
  "cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_data_integrity.R",
  "cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_parameters_boundaries.R",
  "cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_formula_accuracy.R",
  "```",
  "",
  "## Scope notes",
  "",
  "- Offline / mock-driven; no live Yahoo or TPEx network required for Phase 2–4 suites.",
  "- Phase 1 inventory: `Audit_Mapping.md`.",
  "- This report is regenerated on each `run_audit.sh` invocation (do not hand-edit).",
  ""
)

out_dir <- dirname(out_path)
if (!identical(out_dir, ".") && !dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}
writeLines(lines_out, out_path, useBytes = FALSE)
cat("Wrote ", normalizePath(out_path, winslash = "/"), "\n", sep = "")
cat("Overall: ", if (overall_fail) "FAIL" else "PASS", "\n", sep = "")
quit(status = if (overall_fail) 1L else 0L)
