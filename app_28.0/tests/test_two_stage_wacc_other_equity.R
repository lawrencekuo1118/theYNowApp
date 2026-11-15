#!/usr/bin/env Rscript
# YN-88: two-stage WACC auto-apply + other equity claims treatable as 0.
args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
app <- dirname(dirname(normalizePath(script)))
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source(file.path(app, "setup.R"))
source(file.path(app, "ui_locale.R"))
source(file.path(app, "industry_standards.R"))
source(file.path(app, "parameter_completeness.R"))
source(file.path(app, "default_config.R"))

check <- function(label, cond) {
  if (!isTRUE(cond)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  cat(sprintf("PASS: %s\n", label))
}

# --- Other equity claims: book==0 with no contradicting gap → 0 (not NA) ---
bs_no_lines <- data.frame(
  Breakdown = c("Total Assets", "Total Liabilities", "Cash And Cash Equivalents", "Total Debt"),
  `2025` = c(1000, 400, 100, 200), check.names = FALSE
)
check("book proxy is 0 when pref/NCI lines absent",
      identical(statement_other_equity_claims_book(bs_no_lines), 0))
check("no disclosed claims and no equity gap → market claims 0",
      identical(statement_other_equity_claims(bs_no_lines), 0))

bs_match <- data.frame(
  Breakdown = c("Common Stock Equity", "Total Equity Gross Minority Interest"),
  `2025` = c(500, 500), check.names = FALSE
)
check("common==gross still verifies as 0",
      identical(statement_other_equity_claims(bs_match), 0))

bs_gap_no_nci_line <- data.frame(
  Breakdown = c("Common Stock Equity", "Total Equity Gross Minority Interest"),
  `2025` = c(500, 550), check.names = FALSE
)
check("equity gap without booked NCI stays unknown (NA)",
      is.na(statement_other_equity_claims(bs_gap_no_nci_line)))
check("book proxy 0 does not invent market value when gap exists",
      identical(statement_other_equity_claims_book(bs_gap_no_nci_line), 0))

bs_nci <- data.frame(
  Breakdown = c("Common Stock Equity", "Total Equity Gross Minority Interest",
                "Minority Interest", "Preferred Stock"),
  `2025` = c(500, 550, 50, 0), check.names = FALSE
)
check("disclosed NCI book keeps market claims unknown",
      is.na(statement_other_equity_claims(bs_nci)))
check("book proxy reports disclosed NCI",
      identical(statement_other_equity_claims_book(bs_nci), 50))

# --- Completeness: other claims 0 + finite stage WACC → not listed ---
vals <- list(
  dcf_mode = "two_stage", dcf_claim = "fcff",
  bridge_other_claims = 0,
  wacc_stage1 = 8.5, wacc_stage2 = 8.5,
  years = 5, sgr = 2,
  yr_stage1 = 5, g_stage1 = 8,
  bridge_cash = 1, bridge_debt = 1, bridge_shares = 1, bridge_revenue = 1,
  `mod_fcf-fcf_nopat` = 1, `mod_fcf-fcf_depreciation` = 1,
  `mod_fcf-fcf_capex` = 1, `mod_fcf-fcf_delta_nwc` = 1,
  `mod_fcf-fcf_invested_capital` = 1,
  `mod_ddm-ddm_mode` = "gordon", `mod_ri-roe_method` = "constant",
  `mod_pb-basis` = "bvps", `mod_rel-rel_mode` = "equity",
  `.sotp_ready` = TRUE
)
rows <- ynow_required_parameter_rows(vals, APP_DEFAULTS, "en", 9, 8.5)
dcf_ids <- rows$id[rows$model == "dcf"]
check("two-stage with claims=0 and stage WACC filled does not require other claims",
      !"bridge_other_claims" %in% dcf_ids)
check("two-stage with stage WACC filled does not list wacc_stage1/2",
      !any(c("wacc_stage1", "wacc_stage2") %in% dcf_ids))

vals_na <- vals
vals_na$bridge_other_claims <- NA_real_
vals_na$wacc_stage1 <- NA_real_
vals_na$wacc_stage2 <- NA_real_
rows_na <- ynow_required_parameter_rows(vals_na, APP_DEFAULTS, "en", 9, 8.5)
check("empty stage WACC is listed as missing for two-stage",
      all(c("wacc_stage1", "wacc_stage2") %in% rows_na$id[rows_na$model == "dcf"]))
check("empty other claims is listed when input is NA",
      "bridge_other_claims" %in% rows_na$id[rows_na$model == "dcf"])
# Suggestions for stages come from calculated WACC percent.
s1 <- rows_na$suggestion[rows_na$id == "wacc_stage1"][1]
s2 <- rows_na$suggestion[rows_na$id == "wacc_stage2"][1]
check("missing stage WACC carries calculated wacc suggestion",
      isTRUE(all.equal(s1, 8.5)) && isTRUE(all.equal(s2, 8.5)))

# --- WACC apply helper: production pure function (setup.R) ---
check("ynow_apply_calculated_wacc_fields exported from setup.R",
      exists("ynow_apply_calculated_wacc_fields", mode = "function"))

seeded <- list(wacc_gordon = 7, wacc_stage1 = 7, wacc_stage2 = 7)
blocked <- ynow_apply_calculated_wacc_fields(9, NA_real_, seeded)
check("failed other-claims gate does not clear existing WACC fields",
      identical(blocked, seeded))
blocked_nz <- ynow_apply_calculated_wacc_fields(9, 50, seeded)
check("nonzero other claims does not clear existing WACC fields",
      identical(blocked_nz, seeded))
applied <- ynow_apply_calculated_wacc_fields(8.79, 0, list(
  wacc_gordon = NA_real_, wacc_stage1 = NA_real_, wacc_stage2 = NA_real_))
check("successful WACC writes gordon and both stage fields",
      isTRUE(all.equal(applied$wacc_gordon, 8.79)) &&
        isTRUE(all.equal(applied$wacc_stage1, 8.79)) &&
        isTRUE(all.equal(applied$wacc_stage2, 8.79)))

# Server source must not clear-then-bail, and must write both stage fields.
srv <- paste(readLines(file.path(app, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"),
             collapse = "\n")
# After fix: clearing all three immediately before the other_claims early-return
# must be gone; both stage updates must happen whenever WACC is written.
check("server does not clear all WACC inputs immediately before other-claims gate",
      !grepl(
        "for \\(id in c\\(\"wacc_gordon\", \"wacc_stage1\", \"wacc_stage2\"\\)\\) updateNumericInput\\(session, id, value = NA_real_\\)\\s*\\n\\s*other <- \\.shared_bridge\\(\\)\\$other_claims\\s*\\n\\s*if \\(!is\\.finite\\(other\\) \\|\\| other != 0\\) return",
        srv
      ))
check("server routes calculated WACC through ynow_apply_calculated_wacc_fields",
      grepl("ynow_apply_calculated_wacc_fields\\(", srv))
check("server writes wacc_stage1 and wacc_stage2 together on calculated WACC",
      grepl(
        "updateNumericInput\\(session, \"wacc_stage1\", value = applied_wacc\\$wacc_stage1\\)[\\s\\S]*updateNumericInput\\(session, \"wacc_stage2\", value = applied_wacc\\$wacc_stage2\\)",
        srv
      ) ||
        grepl(
          "updateNumericInput\\(session, \"wacc_stage1\".*updateNumericInput\\(session, \"wacc_stage2\"",
          srv
        ))

# Prefer always writing all three (not only when mode != gordon).
auto_fn <- sub(".*(\\.auto_recalc_capm_wacc <- function[\\s\\S]*?^  \\})", "\\1",
               srv, perl = TRUE)
# Fallback: locate by unique marker
if (!grepl("\\.auto_recalc_capm_wacc <- function", auto_fn)) {
  auto_fn <- srv
}
check("calculated WACC path writes stage fields even when mode is gordon",
      grepl("ynow_apply_calculated_wacc_fields\\(", auto_fn) &&
        grepl("updateNumericInput\\(session, \"wacc_gordon\", value = applied_wacc\\$wacc_gordon\\)", auto_fn) &&
        grepl("updateNumericInput\\(session, \"wacc_stage1\", value = applied_wacc\\$wacc_stage1\\)", auto_fn) &&
        grepl("updateNumericInput\\(session, \"wacc_stage2\", value = applied_wacc\\$wacc_stage2\\)", auto_fn) &&
        !grepl("if \\(identical\\(input\\$dcf_mode, \"gordon\"\\) \\|\\| is\\.null\\(input\\$dcf_mode\\)\\) \\{[\\s\\S]*updateNumericInput\\(session, \"wacc_gordon\"[\\s\\S]*\\} else \\{[\\s\\S]*wacc_stage1",
               auto_fn))

cat("ALL PASS: two-stage WACC + other equity claims contracts\n")
