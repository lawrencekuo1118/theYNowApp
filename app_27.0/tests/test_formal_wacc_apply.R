#!/usr/bin/env Rscript
# Bounded property checks for WACC apply / other-claims gate (no network).
# Formal-ish verification for R: generated inputs over a finite domain, on the
# real production function `ynow_apply_calculated_wacc_fields` in setup.R.
# (No TLA+/SMT tool fits this Shiny/R surface well; see skill verify-formal-wacc-apply.)
#
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_formal_wacc_apply.R
# Optional: YNOW_FORMAL_WACC_N=2000  (default 1000 trials)

args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
app <- dirname(dirname(normalizePath(script)))
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source(file.path(app, "setup.R"))

fail <- 0L
n_ok <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    n_ok <<- n_ok + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

fields_equal <- function(a, b) {
  ids <- c("wacc_gordon", "wacc_stage1", "wacc_stage2")
  for (id in ids) {
    xa <- suppressWarnings(as.numeric(a[[id]])[1])
    xb <- suppressWarnings(as.numeric(b[[id]])[1])
    if (is.na(xa) && is.na(xb)) next
    if (!is.finite(xa) || !is.finite(xb) || abs(xa - xb) > 1e-12) return(FALSE)
  }
  TRUE
}

all_equal_w <- function(out, w) {
  isTRUE(all.equal(out$wacc_gordon, w)) &&
    isTRUE(all.equal(out$wacc_stage1, w)) &&
    isTRUE(all.equal(out$wacc_stage2, w))
}

check("production helper exists", exists("ynow_apply_calculated_wacc_fields", mode = "function"))

# --- Spot contracts (known counterexample shapes from #636) ---
seeded <- list(wacc_gordon = 7, wacc_stage1 = 7.25, wacc_stage2 = 6.5)
check(
  "gate NA does not clear seeded fields",
  fields_equal(ynow_apply_calculated_wacc_fields(9, NA_real_, seeded), seeded)
)
check(
  "gate nonzero does not clear seeded fields",
  fields_equal(ynow_apply_calculated_wacc_fields(9, 12.5, seeded), seeded)
)
check(
  "gate 0 writes identical Gordon+stage1+stage2",
  all_equal_w(
    ynow_apply_calculated_wacc_fields(8.79, 0, list(
      wacc_gordon = NA_real_, wacc_stage1 = 1, wacc_stage2 = 2
    )),
    8.79
  )
)
check(
  "non-positive wacc leaves seed alone even when claims=0",
  fields_equal(
    ynow_apply_calculated_wacc_fields(0, 0, seeded),
    seeded
  ) && fields_equal(
    ynow_apply_calculated_wacc_fields(-1, 0, seeded),
    seeded
  )
)

# Broken-variant oracle: the pre-#636 clear-then-bail shape must FAIL the property.
broken_clear_on_fail <- function(wacc_percent, other_claims, current) {
  wacc_percent <- suppressWarnings(as.numeric(wacc_percent)[1])
  other_claims <- suppressWarnings(as.numeric(other_claims)[1])
  cleared <- list(
    wacc_gordon = NA_real_,
    wacc_stage1 = NA_real_,
    wacc_stage2 = NA_real_
  )
  if (!is.finite(other_claims) || other_claims != 0) return(cleared)
  if (!is.finite(wacc_percent) || wacc_percent <= 0) return(cleared)
  list(
    wacc_gordon = wacc_percent,
    wacc_stage1 = wacc_percent,
    wacc_stage2 = wacc_percent
  )
}
broken <- broken_clear_on_fail(9, NA_real_, seeded)
check(
  "deliberate clear-on-fail variant violates no-clear property (detector live)",
  !fields_equal(broken, seeded)
)

# --- Bounded property search ---
n_trials <- suppressWarnings(as.integer(Sys.getenv("YNOW_FORMAL_WACC_N", "1000"))[1])
if (!is.finite(n_trials) || n_trials < 50L) n_trials <- 1000L
set.seed(636L)

# Domain bounds (honest scope — not all reals):
#   wacc_percent ∈ (0, 50] ∪ {NA, 0, -1, Inf}  (percent units)
#   other_claims ∈ {0} ∪ nonzero finite ∪ {NA, Inf, -Inf}
#   seed fields ∈ finite percent ∪ NA
wacc_pool <- c(
  NA_real_, 0, -1, Inf, 0.01, 0.5, 1, 4.5, 7, 8.79, 9, 12, 25, 50
)
claims_pool <- c(
  0, NA_real_, Inf, -Inf, -1, 0.01, 1, 12.5, 50, 1e6
)
seed_pool <- c(NA_real_, 1, 4, 6.5, 7, 7.25, 8.79, 10, 15)

prop_fail <- 0L
cex <- NULL
for (i in seq_len(n_trials)) {
  w <- sample(wacc_pool, 1L)
  # Mix discrete pool with continuous draws inside (0, 50]
  if (runif(1) < 0.35) {
    w <- runif(1, 0.01, 50)
  }
  oc <- sample(claims_pool, 1L)
  if (runif(1) < 0.2) {
    oc <- runif(1, -1e3, 1e3)
    if (abs(oc) < 1e-9) oc <- 0
  }
  seed <- list(
    wacc_gordon = sample(seed_pool, 1L),
    wacc_stage1 = sample(seed_pool, 1L),
    wacc_stage2 = sample(seed_pool, 1L)
  )
  out <- ynow_apply_calculated_wacc_fields(w, oc, seed)

  gate_ok <- is.finite(oc) && oc == 0
  w_ok <- is.finite(w) && w > 0

  if (!gate_ok || !w_ok) {
    if (!fields_equal(out, seed)) {
      prop_fail <- prop_fail + 1L
      if (is.null(cex)) {
        cex <- list(kind = "cleared_or_mutated_on_fail", w = w, oc = oc, seed = seed, out = out)
      }
    }
  } else {
    if (!all_equal_w(out, w)) {
      prop_fail <- prop_fail + 1L
      if (is.null(cex)) {
        cex <- list(kind = "incomplete_write", w = w, oc = oc, seed = seed, out = out)
      }
    }
  }
}

check(
  sprintf("property holds for %d generated inputs (seed=636)", n_trials),
  prop_fail == 0L
)
if (prop_fail > 0L && !is.null(cex)) {
  cat("COUNTEREXAMPLE:\n")
  print(cex)
}

# Server wiring still routes through the helper and writes all three IDs.
srv <- paste(readLines(file.path(app, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"),
             collapse = "\n")
auto_fn <- sub(
  ".*(\\.auto_recalc_capm_wacc <- function[\\s\\S]*?^  \\})",
  "\\1",
  srv,
  perl = TRUE
)
if (!grepl("\\.auto_recalc_capm_wacc <- function", auto_fn)) auto_fn <- srv
check(
  ".auto_recalc_capm_wacc calls ynow_apply_calculated_wacc_fields",
  grepl("ynow_apply_calculated_wacc_fields\\(", auto_fn)
)
check(
  ".auto_recalc_capm_wacc writes gordon+stage1+stage2 from applied_wacc",
  grepl("applied_wacc\\$wacc_gordon", auto_fn) &&
    grepl("applied_wacc\\$wacc_stage1", auto_fn) &&
    grepl("applied_wacc\\$wacc_stage2", auto_fn)
)
check(
  "no clear-all-to-NA immediately before other-claims gate",
  !grepl(
    "for \\(id in c\\(\"wacc_gordon\", \"wacc_stage1\", \"wacc_stage2\"\\)\\) updateNumericInput\\(session, id, value = NA_real_\\)\\s*\\n\\s*other <- \\.shared_bridge\\(\\)\\$other_claims",
    srv
  )
)

# Other-claims zeroing fixtures (shared with #636 contract).
bs_absent <- data.frame(
  Breakdown = c("Total Assets", "Total Liabilities", "Cash And Cash Equivalents", "Total Debt"),
  `2025` = c(1000, 400, 100, 200), check.names = FALSE
)
check("absent pref/NCI → other claims 0", identical(statement_other_equity_claims(bs_absent), 0))
bs_gap <- data.frame(
  Breakdown = c("Common Stock Equity", "Total Equity Gross Minority Interest"),
  `2025` = c(500, 550), check.names = FALSE
)
check("equity gap without NCI line → other claims NA", is.na(statement_other_equity_claims(bs_gap)))

if (fail > 0L) {
  cat("FAILED:", fail, "property_failures:", prop_fail, "\n")
  quit(status = 1L)
}
cat("ALL PASS: formal WACC apply properties (", n_trials, " trials)\n", sep = "")
