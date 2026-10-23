#!/usr/bin/env Rscript
# Corporate AML compose with YNOW verdict (no network).
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_corp_aml_compose.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "earnings_quality_module.R"), local = FALSE)
source(file.path(app_dir, "corp_aml_signals.R"), local = FALSE)
source(file.path(app_dir, "investment_decision_module.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

# YNOW verdict stays statement-only (no price/MOS args)
check(
  "ynow_statement_verdict formals",
  !any(c("mos", "mom", "momentum", "price") %in% names(formals(ynow_statement_verdict)))
)

mk_stmt <- function(...) {
  rows <- list(...)
  nms <- names(rows)
  n <- max(vapply(rows, length, integer(1)))
  df <- data.frame(Metric = nms, check.names = FALSE, stringsAsFactors = FALSE)
  yrs <- paste0("Y", seq_len(n))
  for (j in seq_len(n)) {
    df[[yrs[j]]] <- vapply(rows, function(v) {
      if (length(v) >= j) as.numeric(v[j]) else NA_real_
    }, numeric(1))
  }
  df
}

is_bad <- mk_stmt(
  `Total Revenue` = c(100, 95),
  `Net Income` = c(40, 12)
)
bs_bad <- mk_stmt(
  `Cash And Cash Equivalents` = c(70, 14),
  `Accounts Receivable` = c(45, 12),
  `Inventory` = c(35, 10),
  `Other Receivables` = c(20, 2),
  `Total Assets` = c(120, 80)
)
cf_bad <- mk_stmt(
  `Operating Cash Flow` = c(2, 14),
  `Financing Cash Flow` = c(50, -3)
)
eq <- evaluate_earnings_quality(is_bad, bs_bad, cf_bad)
pack <- evaluate_corp_aml_signals(
  is_bad, bs_bad, cf_bad,
  meta = list(country = "KY", going_concern = TRUE, n_schilit = 3L),
  eq_pack = eq
)
check("compose alert tier", identical(pack$tier, "alert"))
v <- corp_aml_veto_keys(pack)
check("veto title key", identical(v$title_key, "aml_veto_title"))
check("veto text key", identical(v$text_key, "aml_veto_text"))

clear_pack <- list(ok = TRUE, tier = "clear")
check("clear has no veto", is.null(corp_aml_veto_keys(clear_pack)))

marks <- corp_aml_surface_marks(pack)
check("surface marks list", is.list(marks))
check("C2/C7 alert marks QoE", identical(marks$qoe, "alert") || identical(marks$qoe, "watch"))
check("C7 marks Beneish", identical(marks$beneish, "alert") || identical(marks$beneish, "watch"))
check("C7 marks Schilit", identical(marks$schilit, "alert") || identical(marks$schilit, "watch"))
check("risk matrix marked", identical(marks$risk_matrix, "alert") || identical(marks$risk_matrix, "watch"))
check("clear pack no marks", is.null(corp_aml_surface_marks(clear_pack)$qoe))
check("mark ui null on clear", is.null(corp_aml_mark_ui(NULL)))
check("mark ui alert class", {
  m <- corp_aml_mark_ui("alert", locale = "en")
  grepl("ynow-aml-mark--alert", as.character(m), fixed = TRUE)
})

dec <- paste(readLines(file.path(app_dir, "investment_decision_module.R"), warn = FALSE), collapse = "\n")
check("ui_recommendation still uses ynow_statement_verdict", grepl("ynow_statement_verdict\\(", dec))
check("veto rendered beside recommendation", grepl("corp_aml_veto_keys", dec, fixed = TRUE))
check("AML marks wired in decision_server", grepl("corp_aml_surface_marks", dec, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
