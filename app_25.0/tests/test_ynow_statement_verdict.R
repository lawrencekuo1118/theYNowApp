# test_ynow_statement_verdict.R — YNOW verdict from statements only
# Run: cd app_25.0/tests && Rscript test_ynow_statement_verdict.R

.app_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f) == 1L && nzchar(f)) dirname(normalizePath(f)) else getwd()
}, error = function(e) getwd())
if (basename(.app_dir) == "tests") .app_dir <- dirname(.app_dir)
setwd(.app_dir)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# Source only the helper (avoid shiny server boot)
dec_lines <- readLines("investment_decision_module.R", warn = FALSE, encoding = "UTF-8")
dec <- paste(dec_lines, collapse = "\n")
start <- regexpr("ynow_statement_verdict <- function", dec, fixed = TRUE)[1]
end <- regexpr("decision_valuation_compare_ui <- function", dec, fixed = TRUE)[1]
stopifnot(is.finite(start), start > 0, is.finite(end), end > start)
eval(parse(text = substr(dec, start, end - 1L)))

check <- function(lab, cond) {
  cat(if (isTRUE(cond)) "OK  " else "FAIL", lab, "\n")
  if (!isTRUE(cond)) quit(status = 1L)
}

check("missing F-Score → NULL", is.null(ynow_statement_verdict(NULL, NA, 1, 0L)))

manip <- ynow_statement_verdict(
  list(ok = TRUE, m_flag = "警示", qoe_flag = "通過", risk_level = "高", red_flags = c("a", "b")),
  f_score = 8, f_quality = 1, n_alert = 0L
)
check("M-Score alert → manip", identical(manip$title_key, "funnel_v_manip_title"))

weak <- ynow_statement_verdict(
  list(ok = TRUE, m_flag = "通過", qoe_flag = "警示", risk_level = "低", red_flags = character(0)),
  f_score = 3, f_quality = 1, n_alert = 0L
)
check("weak F-Score → quality_weak", identical(weak$title_key, "funnel_v_quality_weak_title"))

watch <- ynow_statement_verdict(
  list(ok = TRUE, m_flag = "觀察", qoe_flag = "通過", risk_level = "中", red_flags = character(0)),
  f_score = 7, f_quality = 1, n_alert = 0L
)
check("M watch → watch", identical(watch$title_key, "funnel_v_watch_title"))

watch_shen <- ynow_statement_verdict(
  list(ok = TRUE, m_flag = "通過", qoe_flag = "通過", risk_level = "低", red_flags = character(0)),
  f_score = 8, f_quality = 1, n_alert = 2L
)
check("Schilit alerts → watch", identical(watch_shen$title_key, "funnel_v_watch_title"))

clear <- ynow_statement_verdict(
  list(ok = TRUE, m_flag = "通過", qoe_flag = "通過", risk_level = "低", red_flags = character(0)),
  f_score = 8, f_quality = 1, n_alert = 0L
)
check("clean statements → clear", identical(clear$title_key, "funnel_v_clear_title"))
check("clear is not success/buy styling", identical(clear$class, "alert-info"))

# No MOS / momentum arguments on the helper signature
check(
  "helper has no mos/mom formals",
  !any(c("mos", "mom", "momentum", "price") %in% names(formals(ynow_statement_verdict)))
)

cat("ALL PASS\n")
