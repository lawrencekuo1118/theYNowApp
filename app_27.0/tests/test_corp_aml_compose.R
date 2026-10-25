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
source(file.path(app_dir, "shenanigans_module.R"), local = FALSE)
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

# Surface shim: no KPI/panel marks (AML lives on 財報警訊 detail rows)
surf <- corp_aml_surface_marks(pack)
check("surface shim null qoe", is.null(surf$qoe))
check("surface shim null fscore", is.null(surf$fscore))
check("surface shim null risk_matrix", is.null(surf$risk_matrix))

# Warning-item marks: overlapping 財報警訊 keys only
marks <- corp_aml_warning_item_marks(pack)
check("warning marks list", is.list(marks))
check("C7 marks BENEISH", identical(marks$BENEISH, "alert") || identical(marks$BENEISH, "watch"))
check("C7 marks SCHILIT compose", identical(marks$SCHILIT, "alert") || identical(marks$SCHILIT, "watch"))
c2 <- corp_aml_flag_severity(pack, "AML_C2")
if (identical(c2, "alert") || identical(c2, "watch")) {
  check("C2 marks X1", identical(marks$X1, c2) || identical(marks$X1, "alert") || identical(marks$X1, "watch"))
  check("C2 marks BENEISH:TATA", !is.null(marks[["BENEISH:TATA"]]))
}
c3 <- corp_aml_flag_severity(pack, "AML_C3")
if (identical(c3, "alert") || identical(c3, "watch")) {
  check("C3 marks X2", !is.null(marks$X2))
  check("C3 marks BENEISH:DSRI", !is.null(marks[["BENEISH:DSRI"]]))
}
check("clear pack no warning marks", length(corp_aml_warning_item_marks(clear_pack)) == 0L)
check("mark ui null on clear", is.null(corp_aml_mark_ui(NULL)))
check("mark ui alert class", {
  m <- corp_aml_mark_ui("alert", locale = "en")
  grepl("ynow-aml-mark--alert", as.character(m), fixed = TRUE)
})
check(
  "severity for X1 inherits SCHILIT",
  identical(
    corp_aml_severity_for_warning_item(list(SCHILIT = "watch"), "X1"),
    "watch"
  )
)
check(
  "severity null when no overlap",
  is.null(corp_aml_severity_for_warning_item(list(BENEISH = "alert"), "X9"))
)

# Schilit UI: AML badge only on elevated overlapping rows
ev_fake <- list(
  ok = TRUE,
  n_alert = 1L,
  n_watch = 0L,
  n_pass = 0L,
  n_na = 0L,
  items = data.frame(
    code = c("X1", "X9"),
    category = c("財報交叉檢查", "財報交叉檢查"),
    name = c("NI vs OCF", "Liquidity"),
    status = c("警示", "警示"),
    reason = c("r1", "r2"),
    value_txt = c("v1", "v2"),
    threshold_txt = c("t1", "t2"),
    stringsAsFactors = FALSE
  )
)
ui_shen <- as.character(shenanigans_results_ui(
  ev_fake,
  aml_marks = list(X1 = "alert"),
  locale = "en"
))
check("schilit X1 has AML mark", grepl("ynow-aml-mark--alert", ui_shen, fixed = TRUE))
# X9 has no overlap key → no second alert mark for X9 specifically via code path;
# still only one mark from X1 in this fixture
check(
  "schilit mark count matches overlap",
  length(gregexpr("ynow-aml-mark--alert", ui_shen, fixed = TRUE)[[1]]) == 1L
)

# Beneish UI: head mark when elevated + BENEISH key
eq_bad_flag <- eq
if (is.list(eq_bad_flag)) {
  # Force elevated flag for UI path if M already elevated; else skip soft
  ui_ben <- as.character(beneish_m_score_widget_ui(
    eq_bad_flag,
    locale = "en",
    aml_marks = list(BENEISH = "alert", `BENEISH:TATA` = "watch", `BENEISH:DSRI` = "watch")
  ))
  if (grepl("ynow-mscore-widget--danger|ynow-mscore-widget--watch", ui_ben)) {
    check("beneish elevated shows AML", grepl("ynow-aml-mark", ui_ben, fixed = TRUE))
  } else {
    check("beneish pass has no head AML", !grepl("ynow-aml-mark--alert", ui_ben, fixed = TRUE))
  }
}

dec <- paste(readLines(file.path(app_dir, "investment_decision_module.R"), warn = FALSE), collapse = "\n")
check("ui_recommendation still uses ynow_statement_verdict", grepl("ynow_statement_verdict\\(", dec))
check("veto rendered beside recommendation", grepl("corp_aml_veto_keys", dec, fixed = TRUE))
check("AML warning marks wired in decision_server", grepl("corp_aml_warning_item_marks", dec, fixed = TRUE))
check("no KPI aml_lab on fscore", !grepl('\\.aml_lab\\(.*fscore', dec))
check("shenanigans panel gets aml_marks", grepl("aml_marks = \\.aml_warning_marks\\(\\)", dec))
check("beneish widget gets aml_marks", grepl("aml_marks = \\.aml_warning_marks\\(\\)", dec))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
