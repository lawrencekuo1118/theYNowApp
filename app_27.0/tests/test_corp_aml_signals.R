#!/usr/bin/env Rscript
# Corporate AML statement proxies (no network).
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_corp_aml_signals.R
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

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

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

cat_reg <- list_corp_aml_typology_catalog()
check("registry has 8 codes", nrow(cat_reg) == 8L)
check("registry codes AML_C1..C8", identical(cat_reg$code, paste0("AML_C", 1:8)))
check("registry zh/en non-blank", all(nzchar(cat_reg$name_zh)) && all(nzchar(cat_reg$name_en)))

empty <- evaluate_corp_aml_signals(NULL, NULL, NULL)
check("empty not ok", identical(empty$ok, FALSE))
check("empty tier na", identical(empty$tier, "na"))
check("empty veto null", is.null(corp_aml_veto_keys(empty)))

is_ok <- mk_stmt(
  `Total Revenue` = c(100, 95, 90),
  `Net Income` = c(12, 11, 10)
)
bs_ok <- mk_stmt(
  `Cash And Cash Equivalents` = c(15, 14, 13),
  `Accounts Receivable` = c(12, 11.5, 11),
  `Inventory` = c(10, 9.5, 9),
  `Other Receivables` = c(2, 2, 2),
  `Total Assets` = c(80, 78, 76)
)
cf_ok <- mk_stmt(
  `Operating Cash Flow` = c(16, 15, 14),
  `Financing Cash Flow` = c(-4, -3, -3)
)
pack_ok <- evaluate_corp_aml_signals(is_ok, bs_ok, cf_ok, meta = list(country = "US"))
check("healthy ok", isTRUE(pack_ok$ok))
check("healthy tier clear or watch", pack_ok$tier %in% c("clear", "watch"))
check("healthy flags 8 rows", nrow(pack_ok$flags) == 8L)
check("healthy C5 clear", {
  r <- pack_ok$flags[pack_ok$flags$code == "AML_C5", , drop = FALSE]
  nrow(r) == 1L && identical(r$severity[1], "clear")
})
check("healthy no veto", is.null(corp_aml_veto_keys(pack_ok)))

is_bad <- mk_stmt(
  `Total Revenue` = c(100, 95, 90),
  `Net Income` = c(40, 12, 11)
)
bs_bad <- mk_stmt(
  `Cash And Cash Equivalents` = c(70, 14, 13),
  `Accounts Receivable` = c(45, 12, 11),
  `Inventory` = c(35, 10, 9),
  `Other Receivables` = c(20, 2, 2),
  `Total Assets` = c(120, 80, 76)
)
cf_bad <- mk_stmt(
  `Operating Cash Flow` = c(2, 14, 13),
  `Financing Cash Flow` = c(50, -3, -3)
)
eq_bad <- evaluate_earnings_quality(is_bad, bs_bad, cf_bad)
pack_bad <- evaluate_corp_aml_signals(
  is_bad, bs_bad, cf_bad,
  meta = list(country = "KY", n_schilit = 3L, going_concern = TRUE),
  eq_pack = eq_bad
)
check("bad ok", isTRUE(pack_bad$ok))
check("bad tier alert", identical(pack_bad$tier, "alert"))
check("bad has alerts", isTRUE(pack_bad$n_alert >= 1L))
check("bad WC flagged", {
  r <- pack_bad$flags[pack_bad$flags$code == "AML_C3", , drop = FALSE]
  nrow(r) == 1L && r$severity[1] %in% c("alert", "watch")
})
check("bad veto keys", {
  v <- corp_aml_veto_keys(pack_bad)
  is.list(v) && identical(v$title_key, "aml_veto_title")
})

pack_peer <- evaluate_corp_aml_signals(
  is_ok, bs_ok, cf_ok,
  meta = list(country = "TW", peer_cash_rev_median = 0.05)
)
check("peer path ok", isTRUE(pack_peer$ok))
check("C8 na without auditor", {
  r <- pack_peer$flags[pack_peer$flags$code == "AML_C8", , drop = FALSE]
  nrow(r) == 1L && identical(r$severity[1], "na")
})

src <- paste(readLines(file.path(app_dir, "corp_aml_signals.R"), warn = FALSE), collapse = "\n")
check(
  "no affirmative buy wording",
  !grepl("建議買進|建議加碼|is a buy|as a buy", src, ignore.case = TRUE)
)
check("out-of-scope day windows documented", grepl("Day-window cash ticket rules are out of scope", src, fixed = TRUE))

g <- paste(readLines(file.path(app_dir, "global.R"), warn = FALSE), collapse = "\n")
check(
  "global sources corp_aml_signals",
  grepl('source\\("corp_aml_signals\\.R"', g)
)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
