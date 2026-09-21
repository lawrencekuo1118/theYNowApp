#!/usr/bin/env Rscript
# Blue Chip lab_im_max_n parsing (no network).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

all_p <- lab_parse_im_max_n("all")
check("all -> unlimited", isTRUE(all_p$unlimited) && is.infinite(all_p$n))
check("all label 全部", identical(all_p$label, "全部"))

null_p <- lab_parse_im_max_n(NULL)
check("NULL defaults to 25", identical(null_p$n, 25L) && !null_p$unlimited)

n25 <- lab_parse_im_max_n("25")
check("25 parsed", identical(n25$n, 25L) && !n25$unlimited)

check("clamp all is Inf", is.infinite(lab_clamp_im_max_n("all")))
check("clamp 25 is 25", identical(lab_clamp_im_max_n("25"), 25L))
check("clamp 999 capped at hi", identical(lab_clamp_im_max_n("999", hi = 500L), 500L))

check(
  "resolve custom uses numeric",
  identical(lab_resolve_im_max_n("custom", 42), 42L)
)
check(
  "resolve preset 100",
  identical(lab_resolve_im_max_n("100", NULL), 100L)
)
check(
  "resolve custom label",
  identical(lab_resolve_im_max_n_label("custom", 42), "42")
)

check(
  "eval_n for display 25 is 100",
  identical(lab_resolve_im_eval_n(25L), 100L)
)
check(
  "eval_n for display 50 is 200",
  identical(lab_resolve_im_eval_n(50L), 200L)
)
check(
  "eval_n for display 200 caps at 500",
  identical(lab_resolve_im_eval_n(200L), 500L)
)
check(
  "eval_n for Inf stays Inf",
  is.infinite(lab_resolve_im_eval_n(Inf))
)
# 120*4=480 < 500
check("eval_n 120 -> 480", identical(lab_resolve_im_eval_n(120L), 480L))

scored <- data.frame(
  ticker = c("A", "B", "C", "D", "E"),
  upside_cagr_pct = c(40, 30, 20, NA, 10),
  f_score = c(8, 7, 5, 9, 8),
  quality_flag = c(1, 1, 1, 1, 0),
  industry_label = rep("Tech", 5),
  stringsAsFactors = FALSE
)
# gate on: A,B,E have F≥7 + upside → 3; display 2 → no pad beyond 2
cap2 <- lab_cap_detail_display(scored, display_n = 2L, eq_only = FALSE, gate_only = TRUE)
check("cap detail to 2 qualified", nrow(cap2) == 2L)
check("cap picks highest upside first", identical(cap2$ticker, c("A", "B")))
# only 3 qualify with gate; ask for 10 → return 3 (no pad)
cap10 <- lab_cap_detail_display(scored, display_n = 10L, eq_only = FALSE, gate_only = TRUE)
check("no pad when under N", nrow(cap10) == 3L)
check("no pad tickers", identical(cap10$ticker, c("A", "B", "E")))
# eq_only drops E (quality_flag 0)
cap_eq <- lab_cap_detail_display(scored, display_n = 10L, eq_only = TRUE, gate_only = TRUE)
check("eq gate drops E", identical(cap_eq$ticker, c("A", "B")))

ch <- lab_im_max_n_select_choices()
check(
  "shared N choice values",
  identical(unname(ch), c("25", "50", "100", "200", "500", "all", "custom"))
)

pool <- data.frame(
  ticker = c("AAA", "BBB", "CCC", "DDD"),
  market_cap = c(100, 400, 200, 50),
  stringsAsFactors = FALSE
)
capped <- lab_rank_and_cap_eval_pool(pool, max_n = 2L)
check("cap to 2 rows", nrow(capped) == 2L)
check("cap picks largest mcap", identical(capped$ticker, c("BBB", "CCC")))

uncapped <- lab_rank_and_cap_eval_pool(pool, max_n = Inf)
check("Inf keeps all rows", nrow(uncapped) == 4L)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1)
}
cat("All lab_im_max_n checks passed.\n")
