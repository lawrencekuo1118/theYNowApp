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
check("NULL defaults to 100", identical(null_p$n, 100L) && !null_p$unlimited)

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

pool <- data.frame(
  ticker = c("AAA", "BBB", "CCC", "DDD"),
  market_cap = c(100, 400, 200, 50),
  size_band = rep("large", 4),
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
