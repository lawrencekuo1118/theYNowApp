#!/usr/bin/env Rscript
# 「建議評價方法」summary rows follow sidebar valuation order (no network).
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

want <- c("nav", "dcf", "ddm", "ri", "pb")
check("LAB_SIDEBAR_METHOD_ORDER", identical(LAB_SIDEBAR_METHOD_ORDER, want))
check(
  "lab_order shuffles to sidebar",
  identical(
    lab_order_methods_like_sidebar(c("pb", "dcf", "nav", "ri", "ddm")),
    want
  )
)
check(
  "lab_order skips missing",
  identical(lab_order_methods_like_sidebar(c("ri", "dcf")), c("dcf", "ri"))
)
check(
  "lab_order unknown last",
  identical(
    lab_order_methods_like_sidebar(c("zzz", "pb", "nav")),
    c("nav", "pb", "zzz")
  )
)

catlg <- data.frame(
  primary = c("pb", "dcf", "nav", "dcf", "ri", "ddm"),
  industry_key = c("a", "b", "c", "d", "e", "f"),
  ticker = c("T1", "T2", "T3", "T4", "T5", "T6"),
  stringsAsFactors = FALSE
)
sm <- lab_method_group_summary(catlg)
check("summary method_key order", identical(sm$method_key, want))
check(
  "summary labels follow keys",
  identical(sm[["建議評價方法"]], unname(LAB_METHOD_LABELS[want]))
)

sm2 <- lab_method_group_summary(
  catlg[catlg$primary %in% c("ri", "nav", "pb"), , drop = FALSE]
)
check("partial summary order", identical(sm2$method_key, c("nav", "ri", "pb")))

if (fail > 0L) {
  cat("FAILED ", fail, " check(s)\n", sep = "")
  quit(status = 1L)
}
cat("PASS lab_method_sidebar_order\n")
