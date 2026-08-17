#!/usr/bin/env Rscript
# Schilit auto-alert engine (no network).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "shenanigans_module.R"), local = FALSE)

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

empty <- evaluate_shenanigans(NULL, NULL, NULL)
check("empty statements not ok", identical(empty$ok, FALSE))
check("empty n_alert 0", identical(empty$n_alert, 0L))

# Healthy-ish manufacturer: NI < OCF, modest AR/inv, D/E < 1
is_ok <- mk_stmt(
  `Total Revenue` = c(100, 95, 90, 85),
  `Gross Profit` = c(40, 38, 36, 34),
  `Cost Of Revenue` = c(60, 57, 54, 51),
  `Operating Expense` = c(20, 19, 18, 17),
  `Operating Income` = c(20, 19, 18, 17),
  `Net Income` = c(12, 11, 10, 9)
)
bs_ok <- mk_stmt(
  `Accounts Receivable` = c(12, 11.5, 11, 10.5),
  `Inventory` = c(10, 9.6, 9.2, 8.8),
  `Accounts Payable` = c(8, 7.7, 7.4, 7.1),
  `Total Assets` = c(80, 78, 76, 74),
  `Current Assets` = c(30, 29, 28, 27),
  `Current Liabilities` = c(15, 14.5, 14, 13.5),
  `Total Debt` = c(10, 10, 10, 10),
  `Common Stock Equity` = c(40, 39, 38, 37),
  `Goodwill` = c(2, 2, 2, 2),
  `Net PPE` = c(20, 19.5, 19, 18.5)
)
cf_ok <- mk_stmt(
  `Operating Cash Flow` = c(16, 15, 14, 13),
  `Free Cash Flow` = c(10, 9, 8, 8),
  `Capital Expenditure` = c(-6, -6, -6, -5),
  `Depreciation` = c(4, 4, 3.8, 3.7)
)
ev_ok <- evaluate_shenanigans(is_ok, bs_ok, cf_ok, industry_key = "tech.Hardware")
check("healthy ok flag", isTRUE(ev_ok$ok))
check("healthy 21 items", nrow(ev_ok$items) == 21L)
x1_ok <- ev_ok$items[ev_ok$items$code == "X1", ]
check("healthy X1 not alert", !identical(x1_ok$status[1], "警示"))
x6 <- ev_ok$items[ev_ok$items$code == "X6", ]
check("X6 insufficient without non-GAAP", identical(x6$status[1], "資料不足"))

# NI persistently > OCF and NI>0 OCF<0 → X1 警示; D/E > 2 → #12 警示
is_bad <- mk_stmt(
  `Total Revenue` = c(100, 90, 80, 70),
  `Gross Profit` = c(50, 40, 32, 28),
  `Operating Income` = c(20, 10, 8, 6),
  `Net Income` = c(25, 20, 18, 15)
)
bs_bad <- mk_stmt(
  `Accounts Receivable` = c(40, 20, 12, 10),
  `Inventory` = c(30, 18, 12, 10),
  `Total Assets` = c(120, 90, 80, 70),
  `Current Assets` = c(70, 40, 30, 25),
  `Current Liabilities` = c(20, 18, 16, 15),
  `Total Debt` = c(90, 40, 30, 20),
  `Common Stock Equity` = c(20, 30, 32, 30),
  `Goodwill` = c(40, 5, 4, 4)
)
cf_bad <- mk_stmt(
  `Operating Cash Flow` = c(-5, 8, 7, 6),
  `Free Cash Flow` = c(-20, 1, 1, 1)
)
ev_bad <- evaluate_shenanigans(is_bad, bs_bad, cf_bad)
x1 <- ev_bad$items[ev_bad$items$code == "X1", ]
d12 <- ev_bad$items[ev_bad$items$code == "12", ]
check("bad X1 alert", identical(x1$status[1], "警示"))
check("bad D/E alert", identical(d12$status[1], "警示"))
check("bad n_alert > 0", isTRUE(ev_bad$n_alert > 0L))

msgs <- collect_fraud_warnings(cf_bad, is_bad, bs_bad)
check("collect_fraud_warnings uses alerts", length(msgs) > 0L && any(grepl("X1", msgs)))

# No AR → X2 資料不足, not a false alert
is_noar <- is_ok
bs_noar <- bs_ok[, setdiff(names(bs_ok), character(0)), drop = FALSE]
bs_noar <- bs_noar[bs_noar$Metric != "Accounts Receivable", , drop = FALSE]
ev_noar <- evaluate_shenanigans(is_noar, bs_noar, cf_ok)
x2 <- ev_noar$items[ev_noar$items$code == "X2", ]
check("missing AR is 資料不足", identical(x2$status[1], "資料不足"))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1)
}
cat("All shenanigans checks passed.\n")
