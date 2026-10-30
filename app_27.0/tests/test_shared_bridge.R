# Shared cash / debt / shares / revenue source for DCF, WACC, Multiples, SOTP.
# Run: cd app_27.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_shared_bridge.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
app_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  normalizePath("..", mustWork = TRUE)
}
setwd(app_dir)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}
source("setup.R", local = FALSE, encoding = "UTF-8")

fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) cat("OK ", label, "\n", sep = "")
  else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

bs <- data.frame(
  Breakdown = c("Cash And Cash Equivalents", "Total Debt", "Ordinary Shares Number"),
  `2025` = c(20, 80, 10),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
cd <- statement_cash_debt(bs)
check("cash and debt from the same rows", identical(cd$cash, 20) && identical(cd$debt, 80))

bs_split <- data.frame(
  Breakdown = c("Cash And Cash Equivalents", "Current Debt", "Long Term Debt"),
  `2025` = c(5, 3, 7),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
cd2 <- statement_cash_debt(bs_split)
check("debt sums short and long when Total Debt is absent", identical(cd2$debt, 10))

empty <- statement_cash_debt(NULL)
check("missing statements preserve unknown cash and debt", is.na(empty$cash) && is.na(empty$debt))

is_df <- data.frame(
  Breakdown = c("Total Revenue", "Operating Revenue"),
  `2025` = c(100, 90),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
check("total revenue prefers Total Revenue", identical(statement_total_revenue(is_df), 100))

check("bridge pick uses the first finite override", identical(bridge_pick_number(NA, 4, 9), 4))
check("bridge pick falls through to the statement", identical(bridge_pick_number(NA, NA, 9), 9))

adr <- adopt_valuation_shares(
  list(shares = 2, shares_bs = 200, method = "market_cap_per_price", note = "adr", ratio = 100),
  statement_ccy = "TWD", quote_ccy = "USD"
)
check("ADR shares are the quote-class count", identical(adr$shares, 2) && identical(adr$method, "market_cap_per_price"))

plain <- adopt_valuation_shares(
  list(shares = 10, shares_bs = 10, method = "balance_sheet", note = NULL, ratio = NA),
  statement_ccy = "USD", quote_ccy = "USD"
)
check("same-currency shares stay on the balance sheet", identical(plain$shares, 10))

fake_input <- structure(
  list(name = "input", attribs = list(id = "cash", type = "number"), children = list()),
  class = "shiny.tag"
)
fake_ui <- structure(
  list(name = "div", attribs = list(class = "form-group"), children = list(fake_input)),
  class = "shiny.tag"
)
locked <- lock_numeric_input(fake_ui)
check("locked bridge input is not editable", identical(locked$children[[1]]$attribs$disabled, "disabled"))

srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("one shared bridge editor", grepl("bridge_cash", ui, fixed = TRUE) && grepl(".shared_bridge", srv, fixed = TRUE))
check("models no longer copy each other's fields", !grepl(".watch_bridge", srv, fixed = TRUE))

# Model Dashboard order: SGR → BETA → shared Cash/Debt/Shares/Revenue
# Use the last get_started tabItem (menuItem may appear earlier); match call site
# `.shared_bridge_box()` — the section:shared_bridge lock lives inside the helper.
gs_matches <- gregexpr('tabName = "get_started"', ui, fixed = TRUE)[[1]]
gs_start <- if (length(gs_matches) && gs_matches[1] > 0L) gs_matches[length(gs_matches)] else -1L
gs_end <- if (gs_start > 0L) regexpr('tabName = "snapshot"', substr(ui, gs_start, nchar(ui)), fixed = TRUE)[1] else -1L
gs_chunk <- if (gs_start > 0L && gs_end > 0L) {
  substr(ui, gs_start, gs_start + gs_end - 1L)
} else {
  ""
}
pos_beta <- if (nzchar(gs_chunk)) regexpr("section:beta_box", gs_chunk, fixed = TRUE)[1] else -1L
pos_bridge <- if (nzchar(gs_chunk)) regexpr(".shared_bridge_box()", gs_chunk, fixed = TRUE)[1] else -1L
check("get_started has BETA and shared inputs", pos_beta > 0L && pos_bridge > 0L)
check("BETA block sits above shared inputs", pos_beta > 0L && pos_bridge > pos_beta)

# Plain-language locale (no jargon title 「共用橋接」/ "Shared bridge")
loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "en shared_bridge_title is plain",
  grepl('shared_bridge_title = "Cash, Debt, Shares & Revenue \\(shared\\)"', loc)
)
check(
  "zh-TW shared_bridge_title is plain",
  grepl("shared_bridge_title = \"Cash、Debt、股數與營收（各模型共用）\"", loc, fixed = TRUE)
)
check("no jargon UI title 共用橋接", !grepl('shared_bridge_title = "共用橋接"', loc, fixed = TRUE))
check("no jargon UI title Shared bridge", !grepl('shared_bridge_title = "Shared bridge"', loc, fixed = TRUE))
check(
  "default box title matches zh-TW plain copy",
  grepl("Cash、Debt、股數與營收（各模型共用）", ui, fixed = TRUE)
)

if (fail > 0L) quit(status = 1)
cat("ALL PASS\n")
