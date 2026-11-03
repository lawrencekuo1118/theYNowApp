#!/usr/bin/env Rscript
# Offline tests: cited supply-chain and orders ledger.
# YNOW_DEBUG_SKIP_PY=1 — no reticulate.

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
app_dir <- normalizePath(file.path(".."), mustWork = TRUE)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
source(file.path(app_dir, "earnings_call_keyword_lexicon.R"), local = FALSE, encoding = "UTF-8")
source(file.path(app_dir, "earnings_call_keyword_framework.R"), local = FALSE, encoding = "UTF-8")
source(file.path(app_dir, "supply_chain_order_report.R"), local = FALSE, encoding = "UTF-8")
source(file.path(app_dir, "ui_locale.R"), local = FALSE, encoding = "UTF-8")

pass <- 0L
fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

html_of <- function(ui) paste(as.character(ui), collapse = " ")

numeric_leaves <- function(x, path = "report") {
  hits <- character(0)
  if (is.numeric(x)) return(path)
  if (is.list(x)) {
    nms <- names(x)
    for (i in seq_along(x)) {
      nm <- if (!is.null(nms) && nzchar(nms[[i]])) nms[[i]] else as.character(i)
      hits <- c(hits, numeric_leaves(x[[i]], paste0(path, "$", nm)))
    }
  }
  hits
}

focus <- list(
  order_notes = c(
    "在手訂單能見度維持約一個季度。",
    "客戶庫存持續調整，不是訂單事實。",
    "新訂單一。",
    "新訂單二。",
    "新訂單三。",
    "新訂單四。",
    "新訂單五。",
    "新訂單六超出上限。"
  ),
  supplier_notes = "主要供應商交期已回穩。",
  customer_notes = character(0)
)
src <- list(
  source_id = "fugle_earnings_memo",
  published = "2026-02-01",
  info_updated_at = "2026-02-01",
  info_updated_kind = "source_event",
  backlog = 1.25e9,
  bookings = 10,
  capex = 3,
  excerpt = "do not copy"
)
sealed <- ynow_supply_report_from_focus(focus, source = src)
check("from_focus keeps five order cites in extractor order", {
  identical(
    sealed$buckets$orders,
    c(
      "在手訂單能見度維持約一個季度。",
      "新訂單一。",
      "新訂單二。",
      "新訂單三。",
      "新訂單四。"
    )
  )
})
check("from_focus drops inventory fluff and the sixth cite", {
  blob <- paste(sealed$buckets$orders, collapse = " ")
  !grepl("客戶庫存", blob, fixed = TRUE) &&
    !grepl("新訂單五", blob, fixed = TRUE) &&
    !grepl("六超出", blob, fixed = TRUE)
})
check("from_focus copies only source dates and id", {
  identical(sealed$source_id, "fugle_earnings_memo") &&
    identical(sealed$published, "2026-02-01") &&
    identical(sealed$info_updated_at, "2026-02-01") &&
    identical(sealed$info_updated_kind, "source_event") &&
    identical(sealed$schema, 1L) &&
    identical(sealed$lexicon_version, "3") &&
    identical(sealed$buckets$suppliers, "主要供應商交期已回穩。") &&
    !any(c("backlog", "bookings", "capex", "excerpt") %in% names(sealed))
})
check("sealed report numeric leaf is schema only", {
  identical(numeric_leaves(sealed), "report$schema")
})

poison <- list(
  schema = 99L,
  backlog = 9,
  top1_share = 0.42,
  excerpt = "hidden",
  buckets = list(
    orders = c("在手訂單維持。", "庫存消化中。"),
    mystery = "nope",
    customers = 123
  ),
  filing = list(customers = "concentrated", suppliers = "table")
)
carried <- ynow_supply_report_carry(poison)
check("carry drops forbidden fields, unknown buckets, and numeric cites", {
  identical(carried$schema, 1L) &&
    identical(carried$buckets$orders, "在手訂單維持。") &&
    length(carried$buckets$customers) == 0L &&
    is.null(carried$buckets$mystery) &&
    identical(carried$filing$customers, "absent") &&
    identical(carried$filing$suppliers, "table") &&
    !any(.YNOW_SUPPLY_FORBIDDEN_FIELDS %in% names(carried)) &&
    !grepl("123", paste(unlist(carried$buckets), collapse = " "), fixed = TRUE) &&
    !grepl("0.42", paste(capture.output(dput(carried)), collapse = " "), fixed = TRUE)
})
check("missing report becomes an empty sealed ledger", {
  empty <- ynow_supply_report_carry(NULL)
  is.list(empty) &&
    identical(empty$schema, 1L) &&
    all(vapply(empty$buckets, length, integer(1)) == 0L) &&
    identical(empty$filing$customers, "absent") &&
    identical(empty$filing$suppliers, "absent") &&
    !isTRUE(ynow_supply_report_has_content(NULL))
})

oc <- list(
  customer_concentration = list(
    eligible = TRUE,
    rows = list(list(name = "A", share = 0.42, revenue = 42)),
    top1_share = 0.42,
    top3_share = 0.88,
    denominator = 100
  ),
  supplier_concentration = list(
    eligible = FALSE,
    method = "threshold_none_above",
    top1_share = 0.99,
    rows = list()
  ),
  market_geography = list(
    eligible = TRUE,
    slices = list(list(name = "US", share = 0.77))
  )
)
filed <- ynow_supply_report_with_filing(NULL, oc)
check("filing enum ignores shares and geography", {
  identical(filed$filing$customers, "table") &&
    identical(filed$filing$suppliers, "threshold") &&
    length(filed$buckets$orders) == 0L &&
    !grepl("0.42|0.77|0.99|42", paste(capture.output(dput(filed)), collapse = " "))
})
check("with_filing is idempotent and keeps cites", {
  again <- ynow_supply_report_with_filing(sealed, oc)
  twice <- ynow_supply_report_with_filing(again, oc)
  identical(again$buckets, sealed$buckets) &&
    identical(again$filing, filed$filing) &&
    identical(again, twice)
})
check("parser-pending concentration stays absent", {
  pending <- ynow_supply_report_with_filing(NULL, list(
    customer_concentration = list(eligible = FALSE, rows = list(), codes = "ORDERS_CUSTOMER_DISCLOSURE_MISSING"),
    supplier_concentration = list(eligible = FALSE)
  ))
  identical(pending$filing$customers, "absent") &&
    identical(pending$filing$suppliers, "absent") &&
    !isTRUE(ynow_supply_report_has_content(pending))
})
check("filing-only ledger still has content", isTRUE(ynow_supply_report_has_content(filed)))

if (!requireNamespace("shiny", quietly = TRUE)) {
  check("shiny is installed for report UI", FALSE)
} else {
  suppressPackageStartupMessages(library(shiny))
  check("empty ledger renders nothing", is.null(ynow_supply_report_ui(NULL, "en")))
  mentioned <- ynow_supply_report_from_focus(list(
    order_notes = "在手訂單能見度維持約一個季度。",
    supplier_notes = "主要供應商交期已回穩。",
    customer_notes = "主要客戶占營收約三成。"
  ), source = src)
  html_en <- html_of(ynow_supply_report_ui(mentioned, "en"))
  html_zh <- html_of(ynow_supply_report_ui(mentioned, "zh-TW"))
  check("mentioned stance uses the status template in both locales", {
    grepl("data-bucket=\"orders\"", html_en, fixed = TRUE) &&
      grepl("data-stance=\"mentioned\"", html_en, fixed = TRUE) &&
      grepl("data-bucket=\"suppliers\"", html_en, fixed = TRUE) &&
      grepl("data-bucket=\"customers\"", html_en, fixed = TRUE) &&
      grepl("The latest call mentions orders, backlog, or bookings.", html_en, fixed = TRUE) &&
      grepl("最新法說提到訂單、在手訂單或 bookings。", html_zh, fixed = TRUE) &&
      grepl("僅狀態。此處不重述金額。", html_zh, fixed = TRUE) &&
      grepl("在手訂單能見度維持約一個季度。", html_en, fixed = TRUE) &&
      grepl("2026-02-01", html_en, fixed = TRUE) &&
      !grepl("ynow-bblab-or__ec-kw", html_en, fixed = TRUE)
  })
  mixed <- ynow_supply_report_carry(list(
    buckets = list(
      orders = c("在手訂單能見度維持約一個季度。", "客戶砍單並延期出貨。"),
      suppliers = c("主要供應商交期已回穩。", "上游缺料，產能瓶頸未解。"),
      customers = "主要客戶砍單。"
    ),
    info_updated_at = "2026-02-01",
    info_updated_kind = "source_event"
  ))
  html_mixed <- html_of(ynow_supply_report_ui(mixed, "en"))
  check("mixed and constrained stances follow constraint tokens", {
    grepl("data-bucket=\"orders\" data-stance=\"mixed\"", html_mixed, fixed = TRUE) &&
      grepl("data-bucket=\"suppliers\" data-stance=\"mixed\"", html_mixed, fixed = TRUE) &&
      grepl("data-bucket=\"customers\" data-stance=\"mentioned\"", html_mixed, fixed = TRUE) &&
      grepl("both order status and an order constraint", html_mixed, fixed = TRUE) &&
      grepl("主要客戶砍單。", html_mixed, fixed = TRUE)
  })
  only_cut <- ynow_supply_report_carry(list(
    buckets = list(orders = "本季砍單，訂單取消。", suppliers = "缺料。")
  ))
  html_cut <- html_of(ynow_supply_report_ui(only_cut, "zh-TW"))
  check("every cite constrained selects the constrained template", {
    grepl("data-bucket=\"orders\" data-stance=\"constrained\"", html_cut, fixed = TRUE) &&
      grepl("data-bucket=\"suppliers\" data-stance=\"constrained\"", html_cut, fixed = TRUE) &&
      grepl("最新法說提到訂單受限", html_cut, fixed = TRUE) &&
      grepl("本季砍單，訂單取消。", html_cut, fixed = TRUE) &&
      !grepl("data-bucket=\"customers\"", html_cut, fixed = TRUE)
  })
  html_file <- html_of(ynow_supply_report_ui(filed, "en"))
  check("filing note names the table and does not print the share", {
    grepl("data-filing=\"customers\"", html_file, fixed = TRUE) &&
      grepl("data-filing-state=\"table\"", html_file, fixed = TRUE) &&
      grepl("data-filing=\"suppliers\"", html_file, fixed = TRUE) &&
      grepl("data-filing-state=\"threshold\"", html_file, fixed = TRUE) &&
      grepl("Shares stay on that chart.", html_file, fixed = TRUE) &&
      grepl("no supplier is above its disclosed concentration threshold.", html_file, fixed = TRUE) &&
      !grepl("42", html_file, fixed = TRUE) &&
      !grepl("0.77", html_file, fixed = TRUE) &&
      !grepl("data-bucket=\"orders\"", html_file, fixed = TRUE)
  })
  five <- ynow_supply_report_carry(list(buckets = list(
    orders = c("新訂單一。", "新訂單二。", "新訂單三。", "新訂單四。", "新訂單五。")
  )))
  html_five <- html_of(ynow_supply_report_ui(five, "en"))
  check("UI shows every stored cite", {
    grepl("新訂單一。", html_five, fixed = TRUE) &&
      grepl("新訂單五。", html_five, fixed = TRUE)
  })
}

lex <- ynow_ec_keyword_lexicon()
check("surface item kinds stay inside the seal set", {
  kinds <- unique(unlist(lapply(c("orders", "suppliers", "customers"), function(role) {
    vapply(lex[[role]]$items, function(it) as.character(it$kind), character(1))
  }), use.names = FALSE))
  length(kinds) > 0L && all(kinds %in% .YNOW_SUPPLY_ITEM_KINDS)
})
reachable_key <- function(role, stance) {
  paste0("bblab_or_supply_report_", role, "_", stance)
}
check("both locales have a template for each reachable stance", {
  roles <- c("orders", "suppliers", "customers")
  keys <- unlist(lapply(roles, function(role) {
    cons <- ynow_ec_lexicon_tokens(role, "constraint", lex)
    stances <- if (length(cons)) c("mentioned", "constrained", "mixed") else "mentioned"
    vapply(stances, function(st) reachable_key(role, st), character(1))
  }))
  filing_keys <- c(
    "bblab_or_supply_report_filing_customers_table",
    "bblab_or_supply_report_filing_customers_threshold",
    "bblab_or_supply_report_filing_suppliers_table",
    "bblab_or_supply_report_filing_suppliers_threshold",
    "bblab_or_supply_report_title",
    "bblab_or_supply_report_status_only"
  )
  keys <- c(keys, filing_keys)
  all(vapply(keys, function(k) {
    en <- .UI_STRINGS$en[[k]]
    zh <- .UI_STRINGS$`zh-TW`[[k]]
    is.character(en) && nzchar(en) && is.character(zh) && nzchar(zh) &&
      !grepl("[\u4e00-\u9fff]", en, perl = TRUE)
  }, logical(1))) &&
    is.null(.UI_STRINGS$en$bblab_or_supply_report_customers_constrained) &&
    is.null(.UI_STRINGS$en$bblab_or_supply_report_customers_mixed)
})

mod <- paste(readLines(file.path(app_dir, "business_breakdown_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("module does not reach past the report helpers", {
  !grepl(".ynow_supply_report_", mod, fixed = TRUE) &&
    !grepl("ynow_ec_kw_extract_order_focus", mod, fixed = TRUE)
})
g <- paste(readLines(file.path(app_dir, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("global sources the report after the framework", {
  li <- regexpr('source("earnings_call_keyword_lexicon.R"', g, fixed = TRUE)
  fi <- regexpr('source("earnings_call_keyword_framework.R"', g, fixed = TRUE)
  ri <- regexpr('source("supply_chain_order_report.R"', g, fixed = TRUE)
  ui <- regexpr('source("us_earnings_call.R"', g, fixed = TRUE)
  li > 0L && fi > li && ri > fi && ui > ri
})

cat("\nPassed: ", pass, " Failed: ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
quit(status = 0L)
