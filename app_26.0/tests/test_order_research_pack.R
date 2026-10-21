#!/usr/bin/env Rscript
# Order research pack — KPI math and honest empty paths.
# Run: cd app_26.0 && Rscript tests/test_order_research_pack.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) dirname(normalizePath(file_arg)) else getwd()
root <- if (file.exists(file.path(test_dir, "..", "business_breakdown_order_research.R"))) {
  normalizePath(file.path(test_dir, ".."))
} else if (file.exists("business_breakdown_order_research.R")) {
  normalizePath(".")
} else if (dir.exists("app_26.0")) {
  normalizePath("app_26.0")
} else stop("Cannot locate business_breakdown_order_research.R")
setwd(root)

source("business_breakdown_schema.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_engine.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_orders.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_order_research.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "") else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

near <- function(a, b, tol = 1e-9) {
  is.finite(a) && is.finite(b) && abs(a - b) < tol
}

empty <- bblab_build_order_research_pack()
check("empty pack has six layers", length(empty$layers) == 6L &&
        identical(vapply(empty$layers, function(l) l$key, character(1)),
                  c("scale", "quality", "conversion", "profit", "cash", "valuation")))
check("empty KPIs are NA", all(vapply(empty$kpis, function(k) is.na(k$value), logical(1))))
check("empty does not invent backlog", !is.finite(empty$kpis$backlog_yoy$value) &&
        !is.finite(empty$kpis$book_to_bill$value) &&
        !is.finite(empty$kpis$backlog_revenue_cover$value) &&
        "ORDERS_BACKLOG_UNDISCLOSED" %in% empty$gaps &&
        "ORDERS_BOOKINGS_UNDISCLOSED" %in% empty$gaps)
check("empty pack has no scenario field", is.null(empty$scenarios))
check("empty valuation layer stays a gap",
      identical(empty$layers[[6]]$status, "gap") && !length(empty$layers[[6]]$metrics))
check("empty profile is other", identical(empty$industry_profile, "other"))

k <- bblab_order_research_kpis(list(
  backlog = 120, backlog_prior = 100, bookings = 150, revenue = 100,
  ttm_revenue = 80, recognized_from_cohort = 40, cohort_original = 100,
  est_order_revenue = 80, est_fulfillment_cost = 50,
  order_amounts = c(50, 30, 20, 10, 5, 5)
))
check("backlog yoy", near(k$backlog_yoy$value, 0.20))
check("book-to-bill", near(k$book_to_bill$value, 1.5))
check("coverage uses ttm", near(k$backlog_revenue_cover$value, 120 / 80))
check("conversion", near(k$order_conversion$value, 0.40))
check("est order gross profit", near(k$est_order_gross_profit$value, 30))
check("top5 order concentration", near(k$customer_concentration$value, 115 / 120))

k0 <- bblab_order_research_kpis(list(backlog = 120, backlog_prior = 0, bookings = 10, revenue = 0))
check("zero prior backlog stays empty", is.na(k0$backlog_yoy$value))
check("zero revenue book-to-bill stays empty", is.na(k0$book_to_bill$value))

k_half <- bblab_order_research_kpis(list(
  est_order_revenue = 80, backlog = 200, gross_margin = 0.4
))
check("one profit leg does not invent gross profit", is.na(k_half$est_order_gross_profit$value))
check("margin times backlog is not order profit",
      is.na(k_half$est_order_gross_profit$value) &&
        identical(k_half$est_order_gross_profit$gap, "ORDERS_ORDER_PROFIT_UNDISCLOSED"))

rev_only <- bblab_build_order_research_pack(
  order_concentration = list(
    customer_concentration = list(
      eligible = TRUE, top1_share = 0.22, top3_share = 0.40,
      source = "sec_concentration_note", rows = list()
    )
  ),
  consolidated = bblab_consolidated(1000, 400, 600, "USD", "2024")
)
check("revenue concentration is not order-dollar KPI",
      is.na(rev_only$kpis$customer_concentration$value) &&
        "ORDERS_ORDER_DOLLARS_UNDISCLOSED" %in% rev_only$gaps)
check("revenue concentration is quality evidence", {
  ev <- rev_only$layers[[2]]$evidence
  length(ev) >= 1L && identical(ev[[1]]$basis, "revenue_not_orders") &&
    near(ev[[1]]$value, 0.22)
})
check("annual revenue covers backlog when no TTM column", {
  pack <- bblab_build_order_research_pack(
    consolidated = bblab_consolidated(100, 40, 60, "USD", "2024"),
    disclosures = list(backlog = 200),
    frequency = "annual"
  )
  near(pack$kpis$backlog_revenue_cover$value, 2) &&
    identical(pack$layers[[1]]$status, "partial")
})
check("quarterly coverage stays empty without TTM", {
  pack <- bblab_build_order_research_pack(
    consolidated = bblab_consolidated(25, 10, 15, "USD", "2024Q4"),
    disclosures = list(backlog = 200),
    frequency = "quarterly"
  )
  is.na(pack$kpis$backlog_revenue_cover$value) &&
    "ORDERS_TTM_REVENUE_UNDISCLOSED" %in% pack$gaps
})

check("semi key", identical(bblab_order_research_industry_profile("sc.Foundry"), "semi_ai"))
check("saas key", identical(bblab_order_research_industry_profile("saas.SaaS_Cloud"), "saas_software"))
check("epc key", identical(bblab_order_research_industry_profile("ind.Construction"), "eng_epc"))
check("manufacturing key", identical(bblab_order_research_industry_profile("ind.Machinery"), "manufacturing"))
check("text lens", identical(
  bblab_order_research_industry_profile("", "Semiconductors — artificial intelligence"),
  "semi_ai"
))
semi <- bblab_build_order_research_pack(industry_key = "sc.Foundry")
check("semi primary metrics", identical(semi$industry_primary, c("book_to_bill", "backlog_revenue_cover")))

ranged <- bblab_build_order_research_pack(
  consolidated = bblab_consolidated(100, 60, 40, "USD", "2024"),
  disclosures = list(
    backlog = 150, backlog_prior = 100, bookings = 110,
    recognized_from_cohort = 50, cohort_original = 150,
    next_4q_conversion = 0.9,
    scenario_ranges = list(
      bear = list(conversion = 0.25, margin = 0.30, cash = 8, valuation = 1.1),
      bull = list(conversion = 0.55, margin = 0.48, cash = 20, valuation = 1.8)
    )
  ),
  frequency = "annual",
  balance_sheet = data.frame(
    Metric = c("Accounts Receivable", "Inventory", "Current Deferred Revenue"),
    Y2024 = c(12, 9, 4),
    Y2023 = c(10, 8, 3),
    stringsAsFactors = FALSE
  ),
  cash_flow = data.frame(
    Metric = c("Operating Cash Flow", "Free Cash Flow"),
    Y2024 = c(18, 11),
    stringsAsFactors = FALSE
  )
)
check("scenario ranges and next-4q rates are ignored", is.null(ranged$scenarios))
check("disclosed gross margin stays on the profit layer", {
  ids <- vapply(ranged$layers[[4]]$metrics, function(m) m$id, character(1))
  near(ranged$layers[[4]]$metrics[[which(ids == "gross_margin")]]$value, 0.40)
})
check("operating cash flow stays on the cash layer", {
  ids <- vapply(ranged$layers[[5]]$metrics, function(m) m$id, character(1))
  near(ranged$layers[[5]]$metrics[[which(ids == "operating_cash_flow")]]$value, 18)
})
check("valuation layer is disclosed coverage only", {
  ms <- ranged$layers[[6]]$metrics
  length(ms) == 1L && identical(ms[[1]]$id, "backlog_revenue_cover") &&
    near(ms[[1]]$value, 1.5) && identical(ranged$layers[[6]]$status, "disclosed")
})
check("no EPS FCF MOS or assumed conversion rows", {
  ids <- unlist(lapply(ranged$layers, function(l) {
    vapply(l$metrics %||% list(), function(m) m$id, character(1))
  }), use.names = FALSE)
  !any(ids %in% c("fcf", "eps", "mos", "next_4q_conversion")) &&
    !any(vapply(ranged$layers[[6]]$metrics, function(m) {
      isTRUE(near(m$value, 1.8)) || isTRUE(near(m$value, 0.25))
    }, logical(1)))
})
check("statement cash items land on the cash layer", {
  ids <- vapply(ranged$layers[[5]]$metrics, function(m) m$id, character(1))
  ar <- ranged$layers[[5]]$metrics[[which(ids == "accounts_receivable")]]
  inv <- ranged$layers[[5]]$metrics[[which(ids == "inventory")]]
  def <- ranged$layers[[5]]$metrics[[which(ids == "deferred_revenue")]]
  near(ar$value, 12) && near(inv$value, 9) && near(def$value, 4) &&
    identical(ranged$layers[[5]]$status, "partial")
})
check("chart points only the finite multiples", {
  ids <- vapply(ranged$chart$multiples, function(p) p$id, character(1))
  identical(sort(ids), c("backlog_revenue_cover", "book_to_bill")) &&
    !any(vapply(ranged$chart$multiples, function(p) !is.finite(p$value), logical(1)))
})
check("yoy from disclosed backlog pair", near(ranged$kpis$backlog_yoy$value, 0.50))

filing <- bblab_build_order_research_pack(
  segment_tables = list(list(
    kind = "backlog",
    headers = c("Item", "2023", "2024"),
    rows = list(c("RPO", "400", "500"))
  ), list(
    kind = "orders",
    headers = c("Customer", "2024"),
    rows = list(c("A", "60"), c("B", "40"))
  )),
  consolidated = bblab_consolidated(200, 80, 120, "USD", "2024"),
  frequency = "annual"
)
check("filing backlog table feeds yoy", near(filing$kpis$backlog_yoy$value, 500 / 400 - 1))
check("filing order dollars feed concentration", near(filing$kpis$customer_concentration$value, 1))
check("no fake amount on missing bookings", is.na(filing$kpis$book_to_bill$value))

source("ui_locale.R", local = TRUE, encoding = "UTF-8")
or_keys <- grep("^bblab_or_|^notif_bblab_orders_(backlog|bookings|ttm|conversion|order_|growth_source|quality_terms|gross_margin|cash_item)",
                names(.UI_STRINGS$en), value = TRUE)
check("order research locale keys match",
      length(or_keys) >= 40L && all(or_keys %in% names(.UI_STRINGS$`zh-TW`)))
check("en order research has no Han", {
  blob <- paste(vapply(or_keys, function(k) paste(.UI_STRINGS$en[[k]], collapse = " "), character(1)),
                collapse = "\n")
  !grepl("[\u4e00-\u9fff]", blob, perl = TRUE)
})
check("start analysis copy",
      identical(.UI_STRINGS$en$bblab_or_start, "Start analysis") &&
        identical(.UI_STRINGS$`zh-TW`$bblab_or_start, "開始分析"))

if (requireNamespace("shiny", quietly = TRUE) && requireNamespace("plotly", quietly = TRUE)) {
  suppressPackageStartupMessages({
    library(shiny)
    library(plotly)
  })
  source("business_breakdown_module.R", local = TRUE, encoding = "UTF-8")
  html_en <- paste(as.character(bblab_order_research_panel_ui(ranged, "en", TRUE)), collapse = " ")
  check("panel shows Book-to-Bill and coverage",
        grepl("Book-to-Bill", html_en, fixed = TRUE) &&
          grepl("1.10×", html_en, fixed = TRUE) &&
          grepl("1.50×", html_en, fixed = TRUE) &&
          grepl("40.0%", html_en, fixed = TRUE) &&
          !grepl("Bear", html_en, fixed = TRUE) &&
          !grepl("Bull", html_en, fixed = TRUE) &&
          !grepl("Next-four-quarter", html_en, fixed = TRUE) &&
          !grepl(">EPS<", html_en, fixed = TRUE) &&
          !grepl(">FCF<", html_en, fixed = TRUE))
  html_idle <- paste(as.character(bblab_order_research_panel_ui(ranged, "en", FALSE)), collapse = " ")
  check("start gate hides figures until analysis",
        grepl("Start analysis", html_idle, fixed = TRUE) &&
          !grepl("1.10×", html_idle, fixed = TRUE))
  html_empty <- paste(as.character(bblab_order_research_panel_ui(empty, "zh-TW", TRUE)), collapse = " ")
  check("empty zh-TW panel has no invented multiple",
        grepl("未揭露", html_empty, fixed = TRUE) &&
          !grepl("×", html_empty, fixed = TRUE))
  fig <- bblab_order_research_bar(ranged, "en", "multiples", TRUE)
  check("coverage chart is plotly", inherits(fig, "plotly"))
  fig0 <- bblab_order_research_bar(empty, "en", "multiples", TRUE)
  check("empty chart has no bar values", !inherits(fig0$x$data, "list") ||
          !length(fig0$x$data) || is.null(fig0$x$data[[1]]$y))
}

if (fail > 0L) quit(status = 1)
cat("ALL PASS\n")
