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
check("empty focus is three gaps", {
  ids <- vapply(empty$focus, function(b) b$id, character(1))
  identical(ids, c("orders", "suppliers", "customers")) &&
    all(vapply(empty$focus, function(b) identical(b$status, "gap"), logical(1)))
})
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

focused <- bblab_build_order_research_pack(
  order_concentration = list(
    customer_concentration = list(
      eligible = TRUE, source = "sec_concentration_note",
      rows = list(list(name = "Customer A", share = 0.22, revenue = 220))
    ),
    supplier_concentration = list(
      eligible = TRUE, source = "mops_annual_report",
      rows = list(list(name = "Supplier B", share = 0.15, revenue = 90))
    )
  ),
  consolidated = bblab_consolidated(1000, 400, 600, "USD", "2024"),
  disclosures = list(
    backlog = 500,
    earnings_call = list(
      backlog = 540,
      suppliers = list(list(name = "Call Foundry", share = 0.40)),
      customers = list(list(name = "Call Customer", share = 0.18))
    )
  ),
  frequency = "annual"
)
check("focus keeps filing backlog and call backlog", {
  facts <- focused$focus[[1]]$facts
  vals <- vapply(facts, function(f) f$value, numeric(1))
  srcs <- vapply(facts, function(f) f$source, character(1))
  any(abs(vals - 500) < 1e-9 & srcs == "mops_filing") &&
    any(abs(vals - 540) < 1e-9 & srcs == "earnings_call")
})
check("supplier bucket keeps filing and earnings call", {
  facts <- focused$focus[[2]]$facts
  nms <- vapply(facts, function(f) f$name, character(1))
  srcs <- vapply(facts, function(f) f$source, character(1))
  "Supplier B" %in% nms && "Call Foundry" %in% nms &&
    any(srcs == "mops_filing") && any(srcs == "earnings_call")
})
check("customer revenue mix is the customer bucket", {
  facts <- focused$focus[[3]]$facts
  hit <- Filter(function(f) identical(f$name, "Customer A"), facts)[[1]]
  near(hit$value, 0.22) && identical(hit$unit, "ratio") && identical(hit$source, "sec_filing") &&
    any(vapply(facts, function(f) identical(f$name, "Call Customer") &&
                 identical(f$source, "earnings_call"), logical(1)))
})

stack_ids <- c(
  "mops_material", "mops_filing", "sec_filing", "company_ir",
  "earnings_call", "statement_proxy", "broker_estimate", "industry_proxy"
)
stack_pri <- c(1L, 2L, 2L, 3L, 4L, 5L, 6L, 7L)
stack_fill <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
xcheck_ids <- c(
  "contract_liability", "rpo", "contract_assets", "inventory",
  "accounts_receivable", "operating_cash_flow"
)
assert_stack <- function(pack) {
  length(pack$source_stack) == 8L &&
    identical(vapply(pack$source_stack, function(t) t$id, character(1)), stack_ids) &&
    identical(vapply(pack$source_stack, function(t) as.integer(t$priority), integer(1)), stack_pri) &&
    identical(vapply(pack$source_stack, function(t) isTRUE(t$can_fill_amount), logical(1)), stack_fill)
}
tier_contributed <- function(pack, id) {
  hit <- Filter(function(t) identical(t$id, id), pack$source_stack)
  length(hit) == 1L && isTRUE(hit[[1]]$contributed)
}
check("empty pack source stack matches the registry", assert_stack(empty) &&
        !any(vapply(empty$source_stack, function(t) isTRUE(t$contributed), logical(1))))
check("empty cross-check is six unavailable proxies", {
  xc <- empty$cross_check
  length(xc) == 6L &&
    identical(vapply(xc, function(item) item$id, character(1)), xcheck_ids) &&
    all(vapply(xc, function(item) {
      identical(item$source, "statement_proxy") &&
        identical(item$status, "UNAVAILABLE") &&
        !is.finite(item$value)
    }, logical(1)))
})
check("built pack source stack matches the registry", assert_stack(focused) && assert_stack(ranged))
check("focus facts mark only the tiers they use", {
  tier_contributed(focused, "mops_filing") &&
    tier_contributed(focused, "sec_filing") &&
    tier_contributed(focused, "earnings_call") &&
    !tier_contributed(focused, "broker_estimate") &&
    !tier_contributed(focused, "industry_proxy") &&
    !tier_contributed(focused, "statement_proxy") &&
    !tier_contributed(ranged, "broker_estimate") &&
    !tier_contributed(ranged, "statement_proxy")
})
check("statement lines cross-check and do not fill order amounts", {
  xc <- ranged$cross_check
  by_id <- stats::setNames(xc, vapply(xc, function(item) item$id, character(1)))
  near(by_id$contract_liability$value, 4) &&
    near(by_id$inventory$value, 9) &&
    near(by_id$accounts_receivable$value, 12) &&
    near(by_id$operating_cash_flow$value, 18) &&
    near(by_id$rpo$value, 150) &&
    identical(by_id$rpo$source, "statement_proxy") &&
    identical(by_id$rpo$credibility, "proxy") &&
    !is.finite(by_id$contract_assets$value) &&
    all(vapply(ranged$focus[[1]]$facts, function(f) {
      !identical(f$source, "statement_proxy") &&
        !identical(f$source, "broker_estimate") &&
        !identical(f$source, "industry_proxy")
    }, logical(1))) &&
    any(vapply(ranged$focus[[1]]$facts, function(f) {
      near(f$value, 150) && identical(f$source, "mops_filing")
    }, logical(1)))
})
proxy_only <- bblab_build_order_research_pack(
  consolidated = bblab_consolidated(100, 40, 60, "USD", "2024"),
  balance_sheet = data.frame(
    Metric = c("Accounts Receivable", "Inventory", "Current Deferred Revenue"),
    Y2024 = c(12, 9, 4),
    stringsAsFactors = FALSE
  ),
  cash_flow = data.frame(
    Metric = "Operating Cash Flow",
    Y2024 = 18,
    stringsAsFactors = FALSE
  ),
  segment_tables = list(list(
    kind = "orders",
    source = "broker_estimate",
    headers = c("Customer", "2024"),
    rows = list(c("Guess Co", "999"))
  ), list(
    kind = "backlog",
    source = "industry_proxy",
    headers = c("Item", "2023", "2024"),
    rows = list(c("Guess backlog", "10", "80"))
  )),
  frequency = "annual"
)
check("proxy and estimate tables do not fill focus order amounts", {
  facts <- proxy_only$focus[[1]]$facts
  nms <- if (length(facts)) vapply(facts, function(f) f$name, character(1)) else character(0)
  vals <- if (length(facts)) vapply(facts, function(f) f$value, numeric(1)) else numeric(0)
  identical(proxy_only$focus[[1]]$status, "gap") &&
    !"Guess Co" %in% nms &&
    !any(abs(vals - 999) < 1e-9) &&
    !any(abs(vals - 80) < 1e-9) &&
    is.na(proxy_only$kpis$backlog_yoy$value) &&
    is.na(proxy_only$kpis$customer_concentration$value) &&
    is.na(proxy_only$kpis$backlog_revenue_cover$value) &&
    is.na(proxy_only$kpis$book_to_bill$value) &&
    !tier_contributed(proxy_only, "broker_estimate") &&
    !tier_contributed(proxy_only, "industry_proxy") &&
    !tier_contributed(proxy_only, "statement_proxy") &&
    near(proxy_only$cross_check[[4]]$value, 9) &&
    identical(proxy_only$cross_check[[4]]$id, "inventory")
})

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
  check("panel leads with the three buckets", {
    primary <- sub("ynow-bblab-or__secondary.*", "", html_en)
    grepl("Order quantity", primary, fixed = TRUE) &&
      grepl("Major suppliers", primary, fixed = TRUE) &&
      grepl("Major-customer revenue mix", primary, fixed = TRUE) &&
      !grepl("Gross margin", primary, fixed = TRUE) &&
      grepl("ynow-bblab-or__secondary", html_en, fixed = TRUE) &&
      !grepl("Bear", html_en, fixed = TRUE) &&
      !grepl("Bull", html_en, fixed = TRUE)
  })
  html_orders_only <- paste(
    as.character(bblab_order_research_focus_ui(ranged, "en", TRUE, bucket_id = "orders")),
    collapse = " "
  )
  html_meta_only <- paste(
    as.character(bblab_order_research_meta_ui(ranged, "en", TRUE)),
    collapse = " "
  )
  check("focus helper isolates one bucket", {
    grepl("Order quantity", html_orders_only, fixed = TRUE) &&
      !grepl("Major suppliers", html_orders_only, fixed = TRUE) &&
      !grepl("Sources and credibility", html_orders_only, fixed = TRUE)
  })
  check("meta helper has sources without focus buckets", {
    meta_primary <- sub("ynow-bblab-or__secondary.*", "", html_meta_only)
    grepl("Sources and credibility", meta_primary, fixed = TRUE) &&
      grepl("Cross-check", meta_primary, fixed = TRUE) &&
      !grepl("data-focus=\"orders\"", html_meta_only, fixed = TRUE) &&
      !grepl("Order quantity", meta_primary, fixed = TRUE) &&
      !grepl("Gross margin", meta_primary, fixed = TRUE) &&
      grepl("ynow-bblab-or__secondary", html_meta_only, fixed = TRUE)
  })
  html_focus <- paste(as.character(bblab_order_research_panel_ui(focused, "en", TRUE)), collapse = " ")
  check("panel labels earnings-call provenance",
        grepl("Earnings call", html_focus, fixed = TRUE) &&
          grepl("Call Foundry", html_focus, fixed = TRUE) &&
          grepl("Customer A", html_focus, fixed = TRUE) &&
          grepl("SEC filing", html_focus, fixed = TRUE) &&
          grepl("Confirmed", html_focus, fixed = TRUE) &&
          grepl("Management", html_focus, fixed = TRUE))
  check("panel shows sources and cross-check before secondary layers", {
    primary <- sub("ynow-bblab-or__secondary.*", "", html_en)
    grepl("Sources and credibility", primary, fixed = TRUE) &&
      grepl("Cross-check", primary, fixed = TRUE) &&
      grepl("It is not Backlog", primary, fixed = TRUE) &&
      grepl("MOPS filing", primary, fixed = TRUE) &&
      grepl("ynow-bblab-or__secondary", html_en, fixed = TRUE) &&
      !grepl("Gross margin", primary, fixed = TRUE)
  })
  check("broker tier stays unused unless a fact used it", {
    broker <- sub(
      ".*data-tier=\"broker_estimate\"(.*?)data-tier=\"industry_proxy\".*",
      "\\1",
      html_en
    )
    grepl("Broker estimate", broker, fixed = TRUE) &&
      grepl("Not used", broker, fixed = TRUE) &&
      grepl("Cannot fill an order amount", broker, fixed = TRUE) &&
      !grepl("Contributed", broker, fixed = TRUE) &&
      !tier_contributed(ranged, "broker_estimate")
  })
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
