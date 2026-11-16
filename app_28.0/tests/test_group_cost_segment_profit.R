#!/usr/bin/env Rscript
# Group cost attribution & segment profitability (TYA-128).
# Run: cd app_28.0 && Rscript tests/test_group_cost_segment_profit.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) dirname(normalizePath(file_arg)) else getwd()
root <- if (file.exists(file.path(test_dir, "..", "business_breakdown_engine.R"))) {
  normalizePath(file.path(test_dir, ".."))
} else if (file.exists("business_breakdown_engine.R")) {
  normalizePath(".")
} else if (dir.exists("app_28.0") && file.exists("app_28.0/business_breakdown_engine.R")) {
  normalizePath("app_28.0")
} else stop("Cannot locate business_breakdown_engine.R")
setwd(root)

source("setup.R", local = TRUE, encoding = "UTF-8")
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_schema.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_engine.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_cost_profit.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_structure.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "") else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

conglomerate_fixture <- function() {
  list(
    ticker = "CONG",
    entity = list(name = "Conglomerate Fixture", industry = "Industrial manufacturing"),
    statement_currency = "USD",
    period = "2024",
    consolidated = bblab_consolidated(
      revenue = 1000, cor = 600, gp = 400, currency = "USD", period = "2024",
      operating_income = 180
    ),
    dimensions = list(bblab_dimension(
      "op", "operating_segment",
      list(
        bblab_component(
          "mfg", "Manufacturing", revenue = 550, cor = 360,
          operating_income = 110, is_reported_segment = TRUE,
          period = "2024", currency = "USD"
        ),
        bblab_component(
          "svc", "Services", revenue = 350, cor = 140,
          operating_income = 90, is_reported_segment = TRUE,
          period = "2024", currency = "USD"
        ),
        bblab_component(
          "corp", "Corporate / Unallocated", revenue = 0,
          operating_income = -30, classification = "UNALLOCATED",
          period = "2024", currency = "USD"
        ),
        bblab_component(
          "elim", "Intercompany eliminations", revenue = -20,
          classification = "ELIMINATION",
          period = "2024", currency = "USD"
        ),
        bblab_component(
          "recon", "Segment reconciliation", revenue = 120,
          classification = "RECONCILIATION",
          period = "2024", currency = "USD"
        )
      ),
      mutually_exclusive = TRUE,
      filed_audited = TRUE,
      flags = c("mutually_exclusive", "separate_revenue", "attributable_cor_gp", "reconciles")
    ))
  )
}

# ---- Industry profiles ----
check("industry manufacturing",
      identical(bblab_cost_profit_industry_profile("ind.Machinery", ""), "manufacturing"))
check("industry saas",
      identical(bblab_cost_profit_industry_profile("saas.SaaS_Cloud", ""), "saas"))
check("industry retail",
      identical(bblab_cost_profit_industry_profile("", "Retail supermarket chain"), "retail"))
check("industry holding",
      identical(bblab_cost_profit_industry_profile("", "Investment holding company"), "holding"))
check("industry financial",
      identical(bblab_cost_profit_industry_profile("fin.Bank", ""), "financial"))
check("industry other default",
      identical(bblab_cost_profit_industry_profile("", ""), "other"))

# ---- Cost class taxonomy ----
check("direct from reported cor",
      identical(bblab_classify_cost_attribution(list(cor_method = "reported_cor", cor = 10)), "DIRECT"))
check("shared needs disclosed allocation",
      identical(bblab_classify_cost_attribution(list(
        cor_method = "company_allocation", company_allocated_cor = 5,
        allocation_basis = "headcount"
      )), "SHARED"))
check("corporate unallocated",
      identical(bblab_classify_cost_attribution(list(
        classification = "UNALLOCATED", economic_role = "CORPORATE"
      )), "CORPORATE_UNALLOCATED"))
check("intercompany elimination",
      identical(bblab_classify_cost_attribution(list(classification = "ELIMINATION")), "INTERCOMPANY"))
check("bare revenue_share without basis is UNAVAILABLE",
      identical(bblab_classify_cost_attribution(list(cor_method = "revenue_share")), "UNAVAILABLE"))

# ---- No fake Segment Gross Margin without COGS ----
no_cogs <- list(
  ticker = "NC", entity = list(name = "No COGS"),
  statement_currency = "USD", period = "2024",
  consolidated = bblab_consolidated(200, 80, 120, "USD", "2024", operating_income = 40),
  dimensions = list(bblab_dimension(
    "op", "operating_segment",
    list(
      bblab_component("a", "A", revenue = 120, operating_income = 25,
                      is_reported_segment = TRUE, period = "2024", currency = "USD"),
      bblab_component("b", "B", revenue = 80, operating_income = 15,
                      is_reported_segment = TRUE, period = "2024", currency = "USD")
    ),
    mutually_exclusive = TRUE, filed_audited = TRUE,
    flags = c("mutually_exclusive", "separate_revenue", "reconciles")
  ))
)
r_nc <- bblab_analyze(no_cogs)
sp_nc <- r_nc$segment_profitability
check("no fake GM without Segment COGS",
      isTRUE(sp_nc$ok) &&
        all(vapply(sp_nc$segments, function(s) identical(s$gm_status, "UNAVAILABLE"), logical(1))) &&
        all(vapply(sp_nc$segments, function(s) !is.finite(s$gm), logical(1))) &&
        all(vapply(sp_nc$segments, function(s) identical(s$oi_status, "REPORTED"), logical(1))))

# ---- Conglomerate: Direct costs, bridges, corporate separate ----
r <- bblab_analyze(conglomerate_fixture())
check("analyze attaches cost-profit pack",
      isTRUE(r$ok) &&
        !is.null(r$segment_profitability) &&
        !is.null(r$revenue_bridge) &&
        !is.null(r$operating_profit_bridge) &&
        isTRUE(r$cost_attribution_summary$forbid_bare_revenue_share))
check("direct cost class on reported segments",
      length(r$businesses) >= 2L &&
        all(vapply(r$businesses, function(c) identical(c$cost_class, "DIRECT"), logical(1))))
check("corporate not in businesses",
      !any(grepl("Corporate|Eliminat|reconcil",
                 vapply(r$businesses, function(c) c$name, character(1)),
                 ignore.case = TRUE)))
check("revenue bridge reconciles",
      isTRUE(r$revenue_bridge$ok) && isTRUE(r$revenue_bridge$pass))
check("op bridge uses disclosed OI only",
      isTRUE(r$operating_profit_bridge$ok) &&
        isTRUE(r$operating_profit_bridge$any_segment_oi) &&
        any(vapply(r$operating_profit_bridge$steps, function(s) {
          identical(s$role, "CORPORATE") || identical(s$role, "CONSOLIDATED")
        }, logical(1))))
check("segment profitability status ladder labels",
      all(vapply(r$segment_profitability$segments, function(s) {
        s$revenue_status %in% BBLAB_PROFIT_STATUS_LADDER &&
          s$cor_status %in% BBLAB_PROFIT_STATUS_LADDER &&
          s$oi_status %in% BBLAB_PROFIT_STATUS_LADDER
      }, logical(1))))
check("valuation hook rejects estimated FCF as disclosed",
      all(vapply(r$businesses, function(c) {
        isFALSE(c$valuation_safe$use_segment_fcf)
      }, logical(1))))

# ---- Holding lens withholds Gross Margin invent ----
hold_pack <- .bblab_segment_gp_pack(
  list(revenue = 100, cor = NA_real_, gp = NA_real_),
  industry_profile = "holding"
)
check("holding lens withholds GM",
      identical(hold_pack$gm_status, "UNAVAILABLE") && !is.finite(hold_pack$gm))

# ---- Single-business path unchanged ----
single <- list(
  ticker = "ONE", entity = list(name = "Single Co"),
  statement_currency = "USD", period = "2024",
  consolidated = bblab_consolidated(100, 40, 60, "USD", "2024", operating_income = 20),
  dimensions = list(bblab_dimension(
    "op", "operating_segment",
    list(bblab_component("only", "Only Biz", revenue = 100, cor = 40,
                         operating_income = 20, is_reported_segment = TRUE,
                         period = "2024", currency = "USD")),
    mutually_exclusive = TRUE, filed_audited = TRUE,
    flags = c("mutually_exclusive", "separate_revenue", "attributable_cor_gp", "reconciles")
  ))
)
rs <- bblab_analyze(single)
check("single-business path still ok",
      isTRUE(rs$ok) && length(rs$businesses) == 1L &&
        identical(rs$businesses[[1]]$cost_class, "DIRECT") &&
        isTRUE(rs$revenue_bridge$ok))

# ---- i18n keys present ----
check("i18n en segment profit keys",
      nzchar(.UI_STRINGS$en$bblab_seg_profit_title) &&
        nzchar(.UI_STRINGS$en$bblab_bridges_title) &&
        nzchar(.UI_STRINGS$en$bblab_status_reported))
check("i18n zh-TW segment profit keys",
      nzchar(.UI_STRINGS$`zh-TW`$bblab_seg_profit_title) &&
        nzchar(.UI_STRINGS$`zh-TW`$bblab_bridges_title) &&
        grepl("部門", .UI_STRINGS$`zh-TW`$bblab_seg_profit_title))

# ---- Config forbids bare revenue-share by default ----
cfg <- bblab_load_config()
check("config forbids bare revenue-share fallback",
      isFALSE(cfg$flags$revenue_share_cost_is_final_fallback))

if (fail > 0L) {
  cat("FAILED ", fail, " check(s)\n", sep = "")
  quit(status = 1L)
}
cat("All group cost / segment profitability checks passed.\n")
