#!/usr/bin/env Rscript
# Business Breakdown Lab — 26 spec tests + Lite hide / parse / no-valuation-wiring.
# Run: cd app_20.0 && Rscript tests/test_business_breakdown_lab.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) dirname(normalizePath(file_arg)) else getwd()
root <- if (file.exists(file.path(test_dir, "..", "business_breakdown_engine.R"))) {
  normalizePath(file.path(test_dir, ".."))
} else if (file.exists("business_breakdown_engine.R")) {
  normalizePath(".")
} else if (dir.exists("app_20.0") && file.exists("app_20.0/business_breakdown_engine.R")) {
  normalizePath("app_20.0")
} else stop("Cannot locate business_breakdown_engine.R")
setwd(root)

source("setup.R", local = TRUE, encoding = "UTF-8")
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_schema.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_engine.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_structure.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_module.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "") else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

engine_src <- paste(readLines("business_breakdown_engine.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
schema_src <- paste(readLines("business_breakdown_schema.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
mod_src <- paste(readLines("business_breakdown_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
server_src <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# ---- TSM / 2330 is a TEST FIXTURE only (not a template; not in the engine) ----
# Customer-platform net revenue is the strongest disclosed dimension.
# Node / process technology is a separate overlapping dimension and must not mix.
tsm_platform_fixture <- function() {
  cons <- bblab_consolidated(revenue = 1000, cor = 420, gp = 580, currency = "TWD", period = "2024")
  platforms <- list(
    bblab_component("plat_hpc", "High Performance Computing", "MAJOR",
                    revenue = 520, is_reported_segment = TRUE, distinct_economics = TRUE,
                    distinct_customers = TRUE, separately_disclosed_revenue = TRUE,
                    period = "2024", currency = "TWD", source_type = "revenue_disaggregation"),
    bblab_component("plat_phone", "Smartphone", "MAJOR",
                    revenue = 310, is_reported_segment = TRUE, distinct_economics = TRUE,
                    distinct_customers = TRUE, separately_disclosed_revenue = TRUE,
                    period = "2024", currency = "TWD", source_type = "revenue_disaggregation"),
    bblab_component("plat_iot", "Internet of Things", "MAJOR",
                    revenue = 80, is_reported_segment = TRUE, distinct_economics = TRUE,
                    separately_disclosed_revenue = TRUE, period = "2024", currency = "TWD"),
    bblab_component("plat_auto", "Automotive", "MAJOR",
                    revenue = 60, is_reported_segment = TRUE, distinct_economics = TRUE,
                    separately_disclosed_revenue = TRUE, period = "2024", currency = "TWD"),
    bblab_component("plat_dce", "Digital Consumer Electronics", "MAJOR",
                    revenue = 30, is_reported_segment = TRUE, separately_disclosed_revenue = TRUE,
                    period = "2024", currency = "TWD")
  )
  nodes <- list(
    bblab_component("node_3", "3nm", revenue = 150, period = "2024", currency = "TWD"),
    bblab_component("node_5", "5nm", revenue = 350, period = "2024", currency = "TWD"),
    bblab_component("node_7", "7nm", revenue = 500, period = "2024", currency = "TWD")
  )
  geo <- list(
    bblab_component("geo_us", "United States", revenue = 700, period = "2024", currency = "TWD"),
    bblab_component("geo_cn", "China", revenue = 150, period = "2024", currency = "TWD"),
    bblab_component("geo_tw", "Taiwan", revenue = 150, period = "2024", currency = "TWD")
  )
  list(
    ticker = "2330.TW",
    entity = list(name = "Fixture foundry (tests only)"),
    statement_currency = "TWD",
    period = "2024",
    frequency = "annual",
    instrument_type = "ADR",
    adr_ratio = NA_real_,
    consolidated = cons,
    shared_corporate = list(rd = 120, ga = 40, sm = 15, interest = 8, tax = 50, ni = 350),
    dimensions = list(
      bblab_dimension(
        "platform", "revenue_disaggregation", platforms,
        mutually_exclusive = TRUE, overlaps_with = "node_tech",
        flags = c("distinct_economics", "separate_revenue", "management_major",
                  "mutually_exclusive", "reconciles", "relevant", "filed_audited"),
        filed_audited = TRUE, period = "2024", currency = "TWD",
        label = "Net revenue by platform"
      ),
      bblab_dimension(
        "node_tech", "technology", nodes,
        mutually_exclusive = TRUE, overlaps_with = "platform",
        flags = c("separate_revenue"),
        filed_audited = TRUE, period = "2024", currency = "TWD",
        label = "Process technology"
      ),
      bblab_dimension(
        "geo", "geography", geo,
        mutually_exclusive = TRUE, is_customer_location_only = TRUE,
        flags = c("separate_revenue"),
        period = "2024", currency = "TWD",
        label = "Revenue by customer location"
      )
    )
  )
}

two_seg_level_a <- function() {
  cons <- bblab_consolidated(200, 80, 120, "USD", "2024")
  comps <- list(
    bblab_component("a", "Business A", revenue = 120, cor = 40, gp = 80,
                    is_reported_segment = TRUE, distinct_economics = TRUE,
                    period = "2024", currency = "USD"),
    bblab_component("b", "Business B", revenue = 80, cor = 40, gp = 40,
                    is_reported_segment = TRUE, distinct_economics = TRUE,
                    period = "2024", currency = "USD")
  )
  list(
    ticker = "FIXT",
    entity = list(name = "Two-segment fixture"),
    statement_currency = "USD", period = "2024",
    consolidated = cons,
    dimensions = list(bblab_dimension(
      "opseg", "operating_segment", comps, mutually_exclusive = TRUE,
      flags = c("distinct_economics", "separate_revenue", "attributable_cor_gp",
                "management_major", "mutually_exclusive", "reconciles", "relevant",
                "filed_audited"),
      filed_audited = TRUE, period = "2024", currency = "USD"
    ))
  )
}

# =====================================================================
# 1 Company-agnostic engine (no hardcoded issuer / node / platform branches)
# =====================================================================
check("01 engine has no TSM ticker branch",
      !grepl("TSM|2330|N3|N5|N7|HPC", engine_src) && !grepl("TSM|2330", schema_src))
check("01 engine has no AMZN ticker branch",
      !grepl("AMZN|Amazon", engine_src) && !grepl("AMZN|Amazon", schema_src))
check("01 yaml has no ticker list", {
  yml <- paste(readLines("business_breakdown_config.yaml", warn = FALSE), collapse = "\n")
  !grepl("tickers:", yml) && grepl("major_share: 0.10", yml) && grepl("min_display_percentage: 0.03", yml)
})

# =====================================================================
# 2 Overlapping dimensions never combined
# =====================================================================
fx <- tsm_platform_fixture()
sel <- bblab_select_primary_dimension(fx$dimensions, fx$consolidated)
check("02 overlapping blocked code",
      "BUSINESS_OVERLAPPING_DIMENSIONS_BLOCKED" %in% sel$codes)
check("02 single primary not mixed",
      identical(sel$dimension$id, "platform") && !identical(sel$dimension$id, "node_tech"))

# =====================================================================
# 3 Geography that is only customer location is not primary
# =====================================================================
geo_only <- list(bblab_dimension(
  "geo", "geography", fx$dimensions[[3]]$components,
  is_customer_location_only = TRUE, flags = c("separate_revenue", "reconciles")
))
sel_geo <- bblab_select_primary_dimension(geo_only, fx$consolidated)
check("03 geo customer-location veto",
      is.null(sel_geo$dimension) &&
        "BUSINESS_GEOGRAPHY_CUSTOMER_LOCATION_ONLY" %in% sel_geo$codes)

# =====================================================================
# 4 Disclosure priority: operating segments beat product / geo
# =====================================================================
mixed <- list(
  bblab_dimension("geo2", "geography", list(bblab_component("g", "G", revenue = 200)),
                  flags = c("separate_revenue", "reconciles", "relevant")),
  bblab_dimension("prod", "product_service", list(bblab_component("p", "P", revenue = 200)),
                  flags = c("separate_revenue", "reconciles", "relevant")),
  bblab_dimension("op", "operating_segment",
                  list(bblab_component("s", "S", revenue = 200, cor = 80, is_reported_segment = TRUE)),
                  flags = c("separate_revenue", "attributable_cor_gp", "reconciles",
                            "relevant", "filed_audited", "mutually_exclusive",
                            "distinct_economics", "management_major"),
                  filed_audited = TRUE)
)
sel_p <- bblab_select_primary_dimension(mixed, bblab_consolidated(200, 80, 120, "USD", "2024"))
check("04 operating segment wins priority", identical(sel_p$dimension$kind, "operating_segment"))

# =====================================================================
# 5–7 Major ≥10%, immaterial → Other, 2–6 (or 1)
# =====================================================================
cons <- bblab_consolidated(100, 40, 60, "USD", "2024")
comps <- list(
  bblab_component("m1", "M1", revenue = 50, is_reported_segment = TRUE),
  bblab_component("m2", "M2", revenue = 40, is_reported_segment = TRUE),
  bblab_component("tiny", "Tiny", revenue = 5)
)
idn <- bblab_identify_major(comps, cons)
check("05 major at 10%+", length(idn$major) == 2L && all(vapply(idn$major, `[[`, "", "id") %in% c("m1", "m2")))
check("06 immaterial to Other", length(idn$other) == 1L && identical(idn$other[[1]]$id, "tiny"))
check("07 count in 2-6", length(idn$major) >= 2L && length(idn$major) <= 6L)

# =====================================================================
# 8–9 Single business: one card, still show donut, do not fabricate
# =====================================================================
one <- list(
  ticker = "ONE", entity = list(name = "Single"),
  statement_currency = "USD", period = "2024",
  consolidated = bblab_consolidated(90, 40, 50, "USD", "2024"),
  dimensions = list(bblab_dimension(
    "ent", "official_description",
    list(bblab_component("only", "Only business", revenue = 90, cor = 40, gp = 50,
                         is_principal_activity = TRUE, period = "2024", currency = "USD")),
    flags = c("relevant", "reconciles", "separate_revenue", "attributable_cor_gp",
              "mutually_exclusive", "filed_audited"),
    filed_audited = TRUE, period = "2024", currency = "USD"
  ))
)
r1 <- bblab_analyze(one)
r1_shares <- vapply(r1$chart$slices, function(s) s$share, numeric(1))
check("08 single card still shows pie",
      length(r1$businesses) == 1L &&
        isTRUE(r1$chart$eligible) &&
        !"BUSINESS_CHART_SINGLE_COMPONENT" %in% r1$chart$codes &&
        !"BUSINESS_CHART_INSUFFICIENT_COMPONENTS" %in% r1$chart$codes &&
        length(r1$chart$slices) >= 1L &&
        abs(r1$chart$denominator - 90) < 1e-9 &&
        abs(sum(r1_shares) - 1) < 1e-8 &&
        all(is.finite(r1_shares)) &&
        all(r1_shares > 0) &&
        "no_multi_business_split" %in% r1$limitations)
check("09 no fabricated second business", length(r1$businesses) == 1L && is.null(r1$other))
one_gap <- one
one_gap$consolidated <- bblab_consolidated(100, 40, 60, "USD", "2024")
r1g <- bblab_analyze(one_gap)
r1g_cls <- vapply(r1g$chart$slices, function(s) s$classification, character(1))
r1g_shares <- vapply(r1g$chart$slices, function(s) s$share, numeric(1))
check("09b one business plus recon slices still donut",
      length(r1g$businesses) == 1L &&
        isTRUE(r1g$chart$eligible) &&
        length(r1g$chart$slices) >= 2L &&
        any(r1g_cls %in% c("OTHER", "UNALLOCATED", "ROUNDING", "RECONCILIATION")) &&
        abs(r1g$chart$denominator - 100) < 1e-9 &&
        abs(sum(r1g_shares) - 1) < 1e-8 &&
        all(is.finite(r1g_shares)) &&
        all(r1g_shares > 0))

# =====================================================================
# 10 Level A HIGH when Rev + CoR/GP reported
# =====================================================================
ra <- bblab_analyze(two_seg_level_a())
check("10 level A HIGH",
      identical(ra$level, "A") && identical(ra$confidence$overall, "HIGH") &&
        all(vapply(ra$businesses, function(c) identical(c$cor_method, "reported_cor") ||
                     identical(c$cor_method, "reported_gp"), logical(1))))

# =====================================================================
# 11 Level B allocated CoR labeled allocated/estimated
# =====================================================================
lb <- two_seg_level_a()
lb$dimensions[[1]]$components <- list(
  bblab_component("a", "A", revenue = 120, company_allocated_cor = 48,
                  is_reported_segment = TRUE, period = "2024", currency = "USD"),
  bblab_component("b", "B", revenue = 80, company_allocated_cor = 32,
                  is_reported_segment = TRUE, period = "2024", currency = "USD")
)
rb <- bblab_analyze(lb)
check("11 level B allocated label",
      identical(rb$level, "B") &&
        all(vapply(rb$businesses, function(c) identical(c$cor_label, "allocated/estimated"), logical(1))) &&
        rb$confidence$cost %in% c("LOW", "MEDIUM"))

# =====================================================================
# 12 Level C: CoR/GP not estimable; consolidated GM fallback off by default
# =====================================================================
lc <- two_seg_level_a()
lc$dimensions[[1]]$components <- list(
  bblab_component("a", "A", revenue = 120, is_reported_segment = TRUE, period = "2024", currency = "USD"),
  bblab_component("b", "B", revenue = 80, is_reported_segment = TRUE, period = "2024", currency = "USD")
)
rc <- bblab_analyze(lc)
check("12 level C unestimable without fallback",
      identical(rc$level, "C") &&
        all(vapply(rc$businesses, function(c) identical(c$gp_display, "Not reliably estimable"), logical(1))) &&
        all(vapply(rc$businesses, function(c) !is.finite(c$cor), logical(1))))
rc_fb <- bblab_analyze(lc, options = list(use_consolidated_gm_fallback = TRUE))
check("12 fallback opt-in applies GM",
      all(vapply(rc_fb$businesses, function(c) is.finite(c$cor) && is.finite(c$gp), logical(1))) &&
        any(vapply(rc_fb$businesses, function(c) identical(c$cor_method, "consolidated_gm_fallback"), logical(1))))

# =====================================================================
# 13 Level D qualitative only — names, no amounts, no chart
# =====================================================================
ld <- list(
  ticker = "QD", entity = list(name = "Qual"),
  statement_currency = "USD", period = "2024",
  consolidated = bblab_consolidated(100, 40, 60, "USD", "2024"),
  dimensions = list(bblab_dimension(
    "mda", "mda",
    list(
      bblab_component("q1", "Named line A", qualitative_only = TRUE, description = "Principal activity"),
      bblab_component("q2", "Named line B", qualitative_only = TRUE, description = "Adjacent activity")
    ),
    flags = c("relevant", "management_major")
  ))
)
rd <- bblab_analyze(ld)
check("13 level D no amounts no chart",
      identical(rd$level, "D") &&
        !isTRUE(rd$chart$eligible) &&
        "BUSINESS_CHART_LEVEL_D" %in% rd$chart$codes &&
        all(vapply(rd$businesses, function(c) !is.finite(c$revenue), logical(1))))

# =====================================================================
# 14 Revenue rounding explicit; disclosed % not rewritten
# =====================================================================
rpct <- two_seg_level_a()
rpct$dimensions[[1]]$components <- list(
  bblab_component("a", "A", revenue_pct = 0.601, is_reported_segment = TRUE, period = "2024", currency = "USD"),
  bblab_component("b", "B", revenue_pct = 0.400, is_reported_segment = TRUE, period = "2024", currency = "USD")
)
rpct$consolidated <- bblab_consolidated(1000, 400, 600, "USD", "2024")
rr <- bblab_analyze(rpct)
check("14 rounding line explicit",
      !is.null(rr$rounding) && identical(rr$rounding$name, "Revenue Rounding Adjustment") &&
        abs(rr$businesses[[1]]$revenue_pct - 0.601) < 1e-12)

# =====================================================================
# 15 Revenue-share cost is final fallback, ALLOCATED_LOW_CONFIDENCE
# =====================================================================
# No GP so GM fallback cannot fire; user opt-in enables revenue-share last.
rs <- two_seg_level_a()
rs$consolidated$gp <- NA_real_
rs$dimensions[[1]]$components <- list(
  bblab_component("a", "A", revenue = 120, is_reported_segment = TRUE, period = "2024", currency = "USD"),
  bblab_component("b", "B", revenue = 80, is_reported_segment = TRUE, period = "2024", currency = "USD")
)
rrs <- bblab_analyze(rs, options = list(use_consolidated_gm_fallback = TRUE))
check("15 revenue-share ALLOCATED_LOW_CONFIDENCE",
      all(vapply(rrs$businesses, function(c) identical(c$cor_method, "revenue_share"), logical(1))) &&
        all(vapply(rrs$businesses, function(c) identical(c$cor_label, "ALLOCATED_LOW_CONFIDENCE"), logical(1))) &&
        "BUSINESS_COST_ALLOCATED_LOW_CONFIDENCE" %in% rrs$codes &&
        "rev_share_cost" %in% rrs$cost_notices)

# =====================================================================
# 16 Shared S&M / G&A / R&D not on GP cards
# =====================================================================
ra2 <- bblab_analyze(two_seg_level_a())
# Inject shared after analyze via payload
sh <- two_seg_level_a()
sh$shared_corporate <- list(sm = 9, ga = 7, rd = 11, interest = 3, tax = 5, ni = 40)
rsh <- bblab_analyze(sh)
check("16 shared excluded from GP cards",
      length(rsh$shared_corporate) > 0 &&
        all(vapply(rsh$businesses, function(c) {
          is.null(c$sm) && is.null(c$ga) && is.null(c$rd) && is.null(c$ni) &&
            is.finite(c$gp) && abs(c$gp - (c$revenue - c$cor)) < 1e-8
        }, logical(1))))

# =====================================================================
# 17 Reconciliation equals consolidated; unexplained not spread; 18 NI not a plug
# =====================================================================
check("17 recon pass on reported segments",
      isTRUE(ra$reconciliation$pass) &&
        abs(ra$reconciliation$revenue$recast - 200) < 1e-6)
gap <- two_seg_level_a()
gap$dimensions[[1]]$components[[1]]$revenue <- 100
rg <- bblab_analyze(gap)
check("17 leftover booked not spread",
      !is.null(rg$recon_amount) && is.finite(rg$recon_amount$revenue) &&
        isTRUE(!isTRUE(rg$reconciliation$used_ni_plug)))
check("18 NI never a plug",
      isTRUE(!isTRUE(rg$reconciliation$used_ni_plug)))

# =====================================================================
# 19 Revaluation not fabricated; CONSOLIDATED_PROXY labeled LOW
# =====================================================================
rvu <- bblab_analyze(two_seg_level_a())
check("19 reval unavailable not fabricated",
      isTRUE(!isTRUE(rvu$revaluation_available)) &&
        "BUSINESS_REVALUATION_UNAVAILABLE" %in% rvu$codes &&
        all(vapply(rvu$businesses, function(c) identical(c$revaluation$status, "UNAVAILABLE"), logical(1))))
proxy <- two_seg_level_a()
proxy$revaluation_inputs <- list(
  list(component_id = "a", reported_basis = 120, adjusted_basis = 132,
       method = "CONSOLIDATED_PROXY", status = "CONSOLIDATED_PROXY", is_estimated = TRUE)
)
rpv <- bblab_analyze(proxy)
check("19 CONSOLIDATED_PROXY LOW",
      identical(rpv$businesses[[1]]$revaluation$status, "CONSOLIDATED_PROXY") &&
        identical(rpv$businesses[[1]]$revaluation$confidence, "LOW") &&
        "BUSINESS_REVALUATION_CONSOLIDATED_PROXY" %in% rpv$codes)

# =====================================================================
# 20 Default view Reported; Adjusted does not overwrite production
# =====================================================================
check("20 default reported view",
      identical(rpv$view, "reported") && isTRUE(!isTRUE(rpv$fold_revaluation_into_gp)) &&
        abs(rpv$businesses[[1]]$revenue - 120) < 1e-9)

# =====================================================================
# 21 Chart: 1+ slices, same period/ccy/dimension, recon pass; denom = reported consolidated
# =====================================================================
check("21 chart eligible two segments",
      isTRUE(ra$chart$eligible) &&
        abs(ra$chart$denominator - 200) < 1e-9 &&
        all(abs(vapply(ra$chart$slices, function(s) s$share, numeric(1)) -
                  vapply(ra$chart$slices, function(s) s$revenue / 200, numeric(1))) < 1e-9))

# =====================================================================
# 22 Negative eliminations are not positive pie slices
# =====================================================================
el <- two_seg_level_a()
el$dimensions[[1]]$components <- c(
  el$dimensions[[1]]$components,
  list(bblab_component("elim", "Eliminations", "ELIMINATION", revenue = -5,
                       period = "2024", currency = "USD"))
)
el$dimensions[[1]]$components[[1]]$revenue <- 125
rel <- bblab_analyze(el)
pie_ids <- vapply(rel$chart$slices, function(s) s$id, character(1))
check("22 elim not a positive slice",
      !is.null(rel$chart$eliminations_legend) &&
        rel$chart$eliminations_legend$revenue < 0 &&
        isTRUE(!isTRUE(rel$chart$eliminations_legend$in_pie)) &&
        !"elim" %in% pie_ids && !"eliminations" %in% pie_ids)

# =====================================================================
# 23 minDisplayPercentage 3% groups display-only; never hide material reported segment
# =====================================================================
tiny_disp <- two_seg_level_a()
tiny_disp$consolidated <- bblab_consolidated(10000, 4000, 6000, "USD", "2024")
tiny_disp$dimensions[[1]]$components <- list(
  bblab_component("big", "Big", revenue = 9700, is_reported_segment = TRUE, period = "2024", currency = "USD", cor = 3880),
  bblab_component("mid", "Mid", revenue = 200, is_reported_segment = TRUE, period = "2024", currency = "USD", cor = 80),
  bblab_component("speck", "Speck", revenue = 100, period = "2024", currency = "USD", cor = 40)
)
rtd <- bblab_analyze(tiny_disp)
src_ids <- vapply(rtd$chart$source_rows, function(s) s$id, character(1))
disp_ids <- vapply(rtd$chart$slices, function(s) s$id, character(1))
grouped_ids <- character(0)
if (length(rtd$chart$grouped_source)) {
  grouped_ids <- unlist(lapply(rtd$chart$slices, function(s) s$grouped_ids %||% character(0)))
  if (!length(grouped_ids) && length(rtd$chart$grouped_source)) {
    grouped_ids <- vapply(rtd$chart$grouped_source, function(s) s$id, character(1))
  }
}
other_member_ids <- if (!is.null(rtd$other$members)) {
  vapply(rtd$other$members, function(c) c$id, character(1))
} else character(0)
check("23 display group keeps source; reported segment kept",
      ("speck" %in% src_ids || "speck" %in% other_member_ids || "speck" %in% grouped_ids) &&
        "mid" %in% disp_ids &&
        "big" %in% disp_ids)

# =====================================================================
# 24 Scoped degradation: missing cost / reval / chart / ADR do not block the rest
# =====================================================================
adr <- bblab_classify_fx_adr("TWD", "TWD", usd_twd = NA_real_,
                             instrument_type = "ADR", adr_ratio = NA_real_,
                             per_adr_display = TRUE)
check("24 missing ADR does not block GP",
      isTRUE(!isTRUE(adr$adr_blocks_business_gp)) &&
        "BUSINESS_ADR_MISSING_NONBLOCKING" %in% adr$codes)
check("24 missing cost keeps revenue",
      all(vapply(rc$businesses, function(c) is.finite(c$revenue), logical(1))) &&
        length(rc$businesses) == 2L)
check("24 missing reval keeps statements",
      isTRUE(rvu$ok) && length(rvu$businesses) == 2L && isTRUE(rvu$reconciliation$pass))
pm <- one
pm$dimensions[[1]]$components[[1]]$period <- "2023"
rpm <- bblab_analyze(pm)
check("24 chart fail keeps cards",
      !isTRUE(rpm$chart$eligible) &&
        "BUSINESS_CHART_PERIOD_MISMATCH" %in% rpm$chart$codes &&
        length(rpm$businesses) == 1L &&
        is.finite(rpm$businesses[[1]]$revenue))

# =====================================================================
# 25 TSM fixture: platforms as businesses; node/tech separate; TWD; no ADR required
# =====================================================================
rt <- bblab_analyze(fx)
fx_fx <- bblab_classify_fx_adr("TWD", "TWD", instrument_type = "ADR",
                               adr_ratio = NA_real_, per_adr_display = FALSE)
check("25 TSM fixture platform primary",
      identical(rt$primary_dimension$id, "platform") &&
        identical(rt$statement_currency, "TWD") &&
        length(rt$businesses) >= 2L && length(rt$businesses) <= 6L &&
        !any(grepl("nm$|node", vapply(rt$businesses, `[[`, "", "name"), ignore.case = TRUE)) &&
        is.null(fx_fx$codes) || !("APPLICABLE_ADR_RATIO_MISSING" %in% fx_fx$codes) &&
        isTRUE(!isTRUE(fx_fx$adr_required)))
check("25 shared fabs not on GP cards",
      is.finite(rt$shared_corporate$rd) &&
        all(vapply(rt$businesses, function(c) is.null(c$rd), logical(1))))

# =====================================================================
# 26 Lab lives under Company (dashboard); no sidebar menu; Lite still hides
# =====================================================================
dashboard_block <- {
  m <- regexpr(
    'tabItem\\(tabName = "dashboard"[\\s\\S]*?tabName = "company_advance"',
    ui_src,
    perl = TRUE
  )
  if (m < 1L) {
    ""
  } else {
    start <- as.integer(m)
    len <- as.integer(attr(m, "match.length"))
    substr(ui_src, start, start + len - 1L)
  }
}
testing_fn_src <- {
  m <- regexpr(
    "\\.ynow_page_ui_testing\\s*<-\\s*function\\s*\\([^)]*\\)\\s*\\{[\\s\\S]*?\\n\\}",
    ui_src,
    perl = TRUE
  )
  if (m < 1L) {
    ""
  } else {
    start <- as.integer(m)
    len <- as.integer(attr(m, "match.length"))
    substr(ui_src, start, start + len - 1L)
  }
}
advance_block <- {
  m <- regexpr(
    "tabItem\\(\\s*tabName\\s*=\\s*\"company_advance\"[\\s\\S]*?\\n\\s*\\),",
    ui_src,
    perl = TRUE
  )
  if (m < 1L) {
    ""
  } else {
    start <- as.integer(m)
    len <- as.integer(attr(m, "match.length"))
    substr(ui_src, start, start + len - 1L)
  }
}
check("26 Business Breakdown sidebar hosts BBL",
      grepl('tabName = "company_advance"', ui_src, fixed = TRUE) &&
        grepl("ynow_menu_company_advance", ui_src, fixed = TRUE) &&
        grepl('ynow_menu_company_advance", "Business Breakdown"', ui_src, fixed = TRUE) &&
        !grepl('tabName = "business_breakdown_lab"', ui_src, fixed = TRUE) &&
        grepl("ynow-bblab--report", mod_src, fixed = TRUE) &&
        grepl("Business Breakdown", mod_src, fixed = TRUE) &&
        grepl("business_breakdown_lab_ui", ui_src, fixed = TRUE))
check("26 Lab markup lives under Company - Advance tab",
      nzchar(advance_block) &&
        grepl("ynow-company-advance-bblab", advance_block, fixed = TRUE) &&
        grepl("business_breakdown_lab_ui", advance_block, fixed = TRUE) &&
        grepl('id = "ynow_company_advance_bblab"', advance_block, fixed = TRUE) &&
        !grepl("business_breakdown_lab_ui", dashboard_block, fixed = TRUE) &&
        !grepl("business_breakdown_lab_ui", testing_fn_src, fixed = TRUE) &&
        !grepl("ynow-testing-bblab", ui_src, fixed = TRUE) &&
        grepl("ynow_testing_page_title", testing_fn_src, fixed = TRUE))
check("26 Lite CSS hide + remap to Company - Advance",
      grepl('data-value="company_advance"', ui_src, fixed = TRUE) &&
        grepl("body.ynow-lite .ynow-company-advance-bblab", ui_src, fixed = TRUE) &&
        grepl("body.ynow-lite .ynow-bblab", ui_src, fixed = TRUE) &&
        grepl("ynow-sidebar-test-link", ui_src, fixed = TRUE) &&
        grepl("ynow-full-only", mod_src, fixed = TRUE) &&
        grepl("remapLegacyTab", ui_src, fixed = TRUE) &&
        grepl("business_breakdown_lab", ui_src, fixed = TRUE) &&
        grepl("return 'company_advance'", ui_src, fixed = TRUE))

# ---- extras: i18n, parse, valuation isolation, toast specificity ----
for (k in c("menu_business_breakdown_lab", "bblab_page_sub", "bblab_report_kicker",
            "bblab_shared_ticker_hint",
            "bblab_listed_only_notice", "bblab_listed_only_scope",
            "bblab_gm_unestimable", "bblab_reval_unavailable", "bblab_single_business_note",
            "bblab_fallback_gm_label",
            "bblab_ch1_title", "bblab_ch2_title", "bblab_ch3_title", "bblab_ch4_title",
            "bblab_ch5_title", "bblab_ch5_help", "bblab_cards_empty",
            "bblab_ch6_title", "bblab_ch7_title",
            "bblab_ch1_help", "bblab_ch2_help", "bblab_ch3_help", "bblab_ch3_current_label",
            "bblab_ch4_help",
            "bblab_ch4_limited", "bblab_geo_veto_why", "bblab_overlap_why",
            "bblab_struct_title", "bblab_struct_help", "bblab_struct_biz_h",
            "bblab_struct_adj_h", "bblab_struct_conc_h", "bblab_struct_not_disclosed",
            "bblab_struct_c1", "bblab_struct_c2", "bblab_struct_c5",
            "bblab_history_yaxis", "bblab_kpi_revenue", "bblab_kpi_cor", "bblab_kpi_gp",
            "bblab_kpi_gm", "bblab_kpi_ni", "bblab_formula_gp", "bblab_formula_gm",
            "notif_bblab_required_fx_rate_missing", "notif_bblab_business_chart_single_component",
            "notif_bblab_business_history_insufficient_years")) {
  en <- .UI_STRINGS$en[[k]]
  zh <- .UI_STRINGS$`zh-TW`[[k]]
  check(paste("i18n", k), is.character(en) && nzchar(en[1]) && is.character(zh) && nzchar(zh[1]))
}
check("i18n chapter titles en + zh-TW", {
  identical(.UI_STRINGS$en$bblab_ch1_title, "Consolidated statement snapshot") &&
    identical(.UI_STRINGS$`zh-TW`$bblab_ch1_title, "合併財報總覽") &&
    identical(.UI_STRINGS$en$bblab_ch2_title, "How the statements split the business") &&
    identical(.UI_STRINGS$`zh-TW`$bblab_ch2_title, "財報如何切分業務") &&
    identical(.UI_STRINGS$en$bblab_ch3_title, "Revenue mix") &&
    identical(.UI_STRINGS$`zh-TW`$bblab_ch3_title, "營收組成") &&
    identical(.UI_STRINGS$en$bblab_ch3_current_label, "Current period") &&
    identical(.UI_STRINGS$`zh-TW`$bblab_ch3_current_label, "當期") &&
    identical(.UI_STRINGS$en$bblab_ch4_title, "Five-year mix evolution") &&
    identical(.UI_STRINGS$`zh-TW`$bblab_ch4_title, "五年結構占比演進") &&
    identical(.UI_STRINGS$en$bblab_ch5_title, "Business cards") &&
    identical(.UI_STRINGS$`zh-TW`$bblab_ch5_title, "各事業簡化財報") &&
    identical(.UI_STRINGS$en$bblab_ch6_title, "Reconciliation") &&
    identical(.UI_STRINGS$`zh-TW`$bblab_ch6_title, "對帳") &&
    identical(.UI_STRINGS$en$bblab_ch7_title, "Sources") &&
    identical(.UI_STRINGS$`zh-TW`$bblab_ch7_title, "來源與方法") &&
    grepl("不足兩個會計年度", .UI_STRINGS$`zh-TW`$bblab_ch4_limited, fixed = TRUE) &&
    !grepl("默认|参数|数据|用户", paste(
      .UI_STRINGS$`zh-TW`$bblab_ch1_title, .UI_STRINGS$`zh-TW`$bblab_ch2_help,
      .UI_STRINGS$`zh-TW`$bblab_ch4_limited, .UI_STRINGS$`zh-TW`$bblab_page_sub,
      sep = " "
    ))
})
check("applyUiLocale wires chapter titles", {
  grepl("setBtText('ynow_bblab_ch1_title', 'bblab_ch1_title')", ui_src, fixed = TRUE) &&
    grepl("setBtText('ynow_bblab_ch3_current_label', 'bblab_ch3_current_label')", ui_src, fixed = TRUE) &&
    grepl("setBtText('ynow_bblab_ch4_title', 'bblab_ch4_title')", ui_src, fixed = TRUE) &&
    grepl("setBtText('ynow_bblab_ch4_limited', 'bblab_ch4_limited')", ui_src, fixed = TRUE) &&
    grepl("setBtText('ynow_bblab_ch5_help', 'bblab_ch5_help')", ui_src, fixed = TRUE) &&
    grepl("setBtText('ynow_bblab_ch7_title', 'bblab_ch7_title')", ui_src, fixed = TRUE)
})
check("mix chapters are one report section with history panel", {
  grepl("Revenue mix", mod_src, fixed = TRUE) &&
    grepl("ynow_bblab_ch3_current_label", mod_src, fixed = TRUE) &&
    grepl("ynow-bblab-subhead", mod_src, fixed = TRUE) &&
    grepl("plotlyOutput(ns(\"donut\")", mod_src, fixed = TRUE) &&
    grepl("plotlyOutput(ns(\"history\")", mod_src, fixed = TRUE) &&
    grepl('uiOutput(ns("history_panel"))', mod_src, fixed = TRUE) &&
    grepl("ynow-bblab-struct-mix", mod_src, fixed = TRUE) &&
    grepl("ynow-bblab-struct-mix__conc", mod_src, fixed = TRUE) &&
    grepl("ynow-bblab-struct-mix__chart", mod_src, fixed = TRUE) &&
    grepl("ynow-bblab-struct--tables", mod_src, fixed = TRUE) &&
    grepl('chapter = "6"', mod_src, fixed = TRUE) &&
    !grepl('chapter = "7"', mod_src, fixed = TRUE) &&
    grepl("ynow-bblab-report__section", mod_src, fixed = TRUE) &&
    grepl("per-business cards",
          paste(.UI_STRINGS$en$bblab_page_sub, collapse = " "), fixed = TRUE) &&
    grepl("Ticker / Stock Code",
          paste(.UI_STRINGS$en$bblab_page_sub, collapse = " "), fixed = TRUE) &&
    grepl("投資報告樣式",
          paste(.UI_STRINGS$`zh-TW`$bblab_page_sub, collapse = " "), fixed = TRUE) &&
    grepl("各事業簡化財報",
          paste(.UI_STRINGS$`zh-TW`$bblab_page_sub, collapse = " "), fixed = TRUE)
})
check("chapter titles do not repeat badge numbering", {
  keys <- paste0("bblab_ch", 1:7, "_title")
  aliases <- c("bblab_summary_title", "bblab_chart_title", "bblab_cards_title",
               "bblab_recon_title", "bblab_sources_title")
  all_keys <- c(keys, aliases)
  en_ok <- all(!grepl("^[0-9]+\\.\\s", vapply(all_keys, function(k) .UI_STRINGS$en[[k]], character(1))))
  zh_ok <- all(!grepl("^[0-9]+\\.\\s", vapply(all_keys, function(k) .UI_STRINGS$`zh-TW`[[k]], character(1))))
  en_ok && zh_ok && grepl("ynow-bblab-report__section-num", mod_src, fixed = TRUE)
})
check("i18n Business Breakdown rename both locales",
      identical(.UI_STRINGS$en$bblab_page_title, "Business Breakdown") &&
        identical(.UI_STRINGS$`zh-TW`$bblab_page_title, "業務拆解") &&
        identical(.UI_STRINGS$en$menu_business_breakdown_lab, "Business Breakdown") &&
        identical(.UI_STRINGS$`zh-TW`$menu_business_breakdown_lab, "業務拆解"))
check("i18n fallback GM label has no default-off parenthetical", {
  en <- .UI_STRINGS$en$bblab_fallback_gm_label
  zh <- .UI_STRINGS$`zh-TW`$bblab_fallback_gm_label
  identical(en, "Use consolidated Gross Margin as low-confidence fallback") &&
    identical(zh, "以合併 Gross Margin 作為低信心後援") &&
    grepl("Gross Margin", zh, fixed = TRUE) &&
    !grepl("（預設關閉）", zh, fixed = TRUE) &&
    !grepl("預設關閉", zh, fixed = TRUE) &&
    !grepl("off by default", en, ignore.case = TRUE) &&
    !grepl("default off", en, ignore.case = TRUE)
})
check("applyUiLocale wires fallback GM label",
      grepl("setBtText('ynow_bblab_fallback_gm_label', 'bblab_fallback_gm_label')",
            ui_src, fixed = TRUE) &&
        grepl('id = "ynow_bblab_fallback_gm_label"', mod_src, fixed = TRUE))
check("GM fallback checkbox remains unchecked by default", {
  m <- regexpr("checkboxInput\\([\\s\\S]*?use_gm_fallback[\\s\\S]*?value\\s*=\\s*FALSE",
               mod_src, perl = TRUE)
  m > 0L &&
    !grepl("use_consolidated_gm_fallback\\s*=\\s*TRUE", engine_src)
})
check("i18n zh-TW uses 預設 not 默认",
      grepl("預設", .UI_STRINGS$`zh-TW`$notif_bblab_business_revaluation_consolidated_proxy,
            fixed = TRUE) &&
        !grepl("默认|参数|数据|用户", paste(.UI_STRINGS$`zh-TW`$bblab_page_sub,
                                         .UI_STRINGS$`zh-TW`$bblab_notes_body,
                                         .UI_STRINGS$`zh-TW`$bblab_fallback_gm_label,
                                         .UI_STRINGS$`zh-TW`$bblab_listed_only_notice,
                                         .UI_STRINGS$`zh-TW`$bblab_listed_only_scope, sep = " ")))

val_files <- c("valuation_guard.R", "investment_decision_module.R", "fcf_projection_module.R",
               "ddm_module.R", "ri_module.R", "pb_asset_module.R")
val_hit <- vapply(val_files, function(f) {
  txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("business_breakdown", txt, fixed = TRUE)
}, logical(1))
check("valuation files unchanged (no bblab wiring)", !any(val_hit))
check("server only mounts lab module",
      grepl("business_breakdown_lab_server", server_src, fixed = TRUE) &&
        !grepl("bblab_analyze\\(", server_src))
check("server remaps legacy BBL tab to Company - Advance",
      grepl('identical(tab, "business_breakdown_lab")', server_src, fixed = TRUE) &&
        grepl("ynowGotoTab", server_src, fixed = TRUE) &&
        grepl('tab = "company_advance"', server_src, fixed = TRUE))

parse_files <- c("business_breakdown_schema.R", "business_breakdown_engine.R",
                 "business_breakdown_structure.R", "business_breakdown_module.R",
                 "global.R", "ui_locale.R",
                 "ynow_ui.R", "ynow_server.R")
for (pf in parse_files) {
  okp <- inherits(tryCatch(parse(pf, encoding = "UTF-8"), error = function(e) e), "expression")
  check(paste("parse", pf), isTRUE(okp))
}

nine_stage_labels <- c(
  "1 Resolve issuer", "2 Retrieve statements", "3 Parse disclosures",
  "4 Detect primary dimension", "5 Identify major businesses",
  "6 Assign revenue", "7 Assign Cost of Revenue / Gross Profit",
  "8 Reconcile and revalue", "9 Chart and cards"
)
check("Lab markup has no 9-step Progress UI", {
  hay <- paste(mod_src, ui_src, collapse = "\n")
  !grepl("ynow-bblab-progress", hay, fixed = TRUE) &&
    !grepl('uiOutput(ns("progress"))', mod_src, fixed = TRUE) &&
    !grepl("output$progress", mod_src, fixed = TRUE) &&
    !grepl("%s / 9", mod_src, fixed = TRUE) &&
    !grepl("Progress 9", hay, fixed = TRUE) &&
    !any(vapply(nine_stage_labels, function(s) grepl(s, hay, fixed = TRUE), logical(1))) &&
    is.null(.UI_STRINGS$en$bblab_progress_label) &&
    is.null(.UI_STRINGS$en$bblab_stage_resolve_issuer) &&
    is.null(.UI_STRINGS$en$bblab_stage_chart_cards) &&
    is.null(.UI_STRINGS$`zh-TW`$bblab_progress_label) &&
    is.null(.UI_STRINGS$`zh-TW`$bblab_stage_resolve_issuer) &&
    is.null(.UI_STRINGS$`zh-TW`$bblab_stage_chart_cards)
})
check("engine still tracks internal stage for scoped errors", {
  is.list(rt$progress) &&
    is.character(rt$progress$stage) &&
    nzchar(rt$progress$stage[1])
})
check("FX missing blocks display only", {
  fxm <- bblab_classify_fx_adr("TWD", "USD", usd_twd = NA_real_)
  "REQUIRED_FX_RATE_MISSING" %in% fxm$codes && isTRUE(fxm$fx_required)
})
check("toasts are specific", {
  t <- bblab_toast_payload("BUSINESS_CHART_PERIOD_MISMATCH", recon_pass = TRUE)
  identical(t[[1]]$blocked_outputs, "composition_chart") &&
    "business_cards" %in% t[[1]]$remaining_outputs
})
check("count codes do not hide composition chart", {
  t1 <- bblab_toast_payload("BUSINESS_CHART_SINGLE_COMPONENT", recon_pass = TRUE)
  t2 <- bblab_toast_payload("BUSINESS_CHART_INSUFFICIENT_COMPONENTS", recon_pass = TRUE)
  !length(t1[[1]]$blocked_outputs) &&
    "composition_chart" %in% t1[[1]]$remaining_outputs &&
    !length(t2[[1]]$blocked_outputs) &&
    "composition_chart" %in% t2[[1]]$remaining_outputs
})
check("i18n single-business note still shows composition chart", {
  en <- paste(.UI_STRINGS$en$bblab_single_business_note, collapse = " ")
  zh <- paste(.UI_STRINGS$`zh-TW`$bblab_single_business_note, collapse = " ")
  grepl("composition chart still renders", en, ignore.case = TRUE) &&
    grepl("組成圖仍會呈現", zh, fixed = TRUE) &&
    !grepl("no composition chart is shown", en, ignore.case = TRUE) &&
    !grepl("也不顯示組成圖", zh, fixed = TRUE) &&
    !grepl("不顯示圓餅圖", zh, fixed = TRUE)
})
check("engine does not gate chart on component count",
      !grepl("n_valid < 2", engine_src, fixed = TRUE) &&
        !grepl("if (n_valid < 2L)", engine_src, fixed = TRUE))
check("notes helper used", grepl("ynow_notes_block", mod_src, fixed = TRUE))
check("Lab numerals do not use HCCSI flow fill",
      !grepl("ynow-hccsi-flow", mod_src, fixed = TRUE) &&
        !grepl("ynow-hccsi-flow", engine_src, fixed = TRUE))
check("no invalid shinydashboard box status default",
      !grepl("status\\s*=\\s*[\"']default[\"']", mod_src))
if (requireNamespace("shiny", quietly = TRUE) &&
    requireNamespace("shinydashboard", quietly = TRUE)) {
  suppressPackageStartupMessages({
    library(shiny)
    library(shinydashboard)
  })
  lab_ui_html <- tryCatch({
    paste(as.character(business_breakdown_lab_ui("bblab")), collapse = " ")
  }, error = function(e) {
    attr(e, "msg") <- conditionMessage(e)
    ""
  })
  check("lab UI constructs at startup", nzchar(lab_ui_html))
  check("lab UI keeps report toolbar and numbered chapters",
        grepl("ynow-bblab--report", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_period_label", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_fallback_gm_label", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_shared_ticker_hint", lab_ui_html, fixed = TRUE) &&
          !grepl("ynow_bblab_search_title", lab_ui_html, fixed = TRUE) &&
          !grepl("id=\"bblab-search\"", lab_ui_html, fixed = TRUE) &&
          !grepl("id=\"bblab-ticker\"", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_ch1_title", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_ch2_title", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_struct_title", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_ch3_title", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_ch3_current_label", lab_ui_html, fixed = TRUE) &&
          grepl("history_panel", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_ch5_title", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_ch5_help", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_ch6_title", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_ch7_title", lab_ui_html, fixed = TRUE))
  check("lab UI HTML: full-width tables; conclusions beside revenue mix", {
    grepl("ynow-bblab-struct-mix", lab_ui_html, fixed = TRUE) &&
      grepl("ynow-bblab-struct-mix__conc", lab_ui_html, fixed = TRUE) &&
      grepl("ynow-bblab-struct-mix__chart", lab_ui_html, fixed = TRUE) &&
      grepl("structure_panel", lab_ui_html, fixed = TRUE) &&
      grepl("structure_conclusions", lab_ui_html, fixed = TRUE) &&
      grepl("ynow-bblab-subhead", lab_ui_html, fixed = TRUE) &&
      grepl(">Revenue mix<", lab_ui_html, fixed = TRUE) &&
      grepl(">Current period<", lab_ui_html, fixed = TRUE) &&
      grepl(">Business cards<", lab_ui_html, fixed = TRUE) &&
      grepl("history_panel", lab_ui_html, fixed = TRUE) &&
      !grepl(">Five-year mix evolution<", lab_ui_html, fixed = TRUE) &&
      grepl("data-bblab-chapter=\"3\"", lab_ui_html, fixed = TRUE) &&
      grepl("data-bblab-chapter=\"4\"", lab_ui_html, fixed = TRUE) &&
      grepl("data-bblab-chapter=\"6\"", lab_ui_html, fixed = TRUE) &&
      !grepl("data-bblab-chapter=\"7\"", lab_ui_html, fixed = TRUE) &&
      !grepl("data-bblab-chapter=\"2b\"", lab_ui_html, fixed = TRUE) &&
      !grepl(">Current revenue mix<", lab_ui_html, fixed = TRUE) &&
      !grepl("ynow-bblab-struct-mix__main", lab_ui_html, fixed = TRUE)
  })
  check("chapter order in report markup", {
    ids <- c("ynow_bblab_page_title",
             "ynow_bblab_ch1_title", "ynow_bblab_ch2_title",
             "ynow_bblab_struct_title", "ynow_bblab_ch3_title",
             "ynow_bblab_ch3_current_label",
             "ynow_bblab_ch5_title", "ynow_bblab_ch6_title",
             "ynow_bblab_ch7_title")
    pos <- vapply(ids, function(id) {
      as.integer(regexpr(id, lab_ui_html, fixed = TRUE)[1])
    }, integer(1))
    all(pos > 0L) && all(diff(pos) > 0L)
  })
  check("lab titles use report section numbers without repeating them in the heading",
        grepl("ynow-bblab-report__section-num", lab_ui_html, fixed = TRUE) &&
          !grepl(">1\\. Consolidated", lab_ui_html) &&
          !grepl(">2\\. How the statements", lab_ui_html) &&
          grepl(">Consolidated statement snapshot<", lab_ui_html, fixed = TRUE) &&
          grepl(">Business Breakdown<", lab_ui_html, fixed = TRUE))
  check("lab UI HTML has no Progress 9/9 or nine stage labels",
        !grepl("bblab-progress", lab_ui_html, fixed = TRUE) &&
          !grepl("ynow-bblab-progress", lab_ui_html, fixed = TRUE) &&
          !grepl("Progress 9", lab_ui_html, fixed = TRUE) &&
          !grepl(" / 9", lab_ui_html, fixed = TRUE) &&
          !any(vapply(nine_stage_labels, function(s) grepl(s, lab_ui_html, fixed = TRUE), logical(1))))
  check("lab UI HTML shows listed-stock notice outside Notes",
        grepl("ynow_bblab_listed_only_notice", lab_ui_html, fixed = TRUE) &&
          grepl("Listed stocks only (Taiwan and U.S. exchanges).", lab_ui_html, fixed = TRUE) &&
          grepl("ynow-bblab__listed-notice", lab_ui_html, fixed = TRUE) &&
          !grepl("ynow_bblab_experimental_badge", lab_ui_html, fixed = TRUE) &&
          !grepl("ynow_notes_block", lab_ui_html, fixed = TRUE) &&
          !grepl("ynow-notes", lab_ui_html, fixed = TRUE))
  check("lab has no in-page Search; Period and GM fallback remain",
        !grepl("id=\"bblab-search\"", lab_ui_html, fixed = TRUE) &&
          !grepl("ynow-bblab-search-controls", lab_ui_html, fixed = TRUE) &&
          !grepl("__ynowBblabSearchGuard", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_period_label", lab_ui_html, fixed = TRUE) &&
          grepl("ynow_bblab_fallback_gm_label", lab_ui_html, fixed = TRUE) &&
          grepl("ynow-bblab-report__toolbar", lab_ui_html, fixed = TRUE))
}
check("report kicker locale wiring",
      grepl("ynow_bblab_report_kicker", ui_src, fixed = TRUE) &&
        grepl("bblab_report_kicker", ui_src, fixed = TRUE) &&
        grepl("bblab_shared_ticker_hint", ui_src, fixed = TRUE))

# Engine analyzes TSM fixture without any issuer-specific code path:
check("TSM fixture is data-only", {
  # Same engine, different payload shape (consumer goods) still works
  groc <- two_seg_level_a()
  groc$dimensions[[1]]$components[[1]]$name <- "Fresh"
  groc$dimensions[[1]]$components[[2]]$name <- "Packaged"
  rgroc <- bblab_analyze(groc)
  isTRUE(rgroc$ok) && identical(rgroc$level, "A") &&
    identical(rt$primary_dimension$kind, "revenue_disaggregation")
})

# ---- Search path: auto-detect from fixture statements; reval does not block cards ----
yahoo_two_product_is <- function() {
  data.frame(
    Breakdown = c("Total Revenue", "Widgets", "Gadgets", "Cost Of Revenue", "Gross Profit",
                  "Research And Development", "Selling General And Administration"),
    `12/31/2024` = c("200", "120", "80", "80", "120", "11", "7"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
yahoo_consol_only_is <- function() {
  data.frame(
    Breakdown = c("Total Revenue", "Operating Revenue", "Cost Of Revenue", "Gross Profit"),
    `12/31/2024` = c("1000", "1000", "420", "580"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
label_only_is <- function() {
  data.frame(
    Breakdown = c("Total Revenue", "Cost Of Revenue", "Gross Profit", "Net Income"),
    stringsAsFactors = FALSE
  )
}

check("one-column IS does not throw subscript out of bounds", {
  p1 <- tryCatch(
    bblab_payload_from_statements(
      label_only_is(), ticker = "TSM", entity_name = "TSMC",
      statement_currency = "TWD"
    ),
    error = function(e) e
  )
  r1 <- if (is.list(p1) && !inherits(p1, "error")) {
    tryCatch(bblab_analyze(p1), error = function(e) e)
  } else p1
  is.list(p1) && !inherits(p1, "error") &&
    is.list(r1) && !inherits(r1, "error") &&
    !isTRUE(.bblab_finite(p1$consolidated$revenue))
})
check("malformed note table is skipped, not a Search crash", {
  messy <- list(
    short_name = "Segment Information",
    kind = "operating_segment",
    headers = list(),
    rows = list(list())
  )
  p2 <- tryCatch(
    bblab_payload_from_statements(
      yahoo_consol_only_is(), ticker = "TSM", entity_name = "TSMC",
      statement_currency = "TWD", notes = list(segment_tables = list(messy))
    ),
    error = function(e) e
  )
  r2 <- if (is.list(p2) && !inherits(p2, "error")) {
    tryCatch(bblab_analyze(p2), error = function(e) e)
  } else p2
  is.list(p2) && !inherits(p2, "error") &&
    is.list(r2) && !inherits(r2, "error") &&
    isTRUE(.bblab_finite(p2$consolidated$revenue))
})
check("Search toast does not leak R subscript errors",
      !grepl("conditionMessage\\(e\\)", mod_src, fixed = TRUE) &&
        grepl("ui_msg(\"bblab_source_unavailable\")", mod_src, fixed = TRUE) &&
        grepl("reorder_financial_columns", mod_src, fixed = TRUE))
check("Search catch-all does not wipe a loaded statement payload",
      grepl("isolate(lab_payload())", mod_src, fixed = TRUE) &&
        grepl("has_stmt", mod_src, fixed = TRUE) &&
        grepl("bblab_history_shares", engine_src, fixed = TRUE) &&
        grepl(".bblab_named_get(hit$amounts, ys)", engine_src, fixed = TRUE))

search_two <- bblab_payload_from_statements(
  yahoo_two_product_is(), ticker = "FIXT", entity_name = "Two-product fixture",
  statement_currency = "USD", period = "12/31/2024"
)
check("Search auto-detects product lines from statements",
      length(search_two$dimensions) >= 1L &&
        identical(search_two$dimensions[[1]]$kind, "product_service") &&
        length(search_two$dimensions[[1]]$components) == 2L)
rsrch <- bblab_analyze(search_two)
check("Search analysis keeps reported Rev cards when reval UNAVAILABLE",
      isTRUE(rsrch$ok) &&
        "BUSINESS_REVALUATION_UNAVAILABLE" %in% rsrch$codes &&
        isTRUE(!isTRUE(rsrch$revaluation_available)) &&
        length(rsrch$businesses) == 2L &&
        all(vapply(rsrch$businesses, function(c) is.finite(c$revenue) && c$revenue > 0, logical(1))) &&
        abs(sum(vapply(rsrch$businesses, function(c) c$revenue, numeric(1))) - 200) < 1e-6)
check("reval toast does not block Rev/CoR/GP", {
  t <- bblab_toast_payload("BUSINESS_REVALUATION_UNAVAILABLE", recon_pass = TRUE,
                           chart_eligible = TRUE)
  identical(t[[1]]$blocked_outputs, "revaluation_ratio") &&
    isTRUE(!isTRUE(t[[1]]$page_error)) &&
    all(c("business_cards", "revenue", "cost_of_revenue", "gross_profit") %in% t[[1]]$remaining_outputs)
})

consol_pl <- bblab_payload_from_statements(
  yahoo_consol_only_is(), ticker = "ONE", entity_name = "Consolidated-only fixture",
  statement_currency = "USD"
)
rconsol <- bblab_analyze(consol_pl)
check("no fabricated split from consolidated-only IS",
      length(consol_pl$dimensions) == 0L &&
        length(rconsol$businesses) == 1L &&
        is.finite(rconsol$businesses[[1]]$revenue) &&
        is.finite(rconsol$businesses[[1]]$cor) &&
        is.finite(rconsol$businesses[[1]]$gp) &&
        "BUSINESS_REVALUATION_UNAVAILABLE" %in% rconsol$codes &&
        isTRUE(rconsol$ok))
check("consolidated-only still renders 100% donut",
      isTRUE(rconsol$chart$eligible) &&
        length(rconsol$chart$slices) >= 1L &&
        abs(rconsol$chart$slices[[1]]$share - 1) < 1e-6 &&
        is.finite(rconsol$chart$slices[[1]]$share) &&
        rconsol$chart$slices[[1]]$share > 0 &&
        !"BUSINESS_CHART_SINGLE_COMPONENT" %in% rconsol$chart$codes)

if (requireNamespace("shiny", quietly = TRUE)) {
  card_html <- paste(as.character(.bblab_card_html(rconsol$businesses[[1]], "en", FALSE, 1000)),
                     collapse = " ")
  check("card HTML keeps Revenue/CoR/GP when reval unavailable",
        grepl("Revenue:", card_html, fixed = TRUE) &&
          grepl("Cost of Revenue:", card_html, fixed = TRUE) &&
          grepl("Gross Profit:", card_html, fixed = TRUE) &&
          grepl("1,000", card_html, fixed = TRUE) &&
          grepl("Revaluation ratio unavailable", card_html, fixed = TRUE))
  snap_html <- paste(as.character(.bblab_snapshot_is_html(
    rconsol$consolidated, "en", statement_currency = "USD", period = "2024"
  )), collapse = " ")
  snap_zh <- paste(as.character(.bblab_snapshot_is_html(
    rconsol$consolidated, "zh-TW", statement_currency = "USD", period = "2024"
  )), collapse = " ")
  check("snapshot uses vertical IS accounting layout", {
    grepl("ynow-bblab-is__table", snap_html, fixed = TRUE) &&
      !grepl("ynow-bblab-kpis", snap_html, fixed = TRUE) &&
      grepl("Revenue − Cost of Revenue", snap_html, fixed = TRUE) &&
      grepl("Gross Profit / Revenue", snap_html, fixed = TRUE) &&
      grepl("ynow-bblab-is__row--total", snap_html, fixed = TRUE) &&
      grepl("Gross Profit = Revenue", .UI_STRINGS$en$bblab_ch1_help, fixed = TRUE) &&
      grepl("Gross Profit = Revenue", .UI_STRINGS$`zh-TW`$bblab_ch1_help, fixed = TRUE) &&
      identical(.UI_STRINGS$en$bblab_formula_gp, "Revenue − Cost of Revenue") &&
      identical(.UI_STRINGS$`zh-TW`$bblab_formula_gp, "Revenue − Cost of Revenue") &&
      grepl("Revenue − Cost of Revenue", snap_zh, fixed = TRUE)
  })
  ni_html <- paste(as.character(.bblab_snapshot_is_html(
    list(revenue = 200, cor = 80, gp = 120, ni = 40, currency = "USD", period = "2024"),
    "en"
  )), collapse = " ")
  check("snapshot shows Net Income when reported",
        grepl("Net Income", ni_html, fixed = TRUE) &&
          grepl("ynow-bblab-is__row--ni", ni_html, fixed = TRUE) &&
          grepl("40", ni_html, fixed = TRUE))
}

note_tbl <- list(list(
  short_name = "Segment Information",
  kind = "operating_segment",
  headers = c("Segment", "Revenue", "Cost of Revenue"),
  rows = list(
    c("Business A", "120", "40"),
    c("Business B", "80", "40"),
    c("Total", "200", "80")
  )
))
note_pl <- bblab_payload_from_statements(
  data.frame(
    Breakdown = c("Total Revenue", "Cost Of Revenue", "Gross Profit"),
    `12/31/2024` = c("200", "80", "120"),
    check.names = FALSE, stringsAsFactors = FALSE
  ),
  ticker = "FIXT", entity_name = "Note fixture",
  statement_currency = "USD", period = "12/31/2024",
  segment_tables = note_tbl
)
rnote <- bblab_analyze(note_pl)
check("filed segment note tables become businesses",
      identical(rnote$primary_dimension$kind, "operating_segment") &&
        identical(rnote$level, "A") &&
        length(rnote$businesses) == 2L &&
        all(vapply(rnote$businesses, function(c) is.finite(c$revenue) && is.finite(c$cor) &&
                     is.finite(c$gp), logical(1))) &&
        "BUSINESS_REVALUATION_UNAVAILABLE" %in% rnote$codes)

geo_only_payload <- list(
  ticker = "GEO", entity = list(name = "Geo only"),
  statement_currency = "USD", period = "2024",
  consolidated = bblab_consolidated(200, 80, 120, "USD", "2024"),
  dimensions = list(bblab_dimension(
    "geo", "geography",
    list(
      bblab_component("us", "United States", revenue = 140, period = "2024", currency = "USD"),
      bblab_component("eu", "Europe", revenue = 60, period = "2024", currency = "USD")
    ),
    is_customer_location_only = TRUE, flags = c("separate_revenue")
  ))
)
rgeo <- bblab_analyze(geo_only_payload)
check("vetoed geography does not hide reported cards",
      isTRUE(rgeo$ok) &&
        length(rgeo$businesses) == 1L &&
        is.finite(rgeo$businesses[[1]]$revenue) &&
        abs(rgeo$businesses[[1]]$revenue - 200) < 1e-9 &&
        "BUSINESS_GEOGRAPHY_CUSTOMER_LOCATION_ONLY" %in% rgeo$codes)

engine_src2 <- paste(readLines("business_breakdown_engine.R", warn = FALSE, encoding = "UTF-8"),
                     collapse = "\n")
check("engine still has no ticker branches after extract helpers",
      !grepl("TSM|2330|N3|N5|N7|HPC|AMZN|Amazon", engine_src2))
check("shared opex from IS not on GP cards",
      is.finite(search_two$shared_corporate$rd) &&
        all(vapply(rsrch$businesses, function(c) is.null(c$rd) && is.null(c$ga), logical(1))))
check("module skips reval unavailable page toast",
      grepl("BUSINESS_REVALUATION_UNAVAILABLE", mod_src, fixed = TRUE) &&
        grepl("Scoped degradation", mod_src, fixed = TRUE))

# ---- Recorded 10-K-like 3-segment table (names are fixture data, not engine branches)
k10_cons_rev <- 716924e6
k10_opseg <- list(
  short_name = "Segment Information",
  kind = "operating_segment",
  layout = "grouped_metrics",
  scale = 1e6,
  headers = c("", "2023", "2024", "2025"),
  rows = list(
    c("North America", "", "", ""),
    c("Net sales", "352828", "387497", "426305"),
    c("Operating expenses", "337951", "362530", "396686"),
    c("Operating income", "14877", "24967", "29619"),
    c("International", "", "", ""),
    c("Net sales", "131200", "142906", "161894"),
    c("Operating expenses", "133856", "139114", "157144"),
    c("Operating income (loss)", "-2656", "3792", "4750"),
    c("AWS", "", "", ""),
    c("Net sales", "90757", "107556", "128725"),
    c("Operating expenses", "66126", "67722", "83119"),
    c("Operating income", "24631", "39834", "45606"),
    c("Consolidated", "", "", ""),
    c("Net sales", "574785", "637959", "716924"),
    c("Operating income", "36852", "68593", "79975")
  )
)
k10_product <- list(
  short_name = "Segment Information",
  kind = "revenue_disaggregation",
  layout = "stub",
  scale = 1e6,
  is_customer_location_only = FALSE,
  headers = c("", "2023", "2024", "2025"),
  rows = list(
    c("Net Sales:", "", "", ""),
    c("Online stores (1)", "231872", "247029", "269287"),
    c("Physical stores (2)", "20030", "21215", "22561"),
    c("Third-party seller services (3)", "140053", "156146", "172162"),
    c("Advertising services (4)", "46906", "56214", "68635"),
    c("Subscription services (5)", "40209", "44374", "49619"),
    c("Cloud infrastructure", "90757", "107556", "128725"),
    c("Other (6)", "4958", "5425", "5935"),
    c("Consolidated", "574785", "637959", "716924")
  )
)
k10_geo <- list(
  short_name = "Segment Information - Net Sales Attributed to Countries",
  kind = "geography",
  layout = "stub",
  scale = 1e6,
  is_customer_location_only = TRUE,
  headers = c("", "2023", "2024", "2025"),
  rows = list(
    c("United States", "395637", "438015", "489657"),
    c("Germany", "37588", "40856", "45900"),
    c("United Kingdom", "33591", "37855", "43212"),
    c("Japan", "26002", "27401", "30688"),
    c("Rest of world", "81967", "93832", "107467"),
    c("Consolidated", "574785", "637959", "716924")
  )
)
k10_is <- data.frame(
  Breakdown = c("Total Revenue", "Cost Of Revenue", "Gross Profit"),
  `12/31/2025` = c(as.character(k10_cons_rev), as.character(k10_cons_rev * 0.72),
                   as.character(k10_cons_rev * 0.28)),
  check.names = FALSE, stringsAsFactors = FALSE
)
k10_pl <- bblab_payload_from_statements(
  k10_is, ticker = "FIXT", entity_name = "Three-segment 10-K fixture",
  statement_currency = "USD", period = "12/31/2025",
  segment_tables = list(k10_opseg, k10_product, k10_geo)
)
rk10 <- bblab_analyze(k10_pl)
k10_names <- vapply(rk10$businesses, `[[`, "", "name")
check("10-K 3-segment table yields >=2 cards",
      isTRUE(rk10$ok) &&
        identical(rk10$primary_dimension$kind, "operating_segment") &&
        length(rk10$businesses) >= 2L &&
        length(rk10$businesses) <= 6L)
check("10-K 3-segment pie still eligible",
      isTRUE(rk10$chart$eligible) &&
        length(rk10$chart$slices) >= 3L &&
        abs(rk10$chart$denominator - k10_cons_rev) < 1 &&
        all(vapply(rk10$chart$slices, function(s) {
          is.finite(s$share) && s$share > 0
        }, logical(1))))
check("10-K operating-segment names kept; geo/product not mixed",
      all(c("North America", "International", "AWS") %in% k10_names) &&
        !any(grepl("United States|Germany|Online stores|Advertising", k10_names)) &&
        abs(sum(vapply(rk10$businesses, function(c) c$revenue, numeric(1))) - k10_cons_rev) < 1 &&
        "BUSINESS_REVALUATION_UNAVAILABLE" %in% rk10$codes)
check("10-K overlapping geo/product not combined as primary",
      identical(rk10$primary_dimension$kind, "operating_segment") &&
        "BUSINESS_OVERLAPPING_DIMENSIONS_BLOCKED" %in% rk10$codes)
check("filed operating-segment table is not geography-vetoed",
      !"BUSINESS_GEOGRAPHY_CUSTOMER_LOCATION_ONLY" %in%
        (if (is.null(rk10$primary_dimension)) character(0) else
           unique(unlist(lapply(rk10$dimension_scores, function(s) {
             if (identical(s$id, rk10$primary_dimension$id)) s$code else NULL
           })))) &&
        isTRUE(!isTRUE(k10_pl$dimensions[[1]]$is_customer_location_only)) &&
        identical(k10_pl$dimensions[[1]]$kind, "operating_segment"))
check("shared opex not copied onto 10-K GP cards",
      all(vapply(rk10$businesses, function(c) {
        is.null(c$operating_expenses) && !is.finite(c$cor)
      }, logical(1))) &&
        identical(rk10$level, "C"))

k10_hist <- rk10$history
k10_hist_names <- if (length(k10_hist$series)) {
  vapply(k10_hist$series, function(s) s$name, character(1))
} else character(0)
na_series <- Filter(function(s) identical(s$name, "North America"), k10_hist$series)
na_s <- if (length(na_series)) na_series[[1]] else NULL
check("10-K history has comparable shares across fiscal years", {
  isTRUE(k10_hist$eligible) &&
    identical(as.character(k10_hist$years), c("2023", "2024", "2025")) &&
    all(c("North America", "International", "AWS") %in% k10_hist_names) &&
    !is.null(na_s) &&
    abs(na_s$shares[["2025"]] - 426305 / 716924) < 1e-6 &&
    abs(na_s$shares[["2024"]] - 387497 / 637959) < 1e-6 &&
    abs(na_s$shares[["2023"]] - 352828 / 574785) < 1e-6 &&
    all(vapply(k10_hist$series, function(s) {
      all(!is.finite(s$shares) | s$shares > 0)
    }, logical(1)))
})
check("10-K history units aligned to consolidated dollars", {
  isTRUE(k10_hist$eligible) &&
    abs(k10_hist$denominator[["2025"]] - k10_cons_rev) / k10_cons_rev < 0.05 &&
    !is.null(na_s) &&
    na_s$revenues[["2025"]] > 1e9 &&
    abs(na_s$revenues[["2025"]] - 426305e6) / 426305e6 < 0.05
})
check("single-period fixture does not fabricate a 5-year series", {
  r2 <- bblab_analyze(two_seg_level_a())
  !isTRUE(r2$history$eligible) &&
    length(r2$history$years) == 0L &&
    "history_insufficient_years" %in% r2$limitations &&
    "BUSINESS_HISTORY_INSUFFICIENT_YEARS" %in% r2$codes &&
    isTRUE(r2$ok) &&
    length(r2$businesses) == 2L &&
    isTRUE(r2$chart$eligible) &&
    isTRUE(!.bblab_history_visible(r2))
})
check("five-year mix panel shows only when a series is eligible", {
  isTRUE(.bblab_history_visible(rk10)) &&
    isTRUE(!.bblab_history_visible(NULL)) &&
    grepl(".bblab_history_visible", mod_src, fixed = TRUE) &&
    grepl("history_panel", mod_src, fixed = TRUE) &&
    !grepl("history_status", mod_src, fixed = TRUE)
})
check("history insufficient toast does not block cards or donut", {
  t <- bblab_toast_payload("BUSINESS_HISTORY_INSUFFICIENT_YEARS", recon_pass = TRUE,
                           chart_eligible = TRUE)
  !length(t[[1]]$blocked_outputs) &&
    all(c("business_cards", "composition_chart") %in% t[[1]]$remaining_outputs)
})

drop_year_col <- function(tbl, year) {
  h <- as.character(tbl$headers)
  drop <- which(grepl(as.character(year), h, fixed = TRUE))
  if (!length(drop)) return(tbl)
  col <- drop[[1]]
  tbl$headers <- h[-col]
  tbl$rows <- lapply(tbl$rows, function(r) {
    r <- as.character(r)
    if (length(r) >= col) r[-col] else r
  })
  tbl
}
k10_miss <- drop_year_col(k10_opseg, "2024")
k10_miss_pl <- bblab_payload_from_statements(
  k10_is, ticker = "FIXT", entity_name = "Missing-year 10-K fixture",
  statement_currency = "USD", period = "12/31/2025",
  segment_tables = list(k10_miss, k10_product, k10_geo)
)
rk10_miss <- bblab_analyze(k10_miss_pl)
hm <- rk10_miss$history
hm_na <- Filter(function(s) identical(s$name, "North America"), hm$series)
hm_na <- if (length(hm_na)) hm_na[[1]] else NULL
check("missing year is omitted not zero-filled", {
  isTRUE(hm$eligible) &&
    identical(as.character(hm$years), c("2023", "2025")) &&
    !"2024" %in% as.character(hm$years) &&
    !is.null(hm_na) &&
    !("2024" %in% names(hm_na$shares)) &&
    !any(is.finite(hm_na$shares) & hm_na$shares == 0) &&
    abs(hm_na$shares[["2025"]] - 426305 / 716924) < 1e-6 &&
    abs(hm_na$shares[["2023"]] - 352828 / 574785) < 1e-6 &&
    length(rk10_miss$businesses) >= 2L
})
check("two comparable years still plot; never pad to fake five", {
  isTRUE(hm$eligible) &&
    length(hm$years) == 2L &&
    length(hm$years) < 5L &&
    all(as.character(hm$years) %in% c("2023", "2025"))
})
check("named year lookup misses without throwing", {
  miss <- tryCatch(
    .bblab_named_get(setNames(c(1, 2), c("2023", "2024")), "2021"),
    error = function(e) e
  )
  hit <- .bblab_named_get(setNames(c(10, 20), c("2023", "2024")), "2024")
  is.null(miss) && !inherits(miss, "error") && identical(as.numeric(hit)[1], 20)
})
check("IS years beyond note years do not abort analysis", {
  k10_is_extra <- k10_is
  k10_is_extra[["12/31/2022"]] <- k10_is[[2]]
  k10_is_extra[["12/31/2021"]] <- k10_is[[2]]
  extra_pl <- tryCatch(
    bblab_payload_from_statements(
      k10_is_extra, ticker = "FIXT", entity_name = "Longer IS than note table",
      statement_currency = "USD", period = "12/31/2025",
      segment_tables = list(k10_opseg, k10_product, k10_geo)
    ),
    error = function(e) e
  )
  extra_r <- if (is.list(extra_pl) && !inherits(extra_pl, "error")) {
    tryCatch(bblab_analyze(extra_pl), error = function(e) e)
  } else extra_pl
  is.list(extra_pl) && !inherits(extra_pl, "error") &&
    is.list(extra_r) && !inherits(extra_r, "error") &&
    isTRUE(extra_r$ok) &&
    isTRUE(.bblab_finite(extra_r$consolidated$revenue)) &&
    length(extra_r$businesses) >= 2L
})

is_hist <- data.frame(
  Breakdown = c("Total Revenue", "Widgets", "Gadgets", "Cost Of Revenue", "Gross Profit"),
  `12/31/2025` = c("220", "140", "80", "90", "130"),
  `12/31/2024` = c("200", "120", "80", "80", "120"),
  `12/31/2023` = c("180", "100", "80", "70", "110"),
  check.names = FALSE, stringsAsFactors = FALSE
)
is_pl <- bblab_payload_from_statements(
  is_hist, ticker = "FIXT", entity_name = "IS mix fixture",
  statement_currency = "USD"
)
ris <- bblab_analyze(is_pl)
ish <- ris$history
w_s <- Filter(function(s) grepl("widget", s$name, ignore.case = TRUE), ish$series)
w_s <- if (length(w_s)) w_s[[1]] else NULL
check("Yahoo annual product mix becomes history shares", {
  isTRUE(ris$ok) &&
    length(ris$businesses) == 2L &&
    isTRUE(ish$eligible) &&
    identical(as.character(ish$years), c("2023", "2024", "2025")) &&
    !is.null(w_s) &&
    abs(w_s$shares[["2025"]] - 140 / 220) < 1e-8 &&
    abs(w_s$shares[["2024"]] - 120 / 200) < 1e-8 &&
    abs(w_s$shares[["2023"]] - 100 / 180) < 1e-8
})

one_is <- data.frame(
  Breakdown = c("Total Revenue", "Cost Of Revenue", "Gross Profit"),
  `12/31/2025` = c("1000", "420", "580"),
  `12/31/2024` = c("900", "400", "500"),
  `12/31/2023` = c("800", "380", "420"),
  check.names = FALSE, stringsAsFactors = FALSE
)
one_pl <- bblab_payload_from_statements(
  one_is, ticker = "ONE", entity_name = "Single-entity annuals",
  statement_currency = "USD"
)
rone <- bblab_analyze(one_pl)
check("one-business still shows donut; history is 100% when annuals exist", {
  isTRUE(rone$chart$eligible) &&
    length(rone$businesses) == 1L &&
    abs(rone$chart$slices[[1]]$share - 1) < 1e-8 &&
    isTRUE(rone$history$eligible) &&
    length(rone$history$years) >= 2L &&
    all(abs(rone$history$series[[1]]$shares - 1) < 1e-8)
})

one_yr <- data.frame(
  Breakdown = c("Total Revenue", "Cost Of Revenue", "Gross Profit"),
  `12/31/2025` = c("1000", "420", "580"),
  check.names = FALSE, stringsAsFactors = FALSE
)
one_yr_pl <- bblab_payload_from_statements(
  one_yr, ticker = "ONE", entity_name = "Single-year entity",
  statement_currency = "USD"
)
r1y <- bblab_analyze(one_yr_pl)
check("one fiscal year of shares shows limitation not a fake series", {
  !isTRUE(r1y$history$eligible) &&
    "history_insufficient_years" %in% r1y$limitations &&
    isTRUE(r1y$chart$eligible) &&
    length(r1y$businesses) == 1L
})
check("dollar-cell grouped rows still parse", {
  messy <- k10_opseg
  messy$layout <- NULL
  messy$headers <- c("", "Year Ended December 31,")
  messy$rows <- list(
    c("", "2023", "", "2024", "", "2025"),
    c("North America", "", "", "", "", ""),
    c("Net sales", "$", "352,828", "", "", "$", "387,497", "", "", "$", "426,305"),
    c("Operating income", "$", "14,877", "", "", "$", "24,967", "", "", "$", "29,619"),
    c("International", "", "", "", "", ""),
    c("Net sales", "$", "131,200", "", "", "$", "142,906", "", "", "$", "161,894"),
    c("Operating income (loss)", "$", "(2,656)", "", "", "$", "3,792", "", "", "$", "4,750"),
    c("Cloud platform", "", "", "", "", ""),
    c("Net sales", "$", "90,757", "", "", "$", "107,556", "", "", "$", "128,725"),
    c("Operating income", "$", "24,631", "", "", "$", "39,834", "", "", "$", "45,606")
  )
  packed <- .bblab_components_from_table(
    messy, cons = bblab_consolidated(k10_cons_rev, NA, NA, "USD", "2025"),
    period = "2025", currency = "USD", kind = "operating_segment",
    source_label = "Segment Information"
  )
  is.list(packed) && length(packed$components) >= 3L &&
    isTRUE(!isTRUE(packed$is_customer_location_only)) &&
    identical(packed$kind, "operating_segment") &&
    abs(packed$components[[1]]$revenue - 426305e6) < 1
})

# ---- Units: millions vs unscaled consolidated must not yield 0.0% or a recon-donut ----
unscaled_tbl <- k10_opseg
unscaled_tbl$scale <- NULL
unscaled_tbl$caption <- NULL
unscaled_is <- data.frame(
  Breakdown = c("Total Revenue", "Cost Of Revenue", "Gross Profit"),
  TTM = c(as.character(k10_cons_rev), as.character(k10_cons_rev * 0.72),
          as.character(k10_cons_rev * 0.28)),
  `12/31/2025` = c(as.character(k10_cons_rev), as.character(k10_cons_rev * 0.72),
                   as.character(k10_cons_rev * 0.28)),
  check.names = FALSE, stringsAsFactors = FALSE
)
unscaled_pl <- bblab_payload_from_statements(
  unscaled_is, ticker = "FIXT", entity_name = "Unscaled millions fixture",
  statement_currency = "USD",
  segment_tables = list(unscaled_tbl)
)
runscaled <- bblab_analyze(unscaled_pl)
unscaled_shares <- vapply(runscaled$businesses, function(c) c$revenue_pct, numeric(1))
unscaled_revs <- vapply(runscaled$businesses, function(c) c$revenue, numeric(1))
slice_classes <- vapply(runscaled$chart$slices, function(s) s$classification, character(1))
check("scaled millions vs consolidated share is not 0.0%",
      isTRUE(runscaled$ok) &&
        length(runscaled$businesses) >= 2L &&
        all(is.finite(unscaled_shares)) &&
        all(unscaled_shares > 0.05) &&
        all(unscaled_revs > 1e9) &&
        abs(sum(unscaled_revs) - k10_cons_rev) / k10_cons_rev < 0.05)
check("pie eligible after unit alignment; slices not recon-dominated",
      isTRUE(runscaled$chart$eligible) &&
        length(runscaled$chart$slices) >= 2L &&
        abs(runscaled$chart$denominator - k10_cons_rev) / k10_cons_rev < 0.05 &&
        !any(slice_classes == "RECONCILIATION") &&
        all(abs(vapply(runscaled$chart$slices, function(s) s$share, numeric(1)) -
                  vapply(runscaled$chart$slices, function(s) s$revenue / k10_cons_rev, numeric(1))) < 1e-8))
check("table scale infers 1e6 when totals would double the sum", {
  raw <- c(387497, 142906, 107556, 637959)
  abs(.bblab_infer_amount_scale(raw, 637959e6, prior = 1) - 1e6) < 1
})

card_200046 <- list(
  ticker = "FIXT", entity = list(name = "200046-scale fixture"),
  statement_currency = "USD", period = "2024",
  consolidated = bblab_consolidated(500000e6, 300000e6, 200000e6, "USD", "2024"),
  dimensions = list(bblab_dimension(
    "opseg", "operating_segment",
    list(
      bblab_component("a", "Alpha", revenue = 200046, operating_income = 6878,
                      is_reported_segment = TRUE, period = "2024", currency = "USD"),
      bblab_component("b", "Beta", revenue = 299954, operating_income = 9000,
                      is_reported_segment = TRUE, period = "2024", currency = "USD")
    ),
    mutually_exclusive = TRUE, filed_audited = TRUE,
    flags = c("separate_revenue", "relevant", "mutually_exclusive", "filed_audited",
              "distinct_economics", "management_major"),
    period = "2024", currency = "USD"
  ))
)
r200 <- bblab_analyze(card_200046)
check("200046-scale card share and pie use consolidated units",
      abs(r200$businesses[[1]]$revenue - 200046e6) < 1 &&
        abs(r200$businesses[[1]]$revenue_pct - 200046 / 500000) < 1e-8 &&
        isTRUE(r200$chart$eligible) &&
        all(vapply(r200$businesses, function(c) c$revenue_pct > 0.05, logical(1))))
check("OI present does not become GP",
      all(vapply(r200$businesses, function(c) {
        is.finite(c$operating_income) &&
          identical(c$gp_display, "Not reliably estimable") &&
          !is.finite(c$cor) && !is.finite(c$gp) &&
          identical(c$gm_display, "Not reliably estimable")
      }, logical(1))) &&
        identical(r200$level, "C") &&
        abs(r200$businesses[[1]]$operating_income - 6878e6) < 1)
check("reval UNAVAILABLE still shows Rev % and cards",
      "BUSINESS_REVALUATION_UNAVAILABLE" %in% r200$codes &&
        isTRUE(!isTRUE(r200$revaluation_available)) &&
        length(r200$businesses) == 2L &&
        all(vapply(r200$businesses, function(c) {
          is.finite(c$revenue) && is.finite(c$revenue_pct) && c$revenue_pct > 0
        }, logical(1))) &&
        isTRUE(r200$ok))
if (requireNamespace("shiny", quietly = TRUE)) {
  html200 <- paste(as.character(.bblab_card_html(r200$businesses[[1]], "en", FALSE,
                                                 r200$consolidated$revenue)),
                   collapse = " ")
  check("card HTML shows non-zero share and keeps OI when CoR unestimable",
        grepl("Revenue:", html200, fixed = TRUE) &&
          grepl("40.0%", html200, fixed = TRUE) &&
          grepl("Operating income:", html200, fixed = TRUE) &&
          grepl("Not reliably estimable", html200, fixed = TRUE) &&
          grepl("Revaluation ratio unavailable", html200, fixed = TRUE))
}

mapped <- two_seg_level_a()
mapped$dimensions <- list(
  mapped$dimensions[[1]],
  bblab_dimension(
    "prod", "product_service",
    list(
      bblab_component("a", "Business A", revenue = 120, cor = 40, gp = 80,
                      gp_reported = TRUE, period = "2024", currency = "USD"),
      bblab_component("b", "Business B", revenue = 80, cor = 40, gp = 40,
                      gp_reported = TRUE, period = "2024", currency = "USD")
    ),
    flags = c("separate_revenue")
  )
)
mapped$dimensions[[1]]$components <- list(
  bblab_component("a", "Business A", revenue = 120, is_reported_segment = TRUE,
                  period = "2024", currency = "USD"),
  bblab_component("b", "Business B", revenue = 80, is_reported_segment = TRUE,
                  period = "2024", currency = "USD")
)
rmap <- bblab_analyze(mapped)
check("name-matched product CoR maps; OI is not required",
      identical(rmap$level, "A") &&
        all(vapply(rmap$businesses, function(c) is.finite(c$cor) && is.finite(c$gp), logical(1))))

# ---- Listed TW/US disclaimer (always visible; not inside Notes) ----
ui_fn_src <- {
  start <- regexpr("business_breakdown_lab_ui <- function", mod_src, fixed = TRUE)[1]
  stop_at <- regexpr(".bblab_card_html <- function", mod_src, fixed = TRUE)[1]
  if (start < 1L || stop_at < 1L) "" else substr(mod_src, start, stop_at - 1L)
}
notes_fn_src <- {
  start <- regexpr("output$notes <- renderUI", mod_src, fixed = TRUE)[1]
  if (start < 1L) "" else substr(mod_src, start, nchar(mod_src))
}
check("listed-only notice is in report UI function, not Notes",
      nzchar(ui_fn_src) &&
        grepl("ynow_bblab_listed_only_notice", ui_fn_src, fixed = TRUE) &&
        grepl("ynow-bblab__listed-notice", ui_fn_src, fixed = TRUE) &&
        grepl("ynow-bblab--report", ui_fn_src, fixed = TRUE) &&
        !grepl("ynow_notes_block", ui_fn_src, fixed = TRUE) &&
        grepl("ynow_notes_block", notes_fn_src, fixed = TRUE) &&
        !grepl("ynow_bblab_listed_only_notice", notes_fn_src, fixed = TRUE))
check("listed-only notice is not a shinydashboard default box",
      !grepl("status\\s*=\\s*[\"']default[\"']", ui_fn_src) &&
        !grepl("box\\([^)]*ynow_bblab_listed_only_notice", ui_fn_src, perl = TRUE))
check("applyUiLocale wires listed-only notice",
      grepl("setBtText('ynow_bblab_listed_only_notice', 'bblab_listed_only_notice')",
            ui_src, fixed = TRUE) &&
        grepl("setBtText('ynow_bblab_listed_only_scope', 'bblab_listed_only_scope')",
              ui_src, fixed = TRUE))
check("i18n listed-only copy en + zh-TW",
      identical(.UI_STRINGS$en$bblab_listed_only_notice,
                "Listed stocks only (Taiwan and U.S. exchanges).") &&
        identical(.UI_STRINGS$`zh-TW`$bblab_listed_only_notice,
                  "僅支援上市個股分析（台股、美股）。") &&
        identical(.UI_STRINGS$en$bblab_listed_only_scope,
                  paste0(
                    "This ticker does not look like a listed Taiwan or U.S. stock. ",
                    "Business Breakdown only supports listed stocks (Taiwan and U.S. exchanges)."
                  )) &&
        identical(.UI_STRINGS$`zh-TW`$bblab_listed_only_scope,
                  paste0(
                    "此 Ticker 看起來不是上市個股（台股、美股）。",
                    "業務拆解僅支援上市個股分析（台股、美股）。"
                  )) &&
        !grepl("默认|参数|数据|用户|简", .UI_STRINGS$`zh-TW`$bblab_listed_only_notice) &&
        !grepl("默认|参数|数据|用户|简", .UI_STRINGS$`zh-TW`$bblab_listed_only_scope))
check("heuristic allows listed TW/US names",
      isTRUE(!bblab_clearly_not_listed_tw_us("AAPL")) &&
        isTRUE(!bblab_clearly_not_listed_tw_us("2330")) &&
        isTRUE(!bblab_clearly_not_listed_tw_us("2330.TW")) &&
        isTRUE(!bblab_clearly_not_listed_tw_us("6488.TWO")) &&
        isTRUE(!bblab_clearly_not_listed_tw_us("BRK.B")) &&
        isTRUE(!bblab_clearly_not_listed_tw_us("BRK-B")) &&
        isTRUE(!bblab_clearly_not_listed_tw_us("AMZN")) &&
        isTRUE(!bblab_clearly_not_listed_tw_us("")))
check("heuristic flags clearly non-listed TW/US names",
      isTRUE(bblab_clearly_not_listed_tw_us("^GSPC")) &&
        isTRUE(bblab_clearly_not_listed_tw_us("0700.HK")) &&
        isTRUE(bblab_clearly_not_listed_tw_us("BTC-USD")) &&
        isTRUE(bblab_clearly_not_listed_tw_us("7203.T")) &&
        isTRUE(bblab_clearly_not_listed_tw_us("TWD=X")))
check("out-of-scope ticker does not req-stop shared ticker load",
      grepl("listed_scope_on(", mod_src, fixed = TRUE) &&
        grepl("bblab_clearly_not_listed_tw_us", mod_src, fixed = TRUE) &&
        !grepl("req(!bblab_clearly_not_listed_tw_us", mod_src, fixed = TRUE) &&
        grepl("lab_ticker(tk)", mod_src, fixed = TRUE))
check("shared ticker observeEvent never session$reload", {
  mark <- regexpr("Shared global Ticker", mod_src, fixed = TRUE)[1]
  oe_rel <- if (mark < 1L) -1L else {
    regexpr("observeEvent(", substr(mod_src, mark, nchar(mod_src)), fixed = TRUE)[1]
  }
  start <- if (mark < 1L || oe_rel < 1L) -1L else mark + oe_rel - 1L
  next_obs <- if (start < 1L) -1L else {
    rest <- substr(mod_src, start + 13L, nchar(mod_src))
    m2 <- gregexpr("observeEvent(", rest, fixed = TRUE)[[1]][1]
    if (is.na(m2) || m2 < 1L) nchar(mod_src) else start + 13L + as.integer(m2) - 2L
  }
  chunk <- if (start < 1L) "" else substr(mod_src, start, next_obs)
  nzchar(chunk) &&
    !grepl("session$reload", chunk, fixed = TRUE) &&
    !grepl("location.reload", chunk, fixed = TRUE) &&
    !grepl("updateQueryString", chunk, fixed = TRUE) &&
    !grepl("session$reload", mod_src, fixed = TRUE) &&
    grepl("tryCatch", chunk, fixed = TRUE) &&
    grepl("cached_fetch_sec_segment_notes", chunk, fixed = TRUE) &&
    !grepl("cached_fetch_sec_report_notes", chunk, fixed = TRUE) &&
    grepl("current_ticker_rv", chunk, fixed = TRUE) &&
    grepl("length(raw) != 1L", chunk, fixed = TRUE) &&
    grepl("company_advance", chunk, fixed = TRUE) &&
    grepl("Defer Yahoo/SEC heavy path", chunk, fixed = TRUE)
})
check("report UI has no native Search form",
      !grepl("ynow-bblab-search-controls", mod_src, fixed = TRUE) &&
        !grepl("type = \"submit\"", mod_src, fixed = TRUE) &&
        !grepl("tags$form", mod_src, fixed = TRUE) &&
        grepl("ynow-bblab-report__toolbar", mod_src, fixed = TRUE))
check("report shares global ticker; Testing sandbox still hides global chrome",
      grepl("current_ticker_rv", mod_src, fixed = TRUE) &&
        grepl("ynow_bblab_shared_ticker_hint", mod_src, fixed = TRUE) &&
        grepl("input.sidebar_tabs != 'testing'", ui_src, fixed = TRUE) &&
        grepl("input.sidebar_tabs != 'about'", ui_src, fixed = TRUE) &&
        grepl("input.sidebar_tabs != 'macro_market'", ui_src, fixed = TRUE) &&
        grepl("ynow-company-advance-bblab", ui_src, fixed = TRUE))
wide_year <- list(
  headers = c("2025"),
  rows = list(
    c("Group", "Alpha", "Beta", "Total"),
    c("Premiums earned", "100", "40", "140"),
    c("Losses and LAE", "60", "20", "80"),
    c("2024"),
    c("Group", "Alpha", "Beta", "Total"),
    c("Premiums earned", "90", "30", "120"),
    c("Losses and LAE", "50", "15", "65")
  ),
  kind = "operating_segment",
  short_name = "Segment earnings"
)
wide_packed <- .bblab_components_from_table(
  wide_year, cons = list(revenue = 140e6), kind = "operating_segment",
  source_label = "Segment earnings"
)
wide_names <- vapply(wide_packed$components, function(c) c$name, character(1))
check("column segments keep one card per business, not one per year",
      identical(sort(wide_names), c("Alpha", "Beta")) &&
        !any(duplicated(.bblab_norm_label(wide_names))))
check("Shiny reconnects in-session instead of forcing Reload overlay",
      grepl('session$allowReconnect("force")', server_src, fixed = TRUE))

# ---- Business & profitability structure (no invented Segment OI) ----
check("economic role: Corporate / Eliminations are not BUSINESS",
      identical(bblab_classify_economic_role("Corporate / Other", "MAJOR"), "CORPORATE") &&
        identical(bblab_classify_economic_role("Intersegment Eliminations"), "ELIMINATION") &&
        identical(bblab_classify_economic_role("Reconciling Items"), "RECONCILIATION") &&
        identical(bblab_classify_economic_role("Investment Gains"), "NON_OPERATING") &&
        identical(bblab_classify_economic_role(
          "Platform Services", "MAJOR", is_reported_segment = TRUE), "BUSINESS"))
struct_payload <- list(
  ticker = "STRUCT", entity = list(name = "Structure fixture"),
  statement_currency = "USD", period = "2024",
  consolidated = bblab_consolidated(
    revenue = 1000, cor = 400, gp = 600, currency = "USD", period = "2024",
    operating_income = 250
  ),
  dimensions = list(bblab_dimension(
    "opseg", "operating_segment",
    list(
      bblab_component("a", "Segment A", revenue = 600, operating_income = 200,
                      is_reported_segment = TRUE, period = "2024", currency = "USD"),
      bblab_component("b", "Segment B", revenue = 450, operating_income = 80,
                      is_reported_segment = TRUE, period = "2024", currency = "USD"),
      bblab_component("corp", "Corporate / Unallocated", revenue = 0,
                      operating_income = -20, classification = "UNALLOCATED",
                      period = "2024", currency = "USD"),
      bblab_component("elim", "Intersegment Eliminations", revenue = -50,
                      classification = "ELIMINATION",
                      period = "2024", currency = "USD")
    ),
    mutually_exclusive = TRUE, filed_audited = TRUE,
    flags = c("separate_revenue", "relevant", "mutually_exclusive", "filed_audited",
              "distinct_economics", "management_major", "reconciles"),
    period = "2024", currency = "USD"
  ))
)
r_struct <- bblab_analyze(struct_payload)
sa <- r_struct$structure_analysis
sa_biz_names <- vapply(sa$businesses %||% list(), function(r) r$business, character(1))
sa_adj_types <- vapply(sa$adjustments %||% list(), function(r) r$type, character(1))
check("structure keeps only BUSINESS in main table",
      isTRUE(sa$ok) &&
        all(c("Segment A", "Segment B") %in% sa_biz_names) &&
        !any(grepl("Corporate|Eliminat", sa_biz_names, ignore.case = TRUE)))
check("structure separates Corporate / Eliminations as adjustments",
      any(sa_adj_types == "CORPORATE") && any(sa_adj_types == "ELIMINATION"))
check("structure uses disclosed OI and never invents missing OI", {
  rev_only <- list(
    businesses = list(
      bblab_component("x", "Only Rev", revenue = 100, is_reported_segment = TRUE)
    ),
    consolidated = bblab_consolidated(100, NA, NA, "USD", "2024")
  )
  sa2 <- bblab_build_structure_analysis(rev_only)
  identical(sa2$businesses[[1]]$profit_status, "NOT_DISCLOSED") &&
    grepl("Segment profitability not disclosed", sa2$conclusions$primary_profit, fixed = TRUE) &&
    grepl("not disclosed", paste(sa2$missing, collapse = " "), ignore.case = TRUE)
})
check("structure summary separates businesses from adjustments",
      grepl("Segment A", sa$summary_sentence, fixed = TRUE) &&
        grepl("accounting / consolidation", sa$summary_sentence, fixed = TRUE))
check("structure UI chapter wired in module",
      grepl("structure_panel", mod_src, fixed = TRUE) &&
        grepl("structure_conclusions", mod_src, fixed = TRUE) &&
        grepl("ynow_bblab_struct_title", mod_src, fixed = TRUE) &&
        grepl("ynow-bblab-struct-mix__conc", mod_src, fixed = TRUE) &&
        grepl("ynow-bblab-struct-mix__chart", mod_src, fixed = TRUE) &&
        grepl("bblab_build_structure_analysis", mod_src, fixed = TRUE) &&
        grepl("bblab_cards_empty", mod_src, fixed = TRUE) &&
        grepl("ynow_bblab_ch5_help", mod_src, fixed = TRUE))
check("structure sourced from global.R",
      grepl("business_breakdown_structure.R",
            paste(readLines("global.R", warn = FALSE), collapse = "\n"), fixed = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1)
}
cat("All Business Breakdown Lab checks passed.\n")
