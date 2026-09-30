#!/usr/bin/env Rscript
# Business Breakdown Lab — 26 spec tests + Lite hide / parse / no-valuation-wiring.
# Run: cd app_19.0 && Rscript tests/test_business_breakdown_lab.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) dirname(normalizePath(file_arg)) else getwd()
root <- if (file.exists(file.path(test_dir, "..", "business_breakdown_engine.R"))) {
  normalizePath(file.path(test_dir, ".."))
} else if (file.exists("business_breakdown_engine.R")) {
  normalizePath(".")
} else if (dir.exists("app_19.0") && file.exists("app_19.0/business_breakdown_engine.R")) {
  normalizePath("app_19.0")
} else stop("Cannot locate business_breakdown_engine.R")
setwd(root)

source("setup.R", local = TRUE, encoding = "UTF-8")
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_schema.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_engine.R", local = TRUE, encoding = "UTF-8")
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
# 8–9 Single business: one card, no pie, do not fabricate
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
check("08 single card no pie",
      length(r1$businesses) == 1L &&
        !isTRUE(r1$chart$eligible) &&
        "BUSINESS_CHART_SINGLE_COMPONENT" %in% r1$chart$codes &&
        "no_multi_business_split" %in% r1$limitations)
check("09 no fabricated second business", length(r1$businesses) == 1L && is.null(r1$other))

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
# 21 Chart: ≥2, same period/ccy/dimension, recon pass; denom = reported consolidated
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
check("24 chart fail keeps cards",
      !isTRUE(r1$chart$eligible) && length(r1$businesses) == 1L)

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
# 26 Lite hides Full-only tab
# =====================================================================
check("26 tab id business_breakdown_lab",
      grepl('tabName = "business_breakdown_lab"', ui_src, fixed = TRUE) &&
        grepl("ynow_menu_bblab", ui_src, fixed = TRUE) &&
        grepl("Experimental Feature", mod_src, fixed = TRUE) &&
        grepl("business_breakdown_lab_ui", ui_src, fixed = TRUE))
check("26 Lite CSS hide",
      grepl('data-value="business_breakdown_lab"', ui_src, fixed = TRUE) &&
        grepl("body.ynow-lite .sidebar-menu a\\[data-value=\"business_breakdown_lab\"\\]", ui_src) &&
        grepl("'business_breakdown_lab'", ui_src, fixed = TRUE) &&
        grepl("ynow-full-only", mod_src, fixed = TRUE))

# ---- extras: i18n, parse, valuation isolation, toast specificity ----
for (k in c("menu_business_breakdown_lab", "bblab_experimental_badge", "bblab_page_sub",
            "bblab_gm_unestimable", "bblab_reval_unavailable", "bblab_single_business_note",
            "notif_bblab_required_fx_rate_missing", "notif_bblab_business_chart_single_component")) {
  en <- .UI_STRINGS$en[[k]]
  zh <- .UI_STRINGS$`zh-TW`[[k]]
  check(paste("i18n", k), is.character(en) && nzchar(en[1]) && is.character(zh) && nzchar(zh[1]))
}
check("i18n experimental both locales",
      identical(.UI_STRINGS$en$bblab_experimental_badge, "Experimental Feature") &&
        identical(.UI_STRINGS$`zh-TW`$bblab_experimental_badge, "實驗功能") &&
        identical(.UI_STRINGS$en$menu_business_breakdown_lab, "Business Breakdown Lab"))
check("i18n zh-TW uses 預設 not 默认",
      grepl("預設", .UI_STRINGS$`zh-TW`$bblab_fallback_gm_label, fixed = TRUE) &&
        !grepl("默认|参数|数据|用户", paste(.UI_STRINGS$`zh-TW`$bblab_page_sub,
                                         .UI_STRINGS$`zh-TW`$bblab_notes_body,
                                         .UI_STRINGS$`zh-TW`$bblab_fallback_gm_label, sep = " ")))

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

parse_files <- c("business_breakdown_schema.R", "business_breakdown_engine.R",
                 "business_breakdown_module.R", "global.R", "ui_locale.R")
for (pf in parse_files) {
  okp <- inherits(tryCatch(parse(pf, encoding = "UTF-8"), error = function(e) e), "expression")
  check(paste("parse", pf), isTRUE(okp))
}

check("progress stages 1-9", identical(length(BBLAB_PROGRESS_STAGES), 9L))
check("FX missing blocks display only", {
  fxm <- bblab_classify_fx_adr("TWD", "USD", usd_twd = NA_real_)
  "REQUIRED_FX_RATE_MISSING" %in% fxm$codes && isTRUE(fxm$fx_required)
})
check("toasts are specific", {
  t <- bblab_toast_payload("BUSINESS_CHART_SINGLE_COMPONENT", recon_pass = TRUE)
  identical(t[[1]]$blocked_outputs, "composition_chart") &&
    "business_cards" %in% t[[1]]$remaining_outputs
})
check("notes helper used", grepl("ynow_notes_block", mod_src, fixed = TRUE))
check("no invalid shinydashboard box status default",
      !grepl("status\\s*=\\s*[\"']default[\"']", mod_src))
if (requireNamespace("shiny", quietly = TRUE) &&
    requireNamespace("shinydashboard", quietly = TRUE)) {
  suppressPackageStartupMessages({
    library(shiny)
    library(shinydashboard)
  })
  ui_err <- tryCatch({
    business_breakdown_lab_ui("bblab")
    NULL
  }, error = function(e) conditionMessage(e))
  check("lab UI constructs at startup", is.null(ui_err))
}
check("experimental badge locale keys",
      grepl("ynow_bblab_experimental_badge", ui_src, fixed = TRUE) &&
        grepl("bblab_experimental_badge", ui_src, fixed = TRUE))

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

if (requireNamespace("shiny", quietly = TRUE)) {
  card_html <- paste(as.character(.bblab_card_html(rconsol$businesses[[1]], "en", FALSE, 1000)),
                     collapse = " ")
  check("card HTML keeps Revenue/CoR/GP when reval unavailable",
        grepl("Revenue:", card_html, fixed = TRUE) &&
          grepl("Cost of Revenue:", card_html, fixed = TRUE) &&
          grepl("Gross Profit:", card_html, fixed = TRUE) &&
          grepl("1,000", card_html, fixed = TRUE) &&
          grepl("Revaluation ratio unavailable", card_html, fixed = TRUE))
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
      !grepl("TSM|2330|N3|N5|N7|HPC", engine_src2))
check("shared opex from IS not on GP cards",
      is.finite(search_two$shared_corporate$rd) &&
        all(vapply(rsrch$businesses, function(c) is.null(c$rd) && is.null(c$ga), logical(1))))
check("module skips reval unavailable page toast",
      grepl("BUSINESS_REVALUATION_UNAVAILABLE", mod_src, fixed = TRUE) &&
        grepl("Scoped degradation", mod_src, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1)
}
cat("All Business Breakdown Lab checks passed.\n")
