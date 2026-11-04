#!/usr/bin/env Rscript
args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
app <- dirname(dirname(normalizePath(script)))
source(file.path(app, "setup.R"))
source(file.path(app, "ui_locale.R"))
source(file.path(app, "parameter_completeness.R"))
source(file.path(app, "industry_standards.R"))
source(file.path(app, "default_config.R"))
for (locale in c("en", "zh-TW")) {
  values <- list(dcf_mode = "two_stage", dcf_claim = "fcff",
                 `mod_ddm-ddm_mode` = "gordon", `mod_ri-roe_method` = "linear",
                 `mod_pb-basis` = "tbvps", `mod_rel-rel_mode` = "enterprise",
                 `.sotp_ready` = FALSE)
  before <- values
  rows <- ynow_required_parameter_rows(values, APP_DEFAULTS, locale, 9, 8)
  stopifnot(identical(values, before),
            all(c("dcf", "ddm", "ri", "pb", "nav", "multiples", "sotp") %in% rows$model),
            all(nzchar(rows$tab)), all(nzchar(rows$hint)),
            "mod_sotp-manual_segments" %in% rows$id,
            "mod_ri-roe_terminal" %in% rows$id,
            "mod_pb-tbvps" %in% rows$id,
            !"mod_pb-bvps" %in% rows$id)
  cash_hint <- rows$hint[rows$id == "bridge_cash"]
  stopifnot(all(grepl(ui_str("param_missing_no_reference", locale), cash_hint, fixed = TRUE)))
  values$bridge_cash <- 0
  values$bridge_debt <- 0
  values$sgr <- 0
  zero_rows <- ynow_required_parameter_rows(values, APP_DEFAULTS, locale)
  stopifnot(!any(zero_rows$id %in% c("bridge_cash", "bridge_debt", "sgr")))
  values$dcf_claim <- "fcfe"
  values$bridge_cash <- NULL
  fcfe <- ynow_required_parameter_rows(values, APP_DEFAULTS, locale)
  stopifnot(!any(fcfe$model == "dcf" & fcfe$id == "bridge_cash"),
            any(fcfe$model == "dcf" & fcfe$id == "wacc_re"))
  values[["mod_rel-rel_mode"]] <- "equity"
  pe <- ynow_required_parameter_rows(values, APP_DEFAULTS, locale)
  stopifnot(!any(pe$model == "multiples" & pe$id %in% c("bridge_cash", "bridge_debt")))
  for (key in c("param_missing_title", "param_missing_body", "param_missing_go_fill",
                "param_missing_reference", "param_missing_no_reference",
                "param_assumption_applied", "param_assumption_footnotes_title",
                "param_sotp_manual_label", "param_sotp_manual_help")) {
    stopifnot(nzchar(ui_str(key, locale)))
  }
}
stopifnot(identical(ui_str("param_assumption_footnotes_title", "en"),
                    "Notes — Lite applied assumptions"),
          identical(ui_str("param_assumption_footnotes_title", "zh-TW"),
                    "註解 — Lite 已套用假設"))
foot_ui <- ynow_lite_assumption_footnotes_ui(
  "Assumed value applied by Lite: WACC = 8. Reference suggestion: 8.", "en")
stopifnot(inherits(foot_ui, "shiny.tag"),
          identical(foot_ui$name, "aside"),
          identical(foot_ui$attribs$class, "ynow-lite-assumption-footnotes"),
          is.null(ynow_lite_assumption_footnotes_ui(character(0), "en")),
          is.null(ynow_lite_assumption_footnotes_ui(NULL, "zh-TW")))
cat("PASS: Lite assumption footnote helper + bilingual title keys\n")
valid <- "name,revenue,multiple\nHardware,100,2\nServices,200,3"
stopifnot(nrow(parse_manual_sotp_segments(valid)) == 2,
          nrow(parse_manual_sotp_segments("name,revenue,multiple\nOnly,100,2")) == 0,
          nrow(parse_manual_sotp_segments("name,revenue,multiple\nA,100,2\nA,200,3")) == 0,
          nrow(parse_manual_sotp_segments("name,revenue,multiple\nA,NA,2\nB,200,3")) == 0,
          nrow(parse_manual_sotp_segments("name,revenue,multiple\nA,100,0\nB,200,3")) == 0)
cat("PASS: seven-engine required inputs, conditional requirements, zero vs missing, bilingual suggestions, immutable inputs, validated manual SOTP segments\n")

# Exercise real Shiny messaging: Full must not write; Lite writes only finite
# references, retains source-missing fields, and exports the applied assumptions
# as footnote payload (not mid-page field callouts).
for (edition in c(FALSE, TRUE)) {
  shiny::testServer(function(input, output, session) {
    sent <- new.env(parent = emptyenv())
    session$sendInputMessage <- function(inputId, message) {
      sent[[inputId]] <- message$value
    }
    session$sendCustomMessage <- function(type, message) {
      if (identical(type, "ynowParameterGuidance")) sent$guidance <<- message
    }
    assumptions <- ynow_parameter_guidance_server(input, session,
      searched = function() TRUE, ticker = function() "TEST", busy = function() FALSE,
      model_rec = function() list(primary = "dcf"), locale = function() "en",
      defaults = APP_DEFAULTS, ke = function() .09, wacc = function() .08,
      lite = function() edition)
  }, {
    session$setInputs(dcf_mode = "gordon", dcf_claim = "fcff")
    session$flushReact()
    session$elapse(400)
    session$flushReact()
    if (edition) {
      stopifnot(identical(sent$wacc_gordon, 8), is.null(sent$bridge_cash),
                is.null(sent[["mod_fcf-fcf_nopat"]]))
      session$setInputs(wacc_gordon = 8)
      session$flushReact()
      session$elapse(400)
      session$flushReact()
      stopifnot(any(grepl("Assumed value applied by Lite: WACC = 8", assumptions(), fixed = TRUE)))
      g <- sent$guidance
      stopifnot(is.list(g), is.list(g$assumptions), length(g$assumptions) >= 1L,
                identical(g$assumptionTitle, "Notes — Lite applied assumptions"),
                any(vapply(g$assumptions, function(x) {
                  isTRUE(identical(x$id, "wacc_gordon")) &&
                    grepl("Assumed value applied by Lite: WACC = 8", x$hint %||% "", fixed = TRUE)
                }, logical(1))),
                !any(vapply(g$items %||% list(), function(x) {
                  grepl("Assumed value applied by Lite", x$hint %||% "", fixed = TRUE)
                }, logical(1))))
      session$setInputs(wacc_gordon = 9)
      stopifnot(!any(grepl("WACC = 8", assumptions(), fixed = TRUE)))
    } else stopifnot(length(setdiff(ls(sent), "guidance")) == 0, length(assumptions()) == 0)
  })
}
cat("PASS: Lite applies finite references with assumption disclosure; Full stays manual; missing financial facts stay missing; manual edits remove stale disclosures\n")
shiny::testServer(function(input, output, session) {
  symbol <- shiny::reactiveVal("A")
  guard <- ynow_input_echo_guard(input, session, symbol, "metric")
}, {
  session$setInputs(metric = 100)
  guard$update(session, "metric", value = 200)
  session$flushReact()
  stopifnot(!guard$ready())
  session$setInputs(metric = 200)
  stopifnot(guard$ready())
  session$setInputs(metric = 250)
  stopifnot(guard$ready()) # manual edits are valid after source acknowledgment
  guard$update(session, "metric", value = NA_real_)
  session$flushReact()
  stopifnot(!guard$ready())
  session$setInputs(metric = NA_real_)
  stopifnot(guard$ready())
  session$setInputs(metric = 300)
  stopifnot(guard$ready()) # manual fill after an honestly missing source
})
cat("PASS: source echo gate rejects old ticker values and accepts later manual fills\n")
bs <- data.frame(Breakdown = c("Total Assets", "Total Liabilities", "Non Operating Assets",
  "Cash And Cash Equivalents", "Total Debt", "Common Stock Equity", "Total Equity Gross Minority Interest"),
  `2025` = c(1000, 500, 50, 100, 200, 500, 500), check.names = FALSE)
stopifnot(statement_operating_invested_capital(bs) == 550,
          statement_other_equity_claims(bs) == 0)
bs[7,2] <- 550
stopifnot(is.na(statement_other_equity_claims(bs)))
stopifnot(is.na(statement_operating_invested_capital(bs[-3, ])))
# Book proxy stays finite for Lite silent DCF when market-value claims are unknown.
bs_nci <- data.frame(
  Breakdown = c("Common Stock Equity", "Total Equity Gross Minority Interest",
                "Minority Interest", "Preferred Stock"),
  `2025` = c(500, 550, 50, 0), check.names = FALSE
)
stopifnot(is.na(statement_other_equity_claims(bs_nci)))
stopifnot(identical(statement_other_equity_claims_book(bs_nci), 50))
bs_missing_lines <- data.frame(
  Breakdown = c("Common Stock Equity", "Total Equity Gross Minority Interest"),
  `2025` = c(500, 550), check.names = FALSE
)
stopifnot(identical(statement_other_equity_claims_book(bs_missing_lines), 0))
cat("PASS: operating IC excludes cash/nonoperating assets/nondebt liabilities; unknown claims stay missing\n")
cat("PASS: Lite book proxy for other equity claims uses Minority Interest (+ preferred)\n")

# A late source refresh may clear a default that the browser already echoed.
# Lite must reapply it; Full must never write it. RI g shares central SGR defaults.
source(file.path(app, "ri_module.R"))
for (locale in c("en", "zh-TW")) {
  refs <- ynow_required_parameter_rows(list(), APP_DEFAULTS, locale, lite = TRUE)
  stopifnot(refs$suggestion[refs$id == "mod_ri-ri_g"] == APP_DEFAULTS$sgr,
    refs$suggestion[refs$id == "mod_ri-ri_payout"] == 40,
    refs$suggestion[refs$id == "mod_ri-ri_roe"] == 15,
    is.finite(refs$suggestion[refs$id == "wacc_gordon"]),
    is.finite(refs$suggestion[refs$id == "mod_ri-ri_ke"]),
    all(is.na(refs$suggestion[refs$id %in% c("bridge_cash", "bridge_shares", "mod_ri-b0")])) )
}
shiny::testServer(function(input, output, session) {
  sent <- new.env(parent = emptyenv())
  session$sendInputMessage <- function(inputId, message) sent[[inputId]] <- message$value
  assumptions <- ynow_parameter_guidance_server(input, session,
    searched = function() TRUE, ticker = function() "TEST", busy = function() FALSE,
    model_rec = function() list(primary = "ri"), locale = function() "zh-TW",
    defaults = APP_DEFAULTS, ke = function() .10, wacc = function() .08,
    lite = function() TRUE)
}, {
  session$setInputs(`mod_ri-b0` = 100)
  session$elapse(400)
  stopifnot(sent[["mod_ri-ri_payout"]] == 40)
  # A real source can arrive before the debounced missing list catches up.
  rm(list = "mod_ri-ri_payout", envir = sent)
  session$setInputs(`mod_ri-ri_payout` = 35)
  session$flushReact()
  stopifnot(is.null(sent[["mod_ri-ri_payout"]]))
  session$setInputs(`mod_ri-ri_payout` = 40)
  session$elapse(400)
  stopifnot(any(grepl("Lite 已套用假設數值", assumptions(), fixed = TRUE)))
  session$setInputs(`mod_ri-ri_payout` = NA_real_)
  session$elapse(400)
  stopifnot(sent[["mod_ri-ri_payout"]] == 40)
  result <- compute_ri_valuation(100, .10, .02, APP_DEFAULTS$ri_years,
    sent[["mod_ri-ri_payout"]] / 100, rep(APP_DEFAULTS$ri_roe / 100, APP_DEFAULTS$ri_years))
  stopifnot(result$status == "success", is.finite(result$intrinsic), result$df$Dividend[1] == 6)
  # Zero payout is a different assumption; NA must never silently mean zero.
  stopifnot(compute_ri_valuation(100, .1, .02, 5, NA_real_, rep(.15, 5))$status == "error")
})
cat("PASS: Lite RI forecast defaults, late refresh reapplication, actual RI calculation and missing-payout rejection\n")

# Execute the production readiness gate, including Two-Stage with an absent
# hidden Gordon rate. No copied readiness formula is used here.
assignments <- list()
walk <- function(x) {
  if (missing(x) || (!is.call(x) && !is.expression(x))) return()
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.symbol(x[[2]]))
    assignments[[as.character(x[[2]])]] <<- x[[3]]
  for (j in seq_along(x)) walk(x[[j]])
}
walk(parse(file.path(app, "ynow_server.R")))
for (edition in c(FALSE, TRUE)) for (mode in c("gordon", "two_stage")) {
  e <- new.env(parent = globalenv())
  e$isolate <- shiny::isolate
  e$lite_mode <- function() edition
  e$.model_integrity_ready <- function(model) TRUE
  e$lite_desired_des <- function() list(two_stage = mode == "two_stage")
  e$fcf_results <- list(source_ready = function() TRUE, df_fcf = function() data.frame(FCFF = 1:5))
  e$calculated_wacc <- function() NA_real_
  e$.auto_calc_shares_ready <- function() TRUE
  e$.shared_bridge <- function() list(revenue = 100)
  e$d_income_statement <- function() NULL
  e$select_current_metric <- function(...) 100
  e$input <- list(years = 5, dcf_mode = mode, dcf_claim = "fcff", sgr = 2,
    wacc_gordon = if (mode == "gordon") 8 else NA_real_,
    wacc_stage1 = 8, wacc_stage2 = 8)
  ready <- eval(assignments[[".auto_calc_primary_ready"]], e)
  stopifnot(identical(ready("dcf"), edition))
  e$calculated_wacc <- function() .08
  stopifnot(ready("dcf"))
  e$ri_results <- list(source_ready = function() TRUE)
  e$input <- list(`mod_ri-b0` = 100, `mod_ri-ri_ke` = 10, `mod_ri-ri_g` = 2,
    `mod_ri-ri_roe` = 15, `mod_ri-ri_payout` = NA_real_, `mod_ri-ri_years` = 5)
  stopifnot(!ready("ri"))
  e$input[["mod_ri-ri_payout"]] <- 40
  stopifnot(ready("ri"))
}
cat("PASS: production readiness gates use disclosed Lite Gordon/Two-Stage defaults; Full requires calculated rates; RI waits for payout\n")

# A source update can erase a default within a single flush, so the browser
# never echoes the intermediate value. A bounded delayed retry must recover.
shiny::testServer(function(input,output,session) {
 sends <- new.env(parent=emptyenv());sends$count<-0L
 session$sendInputMessage <- function(id,message) {
  if(id=='wacc_gordon')sends$count<-sends$count+1L
 }
 assumptions <- ynow_parameter_guidance_server(input,session,
  searched=function()TRUE,ticker=function()'LOST-ECHO',busy=function()FALSE,
  model_rec=function()list(primary='dcf'),locale=function()'en',defaults=APP_DEFAULTS,
  ke=function()NA_real_,wacc=function()NA_real_,lite=function()TRUE)
}, {
 session$setInputs(dcf_mode='gordon',dcf_claim='fcff',wacc_gordon=NA_real_)
 session$elapse(400);session$flushReact()
 stopifnot(sends$count==1L)
 # Deliberately never echo the first update (equivalent to an intervening NA).
 deadline<-Sys.time()+3
 while(sends$count<2L&&Sys.time()<deadline) {later::run_now(.85);session$flushReact()}
 stopifnot(sends$count>=2L,sends$count<=4L)
 session$setInputs(wacc_gordon=APP_DEFAULTS$wacc_gordon)
 session$elapse(400);session$flushReact()
 before<-sends$count
 later::run_now(.85);session$flushReact()
 stopifnot(sends$count==before)
})
cat('PASS: lost default echo recovers with bounded session-safe retries; finite source values are never overwritten\n')
shiny::testServer(function(input,output,session) {
 symbol<-shiny::reactiveVal('A');sends<-new.env(parent=emptyenv());sends$count<-0L
 session$sendInputMessage<-function(id,message)if(id=='wacc_gordon')sends$count<-sends$count+1L
 assumptions<-ynow_parameter_guidance_server(input,session,searched=function()TRUE,ticker=symbol,
  busy=function()FALSE,model_rec=function()list(primary='dcf'),locale=function()'en',
  defaults=APP_DEFAULTS,ke=function()NA_real_,wacc=function()NA_real_,lite=function()TRUE)
},{
 session$setInputs(dcf_mode='gordon',wacc_gordon=NA_real_)
 session$elapse(400);session$flushReact()
 stopifnot(sends$count==1L)
 for(i in 1:5){later::run_now(.85);session$flushReact()}
 stopifnot(sends$count==4L) # no unbounded retries on an absent input binding
 session$setInputs(wacc_gordon=7)
 symbol('B');session$flushReact();session$elapse(400)
 before<-sends$count
 later::run_now(.85);session$flushReact()
 stopifnot(sends$count==before,input$wacc_gordon==7)
})
cat('PASS: default retry cap is four; stock changes cancel old retry generations and retain finite user/source input\n')

# Lite silent DCF must use disclosed UI WACC when calculated_wacc is NA.
# Full silent path still waits (no estimate written).
cell <- function(initial = NULL) {
  value <- initial
  function(x) { if (!missing(x)) value <<- x; value }
}
for (edition in c(FALSE, TRUE)) {
  e <- new.env(parent = globalenv())
  e$isolate <- function(x) x
  e$lite_mode <- function() edition
  e$.model_integrity_ready <- function(model) TRUE
  e$lite_dcf_silent <- function() TRUE
  e$lite_desired_des <- function() list(two_stage = FALSE, claim = "fcff", method = "sgr")
  e$fcf_results <- list(df_fcf = function() data.frame(FCFF = c(10, 12, 14, 16, 18)))
  e$calculated_wacc <- function() NA_real_
  e$current_ticker <- function() "AAPL"
  e$input <- list(years = 5, dcf_mode = "gordon", dcf_claim = "fcff", sgr = 2,
                  wacc_gordon = 9, use_estimated_re = FALSE)
  e$.effective_dcf_scenario <- eval(assignments[[".effective_dcf_scenario"]], e)
  e$.shared_bridge <- function() list(cash = 20, debt = 10, shares = 10, other_claims = 0)
  e$.valuation_shares <- function() list(shares = 10, method = "balance_sheet")
  e$.dcf_per_share <- function(eq, sh) eq / sh$shares
  e$stock_price_estimate_val <- cell()
  e$dcf_value_result <- cell()
  e$dcf_audit_snap <- cell()
  e$lite_dcf_block <- cell()
  e$req <- function(...) invisible(TRUE)
  e$updateNumericInput <- function(...) invisible(NULL)
  e$APP_DEFAULTS <- APP_DEFAULTS
  e$.execute_dcf_calc <- eval(assignments[[".execute_dcf_calc"]], e)
  e$.execute_dcf_calc(notify = FALSE)
  px <- e$stock_price_estimate_val()
  if (isTRUE(edition)) {
    stopifnot(is.finite(px), px > 0)
  } else {
    stopifnot(is.null(px) || !is.finite(suppressWarnings(as.numeric(px)[1])))
  }
}
cat("PASS: Lite silent DCF uses disclosed WACC when calculated_wacc is NA; Full silent still waits\n")

# Missing-parameter toast: Model Dashboard → Quant Backtest Lab only; locked = no toast.
range_tabs <- ynow_missing_params_notify_tabs()
stopifnot(identical(range_tabs[1], "get_started"),
          identical(range_tabs[length(range_tabs)], "lab_notes"),
          all(c("dcf_calculator", "hfv", "sotp_calculator", "nav_calculator",
                "pb_calculator", "ddm_calculator", "ri_calculator",
                "rel_mult_ev", "rel_multiples_calculator", "rel_mult_rev") %in% range_tabs),
          !"home" %in% range_tabs,
          !"dashboard" %in% range_tabs,
          !"decision_checklist" %in% range_tabs,
          !"smart_analysis" %in% range_tabs,
          !"mos_assumptions" %in% range_tabs)
for (ok in c("get_started", "dcf_calculator", "hfv", "lab_notes")) {
  stopifnot(isTRUE(ynow_missing_params_notify_allowed(ok)),
            !isTRUE(ynow_missing_params_notify_allowed(ok, locked = TRUE)))
}
for (off in c("home", "dashboard", "smart_analysis", "decision_checklist",
              "price_alerts", "about", "", NA_character_)) {
  stopifnot(!isTRUE(ynow_missing_params_notify_allowed(off)),
            !isTRUE(ynow_missing_params_notify_allowed(off, locked = TRUE)))
}
param_src <- paste(readLines(file.path(app, "parameter_completeness.R"),
                             warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_src <- paste(readLines(file.path(app, "ynow_server.R"),
                           warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl("ynow_missing_params_notify_allowed\\(active_tab, locked = is_locked\\)", param_src),
  grepl("input\\$sidebar_tabs", param_src),
  grepl("tab_locked = function\\(tab\\) FALSE", param_src),
  grepl("!isTRUE\\(allow_toast\\)", param_src),
  grepl("tab_locked = \\.ynow_missing_params_tab_locked", srv_src),
  grepl("ynow_sidebar_tab_guest_locked", srv_src),
  grepl("ynow_access_is_locked", srv_src)
)
# Guidance server still runs field hints out of range; toast path gated by allow_toast.
shiny::testServer(function(input, output, session) {
  sent <- new.env(parent = emptyenv())
  session$sendCustomMessage <- function(type, message) {
    if (identical(type, "ynowParameterGuidance")) sent$guidance <<- message
  }
  session$sendInputMessage <- function(...) invisible(NULL)
  ynow_parameter_guidance_server(
    input, session,
    searched = function() TRUE, ticker = function() "TEST", busy = function() FALSE,
    model_rec = function() list(primary = "dcf"), locale = function() "en",
    defaults = APP_DEFAULTS, ke = function() .09, wacc = function() .08,
    lite = function() FALSE,
    tab_locked = function(tab) identical(tab, "lab_notes")
  )
}, {
  session$setInputs(sidebar_tabs = "home", dcf_mode = "gordon", dcf_claim = "fcff")
  session$elapse(400)
  session$flushReact()
  stopifnot(is.list(sent$guidance), length(sent$guidance$items) >= 1L)
  session$setInputs(sidebar_tabs = "dcf_calculator")
  session$elapse(400)
  session$flushReact()
  stopifnot(is.list(sent$guidance), length(sent$guidance$items) >= 1L)
  session$setInputs(sidebar_tabs = "lab_notes")
  session$elapse(400)
  session$flushReact()
  stopifnot(is.list(sent$guidance), length(sent$guidance$items) >= 1L)
})
cat("PASS: missing-parameter toast scoped to Model Dashboard→Quant Backtest Lab; locked tabs suppress\n")

# Applied Lite assumptions render as tab-footer footnotes, not field help-blocks.
js_src <- paste(readLines(file.path(app, "www/parameter_completeness.js"),
                           warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui_src <- paste(readLines(file.path(app, "ynow_ui.R"),
                          warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_src2 <- paste(readLines(file.path(app, "ynow_server.R"),
                            warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl("message\\.assumptions", js_src),
  grepl("ynow-lite-assumption-footnotes", js_src),
  grepl("ynow-lite-assumption-footnotes", ui_src),
  grepl("smart_analysis_assumption_notes", ui_src),
  grepl("output\\$smart_analysis_assumption_notes", srv_src2),
  grepl("ynow_lite_assumption_footnotes_ui", srv_src2),
  # Mid-page Smart Analysis alert-info for applied assumptions must be gone.
  !grepl("if \\(length\\(notes\\)\\) tags\\$div\\(class = \"alert alert-info\"", srv_src2),
  grepl("assumptions = assumption_items", param_src),
  grepl("assumptionTitle", param_src)
)
cat("PASS: Lite applied assumptions routed to footer footnotes (JS + Smart Analysis)\n")
