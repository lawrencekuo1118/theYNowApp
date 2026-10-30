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
                "param_sotp_manual_label", "param_sotp_manual_help")) {
    stopifnot(nzchar(ui_str(key, locale)))
  }
}
valid <- "name,revenue,multiple\nHardware,100,2\nServices,200,3"
stopifnot(nrow(parse_manual_sotp_segments(valid)) == 2,
          nrow(parse_manual_sotp_segments("name,revenue,multiple\nOnly,100,2")) == 0,
          nrow(parse_manual_sotp_segments("name,revenue,multiple\nA,100,2\nA,200,3")) == 0,
          nrow(parse_manual_sotp_segments("name,revenue,multiple\nA,NA,2\nB,200,3")) == 0,
          nrow(parse_manual_sotp_segments("name,revenue,multiple\nA,100,0\nB,200,3")) == 0)
cat("PASS: seven-engine required inputs, conditional requirements, zero vs missing, bilingual suggestions, immutable inputs, validated manual SOTP segments\n")

# Exercise real Shiny messaging: Full must not write; Lite writes only finite
# references, retains source-missing fields, and exports the applied assumptions.
for (edition in c(FALSE, TRUE)) {
  shiny::testServer(function(input, output, session) {
    sent <- new.env(parent = emptyenv())
    session$sendInputMessage <- function(inputId, message) {
      sent[[inputId]] <- message$value
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
      stopifnot(any(grepl("Assumed value applied by Lite: WACC = 8", assumptions(), fixed = TRUE)))
      session$setInputs(wacc_gordon = 9)
      stopifnot(!any(grepl("WACC = 8", assumptions(), fixed = TRUE)))
    } else stopifnot(length(ls(sent)) == 0, length(assumptions()) == 0)
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
cat("PASS: operating IC excludes cash/nonoperating assets/nondebt liabilities; unknown claims stay missing\n")
