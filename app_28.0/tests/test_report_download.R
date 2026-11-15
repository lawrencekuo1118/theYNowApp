#!/usr/bin/env Rscript
# Offline regressions: optional DCF, TPEx-style partial statements, Fair Value
# primary selection, and actual HTML/PDF rendering across market/edition/locale.
args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
app_dir <- dirname(dirname(normalizePath(script)))
source(file.path(app_dir, "setup.R"))
stopifnot(requireNamespace("shiny", quietly = TRUE),
          requireNamespace("rmarkdown", quietly = TRUE))
work <- Sys.getenv("YNOW_REPORT_TEST_OUTPUT_DIR", tempfile("report_tests_"))
dir.create(work, recursive = TRUE, showWarnings = FALSE)

for (key in c("dcf", "ddm", "ri", "pb", "nav")) {
  stopifnot(.report_primary_target(key, list(base = 125)) == 125)
  stopifnot(.report_primary_target(key, model_point = function(k) {
    stopifnot(identical(k, key))
    110
  }) == 110)
  stopifnot(is.na(.report_primary_target(key, model_point = function(k) NULL)))
}
for (key in list(NULL, "", "multiples", "sotp")) {
  stopifnot(is.na(.report_primary_target(key, list(base = 999),
                                        function(k) stop("must not fall back"))))
}
stopifnot(is.na(.report_primary_target("ri", model_point = function(k) {
  stop("primary unavailable")
})))
cat("PASS: Fair Value primary; NAV supported; no unrelated DCF fallback\n")

png <- file.path(work, "fcf.png")
shiny::withReactiveDomain(NULL, {
  stopifnot(is.na(.report_save_fcf_plot(function() shiny::req(FALSE), png)))
})
stopifnot(is.na(.report_save_fcf_plot(function() stop("missing CF"), png)),
          is.na(.report_save_fcf_plot(function() NULL, png)),
          is.na(.report_save_fcf_plot(function() stop("Lite must skip"), png, FALSE)))
stopifnot(!file.exists(png))
calls <- 0L
plot_fn <- function() {
  calls <<- calls + 1L
  ggplot2::ggplot(data.frame(year = 1:3, fcf = 1:3),
                  ggplot2::aes(year, fcf)) + ggplot2::geom_col()
}
stopifnot(identical(.report_save_fcf_plot(plot_fn, png), png),
          calls == 1L, file.info(png)$size > 0)
cat("PASS: optional FCF handles Shiny cancellation, errors, Lite and valid plots\n")

# Synthetic partial statements shaped like TPEx's 7772 fixture: no CF or CapEx.
is_partial <- data.frame(Breakdown = c("Total Revenue", "Net Income"),
                         `2025` = c(223895000, 28433000), check.names = FALSE)
bs_partial <- data.frame(Breakdown = c("Stockholders Equity", "Ordinary Shares Number"),
                         `2025` = c(690000000, 24078600), check.names = FALSE)
cf_empty <- data.frame(Breakdown = character())
stopifnot(is.data.frame(build_report_kpi_df(is_partial, bs_partial, cf_empty)),
          is.na(compute_report_f_score(is_partial, bs_partial, cf_empty)$total))

pdf_enabled <- identical(Sys.getenv("YNOW_REPORT_TEST_PDF"), "1")
profiles <- list(
  dcf = list(primary = "dcf", target = 120, market = "US", ticker = "AAPL"),
  bank = list(primary = "pb", target = 60, market = "US", ticker = "JPM"),
  dividend = list(primary = "ddm", target = 50, market = "US", ticker = "KO"),
  nav = list(primary = "nav", target = 80, market = "US", ticker = "REIT"),
  tpex = list(primary = "ri", target = 35, market = "TW", ticker = "7772"),
  no_fv = list(primary = "", target = NA_real_, market = "TW", ticker = "NO_FS")
)
for (profile in names(profiles)) for (locale in c("en", "zh-TW")) for (lite in c(FALSE, TRUE)) {
  p <- profiles[[profile]]
  name <- paste(profile, locale, if (lite) "Lite" else "Full", sep = "_")
  target <- .report_primary_target(p$primary, list(base = p$target))
  copy <- build_ticker_report_copy(
    locale = locale, stock_code = p$ticker, company_name = p$ticker,
    current_price = 30, target_price = target, primary_method = p$primary,
    money_prefix = if (p$market == "TW") "NT$" else "$",
    market_mode = p$market, report_condensed = lite
  )
  html <- file.path(work, paste0(name, ".html"))
  rmarkdown::render(
    file.path(app_dir, "report_template.Rmd"), output_file = basename(html),
    output_dir = work, intermediates_dir = work, quiet = TRUE,
    params = list(
      stock_code = p$ticker, company_name = p$ticker,
      current_price = 30, target_price = target, primary_base = target,
      primary_method = p$primary, report_locale = locale, report_copy = copy,
      market_mode = p$market, report_condensed = lite,
      session_currency = if (p$market == "TW") "TWD" else "USD",
      fcf_plot_path = if (profile == "dcf" && !lite) png else NA_character_,
      kpi_df = build_report_kpi_df(is_partial, bs_partial, cf_empty),
      income_df = if (profile == "tpex") is_partial else NULL,
      balance_df = if (profile == "tpex") bs_partial else NULL,
      cashflow_df = NULL
    ), envir = new.env(parent = globalenv())
  )
  body <- paste(readLines(html, warn = FALSE), collapse = "\n")
  stopifnot(grepl(p$ticker, body, fixed = TRUE),
            grepl('<div class="report-cover">', body, fixed = TRUE),
            !grepl("params$", body, fixed = TRUE),
            !grepl("```{r", body, fixed = TRUE))
  if (profile == "tpex") {
    stopifnot(grepl("7772", body, fixed = TRUE), grepl("NT$35.00", body, fixed = TRUE))
  }
  if (pdf_enabled && locale == "en" && profile %in% c("dcf", "tpex", "no_fv")) {
    pdf <- file.path(work, paste0(name, ".pdf"))
    render_report_pdf(html, pdf)
    header <- readChar(pdf, 5, useBytes = TRUE)
    stopifnot(identical(header, "%PDF-"), file.info(pdf)$size > 1000)
  }
  cat("PASS:", name, "\n")
}
cat("ALL REPORT DOWNLOAD CHECKS PASSED\n")
