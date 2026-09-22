#!/usr/bin/env Rscript
# Ticker PDF report helpers (broker-style copy pack; no network).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "setup.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

pack_zh <- build_ticker_report_copy(
  locale = "zh-TW",
  stock_code = "AAPL",
  company_name = "Apple Inc.",
  current_price = 100,
  target_price = 120,
  primary_method = "DCF／FCFF",
  method_rationale = "FCF 穩健，採 DCF 為主。",
  margin_of_safety = (120 - 100) / 120 * 100,
  upside_pct = 20,
  dcf_price = 118,
  pb_value = 110,
  primary_bear = 90,
  primary_base = 120,
  primary_bull = 150,
  wacc = "8%",
  terminal_growth = "2.5%",
  forecast_years = 5,
  dcf_mode = "明確預測期 + Gordon 終值",
  eps = 6,
  bvps = 4,
  roe_pct = 25,
  rev_growth_pct = 8,
  capex_rev_pct = 3,
  capex_avg_pct = 2.5,
  capex_n_years = 4L,
  fscore_total = 7,
  money_prefix = "$"
)
check("zh section1 title", identical(pack_zh$titles$section1, "壹、投資建議"))
check("MOS formula present", grepl("MOS", pack_zh$mos_formula, fixed = TRUE))
check("target formula has 120", grepl("120", pack_zh$target_formula, fixed = TRUE))
check("DCF bullet", any(grepl("DCF", pack_zh$investment_bullets)))
check("Bear/Base/Bull bullet", any(grepl("Bear", pack_zh$investment_bullets)))
check("no peer/lab narrative in bullets", !any(grepl("同業排名|Lab 宇宙|peer ranking", pack_zh$investment_bullets, ignore.case = TRUE)))
check("CapEx growth bullet", any(grepl("CapEx", pack_zh$growth_bullets)))
check("implied P/B = 30", is.finite(pack_zh$implied_pb) && abs(pack_zh$implied_pb - 30) < 1e-6)
check("disclaimer excludes peers", grepl("同業排名|Lab", pack_zh$titles$disclaimer))
check("analysis paragraphs count >= 6", length(pack_zh$analysis_paragraphs) >= 6L)
check("analysis para1 has MOS", {
  p1 <- pack_zh$analysis_paragraphs[[1]]
  grepl("MOS", p1$body, fixed = TRUE)
})
check("analysis para title 投資立場", {
  identical(pack_zh$analysis_paragraphs[[1]]$title, "投資立場與目標價")
})
check("analysis excludes buy-signal wording misuse", {
  bodies <- vapply(pack_zh$analysis_paragraphs, function(p) p$body, character(1))
  !any(grepl("建議買進|強力買進|Buy now", bodies, ignore.case = TRUE)) &&
    any(grepl("非買進訊號|never a buy signal|不作券商", bodies))
})

pack_en <- build_ticker_report_copy(
  locale = "en",
  current_price = 50,
  target_price = 40,
  primary_method = "P/B",
  margin_of_safety = (40 - 50) / 40 * 100,
  upside_pct = -20,
  money_prefix = "$"
)
check("en section1 title", identical(pack_en$titles$section1, "I. Investment view"))
check("en disclaimer mentions peer exclusion", grepl("peer ranking|lab-universe", pack_en$titles$disclaimer, ignore.case = TRUE))
check("en analysis paragraphs >= 6", length(pack_en$analysis_paragraphs) >= 6L)
check("en analysis heading key", identical(pack_en$titles$analysis_heading, "Analysis notes (numbered)"))

m <- matrix(
  c(100, 110, 120, 105, 115, 125),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("WACC 8%", "WACC 9%"), c("g 1%", "g 2%", "g 3%"))
)
df <- .report_sensitivity_df(m)
check("sensitivity df rows", is.data.frame(df) && nrow(df) == 2L)

tpl <- file.path(app_dir, "report_template.Rmd")
txt <- paste(readLines(tpl, warn = FALSE), collapse = "\n")
check("template has report_copy", grepl("report_copy", txt, fixed = TRUE))
check("template has sensitivity", grepl("sensitivity_df", txt, fixed = TRUE))
check("template no peer-comp section", !grepl("同業比較（相對比較法）", txt, fixed = TRUE))
check("template logo blue", grepl("#0C5484", txt, fixed = TRUE))
check("template logo green", grepl("#249C60", txt, fixed = TRUE))
check("template analysis-box", grepl("analysis-box", txt, fixed = TRUE))
check("template report_condensed", grepl("report_condensed", txt, fixed = TRUE))
check("locale keys download_report_about exist", {
  loc_path <- file.path(app_dir, "ui_locale.R")
  loc_txt <- paste(readLines(loc_path, warn = FALSE), collapse = "\n")
  grepl("download_report_about", loc_txt, fixed = TRUE)
})

# Smoke-render HTML (no chrome) with minimal params
tmp_html <- tempfile(fileext = ".html")
ok_render <- FALSE
if (requireNamespace("rmarkdown", quietly = TRUE)) {
  ok_render <- tryCatch({
    rmarkdown::render(
      input = tpl,
      output_file = basename(tmp_html),
      output_dir = dirname(tmp_html),
      intermediates_dir = tempdir(),
      clean = TRUE,
      quiet = TRUE,
      params = list(
        stock_code = "AAPL",
        company_name = "Apple Inc.",
        sector = "Technology",
        industry = "Consumer Electronics",
        report_date = "2026/09/22",
        rating = "未評等（僅列 MOS／價差）",
        rating_en = "NR",
        rating_color = "#6c757d",
        current_price = 100,
        target_price = 120,
        upside_pct = 20,
        dcf_price = 118,
        ddm_value = NA_real_,
        pb_value = 110,
        ri_value = NA_real_,
        ev_value = 2e12,
        margin_of_safety = 16.6667,
        primary_method = "DCF／FCFF",
        method_rationale = "FCF 穩健。",
        primary_bear = 90,
        primary_base = 120,
        primary_bull = 150,
        secondary_point = NA_real_,
        confidence_level = "Medium",
        confidence_score = 0.6,
        wacc = "8%",
        terminal_growth = "2.5%",
        forecast_years = 5,
        dcf_mode = "明確預測期 + Gordon 終值",
        market_cap = "3T",
        pe_ratio = "28",
        beta = "1.1",
        dividend_yield = "0.5%",
        eps = 6,
        bvps = 4,
        kpi_df = data.frame(指標 = "ROE", 數值 = "25.0%", check.names = FALSE),
        fcf_plot_path = NA_character_,
        warnings = "",
        fscore_total = 7,
        fscore_quality = 1,
        fscore_checklist = data.frame(`檢驗維度` = "ROA > 0", `得分` = "通過", check.names = FALSE),
        investment_highlights = pack_zh$investment_bullets,
        summary_df = data.frame(Item = "Market Cap", Value = "3T", stringsAsFactors = FALSE),
        income_df = NULL,
        balance_df = NULL,
        cashflow_df = NULL,
        session_currency = "USD",
        fx_usd_twd = 32,
        report_locale = "zh-TW",
        report_copy = pack_zh,
        sensitivity_df = df,
        app_version = "v17.92",
        report_condensed = FALSE
      ),
      envir = new.env(parent = globalenv())
    )
    out_html <- file.path(dirname(tmp_html), basename(tmp_html))
    exists_ok <- file.exists(out_html) || file.exists(tmp_html)
    if (exists_ok) {
      html_body <- paste(readLines(if (file.exists(out_html)) out_html else tmp_html, warn = FALSE), collapse = "\n")
      check("rendered has logo blue", grepl("#0C5484", html_body, fixed = TRUE))
      check("rendered has analysis notes", grepl("分析說明|投資立場與目標價", html_body))
      check("rendered has analysis-box", grepl("analysis-box", html_body, fixed = TRUE))
    }
    exists_ok
  }, error = function(e) {
    cat("RENDER_ERR ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
}
check("rmarkdown HTML smoke render", isTRUE(ok_render))

# Condensed (Lite) smoke — appendix omitted
tmp_html_lite <- tempfile(fileext = ".html")
ok_lite <- FALSE
if (requireNamespace("rmarkdown", quietly = TRUE)) {
  ok_lite <- tryCatch({
    rmarkdown::render(
      input = tpl,
      output_file = basename(tmp_html_lite),
      output_dir = dirname(tmp_html_lite),
      intermediates_dir = tempdir(),
      clean = TRUE,
      quiet = TRUE,
      params = list(
        stock_code = "AAPL",
        company_name = "Apple Inc.",
        sector = "Technology",
        industry = "Consumer Electronics",
        report_date = "2026/09/22",
        rating = "NR",
        rating_en = "NR",
        rating_color = "#6c757d",
        current_price = 100,
        target_price = 120,
        upside_pct = 20,
        dcf_price = 118,
        ddm_value = NA_real_,
        pb_value = 110,
        ri_value = NA_real_,
        ev_value = 2e12,
        margin_of_safety = 16.6667,
        primary_method = "DCF／FCFF",
        method_rationale = "FCF 穩健。",
        primary_bear = 90,
        primary_base = 120,
        primary_bull = 150,
        secondary_point = NA_real_,
        confidence_level = "Medium",
        confidence_score = 0.6,
        wacc = "8%",
        terminal_growth = "2.5%",
        forecast_years = 5,
        dcf_mode = "明確預測期 + Gordon 終值",
        market_cap = "3T",
        pe_ratio = "28",
        beta = "1.1",
        dividend_yield = "0.5%",
        eps = 6,
        bvps = 4,
        kpi_df = NULL,
        fcf_plot_path = NA_character_,
        warnings = "",
        fscore_total = 7,
        fscore_quality = 1,
        fscore_checklist = NULL,
        investment_highlights = pack_zh$investment_bullets,
        summary_df = data.frame(Item = "Market Cap", Value = "3T", stringsAsFactors = FALSE),
        income_df = data.frame(Item = "Revenue", `2024` = "1", check.names = FALSE),
        balance_df = NULL,
        cashflow_df = NULL,
        session_currency = "USD",
        fx_usd_twd = 32,
        report_locale = "zh-TW",
        report_copy = pack_zh,
        sensitivity_df = NULL,
        app_version = "v17.92",
        report_condensed = TRUE
      ),
      envir = new.env(parent = globalenv())
    )
    out_l <- file.path(dirname(tmp_html_lite), basename(tmp_html_lite))
    path_l <- if (file.exists(out_l)) out_l else tmp_html_lite
    if (!file.exists(path_l)) return(FALSE)
    body_l <- paste(readLines(path_l, warn = FALSE), collapse = "\n")
    check("lite badge present", grepl("Lite", body_l, fixed = TRUE) || grepl("lite-badge", body_l, fixed = TRUE))
    check("lite omits income appendix table", !grepl(">Revenue<", body_l, fixed = TRUE))
    check("lite appendix note", grepl("精簡版省略財報附錄|omits statement", body_l))
    TRUE
  }, error = function(e) {
    cat("LITE_RENDER_ERR ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
}
check("rmarkdown Lite condensed smoke render", isTRUE(ok_lite))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
