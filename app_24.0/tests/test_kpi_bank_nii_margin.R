# Bank / financial IS: no Gross Profit → NII/Revenue heuristic (not fake GM)
suppressPackageStartupMessages({
  if (!requireNamespace("testthat", quietly = TRUE)) {
    # lightweight runner when testthat is unavailable
  }
})

root <- normalizePath(file.path(".."), mustWork = TRUE)
src_setup <- file.path(root, "setup.R")
src_locale <- file.path(root, "ui_locale.R")
src_kpi <- file.path(root, "kpi_module.R")

# Minimal deps for helpers in setup.R
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
source(src_locale, local = TRUE, encoding = "UTF-8")
# setup.R is large; source only the metric helpers by eval after parsing select helpers.
# Prefer sourcing setup if environment already loads it; else define via source with care.
suppressWarnings(try(source(src_setup, local = TRUE, encoding = "UTF-8"), silent = TRUE))

pass <- 0L
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK " else "FAIL ", label, "\n", sep = "")
  if (ok) pass <<- pass + 1L else fail <<- fail + 1L
}

.mk_is <- function(rows) {
  n <- length(rows[[1]])
  years <- paste0("FY", seq_len(n))
  df <- data.frame(Breakdown = names(rows), stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_len(n)) {
    df[[years[i]]] <- vapply(rows, function(v) suppressWarnings(as.numeric(v[i])), numeric(1))
  }
  df
}

# --- Locale keys ---
check("en key kpi_bank_nii_margin_subtitle",
      nzchar(ui_str("kpi_bank_nii_margin_subtitle", "en")))
check("zh-TW key kpi_bank_nii_margin_subtitle",
      nzchar(ui_str("kpi_bank_nii_margin_subtitle", "zh-TW")))
check("en subtitle not called Gross Margin",
      grepl("not Gross Margin", ui_str("kpi_bank_nii_margin_subtitle", "en"), fixed = TRUE))
check("zh-TW subtitle says 非毛利率",
      grepl("非毛利率", ui_str("kpi_bank_nii_margin_subtitle", "zh-TW"), fixed = TRUE))
check("en unavailable mentions Data-limited",
      grepl("Data-limited", ui_str("kpi_gross_margin_unavailable_subtitle", "en"), fixed = TRUE))
check("zh-TW unavailable mentions 資料受限",
      grepl("資料受限", ui_str("kpi_gross_margin_unavailable_subtitle", "zh-TW"), fixed = TRUE))

# --- Helpers exist ---
check("stmt_has_gross_profit defined", exists("stmt_has_gross_profit", mode = "function"))
check("stmt_is_bank_interest_structure defined",
      exists("stmt_is_bank_interest_structure", mode = "function"))
check("kpi_gross_margin_box_metrics defined",
      exists("kpi_gross_margin_box_metrics", mode = "function"))

if (exists("kpi_gross_margin_box_metrics", mode = "function")) {
  bank <- .mk_is(list(
    `Total Revenue` = c(100, 90, 80),
    `Operating Revenue` = c(100, 90, 80),
    `Net Interest Income` = c(40, 36, 30),
    `Interest Income` = c(55, 50, 45),
    `Interest Expense` = c(15, 14, 15),
    `Net Income` = c(20, 18, 15)
  ))
  m_bank <- kpi_gross_margin_box_metrics(bank)
  check("bank mode is bank_nii", identical(m_bank$mode, "bank_nii"))
  check("bank margin = NII/Rev avg", {
    # get_avg(NII)/get_avg(Rev)*100 = (40+36+30)/3 / (100+90+80)/3 * 100 = 106/270*100
    expected <- ((40 + 36 + 30) / 3) / ((100 + 90 + 80) / 3) * 100
    is.finite(m_bank$margin_pct) && abs(m_bank$margin_pct - expected) < 1e-6
  })
  check("bank growth uses NII series", is.finite(m_bank$growth_pct))
  check("bank not inventing Gross Profit label", !isTRUE(stmt_has_gross_profit(bank)))
  check("bank interest structure detected", isTRUE(stmt_is_bank_interest_structure(bank)))

  corp <- .mk_is(list(
    `Total Revenue` = c(200, 180, 160),
    `Gross Profit` = c(80, 72, 60),
    `Cost Of Revenue` = c(120, 108, 100),
    `Net Income` = c(30, 25, 20)
  ))
  m_corp <- kpi_gross_margin_box_metrics(corp)
  check("corp mode is gross", identical(m_corp$mode, "gross"))
  check("corp margin = GP/Rev", {
    expected <- ((80 + 72 + 60) / 3) / ((200 + 180 + 160) / 3) * 100
    is.finite(m_corp$margin_pct) && abs(m_corp$margin_pct - expected) < 1e-6
  })
  check("corp not bank structure", !isTRUE(stmt_is_bank_interest_structure(corp)))

  bare <- .mk_is(list(
    `Total Revenue` = c(50, 40),
    `Net Income` = c(5, 4)
  ))
  m_bare <- kpi_gross_margin_box_metrics(bare)
  check("no-GP no-NII → unavailable", identical(m_bare$mode, "unavailable"))
  check("unavailable reason missing_gross_profit_row",
        identical(as.character(m_bare$reason)[1], "missing_gross_profit_row"))
  check("unavailable margin NA", !is.finite(m_bare$margin_pct))
}

# Server / module wiring
kpi_txt <- paste(readLines(src_kpi, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("kpi uses kpi_gross_margin_box_metrics",
      grepl("kpi_gross_margin_box_metrics", kpi_txt, fixed = TRUE))
check("kpi bank subtitle key",
      grepl("kpi_bank_nii_margin_subtitle", kpi_txt, fixed = TRUE))
check("kpi unavailable subtitle key",
      grepl("kpi_gross_margin_unavailable_subtitle", kpi_txt, fixed = TRUE))
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"),
             collapse = "\n")
check("kpi_module_server gets ui_locale",
      grepl("kpi_module_server\\([\\s\\S]*ui_locale\\s*=\\s*ui_locale", srv, perl = TRUE))

cat("\nPASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
