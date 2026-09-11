#!/usr/bin/env Rscript
# TW financial-statement label localization (display layer only).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

`%||%` <- function(a, b) if (!is.null(a)) a else b
source(file.path(app_dir, "market_profile.R"), local = TRUE, encoding = "UTF-8")
source(file.path(app_dir, "ui_locale.R"), local = TRUE, encoding = "UTF-8")
source(file.path(app_dir, "fs_locale_zh_tw.R"), local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "")
  else { cat("FAIL ", label, "\n", sep = ""); fail <<- fail + 1L }
}

check("TW mode enables zh-TW FS", isTRUE(should_localize_fs_zh_tw(mode = "TW")))
check("US mode keeps English FS", isFALSE(should_localize_fs_zh_tw(mode = "US", locale = "en")))
check("locale zh-TW enables FS", isTRUE(should_localize_fs_zh_tw(locale = "zh-TW")))

check(
  "Total Revenue → 營業收入總額",
  identical(localize_fs_label_zh_tw("Total Revenue"), "營業收入總額")
)
check(
  "Net Income → 淨利",
  identical(localize_fs_label_zh_tw("Net Income"), "淨利")
)
check(
  "Capital Expenditure → 資本支出",
  identical(localize_fs_label_zh_tw("Capital Expenditure"), "資本支出")
)
check(
  "Stockholders Equity → 股東權益",
  identical(localize_fs_label_zh_tw("Stockholders Equity"), "股東權益")
)
check(
  "And/ampersand normalize",
  identical(
    localize_fs_label_zh_tw("Selling General & Administration"),
    "銷管費用"
  )
)
check(
  "Unknown label kept",
  identical(localize_fs_label_zh_tw("Totally Made Up Line"), "Totally Made Up Line")
)

df <- data.frame(
  Breakdown = c("Total Revenue", "Net Income", "Free Cash Flow"),
  TTM = c(100, 20, 15),
  stringsAsFactors = FALSE
)
zh <- localize_financial_df_zh_tw(df, enabled = TRUE)
check("col Breakdown → 科目", identical(colnames(zh)[1], "科目"))
check("col TTM localized", identical(colnames(zh)[2], "近十二個月（TTM）"))
check("row labels zh", identical(zh[[1]][1], "營業收入總額"))
check("values untouched", isTRUE(all.equal(zh$`近十二個月（TTM）`, c(100, 20, 15))))

en <- localize_financial_df_zh_tw(df, enabled = FALSE)
check("disabled keeps English", identical(en$Breakdown[1], "Total Revenue"))

ch_tw <- is_metric_choices_for_locale("TW")
check("IS choices values English", identical(unname(ch_tw), c("Total Revenue", "Gross Profit", "EBITDA")))
check("IS choices labels zh", identical(names(ch_tw)[1], "營業收入總額"))

check(
  "summary section Price",
  identical(localize_summary_section_zh_tw("Price", TRUE), "股價")
)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1)
}
cat("All TW FS locale checks passed.\n")
