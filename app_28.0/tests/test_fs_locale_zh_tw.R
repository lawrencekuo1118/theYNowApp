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

check("TW mode enables thousands scaling", {
  isTRUE(should_scale_fs_thousands("TW")) &&
    isTRUE(should_scale_fs_thousands("TAIWAN")) &&
    isFALSE(should_scale_fs_thousands("US"))
})
check("EPS and share rows skip thousands", {
  isFALSE(fs_row_scale_to_thousands("Basic EPS")) &&
    isFALSE(fs_row_scale_to_thousands("Diluted EPS")) &&
    isFALSE(fs_row_scale_to_thousands("Share Issued")) &&
    isFALSE(fs_row_scale_to_thousands("Tax Rate For Calcs")) &&
    isFALSE(fs_row_scale_to_thousands("基本每股盈餘")) &&
    isTRUE(fs_row_scale_to_thousands("Total Revenue")) &&
    isTRUE(fs_row_scale_to_thousands("營業收入"))
})

check("scale_financial_df_thousands_display ÷1000", {
  df_t <- data.frame(
    Metric = c("Total Revenue", "Basic EPS", "Share Issued"),
    `2024` = c(1000000, 12.5, 5000),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out <- scale_financial_df_thousands_display(df_t, enabled = TRUE, divisor = 1000)
  same <- scale_financial_df_thousands_display(df_t, enabled = FALSE)
  isTRUE(all.equal(as.numeric(out[["2024"]][1]), 1000)) &&
    isTRUE(all.equal(as.numeric(out[["2024"]][2]), 12.5)) &&
    isTRUE(all.equal(as.numeric(out[["2024"]][3]), 5000)) &&
    identical(same[["2024"]], df_t[["2024"]])
})

check("thousands unit caption", {
  cap <- fs_thousands_unit_caption("TWD", TRUE)
  is.character(cap) && grepl("仟元", cap) && grepl("新台幣", cap) &&
    is.null(fs_thousands_unit_caption("USD", FALSE))
})

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1)
}
cat("All TW FS locale checks passed.\n")
