#!/usr/bin/env Rscript
# Device / browser language tags → app UI locale (en | zh-TW).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

`%||%` <- function(a, b) if (is.null(a)) b else a
source(file.path(app_dir, "ui_locale.R"), local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

check("helper exists", exists("locale_from_device_language", mode = "function"))

check("en", identical(locale_from_device_language("en"), "en"))
check("en-US", identical(locale_from_device_language("en-US"), "en"))
check("en_GB underscore", identical(locale_from_device_language("en_GB"), "en"))
check("English", identical(locale_from_device_language("English"), "en"))

check("zh", identical(locale_from_device_language("zh"), "zh-TW"))
check("zh-TW", identical(locale_from_device_language("zh-TW"), "zh-TW"))
check("zh-Hant", identical(locale_from_device_language("zh-Hant"), "zh-TW"))
check("zh-Hant-TW", identical(locale_from_device_language("zh-Hant-TW"), "zh-TW"))
check("zh-CN still zh-TW", identical(locale_from_device_language("zh-CN"), "zh-TW"))
check("Chinese", identical(locale_from_device_language("Chinese"), "zh-TW"))

check(
  "preference order first match",
  identical(locale_from_device_language(c("fr-FR", "zh-TW", "en")), "zh-TW")
)
check(
  "comma-separated navigator.languages",
  identical(locale_from_device_language("zh-TW,en-US;q=0.8"), "zh-TW")
)
check(
  "Accept-Language q-weight ignored for match",
  identical(locale_from_device_language("en-US;q=0.9,zh-TW;q=0.8"), "en")
)
check("unknown → en", identical(locale_from_device_language("ja-JP"), "en"))
check("empty → en", identical(locale_from_device_language(""), "en"))
check("NULL → en", identical(locale_from_device_language(NULL), "en"))
check(
  "custom default",
  identical(locale_from_device_language("ja", default = "zh-TW"), "zh-TW")
)
check(
  "normalize_ui_locale still zh-TW aliases",
  identical(normalize_ui_locale("zh"), "zh-TW") &&
    identical(normalize_ui_locale("en-US"), "en")
)

if (fail > 0L) {
  cat("FAILED: ", fail, " check(s)\n", sep = "")
  quit(status = 1L)
}
cat("All device UI locale mapping checks passed.\n")
