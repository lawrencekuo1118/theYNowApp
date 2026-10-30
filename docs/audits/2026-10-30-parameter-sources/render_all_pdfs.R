# Run the existing report regressions with real PDF output for all 24 cases.
args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
root <- normalizePath(file.path(dirname(script), "../../.."))
app <- file.path(root, "app_27.0")
code <- readLines(file.path(app, "tests/test_report_download.R"))
code <- sub("app_dir <- dirname(dirname(normalizePath(script)))",
            paste0("app_dir <- ", deparse(app)), code, fixed = TRUE)
code <- sub('if (pdf_enabled && locale == "en" && profile %in% c("dcf", "tpex", "no_fv"))',
            'if (pdf_enabled)', code, fixed = TRUE)
Sys.setenv(YNOW_REPORT_TEST_PDF = "1")
eval(parse(text = code), envir = globalenv())
