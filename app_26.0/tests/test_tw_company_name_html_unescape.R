# test_tw_company_name_html_unescape.R — TWSE &#NNNN; in 公司名稱 must decode
# Run: Rscript app_26.0/tests/test_tw_company_name_html_unescape.R

.app_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f) == 1L && nzchar(f)) dirname(normalizePath(f)) else getwd()
}, error = function(e) getwd())
if (basename(.app_dir) == "tests") .app_dir <- dirname(.app_dir)
if (!file.exists(file.path(.app_dir, "market_profile.R"))) {
  .app_dir <- normalizePath(file.path(getwd(), "app_26.0"), mustWork = FALSE)
}
setwd(.app_dir)

source("market_profile.R", local = TRUE)
source("lab_tw_universe.R", local = TRUE)

check <- function(lab, cond) {
  cat(if (isTRUE(cond)) "OK  " else "FAIL", lab, "\n")
  if (!isTRUE(cond)) quit(status = 1L)
}

raw <- "邁&#33834;科技股份有限公司"
decoded <- unescape_html_text(raw)
check("helper exists", exists("unescape_html_text", mode = "function"))
check("decodes decimal entity", identical(decoded, paste0("邁", intToUtf8(33834L), "科技股份有限公司")))
check("no raw &# left", !grepl("&#", decoded, fixed = TRUE))
check("hex form", identical(unescape_html_text("邁&#x842A;科"), paste0("邁", intToUtf8(0x842AL), "科")))
check("passthrough clean CJK", identical(unescape_html_text("台積電"), "台積電"))

# split_corp_names / clean cands
parts <- split_corp_names_zh_en(raw, "Taiwan Microloops Corp.", ticker = "6831.TW")
check("split zh decoded", identical(parts$zh, decoded))
check("split en kept", grepl("Microloops", parts$en, fixed = TRUE))

# lab_tw_row_to_record ingest path
row <- list(`公司代號` = "6831", `公司名稱` = raw, `產業別` = "25")
rec <- lab_tw_row_to_record(row, "TWSE", "test", "2026-10-13T00:00:00Z")
check("ingest unescapes name", is.data.frame(rec) && identical(rec$name[[1]], decoded))

# Cached universe lookup (CSV should be patched or unescape at lookup)
nm <- lookup_tw_universe_company_name("6831.TW")
check("lookup 6831 has no entity", !grepl("&#", nm, fixed = TRUE))
check("lookup 6831 still CJK", grepl("[\u4e00-\u9fff]", nm))

cat("ALL PASS\n")
