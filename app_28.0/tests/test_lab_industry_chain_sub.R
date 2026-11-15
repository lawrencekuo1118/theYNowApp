#!/usr/bin/env Rscript
# Blue Chip industry chain × sub-industry taxonomy / resolve
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "lab_sp500_universe.R"), local = FALSE)
source(file.path(app_dir, "ui_locale.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "") else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

check("sc.IC_Design chain id", identical(industry_chain_id("sc.IC_Design"), "半導體"))
check("sc.IC_Design sub zh", identical(industry_sub_label("sc.IC_Design", "zh-TW"), "IC 設計"))
check("sc.IC_Design sub en", identical(industry_sub_label("sc.IC_Design", "en"), "IC Design"))
check(
  "chain label en for 半導體",
  identical(industry_chain_label("半導體", "en"), "Semiconductors")
)

chains_zh <- industry_chain_picker_choices("zh-TW")
chains_en <- industry_chain_picker_choices("en")
check("chain picker has 半導體", "半導體" %in% unname(chains_zh))
check("chain picker en values stable", identical(sort(unname(chains_zh)), sort(unname(chains_en))))
check("chain picker en has Semiconductors label", "Semiconductors" %in% names(chains_en))

subs_sc <- industry_sub_picker_choices("半導體", "zh-TW")
check("半導體 has ≥4 subs", length(subs_sc) >= 4L)
check("IC Design in 半導體", "sc.IC_Design" %in% unname(subs_sc))
check("no bank under 半導體", !"fn.Banking" %in% unname(subs_sc))
check("sub labels omit chain prefix", !any(grepl("｜", names(subs_sc), fixed = TRUE)))

# Resolve
r0 <- resolve_industry_filter_keys(character(0), character(0))
check("empty = unrestricted", identical(r0, character(0)))

r_sc <- resolve_industry_filter_keys("半導體", character(0))
check("chain only → sc.* keys", length(r_sc) >= 4L && all(startsWith(r_sc, "sc.")))

r_sub <- resolve_industry_filter_keys(character(0), "sc.IC_Design")
check("sub only", identical(r_sub, "sc.IC_Design"))

r_bad <- resolve_industry_filter_keys("金融", "sc.IC_Design")
check("incompatible → __none__", identical(r_bad, "__none__"))

# Lab helpers include unmapped
lab_ch <- lab_industry_chain_picker_choices("zh-TW")
check("lab chains include 未對應", "未對應" %in% unname(lab_ch))
lab_sub_u <- lab_industry_sub_picker_choices("未對應", "zh-TW")
check("lab unmapped-only sub", identical(unname(lab_sub_u), LAB_UNMAPPED_KEY))

r_lab <- lab_resolve_industry_filter_keys("半導體", character(0))
check("lab resolve chain", length(r_lab) >= 4L && all(startsWith(r_lab, "sc.")))

# Locale keys + UI
for (loc in c("en", "zh-TW")) {
  check(
    paste0("locale keys (", loc, ")"),
    nzchar(ui_str("lab_im_chains_label", loc)) &&
      nzchar(ui_str("lab_im_industries_label", loc)) &&
      nzchar(ui_str("lab_im_chains_hint", loc)) &&
      nzchar(ui_str("lab_im_industries_hint", loc))
  )
}
check("zh labels", identical(ui_str("lab_im_chains_label", "zh-TW"), "產業鏈"))
check("en labels", identical(ui_str("lab_im_chains_label", "en"), "Industry chain"))
check("zh sub label", identical(ui_str("lab_im_industries_label", "zh-TW"), "子產業"))
check("en sub label", identical(ui_str("lab_im_industries_label", "en"), "Sub-industry"))

ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_txt <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "UI has dual pickers",
  grepl('"lab_im_chains"', ui_txt, fixed = TRUE) &&
    grepl("ynow_lab_im_chains_label", ui_txt, fixed = TRUE) &&
    grepl("ynow_lab_im_industries_label", ui_txt, fixed = TRUE) &&
    grepl("lab_industry_chain_picker_choices", ui_txt, fixed = TRUE) &&
    grepl("lab_industry_sub_picker_choices", ui_txt, fixed = TRUE)
)
check(
  "server resolves via lab_im_industry_filter",
  grepl("lab_im_industry_filter <- reactive", srv_txt, fixed = TRUE) &&
    grepl("lab_resolve_industry_filter_keys", srv_txt, fixed = TRUE) &&
    grepl("industry_filter = lab_im_industry_filter()", srv_txt, fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, " check(s)\n", sep = "")
  quit(status = 1L)
}
cat("All industry chain/sub checks passed.\n")
