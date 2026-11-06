#!/usr/bin/env Rscript
# QueryConditions module: catalog, bridge, assert helper, UI wiring.
# Run: cd app_27.0/tests && Rscript test_query_conditions.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source(file.path(app_dir, "ui_locale.R"), local = FALSE)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)
source(file.path(app_dir, "query_conditions_module.R"), local = FALSE)
# APP_DEFAULTS may be absent in offline unit tests; catalog falls back.
if (!exists("APP_DEFAULTS")) {
  APP_DEFAULTS <<- list(lab_im_methods = c("nav", "pb", "dcf", "ddm", "ri"))
}

check("ynq_lab_catalog validates", {
  cat0 <- ynq_lab_catalog(c("nav", "dcf", "ddm", "ri", "pb"))
  isTRUE(ynq_validate_catalog(cat0))
})

check("production ids match LAB_METHOD taxonomy", {
  cat0 <- ynq_lab_catalog()
  abs_ids <- cat0$id[cat0$legacy_group == "absolute"]
  rel_ids <- cat0$id[cat0$legacy_group == "relative"]
  identical(sort(abs_ids), sort(LAB_METHOD_ABS_KEYS)) &&
    identical(sort(rel_ids), sort(LAB_METHOD_REL_KEYS))
})

check("Justified P/B is justified ui_group but absolute legacy", {
  cat0 <- ynq_lab_catalog()
  row <- cat0[cat0$id == "pb", , drop = FALSE]
  nrow(row) == 1L &&
    identical(as.character(row$ui_group[[1]]), "justified") &&
    identical(as.character(row$legacy_group[[1]]), "absolute")
})

check("bridge maps __ALL__ chain to all picker chains", {
  p <- list(
    chain = ynq_all,
    subindustry = ynq_all,
    selected_models = c("nav", "dcf"),
    earnings_quality = TRUE,
    piotroski = FALSE,
    no_warning = TRUE,
    include_adr = FALSE
  )
  b <- ynq_filters_to_lab_bridge(p, locale = "en")
  all_ch <- unname(as.character(lab_industry_chain_picker_choices("en")))
  is.list(b) &&
    identical(sort(as.character(b$lab_im_chains)), sort(all_ch)) &&
    identical(sort(as.character(b$lab_im_methods)), sort(c("nav", "dcf"))) &&
    isTRUE(b$lab_im_eq_only) &&
    !isTRUE(b$lab_im_gate_only) &&
    isTRUE(b$lab_im_no_alert) &&
    !isTRUE(b$lab_im_include_adr)
})

check("ynq_chain_select_choices bakes picker chains (not only __ALL__)", {
  ch <- ynq_chain_select_choices("en")
  all_ch <- unname(as.character(lab_industry_chain_picker_choices("en")))
  vals <- unname(as.character(ch))
  labs <- names(ch)
  mapped_labs <- labs[vals != ynq_all & vals != "未對應"]
  length(ch) >= 2L &&
    identical(vals[[1]], ynq_all) &&
    all(all_ch %in% vals) &&
    !any(grepl("[\u4e00-\u9fff]", mapped_labs))
})

check("ynq_lab_industry_tree non-empty", {
  tree <- ynq_lab_industry_tree("en")
  is.data.frame(tree) &&
    nrow(tree) > 0L &&
    all(c("chain", "subindustry") %in% names(tree))
})

check("ynq_assert_same_candidates passes on equal tickers", {
  a <- data.frame(ticker = c("A", "B"), stringsAsFactors = FALSE)
  b <- data.frame(ticker = c("B", "A"), stringsAsFactors = FALSE)
  isTRUE(ynq_assert_same_candidates(a, b, id_column = "ticker"))
})

check("ynq_assert_same_candidates fails on mismatch", {
  a <- data.frame(ticker = c("A"), stringsAsFactors = FALSE)
  b <- data.frame(ticker = c("A", "B"), stringsAsFactors = FALSE)
  out <- tryCatch(
    ynq_assert_same_candidates(a, b, id_column = "ticker"),
    error = function(e) e
  )
  inherits(out, "error")
})

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
glb <- paste(readLines(file.path(app_dir, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("global sources query_conditions_module.R",
      grepl('source\\("query_conditions_module\\.R"', glb))
check("UI embeds ynq_query_ui lab_ynq",
      grepl('ynq_query_ui\\(\\s*"lab_ynq"', ui) &&
        grepl("embed\\s*=\\s*TRUE", ui) &&
        grepl("locale\\s*=\\s*\\.bc_loc", ui))
check("UI keeps bridge lab_im_methods helper",
      grepl("\\.lab_im_methods_filter_ui\\(APP_DEFAULTS\\$lab_im_methods\\)", ui))
check("UI keeps TW boards outside module",
      grepl('"lab_im_boards"', ui) && grepl("ynow-lab-im-boards", ui, fixed = TRUE))
check("server mounts ynq_query_server",
      grepl('ynq_query_server\\(\\s*"lab_ynq"', srv) &&
        grepl("ynq_filters_to_lab_bridge", srv, fixed = TRUE))
check("server refresh_token wired for lazy Blue Chip remount", {
  grepl("refresh_token\\s*=\\s*reactive", srv) &&
    grepl("lazy_mount_nonce\\(\\)", srv) &&
    grepl("\\.ynow_bluechip_mounted\\(\\)", srv)
})
check("lock JS also targets ynq wrap",
      grepl("lab_ynq-wrap", ui, fixed = TRUE) &&
        grepl("ynowLabImMethodsLock", ui, fixed = TRUE))

check("en locale ynq keys", {
  identical(ui_str("ynq_sec_models", "en"), "Valuation models") &&
    identical(ui_str("ynq_grp_intrinsic", "en"), "Intrinsic value") &&
    identical(ui_str("ynq_help_stats_sum", "en"), "Statistic scope notes") &&
    !grepl("[\u4e00-\u9fff]", ui_str("ynq_sec_pool", "en"))
})
check("zh-TW locale ynq keys (Taiwan wording)", {
  identical(ui_str("ynq_sec_models", "zh-TW"), "估值模型") &&
    identical(ui_str("ynq_grp_justified", "zh-TW"), "理論倍數") &&
    identical(ui_str("ynq_help_stats_sum", "zh-TW"), "統計範圍說明") &&
    !grepl("統計口徑|默认|参数|数据", ui_str("ynq_help_stats_sum", "zh-TW")) &&
    !grepl("統計口徑", ui_str("ynq_help_stats_body", "zh-TW"))
})

mod <- paste(readLines(file.path(app_dir, "query_conditions_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("module avoids Mainland 統計口徑", !grepl("統計口徑", mod, fixed = TRUE))
check("Lite: pool/models/results marked full-only; quality stays open", {
  grepl("ynq-card ynow-full-only", mod, fixed = TRUE) &&
    grepl("ynq_sec_quality", mod, fixed = TRUE) &&
    grepl('class = "ynq-card"', mod) # quality section without full-only on same card opener path
})

# Valuation models checkboxes: one equal-height row per topic; wrap without clip.
check("model optgroup uses auto-fit equal-height row grid", {
  grepl(
    "\\.ynq-optgroup \\.shiny-options-group\\{[\\s\\S]*?grid-template-columns:repeat\\(auto-fit,minmax\\(min\\(100%,7\\.5rem\\),1fr\\)\\)",
    mod,
    perl = TRUE
  ) &&
    grepl(
      "\\.ynq-optgroup \\.shiny-options-group\\{[\\s\\S]*?align-items:stretch",
      mod,
      perl = TRUE
    ) &&
    !grepl(
      "\\.ynq-optgroup \\.shiny-options-group\\{display:grid;grid-template-columns:repeat\\(2,",
      mod,
      fixed = FALSE
    )
})
check("model checkbox labels stretch to equal height", {
  grepl(
    "\\.ynq-optgroup \\.checkbox label\\{[\\s\\S]*?height:100%",
    mod,
    perl = TRUE
  ) &&
    grepl(
      "\\.ynq-optgroup \\.checkbox label\\{[\\s\\S]*?overflow-wrap:anywhere",
      mod,
      perl = TRUE
    )
})
# Match Include ADRs / Earnings quality: box on the label's left edge (not the
# -20px bleed from #690, and not vertically centered in the stretched row).
# Text gutter stays Bootstrap's 20px. Equal-height stretch stays on the label.
check("model checkboxes share ADR left edge and first-line gutter", {
  grepl(
    "\\.ynq-optgroup \\.checkbox input\\[type='checkbox'\\]\\{[\\s\\S]*?position:absolute",
    mod,
    perl = TRUE
  ) &&
    grepl(
      "\\.ynq-optgroup \\.checkbox input\\[type='checkbox'\\]\\{[\\s\\S]*?left:0",
      mod,
      perl = TRUE
    ) &&
    grepl(
      "\\.ynq-optgroup \\.checkbox input\\[type='checkbox'\\]\\{[\\s\\S]*?top:4px",
      mod,
      perl = TRUE
    ) &&
    grepl(
      "\\.ynq-optgroup \\.checkbox input\\[type='checkbox'\\]\\{[\\s\\S]*?margin:0",
      mod,
      perl = TRUE
    ) &&
    grepl(
      "\\.ynq-optgroup \\.checkbox label\\{[\\s\\S]*?align-items:flex-start",
      mod,
      perl = TRUE
    ) &&
    grepl(
      "\\.ynq-optgroup \\.checkbox label\\{[\\s\\S]*?padding:4px 0 4px 20px",
      mod,
      perl = TRUE
    ) &&
    !grepl(
      "\\.ynq-optgroup \\.checkbox input\\[type='checkbox'\\]\\{[\\s\\S]*?margin-left:-20px",
      mod,
      perl = TRUE
    )
})
check("narrow viewport still wraps model checkboxes", {
  grepl(
    "@media\\(max-width:640px\\)\\{[\\s\\S]*?\\.ynq-optgroup \\.shiny-options-group\\{[\\s\\S]*?auto-fit,minmax\\(min\\(100%,6\\.5rem\\)",
    mod,
    perl = TRUE
  ) &&
    grepl(
      "@media\\(max-width:360px\\)\\{[\\s\\S]*?grid-template-columns:minmax\\(0,1fr\\)",
      mod,
      perl = TRUE
    )
})
check("Valuation models still has Intrinsic/Justified/Multiples topics", {
  grepl('group_input\\("intrinsic"\\)', mod) &&
    grepl('group_input\\("justified"\\)', mod) &&
    grepl('group_input\\("multiples"\\)', mod) &&
    grepl("ynq_sec_models", mod, fixed = TRUE)
})

if (fail > 0L) {
  cat("FAILED ", fail, " check(s)\n", sep = "")
  quit(status = 1L)
}
cat("PASS test_query_conditions\n")
