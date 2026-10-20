# Membership lock catalog — every gated feature id is A or B.
# Run: Rscript app_25.0/tests/test_membership_locks.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_membership_locks.R", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

ok <- 0L
bad <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    message("OK  ", label)
  } else {
    bad <<- bad + 1L
    message("FAIL ", label)
  }
  invisible(isTRUE(cond))
}

source(file.path(app_dir, "membership_locks.R"), local = TRUE, encoding = "UTF-8")
source(file.path(app_dir, "auth_account.R"), local = TRUE, encoding = "UTF-8")
source(file.path(app_dir, "points_wallet.R"), local = TRUE, encoding = "UTF-8")

check("assert classified passes", {
  isTRUE(tryCatch(ynow_membership_lock_assert_classified(), error = function(e) FALSE))
})

cat <- ynow_membership_lock_catalog()
check("catalog non-empty", nrow(cat) >= 20L)
check("all tiers A or B", all(cat$tier %in% c("A", "B")))
check("no duplicate ids", !anyDuplicated(cat$id))

# Required product surfaces
need_a <- c(
  "lab_notes", "decision_checklist", "price_alerts",
  "account_admin", "account_points",
  "lab", "checklist", "alerts", "account",
  "download_snapshot", "download_param_restore", "download_defaults",
  "lab_im_download_report", "bblab-export_data", "bblab-export_chart",
  "param_audit_pdf_go",
  "ynow_lab_body", "ynow_chk_body", "ynow_pa_body"
)
need_b <- c(
  "pdf", "download_report",
  "alert_slot", "bt_run", "pdf_report"
)
for (id in need_a) {
  check(paste0(id, " is A"), identical(ynow_membership_lock_tier(id), "A"))
}
for (id in need_b) {
  check(paste0(id, " is B"), identical(ynow_membership_lock_tier(id), "B"))
}

check("sidebar guest locks derive from catalog A", {
  identical(
    sort(ynow_sidebar_guest_lock_tabs()),
    sort(ynow_membership_lock_ids(tier = "A", kind = "sidebar_tab", mechanism = "guest_menu_lock"))
  )
})
check("gated downloads derive from catalog", {
  identical(sort(ynow_gated_download_ids()), sort(ynow_membership_lock_ids(kind = "download")))
})
check("every gated download has A or B", {
  ids <- ynow_gated_download_ids()
  tiers <- vapply(ids, ynow_membership_lock_tier, character(1))
  length(ids) > 0L && all(tiers %in% c("A", "B")) &&
    identical(ynow_membership_lock_tier("download_report"), "B")
})

check("points SKUs are exactly membership B skus", {
  defs <- ynow_points_catalog_defaults()
  skus <- sort(as.character(defs$sku))
  identical(skus, sort(ynow_membership_points_skus())) &&
    !"hfv_run" %in% skus &&
    all(vapply(skus, function(s) identical(ynow_membership_lock_tier(s), "B"), logical(1)))
})

check("gate features = membership + ops", {
  feats <- sort(ynow_gate_features())
  identical(
    feats,
    sort(unique(c(ynow_membership_gate_features(), ynow_ops_gate_features())))
  )
})
check("ops gates are non-membership (no A/B tier)", {
  ops <- ynow_ops_gate_features()
  length(ops) >= 3L &&
    all(is.na(vapply(ops, ynow_membership_lock_tier, character(1)))) &&
    all(c("snap_defaults", "inner_lock", "atx") %in% ops)
})

nm <- ynow_non_membership_lock_catalog()
check("non-membership catalog documents ACL + retired hfv", {
  nrow(nm) >= 5L &&
    "access_locks" %in% nm$id &&
    "hfv_run_retired" %in% nm$id &&
    !"unclassified" %in% tolower(nm$scope)
})

# Member gate HTML exposes data-ynow-lock-tier (needs htmltools/shiny tags)
check("member gate has data-ynow-lock-tier=A", {
  if (!requireNamespace("htmltools", quietly = TRUE) &&
      !requireNamespace("shiny", quietly = TRUE)) {
    # Fallback: auth source must wire the attr in member gate builder
    auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    return(grepl("data-ynow-lock-tier", auth, fixed = TRUE) &&
             grepl("ynow_membership_lock_tier_attr", auth, fixed = TRUE))
  }
  if (requireNamespace("shiny", quietly = TRUE) && !exists("tags", mode = "list")) {
    suppressPackageStartupMessages(library(shiny))
  } else if (requireNamespace("htmltools", quietly = TRUE) && !exists("tags", mode = "list")) {
    suppressPackageStartupMessages(library(htmltools))
  }
  gate_html <- paste(as.character(ynow_member_gate_ui(
    "t_gate", "t_title", "t_body", "t_btn", feature = "lab"
  )), collapse = "")
  grepl("data-ynow-lock-tier", gate_html, fixed = TRUE) &&
    grepl("A", gate_html, fixed = TRUE)
})

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("UI injects gated download ids from catalog", {
  grepl("__YNOW_GATED_DOWNLOAD_IDS", ui, fixed = TRUE) &&
    grepl("__YNOW_GATED_DOWNLOAD_TIERS", ui, fixed = TRUE) &&
    grepl("ynow_gated_download_ids", ui, fixed = TRUE)
})
check("UI marks ops ACL templates with lock-scope", {
  grepl('data-ynow-lock-scope` = "ops_acl"', ui, fixed = TRUE) &&
    grepl('data-ynow-lock-scope` = "ops_sysadmin"', ui, fixed = TRUE)
})
check("Lite omit prefs still wired", {
  grepl("ynow_sidebar_lite_omit", ui, fixed = TRUE) ||
    grepl("__YNOW_LITE_OMIT_TABS", ui, fixed = TRUE)
})
check("global sources membership_locks before auth", {
  g <- paste(readLines(file.path(app_dir, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('source("membership_locks.R"', g, fixed = TRUE) &&
    regexpr('source("membership_locks.R"', g, fixed = TRUE) <
      regexpr('source("auth_account.R"', g, fixed = TRUE)
})

# Lite free PDF / omit intact
check("Lite omit still includes guest-locked tabs", {
  omit <- ynow_sidebar_lite_omit_tabs()
  all(c("lab_notes", "decision_checklist", "price_alerts") %in% omit)
})
check("download_report remains gated id (Full B; Lite free elsewhere)", {
  "download_report" %in% ynow_gated_download_ids() &&
    identical(ynow_membership_lock_tier("download_report"), "B")
})

# Shared labels with access_locks (true duplicates only)
check("shared surface labels wired", {
  exists("ynow_lock_shared_surface_labels", mode = "function") &&
    identical(ynow_lock_shared_label("lab_notes", "en"), "Quant Backtest Lab") &&
    identical(ynow_lock_shared_label("decision_checklist", "zh-TW"), "決策檢核") &&
    identical(ynow_lock_shared_label("price_alerts", "en"), "Value Alerts") &&
    identical(ynow_lock_shared_label("checklist_body", "zh-TW"), "決策檢核內容")
})
check("membership sidebar tabs use shared labels", {
  identical(cat$label_en[cat$id == "lab_notes"], "Quant Backtest Lab") &&
    identical(cat$label_zh_tw[cat$id == "decision_checklist"], "決策檢核") &&
    identical(cat$label_en[cat$id == "price_alerts"], "Value Alerts") &&
    identical(cat$label_zh_tw[cat$id == "ynow_chk_body"], "決策檢核內容")
})
check("points SKU titles derive from membership catalog", {
  defs <- ynow_membership_points_sku_defaults()
  identical(defs$title_en[defs$sku == "pdf_report"], "Valuation report (PDF)") &&
    identical(defs$title_zh_tw[defs$sku == "alert_slot"], "到值通知一筆")
})
check("global sources lock_catalog_shared before membership", {
  g <- paste(readLines(file.path(app_dir, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('source("lock_catalog_shared.R"', g, fixed = TRUE) &&
    regexpr('source("lock_catalog_shared.R"', g, fixed = TRUE) <
      regexpr('source("membership_locks.R"', g, fixed = TRUE)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
