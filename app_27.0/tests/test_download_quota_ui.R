#!/usr/bin/env Rscript
# Download-report quota UI: remaining free count + points rules chrome.
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

app_dir <- normalizePath(file.path(".."), mustWork = TRUE)
if (!file.exists(file.path(app_dir, "points_wallet.R"))) {
  app_dir <- normalizePath("app_27.0", mustWork = TRUE)
}

ok <- 0L
bad <- 0L
check <- function(label, expr) {
  passed <- isTRUE(tryCatch(expr, error = function(e) {
    message("ERROR [", label, "]: ", conditionMessage(e))
    FALSE
  }))
  if (passed) {
    ok <<- ok + 1L
    message("PASS ", label)
  } else {
    bad <<- bad + 1L
    message("FAIL ", label)
  }
}

source(file.path(app_dir, "ui_locale.R"), local = TRUE)
source(file.path(app_dir, "points_wallet.R"), local = TRUE)

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
wallet <- paste(readLines(file.path(app_dir, "points_wallet.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("quota status helper exists", exists("ynow_pdf_quota_status", mode = "function"))
check("status helper wired in wallet source", grepl("ynow_pdf_quota_status", wallet, fixed = TRUE))

ynow_pdf_reset_memory()
.ynow_points_mem_reset()
st0 <- ynow_pdf_quota_status("quota-ui-user", day = "2026-10-21")
check("guest-empty status starts at full free quota", {
  isTRUE(st0$ok) && identical(st0$used, 0L) &&
    identical(st0$left, ynow_pdf_free_per_day()) &&
    identical(st0$limit, ynow_pdf_free_per_day()) &&
    identical(st0$cost, ynow_points_sku_cost("pdf_report"))
})
check("blank user reports not logged in", {
  g <- ynow_pdf_quota_status("", day = "2026-10-21")
  isTRUE(g$ok) && isTRUE(!isTRUE(g$logged_in)) && identical(g$left, ynow_pdf_free_per_day())
})

invisible(ynow_points_credit("quota-ui-user", 50L, reason = "earn_signup", ref_id = "quota-seed", wallet = "fyn"))
invisible(ynow_pdf_claim("quota-ui-user", day = "2026-10-21"))
invisible(ynow_pdf_claim("quota-ui-user", day = "2026-10-21"))
st2 <- ynow_pdf_quota_status("quota-ui-user", day = "2026-10-21")
check("status reflects two free claims without spending extra", {
  identical(st2$used, 2L) && identical(st2$left, ynow_pdf_free_per_day() - 2L)
})
invisible(ynow_pdf_claim("quota-ui-user", day = "2026-10-21"))
st3 <- ynow_pdf_quota_status("quota-ui-user", day = "2026-10-21")
check("status shows zero left after free quota exhausted", {
  identical(st3$used, ynow_pdf_free_per_day()) && identical(st3$left, 0L)
})
check("status is read-only (no fourth free slot invented)", {
  st3b <- ynow_pdf_quota_status("quota-ui-user", day = "2026-10-21")
  identical(st3b$left, 0L) && identical(st3b$used, st3$used)
})

check("sidebar quota chrome has remaining + rules nodes", {
  grepl('id = "ynow_pdf_quota_remaining"', ui, fixed = TRUE) &&
    grepl('id = "ynow_pdf_quota_rules"', ui, fixed = TRUE) &&
    grepl("ynow-pdf-quota-remaining", ui, fixed = TRUE) &&
    grepl("ynow-pdf-quota-rules", ui, fixed = TRUE)
})
check("snapshot download quota chrome present", {
  grepl('id = "ynow_snapshot_download_quota"', ui, fixed = TRUE) &&
    grepl("ynow-snapshot-download-quota", ui, fixed = TRUE)
})
check("client handler ynowPdfQuota registered", {
  grepl("ynowPdfQuota", ui, fixed = TRUE) &&
    grepl("ynowApplyPdfQuotaCopy", ui, fixed = TRUE) &&
    grepl("__ynowPdfQuotaState", ui, fixed = TRUE)
})
check("server pushes quota near banner observe", {
  grepl(".ynow_pdf_quota_push", srv, fixed = TRUE) &&
    grepl("ynowPdfQuota", srv, fixed = TRUE) &&
    grepl("ynow_pdf_quota_status", srv, fixed = TRUE)
})
check("server refreshes quota after PDF download", {
  grepl("tryCatch\\(\\.ynow_pdf_quota_push\\(\\)", srv)
})

check("locale keys en remaining + rules", {
  grepl("Free PDF downloads left today", ui_str("pdf_quota_remaining_fmt", "en"), fixed = TRUE) &&
    grepl("FYN", ui_str("pdf_quota_rules_fmt", "en"), fixed = TRUE) &&
    grepl("Sign-in required", ui_str("pdf_quota_guest_remaining_fmt", "en"), fixed = TRUE) &&
    grepl("no FYN", ui_str("snapshot_download_quota_hint", "en"), fixed = TRUE) &&
    grepl("stay free", ui_str("snapshot_download_quota_hint_lite", "en"), fixed = TRUE)
})
check("locale keys zh-TW Taiwan wording", {
  grepl("剩餘免費", ui_str("pdf_quota_remaining_fmt", "zh-TW"), fixed = TRUE) &&
    grepl("點數規則", ui_str("pdf_quota_rules_fmt", "zh-TW"), fixed = TRUE) &&
    grepl("需登入", ui_str("pdf_quota_guest_remaining_fmt", "zh-TW"), fixed = TRUE) &&
    !grepl("一鍵", ui_str("pdf_quota_rules_fmt", "zh-TW"), fixed = TRUE) &&
    !grepl("点击|點擊|默认|参数|数据", ui_str("pdf_quota_remaining_fmt", "zh-TW")) &&
    grepl("不扣 FYN", ui_str("snapshot_download_quota_hint_authed", "zh-TW"), fixed = TRUE) &&
    grepl("維持免費", ui_str("snapshot_download_quota_hint_lite", "zh-TW"), fixed = TRUE)
})
check("legacy pdf_quota_hint still bilingual", {
  grepl("10 FYN", ui_str("pdf_quota_hint", "en"), fixed = TRUE) &&
    grepl("10 FYN", ui_str("pdf_quota_hint", "zh-TW"), fixed = TRUE)
})
check("locale source registers new keys both sides", {
  grepl("pdf_quota_remaining_fmt", loc, fixed = TRUE) &&
    grepl("pdf_quota_rules_fmt", loc, fixed = TRUE) &&
    grepl("snapshot_download_quota_hint_lite", loc, fixed = TRUE) &&
    length(gregexpr("pdf_quota_remaining_fmt", loc, fixed = TRUE)[[1]]) >= 2L
})
check("Lite PDF remains ungated in authorize", {
  grepl('kind = "lite"', srv, fixed = TRUE) &&
    grepl("pdf_quota_hint_lite", srv, fixed = TRUE)
})
check("membership download_report stays tier B", {
  source(file.path(app_dir, "membership_locks.R"), local = TRUE)
  identical(ynow_membership_lock_tier("download_report"), "B") &&
    identical(ynow_membership_lock_tier("download_snapshot"), "A")
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
