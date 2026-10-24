#!/usr/bin/env Rscript
# Count (remaining/total) + time-window mini countdown badges.
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

app_dir <- normalizePath(file.path(".."), mustWork = TRUE)
if (!file.exists(file.path(app_dir, "access_locks.R"))) {
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
source(file.path(app_dir, "access_locks.R"), local = TRUE)

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

tmp <- tempfile(fileext = ".json")
tmp_state <- tempfile(fileext = ".json")
Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp, YNOW_ACCESS_UNLOCK_STATE_PATH = tmp_state)
on.exit({
  unlink(tmp)
  unlink(tmp_state)
}, add = TRUE)
ynow_access_reset_memory()

check("quota badge helper exists", exists("ynow_access_quota_badges", mode = "function"))
check("ends_at iso helper exists", exists("ynow_access_ends_at_iso", mode = "function"))

invisible(ynow_access_set("user-badge", "report:download_snapshot", TRUE,
                effect = "hide", unlock_type = "conditional", free_count = 3L))
b0 <- ynow_access_quota_badges("user-badge", authed = TRUE)
check("free-count badge starts at full remaining", {
  length(b0) >= 1L &&
    identical(b0[[1]]$id, "report:download_snapshot") &&
    identical(as.integer(b0[[1]]$remaining), 3L) &&
    identical(as.integer(b0[[1]]$total), 3L) &&
    is.null(b0[[1]]$ends_at)
})
invisible(ynow_access_touch_conditional("user-badge", "report:download_snapshot", consume_free = TRUE))
b1 <- ynow_access_quota_badges("user-badge", authed = TRUE)
check("free-count badge decrements after use", {
  identical(as.integer(b1[[1]]$remaining), 2L) &&
    identical(as.integer(b1[[1]]$total), 3L)
})

ynow_access_reset_memory()
unlink(tmp); unlink(tmp_state)
Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp, YNOW_ACCESS_UNLOCK_STATE_PATH = tmp_state)
invisible(ynow_access_set(YNOW_ACCESS_ALL_USERS, "output:sgr_pct", TRUE,
                effect = "lock", unlock_type = "conditional", time_limit_hours = 2L))
bt0 <- ynow_access_quota_badges("user-ttl", authed = TRUE)
check("time-only badge has no countdown before window starts", {
  length(bt0) >= 1L &&
    identical(bt0[[1]]$id, "output:sgr_pct") &&
    is.null(bt0[[1]]$remaining) &&
    is.null(bt0[[1]]$ends_at)
})
invisible(ynow_access_touch_conditional("user-ttl", "output:sgr_pct", consume_free = FALSE, now = Sys.time()))
bt1 <- ynow_access_quota_badges("user-ttl", authed = TRUE)
check("time-only badge exposes ends_at while window active", {
  length(bt1) >= 1L &&
    is.character(bt1[[1]]$ends_at) &&
    grepl("T", bt1[[1]]$ends_at, fixed = TRUE) &&
    grepl("Z$", bt1[[1]]$ends_at)
})

check("client has count + countdown badge helpers", {
  grepl("ynowSyncDownloadReportLabel", ui, fixed = TRUE) &&
    grepl("ynowApplyQuotaBadges", ui, fixed = TRUE) &&
    grepl("ynowFmtCountdown", ui, fixed = TRUE) &&
    grepl("ynow-count-badge", ui, fixed = TRUE) &&
    grepl("ynow-ttl-countdown", ui, fixed = TRUE) &&
    grepl("ynowTickQuotaCountdowns", ui, fixed = TRUE)
})
check("access lock push includes badges payload", {
  grepl("ynow_access_quota_badges", srv, fixed = TRUE) &&
    grepl("badges = badges", srv, fixed = TRUE) &&
    grepl("ynow_quota_badge_refresh", srv, fixed = TRUE) &&
    grepl(".ynow_access_note_conditional_use", srv, fixed = TRUE)
})
check("PDF label embeds remaining/total via sync helper", {
  grepl("ynowSyncDownloadReportLabel", ui, fixed = TRUE) &&
    grepl("quota_count_fmt", ui, fixed = TRUE)
})
check("locale keys en + zh-TW for count fmt", {
  identical(ui_str("quota_count_fmt", "en"), "(%s/%s)") &&
    identical(ui_str("quota_count_fmt", "zh-TW"), "(%s/%s)") &&
    grepl("Time left", ui_str("quota_countdown_left", "en"), fixed = TRUE) &&
    grepl("剩餘時間", ui_str("quota_countdown_left", "zh-TW"), fixed = TRUE) &&
    grepl("剩餘免費次數", ui_str("quota_remaining_uses", "zh-TW"), fixed = TRUE) &&
    !grepl("点击|點擊|默认|参数|数据|帶寬|口徑", ui_str("quota_remaining_uses", "zh-TW"))
})
check("locale source registers both sides", {
  length(gregexpr("quota_count_fmt", loc, fixed = TRUE)[[1]]) >= 2L &&
    length(gregexpr("quota_countdown_left", loc, fixed = TRUE)[[1]]) >= 2L
})
check("guest badge shows full free remaining without user state", {
  ynow_access_reset_memory()
  unlink(tmp); unlink(tmp_state)
  Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp, YNOW_ACCESS_UNLOCK_STATE_PATH = tmp_state)
  ynow_access_set(YNOW_ACCESS_ALL_USERS, "report:FS_download", TRUE,
                  effect = "lock", unlock_type = "conditional", free_count = 5L)
  bg <- ynow_access_quota_badges("", authed = FALSE)
  length(bg) >= 1L &&
    identical(as.integer(bg[[1]]$remaining), 5L) &&
    identical(as.integer(bg[[1]]$total), 5L)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
