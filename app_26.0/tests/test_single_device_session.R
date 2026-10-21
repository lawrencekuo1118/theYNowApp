# Single-device concurrent session (one active resume row + live kick).
# Run: cd app_26.0 && Rscript tests/test_single_device_session.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

app_dir <- if (file.exists("auth_db.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../auth_db.R")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot locate auth_db.R")
}
setwd(app_dir)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

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
}

db <- paste(readLines("auth_db.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines("auth_account.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("mint revokes other sessions in transaction", {
  grepl("Single active resume row", db, fixed = TRUE) &&
    grepl("token_hash <> $2", db, fixed = TRUE) &&
    grepl("dbWithTransaction", db, fixed = TRUE) &&
    grepl("SET revoked = TRUE", db, fixed = TRUE)
})

check("still_valid helper exists", {
  grepl("ynow_auth_session_still_valid <- function", db, fixed = TRUE)
})

check("server kick does not revoke held token", {
  grepl(".kick_session_taken_elsewhere", auth, fixed = TRUE) &&
    grepl(".clear_auth_browser(FALSE)", auth, fixed = TRUE) &&
    grepl("ynowAuthSessionTaken", auth, fixed = TRUE) &&
    grepl("auth_msg_session_taken", auth, fixed = TRUE)
})

check("server polls still_valid while signed in", {
  grepl("ynow_auth_session_still_valid", auth, fixed = TRUE) &&
    grepl("invalidateLater(4000L, session)", auth, fixed = TRUE) &&
    grepl("identical(valid, FALSE)", auth, fixed = TRUE)
})

check("peer takeover input wired", {
  grepl("ynow_auth_session_peer_takeover", auth, fixed = TRUE) &&
    grepl("ynow_auth_session_peer_takeover", ui, fixed = TRUE) &&
    grepl("storage", ui, fixed = TRUE) &&
    grepl("ynow_auth_resume_v1", ui, fixed = TRUE)
})

check("client handles ynowAuthSessionTaken", {
  grepl("ynowAuthSessionTaken", ui, fixed = TRUE) &&
    grepl("classList.remove('ynow-authed')", ui, fixed = TRUE)
})

check("locale en-US + zh-TW session_taken", {
  grepl("auth_msg_session_taken", loc, fixed = TRUE) &&
    grepl("another device", loc, fixed = TRUE) &&
    grepl("其他裝置或瀏覽器", loc, fixed = TRUE)
})

source("auth_db.R", local = TRUE, encoding = "UTF-8")
check("still_valid without DB is NA or FALSE", {
  v <- ynow_auth_session_still_valid("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
  isFALSE(v) || is.na(v)
})
check("still_valid empty is FALSE", identical(ynow_auth_session_still_valid(""), FALSE))
check("mint without DB still fails closed", {
  res <- ynow_auth_session_mint("00000000-0000-4000-8000-000000000001")
  isFALSE(res$ok)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
