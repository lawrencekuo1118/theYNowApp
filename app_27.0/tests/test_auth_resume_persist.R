#!/usr/bin/env Rscript
# Durable browser-resume auth — schema helpers + UI/server contracts.
# Run: cd app_27.0 && Rscript tests/test_auth_resume_persist.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
root <- if (file.exists(file.path(test_dir, "..", "auth_db.R"))) {
  normalizePath(file.path(test_dir, ".."))
} else if (file.exists("auth_db.R")) {
  normalizePath(".")
} else {
  stop("Cannot locate auth_db.R")
}
setwd(root)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "") else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

suppressPackageStartupMessages({
  if (!requireNamespace("digest", quietly = TRUE)) stop("digest required")
})
source("auth_account.R", local = TRUE, encoding = "UTF-8")
source("auth_db.R", local = TRUE, encoding = "UTF-8")

check("resume TTL default 30", identical(ynow_auth_resume_ttl_days(), 30L))
Sys.setenv(YNOW_AUTH_RESUME_DAYS = "7")
check("resume TTL env 7", identical(ynow_auth_resume_ttl_days(), 7L))
Sys.setenv(YNOW_AUTH_RESUME_DAYS = "999")
check("resume TTL capped 365", identical(ynow_auth_resume_ttl_days(), 365L))
Sys.unsetenv("YNOW_AUTH_RESUME_DAYS")

tok_a <- .ynow_auth_random_token(32L)
tok_b <- .ynow_auth_random_token(32L)
check("random token length", nchar(tok_a) >= 64L)
check("random tokens differ", !identical(tok_a, tok_b))
ha <- .ynow_auth_token_hash(tok_a)
hb <- .ynow_auth_token_hash(tok_b)
check("token hash stable", identical(ha, .ynow_auth_token_hash(tok_a)))
check("token hashes differ", nzchar(ha) && nzchar(hb) && !identical(ha, hb))
check("empty token hash empty", identical(.ynow_auth_token_hash(""), ""))

check("mint without DB fails closed", {
  res <- ynow_auth_session_mint("00000000-0000-4000-8000-000000000001")
  isFALSE(res$ok) && identical(res$token, "")
})
check("resume garbage fails closed", {
  res <- ynow_auth_session_resume("not-a-real-token")
  isFALSE(res$ok) && is.null(res$user)
})
check("revoke missing is no-op", isFALSE(ynow_auth_session_revoke("")))

db <- paste(readLines("auth_db.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines("auth_account.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("schema creates ynow_auth_sessions", grepl("CREATE TABLE IF NOT EXISTS ynow_auth_sessions", db, fixed = TRUE))
check("schema RLS fail-closed", grepl('ynow_auth_sessions"', db, fixed = TRUE) &&
        grepl("ynow_auth_sessions_ensure_schema", db, fixed = TRUE))
check("ensure_schema wires sessions", grepl("ynow_auth_sessions_ensure_schema", db, fixed = TRUE))
check("purge deletes auth_sessions", grepl("DELETE FROM ynow_auth_sessions", db, fixed = TRUE))
check("server mints on login", grepl(".persist_auth_browser", auth, fixed = TRUE) &&
        grepl("ynow_auth_session_mint", auth, fixed = TRUE))
check("server resumes from input", grepl("ynow_auth_resume_token", auth, fixed = TRUE) &&
        grepl("ynow_auth_session_resume", auth, fixed = TRUE))
check("sign-out clears browser resume", grepl(".clear_auth_browser", auth, fixed = TRUE) &&
        grepl("ynow_auth_session_revoke", auth, fixed = TRUE))
check("password change revokes resume", grepl("ynow_auth_session_revoke_user", auth, fixed = TRUE))
check("mint single-device revokes other rows", {
  grepl("Single active resume row", db, fixed = TRUE) &&
    grepl("ynow_auth_session_still_valid", db, fixed = TRUE)
})
check("live kick clears without revoke_held", {
  grepl(".kick_session_taken_elsewhere", auth, fixed = TRUE) &&
    grepl(".clear_auth_browser(FALSE)", auth, fixed = TRUE)
})
check("client localStorage key", grepl("ynow_auth_resume_v1", ui, fixed = TRUE))
check("client persist handler", grepl("ynowAuthPersist", ui, fixed = TRUE))
check("client offers resume on connect", grepl("ynow_auth_resume_token", ui, fixed = TRUE) &&
        grepl("ynowAuthResumeOffer", ui, fixed = TRUE))
check("client last UI localStorage key", grepl("ynow_last_ui_v1", ui, fixed = TRUE))
check("resume restores last UI via PostLogin",
      grepl("restore_ticker", auth, fixed = TRUE) &&
        grepl("ynowAuthPostLogin", auth, fixed = TRUE) &&
        grepl(".load_last_ui_surface", auth, fixed = TRUE) &&
        grepl(".persist_last_ui_surface", auth, fixed = TRUE))

if (fail > 0L) {
  cat("FAIL count=", fail, "\n", sep = "")
  quit(status = 1L)
}
cat("ALL PASS\n")
