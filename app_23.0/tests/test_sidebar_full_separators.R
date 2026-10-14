# Full-mode sidebar white separators between specified menu groups.
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK" else "FAIL", ":", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

root <- normalizePath("..", mustWork = TRUE)
if (!file.exists(file.path(root, "ynow_ui.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "Full-only selector body:not(.ynow-lite)",
  grepl("body:not\\(\\.ynow-lite\\) \\.sidebar-menu > li:has\\(> a\\[data-value=\"macro_market\"\\]\\)", ui, perl = TRUE)
)
for (dv in c("macro_market", "bluechip", "get_started", "lab_notes", "about")) {
  check(
    paste0("separator target ", dv),
    grepl(
      paste0(
        "body:not\\(\\.ynow-lite\\) \\.sidebar-menu > li:has\\(> a\\[data-value=\"",
        dv,
        "\"\\]\\)"
      ),
      ui,
      perl = TRUE
    )
  )
}
check(
  "white border color",
  grepl("border-top:\\s*1px solid rgba\\(255,\\s*255,\\s*255", ui, perl = TRUE)
)
# Ensure Lite does not get a separate copy of these rules under body.ynow-lite
check(
  "no Lite-forced separators",
  !grepl(
    "body\\.ynow-lite \\.sidebar-menu > li:has\\(> a\\[data-value=\"macro_market\"\\]\\)\\s*\\{",
    ui,
    perl = TRUE
  )
)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
