# Full-mode sidebar short white dashed separators between specified menu groups.
# Home-below (macro_market) and About-above separators are intentionally absent.
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
  grepl("body:not\\(\\.ynow-lite\\) \\.sidebar-menu > li:has\\(> a\\[data-value=\"bluechip\"\\]\\)", ui, perl = TRUE)
)
for (dv in c("bluechip", "get_started", "lab_notes")) {
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
  check(
    paste0("short dashed ::before ", dv),
    grepl(
      paste0(
        "body:not\\(\\.ynow-lite\\) \\.sidebar-menu > li:has\\(> a\\[data-value=\"",
        dv,
        "\"\\]\\)::before"
      ),
      ui,
      perl = TRUE
    )
  )
}
for (dv in c("macro_market", "about")) {
  check(
    paste0("no separator target ", dv),
    !grepl(
      paste0(
        "body:not\\(\\.ynow-lite\\) \\.sidebar-menu > li:has\\(> a\\[data-value=\"",
        dv,
        "\"\\]\\)\\s*[,\\{]"
      ),
      ui,
      perl = TRUE
    )
  )
  check(
    paste0("no ::before ", dv),
    !grepl(
      paste0(
        "body:not\\(\\.ynow-lite\\) \\.sidebar-menu > li:has\\(> a\\[data-value=\"",
        dv,
        "\"\\]\\)::before"
      ),
      ui,
      perl = TRUE
    )
  )
}
check(
  "white dashed border on ::before",
  grepl("border-top:\\s*1px dashed #ffffff", ui, perl = TRUE) ||
    grepl("border-top:\\s*1px dashed rgba\\(255,\\s*255,\\s*255", ui, perl = TRUE)
)
check(
  "shortened inset left/right",
  grepl("left:\\s*22%", ui, perl = TRUE) && grepl("right:\\s*22%", ui, perl = TRUE)
)
# Ensure Lite does not get a separate copy of these rules under body.ynow-lite
check(
  "no Lite-forced separators",
  !grepl(
    "body\\.ynow-lite \\.sidebar-menu > li:has\\(> a\\[data-value=\"bluechip\"\\]\\)\\s*\\{",
    ui,
    perl = TRUE
  )
)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
