#!/usr/bin/env Rscript
# Header US/TW market buttons: equal-size flags, flush stack after toggle.
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK" else "FAIL", ":", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

root <- normalizePath("..", mustWork = TRUE)
if (!file.exists(file.path(root, "ynow_ui.R"))) root <- normalizePath(getwd(), mustWork = TRUE)
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("US/TW buttons present",
      grepl("ynow_mkt_btn_us", ui, fixed = TRUE) &&
        grepl("ynow_mkt_btn_tw", ui, fixed = TRUE))
check("equal-size SVG flags (not emoji text)",
      grepl('class="ynow-mkt-flag"', ui, fixed = TRUE) &&
        grepl('width="18" height="12"', ui, fixed = TRUE) &&
        grepl("viewBox=\"0 0 19 10\"", ui, fixed = TRUE) &&
        grepl("viewBox=\"0 0 18 12\"", ui, fixed = TRUE) &&
        !grepl("\\\\U0001F1FA\\\\U0001F1F8", ui, fixed = TRUE))
check("CSS locks flag box 18x12",
      grepl("width: 18px !important", ui, fixed = TRUE) &&
        grepl("height: 12px !important", ui, fixed = TRUE))
check("stack fills 50px header",
      grepl("#ynow-market-header .ynow-market-stack", ui, fixed = TRUE) &&
        grepl("height: 50px", ui, fixed = TRUE) &&
        grepl("height: 25px", ui, fixed = TRUE))
check("absolute flush after toggle (JS left)",
      grepl("placeMarketHeaderByToggle", ui, fixed = TRUE) &&
        grepl("position: absolute !important", ui, fixed = TRUE) &&
        grepl("market.style.left", ui, fixed = TRUE))
check("locale updates aria/title only (keeps flags)",
      grepl("Market buttons show equal-size flag", ui, fixed = TRUE) &&
        grepl("mktUs.setAttribute('aria-label', s.market_us)", ui, fixed = TRUE) &&
        !grepl("mktUs.textContent = s.market_us", ui, fixed = TRUE) &&
        !grepl("mktTw.textContent = s.market_tw", ui, fixed = TRUE))
check("mobile bare flags (no frame)", {
  grepl("Bare flags only", ui, fixed = TRUE) &&
    grepl("Mobile: flags sit in the flex row immediately after the hamburger", ui, fixed = TRUE) &&
    grepl("position: static !important", ui, fixed = TRUE) &&
    grepl("innerWidth <= 767", ui, fixed = TRUE) &&
    grepl("appearance: none !important", ui, fixed = TRUE)
})

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
