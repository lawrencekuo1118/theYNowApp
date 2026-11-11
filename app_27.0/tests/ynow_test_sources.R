# Shipped chrome text for tests: ynow_ui.R plus the static assets
# extracted from it (first-paint CSS, app JS, widget CSS).
ynow_test_read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

ynow_test_ui_sources <- function(app_dir) {
  ui <- ynow_test_read_text(file.path(app_dir, "ynow_ui.R"))
  asset_files <- c("ynow_chrome.css", "ynow_app.js", "ynow_widgets.css")
  assets <- vapply(asset_files, function(name) {
    ynow_test_read_text(file.path(app_dir, "www", name))
  }, character(1), USE.NAMES = TRUE)
  list(
    ui = ui,
    assets = assets,
    shipped = paste(c(ui, unname(assets)), collapse = "\n")
  )
}
