# Sensitivity matrices → shared Plotly heatmap (display-only; offline).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
options(warn = 1)

suppressPackageStartupMessages({
  library(plotly)
})

root <- normalizePath("..", winslash = "/", mustWork = TRUE)
source(file.path(root, "setup.R"), local = TRUE)
source(file.path(root, "ui_locale.R"), local = TRUE)

fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK " else "FAIL ", label, "\n", sep = "")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

# --- Helper API ---
check("heatmap helper exists", exists("ynow_sensitivity_heatmap", mode = "function"))
check("colorscale helper exists", exists("ynow_sensitivity_heatmap_colorscale", mode = "function"))
check("axis helper exists", exists("ynow_sensitivity_axis_vals", mode = "function"))

z <- matrix(
  c(10, 12, 14, 11, 13, 15, 12, 14, 16),
  nrow = 3, byrow = TRUE,
  dimnames = list(
    c("WACC 12%", "WACC 10%", "WACC 8%"),
    c("g (SGR) 1%", "g (SGR) 2%", "g (SGR) 3%")
  )
)
disc <- c(12, 10, 8)
g <- c(1, 2, 3)
p <- ynow_sensitivity_heatmap(
  z = z,
  disc_vals = disc,
  g_vals = g,
  disc_label = "WACC",
  title = "Sensitivity heatmap",
  center_ij = c(2L, 2L)
)
check("heatmap returns plotly", inherits(p, "plotly"))
check("heatmap is native plot_ly (type heatmap)", {
  # plotly_build keeps heatmap traces without ggplotly
  built <- tryCatch(plotly::plotly_build(p), error = function(e) NULL)
  !is.null(built) &&
    any(vapply(built$x$data, function(tr) identical(tr$type, "heatmap"), logical(1)))
})

p_empty <- ynow_sensitivity_heatmap(NULL, numeric(0), numeric(0))
check("empty input returns plotly", inherits(p_empty, "plotly"))

ax_d <- ynow_sensitivity_axis_vals(z, list(disc = 10, g = 2), "disc")
ax_g <- ynow_sensitivity_axis_vals(z, list(disc = 10, g = 2), "g")
check("axis parse disc from labels", length(ax_d) == 3L && all(is.finite(ax_d)))
check("axis parse g from labels", length(ax_g) == 3L && all(is.finite(ax_g)))

# --- UI wiring: DCF / DDM / RI use shared heatmap ---
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE), collapse = "\n")
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE), collapse = "\n")
ri <- paste(readLines(file.path(root, "ri_module.R"), warn = FALSE), collapse = "\n")
plotly_src <- paste(readLines(file.path(root, "setup_plotly.R"), warn = FALSE), collapse = "\n")

check("DCF UI mounts heatmap", grepl('plotlyOutput\\("dcf_sensitivity_heatmap"', ui))
check("DDM UI mounts heatmap", grepl('plotlyOutput\\("ddm_sensitivity_heatmap"', ui))
check("DCF numeric table remains (details)", grepl("dcf_sensitivity_table", ui, fixed = TRUE))
check("DDM numeric table remains (details)", grepl("ddm_sensitivity_table", ui, fixed = TRUE))
check("server renders DCF heatmap", grepl("dcf_sensitivity_heatmap\\s*<-\\s*plotly::renderPlotly", srv))
check("server renders DDM heatmap", grepl("ddm_sensitivity_heatmap\\s*<-\\s*plotly::renderPlotly", srv))
check("server uses shared helper", grepl("\\.ynow_sens_heatmap_from_built", srv) &&
        grepl("ynow_sensitivity_heatmap", srv, fixed = TRUE))
check("RI heatmap uses shared helper", grepl("ynow_sensitivity_heatmap\\(", ri))
check("RI heatmap before numeric table", {
  i_h <- regexpr("plt_ri_heatmap", ri, fixed = TRUE)[1]
  i_t <- regexpr("tbl_ri_sensitivity", ri, fixed = TRUE)[1]
  i_h > 0 && i_t > 0 && i_h < i_t
})
check("shared helper is native plot_ly heatmap", {
  grepl("ynow_sensitivity_heatmap\\s*<-\\s*function", plotly_src) &&
    grepl('type = "heatmap"', plotly_src, fixed = TRUE) &&
    !grepl("ggplotly\\(", plotly_src)
})

# --- i18n keys present in both locales ---
keys <- c(
  "sens_heatmap_title", "sens_heatmap_title_ri", "sens_heatmap_colorbar",
  "sens_heatmap_x_g", "sens_heatmap_y_wacc", "sens_heatmap_y_ke",
  "sens_heatmap_na", "sens_numeric_table_summary", "dcf_sens_help",
  "sens_brief_title", "sens_brief_vs_price", "ri_sens_heatmap_help"
)
for (k in keys) {
  en <- tryCatch(ui_str(k, "en"), error = function(e) "")
  zh <- tryCatch(ui_str(k, "zh-TW"), error = function(e) "")
  check(paste0("i18n en ", k), nzchar(en) && !identical(en, k))
  check(paste0("i18n zh-TW ", k), nzchar(zh) && !identical(zh, k))
}

# --- Lite stays open: no new lock on sensitivity heatmaps ---
check("DCF heatmap not full-only locked", {
  !grepl('data-ynow-lock[^>]{0,80}dcf_sensitivity_heatmap', ui) &&
    !grepl("ynow-full-only[^\\n]{0,120}dcf_sensitivity_heatmap", ui)
})
check("DDM heatmap not full-only locked", {
  !grepl('data-ynow-lock[^>]{0,80}ddm_sensitivity_heatmap', ui) &&
    !grepl("ynow-full-only[^\\n]{0,120}ddm_sensitivity_heatmap", ui)
})

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("All sensitivity heatmap checks passed.\n")
