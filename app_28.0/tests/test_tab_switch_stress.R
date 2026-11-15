#!/usr/bin/env Rscript
# Tab / mode switch stress + static guards for load efficiency (no network).
# Methodology: rapid MockShinySession sidebar cycling + source-level contracts
# for tab gates, server-side DT, and lazy-mount locale push budget.
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
options(warn = 1)

suppressPackageStartupMessages({
  if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny required")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("htmltools required")
})

root <- if (file.exists("lazy_tabs.R")) {
  "."
} else if (file.exists("../lazy_tabs.R")) {
  ".."
} else {
  stop("Cannot locate app root")
}
setwd(root)

source("lazy_tabs.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK " else "FAIL ", label, "\n", sep = "")
  if (ok) pass <<- pass + 1L else fail <<- fail + 1L
  invisible(ok)
}

srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
dc <- paste(readLines("decision_checklist_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# --- Static contracts (optimization targets) ---
check(
  "lazy after_mount is dual push (not triple)",
  grepl("delay = 0\\.12", srv) &&
    !grepl("later::later\\(push_loc, delay = 0\\.05\\)", srv) &&
    !grepl("delay = 0\\.25", srv)
)

check(
  "statement DT gated on dashboard + Income Statement",
  grepl(
    "tbIncomeStatement[\\s\\S]{0,220}?req\\(identical\\(\\.ynow_sidebar_tab\\(\\), \"dashboard\"\\)\\)",
    srv,
    perl = TRUE
  ) &&
    grepl(
      "tbIncomeStatement[\\s\\S]{0,350}?dashboard_fin_report[\\s\\S]{0,80}?Income Statement",
      srv,
      perl = TRUE
    )
)

check(
  "IS/BS/CF plots gated on dashboard sub-tab",
  grepl("is_plot <- renderPlotly[\\s\\S]{0,200}?dashboard", srv, perl = TRUE) &&
    grepl("bs_plot <- renderPlotly[\\s\\S]{0,200}?Balance Sheet", srv, perl = TRUE) &&
    grepl("cf_plot <- renderPlotly[\\s\\S]{0,200}?Cash Flow", srv, perl = TRUE)
)

check(
  "bs_equity_charts_ui stable dual hosts (no layout swap)",
  grepl(
    "bs_equity_charts_ui <- renderUI\\(\\{[\\s\\S]{0,280}?plotlyOutput\\(\"bs_plot\"[\\s\\S]{0,120}?plotlyOutput\\(\"eq_structure_plot\"",
    srv,
    perl = TRUE
  ) &&
    !grepl(
      "bs_equity_charts_ui <- renderUI\\(\\{[\\s\\S]{0,400}?has_eq",
      srv,
      perl = TRUE
    )
)

check(
  "snapshot table uses DT::renderDT server=TRUE + deferRender",
  grepl("snapshot_table <- DT::renderDT", srv, fixed = TRUE) &&
    grepl(
      "snapshot_table <- DT::renderDT\\(\\{[\\s\\S]{0,400}?server = TRUE\\)",
      srv,
      perl = TRUE
    ) &&
    grepl(
      "snapshot_table <- DT::renderDT\\(\\{[\\s\\S]{0,350}?deferRender = TRUE",
      srv,
      perl = TRUE
    ) &&
    grepl(
      "snapshot_table <- DT::renderDT\\(\\{[\\s\\S]{0,120}?req\\(identical\\(\\.ynow_sidebar_tab\\(\\), \"snapshot\"\\)\\)",
      srv,
      perl = TRUE
    )
)

.ynow_first_dt_server_flag <- function(src, output_id) {
  pos <- regexpr(paste0(output_id, " <- DT::renderDT"), src, fixed = TRUE)
  if (pos < 1L) return(NA_character_)
  chunk <- substr(src, pos, min(nchar(src), pos + 12000L))
  m <- regexpr("server\\s*=\\s*(TRUE|FALSE)", chunk, perl = TRUE)
  if (m < 1L) return(NA_character_)
  sub("^server\\s*=\\s*", "", regmatches(chunk, m))
}
check(
  "Blue Chip Lab DT tables use server=TRUE",
  identical(.ynow_first_dt_server_flag(srv, "lab_im_summary"), "TRUE") &&
    identical(.ynow_first_dt_server_flag(srv, "lab_im_ind_mcap_table"), "TRUE") &&
    identical(.ynow_first_dt_server_flag(srv, "lab_im_leaderboard"), "TRUE") &&
    identical(.ynow_first_dt_server_flag(srv, "lab_im_table"), "TRUE") &&
    identical(.ynow_first_dt_server_flag(srv, "lab_cluster_table"), "TRUE")
)

check(
  "lab catalog defers until bluechip mounted",
  grepl("lab_im_catalog <- reactive\\(\\{[\\s\\S]{0,200}?\\.ynow_bluechip_mounted\\(\\)", srv, perl = TRUE)
)

check(
  "market_mode ind-mcap reload skips when bluechip never opened",
  grepl(
    "observeEvent\\(market_mode\\(\\),[\\s\\S]{0,180}?\\.ynow_bluechip_mounted\\(\\)",
    srv,
    perl = TRUE
  )
)

check(
  "checklist locale observers gate on decision_checklist tab",
  grepl(
    "if \\(!identical\\(as\\.character\\(input\\$sidebar_tabs[^\\n]*\\)\\[1\\], \"decision_checklist\"\\)\\) return\\(\\)",
    dc,
    perl = TRUE
  )
)

# --- Stress: rapid lazy-tab mount cycling ---
tabs <- c(
  "home", "bluechip", "hfv", "lab_notes", "macro_market",
  "asset_transmission", "decision_checklist", "dashboard",
  "bluechip", "hfv", "lab_notes", "bluechip", "home"
)
build_counts <- new.env(parent = emptyenv())
for (nm in c(
  "macro_market", "bluechip", "hfv", "lab_notes",
  "testing", "asset_transmission", "decision_checklist"
)) {
  build_counts[[nm]] <- 0L
}

t0 <- proc.time()[["elapsed"]]
session <- shiny::MockShinySession$new()
shiny::withReactiveDomain(session, {
  input <- session$input
  output <- session$output
  builders <- list(
    macro_market = function() {
      build_counts$macro_market <- build_counts$macro_market + 1L
      htmltools::tags$div("macro")
    },
    bluechip = function() {
      build_counts$bluechip <- build_counts$bluechip + 1L
      htmltools::tags$div("bluechip")
    },
    hfv = function() {
      build_counts$hfv <- build_counts$hfv + 1L
      htmltools::tags$div("hfv")
    },
    lab_notes = function() {
      build_counts$lab_notes <- build_counts$lab_notes + 1L
      htmltools::tags$div("lab")
    },
    testing = function() {
      build_counts$testing <- build_counts$testing + 1L
      htmltools::tags$div("testing")
    },
    asset_transmission = function() {
      build_counts$asset_transmission <- build_counts$asset_transmission + 1L
      htmltools::tags$div("atx")
    },
    decision_checklist = function() {
      build_counts$decision_checklist <- build_counts$decision_checklist + 1L
      htmltools::tags$div("dc")
    }
  )
  ctrl <- .ynow_register_lazy_tabs(input, output, session, builders = builders)

  # Rapid concurrent-style cycling (many switches, reuse mounts)
  cycle <- rep(tabs, times = 8L)
  for (tab in cycle) {
    session$setInputs(sidebar_tabs = tab)
    session$flushReact()
  }
})
elapsed_ms <- round(1000 * (proc.time()[["elapsed"]] - t0), 1)

check("stress: bluechip builder once", identical(as.integer(build_counts$bluechip), 1L))
check("stress: hfv builder once", identical(as.integer(build_counts$hfv), 1L))
check("stress: lab_notes builder once", identical(as.integer(build_counts$lab_notes), 1L))
check("stress: macro_market builder once", identical(as.integer(build_counts$macro_market), 1L))
check(
  "stress: decision_checklist builder once",
  identical(as.integer(build_counts$decision_checklist), 1L)
)
check(
  "stress: asset_transmission builder once",
  identical(as.integer(build_counts$asset_transmission), 1L)
)
check("stress: testing never mounted in cycle", identical(as.integer(build_counts$testing), 0L))
check(
  "stress: rapid cycle finishes under 5s",
  is.finite(elapsed_ms) && elapsed_ms < 5000
)

cat(
  sprintf(
    "\nSTRESS_TIMING_MS=%s CYCLES=%d UNIQUE_LAZY_MOUNTS=%d\n",
    elapsed_ms,
    length(rep(tabs, times = 8L)),
    sum(vapply(
      c("macro_market", "bluechip", "hfv", "lab_notes", "asset_transmission", "decision_checklist"),
      function(nm) as.integer(build_counts[[nm]]),
      integer(1)
    ))
  )
)

cat("\nPASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) {
  quit(status = 1L)
}
cat("ALL PASS\n")
