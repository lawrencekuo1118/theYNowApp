#!/usr/bin/env Rscript
# Latest-selected tab wins: a slow on_first_mount must not serialize the next mount.
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
options(warn = 1)

suppressPackageStartupMessages({
  if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny required")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("htmltools required")
  if (!requireNamespace("later", quietly = TRUE)) stop("later required")
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
macro <- paste(readLines("macro_market_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
lazy <- paste(readLines("lazy_tabs.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "lazy_tabs defers on_first_mount via later",
  grepl(".ynow_lazy_defer", lazy, fixed = TRUE) &&
    grepl("flags[[tab]] <- TRUE", lazy, fixed = TRUE) &&
    grepl("delay = 0.05", lazy, fixed = TRUE)
)

check(
  "ticker search yields with later phases + gen check",
  grepl(".run_search_phase1", srv, fixed = TRUE) &&
    grepl(".run_search_phase1b", srv, fixed = TRUE) &&
    grepl(".run_search_phase2", srv, fixed = TRUE) &&
    grepl(".search_alive", srv, fixed = TRUE) &&
    grepl("later::later", srv, fixed = TRUE)
)

check(
  "same-button re-click queues latest-wins pending restart",
  grepl("lab_im_eval_pending", srv, fixed = TRUE) &&
    grepl("lab_im_ind_mcap_pending", srv, fixed = TRUE) &&
    grepl("lab_cluster_pending", srv, fixed = TRUE) &&
    grepl(".lab_im_finish_eval_slot", srv, fixed = TRUE) &&
    grepl("lab_cluster_pending(job_args)", srv, fixed = TRUE)
)

check(
  "Macro cancels quote/HTCDI ticks when tab inactive",
  grepl("tab_active_rv", macro, fixed = TRUE) &&
    grepl(".tab_active", macro, fixed = TRUE) &&
    grepl(
      "tab_active_rv = reactive\\([\\s\\S]{0,120}?macro_market",
      srv,
      perl = TRUE
    )
)

hook_a_started <- NA_real_
hook_a_finished <- NA_real_
hook_b_started <- NA_real_
t_a_flush_ms <- NA_real_
t_b_mount_ms <- NA_real_
mount_b_at <- NA_real_

session <- shiny::MockShinySession$new()
shiny::withReactiveDomain(session, {
  input <- session$input
  output <- session$output
  ctrl <- .ynow_register_lazy_tabs(
    input, output, session,
    builders = list(
      page_a = function() htmltools::tags$div("A"),
      page_b = function() htmltools::tags$div("B")
    ),
    on_first_mount = list(
      page_a = function() {
        hook_a_started <<- as.numeric(Sys.time())
        Sys.sleep(0.35)
        hook_a_finished <<- as.numeric(Sys.time())
      },
      page_b = function() {
        hook_b_started <<- as.numeric(Sys.time())
      }
    )
  )

  t0 <- as.numeric(Sys.time())
  session$setInputs(sidebar_tabs = "page_a")
  session$flushReact()
  t_a_flush_ms <<- 1000 * (as.numeric(Sys.time()) - t0)
  check("page_a mounted after first flush", isTRUE(ctrl$mounted("page_a")))
  check(
    "page_a flush returns without waiting for 350ms hook",
    is.finite(t_a_flush_ms) && t_a_flush_ms < 200
  )

  session$setInputs(sidebar_tabs = "page_b")
  session$flushReact()
  mount_b_at <<- as.numeric(Sys.time())
  t_b_mount_ms <<- 1000 * (mount_b_at - t0)
  check("page_b mounted while page_a hook still pending", isTRUE(ctrl$mounted("page_b")))
  check(
    "page_b mount completes well under page_a hook duration",
    is.finite(t_b_mount_ms) && t_b_mount_ms < 200
  )

  deadline <- Sys.time() + 2
  while ((is.na(hook_a_finished) || is.na(hook_b_started)) && Sys.time() < deadline) {
    later::run_now(timeoutSecs = 0.05)
  }
})

check("page_a hook completed after drain", !is.na(hook_a_finished))
check("page_b hook started after drain", !is.na(hook_b_started))
check(
  "page_b mount timestamp precedes page_a hook completion",
  is.finite(mount_b_at) &&
    is.finite(hook_a_finished) &&
    mount_b_at < hook_a_finished
)

cat(sprintf(
  "TIMING_MS a_flush=%.1f b_mount=%.1f a_hook=%.0f\n",
  t_a_flush_ms,
  t_b_mount_ms,
  1000 * (hook_a_finished - hook_a_started)
))

if (fail > 0L) {
  cat(sprintf("FAIL test_parallel_tab_switch_loads (%d failed / %d passed)\n", fail, pass))
  quit(status = 1)
}
cat(sprintf("PASS test_parallel_tab_switch_loads (%d checks)\n", pass))
