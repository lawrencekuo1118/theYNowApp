# Once-per-session lazy tab mounts.
# First visit to a sidebar tab builds that page's UI (and optional server);
# revisits reuse the mounted host — no remount / no duplicate moduleServer.

.ynow_lazy_host_id <- function(tab) {
  paste0("ynow_lazy_host_", gsub("[^A-Za-z0-9_]", "_", as.character(tab)[1]))
}

.ynow_lazy_or <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (is.character(x) && !nzchar(x[1]))) y else x
}

#' Register lazy tab hosts.
#'
#' @param input,output,session Shiny session objects
#' @param builders named list: tabName -> zero-arg function returning UI tags
#' @param on_first_mount optional named list: tabName -> zero-arg function
#'   called once before UI is shown (e.g. moduleServer). Must be idempotent
#'   with respect to double registration (called only once here).
#' @param after_mount optional function(tab) called after any new mount
#'   (e.g. re-push locale labels for newly created inputs)
.ynow_register_lazy_tabs <- function(input, output, session,
                                     builders,
                                     on_first_mount = list(),
                                     after_mount = NULL) {
  stopifnot(is.list(builders), length(builders) > 0L)
  cache <- new.env(parent = emptyenv())
  mounted <- new.env(parent = emptyenv())
  flags <- shiny::reactiveValues()
  for (nm in names(builders)) {
    flags[[nm]] <- FALSE
  }

  for (nm in names(builders)) {
    local({
      tab <- nm
      host <- .ynow_lazy_host_id(tab)
      build <- builders[[tab]]
      output[[host]] <- shiny::renderUI({
        shiny::req(isTRUE(flags[[tab]]))
        if (is.null(cache[[tab]])) {
          cache[[tab]] <- tryCatch(
            build(),
            error = function(e) {
              htmltools::tags$div(
                class = "alert alert-warning",
                sprintf("Failed to load page “%s”: %s", tab, conditionMessage(e))
              )
            }
          )
        }
        cache[[tab]]
      })
    })
  }

  shiny::observeEvent(
    input$sidebar_tabs,
    {
      tab <- as.character(.ynow_lazy_or(input$sidebar_tabs, ""))[1]
      if (!nzchar(tab) || !tab %in% names(builders)) return()
      if (isTRUE(mounted[[tab]])) return()
      mounted[[tab]] <- TRUE
      hook <- on_first_mount[[tab]]
      if (is.function(hook)) {
        tryCatch(hook(), error = function(e) {
          warning(sprintf("lazy tab on_first_mount(%s): %s", tab, conditionMessage(e)))
        })
      }
      flags[[tab]] <- TRUE
      if (is.function(after_mount)) {
        session$onFlushed(function() {
          tryCatch(after_mount(tab), error = function(e) NULL)
        }, once = TRUE)
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  invisible(list(
    mounted = function(tab = NULL) {
      if (is.null(tab)) {
        return(vapply(names(builders), function(nm) isTRUE(mounted[[nm]]), logical(1)))
      }
      isTRUE(mounted[[as.character(tab)[1]]])
    }
  ))
}
