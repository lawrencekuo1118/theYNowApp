# Tab-visit auto-calc gate helpers (P1-D / acceptance).
# Pure predicates shared by ynow_server.R observers and offline tests.
# Does NOT compute valuation — only decides whether to fire auto-calc.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

#' Map sidebar tabName → primary auto-calc key (companion models).
#' @return "" when the tab is not a companion auto-calc target
tab_visit_companion_key <- function(tab) {
  tab <- as.character(tab %||% "")[1]
  if (identical(tab, "ddm_calculator")) return("ddm")
  if (identical(tab, "ri_calculator")) return("ri")
  if (tab %in% c("rel_multiples_calculator", "rel_mult_ev", "rel_mult_rev")) {
    return("multiples")
  }
  ""
}

#' DCF tab-visit: fire only when searched, FV missing, and DCF inputs ready.
tab_visit_dcf_should_fire <- function(tab, user_searched, has_fv, ready) {
  identical(as.character(tab %||% "")[1], "dcf_calculator") &&
    isTRUE(user_searched) &&
    !isTRUE(has_fv) &&
    isTRUE(ready)
}

#' Companion tab-visit (DDM / RI / MULT): fire when searched and ready.
#' Note: companion path does not skip on existing FV (matches server design).
tab_visit_companion_should_fire <- function(tab, user_searched, ready) {
  key <- tab_visit_companion_key(tab)
  nzchar(key) && isTRUE(user_searched) && isTRUE(ready)
}

#' Simulate one tab-visit decision cycle (for runtime/unit harnesses).
#' @param fire_fn callback receiving primary key when a fire is warranted
#' @return list(fired=logical, key=character, reason=character)
tab_visit_decide_and_fire <- function(tab, user_searched, has_fv, ready,
                                      fire_fn = NULL) {
  tab <- as.character(tab %||% "")[1]
  if (identical(tab, "dcf_calculator")) {
    if (!isTRUE(user_searched)) {
      return(list(fired = FALSE, key = "dcf", reason = "not_searched"))
    }
    if (isTRUE(has_fv)) {
      return(list(fired = FALSE, key = "dcf", reason = "fv_present"))
    }
    if (!isTRUE(ready)) {
      return(list(fired = FALSE, key = "dcf", reason = "not_ready"))
    }
    if (is.function(fire_fn)) fire_fn("dcf")
    return(list(fired = TRUE, key = "dcf", reason = "fire"))
  }
  key <- tab_visit_companion_key(tab)
  if (!nzchar(key)) {
    return(list(fired = FALSE, key = "", reason = "not_model_tab"))
  }
  if (!isTRUE(user_searched)) {
    return(list(fired = FALSE, key = key, reason = "not_searched"))
  }
  if (!isTRUE(ready)) {
    return(list(fired = FALSE, key = key, reason = "not_ready"))
  }
  if (is.function(fire_fn)) fire_fn(key)
  list(fired = TRUE, key = key, reason = "fire")
}
