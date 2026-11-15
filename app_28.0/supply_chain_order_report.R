if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.YNOW_SUPPLY_BUCKETS <- c("orders", "suppliers", "customers")
.YNOW_SUPPLY_ITEM_KINDS <- c("mention", "constraint")
.YNOW_SUPPLY_STANCES <- c("silent", "mentioned", "constrained", "mixed")
.YNOW_SUPPLY_FILING_STATES <- c("absent", "table", "threshold")
.YNOW_SUPPLY_CITE_CAP <- 5L
.YNOW_SUPPLY_SCHEMA <- 1L

.YNOW_SUPPLY_FORBIDDEN_FIELDS <- c(
  "backlog", "bookings", "capex", "value", "amount", "share", "pct", "percent",
  "top1_share", "top3_share", "revenue", "quantity"
)

.ynow_supply_chr <- function(x, default = "") {
  v <- as.character(x %||% default)[1]
  if (!nzchar(v) || is.na(v)) default else v
}

.ynow_supply_lexicon_version <- function() {
  v <- .ynow_supply_chr(ynow_ec_keyword_lexicon()$version, "3")
  if (!nzchar(v)) "3" else v
}

.ynow_supply_notes <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.list(x)) {
    x <- x[vapply(x, is.character, logical(1))]
    if (!length(x)) return(character(0))
  }
  if (!is.character(x)) return(character(0))
  v <- trimws(as.character(unlist(x, use.names = FALSE)))
  v <- v[nzchar(v) & !is.na(v)]
  if (!length(v)) return(character(0))
  v <- v[!vapply(v, .ynow_ec_kw_order_focus_reject, logical(1))]
  v <- unique(v)
  utils::head(v, .YNOW_SUPPLY_CITE_CAP)
}

.ynow_supply_filing_state <- function(x) {
  v <- .ynow_supply_chr(x, "absent")
  if (v %in% .YNOW_SUPPLY_FILING_STATES) v else "absent"
}

.ynow_supply_filing_enum <- function(pack) {
  if (!is.list(pack)) return("absent")
  rows <- pack$rows %||% pack$slices %||% list()
  if (isTRUE(pack$eligible) && length(rows) > 0L) return("table")
  method <- .ynow_supply_chr(pack$method, "")
  if (identical(method, "threshold_none_above")) return("threshold")
  "absent"
}

.ynow_supply_stance <- function(cites, role) {
  cites <- as.character(cites %||% character(0))
  cites <- cites[nzchar(cites) & !is.na(cites)]
  out <- if (!length(cites)) {
    "silent"
  } else {
    cons <- ynow_ec_lexicon_tokens(role, kind = "constraint")
    cons <- cons[nzchar(cons)]
    if (!length(cons)) {
      "mentioned"
    } else {
      matched <- vapply(cites, function(s) {
        any(vapply(cons, function(p) {
          grepl(p, s, perl = TRUE, ignore.case = TRUE)
        }, logical(1)))
      }, logical(1))
      if (!any(matched)) "mentioned" else if (all(matched)) "constrained" else "mixed"
    }
  }
  if (out %in% .YNOW_SUPPLY_STANCES) out else "silent"
}

.ynow_supply_msg <- function(key, locale) {
  if (exists("ui_str", mode = "function")) {
    return(tryCatch(ui_str(key, locale), error = function(e) key))
  }
  key
}

#' @param focus list(order_notes, supplier_notes, customer_notes)
#' @param source fetch result after ynow_ec_stamp_info_updated
#' @return sealed supply_report
ynow_supply_report_from_focus <- function(focus, source = list()) {
  focus <- if (is.list(focus)) focus else list()
  source <- if (is.list(source)) source else list()
  ynow_supply_report_carry(list(
    lexicon_version = .ynow_supply_lexicon_version(),
    source_id = .ynow_supply_chr(source$source_id),
    published = .ynow_supply_chr(source$published),
    info_updated_at = .ynow_supply_chr(source$info_updated_at),
    info_updated_kind = .ynow_supply_chr(source$info_updated_kind),
    buckets = list(
      orders = focus$order_notes,
      suppliers = focus$supplier_notes,
      customers = focus$customer_notes
    ),
    filing = list(customers = "absent", suppliers = "absent")
  ))
}

#' Missing input becomes a sealed empty report, not NULL.
#' @param report list or NULL
#' @return sealed supply_report
ynow_supply_report_carry <- function(report) {
  src <- if (is.list(report)) report else list()
  drop <- intersect(names(src), .YNOW_SUPPLY_FORBIDDEN_FIELDS)
  if (length(drop)) src[drop] <- NULL
  buckets_in <- if (is.list(src$buckets)) src$buckets else list()
  buckets <- stats::setNames(
    lapply(.YNOW_SUPPLY_BUCKETS, function(nm) .ynow_supply_notes(buckets_in[[nm]])),
    .YNOW_SUPPLY_BUCKETS
  )
  filing_in <- if (is.list(src$filing)) src$filing else list()
  ver <- .ynow_supply_chr(src$lexicon_version)
  if (!nzchar(ver)) ver <- .ynow_supply_lexicon_version()
  list(
    schema = .YNOW_SUPPLY_SCHEMA,
    lexicon_version = ver,
    source_id = .ynow_supply_chr(src$source_id),
    published = .ynow_supply_chr(src$published),
    info_updated_at = .ynow_supply_chr(src$info_updated_at),
    info_updated_kind = .ynow_supply_chr(src$info_updated_kind),
    buckets = buckets,
    filing = list(
      customers = .ynow_supply_filing_state(filing_in$customers),
      suppliers = .ynow_supply_filing_state(filing_in$suppliers)
    )
  )
}

#' Idempotent. Replaces filing flags and does not touch cites.
#' @param report sealed report or NULL
#' @param order_concentration bblab order-concentration list
#' @return sealed supply_report
ynow_supply_report_with_filing <- function(report, order_concentration = NULL) {
  sealed <- ynow_supply_report_carry(report)
  oc <- if (is.list(order_concentration)) order_concentration else list()
  sealed$filing$customers <- .ynow_supply_filing_enum(oc$customer_concentration)
  sealed$filing$suppliers <- .ynow_supply_filing_enum(oc$supplier_concentration)
  sealed
}

#' TRUE when any bucket has cites or any filing state is not absent.
ynow_supply_report_has_content <- function(report) {
  sealed <- ynow_supply_report_carry(report)
  cites <- any(vapply(.YNOW_SUPPLY_BUCKETS, function(nm) {
    length(sealed$buckets[[nm]]) > 0L
  }, logical(1)))
  filed <- any(vapply(c("customers", "suppliers"), function(nm) {
    !identical(sealed$filing[[nm]], "absent")
  }, logical(1)))
  isTRUE(cites) || isTRUE(filed)
}

.ynow_supply_updated_row <- function(sealed, locale) {
  updated <- .ynow_supply_chr(sealed$info_updated_at)
  kind <- .ynow_supply_chr(sealed$info_updated_kind)
  kind_lab <- if (identical(kind, "fetched_at")) {
    .ynow_supply_msg("bblab_or_ec_kw_updated_kind_fetched", locale)
  } else if (nzchar(kind)) {
    .ynow_supply_msg("bblab_or_ec_kw_updated_kind_event", locale)
  } else {
    ""
  }
  if (!nzchar(updated)) {
    return(shiny::tags$p(
      class = "help-block",
      shiny::tags$b(paste0(.ynow_supply_msg("bblab_or_ec_kw_updated", locale), ": ")),
      .ynow_supply_msg("bblab_or_ec_kw_updated_unknown", locale)
    ))
  }
  shiny::tags$p(
    class = "help-block",
    shiny::tags$b(paste0(.ynow_supply_msg("bblab_or_ec_kw_updated", locale), ": ")),
    updated,
    if (nzchar(kind_lab)) shiny::tags$span(paste0(" (", kind_lab, ")")) else NULL
  )
}

#' Returns NULL when has_content is FALSE so the chrome keeps its empty line.
#' @param report sealed report
#' @param locale "en" or "zh-TW"
ynow_supply_report_ui <- function(report, locale = "en") {
  sealed <- ynow_supply_report_carry(report)
  if (!isTRUE(ynow_supply_report_has_content(sealed))) return(NULL)
  buckets <- lapply(.YNOW_SUPPLY_BUCKETS, function(role) {
    cites <- sealed$buckets[[role]]
    stance <- .ynow_supply_stance(cites, role)
    if (identical(stance, "silent")) return(NULL)
    shiny::tags$div(
      class = "ynow-bblab-or__supply-bucket",
      `data-bucket` = role,
      `data-stance` = stance,
      shiny::tags$p(shiny::tags$b(
        .ynow_supply_msg(paste0("bblab_or_ec_kw_", role), locale)
      )),
      shiny::tags$p(.ynow_supply_msg(
        paste0("bblab_or_supply_report_", role, "_", stance),
        locale
      )),
      shiny::tags$ul(lapply(cites, shiny::tags$li))
    )
  })
  filing_bits <- lapply(c("customers", "suppliers"), function(side) {
    state <- sealed$filing[[side]]
    if (identical(state, "absent")) return(NULL)
    shiny::tags$p(
      class = "help-block",
      `data-filing` = side,
      `data-filing-state` = state,
      .ynow_supply_msg(
        paste0("bblab_or_supply_report_filing_", side, "_", state),
        locale
      )
    )
  })
  shiny::tags$div(
    class = "ynow-bblab-or__supply-report",
    shiny::tags$p(
      class = "help-block",
      .ynow_supply_msg("bblab_or_supply_report_status_only", locale)
    ),
    .ynow_supply_updated_row(sealed, locale),
    buckets,
    filing_bits
  )
}
