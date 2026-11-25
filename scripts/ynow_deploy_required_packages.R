# Canonical R packages that MUST be installed in the deploy VM before
# rsconnect::deployApp(). packrat/renv snapshots only installed packages; in
# non-interactive CI, missing packages are omitted from the shinyapps manifest
# (see Tip-ship #3 / bundle 12867372 exit status 1).
#
# Keep boot Listen-path policy: pagedown is installed for snapshot completeness
# (PDF via pagedown:: in setup_report.R) but must NOT be pacman::p_load'd /
# library()'d at boot — see app_28.0/tests/test_pagedown_boot_guard.R.
#
# Sourced by scripts/deploy_app_21.R and scripts/install_deploy_app_packages.R.

ynow_deploy_required_packages <- function() {
  c(
    # Deploy tooling
    "rsconnect", "curl", "renv", "packrat",
    # Boot stack — mirrors app_28.0/global.R pacman::p_load (+ common :: use)
    "pacman",
    "tidyverse", "dplyr", "stringr", "purrr", "magrittr", "glue", "jsonlite",
    "ggplot2", "ggrepel", "plotly", "DT", "rmarkdown", "knitr", "scales",
    "rvest", "xml2", "reticulate",
    "shiny", "shinydashboard", "shinyjs", "shinycustomloader",
    "shinyWidgets", "shinyBS", "shinycssloaders",
    "memoise", "cachem", "TTR", "httr2", "digest",
    "DBI", "RPostgres", "pool", "sodium", "openssl",
    "future", "promises", "later", "callr",
    # Referenced via :: / crawlers / tests; must be snapshotted
    "httr", "quantmod", "zoo", "htmltools", "htmlwidgets", "testthat",
    # PDF — install for manifest; never attach on Listen path
    "pagedown"
  )
}

#' Boot-critical subset: absence almost always yields shinyapps exit status 1.
ynow_deploy_boot_critical_packages <- function() {
  c(
    "shiny", "shinydashboard", "shinyjs", "shinyWidgets", "shinyBS",
    "shinycustomloader", "shinycssloaders",
    "pacman", "dplyr", "jsonlite", "htmltools",
    "reticulate", "DBI", "RPostgres", "pool", "sodium", "future"
  )
}

ynow_deploy_missing_packages <- function(pkgs = ynow_deploy_required_packages()) {
  pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}
