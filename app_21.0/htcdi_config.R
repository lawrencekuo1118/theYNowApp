# HTCDI observation-tool config. YAML sibling is the documented copy.
# Runtime source of truth is HTCDI_DEFAULT_CONFIG (no yaml package).

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.HTCDI_ISSUER <- function(id, tickers, economic_issuer, function_id, layer,
                          criticality_prior, quote_currency, statement_currency,
                          instrument_type = "ordinary",
                          underlying_share_comparison_required = FALSE,
                          substitutes = "", replacement_time_years = 3,
                          switching_difficulty = 70, buffer = 20,
                          issuer_cap_override = NA_real_,
                          local_ordinary_ticker = NA_character_,
                          adr_ratio = NA_real_) {
  list(
    id = id, tickers = tickers, economic_issuer = economic_issuer,
    function_id = function_id, layer = layer,
    criticality_prior = as.numeric(criticality_prior),
    quote_currency = quote_currency, statement_currency = statement_currency,
    instrument_type = instrument_type,
    underlying_share_comparison_required = isTRUE(underlying_share_comparison_required),
    substitutes = substitutes,
    replacement_time_years = as.numeric(replacement_time_years),
    switching_difficulty = as.numeric(switching_difficulty),
    buffer = as.numeric(buffer),
    issuer_cap_override = suppressWarnings(as.numeric(issuer_cap_override)[1]),
    local_ordinary_ticker = local_ordinary_ticker,
    adr_ratio = suppressWarnings(as.numeric(adr_ratio)[1])
  )
}

HTCDI_DEFAULT_CONFIG <- list(
  meta = list(
    id = "HTCDI",
    name = "Human Tech Civilization Destruction Index",
    role = "civilization_destruction_reading",
    not = c("buy_signal", "literal_extinction_forecast", "ordinary_market_cap_index"),
    governance = list(
      rebalance = "quarterly", constituent_review = "annual",
      extraordinary_review = c("merger", "delist", "bankruptcy", "structural_split", "confirmed_critical_infra_disruption"),
      add_names_on = c("dependency", "replacement_difficulty"),
      do_not_add_on = c("size", "popularity")
    )
  ),
  criticality_factor_weights = c(
    cross_industry_dependency = 0.20, substitute_scarcity = 0.20,
    replacement_time = 0.15, switching_difficulty = 0.10,
    installed_base_network = 0.10, dependency_of_other_critical_sectors = 0.10,
    failure_speed = 0.10, inverse_buffer = 0.05
  ),
  weighting = list(
    method = "criticality", layer_cap = 0.25, issuer_cap = 0.12,
    issuer_cap_exceptions = c(ASML = 0.15, TSM = 0.15, OIL = 0.15),
    liquidity_adj_max = 0.20, normalize_to = 1.0
  ),
  composite = c(
    statement_development = 0.30, market_vs_benchmark = 0.25,
    influence_vs_market = 0.25, trajectory_vs_history = 0.20
  ),
  statement_weights = c(rev_yoy = 0.45, gm_delta = 0.25, capex_vs_own = 0.30),
  market_weights = c(excess = 0.60, absolute = 0.40),
  influence_weights = c(beta = 0.55, excess = 0.45),
  trajectory_weights = c(price_vs_hist = 0.55, rev_yoy_vs_own = 0.45),
  score_scales = list(rev_yoy = 0.20, gm_delta = 0.10, capex_vs_own = 0.05,
                      excess = 0.30, absolute = 0.30, price_vs_hist = 0.50,
                      rev_yoy_vs_own = 0.10),
  alerts = list(expanding_min = 70, steady_min = 55, cooling_min = 40,
                cooling_score = 45, persist_days = 20),
  layers = list(
    energy_oil = "OIL", lithography = "ASML", wafer_process_equipment = "ASM",
    foundries = "TSM", eda = c("SNPS", "CDNS"),
    chip_ip = "ARM", semiconductors = c("TSM", "AVGO", "NVDA"),
    cloud = c("MSFT", "AMZN", "GOOGL"),
    enterprise_identity = "MSFT", consumer_os_ecosystems = c("AAPL", "MSFT"),
    payment_networks = c("V", "MA"), aviation_comms = c("ARINC", "SITA"),
    enterprise_dbs = "ORCL", dc_networking = "AVGO", ai_computing = "NVDA"
  ),
  contagion_channels = list(
    list(id = "oil_litho", issuers = c("OIL", "ASML"), persist_days = 20, min_stressed = 2L),
    list(id = "asml_asm", issuers = c("ASML", "ASM"), persist_days = 20, min_stressed = 2L),
    list(id = "litho_foundry", issuers = c("ASML", "TSM"), persist_days = 20, min_stressed = 2L),
    list(id = "foundry_eda", issuers = c("TSM", "SNPS", "CDNS"), persist_days = 20, min_stressed = 2L),
    list(id = "arm_ai", issuers = c("ARM", "NVDA"), persist_days = 20, min_stressed = 2L),
    list(id = "cloud_identity", issuers = c("MSFT", "AMZN", "GOOGL"), persist_days = 20, min_stressed = 2L),
    list(id = "payments", issuers = c("V", "MA"), persist_days = 20, min_stressed = 2L),
    list(id = "arinc_sita", issuers = c("ARINC", "SITA"), persist_days = 20, min_stressed = 2L),
    list(id = "semi_cloud_pay", layers = c("semiconductors", "cloud", "payment_networks"),
         persist_days = 20, min_stressed = 3L)
  ),
  issuers = list(
    .HTCDI_ISSUER("OIL", "USO", "oil_index", "energy_oil", "energy_oil", 93, "USD", "USD",
                  instrument_type = "etf",
                  substitutes = "other crude benchmarks (WTI / Brent futures; limited)",
                  replacement_time_years = 0, switching_difficulty = 95, buffer = 5, issuer_cap_override = 0.15),
    .HTCDI_ISSUER("ASML", "ASML", "asml", "semiconductor_equipment", "lithography", 95, "USD", "EUR",
                  substitutes = "limited trailing-edge lithography (Nikon / Canon)",
                  replacement_time_years = 8, switching_difficulty = 92, buffer = 8, issuer_cap_override = 0.15),
    .HTCDI_ISSUER("ASM", "ASMIY", "asm_international", "wafer_process_equipment", "wafer_process_equipment", 90, "USD", "EUR",
                  instrument_type = "ADR",
                  substitutes = "partial ALD / epitaxial peers (Tokyo Electron / Lam; limited for leading-edge ALD)",
                  replacement_time_years = 6, switching_difficulty = 88, buffer = 12),
    .HTCDI_ISSUER("TSM", "TSM", "tsmc", "advanced_foundry", "foundries", 92, "USD", "TWD",
                  instrument_type = "ADR", local_ordinary_ticker = "2330.TW", adr_ratio = 5,
                  substitutes = "limited advanced-node capacity",
                  replacement_time_years = 6, switching_difficulty = 90, buffer = 10, issuer_cap_override = 0.15),
    .HTCDI_ISSUER("MSFT", "MSFT", "microsoft", "enterprise_os_identity_cloud", "enterprise_identity",
                  88, "USD", "USD", substitutes = "partial (Google Workspace / Linux / AWS IAM)",
                  replacement_time_years = 4, switching_difficulty = 85, buffer = 18),
    .HTCDI_ISSUER("ARINC", "RTX", "arinc", "aviation_arinc", "aviation_comms", 87, "USD", "USD",
                  substitutes = "partial airline ACARS / other ground–air messaging (limited)",
                  replacement_time_years = 6, switching_difficulty = 90, buffer = 10),
    .HTCDI_ISSUER("SITA", "AMADY", "sita", "aviation_sita", "aviation_comms", 86, "USD", "EUR",
                  instrument_type = "ADR",
                  substitutes = "partial airline/airport IT peers (SITA itself is private; Amadeus ADR is the listed market proxy)",
                  replacement_time_years = 5, switching_difficulty = 88, buffer = 12),
    .HTCDI_ISSUER("ARM", "ARM", "arm", "chip_ip", "chip_ip", 84, "USD", "USD",
                  substitutes = "partial RISC-V / x86 design IP (long software ecosystem lock-in)",
                  replacement_time_years = 5, switching_difficulty = 86, buffer = 14),
    .HTCDI_ISSUER("AMZN", "AMZN", "amazon", "cloud_internet_infrastructure", "cloud",
                  83, "USD", "USD", substitutes = "Azure / GCP (partial)",
                  replacement_time_years = 3, switching_difficulty = 80, buffer = 20),
    .HTCDI_ISSUER("GOOGL", c("GOOGL", "GOOG"), "alphabet", "cloud_internet_infrastructure", "cloud",
                  80, "USD", "USD", substitutes = "Bing / Azure / AWS (partial)",
                  replacement_time_years = 3, switching_difficulty = 78, buffer = 22),
    .HTCDI_ISSUER("AAPL", "AAPL", "apple", "consumer_digital_ecosystem", "consumer_os_ecosystems",
                  79, "USD", "USD", substitutes = "Android / Windows (partial)",
                  replacement_time_years = 3, switching_difficulty = 82, buffer = 25),
    .HTCDI_ISSUER("SNPS", "SNPS", "synopsys", "eda", "eda", 78, "USD", "USD",
                  substitutes = "CDNS (partial; long recertification)",
                  replacement_time_years = 5, switching_difficulty = 88, buffer = 12),
    .HTCDI_ISSUER("CDNS", "CDNS", "cadence", "eda", "eda", 76, "USD", "USD",
                  substitutes = "SNPS (partial; long recertification)",
                  replacement_time_years = 5, switching_difficulty = 86, buffer = 12),
    .HTCDI_ISSUER("V", "V", "visa", "payment_networks", "payment_networks", 74, "USD", "USD",
                  substitutes = "MA / domestic rails (partial)",
                  replacement_time_years = 4, switching_difficulty = 84, buffer = 16),
    .HTCDI_ISSUER("MA", "MA", "mastercard", "payment_networks", "payment_networks", 72, "USD", "USD",
                  substitutes = "V / domestic rails (partial)",
                  replacement_time_years = 4, switching_difficulty = 84, buffer = 16),
    .HTCDI_ISSUER("ORCL", "ORCL", "oracle", "enterprise_data_virtualization", "enterprise_dbs",
                  68, "USD", "USD", substitutes = "SQL Server / cloud DBs (partial)",
                  replacement_time_years = 4, switching_difficulty = 80, buffer = 20),
    .HTCDI_ISSUER("AVGO", "AVGO", "broadcom", "enterprise_data_virtualization", "dc_networking",
                  68, "USD", "USD", substitutes = "limited DC switching / VMware stack",
                  replacement_time_years = 3, switching_difficulty = 76, buffer = 18),
    .HTCDI_ISSUER("NVDA", "NVDA", "nvidia", "ai_accelerated_computing", "ai_computing",
                  65, "USD", "USD", substitutes = "AMD / custom ASICs (partial; software lock-in)",
                  replacement_time_years = 3, switching_difficulty = 74, buffer = 15)
  )
)

htcdi_config_yaml_path <- function() {
  candidates <- c("htcdi_config.yaml", file.path("app_21.0", "htcdi_config.yaml"))
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) return(normalizePath(hit[[1]], mustWork = FALSE))
  NA_character_
}

htcdi_deep_merge <- function(base, override) {
  if (is.null(override)) return(base)
  if (!is.list(base) || !is.list(override) || is.null(names(override))) return(override)
  out <- base
  for (nm in names(override)) {
    if (nm %in% names(out) && is.list(out[[nm]]) && is.list(override[[nm]]) &&
        !is.null(names(out[[nm]])) && !is.null(names(override[[nm]]))) {
      out[[nm]] <- htcdi_deep_merge(out[[nm]], override[[nm]])
    } else {
      out[[nm]] <- override[[nm]]
    }
  }
  out
}

htcdi_load_config <- function(override = NULL, rds_path = NULL) {
  cfg <- HTCDI_DEFAULT_CONFIG
  if (!is.null(rds_path) && nzchar(as.character(rds_path)[1]) && file.exists(rds_path)) {
    loaded <- tryCatch(readRDS(rds_path), error = function(e) NULL)
    if (is.list(loaded)) cfg <- htcdi_deep_merge(cfg, loaded)
  }
  if (is.list(override)) cfg <- htcdi_deep_merge(cfg, override)
  cfg
}

htcdi_issuers <- function(cfg = NULL) {
  cfg <- if (is.null(cfg)) htcdi_load_config() else cfg
  cfg$issuers
}

htcdi_issuer_ids <- function(cfg = NULL) {
  vapply(htcdi_issuers(cfg), function(x) as.character(x$id)[1], character(1))
}

htcdi_find_issuer <- function(id_or_ticker, cfg = NULL) {
  issuers <- htcdi_issuers(cfg)
  key <- toupper(trimws(as.character(id_or_ticker %||% "")[1]))
  if (!nzchar(key)) return(NULL)
  for (iss in issuers) {
    ids <- toupper(c(iss$id, iss$tickers, iss$local_ordinary_ticker, iss$economic_issuer))
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (key %in% ids) return(iss)
  }
  NULL
}

htcdi_economic_issuer_id <- function(id_or_ticker, cfg = NULL) {
  iss <- htcdi_find_issuer(id_or_ticker, cfg)
  if (is.null(iss)) return(NA_character_)
  as.character(iss$economic_issuer %||% iss$id)[1]
}

htcdi_dedupe_tickers <- function(tickers, cfg = NULL) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[!is.na(tks) & nzchar(tks)]
  if (!length(tks)) return(list(tickers = character(0), issuers = character(0), code = NULL))
  seen <- character(0); keep <- character(0); issuer_ids <- character(0); dup <- FALSE
  for (tk in tks) {
    eid <- htcdi_economic_issuer_id(tk, cfg)
    if (is.na(eid) || !nzchar(eid)) {
      if (!(tk %in% keep)) { keep <- c(keep, tk); issuer_ids <- c(issuer_ids, tk) }
      next
    }
    if (eid %in% seen) { dup <- TRUE; next }
    seen <- c(seen, eid)
    iss <- htcdi_find_issuer(tk, cfg)
    keep <- c(keep, as.character(iss$tickers[[1]] %||% tk))
    issuer_ids <- c(issuer_ids, as.character(iss$id)[1])
  }
  list(tickers = keep, issuers = issuer_ids, code = if (isTRUE(dup)) "DUPLICATE_ECONOMIC_ISSUER" else NULL)
}
