# Targeted tests: industry catalog + TW/Yahoo mapping fallbacks
root <- getwd()
app_dir <- if (basename(root) == "tests") {
  dirname(root)
} else if (dir.exists(file.path(root, "app_17.0"))) {
  file.path(root, "app_17.0")
} else if (file.exists(file.path(root, "industry_standards.R"))) {
  root
} else {
  stop("Cannot locate app_17.0")
}
`%||%` <- function(a, b) if (is.null(a)) b else a

source(file.path(app_dir, "industry_standards.R"), local = FALSE, encoding = "UTF-8")
source(file.path(app_dir, "lab_sp500_universe.R"), local = FALSE, encoding = "UTF-8")
source(file.path(app_dir, "lab_tw_universe.R"), local = FALSE, encoding = "UTF-8")

check <- function(label, cond) {
  if (!isTRUE(cond)) stop(paste0("FAIL: ", label), call. = FALSE)
  cat("OK:", label, "\n")
}

check("catalog size >= 55", length(industry_standards) >= 55L)
check("labels match standards", identical(sort(names(industry_labels)), sort(names(industry_standards))))
check("en labels match standards", identical(sort(names(industry_labels_en)), sort(names(industry_standards))))

snap_mets <- c("rev_growth", "roe", "roa", "opex_ratio", "eqt_multiplier")
for (m in snap_mets) {
  n <- sum(vapply(industry_standards, function(x) !is.null(x[[m]]), logical(1)))
  check(paste0("metric ", m, " covered"), n == length(industry_standards))
}

check("unknown key color none", identical(get_box_color("no.such.industry", "roe", 10), "none"))
check("unknown key band dash", identical(annotation_format_band("no.such.industry", "roe"), "—"))
check("empty yahoo resolve", identical(resolve_industry_key_from_yahoo(""), ""))
check("yahoo semiconductors", identical(
  map_yahoo_industry_to_key("Technology", "Semiconductors"),
  "sc.IC_Design"
))
check("yahoo restaurants", identical(
  map_yahoo_industry_to_key("Consumer Cyclical", "Restaurants"),
  "cons.Restaurants"
))

check("TW code 24 semiconductor", identical(lab_map_tw_industry_to_key("24"), "sc.Foundry"))
check("TW code 36 cloud", identical(lab_map_tw_industry_to_key("36"), "saas.SaaS_Cloud"))
check("TW 綠能環保", identical(lab_map_tw_industry_to_key("綠能環保"), "en.Environmental"))
check("TW 紡織纖維", identical(lab_map_tw_industry_to_key("紡織纖維"), "mat.Textiles"))
check("TW unknown unmapped", identical(lab_map_tw_industry_to_key("91"), LAB_UNMAPPED_KEY))

# GICS map targets exist in catalog
smap <- lab_gics_subindustry_map()
missing_keys <- setdiff(unique(unname(smap)), names(industry_standards))
check(paste0("GICS targets in catalog (", length(missing_keys), ")"), length(missing_keys) == 0L)

# ADR／非 S&P overrides (Lab US universe load path)
source(file.path(app_dir, "lab_us_universe.R"), local = FALSE, encoding = "UTF-8")
ov <- lab_ticker_industry_overrides()
check("TSM override Foundry", identical(unname(ov[["TSM"]]), "sc.Foundry"))
check("SKHY override Memory", identical(unname(ov[["SKHY"]]), "sc.Memory"))
check("MU override Memory", identical(unname(ov[["MU"]]), "sc.Memory"))

# Synthetic Unmapped rows remapped on US overlay
synth <- data.frame(
  ticker = c("TSM", "SKHY", "ZZZZ"),
  name = c("TSMC ADR", "SK hynix", "Dummy"),
  exchange = c("NYSE", "NASDAQ", "NYSE"),
  industry_raw = NA_character_,
  industry_key = c(LAB_UNMAPPED_KEY, LAB_UNMAPPED_KEY, LAB_UNMAPPED_KEY),
  fetched_at = "2026-01-01T00:00:00Z",
  source = "test",
  stringsAsFactors = FALSE
)
mapped <- lab_us_overlay_ticker_industry_overrides(synth)
check("TSM overlay → Foundry", identical(mapped$industry_key[mapped$ticker == "TSM"], "sc.Foundry"))
check("SKHY overlay → Memory", identical(mapped$industry_key[mapped$ticker == "SKHY"], "sc.Memory"))
check("unknown stays Unmapped", identical(mapped$industry_key[mapped$ticker == "ZZZZ"], LAB_UNMAPPED_KEY))

# Remap cache keys if CSV present
csv <- file.path(app_dir, "data", "tw_universe.csv")
if (file.exists(csv)) {
  u <- lab_read_tw_cache()
  n_um <- sum(u$industry_key == LAB_UNMAPPED_KEY, na.rm = TRUE)
  check("TW cache remap reduces unmapped", n_um < 400L)
  cat("TW unmapped after remap:", n_um, "/", nrow(u), "\n")
}

# Live US cache: TSM/SKHY must not remain Unmapped after load overlay
if (file.exists(file.path(app_dir, "data", "us_universe.csv"))) {
  us <- lab_get_us_universe(FALSE)
  tsm_key <- us$industry_key[match("TSM", toupper(us$ticker))]
  skhy_key <- us$industry_key[match("SKHY", toupper(us$ticker))]
  check("US cache TSM → Foundry", identical(as.character(tsm_key), "sc.Foundry"))
  check("US cache SKHY → Memory", identical(as.character(skhy_key), "sc.Memory"))
}

cat("ALL PASS\n")
