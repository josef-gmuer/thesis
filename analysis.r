# =============================================================================
# Analysis code for:
#   "Removal of EU Trade Protection on Chinese Solar Panels:
#    Firm Exit, Market Structure and Deployment"
#   Josef Gmür, MA Thesis, University of St. Gallen
#
# Builds the data and produces every table, figure and in-text number in
# the thesis. Run from this folder:  source("analysis.R")
# See README.md for data sources, requirements and runtime.
# =============================================================================


# =============================================================================
# 0. SETUP
# =============================================================================
library(tidyverse); library(countrycode); library(fixest)
library(httr2); library(jsonlite); library(eurostat); library(pxweb); library(readxl)

raw_dir <- "raw"; proc_dir <- "processed"; out_dir <- "output"
for (d in c(raw_dir, proc_dir, out_dir)) dir.create(d, showWarnings = FALSE)

START <- 1999L; END <- 2024L      # country-level study window
EXIT_START <- 2012L               # ORBIS records no exits before 2012
PRE <- 2010:2012                  # exposure years
COUNTRIES <- c("AUT","BEL","LUX","DNK","FIN","FRA","DEU","GRC",
               "IRL","ITA","NLD","PRT","ESP","SWE","GBR")
HS_CODES <- c("854140","854150","854142","854143")   # HS2017 + HS2022 solar codes
PRODCOM_CODES <- c("26112240","26114070")
B_BOOT <- 999; N_PERM <- 1000; SEED <- 20260920

# Downloads a source once and caches it in raw/; later runs read the cache.
cached <- function(file, fetch) {
  f <- file.path(raw_dir, file)
  if (!file.exists(f)) { message("Downloading ", file); write_csv(fetch(), f) }
  read_csv(f, show_col_types = FALSE)
}
eurostat_comext <- function(dataset, query, key = "*") {
  request("https://ec.europa.eu/eurostat/api/comext/dissemination") %>%
    req_url_path_append("sdmx", "3.0", "data", "dataflow", "ESTAT", dataset, "1.0", key) %>%
    req_url_query(!!!query, compress = "false", format = "csvdata", formatVersion = "1.0") %>%
    req_error(is_error = function(r) FALSE) %>% req_retry(max_tries = 3) %>%
    req_timeout(120) %>% req_perform() %>%
    { if (resp_status(.) == 200) read_csv(I(resp_body_string(.)), show_col_types = FALSE) else tibble() }
}
show <- function(x, name) {           # print a table and save it to output/
  cat("\n", strrep("=", 70), "\n", name, "\n", strrep("=", 70), "\n", sep = "")
  print(x, n = 50, width = Inf)
  write_csv(x, file.path(out_dir, paste0(gsub("[^A-Za-z0-9]+", "_", name), ".csv")))
}


# =============================================================================
# 1. DATA DOWNLOADS (skipped where the file already exists in raw/)
# =============================================================================
usd_eur <- cached("usd_eur_raw.csv", function()
  get_eurostat("ert_bil_eur_a", filters = list(currency = "USD", statinfo = "AVG"),
               time_format = "num") %>% transmute(year = as.integer(time), usd_per_eur = values))

# --- Prodcom: production, total imports and exports (EUR -> USD) ----------
prodcom_raw <- cached("prodcom_solar_raw.csv", function()
  eurostat_comext("ds-059358", list(
    `c[freq]` = "A", `c[reporter]` = paste(countrycode(COUNTRIES, "iso3c", "iso2c"), collapse = ","),
    `c[product]` = paste(PRODCOM_CODES, collapse = ","),
    `c[indicators]` = "PRODVAL,PRODQNT,IMPVAL,EXPVAL",
    `c[TIME_PERIOD]` = paste(START:END, collapse = ",")), key = "*.*.*.*") %>%
    left_join(usd_eur, by = c("TIME_PERIOD" = "year")) %>%
    mutate(OBS_VALUE_USD = if_else(indicators %in% c("PRODVAL","IMPVAL","EXPVAL"),
                                   OBS_VALUE * usd_per_eur, NA_real_)))

eu_prodval <- cached("eu_prodval_raw.csv", function()
  eurostat_comext("ds-059358", list(
    `c[freq]` = "A", `c[reporter]` = "EU27_2020", `c[product]` = paste(PRODCOM_CODES, collapse = ","),
    `c[indicators]` = "PRODVAL", `c[TIME_PERIOD]` = paste(START:END, collapse = ",")), key = "*.*.*.*"))

# --- UN Comtrade: Chinese exports to each sample country -------------------
# Requires a free API key in the environment variable COMTRADE_API_KEY.
comtrade_get <- function(reporter, partner, year, flow) {
  r <- request("https://comtradeapi.un.org") %>%
    req_url_path_append("data", "v1", "get", "C", "A", "HS") %>%
    req_url_query(reporterCode = reporter, period = year, partnerCode = partner,
                  cmdCode = paste(HS_CODES, collapse = ","), flowCode = flow,
                  breakdownMode = "classic", includeDesc = "true") %>%
    req_headers("Ocp-Apim-Subscription-Key" = Sys.getenv("COMTRADE_API_KEY")) %>%
    req_retry(max_tries = 3, backoff = ~5) %>% req_perform() %>% resp_body_json()
  Sys.sleep(1.5)
  bind_rows(lapply(r$data, function(x) as_tibble(lapply(x, function(v) if (is.null(v)) NA else v))))
}
comtrade_codes <- function() {
  rep <- fromJSON("https://comtradeapi.un.org/files/v1/app/reference/Reporters.json")$results
  par <- fromJSON("https://comtradeapi.un.org/files/v1/app/reference/partnerAreas.json")$results
  list(chn = rep$reporterCode[rep$reporterCodeIsoAlpha3 == "CHN"],
       gbr = rep$reporterCode[rep$reporterCodeIsoAlpha3 == "GBR"],
       partners = paste(par$PartnerCode[par$PartnerCodeIsoAlpha3 %in% COUNTRIES], collapse = ","))
}
comtrade_raw <- cached("comtrade_solar_raw.csv", function() {
  cc <- comtrade_codes()
  map_dfr(START:END, ~ comtrade_get(cc$chn, cc$partners, .x, "X"))
})

# --- Comext CN8: PV-only trade, world and extra-EU (alternative exposure) --
comext_raw <- cached("comext_cn8_raw.csv", function() {
  map_dfr(COUNTRIES, function(iso3) map_dfr(START:END, function(yr) {
    codes <- if (yr <= 2021) "85414090" else "85414200,85414300"
    q <- function(flow) eurostat_comext("ds-045409", list(
      `c[freq]` = "A", `c[reporter]` = countrycode(iso3, "iso3c", "iso2c"), `c[product]` = codes,
      `c[flow]` = flow, `c[indicators]` = "VALUE_IN_EUROS", `c[TIME_PERIOD]` = as.character(yr)))
    imp <- q("1"); ex <- q("2"); Sys.sleep(0.7)
    v <- function(df, p) if (nrow(df) == 0) 0 else sum(df$OBS_VALUE[df$partner == p], na.rm = TRUE)
    rate <- usd_eur$usd_per_eur[usd_eur$year == yr]
    tibble(iso3, year = yr, china_imp_usd = v(imp, "CN") * rate,
           imp_world_usd = v(imp, "WORLD") * rate, exp_world_usd = v(ex, "WORLD") * rate,
           imp_extraeu_usd = v(imp, "EXT_EU27_2020") * rate, exp_extraeu_usd = v(ex, "EXT_EU27_2020") * rate)
  }))
})

# LED vs PV split of Chinese imports under HS 854140, 2010-2012 (Section 3.1)
led_raw <- cached("led_share_raw.csv", function()
  map_dfr(PRE, function(yr) eurostat_comext("ds-045409", list(
    `c[freq]` = "A", `c[reporter]` = paste(countrycode(COUNTRIES, "iso3c", "iso2c"), collapse = ","),
    `c[product]` = "85414010,85414090", `c[flow]` = "1", `c[indicators]` = "VALUE_IN_EUROS",
    `c[TIME_PERIOD]` = as.character(yr))) %>% filter(partner == "CN")))

# --- Controls (Eurostat; UK gaps filled from raw/uk_manual_supplement.csv) --
gdp_raw  <- cached("eurostat_gdp_raw.csv",  function() get_eurostat("nama_10_pc",  time_format = "num"))
elec_raw <- cached("eurostat_electricity_raw.csv", function() get_eurostat("nrg_pc_204", time_format = "num"))
ren_raw  <- cached("eurostat_renewable_share_raw.csv", function() get_eurostat("nrg_ind_ren", time_format = "num"))
pop_raw  <- cached("population_raw.csv", function()
  get_eurostat("demo_pjan", filters = list(sex = "T", age = "TOTAL"), time_format = "num"))
uk_supp  <- read_csv(file.path(raw_dir, "uk_manual_supplement.csv"), show_col_types = FALSE)

# --- IRENA: cumulative installed solar PV capacity -------------------------
irena_raw <- cached("irena_capacity_raw.csv", function() {
  q <- list(query = list(
    list(code = "Country/area", selection = list(filter = "item", values = COUNTRIES)),
    list(code = "Technology", selection = list(filter = "item", values = I("2"))),       # solar PV
    list(code = "Grid connection", selection = list(filter = "item", values = I("0"))),  # on-grid
    list(code = "Year", selection = list(filter = "item", values = as.character(0:25)))),
    response = list(format = "json"))
  url <- paste0("https://pxweb.irena.org/api/v1/en/IRENASTAT/",
                URLencode("Power Capacity and Generation"), "/", URLencode("Country_ELECCAP_2026_H1_v-PX 1.px"))
  as.data.frame(pxweb_get(url, query = toJSON(q, auto_unbox = TRUE)),
                column.name.type = "text", variable.value.type = "code")
})

# --- ORBIS (licensed; not included -- see README) --------------------------
orbis_raw <- read_excel(file.path(raw_dir, "orbis.xlsx"), sheet = "Results")


# =============================================================================
# 2. BUILD ANALYSIS PANELS
# =============================================================================

# --- 2.1 Exposure: import penetration, Comtrade + Prodcom (Section 3.1) ----
china_exports <- comtrade_raw %>%
  group_by(iso3 = partnerISO, year = as.integer(refYear)) %>%
  summarise(china_exports_value = sum(as.numeric(primaryValue), na.rm = TRUE), .groups = "drop")

prodcom_panel <- prodcom_raw %>%
  filter(indicators %in% c("PRODVAL","IMPVAL","EXPVAL")) %>%
  mutate(iso3 = countrycode(reporter, "iso2c", "iso3c")) %>%
  filter(iso3 %in% COUNTRIES) %>%
  group_by(iso3, year = TIME_PERIOD, indicators) %>%
  summarise(v = sum(OBS_VALUE_USD, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicators, values_from = v) %>%
  rename(prod = PRODVAL, imp = IMPVAL, exp = EXPVAL)

# Suppressed production: country's average share of EU production (2015-2018,
# or all unsuppressed years if none in that window) x EU aggregate
eu_total <- eu_prodval %>% left_join(usd_eur, by = c("TIME_PERIOD" = "year")) %>%
  group_by(year = TIME_PERIOD) %>% summarise(eu_prod = sum(OBS_VALUE * usd_per_eur, na.rm = TRUE), .groups = "drop")
shares <- prodcom_panel %>% filter(!is.na(prod), prod > 0) %>% group_by(iso3) %>%
  summarise(avg = if (any(year %in% 2015:2018)) mean(prod[year %in% 2015:2018]) else mean(prod), .groups = "drop") %>%
  mutate(share = avg / sum(avg))
prodcom_panel <- prodcom_panel %>%
  full_join(expand_grid(iso3 = shares$iso3, year = START:END) %>%
              left_join(shares, by = "iso3") %>% left_join(eu_total, by = "year") %>%
              transmute(iso3, year, imputed = share * eu_prod), by = c("iso3", "year")) %>%
  mutate(is_imputed = is.na(prod) & !is.na(imputed), prod = coalesce(prod, imputed)) %>%
  select(-imputed)

# UK: Prodcom trade stops after 2019 -> total imports/exports from Comtrade
uk_trade <- cached("uk_trade_raw.csv", function() {
  cc <- comtrade_codes()
  yrs <- prodcom_panel %>% filter(iso3 == "GBR", is.na(imp) | is.na(exp)) %>% pull(year)
  map_dfr(yrs, ~ comtrade_get(cc$gbr, "0", .x, "M,X") %>% mutate(year = .x))
}) %>%
  group_by(year, flowDesc) %>% summarise(v = sum(as.numeric(primaryValue), na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = flowDesc, values_from = v) %>% transmute(iso3 = "GBR", year, uk_imp = Import, uk_exp = Export)
prodcom_panel <- prodcom_panel %>% left_join(uk_trade, by = c("iso3", "year")) %>%
  mutate(imp = coalesce(imp, uk_imp), exp = coalesce(exp, uk_exp)) %>% select(-uk_imp, -uk_exp)

exposure_panel <- china_exports %>%
  left_join(prodcom_panel, by = c("iso3", "year")) %>%
  mutate(import_penetration = 100 * china_exports_value / (prod + imp - exp))
lims <- quantile(exposure_panel$import_penetration, c(0.015, 0.985), na.rm = TRUE)
exposure_panel <- exposure_panel %>%
  mutate(import_penetration_winsorized = pmin(pmax(import_penetration, lims[1]), lims[2]))
exposure <- exposure_panel %>% filter(year %in% PRE) %>% group_by(iso3) %>%
  summarise(exposure = mean(import_penetration_winsorized, na.rm = TRUE), .groups = "drop")

# --- 2.2 Alternative exposure from Comext (world / extra-EU) --------------
winsor <- function(x) { q <- quantile(x, c(0.015, 0.985), na.rm = TRUE); pmin(pmax(x, q[1]), q[2]) }
prod_only <- prodcom_raw %>% filter(indicators == "PRODVAL") %>%
  group_by(iso3 = countrycode(reporter, "iso2c", "iso3c"), year = TIME_PERIOD) %>%
  summarise(prod = sum(OBS_VALUE_USD, na.rm = TRUE), .groups = "drop")
exposure_comext <- comext_raw %>% left_join(prod_only, by = c("iso3", "year")) %>%
  mutate(prod = coalesce(prod, 0),
         w  = winsor(100 * china_imp_usd / (prod + imp_world_usd - exp_world_usd)),
         xe = winsor(100 * china_imp_usd / (prod + imp_extraeu_usd - exp_extraeu_usd))) %>%
  filter(year %in% PRE) %>% group_by(iso3) %>%
  summarise(exposure_world = mean(w, na.rm = TRUE), exposure_extraeu = mean(xe, na.rm = TRUE), .groups = "drop")

# --- 2.3 Country-year panel: controls + capacity (Sections 3.3-3.4) ------
eu_geo <- function(df) df %>% mutate(iso3 = suppressWarnings(countrycode(geo, "eurostat", "iso3c"))) %>% filter(iso3 %in% COUNTRIES)
gdp <- gdp_raw %>% filter(na_item == "B1GQ", unit == "CLV10_EUR_HAB") %>% eu_geo() %>%
  transmute(iso3, year = TIME_PERIOD, gdp_pc = values)
gdp <- bind_rows(gdp, uk_supp %>% filter(!is.na(gdp_pc)) %>% select(iso3, year, gdp_pc) %>% anti_join(gdp, by = c("iso3", "year")))
ren <- ren_raw %>% filter(nrg_bal == "REN_ELC", unit == "PC") %>% eu_geo() %>%
  transmute(iso3, year = TIME_PERIOD, renewable_share_pct = values)
ren <- bind_rows(ren, uk_supp %>% filter(!is.na(renewable_share_pct)) %>% select(iso3, year, renewable_share_pct) %>% anti_join(ren, by = c("iso3", "year")))
elec <- elec_raw %>% filter(siec == "E7000", nrg_cons == "KWH2500-4999", unit == "KWH",
                            tax == "I_TAX", currency == "EUR") %>% eu_geo() %>%
  group_by(iso3, year = floor(TIME_PERIOD)) %>% summarise(electricity_price = mean(values, na.rm = TRUE), .groups = "drop")
irena <- irena_raw %>%
  transmute(iso3 = `Country/area`, year = as.integer(Year) + 2000L,
            cap = as.numeric(.data[[names(irena_raw)[ncol(irena_raw)]]])) %>%
  filter(iso3 %in% COUNTRIES) %>% arrange(iso3, year) %>% group_by(iso3) %>%
  mutate(capacity_additions_mw = cap - lag(cap)) %>% ungroup()

country_year <- exposure_panel %>%
  left_join(gdp, by = c("iso3", "year")) %>% left_join(elec, by = c("iso3", "year")) %>%
  left_join(ren, by = c("iso3", "year")) %>% left_join(irena, by = c("iso3", "year"))

# --- 2.4 ORBIS firm-year exit panel (Section 3.2) -------------------------
col <- function(p) names(orbis_raw)[grepl(p, names(orbis_raw), ignore.case = TRUE)][1]
emp_cols <- names(orbis_raw)[grepl("^Number of employees \\d{4}$", names(orbis_raw))]
emp_cols <- emp_cols[!duplicated(str_extract(emp_cols, "\\d{4}"))]   # export contains the block twice
rev_cols <- names(orbis_raw)[grepl("^Operating revenue.*\\d{4}$", names(orbis_raw))]

orbis <- orbis_raw %>%
  mutate(firm_name = .data[[col("Company name")]],
         iso3 = countrycode(.data[[col("Country ISO code")]], "iso2c", "iso3c"),
         branch = .data[[col("^Branch$")]]) %>%
  filter(iso3 %in% COUNTRIES, !is.na(firm_name), is.na(branch) | branch == "No") %>%
  group_by(firm_name, iso3) %>% slice(1) %>% ungroup() %>%   # one record per firm
  mutate(is_installer_primary = grepl("3320", .data[[col("NACE.*primary code")]], fixed = TRUE),
         is_inactive = .data[[col("^Inactive$")]] == "Yes",
         status_date = .data[[col("Last update.*Status")]],
         status_type = .data[[col("^Status$")]],
         incorporation_year = suppressWarnings(as.integer(format(as.Date(
           as.numeric(.data[[col("Date of incorporation|Incorporation date")]]), origin = "1899-12-30"), "%Y"))),
         exit_year = if_else(is_inactive & !is.na(status_date), as.integer(format(status_date, "%Y")), NA_integer_))

n_inactive_no_date <- sum(orbis$is_inactive & is.na(orbis$exit_year))
firms <- orbis %>%
  filter(!(is_inactive & is.na(exit_year)), is.na(exit_year) | exit_year >= START) %>%
  mutate(exits_in_window = !is.na(exit_year) & exit_year <= END,
         last = if_else(exits_in_window, exit_year, END),
         first = pmax(coalesce(incorporation_year, START), START))
n_no_incorp <- sum(is.na(firms$incorporation_year))
n_incorp_after_exit <- sum(firms$first > firms$last)

firm_panel <- firms %>% filter(first <= last) %>%
  select(firm_name, iso3, is_installer_primary, status_type, incorporation_year, exit_year, exits_in_window, first, last) %>%
  rowwise() %>% mutate(year = list(first:last)) %>% ungroup() %>% unnest(year) %>%
  mutate(exited = as.integer(exits_in_window & year == exit_year))
write_csv(firm_panel, file.path(proc_dir, "firm_panel.csv"))

# Firm-year employment, 2012-2024 (market structure, Section 4.3)
emp <- orbis %>% select(firm_name, iso3, all_of(emp_cols)) %>%
  mutate(across(all_of(emp_cols), as.character)) %>%
  pivot_longer(all_of(emp_cols), names_to = "year", values_to = "employees") %>%
  mutate(year = as.integer(str_extract(year, "\\d{4}")), employees = suppressWarnings(as.numeric(employees))) %>%
  filter(!is.na(employees), employees > 0, year >= EXIT_START, year <= END)

# --- 2.5 Estimation samples ------------------------------------------------
add_post <- function(d) mutate(d, post_2018 = as.integer(year >= 2018))
exit_full <- firm_panel %>% filter(year <= END) %>% inner_join(exposure, by = "iso3") %>% add_post()
exit_data <- exit_full %>% filter(year >= EXIT_START)                     # main exit sample
cy <- country_year %>% select(-any_of("exposure")) %>% inner_join(exposure, by = "iso3") %>%
  add_post() %>% filter(!is.na(capacity_additions_mw))                    # deployment sample
pop0 <- pop_raw %>% transmute(iso3 = suppressWarnings(countrycode(geo, "eurostat", "iso3c")), year = as.integer(time), values) %>%
  filter(iso3 %in% COUNTRIES, year %in% PRE) %>% group_by(iso3) %>% summarise(pop0 = mean(values), .groups = "drop")
fs <- exposure_panel %>% inner_join(exposure, by = "iso3") %>% add_post() %>%
  filter(!is.na(china_exports_value), china_exports_value > 0) %>% left_join(pop0, by = "iso3") %>%
  mutate(usd_m = china_exports_value / 1e6, usd_pc = china_exports_value / pop0, log_china = log(china_exports_value))


# =============================================================================
# 3. ESTIMATION HELPERS
# =============================================================================
did_f <- function(y, ctrl = "") as.formula(sprintf("%s ~ exposure:post_2018%s | iso3 + year", y, ctrl))
did <- function(d, y, ctrl = "") feols(did_f(y, ctrl), data = d, cluster = ~iso3)

# Restricted wild cluster bootstrap p-value (Rademacher weights)
boot_p <- function(d, y) {
  set.seed(SEED)
  m <- did(d, y); t0 <- coef(m)[["exposure:post_2018"]] / se(m)[["exposure:post_2018"]]
  m0 <- feols(as.formula(paste(y, "~ 1 | iso3 + year")), data = d)
  fit <- predict(m0); res <- resid(m0); cl <- as.character(d$iso3); uc <- unique(cl)
  tb <- replicate(B_BOOT, {
    v <- setNames(sample(c(-1, 1), length(uc), replace = TRUE), uc)
    d$y_star <- fit + res * v[cl]
    mb <- tryCatch(did(d, "y_star"), error = function(e) NULL)
    if (is.null(mb)) NA_real_ else coef(mb)[[1]] / se(mb)[[1]]
  })
  mean(abs(tb) >= abs(t0), na.rm = TRUE)
}
# Unrestricted wild cluster bootstrap-t 95% confidence interval
boot_ci <- function(d, y) {
  set.seed(SEED)
  m <- did(d, y); b <- coef(m)[[1]]; s <- se(m)[[1]]
  fit <- predict(m); res <- resid(m); cl <- as.character(d$iso3); uc <- unique(cl)
  tb <- replicate(B_BOOT, {
    v <- setNames(sample(c(-1, 1), length(uc), replace = TRUE), uc)
    d$y_star <- fit + res * v[cl]
    mb <- tryCatch(did(d, "y_star"), error = function(e) NULL)
    if (is.null(mb)) NA_real_ else (coef(mb)[[1]] - b) / se(mb)[[1]]
  })
  q <- quantile(tb, c(0.025, 0.975), na.rm = TRUE)
  c(lo = b - q[[2]] * s, hi = b - q[[1]] * s)
}
# One results row: coefficient, SE, conventional p, bootstrap p, N
est <- function(d, label, y = "exited", scale = 1e5, ci = FALSE) {
  m <- did(d, y)
  r <- tibble(spec = label, coef = scale * coef(m)[[1]], se = scale * se(m)[[1]],
              p_conv = pvalue(m)[[1]], p_boot = boot_p(d, y), n = nobs(m),
              mean_y = if (y == "exited") 100 * mean(d$exited) else NA_real_)
  if (ci) { b <- boot_ci(d, y); r <- mutate(r, ci_lo = scale * b[["lo"]], ci_hi = scale * b[["hi"]]) }
  r
}
event_study <- function(d, y, scale = 1) {
  m <- feols(as.formula(paste(y, "~ i(year, exposure, ref = 2017) | iso3 + year")), data = d, cluster = ~iso3)
  ct <- coeftable(m)
  tibble(year = as.integer(str_extract(rownames(ct), "\\d{4}")), coef = scale * ct[, 1],
         se = scale * ct[, 2], p = ct[, 4])
}


# =============================================================================
# 4. RESULTS, in thesis order
# =============================================================================

# --- Figure 1.1: Chinese imports and capacity additions, 15 countries -------
fig <- function(d, file) {
  p <- ggplot(d, aes(year, value)) +
    annotate("rect", xmin = 2013 + 5/12, xmax = 2018 + 8/12, ymin = -Inf, ymax = Inf, fill = "grey60", alpha = 0.15) +
    geom_line(colour = "#185FA5", linewidth = 0.7) + geom_point(colour = "#185FA5", size = 1.4) +
    scale_x_continuous(breaks = seq(2000, 2024, 4), limits = c(START, END)) +
    labs(x = NULL, y = NULL) + theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
  ggsave(file.path(out_dir, file), p, width = 3.4, height = 2.8)
}
fig(exposure_panel %>% group_by(year) %>% summarise(value = sum(china_exports_value, na.rm = TRUE) / 1e9), "fig1_1a_imports.pdf")
fig(cy %>% group_by(year) %>% summarise(value = sum(capacity_additions_mw) / 1e3), "fig1_1b_capacity.pdf")

# --- Chapter 3: in-text numbers ---------------------------------------------
# share of firms with at least one positive value (ORBIS stores missing as "n.a.")
coverage <- function(cols) if (!length(cols)) NA else
  round(100 * mean(rowSums(sapply(orbis[cols], function(x) coalesce(suppressWarnings(as.numeric(x)) > 0, FALSE))) > 0))
show(tibble(
  item = c("LED share of Chinese 854140 imports, 2010-2012 (%)",
           "Manufacturers (firms)", "Installers (firms)",
           "Inactive firms with no status date (dropped)",
           "Firms with no incorporation date", "Firms dropped: incorporation after exit",
           "Production imputed (country-years)", "Country-years in first-stage sample",
           "Firms with any employment data (%)", "Firms with any revenue data (%)"),
  value = c(round(100 * sum(led_raw$OBS_VALUE[as.character(led_raw$product) == "85414010"]) / sum(led_raw$OBS_VALUE), 1),
            with(filter(exit_data, !is_installer_primary), n_distinct(firm_name, iso3)),
            with(filter(exit_data, is_installer_primary), n_distinct(firm_name, iso3)),
            n_inactive_no_date, n_no_incorp, n_incorp_after_exit,
            sum(fs$is_imputed, na.rm = TRUE), nrow(fs),
            coverage(emp_cols), coverage(rev_cols))),
  "Chapter 3 in-text numbers")
show(fs %>% filter(is_imputed) %>% count(iso3, name = "imputed_years"), "Chapter 3 imputed production by country")

# --- Table 3.1: exposure and exits by country -------------------------------
show(exit_data %>% group_by(iso3) %>%
       summarise(exposure = round(first(exposure), 1), firms = n_distinct(firm_name), firm_years = n(),
                 exits_2012_17 = sum(exited[year < 2018]), exits_2018_24 = sum(exited[year >= 2018]),
                 exits = sum(exited), exit_rate = round(100 * mean(exited), 2)) %>% arrange(desc(exposure)),
     "Table 3.1 Exposure and exits by country")

# --- Table 5.1: first stage --------------------------------------------------
fs_spec <- list(All = fs, `Excl. NL` = filter(fs, iso3 != "NLD"), `Excl. NL, BE` = filter(fs, !iso3 %in% c("NLD","BEL")))
show(bind_rows(lapply(c("usd_m", "usd_pc"), function(y) bind_rows(lapply(names(fs_spec), function(nm)
  est(fs_spec[[nm]], paste(y, nm), y = y, scale = 1) %>% mutate(countries = n_distinct(fs_spec[[nm]]$iso3)))))) %>%
    select(-mean_y), "Table 5.1 First stage")
med_exp <- median(exposure$exposure)
grp <- fs %>% mutate(high = exposure > med_exp)
show(tibble(
  outcome = c("log imports", "import penetration (winsorized)"),
  coef = c(coef(did(fs, "log_china"))[[1]], coef(did(filter(fs, !is.na(import_penetration_winsorized)), "import_penetration_winsorized"))[[1]]),
  se   = c(se(did(fs, "log_china"))[[1]],   se(did(filter(fs, !is.na(import_penetration_winsorized)), "import_penetration_winsorized"))[[1]])),
  "Table 5.1 note: log and penetration first stage")
show(grp %>% group_by(high, period = if_else(year < 2018, "pre-2018", "2018+")) %>%
       summarise(mean_log_imports = mean(log_china), .groups = "drop"),
     "Table 5.1 note: mean log imports by exposure group")

# --- Table A.7: first stage, leave one country out --------------------------
show(map_dfr(sort(unique(fs$iso3)), function(cc) {
  d <- filter(fs, iso3 != cc); m1 <- did(d, "usd_m"); m2 <- did(d, "usd_pc")
  tibble(dropped = cc, usd_m = coef(m1)[[1]], p_usd_m = pvalue(m1)[[1]], usd_pc = coef(m2)[[1]], p_usd_pc = pvalue(m2)[[1]])
}), "Table A.7 First stage leave one out")

# --- Table A.9: event study (parallel trends, Section 5.1) -----------------
es_exit <- event_study(exit_data, "exited", 1e5)
es_cap  <- event_study(filter(cy, year >= EXIT_START), "capacity_additions_mw")
show(full_join(rename_with(es_exit, ~ paste0("exit_", .x), -year), rename_with(es_cap, ~ paste0("cap_", .x), -year), by = "year"),
     "Table A.9 Event study")
show(bind_rows(
  event_study(filter(exit_data, iso3 != "NLD"), "exited", 1e5) %>% filter(year == 2016) %>% mutate(dropped = "NLD (table note)"),
  map_dfr(unique(exit_data$iso3), ~ event_study(filter(exit_data, iso3 != .x), "exited", 1e5) %>%
            filter(year == 2016) %>% mutate(dropped = .x))), "Section 5.1 2016 coefficient leave one out")

# --- Table 5.2: firm exit -----------------------------------------------------
# These four specifications are reused in Tables 5.3 and A.8
main     <- est(exit_data, "2012-2024", ci = TRUE)
no_nl    <- est(filter(exit_data, iso3 != "NLD"), "2012-2024 excl. NL", ci = TRUE)
from13   <- est(filter(exit_data, year >= 2013), "2013-2024", ci = TRUE)
from13nl <- est(filter(exit_data, year >= 2013, iso3 != "NLD"), "2013-2024 excl. NL", ci = TRUE)

t52 <- bind_rows(
  mutate(main, spec = "(1) All firms"),
  mutate(no_nl, spec = "(2) Excl. Netherlands"),
  est(filter(exit_data, !is_installer_primary), "(3) Manufacturers"),
  est(filter(exit_data, is_installer_primary), "(4) Installers"))
show(t52, "Table 5.2 Firm exit")

set.seed(SEED)
perm <- replicate(N_PERM, {
  d <- firm_panel %>% filter(year >= EXIT_START, year <= END) %>%
    inner_join(mutate(exposure, exposure = sample(exposure)), by = "iso3") %>% add_post()
  tryCatch(coef(did(d, "exited"))[[1]], error = function(e) NA_real_)
})
cat("\nRandomization inference p-value (Table 5.2 note):",
    round(mean(abs(perm) >= abs(coef(did(exit_data, "exited"))[[1]]), na.rm = TRUE), 3), "\n")

# --- Table 5.3: robustness ---------------------------------------------------
neg <- exposure$iso3[exposure$exposure < 0]
alt_exp <- function(v) firm_panel %>% filter(year >= EXIT_START, year <= END) %>%
  inner_join(select(exposure_comext, iso3, exposure = all_of(v)), by = "iso3") %>% add_post()
show(bind_rows(
  mutate(main, spec = "Baseline (2012-2024)"),
  est(filter(exit_data, iso3 != "DEU"), "Excl. Germany"),
  est(filter(exit_data, !iso3 %in% c("NLD","DEU")), "Excl. Netherlands and Germany"),
  est(filter(exit_data, year != 2020), "Excl. 2020"),
  est(filter(exit_data, !iso3 %in% neg), "Excl. negative exposure"),
  mutate(from13, spec = "From 2013"),
  mutate(from13nl, spec = "From 2013, excl. Netherlands"),
  est(filter(exit_data, year >= 2014), "From 2014"),
  est(exit_full, "Full panel, 1999-2024"),
  est(alt_exp("exposure_world"), "Comext, world trade"),
  est(alt_exp("exposure_extraeu"), "Comext, extra-EU trade")), "Table 5.3 Robustness")

# --- Table A.8: bootstrap CIs and implied effect sizes ----------------------
gap <- round(exposure$exposure[exposure$iso3 == "ESP"], 1) - round(exposure$exposure[exposure$iso3 == "DEU"], 1)
show(bind_rows(main, no_nl, from13, from13nl) %>%
       mutate(across(c(coef, ci_lo, ci_hi), ~ gap * .x / 1e5 * 100, .names = "pp_{.col}"),       # pp change, Germany-Spain gap
              across(c(pp_coef, pp_ci_lo, pp_ci_hi), ~ 100 * .x / mean_y, .names = "rel_{.col}")), # % of mean exit rate
     "Table A.8 Bootstrap CIs and effect sizes")

# --- Table 5.4: deployment ---------------------------------------------------
cy_c <- filter(cy, !is.na(gdp_pc), !is.na(electricity_price), !is.na(renewable_share_pct))
m_ctrl <- did(cy_c, "capacity_additions_mw", " + gdp_pc + electricity_price + renewable_share_pct")
show(bind_rows(
  est(cy, "(1) Baseline", "capacity_additions_mw", 1),
  tibble(spec = "(2) Controls", coef = coef(m_ctrl)[["exposure:post_2018"]], se = se(m_ctrl)[["exposure:post_2018"]],
         p_conv = pvalue(m_ctrl)[["exposure:post_2018"]],
         p_boot = boot_p(cy_c, "capacity_additions_mw"), n = nobs(m_ctrl)),   # as in thesis: bootstrap of the no-controls model on the controls sample
  est(filter(cy, iso3 != "NLD"), "(3) Excl. NL", "capacity_additions_mw", 1),
  est(filter(cy, !iso3 %in% c("NLD","BEL")), "(4) Excl. NL, BE", "capacity_additions_mw", 1),
  est(filter(cy, year >= 2013), "(5) From 2013", "capacity_additions_mw", 1),
  est(filter(cy, year >= 2013, iso3 != "NLD"), "(6) From 2013, excl. NL", "capacity_additions_mw", 1)) %>%
    select(-mean_y), "Table 5.4 Deployment")
show(tibble(term = names(coef(m_ctrl)), coef = coef(m_ctrl), se = se(m_ctrl)), "Table 5.4 control coefficients")

# --- Table 5.5: market concentration ----------------------------------------
shares_cy <- function(e) e %>% group_by(iso3, year) %>% filter(n() >= 3) %>%
  mutate(s = employees / sum(employees))
hhi  <- shares_cy(emp) %>% summarise(hhi = 10000 * sum(s^2), .groups = "drop")
fixed_firms <- emp %>% count(firm_name, iso3) %>% filter(n >= 10)
hhi_fixed <- shares_cy(semi_join(emp, fixed_firms, by = c("firm_name", "iso3"))) %>%
  summarise(hhi_fixed = 10000 * sum(s^2), .groups = "drop")
top3 <- shares_cy(emp) %>% summarise(top3 = 100 * sum(sort(s, decreasing = TRUE)[1:3]), .groups = "drop")
prep <- function(d) d %>% inner_join(exposure, by = "iso3") %>% add_post()
show(bind_rows(
  est(prep(hhi), "(1) HHI", "hhi", 1),
  est(prep(hhi_fixed), "(2) HHI, fixed panel", "hhi_fixed", 1),
  est(prep(top3), "(3) Top-three share", "top3", 1)) %>% select(-mean_y) %>%
    mutate(countries = c(n_distinct(prep(hhi)$iso3), n_distinct(prep(hhi_fixed)$iso3), n_distinct(prep(top3)$iso3))),
  "Table 5.5 Market concentration")
cat("\nFixed panel firms:", nrow(fixed_firms), "of", n_distinct(paste(emp$firm_name, emp$iso3)),
    "| country-years dropped (<3 firms):", nrow(distinct(emp, iso3, year)) - nrow(hhi), "of", nrow(distinct(emp, iso3, year)), "\n")

# --- Table 5.6: exit rates by firm size --------------------------------------
size <- exit_data %>% inner_join(emp, by = c("firm_name", "iso3", "year")) %>%
  group_by(iso3, year) %>% mutate(group = if_else(employees > median(employees), "Larger", "Smaller")) %>% ungroup()
show(bind_rows(
  size %>% group_by(group) %>% summarise(firm_years = n(), exits = sum(exited)),
  size %>% summarise(group = "Employment-reporting", firm_years = n(), exits = sum(exited)),
  exit_data %>% summarise(group = "All firms", firm_years = n(), exits = sum(exited))) %>%
    mutate(exit_rate = round(100 * exits / firm_years, 2)), "Table 5.6 Exit rates by firm size")

# --- Table A.2: years with Comtrade data, by HS code -------------------------
show(comtrade_raw %>% filter(as.numeric(primaryValue) > 0) %>% group_by(cmdCode) %>%
       summarise(first = min(refYear), last = max(refYear)), "Table A.2 Comtrade years by code")

# --- Table A.4: composition of exits by ORBIS status -------------------------
show(exit_data %>% filter(exited == 1) %>%
       mutate(status = case_when(status_type %in% c("Bankruptcy", "Dissolved (bankruptcy)") ~ "Bankruptcy incl. dissolution following bankruptcy",
                                 status_type %in% c("Inactive (no precision)", "Active") ~ "Inactive, no further detail",
                                 TRUE ~ status_type)) %>%
       count(status, sort = TRUE) %>% mutate(share = round(100 * n / sum(n), 1)), "Table A.4 Exit composition")

# --- Table A.5: exits by year and incorporation cohort -----------------------
show(exit_data %>% group_by(year) %>%
       summarise(firm_years = n(), exits = sum(exited), exit_rate = round(100 * mean(exited), 2),
                 exits_incorp_pre2012 = sum(exited[coalesce(incorporation_year, 0L) < 2012]),
                 exits_incorp_2012plus = sum(exited[coalesce(incorporation_year, 0L) >= 2012])),
     "Table A.5 Exits by year")

# --- Table A.6: exits by country and year ------------------------------------
show(exit_data %>% count(iso3, year, wt = exited) %>% pivot_wider(names_from = year, values_from = n, values_fill = 0) %>%
       mutate(Total = rowSums(across(-iso3))), "Table A.6 Exits by country and year")

cat("\nDone. All tables are saved as CSV files in output/.\n")