source("R/00_libraries.R")
source("R/01_utils.R")


# Demand data sources-----------------------------------------------------------

# population

url <- "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates"
zip_files <- obtain_links(url) |> 
  (\(x) x[grepl("zip$|xlsx$", x)])() |> 
  (\(x) x[!grepl("2018on20", x)])() |> 
  (\(x) x[grepl("unformatted", x)])() |> 
  (\(x) x[!grepl("^rft-lsoa", basename(x))])() |> 
  (\(x) x[!grepl("males.zip$", x)])() |> 
  (\(x) x[!grepl("sape19|sape18", x)])() |> 
  (\(x) x[!grepl("^rft", basename(x))])()

files <- purrr::map_chr(
  paste0(
    "https://www.ons.gov.uk/",
    zip_files
  ),
  ~ check_and_download(
    url = .x,
    filepath = paste0("data-raw/Population/", basename(.x))
  )
)

population_by_age_band <- purrr::map_dfr(
  files,
  calculate_icb_populations
)

if (max(population_by_age_band$year) < 2021) {
  population_by_age_band_21_22 <- estimate_21_22_populations()
  
  population_by_age_band <- bind_rows(
    population_by_age_band,
    population_by_age_band_21_22
  )
}

write.csv(
  population_by_age_band,
  "data/population.csv",
  row.names = FALSE
)

# risk factors

risk_factors <- fingertipsR::indicators() |> 
  filter(
    grepl("[Rr]isk", DomainName),
    grepl("[Ff]actor", DomainName),
    !grepl("Deprivation score \\(IMD 2019\\)", IndicatorName)
  ) |> 
  pull(
    IndicatorID
  ) |> 
  unique() |> 
  fingertipsR::fingertips_data(
    AreaTypeID = 221
  ) |> 
  filter(
    AreaType == "ICBs"
  ) |> 
  mutate(
    AreaCode = gsub("n", "", AreaCode)
  ) |> 
  # remove unnecessary sex/age groups from Smoking prevalence indicator
  anti_join(
    tibble(
      IndicatorID = 92443,
      Sex = c("Male", "Female", "Persons"),
      Age = c("18+ yrs", "18+ yrs", "18-64 yrs")
    ),
    by = join_by(
      IndicatorID,
      Sex,
      Age
    )
  ) |> 
  select(
    year = "Timeperiod",
    org = "AreaCode",
    org_name = "AreaName",
    metric = "IndicatorName",
    numerator = "Count",
    denominator = "Denominator",
    value = "Value",
  ) |> 
  mutate(
    frequency = case_when(
      grepl("/", year) ~ "annual financial",
      .default = "annual calendar"
    ),
    year = as.integer(
      stringr::str_extract(
        year,
        "^[0-9]{4}"
      )
    )
  ) |> 
  convert_ons_to_health_code(
    area_type = "icb",
    latest_codes_used = TRUE
  )

# if there is data already recorded in the data folder for risk factors, keep the old records that have been deleted
if (file.exists("data/risk-factors-fingertips.csv")) {
  removed_data <- read.csv("data/risk-factors-fingertips.csv") |> 
    anti_join(
      risk_factors,
      by = join_by(
        org, metric, year, frequency
      )
    )
  
  risk_factors <- risk_factors |> 
    bind_rows(
      removed_data
    )
}


risk_factors |> 
  write.csv(
    "data/risk-factors-fingertips.csv",
    row.names = FALSE
)

# deprivation
quintile_descriptions <- fingertipsR::category_types() |> 
  filter(CategoryTypeId == 6) |> 
  select(
    "Id", 
    quintile_name = "ShortName"
  )

lsoa_icb_lkp <- check_and_download(
  filepath = "data-raw/Lookups/lsoa_icb.xlsx",
  url = "https://www.arcgis.com/sharing/rest/content/items/1ac8547e9a8945478f0b5ea7ffe1a6b1/data"
) |> 
  readxl::read_excel() |> 
  select(
    "LSOA11CD",
    "ICB22CDH"
  )

pops_files <- list.files("data-raw/Population/",
                         full.names = TRUE)

pops_files <- set_names(
  pops_files,
  nm = str_extract(
    pops_files,
    "[0-9]{4}"
  )
)

lsoa_pops <- map(
  pops_files,
  open_pops_file
) |> 
  purrr::map_df(
    ~ pluck(.x, "data"),
    .id = "year"
  ) |> 
  select(
    year,
    "LSOA11CD", 
    population = "All Ages"
  ) |> 
  filter(
    grepl("^E", LSOA11CD)
  )

lsoa2122pops <- lsoa_populations_21_22() |> 
  summarise(
    population = sum(population),
    .by = c(
      year, LSOA11CD
    )
  )

lsoa_pops <- bind_rows(
  lsoa_pops,
  lsoa2122pops
)


deprivation <- check_and_download(
  filepath = "data-raw/Deprivation/deprivation.xlsx",
  url = "https://assets.publishing.service.gov.uk/media/5d8b3b51ed915d036a455aa6/File_5_-_IoD2019_Scores.xlsx"
) |> 
  readxl::read_excel(
    sheet = "IoD2019 Scores"
  ) |> 
  select(
    LSOA11CD = "LSOA code (2011)",
    imd = "Index of Multiple Deprivation (IMD) Score"
  ) |> 
  mutate(
    quintile = as.integer(6 - ntile(imd, 5))
  ) |> 
  left_join(
    quintile_descriptions,
    by = join_by(quintile == Id)
  ) |> 
  left_join(
    lsoa_icb_lkp,
    by = join_by(
      LSOA11CD
    )
  ) |> 
  left_join(
    lsoa_pops,
    by = join_by(
      LSOA11CD
    ),
    relationship = "many-to-many"
  ) |> 
  summarise(
    population = sum(population),
    .by = c(
      year,
      ICB22CDH, 
      quintile_name
    )
  ) |> 
  mutate(
    denominator = sum(population),
    .by = c(year, ICB22CDH)
  ) |> 
  rename(
    numerator = "population",
    org = "ICB22CDH"
  ) |> 
  mutate(
    value = numerator / denominator,
    frequency = "annual calendar",
    metric = paste0(
      "Proportion of resident population in national deprivation quintile (",
      quintile_name,
      ") - IMD 2019"
    )
  ) |> 
  select(!c("quintile_name"))

write.csv(
  deprivation,
  "data/deprivation.csv",
  row.names = FALSE
)

# Capacity data sources ---------------------------------------------------

## Hospital beds
# Overnight
url <- "https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/bed-data-overnight/"

excel_urls <- obtain_links(url)

excel_urls <- excel_urls[grepl("xls$|xlsx$", excel_urls, ignore.case = TRUE)]

# download files
files <- purrr::map_chr(
  excel_urls,
  ~ check_and_download(
    url = .x,
    filepath = paste0(
      "data-raw/Bed Availability and Occupancy Data – Overnight/",
      basename(.x)
    )
  )
)

# clean filenames and remove unnecessary files
purrr::walk(
  files,
  rename_hospital_beds_xls
)

# tidy xlsx spreadsheets
# this function ignores the xls files
quarterly_overnight_beds_by_trust <- list.files(
  "data-raw/Bed Availability and Occupancy Data – Overnight/",
  full.names = TRUE
) |> 
  purrr::map_dfr(
    ~ reformat_bed_availability_data(
      filepath = .x,
      bed_type = "overnight"
    )
  )

quarterly_overnight_beds <- quarterly_overnight_beds_by_trust |> 
  apply_catchment_proportions()
  

annual_overnight_beds <- quarterly_to_annual_sum(
  quarterly_overnight_beds,
  year_type = "financial"
)

bind_rows(
  quarterly_overnight_beds,
  annual_overnight_beds
) |> 
  write.csv(
  "data/overnight-beds.csv",
  row.names = FALSE
)

# Day only
url <- "https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/bed-data-day-only/"
excel_urls <- obtain_links(url)
excel_urls <- excel_urls[grepl("xls$|xlsx$", excel_urls, ignore.case = TRUE)]

# download files
files <- purrr::map_chr(
  excel_urls,
  ~ check_and_download(
    url = .x,
    filepath = paste0(
      "data-raw/Bed Availability and Occupancy Data – Day/",
      basename(.x)
    )
  )
)

# clean and move file names
purrr::walk(
  files,
  rename_hospital_beds_xls
)

# tidy xlsx spreadsheets
# this function ignores the xls files
quarterly_day_beds_by_trust <- list.files(
  "data-raw/Bed Availability and Occupancy Data – Day/",
  full.names = TRUE
) |> 
  purrr::map_dfr(
    ~ reformat_bed_availability_data(
      filepath = .x,
      bed_type = "day"
    )
  )

quarterly_day_beds <- quarterly_day_beds_by_trust |> 
  apply_catchment_proportions()

annual_day_beds <- quarterly_to_annual_sum(
  quarterly_day_beds,
  year_type = "financial"
)


bind_rows(
  quarterly_day_beds,
  annual_day_beds
) |> 
  write.csv(
    "data/day-beds.csv",
    row.names = FALSE
  )

# Covid hospital activity -------------------------------------------------

url <- "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/"

links <- obtain_links(url) |> 
  (\(x) x[grepl("xlsx$", x)])() |> 
  (\(x) x[!grepl("Supplementary", x)])() |> 
  (\(x) x[grepl("Covid-Publication", x)])()


files <- purrr::map_chr(
  links,
  ~ check_and_download(
    url = .x,
    filepath = paste0("data-raw/Covid activity/", basename(.x))
  )
)

# remove existing files that have been replaced online
existing_files <- list.files(
  "data-raw/Covid activity/",
  full.names = TRUE
)

setdiff(
  existing_files,
  files
) |> 
  file.remove() |> 
  invisible()

covid_numerators_by_trust <- files |> 
  map_df(
    # this function sums the numerators for each day (rather than taking a mean)
    tidy_covid_beds
  ) |> 
  # take the most recent published data where days are published more than once
  filter(
    published_date == max(published_date), 
    .by = c(
      Date
    )
  ) |> 
  summarise(
    numerator = sum(numerator),
    .by = c(
      sheet, year, quarter, org
    ),
    days = n()
  ) |> 
  arrange(sheet, org, year, quarter)

## covid bed occupancy
quarterly_total_beds_by_trust <- bind_rows(
  quarterly_day_beds_by_trust,
  quarterly_overnight_beds_by_trust
) |> 
  filter(
    grepl("Total", metric)
  ) |> 
  select(
    "year",
    "quarter",
    "org",
    "denominator"
  ) |> 
  summarise(
    denominator = sum(denominator),
    .by = c(
      year, 
      quarter,
      org
    )
  )

quarterly_covid_beds <- covid_numerators_by_trust |> 
  filter(
    sheet == "Total Beds Occupied Covid"
  ) |> 
  select(!c("sheet")) |> 
  mutate(
    numerator = numerator / days
  ) |> 
  right_join(
    quarterly_total_beds_by_trust,
    by = join_by(
      year,
      quarter,
      org
    )
) |> 
  mutate(
    numerator = replace_na(numerator, 0),
    metric = "Mean proportion of beds that contain a patient with confirmed COVID",
    frequency = "quarter"
  ) |> 
  apply_catchment_proportions()

annual_covid_beds <- quarterly_covid_beds |> 
  quarterly_to_annual_mean(
    year_type = "financial"
  )

bind_rows(
  quarterly_covid_beds,
  annual_covid_beds
) |> 
  write.csv(
    "data/covid-beds.csv",
    row.names = FALSE
  )

## covid admissions
## getting total annual admissions data by trust

url <- "https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity"

links <- obtain_links(url) |> 
  (\(x) x[grepl("[0-9]{4}-[0-9]{2}$", x)])() |> 
  (\(x) paste0("https://digital.nhs.uk", x))() |> 
  map(
    obtain_links
  ) |> 
  unlist() |> 
  (\(x) x[grepl("xlsx$", x)])() |> 
  (\(x) x[grepl("prov", x)])() |> 
  (\(x) x[!grepl("leve", x)])()

files <- links |> 
  purrr::map_chr(
    ~ check_and_download(
      filepath = paste0("data-raw/Hospital admissions/", basename(.x)),
      url = .x
    )
  )

admissions_by_trust <- files |> 
  map_df(
    tidy_admissions
  )


covid_admissions <- covid_numerators_by_trust |> 
  filter(
    sheet == "Admissions Total"
  ) |> 
  mutate(
    org = case_when(
      grepl("^R", org) & (nchar(org) == 5) ~ substr(org, 1, 3),
      .default = org
    )
  ) |> 
  summarise(
    numerator = sum(numerator),
    .by = c(
      year, org
    )
  )

# Org codes in covid_admissions and not in admissions_by_trust are community
# hospitals. Org codes in admissions_by_trust and not in covid_admissions are
# "treatment centres" (R0176), which often includes eye care
admissions_with_covid <- covid_admissions |> 
  inner_join(
    admissions_by_trust,
    by = join_by(org, year)
  ) |> 
  mutate(
    frequency = "annual financial",
    metric = "Mean proportion of admissions with COVID diagnosis in last 24 hours"
  ) |> 
  apply_catchment_proportions()

backcast_denominator <- admissions_with_covid |> 
  filter(year == min(year)) |> 
  select(
    !c("year", "numerator", "value")
  ) |> 
  cross_join(
    tibble(
      year = seq(
        from = min(admissions_by_trust$year),
        to = min(admissions_with_covid$year) - 1,
        by = 1
      ),
      numerator = 0,
      value = 0
    )
  )

admissions_with_covid |> 
  bind_rows(
    backcast_denominator
  ) |> 
  write.csv(
    "data/covid-admissions.csv",
    row.names = FALSE
  )
  
## covid absences
workforce <- tidy_workforce_ftes(
  type = "HC" # headcount
) |> 
  rename(
    denominator = "numerator"
  ) |> 
  filter(
    grepl("^Total", metric)
  )

covid_absences <- covid_numerators_by_trust |> 
  filter(
    sheet == "Covid Absences"
  ) |> 
  summarise(
    numerator = sum(numerator / days),
    .by = c(
      year, org
    )
  ) |> 
  inner_join(
    workforce,
    by = join_by(
      org, year
    )
  ) |> 
  select(!c("month", "metric")) |> 
  apply_catchment_proportions() |> 
  mutate(
    frequency = "annual financial",
    metric = "Mean proportion of workforce absent in a provider setting due to COVID per available headcount"
  )

backcast_denominator <- covid_absences |> 
  filter(year == min(year)) |> 
  select(
    !c("year", "numerator", "value")
  ) |> 
  cross_join(
    tibble(
      year = seq(
        from = min(workforce$year),
        to = min(covid_absences$year) - 1,
        by = 1
      ),
      numerator = 0,
      value = 0
    )
  )

covid_absences |> 
  bind_rows(
    backcast_denominator
  ) |> 
  write.csv(
    "data/covid-absences.csv",
    row.names = FALSE
  )

# Total beds per 60+ population

population_60_plus <- read.csv(
  "data/population.csv"
) |> 
  filter(
    grepl(paste(seq(60, 90, by = 10), collapse = "|"),
          metric)
  ) |> 
  summarise(
    denominator = sum(numerator),
    .by = c(
      year, org
    )
  )

beds_per_60plus <- c(
  "data/overnight-beds.csv",
  "data/day-beds.csv"
) |> 
  purrr::map_dfr(
    read.csv
  ) |> 
  filter(
    frequency == "annual financial",
    grepl("General", metric)
  ) |> 
  select(
    "year", "org", "frequency",
    numerator = "denominator",
    metric
  ) |> 
  inner_join(
    population_60_plus,
    by = join_by(
      year, org
    )
  ) |> 
  mutate(
    metric = gsub(
      "Bed availability",
      "Total beds per 1,000 60+ yrs",
      metric
    ),
    value = 1e3 * numerator / denominator
  )

write.csv(
  beds_per_60plus,
  "data/beds-per-1000-60plus.csv",
  row.names = FALSE
)


# NHS workforce per population
# collecting the numerator
annual_workforce_fte <- tidy_workforce_ftes() |> 
  apply_catchment_proportions()

# collecting the denominator (populations by health areas)
quarterly_health_pop_denominators <- quarterly_ics_populations()

workforce_metrics <- annual_workforce_fte |> 
  complete(
    year, month, org, metric
  ) |> 
  inner_join(
    quarterly_health_pop_denominators,
    by = join_by(
      org,
      year, 
      month
    )
  ) |> 
  mutate(
    numerator = replace_na(numerator, 0),
    metric = paste0(
      "Workforce FTEs per 10,000 population (",
      metric,
      ")"
    ),
    value = numerator / (denominator / 1e4),
    frequency = "annual calendar"
  ) |> 
  select(!c("month"))

write.csv(
  workforce_metrics,
  "data/nhs_workforce_metrics.csv",
  row.names = FALSE
)

# General practice workforce
quarterly_gps_per_population <- fingertipsR::fingertips_data(
  IndicatorID = 93966,
  AreaTypeID = 221
) |>
  filter(
    AreaType == "ICBs"
  ) |> 
  mutate(
    year = as.integer(
      substr(
        Timeperiod, 1, 4
      )
    ),
    quarter = as.integer(
      substr(
        Timeperiod, 
        nchar(Timeperiod),
        nchar(Timeperiod)
      )
    ),
    org = gsub("n", "", AreaCode),
    frequency = "quarterly"
  ) |> 
  select(
    "year",
    "quarter",
    "org",
    org_name = "AreaName",
    metric = "IndicatorName",
    numerator = "Count",
    denominator = "Denominator",
    value = "Value",
    "frequency"
  ) |> 
  convert_ons_to_health_code(
    area_type = "icb",
    latest_codes_used = TRUE
  )

annual_gps_per_population <- quarterly_to_annual_mean(
  quarterly_gps_per_population,
  year_type = "financial"
)

bind_rows(
  quarterly_gps_per_population,
  annual_gps_per_population
) |> 
  write.csv(
  "data/clinical-workforce-per-population.csv",
  row.names = FALSE
)

# sickness absence
# collecting numerators
# April 2009 to March 2022 file
url <- "https://files.digital.nhs.uk/A6/67E0C1/ESR_ABSENCE_CSV_NHSE.csv"

monthly_sickness_absence <- read.csv(url) |> 
  mutate(
    year = as.integer(
      substr(
        Date, 1, 4
        )
      ),
    month = match(
      tolower(
        gsub(".*-", "", Date)
      ),
      tolower(month.abb)
    ),
    Org.Type = tolower(Org.Type),
    frequency = "monthly"
  ) |> 
  select(
    "year",
    "month",
    org = "Org.Code",
    org_name = "Org.Name",
    `FTE days sick` = "FTE.Days.Sick",
    `FTE days available` = "FTE.Days.Available",
    Org.Type,
    "frequency"
  ) |> 
  pivot_longer(
    cols = starts_with("FTE"),
    names_to = "metric",
    values_to = "numerator"
  ) |> 
  mutate(
    metric = paste0(
      metric,
      " per 10,000 population (",
      Org.Type,
      ")"
    )
  ) |> 
  select(
    !c("Org.Type")
  )

# monthly files April 2022 onwards
url <- "https://digital.nhs.uk/data-and-information/publications/statistical/nhs-sickness-absence-rates"

links <- obtain_links(url) |> 
  (\(x) x[grepl(tolower(paste(month.name, collapse = "|")), x)])() |> 
  (\(x) x[!grepl(tolower(paste(2014:2021, collapse = "|")), x)])() |> 
  (\(x) x[!grepl(tolower(paste(c("january", "february"), 2022, 
                               sep = "-", 
                               collapse = "|")), x)])()

csv_links <- purrr::map(
  .x = paste0(
    "https://digital.nhs.uk/",
    links
  ),
  .f = obtain_links
) |> 
  unlist() |> 
  (\(x) x[grepl("csv$", x)])() |> 
  (\(x) x[!grepl("benchmarking|reason|COVID|ESR_ABSENCE_CSV_NHSE", x, ignore.case = TRUE)])()

monthly_sickness_monthly_files <- purrr::map_dfr(
  csv_links,
  read.csv
) |> 
  mutate(
    year = as.integer(
      substr(
        DATE, nchar(DATE) - 3, nchar(DATE)
      )
    ),
    month = as.integer(
      substr(
        DATE, nchar(DATE) - 6, nchar(DATE) - 5
      )
    ),
    ORG_TYPE = tolower(ORG_TYPE),
    frequency = "monthly"
  ) |> 
  select(
    "year",
    "month",
    org = "ORG_CODE",
    org_name = "ORG_NAME",
    "ORG_TYPE",
    `FTE days sick` = "FTE_DAYS_LOST",
    `FTE days available` = "FTE_DAYS_AVAILABLE",
    "frequency"
  ) |> 
  pivot_longer(
    cols = starts_with("FTE"),
    names_to = "metric",
    values_to = "numerator"
  ) |> 
  mutate(
    metric = paste0(
      metric,
      " per 10,000 population (",
      ORG_TYPE,
      ")"
    )
  ) |> 
  select(
    !c("ORG_TYPE")
  )

monthly_sickness_absence <- bind_rows(
    monthly_sickness_absence, 
    monthly_sickness_monthly_files
  )

trust_ics_lkp <- trust_to_ics_proportions(
  final_year = max(monthly_sickness_absence$year)
) |> 
  rename(
    health_org_code = "TrustCode",
    icb_code = "org"
  )

orgs <- monthly_sickness_absence |> 
  distinct(org) |> 
  mutate(
    role = map_chr(
      .x = org,
      .f = health_org_role
    )
  ) |> 
  filter(
    role %in% c(
      # "RO98" CCG 200
      "RO107", # care trust 11
      # "RO108" # care trust sites 0
      # "RO116" Executive agency programme 3
      "RO157", # Independent providers 4
      # "RO162" Executive agency programme 3
      "RO172", # Independent Sector Healthcare providers 32
      # "RO189" Special Health Authorities 13
      "RO197" # NHS trust 259
      # "RO198", # NHS trust sites 0
      # "RO213" Commissioning Support Units 32
      # "RO261" Executive Agency Programme 42
    )
  ) |> 
  pull(org) |> 
  setdiff(
    unique(trust_ics_lkp$health_org_code)
  )

org_lkp <- nearest_health_orgs(
  missing_orgs = orgs,
  known_orgs = unique(trust_ics_lkp$health_org_code),
  n = 2
) |> 
  mutate(
    known_org_proportion = 1 - (distance / sum(distance)),
    .by = missing_org
  ) |> 
  left_join(
    trust_ics_lkp,
    by = join_by(
      known_org == health_org_code
    ),
    relationship = "many-to-many"
  ) |> 
  mutate(
    proportion = proportion * known_org_proportion
  ) |> 
  select(
    "year",
    health_org_code = "missing_org",
    "icb_code",
    "proportion"
  ) |> 
  bind_rows(
    trust_ics_lkp
  )

# denominators
quarterly_health_pop_denominators <- quarterly_ics_populations() |> 
  rename(
    quarter = "month"
  ) |> 
  mutate(
    quarter = case_when(
      quarter == 1 ~ 4L,
      quarter == 4 ~ 1L,
      quarter == 7 ~ 2L,
      quarter == 10 ~ 3L,
      .default = NA_real_
    )
  )

quarterly_sickness_absence <- monthly_sickness_absence |> 
  inner_join(
    org_lkp,
    by = join_by(
      year,
      org == health_org_code
    ),
    relationship = "many-to-many"
  ) |> 
  mutate(
    quarter = case_when(
      month %in% 1:3 ~ 4L,
      month %in% 4:6 ~ 1L,
      month %in% 7:9 ~ 2L,
      month %in% 10:12 ~ 3L,
      .default = NA_real_
    ),
    year = case_when(
      quarter == 4 ~ year - 1,
      .default = year
    )
  ) |> 
  summarise(
    across(
      c(numerator),
      ~ sum(.x * proportion, na.rm = TRUE)
    ),
    .by = c(
      year, quarter, metric, icb_code
    )
  ) |> 
  inner_join(
    quarterly_health_pop_denominators,
    by = join_by(
      icb_code == org,
      year,
      quarter
    )
  ) |> 
  mutate(
    value = (numerator / denominator) * 1e4,
    frequency = "quarterly"
  ) |> 
  rename(
    org = "icb_code"
  )

annual_sickness_absence <- quarterly_to_annual_mean(
  quarterly_sickness_absence,
  year_type = "financial"
)

bind_rows(
  quarterly_sickness_absence,
  annual_sickness_absence
) |> 
  write.csv(
    "data/sickness-absence.csv",
    row.names = FALSE
    )

# Social care funding
url <- "https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-activity-and-finance-report"

annual_links <- obtain_links(url) |> 
  (function(x) x[grepl("[0-9]{4}-[0-9]{2}$", x)])() |>
  (function(x) paste0("https://digital.nhs.uk",
                      x))()
  
excel_links <- purrr::map(
    .x = annual_links,
    .f = obtain_links
  ) |> 
  unlist() |> 
  (\(x) x[grepl("xls$|xlsx$", x)])() |> 
  (\(x) x[!grepl("dash|comm-care|per-soc|pss-exp|summary", x, ignore.case = TRUE)])()

xl_files <- purrr::map_chr(
  excel_links,
  ~ check_and_download(
    url = .x,
    filepath = paste0("data-raw/Social care funding/", basename(.x))
  )
) 

xl_files <- xl_files |> 
  purrr::map_chr(
    rename_remove_social_care_files
  ) |> 
  (function(x) x[!grepl("deleted", x)])()

annual_social_care <- purrr::map_dfr(
  xl_files,
  tidy_social_care_funding
)

pop_weighted_utla_lkp <- lsoa_utla_icb_weighted_pops()

# convert utla to icb
annual_social_care_icb <- annual_social_care |> 
  mutate(
    year = as.integer(year)
  ) |> 
  inner_join(
    pop_weighted_utla_lkp,
    by = join_by(
      year == year,
      org == UTLACD
    )
  ) |> 
  mutate(
    weighted_pop = population / sum(population),
    .by = c(
      year, org
    )
  ) |> 
  mutate(
    across(
      c(numerator, denominator),
      function(x) x * weighted_pop
    )
  ) |> 
  summarise(
    across(
      c(numerator, denominator),
      sum
    ),
    .by = c(
      ICB22CDH, 
      metric,
      frequency,
      year
    )
  ) |> 
  mutate(
    value = numerator / denominator
  ) |> 
  rename(
    org = ICB22CDH
  )

write.csv(
  annual_social_care_icb,
  "data/annual-social-care-expenditure.csv",
  row.names = FALSE
)



# Performance -------------------------------------------------------------

# For interpretation, the performance metrics are calculated so the higher they
# are the worse the metric


# GP wait times these are help in zip files containing monthly of data (each zip
# file contains multiple csvs, one for each month). The zip files are produced
# monthly, so there is overlap between the csv files in multiple zip files, but
# the data changes slightly (as information updates?). This function will always
# use the most recent file for each month for its information
url <- "https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice/"

url_links <- obtain_links(url) |> 
  (function(url) url[grepl("[a-z].*-[0-9]{4}$", url)])() |> 
  (function(url) paste0("https://digital.nhs.uk", url))() |> 
  purrr::map(
    obtain_links
  ) |> 
  lapply(
    function(x) x[grepl("zip$", x)]
  ) |> 
  lapply(
    function(x) if (length(x) > 1) {
      x[grepl("Daily", x)]
    } else {
      x
    }
  ) |> 
  unlist() |> 
  (\(x) x[grepl("[a-z]{3}_{0,1}[0-9]{2}", x, ignore.case = TRUE)])() |> 
  (\(x) x[grepl(paste(month.abb, collapse = "|"), x, ignore.case = TRUE)])() |> 
  (\(x) x[seq(1, length(x), 4)])()

list.files("data-raw/GP wait times/",
           full.names = TRUE) |> 
  file.remove() |> 
  invisible()

files <- purrr::walk(
  url_links,
  download_unzip_gp_wait_times
)

monthly_gp_wait_times <- setNames(
  list.files(
    "data-raw/GP wait times/"
  ),
  nm = gsub(".csv", "", list.files(
    "data-raw/GP wait times/"
  ))) |> 
  purrr::map_dfr(
    ~ read.csv(
      paste0(
        "data-raw/GP wait times/",
        .x
      ),
      colClasses = c(
        "character",
        "character",
        "integer"
      )
    ),
    .id = "date"
  ) |> 
  mutate(
    year = as.integer(
      paste0(
        "20",
        substr(
          date, 5, 6
        )
      )
    ),
    month = match(
      substr(
        date, 1, 3
      ),
      month.abb
    ),
    TIME_BETWEEN_BOOK_AND_APPT = case_when(
      TIME_BETWEEN_BOOK_AND_APPT %in% c("Same Day", "1 Day", "2 to 7 Days") ~ "0 to 1 weeks",
      TIME_BETWEEN_BOOK_AND_APPT %in% c("8  to 14 Days") ~ "1 to 2 weeks",
      TIME_BETWEEN_BOOK_AND_APPT %in% c("15  to 21 Days") ~ "2 to 3 weeks",
      TIME_BETWEEN_BOOK_AND_APPT %in% c("22  to 28 Days") ~ "3 to 4 weeks",
      TIME_BETWEEN_BOOK_AND_APPT %in% c("More than 28 Days") ~ "Over 4 weeks",
      .default = "Unknown/Data Issue"
    ),
    metric = paste0(
      "Proportion of attended appointments",
      " (",
      TIME_BETWEEN_BOOK_AND_APPT,
      " wait time)"
    )
  ) |> 
  filter(
    TIME_BETWEEN_BOOK_AND_APPT != "Unknown/Data Issue"
  ) |>
  summarise(
    numerator = sum(COUNT_OF_APPOINTMENTS),
    .by = c(
      date,
      year,
      month,
      ICB_STP_ONS_CODE,
      metric,
      TIME_BETWEEN_BOOK_AND_APPT
    )
  ) |> 
  mutate(
    denominator = sum(numerator),
    .by = c(
      date,
      year, 
      month,
      ICB_STP_ONS_CODE
    )
  ) |> 
  mutate(
    value = numerator / denominator,
    frequency = "monthly"
  ) |> 
  rename(
    org = "ICB_STP_ONS_CODE"
  ) |> 
  convert_ons_to_health_code(
    latest_codes_used = FALSE,
    retain_fields = c("month")
  ) |> 
  select(
    "year",
    "month",
    "org",
    "metric",
    "numerator",
    "denominator",
    "value",
    "frequency"
  )

quarterly_gp_wait_times <- monthly_to_quarterly_sum(
  monthly_gp_wait_times
)

annual_gp_wait_times <- monthly_to_annual_sum(
  monthly_gp_wait_times,
  year_type = "financial"
)


bind_rows(
  monthly_gp_wait_times,
  quarterly_gp_wait_times,
  annual_gp_wait_times
) |> 
  write.csv(
    "data/gp-wait-times.csv",
    row.names = FALSE
    )  

# ambulance response times
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/09/20230825-AmbSYS-AmbCO-indicator-list.xlsx"

file <- check_and_download(
  url = url,
  filepath = paste0("data-raw/Ambulance response times/", basename(url))
)

keep_indicators <- paste0("A", c(8:12, 24, 27, 30, 33, 36))

ambsys_lookup <- read_excel(
  file,
  sheet = "AmbSYS",
  range = "R5C1:R136C3",
  col_names = c(
    "id",
    "description",
    "metric"
  )
) |> 
  filter(
    id %in% keep_indicators
  )

csv_link <- obtain_links("https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/") |> 
  (function(x) x[grepl("csv$", x)])() |> 
  (function(x) x[grepl("^AmbSYS", basename(x))])()

file <- check_and_download(
  url = csv_link,
  filepath = paste0("data-raw/Ambulance response times/", basename(csv_link))
)

monthly_ambsys <- read.csv(
  file,
  na.strings = "."
) |> 
  tidyr::pivot_longer(
    cols = starts_with("A"),
    names_to = "id",
    values_to = "value", 
    values_transform = function(x) gsub(",", "", x)
  ) |> 
  inner_join(
    ambsys_lookup,
    by = join_by(id)
  ) |> 
  mutate(
    description = gsub(" response", "", description)
  ) |> 
  select(!c("id")) |> 
  tidyr::pivot_wider(
    names_from = metric,
    values_from = value
  ) |> 
  rename(
    numerator = "total time",
    denominator = "incident count"
  ) |> 
  mutate(
    across(
      c(numerator, denominator),
      as.numeric
    ),
    metric = paste0(
      "Mean ambulance response time (",
      description,
      ")"
    ),
    value = numerator / denominator,
    frequency = "monthly"
  ) |> 
  filter(
    !is.na(numerator),
    !is.na(denominator)
  ) |> 
  select(
    year = "Year",
    month = "Month",
    org = "Org.Code",
    org_name = "Org.Name",
    "metric",
    "value",
    "numerator",
    "denominator",
    "frequency"
  )

quarterly_ambsys <- monthly_to_quarterly_sum(
  monthly_ambsys
)

annual_ambsys <- monthly_to_annual_sum(
  monthly_ambsys,
  year_type = "financial"
)

bind_rows(
  monthly_ambsys,
  quarterly_ambsys,
  annual_ambsys
) |> 
  write.csv(
    "data/ambulance-response-times.csv",
    row.names = FALSE
)

# No criteria to reside
url <- "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-acute-data/"
excel_links <- obtain_links(url) |> 
  (\(x) x[grepl("xlsx$", x)])() |> 
  (\(x) x[!grepl("[[:alpha:]].*[0-9]{4}-[[:alpha:]].*[0-9]{4}", x)])() |> 
  (\(x) x[grepl("sitrep", x)])()

files <- purrr::map_chr(
  excel_links,
  ~ check_and_download(
    filepath = paste0("data-raw/No criteria to reside/", basename(.x)),
    url = .x
  )
)

tidy_nctr <- purrr::map(
  files,
  tidy_nctr_file
)

monthly_nctr <- purrr::map_dfr(
  tidy_nctr,
  aggregate_nctr_to_month
) |> 
  apply_catchment_proportions()

quarterly_nctr <- monthly_to_quarterly_sum(
  monthly_nctr
)

annual_nctr <- monthly_to_annual_sum(
  monthly_nctr,
  year_type = "financial"
)

bind_rows(
  monthly_nctr,
  quarterly_nctr,
  annual_nctr
) |> 
  write.csv(
    "data/no-criteria-to-reside.csv",
    row.names = FALSE
  )

# 62 day cancer waiting times
url <- "https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/"
excel_links <- obtain_links(url) |> 
  (\(x) x[grepl("xlsx$", x)])() |> 
  (\(x) x[grepl("Commissioner", x)])() |> 
  (\(x) x[grepl("Revision", x)])() |> 
  (\(x) x[!grepl("Jul-2022-Sep-2023", x)])()

list.files("data-raw/Cancer wait times/", full.names = TRUE) |> 
  file.remove() |> 
  invisible()

files <- purrr::map_chr(
  excel_links,
  ~ check_and_download(
    filepath = paste0("data-raw/Cancer wait times/", basename(.x)),
    url = .x
  )
)

monthly_cancer_wait_times <- purrr::map_dfr(
  files,
  tidy_cancer_wait_times
)

orgs <- monthly_cancer_wait_times |> 
  pull(org) |> 
  unique() |> 
  (\(x) x[!grepl("UNKNOWN", x)])()
  
org_lkp <- orgs |> 
  attach_icb_to_org()

monthly_cancer_wait_times <- monthly_cancer_wait_times |> 
  filter(
    # remove welsh organisations and Unknown
    !(org %in% c(
      paste0("7A", 1:7),
      "6A8",
      "UNKNOWN"
    ))
  ) |> 
  left_join(
    org_lkp,
    by = join_by(org == health_org_code)
  ) |> 
  summarise(
    across(
      c(numerator, denominator),
      ~ sum(.x, na.rm = TRUE)
    ),
    .by = c(
      year, month, icb_code, metric, frequency
    )
  ) |> 
  rename(
    org = icb_code
  ) |> 
  mutate(
    value = numerator / denominator
  )

quarterly_cancer_wait_times <- monthly_to_quarterly_sum(
  monthly_cancer_wait_times
)

annual_cancer_wait_times <- monthly_to_annual_sum(
  monthly_cancer_wait_times,
  year_type = "financial"
)

bind_rows(
  monthly_cancer_wait_times,
  quarterly_cancer_wait_times,
  annual_cancer_wait_times
) |> 
  write.csv(
    "data/62-day-cancer-wait-times.csv",
    row.names = FALSE
  )

# A&E 4 hour waits

url <- "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/"

monthly_data_links <- obtain_links(url) |> 
  (function(x) x[grepl("ae-attendances", basename(x))])() |> 
  unique()

excel_file_links <- purrr::map(
  monthly_data_links,
  obtain_links
  ) |> 
  unlist() |> 
  (function(x) x[grepl("xls$|xlsx$", x)])() |> 
  (function(x) x[grepl(paste(month.name, collapse = "|"),
                       basename(x))])() |> 
  # (function(x) x[grepl("by-provider", x)])()
  (function(x) x[!grepl("Growth", x)])() |> 
  (function(x) x[!grepl("Quarter", x)])() |> 
  (function(x) x[!grepl("Supplementary", x)])() |> 
  (\(x) x[!grepl("Attribution", x)])()

files <- purrr::map_chr(
  excel_file_links,
  ~ check_and_download(
    filepath = paste0("data-raw/A and E wait times/", basename(.x)),
    url = .x
  )
)

monthly_a_and_e <- purrr::map_dfr(
  files, 
  tidy_a_and_e
) |> 
  apply_catchment_proportions()

quarterly_a_and_e <- monthly_to_quarterly_sum(
  monthly_a_and_e
)

annual_a_and_e <- monthly_to_annual_sum(
  monthly_a_and_e,
  year_type = "financial"
)  

bind_rows(
  monthly_a_and_e,
  quarterly_a_and_e,
  annual_a_and_e
) |> 
  write.csv(
    "data/a-and-e-4-hours.csv",
    row.names = FALSE
  )

# referral to treatment
url <- "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/"
annual_urls <- obtain_links(url) |> 
  (\(x) x[grepl("^https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/", x)])() |> 
  (\(x) x[grep("[0-9]{4}-[0-9]{2}", x)])() |> 
  unique() |> 
  (\(x) x[!grepl("2011-12|2012-13", x)])()

xl_files <- purrr::map(
  annual_urls,
  obtain_links
) |> 
  unlist() |> 
  (function(x) x[grepl("xls$|xlsx$", x)])() |> 
  (function(x) x[grepl("Commissioner", x)])() |> 
  (function(x) x[grepl("Incomplete", x)])()

files <- purrr::map_chr(
  xl_files,
  ~ check_and_download(
    filepath = paste0("data-raw/Referral to Treatment/", basename(.x)),
    url = .x
  )
) |> 
  purrr::map_chr(
    rename_rtt_files
  )

# obtain new list of files in the folder
files <- list.files(
  "data-raw/Referral to Treatment",
  full.names = TRUE
)


# tidy the data in the files
monthly_rtt <- files |> 
  purrr::map_dfr(
    tidy_rtt
  )

org_icb_lkp <- monthly_rtt |> 
  distinct(
    org
  ) |> 
  filter(
    org != "-"
  ) |> 
  pull() |> 
  attach_icb_to_org()

monthly_rtt <- monthly_rtt |> 
  inner_join(
    org_icb_lkp,
    by = join_by(
      org == health_org_code
    )
  ) |> 
  summarise(
    across(
      c(numerator, denominator),
      ~ sum(.x, na.rm = TRUE)
    ),
    .by = c(
      year, month, icb_code, metric, frequency
    )
  ) |> 
  rename(
    org = "icb_code"
  ) |> 
  mutate(
    value = numerator / denominator
  )

quarterly_rtt <- monthly_to_quarterly_sum(
  monthly_rtt
)

annual_rtt <- monthly_to_annual_sum(
  monthly_rtt,
  year_type = "financial"
)

bind_rows(
  monthly_rtt,
  quarterly_rtt,
  annual_rtt
) |> 
  write.csv(
    "data/referral-to-treatment.csv",
    row.names = FALSE
  )

