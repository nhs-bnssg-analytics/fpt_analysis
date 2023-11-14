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
  ~ download_url_to_directory(
    url = .x,
    new_directory = "Population",
    filename = basename(.x)
  )
)



older_population <- purrr::map_dfr(
  files,
  calculate_icb_populations
)

write.csv(
  older_population,
  "data/population.csv",
  row.names = FALSE
)

# risk factors

risk_factors <- fingertipsR::indicators() |> 
  filter(
    grepl("[Rr]isk", DomainName),
    grepl("[Ff]actor", DomainName)
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
    year = stringr::str_extract(
      year,
      "^[0-9]{4}"
    )
  ) |> 
  convert_ons_to_health_code(
    area_type = "icb"
  )

write.csv(
  risk_factors,
  "data/risk-factors-fingertips.csv",
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
  ~ download_url_to_directory(
    url = .x,
    new_directory = "Bed Availability and Occupancy Data – Overnight"
  )
)

# clean filenames and remove unnecessary files
purrr::walk(
  files,
  rename_hospital_beds_xls
)

# tidy xlsx spreadsheets
# this function ignores the xls files
quarterly_overnight_beds <- list.files(
  "data-raw/Bed Availability and Occupancy Data – Overnight/",
  full.names = TRUE
) |> 
  purrr::map_dfr(
    ~ reformat_bed_availability_data(
      filepath = .x,
      bed_type = "overnight"
    )
  )

orgs <- quarterly_overnight_beds |> 
  pull(org) |> 
  unique()

org_lkp <- attach_icb_to_org(orgs)

quarterly_overnight_beds <- quarterly_overnight_beds |> 
  left_join(
    org_lkp,
    by = join_by(
      org == health_org_code
    )
  ) |> 
  summarise(
    across(
      c(numerator, denominator),
      ~ sum(.x, na.rm = TRUE)
    ),
    .by = c(year, quarter, icb_code, metric, frequency)
  ) |> 
  rename(
    org = icb_code
  ) |> 
  mutate(value = numerator / denominator)

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
  ~ download_url_to_directory(
    url = .x,
    new_directory = "Bed Availability and Occupancy Data – Day"
  )
)


# clean and move file names
purrr::walk(
  files,
  rename_hospital_beds_xls
)

# tidy xlsx spreadsheets
# this function ignores the xls files
quarterly_day_beds <- list.files(
  "data-raw/Bed Availability and Occupancy Data – Day/",
  full.names = TRUE
) |> 
  purrr::map_dfr(
    ~ reformat_bed_availability_data(
      filepath = .x,
      bed_type = "day"
    )
  )

orgs <- quarterly_day_beds |> 
  pull(org) |> 
  unique()

org_lkp <- attach_icb_to_org(orgs)

quarterly_day_beds <- quarterly_day_beds |> 
  left_join(
    org_lkp,
    by = join_by(
      org == health_org_code
    )
  ) |> 
  summarise(
    across(
      c(numerator, denominator),
      ~ sum(.x, na.rm = TRUE)
    ),
    .by = c(year, quarter, icb_code, metric, frequency)
  ) |> 
  rename(
    org = icb_code
  ) |> 
  mutate(value = numerator / denominator)

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



# GPs per population
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
    metric = paste0(
      "ESR absence (",
      tolower(Org.Type),
      " organisation type)"
      ),
    frequency = "monthly"
  ) |> 
  select(
    "year",
    "month",
    "metric",
    org = "Org.Code",
    org_name = "Org.Name",
    numerator = "FTE.Days.Sick",
    denominator = "FTE.Days.Available",
    value = "SA.Rate....",
    "frequency"
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
    metric = paste0(
      "ESR absence (",
      tolower(ORG_TYPE),
      " organisation type)"
    ),
    frequency = "monthly"
  ) |> 
  select(
    "year",
    "month",
    "metric",
    org = "ORG_CODE",
    org_name = "ORG_NAME",
    numerator = "FTE_DAYS_LOST",
    denominator = "FTE_DAYS_AVAILABLE",
    value = "SICKNESS_ABSENCE_RATE_PERCENT",
    "frequency"
  )

monthly_sickness_absence <- bind_rows(
    monthly_sickness_absence, 
    monthly_sickness_monthly_files
  )

org_icb_lkp <- monthly_sickness_absence |> 
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
  attach_icb_to_org()

monthly_sickness_absence <- monthly_sickness_absence |> 
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
      year, month, metric, icb_code, frequency
    )
  ) |> 
  mutate(
    value = numerator / denominator
  ) |> 
  rename(
    org = icb_code
  )

quarterly_sickness_absence <- monthly_to_quarterly_mean(
  monthly_sickness_absence
)

annual_sickness_absence <- monthly_to_annual_mean(
  monthly_sickness_absence,
  year_type = "financial"
)

bind_rows(
  monthly_sickness_absence,
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
  ~ download_url_to_directory(
    url = .x,
    new_directory = "Social care funding"
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
  unlist()

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
  select(
    "year",
    "month",
    org = "ICB_STP_ONS_CODE",
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

file <- download_url_to_directory(
  url,
  "Ambulance response times"
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

file <- download_url_to_directory(
  csv_link,
  "Ambulance response times"
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
  ~ download_url_to_directory(
    url = .x,
    new_directory = "No criteria to reside"
  )
)

tidy_nctr <- purrr::map(
  files,
  tidy_nctr_file
)

monthly_nctr <- purrr::map_dfr(
  tidy_nctr,
  aggregate_nctr_to_month
)

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
excel_links <- obtain_links(url)
excel_links <- excel_links[grepl("xlsx$", excel_links)]
excel_links <- excel_links[grepl("Commissioner", excel_links)]
excel_links <- excel_links[grepl("Revision", excel_links)]

files <- purrr::map_chr(
  excel_links,
  ~ download_url_to_directory(
    url = .x,
    new_directory = "Cancer wait times"
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
  ~ download_url_to_directory(
    url = .x,
    new_directory = "A and E wait times"
  )
)

monthly_a_and_e <- purrr::map_dfr(
  files, 
  tidy_a_and_e
) |> 
  convert_ons_to_health_code()

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
  (function(x) x[grepl("Admitted", x)])()

files <- purrr::map_chr(
  xl_files,
  ~ download_url_to_directory(
    url = .x,
    new_directory = "Referral to Treatment"
  )
) |> 
  purrr::map_chr(
    rename_rtt_files
  )

# replace unadjusted files with adjusted ones where they exist
for (file in files) {
  if (grepl("adjusted", file)) {
    unadjusted_file <- gsub("adjusted", "admitted", file)
    if (file.exists(unadjusted_file)) {
      file.remove(
        unadjusted_file
      )
      
      file.rename(
        from = file,
        to = unadjusted_file
      )
    }
    
  }
}

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

