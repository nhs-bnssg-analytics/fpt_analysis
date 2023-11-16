# url processing ----------------------------------------------------------

obtain_links <- function(url) {
  links <- httr::GET(url) %>% 
    XML::htmlParse() %>% 
    XML::xpathSApply(
      path = "//a",
      xmlGetAttr,
      "href"
    )
  return(links)
}


# downloading files -------------------------------------------------------

download_url_to_directory <- function(url, new_directory, filename) {
  new_directory <- paste0(
    "data-raw/",
    new_directory,
    "/"
  )
  if (!isTRUE(file.info(new_directory)$isdir)) 
    dir.create(new_directory, recursive = TRUE)
  
  
  if (missing(filename)) {
    new_filename <- paste0(
      new_directory,
      basename(url)
    )
  } else {
    new_filename <- paste0(
      new_directory,
      filename
    )
  }
  
  
  download.file(
    url,
    new_filename,
    mode = "wb"
  )
  
  return(new_filename)
}

check_and_download <- function(filepath, url) {
  
  if (!file.exists(filepath)) {
    filepath <- download_url_to_directory(
      url = url,
      new_directory = basename(dirname(filepath)),
      filename = basename(filepath)
    )
  }
  
  return(filepath)
}

# file processing ---------------------------------------------------------

rename_hospital_beds_xls <- function(filepath) {
  period <- readxl::read_excel(
    filepath,
    sheet = 1,
    range = "R1C2:R20C3",
    col_names = c("col1", "col2")
  ) %>% 
    filter(
      col1 == "Period:"
    ) %>% 
    pull(col2) 
  period <- gsub(
    "/",
    "",
    period
  )
  
  if (length(period) == 1) {
    file.rename(
      from = filepath,
      to = paste0(
        dirname(filepath),
        "/",
        period,
        ".",
        tools::file_ext(filepath)
      )
    )
  } else {
    file.remove(
      filepath
    )
  }
}

download_unzip_gp_wait_times <- function(zip_url) {
  
  new_directory <- "data-raw/GP wait times/"
  
  if (!isTRUE(file.info(new_directory)$isdir)) 
    dir.create(new_directory, recursive = TRUE)
  
  temp <- tempfile()
  
  download.file(zip_url,
                temp)
  
  zipped_files <- unzip(
    zipfile = temp, 
    list = TRUE
    ) |> 
    filter(
      grepl("[[:alpha:]]{3}_[0-9]{2}.csv", Name)
    ) |> 
    pull(Name)
  
  # remove files that already exist in the directory
  existing_files <- list.files(new_directory)
  if (length(existing_files) > 0) {
    zipped_files <- zipped_files[!grepl(paste(existing_files, collapse = "|"), zipped_files)]
  }
  
  # unzip the new csvs and summarise them to icb/stp geography before saving
  # them in the data-raw folder
  complete <- lapply(
    zipped_files,
    unzip_summarise_write_data,
    temp_file = temp,
    file_directory = new_directory
  )
  
  unlink(temp)
  
  return(TRUE)
}

unzip_summarise_write_data <- function(zipped_csv_file, temp_file, file_directory) {
  
  data <- read.csv(
    unz(
      temp_file, 
      zipped_csv_file)
    ) |> 
    rename(
      any_of(
        c(
          ICB_STP_ONS_CODE = "STP_ONS_CODE",
          ICB_STP_ONS_CODE = "ICB_ONS_CODE",
          ICB_STP_ONS_CODE = "STP_CODE"
        )
      )
    ) |> 
    filter(
      if_all(
        any_of(c("APPT_STATUS", "APPOINTMENT_STATUS")),
        ~ .x == "Attended"
      )
    ) |> 
    summarise(
      COUNT_OF_APPOINTMENTS = sum(COUNT_OF_APPOINTMENTS),
      .by = c(
        ICB_STP_ONS_CODE,
        TIME_BETWEEN_BOOK_AND_APPT
      )
    )
  
  if (nrow(data) > 0) {
    data |> 
      write.csv(
        paste0(
          file_directory,
          stringr::str_extract(
            zipped_csv_file, 
            "[[:alpha:]]{3}_[0-9]{2}.csv"
          )
        ),
        row.names = FALSE
      )
  }
  
  return(TRUE)
}

# reformat no criteria to reside data
tidy_nctr_file <- function(filepath) {
  
  nctr <- tidyxl::xlsx_cells(
    filepath,
    sheets = "Table 2"
  ) |> 
    filter(
      !is.na(content)
    )
  
  min_row <- nctr |> 
    filter(
      col == 5
    ) |> 
    pull(row) |> 
    min()
  
  nctr <- nctr |> 
    filter(
      row >= min_row
    ) |> 
    behead(
      direction = "up-left",
      name = "date_field"
    ) |> 
    behead(
      direction = "left",
      name = "region"
    ) |> 
    behead(
      direction = "left",
      name = "org"
    ) |> 
    behead(
      direction = "left",
      name = "org_name"
    ) |> 
    behead(
      direction = "up",
      name = "metric"
    ) |> 
    select(
      date = "date_field",
      "region",
      "org",
      "org_name",
      "metric",
      value = "numeric"
    ) |> 
    filter(
      metric != "Number of patients discharged",
      grepl("^Q", org)
    ) |> 
    mutate(
      metric = case_when(
        metric == "Number of patients who no longer meet the criteria to reside" ~ "denominator",
        metric == "Number of patients remaining in hospital who no longer meet the criteria to reside" ~ "numerator",
        .default = "error"
      )
    ) |> 
    tidyr::pivot_wider(
      names_from = metric,
      values_from = value
    ) |> 
    mutate(
      metric = "Proportion of patients who no longer meet the criteria to reside that remain in hospital",
      value = numerator / denominator
    ) |> 
    select(
      "date",
      "region",
      "org",
      "org_name",
      "metric",
      "value",
      "numerator",
      "denominator"
    )
  
  return(nctr)
}

tidy_cancer_wait_times <- function(filepath) {
  sheet_names <- tidyxl::xlsx_sheet_names(filepath) |> 
    (function(x) x[grepl("^62", x)])()
  
  # create version of as.Date that doesn't error when ambiguous character string
  # provided
  as.Date_safe <- purrr::possibly(as.Date, otherwise = NA)
  
  tidy_cancer_wait_time_sheet <- function(filepath, sheet) {
    tidied_sheet <- tidyxl::xlsx_cells(
      filepath,
      sheets = sheet
    ) |> 
      filter(
        !is.na(content),
        row > 3
      ) |> 
      behead(
        direction = "up-left",
        name = "month_year"
      ) |> 
      behead(
        direction = "left",
        name = "org"
      ) |> 
      behead(
        direction = "left",
        name = "org_name"
      ) |> 
      behead(
        direction = "up",
        name = "type"
      ) |> 
      filter(
        type != "Performance"
      ) |> 
      mutate(
        type = case_when(
          stringr::str_trim(type) == "Total Treated" ~ "denominator",
          stringr::str_trim(type) == "Within Standard" ~ "numerator",
          .default = "error"
        ),
        month_year = case_when(
          is.na(as.Date_safe(month_year)) ~ as.Date_safe(
            paste("01", month_year),
            format = "%d %B %Y"
          ),
          !is.na(as.Date_safe(month_year)) ~ as.Date_safe(month_year),
          .default = NA_Date_
        ),
        month = lubridate::month(month_year),
        year = lubridate::year(month_year)
      ) |>
      select(
        "year",
        "month",
        "org",
        "org_name",
        metric = "sheet",
        "type",
        "numeric"
      ) |>
      tidyr::pivot_wider(
        names_from = type,
        values_from = numeric
      ) |> 
      mutate(
        value = numerator / denominator,
        frequency = "monthly",
        metric = paste(
          metric,
          "(proportion within standard)"
        )
      )
    
    return(tidied_sheet)
  }
  
  cancer_wait_times <- sheet_names |> 
    purrr::map_dfr(
      .f = ~ tidy_cancer_wait_time_sheet(
        filepath = filepath,
        sheet = .x
      )
    )
  return(cancer_wait_times)
}

tidy_a_and_e <- function(filepath) {
  
  sheets <- readxl::excel_sheets(
    filepath
  ) |> 
    # (function(x) x[grepl("Provider Level Data|A&E Data", x)])()
    (function(x) x[grepl("STP Level Data|System Level Data", x)])()
  
  if (length(sheets) == 0) {
    return(
      tibble(
        org = character(),
        org_name = character(),
        metric = character(),
        denominator = numeric(),
        numerator = numeric(),
        year = numeric(),
        month = numeric(),
        value = numeric(),
        frequency = character()
      )
    )
  }
  
  a_and_e_tidy <- readxl::read_xls(
    filepath,
    sheet = sheets,
    range = "R2C2:R500C31",
    na = c("N/A", "-", ""),
    col_names = paste0("col", 1:30)
  ) 
  
  period <- a_and_e_tidy |> 
    filter(
      col1 == "Period:"
    ) |> 
    pull(col2)
  
  dt <- as.Date(
    paste("01",
          period),
    format = "%d %B %Y"
  )
  
  a_and_e_tidy <- a_and_e_tidy |> 
    unpivotr::as_cells() |> 
    filter(
      row > 13,
      !is.na(chr)
    ) |> 
    behead(
      direction = "up-left",
      name = "category"
    ) |> 
    behead(
      direction = "left",
      name = "org"
    ) |> 
    behead(
      direction = "left",
      name = "org_name"
    ) |> 
    # behead(
    #   direction = "left",
    #   name = "org_name"
    # ) |> 
    behead(
      direction = "up",
      name = "metric"
    ) |> 
    mutate(
      category = case_when(
        category == "A&E attendances" ~ "denominator",
        category %in% c("A&E attendances > 4 hours from arrival to admission, transfer or discharge",
                        "A&E attendances greater than 4 hours from arrival to admission, transfer or discharge") ~ "numerator",
        .default = "error"
      ),
      metric = replace(metric, metric == "Total Attendances > 4 hours", "Total attendances")
    ) |> 
    filter(
      category != "error",
      metric %in% c(
        "Type 1 Departments - Major A&E",
        "Type 2 Departments - Single Specialty",
        "Type 3 Departments - Other A&E/Minor Injury Unit",
        "Total attendances"
      )
    ) |> 
    select(!c(
      "row",
      "col",
      "data_type",
      "org_name",
      "lgl"
    )) |> 
    tidyr::pivot_wider(
      names_from = category,
      values_from = chr,
      values_fn = as.numeric
    ) |> 
    mutate(
      year = lubridate::year(dt),
      month = lubridate::month(dt),
      value = numerator / denominator,
      metric = paste0(
        "Proportion of A&E attendances greater than 4 hours (",
        metric,
        ")"
      ),
      frequency = "monthly"
    ) |> 
    filter(
      !is.na(org)
    )
  
  return(a_and_e_tidy)
}

rename_rtt_files <- function(filepath) {
  monthyear <- stringr::str_extract(
    filepath,
    "[A-za-z]{3}[0-9]{2}"
  )
  
  if (is.na(monthyear)) {
    monthyear <- readxl::read_excel(
      filepath,
      sheet = 1,
      range = "R1C2:R20C3",
      col_names = c("col1", "col2")
    ) %>% 
      filter(
        col1 == "Period:"
      ) %>% 
      pull(col2) |> 
      gsub(" 20", "", x = _)
  }
  
  extension <- tools::file_ext(filepath)
  
  if (grepl("Adjusted", filepath)) {
    description <- "adjusted."
  } else if (grepl("NonAdmitted", filepath)) {
    description <- "nonadmitted."
  } else if (grepl("Admitted", filepath)) {
    description <- "admitted."
  }
  
  filename <- paste0(
    dirname(filepath),
    "/",
    monthyear,
    "_",
    description,
    extension
  )
  
  file.rename(
    from = filepath,
    to = filename
  )
  return(filename)
}

tidy_rtt <- function(filepath) {
  if (grepl("nonadmitted", filepath)) {
    admission_type <- "not admitted"
  } else if (grepl("adjusted", filepath)) {
    admission_type <- "admitted - adjusted"
  } else if (grepl("admitted", filepath)) {
    admission_type <- "admitted"
  }
  
  # check sheet names
  sh_names <- readxl::excel_sheets(
    filepath
  )
  
  if ("System" %in% sh_names) {
    use_sheet <- "System"
  } else if ("ICB" %in% sh_names) {
    use_sheet <- "ICB"
  } else if ("Commissioner" %in% sh_names) {
    use_sheet <- "Commissioner"
  }
  
  rtt <- readxl::read_excel(
    filepath,
    .name_repair = "minimal",
    sheet = use_sheet
  ) |> 
    unpivotr::as_cells() |> 
    filter(
      !is.na(chr),
      row > 11
    )
  
  if (use_sheet == "Commissioner") {
    rtt <- rtt |> 
      behead(
        direction = "left",
        name = "region"
      )
  }
  rtt <- rtt |> 
    behead(
      direction = "left",
      name = "org"
    ) |> 
    behead(
      direction = "left",
      name = "org_name"
    ) |> 
    behead(
      direction = "left",
      name = "treatment_function_code"
    ) |> 
    behead(
      direction = "left",
      name = "treatment_function"
    ) |> 
    behead(
      direction = "up",
      name = "measure"
    ) |> 
    mutate(
      type = case_when(
        measure %in% paste0(">", 0:17,"-", 1:18) ~ "numerator",
        measure == "Total number of completed pathways (with a known clock start)" ~ "denominator",
        .default = "not required"
      )
    ) |> 
    filter(
      type != "not required",
      treatment_function == "Total"
    ) |> 
    mutate(
      chr = gsub("-", "0", chr),
      count = as.numeric(chr)
    ) |> 
    summarise(
      count = sum(count, na.rm = TRUE),
      .by = c(
        org,
        org_name,
        type
      )
    ) |> 
    tidyr::pivot_wider(
      names_from = type,
      values_from = count
    ) |> 
    mutate(
      month = match(substr(
        basename(filepath),
        1, 3
      ), month.abb),
      year = as.integer(
        paste0(
          "20",
          substr(
            basename(filepath),
            4, 5
          )
        )
      ),
      numerator = denominator - numerator,
      value = numerator / denominator,
      metric = paste0(
        "Proportion of completed pathways greater than 18 weeks from referral (",
        admission_type,
        ")"
      ),
      frequency = "monthly"
    )
  
  return(rtt)
}

rename_remove_social_care_files <- function(filepath) {
  if (!("T1" %in% readxl::excel_sheets(filepath))) {
    file.remove(filepath)
    return("deleted")
  } else {
    new_name <- readxl::read_excel(
      path = filepath,
      sheet = "Title",
      col_names = paste0("col", 1:2)
    ) |> 
      filter(
        grepl("^Adult Social Care Activity and Finance", col1)
      ) |> 
      pull(col1) |> 
      (function(x) gsub("(\\n)|(\\r)", " ", x))() |> 
      stringr::str_squish() |> 
      (function(x) paste0(
        dirname(filepath),
        "/",
        gsub(":", "", x),
        ".",
        tools::file_ext(filepath)
      ))()
    file.rename(
      from = filepath,
      to = new_name
    )
    return(new_name)
  }
  
}

tidy_social_care_funding <- function(filepath) {
  tidy_sc_funding <- tidyxl::xlsx_cells(
    path = filepath,
    sheets = "T1"
  ) 
  
  min_row <- tidy_sc_funding |> 
    filter(character %in% c("Geography code",
                            "Local Authority code and description")) |> 
    pull(row)
  
  tidy_sc_funding <- tidy_sc_funding |> 
    filter(
      !is.na(content),
      row >= min_row
    ) |> 
    behead(
      direction = "left",
      name = "org"
    ) |> 
    behead(
      direction = "left",
      name = "number"
    ) |> 
    behead(
      direction = "left",
      name = "org_name"
    ) |> 
    behead(
      direction = "left",
      name = "region"
    ) |> 
    behead(
      direction = "left",
      name = "region_name"
    ) |> 
    behead(
      direction = "up-left",
      name = "metric"
    ) |> 
    behead(
      direction = "up",
      name = "age_band"
    ) |> 
    mutate(
      type = case_when(
        metric == "Gross Total Expenditure" ~ "numerator",
        metric == "Long Term Support during the year" ~ "denominator",
        .default = "Not required"
      )
    ) |> 
    filter(
      metric != "Not required"
    ) |> 
    summarise(
      numeric = sum(numeric),
      .by = c(
        org, region, type
      )
    ) |> 
    mutate(
      org = case_when(
        is.na(org) ~ region,
        org == "" ~ region,
        .default = org
      ),
      numeric = as.numeric(numeric)
    ) |> 
    tidyr::pivot_wider(
      names_from = type,
      values_from = numeric
    ) |> 
    select(
      "org", 
      "numerator",
      "denominator"
    ) |> 
    mutate(
      value = numerator / denominator,
      metric = "Gross Total Expenditure (£000s) per individual with long term support during the year",
      frequency = "annual financial",
      year = stringr::str_extract(
        filepath, "[0-9]{4}"
      )
    ) |> 
    filter(
      grepl("^E", org),
      !grepl("E92|E12", org)
    )
  
  return(tidy_sc_funding)
}


open_pops_file <- function(raw_pops_file) {
  
  yr <- stringr::str_extract(
    raw_pops_file,
    "[0-9]{4}"
  )
  
  if (grepl("zip$", raw_pops_file)) {
    fl <- unzip(
      raw_pops_file, 
      list = TRUE
    ) |> 
      pull(Name)
    
    unzipped_file <- unzip(
      raw_pops_file, 
      fl,
      exdir = dirname(raw_pops_file)
    )
    
    persons_sheet <- readxl::excel_sheets(
      unzipped_file
    ) |> 
      (\(x) x[grepl("Persons", x)])()
    
    all_persons <- unzipped_file
  } else if (grepl("xlsx$", raw_pops_file)) {
    
    persons_sheet <- readxl::excel_sheets(
      raw_pops_file
    ) |> 
      (\(x) x[grepl("Persons", x)])()
    
    all_persons <- raw_pops_file
  }
  
  all_persons <- all_persons |> 
    readxl::read_excel(
      sheet = persons_sheet,
      skip = 4
    )
  
  lsoa_field <- names(all_persons)[1]
  all_persons <- all_persons |> 
    rename(LSOA11CD = all_of(lsoa_field))
  
  if (grepl("zip$", raw_pops_file)) file.remove(unzipped_file)
  
  return(list('year' = yr, 'data' = all_persons))
  
}


calculate_icb_populations <- function(raw_pops_file) {
  
  lsoa_lkp_file <- check_and_download(
    filepath = "data-raw/Lookups/lsoa_icb.xlsx",
    url = "https://www.arcgis.com/sharing/rest/content/items/1ac8547e9a8945478f0b5ea7ffe1a6b1/data"
  )|> 
    readxl::read_excel() |> 
    select(
      "LSOA11CD",
      "ICB22CDH"
    )
  
  dta <- open_pops_file(raw_pops_file)
  yr <- dta$year
  all_persons <- dta$data
  
  
  all_persons <- all_persons |> 
    select(
      LSOA11CD,
      as.character(0:89),
      "90+"
    ) |>
    filter(
      grepl("^E", LSOA11CD)
    ) |> 
    pivot_longer(
      cols = !c("LSOA11CD"),
      names_to = "age",
      values_to = "population"
    ) |> 
    mutate(
      age = ifelse(age == "90+", 90, age),
      age_band = floor(as.numeric(age) / 10) * 10
    ) |> 
    left_join(
      lsoa_lkp_file,
      by = join_by(LSOA11CD)
    ) |> 
    summarise(
      numerator = sum(population),
      .by = c(
        ICB22CDH,
        age_band
      )
    ) |> 
    mutate(
      metric = case_when(
        age_band != "90" ~ paste(age_band, age_band + 9, sep = "-"),
        .default = "90+"
      ),
      metric = paste0(
        "Proportion of population in age band (",
        metric,
        ")"
      )
    ) |> 
    mutate(
      denominator = sum(numerator),
      .by = ICB22CDH
    ) |> 
    mutate(
      value = numerator / denominator,
      year = yr,
      frequency = "annual calendar"
    ) |> 
    select(
      !c("age_band")
    ) |> 
    rename(
      org = "ICB22CDH"
    )
  
  return(all_persons)
}

lsoa_populations <- function(raw_pops_file) {
  lsoa_populations <- open_pops_file(raw_pops_file) |> 
    pluck("data") |> 
    select(
      "LSOA11CD",
      "All Ages"
    )
  
  return(lsoa_populations)
}
# data processing ---------------------------------------------------------

reformat_bed_availability_data <- function(filepath, bed_type) {
  
  if (grepl("xlsx$", filepath)) {
    tidy_data <- tidyxl::xlsx_cells(
      path = filepath,
      sheets = "NHS Trust by Sector"
    ) |> 
      filter(
        row > 13,
        !(row %in% 16:17),
        col > 1,
        !(col %in% c(12, 18)),
        !is.na(content)
      ) |> 
      unpivotr::behead(
        direction = "up-left",
        name = "availability_metric"
      ) |> 
      unpivotr::behead(
        direction = "left",
        name = "year"
      ) |> 
      unpivotr::behead(
        direction = "left",
        name = "period"
      ) |> 
      unpivotr::behead(
        direction = "left",
        name = "at_code"
      ) |> 
      unpivotr::behead(
        direction = "left",
        name = "org"
      ) |> 
      unpivotr::behead(
        direction = "left",
        name = "org_name"
      ) |> 
      unpivotr::behead(
        direction = "up",
        name = "category"
      ) |> 
      mutate(
        quarter = purrr::map_int(
          period,
          quarter_from_month_string
        ),
        year = purrr::map_int(
          year,
          year_from_financial_year
        ),
        metric = paste(
          "bed availability",
          bed_type,
          category,
          sep = " - "
        ),
        availability_metric = case_when(
          availability_metric == "Available" ~ "numerator",
          availability_metric == "Occupied" ~ "denominator",
          .default = "value"
        ),
        frequency = "quarterly"
      ) |> 
      select(
        "year",
        "quarter",
        "at_code",
        "org",
        "org_name",
        "metric",
        "availability_metric",
        "numeric",
        "frequency"
      ) |> 
      tidyr::pivot_wider(
        names_from = availability_metric,
        values_from = numeric
      )
  } else {
    tidy_data <- tibble::tibble(
      year = integer(), 
      quarter = integer(), 
      at_code = character(), 
      org = character(), 
      org_name = character(), 
      metric = character(), 
      numerator = numeric(), 
      denominator = numeric(), 
      value = numeric(),
      frequency = character()
    )
  }
  
  
  return(tidy_data)
}

aggregate_nctr_to_month <- function(daily_nctr) {
  monthly_nctr <- daily_nctr |> 
    mutate(
      month = lubridate::month(date),
      year = lubridate::year(date)
    ) |> 
    summarise(
      across(
        c(
          numerator, denominator
        ),
        mean
      ),
      .by = c(year, month, org, org_name, metric)
    ) |> 
    mutate(
      value = numerator / denominator,
      metric = "Average daily proportion of patients who no longer meet the criteria to reside that remain in hospital",
      frequency = "monthly"
    )
  
  return(monthly_nctr)
}

# reformatting code -------------------------------------------------------


# calculate quarter number from month string, where month is the final month in
# the quarter (eg, June = 1)
quarter_from_month_string <- function(month_string) {
  if (month_string %in% c("April", "May", "June")) {
    quarter <- 1L
  } else if (month_string %in% c("July", "August", "September")) {
    quarter <- 2L
  } else if (month_string %in% c("October", "November", "December")) {
    quarter <- 3L
  } else if (month_string %in% c("January", "February", "March")) {
    quarter <- 4L
  }
  return(quarter)
}

# calculate 4 digit year from financial year input. Currently accepts the format
# "####-##"
year_from_financial_year <- function(fyear) {
  if (grepl("^[0-9]{4}-[0-9]{2}$", fyear)) {
    year <- substr(
      fyear, 1, 4
    )
  } else {
    
    year <- -999
  }
  
  return(as.integer(year))
}

#' @description This function is for converting ons "E" codes to "Health" codes.
#'   Note, for STPs, there were boundary changes prior to 2020, which means that
#'   earlier STPs don't map exactly to ICBs
#' @param data tibble with a field called org, which is the field containing
#'   "E%" values that need converting to "A%" values
#' @param area_type string; one of "stp", "icb" or "ccg". The area type that the
#'   E values refer to (that need converting)
#' @param latest_codes_used logical; data that is publised monthly as it is
#'   created will use the latest codes at the time of publishing, whereas some
#'   datasets that are published retrospectively, will use up to date area
#'   codes. This argument should be TRUE in the latter example (only works for
#'   "stp" and "icb" currently)
#' @retain_fields string; field names of any fields to retain during this step
convert_ons_to_health_code <- function(data, area_type = "stp", latest_codes_used, retain_fields = c("month", "quarter")) {
  
  # some datasets move from ONS codes (eg, E%) to Health codes (eg, Q%) within
  # the data set
  data_q <- data |> 
    filter(
      grepl("^Q", org)
    )
  
  data <- data |> 
    filter(
      grepl("^E", org)
    )
  
  area_type <- match.arg(area_type, c("stp", "ccg", "icb"))
  
  if (area_type == "ccg") {
    url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/CCG21_STP21_EN_LU_1ce1f924d3dd44eca0797b516db80280/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
    ccg_ons_code_lkp <- jsonlite::fromJSON(
      txt = url
    ) |> 
      pluck(
        "features",
        "properties"
      ) |> 
      select(
        "CCG21CD",
        "STP21CDH"
      )
    
    data <- data |> 
      mutate(
        CCG21CD = case_when(
          grepl("^E", org) ~ org,
          .default = NA_character_
        )
      ) |> 
      left_join(
        ccg_ons_code_lkp,
        by = join_by(CCG21CD)
      ) |> 
      mutate(
        org = case_when(
          !is.na(STP21CDH) ~ STP21CDH,
          .default = org
        )
      ) |> 
      select(
        !c("CCG21CD", "STP21CDH")
      )
  } else if (area_type %in% c("icb", "stp")) {
    
    if (latest_codes_used == TRUE) {
      post_2022 <- data
      between_2017_2021 <- data |> 
        filter(org == "junk")
      pre_2017 <- data |>
        filter(org == "junk")
    } else {
      
      between_2017_2021 <- data |> 
        filter(year %in% 2017:2021)
      
      pre_2017 <- data |> 
        filter(year < 2017)
      
      if (nrow(pre_2017) > 0 | nrow(between_2017_2021) > 0) {
        weighted_pops <- lsoa_stp_icb_weighted_pops()
      }
      
      incorrect_2017_2021_orgs <- between_2017_2021 |> 
        filter(!(org %in% weighted_pops$STPCD))
      
      incorrect_pre2017_orgs <- pre_2017 |> 
        filter(!(org %in% weighted_pops$STPCD))
      
      post_2022 <- data |> 
        filter(year >= 2022) |> 
        bind_rows(
          incorrect_2017_2021_orgs,
          incorrect_pre2017_orgs
        )
      
    }
    
    if (nrow(post_2022) > 0) {
      icb_lkp <- check_and_download(
        filepath = "data-raw/Lookups/ICB22_name_lookup.xlsx",
        url = "https://www.arcgis.com/sharing/rest/content/items/25ba241a775e4a9db8e5c721ee73d85d/data"
      )
      
      icb22_ons_code_lkp <- readxl::read_excel(icb_lkp) |> 
        select(
          c(
            ICBCD = "ICB22CD", 
            ICBCDH = "ICB22CDH")
        )
      
      url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/ICB_APR_2023_EN_NC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
      icb_ons_code_lkp <- jsonlite::fromJSON(
        txt = url
      ) |> 
        pluck(
          "features",
          "properties"
        ) |> 
        select(
          ICBCD = "ICB23CD",
          ICBCDH = "ICB23CDH"
        ) |> 
        union(
          icb22_ons_code_lkp
        )
      
      post_2022 <- post_2022 |> 
        mutate(
          ICBCD = case_when(
            grepl("^E", org) ~ org,
            .default = NA_character_
          )
        ) |> 
        left_join(
          icb_ons_code_lkp,
          by = join_by(ICBCD)
        ) |> 
        mutate(
          org = case_when(
            !is.na(ICBCDH) ~ ICBCDH,
            .default = org
          )
        ) |> 
        select(
          !c("ICBCD", "ICBCDH")
        )
    }
    
    
    if (nrow(pre_2017) > 0) {
      years_required <- unique(pre_2017$year)
      
      pre_2017_weighted_pops <- weighted_pops |> 
        filter(
          year == min(year)
        ) |> 
        select(!c("year")) |> 
        cross_join(
          tibble(
            year = years_required
          )
        )
      
      pre_2017 <- pre_2017 |> 
        inner_join(
          pre_2017_weighted_pops,
          by = join_by(
            year,
            org == STPCD
          ),
          relationship = "many-to-many"
        ) |> 
        mutate(
          population = population / sum(population),
          .by = c(
            year, 
            any_of(c("month", "quarter")),
            metric, 
            org
          )
        ) |> 
        summarise(
          across(
            c(numerator, denominator),
            function(x) x * population
          ),
          .by = c(
            year, ICB22CDH, metric, frequency, any_of(retain_fields)
          )
        ) |> 
        mutate(
          value = numerator / denominator
        ) |> 
        rename(
          org = "ICB22CDH"
        )
      
    }
    
    if (nrow(between_2017_2021) > 0) {
      
      between_2017_2021 <- between_2017_2021 |> 
        inner_join(
          weighted_pops,
          by = join_by(
            year,
            org == STPCD
          ),
          relationship = "many-to-many"
        ) |> 
        mutate(
          population = population / sum(population),
          .by = c(
            year, 
            any_of(c("month", "quarter")),
            metric,
            org
          )
        ) |> 
        mutate(
          across(
            c(numerator, denominator),
            function(x) x * population
          )
        ) |> 
        summarise(
          across(
            c(numerator, denominator),
            sum
          ),
          .by = c(
            year, ICB22CDH, metric, frequency, any_of(retain_fields)
          )
        ) |> 
        mutate(
          value = numerator / denominator
        ) |> 
        rename(
          org = "ICB22CDH"
        )
    }
    
    data <- bind_rows(
      pre_2017,
      between_2017_2021,
      post_2022,
      data_q
    )
  }
  
  return(data)
}

clean_names <- function(data) {
  data_names <- names(data) |> 
    (\(x) gsub("^X...", "", x))()
  
  names(data) <- data_names
  return(data)
}

# aggregation tasks -------------------------------------------------------

# takes a mean of numerator and denominator for each month in the quarter and
# calculates a new value based on that. Requires the columns year, month, org,
# org_name, metric, numerator and denominator
monthly_to_quarterly_mean <- function(data) {
  data <- data |> 
    mutate(
      quarter = case_when(
        month %in% 1:3 ~ 4L,
        month %in% 4:6 ~ 1L,
        month %in% 7:9 ~ 2L,
        month %in% 10:12 ~ 3L,
        .default = NA_real_
      )
    ) |>
    summarise(
      across(
        c(numerator, denominator),
        ~ mean(.x, na.rm = TRUE)
      ),
      .by = any_of(
        c("year", "quarter", "org", "org_name", "metric")
      )
    ) |> 
    mutate(
      value = numerator / denominator,
      frequency = "quarterly"
    )
  
  return(data)
}

# takes a mean of numerator and denominator for each month in the quarter and
# calculates a new value based on that. Requires the columns year, month, org,
# org_name, metric, numerator and denominator
monthly_to_quarterly_sum <- function(data) {
  data <- data |> 
    mutate(
      quarter = case_when(
        month %in% 1:3 ~ 4L,
        month %in% 4:6 ~ 1L,
        month %in% 7:9 ~ 2L,
        month %in% 10:12 ~ 3L,
        .default = NA_real_
      )
    ) |>
    summarise(
      across(
        c(numerator, denominator),
        ~ sum(.x, na.rm = TRUE)
      ),
      .by = any_of(
        c("year", "quarter", "org", "org_name", "metric")
      )
    ) |> 
    mutate(
      value = numerator / denominator,
      frequency = "quarterly"
    )
  
  return(data)
}

# takes a mean of numerator and denominator for each month in the year and
# calculates a new value based on that. Requires the columns year, month, org,
# org_name, metric, numerator and denominator
monthly_to_annual_mean <- function(data, year_type = "financial") {
  
  year_type <- match.arg(year_type, c("financial", "calendar"))
  
  if (year_type == "financial") {
    data <- data |> 
      mutate(
        year = case_when(
          month %in% 1:3 ~ year - 1,
          .default = year
        )
      )
  }
  
  data <- data |> 
    summarise(
      across(
        c(numerator, denominator),
        ~ mean(.x, na.rm = TRUE)
      ),
      .by = any_of(
        c("year", "org", "org_name", "metric")
      )
    ) |> 
    mutate(
      value = numerator / denominator,
      frequency = paste(
        "annual",
        year_type
      )
    )
  
  return(data)
}

# takes a mean of numerator and denominator for each month in the year and
# calculates a new value based on that. Requires the columns year, month, org,
# org_name, metric, numerator and denominator
monthly_to_annual_sum <- function(data, year_type = "financial") {
  year_type <- match.arg(year_type, c("financial", "calendar"))
  
  if (year_type == "financial") {
    data <- data |> 
      mutate(
        year = case_when(
          month %in% 1:3 ~ year - 1,
          .default = year
        )
      )
  }
  
  data <- data |> 
    summarise(
      across(
        c(numerator, denominator),
        ~ sum(.x, na.rm = TRUE)
      ),
      .by = any_of(
        c("year", "org", "org_name", "metric")
      )
    ) |> 
    mutate(
      value = numerator / denominator,
      frequency = paste(
        "annual",
        year_type
      )
    )
  
  return(data)
}

# takes a mean of numerator and denominator for each quarter in the year and
# calculates a new value based on that. Requires the columns year, month, org,
# org_name, metric, numerator and denominator
quarterly_to_annual_mean <- function(data, year_type) {
  year_type <- match.arg(year_type, c("financial", "calendar"))
  
  if (year_type == "financial") {
    data <- data |> 
      mutate(
        year = case_when(
          quarter == 1 ~ year - 1,
          .default = year
        )
      )
  }
  
  data <- data |> 
    summarise(
      across(
        c(numerator, denominator),
        ~ mean(.x, na.rm = TRUE)
      ),
      .by = any_of(
        c("year", "org", "org_name", "metric")
      )
    ) |> 
    mutate(
      value = numerator / denominator,
      frequency = paste(
        "annual",
        year_type
      )
    )
  
  return(data)
}

# takes a sum of numerator and denominator for each quarter in the year and
# calculates a new value based on that. Requires the columns year, month, org,
# org_name, metric, numerator and denominator
quarterly_to_annual_sum <- function(data, year_type) {
  year_type <- match.arg(year_type, c("financial", "calendar"))
  
  if (year_type == "financial") {
    data <- data |> 
      mutate(
        year = case_when(
          quarter == 1 ~ year - 1,
          .default = year
        )
      )
  }
  
  data <- data |> 
    summarise(
      across(
        c(numerator, denominator),
        ~ sum(.x, na.rm = TRUE)
      ),
      .by = any_of(
        c("year", "org", "org_name", "metric")
      )
    ) |> 
    mutate(
      value = numerator / denominator,
      frequency = paste(
        "annual",
        year_type
      )
    )
  
  return(data)
}



# lookups -----------------------------------------------------------------
# scrapes table from NHS Shared Business services. 
ccg_to_icb <- function() {
  url <- "https://www.sbs.nhs.uk/ccg-icb-list"
  ccg_to_icb <- url |> 
    rvest::read_html() |> 
    rvest::html_nodes("tbody") |> 
    rvest::html_table(
      header = FALSE
    ) |> 
    purrr::pluck(1) |> 
    purrr::set_names(
      c("ccg_code", "ccg_name", "icb_code", "icb_name")
    )
  
  return(ccg_to_icb)
}

# provides all available information from the Organsiational Data Services API
# based on provided health code
ods_info <- function(health_org_code) {
  
  url <- paste0(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/",
    health_org_code
  )
  
  httpResponse <- GET(url, accept_json())
  ods_information <- jsonlite::fromJSON(
    content(
      httpResponse, 
      "text", 
      encoding="UTF-8"
    )
  )
  return(ods_information)
}


ods_lookup <- function(health_org_code, table1, table2, filter_category) {
  if (is.na(health_org_code)) return(NA)
  
  ods_information <- ods_info(health_org_code)
  
  
  lkp <- purrr::pluck(
    ods_information,
    "Organisation",
    table1,
    table2
  )
  
  if (!is.null(lkp)) {
    if (filter_category == "Active") {
      lkp <- lkp |> 
        filter(
          Status == "Active"
        )
    } else if (filter_category == "Successor") {
      lkp <- lkp |> 
        filter(
          Type == "Successor"
        )
    }
    
    if (nrow(lkp) > 0) {
      lkp <- lkp |> 
        tibble() |> 
        unnest(cols = Target) |> 
        unnest(cols = OrgId) |> 
        pull(extension)
    } else {
      lkp <- NA
    }
    
  } else {
    lkp <- NA
  }
  
}


# identifies active parent organisations for health org code provided
health_org_lookup <- function(health_org_code) {
  lkp <- ods_lookup(
    health_org_code,
    table1 = "Rels",
    table2 = "Rel",
    filter_category = "Active"
  )
  
  return(lkp)
}

# identifies successor organisations for health org code provided
health_org_successors <- function(health_org_code) {
  lkp <- ods_lookup(
    health_org_code,
    table1 = "Succs",
    table2 = "Succ",
    filter_category = "Successor"
  )
  
  return(lkp)
}

health_org_role <- function(health_org_code) {
  role <- ods_info(health_org_code) |> 
    pluck(
      "Organisation",
      "Roles",
      "Role"
    )
  
  if (is.null(role)) {
    role <- NA
  } else {
    role <- role |> 
      filter(primaryRole == TRUE) |> 
      pull(id)
  }
  
  return(role)
}

#' returns icb22 code from health org code by retrieving the post code of the
#' health org from the ods API, and looking up the LSOA code using the
#' postcodes.io API, then using the LSOA to ICB lookup from the open geography
#' portal
org_to_icb_postcode_lookup_method <- function(health_org_code) {
  
  filepath <- check_and_download(
    filepath = "data-raw/Lookups/lsoa_icb.xlsx",
    url = "https://www.arcgis.com/sharing/rest/content/items/1ac8547e9a8945478f0b5ea7ffe1a6b1/data"
  )
  
  lsoa_icb <- readxl::read_excel(
    filepath,
    sheet = "LSOA11_LOC22_ICB22_LAD22"
  ) |> 
    select(LSOA11CD, ICB22CDH)
  
  site_postcode <- ods_info(health_org_code) |> 
    pluck(
      "Organisation",
      "GeoLoc",
      "Location",
      "PostCode"
    )
  
  validate_postcodes <- function(postcode) {
    valid_postcodes <- tryCatch({
      lkp <- PostcodesioR::postcode_lookup(postcode)
      postcode
    },
    warning = function(w) {
      new_postcode <- PostcodesioR::postcode_autocomplete(
        substr(
          postcode,
          1, nchar(postcode) - 1
        ), limit = 1) |> 
        pull(postcode)
      
      return(new_postcode)
    },
    error = function(e) {
      print("this had an error")
      return(NULL)
    }
    )
  }
  
  
  icb_code <- site_postcode |> 
    validate_postcodes() |> 
    PostcodesioR::postcode_lookup() |> 
    select(lsoa_code) |> 
    left_join(
      lsoa_icb,
      by = join_by(
        lsoa_code == LSOA11CD
      )
    ) |> 
    pull(ICB22CDH)
  
  return(icb_code)
}

#' @description provides table of health org and ICB code. 
#' @details ICB codes are identified from the Shared Business services website.
#' Then, any successor organisations to the codes provided are identified from the ODS API. 
#' Sometimes an organisation can be divided into multiple organisations, 
#' so these are all included. Then the ODS API is used to identify active 
#' "relative" organisations - and these organisations are cross checked with the 
#' ICB codes previously obtained. Finally, where ICBs are not yet identified, 
#' the post code for the organisation is retrieve from the ODS API, this is 
#' used to identify the LSOA using the Postcodes.io API, which is then used to 
#' identify ICB22 using the Open Geography Portal.
attach_icb_to_org <- function(health_org) {
  
  icb_codes <- ccg_to_icb() |> 
    pull(icb_code) |> 
    unique()
  
  lkp <- tibble(
    health_org_code = health_org
  ) |> 
    mutate(
      successor_code = purrr::map(
        health_org_code,
        health_org_successors
      )
    ) |> 
    tidyr::unnest(
      cols = successor_code
    ) |> 
    mutate(
      code_for_lkp = case_when(
        is.na(successor_code) ~ health_org_code,
        .default = successor_code
      ),
      parent_code = purrr::map(
        code_for_lkp,
        health_org_lookup
      )
    ) |> 
    tidyr::unnest(
      cols = parent_code
    ) |> 
    distinct() |> 
    mutate(
      icb_code = case_when(
        health_org_code %in% icb_codes ~ health_org_code,
        parent_code %in% icb_codes ~ parent_code,
        .default = NA_character_
      )
    ) |> 
    mutate(
      retain = sum(!is.na(icb_code)),
      .by = health_org_code
    ) |> 
    select(!c("successor_code", "code_for_lkp")) |> 
    distinct() |> 
    anti_join(
      tibble(
        icb_code = NA,
        retain = 1
      ),
      by = join_by(icb_code, retain)
    ) |> 
    select(!c("retain")) |> 
    mutate(
      icb_code = map2_chr(
        .x  = icb_code,
        .y = health_org_code,
        .f = function(x, y) if (is.na(x)) {
          org_to_icb_postcode_lookup_method(y)
        } else {
          x
        }
      )
    ) |> 
    select(
      health_org_code, icb_code
    )
  return(lkp)
}

lsoa_utla_icb_weighted_pops <- function() {
  # LSOA11 to UTLA lookups
  urls <- c(
    `2016` = "https://opendata.arcgis.com/api/v3/datasets/2f18c0488b514dd9b1369345a91a196c_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
    `2017` = "https://opendata.arcgis.com/api/v3/datasets/2f18c0488b514dd9b1369345a91a196c_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
    `2018` = "https://opendata.arcgis.com/api/v3/datasets/2f18c0488b514dd9b1369345a91a196c_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
    `2019` = "https://opendata.arcgis.com/api/v3/datasets/6f5221e8123a480883874849ddf5cbd8_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
    `2020` = "https://opendata.arcgis.com/api/v3/datasets/77ade1327615430eb4bc5dadb8bbeafa_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
    `2021` = "https://opendata.arcgis.com/api/v3/datasets/24322a71f0f54446bcdb406b54e42956_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1"
  )
  
  file <- paste0(
    "data-raw/Lookups/LSOA11_UTLA", 
    names(urls), 
    ".csv"
    )
  
  if (any(!file.exists(file))) {
    file <- purrr::lmap(
      urls, 
      ~ list(
        download_url_to_directory(
          url = .x,
          new_directory = "Lookups",
          filename = paste0(
            "LSOA11_UTLA",
            names(.x),
            ".csv"
          )
        )
      )
    )
  }
  
  names(file) <- names(urls)
  
  process_lsoa_lkp_file <- function(filepath) {
    lkp <- read.csv(filepath) |> 
      filter(
        grepl("^E", LSOA11CD)
      ) |> 
      select(
        "LSOA11CD", starts_with("UTLA")
      ) |> 
      select(
        ends_with("CD")
      ) |> 
      rename(
        UTLACD = starts_with("UTLA")
      )
  }
  
  lkp <- purrr::map_df(
    file,
    process_lsoa_lkp_file,
    .id = "year"
  ) |> 
    mutate(year = as.integer(year))
  
  
  
  # create all ages population of LSOA11 to UTLA by year
  # Note, missing 2021 and 2022 populations
  pops <- list.files("data-raw/Population/",
                     full.names = TRUE) |> 
    set_names(
      nm = function(x) str_extract(x, "[0-9]{4}")
    )
  
  pops <- pops[names(pops) %in% names(urls)] |> 
    purrr::map_df(
      lsoa_populations,
      .id = "year"
    ) |> 
    filter(
      grepl("^E", LSOA11CD)
    ) |> 
    rename(
      population = "All Ages"
    ) |> 
    mutate(
      year = as.integer(year)
    )
  
  
  # 2021 pops from the census via the ONS census API
  ons_end_point <- "https://api.beta.ons.gov.uk/v1/"
  
  lsoa21_pops <- jsonlite::read_json(
    paste0(
      ons_end_point,
      "population-types/UR/census-observations?area-type=lsoa&dimensions=sex"
    )
  ) |> 
    pluck("observations") |> 
    map_df(
      ~ data.frame(
        LSOA21CD = pluck(.x, 1, 1, "option_id"),
        sex = pluck(.x, 1, 2, "option"),
        population = pluck(.x, "observation")
      )
    ) |> 
    summarise(
      population = sum(population),
      .by = LSOA21CD
    ) |> 
    mutate(
      year = 2021
    ) |> 
    filter(grepl("^E", LSOA21CD))
  
  lsoa11_pops_year_2021 <- get_lsoa21_pops_with_lsoa11_codes()
  
  pops <- bind_rows(
    pops,
    lsoa11_pops_year_2021
  )
  
  lsoa_icb_lkp <- readxl::read_excel(
    "data-raw/Lookups/lsoa_icb.xlsx"
  ) |> 
    select(
      "LSOA11CD",
      "ICB22CDH"
    )
  
  lkp <- lkp |> 
    left_join(
      pops,
      by = join_by(
        year,
        LSOA11CD
      )
    ) |> 
    left_join(
      lsoa_icb_lkp,
      by = join_by(
        LSOA11CD
      )
    )
  
  return(lkp)
}


lsoa_stp_icb_weighted_pops <- function() {
  # LSOA11 to STP lookups
  urls <- c(
    `2017` = "https://opendata.arcgis.com/api/v3/datasets/d02e4a70dc594ffc9438b2f3d73988a2_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
    `2018` = "https://opendata.arcgis.com/api/v3/datasets/30b31100dc994d9898f7c2bb77de4a25_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
    `2019` = "https://opendata.arcgis.com/api/v3/datasets/122bd531f25b46fcb5e9c292bd8d6fe0_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
    `2020` = "https://opendata.arcgis.com/api/v3/datasets/bb40c565f684400c9f01f7abdc26dd63_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
    `2021` = "https://opendata.arcgis.com/api/v3/datasets/60c96f6f776a4059b3ef23af3290a9fc_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1"
  )
  
  file <- paste0(
    "data-raw/Lookups/LSOA11_STP", 
    names(urls), 
    ".csv"
  )
  
  if (any(!file.exists(file))) {
    file <- purrr::lmap(
      urls, 
      ~ list(
        download_url_to_directory(
          url = .x,
          new_directory = "Lookups",
          filename = paste0(
            "LSOA11_STP",
            names(.x),
            ".csv"
          )
        )
      )
    )
  }
  
  names(file) <- names(urls)
  
  process_lsoa_lkp_file <- function(filepath) {
    lkp <- read.csv(filepath) |> 
      clean_names() |> 
      filter(
        grepl("^E", LSOA11CD)
      ) |> 
      select(
        "LSOA11CD", starts_with("STP")
      ) |> 
      select(
        ends_with("CD")
      ) |> 
      rename(
        STPCD = starts_with("STP")
      ) |> 
      distinct()
  }
  
  lkp <- purrr::map_df(
    file,
    process_lsoa_lkp_file,
    .id = "year"
  ) |> 
    mutate(year = as.integer(year))
  
  
  
  # create all ages population of LSOA11 to UTLA by year
  # Note, missing 2021 and 2022 populations
  pops <- list.files("data-raw/Population/",
                     full.names = TRUE) |> 
    set_names(
      nm = function(x) str_extract(x, "[0-9]{4}")
    )
  
  pops <- pops[names(pops) %in% names(urls)] |> 
    purrr::map_df(
      lsoa_populations,
      .id = "year"
    ) |> 
    filter(
      grepl("^E", LSOA11CD)
    ) |> 
    rename(
      population = "All Ages"
    ) |> 
    mutate(
      year = as.integer(year)
    )
  
  lsoa11_pops_year_2021 <- get_lsoa21_pops_with_lsoa11_codes()
  
  pops <- bind_rows(
    pops,
    lsoa11_pops_year_2021
  )
  
  lsoa_icb_lkp <- readxl::read_excel(
    "data-raw/Lookups/lsoa_icb.xlsx"
  ) |> 
    select(
      "LSOA11CD",
      "ICB22CDH"
    )
  
  lkp <- lkp |> 
    left_join(
      pops,
      by = join_by(
        year,
        LSOA11CD
      )
    ) |> 
    left_join(
      lsoa_icb_lkp,
      by = join_by(
        LSOA11CD
      )
    )
  
  return(lkp)
}


get_lsoa21_pops_with_lsoa11_codes <- function() {
  # 2021 pops from the census via the ONS census API
  ons_end_point <- "https://api.beta.ons.gov.uk/v1/"
  
  lsoa21_pops <- jsonlite::read_json(
    paste0(
      ons_end_point,
      "population-types/UR/census-observations?area-type=lsoa&dimensions=sex"
    )
  ) |> 
    pluck("observations") |> 
    map_df(
      ~ data.frame(
        LSOA21CD = pluck(.x, 1, 1, "option_id"),
        sex = pluck(.x, 1, 2, "option"),
        population = pluck(.x, "observation")
      )
    ) |> 
    summarise(
      population = sum(population),
      .by = LSOA21CD
    ) |> 
    mutate(
      year = 2021
    ) |> 
    filter(grepl("^E", LSOA21CD))
  
  lsoa11_pops_year_2021 <- check_and_download(
    filepath = "data-raw/Lookups/lsoa11_lsoa21.csv",
    url = "https://opendata.arcgis.com/api/v3/datasets/e99a92fb7607495689f2eeeab8108fd6_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1"
  )
  
  lsoa11_pops_year_2021 <- read.csv(
    lsoa11_pops_year_2021
  ) |> 
    clean_names() |> 
    filter(
      grepl("^E", LSOA11CD)
    ) |> 
    select(
      "LSOA11CD",
      "LSOA21CD"
    ) |> 
    left_join(
      lsoa21_pops,
      by = join_by(LSOA21CD)
    ) |> 
    mutate(
      # divide the population equally between LSOAs the LSOAs have merged (eg, 2
      # LSOA11 merged into 1 LSOA21) or there have been irregular boundary changes
      # (eg, 2 LSOA11s have turned into 2 new LSOA11s)
      population = population / n(),
      .by = LSOA21CD
    ) |> 
    summarise(
      population = sum(population),
      .by = c(
        LSOA11CD, year
      )
    )
  
  return(lsoa11_pops_year_2021)
}
