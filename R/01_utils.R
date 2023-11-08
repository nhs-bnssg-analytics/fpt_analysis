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
      APPT_STATUS == "Attended"
    ) |> 
    summarise(
      COUNT_OF_APPOINTMENTS = sum(COUNT_OF_APPOINTMENTS),
      .by = c(
        ICB_STP_ONS_CODE,
        TIME_BETWEEN_BOOK_AND_APPT
      )
    )
  
  
  write.csv(
    data,
    paste0(
      file_directory,
      stringr::str_extract(
        zipped_csv_file, 
        "[[:alpha:]]{3}_[0-9]{2}.csv"
      )
    ),
    row.names = FALSE
  )
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
      metric != "Number of patients discharged"
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
  
  filename <- gsub(
    "Daily-discharge-sitrep-monthly-data-webfile-",
    "",
    basename(filepath)
  )
  
  filename <- stringr::str_extract(
    filename,
    "[[:alpha:]].*[0-9]{4}"
  )
  
  filename <- paste0(
    dirname(filepath),
    "/",
    filename,
    ".csv"
  )
  
  write.csv(
    nctr,
    filename,
    row.names = FALSE
  )
  
  return(filename)
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
      grepl("^E", org)
    )
  
  return(tidy_sc_funding)
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

aggregate_nctr_to_month <- function(filepath) {
  monthly_nctr <- read.csv(filepath) |> 
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

# This function is for converting ons "E" codes to "Health" codes. Note, for
# STPs, there were boundary changes prior to 2020, which means that earlier STPs
# don't map exactly to ICBs
convert_ons_to_health_code <- function(data, area_type = "stp") {
  
  area_type <- match.arg(area_type, c("stp", "ccg", "icb"))
  
  if (area_type == "stp") {
    stp_ons_code_lkp <- download_url_to_directory(
      url = "https://www.arcgis.com/sharing/rest/content/items/bec635f6c83e4582bcf76ce02c2be840/data",
      new_directory = "Lookups",
      filename = "STP21_name_lookup.xlsx"
    ) |> 
      readxl::read_excel() |> 
      select(
        c("STP21CD", "STP21CDH")
      )
    
    data <- data |> 
      mutate(
        STP21CD = case_when(
          grepl("^E", org) ~ org,
          .default = NA_character_
        )
      ) |> 
      left_join(
        stp_ons_code_lkp,
        by = join_by(STP21CD)
      ) |> 
      mutate(
        org = case_when(
          !is.na(STP21CDH) ~ STP21CDH,
          .default = org
        )
      ) |> 
      select(
        !c("STP21CD", "STP21CDH")
      )
  } else if (area_type == "ccg") {
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
  } else if (area_type == "icb") {
    icb22_ons_code_lkp <- download_url_to_directory(
      url = "https://www.arcgis.com/sharing/rest/content/items/25ba241a775e4a9db8e5c721ee73d85d/data",
      new_directory = "Lookups",
      filename = "ICB22_name_lookup.xlsx"
    ) |> 
      readxl::read_excel() |> 
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
    
    data <- data |> 
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
  
  filepath <- "data-raw/Lookups/lsoa_icb.xlsx"
  
  if (!file.exists(filepath)) {
    lsoa_icb <- download_url_to_directory(
      "https://www.arcgis.com/sharing/rest/content/items/1ac8547e9a8945478f0b5ea7ffe1a6b1/data",
      new_directory = "Lookups",
      filename = "lsoa_icb.xlsx"
    ) 
  }
  
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
