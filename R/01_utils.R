# url processing ----------------------------------------------------------

obtain_links <- function(url, include_link_text = FALSE) {
  url_html <- xml2::read_html(url)
  
  links <- url_html |> 
    rvest::html_nodes("a") |> 
    rvest::html_attr("href")
  
  if (include_link_text) {
    link_text <- url_html |> 
      rvest::html_nodes("a") %>%
      rvest::html_text() |> 
      str_squish()
    
    links <- set_names(
      links,
      nm = link_text
    )
  }
  
  return(links)
}

obtain_links_and_text <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  urls <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  links <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  
  links <- rlang::set_names(
    urls,
    nm = links
  )
  return(links)
}
# downloading files -------------------------------------------------------

download_url_to_directory <- function(url, new_directory, filename) {
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
      new_directory = paste0(dirname(filepath), "/"),
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

#' provided with a url of a zip file, a directory to unzip it to, and a pattern
#' to identify the contents of the file to unzip, this function will unzip the
#' files determined by the specified pattern from the zip file obtained from the
#' url. #' @param add_filename_prefix logical; if true, the unzipped file(s)
#' will be prefixed with the .zip file filename. This is useful when there are
#' different zip files for different periods, but the contents all have the same
#' file name, and so would naturally write over one another
download_unzip_files <- function(zip_url, directory, zip_file_pattern, add_filename_prefix = FALSE) {
  if (!isTRUE(file.info(directory)$isdir)) 
    dir.create(directory, recursive = TRUE)
  
  temp <- tempfile()
  
  download.file(zip_url,
                temp)
  
  zipped_files <- unzip(
    zipfile = temp, 
    list = TRUE
  ) |> 
    filter(
      grepl(zip_file_pattern, Name)
    ) |> 
    pull(Name)
  
  # remove files that already exist in the directory and are in the zip file
  list.files(directory, full.names = TRUE) |> 
    dplyr::intersect(zipped_files) |> 
    file.remove() |> 
    invisible()
  
  
  if (length(zipped_files) > 0) {
    extracted_files <- unzip(
      zipfile = temp,
      files = zipped_files,
      exdir = directory
    )
    if (isTRUE(add_filename_prefix)) {
      zip_filename <- gsub("\\.zip", "", basename(zip_url))
      
      new_file_names <- paste0(
        dirname(extracted_files),
        "/",
        zip_filename,
        "_",
        basename(extracted_files)
      )
      rename_files <- file.rename(
        from = extracted_files,
        to = new_file_names
      )
      
      extracted_files <- new_file_names
    }
  } else {
    extracted_files <- zipped_files
  }
  
  
  unlink(temp)
  
  return(extracted_files)
}

download_unzip_gp_wait_times <- function(zip_url) {
  
  new_directory <- "data-raw/GP wait times/"
  
  zipped_files <- download_unzip_files(
    zip_url = zip_url,
    directory = new_directory,
    zip_file_pattern = "[[:alpha:]]{3}_[0-9]{2}.csv|[[:alpha:]]{3}[0-9]{2}.csv"
  )
  
  # unzip the new csvs and summarise them to icb/stp geography before saving
  # them in the data-raw folder
  files <- lapply(
    zipped_files,
    summarise_and_write_gp_wait_time_data
  )
  
  return(files)
}

summarise_and_write_gp_wait_time_data <- function(csv_file) {
  
  data <- read.csv(csv_file) |> 
    rename(
      any_of(
        c(
          SUB_ICB_CODE = "SUB_ICB_LOCATION_CODE",
          SUB_ICB_CODE = "CCG_CODE"
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
        SUB_ICB_CODE,
        TIME_BETWEEN_BOOK_AND_APPT
      )
    )
  
  if (nrow(data) > 0) {
    new_filename <- paste0(
      dirname(csv_file),
      "/",
      stringr::str_extract(
        csv_file, 
        "[[:alpha:]]{3}_[0-9]{2}.csv"
      )
    )
      
    new_filename <- gsub("/Daily", "", new_filename)
    
    if (!file.exists(new_filename)) {
      data |> 
        write.csv(
          new_filename,
          row.names = FALSE
        )
    }
    
  } else {
    new_filename <- character()
  }
  
  file.remove(csv_file) |> 
    invisible()
  
  return(new_filename)
}

unzip_file <- function(zip_filepath, filename_pattern) {
  temp <- tempfile()
  
  if (!missing(filename_pattern)) {
    zipped_files <- unzip(
      zipfile = zip_filepath, 
      list = TRUE
    ) |> 
      filter(
        grepl(filename_pattern, Name)
      ) |> 
      pull(Name)
    
    df <- read.csv(
      unz(
        zip_filepath, 
        zipped_files
        )
    )
  } else {
    zipped_files <- unzip(
      zipfile = zip_filepath, 
      list = TRUE
    ) |> 
      filter(
        grepl("csv$", Name)
      ) |> 
      pull(Name)
    
    df <- zipped_files |> 
      purrr::map(
        ~ read.csv(
          unz(
            zip_filepath,
            .x
          )
        )
      )
    
  }
  unlink(temp)
  return(df)
}

# tidy the qof xlsx files 
tidy_qof <- function(filepath) {
  yr <- stringr::str_extract(
    filepath,
    pattern = "[0-9]{4}-[0-9]{2}"
  )
  
  rename_csv_lkp_function <- function(df) {
    df <- df |> 
      rename(
        metric = any_of(c("INDICATOR_GROUP_CODE", "GROUP_CODE")),
        numerator = "REGISTER",
        denominator = any_of(c("PRACTICE_LIST_SIZE", "PATIENT_LIST_SIZE"))
        
      )
  }
  
  qof <- filepath |> 
    read.csv() |> 
    rename(
      metric = any_of(c("INDICATOR_GROUP_CODE", "GROUP_CODE")),
      numerator = "REGISTER",
      denominator = any_of(c("PRACTICE_LIST_SIZE", "PATIENT_LIST_SIZE"))
    )|>
    mutate(
      year = as.integer(
        substr(yr, 1, 4)
      ),
      metric = case_when(
        metric == "AF" ~ "Atrial fibrillation: QOF prevalence (all ages)",
        metric == "DEP" ~ "Depression: QOF prevalence (18+ yrs)",
        metric == "LD" ~ "Learning disability: QOF prevalence (all ages)",
        metric == "MH" ~ "Mental Health: QOF prevalence (all ages)",
        metric == "OB" ~ "Obesity: QOF prevalence (18+ yrs)",
        metric == "SMOK" ~ "Smoking: QOF prevalence (15+ yrs)",
        metric == "HYP" ~ "Hypertension: QOF prevalence (all ages)",
        metric == "AST" ~ "Asthma: QOF prevalence (all ages)",
        metric == "CAN" ~ "Cancer: QOF prevalence (all ages)",
        metric == "CHD" ~ "CHD: QOF prevalence (all ages)",
        metric == "CKD" ~ "CKD: QOF prevalence (18+ yrs)",
        metric == "COPD" ~ "COPD: QOF prevalence (all ages)",
        metric == "DEM" ~ "Dementia: QOF prevalence (all ages)",
        metric == "DM" ~ "Diabetes mellitus: QOF prevalence (17+ yrs)",
        metric == "EP" ~ "Epilepsy: QOF prevalence (18+ yrs)",
        metric == "HF" ~ "Heart failure: QOF prevalence (all ages)",
        metric == "OST" ~ "Osteoporosis: QOF prevalence (50+ yrs)",
        metric == "PAD" ~ "Peripheral arterial disease: QOF prevalence (all ages)",
        metric == "PC" ~ "Palliative care: QOF prevalence (all ages)",
        metric == "RA" ~ "Rheumatoid arthritis: QOF prevalence (16+ yrs)",
        metric == "STIA" ~ "Stroke and transient ischaemic attack: QOF prevalence (all ages)",
        .default = NA_character_
      ),
      frequency = "annual financial",
      .keep = "unused"
    ) |> 
    filter(
      !is.na(metric),
      !is.na(numerator)
    )
  
  # asthma denominator changes from "all ages" to "6plus" in qof from 2020. Here
  # we obtain the total list size for each practice and replace the asthma
  # denominators with that to ensure the asthma metric is a consistent time
  # series
  
  total_list_size <- qof |> 
    filter(PATIENT_LIST_TYPE == "TOTAL") |> 
    distinct(
      year, PRACTICE_CODE, denominator
    ) |> 
    mutate(
      metric = "Asthma: QOF prevalence (all ages)",
      PATIENT_LIST_TYPE = "TOTAL"
    )
  
  qof <- qof |> 
    rows_update(
      total_list_size,
      by = c("year", "metric", "PRACTICE_CODE")
    )
  
  return(qof)
  
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
      grepl("^R", org)
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
      filter(
        !grepl("COMMISSIONING HUB|LHB|UNKNOWN", org_name)
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
        numerator = denominator - numerator, # forces high = bad
        value = numerator / denominator,
        frequency = "monthly",
        metric = "Proportion of suspected cancer or referral to first definitive treatment that are longer than 62 days wait"
      )
    
    return(tidied_sheet)
  }
  
  cancer_wait_times <- sheet_names |> 
    purrr::map_dfr(
      .f = ~ tidy_cancer_wait_time_sheet(
        filepath = filepath,
        sheet = .x
      )
    ) |> 
    filter(
      !grepl("COMMISSIONING|LHB", org_name, ignore.case = TRUE)
    )
  
  if (length(sheet_names) > 1) {
    cancer_wait_times <- cancer_wait_times |> 
      summarise(
        across(
          c(numerator, denominator),
          sum
        ),
        .by = c(
          year, month, org, org_name, frequency
        )
      ) |> 
      mutate(
        value = numerator / denominator,
        metric = "Proportion of suspected cancer or referral to first definitive treatment that are longer than 62 days wait"
      )
  }
  
  return(cancer_wait_times)
}

tidy_a_and_e <- function(filepath) {
  
  sheets <- readxl::excel_sheets(
    filepath
  ) |> 
    (function(x) x[grepl("Provider Level Data|A&E Data", x)])()
    # (function(x) x[grepl("STP Level Data|System Level Data", x)])()
  
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
  
  a_and_e_tidy <- readxl::read_excel(
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
        "Type 1 Departments - Major A&E"#,
        # "Type 2 Departments - Single Specialty",
        # "Type 3 Departments - Other A&E/Minor Injury Unit",
        # "Total attendances"
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
        "Proportion of A&E attendances with greater than 4 hours wait (",
        metric,
        ")"
      ),
      frequency = "monthly"
    ) |> 
    filter(
      !is.na(org),
      denominator != 0
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
  
  description <- "incomplete"
  
  filename <- paste0(
    dirname(filepath),
    "/",
    monthyear,
    "_",
    description,
    ".",
    extension
  )
  
  file.rename(
    from = filepath,
    to = filename
  )
  return(filename)
}

tidy_rtt <- function(filepath, num_weeks) {
  admission_type <- "incomplete"
  
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
  
  rtt <- rtt |> 
      behead(
        direction = "up",
        name = "headers"
      )
  
  if (use_sheet == "Commissioner") {
    rtt <- rtt |> 
      behead(
        direction = "left",
        name = "parent_code"
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
    mutate(
      headers = case_when(
        headers %in% paste0(">", 0:(num_weeks - 1), "-", 1:num_weeks) ~ "numerator",
        headers == "Total number of incomplete pathways" ~ "denominator",
        .default = NA_character_
      )
    ) |> 
    filter(
      headers %in% c(
        "numerator",
        "denominator"
      ),
      treatment_function == "Total"
    ) |> 
    summarise(
      chr = sum(as.numeric(chr)),
      .by = c(
        "org",
        "org_name",
        "headers"
      )
    ) |> 
    tidyr::pivot_wider(
      names_from = headers,
      values_from = chr
      # values_fn = as.numeric
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
      numerator = denominator - numerator, # forces bad = high
      value = numerator / denominator,
      metric = paste(
        "Proportion of incomplete pathways greater than",
        num_weeks,
        "weeks from referral"
      ),
      frequency = "monthly"
    ) |> 
    filter(
      !grepl("NHS ENGLAND|COMMISSIONING HUB|COMMISSIONING REGION", org_name)
    )
  
  return(rtt)
}

tidy_covid_beds <- function(filepath) {
  
  covid <- tidyxl::xlsx_cells(
    filepath,
    sheets = c(
      "Total Beds Occupied Covid",
      "Admissions Total",
      "Covid Absences"
      )
    )
  
  pub_date_row <- covid |> 
    filter(
      character == "Published:"
    ) |> 
    pull(row) |> 
    unique()
  
  pub_date <- covid |> 
    filter(
      row == pub_date_row,
      col == 3
    ) |> 
    distinct(
      date, character
    ) |> 
    mutate(
      pub_date = case_when(
        is.na(date) ~ lubridate::dmy(character),
        .default = date
      )) |> 
    pull()
  
  covid <- covid |> 
    filter(
      is_blank != TRUE,
      row >= 13
    ) |> 
    group_by(
      sheet
    ) |> 
    behead(
      direction = "left",
      name = "Region"
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
      name = "Date"
    ) |> 
    ungroup() |> 
    filter(
      !is.na(org)
    ) |> 
    select(
      "sheet",
      numerator = "numeric",
      "org", "org_name",
      "Date"
    ) |> 
    mutate(
      Date = as.Date(Date),
      year = lubridate::year(Date),
      month_name = lubridate::month(
        Date,
        label = TRUE,
        abbr = FALSE),
      quarter = purrr::map_int(
        month_name,
        quarter_from_month_string
      ),
      year = case_when(
        quarter == 4 ~ year - 1,
        .default = year
      ),
      published_date = pub_date
    )
  
  return(covid)
}

#' @param type "HC" or "FTE"
tidy_workforce_ftes <- function(type = "FTE") {
  
  type <- match.arg(type, c("HC", "FTE"))
  
  url <- "https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics"
  
  links <- obtain_links(url) |> 
    (\(x) x[grepl(paste(month.abb, collapse = "|"), x, ignore.case = TRUE)])() |> 
    (\(x) x[grepl("[0-9]{4}$", x)])() |> 
    head(1) |> 
    (\(x) paste0("https://digital.nhs.uk/", x))() |> 
    obtain_links() |> 
    (\(x) x[grepl("zip$", x)])() |> 
    (\(x) x[grepl("csv", x)])()
  
  
  file <- download_url_to_directory(
    url = links,
    new_directory = "data-raw/Clinical workforce/",
    filename = "Latest workforce statistics.zip"
  )
  
  operational_ftes <- unzip_file(
    zip_filepath = file,
    filename_pattern = "Organisation"
  ) |> 
    filter(
      Data.Type == type,
      # remove ambulance because FTEs only applied to one ICS in each region
      # remove PCT, ICB and CCG so as only to retain front line staff
      !(Cluster.Group %in% c("Ambulance" , "PCT",
                             "Clinical Commissioning Group", 
                             "Integrated Care Board")),
      Staff.Group != "Ambulance staff"
    ) |> 
    mutate(
      Date = as.Date(Date),
      month = lubridate::month(Date),
      year = lubridate::year(Date),
      months_from_july = abs(7 - month) # some years dont have july data
    ) |> 
    filter(
      year > 2013
    ) |> 
    filter(
      months_from_july == min(months_from_july),
      .by = year
    ) |> 
    mutate(
      month = 7,
      metric = paste(
        Staff.Group,
        Cluster.Group,
        sep = " - "
      )
    ) |>
    select(
      "year",
      "month",
      "ICS.code",
      org = "Org.Code",
      "metric",
      numerator = "Total"
    ) |> 
    mutate(
      org = case_when(
        grepl("-", org) ~ gsub("-.*", "", org),
        .default= org
      )
    )
  
  return(operational_ftes)
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
      ),
      numeric = case_when(
        character %in% c("*", "[c]") ~ 2.5,
        .default = numeric
      )
    ) |> 
    filter(
      type != "Not required",
      grepl("^E[0-9]{8}", org)
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

tidy_admissions <- function(filepath) {
  
  year_from_file = str_extract(
    filepath,
    pattern = "[0-9]{4}"
  ) |> 
    as.numeric()
  
  admissions <- filepath |> 
    tidyxl::xlsx_cells() |> 
    filter(
      grepl("Hospital", sheet)
    )
  
  min_details <- admissions |> 
    filter(
      grepl("Hospital provider code and description", character)
    )
  
  min_row <- min_details |> 
    pull(row)
  min_col <- min_details |> 
    pull(col)
  
  admissions <- admissions |> 
    filter(
      !is.na(content),
      row >= min_row,
      col >= min_col
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
      type == "Admissions",
      !grepl("^Y|Total|^Q", org),
      org != ""
    ) |> 
    mutate(
      numeric = case_when(
        character == "*" ~ 0,
        .default = numeric
      ),
      year = year_from_file,
      org = case_when(
        grepl("^R", org) & (nchar(org) == 5) ~ substr(org, 1, 3),
        .default = org
      )
    ) |> 
    summarise(
      denominator = sum(numeric),
      .by = c(
        year,
        org,
        org_name
      )
    )
  
  return(admissions)
}

tidy_mh_file <- function(filename) {
  
  if (grepl("xlsm$", filename)) {
    sh_nms <- excel_sheets(filename)
    
    sheet_data <- map(
      sh_nms,
      ~ readxl::read_excel(
        filename,
        sheet = .x,
      )
    ) |> 
      set_names(
        sh_nms
      )
  } else {
    read_xlsb_sheets <- function(sheet_index, filename) {
      tryCatch(
        {
          read_xlsb(
            filename,
            sheet = sheet_index,
            range = "A1:BZ500",
            col_names = FALSE
          )
        },
        error = function(cond) {
          NA
        }
      )
    }
    
    get_unique_id <- function(data) {
      unique_id1 <- data |> 
        select(1) |> 
        slice(2)
      
      unique_id2 <- data |> 
        select(2) |> 
        slice(2)
      
      unique_id <- paste(unique_id1, unique_id2)
      
      if (grepl("Unique ID:", unique_id))
        unique_id <- trimws(gsub("Unique ID:", "", unique_id))
      return(unique_id)
    }
    
    get_name <- function(data) {
      name1 <- data |> 
        select(1) |> 
        slice(1)
      
      name2 <- data |> 
        select(2) |> 
        slice(1)
      
      name <- paste(name1, name2)
      
      if (grepl("Title:", name)) {
        name <- trimws(gsub("Title:", "", name))
        if (name == "") {
          name <- "drop"
        } 
        
      } else {
        name <- "drop"
      }
      return(name)
    }
    
    sheet_data <- list()
    i <- 1
    continue <- TRUE
    while (isTRUE(continue)) {
      df <- read_xlsb_sheets(i, filename)
      # browser()
      if (class(df) != "data.frame") {
        continue <- FALSE
      } else {
        new_name <- get_name(df)
        if (new_name != "drop") {
          new_name <- setNames(
            "column.1",
            nm = new_name
          )
          uid <- get_unique_id(df)
          
          # if the unique ID is documented incorrectly in the spreadsheet; add
          # an x on the end
          if (uid %in% names(sheet_data)) {
            while(uid %in% names(sheet_data)) {
              uid <- paste0(uid, "x")
            }
          }
          df <- df |> 
            rename(
              all_of(new_name)
            )
          sheet_data[[uid]] <- df
        }
        i <- i + 1
      }
      
    }
    
  }
  
  # browser()
  file_spend_data <- sheet_data[grepl("\\(", names(sheet_data))] |> 
    map(tidy_mh_sheets) |> 
    list_rbind() |> 
    filter(
      grepl("spend", metric, ignore.case = TRUE),
      !grepl("planned", time_period)
    ) |> 
    select(!any_of(c("To delete", "Parent code"))) |> 
    mutate(
      time_period = gsub(paste0("Q", 1:4, " ", collapse = "|"), "", time_period),
      value = as.numeric(value)
    ) |> 
    summarise(
      across(
        c(value),
        sum
      ),
      .by = c(
        `Org code`, 
        `Org Type`,
        metric,
        time_period
      )
    ) |> 
    rename(
      year = "time_period"
    ) |> 
    filter(
      `Org Type` == "STP"
    ) |> 
    mutate(
      frequency = "annual financial",
      year = as.integer(
        substr(year, 1, 4)
      ),
      metric = case_when(
        grepl("2015/16", metric) ~ gsub("\\(2015/16\\)", "", metric),
        .default = paste(metric, "per population")
      ),
      metric = stringr::str_replace(
        metric,
        "CYP",
        "Children and Young People"
      ),
      metric = stringr::str_replace(
        metric,
        "EIP",
        "Early Intervention in Psychosis"
      ),
      metric = stringr::str_replace(
        metric,
        "IAPT",
        "Improving Access to Psychological Therapies"
      ),
      metric = stringr::str_replace(
        metric,
        "MH",
        "Mental Health"
      ),
      metric = stringr::str_replace(
        metric,
        "\\(k\\)",
        "(£k)"
      ),
      metric = stringr::str_remove(metric, " CCG"),
      metric = stringr::str_squish(metric)
    ) |> 
    select(
      org = "Org code",
      "metric",
      "year", 
      numerator = "value",
      "frequency"
    )
  
  return(file_spend_data)
}

tidy_mh_sheets <- function(sheet_data) {
  title <- names(sheet_data)[1] |> 
    (\(x) gsub("Title: ", "", x))()
  
  if (title %in% c("Title:")) {
    title <- names(sheet_data)[2] |> 
      (\(x) gsub("Title: ", "", x))()
  }
  
  title <- gsub(
    "CYP Mental Health CCG spend",
    "Children and Young People Mental Health spend",
    title
  )
  
  title <- gsub(
    "MH total spend",
    "Mental Health total spend",
    title
  )
  
  first_row <- match(
    "Region Code",
    sheet_data[[1]]
  )
  
  if (is.na(first_row)) {
    first_row <- match(
      "ONS Area Codes",
      sheet_data[[1]]
    )
  }
  
  first_row_is_header <- function(df) {
    
    all_na_field_names <- (colSums(is.na(slice(df, 1))) == 1) |> 
      (\(x) x[which(x)])() |> 
      names()
    
    all_dot_field_names <- (colSums(slice(df, 1) == ".") == 1) |> 
      (\(x) x[which(x)])() |> 
      names()
    
    
    df <- df |> 
      select(-any_of(c(
        all_na_field_names,
        all_dot_field_names)
      )
      )
    
    headers <- df[1, ] |> 
      as.character()
    
    # check for potential field names that are blank, and exclude them
    if (any(headers == "")) {
      remove_cols <- match("", headers)
      df <- df |> 
        select(-all_of(remove_cols))
      headers <- headers[-remove_cols]
    }
    
    df <- df |> 
      slice(-1) |> 
      set_names(
        headers
      )
    return(df)
  }
  
  sheet_data <- sheet_data |> 
    slice(first_row:nrow(sheet_data)) |> 
    first_row_is_header() |> 
    mutate(
      metric = title,
      .before = 1
    ) |> 
    pivot_longer(
      cols = contains("2"),
      names_to = "time_period",
      values_to = "value"
    ) |> 
    rename(
      `Region Code` = any_of("ONS Area Codes")
    ) |> 
    filter(
      !is.na(value),
      value != "N/A"
    )
  
  return(sheet_data)
}

#' @param filepath string; filespath to a&E workforce file
#' @param type string; "fte" or "stability"
tidy_a_and_e_workforce <- function(filepath, type) {
  
  type <- match.arg(type,
                    c("fte", "stability"))
  
  a_and_e_workforce_quarterly <- tidyxl::xlsx_cells(
    path = filepath,
    include_blank_cells = FALSE
    ) |> 
    mutate(
      year = 2000 + as.numeric(
        substr(str_extract(filepath, "[0-9]{4}"), 1, 2))
    ) |> 
    filter(
      grepl("^Table_1|^Table_2", sheet)
    )
  
  ae_workforce_metrics <- a_and_e_workforce_quarterly |> 
    filter(grepl("^Table", character)) |> 
    mutate(
      year = as.numeric(
        str_extract(
          character,
          "[0-9]{4}"
        )
      ),
      character = str_remove_all(
        character,
        ", [0-9]{4}-[0-9]{2}$"
      )
    ) |> 
    distinct(year, character) |> 
    separate(
      col = character,
      into = c("sheet", "metric"),
      sep = ": "
    ) |> 
    filter(
      grepl("^Full Time|^Stability", metric)
    ) |> 
    mutate(
      sheet = gsub(" ", "_", sheet)
    )
  
  a_and_e_workforce_quarterly <- a_and_e_workforce_quarterly |> 
    inner_join(
      ae_workforce_metrics,
      by = join_by(
        sheet, year
      )
    )
  
  
  top_row <- a_and_e_workforce_quarterly |> 
    filter(character == "Code") |> 
    distinct(
      year, metric, row
    ) |> 
    rename(
      top_row = "row"
    )
  
  
  a_and_e_workforce_quarterly <- a_and_e_workforce_quarterly |> 
    left_join(
      top_row,
      by = join_by(
        year, metric
      )
    ) |> 
    filter(
      row >= top_row
    ) |> 
    group_by(
      year, metric
    ) |> 
    behead(
      direction = "left",
      name = "org"
    ) |> 
    behead(
      direction = "left",
      name = "org_name"
    ) |> 
    filter(
      !grepl("ambulance", org_name, ignore.case = TRUE)
    )
  
  if (type == "stability") {
    output <- a_and_e_workforce_quarterly |> 
      filter(
        grepl("Stability", metric)
      ) |> 
      behead(
        direction = "up-left",
        name = "hchs"
      ) |> 
      behead(
        direction = "up",
        name = "staff_group"
      ) |> 
      select(
        "year",
        "metric",
        "staff_group",
        "org",
        "org_name",
        value = "numeric",
      ) |> 
      ungroup() |> 
      filter(
        grepl("^R", org),
        staff_group != "All Staff"
      ) |> 
      mutate(
        metric = paste0(
          metric, 
          " (", staff_group, ")"
        ),
        year = as.integer(year),
        frequency = "annual financial"
      )
    select(!c("staff_group"))
  } else if (type == "fte") {
    output <- a_and_e_workforce_quarterly |> 
      filter(
        !grepl("Stability", metric)
      ) |> 
      behead(
        direction = "up-left",
        name = "yr"
      ) |> 
      behead(
        direction = "up",
        name = "month"
      ) |> 
      select(
        "year", 
        "month",
        "metric",
        "org",
        "org_name",
        numerator = "numeric"
      ) |> 
      ungroup() |> 
      filter(
        grepl("^R", org),
        !grepl("Average", year)
      ) |> 
      mutate(
        year = as.integer(year),
        month = match(month, month.name),
        quarter = case_when(
          month == 6 ~ 1L,
          month == 9 ~ 2L,
          month == 12 ~ 3L,
          month == 3 ~ 4L,
          .default = NA_integer_
        ),
        frequency = "quarterly",
        numerator = replace_na(numerator, 0),
        metric = paste(
          "A and E",
          gsub(" Figures", "", metric)
        )
      ) |> 
      filter(!is.na(quarter)) |> 
      select(!c("month"))
  }
  
  return(output)
  
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

estimate_21_22_populations <- function() {
  # start with LSOA2020 population estimates by 10 year age bands
  # apply lsoa to LAD21 lookup to calculate the proportion of each LAD21 within each LSOA
  
  
  
  # LSOA to ICS lookup
  filepath <- check_and_download(
    filepath = "data-raw/Lookups/lsoa_icb.xlsx",
    url = "https://www.arcgis.com/sharing/rest/content/items/1ac8547e9a8945478f0b5ea7ffe1a6b1/data"
  )
  
  lsoa_icb <- readxl::read_excel(
    filepath,
    sheet = "LSOA11_LOC22_ICB22_LAD22"
  ) |> 
    select(LSOA11CD, ICB22CDH)
  
  # apply those proportions to 2021 and 2022 estimates published here: 
  # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales
  
  ics_21_22 <- lsoa_populations_21_22() |> 
    # apply LSOA to ICS lookup
    left_join(
      lsoa_icb,
      by = join_by(
        LSOA11CD
      )
    ) |> 
    # aggregate to ICS
    summarise(
      numerator = sum(population),
      .by = c(
        year, ICB22CDH, age_band
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
      .by = c(year, ICB22CDH)
    ) |> 
    mutate(
      value = numerator / denominator,
      frequency = "annual calendar"
    ) |> 
    select(
      !c("age_band")
    ) |> 
    rename(
      org = "ICB22CDH"
    )
  
  
  return(ics_21_22)
  
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

lsoa_populations_21_22 <- function() {
  lsoa2020 <- check_and_download(
    filepath = "data-raw/Population/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx",
    url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"
  ) |> 
    open_pops_file() |> 
    pluck("data") |> 
    filter(
      grepl("^E", LSOA11CD)
    ) |> 
    select(
      LSOA11CD,
      LAD21CD = "LA Code (2021 boundaries)",
      as.character(0:89),
      "90+"
    ) |> 
    pivot_longer(
      cols = !c("LSOA11CD", "LAD21CD"),
      names_to = "age",
      values_to = "population"
    ) |> 
    mutate(
      age = gsub("\\+", "", age),
      age_band = floor(as.numeric(age) / 10) * 10
    ) |> 
    summarise(
      population = sum(population),
      .by = c(
        LSOA11CD,
        LAD21CD,
        age_band
      )
    ) |> 
    mutate(
      proportion = population / sum(population),
      .by = c(LAD21CD, age_band)
    ) |> 
    mutate(
      LAD21CD = str_trim(LAD21CD)
    ) |> 
    select(!c("population"))
  
  lsoa_age_band_pops <- check_and_download(
    filepath = "data-raw/Population Local Authority/myebtablesenglandwales20112022v2.xlsx",
    url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales/mid2011tomid2022detailedtimeseries/myebtablesenglandwales20112022v2.xlsx"
  ) |> 
    readxl::read_excel(
      sheet = "MYEB1 (2021 Geography)",
      skip = 1
    ) |> 
    filter(
      grepl("^E", ladcode21)
    ) |> 
    mutate(
      age_band = floor(as.numeric(age) / 10) * 10
    ) |> 
    select(
      LAD21CD = "ladcode21",
      "age_band",
      "population_2021",
      "population_2022"
    ) |> 
    summarise(
      across(
        c(population_2021, population_2022),
        sum
      ),
      .by = c(LAD21CD, age_band)
    ) |> 
    pivot_longer(
      cols = starts_with("population"),
      names_to = "year",
      values_to = "population"
    ) |> 
    left_join(
      lsoa2020,
      by = join_by(
        LAD21CD, age_band
      ),
      relationship = "many-to-many"
    ) |> 
    mutate(
      population = population * proportion,
      year = gsub("population_", "", year)
    ) |> 
    select(!c("proportion"))
  
  return(lsoa_age_band_pops)
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
        metric = paste0(
          "Proportion of available beds that are occupied (",
          category,
          " - ",
          bed_type,
          ")"
        ),
        availability_metric = case_when(
          availability_metric == "Available" ~ "denominator",
          availability_metric == "Occupied" ~ "numerator",
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
      value = (numerator / denominator) * 100,
      metric = "Average daily proportion of patients who no longer meet the criteria to reside that remain in hospital",
      frequency = "monthly"
    )
  
  return(monthly_nctr)
}

calculate_lag_years_from_last_fit <- function(last_fit_object) {
  lagged_years <- last_fit_object |> 
    extract_workflow() |> 
    pluck("pre", "actions", "recipe", "recipe", "var_info") |> 
    pull(variable) |> 
    (\(x) substr(x[grepl("^lag", x)], 1, 5))() |> 
    unique() |> 
    substr(x = _, 5, 5) |> 
    (\(x) c(as.numeric(x), 0))() |> 
    max()
  
  return(lagged_years)
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
#' @param latest_codes_used logical; data that is published monthly as it is
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
            function(x) sum(x * population)
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


summarise_health_pop_files <- function(filepath, incl_agebands) {
  possible_ccg_subicb_fields <- c(
    "CCG_CODE",
    "NHSE_CCG_CODE",
    "ORG_CODE"
  )
  
  possible_denominator_fields = c(
    "NUMBER_OF_PATIENTS",
    "TOTAL_ALL"
  )
  
  if (grepl("csv$", filepath)) {
    pops <- read.csv(filepath)
  } else if (grepl("zip$", filepath)) {
    pops <- unzip_file(
      filepath,
      "prac"
    )
  }
  datatype <- ifelse("ORG_TYPE" %in% names(pops), "long", "wide")
  
  if (incl_agebands == FALSE) {
    sum_groupings <- "health_org_code"
    if (datatype == "long") {
      pops <- pops |> 
        filter(
          ORG_TYPE %in% c("CCG", "ICB"),
          AGE == "ALL",
          SEX == "ALL"
        )
      
    }
  } else {
    sum_groupings <- c("health_org_code", "AGE")
    if (datatype == "long") {
      pops <- pops |> 
        filter(
          ORG_TYPE %in% c("CCG", "ICB"),
          AGE != "ALL",
          SEX != "ALL"
        ) |> 
        mutate(
          AGE = as.integer(str_extract(AGE, "[0-9]{1,2}")),
          AGE = floor(AGE / 5) * 5
        )
        
    } else if (datatype == "wide") {
      pops <- pops |> 
        select(
          starts_with(c("MALE", "FEMALE", possible_ccg_subicb_fields))
        ) |> 
        pivot_longer(
          cols = !any_of(possible_ccg_subicb_fields),
          names_to = c("SEX", "AGE"),
          names_sep = "_",
          values_to = "NUMBER_OF_PATIENTS"
        ) |> 
        mutate(
          AGE = as.integer(str_extract(AGE, "[0-9]{1,2}"))
        )
    }
  }
  
  pops <- pops |> 
    rename(
      health_org_code = any_of(possible_ccg_subicb_fields),
      denominator = any_of(possible_denominator_fields)
    ) |> 
    summarise(
      denominator = sum(denominator),
      .by = c(all_of(sum_groupings))
    ) |> 
    mutate(
      year = as.integer(
        stringr::str_extract(
          filepath,
          "[0-9]{4}"
        )
      ),
      month = match(
        substr(basename(filepath), 1, 3),
        month.abb
      )
    )
  
  return(pops)
}


# aggregation tasks -------------------------------------------------------

remove_incomplete_period <- function(data, from, to) {
  from <- match.arg(
    from,
    c("month", "quarter")
  )
  
  to <- match.arg(
    to,
    c("quarter", "year")
  )
  
  if (from == "month") {
    if (to == "quarter") {
      expected_n <- 3
      data <- data |> 
        mutate(
          quarter = case_when(
            month %in% 1:3 ~ 4L,
            month %in% 4:6 ~ 1L,
            month %in% 7:9 ~ 2L,
            month %in% 10:12 ~ 3L,
            .default = NA_real_
          ),
          year = case_when(
            quarter == 4L ~ year - 1,
            .default = year
          )
        )
      group_fields <- c("year", "quarter")
    } else if (to == "year") {
      expected_n <- 12
      group_fields <- "year"
    }
  } else if (from == "quarter") {
    data <- data |> 
      mutate(
        year = case_when(
          quarter == 4L ~ year + 1,
          .default = year
        )
      )
    if (to == "year") {
      expected_n <- 4
      group_fields <- "year"
    }
  }
  group_fields <- c(group_fields, "org", "metric")
  
  data_keep <- data |> 
    summarise(
      cnt = n(),
      .by = any_of(
        group_fields
      )
    ) |> 
    filter(
      cnt == expected_n
    ) |> 
    select(!c("cnt", "org")) |> 
    distinct()
    
  
  data <- data |> 
    inner_join(
      data_keep,
      by  = unique(c("year", to, "metric")),
      relationship = "many-to-one"
    )
  
  return(data)
}

# takes a mean of numerator and denominator for each month in the quarter and
# calculates a new value based on that. Requires the columns year, month, org,
# org_name, metric, numerator and denominator
#'
#' @param multiplier integer; what to multiply the final value by
monthly_to_quarterly_mean <- function(data, multiplier = 1) {
  data <- data |> 
    remove_incomplete_period(
      from = "month",
      to = "quarter"
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
      value = (numerator / denominator) * multiplier,
      frequency = "quarterly"
    )
  
  return(data)
}

# takes a mean of numerator and denominator for each month in the quarter and
# calculates a new value based on that. Requires the columns year, month, org,
# org_name, metric, numerator and denominator
#'
#' @param multiplier integer; what to multiply the final value by
monthly_to_quarterly_sum <- function(data, multiplier = 1) {
  data <- data |> 
    remove_incomplete_period(
      from = "month",
      to = "quarter"
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
      value = (numerator / denominator) * multiplier,
      frequency = "quarterly"
    )
  
  return(data)
}

#' takes a mean of numerator and denominator for each month in the year and
#' calculates a new value based on that. Requires the columns year, month, org,
#' org_name, metric, numerator and denominator
#'
#' @param multiplier integer; what to multiply the final value by
monthly_to_annual_mean <- function(data, year_type = "financial", multiplier = 1) {
  
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
    remove_incomplete_period(
      from = "month",
      to = "year"
    ) |> 
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
      value = (numerator / denominator) * multiplier,
      frequency = paste(
        "annual",
        year_type
      )
    )
  
  return(data)
}

#' takes a mean of numerator and denominator for each month in the year and
#' calculates a new value based on that. Requires the columns year, month, org,
#' org_name, metric, numerator and denominator
#' 
#' @param multiplier integer; what to multiply the final value by
monthly_to_annual_sum <- function(data, year_type = "financial", multiplier = 1) {
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
    remove_incomplete_period(
      from = "month",
      to = "year"
    ) |> 
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
      value = (numerator / denominator) * multiplier,
      frequency = paste(
        "annual",
        year_type
      )
    )
  
  return(data)
}

#' takes a mean of numerator and denominator for each quarter in the year and
#' calculates a new value based on that. Requires the columns year, month, org,
#' org_name, metric, numerator and denominator
#' 
#' @param multiplier integer; what to multiply the final value by
quarterly_to_annual_mean <- function(data, year_type, multiplier = 1) {
  year_type <- match.arg(year_type, c("financial", "calendar"))
  
  data <- data |> 
    remove_incomplete_period(
      from = "quarter",
      to = "year"
    ) |> 
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
      value = (numerator / denominator) * multiplier,
      frequency = paste(
        "annual",
        year_type
      )
    )
  
  return(data)
}

#' takes a sum of numerator and denominator for each quarter in the year and
#' calculates a new value based on that. Requires the columns year, month, org,
#' org_name, metric, numerator and denominator
#' 
#' @param multiplier integer; what to multiply the final value by
quarterly_to_annual_sum <- function(data, year_type, multiplier = 1) {
  year_type <- match.arg(year_type, c("financial", "calendar"))
  
  data <- data |> 
    remove_incomplete_period(
      from = "quarter",
      to = "year"
    ) |> 
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
      value = (numerator / denominator) * multiplier,
      frequency = paste(
        "annual",
        year_type
      )
    )
  
  return(data)
}

# lookups -----------------------------------------------------------------

practice_to_icb <- function() {
  
  lkp_filepath <- "data-raw/Lookups/practice_to_ics.csv"
  if (!file.exists(lkp_filepath)) {
    
    url <- "https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data"
    
    zip_files <- obtain_links(url, include_link_text = TRUE) |> 
      (\(x) x[grepl("[0-9]{4}-[0-9]{2}$|\\[PAS\\]$", names(x))])() |>
      (\(x) paste0("https://digital.nhs.uk", x))() |>
      purrr::map(
        ~ obtain_links(.x, include_link_text = TRUE)
      ) |>
      purrr::map(
        .f = \(x) x[grepl("zip$", x)]
      ) |> 
      unlist() |> 
      (\(x) x[grepl("Raw", names(x))])() |> 
      (\(x) x[!grepl("1415", x)])()
    
    temp_save_folder <- "data-raw/junk"
    
    fls <- purrr::map(
      zip_files,
      ~ download_unzip_files(
        .x, 
        directory = temp_save_folder,
        zip_file_pattern = "GEOGRAPHIES|ORGANISATION_REFERENCE"
      )
    ) |> 
      purrr::map(
        \(x) if (any(grepl("GEOGRAPHIES", x))) {
          x[grepl("GEOGRAPHIES", x)]
        } else if (any(grepl("ORGANISATION_REFERENCE", x))) {
          x[grepl("ORGANISATION_REFERENCE", x)]
        } else {
          NULL
        }) 
    
    rename_csv_lkp_function <- function(df) {
      df <- df |> 
        rename(
          SUB_ICB_ODS_CODE = any_of(c("SUB_ICB_LOC_ODS_CODE", "CCG_ODS_CODE", "CCG_CODE"))
          
        ) |> 
        select(
          "PRACTICE_CODE",
          org = "SUB_ICB_ODS_CODE"
        )
    }
    
    lkp <- fls |> 
      purrr::map(
        read.csv
      ) |> 
      purrr::map(
        rename_csv_lkp_function
      ) |> 
      purrr::list_rbind(
        names_to = "year"
      ) |> 
      mutate(
        year = stringr::str_extract(year, "[0-9]{4}"),
        year = as.numeric(year)
      ) |> 
      update_to_latest_ics_codes()
    
    # identify practices that don't have an org code
    missing_practices <- lkp |> 
      filter(is.na(org)) |> 
      pull(PRACTICE_CODE) |> 
      unique()
    
    # some of the practices with a missign ICB code have an icb code for a later
    # year, so create a lookup table from the existing dataset
    existing_data_lkp <- lkp |> 
      filter(
        PRACTICE_CODE %in% missing_practices,
        !is.na(org)
      ) |> 
      distinct(
        PRACTICE_CODE,
        org
      )
    
    # update the existing table with the ICB codes we know from later years
    lkp <- lkp |> 
      rows_update(
        existing_data_lkp,
        by = "PRACTICE_CODE"
      )
    
    # fill in the NA values in the org field
    lkp <- assign_org_to_missing_practices(lkp)
    
    # finally, some practices change their ics depending on the year
    # (legitimately). Here we want to map all practices to the latest geography,
    # so we contruct the final table with the latest year of information for
    # each practice
    lkp <- lkp |> 
      arrange(
        PRACTICE_CODE,
        desc(year)
      ) |> 
      slice(1, .by = PRACTICE_CODE) |> 
      select(PRACTICE_CODE, org)
    
    
    write.csv(
      lkp,
      lkp_filepath,
      row.names = FALSE
    )
    
    # remove the temporaray folder used to extract the lookup files
    unlink(
      temp_save_folder,
      recursive = TRUE
    )
    
  } else {
    lkp <- read.csv(lkp_filepath)
  }
  return(lkp)
}

#' identifies NAs in the org column of data object. Then uses PRACTICE_CODE
#' field along with the attach_icb_to_org to assign a new org to the Practice,
#' before replacing it in the original table and returning that updated object
assign_org_to_missing_practices <- function(data) {
  # identify the practices that are still missing a code, and use the
  # attach_icb_to_org function to assign them an icb code
  missing_practices_lkp <- data |> 
    filter(is.na(org)) |> 
    pull(PRACTICE_CODE) |> 
    unique() |> 
    attach_icb_to_org() |> 
    distinct() |> 
    rename(
      PRACTICE_CODE = "health_org_code",
      org = "icb_code"
    )
  
  
  # update the existing table with new codes
  data <- data |> 
    rows_update(
      missing_practices_lkp,
      by = "PRACTICE_CODE"
    )
  
  return(data)
}

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

#' function requires a dataset with an column called org (which are ODS CCG
#' codes. It identifies the the codes that are missing from the list of ccgs
#' that make up the final ccgs before transitioning to sub-icbs. The successor
#' code for the missing ccgs is then obtained and used to determine the ICS code
#' using the health_org_lookup function
update_to_latest_ics_codes <- function(data) {
  latest_sub_icb_codes <- ccg_to_icb() |> 
    pull(ccg_code)
  
  missing_codes <- data |> 
    pull(org) |> 
    setdiff(latest_sub_icb_codes)
  
  new_codes <- map_chr(
    missing_codes,
    health_org_successors
  )
  
  lkp <- tibble(
    missing_codes = missing_codes,
    new_codes = new_codes
  ) |> 
    filter(
      !is.na(new_codes)
    ) |> 
    deframe()
  # replaces org with latest sub-icb codes
  data <- data |> 
    mutate(
      org = str_replace_all(
        org, lkp
      )
    )
  
  # create sub-ics to ics lkp
  sub_ics_lkp <- tibble(
    org = unique(data$org)
  ) |> 
    mutate(
      ics = map_chr(
        org,
        health_org_lookup
      )
    ) |> 
    deframe()
  # replaces org with latest sub-ics codes
  data <- data |> 
    mutate(
      org = str_replace_all(
        org, sub_ics_lkp
      )
    )
  
  return(data)
}
# provides all available information from the Organsational Data Services API
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
  
  return(lkp)
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
#'   Then, any successor organisations to the codes provided are identified from
#'   the ODS API. Sometimes an organisation can be divided into multiple
#'   organisations, so these are all included. Then the ODS API is used to
#'   identify active "relative" organisations - and these organisations are
#'   cross checked with the ICB codes previously obtained. Finally, where ICBs
#'   are not yet identified, the post code for the organisation is retrieve from
#'   the ODS API, this is used to identify the LSOA using the Postcodes.io API,
#'   which is then used to identify ICB22 using the Open Geography Portal.
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
    # creates table of health_org_code and successor_code
    tidyr::unnest(
      cols = successor_code
    ) |> 
    # use successor code for looking up ICB code if it exists
    mutate(
      code_for_lkp = case_when(
        is.na(successor_code) ~ health_org_code,
        .default = successor_code
      ),
      # idenify parent codes (could be more than 1 for each code_for_lkp)
      parent_code = purrr::map(
        code_for_lkp,
        health_org_lookup
      )
    ) |> 
    tidyr::unnest(
      cols = parent_code
    ) |> 
    distinct() |> 
    # where health_org_code is an icb_code, then keep it, otherwise keep the
    # parent_code if it is an icb_code, otherwise make it NA
    mutate(
      icb_code = case_when(
        health_org_code %in% icb_codes ~ health_org_code,
        parent_code %in% icb_codes ~ parent_code,
        .default = NA_character_
      )
    ) |> 
    # mark health_org_codes for retaining where it has at least 1 icb_code
    # identified
    mutate(
      retain = sum(!is.na(icb_code)),
      .by = health_org_code
    ) |> 
    select(!c("successor_code", "code_for_lkp")) |> 
    distinct() |> 
    # remove records where the applicable icb_code is identified for a
    # health_org_code in a separate record (eg, a health_org may have 3 records
    # at this point, and the first one is the one with a valid icb_code; this
    # step will remove the 2nd and 3rd record)
    anti_join(
      tibble(
        icb_code = NA,
        retain = 1
      ),
      by = join_by(icb_code, retain)
    ) |> 
    select(!c("retain")) |> 
    # search for icb by postcode of health_org if there is no valid icb_code
    # identified by the previous method
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
          new_directory = "data-raw/Lookups/",
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
                     full.names = TRUE) 
  
  pops <- set_names(
    pops,
    nm = str_extract(pops, "[0-9]{4}")
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
          new_directory = "data-raw/Lookups",
          filename = paste0(
            "/LSOA11_STP",
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
                     full.names = TRUE)
  
  pops <- set_names(
    pops,
    nm = str_extract(pops, "[0-9]{4}")
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


quarterly_ics_populations <- function(incl_agebands = FALSE) {
  
  summary_groupings <- c("icb_code", "year", "month")
  
  if (incl_agebands == TRUE) {
    summary_groupings <- c(summary_groupings, "age_band")
    summary_filepath <- "data-raw/Health populations/quarterly_population_summary_age_bands.csv"
  } else {
    summary_filepath <- "data-raw/Health populations/quarterly_population_summary.csv"
  }
  
  # gp population file urls from NHS website
  url <- "https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice"
  
  links <- obtain_links_and_text(url) |> 
    (\(x) x[grepl("[a-z]{,9}-[0-9]{4}", basename(x))])() |> 
    (\(x) rlang::set_names(paste0("https://digital.nhs.uk", x),
                           nm = names(x)))() |> 
    (\(x) x[grepl("january|april|july|october", basename(x), ignore.case = TRUE)])() |> 
    purrr::lmap(
      ~ list(
        obtain_links_and_text(
          .x
        )
      )
    ) |> 
    unlist() |> 
    (\(x) rlang::set_names(x, nm = sub("^.*?([A-Z])", "\\1", names(x))))() |> 
    (\(x) rlang::set_names(x, nm = gsub("[\n].*$", "", names(x))))() |>
    (\(x) x[grepl("zip$|csv$", x)])() |> 
    (\(x) x[!grepl("lsoa|males|tall", basename(x))])() |> 
    (\(x) x[grepl("ICB|CCG", names(x))])() |> 
    # (\(x) x[grepl("all|ccg", x)])() |> 
    tibble::enframe() |> 
    mutate(
      mnth = stringr::str_extract(
        name,
        pattern = paste(c(month.name, month.abb), collapse = "|")
      ),
      mnth = substr(mnth, 1, 3),
      year = stringr::str_extract(
        name,
        pattern = "[0-9]{4}"
      )
    ) |> 
    mutate(
      n = n(),
      .by = c(mnth, year)
    ) |> 
    mutate(
      include = case_when(
        n == 1 ~ TRUE,
        .default = grepl("Single", name)
      )
    ) |> 
    filter(
      include == TRUE
    ) |> 
    mutate(
      name = paste(
        mnth,
        year,
        sep = "-"
      )
    ) |> 
    select(c("name", "value")) |> 
    tibble::deframe()
  
  obtain_and_download_ics_populations <- function(links, incl_agebands) {
    files <- purrr::lmap(
      links,
      ~ as.list(
        check_and_download(
          filepath = paste0(
            "data-raw/Health populations/", 
            names(.x),
            " ",
            basename(.x)),
          url = .x
        )
      )
    )
    
    health_pop_denominators <- purrr::map_df(
      files,
       ~ summarise_health_pop_files(
         .x, 
         incl_agebands = incl_agebands
       )
    ) |> 
      filter(health_org_code != "UNKNOWN")
    
    if (incl_agebands) {
      health_pop_denominators <- health_pop_denominators |> 
        mutate(
          age_band = floor(AGE / 10) * 10,
          age_band = case_when(
            age_band >= 80 ~ "80+",
            .default = paste0(age_band, "-", (age_band + 9))
          )
        )
    }
    
    
    org_lkp <- unique(health_pop_denominators$health_org_code) |> 
      attach_icb_to_org()
    
    quarterly_health_pop_denominators <- health_pop_denominators |> 
      left_join(
        org_lkp,
        by = join_by(
          health_org_code
        )
      ) |> 
      summarise(
        denominator = sum(denominator),
        .by = all_of(summary_groupings)
      ) |> 
      rename(
        org = "icb_code"
      )
    
    return(quarterly_health_pop_denominators)
  }
  
  if (file.exists(summary_filepath)) {
    # obtain the latest expected month from the NHSE website
    latest_available_month_year <- names(links[1])
    latest_recorded_month_year <- read.csv(summary_filepath) |> 
      distinct(
        month,
        year
      ) |> 
      filter(
        year == max(year)
      ) |> 
      filter(
        month == max(month)
      ) |> 
      mutate(
        latest = paste(
          month.abb[month],
          year,
          sep = "-"
        )
      ) |> 
      pull(latest)
    
    if (identical(latest_available_month_year, latest_recorded_month_year)) {
      quarterly_health_pop_denominators <- read.csv(summary_filepath)
    } else {
      quarterly_health_pop_denominators <- obtain_and_download_ics_populations(
        links, 
        incl_agebands = incl_agebands)
      
      write.csv(
        quarterly_health_pop_denominators,
        summary_filepath,
        row.names = FALSE
      )
    }
  } else {
    quarterly_health_pop_denominators <- obtain_and_download_ics_populations(
      links, 
      incl_agebands = incl_agebands)
    
    write.csv(
      quarterly_health_pop_denominators,
      summary_filepath,
      row.names = FALSE
    )
  }
  
  return(quarterly_health_pop_denominators)
  
}



trust_to_ics_proportions <- function(final_year = 2020) {
  #lsoa code to icb code
  lsoa_to_icb <- check_and_download(
    filepath = "data-raw/Lookups/lsoa_icb.xlsx",
    url = "https://www.arcgis.com/sharing/rest/content/items/1ac8547e9a8945478f0b5ea7ffe1a6b1/data"
    ) |> 
    readxl::read_excel(
      sheet = "LSOA11_LOC22_ICB22_LAD22"
    ) |> 
    distinct(
      LSOA11CD, 
      ICB22CDH
    ) |> 
    rename(
      org = "ICB22CDH"
    )
  
  # now create MSOA to ICB, with a count of ICBs in an MSOA (eg, if an MSOA goes
  # over an ICB boundary, then it will allow us to divide the final metric by 2)
  url <- "https://opendata.arcgis.com/api/v3/datasets/d382604321554ed49cc15dbc1edb3de3_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1"
  msoa_to_icb <- check_and_download(
    filepath = "data-raw/Lookups/lsoa_to_msoa.csv",
    url = url
  ) |> 
    read.csv() |> 
    distinct(
      LSOA11CD, MSOA11CD
    ) |> 
    left_join(
      lsoa_to_icb,
      by = join_by(LSOA11CD)
    ) |> 
    distinct(
      MSOA11CD, org
    ) |> 
    add_count(
      MSOA11CD,
      name = "divisor"
    )
  
  # this link changes because it is generated with php
  # the file can be found here: https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl
  url <- "https://public.boxcloud.com/d/1/b1!_if4KZ-UlX6Fdqz-M5OQ-8Y6ZRDyIlXfPY9wSrdbBAAOKF27MZM1CfnXKFNdBKMN1blXwHiGsosjEvyMoVvelzX_fyx4EM9lGtjkWNeENiyVAlwKAtE3U62msLnU4jl0Ydkn9HcEHgoPYr5TjddsDXG02DD8GZArAj3s1LK5QO4pFBvUjczfVMXFGxomoEqBQ9hsGBGljY70GpTL4CfSx6P0l_UtE-8zsKFXlWOfGpBNzHFGj1-XkeV5ZVFahJP7zz3vvs7cMDYbsmKr_osn6ICqMrWi3re4ISic3gtsOrRUS9DAXoXC0ra2ODaswc94tFLeg1Faldz15dxFZaRGhirsmfeTYzoQt_AgzifZcLcp9RXHUah6G_bmoHIQYHnG2YPLkNYdUvYJ-rMkgMDmx3v3QS1tYtvZUXFQAmMo1x7dmLn_VMp8v8P5-lr1xOwyClHwCf9X6gOi1dc4bS9ld9K3MfdXzBXumHdx84HYgryrT6fH7gzk5j7dt37V20DNDOfUzDeJBonAfdCe9u08NtjsxXHokjvGvvZWJgwVwhamywpvyYgjoY11KvYBIC3PWTBMkjDd8oA8d04xItkbFD1hIokffSfWm6UkVFyz1tjP3UWcHaxvPiJ6C0FVY0HN0RxmHMyoKKxUL6BE0Fv_W9zEhkMuBG1ehKAgko0BWZsb6k1X4ID0zdavgMvCunA6_g803kNW4x3RCw6woRjLkoIrhsYyuB_XqMvpx4rdmtkhS2IgwFwqzap0BgvIZ88sjCoeq3709Xq1SwzoiTnhPpIl2wM6TQb1HdiK3kb1kQB-H3ckh1F-_MEOXIqKOUypr8aO4S8VkehoX0tYnMkKSnMqSe2lYsOszpyRYJCG2V_-P9ZivTXZx-wMKrvY0Hy_dQwa_E7dw4yWQ7CPM7PkQgi7-A_2q_jjsOnro8FAn_6U38-CVsVE9nTQafwfCDAIbISC3VsLX2U9vPBZwO8UaHdORIsj4VYJ5WRpQIkwiP_ryKJmsu20I6MMWAQg5-Mo-L9ezGXfgM9X3DztUQktXejvaRFd-MT4ytNPwA911fNA5pSWdrjm5Oy8qggppZepK6wLBWpQeqV27AWaKk1Cg0eAP9Jl-KewxiwSwHjjILSHLLPsz2MHC9zoAFH0JGcJA67ZEdqB-aJbtEoEQQ1yVNDBftf7VZJIX49BBL0IgePUYv7n_aeYOLVQbsO8P2Mo1TQFnpZVwBt5HRqGLZyNz9XVmspRxbn1oHfZVRGlGWJxKTZmy7XiTuXIhSfE8rbQeyWqqrSdcSyd63Ekb5csu3CM1AWwckWBEptT9mKSG1AoOfggv0MAJpq7kPXsB-5730VaLymLrpRNUAKBs3Bv9pCQ1emCYyKbbx4VdYAQyPALgHc./download"
  
  file <- check_and_download(
    filepath = "data-raw/Catchment populations/catchment-populations.xlsx",
    url = url
  )
  
  msoas_in_catchments <- c(
    "All Admissions"#,
    # "Elective", 
    # "Emergency"
  ) |> 
    map_df(
      ~ readxl::read_excel(
        path = file,
        sheet = .x
      )
    ) |> 
    # create table of number of patients that attend each trust by msoa and year
    select(
      year = "CatchmentYear",
      # type = "AdmissionType",
      MSOA11CD = "msoa",
      "TrustCode",
      "patients"
    )
  
  trust_to_ics_proportions <- msoas_in_catchments |> 
    # add ICB to patient count data
    left_join(
      msoa_to_icb,
      by = join_by(MSOA11CD),
      relationship = "many-to-many"
    ) |> 
    # divide number of patients between icbs where msoa crosses icb boundary
    mutate(
      patients = patients / divisor
    ) |> 
    # count patients for each trust that come from each icb by year
    summarise(
      patients = sum(patients),
      .by = c(
        year,
        # type,
        TrustCode,
        org
      )
    ) |> 
    # for each year, calculate the proportion of patients that attend each trust
    # from the feeding icbs
    mutate(
      proportion = patients / sum(patients),
      .by = c(
        year,
        # type,
        TrustCode
      )
    ) |> 
    select(
      "year",
      "TrustCode",
      "org",
      "proportion"
    )
  
  if (final_year <= max(trust_to_ics_proportions$year)) {
    trust_to_ics_proportions <- trust_to_ics_proportions |> 
      filter(
        year <= final_year
      )
  } else {
    # the if metric contains years beyond the acute trust catchment analysis,
    # then use the final year of known catchment data to provide the icb
    # proportions for future years
    additional_years <- seq(
      from = max(trust_to_ics_proportions$year) + 1,
      to = final_year,
      by = 1
    )
    trust_to_ics_proportions_extra_years <- trust_to_ics_proportions |> 
      filter(
        year == max(year)
      ) |> 
      select(!c("year")) |> 
      cross_join(
        tibble(year = additional_years)
      )
    
    trust_to_ics_proportions <- bind_rows(
      trust_to_ics_proportions,
      trust_to_ics_proportions_extra_years
    )
  }
  
  return(trust_to_ics_proportions)
}

#' returns tibble of the top n nearest known orgs to any missing orgs, along
#' with the distance between them
#' @param missing_orgs character; org codes for missing orgs
#' @param known_orgs character; org codes for known orgs
#' @param n number; top n nearest orgs to return
#' @details accesses the post code from the ODS API for both missing and known
#'   orgs. Subsequently, obtains longs and lats from the postcodes.io API. Uses
#'   the geosphere package to calculate the distance between each missing org
#'   and known org, then filters for the top n for each missing org
nearest_health_orgs <- function(missing_orgs, known_orgs, n) {
  code_table <- tibble(
    org = c(missing_orgs, known_orgs),
    missing = org %in% missing_orgs
  ) |> 
    mutate(
      postcode = map_chr(
        org,
        ~ ods_info(.x)[["Organisation"]][["GeoLoc"]][["Location"]][["PostCode"]]
      ),
      postcode = gsub(" ", "", postcode),
      postcode = case_when(
        postcode == "B95ST" ~ "B95SS",
        .default = postcode
      )
    )
  
  lats_and_longs <- split(
    code_table$postcode, 
    ceiling(
      seq_along(
        code_table$postcode
      ) / 100
    )
  ) |> 
    lapply(list) |> 
    lapply(
      set_names,
      "postcodes"
    ) |> 
    lapply(
      bulk_postcode_lookup
    ) |> 
    map_depth(
      .depth = 1,
      .f = ~ map_df(
        .x,
        function(x) tibble(
          postcode = pluck(x, "query"),
          longitude = pluck(x, "result", "longitude"),
          latitude = pluck(x, "result", "latitude")
        )
      )
    ) |> 
    bind_rows()
  
  code_table <- code_table |> 
    rename(
      postcode_ods = "postcode"
    ) |> 
    bind_cols(
      lats_and_longs
    )
  
  known_org_locations <- code_table |> 
    filter(missing == FALSE)
  
  missing_org_locations <- code_table |> 
    filter(missing == TRUE)
  
  closest_orgs <- geosphere::distm(
    missing_org_locations[,c('longitude','latitude')], 
    known_org_locations[,c('longitude','latitude')], 
    fun = geosphere::distVincentyEllipsoid) |> 
    data.frame() |> 
    set_names(
      known_org_locations$org
    ) |> 
    bind_cols(
      tibble(
        missing_org = missing_org_locations$org
      )
    ) |> 
    pivot_longer(
      cols = !c(missing_org),
      names_to = "known_org",
      values_to = "distance"
    ) |> 
    mutate(
      rnk = rank(distance),
      .by = missing_org
    ) |> 
    arrange(
      missing_org, rnk
    ) |> 
    filter(
      rnk <= n
    ) |> 
    select(
      missing_org,
      known_org,
      distance
    )
  
  return(closest_orgs)
}

#' applies the numerator/denominator calculated at trust level to the MSOAs in
#' the catchment that feed that trusts (for all admissions). The proportion of
#' that catchment in each ICS is then applied to the trust value to calculate
#' ICS values. Trusts that aren't acute (and hence don't have a published
#' catchment population) have their catchments estimated from the nearest 2
#' known acute trust catchments, where the weighting of the catchment is related
#' to the proximity of the known catchments.
#'
#' @param data; tibble with org, year, (quarter), (metric), (frequency), and
#'   numerator or denominator
#' @param proportion logical; if true, function will multiple the numerator /
#'   denominator by 100 to ensure the value is between 0 and 100
#' 
apply_catchment_proportions <- function(data, proportion = FALSE) {
  
  
  trust_ics_lkp <- trust_to_ics_proportions(
    final_year = max(data$year)
  ) |> 
    rename(
      health_org_code = "TrustCode",
      icb_code = "org"
    ) 
  
  # organisations not in the Trust catchment populations
  # these are usually community hospitals
  orgs <- data |> 
    pull(org) |> 
    unique() |> 
    setdiff(
      unique(trust_ics_lkp$health_org_code)
    )
  
  if (length(orgs) > 0) {
    # table of which known health orgs existed in which years
    health_orgs_by_year <- trust_ics_lkp |> 
      distinct(
        year, health_org_code
      )
    
    org_lkp <- nearest_health_orgs(
      missing_orgs = orgs,
      known_orgs = unique(trust_ics_lkp$health_org_code),
      n = 2
    ) |> 
      left_join(
        health_orgs_by_year,
        by = join_by(known_org == health_org_code),
        relationship = "many-to-many"
      ) |> 
      mutate(
        n_trusts = n(),
        .by = c(missing_org, year)
      ) |> 
      mutate(
        known_org_proportion = case_when(
          n_trusts == 1 ~ 1,
          .default = 1 - (distance / sum(distance))
        ),
        .by = c(missing_org, year)
      ) |> 
      left_join(
        trust_ics_lkp,
        by = join_by(
          year,
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
  } else {
    org_lkp <- trust_ics_lkp
  }
  
  
  data <- data |> 
    left_join(
      org_lkp,
      by = join_by(
        year,
        org == health_org_code
      ),
      relationship = "many-to-many"
    ) |> 
    summarise(
      across(
        any_of(c("numerator", "denominator")),
        ~ sum(.x * proportion, na.rm = TRUE) # some health_orgs attributed to multiple icbs, so these are split between the icbs
      ),
      .by = c(year, any_of(c("quarter", "month", "metric", "frequency")), icb_code)
    ) |> 
    rename(
      org = icb_code
    ) 
  
  if (all(c("numerator", "denominator") %in% names(data))) {
    if (proportion == TRUE) {
      data <- data |> 
        mutate(value = (numerator / denominator) * 100)
    } else {
      data <- data |> 
        mutate(value = numerator / denominator)  
    }
  }
  
  return(data)
}