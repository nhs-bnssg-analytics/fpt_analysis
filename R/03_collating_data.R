source("R/00_libraries.R")
source("R/01_utils.R")

dc_data <- list.files("data", full.names = TRUE) |> 
  purrr::map_dfr(
    read.csv
  ) |> 
  select(
    "metric",
    "year", "quarter", "month", "frequency",
    "org", "org_name", "at_code",
    "numerator", "denominator", "value"
  )


