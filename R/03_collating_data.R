source("R/00_libraries.R")
source("R/01_utils.R")

dc_data <- list.files("data", full.names = TRUE) |> 
  (\(x) x[!grepl("configuration-table|modelling_data", x)])() |> 
  purrr::map_dfr(
    read.csv
  ) |> 
  filter(
    grepl("annual", frequency),
    grepl("^Q", org)
  ) |> 
  select(
    "metric",
    "year", 
    "org", 
    "value"
  ) |> 
  pivot_wider(
    names_from = metric,
    values_from = value
  )


write.csv(
  dc_data,
  "data/modelling_data.csv",
  row.names = FALSE
)
