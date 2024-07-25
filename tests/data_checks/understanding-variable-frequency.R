# understanding which variables exist at different frequencies

source("R/00_libraries.R")

metrics <- read.csv(
  here::here("data/configuration-table.csv"),
  encoding = "latin1"
)


dc_data <- list.files(here::here("data"), full.names = TRUE) |> 
  (\(x) x[!grepl("configuration-table", x)])() |> 
  purrr::map_dfr(
    read.csv
  )


data_frequency <- dc_data |> 
  mutate(
    frequency = case_when(
      grepl("annual", frequency) ~ "annual",
      .default = frequency
    ),
    val = TRUE
  ) |> 
  distinct(
    metric, frequency, val
  ) |> 
  pivot_wider(
    names_from = frequency,
    values_from = val
  )

View(data_frequency)
