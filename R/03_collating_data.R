metrics <- read.csv(
  here::here("data/configuration-table.csv"),
  encoding = "latin1"
) |> 
  filter(!(status %in% c("incorrect geography", "remove"))) |> 
  select(metric)

dc_data <- list.files(here::here("data"), full.names = TRUE) |> 
  (\(x) x[!grepl("configuration-table|modelling_data", x)])() |> 
  purrr::map_dfr(
    read.csv
  ) |> 
  inner_join(
    metrics,
    by = join_by(metric)
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