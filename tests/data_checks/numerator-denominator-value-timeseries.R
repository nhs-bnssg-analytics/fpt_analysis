# This script plots a time series of all the numerators to sense check them

source("R/00_libraries.R")
source("R/01_utils.R")
source("R/04_modelling_utils.R")

metrics <- read.csv(
  here::here("data/configuration-table.csv"),
  encoding = "latin1"
) |> 
  filter(!(status %in% c("incorrect geography", "remove"))) |> 
  select(
    "metric", 
    "numerator_description",
    "denominator_description"
  )

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
    "year", "org", "metric", "numerator", "denominator", "value"
  ) |> 
  mutate(
    year = as.integer(year)
  ) #|> 
  # pivot_longer(
  #   cols = c(numerator, denominator),
  #   names_to = "type",
  #   values_to = "value"
  # )

unique_metrics <- unique(dc_data$metric)

for (i in c("numerator", "denominator", "value")) {
  pdf(
    paste0("tests/data_checks/", i, "-time-series.pdf"),
    width = 12,
    height = 8
  )
  for (met in unique_metrics) {
    p <- dc_data |> 
      rename(
        chart_value = all_of(i)
      ) |> 
      filter(
        metric == met
      ) |> 
      ggplot(
        aes(x = year)
      ) +
      geom_line(
        aes(
          y = chart_value,
          group = org
        )
      ) +
      geom_point(
        aes(y = chart_value)
      ) +
      facet_wrap(
        facets = vars(org)
      ) +
      theme_minimal() +
      scale_x_continuous(
        expand = expansion(0),
        breaks = function(x) seq(x[1], x[2], by = 1)
      ) +
      labs(
        title = met,
        y = "",
        x = ""
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    print(p)
  }
  dev.off()
}

