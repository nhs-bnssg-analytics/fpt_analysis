source("R/00_libraries.R")
library(magrittr)

metrics <- c(
  "Learning disability: QOF prevalence (all ages)",
  "Proportion of completed pathways greater than 18 weeks from referral (admitted)"
)

metric_to_numerator <- function(metric_name) {
  read.csv("data/configuration-table.csv") |> 
    filter(
      metric == metric_name
    ) |> 
    pull(numerator_description)
}

df <- c(
  "data/risk-factors-fingertips.csv",
  "data/referral-to-treatment.csv"
) |> 
  map_df(
    read.csv
  ) |> 
  filter(
    metric %in% metrics,
    frequency == "annual financial",
    year != 2023
  ) |> 
  mutate(
    year = as.integer(year)
  ) |> 
  arrange(
    metric, org, year
  ) |> 
  group_by(
    metric, org
  ) |> 
  group_split() |> 
  map_df(
    .f = ~ mutate(.x, numerator = case_when(
      metric == "Learning disability: QOF prevalence (all ages)" ~ lag(numerator),
      .default = numerator)
    )
  )

df |> 
  mutate(
    metric = map_chr(metric, metric_to_numerator)
  ) |> 
  ggplot(
    aes(
      x = year,
      y = numerator
    )
  ) +
  geom_line(
    aes(
      group = metric,
      colour = metric
    )
  ) +
  theme_minimal() +
  facet_wrap(
    facets = vars(org),
    scales = "free_y"
  ) +
  theme(
    legend.position = "bottom"
  )

df |> 
  filter(
    year > (max(year) - 4)
  ) |> 
  select(
    all_of(
      c(
        "year",
        "org",
        "metric",
        "numerator"
      )
    )
  ) |> 
  pivot_wider(
    names_from = metric,
    values_from = numerator
  ) |> 
  filter(
    if_all(
      all_of(metrics),
      .fns = ~ !is.na(.)
    )
  ) |> 
  ggplot(
    aes(
      x = `Learning disability: QOF prevalence (all ages)`,
      y = `Proportion of completed pathways greater than 18 weeks from referral (admitted)`
    )
  ) +
  geom_point(
    aes(
      colour = factor(year)
    )
  ) +
  theme_minimal() +
  labs(
    x = metric_to_numerator("Learning disability: QOF prevalence (all ages)"),
    y = metric_to_numerator("Proportion of completed pathways greater than 18 weeks from referral (admitted)")
  )


df |> 
  filter(
    year > (max(year) - 4)
  ) |> 
  select(
    all_of(
      c(
        "year",
        "org",
        "metric",
        "numerator"
      )
    )
  ) |> 
  pivot_wider(
    names_from = metric,
    values_from = numerator
  ) |> 
  filter(
    if_all(
      all_of(metrics),
      .fns = ~ !is.na(.)
    )
  ) %$%
  cor(
    `Learning disability: QOF prevalence (all ages)`,
    `Proportion of completed pathways greater than 18 weeks from referral (admitted)` 
  )
