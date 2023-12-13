source("R/00_libraries.R")
library(magrittr)

c(
  "data/risk-factors-fingertips.csv",
  "data/referral-to-treatment.csv"
) |> 
  map_df(
    read.csv
  ) |> 
  filter(
    metric %in% c(
      "Learning disability: QOF prevalence (all ages)",
      "Proportion of completed pathways greater than 18 weeks from referral (admitted)"
    ),
    frequency == "annual financial"
  ) |> 
  mutate(
    year = as.integer(year)
  ) |> 
  ggplot(
    aes(
      x = year,
      y = value
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
  )

metrics <- c(
  "Learning disability: QOF prevalence (all ages)",
  "Proportion of completed pathways greater than 18 weeks from referral (admitted)"
)

c(
  "data/risk-factors-fingertips.csv",
  "data/referral-to-treatment.csv"
) |> 
  map_df(
    read.csv
  ) |> 
  filter(
    metric %in% metrics,
    frequency == "annual financial"
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
  ) |> 
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
  theme_minimal()


c(
  "data/risk-factors-fingertips.csv",
  "data/referral-to-treatment.csv"
) |> 
  map_df(
    read.csv
  ) |> 
  filter(
    metric %in% metrics,
    frequency == "annual financial"
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
  ) |> 
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
