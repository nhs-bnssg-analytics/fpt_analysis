source("R/00_libraries.R")

read.csv("data/population.csv") |> 
  summarise(
    numerator = sum(numerator),
    .by = c(
      org, year
    )
  ) |> 
  mutate(
    year = as.integer(year)
  ) |> 
  arrange(org, year) |> 
  mutate(
    increase = numerator / lag(numerator),
    .by = org
  ) |> 
  ggplot(
    aes(
      x = year,
      y = increase
    )
  ) +
  geom_rect(
    aes(
      xmin = 2021L,
      xmax = 2022L,
      ymin = -Inf,
      ymax = Inf  
    ),
    fill = "gray50",
    alpha = 0.5
  ) +
  geom_line(
    aes(
      group = org
    )
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = 2013:2022#,
    # limits = c(2013, 2022)
  ) +
  facet_wrap(
    facets = vars(org),
    scales = "free_y"
  ) +
  labs(
    title = "Percentage increase in population from previous year for each ICB",
    subtitle = "2021 and 2022 populations are estimated from Local Authority data",
    x = "",
    y = "Percentage increase"
  )