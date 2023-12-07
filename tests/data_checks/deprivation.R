source("R/00_libraries.R")

df <- read.csv("data/deprivation.csv") |> 
  mutate(
    year = as.integer(year),
    metric = factor(
      metric,
      levels = c(
        "Proportion of resident population in national deprivation quintile (Least deprived quintile) - IMD 2019",
        "Proportion of resident population in national deprivation quintile (Second least deprived) - IMD 2019",
        "Proportion of resident population in national deprivation quintile (Average deprived) - IMD 2019", 
        "Proportion of resident population in national deprivation quintile (Second most deprived ) - IMD 2019", 
        "Proportion of resident population in national deprivation quintile (Most deprived) - IMD 2019"          
      )
    )
  ) |> 
  ggplot(
    aes(
      x = year,
      y = value
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
      group = interaction(org, metric),
      colour = metric
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )  + 
  guides(
    colour = guide_legend(ncol = 1)
  ) +
  scale_x_continuous(
    breaks = 2013:2022
  ) +
  scale_colour_manual(
    name = "",
    values = c(
      "#4575b4",
      "#91bfdb",
      "#d0d1e6",
      "#fee08b",
      "#d73027"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  facet_wrap(
    facets = vars(org)
  ) +
  labs(
    title = "Estimated proportion of the population in each deprivation quintile",
    subtitle = "2021 and 2022 populations are estimated from Local Authority data",
    x = "",
    y = "Proportion (%)"
  )

df
