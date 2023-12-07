library("R/00_libraries.R")

df <- read.csv("data/referral-to-treatment.csv") |> 
  filter(
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
      group = interaction(org, metric),
      colour = metric
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )  + 
  guides(
    colour = guide_legend(ncol = 1)
  ) +
  scale_colour_manual(
    name = "",
    values = c(
      "#4575b4",
      "#d73027"
    )
  ) +
  scale_x_continuous(
    breaks = 2013:2023
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  facet_wrap(
    facets = vars(org)
  ) +
  labs(
    title = "Referral to treatment metrics",
    x = "",
    y = "Proportion (%)"
  )
  
df
