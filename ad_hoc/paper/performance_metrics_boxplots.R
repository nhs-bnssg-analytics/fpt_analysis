source("R/00_libraries.R")

metrics_config <- read.csv(
  "data/configuration-table.csv"
) |> 
  filter(
    domain == "Performance",
    status == "modelled"
  ) |> 
  select(metric)

perf_metrics <- list.files(
  "data",
  full.names = TRUE
) |> 
  (\(x) x[!grepl("configuration", x)])() |> 
  map(
    read.csv
  ) |> 
  purrr::list_rbind() |> 
  filter(
    grepl("annual", frequency)
  ) |> 
  inner_join(
    metrics_config,
    by = join_by(metric)
  )

ggplot(
  perf_metrics,
  aes(
    x = value / 100, 
    y = org
  )
) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(
    facets = vars(
      stringr::str_wrap(metric, 25)),
    ncol = 4,
    scales = "free_x"
  ) +
  scale_x_continuous(
    label = scales::percent
  ) +
  labs(
    x = "Proportion (%)",
    y = "Integrated Care System"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


ggsave(
  "outputs/performance_metrics_boxplots.png",
  width = 7,
  height = 7,
  units = "in",
  bg = "white"
)
