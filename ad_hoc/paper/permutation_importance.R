# important variables plot

source("R/00_libraries.R")
library(tidytext)

final_models <- "outputs/model_objects/wfs_best_mape_pi.rds"

model_types <- readRDS(final_models) |> 
  map(
    ~ pluck(.x, "wf","pre", "actions", "recipe", "recipe", "template")
  ) |> 
  list_rbind(
    names_to = "performance_metric"
  ) |>
  filter(!grepl("52", performance_metric)) |> 
  select(!any_of(c("total_cases", "month", "quarter", "year", "nhs_region", "org"))) |>
  summarise(
    across(everything(),
           ~ sum(.x < 0, na.rm = TRUE)),
    .by = performance_metric
  ) |> 
  pivot_longer(
    cols = !("performance_metric"),
    names_to = "metric",
    values_to = "number_negative"
  ) |> 
  summarise(
    type = sum(number_negative),
    .by = performance_metric
  ) |> 
  mutate(
    type = case_when(
      type > 0 ~ "change",
      .default = "proportion"
    )
  )

plot_perm_imp <- function(model_types, type_filter, final_models) {
  p <- readRDS(final_models) |> 
    map(
      ~ pluck(.x, "perm_imp")
    ) |> 
    list_rbind(
      names_to = "performance_metric"
    ) |> 
    arrange(
      performance_metric, desc(Importance)
    ) |> 
    slice_head(
      n = 5,
      by = performance_metric
    ) |> 
    left_join(
      model_types,
      by = join_by(
        performance_metric
      ),
      relationship = "many-to-one"
    ) |> 
    filter(
      type == type_filter
    ) |> 
    mutate(
      Variable = stringr::str_wrap(Variable, 40),
      performance_metric = stringr::str_wrap(performance_metric, 30)
    ) |> 
    ggplot(
      aes(
        x = Importance,
        y = tidytext::reorder_within(
          Variable, Importance, performance_metric
        )
      )
    ) +
    geom_col(
      fill = NA,
      colour = "black"
    ) +
    geom_errorbarh(
      aes(
        xmin = Importance - StDev,
        xmax = Importance + StDev
      ),
      height = 0.2
    ) +
    facet_wrap(
      facets = vars(performance_metric),
      scales = "free",
      ncol = 2
    ) +
    scale_y_reordered() +
    theme_bw() + 
    scale_x_continuous(
      expand = expansion(
        mult = c(0, 0.1)
      )
    ) +
    coord_cartesian(
      xlim = c(0, NA)
    ) +
    # xlim(0, NA) +
    labs(
      y = "",
      x = "Mean increase in MAPE from 10 simulations"
    ) +
    theme(
      axis.ticks.y = element_blank()
    )
  
  return(p)
}

prop <- plot_perm_imp(
  model_types = model_types,
  type_filter = "proportion",
  final_models = final_models
)
debugonce(plot_perm_imp)
change <- plot_perm_imp(
  model_types = model_types,
  type_filter = "change",
  final_models = final_models
)



ggsave(
  plot = prop,
  "ad_hoc/paper/images/permutation_importance_proportion.png",
  width = 7,
  height = 3.5,
  units = "in",
  bg = "white"
)
ggsave(
  plot = change,
  "ad_hoc/paper/images/permutation_importance_change.png",
  width = 10,
  height = 7,
  units = "in",
  bg = "white"
)
