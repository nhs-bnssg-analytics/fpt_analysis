source("R/00_libraries.R")
model_summary <- readRDS("tests/model_testing/model_summary_information.rds")

long_data <- model_summary |> 
  filter(
    `Tuning objective` == "mape"#,
    # !grepl("52", `Target variable`)
  ) |> 
  pivot_longer(
    cols = c(`Test set value`, starts_with("Baseline")),
    names_to = "scenario",
    values_to = "mape"
  ) |>
  mutate(
    count_of_models = n(),
    .by = c(`Target variable`, `Model type`, `Target variable type`)
  ) |> 
  mutate(
    `Target variable detailed` = str_wrap(`Target variable`, 25),
    `Target variable broad` = str_wrap(`Target variable`, 40),
    `Number lagged target years` = gsub(" lagged years", "", `Number lagged target years`),
    `Years in dataset` = `Number training years` + 1,
    `Model type` = str_to_sentence(gsub("_", " ", `Model type`)),
    `Model type` = case_when(
      `Model type` == "Logistic regression" ~ "GLM",
      `Model type` == "Random forest" ~ "RF",
      .default = "unknown"
    ),
    `Target variable type` = case_when(
      `Target variable type` == "proportion" ~ "(proportion)",
      .default = "(CiP)"
    ),
    scenario_broad = paste0(
      `Target variable type`,
      " (n=",
      count_of_models,
      ")"
    ),
    scenario = case_when(
      grepl("linear", scenario) ~ "Baseline - 3 year linear extrapolation",
      grepl("same as last year", scenario) ~ "Baseline - same value as last year",
      .default = scenario
    )
  ) |> 
  select(
    "Target variable detailed",
    "Target variable broad",
    "Number lagged target years",
    "Years in dataset",
    "Model type",
    "Target variable type",
    "mape",
    "Number lagged years",
    "scenario",
    "scenario_broad"
  )


detailed_plot_data <- long_data |> 
  mutate(
    scenario = case_when(
      `Number lagged target years` == 1 &
        scenario == "Test set value" ~ "Test set value (with lagged target value incl. as predictor)",
      .default = scenario
    ),
    facet = str_wrap(
      paste(
        `Model type`, `Target variable type`
      ),
      30
    ),
    facet = factor(
      facet,
      levels = c(
        "GLM (proportion)",
        "RF (proportion)",
        "RF (CiP)"
      )
    ),
    x_axis = paste0(
      `Years in dataset`, 
      "[",
      `Number lagged years`,
      "]"
    )
  )

x_breaks <- detailed_plot_data |> 
  pull(
    x_axis
  ) |> 
  unique()

x_labels <- x_breaks |> 
  sapply(
    function(x) parse(text = x)[[1]]
  )

detailed_plot <- detailed_plot_data |> 
  ggplot(
    aes(
      y = mape,
      x = x_axis
    )
  ) +
  geom_point(
    aes(
      shape = scenario,
      size = scenario,
      colour = scenario
    ),
    fill = NA,
  ) +
  geom_line(
    aes(
      group = interaction(`Years in dataset`, scenario),
      linetype = scenario,
      colour = scenario
    ),
    linewidth = 1
  ) +
  scale_shape_manual(
    name = "",
    values = c(
      "Baseline - same value as last year" = 15,
      "Baseline - 3 year linear extrapolation" = 15,
      "Test set value" = 16,
      "Test set value (with lagged target value incl. as predictor)" = 21
    )
  ) +
  scale_linetype_manual(
    name = "",
    values = c(
      "Baseline - same value as last year" = "solid",
      "Baseline - 3 year linear extrapolation" = "solid",
      "Test set value" = NA,
      "Test set value (with lagged target value incl. as predictor)" = NA
    )
  ) +
  scale_size_manual(
    name = "",
    values = c(
      "Baseline - same value as last year" = 0.8,
      "Baseline - 3 year linear extrapolation" = 0.8,
      "Test set value" = 1,
      "Test set value (with lagged target value incl. as predictor)" = 1
    )
  ) +
  scale_colour_manual(
    name = "",
    values = c(
      "Baseline - same value as last year" = "gray45",
      "Baseline - 3 year linear extrapolation" = "gray75",
      "Test set value" = "black",
      "Test set value (with lagged target value incl. as predictor)" = "black"
    )
  ) +
  scale_x_discrete(
    breaks = x_breaks,
    labels = x_labels
  ) +
  facet_grid(
    rows = vars(`Target variable detailed`),
    cols = vars(facet),
    scales = "free",
    axes = "all", 
    axis.labels = "all_x"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    y = "Mean absolute percentage error",
    x = expression(`Years in dataset`[`Number of lagged years`])
  ) +
  scale_y_log10()

ggsave(
  plot = detailed_plot,
  "ad_hoc/paper/images/model_validation.png",
  width = 12,
  height = 10,
  units = "in",
  bg = "white"
)


boxplot_mape_results <- function(data) {
  plot <- data |> 
    ggplot(
      aes(x = scenario,
          y = mape)
    ) +
    geom_boxplot() +
    facet_wrap(
      facets = vars(`Target variable broad`),
      scales = "free_x"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        size = 7
      ),
      legend.position = "bottom"
    ) +
    labs(
      y = "Mean absolute percentage error",
      x = ""
    )
  
  return(plot)
}

summary_plot_without_52rtt <- long_data |> 
  filter(
    !grepl("52", `Target variable detailed`)
  ) |> 
  mutate(
    scenario = case_when(
      scenario == "Test set value" ~ paste(`Model type`, scenario_broad,
                                           sep = " - "),
      .default = scenario
    ),
    scenario = gsub("\\) \\(", ", ", scenario),
    scenario = str_wrap(scenario, 20)
  ) |> 
  boxplot_mape_results()
  

summary_plot_52rtt <- long_data |> 
  filter(
    grepl("52", `Target variable detailed`)
  ) |> 
  mutate(
    scenario = case_when(
      scenario == "Test set value" ~ paste(`Model type`, scenario_broad,
                                           sep = " - "),
      .default = scenario
    ),
    scenario = gsub("\\) \\(", ", ", scenario),
    scenario = str_wrap(scenario, 20)
  ) |> 
  boxplot_mape_results() +
  scale_y_log10()

ggsave(
  plot = summary_plot_without_52rtt,
  "ad_hoc/paper/images/model_validation_broad_no_52rtt.png",
  width = 6,
  height = 6,
  units = "in",
  bg = "white"
)

ggsave(
  plot = summary_plot_52rtt,
  "ad_hoc/paper/images/model_validation_broad_52rtt.png",
  width = 3,
  height = 3,
  units = "in",
  bg = "white"
)
