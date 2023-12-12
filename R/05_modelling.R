source("R/00_libraries.R")
source("R/01_utils.R")
source("R/04_modelling_utils.R")


# select fields -----------------------------------------------------------

target_variable <- "Proportion of completed pathways greater than 18 weeks from referral (admitted)"


# random forest -----------------------------------------------------------

dc_data <- load_data(
  target_variable,
  value_type = "numerator"
) |> 
  select(
    all_of(c("org", "year", target_variable)),
    any_of(c("numerator", "remainder")),
    matches("^ESR|^Workforce|^Bed|age band|Year 6|GPPS|QOF")
  ) |> 
  dplyr::filter(
    # retain all rows where target variable is not na
    # retain all rows where we have population age group information
    if_all(
      all_of(
        c(target_variable, "Proportion of population in age band (80-89)")), ~ !is.na(.))
  ) |> 
  arrange(
    year, org
  )


rf_training_years <- set_names(
  2:9,
  nm = paste0("training_years_", 2:9)
  ) |> 
  map(
    ~ modelling_performance(
      data = dc_data,
      target_variable = target_variable,
      lagged_years = 1, 
      training_years = .x,
      keep_current = FALSE,
      remove_lag_target = TRUE,
      time_series_split = TRUE, 
      shuffle_training_records = TRUE,
      model_type = "random_forest", 
      seed = 321 
    )
  )

random_forest <- rf_training_years |> 
  map_df(
    ~ pluck(.x, "evaluation_metrics"),
    .id = "yrs"
  ) |> 
  filter(
    .metric == "rsq"
  ) |> 
  pivot_longer(
    cols = c(train, validation, test),
    names_to = "data_type",
    values_to = "rsq"
  ) |> 
  mutate(
    yrs = str_extract(yrs, "[0-9]")
  ) |> 
  ggplot(
    aes(
      x = yrs,
      y = rsq
    )
  ) +
  geom_line(
    aes(
      group = data_type,
      colour = data_type
    )
  ) +
  theme_minimal() +
  scale_colour_manual(
    name = "Type",
    values = c(
      test = "#33a02c",
      validation = "#ff7f00",
      train = "#1f78b4"
    ),
    labels = c(
      test = "Test",
      validation = "Validation",
      train = "Train"
    ),
    breaks = c("train", "validation", "test"),
    drop = TRUE
  ) +
  labs(
    title = "Rsq for random forest for different numbers of training years",
    subtitle = "Only last years data used (without target variable), shuffled training data",
    caption = target_variable,
    x = "Number of years in training data",
    y = bquote(~R^2)
  )


ggsave(
  random_forest,
  filename = "tests/model_testing/random_forest_rtt_admitted_numerators.png",
  width = 8,
  height = 6,
  units = "in",
  bg = "white"
)


# logistic regression -----------------------------------------------------

# modelling proportions

dc_data <- load_data(
  target_variable, 
  incl_numerator_remainder = TRUE,
  value_type = "value") |> 
  select(
    all_of(c("org", "year", target_variable)),
    any_of(c("numerator", "remainder")),
    matches("^ESR|^Workforce|^Bed|age band|Year 6|GPPS|QOF")
  ) |> 
  dplyr::filter(
    # retain all rows where target variable is not na
    # retain all rows where we have population age group information
    if_all(
      all_of(
        c(target_variable, "Proportion of population in age band (80-89)")), ~ !is.na(.))
  ) |> 
  arrange(
    year, org
  )

inputs <- expand.grid(
  corr = seq(from = 0.3, to = 0.9, by = 0.05),
  yrs = 2:6
)

logistic <- map2(
  .x = inputs$corr,
  .y = inputs$yrs,
  ~ modelling_performance(
    data = dc_data,
    target_variable = target_variable,
    lagged_years = 1, 
    remove_lag_target = TRUE,
    shuffle_training_records = TRUE,
    keep_current = FALSE,
    time_series_split = TRUE, 
    model_type = "logistic_regression", 
    linear_correlation_threshold = .x,
    seed = 321 ,
    training_years = .y,
    predict_proportions = TRUE
  )
)

logistic_results <- logistic |> 
  map_df(
    ~ pluck(.x, "evaluation_metrics")
  ) |> 
  filter(
    .metric == "rsq"
  ) |> 
  bind_cols(inputs) |> 
  pivot_longer(
    cols = c(train, validation, test),
    names_to = "data_type",
    values_to = "rsq"
  ) |> 
  mutate(
    yrs = paste(
      yrs, "years"
    )
  ) |> 
  ggplot(
    aes(
      x = corr,
      y = rsq
    )
  ) +
  geom_line(
    aes(
      group = data_type,
      colour = data_type
    )
  ) +
  theme_minimal() +
  facet_wrap(
    facets = vars(yrs)
  ) +
  ylim(0,1) +
  scale_colour_manual(
    name = "Type",
    values = c(
      test = "#33a02c",
      validation = "#ff7f00",
      train = "#1f78b4"
    ),
    labels = c(
      test = "Test",
      validation = "Validation",
      train = "Train"
    ),
    breaks = c("train", "validation", "test"),
    drop = TRUE
  ) +
  labs(
    title = "Rsq for logistic regression for different values of correlation threshold and number of training years",
    subtitle = "Only last years data used (without target variable), shuffled training data",
    caption = target_variable,
    x = "Correlation threshold",
    y = bquote(~R^2)
  )

ggsave(
  logistic_results,
  filename = "tests/model_testing/logistic_regression_RTT_admitted.png",
  width = 10,
  height = 8,
  units = "in",
  bg = "white"
)

# modelling numerators

dc_data <- load_data(
  target_variable, 
  incl_numerator_remainder = TRUE,
  value_type = "numerator") |> 
  select(
    all_of(c("org", "year", target_variable)),
    any_of(c("numerator", "remainder")),
    matches("^ESR|^Workforce|^Bed|age band|Year 6|GPPS|QOF")
  ) |> 
  dplyr::filter(
    # retain all rows where target variable is not na
    # retain all rows where we have population age group information
    if_all(
      all_of(
        c(target_variable, "Proportion of population in age band (80-89)")), ~ !is.na(.))
  ) |> 
  arrange(
    year, org
  )

inputs <- expand.grid(
  corr = seq(from = 0.3, to = 0.9, by = 0.05),
  yrs = 2:6
) |> 
  slice(37, 24)

logistic <- map2(
  .x = inputs$corr,
  .y = inputs$yrs,
  ~ modelling_performance(
    data = dc_data,
    target_variable = target_variable,
    lagged_years = 1, 
    remove_lag_target = TRUE,
    shuffle_training_records = TRUE,
    keep_current = FALSE,
    time_series_split = TRUE, 
    model_type = "logistic_regression", 
    linear_correlation_threshold = .x,
    seed = 321 ,
    training_years = .y,
    predict_proportions = FALSE
  )
)

logistic_results <- logistic |> 
  map_df(
    ~ pluck(.x, "evaluation_metrics")
  ) |> 
  filter(
    .metric == "rsq"
  ) |> 
  bind_cols(inputs) |> 
  pivot_longer(
    cols = c(train, validation, test),
    names_to = "data_type",
    values_to = "rsq"
  ) |> 
  mutate(
    yrs = paste(
      yrs, "years"
    )
  ) |> 
  ggplot(
    aes(
      x = corr,
      y = rsq
    )
  ) +
  geom_line(
    aes(
      group = data_type,
      colour = data_type
    )
  ) +
  theme_minimal() +
  facet_wrap(
    facets = vars(yrs)
  ) +
  ylim(0,1) +
  scale_colour_manual(
    name = "Type",
    values = c(
      test = "#33a02c",
      validation = "#ff7f00",
      train = "#1f78b4"
    ),
    labels = c(
      test = "Test",
      validation = "Validation",
      train = "Train"
    ),
    breaks = c("train", "validation", "test"),
    drop = TRUE
  ) +
  labs(
    title = "Rsq for logistic regression for different values of correlation threshold and number of training years",
    subtitle = "Only last years data used (without target variable), shuffled training data",
    caption = target_variable,
    x = "Correlation threshold",
    y = bquote(~R^2)
  )

ggsave(
  logistic_results,
  filename = "tests/model_testing/logistic_regression_RTT_admitted_numerators.png",
  width = 10,
  height = 8,
  units = "in",
  bg = "white"
)

# linear regression -------------------------------------------------------

dc_data <- load_data(
  target_variable, 
  incl_numerator_remainder = FALSE,
  value_type = "numerator") |> 
  select(
    all_of(c("org", "year", target_variable)),
    any_of(c("numerator", "remainder")),
    matches("^ESR|^Workforce|^Bed|age band|Year 6|GPPS|QOF")
  ) |> 
  dplyr::filter(
    # retain all rows where target variable is not na
    # retain all rows where we have population age group information
    if_all(
      all_of(
        c(target_variable, "Proportion of population in age band (80-89)")), ~ !is.na(.))
  ) |> 
  arrange(
    year, org
  )

inputs <- expand.grid(
  corr = seq(from = 0.3, to = 0.9, by = 0.05),
  yrs = 2:6
)

linear <- map2(
  .x = inputs$corr,
  .y = inputs$yrs,
  ~ modelling_performance(
    data = dc_data,
    target_variable = target_variable,
    lagged_years = 1, 
    remove_lag_target = TRUE,
    shuffle_training_records = TRUE,
    keep_current = FALSE,
    time_series_split = TRUE, 
    model_type = "linear", 
    linear_correlation_threshold = .x,
    seed = 321 ,
    training_years = .y,
    predict_proportions = TRUE
  )
)

linear_results <- linear |> 
  map_df(
    ~ pluck(.x, "evaluation_metrics")
  ) |> 
  filter(
    .metric == "rsq"
  ) |> 
  bind_cols(inputs) |> 
  pivot_longer(
    cols = c(train, validation, test),
    names_to = "data_type",
    values_to = "rsq"
  ) |> 
  mutate(
    yrs = paste(yrs, "years")
  ) |> 
  ggplot(
    aes(
      x = corr,
      y = rsq
    )
  ) +
  geom_line(
    aes(
      group = data_type,
      colour = data_type
    )
  ) +
  theme_minimal() +
  facet_wrap(
    facets = vars(yrs)
  ) +
  scale_colour_manual(
    name = "Type",
    values = c(
      test = "#33a02c",
      validation = "#ff7f00",
      train = "#1f78b4"
    ),
    labels = c(
      test = "Test",
      validation = "Validation",
      train = "Train"
    ),
    breaks = c("train", "validation", "test"),
    drop = TRUE
  ) +
  ylim(0,1) +
  labs(
    title = "Rsq for linear regression for different values of correlation threshold and number of training years",
    subtitle = "Only last years data used (without target variable), shuffled training data",
    caption = target_variable,
    x = "Correlation threshold",
    y = bquote(~R^2)
  )

ggsave(
  linear_results,
  filename = "tests/model_testing/linear_regression_RTT_admitted_numerators.png",
  width = 10,
  height = 8,
  units = "in",
  bg = "white"
)


