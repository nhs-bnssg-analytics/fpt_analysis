source("R/00_libraries.R")
source("R/01_utils.R")
source("R/04_modelling_utils.R")


# select fields -----------------------------------------------------------

target_variable <- "Proportion of incomplete pathways greater than 18 weeks from referral (incomplete)"
model_value_type <- "value"
predict_year <- 2022

# random forest -----------------------------------------------------------
dc_data <- load_data(
  target_variable,
  value_type = model_value_type
) |> 
  select(
    any_of(c("org", "year", target_variable)),
    any_of(c("numerator", "remainder")),
    matches("^FTE|^Workforce|^Bed|bed days|beds|age band|deprivation|Year 6|obese|GPPS|QOF")
  ) |> 
  dplyr::filter(
    # retain all rows where target variable is not na
    # retain all rows where we have population age group information
    if_all(
      c(
        all_of(c(target_variable)), 
        contains("age band")
      ), ~ !is.na(.)
    )
  ) |> 
  arrange(
    year, org
  ) |> 
  filter(
    year <= predict_year
  )

if (model_value_type == "numerator") {
  new_names <- names(dc_data) |> 
    (\(x) ifelse(x %in% c("year", "org"), x, map_chr(.x = x, .f = metric_to_numerator)))()
  
  names(dc_data) <- new_names
  
  target_variable <- metric_to_numerator(target_variable)
}

inputs <- expand.grid(
  training_years = 2:6,
  lagged_years = 0:2
)

rf_modelling_outputs <- map2(
  .x = inputs$training_years,
  .y = inputs$lagged_years,
  ~ modelling_performance(
    data = dc_data,
    target_variable = target_variable,
    lagged_years = .y, 
    training_years = .x,
    keep_current = FALSE,
    remove_lag_target = TRUE,
    time_series_split = TRUE, 
    shuffle_training_records = TRUE,
    model_type = "random_forest", 
    seed = 321 
  )
)

random_forest <- rf_modelling_outputs |> 
  map_df(
    ~ pluck(.x, "evaluation_metrics")
  ) |> 
  filter(
    .metric == "rsq"
  ) |> 
  bind_cols(
    inputs
  ) |> 
  mutate(
    lagged_years = paste(
      lagged_years,
      "lagged years included in training data"
    )
  ) |> 
  pivot_longer(
    cols = c(
      train,
      validation, 
      test
    ),
    names_to = "data_type",
    values_to = "rsq"
  ) |> 
  ggplot(
    aes(
      x = training_years,
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
  facet_wrap(
    facets = vars(lagged_years),
    ncol = 1
  ) +
  labs(
    title = "Rsq for random forest for different length years and different amounts of lagging in training data",
    subtitle = "Only previous years data used (without target variable), shuffled training data",
    caption = target_variable,
    x = "Number of years in training data",
    y = bquote(~R^2)
  )


ggsave(
  random_forest,
  filename = paste0(
    "tests/model_testing/random_forest_rtt_admitted_", 
    model_value_type, 
    "_predicting_", 
    predict_year, 
    ".png"
  ),
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
  value_type = "value",
  include_weights = TRUE
  ) |> 
  select(
    any_of(c("org", "year", target_variable)),
    any_of(c("numerator", "remainder", "health_population")),
    matches("^FTE|^Workforce|^Bed|bed days|beds|age band|deprivation|Year 6|obese|GPPS|QOF")
  ) |> 
  dplyr::filter(
    # retain all rows where target variable is not na
    # retain all rows where we have population age group information
    if_all(
      c(
        all_of(c(target_variable)), 
        contains("age band")
      ), ~ !is.na(.)
    )
  ) |> 
  arrange(
    year, org
  ) |> 
  filter(
    year <= predict_year
  )

inputs <- expand.grid(
  lagged_years = 0:2,
  training_years = 2:6,
  correlation_threshold = seq(
    from = 0.5,
    to = 0.9,
    length.out = 5
  )
) |> 
  slice(-(63))

custom_logistic_modelling <- function(lagged_years, correlation_threshold, training_years) {
  modelling_performance(
    data = dc_data,
    target_variable = target_variable,
    lagged_years = lagged_years, 
    remove_lag_target = TRUE,
    shuffle_training_records = TRUE,
    keep_current = FALSE,
    time_series_split = TRUE, 
    model_type = "logistic_regression", 
    correlation_threshold = correlation_threshold,
    seed = 321 ,
    training_years = training_years,
    predict_proportions = TRUE
  )
}

logistic <- inputs |> 
  pmap(
    custom_logistic_modelling
  )

logistic_results <- logistic |> 
  map_df(
    ~ pluck(.x, "evaluation_metrics")
  ) |> 
  filter(
    .metric == "rsq"
  ) |> 
  bind_cols(inputs) |> 
  mutate(
    lagged_years = paste(
      lagged_years,
      "lagged years"
    ),
    training_years = paste(
      training_years,
      "training years"
    )
  ) |> 
  pivot_longer(
    cols = c(train, validation, test),
    names_to = "data_type",
    values_to = "rsq"
  ) |> 
  ggplot(
    aes(
      x = correlation_threshold,
      y = rsq
    )
  ) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed"
  ) +
  geom_line(
    aes(
      group = data_type,
      colour = data_type
    )
  ) +
  theme_minimal() +
  facet_grid(
    cols = vars(training_years),
    rows = vars(lagged_years)
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
    title = "Rsq for logistic regression where correlation thresholds, lagged years and training years were applied",
    subtitle = "Only previous years data used (without target variable), shuffled training data",
    caption = target_variable,
    x = "Correlation threshold",
    y = bquote(~R^2)
  )

ggsave(
  logistic_results,
  filename = paste0(
    "tests/model_testing/logistic_regression_rtt_admitted_", 
    model_value_type, 
    "_predicting_", 
    predict_year, 
    ".png"
  ),
  width = 10,
  height = 8,
  units = "in",
  bg = "white"
)

