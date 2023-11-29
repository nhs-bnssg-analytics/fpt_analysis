source("R/00_libraries.R")
source("R/01_utils.R")
source("R/03_collating_data.R")
source("R/04_modelling_utils.R")


# select fields -----------------------------------------------------------

target_variable <- "Proportion of completed pathways greater than 18 weeks from referral (admitted)"

dc_data <- dc_data |> 
  select(
    all_of(c("org", "year", target_variable)),
    matches("^ESR|^Workforce|^Bed|age band|Year 6|GPPS")
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


# splitting data ----------------------------------------------------------

set.seed(321)

proportions <- train_validation_proportions(dc_data)

# split dataset into train, validation and test
splits <- rsample::initial_validation_time_split(
  data = dc_data,
  prop = proportions
)

data_train <- rsample::training(splits)
data_validation <- rsample::validation(splits)
data_test <- rsample::testing(splits)

# check when train, validation and test data start and finish
lapply(
  list(data_train, data_validation, data_test),
  function(x) range(x$year)
)

# create train and validation set for tuning hyperparameters
data_validation_set <- validation_set(splits)


# set model ---------------------------------------------------------------
linear_model <- parsnip::linear_reg(
  mode = "regression",
  engine = "lm"
)


# set recipe --------------------------------------------------------------
missing_data <- names(dc_data)[colSums(is.na(dc_data)) > 0]

linear_recipe <- recipe(data_train) |> 
  update_role(
    all_of(target_variable),
    new_role = "outcome"
  ) |> 
  update_role(
    org, year,
    new_role = "id variable"
  ) |> 
  update_role(
    matches("^ESR|^Workforce|^Bed|age band|Year 6|GPPS"),
    new_role = "predictor"
  ) |> 
  step_center(all_predictors()) |> 
  step_scale(all_predictors()) |> 
  step_impute_knn(all_of(missing_data)) |> 
  step_corr(all_predictors(),
            threshold = 0.9) |> 
  # estimate the means and standard deviations
  prep(training = data_train, retain = TRUE)


# create workflow ---------------------------------------------------------

linear_workflow <- workflow() |> 
  add_model(linear_model) |> 
  add_recipe(linear_recipe)


# fit model ---------------------------------------------------------------

linear_fit <- linear_workflow |> 
  fit(data = data_train)



# check linear assumptions ------------------------------------------------

linear_fit$fit$fit |> 
  performance::check_model()

# plot predictions vs observed --------------------------------------------
list(
  train = data_train,
  validation = data_validation
) |> 
  bind_rows(
    .id = "data_type"
  ) |> 
  mutate(
    data_type = factor(
      data_type,
      levels = c("test", "validation", "train")
    )
  ) |> 
  add_prediction_to_data(
    linear_fit
  ) |> 
  plot_observed_expected(
    target_variable
  )


# evaluation metrics on validation data -----------------------------------

list(
  train = data_train,
  validation = data_validation
) |> 
  purrr::map_dfr(
    ~ linear_model_metrics(
      data = .x,
      model_fit = linear_fit
    ),
    .id = "data"
  ) |> 
  tidyr::pivot_wider(
    names_from = data,
    values_from = .estimate
  )

# variable importance -----------------------------------------------------

linear_fit|> 
  extract_fit_parsnip() |> 
  vip::vip(num_features = 10)
