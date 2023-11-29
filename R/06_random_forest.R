source("R/00_libraries.R")
source("R/01_utils.R")
source("R/03_collating_data.R")
source("R/04_modelling_utils.R")


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
  )

names(dc_data)

rowSums(is.na(dc_data))
colSums(is.na(dc_data)) |> 
  tibble::enframe()



# RF stage ----------------------------------------------------------------

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

# how many cores on the machine so we can parallelise
cores <- parallel::detectCores()

# set model
rf_mod <- 
  rand_forest(
    mtry = tune(), 
    min_n = tune(), 
    trees = 1000
  ) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("regression")

# create recipes
# interaction variables (year and variables?)

# impute missings?
# variables with missing data
missing_data <- names(dc_data)[colSums(is.na(dc_data)) > 0]

rf_recipe <- recipe(data_train) |> 
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
  step_impute_knn(all_of(missing_data)) |> 
  step_corr(all_predictors(),
            threshold = 0.6)
# %>%
# step_lag(matches("^ESR|^Workforce|^Bed|age band|Year 6|GPPS"),
#          lag = 1:2)


# add recipe to workflow
rf_workflow <- 
  workflow() |> 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

# show what will be tuned
extract_parameter_set_dials(rf_mod)


# design the tuning of the hyperparameters
rf_res <- 
  rf_workflow %>% 
  tune_grid(data_validation_set,
            grid = 50,
            control = control_grid(save_pred = TRUE))

# show the best parameters
rf_res %>% 
  show_best(metric = "rsq")


autoplot(rf_res)

# select the best parameters
rf_best <- 
  rf_res %>% 
  select_best(metric = "rsq")


# the last model
last_rf_mod <- 
  rand_forest(mtry = rf_best$mtry, min_n = rf_best$min_n, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("regression")

# the last workflow
last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(splits)


# plot predictions vs observed --------------------------------------------

# fit a model to the training data
rf_fit <- list(
  train = data_train
) |> 
  bind_rows(
    .id = "data_type"
  ) |> 
  fit(
    data = _,
    object = last_rf_workflow
  )

# view this fitted model on estimates for the training, validation (and test) data
list(
  train = data_train,
  validation = data_validation,
  test = data_test
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
    model_fit = rf_fit
  ) |> 
  plot_observed_expected(
    target_variable
  )

# evaluation metrics on validation data -----------------------------------

list(
  train = data_train,
  validation = data_validation,
  test = data_test
) |> 
  purrr::map_dfr(
    ~ linear_model_metrics(
      data = .x,
      model_fit = rf_fit
    ),
    .id = "data"
  ) |> 
  tidyr::pivot_wider(
    names_from = data,
    values_from = .estimate
  )


last_rf_fit %>% 
  collect_metrics()

last_rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vip(num_features = 10)
