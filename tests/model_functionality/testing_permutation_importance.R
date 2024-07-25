# attempting permutation importance for predictor variables

source("R/00_libraries.R")
source("R/01_utils.R")
source("R/04_modelling_utils.R")


# select fields -----------------------------------------------------------

target_variable <- "Proportion of incomplete pathways greater than 18 weeks from referral"
model_value_type <- "value"
predict_year <- 2022
model_method <- "logistic_regression"
eval_metric <- "mae"

dc_data <- load_data(
  target_variable,
  value_type = "value",
  incl_numerator_remainder = include_numerator_remainder(model_method),
  binary_covid = FALSE
) |> 
  select(
    any_of(c("org", "year", "nhs_region", target_variable)),
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
  ) |> 
  mutate(
    across(
      c(numerator, remainder),
      ~ as.integer(
        round(
          .x, 0
        )
      )
    ),
    total_cases = frequency_weights(numerator + remainder)
  ) |> 
  select(!c("numerator", "remainder"))

# splitting data (time series)
proportions <- c(0.6, 0.2)
splits <- rsample::initial_validation_split(
  data = dc_data,
  prop = proportions
) 

data_train <- rsample::training(splits)
data_validation <- rsample::validation(splits)
data_test <- rsample::testing(splits)

# create train and validation set for tuning hyperparameters
data_validation_set <- group_vfold_cv(
  bind_rows(
    data_train,
    data_validation
  ), 
  group = nhs_region
)


# set up modelling
cores <- parallel::detectCores()

model_setup <- parsnip::linear_reg(
  penalty = tune(),
  mixture = tune()
) |> 
  set_engine(
    "glmnet", 
    family = stats::quasibinomial(link = "logit"), 
    nlambda = 150,
    num.threads = cores,
    standardize = FALSE
  ) |> 
  set_mode("regression")

# modelling numerator/remainder
vars_selection <- identify_missing_vars(
  data_train = data_train,
  data_validation = data_validation,
  data_test = data_test,
  model_type = model_type
)

# identify predictor variables
predictor_variables <- names(dc_data)[!(names(dc_data) %in% c(
  "year", "quarter", "month",
  target_variable, "total_cases"))] |> 
  # remove variables that are mostly/entirely missing
  (\(x) x[!(x %in% vars_selection[["vars_to_remove"]])])()

# recipe
model_recipe <- data_train |> 
  recipe() |> 
  step_rm(
    any_of(vars_selection[["vars_to_remove"]])
  ) |> 
  update_role(
    all_of(target_variable),
    new_role = "outcome"
  ) |> 
  update_role(
    all_of(predictor_variables),
    new_role = "predictor"
  ) |> 
  step_impute_knn(
    all_of(vars_selection[["vars_to_impute"]])
  ) |>
  step_zv(
    all_numeric_predictors()
  ) |> 
  step_range(
    all_numeric_predictors(),
    clipping = FALSE
  )


modelling_workflow <- workflow() |> 
  add_model(model_setup) |> 
  add_recipe(model_recipe) |> 
  add_case_weights(total_cases)

# tuning
tuning_grid <- 30

evaluation_metrics <- metric_set(
  yardstick::mae
)

residuals <- modelling_workflow |> 
  tune_grid(
    resamples = data_validation_set,
    grid = tuning_grid,
    metrics = evaluation_metrics,
    control = control_grid(
      save_pred = TRUE
    )
  )

tuning_parameters <- autoplot(residuals)

# select the best parameters
best <- residuals |> 
  select_best(metric = eval_metric)

# the last workflow
modelling_workflow_final <- modelling_workflow |>
  finalize_workflow(best)

# the last fit
model_fit <- last_fit(
  modelling_workflow_final,
  splits,
  add_validation_set = TRUE,
  metrics = evaluation_metrics
)

lambda_penalty <- model_fit |> 
  extract_fit_parsnip() |> 
  pluck("spec", "args", "penalty")

debugonce(vip::vi_model)
model_specific_vi <- vip::vi(
  object = model_fit |> 
    extract_fit_parsnip(),
  lambda = lambda_penalty
  
)

pfun_reg <- function(object, newdata) {
  
  model_type <- object |> 
    class() |> 
    head(1)
  
  if (grepl("glm", model_type)) {
    lambda_penalty <- object |> 
      pluck("spec", "args", "penalty")
    
    pred_table <- predict(object, new_data = newdata, penalty = lambda_penalty)
  } else if (grepl("random", model_type)) {
    pred_table <- predict(object, new_data = newdata)
  }
  
  preds <- pred_table |> 
    pull(.pred)
  return(preds)
}
debugonce(pfun_reg)
pfun_reg(
  model_fit |> 
    extract_workflow() |> 
    extract_fit_parsnip(),
  newdata = data_test
)

permute_vi <- vip::vi(
  model_fit |> 
    extract_fit_parsnip(),
  feature_names = predictor_variables,
  method = "permute",
  train = data_test, # do variable permutation importance on the test data
  target = target_variable,
  metric = eval_metric,  
  pred_wrapper = pfun_reg,
  nsim = 30  # use 30 repetitions
)

junk <- permute_vi
