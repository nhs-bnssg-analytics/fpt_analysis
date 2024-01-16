# attempting glmnet for logistic modelling

source("R/00_libraries.R")
source("R/01_utils.R")
source("R/04_modelling_utils.R")


# select fields -----------------------------------------------------------

target_variable <- "Proportion of incomplete pathways greater than 18 weeks from referral (incomplete)"
model_value_type <- "value"
predict_year <- 2022

dc_data <- load_data(
  target_variable, 
  incl_numerator_remainder = TRUE,
  value_type = "value",
  include_weights = FALSE
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
  ) |> 
  mutate(
    total_cases = importance_weights(
      numerator + remainder
    )
  )

# splitting data
proportions <- train_validation_proportions(dc_data)
final_year <- dc_data |> 
  filter(year == max(year))

dc_data <- dc_data |> 
  filter(year != max(year)) |> 
  slice_sample(prop = 1) |> 
  bind_rows(final_year)

splits <- rsample::initial_validation_time_split(
  data = dc_data,
  prop = proportions
)

data_train <- rsample::training(splits)
data_validation <- rsample::validation(splits)
data_test <- rsample::testing(splits)

lapply(
  list(data_train, data_validation, data_test),
  function(x) range(x$year)
)

# create train and validation set for tuning hyperparameters
data_validation_set <- validation_set(splits)


model_setup <- parsnip::linear_reg(
  penalty = tune(),
  mixture = tune()
) |> 
  set_engine(
    "glmnet", 
    family = stats::binomial(link = "logit")
  ) |> 
  set_mode("regression")

# modelling numerator/remainder
predictors <- names(dc_data)[!grepl("total_cases|org|numerator|remainder", names(dc_data))] |> 
  (\(x) x[x != target_variable])()

vars_selection <- identify_missing_vars(
  data_train = data_train,
  data_validation = data_validation,
  data_test = data_test,
  model_type = "logistic_regression"
)

model_recipe_num_rem <- data_train |> 
  recipe() |> 
  step_rm(
    remainder, numerator
  ) |> 
  step_rm(
    vars_selection[["vars_to_remove"]]
  ) |> 
  update_role(
    all_of(predictors),
    new_role = "predictor"
  ) |> 
  update_role(
    all_of(target_variable), 
    new_role = "outcome"
  ) |> 
  step_impute_median(
    vars_selection[["vars_to_impute"]]
  ) |> 
  step_normalize(
    all_of(predictors[predictors != "year"])
  ) |> 
  prep(training = dc_data, retain = TRUE)


modelling_workflow_num_rem <- workflow() |> 
  add_model(model_setup) |> 
  add_recipe(model_recipe_num_rem) |> 
  add_case_weights(total_cases)

residuals <- modelling_workflow_num_rem |> 
  tune_grid(
    resamples = data_validation_set,
    grid = 20,
    control = control_grid(save_pred = TRUE)
  )

tuning_parameters <- autoplot(residuals)
