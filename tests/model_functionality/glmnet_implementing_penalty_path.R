# inspired by https://www.tidymodels.org/learn/models/coefficients/#:~:text=Tuning%20a%20glmnet%20model,tidymodels%20tune_*()%20functions

source("R/00_libraries.R")

set.seed(1234)

df <- readRDS("tests/model_testing/best_performing_logistic.rds")
target_variable <- "Proportion of incomplete pathways greater than 18 weeks from referral (incomplete)"
predictor_variables <- names(df)[ !(names(df) %in% c(target_variable, "total_cases"))]

# splitting
splits <- rsample::initial_validation_split(
  data = df,
  prop = c(0.6, 0.2)
) 

data_train <- rsample::training(splits)
data_validation <- rsample::validation(splits)
data_test <- rsample::testing(splits)


validation_set <- vfold_cv(
  bind_rows(
    data_train,
    data_validation
  ),
  v = 4,
  strata = all_of(target_variable)
)


# model setup
path_length <- 150
pen_vals <- 10 ^ seq(-3, 0, length.out = path_length)
tuning_grid <- crossing(
  penalty = pen_vals[seq_len(path_length) %% 30 == 0], 
  mixture = seq(
    from = 0.05,
    to = 1.0,
    length.out = 10
  ))

cores <- parallel::detectCores()
model_setup <- parsnip::linear_reg(
  penalty = tune(),
  mixture = tune()
) |> 
  set_engine(
    "glmnet", 
    family = stats::quasibinomial(link = "logit"), 
    path_values = pen_vals,
    num.threads = cores
  ) |> 
  set_mode("regression")


# recipe
model_recipe <- data_train |> 
  recipe() |> 
  update_role(
    all_of(target_variable),
    new_role = "outcome"
  ) |> 
  update_role(
    all_of(predictor_variables),
    new_role = "predictor"
  ) |> 
  prep(training = data_train, retain = TRUE)

# workflow

modelling_workflow <- workflow() |> 
  add_model(model_setup) |> 
  add_recipe(model_recipe) |> 
  add_case_weights(total_cases)

# modelling
evaluation_metrics <- metric_set(
  yardstick::rmse,
  yardstick::rsq,
  yardstick::mae,
  yardstick::mape,
  yardstick::smape)

# obtain resample coefficients function
get_glmnet_coefs <- function(x) {
  x %>% 
    extract_fit_engine() %>% 
    tidy(return_zeros = TRUE) %>% 
    rename(penalty = lambda)
}

# tuning
residuals <- modelling_workflow |> 
  tune_grid(
    resamples = validation_set,
    grid = tuning_grid,
    metrics = evaluation_metrics,
    control = control_grid(
      save_pred = TRUE,
      extract = get_glmnet_coefs
      )
    )
  
best <- residuals |> 
  select_best(metric = "rmse")

collect_metrics(residuals) |> 
  View()

# collate coefficients
glmnet_coefs <- 
  residuals %>% 
  select(id, .extracts) %>% 
  unnest(.extracts) %>% 
  select(id, mixture, .extracts) %>% 
  slice(1,
        .by = c(mixture, id)) %>%   # │ Remove the redundant results
  unnest(.extracts)
