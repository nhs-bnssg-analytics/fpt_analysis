source("R/00_libraries.R")

df <- readRDS("tests/model_functionality/preprocessed_training_data.rds")
target_variable <- "Proportion of incomplete pathways greater than 18 weeks from referral (incomplete)"
val_set <- vfold_cv(
  df,
  v = 4,
  strata = all_of(target_variable)
)
cores <- parallel::detectCores()
m <- linear_reg(
  penalty = tune(),
  mixture = tune()
) |> 
  set_engine(
    "glmnet",
    family = quasibinomial(link = "logit"),
    nlambda = 600, # the default is 100 - increasing this number reduces the jump between lambda values and allows the models to converge better
    num.cores = cores
  ) |> 
  set_mode("regression")

rec <- df |> 
  recipe() |> 
  update_role(
    all_of(target_variable),
    new_role = "outcome"
  ) |> 
  update_role(
    !all_of(c(target_variable, "total_cases")),
    new_role = "predictor"
  )

wf <- workflow() |> 
  add_model(m) |> 
  add_recipe(rec) |> 
  add_case_weights(total_cases)

res <- wf |> 
  tune_grid(
    resamples = val_set,
    grid = 2,
    control = control_grid(save_pred = TRUE)
  )

collect_metrics(res)
