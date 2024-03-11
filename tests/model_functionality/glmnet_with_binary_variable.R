source("R/00_libraries.R")

# this example shows that the same mode can be built using every combinations of
# tidymodels and the raw glm function, along with the outcome of a single value
# (the proportion of success) and an outcome of cbind(success, failure)

data(mtcars)


junk <- mtcars |> 
  mutate(
    mpg = round(mpg, 0),
    total = hp + mpg,
    prop = hp / total,
    total = frequency_weights(total),
    binary = case_when(
      mpg < 20 ~ 0,
      .default = 1
    )#,
    # binary = factor(binary)
  )

splits <- rsample::initial_validation_split(
  data = junk
)

train <- training(splits)

val_set <- validation_set(splits)

rec <- train |> 
  recipe() |> 
  update_role(
    prop, 
    new_role = "outcome"
  ) |> 
  update_role(
    cyl, disp, drat, wt, qsec, vs, am, gear, carb, binary,
    new_role = "predictor"
  ) |> 
  step_range(
    all_numeric_predictors(),
    clipping = FALSE
  ) |> 
  prep()

eng <- linear_reg(
  penalty = tune(),
  mixture = tune()
) |> 
  set_engine(
    "glmnet",
    family = stats::binomial(link = "logit")
  ) |> 
  set_mode("regression")

wf <- workflow() |> 
  add_recipe(rec) %>%
  add_model(eng) |> 
  add_case_weights(total)

evaluation_metrics <- metric_set(
  yardstick::rmse,
  yardstick::mae,
  yardstick::mape,
  yardstick::smape)

residuals <- wf |> 
  tune_grid(
    resamples = val_set,
    metrics = evaluation_metrics,
    control = control_grid(
      save_pred = TRUE
    )
  )



residuals |> 
  collect_predictions()

residuals |> 
  collect_metrics()
