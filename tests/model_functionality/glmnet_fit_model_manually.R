set.seed(1234)

# recreating best model
# the following penalty and mixture values created the best results from tuning
pen <- 0.0000535
mix <- 0.464

df <- readRDS("tests/model_testing/best_performing_logistic.rds")

splits <- rsample::initial_split(
  data = df
) 

data_train <- rsample::training(splits)
data_test <- rsample::testing(splits)

eng <- parsnip::linear_reg(
  penalty = pen,
  mixture = mix
) |> 
  set_engine(
    "glmnet", 
    family = stats::quasibinomial(link = "logit"), 
    nlambda = 200,
    num.threads = 12
  ) |> 
  set_mode("regression")

preds <- names(df)[!(names(df) %in% c("Proportion of incomplete pathways greater than 18 weeks from referral (incomplete)", "total_cases"))]
rec <- data_train |> 
  recipe() |> 
  update_role(
    all_of("Proportion of incomplete pathways greater than 18 weeks from referral (incomplete)"),
    new_role = "outcome"
  ) |> 
  update_role(
    all_of(preds),
    new_role = "predictor"
  ) |> 
  prep()

wf <- workflow() |> 
  add_model(eng) |> 
  add_recipe(rec) |> 
  add_case_weights(total_cases)

ft <- wf |> 
  fit(
    data = data_train
  )

evaluation_metrics <- metric_set(
  yardstick::rsq)


predict(
  ft,
  new_data = data_test
) |> 
  bind_cols(data_test) |> 
  select(all_of(
    c("Proportion of incomplete pathways greater than 18 weeks from referral (incomplete)",
      ".pred")
  )) |> 
  evaluation_metrics(
    truth = `Proportion of incomplete pathways greater than 18 weeks from referral (incomplete)`,
    estimate = .pred
  )

ft |> 
  tidy() |> 
  count(term, sort = TRUE) |> 
  mutate(term = factor(term, levels = rev(term))) |> 
  ggplot(
    aes(x = n, 
        y = term)
  ) +
  geom_col()

# COVID variables are important - this is worrying as for many years they should
# be 0 (or in this case they are centred and scaled)

df |> 
  select(
    contains("COVID")
  )

# understand whether all variables are used in the model when pure ridge regression occurs
mix <- 0
  
eng <- parsnip::linear_reg(
  penalty = pen,
  mixture = mix
) |> 
  set_engine(
    "glmnet", 
    family = stats::quasibinomial(link = "logit"), 
    nlambda = 200,
    num.threads = 12
  ) |> 
  set_mode("regression")


wf <- workflow() |> 
  add_model(eng) |> 
  add_recipe(rec) |> 
  add_case_weights(total_cases)

ft <- wf |> 
  fit(
    data = data_train
  )

ft |> 
  tidy() |> 
  count(term) |> 
  nrow()
 #316 rows (eg, width of df minus outcome and case weights plus intercept)

# how many vars are left when pure lasso?
mix <- 1

eng <- parsnip::linear_reg(
  penalty = pen,
  mixture = mix
) |> 
  set_engine(
    "glmnet", 
    family = stats::quasibinomial(link = "logit"), 
    nlambda = 200,
    num.threads = 12
  ) |> 
  set_mode("regression")


wf <- workflow() |> 
  add_model(eng) |> 
  add_recipe(rec) |> 
  add_case_weights(total_cases)

ft <- wf |> 
  fit(
    data = data_train
  )

ft |> 
  tidy() |> 
  count(term) |> 
  nrow()