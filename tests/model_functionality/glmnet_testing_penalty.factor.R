library(glmnet)
# Gaussian
x = as.data.frame(matrix(rnorm(100 * 20), 100, 20)) |> 
  tibble()
y = tibble(outcome = rnorm(100))
pen_val <- 0.01

penalise_1s <- ifelse(grepl("8", names(x)), 0.01, 1)


# standard method
fit_raw = glmnet(
  as.matrix(x),
  as.matrix(y),
  family = "gaussian",
  penalty.factor = penalise_1s
)

# tidymodels
rec <- bind_cols(y, x) |> 
  recipe() |> 
  update_role(
    all_of(names(y)),
    new_role = "outcome"
  ) |> 
  update_role(
    all_of(names(x)),
    new_role = "predictor"
  )

model_setup <- parsnip::linear_reg(
  penalty = pen_val
) |> 
  set_engine(
    "glmnet", 
    family = gaussian,
    penalty.factor = penalise_1s
  ) |> 
  set_mode("regression")

modelling_workflow <- workflow() |> 
  add_model(model_setup) |> 
  add_recipe(rec)

fit_tidy <- fit(modelling_workflow, 
    bind_cols(x, y))


predict(fit_raw, newx = as.matrix(x),
        s = pen_val) |> 
  as_tibble() |> 
  rename(.pred_raw = s1) |> 
  bind_cols(
    predict(fit_tidy, new_data = x,
            penalty = pen_val)
  ) |> 
  mutate(
    difference = .pred_raw- .pred
  ) |> 
  arrange(abs(difference)) |> 
  View()


tidy(fit_tidy)
tidy(fit_raw)

##############################

m |>
  pluck("ft") |> extract_recipe() |> prep(retain = T) |> bake(new_data = NULL)
  extract_workflow() |> 
  View()
