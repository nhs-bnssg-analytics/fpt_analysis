source("R/00_libraries.R")

# this example shows that the same mode can be built using every combinations of
# tidymodels and the raw glm function, along with the outcome of a single value
# (the proportion of success) and an outcome of cbind(success, failure)

data(mtcars)


junk <- mtcars |> 
  mutate(
    mpg = round(mpg, 0),
    total = hp + mpg,
    prop = hp / total
  )
  
m_raw_prop <- glm(
  prop ~ cyl + disp + drat + wt + qsec + vs + am + gear + carb,
  data = junk,
  family = binomial("logit"),
  weights = total
)  

m_raw_cbind <- glm(
  cbind(mpg, hp) ~ cyl + disp + drat + wt + qsec + vs + am + gear + carb,
  data = junk,
  family = binomial("logit")
)

rec <- junk |> 
  recipe() |> 
  update_role(
    hp, mpg, 
    new_role = "outcome"
  ) |> 
  update_role(
    cyl, disp, drat, wt, qsec, vs, am, gear, carb,
    new_role = "predictor"
  )

eng <- linear_reg() |> 
  set_engine(
    "glm",
    family = stats::binomial(link = "logit")
  ) |> 
  set_mode("regression")

wf <- workflow() |> 
  add_recipe(rec) %>%
  add_model(eng)

m_tidy_cbind <- fit(
  wf,
  junk
)

junk1 <- junk |> 
  mutate(
    total = frequency_weights(total)
  )

rec <- junk1 |> 
  recipe() |> 
  update_role(
    prop, 
    new_role = "outcome"
  ) |> 
  update_role(
    cyl, disp, drat, wt, qsec, vs, am, gear, carb,
    new_role = "predictor"
  )

eng <- linear_reg() |> 
  set_engine(
    "glm",
    family = stats::binomial(link = "logit")
  ) |> 
  set_mode("regression")

wf <- workflow() |> 
  add_recipe(rec) %>%
  add_model(eng) |> 
  add_case_weights(total)

m_tidy_prop <- fit(
  wf,
  junk1
)


tidy(m_raw_cbind)
tidy(m_tidy_cbind)

tidy(m_raw_prop)
tidy(m_tidy_prop)
