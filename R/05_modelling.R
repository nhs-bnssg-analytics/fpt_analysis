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



lm_no_lag <- modelling_performance(
  data = dc_data,
  target_variable = target_variable,
  lagged_years = 1, 
  time_series_split = FALSE, 
  model_type = "linear", 
  linear_correlation_threshold = 0.7,
  seed = 321 
)

lm_no_lag$evaluation_metrics
lm_no_lag$prediction_plot
lm_no_lag$variable_importance

rf_no_lag <- modelling_performance(
  data = dc_data,
  target_variable = target_variable,
  lagged_years = 0,
  time_series_split = FALSE,
  model_type = "random_forest",
  seed = 321
)

rf_no_lag$evaluation_metrics
rf_no_lag$prediction_plot
rf_no_lag$assumptions_check
rf_no_lag$variable_importance
