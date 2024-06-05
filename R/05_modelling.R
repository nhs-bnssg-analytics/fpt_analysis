source("R/00_libraries.R")
source("R/01_utils.R")
source("R/04_modelling_utils.R")

# configure modelling  -----------------------------------------------------------
for (target_variable in c(
  "Proportion of incomplete pathways greater than 52 weeks from referral",
  "Proportion of suspected cancer or referral to first definitive treatment that are longer than 62 days wait",
  "Proportion of A&E attendances greater than 4 hours wait (Type 1 Departments - Major A&E)",
  "Proportion of attended GP appointments (over 4 weeks wait time)"
  )) {
  
  # target_variable <- "Proportion of incomplete pathways greater than 52 weeks from referral"
  
  model_value_type <- "value"
  predict_year <- 2023
  val_type <- "leave_group_out_validation"
  ts_split <- FALSE #set whether to leave the latest year for testing (TRUE) or not (FALSE)
  evaluation_metric <- "mae"
  
  for (model_method in c("logistic_regression", "random_forest")) {
    # model_method <- "random_forest"
    
    numerator_remainder <- include_numerator_remainder(model_method)
    
    if (model_method == "random_forest") {
      target_type <- c("proportion", "difference from previous")
    } else if (model_method == "logistic_regression") {
      target_type <- "proportion"
    }
    
    for (tt in target_type) {
      # tt <- "proportion"
      # modelling -----------------------------------------------------------
      
      dc_data <- load_data(
        target_variable,
        value_type = model_value_type,
        incl_numerator_remainder = numerator_remainder,
        binary_covid = FALSE
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
        # make sure target variable is between 0 and 100
        mutate(
          across(
            all_of(target_variable),
            \(x) x / 100
          )
        )
      
      inputs <- modelling_grid(
        data = dc_data, 
        target_variable = target_variable, 
        predict_year = predict_year,
        target_type = tt
      )
      
      modelling_outputs <- inputs |> 
        pmap(
          \(lagged_years, training_years, lag_target)
          modelling_performance(
            data = dc_data,
            target_variable = target_variable,
            lagged_years = lagged_years, 
            training_years = training_years,
            keep_current = FALSE,
            lag_target = lag_target,
            time_series_split = ts_split, 
            shuffle_training_records = TRUE,
            model_type = model_method,
            validation_type = val_type,
            target_type = tt,
            seed = 321,
            eval_metric = evaluation_metric
          )
        )
      
      model_summary <- map_df(
        modelling_outputs,
        ~ record_model_outputs(
          model_outputs = .x,
          eval_metric = evaluation_metric,
          validation_type = val_type,
          target_type = tt
        )
      )
    }
  }
}
