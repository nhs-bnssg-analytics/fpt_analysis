source("R/00_libraries.R")
source("R/01_utils.R")
source("R/04_modelling_utils.R")


# configure modelling  -----------------------------------------------------------

for (target_variable in c(
  "Proportion of incomplete pathways greater than 18 weeks from referral (incomplete)",
  "62 day wait from suspected cancer or referral to first definitive treament (proportion outside of standard)",
  "Proportion of A&E attendances greater than 4 hours (Type 1 Departments - Major A&E)",
  "Proportion of attended appointments (Over 4 weeks wait time)"
  )) {
  
  # target_variable <- "Proportion of incomplete pathways greater than 18 weeks from referral (incomplete)"
  
  model_value_type <- "value"
  predict_year <- 2023
  val_type <- "leave_group_out_validation"
  
  
  ts_split <- FALSE #set whether to leave the latest year for testing (TRUE) or not (FALSE)
  if (ts_split) {
    chart_subtitle <- "Only previous years data used for training (without target variable), shuffled training data"
    figure_caption <- paste(
      target_variable,
      "in",
      predict_year
    )
  } else {
    chart_subtitle <- "Observations are randomly selected for training, validating and testing models"
    figure_caption <- paste(
      target_variable
    )
  }
  
  evaluation_metric <- "mae"
  
  for (model_method in c("logistic_regression", "random_forest")) {
    # model_method <- "random_forest"
    
    
    if (model_method == "random_forest") {
      numerator_remainder <- FALSE
      target_type <- c("proportion", "difference from previous")
    } else if (model_method == "logistic_regression") {
      numerator_remainder <- TRUE
      target_type <- "proportion"
    }
    
    for (tt in target_type) {
      # modelling -----------------------------------------------------------
      
      dc_data <- load_data(
        target_variable,
        value_type = model_value_type,
        incl_numerator_remainder = numerator_remainder
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
        )
      
      inputs <- expand.grid(
        training_years = 2:6,
        lagged_years = 0:2
      ) |> 
        mutate(
          input_id = row_number()
        )
      
      for (target_lag in 0:1) {
        modelling_outputs <- map2(
          .x = inputs$training_years,
          .y = inputs$lagged_years,
          ~ modelling_performance(
            data = dc_data,
            target_variable = target_variable,
            lagged_years = .y, 
            training_years = .x,
            keep_current = FALSE,
            lag_target = target_lag,
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
            best_model_outputs = .x,
            eval_metric = evaluation_metric,
            validation_type = val_type,
            target_type = tt
          )
        )
      }
    }
    
  }
}


p <- plot_modelling_performance(
  modelling_results = modelling_outputs,
  inputs = inputs,
  val_type = val_type,
  evaluation_metric = evaluation_metric,
  chart_subtitle = chart_subtitle,
  figure_caption = figure_caption,
  show_validation_variance = TRUE,
  model_type = model_method
)

fname <- paste0(
  "tests/model_testing/",
  model_method,
  "/",
  gsub("[[:punct:]]", "", Sys.time()),
  "_",
  abbreviate(target_variable, 15), 
  "_predicting_", 
  predict_year, 
  "_using_",
  val_type,
  ".png"
)

ggsave(
  p,
  filename = fname,
  width = 6,
  height = 6,
  units = "in",
  bg = "white"
)


# prediction plot for best performing model
best_pred_plot <- pick_model(
  modelling_outputs = modelling_outputs, 
  evaluation_metric = evaluation_metric
) |> 
  (\(x) x$prediction_plot +
     labs(
       title = paste(
         x$inputs$`Training years`,
         "training years,",
         x$inputs$`Lagged years`,
         "lagged years"
       ),
       subtitle = paste0(evaluation_metric, ": ", x$best_metric),
       caption = paste(
         "Predicting",
         predict_year
       )
     ))()

ggsave(
  plot = best_pred_plot,
  filename = gsub(".png", "_best_model.png", fname),
  width = 10,
  height = 5,
  units = "in",
  bg = "white"
)

vi_plot <- pick_model(
  modelling_outputs = modelling_outputs, 
  evaluation_metric = evaluation_metric
  ) |> 
  pluck("ft") |> 
  plot_variable_importance(
    top_n = 15
  )

ggsave(
  plot = vi_plot,
  filename = gsub(".png", "_vi.png", fname),
  width = 12,
  height = 6,
  units = "in",
  bg = "white"
)


# predicting --------------------------------------------------------------
prediction_configuration <- tibble(
  yrs = rep(3:4, times = 2),
  meth = rep(c("linear", "spline"), each = 2)
) |> 
  rowid_to_column(var = "scenario_id") |> 
  mutate(
    scenario_id = as.character(scenario_id)
  )
debugonce(apply_model_to_scenario)
preds_baseline <- prediction_configuration %$%
  map2_dfr(
    .x = yrs,
    .y = meth,
    .f = ~ apply_model_to_scenario(
      input_data = dc_data, 
      number_year_to_extrapolate = .x,
      area_code = "QUY", 
      extrapolation_method = .y, 
      model_fit = pick_model(
        modelling_outputs = modelling_outputs,
        evaluation_metric = evaluation_metric
      ) |> 
        pluck("ft"),
      target_variable = target_variable
    ),
    .id = "scenario_id"
  ) |> 
  left_join(
    prediction_configuration,
    by = join_by(scenario_id)
  ) |> 
  mutate(
    scenario_id = paste0(
      str_to_sentence(meth),
      " extrapolation based on ",
      yrs,
      " years"
    )
  ) |> 
  rename(
    baseline = all_of(target_variable)
  )
  

# scenario 1 - make more general and acute beds available (5%)
scenario <- tibble(
  metric = "Proportion of available beds that are occupied (General & Acute - overnight)",
  multiplier = 0.95
)
debug(apply_model_to_scenario)
preds_scenario <- prediction_configuration %$%
  map2_dfr(
    .x = yrs,
    .y = meth,
    .f = ~ apply_model_to_scenario(
      input_data = dc_data, 
      number_year_to_extrapolate = .x,
      area_code = "QUY", 
      extrapolation_method = .y, 
      model_fit = pick_model(
        modelling_outputs = modelling_outputs,
        evaluation_metric = evaluation_metric
      ) |> 
        pluck("ft"),
      target_variable = target_variable,
      scenario = scenario),
    .id = "scenario_id"
  ) |> 
  left_join(
    prediction_configuration,
    by = join_by(scenario_id)
  ) |> 
  mutate(
    scenario_id = paste0(
      str_to_sentence(meth),
      " extrapolation based on ",
      yrs,
      " years"
    )
  ) |> 
  rename(
    scenario = all_of(target_variable)
  )

plot_scenario <- preds_baseline |> 
  left_join(
    preds_scenario,
    by = join_by(
      scenario_id, 
      year, 
      org, 
      nhs_region, 
      type, 
      yrs, 
      meth)
  ) |> 
  mutate(
    type = str_to_sentence(type)
  ) |> 
  pivot_longer(
    cols = c(scenario, baseline),
    names_to = "scenario",
    values_to = "value"
  ) |> 
  mutate(
    type = case_when(
      type == "Observed" ~ type,
      scenario == "baseline" ~ "Prediction based on historic trend",
      .default = "Prediction based on decrease bed occupancy by 5% from trend"
    ),
    type = factor(type,
                  levels = c("Observed", 
                             "Prediction based on historic trend",
                             "Prediction based on decrease bed occupancy by 5% from trend"))
  ) |> 
  ggplot(
    aes(
      x = year,
      y = value
    )
  ) +
  geom_line(
    aes(
      group = type,
      linetype = type,
      colour = type
    )
  ) +
  theme_minimal() +
  scale_linetype_manual(
    name = "Scenario",
    values = c(
      Observed = "solid",
      `Prediction based on historic trend` = "solid",
      `Prediction based on decrease bed occupancy by 5% from trend` = "dashed"
    )
  ) +
  scale_colour_manual(
    name = "Scenario",
    values = c(
      Observed = "#0072b2",
      `Prediction based on historic trend` = "#d55e00",
      `Prediction based on decrease bed occupancy by 5% from trend` = "#d55e00"
    )
  ) +
  labs(
    x = "",
    y = "Proportion outside of 62 day wait",
    title = "62 day wait from suspected cancer or referral to first definitive treament (proportion outside of standard)",
    subtitle = "Baseline and scenario"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  facet_wrap(
    facets = vars(scenario_id)
  )



ggsave(
  filename = gsub(
    ".png",
    "_baseline_scenario.png",
    fname
  ),
  plot = plot_scenario,
  width = 11, 
  height = 7,
  units = "in",
  bg = "white"
)
