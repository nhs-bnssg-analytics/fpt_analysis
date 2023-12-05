#' @description using the data; the final year is attributed to testing, and the
#'   penultimate year is attributes to validation, while all the earlier years
#'   are attributed to training.
#' @value a numeric vector of length 2 representing the proportions of the total
#'   dataset that should be split into training and validation
train_validation_proportions <- function(data) {
  proportions <- data |> 
    count(year, sort = TRUE) |> 
    mutate(
      split_type = case_when(
        year == max(year) ~ "test",
        year == max(year) - 1 ~ "validation",
        .default = "train"
      )
    ) |> 
    summarise(
      records = sum(n),
      .by = split_type
    ) |> 
    mutate(
      proportions = records / sum(records)
    ) |> 
    pull(
      proportions
    ) |> 
    head(2)
  
  return(proportions)
}

add_prediction_to_data <- function(data, model_fit) {
  data <- data |> 
    bind_cols(
      predict(
        model_fit,
        new_data = data
        )
      )
  return(data)
}

model_metrics <- function(data, model_fit) {
  evaluation_metrics <- metric_set(
    yardstick::rmse,
    yardstick::rsq,
    yardstick::mae)
  
  metrics <- add_prediction_to_data(data, model_fit) |> 
    evaluation_metrics(
      truth = all_of(target_variable),
      estimate = .pred
    )
  
  return(metrics)
}

plot_observed_expected <- function(data, target_variable) {
 p <- data |> 
    ggplot(
      aes(x = .data[[target_variable]], 
          y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(
      alpha = 0.5,
      aes(
        colour = data_type
      )
    ) + 
    labs(
      y = stringr::str_wrap(
        paste("Predicted", tolower(target_variable)),
        60),
      x = stringr::str_wrap(target_variable, 60)
    ) +
    # Scale and size the x- and y-axis uniformly:
    coord_obs_pred() +
    theme_minimal() +
    scale_colour_manual(
      name = "Type",
      values = c(
        test = "#33a02c",
        validation = "#ff7f00",
        train = "#1f78b4"
      ),
      labels = c(
        test = "Test",
        validation = "Validation",
        train = "Train"
      ),
      breaks = c("train", "validation", "test"),
      drop = TRUE
    )
 
 return(p)
}


# modelling ---------------------------------------------------------------

#' @param data tibble containing data for modelling. Each record is a year
#'   (field name is "year") and an ICS (field name is "org"), with all other
#'   columns representing target and predictor variables
#' @param target_variable string; field name for target variable
#' @param lagged_years numeric; number of lagged years. Use 1:3 for lagging up
#'   to 3 years prior
#' @param keep_current logical; if lagged_years is greater than 0 and
#'   time_series_split is TRUE, set this to FALSE to remove data from the
#'   predictor variables where they occur in the same year as the target
#'   variable that is being predicted
#' @param remove_lag_target logical; if including lagged data, whether to
#'   include the lagged target variable or not
#' @param time_series_split logical; order the data by year prior to splitting,
#'   so training data is before validation data, which is before test data
#' @param model_type character string; the modelling method to use
#' @param rf_tuning_grid data.frame with numeric columns for mtry, min_n and
#'   trees. Can also take the value "auto"
#' @param linear_correlation_threshold numeric between 0 and 1; threshold to
#'   remove correlated variables for linear model
#' @param seed numeric; seed number
modelling_performance <- function(data, target_variable, lagged_years = 0, 
                                  keep_current = TRUE, remove_lag_target = TRUE,
                                  time_series_split = TRUE, 
                                  model_type = "linear", 
                                  rf_tuning_grid = "auto",
                                  linear_correlation_threshold = 0.9,
                                  seed = 321) {
  
  model_type <- match.arg(
    model_type,
    c("linear", "random_forest")
  )
  
  if (model_type == "random_forest") {
    if (class(rf_tuning_grid) == "data.frame") {
      if (!all(names(rf_tuning_grid) %in% c("mtry", "min_n", "trees"))) {
        stop("rf_tuning_grid is incorrect")
      }
    } else if (rf_tuning_grid != "auto") {
      stop("rf_tuning_grid is incorrect")
    }
  }
  
  set.seed(seed)
  
  if (lagged_years > 0) {
    
    map_lag <- set_names(
      seq_len(lagged_years),
      nm = paste("lag", seq_len(lagged_years), sep = "_")
    ) |> 
      map(
        ~ purrr::partial(lag, n = .x)
      )
    
    # create lag variables
    
    if (remove_lag_target) {
      not_lag_variables <- c("year", "org", target_variable)
    }
    else {
      not_lag_variables <- c("year", "org")
    }
    
    data <- data |> 
      arrange(
        year, org
      ) |>
      group_by(org) |> 
      group_split() |> 
      purrr::map_df(
        ~ mutate(
          .x,
          across(
            !all_of(not_lag_variables),
            .fn = map_lag,
            .names = "{.fn}_{.col}"
          ),
        )
      )
    
    if (time_series_split) {
      data <- data |> 
        arrange(
          year, org
        )
      
      if (!keep_current) {
        data <- data |>
          select(
            all_of(
              c("year", "org", target_variable)
            ),
            starts_with("lag")
          ) |>
          filter(
            year != min(year)
          )
      } 
    }
  }
  
  
  # splitting
  
  proportions <- train_validation_proportions(data)
  
  # split dataset into train, validation and test
  if (time_series_split) {
    splits <- rsample::initial_validation_time_split(
      data = data,
      prop = proportions
    )
    
  } else {
    splits <- rsample::initial_validation_split(
      data = data,
      prop = proportions
    ) 
  }
  data_train <- rsample::training(splits)
  data_validation <- rsample::validation(splits)
  data_test <- rsample::testing(splits)
  
  dataset_yrs <- lapply(
    list(data_train, data_validation, data_test),
    function(x) range(x$year)
  )
  
  # create train and validation set for tuning hyperparameters
  data_validation_set <- validation_set(splits)
  
  
  # set model
  
  if (model_type == "linear") {
    model_setup <- parsnip::linear_reg(
      mode = "regression",
      engine = "lm"
    )  
  } else if (model_type == "random_forest") {
    # how many cores on the machine so we can parallelise
    cores <- parallel::detectCores()
    
    # set model
    model_setup <- rand_forest(
      mtry = tune(), 
      min_n = tune(), 
      trees = tune()
    ) |> 
      set_engine("ranger", num.threads = cores) |> 
      set_mode("regression")
  }
  
  
  
  # set recipe
  missing_data <- names(data)[colSums(is.na(data)) > 0]
  
  predictor_variables <- names(data)[!(names(data) %in% c("year", "org", target_variable))]
  
  model_recipe <- data_train |> 
    select(
      !c("year", "org")
    ) |> 
    recipe() |> 
    update_role(
      all_of(target_variable),
      new_role = "outcome"
    ) |> 
    update_role(
      all_of(predictor_variables),
      new_role = "predictor"
    )
  
  if (model_type == "linear") {
    model_recipe <- model_recipe |> 
      step_center(all_predictors()) |> 
      step_scale(all_predictors())
  }
  
  model_recipe <- model_recipe |> 
    step_impute_knn(all_of(missing_data))
  
  if (model_type == "linear") {
    model_recipe <- model_recipe |> 
      step_corr(all_predictors(),
                threshold = linear_correlation_threshold)
  }
  
  model_recipe <- model_recipe |> 
    # estimate the means and standard deviations
    prep(training = data_train, retain = TRUE)
  
  
  # create workflow
  
  modelling_workflow <- workflow() |> 
    add_model(model_setup) |> 
    add_recipe(model_recipe)
  
  if (model_type == "linear") {
    # fit model
    
    model_fit <- modelling_workflow |> 
      fit(data = data_train)
    
    # check linear assumptions
    assumptions_check <- model_fit$fit$fit |> 
      performance::check_model()
    
  } else if (model_type == "random_forest") {
    # design the tuning of the hyperparameters
    
    if (class(rf_tuning_grid) == "data.frame") {
      tuning_grid <- rf_tuning_grid
    } else {
      tuning_grid <- expand.grid(
        mtry = seq(
          from = round((length(predictor_variables) / 3), 0) - 10,
          to = round((length(predictor_variables) / 3), 0) + 10,
          by = 4
        ),
        min_n = seq(
          from = round(nrow(data_train) / 10, 0) - 10,
          to = round(nrow(data_train) / 10, 0) + 10,
          by = 4
        ),
        trees = seq(
          from = ceiling((length(predictor_variables) / 5) / 10) * 10,
          to = (ceiling((length(predictor_variables) / 5) / 10) * 10) + 100,
          by = 10
        )
      )
    }
    
    
    rf_residuals <- modelling_workflow |> 
      tune_grid(data_validation_set,
                grid = tuning_grid[1,],
                control = control_grid(save_pred = TRUE))
    
    tuning_parameters <- autoplot(rf_residuals)
    
    # select the best parameters
    rf_best <- rf_residuals |> 
      select_best(metric = "rsq")
    
    # the last model
    last_rf_mod <- rand_forest(
      mtry = rf_best$mtry, 
      min_n = rf_best$min_n, 
      trees = rf_best$trees) |> 
      set_engine("ranger", num.threads = cores, importance = "impurity") |> 
      set_mode("regression")
    
    # the last workflow
    modelling_workflow <- modelling_workflow |> 
      update_model(last_rf_mod)
    
    # the last fit
    model_fit <- list(
      train = data_train
    ) |> 
      bind_rows(
        .id = "data_type"
      ) |> 
      fit(
        data = _,
        object = modelling_workflow
      )
    
    rf_metrics <- modelling_workflow |> 
      last_fit(splits) |> 
      collect_metrics(summarize = FALSE)
  }
  
  
  
  # plot predictions vs observed
  prediction_plot <- list(
    train = data_train,
    validation = data_validation,
    test = data_test
  ) |> 
    bind_rows(
      .id = "data_type"
    ) |> 
    mutate(
      data_type = factor(
        data_type,
        levels = c("test", "validation", "train")
      )
    ) |> 
    add_prediction_to_data(
      model_fit
    ) |> 
    plot_observed_expected(
      target_variable
    )
  
  
  # evaluation metrics on validation data
  
  evaluation_metrics <- list(
    train = data_train,
    validation = data_validation,
    test = data_test
  ) |> 
    purrr::map_dfr(
      ~ model_metrics(
        data = .x,
        model_fit = model_fit
      ),
      .id = "data"
    ) |> 
    tidyr::pivot_wider(
      names_from = data,
      values_from = .estimate
    )
  
  # variable importance
  
  variable_importance <- model_fit |> 
    extract_fit_parsnip() |> 
    vip::vip(num_features = 10)
  
  if (model_type == "linear") {
    output <- list(
      dataset_yrs = dataset_yrs,
      assumptions_check = assumptions_check,
      prediction_plot = prediction_plot,
      evaluation_metrics = evaluation_metrics,
      variable_importance = variable_importance
    )
  } else if (model_type == "random_forest") {
    output <- list(
      dataset_yrs = dataset_yrs,
      tuning_parameters = tuning_parameters,
      prediction_plot = prediction_plot,
      evaluation_metrics = evaluation_metrics,
      rf_metrics = rf_metrics,
      variable_importance = variable_importance
    )
  }
  
  return(output)
  
}