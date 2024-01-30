#' @description using the data; the final year is attributed to testing, and the
#'   penultimate year is attributes to validation, while all the earlier years
#'   are attributed to training.
#' @value a numeric vector of length 2 representing the proportions of the total
#'   dataset that should be split into training and validation
train_validation_proportions <- function(data, shuffle_training_records = FALSE) {
  
  proportions <- data |> 
    count(year) |> 
    arrange(year) |> 
    mutate(
      split_type = case_when(
        row_number() == n() ~ "test",
        row_number() == (n() - 1) ~ "validation",
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
  
  if (shuffle_training_records) {
    proportions <- sum(proportions) * c(0.75, 0.25)
  }
  
  return(proportions)
}

add_prediction_to_data <- function(data, final_fit) {
  
  data <- final_fit |> 
    workflowsets::extract_workflow() |> 
    predict(data) |> 
    bind_cols(
      data
    )
  
  return(data)
}

calculate_train_metric <- function(final_fit, training_data, target_variable, metric) {
  
  training_metric <- add_prediction_to_data(
    data = training_data,
    final_fit = final_fit
  ) |> 
    select(
      observed = all_of(target_variable),
      ".pred"
    )
  
  if (metric == "rsq") {
    training_metric <- training_metric %$%
      cor(
        observed,
        .pred
      ) ^ 2
  }
  
  training_metric <- tibble(
    data = "train",
    .metric = metric,
    .estimate = training_metric
  )
  
  return(training_metric)
}

plot_observed_expected <- function(data, target_variable, distinguish_year) {
  p <- data |> 
    ggplot(
      aes(x = .data[[target_variable]], 
          y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) +
    geom_point(
      alpha = 0.5,
      shape = 21,
      aes(
        fill = data
      )
    )
  
  if (distinguish_year == TRUE) {
    p <- p +
      geom_point(
        data = ~ filter(.x, year == max(year)),
        shape = 21,
        fill = NA,
        colour = "black"
      )
  } 
  
  p <- p + 
    labs(
      y = stringr::str_wrap(
        paste("Predicted", tolower(target_variable)),
        60),
      x = stringr::str_wrap(target_variable, 60)
    ) +
    # Scale and size the x- and y-axis uniformly:
    coord_obs_pred() +
    theme_minimal() +
    scale_fill_manual(
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

metric_to_numerator <- function(metric_name) {
  numerator_description <- read.csv("data/configuration-table.csv") |> 
    filter(
      metric == metric_name
    ) |> 
    pull(numerator_description)
  
  if (length(numerator_description) == 0) numerator_description <- NA_character_
  
  return(numerator_description)
}

identify_missing_vars <- function(data_train, data_validation, data_test, model_type) {
  
  data <- bind_rows(
    data_train, 
    data_validation,
    data_test
  )
  
  # identify variables in train and val that are over 40% missing, so they are
  # subsequently removed
  mostly_missing_vars <- bind_rows(
    data_train,
    data_validation
  ) |> 
    lapply(
      function(x) sum(is.na(x)) / length(x)
    ) |> 
    (\(x) x[x > 0.4])() |> 
    names(x = _)
  
  # mostly zero vars should be covid variables when modelling pre-pandemic
  mostly_zero_vars <- bind_rows(
    data_train,
    data_validation
  ) |> 
    select(
      !any_of("total_cases")
    ) |> 
    lapply(
      function(x) sum(x == 0, na.rm = TRUE) / length(x)
    ) |> 
    (\(x) x[x > 0.95])() |> 
    names(x = _)
  
  # identify variables that are (almost) entirely missing from any of the
  # datasets
  
  entirely_missing_vars <- list(
    data_train,
    data_validation,
    data_test
  ) |> 
    lapply(
      function(x) names(x)[(colSums(is.na(x)) / nrow(x)) > 0.95]
    ) |> 
    unlist() |> 
    unique()
  
  # add the entirely missing vars to the mostly missing vars
  vars_to_remove <- unique(
    c(mostly_missing_vars, mostly_zero_vars, entirely_missing_vars, "org", "nhs_region")
  )
  
  # if (model_type == "random_forest") {
    vars_to_remove <- c(vars_to_remove, "year")
  # }
  
  # identify fields to impute
  missing_data <- names(data)[colSums(is.na(data)) > 0] |> 
    (\(x) x[!(x %in% vars_to_remove)])()
  
  vars <- list(
    vars_to_remove = vars_to_remove,
    vars_to_impute = missing_data
  )
  return(vars)
}

# load data ---------------------------------------------------------------

load_data <- function(target_variable, value_type = "value", incl_numerator_remainder = FALSE,
                      broad_age_bands = TRUE) {
  
  metrics <- read.csv(
    here::here("data/configuration-table.csv"),
    encoding = "latin1"
  ) |> 
    filter(
      # !(grepl("incorrect geography|remove", status))
      grepl("include", status) |
        metric == target_variable
    ) |> 
    select(metric, denominator_description)
  
  dc_data <- list.files(here::here("data"), full.names = TRUE) |> 
    (\(x) x[!grepl("configuration-table|modelling_data", x)])() |> 
    purrr::map_dfr(
      read.csv
    )
  
  # ensure full year of data for target variable
  target_data <- dc_data |> 
    filter(
      metric == target_variable
    )
  
  # check whether monthly data area recorded
  if (sum(!is.na(target_data[["month"]])) > 0) {
    full_years <- target_data |> 
      filter(!is.na(month)) |> 
      distinct(
        year, month
      ) |> 
      count(year) |> 
      filter(n == 12) |> 
      pull(year)
    
    dc_data <- dc_data |> 
      filter(year %in% full_years)
  } else if (sum(!is.na(target_data[["quarter"]])) > 0) {
    full_years <- target_data |> 
      filter(!is.na(quarter)) |> 
      distinct(
        year, quarter
      ) |> 
      count(year) |> 
      filter(n == 4) |> 
      pull(year)
    
    dc_data <- dc_data |> 
      filter(year %in% full_years)
  }
  
  dc_data <- dc_data |> 
    inner_join(
      metrics,
      by = join_by(metric)
    ) |> 
    filter(
      grepl("annual", frequency),
      grepl("^Q", org)
    )
  
  if (broad_age_bands == TRUE) {
    age_band_data <- dc_data |> 
      filter(
        grepl("age band", metric)
      ) |> 
      mutate(
        metric = str_replace_all(
          metric,
          c("0-9" = "0-29",
            "10-19" = "0-29",
            "20-29" = "0-29",
            "30-39" = "30-59",
            "40-49" = "30-59",
            "50-59" = "30-59",
            "60-69" = "60+",
            "70-79" = "60+",
            "80-89" = "60+",
            "90\\+" = "60+")
        )
      ) |> 
      summarise(
        numerator = sum(numerator),
        denominator = mean(denominator),
        .by = c(
          year, org, frequency, metric,
        )
      ) |> 
      mutate(
        value = numerator / denominator
      )
    
    dc_data <- dc_data |> 
      filter(
        !grepl("age band", metric)
      ) |> 
      bind_rows(
        age_band_data
      )
  }
  
  if (incl_numerator_remainder == TRUE) {
    numerator_remainder <- dc_data |> 
      filter(
        metric == target_variable
      ) |> 
      mutate(
        remainder = denominator - numerator
      ) |> 
      select(!c("value", "metric", "denominator")) |> 
      pivot_longer(
        cols = c("numerator", "remainder"),
        names_to = "metric",
        values_to = value_type
      )
    
    dc_data <- dc_data |> 
      bind_rows(
        numerator_remainder
      )
  }
  
  ics_to_nhs_region <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/SICBL22_ICB22_NHSER22_EN_LU/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson" |> 
    jsonlite::fromJSON() |> 
    pluck("features", "properties") |> 
    distinct(
      ICB22CDH,
      NHSER22CDH
    ) |> 
    rename(
      org = "ICB22CDH",
      nhs_region = "NHSER22CDH"
    )
  
  dc_data <- dc_data |> 
    select(
      all_of(
        c(
          "metric",
          "year", 
          "org", 
          value_type
        )
      )
    ) |> 
    rename(
      value = all_of(value_type)
    ) |> 
    pivot_wider(
      names_from = metric,
      values_from = value
    ) |> 
    left_join(
      ics_to_nhs_region,
      by = join_by(
        org
      )
    ) |> 
    relocate(
      nhs_region, .after = org
    )
  
  return(dc_data)
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
#' @param training_years integer; number of years to use for training the model.
#'   If NULL, all available years will be used. If contains an integer, it must
#'   be greater than 1
#' @param remove_years numeric; which years should be removed. The training
#'   years will modified if the years fall within the training years time frame
#'   (eg, if the training years were covering 2020 and 2021, and we remove 2020,
#'   the training years will update to incorporate 2019 as well)
#' @param shuffle_training_records logical; should the training/validation set
#'   be ordered or shuffled for imte_series_split = TRUE
#' @param model_type character string; the modelling method to use
#' @param tuning_grid data.frame with numeric columns for mtry, min_n and trees
#'   (if using random_forest) or a signle column named threshold (if performing
#'   logistic_regression). Can also take the value "auto", which is the default.
#' @param predict_proportions logical; should proportions be the predicted value
#'   (TRUE) or a count (FALSE)
#' @param validation_type string; either "cross_validation",
#'   "leave_group_out_validation" or "train_validation"
#' @param seed numeric; seed number
#' @details This webpage was useful
#'   https://www.tidyverse.org/blog/2022/05/case-weights/
#' 
modelling_performance <- function(data, target_variable, lagged_years = 0, 
                                  keep_current = TRUE, remove_lag_target = TRUE,
                                  time_series_split = TRUE, training_years = NULL,
                                  remove_years = NULL,
                                  shuffle_training_records = FALSE,
                                  model_type = "logistic", 
                                  tuning_grid = "auto",
                                  predict_proportions = TRUE,
                                  validation_type,
                                  seed = 321) {
  
  model_type <- match.arg(
    model_type,
    c("random_forest", "logistic_regression")
  )
  
  validation_type <- match.arg(
    validation_type,
    c("train_validation", "leave_group_out_validation", "cross_validation")
  )
  
  if (model_type == "random_forest") {
    if (class(tuning_grid) == "data.frame") {
      if (!all(names(tuning_grid) %in% c("mtry", "min_n", "trees"))) {
        stop("tuning_grid is incorrect")
      }
    } else if (tuning_grid != "auto") {
      stop("tuning_grid is incorrect")
    }
  } 
  
  if (!is.null(training_years)) {
    if (training_years < 2) stop("training years must be NULL or greater than 1")
  }
  
  set.seed(seed)
  
  # create case weights field for logistic regression
  if (model_type == "logistic_regression" &
      all(c("numerator", "remainder") %in% names(data))
      ) {
    data <- data |> 
      mutate(
        total_cases = frequency_weights(numerator + remainder)
      ) |> 
      select(!c("numerator", "remainder"))
  }
  
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
      not_lag_variables <- c("year", "org", "nhs_region", target_variable)
    } else {
      not_lag_variables <- c("year", "org", "nhs_region")
    }
    
    if (model_type == "logistic_regression") {
      not_lag_variables <- c(not_lag_variables, "total_cases")
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
        keep_variables <- c("year", "org", "nhs_region", target_variable)
        
        if (model_type == "logistic_regression") {
          keep_variables <- c(keep_variables, "total_cases")
        }
        
        data <- data |>
          select(
            all_of(keep_variables),
            starts_with("lag")
          ) |>
          filter(
            year != min(year)
          )
      } 
    }
  }
  
  if (!is.null(remove_years)) {
    data <- data |> 
      filter(
        !(year %in% remove_years)
      )
    
    if (!is.null(training_years)) {
      overlapping_years <- intersect(
        remove_years, 
        seq(
          from = max(data[["year"]]) - training_years,
          to = max(data[["year"]]) - 1
        )
      )
      training_years <- training_years + length(overlapping_years)
    }
  }
  
  # limit the years used for training purposes
  if (!is.null(training_years)) {
    
    data <- data |> 
      filter(
        year >= (max(year) - training_years)
      )
  }
  
  # splitting
  
  
  # split dataset into train, validation and test
  if (time_series_split) {
    proportions <- train_validation_proportions(
      data,
      shuffle_training_records = shuffle_training_records
    )
    
    if (shuffle_training_records) {
      final_year <- data |> 
        filter(year == max(year))
      
      data <- data |> 
        filter(year != max(year)) |> 
        slice_sample(prop = 1) |> 
        bind_rows(final_year)
        
    }
    
    splits <- rsample::initial_validation_time_split(
      data = data,
      prop = proportions
    )
    
  } else {
    # 60% train, 20% val, 20% test
    proportions <- c(0.6, 0.2)
    splits <- rsample::initial_validation_split(
      data = data,
      prop = proportions
    ) 
  }
  
  data_train <- rsample::training(splits)
  data_validation <- rsample::validation(splits)
  data_test <- rsample::testing(splits)
  
  # check what the year ranges are for each training set
  dataset_yrs <- lapply(
    list(data_train, data_validation, data_test),
    function(x) range(x$year)
  )
  
  # create train and validation set for tuning hyperparameters
  if (validation_type == "cross_validation") {
    validation_set <- vfold_cv(
      bind_rows(
        data_train,
        data_validation
      ),
      v = 4,
      strata = all_of(target_variable)
    )  
  } else if (validation_type == "train_validation") {
    validation_set <- validation_set(splits)
  } else if (validation_type == "leave_group_out_validation") {
    validation_set <- group_vfold_cv(
      bind_rows(
        data_train,
        data_validation
      ), 
      group = nhs_region
    )
  }
  
  
  # how many cores on the machine so we can parallelise
  cores <- parallel::detectCores()
  
  # set model
  if (model_type == "random_forest") {
    
    # set model
    model_setup <- rand_forest(
      mtry = tune(), 
      min_n = tune(), 
      trees = tune()
    ) |> 
      set_engine(
        "randomForest", 
        num.threads = cores
      ) |> 
      set_mode("regression")
  } else if (model_type == "logistic_regression") {
    model_setup <- parsnip::linear_reg(
      penalty = tune(),
      mixture = tune()
    ) |> 
      set_engine(
        "glmnet", 
        family = stats::quasibinomial(link = "logit"), 
        nlambda = 200,
        num.threads = cores
      ) |> 
      set_mode("regression")
  }
  
  # identify variables in train and val that are over 40% missing, so they are
  # subsequently removed
  vars_selection <- identify_missing_vars(
    data_train = data_train,
    data_validation = data_validation,
    data_test = data_test,
    model_type = model_type
  )
  
  # identify predictor variables
  predictor_variables <- names(data)[!(names(data) %in% c("year", target_variable, "total_cases"))] |> 
    # remove variables that are mostly/entirely missing
    (\(x) x[!(x %in% vars_selection[["vars_to_remove"]])])()
  
  model_recipe <- data_train |> 
    recipe() |> 
    step_rm(
      all_of(vars_selection[["vars_to_remove"]])
    ) |> 
    update_role(
      all_of(target_variable),
      new_role = "outcome"
    )
  
  if (model_type %in% c("random_forest")) {
    model_recipe <- model_recipe |> 
      step_rm(
        any_of(
          c("numerator", "remainder")
        )
      )
  }
  
  model_recipe <- model_recipe |> 
    update_role(
      all_of(predictor_variables),
      new_role = "predictor"
    )
  
  model_recipe <- model_recipe |> 
    step_impute_median(
      all_of(vars_selection[["vars_to_impute"]])
    )
  
  if (model_type %in% c("logistic_regression")) {
    model_recipe <- model_recipe |> 
      step_zv(
        all_of(predictor_variables)
      ) |> 
      step_normalize(
        any_of(predictor_variables)
      )
  }
  
  model_recipe <- model_recipe |> 
    prep(training = data_train, retain = TRUE)
  
  # start workflow
  modelling_workflow <- workflow() |> 
    add_model(model_setup) |> 
    add_recipe(model_recipe)
  
  # if logistic regression add case weights to workflow
  if (model_type == "logistic_regression") {
    modelling_workflow <- modelling_workflow |> 
      add_case_weights(total_cases)
  }
  
  if (model_type == "random_forest") {
    # design the tuning of the hyperparameters
    
    if (class(tuning_grid) == "data.frame") {
      tuning_grid <- tuning_grid
    } else {
      tuning_grid <- expand.grid(
        mtry = seq(
          from = round((length(predictor_variables) / 3), 0) - 10,
          to = round((length(predictor_variables) / 3), 0) + 10,
          by = 4
        ),
        min_n = seq(
          from = round(nrow(data_train) / 10, 0),
          to = round(nrow(data_train) / 10, 0) + 10,
          by = 4
        ),
        trees = seq(
          from = ceiling((length(predictor_variables) / 5) / 10) * 10,
          to = (ceiling((length(predictor_variables) / 5) / 10) * 10) + 100,
          by = 10
        ) * 10
      )
    }
  } else if (model_type == "logistic_regression") {
    tuning_grid <- 20
  }
 
  evaluation_metrics <- metric_set(
    yardstick::rmse,
    yardstick::rsq,
    yardstick::mae,
    yardstick::mape,
    yardstick::smape)
  
  residuals <- modelling_workflow |> 
    tune_grid(
      resamples = validation_set,
      grid = tuning_grid,
      metrics = evaluation_metrics,
      control = control_grid(save_pred = TRUE)
    )
  
  tuning_parameters <- autoplot(residuals)
    
  # select the best parameters
  best <- residuals |> 
    select_best(metric = "rsq")
  
  # the last workflow
  modelling_workflow_final <- modelling_workflow |>
    finalize_workflow(best)
  
  # the last fit
  model_fit <- last_fit(
    modelling_workflow_final,
    splits,
    add_validation_set = TRUE,
    metrics = evaluation_metrics
  )
  
  validation_metrics <- residuals |> 
    collect_metrics()
  
  if (model_type == "random_forest") {
    validation_metrics <- validation_metrics |> 
      inner_join(
        best,
        by = join_by(mtry, trees, min_n, .config)
      )
  } else if (model_type == "logistic_regression") {
    validation_metrics <- validation_metrics |> 
      inner_join(
        best,
        by = join_by(mixture, penalty, .config)
      )
  }
  
  validation_metrics <- validation_metrics |> 
    select(
      ".metric",
      .estimate = "mean"
    ) |> 
    mutate(
      data = "validation"
    )
  
  test_metrics <- model_fit |> 
    collect_metrics() |> 
    select(
      ".metric",
      ".estimate"
    ) |> 
    mutate(
      data = "test"
    )
  
  train_metrics <- calculate_train_metric(
    final_fit = model_fit,
    training_data = data_train,
    target_variable = target_variable,
    metric = "rsq"
  )
  
  evaluation_metrics <- bind_rows(
    train_metrics,
    validation_metrics,
    test_metrics
  ) |>
    tidyr::pivot_wider(
      names_from = data,
      values_from = .estimate
    )
  
  validation_row_years <- data_validation |> 
    mutate(
      .row = row_number()
    ) |> 
    select(
      ".row",
      "year"
    )
  
  validation_predictions <- residuals |> 
    collect_predictions(parameters = best) |> 
    mutate(
      data = "validation"
    ) |> 
    left_join(
      validation_row_years,
      by = join_by(.row)
    )
  
  test_predictions <- model_fit %>% 
    collect_predictions() |> 
    mutate(
      data = "test"
    ) |> 
    bind_cols(
      tibble(
        year = data_test$year
      )
    )
  
  train_predictions <- add_prediction_to_data(
    data = data_train,
    final_fit = model_fit
  ) |> 
    mutate(
      data = "train"
    )
  
  dataset_predictions <- bind_rows(
    train_predictions,
    validation_predictions,
    test_predictions
  )
    
  prediction_plot <- dataset_predictions |> 
    plot_observed_expected(
      target_variable = target_variable,
      distinguish_year = TRUE
    )
  
  # variable importance
  variable_importance <- model_fit |> 
    extract_fit_parsnip() |> 
    vip::vip(num_features = 10)
  
  inputs <- tibble(
    `Model type` = model_type,
    `Split type` = ifelse(time_series_split == TRUE, "Time-series", "Random"),
    `Shuffled training years` = ifelse(time_series_split == TRUE, shuffle_training_records, NA),
    `Training years` = (max(data_validation$year) - min(data_train$year)) + 1,
    `Lagged years` = lagged_years,
    `Current year included` = ifelse(lagged_years > 0, keep_current, NA),
    `Lagged target variable` = ifelse(lagged_years > 0, !remove_lag_target, NA)
  )
  
  output <- list(
    dataset_yrs = dataset_yrs,
    tuning_parameters = tuning_parameters,
    prediction_plot = prediction_plot,
    evaluation_metrics = evaluation_metrics,
    variable_importance = variable_importance,
    inputs = inputs
  )
  
  return(output)
  
}
