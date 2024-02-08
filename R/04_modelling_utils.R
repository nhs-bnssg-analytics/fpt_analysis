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

calculate_train_metric <- function(final_fit, training_data, target_variable, case_weights = FALSE) {
  
  training_metric <- add_prediction_to_data(
    data = training_data,
    final_fit = final_fit
  ) |> 
    rename(
      observed = all_of(target_variable)
    )
  
  
  if (case_weights) {
    # rsq <- training_metric  |> 
    #   yardstick::rsq(
    #     truth = observed,
    #     estimate = .pred,
    #     case_weights = total_cases
    #   )
    rmse <- training_metric  |> 
      yardstick::rmse(
        truth = observed,
        estimate = .pred,
        case_weights = total_cases
      )
    mae <- training_metric  |> 
      yardstick::mae(
        truth = observed,
        estimate = .pred,
        case_weights = total_cases
      )
    mape <- training_metric  |> 
      yardstick::mape(
        truth = observed,
        estimate = .pred,
        case_weights = total_cases
      )
    smape <- training_metric  |> 
      yardstick::smape(
        truth = observed,
        estimate = .pred,
        case_weights = total_cases
      )
  } else {
    # rsq <- training_metric  |> 
    #   yardstick::rsq(
    #     truth = observed,
    #     estimate = .pred
    #   )
    rmse <- training_metric  |> 
      yardstick::rmse(
        truth = observed,
        estimate = .pred
      )
    mae <- training_metric  |> 
      yardstick::mae(
        truth = observed,
        estimate = .pred
      )
    mape <- training_metric  |> 
      yardstick::mape(
        truth = observed,
        estimate = .pred
      )
    smape <- training_metric  |> 
      yardstick::smape(
        truth = observed,
        estimate = .pred
      )
  }
  
  training_metric <- bind_rows(
    # rsq,
    rmse,
    mae,
    mape,
    smape
  ) |> 
    mutate(
      data = "train",  
    ) |> 
    select(!c(".estimator"))
  
  return(training_metric)
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


# plotting functions ------------------------------------------------------

plot_observed_expected <- function(data, target_variable, distinguish_year) {
  p <- data |> 
    mutate(
      data = factor(
        data,
        levels = c("train", "validation", "test"),
        labels = c("Train", "Validation", "Test")
      )
    ) |> 
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
        Test = "#33a02c",
        Validation = "#ff7f00",
        Train = "#1f78b4"
      ),
      breaks = c("Train", "Validation", "Test"),
      drop = TRUE
    ) +
    facet_wrap(
      facets = vars(data),
      ncol = 3
    )
  
  
  return(p)
}

plot_modelling_performance <- function(modelling_results, inputs, val_type, 
                                       evaluation_metric, chart_subtitle, 
                                       figure_caption, show_validation_variance, model_type) {
  
  table <- modelling_results[[nrow(inputs)]]$inputs |> 
    select(!c("Training years", "Lagged years")) |> 
    mutate(
      `Validation method` = val_type
    )
  
  table_annotation <- ggplot(table) +
    ggplot2::annotation_custom(
      tableGrob(table, rows = NULL, 
                theme = ttheme_default(base_size = 5)),
      xmin = 0, 
      xmax = 1, 
      ymin = -0, 
      ymax = 1
    ) +
    theme(
      rect = element_blank()
    )
  
  modelling_plot <- modelling_results |> 
    map_df(
      ~ pluck(.x, "evaluation_metrics"),
      .id = "input_id"
    ) |> 
    mutate(
      input_id = as.integer(input_id)
    ) |> 
    filter(
      .metric == evaluation_metric
    ) |> 
    left_join(
      inputs,
      by = join_by(input_id)
    ) |> 
    mutate(
      lagged_years = paste(
        lagged_years,
        "lagged years"
      )
    ) |> 
    pivot_longer(
      cols = c(train, validation, test),
      names_to = "data_type",
      values_to = evaluation_metric
    ) |> 
    ggplot(
      aes(
        x = training_years,
        y = .data[[evaluation_metric]]
      )
    ) +
    # geom_hline(
    #   yintercept = 0.5,
    #   linetype = "dashed"
    # ) +
    geom_line(
      aes(
        group = data_type,
        colour = data_type
      )
    ) +
    theme_bw() +
    facet_wrap(
      facets = vars(lagged_years),
      ncol = 1
    ) +
    ylim(0, NA) +
    scale_colour_manual(
      name = "Data type",
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
    ) +
    labs(
      title = paste(
        toupper(evaluation_metric),
        "for",
        gsub("_", " ", model_type),
        "where different combinations of lagged years and training years were applied"
      ),
      subtitle = chart_subtitle,
      caption = figure_caption,
      x = "Number of training years",
      y = evaluation_metric
    ) +
    theme(
      plot.title = element_text(size = 8),
      plot.subtitle = element_text(size = 6),
      plot.caption = element_text(size = 5),
      axis.text = element_text(size = 5),
      axis.title = element_text(size = 9),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7)
    )
  
  if (show_validation_variance) {
    validation_data <- modelling_results |> 
      map_df(
        ~ pluck(.x, "errors_per_fold"),
        .id = "input_id"
      ) |> 
      mutate(
        input_id = as.integer(input_id)
      ) |> 
      filter(
        .metric == evaluation_metric
      ) |> 
      left_join(
        inputs,
        by = join_by(input_id)
      ) |> 
      mutate(
        lagged_years = paste(
          lagged_years, "lagged years"
        )
      ) |> 
      pivot_wider(
        names_from = .metric,
        values_from = .estimate
      )
    
    modelling_plot <- modelling_plot +
      geom_point(
        data = validation_data,
        colour = "gray30",
        aes(
          shape = .estimator
        )
      ) +
      scale_shape_manual(
        name = "",
        values = c(
          standard = 1
        ),
        labels = c(
          standard = "Error on validation\nset folds"
        )
      ) + 
      guides(
        color = guide_legend(order = 1),
        shape = guide_legend(order = 2)
      )
    
  }
  
  p <- modelling_plot / table_annotation + 
    plot_layout(
      heights = c(6, 1)
    )
  
  return(p)
}

plot_variable_importance <- function(model_last_fit, top_n = 10) {
  
  model_type <- model_last_fit |> 
    extract_fit_engine() |> 
    pluck("call") |> 
    as.character() |> 
    head(1)
  
  if (grepl("random", model_type, ignore.case = TRUE)) {
    model_type <- "random_forest"
  } else if (grepl("glm", model_type, ignore.case = TRUE)) {
    model_type <- "logistic_regression"
  } else {
    stop("unknown model type")
  }
  
  p <- model_last_fit |> 
    extract_fit_parsnip() |> 
    vi() |> 
    filter(
      Importance != 0
    ) |> 
    head(top_n) |>
    arrange(
      Importance
    ) |> 
    mutate(
      Variable = factor(Variable,
                        levels = Variable)
    ) |> 
    ggplot(
      aes(
        x = Importance,
        y = Variable
      )
    ) +
    theme_minimal()
  
  if (model_type == "logistic_regression") {
    p <- p +
      geom_col(
        aes(
          fill = Sign
        )
      ) +
      scale_fill_manual(
        name = "Direction of impact",
        values = c(
          POS = "#56B4E9",
          NEG = "#E69F00"
        ),
        labels = c(
          POS = "Positive",
          NEG = "Negative"
        )
      )
  } else if (model_type == "random_forest") {
    p <- p +
      geom_col() 
  }
  
  return(p)
}

#load data --------------------------------------------------------------- '
#' @param binary_covid logical; whether to remove all calculated COVID fields 
#' and include one that is 1 for years from 2020 onwards, and 0 prior to that
load_data <- function(target_variable, value_type = "value", incl_numerator_remainder = FALSE,
                      broad_age_bands = TRUE, binary_covid = TRUE) {
  
  metrics <- read.csv(
    here::here("data/configuration-table.csv"),
    encoding = "latin1"
  ) |> 
    filter(
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
  
  if (binary_covid) {
    dc_data <- dc_data |> 
      select(
        !contains("COVID")
      ) |> 
      mutate(
        pandemic_onwards = case_when(
          year >= 2020 ~ 1L,
          .default = 0L
        )
      )
  }
  
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
#' @param model_type character string; the modelling method to use. Currently
#'   accepts "logistic_regression" or "random_forest"
#' @param tuning_grid data.frame with numeric columns for mtry, min_n and trees
#'   (if using random_forest) or a signle column named threshold (if performing
#'   logistic_regression). Can also take the value "auto", which is the default.
#' @param predict_proportions logical; should proportions be the predicted value
#'   (TRUE) or a count (FALSE)
#' @param validation_type string; either "cross_validation",
#'   "leave_group_out_validation" or "train_validation"
#' @param seed numeric; seed number
#' @param eval_metric string; one of "rmse", "mae", "smape", "mape"
#' @details This webpage was useful
#'   https://www.tidyverse.org/blog/2022/05/case-weights/
#' 
modelling_performance <- function(data, target_variable, lagged_years = 0, 
                                  keep_current = TRUE, remove_lag_target = TRUE,
                                  time_series_split = TRUE, training_years = NULL,
                                  remove_years = NULL,
                                  shuffle_training_records = FALSE,
                                  model_type = "logistic_regression", 
                                  tuning_grid = "auto",
                                  predict_proportions = TRUE,
                                  validation_type,
                                  eval_metric,
                                  seed = 321) {
  
  model_type <- match.arg(
    model_type,
    c("random_forest", "logistic_regression")
  )
  
  validation_type <- match.arg(
    validation_type,
    c("train_validation", "leave_group_out_validation", "cross_validation")
  )
  
  eval_metric <- match.arg(
    eval_metric,
    c("rmse", "mae", "mape", "smape")
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
  
  print(
    paste0(
      training_years,
      " training years, ",
      lagged_years,
      " lagged years ",
      Sys.time()
    )
  )
  
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
    path_length <- 300
    pen_vals <- 10 ^ seq(-4, 0, length.out = path_length)
    
    model_setup <- parsnip::linear_reg(
      penalty = tune(),
      mixture = tune()
    ) |> 
      set_engine(
        "glmnet", 
        family = stats::quasibinomial(link = "logit"), 
        # path_values = pen_vals,
        nlambda = 150,
        num.threads = cores,
        standardize = FALSE
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
      step_range(
        all_numeric_predictors(),
        clipping = FALSE
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
    
    extract_input <- NULL
  } else if (model_type == "logistic_regression") {
    tuning_grid <- crossing(
      penalty = pen_vals[seq_len(path_length) %% 10 == 0], 
      mixture = seq(
        from = 0,
        to = 1.0,
        length.out = 10
      ))
    
    tuning_grid <- 30
    
    # obtain resample coefficients function
    get_glmnet_coefs <- function(x) {
      x %>% 
        extract_fit_engine() %>% 
        tidy(return_zeros = TRUE) %>% 
        rename(penalty = lambda)
    }
    
    extract_input <- get_glmnet_coefs
  }
 
  evaluation_metrics <- metric_set(
    yardstick::rmse,
    # yardstick::rsq,
    yardstick::mae,
    yardstick::mape,
    yardstick::smape)
  
  residuals <- modelling_workflow |> 
    tune_grid(
      resamples = validation_set,
      grid = tuning_grid,
      metrics = evaluation_metrics,
      control = control_grid(
        save_pred = TRUE,
        extract = extract_input
      )
    )
  
  tuning_parameters <- autoplot(residuals)
    
  # collate coefficients
  if (model_type == "logistic_regression") {
    tuning_coefs <- 
      residuals |> 
      select(id, .extracts) |> 
      unnest(.extracts) |> 
      select(id, mixture, .extracts) |>  
      slice(
        1,
        .by = c(mixture, id)
      ) |>   # │ Remove the redundant results
      unnest(.extracts)
    
    joining_fields <- join_by(penalty, mixture, .config)
  } else if (model_type == "random_forest") {
    joining_fields <- join_by(mtry, min_n, trees, .config)
  }
  
  # select the best parameters
  best <- residuals |> 
    select_best(metric = eval_metric)
  
  # create metrics for each fold for best performing model
  errors_per_fold <- collect_metrics(
    residuals,
    summarize = FALSE
  ) |> 
    inner_join(
      best,
      by = joining_fields
    )
  
  
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
        by = joining_fields
      )
  } else if (model_type == "logistic_regression") {
    validation_metrics <- validation_metrics |> 
      inner_join(
        best,
        by = joining_fields
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
  
  if (model_type == "logistic_regression") {
    case_wts <- TRUE
  } else if (model_type == "random_forest") {
    case_wts <- FALSE
  }
  
  train_metrics <- calculate_train_metric(
    final_fit = model_fit,
    training_data = data_train,
    target_variable = target_variable,
    case_weights = case_wts
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
  
  if (validation_type == "train_validation") {
    validation_row_years <- data_validation |> 
      mutate(
        .row = row_number()
      ) |> 
      select(
        ".row",
        "year"
      )
    
  } else if (validation_type %in% c("cross_validation", "leave_group_out_validation")) {
    validation_row_years <- data_train |> 
      bind_rows(data_validation) |> 
      mutate(
        .row = row_number()
      ) |> 
      select(
        ".row",
        "year"
      )
    
  }
  
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
  
  inputs <- tibble(
    `Model type` = model_type,
    `Split type` = ifelse(time_series_split == TRUE, "Time-series", "Random"),
    `Shuffled training years` = ifelse(time_series_split == TRUE, shuffle_training_records, NA),
    `Training years` = training_years,
    `Lagged years` = lagged_years,
    `Current year included` = ifelse(lagged_years > 0, keep_current, NA),
    `Lagged target variable` = ifelse(lagged_years > 0, !remove_lag_target, NA)
  )
  
  output <- list(
    dataset_yrs = dataset_yrs,
    tuning_parameters = tuning_parameters,
    prediction_plot = prediction_plot,
    evaluation_metrics = evaluation_metrics,
    errors_per_fold = errors_per_fold,
    inputs = inputs,
    # coeffs = tuning_coefs
    ft = model_fit
  )
  
  return(output)
  
}

# Modelling output functions ----------------------------------------------

pick_best_model <- function(modelling_outputs, evaluation_metric) {
  best_model <- modelling_outputs |> 
    map_df(
      ~ pluck(.x, "evaluation_metrics")
    ) |> 
    filter(
      .metric == evaluation_metric
    ) |> 
    select(
      metric = "test"
    ) |> 
    mutate(
      input_id = row_number()
    ) |> 
    filter(metric == min(metric)) |> 
    slice(1)
  
  best_model_id <- best_model |> 
    pull(input_id)
  
  outputs <- modelling_outputs[[best_model_id]]
  
  outputs[["best_metric"]] <- best_model |> 
    pull(metric)
  
  return(outputs)
}
