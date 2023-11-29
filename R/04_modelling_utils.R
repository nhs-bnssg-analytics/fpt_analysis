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

linear_model_metrics <- function(data, model_fit) {
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
