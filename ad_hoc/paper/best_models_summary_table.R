source("R/00_libraries.R")
source("R/04_modelling_utils.R")

# copy table to clipboard of the best models to paste into the paper
readRDS("tests/model_testing/model_summary_information.rds") |> 
  filter(
    `Tuning objective` == "mape"#,
    # !grepl("52", `Target variable`)
  ) |> 
  filter(
    `Test set value` == min(`Test set value`),
    .by = c(`Target variable`, `Model type`, `Target variable type`)
  ) |> 
  filter(
    `Number training years` == min(`Number training years`),
    .by = c(`Target variable`, `Model type`, `Target variable type`)
  ) |>
  arrange(
    `Target variable`,
    `Model type`,
    `Target variable type`
  ) |> 
  mutate(
    `Number lagged target years` = as.integer(
      stringr::str_extract(
        `Number lagged target years`,
        "[0-9]"
      )
    )
  ) |>
  filter(
    `Test set value` == min(`Test set value`),
    .by = `Target variable`
  ) |> 
  select(
    "Target variable",
    "Model type",
    "Target variable type",
    "Number training years", 
    "Number lagged years",
    "Number lagged target years",
    "Test set value"
  ) |> 
  mutate(
    across(
      c("Model type", "Target variable type"),
      ~ str_to_sentence(
        gsub(
          "_", " ", .x
        )
      )
    ),
    `Target variable type` = gsub("previous", "previous year", `Target variable type`),
    `Number training years` = `Number training years` + 1,
    `Test set value` = round(`Test set value`, 4)
  ) |> 
  rename_with(
    ~ gsub("Number", "Number of", .x)
  ) |> 
  rename(
    `Mean absolute percentage error` = "Test set value"
  ) |> 
  write.table(
    "clipboard", 
    sep = "\t", 
    row.names = FALSE, 
    col.names = TRUE
  )
