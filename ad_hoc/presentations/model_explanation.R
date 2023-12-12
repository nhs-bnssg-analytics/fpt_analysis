y_axis_breaks <- set_names(
  c(
    paste0("ICS", 1:18),
    "...",
    "ICSn"
    ),
  nm = paste0("ICS", 1:20)
)

tibble(
  ics = factor(
    rep(paste0("ICS", 1:20), times = 9),
    levels = rev(paste0("ICS", 1:20))
  ),
  year = rep(2015:2023, each = 20),
  data_type = case_when(
    year == 2023 ~ "Test",
    .default = sample(c("Train", "Validation"),
                      size = 20 * 9,
                      replace = TRUE,
                      prob = c(0.7, 0.3))
  )
) |> 
  ggplot(
    aes(
      x = year,
      y = ics
    )
  ) +
  geom_tile(
    aes(
      fill = data_type
    ),
    colour = "black"
  ) +
  geom_text(
    label = "D&C input data\nPerformance metric",
    size = 2
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = 2015:2023
  ) +
  scale_y_discrete(
    labels = set_names(
      c(
        paste0("ICS", 1:18),
        "...",
        "ICSn"
      ),
      nm = paste0("ICS", 1:20)
    )
  ) +
  scale_fill_manual(
    name = "Data type",
    values = c(
      Test = "#33a02c",
      Validation = "#ff7f00",
      Train = "#1f78b4"
    ),
    labels = c(
      Test = "Testing the final model",
      Validation = "Validating the model parameters",
      Train = "Training the model"
    ),
    breaks = c("Train", "Validation", "Test"),
    drop = TRUE
  ) +
  labs(
    title = "How the modelling is being performed",
    y = "",
    x = ""
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank()
  )


ggsave(
  "modelling_process.png"
)
f