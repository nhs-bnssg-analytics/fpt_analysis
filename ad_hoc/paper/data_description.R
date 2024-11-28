source("R/00_libraries.R")
source("R/04_modelling_utils.R")

target_variable <- c(
  "Proportion of incomplete pathways greater than 18 weeks from referral",
  "Proportion of incomplete pathways greater than 52 weeks from referral",
  "Proportion of suspected cancer or referral to first definitive treatment that are longer than 62 days wait",
  "Proportion of A&E attendances with greater than 4 hours wait (Type 1 Departments - Major A&E)",
  "Proportion of attended GP appointments (over 4 weeks wait time)"
)

metrics <- read.csv("data/configuration-table.csv",
                    encoding = "latin1") |> 
  select(
    "metric", "domain"
  )

# removes records where NAs exist for target variables because the target
# variable columns only exist once in each tibble from the "map" step, but NAs
# are generated when they are list_rbind() together
clean_target_variables <- function(data, target_variables) {
  targ_vars <- data |> 
    filter(
      metric %in% target_variables,
      !is.na(value)
    )
  
  data <- data |> 
    filter(
      !(metric %in% target_variables)
    ) |> 
    bind_rows(
      targ_vars
    )
  
  return(data)
}

df <- target_variable |> 
  map(
    ~ load_data(
      target_variable = .x,
      value_type = "value",
      incl_numerator_remainder = FALSE,
      binary_covid = FALSE
    )
) |> 
  list_rbind() |> 
  select(!c("nhs_region", "quarter", "month")) |> 
  pivot_longer(
    cols = !c("year", "org"),
    names_to = "metric",
    values_to = "value"
  ) |> 
  distinct() |> 
  clean_target_variables(
    target_variables = target_variable
  )

min_max_years <- df |> 
  filter(!is.na(value)) |> 
  summarise(
    min_year = min(year),
    max_year = max(year),
    .by = metric
  )

df <- df |> 
  left_join(
    metrics,
    by = join_by(
      metric
    )
  ) |> 
  filter(
    !grepl("Total - Acute", metric)
  ) |> 
  mutate(
    domain = factor(domain, levels = c("Performance", "Demand", "Capacity"))
  ) |>
  left_join(
    min_max_years,
    by = join_by(
      metric
    )
  ) |> 
  filter(
    between(
      year,
      min_year,
      max_year
    )
  ) 

# df at this point is used to quantify the size of the data for the paper
dim(df)
df |> distinct(metric) |> count()


df_final <- df |> 
  select(!c("year", "org")) |> 
  tidyr::nest(
    .by = c(domain, metric, min_year, max_year)
  ) |> 
  mutate(
    data = map(
      data,
      \(x) x |> 
        summary() |> 
        as.data.frame() |> 
        separate(
          Freq,
          sep = ":",
          into = c("stat", "val")
        ) |> 
        mutate(
          stat = str_trim(stat),
          val = as.numeric(val)
        ) |> 
        select(
          !c("Var1", "Var2")
        ) |> 
        pivot_wider(
          names_from = stat,
          values_from = val
        )
    )
  ) |> 
  tidyr::unnest(
    cols = data
  ) |> 
  mutate(
    `NA's` = replace_na(`NA's`, 0),
    across(
      c(`Min.`, `1st Qu.`, Median, Mean, `3rd Qu.`, `Max.`),
      ~ round(.x, 1)
    )
  ) |> 
  arrange(
    domain, desc(metric)
  ) |> 
  rename(
    Domain = "domain",
    Metric = "metric",
    Minimum.year = "min_year",
    Maximum.year = "max_year"
  )


write.table(
  df_final, 
  "clipboard", 
  sep = "\t", 
  row.names = FALSE, 
  col.names = TRUE
)

# creating description chart ----------------------------------------------

library(ggplot2)
library(dplyr)
library(patchwork)

abbreviate_domain_metric_year <- function(vec, str_length, met, min_yr, max_yr) {
  abbreviation <- paste0(
    "(",
    substr(vec, 1, str_length),
    ") ",
    met,
    " (",
    min_yr,
    "-",
    max_yr,
    ")"
  )
}

max_x_limit <- 100

main_plot <- tibble::tribble(
                         ~Domain,                                                                                                               ~Metric, ~Minimum.year, ~Maximum.year, ~Min., ~`1st.Qu.`, ~Median, ~Mean, ~`3rd.Qu.`,  ~Max., ~`NA's`,
                   "Performance",          "Proportion of suspected cancer or referral to first definitive treatment that are longer than 62 days wait",         2012L,         2023L,   7.1,       13.9,    18.1,  20.7,         25,   51.8,      0L,
                   "Performance",                                               "Proportion of incomplete pathways greater than 52 weeks from referral",         2014L,         2023L,     0,          0,     0.1,   1.8,        3.3,   13.5,      0L,
                   "Performance",                                               "Proportion of incomplete pathways greater than 18 weeks from referral",         2014L,         2023L,   3.6,        8.3,    13.5,  20.5,       34.6,   51.2,      0L,
                   "Performance",                                                     "Proportion of attended GP appointments (over 4 weeks wait time)",         2019L,         2023L,   0.8,        2.3,     3.3,   3.4,        4.4,    8.1,      0L,
                   "Performance",                       "Proportion of A&E attendances with greater than 4 hours wait (Type 1 Departments - Major A&E)",         2016L,         2023L,   2.4,       15.8,    21.5,  24.4,       33.5,   53.7,      0L,
                        "Demand",                       "Proportion of resident population in national deprivation quintile (Most deprived) - IMD 2019",         2012L,         2022L,   0.6,         10,    13.4,  17.4,       20.6,   51.6,      0L,
                        "Demand",             "Proportion of resident population in national deprivation quintile (Least deprived quintile) - IMD 2019",         2012L,         2022L,   1.5,       11.8,    18.8,  20.5,       26.5,   51.2,      0L,
                        "Demand",                                                                         "Proportion of population in age band (0-29)",         2015L,         2023L,  30.1,       33.4,    35.2,  35.5,       37.6,   43.2,      0L,
                        "Demand",                                                                        "Proportion of population in age band (30-59)",         2015L,         2023L,  37.1,       39.3,      40,  40.7,       41.6,   47.3,      0L,
                        "Demand",                                                                          "Proportion of population in age band (60+)",         2015L,         2023L,    13,       21.2,    24.1,  23.8,       27.2,   32.2,      0L,
                        "Demand",                                                    "Stroke and transient ischaemic attack: QOF prevalence (all ages)",         2015L,         2023L,   0.9,        1.6,     1.9,   1.9,        2.2,    3.1,      0L,
                        "Demand",                                                                      "Rheumatoid arthritis: QOF prevalence (16+ yrs)",         2015L,         2023L,   0.5,        0.7,     0.8,   0.8,        0.9,      1,      0L,
                        "Demand",                                                              "Peripheral arterial disease: QOF prevalence (all ages)",         2015L,         2023L,   0.3,        0.5,     0.6,   0.6,        0.7,      1,      0L,
                        "Demand",                                                                          "Palliative care: QOF prevalence (all ages)",         2015L,         2023L,   0.2,        0.3,     0.4,   0.4,        0.5,    0.9,      0L,
                        "Demand",                                                                              "Osteoporosis: QOF prevalence (50+ yrs)",         2015L,         2023L,   0.2,        0.5,     0.7,   0.8,        0.9,    2.5,      0L,
                        "Demand",                                                                                   "Obesity: QOF prevalence (18+ yrs)",         2015L,         2023L,   5.1,        8.5,     9.9,    10,       11.4,   17.4,      0L,
                        "Demand",                                                                             "Hypertension: QOF prevalence (all ages)",         2015L,         2023L,  10.3,       13.2,    14.6,  14.4,       15.7,   18.3,      0L,
                        "Demand",                                                                            "Heart failure: QOF prevalence (all ages)",         2015L,         2023L,   0.4,        0.8,     0.9,   0.9,        1.1,    1.8,      0L,
                        "Demand",                                                                                  "Epilepsy: QOF prevalence (18+ yrs)",         2015L,         2023L,   0.5,        0.7,     0.8,   0.8,        0.9,    1.2,      0L,
                        "Demand",                                                                         "Diabetes mellitus: QOF prevalence (17+ yrs)",         2015L,         2023L,     5,        6.5,     7.1,     7,        7.5,    9.7,      0L,
                        "Demand",                                                                                 "Dementia: QOF prevalence (all ages)",         2015L,         2023L,   0.3,        0.7,     0.8,   0.8,        0.9,    1.1,      0L,
                        "Demand",                                                                                   "Cancer: QOF prevalence (all ages)",         2015L,         2023L,   1.3,        2.7,     3.1,   3.2,        3.7,    5.2,      0L,
                        "Demand",                                                                                     "COPD: QOF prevalence (all ages)",         2015L,         2023L,   0.9,        1.6,     1.9,   1.9,        2.2,    2.9,      0L,
                        "Demand",                                                                                       "CKD: QOF prevalence (18+ yrs)",         2015L,         2023L,     2,        3.6,     4.4,   4.3,          5,    7.6,      0L,
                        "Demand",                                                                                      "CHD: QOF prevalence (all ages)",         2015L,         2023L,   1.8,        2.7,     3.1,   3.1,        3.7,    4.5,      0L,
                        "Demand",                                                                      "Atrial fibrillation: QOF prevalence (all ages)",         2015L,         2023L,   0.8,        1.8,     2.1,   2.1,        2.5,    3.2,      0L,
                        "Demand",                                                                                   "Asthma: QOF prevalence (all ages)",         2015L,         2023L,   4.2,        5.9,     6.3,   6.2,        6.7,    7.6,      0L,
                        "Demand",                                                                                             "% active smokers (GPPS)",         2013L,         2023L,  10.6,       13.7,      15,  15.2,       16.4,   22.3,      0L,
                      "Capacity",                                                          "Total beds per 1,000 60+ yrs (General & Acute - overnight)",         2014L,         2023L,   0.9,        1.3,     1.5,   1.6,        1.7,    3.7,      0L,
                      "Capacity",                                         "Proportion of available beds that are occupied (Mental Illness - overnight)",         2014L,         2023L,  65.6,         86,    88.7,  88.1,       91.2,  127.4,      0L,
                      "Capacity",                                              "Proportion of available beds that are occupied (Maternity - overnight)",         2014L,         2023L,  33.4,       50.6,      59,  58.8,         66,   90.3,      0L,
                      "Capacity",                                        "Proportion of available beds that are occupied (General & Acute - overnight)",         2014L,         2023L,  73.2,       87.4,    89.7,  88.8,       91.8,   97.1,      0L,
                      "Capacity",                                              "Proportion of available beds that are occupied (General & Acute - day)",         2014L,         2023L,  49.2,       77.7,    84.3,  82.9,       89.8,     99,      0L,
                      "Capacity",                                                       "Primary care workforce (FTEs) per 100,000 population (Nurses)",         2018L,         2023L,  12.5,       23.6,    30.2,  28.5,       33.9,   41.1,      0L,
                      "Capacity",                                                           "Primary care workforce (FTEs) per 100,000 population (GP)",         2018L,         2023L,  45.2,       53.2,    56.6,    57,       60.7,   71.7,      0L,
                      "Capacity",                                          "Primary care workforce (FTEs) per 100,000 population (Direct Patient Care)",         2018L,         2023L,   7.9,       16.4,    22.6,  24.3,       29.4,   57.6,      0L,
                      "Capacity",                                           "Primary care workforce (FTEs) per 100,000 population (Admin/Non-Clinical)",         2018L,         2023L,  83.9,      103.1,     119, 116.9,      128.2,  160.9,      0L,
                      "Capacity",                                                           "Mental health spend as a proportion of overall allocation",         2016L,         2023L,   9.9,       12.4,    13.6,  13.8,       15.1,   18.5,      4L,
                      "Capacity",                                                                            "Mental Health total spend per population",         2016L,         2023L, 114.9,      164.4,     187, 193.8,      217.1,  342.2,      0L,
                      "Capacity",                                                 "Mean proportion of beds that contain a patient with confirmed COVID",         2014L,         2023L,     0,          0,       0,     2,        4.3,    9.2,      0L,
                      "Capacity",                                               "Improving Access to Psychological Therapies spend (£k) per population",         2016L,         2023L,     0,        7.2,     9.5,  10.2,       12.7,   20.3,      8L,
                      "Capacity",                                                                 "FTE days sick per 10,000 population (mental health)",         2015L,         2023L,  10.3,       92.6,   139.9, 146.5,      188.8,    487,      0L,
                      "Capacity",                                                                         "FTE days sick per 10,000 population (acute)",         2015L,         2023L, 275.6,        433,   530.9, 567.9,      661.2, 1219.8,      0L,
                      "Capacity",                                                               "FTE days available per 100 population (mental health)",         2015L,         2023L,   2.2,         21,    27.6,  29.1,       37.8,  110.4,      0L,
                      "Capacity",                                                                       "FTE days available per 100 population (acute)",         2015L,         2023L,  74.8,      111.6,   122.6,   127,      138.8,  220.4,      0L,
                      "Capacity",                                                           "Early Intervention in Psychosis spend (£k) per population",         2016L,         2023L,     0,        2.1,     2.9,     3,        3.9,   17.1,      6L,
                      "Capacity", "Children and Young People Mental Health spend - excluding learning disabilities and eating disorders per population",         2016L,         2023L,   5.7,       10.7,    12.6,  13.4,       15.7,   30.3,      0L,
                      "Capacity",                                     "Children and Young People Mental Health spend - eating disorders per population",         2016L,         2023L,     0,        0.7,       1,   1.1,        1.3,      4,      0L,
                      "Capacity",                                                               "Adult community crisis care spend (£k) per population",         2016L,         2023L,     0,          5,     7.5,   7.8,        9.8,   21.3,      0L,
                      "Capacity",                                           "A and E and Ward Liaison mental health services spend (£k) per population",         2016L,         2023L,     0,          2,     3.1,   3.4,        4.3,   13.4,      0L,
                      "Capacity",                                              "A and E Full Time Equivalent (FTE) for Nurses (per 100,000 population)",         2018L,         2023L,  13.4,       22.3,    25.7,  26.1,       29.9,   39.8,      0L,
                      "Capacity",                     "A and E Full Time Equivalent (FTE) for Doctors (excluding Consultants) (per 100,000 population)",         2018L,         2023L,   3.5,        7.6,     8.8,     9,       10.4,   15.9,      0L,
                      "Capacity",                                         "A and E Full Time Equivalent (FTE) for Consultants (per 100,000 population)",         2018L,         2023L,   1.2,        2.9,     3.3,   3.3,        3.7,    5.1,      0L,
                      "Capacity",                                      "A and E Full Time Equivalent (FTE) for Care Providers (per 100,000 population)",         2018L,         2023L,   8.8,       12.4,    15.3,  15.8,       18.5,   29.5,      0L
                   ) |> 

  rename(
    min = "Min.",
    max = "Max.",
    first_quartile = "1st.Qu.",
    third_quartile = "3rd.Qu.",
    mean = "Mean",
    median = "Median"
  ) |> 
  mutate(
    metric = case_when(
      Domain == "Performance" ~ abbreviate_domain_metric_year(Domain, 4, Metric, Minimum.year, Maximum.year),
      .default = abbreviate_domain_metric_year(Domain, 3, Metric, Minimum.year, Maximum.year)
    ),
    .keep = "unused"
  ) |> 
  mutate(
    across(
      .cols = all_of(
        c("min", 
          "first_quartile",
          "mean",
          "median",
          "third_quartile",
          "max"
        )
      ),
      .fns = ~ if_else(.x > max_x_limit, as.character(.x), "-"),
      .names = "{.col}_label"
    )
  ) |> 
  mutate(
    final_label = paste0(
      "(",
      min_label,
      ", ",
      first_quartile_label,
      ", ",
      median_label,
      ", ",
      mean_label,
      ", ",
      third_quartile_label,
      ", ",
      max_label,
      ")"
    ),
    final_label = case_when(
      final_label == "(-, -, -, -, -, -)" ~ NA_character_,
      .default = final_label
    ),
    .keep = "unused"
  ) |> 
  mutate(
    across(
      .cols = all_of(
        c("min",
          "first_quartile",
          "third_quartile",
          "max"
        )
      ),
      .fns = ~ if_else(.x > max_x_limit, max_x_limit, .x)
    ),
    across(
      .cols = all_of(
        c("mean",
          "median"
        )
      ),
      .fns = ~ if_else(.x > max_x_limit, NA_real_, .x)
    )
  ) |> #View()
  ggplot(
    aes(
      y = metric
    )
  ) +
  geom_segment(
    x = 0,
    xend = 100,
    aes(
      yend = metric  
    ),
    colour = "gray90"
  ) +
  geom_segment(
    aes(
      x = min,
      xend = max,
      yend = metric
    ),
    colour = "gray50"
  ) +
  geom_segment(
    aes(
      x = first_quartile,
      xend = third_quartile,
      yend = metric
    ),
    colour = "black",
    linewidth = 1
  ) + 
  geom_point(
    aes(
      x = mean
    ), 
    shape = 16,
    size = 4
  ) +
  geom_point(
    aes(
      x  = median
    ),
    shape = 4,
    size = 4
  ) +
  geom_text(
    aes(
      label = final_label,
      x = max_x_limit * 1.02
    ),
    hjust = 0,
    size = 3
  ) +
  coord_cartesian(
    xlim = c(0, max_x_limit),
    ylim = c(0, NA),
    expand = FALSE,
    clip = "off"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(
      r = 180
    )
  ) +
  labs(
    x = "Value",
    y = "",
    title = ""
  ) +
  annotate(
    "text", 
    x = max_x_limit * 1.02, 
    y = -1, 
    size = 3,
    hjust = 0,
    label = "(Min, 1st.Qu.,Med, Mean, 3rd.Qu., Max)")


legend_plot <- tibble(
  min = 0,
  first_quartile = 0.25,
  median = 0.7,
  mean = 1,
  third_quartile = 1.3, 
  max = 1.6,
  metric = "(Domain) Metric name"
) |> 
  ggplot(
    aes(y = metric)
  ) +
  geom_segment(
    aes(
      x = min,
      xend = max,
      yend = metric
    ),
    colour = "gray50"
  ) +
  geom_segment(
    aes(
      x = first_quartile,
      xend = third_quartile,
      yend = metric
    ),
    colour = "black",
    linewidth = 1
  ) + 
  geom_point(
    aes(
      x = mean
    ), 
    shape = 16,
    size = 4
  ) +
  geom_point(
    aes(
      x  = median
    ),
    shape = 4,
    size = 4
  ) +
  geom_text(
    data = tibble(
      position = c(0, 0.25, 0.7, 1, 1.3, 1.6),
      label = c("Min", "1st.Qu.", "Median", "Mean", "3rd.Qu.", "Max"),
      metric = "(Domain) Metric name"
    ),
    aes(
      x = position,
      label = label
    ),
    vjust = -1
  ) +
  theme_void() +
  theme(
    axis.text.y = element_text()
  ) +
  scale_y_discrete(
    expand = expansion(add = 1.5)
  )

layout <- "
AAAAA
#####
#BB##
"

final_plot <- main_plot / legend_plot +
  patchwork::plot_layout(
    heights = c(25, 0.5, 1.5),
    design = layout
  )

ggsave(
  plot = final_plot, 
  "ad_hoc/paper/images/data_description.png",
  width = 20,
  height = 20 * 15 / 24.5,
  units = "in"
  )
