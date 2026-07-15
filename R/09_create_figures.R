library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(here)

#Settings and Aesthetics

navy <- "#0C234B"
red <- "#AB0520"
blue <- "#1E5288"
light_blue <- "#81A3C7"
light_gray <- "#E6E7E8"
dark_gray <- "#4A4A4A"
white <- "#FFFFFF"

finish_order <- c(
  "Sweet 16",
  "Elite Eight",
  "Final Four",
  "Runner-up",
  "Champion"
)

dir.create(
  here("figures"),
  recursive = TRUE,
  showWarnings = FALSE
)

project_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold",
      color = navy
    ),
    plot.subtitle = element_text(
      size = 13,
      color = dark_gray,
      margin = margin(b = 16)
    ),
    plot.caption = element_text(
      size = 9,
      color = dark_gray,
      hjust = 0,
      lineheight = 1.15,
      margin = margin(t = 14)
    ),
    axis.title = element_text(
      face = "bold",
      color = navy
    ),
    axis.text = element_text(
      color = dark_gray
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = light_gray,
      linewidth = 0.4
    ),
    plot.background = element_rect(
      fill = white,
      color = NA
    ),
    panel.background = element_rect(
      fill = white,
      color = NA
    ),
    plot.margin = margin(
      t = 20,
      r = 25,
      b = 45,
      l = 20
    )
  )

#Load Historical Data

historical_statistics <- read_csv(
  here(
    "data",
    "processed",
    "historical_tournament_statistics_2026.csv"
  ),
  show_col_types = FALSE
)

historical_summary <- read_csv(
  here(
    "data",
    "processed",
    "historical_tournament_summary_2026.csv"
  ),
  show_col_types = FALSE
)

#FIGURE 01: Historical Data Coverage

historical_coverage <- historical_statistics |>
  count(
    season,
    name = "tournament_games"
  ) |>
  complete(
    season = 2012:2025,
    fill = list(
      tournament_games = 0L
    )
  )

coverage_subtitle <- paste0(
  sum(historical_coverage$tournament_games > 0),
  " tournaments | ",
  comma(sum(historical_coverage$tournament_games)),
  " tournament games | ",
  comma(nrow(distinct(historical_summary, season, team))),
  " Sweet 16-or-better team-seasons"
)

figure_01 <- ggplot(
  historical_coverage,
  aes(
    x = factor(season, levels = 2012:2025),
    y = tournament_games
  )
) +
  geom_col(
    fill = blue,
    width = 0.72
  ) +
  geom_text(
    data = filter(
      historical_coverage,
      tournament_games > 0
    ),
    aes(
      label = tournament_games
    ),
    vjust = -0.5,
    size = 3.8,
    fontface = "bold",
    color = navy
  ) +
  annotate(
    "text",
    x = 9,
    y = 6,
    label = "2020 tournament\ncanceled",
    size = 3.6,
    fontface = "bold",
    color = dark_gray
  ) +
  scale_y_continuous(
    limits = c(0, 76),
    breaks = seq(0, 70, 10),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Fourteen Years of NCAA Tournament History",
    subtitle = coverage_subtitle,
    x = "Tournament season",
    y = "Games included",
    caption = paste(
      "Historical training data include every played NCAA Men's Tournament from 2012 through 2025.",
      "The 2020 tournament was canceled; 2021 contains 66 played games because Oregon–VCU was declared a no-contest.",
      sep = "\n"
    )
  ) +
  project_theme +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  filename = here(
    "figures",
    "01_historical_data_coverage.png"
  ),
  plot = figure_01,
  width = 12,
  height = 7.8,
  units = "in",
  dpi = 300,
  bg = white
)

print(figure_01)

# FIGURE 02: Programs with the Deepest Tournament Runs

top_programs <- historical_summary |>
  count(
    team,
    name = "total_appearances"
  ) |>
  slice_max(
    total_appearances,
    n = 15,
    with_ties = FALSE
  ) |>
  pull(team)

deep_run_programs <- historical_summary |>
  filter(
    team %in% top_programs
  ) |>
  mutate(
    tournament_finish = factor(
      tournament_finish,
      levels = finish_order
    )
  ) |>
  count(
    team,
    tournament_finish,
    name = "appearances"
  ) |>
  group_by(team) |>
  mutate(
    total_appearances = sum(appearances)
  ) |>
  ungroup() |>
  mutate(
    team = reorder(
      team,
      total_appearances
    )
  )

figure_02 <- ggplot(
  deep_run_programs,
  aes(
    x = appearances,
    y = team,
    fill = tournament_finish
  )
) +
  geom_col(
    width = 0.72
  ) +
  scale_x_continuous(
    breaks = pretty_breaks()
  ) +
  scale_fill_manual(
    values = c(
      "Sweet 16" = light_blue,
      "Elite Eight" = blue,
      "Final Four" = navy,
      "Runner-up" = dark_gray,
      "Champion" = red
    ),
    drop = FALSE
  ) +
  labs(
    title = "Programs With the Most Deep Tournament Runs",
    subtitle = "Top 15 programs by Sweet 16-or-better appearances, 2012–2025",
    x = "Tournament appearances",
    y = NULL,
    fill = "Deepest finish",
    caption = str_wrap(
      paste(
        "Each segment represents the deepest round reached by a program",
        "during one tournament season in the historical dataset."
      ),
      width = 110
    )
  ) +
  project_theme +
  theme(
    panel.grid.major.x = element_line(
      color = light_gray,
      linewidth = 0.4
    ),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

figure_02_path <- here(
  "figures",
  "02_deep_run_programs.png"
)

ggsave(
  filename = figure_02_path,
  plot = figure_02,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300,
  bg = white
)

message(
  "Created Figure 02: ",
  normalizePath(
    figure_02_path,
    winslash = "/",
    mustWork = FALSE
  )
)

print(figure_02)

#FIGURE 03: Historical Champion Profile

profile_features <- c(
  "avg_point_margin",
  "avg_offensive_rating",
  "avg_defensive_rating",
  "avg_efg_pct",
  "avg_tov_pct",
  "avg_orb_pct",
  "avg_drb_pct",
  "avg_opponent_efg_pct"
)

feature_labels <- c(
  avg_point_margin = "Point margin",
  avg_offensive_rating = "Offensive rating",
  avg_defensive_rating = "Defensive rating",
  avg_efg_pct = "Effective FG%",
  avg_tov_pct = "Ball security",
  avg_orb_pct = "Offensive rebounding",
  avg_drb_pct = "Defensive rebounding",
  avg_opponent_efg_pct = "Opponent eFG% defense"
)

championship_profile <- historical_summary |>
  mutate(
    tournament_finish = factor(
      tournament_finish,
      levels = finish_order
    ),
    
    # Reverse metrics where lower values indicate stronger performance.
    avg_defensive_rating = -avg_defensive_rating,
    avg_tov_pct = -avg_tov_pct,
    avg_opponent_efg_pct = -avg_opponent_efg_pct
  ) |>
  group_by(tournament_finish) |>
  summarise(
    across(
      all_of(profile_features),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = all_of(profile_features),
    names_to = "feature",
    values_to = "value"
  ) |>
  group_by(feature) |>
  mutate(
    standardized_value = as.numeric(
      scale(value)
    )
  ) |>
  ungroup() |>
  mutate(
    feature = factor(
      feature_labels[feature],
      levels = rev(
        unname(feature_labels)
      )
    )
  )

figure_03 <- ggplot(
  championship_profile,
  aes(
    x = tournament_finish,
    y = feature,
    fill = standardized_value
  )
) +
  geom_tile(
    color = white,
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = number(
        standardized_value,
        accuracy = 0.1
      )
    ),
    size = 3.7,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = red,
    mid = white,
    high = blue,
    midpoint = 0
  ) +
  labs(
    title = "The Statistical Profile of a Deep Tournament Run",
    subtitle = paste(
      "Average performance by deepest tournament finish;",
      "positive values indicate stronger-than-average results"
    ),
    x = "Deepest tournament finish",
    y = NULL,
    fill = "Standardized\nperformance",
    caption = str_wrap(
      paste(
        "Metrics are standardized across finish groups.",
        "Defensive rating, turnover percentage, and opponent effective",
        "field-goal percentage are reversed so higher values always",
        "represent stronger performance."
      ),
      width = 105
    )
  ) +
  project_theme +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.margin = margin(
      t = 20,
      r = 25,
      b = 60,
      l = 20
    )
  )

figure_03_path <- here(
  "figures",
  "03_historical_championship_profile.png"
)

ggsave(
  filename = figure_03_path,
  plot = figure_03,
  width = 12,
  height = 8.5,
  units = "in",
  dpi = 300,
  bg = white
)

message(
  "Created Figure 03: ",
  normalizePath(
    figure_03_path,
    winslash = "/",
    mustWork = FALSE
  )
)

print(figure_03)

#FIGURE 04: What Did the Model Learn?
logistic_coefficients <- read_csv(
  here("results", "05_logistic_coefficients.csv"),
  show_col_types = FALSE
)

coefficient_labels <- c(
  team1_drb = "Defensive rebounds",
  team1_orb = "Offensive rebounds",
  team1_stl = "Steals",
  team1_blk = "Blocks",
  team1_tov = "Team turnovers",
  team2_tov = "Opponent turnovers",
  team2_drb = "Opponent defensive rebounds",
  team1_opponent_fg_pct = "Opponent FG% allowed",
  team1_opponent_fg3_pct = "Opponent 3P% allowed",
  team1_points_allowed = "Points allowed",
  team1_fg_pct = "Team FG%",
  team1_ast = "Assists"
)

figure_04_data <- logistic_coefficients |>
  filter(term != "(Intercept)") |>
  mutate(
    # Percentage coefficients are shown per one-percentage-point increase.
    scale_factor = if_else(
      str_detect(term, "_pct$"),
      0.01,
      1
    ),
    effect = estimate * scale_factor,
    lower = (estimate - 1.96 * std.error) * scale_factor,
    upper = (estimate + 1.96 * std.error) * scale_factor,
    feature = coefficient_labels[term],
    direction = if_else(effect > 0, "Positive Association", "Negative Association"),
    feature = reorder(feature, effect)
  )

figure_04 <- ggplot(
  figure_04_data,
  aes(
    x = effect,
    y = feature,
    color = direction
  )
) +
  geom_vline(
    xintercept = 0,
    color = dark_gray,
    linetype = "dashed"
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = upper,
      yend = feature
    ),
    linewidth = 0.8
  ) +
  geom_point(
    size = 3.2
  ) +
  scale_color_manual(
    values = c(
      "Positive Association" = blue,
      "Negative Association" = red
    )
  ) +
  labs(
    title = "What Did the Defensive Logistic Regression Learn?",
    subtitle = "Estimated change in log-odds of winning, with 95% confidence intervals",
    x = "Estimated effect",
    y = NULL,
    color = NULL,
    caption = str_wrap(
      paste(
        "Shooting-percentage effects are expressed per one-percentage-point increase;",
        "counting statistics are expressed per additional recorded event.",
        "Intervals crossing zero indicate greater uncertainty."
      ),
      width = 115
    )
  ) +
  project_theme +
  theme(
    panel.grid.major.x = element_line(
      color = light_gray,
      linewidth = 0.4
    ),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  here("figures", "04_defensive_logistic_coefficients.png"),
  figure_04,
  width = 11,
  height = 8,
  dpi = 300,
  bg = white
)

print(figure_04)

#FIGURE 05: Model Comparison

model_comparison <- read_csv(
  here("results", "05_model_comparison.csv"),
  show_col_types = FALSE
) |>
  mutate(
    Selected = AIC == min(AIC),
    Model = factor(
      Model,
      levels = c("Full", "Reduced 1", "Reduced 2")
    )
  ) |>
  pivot_longer(
    c(AIC, BIC),
    names_to = "criterion",
    values_to = "value"
  )

figure_05 <- ggplot(
  model_comparison,
  aes(
    x = Model,
    y = value,
    fill = criterion
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.68
  ) +
  geom_text(
    aes(
      label = number(value, accuracy = 0.1)
    ),
    position = position_dodge(width = 0.75),
    vjust = -0.45,
    size = 3.7,
    fontface = "bold",
    color = navy
  ) +
  annotate(
    "text",
    x = 1,
    y = max(model_comparison$value) + 11,
    label = "Selected by minimum AIC",
    color = red,
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      AIC = blue,
      BIC = light_blue
    )
  ) +
  scale_y_continuous(
    limits = c(
      0,
      max(model_comparison$value) + 20
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Comparing the Candidate Logistic Regression Models",
    subtitle = "Lower AIC and BIC values indicate a better balance of fit and complexity",
    x = NULL,
    y = "Information criterion",
    fill = NULL,
    caption = paste(
      "The full 12-predictor model produced the lowest AIC and was retained",
      "as the final Defensive Logistic Regression model."
    )
  ) +
  project_theme +
  theme(
    axis.text.x = element_text(angle = 0),
    legend.position = "bottom"
  )

ggsave(
  here("figures", "05_logistic_model_comparison.png"),
  figure_05,
  width = 10,
  height = 7.5,
  dpi = 300,
  bg = white
)

print(figure_05)

#FIGURE 06: Defensive Metric Rankings

defensive_rankings <- read_csv(
  here("results", "2023_defensive_rankings.csv"),
  show_col_types = FALSE
)

actual_2023_finishes <- historical_summary |>
  filter(season == 2023) |>
  transmute(
    team = if_else(
      team == "Connecticut",
      "UConn",
      team
    ),
    tournament_finish
  )

top_20_defensive <- defensive_rankings |>
  slice_head(n = 20) |>
  left_join(
    actual_2023_finishes,
    by = "team"
  ) |>
  mutate(
    tournament_finish = replace_na(
      tournament_finish,
      "Did not reach Sweet 16"
    ),
    tournament_finish = factor(
      tournament_finish,
      levels = c(
        "Did not reach Sweet 16",
        "Sweet 16",
        "Elite Eight",
        "Final Four",
        "Runner-up",
        "Champion"
      )
    ),
    team = reorder(
      team,
      defensive_metric
    )
  )

finish_colors <- c(
  "Did not reach Sweet 16" = light_gray,
  "Sweet 16" = light_blue,
  "Elite Eight" = blue,
  "Final Four" = navy,
  "Runner-up" = dark_gray,
  "Champion" = red
)

figure_06 <- ggplot(
  top_20_defensive,
  aes(
    x = defensive_metric,
    y = team,
    fill = tournament_finish
  )
) +
  geom_col(
    width = 0.72
  ) +
  geom_text(
    aes(
      label = paste0("#", defensive_rank)
    ),
    hjust = -0.25,
    size = 3.5,
    fontface = "bold",
    color = navy
  ) +
  scale_fill_manual(
    values = finish_colors,
    drop = FALSE
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  labs(
    title = "The Defensive Metric's Top 20 Teams for 2023",
    subtitle = "Bar color identifies each team's actual deepest NCAA Tournament finish",
    x = "Defensive metric",
    y = NULL,
    fill = "Actual 2023 finish",
    caption = str_wrap(
      paste(
        "The metric ranked national champion UConn 11th and Final Four team",
        "Florida Atlantic 6th. Runner-up San Diego State ranked 30th, while",
        "Final Four team Miami ranked 44th and therefore does not appear",
        "among the top 20."
      ),
      width = 115
    )
  ) +
  project_theme +
  theme(
    panel.grid.major.x = element_line(
      color = light_gray,
      linewidth = 0.4
    ),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  here("figures", "06_2023_defensive_metric_rankings.png"),
  figure_06,
  width = 11,
  height = 9,
  dpi = 300,
  bg = white
)

print(figure_06)

#FIGURE 07: How Was the Championship Score Model Built?

championship_tuning <- read_csv(
  here("results", "championship_score_tuning_2024.csv"),
  show_col_types = FALSE
)

selected_model <- championship_tuning |>
  slice_min(
    cross_validated_rmse,
    n = 1
  )

figure_07 <- ggplot(
  championship_tuning,
  aes(
    x = alpha,
    y = cross_validated_rmse
  )
) +
  geom_line(
    color = blue,
    linewidth = 1
  ) +
  geom_point(
    color = blue,
    size = 3
  ) +
  geom_point(
    data = selected_model,
    color = red,
    size = 5
  ) +
  geom_label(
    data = selected_model,
    aes(
      label = paste0(
        "Selected model\nα = ",
        alpha,
        "\nλ = ",
        number(lambda_min, accuracy = 0.001),
        "\nRMSE = ",
        number(cross_validated_rmse, accuracy = 0.001)
      )
    ),
    nudge_x = -0.12,
    nudge_y = 0.004,
    hjust = 1,
    color = navy,
    fill = white,
    label.size = 0.3
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.1)
  ) +
  labs(
    title = "Selecting the Championship Score Model",
    subtitle = paste(
      "Season-grouped cross-validation compared Elastic Net models;",
      "lower RMSE indicates better out-of-sample performance"
    ),
    x = "Elastic Net alpha",
    y = "Cross-validated RMSE",
    caption = str_wrap(
      paste(
        "Alpha controls the balance between Ridge and LASSO regularization.",
        "The minimum error occurred at alpha = 1, selecting a pure LASSO model."
      ),
      width = 115
    )
  ) +
  project_theme +
  theme(
    axis.text.x = element_text(angle = 0),
    panel.grid.major.x = element_line(
      color = light_gray,
      linewidth = 0.4
    )
  )

ggsave(
  here("figures", "07_championship_score_model_selection.png"),
  figure_07,
  width = 11,
  height = 7.5,
  dpi = 300,
  bg = white
)

print(figure_07)

#FIGURE 08: What Did the Model Learn?

championship_coefficients <- read_csv(
  here("results", "championship_score_coefficients_2024.csv"),
  show_col_types = FALSE
)

championship_training <- read_csv(
  here(
    "data",
    "final",
    "championship_score_training_predictions_2024.csv"
  ),
  show_col_types = FALSE
)

retained_features <- championship_coefficients |>
  filter(
    feature != "(Intercept)",
    coefficient != 0
  )

excluded_features <- championship_coefficients |>
  filter(
    feature != "(Intercept)",
    coefficient == 0
  )

training_rmse <- sqrt(
  mean(
    (
      championship_training$predicted_finish_score -
        championship_training$finish_weight
    )^2,
    na.rm = TRUE
  )
)

training_mae <- mean(
  abs(
    championship_training$predicted_finish_score -
      championship_training$finish_weight
  ),
  na.rm = TRUE
)

summary_metrics <- tibble(
  metric = c(
    "Candidate features",
    "Features retained",
    "Features excluded",
    "Cross-validated RMSE"
  ),
  value = c(
    nrow(retained_features) + nrow(excluded_features),
    nrow(retained_features),
    nrow(excluded_features),
    selected_model$cross_validated_rmse[[1]]
  ),
  display_value = c(
    as.character(
      nrow(retained_features) + nrow(excluded_features)
    ),
    as.character(
      nrow(retained_features)
    ),
    as.character(
      nrow(excluded_features)
    ),
    number(
      selected_model$cross_validated_rmse[[1]],
      accuracy = 0.001
    )
  )
) |>
  mutate(
    metric = factor(
      metric,
      levels = rev(metric)
    ),
    status = if_else(
      metric == "Features retained",
      "Selected",
      "Summary"
    )
  )

retained_labels <- c(
  avg_point_margin = "Point margin",
  avg_drb_pct = "Defensive rebounding %"
)

retained_feature_text <- retained_features |>
  mutate(
    feature_label = retained_labels[feature],
    sign = if_else(
      coefficient > 0,
      "positive coefficient",
      "negative coefficient"
    ),
    feature_summary = paste0(
      feature_label,
      " (",
      sign,
      ")"
    )
  ) |>
  pull(feature_summary) |>
  paste(
    collapse = "\n"
  )

figure_08 <- ggplot(
  summary_metrics,
  aes(
    x = value,
    y = metric,
    fill = status
  )
) +
  geom_col(
    width = 0.62
  ) +
  geom_text(
    aes(
      label = display_value
    ),
    hjust = -0.25,
    size = 4.2,
    fontface = "bold",
    color = navy
  ) +
  annotate(
    "label",
    x = 7,
    y = 1.55,
    label = paste0(
      "Retained predictors\n",
      retained_feature_text
    ),
    hjust = 0,
    vjust = 0.5,
    size = 4,
    color = navy,
    fill = white,
    linewidth = 0.4,
    label.padding = unit(
      0.35,
      "lines"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Selected" = red,
      "Summary" = blue
    )
  ) +
  scale_x_continuous(
    limits = c(0, 17),
    breaks = seq(0, 16, 2),
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  labs(
    title = "How the Championship Score Reduced the Feature Set",
    subtitle = paste(
      "Elastic Net evaluated 14 correlated measures and selected",
      "the most compact model with the lowest season-grouped cross-validation error"
    ),
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = str_wrap(
      paste(
        "The Championship Score was designed primarily to rank tournament",
        "contenders rather than explain every variation in final tournament finish.",
        "LASSO regularization removed redundant predictors, while season-grouped",
        "cross-validation limited information leakage between tournament years.",
        "The retained model was then tested prospectively on the 2024 tournament field."
      ),
      width = 115
    )
  ) +
  project_theme +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(
      t = 20,
      r = 25,
      b = 60,
      l = 20
    )
  )

ggsave(
  here(
    "figures",
    "08_championship_score_model_summary.png"
  ),
  figure_08,
  width = 11,
  height = 7.8,
  dpi = 300,
  bg = white
)

print(figure_08)

#FIGURE 09: Top Championship Scores
championship_scores <- read_csv(
  here("data", "final", "2024_championship_scores.csv"),
  show_col_types = FALSE
)

actual_2024_finishes <- historical_summary |>
  filter(
    season == 2024
  ) |>
  transmute(
    team = case_when(
      team == "Connecticut" ~ "UConn",
      team == "North Carolina" ~ "UNC",
      TRUE ~ team
    ),
    tournament_finish
  )

top_20_championship <- championship_scores |>
  slice_min(
    championship_rank,
    n = 20
  ) |>
  left_join(
    actual_2024_finishes,
    by = "team"
  ) |>
  mutate(
    tournament_finish = replace_na(
      tournament_finish,
      "Did not reach Sweet 16"
    ),
    tournament_finish = factor(
      tournament_finish,
      levels = c(
        "Did not reach Sweet 16",
        "Sweet 16",
        "Elite Eight",
        "Final Four",
        "Runner-up",
        "Champion"
      )
    ),
    team = reorder(
      team,
      championship_score
    )
  )

championship_finish_colors <- c(
  "Did not reach Sweet 16" = light_gray,
  "Sweet 16" = light_blue,
  "Elite Eight" = blue,
  "Final Four" = navy,
  "Runner-up" = dark_gray,
  "Champion" = red
)

figure_09 <- ggplot(
  top_20_championship,
  aes(
    x = championship_score,
    y = team,
    fill = tournament_finish
  )
) +
  geom_col(
    width = 0.72
  ) +
  geom_text(
    aes(
      label = paste0("#", championship_rank)
    ),
    hjust = -0.25,
    size = 3.5,
    fontface = "bold",
    color = navy
  ) +
  scale_fill_manual(
    values = championship_finish_colors,
    drop = FALSE
  ) +
  scale_x_continuous(
    limits = c(0, 108),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "The Championship Score's Top 20 Teams for 2024",
    subtitle = paste(
      "Pre-tournament rankings after the Strength of Schedule adjustment;",
      "bar color shows actual tournament finish"
    ),
    x = "Championship Score",
    y = NULL,
    fill = "Actual 2024 finish",
    caption = str_wrap(
      paste(
        "The model ranked national champion UConn third, runner-up Purdue fourth,",
        "and Final Four team Alabama eighth. Final Four team NC State ranked",
        "outside the top 20. Championship Scores are relative rankings across",
        "the 68-team field, not direct championship probabilities."
      ),
      width = 115
    )
  ) +
  project_theme +
  theme(
    panel.grid.major.x = element_line(
      color = light_gray,
      linewidth = 0.4
    ),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  here("figures", "09_2024_championship_score_rankings.png"),
  figure_09,
  width = 11,
  height = 9,
  dpi = 300,
  bg = white
)

print(figure_09)

#FIGURE 10: How the Upset Metric Works

upset_steps <- tibble(
  x = c(1, 2.5, 4, 5.5, 7),
  y = 1,
  label = c(
    "Championship\nScores",
    "Identify favorite\nand underdog",
    "Evaluate matchup\nadvantages",
    "Subtract favorite\nquality penalty",
    "Advance predicted\nwinner"
  )
)

figure_10 <- ggplot() +
  geom_segment(
    aes(
      x = upset_steps$x[-5] + 0.45,
      xend = upset_steps$x[-1] - 0.45,
      y = 1,
      yend = 1
    ),
    arrow = arrow(length = unit(0.18, "inches")),
    color = dark_gray,
    linewidth = 0.8
  ) +
  geom_label(
    data = upset_steps,
    aes(x, y, label = label),
    size = 4,
    fontface = "bold",
    color = navy,
    fill = white,
    linewidth = 0.6,
    label.padding = unit(0.45, "lines")
  ) +
  annotate(
    "label",
    x = 4,
    y = 0.15,
    label = paste(
      "Three-point edge: 30%",
      "Turnover pressure: 25%",
      "Rebounding edge: 25%",
      "Defensive resistance: 20%",
      sep = "\n"
    ),
    size = 4,
    color = navy,
    fill = light_gray,
    linewidth = 0.4
  ) +
  annotate(
    "text",
    x = 7,
    y = 0.15,
    label = "Underdog advances only when\nfinal upset score exceeds threshold",
    color = red,
    fontface = "bold",
    size = 4
  ) +
  coord_cartesian(
    xlim = c(0.3, 7.7),
    ylim = c(-0.45, 1.6),
    clip = "off"
  ) +
  labs(
    title = "How the Upset Metric Evaluates Each Matchup",
    subtitle = paste(
      "The favorite advances by default unless the underdog's stylistic",
      "advantages overcome the Championship Score quality gap"
    ),
    caption = str_wrap(
      paste(
        "The process is repeated after every predicted game, allowing winners",
        "to advance into newly created matchups through the championship."
      ),
      width = 115
    )
  ) +
  project_theme +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(20, 25, 55, 20)
  )

ggsave(
  here("figures", "10_upset_metric_framework.png"),
  figure_10,
  width = 13,
  height = 6.5,
  dpi = 300,
  bg = white
)

print(figure_10)

#FIGURE 11: Bracket Comparisons

predicted_bracket <- read_csv(
  here("data", "final", "2025_predicted_bracket.csv"),
  show_col_types = FALSE
)

round_map <- tibble(
  game_round = c(
    "Round of 32",
    "Sweet 16",
    "Elite Eight",
    "Final Four",
    "Championship"
  ),
  stage = c(
    "Sweet 16",
    "Elite Eight",
    "Final Four",
    "Title Game",
    "Champion"
  ),
  stage_order = 1:5
)

predicted_advancement <- predicted_bracket |>
  inner_join(round_map, by = c("round" = "game_round")) |>
  transmute(
    source = "Model Prediction",
    stage,
    stage_order,
    team = predicted_winner
  )

actual_2025 <- historical_summary |>
  filter(season == 2025) |>
  mutate(
    team = case_when(
      team == "Brigham Young" ~ "BYU",
      team == "Mississippi" ~ "Ole Miss",
      TRUE ~ team
    )
  )

actual_advancement <- bind_rows(
  actual_2025 |>
    transmute(team, stage = "Sweet 16", stage_order = 1),
  actual_2025 |>
    filter(finish_weight >= 2) |>
    transmute(team, stage = "Elite Eight", stage_order = 2),
  actual_2025 |>
    filter(finish_weight >= 3) |>
    transmute(team, stage = "Final Four", stage_order = 3),
  actual_2025 |>
    filter(finish_weight >= 4) |>
    transmute(team, stage = "Title Game", stage_order = 4),
  actual_2025 |>
    filter(finish_weight == 5) |>
    transmute(team, stage = "Champion", stage_order = 5)
) |>
  mutate(source = "Actual Result")

predicted_teams <- predicted_advancement |>
  select(stage, team) |>
  mutate(predicted = TRUE)

actual_teams <- actual_advancement |>
  select(stage, team) |>
  mutate(actual = TRUE)

bracket_comparison <- bind_rows(
  predicted_advancement,
  actual_advancement
) |>
  left_join(predicted_teams, by = c("stage", "team")) |>
  left_join(actual_teams, by = c("stage", "team")) |>
  mutate(
    result = case_when(
      predicted %in% TRUE & actual %in% TRUE ~ "Correct",
      source == "Model Prediction" ~ "Predicted only",
      TRUE ~ "Actual only"
    )
  ) |>
  group_by(source, stage, stage_order) |>
  arrange(team, .by_group = TRUE) |>
  mutate(
    row = row_number(),
    total = n(),
    y = (total + 1) / 2 - row
  ) |>
  ungroup()

figure_11 <- ggplot(
  bracket_comparison,
  aes(
    x = stage_order,
    y = y,
    label = team,
    fill = result
  )
) +
  geom_label(
    color = white,
    fontface = "bold",
    size = 3.1,
    linewidth = 0.35,
    label.padding = unit(0.22, "lines")
  ) +
  facet_grid(
    source ~ .,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Correct" = navy,
      "Predicted only" = red,
      "Actual only" = dark_gray
    )
  ) +
  scale_x_continuous(
    breaks = 1:5,
    labels = c(
      "Sweet 16",
      "Elite Eight",
      "Final Four",
      "Title Game",
      "Champion"
    ),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    title = "2025 Upset Metric: Predicted Versus Actual Advancement",
    subtitle = paste(
      "Teams are shown from the Sweet Sixteen through the championship;",
      "navy identifies correctly predicted advancement"
    ),
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = str_wrap(
      paste(
        "Red teams advanced in the model's bracket but not in the actual",
        "tournament. Gray teams advanced in the actual tournament but were",
        "missed by the model at that stage."
      ),
      width = 115
    )
  ) +
  project_theme +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 13,
      color = navy
    ),
    legend.position = "bottom",
    plot.margin = margin(20, 25, 60, 20)
  )

ggsave(
  here("figures", "11_2025_predicted_vs_actual_bracket.png"),
  figure_11,
  width = 14,
  height = 10,
  dpi = 300,
  bg = white
)

print(figure_11)

#FIGURE 12: Advancement Probability by Round

monte_carlo_2026 <- read_csv(
  here("data", "final", "2026_monte_carlo_probabilities.csv"),
  show_col_types = FALSE
)

round_probabilities <- c(
  sweet_16_probability = "Sweet 16",
  elite_8_probability = "Elite Eight",
  final_four_probability = "Final Four",
  championship_game_probability = "Title Game",
  championship_probability = "Champion"
)

top_16_monte_carlo <- monte_carlo_2026 |>
  slice_max(
    championship_probability,
    n = 16,
    with_ties = FALSE
  ) |>
  arrange(championship_probability) |>
  mutate(
    team = factor(team, levels = team)
  ) |>
  pivot_longer(
    cols = all_of(names(round_probabilities)),
    names_to = "round",
    values_to = "probability"
  ) |>
  mutate(
    round = factor(
      round_probabilities[round],
      levels = unname(round_probabilities)
    )
  )

figure_12 <- ggplot(
  top_16_monte_carlo,
  aes(
    x = round,
    y = team,
    fill = probability
  )
) +
  geom_tile(
    color = white,
    linewidth = 1
  ) +
  geom_text(
    aes(label = percent(probability, accuracy = 0.1)),
    size = 3.5,
    fontface = "bold",
    color = navy
  ) +
  scale_fill_gradient(
    low = white,
    high = blue,
    labels = percent_format()
  ) +
  labs(
    title = "2026 Advancement Probability by Tournament Round",
    subtitle = paste(
      "Top 16 teams by simulated championship probability",
      "across all Monte Carlo tournament runs"
    ),
    x = NULL,
    y = NULL,
    fill = "Probability",
    caption = str_wrap(
      paste(
        "Probabilities decline across successive rounds because each team",
        "must survive every prior matchup. Championship probabilities therefore",
        "reflect both team quality and uncertainty throughout the bracket."
      ),
      width = 115
    )
  ) +
  project_theme +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.margin = margin(20, 25, 55, 20)
  )

ggsave(
  here("figures", "12_2026_advancement_probabilities.png"),
  figure_12,
  width = 12,
  height = 9,
  dpi = 300,
  bg = white
)

print(figure_12)

#FIGURE 13: Most Likely 2026 Tournament Outcome via Monte Carlo Simulation

stage_specs <- tibble(
  stage = c(
    "Sweet 16",
    "Elite Eight",
    "Final Four",
    "Title Game",
    "Champion"
  ),
  probability_column = c(
    "sweet_16_probability",
    "elite_8_probability",
    "final_four_probability",
    "championship_game_probability",
    "championship_probability"
  ),
  teams_shown = c(16, 8, 4, 2, 1),
  stage_order = 1:5
)

predicted_2026 <- bind_rows(
  lapply(seq_len(nrow(stage_specs)), function(i) {
    
    probability_column <- stage_specs$probability_column[[i]]
    
    monte_carlo_2026 |>
      slice_max(
        order_by = .data[[probability_column]],
        n = stage_specs$teams_shown[[i]],
        with_ties = FALSE
      ) |>
      transmute(
        source = "Monte Carlo Prediction",
        stage = stage_specs$stage[[i]],
        stage_order = stage_specs$stage_order[[i]],
        team
      )
  })
) |>
  mutate(
    team = case_when(
      team == "Connecticut" ~ "UConn",
      TRUE ~ team
    )
)

actual_2026 <- tribble(
  ~stage, ~stage_order, ~team,
  
  "Sweet 16", 1, "Duke",
  "Sweet 16", 1, "St. John's (NY)",
  "Sweet 16", 1, "Michigan State",
  "Sweet 16", 1, "UConn",
  "Sweet 16", 1, "Arizona",
  "Sweet 16", 1, "Arkansas",
  "Sweet 16", 1, "Texas",
  "Sweet 16", 1, "Purdue",
  "Sweet 16", 1, "Iowa",
  "Sweet 16", 1, "Nebraska",
  "Sweet 16", 1, "Illinois",
  "Sweet 16", 1, "Houston",
  "Sweet 16", 1, "Michigan",
  "Sweet 16", 1, "Alabama",
  "Sweet 16", 1, "Tennessee",
  "Sweet 16", 1, "Iowa State",
  
  "Elite Eight", 2, "Duke",
  "Elite Eight", 2, "UConn",
  "Elite Eight", 2, "Arizona",
  "Elite Eight", 2, "Purdue",
  "Elite Eight", 2, "Iowa",
  "Elite Eight", 2, "Illinois",
  "Elite Eight", 2, "Michigan",
  "Elite Eight", 2, "Tennessee",
  
  "Final Four", 3, "UConn",
  "Final Four", 3, "Arizona",
  "Final Four", 3, "Illinois",
  "Final Four", 3, "Michigan",
  
  "Title Game", 4, "UConn",
  "Title Game", 4, "Michigan",
  
  "Champion", 5, "Michigan"
) |>
  mutate(
    source = "Actual Result"
  )

predicted_keys <- predicted_2026 |>
  transmute(stage, team, predicted = TRUE)

actual_keys <- actual_2026 |>
  transmute(stage, team, actual = TRUE)

figure_13_data <- bind_rows(
  predicted_2026,
  actual_2026
) |>
  left_join(
    predicted_keys,
    by = c("stage", "team")
  ) |>
  left_join(
    actual_keys,
    by = c("stage", "team")
  ) |>
  mutate(
    result = case_when(
      predicted %in% TRUE & actual %in% TRUE ~ "Correct",
      source == "Monte Carlo Prediction" ~ "Predicted only",
      TRUE ~ "Actual only"
    )
  ) |>
  group_by(source, stage, stage_order) |>
  arrange(team, .by_group = TRUE) |>
  mutate(
    row = row_number(),
    total = n(),
    y = (total + 1) / 2 - row
  ) |>
  ungroup()

figure_13 <- ggplot(
  figure_13_data,
  aes(
    x = stage_order,
    y = y,
    label = team,
    fill = result
  )
) +
  geom_label(
    color = white,
    fontface = "bold",
    size = 3.1,
    linewidth = 0.35,
    label.padding = unit(0.22, "lines")
  ) +
  facet_grid(
    source ~ .,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Correct" = navy,
      "Predicted only" = red,
      "Actual only" = dark_gray
    )
  ) +
  scale_x_continuous(
    breaks = 1:5,
    labels = stage_specs$stage,
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    title = "2026 Monte Carlo Prediction Versus Actual Advancement",
    subtitle = paste(
      "The model's highest-probability teams are compared with",
      "the actual tournament field at each stage"
    ),
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = str_wrap(
      paste(
        "Navy teams were correctly identified at that stage.",
        "Red teams were projected by the simulation but did not advance,",
        "while gray teams advanced in the actual tournament but were missed.",
        "Predicted fields are based on marginal advancement probabilities",
        "across all Monte Carlo simulations."
      ),
      width = 115
    )
  ) +
  project_theme +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 13,
      color = navy
    ),
    legend.position = "bottom",
    plot.margin = margin(20, 25, 60, 20)
  )

ggsave(
  here(
    "figures",
    "13_2026_monte_carlo_predicted_vs_actual.png"
  ),
  figure_13,
  width = 14,
  height = 10,
  dpi = 300,
  bg = white
)

print(figure_13)
