library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(here)
library(glmnet)

#Input and output filepaths

historical_statistics_path <- here(
  "data",
  "processed",
  "historical_tournament_statistics_2025.csv"
)

historical_summary_path <- here(
  "data",
  "processed",
  "historical_tournament_summary_2025.csv"
)

tournament_teams_2025_path <- here(
  "data",
  "processed",
  "2025_tournament_teams.csv"
)

model_path <- here(
  "results",
  "championship_score_model_2025.rds"
)

coefficient_path <- here(
  "results",
  "championship_score_coefficients_2025.csv"
)

tuning_path <- here(
  "results",
  "championship_score_tuning_2025.csv"
)

training_predictions_path <- here(
  "data",
  "final",
  "championship_score_training_predictions_2025.csv"
)

championship_scores_path <- here(
  "data",
  "final",
  "2025_championship_scores.csv"
)

strength_of_schedule_path <- here(
  "data",
  "raw",
  "2025_strength_of_schedule.csv"
)

scored_features_path <- here(
  "data",
  "final",
  "2025_championship_score_features.csv"
)

dir.create(
  here("results"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("data", "final"),
  recursive = TRUE,
  showWarnings = FALSE
)

#Confirm input files exist

required_files <- c(
  historical_statistics_path,
  historical_summary_path,
  tournament_teams_2025_path
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "The following required files were not found:\n",
    paste(missing_files, collapse = "\n")
  )
}

#Read datasets

historical_tournament_statistics_2025 <- read_csv(
  historical_statistics_path,
  show_col_types = FALSE
)

historical_tournament_summary_2025 <- read_csv(
  historical_summary_path,
  show_col_types = FALSE
)

tournament_teams_2025 <- read_csv(
  tournament_teams_2025_path,
  show_col_types = FALSE
)

if (!file.exists(strength_of_schedule_path)) {
  stop(
    "The 2025 strength-of-schedule file was not found at: ",
    strength_of_schedule_path
  )
}

strength_of_schedule_2025 <- read_csv(
  strength_of_schedule_path,
  show_col_types = FALSE
) |>
  transmute(
    sos_team = Team,
    strength_of_schedule = SOS
  )


# Standardize common naming differences
team_name_lookup <- tibble(
  team = c(
    "College of Charleston",
    "Florida Atlantic",
    "Grambling",
    "NC State",
    "Saint Mary's",
    "UConn",
    "UNC"
  ),
  
  sos_team = c(
    "Charleston",
    "Florida Atlantic",
    "Grambling State",
    "North Carolina State",
    "Saint Mary's (CA)",
    "Connecticut",
    "North Carolina"
  )
)


# Apply aliases only where needed
tournament_teams_2025 <- tournament_teams_2025 |>
  left_join(
    team_name_lookup,
    by = "team"
  ) |>
  mutate(
    sos_match_name = coalesce(
      sos_team,
      team
    )
  ) |>
  select(
    -sos_team
  ) |>
  left_join(
    strength_of_schedule_2025,
    by = c(
      "sos_match_name" = "sos_team"
    )
  )


# Validate all 68 teams matched before continuing
missing_sos <- tournament_teams_2025 |>
  filter(
    is.na(strength_of_schedule)
  ) |>
  select(
    team,
    sos_match_name
  )

if (nrow(missing_sos) > 0) {
  
  print(missing_sos)
  
  stop(
    nrow(missing_sos),
    " tournament teams are still missing Strength of Schedule values. ",
    "The Championship Score model was not run."
  )
}

message(
  "Successfully matched Strength of Schedule for all 68 teams."
)

#Validate historical seasons

expected_historical_seasons <- c(
  2012,
  2013,
  2014,
  2015,
  2016,
  2017,
  2018,
  2019,
  2021,
  2022,
  2023,
  2024
)

statistics_seasons <- historical_tournament_statistics_2025 |>
  distinct(season) |>
  arrange(season) |>
  pull(season)

summary_seasons <- historical_tournament_summary_2025 |>
  distinct(season) |>
  arrange(season) |>
  pull(season)

if (!identical(
  statistics_seasons,
  expected_historical_seasons
)) {
  stop(
    "The historical game-level dataset does not contain ",
    "the expected 2012–2023 seasons."
  )
}

if (!identical(
  summary_seasons,
  expected_historical_seasons
)) {
  stop(
    "The historical summary does not contain ",
    "the expected 2012–2023 seasons."
  )
}

if (
  anyDuplicated(
    historical_tournament_summary_2025 |>
    select(
      season,
      team
    )
  ) > 0
) {
  stop(
    "Duplicate team-season rows were found in the historical summary."
  )
}

#Validate tournament finish weights

expected_finish_weights <- tibble(
  tournament_finish = c(
    "Sweet 16",
    "Elite Eight",
    "Final Four",
    "Runner-up",
    "Champion"
  ),
  
  finish_weight = c(
    1,
    2,
    3,
    4,
    5
  )
)

actual_finish_weights <- historical_tournament_summary_2025 |>
  distinct(
    tournament_finish,
    finish_weight
  )

finish_weight_validation <- expected_finish_weights |>
  left_join(
    actual_finish_weights,
    by = c(
      "tournament_finish",
      "finish_weight"
    )
  )

if (
  nrow(actual_finish_weights) !=
  nrow(expected_finish_weights) ||
  !all(
    expected_finish_weights$tournament_finish %in%
    actual_finish_weights$tournament_finish
  ) ||
  !all(
    expected_finish_weights$finish_weight %in%
    actual_finish_weights$finish_weight
  )
) {
  print(actual_finish_weights)
  
  stop(
    "The historical tournament finish weights do not match ",
    "the expected 1–5 methodology."
  )
}

#Select model features

model_features <- c(
  "avg_point_margin",
  "avg_estimated_possessions",
  "avg_offensive_rating",
  "avg_defensive_rating",
  "avg_efg_pct",
  "avg_true_shooting_pct",
  "avg_ft_rate",
  "avg_fg3_attempt_rate",
  "avg_tov_pct",
  "avg_forced_tov_pct",
  "avg_orb_pct",
  "avg_drb_pct",
  "avg_opponent_efg_pct",
  "avg_opponent_ft_rate"
)

missing_historical_features <- setdiff(
  model_features,
  names(historical_tournament_summary_2025)
)

if (length(missing_historical_features) > 0) {
  stop(
    "The historical summary is missing these model features: ",
    paste(
      missing_historical_features,
      collapse = ", "
    )
  )
}

#Create historical training data

training_data <- historical_tournament_summary_2025 |>
  select(
    season,
    team,
    tournament_finish,
    finish_weight,
    all_of(model_features)
  ) |>
  arrange(
    season,
    team
  )

if (nrow(training_data) != 192) {
  warning(
    "Expected 176 historical team-seasons, but found ",
    nrow(training_data),
    "."
  )
}

#Learn historical median-imputation values

feature_medians <- training_data |>
  summarise(
    across(
      all_of(model_features),
      ~ median(
        .x,
        na.rm = TRUE
      )
    )
  ) |>
  unlist(
    use.names = TRUE
  )

if (any(!is.finite(feature_medians))) {
  stop(
    "At least one model feature does not have a valid median."
  )
}

#Impute missing hstorical values

training_data_imputed <- training_data

for (feature in model_features) {
  
  missing_rows <- is.na(
    training_data_imputed[[feature]]
  )
  
  training_data_imputed[[feature]][missing_rows] <-
    feature_medians[[feature]]
}

if (
  any(
    is.na(
      training_data_imputed |>
      select(
        all_of(model_features)
      )
    )
  )
) {
  stop(
    "Missing historical values remain after median imputation."
  )
}

#Create model matrices

x_training <- training_data_imputed |>
  select(
    all_of(model_features)
  ) |>
  as.matrix()

y_training <- training_data_imputed$finish_weight

if (any(!is.finite(x_training))) {
  stop(
    "The historical predictor matrix contains invalid values."
  )
}

if (any(!y_training %in% 1:5)) {
  stop(
    "Historical finish weights must range from 1 through 5."
  )
}

#Create season-groupued cross-validations

foldid <- as.integer(
  factor(
    training_data_imputed$season,
    levels = expected_historical_seasons
  )
)

number_of_folds <- length(
  unique(foldid)
)

if (number_of_folds != 12) {
  stop(
    "Expected 11 season-based cross-validation folds, but created ",
    number_of_folds,
    "."
  )
}

#Tune elastic-net alpha param

set.seed(2025)

alpha_grid <- seq(
  0,
  1,
  by = 0.1
)

cv_models <- vector(
  mode = "list",
  length = length(alpha_grid)
)

tuning_results <- vector(
  mode = "list",
  length = length(alpha_grid)
)

for (i in seq_along(alpha_grid)) {
  
  current_alpha <- alpha_grid[[i]]
  
  current_cv_model <- cv.glmnet(
    x = x_training,
    y = y_training,
    family = "gaussian",
    alpha = current_alpha,
    foldid = foldid,
    standardize = TRUE,
    intercept = TRUE,
    type.measure = "mse"
  )
  
  cv_models[[i]] <- current_cv_model
  
  minimum_index <- which.min(
    current_cv_model$cvm
  )
  
  tuning_results[[i]] <- tibble(
    alpha = current_alpha,
    
    lambda_min =
      current_cv_model$lambda.min,
    
    lambda_1se =
      current_cv_model$lambda.1se,
    
    cross_validated_mse =
      current_cv_model$cvm[[minimum_index]],
    
    cross_validated_rmse =
      sqrt(
        current_cv_model$cvm[[minimum_index]]
      )
  )
}

tuning_results <- bind_rows(
  tuning_results
) |>
  arrange(
    cross_validated_mse,
    alpha
  )

best_alpha <- tuning_results$alpha[[1]]

best_cv_model <- cv_models[[
  which(alpha_grid == best_alpha)
]]

best_lambda <- best_cv_model$lambda.min

message(
  "Selected alpha: ",
  best_alpha
)

message(
  "Selected lambda: ",
  signif(
    best_lambda,
    6
  )
)

message(
  "Season-grouped cross-validated RMSE: ",
  round(
    tuning_results$cross_validated_rmse[[1]],
    3
  )
)

#Fit championship score model

championship_score_model <- glmnet(
  x = x_training,
  y = y_training,
  family = "gaussian",
  alpha = best_alpha,
  lambda = best_lambda,
  standardize = TRUE,
  intercept = TRUE
)

#extract model coeffs

coefficient_matrix <- as.matrix(
  coef(
    championship_score_model,
    s = best_lambda
  )
)

championship_score_coefficients <- tibble(
  feature = rownames(
    coefficient_matrix
  ),
  
  coefficient = as.numeric(
    coefficient_matrix[, 1]
  )
) |>
  mutate(
    absolute_coefficient =
      abs(coefficient),
    
    direction = case_when(
      coefficient > 0 ~ "Positive",
      coefficient < 0 ~ "Negative",
      TRUE ~ "Excluded"
    )
  ) |>
  arrange(
    desc(absolute_coefficient)
  )

print(
  championship_score_coefficients
)

#Calculate historical fitted scores

historical_predicted_scores <- as.numeric(
  predict(
    championship_score_model,
    newx = x_training,
    s = best_lambda
  )
)

training_predictions <- training_data_imputed |>
  select(
    season,
    team,
    tournament_finish,
    finish_weight
  ) |>
  mutate(
    predicted_finish_score =
      historical_predicted_scores,
    
    prediction_error =
      predicted_finish_score -
      finish_weight
  )

#Calculate training diagnostics
training_rmse <- sqrt(
  mean(
    training_predictions$prediction_error^2
  )
)

training_mae <- mean(
  abs(
    training_predictions$prediction_error
  )
)

training_r_squared <- cor(
  training_predictions$finish_weight,
  training_predictions$predicted_finish_score
)^2

message(
  "Training RMSE: ",
  round(
    training_rmse,
    3
  )
)

message(
  "Training MAE: ",
  round(
    training_mae,
    3
  )
)

message(
  "Training R-squared: ",
  round(
    training_r_squared,
    3
  )
)

#Validate 2025 tournament team data

if (nrow(tournament_teams_2025) != 68) {
  stop(
    "Expected 68 teams in 2025_tournament_teams.csv, but found ",
    nrow(tournament_teams_2025),
    "."
  )
}

if (!"team" %in% names(tournament_teams_2025)) {
  stop(
    "2025_tournament_teams.csv must contain a column named 'team'."
  )
}

if (
  anyDuplicated(
    tournament_teams_2025$team
  ) > 0
) {
  stop(
    "Duplicate team names were found in the 2025 dataset."
  )
}

#Confirm raw 2025 columns exist

required_2025_columns <- c(
  "team",
  "games",
  "team_game_score",
  "opp_team_game_score",
  "fg",
  "fga",
  "fg3",
  "fg3a",
  "fta",
  "orb",
  "drb",
  "tov",
  "opp_fg",
  "opp_fga",
  "opp_fg3",
  "opp_fta",
  "opp_orb",
  "opp_drb",
  "opp_tov"
)

missing_2025_columns <- setdiff(
  required_2025_columns,
  names(tournament_teams_2025)
)

if (length(missing_2025_columns) > 0) {
  stop(
    "The 2025 dataset is missing these required columns: ",
    paste(
      missing_2025_columns,
      collapse = ", "
    )
  )
}

#Safely divide numeric values

safe_divide <- function(
    numerator,
    denominator
) {
  
  ifelse(
    is.na(denominator) |
      denominator == 0,
    NA_real_,
    numerator / denominator
  )
}

#Confirm 2025 countnts statistics are per-game averages

statistics_are_totals <- median(
  tournament_teams_2025$fga,
  na.rm = TRUE
) > 150

if (statistics_are_totals) {
  message(
    "2025 counting statistics interpreted as season totals."
  )
} else {
  message(
    "2025 counting statistics interpreted as per-game averages."
  )
}

#Calculate matchin 2025 model feautres

tournament_teams_2025_model <- tournament_teams_2025 |>
  mutate(
    season = 2025,
    
    avg_point_margin = if (statistics_are_totals) {
      safe_divide(
        team_game_score -
          opp_team_game_score,
        games
      )
    } else {
      team_game_score -
        opp_team_game_score
    },
    
    team_raw_possessions =
      fga -
      orb +
      tov +
      (0.475 * fta),
    
    opponent_raw_possessions =
      opp_fga -
      opp_orb +
      opp_tov +
      (0.475 * opp_fta),
    
    combined_estimated_possessions =
      (
        team_raw_possessions +
          opponent_raw_possessions
      ) / 2,
    
    avg_estimated_possessions = if (statistics_are_totals) {
      safe_divide(
        combined_estimated_possessions,
        games
      )
    } else {
      combined_estimated_possessions
    },
    
    avg_offensive_rating =
      100 * safe_divide(
        team_game_score,
        combined_estimated_possessions
      ),
    
    avg_defensive_rating =
      100 * safe_divide(
        opp_team_game_score,
        combined_estimated_possessions
      ),
    
    avg_efg_pct =
      safe_divide(
        fg +
          (0.5 * fg3),
        fga
      ),
    
    avg_true_shooting_pct =
      safe_divide(
        team_game_score,
        2 * (
          fga +
            (0.475 * fta)
        )
      ),
    
    avg_ft_rate =
      safe_divide(
        fta,
        fga
      ),
    
    avg_fg3_attempt_rate =
      safe_divide(
        fg3a,
        fga
      ),
    
    avg_tov_pct =
      safe_divide(
        tov,
        fga +
          (0.475 * fta) +
          tov
      ),
    
    avg_forced_tov_pct =
      safe_divide(
        opp_tov,
        opp_fga +
          (0.475 * opp_fta) +
          opp_tov
      ),
    
    avg_orb_pct =
      safe_divide(
        orb,
        orb +
          opp_drb
      ),
    
    avg_drb_pct =
      safe_divide(
        drb,
        drb +
          opp_orb
      ),
    
    avg_opponent_efg_pct =
      safe_divide(
        opp_fg +
          (0.5 * opp_fg3),
        opp_fga
      ),
    
    avg_opponent_ft_rate =
      safe_divide(
        opp_fta,
        opp_fga
      )
  ) |>
  select(
    season,
    team,
    all_of(model_features)
  )

#Validate features

missing_calculated_features <- setdiff(
  model_features,
  names(tournament_teams_2025_model)
)

if (length(missing_calculated_features) > 0) {
  stop(
    "The calculated 2025 model data is missing these features: ",
    paste(
      missing_calculated_features,
      collapse = ", "
    )
  )
}

non_numeric_features <- model_features[
  !vapply(
    tournament_teams_2025_model[model_features],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_features) > 0) {
  stop(
    "These calculated 2025 features are not numeric: ",
    paste(
      non_numeric_features,
      collapse = ", "
    )
  )
}

#Impute missing values

missing_values_2025 <- tournament_teams_2025_model |>
  summarise(
    across(
      all_of(model_features),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "feature",
    values_to = "missing_values"
  ) |>
  filter(
    missing_values > 0
  )

if (nrow(missing_values_2025) > 0) {
  message(
    "Missing 2025 feature values will be replaced ",
    "using historical medians:"
  )
  
  print(
    missing_values_2025
  )
}

for (feature in model_features) {
  
  missing_rows <- is.na(
    tournament_teams_2025_model[[feature]]
  )
  
  tournament_teams_2025_model[[feature]][missing_rows] <-
    feature_medians[[feature]]
}

if (
  any(
    is.na(
      tournament_teams_2025_model |>
      select(
        all_of(model_features)
      )
    )
  )
) {
  stop(
    "Missing 2025 model values remain after imputation."
  )
}

#Create 2025 model matrix

x_2025 <- tournament_teams_2025_model |>
  select(
    all_of(model_features)
  ) |>
  as.matrix()

if (any(!is.finite(x_2025))) {
  stop(
    "The 2025 predictor matrix contains invalid values."
  )
}

#Apply trained model

predicted_finish_scores_2025 <- as.numeric(
  predict(
    championship_score_model,
    newx = x_2025,
    s = best_lambda
  )
)

#Apply strength of schedule adjustement

# Standardize SOS within the 2025 tournament field.
tournament_teams_2025 <- tournament_teams_2025 |>
  mutate(
    sos_z_score = as.numeric(
      scale(strength_of_schedule)
    )
  )

# Positive values indicate a tougher-than-average schedule.
# Negative values indicate an easier-than-average schedule.
sos_adjustment_weight <- 0.25

adjusted_finish_scores_2025 <-
  predicted_finish_scores_2025 +
  (
    sos_adjustment_weight *
      tournament_teams_2025$sos_z_score
  )

#Convert to championship score

championship_results_2025 <- tournament_teams_2025_model |>
  select(
    season,
    team
  ) |>
  mutate(
    predicted_finish_score =
      predicted_finish_scores_2025,
    
    sos_z_score =
      tournament_teams_2025$sos_z_score,
    
    sos_adjustment =
      sos_adjustment_weight *
      tournament_teams_2025$sos_z_score,
    
    adjusted_finish_score =
      adjusted_finish_scores_2025,
    
    adjusted_finish_score_bounded =
      pmin(
        5,
        pmax(
          1,
          adjusted_finish_score
        )
      ),
    
    championship_score =
      100 * (
        min_rank(adjusted_finish_score) -
          1
      ) /
      (
        n() - 1
      ),
    
    championship_rank =
      min_rank(
        desc(adjusted_finish_score)
      )
  ) |>
  arrange(
    championship_rank,
    team
  ) |>
  select(
    championship_rank,
    season,
    team,
    championship_score,
    predicted_finish_score,
    sos_z_score,
    sos_adjustment,
    adjusted_finish_score,
    adjusted_finish_score_bounded
  )

#Add scores

tournament_teams_2025_scored <- tournament_teams_2025 |>
  left_join(
    championship_results_2025,
    by = "team"
  ) |>
  arrange(
    championship_rank,
    team
  )

#Validate final results

if (nrow(championship_results_2025) != 68) {
  stop(
    "Expected 68 scored teams, but created ",
    nrow(championship_results_2025),
    "."
  )
}

if (
  anyDuplicated(
    championship_results_2025$team
  ) > 0
) {
  stop(
    "Duplicate teams were found in the Championship Score results."
  )
}

if (
  any(
    is.na(
      championship_results_2025$championship_score
    )
  )
) {
  stop(
    "At least one 2025 team is missing a Championship Score."
  )
}

if (
  any(
    championship_results_2025$championship_score < 0 |
    championship_results_2025$championship_score > 100
  )
) {
  stop(
    "At least one Championship Score falls outside 0–100."
  )
}

if (
  any(
    is.na(
      tournament_teams_2025_scored$championship_rank
    )
  )
) {
  stop(
    "At least one team failed to join to the final results."
  )
}

#Assembhle trained model object

championship_score_model_2025 <- list(
  model_name =
    "2025 Championship Score",
  
  model_type =
    "Elastic-net Gaussian regression",
  
  training_seasons =
    expected_historical_seasons,
  
  training_rows =
    nrow(training_data_imputed),
  
  target =
    "finish_weight",
  
  feature_names =
    model_features,
  
  feature_medians =
    feature_medians,
  
  alpha_grid =
    alpha_grid,
  
  best_alpha =
    best_alpha,
  
  best_lambda =
    best_lambda,
  
  cross_validated_rmse =
    tuning_results$cross_validated_rmse[[1]],
  
  training_rmse =
    training_rmse,
  
  training_mae =
    training_mae,
  
  training_r_squared =
    training_r_squared,
  
  model =
    championship_score_model
)

saveRDS(
  championship_score_model_2025,
  model_path
)

#Save outputs

write_csv(
  championship_score_coefficients,
  coefficient_path,
  na = ""
)

write_csv(
  tuning_results,
  tuning_path,
  na = ""
)

write_csv(
  training_predictions,
  training_predictions_path,
  na = ""
)

write_csv(
  championship_results_2025,
  championship_scores_path,
  na = ""
)

write_csv(
  tournament_teams_2025_scored,
  scored_features_path,
  na = ""
)

#Display final results

message(
  "Championship Score model complete."
)

message(
  "Selected alpha: ",
  best_alpha
)

message(
  "Selected lambda: ",
  signif(
    best_lambda,
    6
  )
)

message(
  "Season-grouped cross-validated RMSE: ",
  round(
    tuning_results$cross_validated_rmse[[1]],
    3
  )
)

message(
  "Model saved to: ",
  model_path
)

message(
  "Coefficients saved to: ",
  coefficient_path
)

message(
  "Tuning results saved to: ",
  tuning_path
)

message(
  "2025 Championship Scores saved to: ",
  championship_scores_path
)

message(
  "Scored 2025 team data saved to: ",
  scored_features_path
)

print(
  championship_results_2025 |>
    slice_head(
      n = 20
    )
)


