# ==============================================================================
# 08_monte_carlo_simulation.R
#
# 2026 NCAA TOURNAMENT MONTE CARLO SIMULATION
#
# Purpose:
#   1. Train a 2026 Championship Score using 2012-2025 historical summaries
#   2. Score all 2026 tournament teams using pre-tournament regular-season data
#   3. Add a Strength of Schedule adjustment
#   4. Calculate matchup-specific three-point, turnover, rebounding, and
#      defensive-resistance edges
#   5. Convert each matchup into a win probability
#   6. Simulate the complete 2026 bracket thousands of times
#   7. Estimate each team's advancement and championship probabilities
#
# Required inputs:
#   data/processed/historical_tournament_summary_2026.csv
#   data/processed/2026_tournament_teams.csv
#   data/raw/2026_strength_of_schedule.csv
#   data/raw/2026_tournament_bracket.csv
#
# Required bracket columns:
#   round_number, region, round, bracket_game_id,
#   team1, team1_seed, team2, team2_seed,
#   next_game_id, next_slot
#
# Expected bracket rounds:
#   First Four, Round of 64, Round of 32, Sweet 16,
#   Elite Eight, Final Four, Championship
#
# Main outputs:
#   data/final/2026_championship_scores.csv
#   data/final/2026_monte_carlo_probabilities.csv
#   data/final/2026_monte_carlo_matchup_log.csv
#   results/championship_score_model_2026.rds
#   results/championship_score_coefficients_2026.csv
#
# Important:
#   This is a deliberately simplified simulation model. The Championship Score
#   supplies baseline team quality. Matchup edges modify game probabilities,
#   but no historical game-level upset model is trained.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "tidyr",
  "tibble",
  "stringr",
  "purrr",
  "here",
  "glmnet"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install these required packages before running the script: ",
    paste(missing_packages, collapse = ", "),
    "\nRun: install.packages(c(",
    paste0('"', missing_packages, '"', collapse = ", "),
    "))"
  )
}

library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(stringr)
library(purrr)
library(here)
library(glmnet)

#Settings and inputs
tournament_season <- 2026

# Start with 1,000 for a fast complete run. Increase to ~10,000 for final results.
number_of_simulations <- 1000

random_seed <- 2026

# Strength-of-schedule adjustment added to the raw Championship Score.
sos_adjustment_weight <- 0.25

# Matchup probability weights.
quality_weight <- 1.15
three_point_weight <- 0.35
turnover_weight <- 0.30
rebounding_weight <- 0.30
defensive_resistance_weight <- 0.25

# Prevent any single game from becoming a literal 0% or 100% event.
minimum_game_probability <- 0.05
maximum_game_probability <- 0.95

#Filepaths

historical_summary_path <- here(
  "data",
  "processed",
  "historical_tournament_summary_2026.csv"
)

tournament_teams_path <- here(
  "data",
  "processed",
  "2026_tournament_teams.csv"
)

strength_of_schedule_path <- here(
  "data",
  "raw",
  "2026_strength_of_schedule.csv"
)

bracket_path <- here(
  "data",
  "raw",
  "2026_tournament_bracket.csv"
)

championship_model_path <- here(
  "results",
  "championship_score_model_2026.rds"
)

championship_coefficients_path <- here(
  "results",
  "championship_score_coefficients_2026.csv"
)

championship_scores_path <- here(
  "data",
  "final",
  "2026_championship_scores.csv"
)

monte_carlo_probabilities_path <- here(
  "data",
  "final",
  "2026_monte_carlo_probabilities.csv"
)

matchup_log_path <- here(
  "data",
  "final",
  "2026_monte_carlo_matchup_log.csv"
)

dir.create(
  here("data", "final"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("results"),
  recursive = TRUE,
  showWarnings = FALSE
)

#Confirm inputs exist

required_files <- c(
  historical_summary_path,
  tournament_teams_path,
  strength_of_schedule_path,
  bracket_path
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

#Helper functions

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


calculate_z_score <- function(x) {
  
  field_mean <- mean(
    x,
    na.rm = TRUE
  )
  
  field_sd <- sd(
    x,
    na.rm = TRUE
  )
  
  if (
    !is.finite(field_sd) ||
    field_sd == 0
  ) {
    stop(
      "A model feature has zero or invalid standard deviation."
    )
  }
  
  as.numeric(
    (
      x -
        field_mean
    ) /
      field_sd
  )
}


# Convert common project abbreviations to the longer names often used by Sports Reference

canonical_team_name <- function(team_name) {
  
  case_when(
    is.na(team_name) ~
      NA_character_,
    
    str_detect(
      team_name,
      "^Winner of "
    ) ~
      team_name,
    
    team_name == "UConn" ~
      "Connecticut",
    
    team_name == "BYU" ~
      "Brigham Young",
    
    team_name == "VCU" ~
      "Virginia Commonwealth",
    
    team_name == "UNC" ~
      "North Carolina",
    
    team_name == "NC State" ~
      "North Carolina State",
    
    team_name == "Ole Miss" ~
      "Mississippi",
    
    team_name == "SIU Edwardsville" ~
      "Southern Illinois-Edwardsville",
    
    team_name == "Southern Illinois–Edwardsville" ~
      "Southern Illinois-Edwardsville",
    
    team_name == "St. John's" ~
      "St. John's (NY)",
    
    team_name == "Saint Francis" ~
      "Saint Francis (PA)",
    
    team_name == "College of Charleston" ~
      "Charleston",
    
    team_name == "FAU" ~
      "Florida Atlantic",
    
    team_name == "Saint Mary's (CA)" ~
      "Saint Mary's",
    
    team_name == "Grambling" ~
      "Grambling State",
    
    TRUE ~
      team_name
  )
}


normalized_team_key <- function(team_name) {
  
  canonical_team_name(team_name) |>
    str_to_lower() |>
    str_replace_all("&", "and") |>
    str_replace_all("[–—]", "-") |>
    str_replace_all("[^a-z0-9]", "")
}

#Read data

historical_summary <- read_csv(
  historical_summary_path,
  show_col_types = FALSE
)

tournament_teams <- read_csv(
  tournament_teams_path,
  show_col_types = FALSE
)

strength_of_schedule <- read_csv(
  strength_of_schedule_path,
  show_col_types = FALSE
)

tournament_bracket <- read_csv(
  bracket_path,
  show_col_types = FALSE
)

#Validate historical training seasons

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
  2024,
  2025
)

actual_historical_seasons <- historical_summary |>
  distinct(season) |>
  arrange(season) |>
  pull(season)

if (!identical(
  actual_historical_seasons,
  expected_historical_seasons
)) {
  stop(
    "historical_tournament_summary_2026.csv does not contain ",
    "the expected 2012-2025 seasons."
  )
}

if (nrow(historical_summary) != 208) {
  warning(
    "Expected 208 historical Sweet 16-or-better team-seasons, but found ",
    nrow(historical_summary),
    "."
  )
}

#Train simplified championship score

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

required_historical_columns <- c(
  "season",
  "team",
  "tournament_finish",
  "finish_weight",
  model_features
)

missing_historical_columns <- setdiff(
  required_historical_columns,
  names(historical_summary)
)

if (length(missing_historical_columns) > 0) {
  stop(
    "The historical summary is missing these columns: ",
    paste(missing_historical_columns, collapse = ", ")
  )
}

training_data <- historical_summary |>
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

feature_medians <- training_data |>
  summarise(
    across(
      all_of(model_features),
      ~ median(.x, na.rm = TRUE)
    )
  ) |>
  unlist(
    use.names = TRUE
  )

for (feature in model_features) {
  
  missing_rows <- is.na(
    training_data[[feature]]
  )
  
  training_data[[feature]][missing_rows] <-
    feature_medians[[feature]]
}

x_training <- training_data |>
  select(
    all_of(model_features)
  ) |>
  as.matrix()

y_training <- training_data$finish_weight

foldid <- as.integer(
  factor(
    training_data$season,
    levels = expected_historical_seasons
  )
)

set.seed(random_seed)

championship_cv_model <- cv.glmnet(
  x = x_training,
  y = y_training,
  family = "gaussian",
  alpha = 1,
  foldid = foldid,
  standardize = TRUE,
  intercept = TRUE,
  type.measure = "mse"
)

best_lambda <- championship_cv_model$lambda.min

championship_model <- glmnet(
  x = x_training,
  y = y_training,
  family = "gaussian",
  alpha = 1,
  lambda = best_lambda,
  standardize = TRUE,
  intercept = TRUE
)

coefficient_matrix <- as.matrix(
  coef(
    championship_model,
    s = best_lambda
  )
)

championship_coefficients <- tibble(
  feature = rownames(coefficient_matrix),
  coefficient = as.numeric(
    coefficient_matrix[, 1]
  )
) |>
  mutate(
    absolute_coefficient =
      abs(coefficient),
    
    direction = case_when(
      coefficient > 0 ~
        "Positive",
      
      coefficient < 0 ~
        "Negative",
      
      TRUE ~
        "Excluded"
    )
  ) |>
  arrange(
    desc(absolute_coefficient)
  )

saveRDS(
  list(
    model_name =
      "2026 Championship Score",
    
    training_seasons =
      expected_historical_seasons,
    
    feature_names =
      model_features,
    
    feature_medians =
      feature_medians,
    
    alpha =
      1,
    
    lambda =
      best_lambda,
    
    model =
      championship_model
  ),
  championship_model_path
)

write_csv(
  championship_coefficients,
  championship_coefficients_path,
  na = ""
)

#Validate 2026 team statistics

required_team_columns <- c(
  "team",
  "games",
  "team_game_score",
  "opp_team_game_score",
  "fg",
  "fga",
  "fg3",
  "fg3a",
  "fg3_pct",
  "fta",
  "orb",
  "drb",
  "tov",
  "opp_fg",
  "opp_fga",
  "opp_fg3",
  "opp_fg3a",
  "opp_fg3_pct",
  "opp_fta",
  "opp_orb",
  "opp_drb",
  "opp_tov"
)

missing_team_columns <- setdiff(
  required_team_columns,
  names(tournament_teams)
)

if (length(missing_team_columns) > 0) {
  stop(
    "2026_tournament_teams.csv is missing these columns: ",
    paste(missing_team_columns, collapse = ", ")
  )
}

if (nrow(tournament_teams) != 68) {
  stop(
    "Expected 68 tournament teams, but found ",
    nrow(tournament_teams),
    "."
  )
}

tournament_teams <- tournament_teams |>
  mutate(
    team =
      canonical_team_name(team),
    
    team_key =
      normalized_team_key(team)
  )

if (anyDuplicated(tournament_teams$team_key) > 0) {
  stop(
    "Duplicate normalized team names were found in the 2026 team dataset."
  )
}


#Read strength of schedule

if (
  all(
    c("Team", "SOS") %in%
    names(strength_of_schedule)
  )
) {
  
  strength_of_schedule <- strength_of_schedule |>
    transmute(
      team =
        canonical_team_name(Team),
      
      team_key =
        normalized_team_key(team),
      
      strength_of_schedule =
        as.numeric(SOS)
    )
  
} else if (
  all(
    c(
      "team",
      "strength_of_schedule"
    ) %in%
    names(strength_of_schedule)
  )
) {
  
  strength_of_schedule <- strength_of_schedule |>
    transmute(
      team =
        canonical_team_name(team),
      
      team_key =
        normalized_team_key(team),
      
      strength_of_schedule =
        as.numeric(strength_of_schedule)
    )
  
} else {
  
  stop(
    "2026_strength_of_schedule.csv must contain either ",
    "'Team' and 'SOS' or 'team' and 'strength_of_schedule'."
  )
}

strength_of_schedule <- strength_of_schedule |>
  filter(
    !is.na(team_key),
    !is.na(strength_of_schedule)
  ) |>
  distinct(
    team_key,
    .keep_all = TRUE
  )

tournament_teams <- tournament_teams |>
  left_join(
    strength_of_schedule |>
      select(
        team_key,
        strength_of_schedule
      ),
    by = "team_key"
  )

missing_sos <- tournament_teams |>
  filter(
    is.na(strength_of_schedule)
  ) |>
  select(team)

if (nrow(missing_sos) > 0) {
  print(missing_sos)
  
  stop(
    nrow(missing_sos),
    " tournament teams are missing Strength of Schedule values."
  )
}


#Calculate 2026 score features

statistics_are_totals <- median(
  tournament_teams$fga,
  na.rm = TRUE
) > 150

if (statistics_are_totals) {
  message(
    "2026 counting statistics interpreted as season totals."
  )
} else {
  message(
    "2026 counting statistics interpreted as per-game averages."
  )
}

team_profiles <- tournament_teams |>
  mutate(
    season =
      tournament_season,
    
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
  )

for (feature in model_features) {
  
  missing_rows <- is.na(
    team_profiles[[feature]]
  )
  
  team_profiles[[feature]][missing_rows] <-
    feature_medians[[feature]]
}

x_2026 <- team_profiles |>
  select(
    all_of(model_features)
  ) |>
  as.matrix()

raw_finish_score <- as.numeric(
  predict(
    championship_model,
    newx = x_2026,
    s = best_lambda
  )
)

team_profiles <- team_profiles |>
  mutate(
    raw_finish_score =
      raw_finish_score,
    
    sos_z_score =
      calculate_z_score(
        strength_of_schedule
      ),
    
    sos_adjustment =
      sos_adjustment_weight *
      sos_z_score,
    
    adjusted_finish_score =
      raw_finish_score +
      sos_adjustment,
    
    championship_rank =
      min_rank(
        desc(adjusted_finish_score)
      ),
    
    championship_score =
      100 * (
        n() -
          min_rank(adjusted_finish_score)
      ) /
      (
        n() - 1
      )
  )

#Calculat standardized matchup features

team_profiles <- team_profiles |>
  mutate(
    offensive_rating =
      avg_offensive_rating,
    
    defensive_rating =
      avg_defensive_rating,
    
    efg_pct =
      avg_efg_pct,
    
    opponent_efg_pct =
      avg_opponent_efg_pct,
    
    fg3_attempt_rate =
      avg_fg3_attempt_rate,
    
    tov_pct =
      avg_tov_pct,
    
    forced_tov_pct =
      avg_forced_tov_pct,
    
    orb_pct =
      avg_orb_pct,
    
    drb_pct =
      avg_drb_pct,
    
    z_adjusted_finish_score =
      calculate_z_score(
        adjusted_finish_score
      ),
    
    z_offensive_rating =
      calculate_z_score(
        offensive_rating
      ),
    
    z_defensive_rating =
      calculate_z_score(
        defensive_rating
      ),
    
    z_efg_pct =
      calculate_z_score(
        efg_pct
      ),
    
    z_opponent_efg_pct =
      calculate_z_score(
        opponent_efg_pct
      ),
    
    z_fg3_pct =
      calculate_z_score(
        fg3_pct
      ),
    
    z_fg3_attempt_rate =
      calculate_z_score(
        fg3_attempt_rate
      ),
    
    z_opponent_fg3_pct =
      calculate_z_score(
        opp_fg3_pct
      ),
    
    z_tov_pct =
      calculate_z_score(
        tov_pct
      ),
    
    z_forced_tov_pct =
      calculate_z_score(
        forced_tov_pct
      ),
    
    z_orb_pct =
      calculate_z_score(
        orb_pct
      ),
    
    z_drb_pct =
      calculate_z_score(
        drb_pct
      ),
    
    defensive_rating_strength =
      -z_defensive_rating,
    
    opponent_efg_defense_strength =
      -z_opponent_efg_pct,
    
    perimeter_defense_weakness =
      z_opponent_fg3_pct,
    
    ball_security_weakness =
      z_tov_pct,
    
    defensive_rebounding_weakness =
      -z_drb_pct,
    
    offensive_weakness =
      -z_offensive_rating
  )


#Save 2026 championships scores

championship_scores_2026 <- team_profiles |>
  select(
    championship_rank,
    season,
    team,
    championship_score,
    raw_finish_score,
    strength_of_schedule,
    sos_z_score,
    sos_adjustment,
    adjusted_finish_score
  ) |>
  arrange(
    championship_rank,
    team
  )

write_csv(
  championship_scores_2026,
  championship_scores_path,
  na = ""
)

#Team profile lookup

team_profile_lookup <- split(
  team_profiles,
  team_profiles$team_key
)


# Create one consistent key for every unordered matchup.
create_matchup_key <- function(
    team1,
    team2
) {
  
  team_keys <- sort(
    c(
      normalized_team_key(team1),
      normalized_team_key(team2)
    )
  )
  
  paste(
    team_keys,
    collapse = "__"
  )
}


#Returns probability that team 2 defeats team 1

calculate_team1_win_probability <- function(
    team1,
    team2
) {
  
  team1_key <- normalized_team_key(team1)
  team2_key <- normalized_team_key(team2)
  
  profile1 <- team_profile_lookup[[team1_key]]
  profile2 <- team_profile_lookup[[team2_key]]
  
  if (is.null(profile1) || nrow(profile1) != 1) {
    stop(
      "Could not uniquely locate the team profile for ",
      team1,
      "."
    )
  }
  
  if (is.null(profile2) || nrow(profile2) != 1) {
    stop(
      "Could not uniquely locate the team profile for ",
      team2,
      "."
    )
  }
  
  
  # Three-point matchup strength for each team.
  three_point_strength_1 <-
    (
      0.60 *
        profile1$z_fg3_pct
    ) +
    (
      0.25 *
        profile1$z_fg3_attempt_rate
    ) +
    (
      0.15 *
        profile2$perimeter_defense_weakness
    )
  
  three_point_strength_2 <-
    (
      0.60 *
        profile2$z_fg3_pct
    ) +
    (
      0.25 *
        profile2$z_fg3_attempt_rate
    ) +
    (
      0.15 *
        profile1$perimeter_defense_weakness
    )
  
  three_point_edge <-
    three_point_strength_1 -
    three_point_strength_2
  
  
  # Turnover matchup strength for each team.
  turnover_strength_1 <-
    (
      0.60 *
        profile1$z_forced_tov_pct
    ) +
    (
      0.40 *
        profile2$ball_security_weakness
    )
  
  turnover_strength_2 <-
    (
      0.60 *
        profile2$z_forced_tov_pct
    ) +
    (
      0.40 *
        profile1$ball_security_weakness
    )
  
  turnover_edge <-
    turnover_strength_1 -
    turnover_strength_2
  
  
  # Rebounding matchup strength for each team.
  rebounding_strength_1 <-
    (
      0.35 *
        profile1$z_orb_pct
    ) +
    (
      0.20 *
        profile2$defensive_rebounding_weakness
    ) +
    (
      0.30 *
        profile1$z_drb_pct
    ) -
    (
      0.15 *
        profile2$z_orb_pct
    )
  
  rebounding_strength_2 <-
    (
      0.35 *
        profile2$z_orb_pct
    ) +
    (
      0.20 *
        profile1$defensive_rebounding_weakness
    ) +
    (
      0.30 *
        profile2$z_drb_pct
    ) -
    (
      0.15 *
        profile1$z_orb_pct
    )
  
  rebounding_edge <-
    rebounding_strength_1 -
    rebounding_strength_2
  
  
  # Defensive resistance for each team.
  defensive_strength_1 <-
    (
      0.55 *
        profile1$defensive_rating_strength
    ) +
    (
      0.35 *
        profile1$opponent_efg_defense_strength
    ) +
    (
      0.10 *
        profile2$offensive_weakness
    )
  
  defensive_strength_2 <-
    (
      0.55 *
        profile2$defensive_rating_strength
    ) +
    (
      0.35 *
        profile2$opponent_efg_defense_strength
    ) +
    (
      0.10 *
        profile1$offensive_weakness
    )
  
  defensive_resistance_edge <-
    defensive_strength_1 -
    defensive_strength_2
  
  
  quality_edge <-
    profile1$z_adjusted_finish_score -
    profile2$z_adjusted_finish_score
  
  
  linear_predictor <-
    (
      quality_weight *
        quality_edge
    ) +
    (
      three_point_weight *
        three_point_edge
    ) +
    (
      turnover_weight *
        turnover_edge
    ) +
    (
      rebounding_weight *
        rebounding_edge
    ) +
    (
      defensive_resistance_weight *
        defensive_resistance_edge
    )
  
  
  team1_win_probability <-
    plogis(
      linear_predictor
    )
  
  team1_win_probability <-
    pmin(
      maximum_game_probability,
      pmax(
        minimum_game_probability,
        team1_win_probability
      )
    )
  
  
  tibble(
    team1 =
      profile1$team[[1]],
    
    team2 =
      profile2$team[[1]],
    
    quality_edge =
      as.numeric(quality_edge),
    
    three_point_edge =
      as.numeric(three_point_edge),
    
    turnover_edge =
      as.numeric(turnover_edge),
    
    rebounding_edge =
      as.numeric(rebounding_edge),
    
    defensive_resistance_edge =
      as.numeric(defensive_resistance_edge),
    
    linear_predictor =
      as.numeric(linear_predictor),
    
    team1_win_probability =
      as.numeric(team1_win_probability),
    
    team2_win_probability =
      as.numeric(
        1 -
          team1_win_probability
      )
  )
}

#Precalculate all possible matchups

all_team_names <- team_profiles$team

all_possible_matchups <- combn(
  all_team_names,
  2,
  simplify = FALSE
)

message(
  "Precalculating ",
  length(all_possible_matchups),
  " possible matchup probabilities..."
)

matchup_probability_table <- map_dfr(
  all_possible_matchups,
  function(matchup) {
    
    calculate_team1_win_probability(
      team1 = matchup[[1]],
      team2 = matchup[[2]]
    )
  }
) |>
  mutate(
    matchup_key = map2_chr(
      team1,
      team2,
      create_matchup_key
    )
  )

if (
  anyDuplicated(
    matchup_probability_table$matchup_key
  ) > 0
) {
  stop(
    "Duplicate matchup keys were created."
  )
}

matchup_probability_lookup <- split(
  matchup_probability_table,
  matchup_probability_table$matchup_key
)

message(
  "Matchup probabilities precalculated."
)


# Return a precalculated matchup in the requested team1/team2 direction.
get_matchup_probability <- function(
    team1,
    team2
) {
  
  matchup_key <- create_matchup_key(
    team1,
    team2
  )
  
  matchup_row <- matchup_probability_lookup[[
    matchup_key
  ]]
  
  # This fallback should rarely be needed, but prevents a naming mismatch
  # from terminating the entire simulation.
  if (is.null(matchup_row)) {
    
    warning(
      "Precalculated matchup not found for ",
      team1,
      " versus ",
      team2,
      ". Calculating it directly."
    )
    
    return(
      calculate_team1_win_probability(
        team1,
        team2
      )
    )
  }
  
  requested_team1_key <- normalized_team_key(
    team1
  )
  
  stored_team1_key <- normalized_team_key(
    matchup_row$team1[[1]]
  )
  
  if (
    requested_team1_key ==
    stored_team1_key
  ) {
    
    return(
      matchup_row |>
        select(
          -matchup_key
        )
    )
  }
  
  tibble(
    team1 =
      canonical_team_name(team1),
    
    team2 =
      canonical_team_name(team2),
    
    quality_edge =
      -matchup_row$quality_edge[[1]],
    
    three_point_edge =
      -matchup_row$three_point_edge[[1]],
    
    turnover_edge =
      -matchup_row$turnover_edge[[1]],
    
    rebounding_edge =
      -matchup_row$rebounding_edge[[1]],
    
    defensive_resistance_edge =
      -matchup_row$defensive_resistance_edge[[1]],
    
    linear_predictor =
      -matchup_row$linear_predictor[[1]],
    
    team1_win_probability =
      matchup_row$team2_win_probability[[1]],
    
    team2_win_probability =
      matchup_row$team1_win_probability[[1]]
  )
}

#Prepare and validate bracket 

required_bracket_columns <- c(
  "round_number",
  "region",
  "round",
  "bracket_game_id",
  "team1",
  "team1_seed",
  "team2",
  "team2_seed",
  "next_game_id",
  "next_slot"
)

missing_bracket_columns <- setdiff(
  required_bracket_columns,
  names(tournament_bracket)
)

if (length(missing_bracket_columns) > 0) {
  stop(
    "2026_tournament_bracket.csv is missing these columns: ",
    paste(missing_bracket_columns, collapse = ", ")
  )
}

tournament_bracket <- tournament_bracket |>
  mutate(
    team1 =
      canonical_team_name(team1),
    
    team2 =
      canonical_team_name(team2)
  ) |>
  arrange(
    round_number,
    bracket_game_id
  )

expected_round_counts <- tibble(
  round = c(
    "First Four",
    "Round of 64",
    "Round of 32",
    "Sweet 16",
    "Elite Eight",
    "Final Four",
    "Championship"
  ),
  
  expected_games = c(
    4L,
    32L,
    16L,
    8L,
    4L,
    2L,
    1L
  )
)

bracket_round_validation <- tournament_bracket |>
  count(
    round,
    name =
      "actual_games"
  ) |>
  right_join(
    expected_round_counts,
    by =
      "round"
  ) |>
  mutate(
    passed =
      actual_games ==
      expected_games
  )

if (!all(bracket_round_validation$passed)) {
  print(bracket_round_validation)
  
  stop(
    "The 2026 bracket does not contain the expected tournament round counts."
  )
}

known_opening_teams <- tournament_bracket |>
  filter(
    round ==
      "First Four"
  ) |>
  select(
    team1,
    team2
  ) |>
  pivot_longer(
    everything(),
    values_to =
      "team"
  ) |>
  pull(team)

known_round64_teams <- tournament_bracket |>
  filter(
    round ==
      "Round of 64"
  ) |>
  select(
    team1,
    team2
  ) |>
  pivot_longer(
    everything(),
    values_to =
      "team"
  ) |>
  pull(team) |>
  discard(
    ~ is.na(.x) ||
      str_detect(
        .x,
        "^Winner of "
      )
  )

all_known_bracket_teams <- unique(
  c(
    known_opening_teams,
    known_round64_teams
  )
)

unmatched_bracket_teams <- all_known_bracket_teams[
  !normalized_team_key(
    all_known_bracket_teams
  ) %in%
    team_profiles$team_key
]

if (length(unmatched_bracket_teams) > 0) {
  print(
    tibble(
      unmatched_team =
        unmatched_bracket_teams
    )
  )
  
  stop(
    length(unmatched_bracket_teams),
    " bracket teams did not match the 2026 team-profile dataset."
  )
}

#Advance simulated winner

advance_winner <- function(
    bracket,
    next_game_id,
    next_slot,
    winner,
    winner_seed
) {
  
  if (is.na(next_game_id)) {
    return(bracket)
  }
  
  target_row <- which(
    bracket$bracket_game_id ==
      next_game_id
  )
  
  if (length(target_row) != 1) {
    stop(
      "Could not uniquely locate next bracket game ",
      next_game_id,
      "."
    )
  }
  
  if (next_slot == 1L) {
    
    bracket$team1[[target_row]] <-
      winner
    
    bracket$team1_seed[[target_row]] <-
      winner_seed
    
  } else if (next_slot == 2L) {
    
    bracket$team2[[target_row]] <-
      winner
    
    bracket$team2_seed[[target_row]] <-
      winner_seed
    
  } else {
    
    stop(
      "Invalid next_slot for game ",
      next_game_id,
      "."
    )
  }
  
  bracket
}

#Simulate one complete tournament

simulate_one_tournament <- function(
    bracket_template,
    save_matchup_log = FALSE
) {
  
  working_bracket <- bracket_template
  
  round_sequence <- c(
    "First Four",
    "Round of 64",
    "Round of 32",
    "Sweet 16",
    "Elite Eight",
    "Final Four",
    "Championship"
  )
  
  simulation_results <- vector(
    mode = "list",
    length = 67
  )
  
  result_index <- 1L
  
  
  for (current_round in round_sequence) {
    
    game_ids <- working_bracket |>
      filter(
        round ==
          current_round
      ) |>
      arrange(
        bracket_game_id
      ) |>
      pull(
        bracket_game_id
      )
    
    
    for (current_game_id in game_ids) {
      
      current_row <- which(
        working_bracket$bracket_game_id ==
          current_game_id
      )
      
      current_team1 <-
        working_bracket$team1[[current_row]]
      
      current_team2 <-
        working_bracket$team2[[current_row]]
      
      if (
        is.na(current_team1) ||
        is.na(current_team2)
      ) {
        stop(
          "Game ",
          current_game_id,
          " is missing one or both teams."
        )
      }
      
      if (
        str_detect(
          current_team1,
          "^Winner of "
        ) ||
        str_detect(
          current_team2,
          "^Winner of "
        )
      ) {
        stop(
          "An unresolved winner placeholder remains in game ",
          current_game_id,
          "."
        )
      }
      
      probability_result <-
        get_matchup_probability(
          current_team1,
          current_team2
        )
      
      team1_wins <-
        runif(1) <
        probability_result$team1_win_probability[[1]]
      
      if (team1_wins) {
        
        winner <-
          probability_result$team1[[1]]
        
        loser <-
          probability_result$team2[[1]]
        
        winner_seed <-
          working_bracket$team1_seed[[current_row]]
        
      } else {
        
        winner <-
          probability_result$team2[[1]]
        
        loser <-
          probability_result$team1[[1]]
        
        winner_seed <-
          working_bracket$team2_seed[[current_row]]
      }
      
      
      simulation_results[[result_index]] <- tibble(
        round =
          current_round,
        
        region =
          working_bracket$region[[current_row]],
        
        bracket_game_id =
          current_game_id,
        
        team1 =
          probability_result$team1[[1]],
        
        team2 =
          probability_result$team2[[1]],
        
        team1_win_probability =
          probability_result$team1_win_probability[[1]],
        
        team2_win_probability =
          probability_result$team2_win_probability[[1]],
        
        quality_edge =
          probability_result$quality_edge[[1]],
        
        three_point_edge =
          probability_result$three_point_edge[[1]],
        
        turnover_edge =
          probability_result$turnover_edge[[1]],
        
        rebounding_edge =
          probability_result$rebounding_edge[[1]],
        
        defensive_resistance_edge =
          probability_result$defensive_resistance_edge[[1]],
        
        winner =
          winner,
        
        loser =
          loser
      )
      
      result_index <-
        result_index +
        1L
      
      
      working_bracket <- advance_winner(
        bracket =
          working_bracket,
        
        next_game_id =
          working_bracket$next_game_id[[current_row]],
        
        next_slot =
          working_bracket$next_slot[[current_row]],
        
        winner =
          winner,
        
        winner_seed =
          winner_seed
      )
    }
  }
  
  
  simulation_results <- bind_rows(
    simulation_results
  )
  
  if (nrow(simulation_results) != 67) {
    stop(
      "A simulated tournament did not create exactly 67 game results."
    )
  }
  
  simulation_results
}

#Run Monte Carlo simulations

set.seed(random_seed)

message(
  "Running ",
  format(
    number_of_simulations,
    big.mark = ","
  ),
  " Monte Carlo tournament simulations..."
)

simulation_summaries <- vector(
  mode = "list",
  length = number_of_simulations
)

representative_matchup_log <- NULL


for (
  simulation_number in seq_len(
    number_of_simulations
  )
) {
  
  simulation_result <- simulate_one_tournament(
    bracket_template =
      tournament_bracket
  )
  
  if (simulation_number == 1) {
    representative_matchup_log <-
      simulation_result
  }
  
  simulation_summaries[[simulation_number]] <-
    simulation_result |>
    transmute(
      simulation =
        simulation_number,
      
      round,
      winner
    )
  
  if (
    simulation_number == 1 ||
    simulation_number %% 100 == 0 ||
    simulation_number ==
    number_of_simulations
  ) {
    message(
      "Completed simulation ",
      format(
        simulation_number,
        big.mark = ","
      ),
      " of ",
      format(
        number_of_simulations,
        big.mark = ","
      ),
      "."
    )
  }
}


all_simulation_winners <- bind_rows(
  simulation_summaries
)


#Summarize advancement probabilities

round_win_counts <- all_simulation_winners |>
  count(
    winner,
    round,
    name =
      "wins"
  ) |>
  mutate(
    probability =
      wins /
      number_of_simulations
  ) |>
  select(
    team =
      winner,
    
    round,
    probability
  ) |>
  pivot_wider(
    names_from =
      round,
    
    values_from =
      probability,
    
    values_fill =
      0
  )

for (
  required_round in c(
    "First Four",
    "Round of 64",
    "Round of 32",
    "Sweet 16",
    "Elite Eight",
    "Final Four",
    "Championship"
  )
) {
  
  if (!required_round %in% names(round_win_counts)) {
    round_win_counts[[required_round]] <-
      0
  }
}


monte_carlo_probabilities <- team_profiles |>
  select(
    team,
    championship_rank,
    championship_score,
    adjusted_finish_score
  ) |>
  left_join(
    round_win_counts,
    by =
      "team"
  ) |>
  mutate(
    across(
      c(
        `First Four`,
        `Round of 64`,
        `Round of 32`,
        `Sweet 16`,
        `Elite Eight`,
        `Final Four`,
        `Championship`
      ),
      ~ coalesce(.x, 0)
    ),
    
    first_four_win_probability =
      `First Four`,
    
    round_of_32_probability =
      `Round of 64`,
    
    sweet_16_probability =
      `Round of 32`,
    
    elite_8_probability =
      `Sweet 16`,
    
    final_four_probability =
      `Elite Eight`,
    
    championship_game_probability =
      `Final Four`,
    
    championship_probability =
      `Championship`
  ) |>
  select(
    team,
    championship_rank,
    championship_score,
    adjusted_finish_score,
    first_four_win_probability,
    round_of_32_probability,
    sweet_16_probability,
    elite_8_probability,
    final_four_probability,
    championship_game_probability,
    championship_probability
  ) |>
  arrange(
    desc(championship_probability),
    desc(final_four_probability),
    championship_rank
  )

#Validate save ouputs
if (
  abs(
    sum(
      monte_carlo_probabilities$
      championship_probability
    ) -
    1
  ) > 1e-8
) {
  stop(
    "Championship probabilities do not sum to 1."
  )
}

write_csv(
  monte_carlo_probabilities,
  monte_carlo_probabilities_path,
  na = ""
)

write_csv(
  representative_matchup_log,
  matchup_log_path,
  na = ""
)

#Display final results

message(
  "Monte Carlo simulation complete."
)

message(
  "Championship Scores saved to: ",
  championship_scores_path
)

message(
  "Tournament probabilities saved to: ",
  monte_carlo_probabilities_path
)

message(
  "Representative simulated bracket saved to: ",
  matchup_log_path
)

message(
  "Championship model saved to: ",
  championship_model_path
)

print(
  monte_carlo_probabilities |>
    slice_head(
      n = 20
    )
)