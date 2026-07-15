library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(here)

#Input filepath

input_file <- here(
  "data",
  "processed",
  "historical_tournament_statistics.csv"
)

#Output filepath

output_file <- here(
  "data",
  "processed",
  "historical_tournament_summary.csv"
)

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

#Read tournament statistics from games

historical_games <- read_csv(
  input_file,
  show_col_types = FALSE
) |>
  mutate(
    game_date = as.Date(game_date)
  )

required_columns <- c(
  "season",
  "game_id",
  "game_date",
  "game_url",
  "team1_team",
  "team2_team",
  "team1_win",
  "team2_win"
)

missing_columns <- setdiff(
  required_columns,
  names(historical_games)
)

if (length(missing_columns) > 0) {
  stop(
    "The input file is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (anyDuplicated(historical_games$game_id) > 0) {
  stop("Duplicate game IDs were found in the input dataset.")
}

#Identify the main bracket start for each season, assumes first full-bracket day has 16 games

games_per_date <- historical_games |>
  count(
    season,
    game_date,
    name = "games_on_date"
  )

main_bracket_dates <- games_per_date |>
  filter(games_on_date >= 15) |>
  group_by(season) |>
  summarise(
    main_bracket_start = min(game_date),
    .groups = "drop"
  )

missing_start_dates <- setdiff(
  unique(historical_games$season),
  main_bracket_dates$season
)

if (length(missing_start_dates) > 0) {
  stop(
    "Could not identify the main-bracket start for: ",
    paste(missing_start_dates, collapse = ", ")
  )
}

historical_games <- historical_games |>
  left_join(
    main_bracket_dates,
    by = "season"
  ) |>
  mutate(
    first_four_game = game_date < main_bracket_start
  )

#Assign each main bracket data to a tournament round (Dates 1-2: Round of 64, Dates 3-4: Round of 32, Dates 5-6: Sweet 16, Dates 7-8: Elite Eight, Date 9: Final Four, Date 10: Championship)

round_calendar <- historical_games |>
  filter(!first_four_game) |>
  distinct(
    season,
    game_date
  ) |>
  arrange(
    season,
    game_date
  ) |>
  group_by(season) |>
  mutate(
    main_bracket_date_number = row_number(),
    tournament_round = case_when(
      main_bracket_date_number <= 2 ~ "Round of 64",
      main_bracket_date_number <= 4 ~ "Round of 32",
      main_bracket_date_number <= 6 ~ "Sweet 16",
      main_bracket_date_number <= 8 ~ "Elite Eight",
      main_bracket_date_number == 9 ~ "Final Four",
      main_bracket_date_number == 10 ~ "Championship",
      TRUE ~ NA_character_
    )
  ) |>
  ungroup()

round_date_validation <- round_calendar |>
  count(
    season,
    name = "main_bracket_dates"
  )

print(round_date_validation)

if (any(round_date_validation$main_bracket_dates != 10)) {
  stop(
    "At least one season does not contain the expected ",
    "10 main-bracket game dates."
  )
}

historical_games <- historical_games |>
  left_join(
    round_calendar |>
      select(
        season,
        game_date,
        tournament_round
      ),
    by = c(
      "season",
      "game_date"
    )
  ) |>
  mutate(
    tournament_round = if_else(
      first_four_game,
      "First Four",
      tournament_round
    )
  ) |>
  select(
    -main_bracket_start
  )

#Convert one row per game into one row per team per game
build_team_rows <- function(
    data,
    team_prefix,
    opponent_prefix,
    win_column
) {
  
  prefixed_columns <- names(data)[
    str_starts(
      names(data),
      team_prefix
    )
  ]
  
  # The win field is added separately under the standard name "win."
  prefixed_columns <- setdiff(
    prefixed_columns,
    win_column
  )
  
  team_rows <- data |>
    select(
      season,
      game_id,
      game_date,
      game_url,
      first_four_game,
      tournament_round,
      estimated_possessions,
      all_of(prefixed_columns)
    ) |>
    rename_with(
      ~ str_remove(
        .x,
        paste0("^", team_prefix)
      ),
      all_of(prefixed_columns)
    )
  
  team_rows |>
    mutate(
      opponent = data[[paste0(opponent_prefix, "team")]],
      win = as.integer(data[[win_column]])
    ) |>
    relocate(
      season,
      team,
      opponent,
      game_id,
      game_date,
      game_url,
      tournament_round,
      first_four_game,
      win
    )
}

team1_games <- build_team_rows(
  data = historical_games,
  team_prefix = "team1_",
  opponent_prefix = "team2_",
  win_column = "team1_win"
)

team2_games <- build_team_rows(
  data = historical_games,
  team_prefix = "team2_",
  opponent_prefix = "team1_",
  win_column = "team2_win"
)

historical_team_games <- bind_rows(
  team1_games,
  team2_games
) |>
  arrange(
    season,
    team,
    game_date,
    game_id
  )

expected_team_game_rows <- nrow(historical_games) * 2

if (nrow(historical_team_games) != expected_team_game_rows) {
  stop(
    "Expected ",
    expected_team_game_rows,
    " team-game rows, but created ",
    nrow(historical_team_games),
    "."
  )
}

#Determine the team's tournament finish based on their final main-bracket game

team_finishes <- historical_team_games |>
  filter(!first_four_game) |>
  arrange(
    season,
    team,
    desc(game_date),
    desc(game_id)
  ) |>
  group_by(
    season,
    team
  ) |>
  slice(1) |>
  ungroup() |>
  transmute(
    season,
    team,
    final_round = tournament_round,
    final_game_win = win,
    
    tournament_finish = case_when(
      final_round == "Championship" &
        final_game_win == 1 ~ "Champion",
      
      final_round == "Championship" &
        final_game_win == 0 ~ "Runner-up",
      
      final_round == "Final Four" ~ "Final Four",
      
      final_round == "Elite Eight" ~ "Elite Eight",
      
      final_round == "Sweet 16" ~ "Sweet 16",
      
      TRUE ~ NA_character_
    ),
    
    finish_weight = case_when(
      tournament_finish == "Sweet 16" ~ 1,
      tournament_finish == "Elite Eight" ~ 2,
      tournament_finish == "Final Four" ~ 3,
      tournament_finish == "Runner-up" ~ 4,
      tournament_finish == "Champion" ~ 5,
      TRUE ~ NA_real_
    )
  ) |>
  filter(
    !is.na(tournament_finish)
  )

#Identify numeric statistics to average for tournament performance stats

numeric_columns <- historical_team_games |>
  select(where(is.numeric)) |>
  names()

summary_metrics <- setdiff(
  numeric_columns,
  c(
    "season",
    "win"
  )
)

#Build one summary row per succesful team per tournament
historical_tournament_summary <- historical_team_games |>
  inner_join(
    team_finishes,
    by = c(
      "season",
      "team"
    )
  ) |>
  group_by(
    season,
    team,
    tournament_finish,
    finish_weight
  ) |>
  summarise(
    games_played = n(),
    
    wins = sum(
      win,
      na.rm = TRUE
    ),
    
    losses = sum(
      win == 0,
      na.rm = TRUE
    ),
    
    first_four_games = sum(
      first_four_game,
      na.rm = TRUE
    ),
    
    main_bracket_games = sum(
      !first_four_game,
      na.rm = TRUE
    ),
    
    main_bracket_wins = sum(
      win[!first_four_game],
      na.rm = TRUE
    ),
    
    across(
      all_of(summary_metrics),
      ~ mean(
        .x,
        na.rm = TRUE
      ),
      .names = "avg_{.col}"
    ),
    
    sd_defensive_rating = sd(
      defensive_rating,
      na.rm = TRUE
    ),
    
    sd_points_allowed = sd(
      points_allowed,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(
        is.nan(.x),
        NA_real_,
        .x
      )
    ),
    
    tournament_finish = factor(
      tournament_finish,
      levels = c(
        "Sweet 16",
        "Elite Eight",
        "Final Four",
        "Runner-up",
        "Champion"
      ),
      ordered = TRUE
    )
  ) |>
  arrange(
    season,
    tournament_finish,
    team
  )

#Validate row counts

expected_total_rows <- 160

message(
  "Summary rows created: ",
  nrow(historical_tournament_summary)
)

if (nrow(historical_tournament_summary) != expected_total_rows) {
  warning(
    "Expected 160 team-season rows, but created ",
    nrow(historical_tournament_summary),
    "."
  )
} else {
  message(
    "The dataset contains all 160 expected team-season rows."
  )
}

# Confirm 16 teams per season.

teams_per_season <- historical_tournament_summary |>
  count(
    season,
    name = "successful_teams"
  )

print(teams_per_season)

if (any(teams_per_season$successful_teams != 16)) {
  warning(
    "At least one season does not contain exactly 16 teams."
  )
}

# Confirm the expected finish distribution.

finish_validation <- historical_tournament_summary |>
  count(
    season,
    tournament_finish,
    name = "actual_teams",
    .drop = FALSE
  ) |>
  mutate(
    expected_teams = case_when(
      tournament_finish == "Sweet 16" ~ 8L,
      tournament_finish == "Elite Eight" ~ 4L,
      tournament_finish == "Final Four" ~ 2L,
      tournament_finish == "Runner-up" ~ 1L,
      tournament_finish == "Champion" ~ 1L
    ),
    passed = actual_teams == expected_teams
  )

print(finish_validation)

if (any(!finish_validation$passed)) {
  warning(
    "At least one season has an unexpected finish distribution."
  )
} else {
  message(
    "Every season has the expected tournament-finish distribution."
  )
}

# Confirm one unique row per team per season.

if (
  anyDuplicated(
    historical_tournament_summary |>
    select(
      season,
      team
    )
  ) > 0
) {
  stop(
    "Duplicate team-season rows were found in the summary."
  )
}

#Save final dataset

write_csv(
  historical_tournament_summary,
  output_file,
  na = ""
)

message(
  "Finished. Historical tournament summary saved to: ",
  output_file
)
