library(httr2)
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(here)

#Input filepath

input_file <- here(
  "data",
  "raw",
  "tournament_brackets.csv"
)

#Output filepath

output_file <- here(
  "data",
  "processed",
  "historical_tournament_statistics.csv"
)

error_file <- here(
  "data",
  "processed",
  "historical_tournament_statistics_errors.csv"
)

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

#Read the tournament game index

tournament_brackets <- read_csv(
  input_file,
  show_col_types = FALSE
)

required_input_columns <- c(
  "season",
  "game_url"
)

missing_input_columns <- setdiff(
  required_input_columns,
  names(tournament_brackets)
)

if (length(missing_input_columns) > 0) {
  stop(
    "The input file is missing required columns: ",
    paste(missing_input_columns, collapse = ", ")
  )
}

if (anyDuplicated(tournament_brackets$game_url) > 0) {
  warning("Duplicate game URLs were found and removed.")
  
  tournament_brackets <- tournament_brackets |>
    distinct(game_url, .keep_all = TRUE)
}

#Helpful functions

# Convert a value to numeric without producing warning messages.
to_numeric <- function(x) {
  suppressWarnings(
    as.numeric(
      str_replace_all(
        as.character(x),
        "[^0-9.-]",
        ""
      )
    )
  )
}

# Remove team records from table captions.
clean_team_name <- function(team_name) {
  team_name |>
    str_squish() |>
    str_remove("\\s*\\([0-9]+-[0-9]+\\)\\s*$")
}

# Extract a statistic from a Team Totals row.
get_stat <- function(total_row, statistic) {
  
  cell <- total_row |>
    html_element(
      paste0(
        "[data-stat='",
        statistic,
        "']"
      )
    )
  
  if (length(cell) == 0 || is.na(cell)) {
    return(NA_real_)
  }
  
  to_numeric(
    html_text2(cell)
  )
}

# Extract and clean the team name from a box-score table caption.
get_team_name <- function(box_table) {
  
  caption <- box_table |>
    html_element("caption") |>
    html_text2()
  
  caption |>
    str_remove(
      regex(
        "\\s+(Basic Box Score Stats|Basic Box Score|Table).*$",
        ignore_case = TRUE
      )
    ) |>
    clean_team_name() |>
    str_squish()
}


# Extract one team's totals from the table footer.
extract_team_totals <- function(box_table) {
  
  team_name <- get_team_name(box_table)
  
  # The footer contains the team/school totals row.
  total_rows <- box_table |>
    html_elements("tfoot tr")
  
  if (length(total_rows) == 0) {
    stop(
      "No totals row was found for ",
      team_name,
      "."
    )
  }

  total_row <- total_rows[[length(total_rows)]]
  
  tibble(
    team = team_name,
    fg = get_stat(total_row, "fg"),
    fga = get_stat(total_row, "fga"),
    fg_pct = get_stat(total_row, "fg_pct"),
    fg2 = get_stat(total_row, "fg2"),
    fg2a = get_stat(total_row, "fg2a"),
    fg2_pct = get_stat(total_row, "fg2_pct"),
    fg3 = get_stat(total_row, "fg3"),
    fg3a = get_stat(total_row, "fg3a"),
    fg3_pct = get_stat(total_row, "fg3_pct"),
    ft = get_stat(total_row, "ft"),
    fta = get_stat(total_row, "fta"),
    ft_pct = get_stat(total_row, "ft_pct"),
    orb = get_stat(total_row, "orb"),
    drb = get_stat(total_row, "drb"),
    trb = get_stat(total_row, "trb"),
    ast = get_stat(total_row, "ast"),
    stl = get_stat(total_row, "stl"),
    blk = get_stat(total_row, "blk"),
    tov = get_stat(total_row, "tov"),
    pf = get_stat(total_row, "pf"),
    pts = get_stat(total_row, "pts")
  )
}

# Add a prefix such as team1_ or team2_ to each statistic.
prefix_team_columns <- function(team_data, prefix) {
  
  team_data |>
    rename_with(
      ~ paste0(prefix, .x)
    )
}

# NCAA possession estimate for a single team.
estimate_team_possessions <- function(
    fga,
    orb,
    tov,
    fta
) {
  fga - orb + tov + (0.475 * fta)
}

# Safely divide two values.
safe_divide <- function(numerator, denominator) {
  
  ifelse(
    is.na(denominator) | denominator == 0,
    NA_real_,
    numerator / denominator
  )
}

#Scrape one tournament game
collect_game_statistics <- function(
    season,
    game_url
) {
  
  message(
    "Collecting: ",
    season,
    " | ",
    game_url
  )
  
  response <- request(game_url) |>
    req_user_agent(
      paste(
        "DATA367 sports analytics project",
        "maddiegrossnicklaus@gmail.com"
      )
    ) |>
    req_timeout(seconds = 30) |>
    req_retry(
      max_tries = 3,
      backoff = ~ 5
    ) |>
    req_perform()
  
  if (resp_status(response) != 200) {
    stop(
      "Request failed. HTTP status: ",
      resp_status(response)
    )
  }
  
  page_text <- resp_body_string(response)
  
  # Sports Reference sometimes stores tables inside HTML comments.
  page_text <- page_text |>
    str_replace_all("<!--", "") |>
    str_replace_all("-->", "")
  
  game_page <- read_html(page_text)
  
  basic_tables <- game_page |>
    html_elements(
      "table[id^='box-score-basic-']"
    )
  
  # Remove accidental duplicate table selections.
  table_ids <- basic_tables |>
    html_attr("id")
  
  basic_tables <- basic_tables[
    !duplicated(table_ids)
  ]
  
  if (length(basic_tables) != 2) {
    stop(
      "Expected two basic team box-score tables, but found ",
      length(basic_tables),
      "."
    )
  }
  
  team1 <- extract_team_totals(
    basic_tables[[1]]
  )
  
  team2 <- extract_team_totals(
    basic_tables[[2]]
  )
  
  if (
    is.na(team1$pts) ||
    is.na(team2$pts)
  ) {
    stop("One or both team scores are missing.")
  }
  
  if (team1$pts == team2$pts) {
    stop("The final game score is tied.")
  }
  
  team1 <- prefix_team_columns(
    team1,
    "team1_"
  )
  
  team2 <- prefix_team_columns(
    team2,
    "team2_"
  )
  
  game_id <- game_url |>
    basename() |>
    str_remove("\\.html$")
  
  game_date <- game_id |>
    str_extract(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}"
    ) |>
    as.Date()
  
  game_data <- bind_cols(
    tibble(
      season = season,
      game_id = game_id,
      game_date = game_date,
      game_url = game_url
    ),
    team1,
    team2
  ) |>
    mutate(
      team1_win = as.integer(
        team1_pts > team2_pts
      ),
      
      team2_win = as.integer(
        team2_pts > team1_pts
      ),
      
      winner = if_else(
        team1_win == 1,
        team1_team,
        team2_team
      ),
      
      loser = if_else(
        team1_win == 1,
        team2_team,
        team1_team
      ),
      
      score_margin = abs(
        team1_pts - team2_pts
      ),
      
      team1_point_margin =
        team1_pts - team2_pts,
      
      team2_point_margin =
        team2_pts - team1_pts
    )
  
#Estimate possessions
game_data <- game_data |>
  mutate(
    team1_raw_possessions =
      estimate_team_possessions(
        team1_fga,
        team1_orb,
        team1_tov,
        team1_fta
      ),
      
    team2_raw_possessions =
      estimate_team_possessions(
        team2_fga,
        team2_orb,
        team2_tov,
        team2_fta
      ),
      
    estimated_possessions =
      (
        team1_raw_possessions +
          team2_raw_possessions
      ) / 2
  )

#Offensive and defensive efficiency
game_data <- game_data |>
  mutate(
    team1_offensive_rating =
      100 * safe_divide(
        team1_pts,
        estimated_possessions
      ),
    
    team2_offensive_rating =
      100 * safe_divide(
        team2_pts,
        estimated_possessions
      ),
    
    team1_defensive_rating =
      100 * safe_divide(
        team2_pts,
        estimated_possessions
      ),
    
    team2_defensive_rating =
      100 * safe_divide(
        team1_pts,
        estimated_possessions
      )
  )

#Shooting metrics
game_data <- game_data |>
  mutate(
    team1_efg_pct =
      safe_divide(
        team1_fg + (0.5 * team1_fg3),
        team1_fga
      ),
    
    team2_efg_pct =
      safe_divide(
        team2_fg + (0.5 * team2_fg3),
        team2_fga
      ),
    
    team1_true_shooting_pct =
      safe_divide(
        team1_pts,
        2 * (
          team1_fga +
            (0.475 * team1_fta)
        )
      ),
    
    team2_true_shooting_pct =
      safe_divide(
        team2_pts,
        2 * (
          team2_fga +
            (0.475 * team2_fta)
        )
      ),
    
    team1_ft_rate =
      safe_divide(
        team1_fta,
        team1_fga
      ),
    
    team2_ft_rate =
      safe_divide(
        team2_fta,
        team2_fga
      ),
    
    team1_fg3_attempt_rate =
      safe_divide(
        team1_fg3a,
        team1_fga
      ),
    
    team2_fg3_attempt_rate =
      safe_divide(
        team2_fg3a,
        team2_fga
      )
  )

#Turnover metrics
game_data <- game_data |>
  mutate(
    team1_tov_pct =
      safe_divide(
        team1_tov,
        team1_fga +
          (0.475 * team1_fta) +
          team1_tov
      ),
    
    team2_tov_pct =
      safe_divide(
        team2_tov,
        team2_fga +
          (0.475 * team2_fta) +
          team2_tov
      ),
    
    team1_forced_tov_pct =
      team2_tov_pct,
    
    team2_forced_tov_pct =
      team1_tov_pct
  )

#Rebounding metrics
game_data <- game_data |>
  mutate(
    team1_orb_pct =
      safe_divide(
        team1_orb,
        team1_orb + team2_drb
      ),
    
    team2_orb_pct =
      safe_divide(
        team2_orb,
        team2_orb + team1_drb
      ),
    
    team1_drb_pct =
      safe_divide(
        team1_drb,
        team1_drb + team2_orb
      ),
    
    team2_drb_pct =
      safe_divide(
        team2_drb,
        team2_drb + team1_orb
      )
  )

#Defensive opponent metrics
game_data <- game_data |>
  mutate(
    team1_opponent_fg_pct =
      team2_fg_pct,
    
    team2_opponent_fg_pct =
      team1_fg_pct,
    
    team1_opponent_fg2_pct =
      team2_fg2_pct,
    
    team2_opponent_fg2_pct =
      team1_fg2_pct,
    
    team1_opponent_fg3_pct =
      team2_fg3_pct,
    
    team2_opponent_fg3_pct =
      team1_fg3_pct,
    
    team1_opponent_efg_pct =
      team2_efg_pct,
    
    team2_opponent_efg_pct =
      team1_efg_pct,
    
    team1_opponent_ft_rate =
      team2_ft_rate,
    
    team2_opponent_ft_rate =
      team1_ft_rate,
    
    team1_points_allowed =
      team2_pts,
    
    team2_points_allowed =
      team1_pts
  )

game_data
}

#Collect all tournament games
game_results <- vector(
  mode = "list",
  length = nrow(tournament_brackets)
)

collection_errors <- vector(
  mode = "list",
  length = nrow(tournament_brackets)
)

for (i in seq_len(nrow(tournament_brackets))) {
  
  current_season <-
    tournament_brackets$season[[i]]
  
  current_url <-
    tournament_brackets$game_url[[i]]
  
  result <- tryCatch(
    {
      collect_game_statistics(
        season = current_season,
        game_url = current_url
      )
    },
    error = function(error) {
      
      message(
        "ERROR: ",
        current_url,
        " | ",
        conditionMessage(error)
      )
      
      collection_errors[[i]] <<- tibble(
        season = current_season,
        game_url = current_url,
        error_message = conditionMessage(error)
      )
      
      NULL
    }
  )
  
  game_results[[i]] <- result
  
  # Be respectful of Sports Reference request limits.
  if (i < nrow(tournament_brackets)) {
    Sys.sleep(7)
  }
}

historical_tournament_statistics <- game_results |>
  compact() |>
  bind_rows() |>
  arrange(
    season,
    game_date,
    game_id
  )

collection_errors <- collection_errors |>
  compact() |>
  bind_rows()

#Validation
expected_games <- nrow(
  tournament_brackets
)

collected_games <- nrow(
  historical_tournament_statistics
)

message(
  "Expected games: ",
  expected_games
)

message(
  "Successfully collected games: ",
  collected_games
)

message(
  "Failed games: ",
  nrow(collection_errors)
)

if (
  anyDuplicated(
    historical_tournament_statistics$game_url
  ) > 0
) {
  warning(
    "Duplicate game URLs exist in the processed dataset."
  )
}

if (
  any(
    historical_tournament_statistics$team1_team ==
    historical_tournament_statistics$team2_team
  )
) {
  warning(
    "At least one game lists the same team as team1 and team2."
  )
}

if (
  any(
    historical_tournament_statistics$team1_pts ==
    historical_tournament_statistics$team2_pts
  )
) {
  warning(
    "At least one game has a tied final score."
  )
}

if (
  any(
    !historical_tournament_statistics$team1_win %in%
    c(0, 1)
  )
) {
  warning(
    "At least one team1_win value is not zero or one."
  )
}

games_per_season <- historical_tournament_statistics |>
  count(
    season,
    name = "collected_games"
  )

expected_games_per_season <- tibble(
  season = c(
    2012,
    2013,
    2014,
    2015,
    2016,
    2017,
    2018,
    2019,
    2021,
    2022
  ),
  expected_games = c(
    67,
    67,
    67,
    67,
    67,
    67,
    67,
    67,
    66,
    67
  )
)

season_validation <- expected_games_per_season |>
  left_join(
    games_per_season,
    by = "season"
  ) |>
  mutate(
    collected_games = coalesce(
      collected_games,
      0L
    ),
    passed =
      collected_games == expected_games
  )

print(season_validation)

#Save results

write_csv(
  historical_tournament_statistics,
  output_file,
  na = ""
)

if (nrow(collection_errors) > 0) {
  
  write_csv(
    collection_errors,
    error_file,
    na = ""
  )
  
  warning(
    nrow(collection_errors),
    " games failed. Error details were saved to: ",
    error_file
  )
  
} else {
  
  if (file.exists(error_file)) {
    file.remove(error_file)
  }
  
  message(
    "All tournament games were collected successfully."
  )
}

if (collected_games != 669) {
  warning(
    "Expected 669 games, but the output contains ",
    collected_games,
    "."
  )
} else {
  message(
    "The dataset contains all 669 expected tournament games."
  )
}

message(
  "Finished. Historical tournament statistics saved to: ",
  output_file
)
  

