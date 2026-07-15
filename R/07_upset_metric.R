library(httr2)
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(lubridate)
library(here)

#Filepaths
tournament_season <- 2025

sports_reference_root <-
  "https://www.sports-reference.com"

tournament_page_url <- paste0(
  sports_reference_root,
  "/cbb/postseason/men/",
  tournament_season,
  "-ncaa.html"
)

bracket_path <- here(
  "data",
  "raw",
  "2025_tournament_bracket.csv"
)

first_four_path <- here(
  "data",
  "raw",
  "2025_first_four_games.csv"
)

round_of_64_path <- here(
  "data",
  "raw",
  "2025_round_of_64_games.csv"
)

game_url_path <- here(
  "data",
  "raw",
  "2025_tournament_game_urls.csv"
)

error_path <- here(
  "data",
  "raw",
  "2025_bracket_collection_errors.csv"
)

dir.create(
  here("data", "raw"),
  recursive = TRUE,
  showWarnings = FALSE
)

#Helper functions
read_sports_reference_page <- function(url) {
  
  response <- request(url) |>
    req_user_agent(
      paste(
        "March Madness analytics project",
        "maddiegrossnicklaus@gmail.com"
      )
    ) |>
    req_timeout(
      seconds = 30
    ) |>
    req_retry(
      max_tries = 3,
      backoff = ~ 5
    ) |>
    req_perform()
  
  if (resp_status(response) != 200) {
    stop(
      "Request failed with HTTP status ",
      resp_status(response),
      "."
    )
  }
  
  page_text <- resp_body_string(
    response
  ) |>
    str_replace_all(
      "<!--",
      ""
    ) |>
    str_replace_all(
      "-->",
      ""
    )
  
  read_html(page_text)
}

#Clean team names

clean_team_name <- function(team_name) {
  
  team_name |>
    str_squish() |>
    str_remove(
      "\\s*\\([0-9]+-[0-9]+\\)\\s*$"
    )
}


#Extract two teams

extract_game_teams <- function(game_url) {
  
  game_page <- read_sports_reference_page(
    game_url
  )
  
  basic_tables <- game_page |>
    html_elements(
      "table[id^='box-score-basic-']"
    )
  
  table_ids <- basic_tables |>
    html_attr("id")
  
  basic_tables <- basic_tables[
    !duplicated(table_ids)
  ]
  
  if (length(basic_tables) != 2) {
    stop(
      "Expected two box-score tables, but found ",
      length(basic_tables),
      "."
    )
  }
  
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
      clean_team_name()
  }
  
  team1 <- get_team_name(
    basic_tables[[1]]
  )
  
  team2 <- get_team_name(
    basic_tables[[2]]
  )
  
  game_id <- game_url |>
    basename() |>
    str_remove("\\.html$")
  
  game_date <- game_id |>
    str_extract(
      "^\\d{4}-\\d{2}-\\d{2}"
    ) |>
    ymd()
  
  tibble(
    game_id = game_id,
    game_date = game_date,
    game_url = game_url,
    team1 = team1,
    team2 = team2
  )
}

#Collect all 2025 game URLs

message(
  "Collecting 2025 NCAA Tournament game URLs..."
)

tournament_page <- read_sports_reference_page(
  tournament_page_url
)

tournament_game_urls <- tournament_page |>
  html_elements("a") |>
  html_attr("href") |>
  discard(is.na) |>
  keep(
    ~ str_detect(
      .x,
      "^/cbb/boxscores/2025-[0-9]{2}-[0-9]{2}-.+\\.html$"
    )
  ) |>
  unique() |>
  tibble(
    relative_url = _
  ) |>
  mutate(
    game_url = paste0(
      sports_reference_root,
      relative_url
    ),
    
    game_id = str_extract(
      relative_url,
      "(?<=/boxscores/).+(?=\\.html)"
    ),
    
    game_date = ymd(
      str_extract(
        game_id,
        "^\\d{4}-\\d{2}-\\d{2}"
      )
    )
  ) |>
  select(
    game_id,
    game_date,
    game_url
  ) |>
  arrange(
    game_date,
    game_id
  )

if (nrow(tournament_game_urls) != 67) {
  stop(
    "Expected 67 tournament game URLs, but found ",
    nrow(tournament_game_urls),
    "."
  )
}

write_csv(
  tournament_game_urls,
  game_url_path,
  na = ""
)

#First Four and Round of 64 URLs

opening_round_urls <- tournament_game_urls |>
  filter(
    game_date >= as.Date("2025-03-18"),
    game_date <= as.Date("2025-03-21")
  )

if (nrow(opening_round_urls) != 36) {
  stop(
    "Expected 36 opening-round games ",
    "(4 First Four plus 32 Round of 64), but found ",
    nrow(opening_round_urls),
    "."
  )
}


#Read participating teams

opening_game_results <- vector(
  mode = "list",
  length = nrow(opening_round_urls)
)

collection_errors <- vector(
  mode = "list",
  length = nrow(opening_round_urls)
)

for (i in seq_len(nrow(opening_round_urls))) {
  
  message(
    "Collecting opening-round game ",
    i,
    " of ",
    nrow(opening_round_urls),
    ": ",
    opening_round_urls$game_url[[i]]
  )
  
  current_url <-
    opening_round_urls$game_url[[i]]
  
  result <- tryCatch(
    {
      extract_game_teams(
        current_url
      )
    },
    error = function(error) {
      
      collection_errors[[i]] <<- tibble(
        game_url = current_url,
        error_message = conditionMessage(error)
      )
      
      NULL
    }
  )
  opening_game_results[[i]] <- result
  
  if (i < nrow(opening_round_urls)) {
    Sys.sleep(7)
  }
}

opening_games <- opening_game_results |>
  compact() |>
  bind_rows() |>
  arrange(
    game_date,
    game_id
  )

collection_errors <- collection_errors |>
  compact() |>
  bind_rows()

if (nrow(collection_errors) > 0) {
  
  write_csv(
    collection_errors,
    error_path,
    na = ""
  )
  
  stop(
    nrow(collection_errors),
    " opening-round games failed to load."
  )
}

if (nrow(opening_games) != 36) {
  stop(
    "Expected 36 opening-round matchups, but collected ",
    nrow(opening_games),
    "."
  )
}

#Define First Four structure

first_four_definition <- tribble(
  ~game_id,       ~region,    ~seed, ~team1,              ~team2,             ~target_opponent,
  "FF_01",        "South",       16, "Alabama State",     "Saint Francis (PA)",    "Auburn",
  "FF_02",        "East",        16, "American",          "Mount St. Mary's", "Duke",
  "FF_03",        "Midwest",     11, "Texas",             "Xavier",           "Illinois",
  "FF_04",        "South",       11, "San Diego State",   "North Carolina",   "Mississippi"
) |>
  mutate(
    round = "First Four",
    
    winner_placeholder = paste0(
      "Winner of ",
      game_id
    )
  )

#MAtch first four games to definitions
normalize_name <- function(x) {
  
  x |>
    str_to_lower() |>
    
    # Remove qualifiers such as "(PA)", "(CA)", and "(FL)".
    str_remove_all(
      "\\s*\\([^)]*\\)"
    ) |>
    
    str_replace_all(
      "&",
      "and"
    ) |>
    
    str_replace_all(
      "[^a-z0-9 ]",
      " "
    ) |>
    
    str_squish()
}

opening_games <- opening_games |>
  mutate(
    team1_normalized =
      normalize_name(team1),
    
    team2_normalized =
      normalize_name(team2),
    
    matchup_key = map2_chr(
      team1_normalized,
      team2_normalized,
      ~ paste(
        sort(c(.x, .y)),
        collapse = " | "
      )
    )
  )


first_four_definition <- first_four_definition |>
  mutate(
    matchup_key = map2_chr(
      normalize_name(team1),
      normalize_name(team2),
      ~ paste(
        sort(c(.x, .y)),
        collapse = " | "
      )
    )
  )


first_four_games <- first_four_definition |>
  left_join(
    opening_games |>
      select(
        matchup_key,
        scraped_game_id = game_id,
        game_date,
        game_url
      ),
    by = "matchup_key"
  ) |>
  select(
    game_id,
    region,
    round,
    seed,
    team1,
    team2,
    target_opponent,
    winner_placeholder,
    scraped_game_id,
    game_date,
    game_url
  )


if (
  any(
    is.na(
      first_four_games$game_url
    )
  )
) {
  print(
    first_four_games |>
      filter(
        is.na(game_url)
      )
  )
  
  stop(
    "At least one First Four matchup did not match the scraped games."
  )
}

write_csv(
  first_four_games,
  first_four_path,
  na = ""
)

#Identify Round of 64 Games
first_four_scraped_ids <-
  first_four_games$scraped_game_id

round_of_64_scraped <- opening_games |>
  filter(
    !game_id %in%
      first_four_scraped_ids
  ) |>
  select(
    game_id,
    game_date,
    game_url,
    team1,
    team2
  )

if (nrow(round_of_64_scraped) != 32) {
  stop(
    "Expected 32 Round-of-64 games, but found ",
    nrow(round_of_64_scraped),
    "."
  )
}

#Replace First Four winners with neutral placeholders

round_of_64_games <- round_of_64_scraped

for (i in seq_len(nrow(first_four_games))) {
  
  target_opponent <-
    first_four_games$target_opponent[[i]]
  
  placeholder <-
    first_four_games$winner_placeholder[[i]]
  
  first_four_teams <- c(
    first_four_games$team1[[i]],
    first_four_games$team2[[i]]
  )
  
  matching_game <- which(
    round_of_64_games$team1 == target_opponent |
      round_of_64_games$team2 == target_opponent
  )
  
  if (length(matching_game) != 1) {
    stop(
      "Could not uniquely locate the Round-of-64 game for ",
      target_opponent,
      "."
    )
  }
  
  row_number <- matching_game[[1]]
  
  if (
    round_of_64_games$team1[[row_number]] ==
    target_opponent
  ) {
    
    actual_other_team <-
      round_of_64_games$team2[[row_number]]
    
    if (
      !actual_other_team %in%
      first_four_teams
    ) {
      stop(
        "Unexpected opponent in the ",
        target_opponent,
        " Round-of-64 game: ",
        actual_other_team
      )
    }
    
    round_of_64_games$team2[[row_number]] <-
      placeholder
    
  } else {
    
    actual_other_team <-
      round_of_64_games$team1[[row_number]]
    
    if (
      !actual_other_team %in%
      first_four_teams
    ) {
      stop(
        "Unexpected opponent in the ",
        target_opponent,
        " Round-of-64 game: ",
        actual_other_team
      )
    }
    
    round_of_64_games$team1[[row_number]] <-
      placeholder
  }
}

#Add region and seed information

team_seed_lookup <- tribble(
  ~team,                    ~seed, ~region,
  
  # South
  "Auburn",                    1,  "South",
  "Louisville",                8,  "South",
  "Creighton",                 9,  "South",
  "Michigan",                  5,  "South",
  "UC San Diego",             12,  "South",
  "Texas A&M",                 4,  "South",
  "Yale",                     13,  "South",
  "Mississippi",                  6,  "South",
  "Winner of FF_04",          11,  "South",
  "Iowa State",                3,  "South",
  "Lipscomb",                 14,  "South",
  "Marquette",                 7,  "South",
  "New Mexico",               10,  "South",
  "Michigan State",            2,  "South",
  "Bryant",                   15,  "South",
  "Winner of FF_01",          16,  "South",
  
  # East
  "Duke",                      1,  "East",
  "Mississippi State",         8,  "East",
  "Baylor",                    9,  "East",
  "Oregon",                    5,  "East",
  "Liberty",                  12,  "East",
  "Arizona",                   4,  "East",
  "Akron",                    13,  "East",
  "Brigham Young",            6,  "East",
  "Virginia Commonwealth",    11,  "East",
  "Wisconsin",                 3,  "East",
  "Montana",                  14,  "East",
  "Saint Mary's",              7,  "East",
  "Vanderbilt",               10,  "East",
  "Alabama",                   2,  "East",
  "Robert Morris",            15,  "East",
  "Winner of FF_02",          16,  "East",
  
  # West
  "Florida",                   1,  "West",
  "Connecticut",               8,  "West",
  "Oklahoma",                  9,  "West",
  "Memphis",                   5,  "West",
  "Colorado State",           12,  "West",
  "Maryland",                  4,  "West",
  "Grand Canyon",             13,  "West",
  "Missouri",                  6,  "West",
  "Drake",                    11,  "West",
  "Texas Tech",                3,  "West",
  "UNC Wilmington",           14,  "West",
  "Kansas",                    7,  "West",
  "Arkansas",                 10,  "West",
  "St. John's (NY)",         2,  "West",
  "Omaha",                    15,  "West",
  "Norfolk State",            16,  "West",
  
  # Midwest
  "Houston",                   1,  "Midwest",
  "Gonzaga",                   8,  "Midwest",
  "Georgia",                   9,  "Midwest",
  "Clemson",                   5,  "Midwest",
  "McNeese",                  12,  "Midwest",
  "Purdue",                    4,  "Midwest",
  "High Point",               13,  "Midwest",
  "Illinois",                  6,  "Midwest",
  "Winner of FF_03",          11,  "Midwest",
  "Kentucky",                  3,  "Midwest",
  "Troy",                     14,  "Midwest",
  "UCLA",                      7,  "Midwest",
  "Utah State",               10,  "Midwest",
  "Tennessee",                 2,  "Midwest",
  "Wofford",                  15,  "Midwest",
  "Southern Illinois–Edwardsville",         16,  "Midwest"
)

#Attach seeds and regions

round_of_64_games <- round_of_64_games |>
  left_join(
    team_seed_lookup |>
      rename(
        team1 = team,
        team1_seed = seed,
        team1_region = region
      ),
    by = "team1"
  ) |>
  left_join(
    team_seed_lookup |>
      rename(
        team2 = team,
        team2_seed = seed,
        team2_region = region
      ),
    by = "team2"
  )


unmatched_round_of_64 <- round_of_64_games |>
  filter(
    is.na(team1_seed) |
      is.na(team2_seed)
  )

if (nrow(unmatched_round_of_64) > 0) {
  
  print(
    unmatched_round_of_64 |>
      select(
        team1,
        team2
      )
  )
  
  stop(
    "At least one Round-of-64 team did not match the seed lookup."
  )
}


if (
  any(
    round_of_64_games$team1_region !=
    round_of_64_games$team2_region
  )
) {
  stop(
    "At least one Round-of-64 matchup contains teams from different regions."
  )
}


round_of_64_games <- round_of_64_games |>
  mutate(
    region = team1_region,
    
    round = "Round of 64",
    
    seed_pair_low =
      pmin(
        team1_seed,
        team2_seed
      )
  ) |>
  select(
    region,
    round,
    game_id,
    game_date,
    game_url,
    team1,
    team1_seed,
    team2,
    team2_seed,
    seed_pair_low
  )


#Assign round of 64 IDs

seed_order <- c(
  1,
  8,
  5,
  4,
  6,
  3,
  7,
  2
)

round_of_64_games <- round_of_64_games |>
  mutate(
    regional_order = match(
      seed_pair_low,
      seed_order
    )
  ) |>
  arrange(
    factor(
      region,
      levels = c(
        "South",
        "East",
        "West",
        "Midwest"
      )
    ),
    regional_order
  ) |>
  group_by(region) |>
  mutate(
    regional_game_number =
      row_number(),
    
    bracket_game_id = paste0(
      str_sub(region, 1, 1),
      "_R64_",
      str_pad(
        regional_game_number,
        width = 2,
        pad = "0"
      )
    ),
    
    next_game_id = paste0(
      str_sub(region, 1, 1),
      "_R32_",
      str_pad(
        ceiling(
          regional_game_number / 2
        ),
        width = 2,
        pad = "0"
      )
    ),
    
    next_slot = if_else(
      regional_game_number %% 2 == 1,
      1L,
      2L
    )
  ) |>
  ungroup() |>
  select(
    region,
    round,
    bracket_game_id,
    team1,
    team1_seed,
    team2,
    team2_seed,
    next_game_id,
    next_slot,
    source_game_url = game_url
  )


write_csv(
  round_of_64_games,
  round_of_64_path,
  na = ""
)

#Add advancement link for first four games

first_four_games <- first_four_games |>
  rowwise() |>
  mutate(
    next_game_id = {
      
      target_row <- round_of_64_games |>
        filter(
          team1 == target_opponent |
            team2 == target_opponent
        )
      
      if (nrow(target_row) != 1) {
        stop(
          "Could not determine next game for ",
          game_id,
          "."
        )
      }
      
      target_row$bracket_game_id[[1]]
    },
    
    next_slot = {
      
      target_row <- round_of_64_games |>
        filter(
          team1 == target_opponent |
            team2 == target_opponent
        )
      
      if (
        target_row$team1[[1]] ==
        winner_placeholder
      ) {
        1L
      } else {
        2L
      }
    }
  ) |>
  ungroup() |>
  select(
    region,
    round,
    bracket_game_id = game_id,
    team1,
    team1_seed = seed,
    team2,
    team2_seed = seed,
    next_game_id,
    next_slot,
    source_game_url = game_url
  )


#Build empty later round bracket slots

build_regional_round <- function(
    region,
    round_name,
    round_code,
    number_of_games,
    next_round_code = NA_character_
) {
  
  tibble(
    region = region,
    round = round_name,
    
    bracket_game_id = paste0(
      str_sub(region, 1, 1),
      "_",
      round_code,
      "_",
      str_pad(
        seq_len(number_of_games),
        width = 2,
        pad = "0"
      )
    ),
    
    team1 = NA_character_,
    team1_seed = NA_integer_,
    team2 = NA_character_,
    team2_seed = NA_integer_,
    
    next_game_id = if (
      is.na(next_round_code)
    ) {
      NA_character_
    } else {
      paste0(
        str_sub(region, 1, 1),
        "_",
        next_round_code,
        "_",
        str_pad(
          ceiling(
            seq_len(number_of_games) / 2
          ),
          width = 2,
          pad = "0"
        )
      )
    },
    
    next_slot = if (
      is.na(next_round_code)
    ) {
      NA_integer_
    } else {
      if_else(
        seq_len(number_of_games) %% 2 == 1,
        1L,
        2L
      )
    },
    
    source_game_url = NA_character_
  )
}


regional_rounds <- bind_rows(
  map_dfr(
    c(
      "South",
      "East",
      "West",
      "Midwest"
    ),
    ~ build_regional_round(
      region = .x,
      round_name = "Round of 32",
      round_code = "R32",
      number_of_games = 4,
      next_round_code = "S16"
    )
  ),
  
  map_dfr(
    c(
      "South",
      "East",
      "West",
      "Midwest"
    ),
    ~ build_regional_round(
      region = .x,
      round_name = "Sweet 16",
      round_code = "S16",
      number_of_games = 2,
      next_round_code = "E8"
    )
  ),
  
  map_dfr(
    c(
      "South",
      "East",
      "West",
      "Midwest"
    ),
    ~ build_regional_round(
      region = .x,
      round_name = "Elite Eight",
      round_code = "E8",
      number_of_games = 1
    )
  )
)

#Connect Elite Eight WINNERS to Final Four

regional_rounds <- regional_rounds |>
  mutate(
    next_game_id = case_when(
      round == "Elite Eight" &
        region %in% c(
          "South",
          "West"
        ) ~
        "N_SF_01",
      
      round == "Elite Eight" &
        region %in% c(
          "East",
          "Midwest"
        ) ~
        "N_SF_02",
      
      TRUE ~
        next_game_id
    ),
    
    next_slot = case_when(
      round == "Elite Eight" &
        region == "South" ~
        1L,
      
      round == "Elite Eight" &
        region == "West" ~
        2L,
      
      round == "Elite Eight" &
        region == "East" ~
        1L,
      
      round == "Elite Eight" &
        region == "Midwest" ~
        2L,
      
      TRUE ~
        next_slot
    )
  )


national_rounds <- tibble(
  region = "National",
  
  round = c(
    "Final Four",
    "Final Four",
    "Championship"
  ),
  
  bracket_game_id = c(
    "N_SF_01",
    "N_SF_02",
    "N_F_01"
  ),
  
  team1 = NA_character_,
  team1_seed = NA_integer_,
  team2 = NA_character_,
  team2_seed = NA_integer_,
  
  next_game_id = c(
    "N_F_01",
    "N_F_01",
    NA_character_
  ),
  
  next_slot = c(
    1L,
    2L,
    NA_integer_
  ),
  
  source_game_url = NA_character_
)


#Combine Complete bracket

complete_bracket <- bind_rows(
  first_four_games,
  round_of_64_games,
  regional_rounds,
  national_rounds
)


round_order <- c(
  "First Four",
  "Round of 64",
  "Round of 32",
  "Sweet 16",
  "Elite Eight",
  "Final Four",
  "Championship"
)

complete_bracket <- complete_bracket |>
  mutate(
    round_number = match(
      round,
      round_order
    )
  ) |>
  arrange(
    round_number,
    factor(
      region,
      levels = c(
        "South",
        "East",
        "West",
        "Midwest",
        "National"
      )
    ),
    bracket_game_id
  ) |>
  select(
    round_number,
    region,
    round,
    bracket_game_id,
    team1,
    team1_seed,
    team2,
    team2_seed,
    next_game_id,
    next_slot,
    source_game_url
  )


#Validate braket topology
expected_game_counts <- tibble(
  round = round_order,
  
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


actual_game_counts <- complete_bracket |>
  count(
    round,
    name = "actual_games"
  ) |>
  right_join(
    expected_game_counts,
    by = "round"
  ) |>
  mutate(
    passed =
      actual_games ==
      expected_games
  )


print(
  actual_game_counts
)


if (!all(actual_game_counts$passed)) {
  stop(
    "The complete bracket has incorrect round counts."
  )
}


if (
  anyDuplicated(
    complete_bracket$bracket_game_id
  ) > 0
) {
  stop(
    "Duplicate bracket game IDs were created."
  )
}


valid_game_ids <-
  complete_bracket$bracket_game_id

invalid_next_games <- complete_bracket |>
  filter(
    !is.na(next_game_id),
    !next_game_id %in%
      valid_game_ids
  )

if (nrow(invalid_next_games) > 0) {
  
  print(
    invalid_next_games
  )
  
  stop(
    "At least one next_game_id does not exist in the bracket."
  )
}

#Save complete bracket

write_csv(
  complete_bracket,
  bracket_path,
  na = ""
)


message(
  "Bracket collection complete."
)

message(
  "First Four file: ",
  first_four_path
)

message(
  "Round-of-64 file: ",
  round_of_64_path
)

message(
  "Complete bracket file: ",
  bracket_path
)


print(
  complete_bracket |>
    filter(
      round %in%
        c(
          "First Four",
          "Round of 64"
        )
    ) |>
    select(
      region,
      round,
      bracket_game_id,
      team1,
      team1_seed,
      team2,
      team2_seed,
      next_game_id,
      next_slot
    )
)

#Assign 2025 championship scores to teams in bracket, fom,, script 06_championship_score.R

championship_scores_path <- here(
  "data",
  "final",
  "2025_championship_scores.csv"
)

if (!file.exists(championship_scores_path)) {
  stop(
    "The 2025 Championship Score file was not found at: ",
    championship_scores_path
  )
}

championship_scores_2025 <- read_csv(
  championship_scores_path,
  show_col_types = FALSE
)

# Match Championship Score names to the bracket's Sports Reference names

championship_scores_2025 <- championship_scores_2025 |>
  mutate(
    team = case_when(
      team == "VCU" ~
        "Virginia Commonwealth",
      
      team == "SIU Edwardsville" ~
        "Southern Illinois–Edwardsville",
      
      team == "UNC" ~
        "North Carolina",
      
      team == "Ole Miss" ~
        "Mississippi",
      
      team == "BYU" ~
        "Brigham Young",
      
      team == "UConn" ~
        "Connecticut",
      
      TRUE ~
        team
    )
  )

#Validate championship score columns

required_score_columns <- c(
  "team",
  "championship_rank",
  "championship_score",
  "adjusted_finish_score"
)

missing_score_columns <- setdiff(
  required_score_columns,
  names(championship_scores_2025)
)

if (length(missing_score_columns) > 0) {
  stop(
    "2025_championship_scores.csv is missing these columns: ",
    paste(
      missing_score_columns,
      collapse = ", "
    )
  )
}

if (nrow(championship_scores_2025) != 68) {
  stop(
    "Expected 68 Championship Score rows, but found ",
    nrow(championship_scores_2025),
    "."
  )
}

if (
  anyDuplicated(
    championship_scores_2025$team
  ) > 0
) {
  stop(
    "Duplicate team names were found in the Championship Score file."
  )
}

#Create team1 championship score lookup

team1_score_lookup <- championship_scores_2025 |>
  select(
    team1 = team,
    
    team1_championship_rank =
      championship_rank,
    
    team1_championship_score =
      championship_score,
    
    team1_adjusted_finish_score =
      adjusted_finish_score
  )

#Create team2 championship score lookup

team2_score_lookup <- championship_scores_2025 |>
  select(
    team2 = team,
    
    team2_championship_rank =
      championship_rank,
    
    team2_championship_score =
      championship_score,
    
    team2_adjusted_finish_score =
      adjusted_finish_score
  )

#Attach rankings to all currently populated bracket spots

complete_bracket <- complete_bracket |>
  left_join(
    team1_score_lookup,
    by = "team1"
  ) |>
  left_join(
    team2_score_lookup,
    by = "team2"
  )

unmatched_team1 <- complete_bracket |>
  filter(
    !is.na(team1),
    !str_detect(
      team1,
      "^Winner of FF_"
    ),
    is.na(team1_championship_rank)
  ) |>
  transmute(
    bracket_game_id,
    bracket_team = team1
  )

unmatched_team2 <- complete_bracket |>
  filter(
    !is.na(team2),
    !str_detect(
      team2,
      "^Winner of FF_"
    ),
    is.na(team2_championship_rank)
  ) |>
  transmute(
    bracket_game_id,
    bracket_team = team2
  )

unmatched_bracket_teams <- bind_rows(
  unmatched_team1,
  unmatched_team2
) |>
  distinct()


if (nrow(unmatched_bracket_teams) > 0) {
  
  print(
    unmatched_bracket_teams
  )
  
  stop(
    nrow(unmatched_bracket_teams),
    " real bracket teams did not match the 2025 Championship Score file."
  )
}

#Determine fixed baseline favorite and underdog

complete_bracket <- complete_bracket |>
  mutate(
    baseline_favorite = case_when(
      !is.na(team1_adjusted_finish_score) &
        !is.na(team2_adjusted_finish_score) &
        team1_adjusted_finish_score >=
        team2_adjusted_finish_score ~
        team1,
      
      !is.na(team1_adjusted_finish_score) &
        !is.na(team2_adjusted_finish_score) ~
        team2,
      
      TRUE ~
        NA_character_
    ),
    
    baseline_underdog = case_when(
      !is.na(team1_adjusted_finish_score) &
        !is.na(team2_adjusted_finish_score) &
        team1_adjusted_finish_score >=
        team2_adjusted_finish_score ~
        team2,
      
      !is.na(team1_adjusted_finish_score) &
        !is.na(team2_adjusted_finish_score) ~
        team1,
      
      TRUE ~
        NA_character_
    ),
    
    baseline_favorite_score = case_when(
      baseline_favorite == team1 ~
        team1_adjusted_finish_score,
      
      baseline_favorite == team2 ~
        team2_adjusted_finish_score,
      
      TRUE ~
        NA_real_
    ),
    
    baseline_underdog_score = case_when(
      baseline_underdog == team1 ~
        team1_adjusted_finish_score,
      
      baseline_underdog == team2 ~
        team2_adjusted_finish_score,
      
      TRUE ~
        NA_real_
    ),
    
    baseline_quality_gap =
      baseline_favorite_score -
      baseline_underdog_score
  )

#Validate populoated opening round games

first_four_score_validation <- complete_bracket |>
  filter(
    round == "First Four"
  ) |>
  summarise(
    games = n(),
    
    games_with_two_scores = sum(
      !is.na(team1_adjusted_finish_score) &
        !is.na(team2_adjusted_finish_score)
    )
  )

if (
  first_four_score_validation$games != 4 ||
  first_four_score_validation$games_with_two_scores != 4
) {
  stop(
    "Not all four First Four games received two Championship Scores."
  )
}


round_of_64_score_validation <- complete_bracket |>
  filter(
    round == "Round of 64"
  ) |>
  summarise(
    games = n(),
    
    games_with_two_scores = sum(
      !is.na(team1_adjusted_finish_score) &
        !is.na(team2_adjusted_finish_score)
    ),
    
    games_waiting_for_first_four = sum(
      str_detect(
        coalesce(team1, ""),
        "^Winner of FF_"
      ) |
        str_detect(
          coalesce(team2, ""),
          "^Winner of FF_"
        )
    )
  )

print(
  first_four_score_validation
)

print(
  round_of_64_score_validation
)

#Save the ranked bracket

ranked_bracket_path <- here(
  "data",
  "raw",
  "2025_tournament_bracket_ranked.csv"
)

write_csv(
  complete_bracket,
  ranked_bracket_path,
  na = ""
)

message(
  "Fixed 2025 Championship Scores assigned to bracket teams."
)

message(
  "Ranked bracket saved to: ",
  ranked_bracket_path
)


# Display the currently complete matchups.
print(
  complete_bracket |>
    filter(
      round %in%
        c(
          "First Four",
          "Round of 64"
        )
    ) |>
    select(
      region,
      round,
      bracket_game_id,
      team1,
      team1_seed,
      team1_championship_rank,
      team1_adjusted_finish_score,
      team2,
      team2_seed,
      team2_championship_rank,
      team2_adjusted_finish_score,
      baseline_favorite,
      baseline_underdog,
      baseline_quality_gap
    )
)

#Upset metric filepaths

scored_team_features_path <- here(
  "data",
  "final",
  "2025_championship_score_features.csv"
)

upset_predictions_path <- here(
  "data",
  "final",
  "2025_upset_matchup_predictions.csv"
)

predicted_bracket_path <- here(
  "data",
  "final",
  "2025_predicted_bracket.csv"
)

#Read team statistics and championship scores

if (!file.exists(scored_team_features_path)) {
  stop(
    "The scored 2025 team-feature file was not found at: ",
    scored_team_features_path
  )
}

team_profiles_2025 <- read_csv(
  scored_team_features_path,
  show_col_types = FALSE
)

# Match team-profile names to the bracket's Sports Reference names

team_profiles_2025 <- team_profiles_2025 |>
  mutate(
    team = case_when(
      team == "VCU" ~
        "Virginia Commonwealth",
      
      team == "SIU Edwardsville" ~
        "Southern Illinois–Edwardsville",
      
      team == "UNC" ~
        "North Carolina",
      
      team == "Ole Miss" ~
        "Mississippi",
      
      team == "BYU" ~
        "Brigham Young",
      
      team == "UConn" ~
        "Connecticut",
      
      TRUE ~
        team
    )
  )

required_profile_columns <- c(
  "team",
  "championship_rank",
  "adjusted_finish_score",
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

missing_profile_columns <- setdiff(
  required_profile_columns,
  names(team_profiles_2025)
)

if (length(missing_profile_columns) > 0) {
  stop(
    "2025_championship_score_features.csv is missing these columns: ",
    paste(
      missing_profile_columns,
      collapse = ", "
    )
  )
}

if (nrow(team_profiles_2025) != 68) {
  stop(
    "Expected 68 team profiles, but found ",
    nrow(team_profiles_2025),
    "."
  )
}

if (
  anyDuplicated(
    team_profiles_2025$team
  ) > 0
) {
  stop(
    "Duplicate teams were found in the 2025 team-profile file."
  )
}

#Safe division
safe_divide_upset <- function(
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

#Standardization

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
      "A matchup feature has zero or invalid standard deviation."
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

#Calculate matchip statistic for every 2025 tournament team

team_profiles_2025 <- team_profiles_2025 |>
  mutate(
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
    
    estimated_possessions =
      (
        team_raw_possessions +
          opponent_raw_possessions
      ) / 2,
    
    offensive_rating =
      100 * safe_divide_upset(
        team_game_score,
        estimated_possessions
      ),
    
    defensive_rating =
      100 * safe_divide_upset(
        opp_team_game_score,
        estimated_possessions
      ),
    
    efg_pct =
      safe_divide_upset(
        fg +
          (0.5 * fg3),
        fga
      ),
    
    opponent_efg_pct =
      safe_divide_upset(
        opp_fg +
          (0.5 * opp_fg3),
        opp_fga
      ),
    
    fg3_attempt_rate =
      safe_divide_upset(
        fg3a,
        fga
      ),
    
    opponent_fg3_attempt_rate =
      safe_divide_upset(
        opp_fg3a,
        opp_fga
      ),
    
    tov_pct =
      safe_divide_upset(
        tov,
        fga +
          (0.475 * fta) +
          tov
      ),
    
    forced_tov_pct =
      safe_divide_upset(
        opp_tov,
        opp_fga +
          (0.475 * opp_fta) +
          opp_tov
      ),
    
    orb_pct =
      safe_divide_upset(
        orb,
        orb +
          opp_drb
      ),
    
    drb_pct =
      safe_divide_upset(
        drb,
        drb +
          opp_orb
      )
  )

#Standardize matchup stat


team_profiles_2025 <- team_profiles_2025 |>
  mutate(
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
    
    # Lower defensive rating is better.
    defensive_rating_strength =
      -z_defensive_rating,
    
    # Lower opponent shooting is better.
    opponent_efg_defense_strength =
      -z_opponent_efg_pct,
    
    # Higher opponent 3P% represents weaker perimeter defense.
    perimeter_defense_weakness =
      z_opponent_fg3_pct,
    
    # Higher turnover percentage represents weaker ball security.
    ball_security_weakness =
      z_tov_pct,
    
    # Low defensive rebounding represents a weakness.
    defensive_rebounding_weakness =
      -z_drb_pct,
    
    # Low offensive rating represents an offensive weakness.
    offensive_weakness =
      -z_offensive_rating
  )


standardized_columns <- team_profiles_2025 |>
  select(
    z_adjusted_finish_score,
    z_offensive_rating,
    z_defensive_rating,
    z_efg_pct,
    z_opponent_efg_pct,
    z_fg3_pct,
    z_fg3_attempt_rate,
    z_opponent_fg3_pct,
    z_tov_pct,
    z_forced_tov_pct,
    z_orb_pct,
    z_drb_pct
  ) |>
  as.matrix()

if (any(!is.finite(standardized_columns))) {
  stop(
    "The standardized matchup-feature table contains invalid values."
  )
}

#Upset metric settings

three_point_weight <- 0.30
turnover_weight <- 0.25
rebounding_weight <- 0.25
defensive_resistance_weight <- 0.20

quality_gap_weight <- 0.65
upset_threshold <- 0.35


if (
  abs(
    three_point_weight +
    turnover_weight +
    rebounding_weight +
    defensive_resistance_weight -
    1
  ) > 1e-8
) {
  stop(
    "The matchup-component weights must sum to 1."
  )
}

#Predict one matchup

predict_upset_matchup <- function(
    team1,
    team2,
    team1_seed,
    team2_seed,
    round_name,
    region,
    bracket_game_id
) {
  
  team1_profile <- team_profiles_2025 |>
    filter(
      team == team1
    )
  
  team2_profile <- team_profiles_2025 |>
    filter(
      team == team2
    )
  
  if (nrow(team1_profile) != 1) {
    stop(
      "Could not uniquely locate the team profile for ",
      team1,
      "."
    )
  }
  
  if (nrow(team2_profile) != 1) {
    stop(
      "Could not uniquely locate the team profile for ",
      team2,
      "."
    )
  }
  
#Determine underdog and favorite
  
  team1_is_favorite <- case_when(
    team1_profile$adjusted_finish_score >
      team2_profile$adjusted_finish_score ~
      TRUE,
    
    team1_profile$adjusted_finish_score <
      team2_profile$adjusted_finish_score ~
      FALSE,
    
    !is.na(team1_seed) &
      !is.na(team2_seed) &
      team1_seed <
      team2_seed ~
      TRUE,
    
    !is.na(team1_seed) &
      !is.na(team2_seed) &
      team1_seed >
      team2_seed ~
      FALSE,
    
    TRUE ~
      team1_profile$championship_rank <
      team2_profile$championship_rank
  )
  
  
  if (team1_is_favorite) {
    
    favorite <- team1_profile
    underdog <- team2_profile
    
    favorite_seed <- team1_seed
    underdog_seed <- team2_seed
    
  } else {
    
    favorite <- team2_profile
    underdog <- team1_profile
    
    favorite_seed <- team2_seed
    underdog_seed <- team1_seed
  }
  
  
#^Three-point edge
  underdog_three_point_strength <-
    (
      0.60 *
        underdog$z_fg3_pct
    ) +
    (
      0.40 *
        underdog$z_fg3_attempt_rate
    )
  
  favorite_perimeter_weakness <-
    favorite$perimeter_defense_weakness
  
  three_point_edge <-
    (
      0.65 *
        underdog_three_point_strength
    ) +
    (
      0.35 *
        favorite_perimeter_weakness
    )
  
#Turnover pressure edge
  
  turnover_pressure_edge <-
    (
      0.60 *
        underdog$z_forced_tov_pct
    ) +
    (
      0.40 *
        favorite$ball_security_weakness
    )
  
#Rebounding edge
  
  offensive_rebounding_edge <-
    (
      0.65 *
        underdog$z_orb_pct
    ) +
    (
      0.35 *
        favorite$defensive_rebounding_weakness
    )
  
  defensive_rebounding_edge <-
    (
      0.70 *
        underdog$z_drb_pct
    ) -
    (
      0.30 *
        favorite$z_orb_pct
    )
  
  rebounding_edge <-
    (
      offensive_rebounding_edge +
        defensive_rebounding_edge
    ) / 2
  
#Defensive-resistence edge
  
  underdog_defensive_strength <-
    (
      0.55 *
        underdog$defensive_rating_strength
    ) +
    (
      0.45 *
        underdog$opponent_efg_defense_strength
    )
  
  defensive_resistance_edge <-
    (
      0.80 *
        underdog_defensive_strength
    ) +
    (
      0.20 *
        favorite$offensive_weakness
    )
  
#Combine matchup specific advantages
  
  matchup_advantage <-
    (
      three_point_weight *
        three_point_edge
    ) +
    (
      turnover_weight *
        turnover_pressure_edge
    ) +
    (
      rebounding_weight *
        rebounding_edge
    ) +
    (
      defensive_resistance_weight *
        defensive_resistance_edge
    )
  
#Favorite quality advantage
  quality_gap <-
    favorite$z_adjusted_finish_score -
    underdog$z_adjusted_finish_score
  
  quality_gap_penalty <-
    quality_gap_weight *
    quality_gap
  
  
  final_upset_score <-
    matchup_advantage -
    quality_gap_penalty
  
  
  upset_predicted <-
    final_upset_score >
    upset_threshold
  
  
  predicted_winner <- if (
    upset_predicted
  ) {
    underdog$team
  } else {
    favorite$team
  }
  
  
  predicted_winner_seed <- if (
    upset_predicted
  ) {
    underdog_seed
  } else {
    favorite_seed
  }
  
  
  favorite_name <-
    favorite$team[[1]]
  
  favorite_championship_rank_value <-
    favorite$championship_rank[[1]]
  
  favorite_adjusted_finish_score_value <-
    favorite$adjusted_finish_score[[1]]
  
  underdog_name <-
    underdog$team[[1]]
  
  underdog_championship_rank_value <-
    underdog$championship_rank[[1]]
  
  underdog_adjusted_finish_score_value <-
    underdog$adjusted_finish_score[[1]]
  
  
  tibble(
    round =
      round_name,
    
    region =
      region,
    
    bracket_game_id =
      bracket_game_id,
    
    team1 =
      team1,
    
    team1_seed =
      team1_seed,
    
    team2 =
      team2,
    
    team2_seed =
      team2_seed,
    
    favorite =
      favorite_name,
    
    favorite_seed =
      favorite_seed,
    
    favorite_championship_rank =
      favorite_championship_rank_value,
    
    favorite_adjusted_finish_score =
      favorite_adjusted_finish_score_value,
    
    underdog =
      underdog_name,
    
    underdog_seed =
      underdog_seed,
    
    underdog_championship_rank =
      underdog_championship_rank_value,
    
    underdog_adjusted_finish_score =
      underdog_adjusted_finish_score_value,
    
    quality_gap =
      as.numeric(quality_gap),
    
    quality_gap_penalty =
      as.numeric(quality_gap_penalty),
    
    underdog_three_point_strength =
      as.numeric(underdog_three_point_strength),
    
    favorite_perimeter_weakness =
      as.numeric(favorite_perimeter_weakness),
    
    three_point_edge =
      as.numeric(three_point_edge),
    
    turnover_pressure_edge =
      as.numeric(turnover_pressure_edge),
    
    offensive_rebounding_edge =
      as.numeric(offensive_rebounding_edge),
    
    defensive_rebounding_edge =
      as.numeric(defensive_rebounding_edge),
    
    rebounding_edge =
      as.numeric(rebounding_edge),
    
    underdog_defensive_strength =
      as.numeric(underdog_defensive_strength),
    
    defensive_resistance_edge =
      as.numeric(defensive_resistance_edge),
    
    matchup_advantage =
      as.numeric(matchup_advantage),
    
    final_upset_score =
      as.numeric(final_upset_score),
    
    upset_threshold =
      upset_threshold,
    
    upset_predicted =
      as.logical(upset_predicted),
    
    predicted_winner =
      as.character(predicted_winner),
    
    predicted_winner_seed =
      as.numeric(predicted_winner_seed)
  )
}

#Create working bracket

working_bracket <- complete_bracket |>
  select(
    round_number,
    region,
    round,
    bracket_game_id,
    team1,
    team1_seed,
    team2,
    team2_seed,
    next_game_id,
    next_slot,
    source_game_url
  )

#Advance a winner

advance_predicted_winner <- function(
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

#Process every matchup

process_tournament_round <- function(
    bracket,
    round_name,
    existing_predictions
) {
  
  game_ids <- bracket |>
    filter(
      round == round_name
    ) |>
    arrange(
      bracket_game_id
    ) |>
    pull(
      bracket_game_id
    )
  
  
  if (length(game_ids) == 0) {
    stop(
      "No games were found for round: ",
      round_name
    )
  }
  
  
  for (current_game_id in game_ids) {
    
    current_row <- which(
      bracket$bracket_game_id ==
        current_game_id
    )
    
    if (length(current_row) != 1) {
      stop(
        "Could not uniquely locate bracket game ",
        current_game_id,
        "."
      )
    }
    
    
    current_team1 <-
      bracket$team1[[current_row]]
    
    current_team2 <-
      bracket$team2[[current_row]]
    
    
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
        "^Winner of FF_"
      ) ||
      str_detect(
        current_team2,
        "^Winner of FF_"
      )
    ) {
      stop(
        "A First Four placeholder remains in game ",
        current_game_id,
        "."
      )
    }
    
    
    matchup_prediction <- predict_upset_matchup(
      team1 =
        current_team1,
      
      team2 =
        current_team2,
      
      team1_seed =
        bracket$team1_seed[[current_row]],
      
      team2_seed =
        bracket$team2_seed[[current_row]],
      
      round_name =
        bracket$round[[current_row]],
      
      region =
        bracket$region[[current_row]],
      
      bracket_game_id =
        current_game_id
    )
    
    
    existing_predictions <- bind_rows(
      existing_predictions,
      matchup_prediction
    )
    
    
    bracket <- advance_predicted_winner(
      bracket =
        bracket,
      
      next_game_id =
        bracket$next_game_id[[current_row]],
      
      next_slot =
        bracket$next_slot[[current_row]],
      
      winner =
        matchup_prediction$predicted_winner[[1]],
      
      winner_seed =
        matchup_prediction$predicted_winner_seed[[1]]
    )
  }
  
  
  list(
    bracket =
      bracket,
    
    predictions =
      existing_predictions
  )
}

#Process the entire tournament in bracket order

round_sequence <- c(
  "First Four",
  "Round of 64",
  "Round of 32",
  "Sweet 16",
  "Elite Eight",
  "Final Four",
  "Championship"
)


all_matchup_predictions <- tibble()


for (current_round in round_sequence) {
  
  message(
    "Predicting ",
    current_round,
    "..."
  )
  
  
  round_result <- process_tournament_round(
    bracket =
      working_bracket,
    
    round_name =
      current_round,
    
    existing_predictions =
      all_matchup_predictions
  )
  
  
  working_bracket <-
    round_result$bracket
  
  all_matchup_predictions <-
    round_result$predictions
}

#Identify predicted champion

championship_prediction <- all_matchup_predictions |>
  filter(
    round == "Championship"
  )

if (nrow(championship_prediction) != 1) {
  stop(
    "Expected exactly one championship-game prediction."
  )
}

predicted_champion <-
  championship_prediction$predicted_winner[[1]]

#Add prediction results and fixed rankings to completed bracket

team1_final_lookup <- team_profiles_2025 |>
  select(
    team1 = team,
    
    team1_championship_rank =
      championship_rank,
    
    team1_adjusted_finish_score =
      adjusted_finish_score
  )


team2_final_lookup <- team_profiles_2025 |>
  select(
    team2 = team,
    
    team2_championship_rank =
      championship_rank,
    
    team2_adjusted_finish_score =
      adjusted_finish_score
  )


predicted_bracket_2025 <- working_bracket |>
  left_join(
    team1_final_lookup,
    by = "team1"
  ) |>
  left_join(
    team2_final_lookup,
    by = "team2"
  ) |>
  left_join(
    all_matchup_predictions |>
      select(
        bracket_game_id,
        favorite,
        underdog,
        quality_gap,
        matchup_advantage,
        final_upset_score,
        upset_predicted,
        predicted_winner,
        predicted_winner_seed
      ),
    by = "bracket_game_id"
  ) |>
  arrange(
    round_number,
    factor(
      region,
      levels = c(
        "South",
        "East",
        "West",
        "Midwest",
        "National"
      )
    ),
    bracket_game_id
  )

#Validate complete bracket predictions

if (nrow(all_matchup_predictions) != 67) {
  stop(
    "Expected 67 tournament-game predictions, but created ",
    nrow(all_matchup_predictions),
    "."
  )
}

if (
  anyDuplicated(
    all_matchup_predictions$bracket_game_id
  ) > 0
) {
  stop(
    "Duplicate matchup predictions were created."
  )
}

if (
  any(
    is.na(
      all_matchup_predictions$predicted_winner
    )
  )
) {
  stop(
    "At least one matchup is missing a predicted winner."
  )
}

if (
  any(
    is.na(
      predicted_bracket_2025$team1
    ) |
    is.na(
      predicted_bracket_2025$team2
    )
  )
) {
  stop(
    "At least one completed bracket game is missing a team."
  )
}

#Save matchp predictions and completed bracket

write_csv(
  all_matchup_predictions,
  upset_predictions_path,
  na = ""
)

write_csv(
  predicted_bracket_2025,
  predicted_bracket_path,
  na = ""
)

#Display preedicted results

predicted_upsets <- all_matchup_predictions |>
  filter(
    upset_predicted
  ) |>
  arrange(
    desc(
      final_upset_score
    )
  )


message(
  "Predicted tournament upsets: ",
  nrow(predicted_upsets)
)


print(
  predicted_upsets |>
    select(
      round,
      region,
      favorite,
      favorite_seed,
      underdog,
      underdog_seed,
      quality_gap,
      three_point_edge,
      turnover_pressure_edge,
      rebounding_edge,
      defensive_resistance_edge,
      matchup_advantage,
      final_upset_score,
      predicted_winner
    )
)

#Display predicted final four and champion

predicted_final_four <- all_matchup_predictions |>
  filter(
    round == "Elite Eight"
  ) |>
  transmute(
    region,
    regional_champion =
      predicted_winner,
    
    regional_champion_seed =
      predicted_winner_seed
  )


print(
  predicted_final_four
)


print(
  all_matchup_predictions |>
    filter(
      round %in%
        c(
          "Final Four",
          "Championship"
        )
    ) |>
    select(
      round,
      bracket_game_id,
      team1,
      team1_seed,
      team2,
      team2_seed,
      favorite,
      underdog,
      upset_predicted,
      predicted_winner
    )
)


message(
  "Upset predictions saved to: ",
  upset_predictions_path
)

message(
  "Completed predicted bracket saved to: ",
  predicted_bracket_path
)

message(
  "Predicted 2025 National Champion: ",
  predicted_champion
)

