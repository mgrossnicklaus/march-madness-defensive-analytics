library(httr2)
library(rvest)
library(here)

#NCAA men's tournaments in the study period, 2020 was cancelled for COVID

tournament_years <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022)

sports_reference_root <- "https://www.sports-reference.com"

postseason_base_url <- paste0(
  sports_reference_root,
  "/cbb/postseason/men"
)

output_file <- here(
  "data",
  "raw",
  "tournament_brackets.csv"
)

#Collect all game URLs for one tournament season.
collect_tournament_games <- function(year) {
  
  message("Collecting tournament games for ", year, "...")
  
  tournament_url <- paste0(
    postseason_base_url,
    "/",
    year,
    "-ncaa.html"
  )
  
  response <- request(tournament_url) |>
    req_user_agent(
      "DATA367 sports analytics project - maddiegrossnicklaus@gmail.com"
    ) |>
    req_timeout(seconds = 30) |>
    req_retry(max_tries = 3) |>
    req_perform()
  
  if (resp_status(response) != 200) {
    stop(
      "Tournament page request failed for ",
      year,
      ". HTTP status: ",
      resp_status(response)
    )
  }
  
  tournament_page <- resp_body_html(response)
  
  game_links <- tournament_page |>
    html_elements("a") |>
    html_attr("href")
  
  # Keep only men's box-score links for the selected season.
  game_links <- game_links[
    !is.na(game_links) &
      grepl(
        paste0("^/cbb/boxscores/", year, "-"),
        game_links
      ) &
      !grepl("_w\\.html$", game_links)
  ]
  
  game_links <- unique(game_links)
  
  tournament_games <- data.frame(
    season = year,
    game_url = paste0(
      sports_reference_root,
      game_links
    ),
    stringsAsFactors = FALSE
  )
  
  message(
    "Found ",
    nrow(tournament_games),
    " games for ",
    year,
    "."
  )
  
  #Pause between requests to avoid too much traffic. 
  Sys.sleep(7)
  
  tournament_games
}

#Repeat the function for all ten completed tournament seasons of interest.
tournament_brackets <- do.call(
  rbind,
  lapply(
    tournament_years,
    collect_tournament_games
  )
)

#Remove any accidental duplicate rows.
tournament_brackets <- unique(tournament_brackets)

#Sort by season and URL.
tournament_brackets <- tournament_brackets[
  order(
    tournament_brackets$season,
    tournament_brackets$game_url
  ),
]

row.names(tournament_brackets) <- NULL

#Count the collected games for each season.
games_per_season <- aggregate(
  game_url ~ season,
  data = tournament_brackets,
  FUN = length
)

names(games_per_season)[2] <- "number_of_games"

# Every season should have 67 played games except 2021, Oregon vs. VCU in 2021 was declared a no-contest
expected_games <- data.frame(
  season = tournament_years,
  expected_number_of_games = c(
    67, 67, 67, 67, 67,
    67, 67, 67, 66, 67
  )
)

validation_results <- merge(
  games_per_season,
  expected_games,
  by = "season",
  all = TRUE
)

validation_results <- validation_results[
  order(validation_results$season),
]

print(validation_results)

if (
  any(
    validation_results$number_of_games !=
    validation_results$expected_number_of_games
  )
) {
  warning(
    "At least one season does not match its expected game count."
  )
} else {
  message("All tournament seasons passed validation.")
}

# The expected total is 669 played tournament games.
if (nrow(tournament_brackets) != 669) {
  warning(
    "Expected 669 total games, but collected ",
    nrow(tournament_brackets),
    "."
  )
}

#Save the CSV with games and URLs for all ten seasons.
write.csv(
  tournament_brackets,
  output_file,
  row.names = FALSE
)

message(
  "Finished. Tournament bracket index saved to: ",
  output_file
)