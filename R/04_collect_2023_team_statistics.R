library(httr2)
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(here)

#Collecting the 68 teams in the 2023 ncaa tournament

sports_reference_root <- "https://www.sports-reference.com"

tournament_url <- paste0(
  sports_reference_root,
  "/cbb/postseason/men/2023-ncaa.html"
)

output_file <- here(
  "data",
  "raw",
  "2023_tournament_teams.csv"
)

message("Collecting 2023 NCAA Tournament teams...")

response <- request(tournament_url) |>
  req_user_agent(
    "DATA367 sports analytics project - maddiegrossnicklaus@gmail.com"
  ) |>
  req_timeout(seconds = 30) |>
  req_retry(max_tries = 3) |>
  req_perform()

if (resp_status(response) != 200) {
  stop(
    "Failed to download tournament page. HTTP Status: ",
    resp_status(response)
  )
}

tournament_page <- resp_body_html(response)

#Finding all of the 68 teams' links to game logs

team_links <- tournament_page |>
  html_elements("a")

teams <- tibble(
  team = html_text2(team_links),
  href = html_attr(team_links, "href")
) |>
  
  filter(
    !is.na(href),
    str_detect(
      href,
      "^/cbb/schools/.+/men/2023\\.html$"
    )
  ) |>
  mutate(
    team = str_squish(team),
    
    team_url = paste0(
      sports_reference_root,
      href
    ),
    
    gamelog_url = str_replace(
      team_url,
      "2023\\.html$",
      "2023-gamelogs.html"
    )
  ) |>
  distinct(
    team,
    .keep_all = TRUE
  ) |>
  arrange(team)

#Validation

message(
  "Teams found: ",
  nrow(teams)
)

if (nrow(teams) != 68) {
  
  warning(
    "Expected 68 tournament teams, but found ",
    nrow(teams),
    "."
  )
  
} else {
  
  message("Successfully collected all 68 tournament teams.")
  
}

#Save url of 68 teams names and urls

write_csv(
  teams,
  output_file
)

message(
  "Saved team list to: ",
  output_file
)

#Collecting regular season statistis for each tournament team

extract_team_statistics <- function(team_name, gamelog_url){
  
  message("Collecting: ", team_name)
  
  response <- request(gamelog_url) |>
    req_user_agent(
      "DATA367 sports analytics project - maddiegrossnicklaus@gmail.com"
    ) |>
    req_timeout(seconds = 30) |>
    req_retry(max_tries = 3) |>
    req_perform()
  
  page <- resp_body_html(response)
  
  rows <- page |>
    html_elements("#team_game_log tbody tr")
  
  # Skip header rows inside tbody
  rows <- rows[
    !grepl("thead", html_attr(rows, "class"))
  ]
  
  game_stats <- lapply(rows, function(row){
    
    tibble(
      
      date = html_element(row, "[data-stat='date']") |>
        html_text2(),
      
      team_game_result = html_element(row, "[data-stat='team_game_result']") |>
        html_text2(),
      
      team_game_score = as.numeric(
        html_text2(
          html_element(row, "[data-stat='team_game_score']")
        )
      ),
      
      opp_team_game_score = as.numeric(
        html_text2(
          html_element(row, "[data-stat='opp_team_game_score']")
        )
      ),
      
      fg = as.numeric(html_text2(html_element(row,"[data-stat='fg']"))),
      fga = as.numeric(html_text2(html_element(row,"[data-stat='fga']"))),
      fg_pct = as.numeric(html_text2(html_element(row,"[data-stat='fg_pct']"))),
      
      fg2 = as.numeric(html_text2(html_element(row,"[data-stat='fg2']"))),
      fg2a = as.numeric(html_text2(html_element(row,"[data-stat='fg2a']"))),
      fg2_pct = as.numeric(html_text2(html_element(row,"[data-stat='fg2_pct']"))),
      
      fg3 = as.numeric(html_text2(html_element(row,"[data-stat='fg3']"))),
      fg3a = as.numeric(html_text2(html_element(row,"[data-stat='fg3a']"))),
      fg3_pct = as.numeric(html_text2(html_element(row,"[data-stat='fg3_pct']"))),
      
      efg_pct = as.numeric(html_text2(html_element(row,"[data-stat='efg_pct']"))),
      
      ft = as.numeric(html_text2(html_element(row,"[data-stat='ft']"))),
      fta = as.numeric(html_text2(html_element(row,"[data-stat='fta']"))),
      ft_pct = as.numeric(html_text2(html_element(row,"[data-stat='ft_pct']"))),
      
      orb = as.numeric(html_text2(html_element(row,"[data-stat='orb']"))),
      drb = as.numeric(html_text2(html_element(row,"[data-stat='drb']"))),
      trb = as.numeric(html_text2(html_element(row,"[data-stat='trb']"))),
      
      ast = as.numeric(html_text2(html_element(row,"[data-stat='ast']"))),
      stl = as.numeric(html_text2(html_element(row,"[data-stat='stl']"))),
      blk = as.numeric(html_text2(html_element(row,"[data-stat='blk']"))),
      tov = as.numeric(html_text2(html_element(row,"[data-stat='tov']"))),
      pf = as.numeric(html_text2(html_element(row,"[data-stat='pf']"))),
      
      opp_fg = as.numeric(html_text2(html_element(row,"[data-stat='opp_fg']"))),
      opp_fga = as.numeric(html_text2(html_element(row,"[data-stat='opp_fga']"))),
      opp_fg_pct = as.numeric(html_text2(html_element(row,"[data-stat='opp_fg_pct']"))),
      
      opp_fg2 = as.numeric(html_text2(html_element(row,"[data-stat='opp_fg2']"))),
      opp_fg2a = as.numeric(html_text2(html_element(row,"[data-stat='opp_fg2a']"))),
      opp_fg2_pct = as.numeric(html_text2(html_element(row,"[data-stat='opp_fg2_pct']"))),
      
      opp_fg3 = as.numeric(html_text2(html_element(row,"[data-stat='opp_fg3']"))),
      opp_fg3a = as.numeric(html_text2(html_element(row,"[data-stat='opp_fg3a']"))),
      opp_fg3_pct = as.numeric(html_text2(html_element(row,"[data-stat='opp_fg3_pct']"))),
      
      opp_efg_pct = as.numeric(html_text2(html_element(row,"[data-stat='opp_efg_pct']"))),
      
      opp_ft = as.numeric(html_text2(html_element(row,"[data-stat='opp_ft']"))),
      opp_fta = as.numeric(html_text2(html_element(row,"[data-stat='opp_fta']"))),
      opp_ft_pct = as.numeric(html_text2(html_element(row,"[data-stat='opp_ft_pct']"))),
      
      opp_orb = as.numeric(html_text2(html_element(row,"[data-stat='opp_orb']"))),
      opp_drb = as.numeric(html_text2(html_element(row,"[data-stat='opp_drb']"))),
      opp_trb = as.numeric(html_text2(html_element(row,"[data-stat='opp_trb']"))),
      
      opp_ast = as.numeric(html_text2(html_element(row,"[data-stat='opp_ast']"))),
      opp_stl = as.numeric(html_text2(html_element(row,"[data-stat='opp_stl']"))),
      opp_blk = as.numeric(html_text2(html_element(row,"[data-stat='opp_blk']"))),
      opp_tov = as.numeric(html_text2(html_element(row,"[data-stat='opp_tov']"))),
      opp_pf = as.numeric(html_text2(html_element(row,"[data-stat='opp_pf']")))
      
    )
    
  })
  
  game_stats <- bind_rows(game_stats)
  
  game_stats$date <- as.Date(game_stats$date)
  
  game_stats <- game_stats |>
    filter(date < as.Date("2023-03-14"))
  
  wins <- sum(game_stats$team_game_result == "W")
  losses <- sum(game_stats$team_game_result == "L")
  
  summary <- game_stats |>
    summarise(
      across(
        where(is.numeric),
        \(x) mean(x, na.rm = TRUE)
      )
    )
  
  summary$team <- team_name
  summary$wins <- wins
  summary$losses <- losses
  summary$games <- nrow(game_stats)
  
  summary
}

#Run for all 68 teams

team_statistics <- purrr::map2_dfr(
  teams$team,
  teams$gamelog_url,
  extract_team_statistics
)

team_statistics <- team_statistics |>
  relocate(
    team,
    games,
    wins,
    losses
  )

write_csv(
  team_statistics,
  here(
    "data",
    "processed",
    "2023_tournament_teams.csv"
  )
)

message("Finished collecting 2023 tournament team statistics.")
