library(dplyr)
library(readr)
library(stringr)
library(here)
library(httr2)
library(rvest)
library(broom)

#Load 10 years of historical tournament data

historical_games <- read_csv(
  here(
    "data",
    "processed",
    "historical_tournament_statistics.csv"
  ),
  show_col_types = FALSE
)

message(
  "Historical tournament games loaded: ",
  nrow(historical_games)
)

#Prepare data for logistic regression analysis

model_data <- historical_games |>
  transmute(
    Results = team1_win,
    
    team1_drb,
    team1_orb,
    team1_stl,
    team1_blk,
    team1_tov,
    team2_tov,
    team2_drb,
    team1_opponent_fg_pct,
    team1_opponent_fg3_pct,
    team1_points_allowed,
    team1_fg_pct,
    team1_ast
  ) |>
  drop_na()

message(
  "Games available for modeling: ",
  nrow(model_data)
)

#Full logistic regression model

message("Fitting full logistic regression model...")

full_model <- glm(
  
  Results ~
    team1_drb +
    team1_orb +
    team1_stl +
    team1_blk +
    team1_tov +
    team2_tov +
    team2_drb +
    team1_opponent_fg_pct +
    team1_opponent_fg3_pct +
    team1_points_allowed +
    team1_fg_pct +
    team1_ast,
  
  data = model_data,
  family = binomial()
  
)

summary(full_model)

#Reduced model 1, remove variables with weak statistical significance

message("Fitting reduced model 1...")

reduced_model_1 <- glm(
  
  Results ~
    team1_drb +
    team1_orb +
    team1_stl +
    team1_blk +
    team1_tov +
    team2_tov +
    team2_drb +
    team1_opponent_fg_pct +
    team1_opponent_fg3_pct +
    team1_fg_pct,
  
  data = model_data,
  family = binomial()
  
)

summary(reduced_model_1)

#Reduce model 2, further removes weak variables

message("Fitting reduced model 2...")

reduced_model_2 <- glm(
  
  Results ~
    team1_drb +
    team2_tov +
    team1_tov +
    team2_drb +
    team1_opponent_fg_pct +
    team1_opponent_fg3_pct +
    team1_fg_pct,
  
  data = model_data,
  family = binomial()
  
)

summary(reduced_model_2)

#Select final logistic regression model

if (AIC(full_model) <= AIC(reduced_model_1) &&
    AIC(full_model) <= AIC(reduced_model_2)) {
  
  final_model <- full_model
  final_model_name <- "Full Model"
  
} else if (AIC(reduced_model_1) <= AIC(reduced_model_2)) {
  
  final_model <- reduced_model_1
  final_model_name <- "Reduced Model 1"
  
} else {
  
  final_model <- reduced_model_2
  final_model_name <- "Reduced Model 2"
  
}

message("Selected final model: ", final_model_name)

summary(final_model)

#Compare models

model_comparison <- tibble(
  
  Model = c(
    "Full",
    "Reduced 1",
    "Reduced 2"
  ),
  
  Predictors = c(
    length(coef(full_model)) - 1,
    length(coef(reduced_model_1)) - 1,
    length(coef(reduced_model_2)) - 1
  ),
  
  AIC = c(
    AIC(full_model),
    AIC(reduced_model_1),
    AIC(reduced_model_2)
  ),
  
  BIC = c(
    BIC(full_model),
    BIC(reduced_model_1),
    BIC(reduced_model_2)
  )
  
)

print(model_comparison)

#Extract final model coefficients

coefficient_table <- tidy(final_model)

print(coefficient_table)

#Save model outputs

coefficient_table <- tidy(final_model)

print(coefficient_table)

#Highlight slected model
model_comparison <- model_comparison |>
  mutate(
    Selected = Model == final_model_name
  )

print(model_comparison)

#Extract final model coefficients 
coefficient_table <- tidy(final_model)

print(coefficient_table)

#Create results directory
dir.create(
  here("results"),
  showWarnings = FALSE
)

#Save model outputs
write_csv(
  coefficient_table,
  here(
    "results",
    "05_logistic_coefficients.csv"
  )
)

write_csv(
  model_comparison,
  here(
    "results",
    "05_model_comparison.csv"
  )
)

saveRDS(
  final_model,
  here(
    "results",
    "05_defensive_logistic_model.rds"
  )
)

message("Logistic regression model successfully developed.")

#Load the 2023 tournament teams

teams2023 <- read_csv(
  here(
    "data",
    "processed",
    "2023_tournament_teams.csv"
  ),
  show_col_types = FALSE
)

#Calculate Advanced Defensive Metrics

teams2023 <- teams2023 |>
  
  mutate(
    
    estimated_possessions =
      0.5 * (
        (fga + 0.44 * fta - orb + tov) +
          (opp_fga + 0.44 * opp_fta - opp_orb + opp_tov)
      ),
    
    offensive_rating =
      (team_game_score / estimated_possessions) * 100,
    
    defensive_rating =
      (opp_team_game_score / estimated_possessions) * 100,
    
    orb_pct =
      orb / (orb + opp_drb),
    
    drb_pct =
      drb / (drb + opp_orb),
    
    tov_pct =
      tov / estimated_possessions,
    
    forced_tov_pct =
      opp_tov / estimated_possessions,
    
    opp_efg_pct =
      (opp_fg + 0.5 * opp_fg3) / opp_fga,
    
    opp_ft_rate =
      opp_fta / opp_fga
    
  )

message(
  "2023 tournament teams loaded: ",
  nrow(teams2023)
)

#Extract ceofficients

coef_values <- coef(final_model)
intercept <- coef_values["(Intercept)"]

coefficient_value <- function(term) {
  if (term %in% names(coef_values)) {
    unname(coef_values[term])
  } else {
    0
  }
}

#Calculate metric

teams2023 <- teams2023 |>
  mutate(
    defensive_metric =
      intercept +
      coefficient_value("team1_drb") * drb +
      coefficient_value("team1_orb") * orb +
      coefficient_value("team1_stl") * stl +
      coefficient_value("team1_blk") * blk +
      coefficient_value("team1_tov") * tov +
      coefficient_value("team2_tov") * opp_tov +
      coefficient_value("team2_drb") * opp_drb +
      coefficient_value("team1_opponent_fg_pct") * opp_fg_pct +
      coefficient_value("team1_opponent_fg3_pct") * opp_fg3_pct +
      coefficient_value("team1_points_allowed") * opp_team_game_score +
      coefficient_value("team1_fg_pct") * fg_pct +
      coefficient_value("team1_ast") * ast
  )

#Rank teams

rankings2023 <- teams2023 |>
  
  arrange(
    desc(defensive_metric)
  ) |>
  
  mutate(
    defensive_rank = row_number()
  ) |>
  
  select(
    defensive_rank,
    team,
    defensive_metric
  )

#Save rankings

write_csv(
  
  rankings2023,
  
  here(
    "results",
    "2023_defensive_rankings.csv"
  )
  
)

message(
  "2023 defensive rankings successfully created."
)