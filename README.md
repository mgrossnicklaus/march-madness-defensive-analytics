# Predicting March Madness Success Using Defensive Analytics

## An Analytical Approach to NCAA Tournament Prediction

**Author:** Maddie Grossnicklaus  
**Last Revised:** July 14th, 2026

---

## Abstract

With an estimated **9.2 quintillion** possible combinations of March Madness bracket picks, predicting success in the NCAA Men's Basketball Tournament remains one of the most challenging problems in sports analytics due to the tournament's single-elimination format and the prevalence of unpredictable upsets. Sports analysts, celebrities, and college basketball fans across the country all try their best to complete an accurate bracket. Some prioritize overall team records and strength of schedule, others rely on advanced offensive and defensive efficiency metrics, while casual fans may simply choose based on intuition or even their preferred team colors. This project investigates several predictive approaches, ranging from regression analysis to Monte Carlo simulation, to better identify teams capable of making deep NCAA Tournament runs using only regular-season performance statistics.

The analytical framework evolves through four successive stages. An initial **Defensive Logistic Regression** model establishes the importance of defensive performance in predicting tournament success, motivated by the more defense-oriented style of play often observed in college basketball. This foundation is expanded through the **Championship Score**, a regularized machine learning model that learns the statistical profile associated with historically successful tournament teams and generates pre-tournament team rankings. The **Upset Metric** extends this approach by evaluating individual matchups, identifying situations in which stylistic advantages, such as three-point shooting, rebounding, turnover pressure, and defensive resistance, may allow an underdog to outperform a higher-ranked opponent. Finally, these components are integrated into a **Monte Carlo Tournament Simulation**, which repeatedly simulates the NCAA Tournament using team quality, matchup-specific characteristics, and probabilistic game outcomes to estimate each team's likelihood of advancing through every tournament round and ultimately winning the national championship.

---

## Background

This project started as the final assignment for my Sports Analytics course during my junior year at the University of Arizona. As an Arizona student, there was no sport I was more excited to analyze than college basketball. Every March, one of my favorite traditions is trying to build the best NCAA Tournament bracket among friends, family, and coworkers. I'll admit it, I used to be the person who made picks based purely on " feeling," and I was curious whether there was actually more to predicting March Madness success than intuition or luck.

The first version of this project was completed in May 2023, immediately following one of the wildest NCAA Tournaments in recent history. Top-seeded Purdue became just the second No. 1 seed ever to lose to a No. 16 seed, while my own Arizona Wildcats suffered a shocking first-round loss as a No. 2 seed to Princeton, a No. 15 seed. If there was ever a year to begin trying to predict the "perfect" bracket, this was it.

The original project focused on a simple **Defensive Logistic Regression** model. I had noticed that successful college basketball teams often relied more heavily on defensive play than their NBA counterparts, and I wondered if defense was the quiet indicator that many prediction models overlooked. While that first model was admittedly simple, it performed surprisingly well, identifying several unexpected tournament teams more effectively than popular ranking systems such as NET or KenPom. More importantly, it convinced me there was something worth exploring.

When February 2024 rolled around, I found myself wanting to improve on my original class project simply because I thought it would be fun. Rather than predicting tournament success using a single regression model, I wanted to learn what a championship team actually looked like statistically. This led to the development of the **Championship Score**, a regularized machine learning model using the Elastic Net technique that learns the statistical profile of historically successful NCAA Tournament teams and produces pre-tournament rankings based entirely on regular-season performance and how closely each team looks like a champion.

Although the Championship Score identified strong teams well, something still felt incomplete. Every March, there are always a handful of games where an underdog simply seems to have the perfect matchup against a favorite. In 2025, now a college graduate competing in office bracket pools instead of classroom assignments, I developed the **Upset Metric**. Rather than asking which team is better overall, the Upset Metric asks a different question: *Does the underdog possess the exact strengths needed to exploit the favorite's weaknesses?* By evaluating stylistic advantages such as three-point shooting, rebounding, turnover pressure, and defensive resistance, the model attempts to identify the types of matchups that produce memorable March Madness upsets. That year, it successfully predicted a perfect Final Four, convincing me I was moving in the right direction.

By 2026, I wanted to bring everything together. With Arizona once again entering the tournament as a legitimate contender, the Wildcat in me was especially excited for March Madness. This current iteration combines the Championship Score and Upset Metric within a **Monte Carlo simulation**, allowing the tournament to be simulated thousands of times. Rather than producing a single bracket prediction, the model estimates each team's probability of advancing through every round based on overall team quality, matchup-specific characteristics, and the inherent randomness that makes the NCAA Tournament one of the most exciting sporting events in the world.

What began as a class assignment has grown into a multi-year personal project that has introduced me to increasingly sophisticated analytical methods, strengthened my programming and machine learning skills, and, perhaps most importantly, earned me a few years of bragging rights during March Madness. I'm already looking forward to seeing what the 2027 tournament inspires next.

---

## Research Question

> Can historical NCAA Tournament performance be used to identify the characteristics of successful tournament teams, and can those characteristics be applied to regular-season data to accurately predict future NCAA Tournament outcomes before the tournament begins?

More specifically, this project investigates whether patterns learned from historical NCAA Tournament teams can be used to identify championship contenders, predict potential upsets, and estimate each team's probability of advancing through the tournament using only the information available prior to Selection Sunday.

---

# Project Objectives

The primary objectives of this project are:

- Build a historical NCAA Tournament database.
- Engineer advanced offensive and defensive efficiency statistics.
- Learn the statistical characteristics of successful tournament teams.
- Rank future tournament teams using a machine learning model.
- Identify potential tournament upsets through matchup analysis.
- Simulate the NCAA Tournament thousands of times using Monte Carlo methods.

---

# Repository Structure

```text
march-madness-defensive-analytics/

├── R/
│   Complete data collection and modeling scripts
│
├── data/
│   ├── raw/
│   ├── processed/
│   └── final/
│
├── figures/
│   Publication-quality figures
│
├── results/
│   Saved models and model outputs
│
├── README.md
└── march-madness-defensive-analytics.Rproj
```

The repository is organized so that each script builds upon the outputs of the previous stage. While each annual iteration introduced new predictive techniques, together the scripts form one complete analytical pipeline from historical data collection through tournament simulation. Right now it the code is not setup to easily apply the 06 Script (created for the 2024 season), for example, to a different season than intended, without intense rebuilding. 

---

# Workflow

The complete project follows the pipeline below. Keep in mind that each piece of the puzzle was created in a different year, and sort of fit together into one process in 2026. Some scripts may still feel a bit disjointed though.

```
Historical NCAA Data
        │
        ▼
Data Collection
        │
        ▼
Feature Engineering
        │
        ▼
Championship Score Model
        │
        ▼
Upset Metric
        │
        ▼
Monte Carlo Simulation
        │
        ▼
Tournament Predictions
```

---

# Figures

Figures provide a visual summary of the project, illustrating how the dataset was constructed, how each predictive model evolved, and how the models performed on historical NCAA Tournaments.

- **Figure 01:** Summarizes the historical NCAA Tournament dataset used to train the models, including tournament coverage, total games, and the number of Sweet Sixteen-or-better team seasons.

- **Figure 02:** Highlights the programs with the most deep NCAA Tournament runs from 2012–2025, illustrating which teams most frequently appear among the tournament's strongest performers.

- **Figure 03:** Visualizes the statistical profile of successful tournament teams by comparing average performance across key offensive and defensive metrics for Sweet Sixteen teams through national champions.

- **Figure 04:** Displays the coefficients of the Defensive Logistic Regression model, showing which regular-season statistics were positively or negatively associated with winning NCAA Tournament games.

- **Figure 05:** Compares candidate Defensive Logistic Regression models using AIC and BIC, demonstrating how the final model was selected.

- **Figure 06:** Ranks the top twenty teams according to the 2023 Defensive Metric and compares those preseason rankings with each team's actual NCAA Tournament finish.

- **Figure 07:** Illustrates the model selection process for the Championship Score by comparing Elastic Net models through season-grouped cross-validation.

- **Figure 08:** Summarizes how the Championship Score reduced fourteen candidate statistics into a compact predictive model through LASSO regularization.

- **Figure 09:** Presents the 2024 Championship Score rankings and compares the model's highest-rated teams with their actual NCAA Tournament performance.

- **Figure 10:** Demonstrates the decision-making process of the Upset Metric, showing how stylistic matchup advantages are combined with overall team quality to identify potential tournament upsets.

- **Figure 11:** Compares the 2025 bracket predicted by the Upset Metric with the actual NCAA Tournament results from the Sweet Sixteen through the national championship game.

- **Figure 12:** Displays Monte Carlo-estimated probabilities of reaching each tournament round for the leading 2026 teams, illustrating how uncertainty accumulates throughout the tournament.

- **Figure 13:** Compares the most likely tournament outcomes generated by the Monte Carlo Simulation with the actual 2026 NCAA Tournament results, providing a final evaluation of the complete predictive framework.

---

# R Scripts

The project is organized as eight R scripts that reflect the progression of the analysis from historical data collection to full-tournament simulation.

---

## 01_collect_tournament_brackets

Collects NCAA Men's Basketball Tournament game URLs from Sports Reference for each historical tournament season included in the study. The script validates the expected number of tournament games and produces a master list of game URLs for downstream data collection.

**Input**

- Sports Reference NCAA Tournament pages

**Output**

- Historical tournament game URL database (`tournament_brackets.csv`)

---

## 02_collect_historical_statistics

Downloads box score statistics for every historical NCAA Tournament game and engineers advanced offensive and defensive efficiency metrics for both teams in every matchup.

Examples include:

- Estimated Possessions
- Offensive Rating
- Defensive Rating
- Effective Field Goal Percentage
- True Shooting Percentage
- Turnover Percentage
- Offensive & Defensive Rebounding Percentage
- Opponent Shooting Metrics
- Point Margin

**Input**

- Historical tournament game URL database

**Output**

- Historical tournament game statistics (`historical_tournament_statistics.csv`)
- Error log for any failed collections

---

## 03_build_historical_summary

Converts game-level tournament statistics into season-level summaries for every team reaching the Sweet Sixteen or beyond. Average tournament statistics are calculated and each team is assigned a finish weight based on how far it advanced.

Tournament finish weights:

- Sweet Sixteen
- Elite Eight
- Final Four
- Runner-up
- National Champion

**Inputs**

- Historical tournament game statistics

**Outputs**

- `historical_tournament_statistics_20XX.csv`
- `historical_tournament_summary_20XX.csv`

---

## 04_collect_2023_team_statistics

Collects pre-tournament regular-season statistics for every NCAA Tournament team. Tournament games are excluded so that all predictions are based solely on information available before the NCAA Tournament begins.

**Inputs**

- Sports Reference team game logs

**Outputs**

- Tournament team regular-season statistics (`20XX_tournament_teams.csv`)

---

## 05_defensive_logistic_regression

Develops the original Defensive Logistic Regression model introduced in the first iteration of the project.

Historical NCAA Tournament games are used to train a logistic regression model that predicts the probability of winning a tournament game using defensive and efficiency-based statistics. The trained model is then applied to the current tournament field to generate defensive rankings.

**Inputs**

- Historical tournament game statistics
- Tournament team regular-season statistics

**Outputs**

- Trained logistic regression model
- Model coefficients
- Model comparison results
- Tournament defensive rankings

---

## 06_championship_score

The Championship Score model expands beyond defensive performance by learning the statistical profile associated with historically successful NCAA Tournament teams.

Using Sweet Sixteen-or-better historical tournament summaries, an Elastic Net (LASSO-selected) regression model is trained to predict tournament success from offensive and defensive efficiency metrics. Strength of Schedule is incorporated after model prediction to improve the resulting team rankings.

The final Championship Score provides an overall measure of championship-caliber performance prior to the NCAA Tournament.

**Inputs**

- Historical tournament summary dataset
- Tournament team regular-season statistics
- Strength of Schedule data

**Outputs**

- Trained Championship Score model
- Model coefficients
- Cross-validation results
- Championship Scores
- Tournament team rankings

---

## 07_upset_metric

The Upset Metric evaluates individual tournament matchups rather than team quality alone.

Rather than assuming the higher-ranked team always advances, the model searches for stylistic mismatches that may favor an underdog, including:

- Three-point shooting advantages
- Turnover pressure
- Rebounding mismatches
- Defensive resistance

The metric combines these matchup-specific characteristics with each team's Championship Score to determine whether the underdog possesses enough statistical advantages to overcome the favorite. Predicted winners automatically advance through each successive tournament round, producing a complete projected NCAA Tournament bracket.

**Inputs**

- Championship Score rankings
- Tournament team statistics
- Tournament bracket

**Outputs**

- Ranked tournament bracket
- Matchup predictions
- Predicted tournament bracket

---

## 08_monte_carlo_simulation

The final stage combines the Championship Score with the Upset Metric within a Monte Carlo simulation framework.

Every possible matchup between tournament teams is assigned a win probability based on:

- Overall Championship Score
- Three-point shooting advantage
- Turnover pressure
- Rebounding advantage
- Defensive resistance
- Random tournament variance

The NCAA Tournament is then simulated thousands of times, allowing game outcomes to vary probabilistically rather than deterministically.

Repeated tournament simulations estimate each team's probability of:

- Winning a First Four game (if applicable)
- Reaching the Round of 32
- Reaching the Sweet Sixteen
- Reaching the Elite Eight
- Reaching the Final Four
- Reaching the National Championship Game
- Winning the National Championship

**Inputs**

- Championship Score rankings
- Tournament team statistics
- Tournament bracket
- Strength of Schedule data

**Outputs**

- Monte Carlo tournament simulations
- Advancement probabilities
- Championship probabilities
- Simulated tournament brackets

---

# Data Sources

The project uses publicly available NCAA basketball data including:

- Sports Reference College Basketball
- NCAA Tournament Results
- Strength of Schedule (SOS)

Historical tournament seasons span **2012–2025**.

---

# Future Improvements

Right now the project and scripts exist to show the process used to try to predict March Madness success. Future userability improvements include making the scripts more general so that years and teams are not hard-coded in, which makes it difficult to apply the metrics developed for specific seasons, to other years.

For my analysis, I intend to keep looking at machine learning models, incorporate more sophisticated predictive techniques, and play around with different statistics incorporated into models. The upset metric remains one of my major goals, I think refining that fully would allow for a lot better predictions!

---

# License

This project is released under the MIT License.