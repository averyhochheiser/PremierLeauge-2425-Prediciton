set.seed(42)  # For reproducibility
library(dplyr)
library(ggplot2)
library(tidyr)             


NUM_TEAMS <- 20
NUM_SIMULATIONS <- 10000

# Team names
team_names <- c(
  "Man City", "Tottenham", "Brentford", "Chelsea", "Liverpool", 
  "Brighton", "Arsenal", "Aston Villa", "Fulham", "Wolves", 
  "Bournemouth", "Nottingham Forest", "Leicester", "Newcastle", 
  "West Ham", "Ipswich", "Man United", "Everton", "Crystal Palace", 
  "Southampton"
)

# Data for goals scored 
goals_scored_rate <- c(
  22 / 11, 23 / 11, 22 / 11, 21 / 11, 21 / 11, 
  19 / 11, 18 / 11, 17 / 11, 16 / 11, 16 / 11, 
  15 / 11, 15 / 11, 14 / 11, 13 / 11, 13 / 11, 
  12 / 11, 12 / 11, 10 / 11, 8 / 11, 7 / 11
)

# Data for goals conceded
goals_conceded_rate <- c(
  10 / 11, 12 / 11, 13 / 11, 15 / 11, 9 / 11, 
  14 / 11, 11 / 11, 12 / 11, 13 / 11, 16 / 11, 
  17 / 11, 14 / 11, 15 / 11, 13 / 11, 11 / 11, 
  18 / 11, 12 / 11, 15 / 11, 16 / 11, 18 / 11
)

final_points <- matrix(0, nrow = NUM_SIMULATIONS, ncol = NUM_TEAMS)

# Simulates a single match
simulate_match <- function(home_team, away_team, home_scored, home_conceded, away_scored, away_conceded) {
  # Calculate expected goals for each team
  home_expected_goals <- home_scored * away_conceded
  away_expected_goals <- away_scored * home_conceded
  
  # Simulate goals scored by each team
  home_goals <- rpois(1, home_expected_goals)
  away_goals <- rpois(1, away_expected_goals)
  
  # Assign points based on match outcome
  if (home_goals > away_goals) {
    return(c(3, 0))  # Home team wins
  } else if (home_goals < away_goals) {
    return(c(0, 3))  # Away team wins
  } else {
    return(c(1, 1))  # Draw
  }
}

# Run simulations
for (sim in 1:NUM_SIMULATIONS) {
  points <- rep(0, NUM_TEAMS)
  
  # Simulate all matches 
  for (i in 1:(NUM_TEAMS - 1)) {
    for (j in (i + 1):NUM_TEAMS) {
      # Simulate match with team i as home team
      result <- simulate_match(
        i, j,
        goals_scored_rate[i], goals_conceded_rate[i],
        goals_scored_rate[j], goals_conceded_rate[j]
      )
      
      points[i] <- points[i] + result[1]
      points[j] <- points[j] + result[2]
      
      # Simulate reverse match with team j as home team
      result <- simulate_match(
        j, i,
        goals_scored_rate[j], goals_conceded_rate[j],
        goals_scored_rate[i], goals_conceded_rate[i]
      )
      
      points[j] <- points[j] + result[1]
      points[i] <- points[i] + result[2]
    }
  }
  
  final_points[sim, ] <- points
}

# Calculate average points per team across simulations
average_points <- colMeans(final_points)
team_points <- data.frame(Team = team_names, Points = average_points) %>%
  arrange(desc(Points))

print(team_points)


# Average Points per Team (Simulated)
rainbow_colors <- rainbow(n = nrow(team_points))
ggplot(team_points, aes(x = reorder(Team, Points), y = Points, fill = Team)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rainbow_colors) +
  coord_flip() +
  labs(
    title = "Average Points per Team (Simulated)",
    x = "Team",
    y = "Average Points"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 


# Final season point standings prediction
NUM_MATCHES <- 2 * (NUM_TEAMS - 1)
predicted_goals <- goals_scored_rate * NUM_MATCHES
predicted_goals_df <- data.frame(
  Team = team_names,
  PredictedGoals = predicted_goals
)
print(predicted_goals_df)


team_data <- data.frame(
  Team = team_names,
  Points = average_points,
  PredictedGoals = predicted_goals
)

# Average Points vs. Predicted Goals Scored per Team
ggplot(team_data, aes(y = reorder(Team, Points))) +
  geom_point(aes(x = Points, color = "Average Points"), size = 4) +
  geom_point(aes(x = PredictedGoals, color = "Predicted Goals"), size = 4) +
  geom_segment(aes(x = Points, xend = PredictedGoals, yend = reorder(Team, Points)), color = "gray") +
  scale_color_manual(values = c("Average Points" = "blue", "Predicted Goals" = "green")) +
  labs(
    title = "Average Points vs. Predicted Goals Scored per Team",
    x = NULL,
    y = "Team"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


# Distribution of Points per Team Across Simulations
final_points_long <- as.data.frame(final_points)
colnames(final_points_long) <- team_names
final_points_long <- pivot_longer(final_points_long, cols = everything(), names_to = "Team", values_to = "Points")

ggplot(final_points_long, aes(x = reorder(Team, Points, median), y = Points, fill = Team)) +
  geom_boxplot() +
  scale_fill_manual(values = rainbow_colors) +
  coord_flip() +
  labs(
    title = "Distribution of Points per Team Across Simulations",
    x = "Team",
    y = "Points"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



