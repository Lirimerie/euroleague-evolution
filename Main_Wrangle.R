#Import the dataset and libraries
library(tidyverse)
library(hexbin)
library(viridis)
library(ggplot2)
library (patchwork)
library(ggrepel)
library(gridExtra)

euroleague<-read.csv("data/euroleague.csv") 
#We only focused on Euroleague, not Eurocup

unique_playtypes <- unique(euroleague$PLAYINFO)
unique_playtypes


euroleague <- euroleague |>
  mutate(PLAYINFO = str_remove(PLAYINFO, "\\s*\\([^)]+\\)"))

#This line reduces the details of the Play information to 33 different ones
#by removing a space and what is inside the parenthesis

#Adds a column containing the points made 
euroleague <- euroleague |>
  mutate(points_made = case_when(
    PLAYINFO %in% c("Layup Made", "Two Pointer","Dunk") ~ 2,
    PLAYINFO == "Three Pointer" ~ 3,
    PLAYINFO == "Free Throw In" ~ 1,
    TRUE ~ 0
  ))




#While making our project, we wrote 37 instead of 36(minutes),
# as it was too late to modify everything, we decided to keep 37
# for the names of variables, it's definitely not good but we
# did not have time to change it



source("CreationDataFrame.R") #See CreationDataFrame.R for more explanations
stat_per_games <- CreationDataFrameTEST(euroleague)
stat_per_games_last_4 <- CreationDataFrame_last4(euroleague)
stat_per_games_first37<- CreationDataFrame_37(euroleague)

stat_per_games <- uniformisation(stat_per_games)
stat_per_games_first37 <- uniformisation(stat_per_games_first37)
stat_per_games_last_4 <- uniformisation(stat_per_games_last_4)

#add column about the points 
stat_per_games <- stat_per_games |>
  rowwise() |>
  mutate(
    Tot_Point_A = 3 * sum(ThreeS_A, na.rm = TRUE) + 
      2 * sum(TwoS_A, na.rm = TRUE) + 
      sum(FTS_A, na.rm = TRUE),
    Tot_Point_B = 3 * sum(ThreeS_B, na.rm = TRUE) + 
      2 * sum(TwoS_B, na.rm = TRUE) + 
      sum(FTS_B, na.rm = TRUE)
  ) |>
  ungroup()

#Sometimes, the team's name was in Caps, sometimes not. We solved this by
#putting every team's names in caps
stat_per_games <- stat_per_games |>
  mutate(TeamA = toupper(TeamA))|>
  mutate(TeamB = toupper(TeamB))

stat_per_games_last_4 <- stat_per_games_last_4 |>
  mutate(TeamA = toupper(TeamA))|>
  mutate(TeamB = toupper(TeamB))

stat_per_games_first37 <- stat_per_games_first37 |>
  mutate(TeamA = toupper(TeamA))|>
  mutate(TeamB = toupper(TeamB))

#Creates a winner variable with the name of the team winning
stat_per_games <- stat_per_games |>
  mutate(winner = ifelse(Tot_Point_A > Tot_Point_B, TeamA, TeamB),
         Total_Difference = abs(Tot_Point_A - Tot_Point_B))

stat_per_games_first37 <- stat_per_games_first37|>
  mutate(foul_relative_minutes = (tot_foul/37))

stat_per_games_last_4 <- stat_per_games_last_4 |>
  left_join(select(stat_per_games, gamenumber, year, Tot_Point_A,
                   Tot_Point_B, winner,Total_Difference),
            by = c("gamenumber", "year"))
stat_per_games_first37 <- stat_per_games_first37 |>
  left_join(select(stat_per_games, gamenumber, year, Tot_Point_A,
                   Tot_Point_B, winner,Total_Difference),
            by = c("gamenumber", "year"))

#take the last points in at the 37minutes to add it on the dataframe of 
#the last 4 minutes 
last_pts <- euroleague |>
  filter(MINUTE <= 37 & !is.na(POINTS_A) & !is.na(POINTS_B)) |>
  group_by(year, gamenumber) |>
  summarise(
    pts_A = max(POINTS_A),
    pts_B = max(POINTS_B)
  )

#we add it here
stat_per_games_last_4 <- stat_per_games_last_4 |>
  left_join(last_pts, by = c("year", "gamenumber"))

#count the number of foul per minute in the last 4 minutes and add this statistic 
stat_per_games_last_4 <- stat_per_games_last_4|>
  mutate(absolute_diff_points = abs(pts_A - pts_B),
         foul_relative_minutes = (tot_foul/4),
         winner_foul = ifelse(pts_A > pts_B, Foul_commited_A, Foul_commited_B),
         looser_foul = ifelse(pts_A < pts_B, Foul_commited_A, Foul_commited_B))





source("Ranking.R") #Creates usable dataframes with statistics per team, per game
#and pert team, per season

team_stats_df <- process_team_stats_data(stat_per_games)
team_stats_season <- calculate_team_season_stats(team_stats_df)
#creates two dataframes with statistics per team and per game/per season

#the following separates the last five minutes out and create new dataframes
team_stats_df_37 <- process_team_stats_data(stat_per_games_first37)
team_stats_season_37 <- calculate_team_season_stats(team_stats_df_37)
team_stats_df_4 <- process_team_stats_data(stat_per_games_last_4)
team_stats_df_4_not_desperate <- team_stats_df_4|>
  filter(Diff_Points_Min_35 <= 10 & Diff_Points_Min_35 > 0)
team_stats_season_4 <- calculate_team_season_stats(team_stats_df_4)
#team_stats_df_4_not_desperate wants to show tied game and not every game


source("playerdf_script.R") # Creates the player's dataframes and statistics
source("first_graphs.R") # use Functions.R to make interesting graphs
