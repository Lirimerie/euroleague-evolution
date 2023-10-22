
library(tidyverse)

euroleague<-read.csv("data/euroleague.csv")

unique_playtypes <- unique(euroleague$PLAYINFO)
unique_playtypes


euroleague <- euroleague |>
  mutate(PLAYINFO = str_remove(PLAYINFO, "\\s*\\([^)]+\\)"))

#This line reduces the details of the Play information to 33 different ones
#by removing a space and what is inside the parenthesis


#euroleague <- euroleague |>
  #filter(!str_detect(PLAYINFO, "Fighting|Bench Foul|Coach Foul|Begin Period|
                     #End Period"))|>
  #select(-TYPE,-PLAYTYPE,-CODETEAM, -DORSAL,-MARKERTIME,-PLAYER_ID,
         #-X,-NUMBEROFPLAY) 

# all variables in select() are unnecessary variables. 
# Out|In could be taken out but we need to let Free throw in in the data set
#(Will work on it)
# I don't know which story could be told with Fighting
# Begin Period|End Period are not that necessary or so do I think as of now
# Coach Foul and Bench Foul are not as relevant

euroleague <- euroleague |>
  mutate(points_made = case_when(
    PLAYINFO %in% c("Layup Made", "Two Pointer","Dunk") ~ 2,
    PLAYINFO == "Three Pointer" ~ 3,
    PLAYINFO == "Free Throw In" ~ 1,
    TRUE ~ 0
  ))

source("CreationDataFrame.R")
stat_per_games <- CreationDataFrameTEST(euroleague)
stat_per_games_last_4 <- CreationDataFrame_last4(euroleague)
stat_per_games_first37<- CreationDataFrame_37(euroleague)

stat_per_games <- uniformisation(stat_per_games)
stat_per_games_first37 <- uniformisation(stat_per_games_first37)
stat_per_games_last_4 <- uniformisation(stat_per_games_last_4)

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

stat_per_games <- stat_per_games |>
  mutate(TeamA = toupper(TeamA))|>
  mutate(TeamB = toupper(TeamB))

stat_per_games_last_4 <- stat_per_games_last_4 |>
  mutate(TeamA = toupper(TeamA))|>
  mutate(TeamB = toupper(TeamB))

stat_per_games_first37 <- stat_per_games_first37 |>
  mutate(TeamA = toupper(TeamA))|>
  mutate(TeamB = toupper(TeamB))

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

# Utilisez la fonction pour extraire les dernières valeurs non-NA de POINTS_A et POINTS_B
last_pts <- euroleague |>
  filter(MINUTE <= 35 & !is.na(POINTS_A) & !is.na(POINTS_B)) |>
  group_by(year, gamenumber) |>
  summarise(
    pts_A = max(POINTS_A),
    pts_B = max(POINTS_B)
  )

# Join le résultat avec stat_per_games_last_4
stat_per_games_last_4 <- stat_per_games_last_4 |>
  left_join(last_pts, by = c("year", "gamenumber"))
stat_per_games_last_4 <- stat_per_games_last_4|>
  mutate(absolute_diff_points = abs(pts_A - pts_B),
         foul_relative_minutes = (tot_foul/4),
         winner_foul = ifelse(pts_A > pts_B, Foul_commited_A, Foul_commited_B),
         looser_foul = ifelse(pts_A < pts_B, Foul_commited_A, Foul_commited_B))





source("Ranking.R") 

team_stats_df <- process_team_stats_data(stat_per_games)
team_stats_season <- calculate_team_season_stats(team_stats_df)
#creates two dataframes with statistics per team and per game/per season

#the following take the last four minutes out and create new dataframes
team_stats_df_37 <- process_team_stats_data(stat_per_games_first37)
team_stats_season_37 <- calculate_team_season_stats(team_stats_df_37)
team_stats_df_4 <- process_team_stats_data(stat_per_games_last_4)
team_stats_df_4_not_desperate <- team_stats_df_4|>
  filter(Diff_Points_Min_35 <= 5 & Diff_Points_Min_35 > 0)
team_stats_season_4 <- calculate_team_season_stats(team_stats_df_4)
#creates two dataframes with statistics per team and per game/per season


source("playerdf_script.R")


