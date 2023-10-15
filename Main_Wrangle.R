#TEST GITjjlmnkéméffed
library(tidyverse)

euroleague<-read.csv("data/euroleague.csv")

unique_playtypes <- unique(euroleague$PLAYINFO)
unique_playtypes


euroleague <- euroleague |>
  mutate(PLAYINFO = str_remove(PLAYINFO, "\\s*\\([^)]+\\)"))

#Ugly line to reduce the details of the Play information to 33 different ones
#by removing a space and what is inside the parenthesis




unique_playtypes <- unique(euroleague$PLAYINFO)

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


stat_per_games <- stat_per_games |>
  rowwise() |>
  mutate(
    Tot_Point_A = 3 * sum(ThreeS_A, na.rm = TRUE) + 
      2 * sum(TwoS_A, na.rm = TRUE) + 
      sum(FTS_A, na.rm = TRUE) + 
      2 * sum(LUS_A, na.rm = TRUE) + 
      2 * sum(DUNK_A, na.rm = TRUE),
    Tot_Point_B = 3 * sum(ThreeS_B, na.rm = TRUE) + 
      2 * sum(TwoS_B, na.rm = TRUE) + 
      sum(FTS_B, na.rm = TRUE) + 
      2 * sum(LUS_B, na.rm = TRUE) + 
      2 * sum(DUNK_B, na.rm = TRUE)
  ) |>
  ungroup()

stat_per_games <- stat_per_games |>
  mutate(TeamA = toupper(TeamA))|>
  mutate(TeamB = toupper(TeamB))

stat_per_games <- stat_per_games |>
  mutate(winner = ifelse(Tot_Point_A > Tot_Point_B, TeamA, TeamB))

stat_per_games_first37 <- stat_per_games_first37|>
  mutate(foul_relative_minutes = (tot_foul/37))

stat_per_games_last_4 <- stat_per_games_last_4 |>
  left_join(select(stat_per_games, gamenumber, year, Tot_Point_A, Tot_Point_B, winner),
            by = c("gamenumber", "year"))
stat_per_games_firts_37 <- stat_per_games_first_37 |>
  left_join(select(stat_per_games, gamenumber, year, Tot_Point_A, Tot_Point_B, winner),
            by = c("gamenumber", "year"))

# Utilisez la fonction pour extraire les dernières valeurs non-NA de POINTS_A et POINTS_B
last_pts <- euroleague |>
  filter(MINUTE == 35 & !is.na(POINTS_A) & !is.na(POINTS_B)) |>
  group_by(year, gamenumber) |>
  summarise(
    pts_A = tail(POINTS_A, 1),
    pts_B = tail(POINTS_B, 1)
  )

# Join le résultat avec stat_per_games_last_4
stat_per_games_last_4 <- stat_per_games_last_4 |>
  left_join(last_pts, by = c("year", "gamenumber"))
stat_per_games_last_4 <- stat_per_games_last_4|>
  mutate(absolute_diff_points = abs(pts_A - pts_B),
         foul_relative_minutes = (tot_foul/4),
         winner_foul = ifelse(pts_A > pts_B, Foul_commited_A, Foul_commited_B),
         looser_foul = ifelse(pts_A < pts_B, Foul_commited_A, Foul_commited_B))

p1 <- ggplot(data = stat_per_games_last_4) +
  geom_point(data = subset(stat_per_games_last_4, absolute_diff_points < 25),
             aes(x = absolute_diff_points, y = tot_foul)) +
  geom_smooth(data = subset(stat_per_games_last_4, absolute_diff_points < 25),
              aes(x = absolute_diff_points, y = tot_foul)) +
  theme(legend.position = "none")
p1





plot_foul<- ggplot(data = stat_per_games_last_4) +
  geom_point(data = subset(stat_per_games_last_4,
                           absolute_diff_points < 25 & absolute_diff_points > 10),
             aes(x = absolute_diff_points, y = tot_foul,
                 color = ifelse((Foul_commited_A >Foul_commited_B & pts_A < pts_B) | (Foul_commited_B > Foul_commited_A & pts_B < pts_A), "red", "black"))) +
  geom_smooth(data = subset(stat_per_games_last_4, absolute_diff_points < 25),
              aes(x = absolute_diff_points, y = tot_foul), color = "blue") +
  theme(legend.position = "none") +
  scale_color_identity()
plot_foul




mean_foul_relative_minutes_first37 <- mean(stat_per_games_first37$foul_relative_minutes, na.rm = TRUE)
mean_foul_relative_minutes_last4_serré <- mean(stat_per_games_last_4$foul_relative_minutes[stat_per_games_last_4$absolute_diff_points < 15], na.rm = TRUE)
mean_foul_relative_minutes_last4_pas_serré <- mean(stat_per_games_last_4$foul_relative_minutes[stat_per_games_last_4$absolute_diff_points >= 15], na.rm = TRUE)
df <- data.frame(Dataset = c("first_part_game", "tight_game", "non_tight_game"),
                 MeanFoulRelativeMinutes = c(mean_foul_relative_minutes_first37, mean_foul_relative_minutes_last4_serré, mean_foul_relative_minutes_last4_pas_serré))
p2 <- ggplot(data = df, aes(x = Dataset, y = MeanFoulRelativeMinutes, fill = Dataset)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("first_part_game" = "blue", "tight_game" = "red", "non_tight_game" = "lightcoral"),
                    labels = c("first_part_game" = "first_part_game", "tight_game" = "tight_game", "non_tight_game" = "non_tight_game"))+
  theme(legend.position = "bottom") +
  ylab("Mean Foul Relative Minutes") +
  xlab(NULL)
p2


mean_looser_foul <- mean(stat_per_games_last_4$looser_foul, na.rm = TRUE)
mean_winner_foul <- mean(stat_per_games_last_4$winner_foul, na.rm = TRUE)
labels <- c("Looser", "Winner")

plot_mean_foul <- ggplot(data = NULL, aes(x = labels)) +
  geom_bar(stat = "identity", aes(y = c(mean_looser_foul, mean_winner_foul)), fill = c("red", "blue")) +
  labs(y = "Moyenne des Foul", title = "Moyenne des Foul pour Looser et Winner") +
  theme(axis.text.x = element_blank())  # Pour masquer les étiquettes de l'axe x
print(plot_mean_foul)




source("Ranking.R") 

team_stats_df <- process_team_stats_data(stat_per_games)
team_stats_season <- calculate_team_season_stats(team_stats_df)
#creates two dataframes with statistics per team and per game/per season

#the following take the last four minutes out and create new dataframes
#team_stats_df_37 <- process_team_stats_data(stat_per_games_last_4)
#team_stats_season_37 <- calculate_team_season_stats(team_stats_df_37)
#team_stats_df_4 <- process_team_stats_data(stat_per_game_first37)
#team_stats_season_4 <- calculate_team_season_stats(team_stats_df_4)
#creates two dataframes with statistics per team and per game/per season


source("playerdf_script.R")


