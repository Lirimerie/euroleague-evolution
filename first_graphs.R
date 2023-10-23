library(ggplot2)
library (patchwork)
library(ggrepel)
library(gridExtra)
source("Function.R")

#Statistics per team about three pointers

#this script is where we create all the plot that we wanted 

plots <- 
  team_stats_plot_over_years(team_stats_season,
                             average_threeS,
                             "Average of three pointer made per game",
                             "Average Three pointer made by a team in a season",
                             "plot_threeS_over_years"
                                    )
plot_threeS_over_years <- plot_threeS_over_years +
  labs(subtitle = "this is a test")
print(plot_threeS_over_years)

plots <- 
  team_stats_plot_over_years(team_stats_season,
                             average_three_attempts,
                             "Average of three pointer attempted per game",
                             "Average Three pointer attempted by the team",
                             "plot_three_attempts_over_years"
)
print(plot_three_attempts_over_years)

plots <- 
  team_stats_plot_over_years(
    team_stats_season,
    avg_3pt_accuracy,
    "Average accuracy of three per game",
    "Average accuracy of a team while shooting three pointers",
    "plot_three_accuracy_over_years"
)
print(plot_three_accuracy_over_years)

#Statistics per team about two pointers
plots <- team_stats_plot_over_years(team_stats_season,average_twoS,
                                    "Average of two pointer made per game",
                                    "Average two pointer made by the team",
                                    "plot_twoS_over_years"
)
print(plot_twoS_over_years)

plots <- team_stats_plot_over_years(team_stats_season,average_two_attempts,
                                    "Average of two pointer attempted per game",
                                    "Average two pointer attempted by a team",
                                    "plot_two_attempts_over_years"
)
print(plot_two_attempts_over_years)

plots <- team_stats_plot_over_years(team_stats_season,avg_2pt_accuracy,
                                    "Average accuracy of two per game",
                                    "Average accuracy of two pointers",
                                    "plot_two_accuracy_over_years"
)
print(plot_two_accuracy_over_years)

#Statistics per team about points made
plots <- team_stats_plot_over_years(team_stats_season,average_points,
                                    "Average of points made per game",
                                    "Average points made by the team",
                                    "plot_points_over_years"
)
print(plot_points_over_years)
#Statistics per team about rebounds made
plots <- team_stats_plot_over_years(team_stats_season,average_off_reb,
                                    "Average of off rebounds made per game",
                                    "Average off reb made by the team",
                                    "plot_offreb_over_years"
)
print(plot_offreb_over_years)

plots <- team_stats_plot_over_years(team_stats_season,average_def_reb,
                                    "Average of def rebounds made per game",
                                    "Average def reb made by the team",
                                    "plot_defreb_over_years"
)
print(plot_defreb_over_years)
#Statistics per team about rebounds made
plots <- team_stats_plot_over_years(team_stats_season,average_FT_attempts,
                                    "Average of FT attempted per game",
                                    "Average FT made by the team",
                                    "plot_FT_attempts_over_years"
)
print(plot_FT_attempts_over_years)

plots <- team_stats_plot_over_years(team_stats_season,avg_FT_accuracy,
                                    "Average accuracy of FT per game",
                                    "Average accruracy of FT pointers",
                                    "plot_FT_accuracy_over_years"
)
print(plot_FT_accuracy_over_years)

team_stats_season <- team_stats_season |>
  arrange(year, desc(win_percentage))

top_teams <- team_stats_season |>
  group_by(year) |>
  slice_head(n = 3)

# Créez le graphique à barres et ajoutez le texte des noms d'équipe en position verticale
top_winners<-ggplot(top_teams, aes(x = as.factor(year),
                                   y = win_percentage, fill = Team)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Team), position = position_dodge(width = 0.9),
            angle = 90, hjust = 1, vjust = 0.5) + 
  labs(title = "Les 3 équipes avec le meilleur win percentage chaque année",
       x = "Année",
       y = "Win Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")
  
top_winners
grid.arrange(top_winners,p1,nrow=2)


Effect_3Attempts_on_Win <- plot_separated_effect_2016(team_stats_df, 
                                                      three_attempts, 
                                                      "Three-point attempts")+
  coord_cartesian( ylim = c(0.2, 0.8))

print(Effect_3Attempts_on_Win)


Effect_3Attempts_on_Win_first37 <-
  plot_separated_effect_2016(
    team_stats_df_37,
    three_attempts,
    "Three-point attempts in the first 37 minutes of the game")

Effect_3Attempts_on_Win_first37 <- Effect_3Attempts_on_Win_first37 +
  coord_cartesian(xlim = c(10,30), ylim = c(0.2, 0.8))
print(Effect_3Attempts_on_Win_first37)

Effect_3Attempts_on_Win_last_4 <-
  plot_separated_effect_2016(
    team_stats_df_4,
    three_attempts,
    "Three-point attempts in the last 4 minutes of the game")+
  coord_cartesian(xlim = c(0,9), ylim = c(0.2, 0.8))

print(Effect_3Attempts_on_Win_last_4)

Effect_3accuracy_on_Win <- plot_effect_game(team_stats_df, 
                                            three_accuracy, 
                                            "Three-point accuracy") +
  coord_cartesian( xlim= c(0.1,0.6), ylim = c(0, 1))

print(Effect_3accuracy_on_Win)

Effect_2Attempts_on_Win <- plot_separated_effect_2016(team_stats_df,
                                                      two_attempts, 
                                                      "Two-point attempts")+
  coord_cartesian(ylim = c(0.2, 0.8))

print(Effect_2Attempts_on_Win)

Effect_2accuracy_on_Win <- plot_effect_game(team_stats_df, 
                                            two_accuracy, 
                                            "Two-point accuracy")+
  coord_cartesian( xlim = c(0.3, 0.75), ylim = c(0,1))

print(Effect_2accuracy_on_Win)

#effect of the total of points made or points taken were not that interesting

Effect_Foul_on_Win <- plot_effect_game(team_stats_df, 
                                                      Fouls_commited, 
                                                      "Fouls commited")+
  coord_cartesian(ylim = c(0.2, 0.8))

print(Effect_Foul_on_Win)

Effect_Foul_on_Win_last_4 <- 
  plot_effect_game(team_stats_df_4,
                   Fouls_commited,
                   "Fouls commited in the last four minutes")+
  coord_cartesian(xlim = c(0,8), ylim = c(0.1, 0.9))

print(Effect_Foul_on_Win_last_4)

Effect_Foul_on_Gap_last_4 <- 
  plot_effect_game(team_stats_df_4,
                   Fouls_commited,
                   "Fouls commited in the last four minutes",
                   Variation_Of_Gap,
                   "change in difference of points")+coord_cartesian()

print(Effect_Foul_on_Gap_last_4)

Effect_Foul_on_Gap_last_4_not_Desperate <-
  plot_effect_game(team_stats_df_4_not_desperate,
                   Fouls_commited,
                   "Fouls commited in the last four minutes",
                   Variation_Of_Gap,"change in difference of points")+
  coord_cartesian()

print(Effect_Foul_on_Gap_last_4_not_Desperate)



Effect_Three_accuracy_on_Gap_last_4_not_Desperate <- 
  plot_effect_game(team_stats_df_4_not_desperate,
                   three_accuracy,
                   "Three accu commited in the last four minutes",
                   Variation_Of_Gap,
                   "change in difference of points")+
  coord_cartesian()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

print(Effect_Three_accuracy_on_Gap_last_4_not_Desperate)

print(intersection_x)

Effect_Three_attempts_on_Gap_last_4_not_Desperate <- 
  plot_effect_game(team_stats_df_4_not_desperate,
                   three_attempts,
                   "Three attempts commited in the last four minutes",
                   Variation_Of_Gap,
                   "change in difference of points")+
  coord_cartesian()

print(Effect_Three_attempts_on_Gap_last_4_not_Desperate)

Effect_Three_made_on_Gap_last_4_not_Desperate <-
  plot_effect_game(team_stats_df_4_not_desperate,
                   ThreeS,
                   "Three made commited in the last four minutes",
                   Variation_Of_Gap,
                   "change in difference of points")+
  coord_cartesian()

print(Effect_Three_made_on_Gap_last_4_not_Desperate)

Effect_Gap_on_Win <- plot_effect_game(team_stats_df_4,
                                      Variation_Of_Gap,
                                      "change in difference of points")+
                                                                  
  coord_cartesian()

print(Effect_Gap_on_Win)

points07<- create_team_points_plot(2007)
points08<- create_team_points_plot(2008)
points09<- create_team_points_plot(2009)
points10<- create_team_points_plot(2010)
points11<- create_team_points_plot(2011)
points12<- create_team_points_plot(2012)
points13<- create_team_points_plot(2013)
points14<- create_team_points_plot(2014)
points15<- create_team_points_plot(2015)
points16<- create_team_points_plot(2016)
points17<- create_team_points_plot(2017)
points18<- create_team_points_plot(2018)
points19<- create_team_points_plot(2019)
points20<- create_team_points_plot(2020)

library(gridExtra)

grid.arrange(points07, points08, points09, points10, ncol = 2)
grid.arrange(points11, points12, points13, points14, ncol = 2)
grid.arrange(points15, points16, points17, points18, ncol = 2)
grid.arrange(points19, points20, ncol = 2)





p1 <- ggplot(data = stat_per_games_last_4) +
  geom_point(data = subset(stat_per_games_last_4, absolute_diff_points < 30),
             aes(x = absolute_diff_points, y = tot_foul)) +
  geom_smooth(data = subset(stat_per_games_last_4, absolute_diff_points < 30),
              aes(x = absolute_diff_points, y = tot_foul)) +
  theme(legend.position = "none")
p1





plot_foul<- ggplot(data = stat_per_games_last_4) +
  geom_point(data = subset(stat_per_games_last_4,
                           absolute_diff_points < 25 &
                             absolute_diff_points > 10),
             aes(x = absolute_diff_points, y = tot_foul,
                 color = ifelse((Foul_commited_A >Foul_commited_B &
                                   pts_A < pts_B) |
                                  (Foul_commited_B > Foul_commited_A &
                                     pts_B < pts_A), "red", "black"))) +
  geom_smooth(data = subset(stat_per_games_last_4, absolute_diff_points < 25),
              aes(x = absolute_diff_points, y = tot_foul), color = "blue") +
  theme(legend.position = "none") +
  scale_color_identity()
plot_foul




mean_foul_relative_minutes_first37 <- 
  mean(stat_per_games_first37$foul_relative_minutes, na.rm = TRUE)
mean_foul_relative_minutes_last4_serré <- 
  mean(stat_per_games_last_4$foul_relative_minutes
       [stat_per_games_last_4$absolute_diff_points < 15],
       na.rm = TRUE)
mean_foul_relative_minutes_last4_pas_serré <-
  mean(stat_per_games_last_4$foul_relative_minutes
       [stat_per_games_last_4$absolute_diff_points >= 15], 
       na.rm = TRUE)
df <- data.frame(Dataset = c("first_part_game", "tight_game", "non_tight_game"),
                 MeanFoulRelativeMinutes = 
                   c(mean_foul_relative_minutes_first37,
                     mean_foul_relative_minutes_last4_serré,
                     mean_foul_relative_minutes_last4_pas_serré))
p2 <- ggplot(data = df, aes(x = Dataset,
                            y = MeanFoulRelativeMinutes, fill = Dataset)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("first_part_game" = "blue",
                               "tight_game" = "red",
                               "non_tight_game" = "lightcoral"),
                    labels = c("first_part_game" = "first_part_game",
                               "tight_game" = "tight_game",
                               "non_tight_game" = "non_tight_game"))+
  theme(legend.position = "bottom") +
  ylab("Mean Foul Relative Minutes") +
  xlab(NULL)
p2

subset_data <- subset(stat_per_games_last_4, absolute_diff_points <= 15)

# Calculer la moyenne de looser_foul et winner_foul dans les données filtrées
mean_looser_foul <- mean(subset_data$looser_foul, na.rm = TRUE)
mean_winner_foul <- mean(subset_data$winner_foul, na.rm = TRUE)
labels <- c("Looser", "Winner")

plot_mean_foul <- ggplot(data = NULL, aes(x = labels)) +
  geom_bar(stat = "identity", aes(y = c(mean_looser_foul, mean_winner_foul)),
           fill = c("red", "blue")) +
  labs(y = "Moyenne des Foul",
       title = "Moyenne des Foul pour Looser et Winner") +
  theme(axis.text.x = element_blank())  # Pour masquer les étiquettes de l'axe x
print(plot_mean_foul)
