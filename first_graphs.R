#Make a ton of graphs, but many of them were finally not used as
# the report was already long enough
source("Function.R")
library(gridExtra)
#Statistics per team about three pointers

#this script is where we create all the plot that we wanted
#We added "#" to the lines making the plot as they make the creation
#of the report really time-consuming



# all the plots calling the function team_stats_plot_over_years
#plot the evolution of a certain statistic over the years

plots <- team_stats_plot_over_years(team_stats_season,
                                    average_threeS,
                                    "It started well before 2016",
                                    "Average three-pointer made by a team in a season",
                                    "plot_threeS_over_years"
)
plot_threeS_over_years <- plot_threeS_over_years +
  labs(subtitle = "There has been a clear increase in 
the number of three-pointer made per game,and the best teams are usually the same")
plot_threeS_over_years

plots <- 
  team_stats_plot_over_years(team_stats_season,
                             average_three_attempts,
                             '"We should shoot from far away"',
                             "Average three-pointer attempted 
by the team, per season",
                             "plot_three_attempts_over_years"
  )

plot_three_attempts_over_years<- plot_three_attempts_over_years +
  labs(subtitle = "There has also been a clear increase in thenumber of 
three-pointer attempted, and they'e not necessarily the same teams as before")
#plot_three_attempts_over_years

plots <- team_stats_plot_over_years(
  team_stats_season,
  avg_3pt_accuracy,
  "Why bother to shoot better, just shoot more",
  "Average accuracy of a team while 
shooting three-pointers, per season",
  "plot_three_accuracy_over_years"
)


plot_three_accuracy_over_years <- plot_three_accuracy_over_years +
  labs(subtitle = "The increase in accuracy is relatively small 
compared to the increase in attempts")
#plot_three_accuracy_over_years

#Statistics per team about two pointers
plots <- team_stats_plot_over_years(
  team_stats_season,average_twoS,
  "Did nothing change?",
  "Average number of two-pointer made by the team, 
  per season",
  "plot_twoS_over_years"
)

plot_twoS_over_years <- plot_twoS_over_years +
  labs(subtitle = "At first, we would think that the players didn't change their way to play")
#plot_twoS_over_years

plots <- team_stats_plot_over_years(
  team_stats_season,average_two_attempts,
  "Two-pointers aren't sensational enough anymore",
  "Average number of two-pointer attempted by a team,
  per season",
  "plot_two_attempts_over_years")

plot_two_attempts_over_years <- plot_two_attempts_over_years +
  labs(subtitle = "Players started to care less about two-pointers after 2012")
#plot_two_attempts_over_years

plots <- team_stats_plot_over_years(team_stats_season,avg_2pt_accuracy,
                                    "Two-pointers are deadlier than ever",
                                    "Average accuracy of two pointers
by a team, per season",
                                    "plot_two_accuracy_over_years"
)
plot_two_accuracy_over_years <- plot_two_accuracy_over_years+
  labs(subtitle = "The players shoot less but make them more often
There is also less unity in the name of the best teams at it")
#print(plot_two_accuracy_over_years)


#Statistics per team about points made
plots <- team_stats_plot_over_years(
  team_stats_season,
  average_points,
  "The average of points made per game is not a shock",
  "Average points made by the team, per season",
  "plot_points_over_years"
)

plot_points_over_years <- plot_points_over_years +
  labs(subtitle = "People make the same amount of two-pointers, and more three-pointers, 
so it makes sense")
#plot_points_over_years

#Statistics per team about rebounds made
plots <- team_stats_plot_over_years(team_stats_season,average_off_reb,
                                    "Some teams really love offensive rebound",
                                    "Average offensive rebounds made 
by the team, per season",
                                    "plot_offreb_over_years"
)
plot_offreb_over_years <- plot_offreb_over_years+
  labs(subtitle = "From a lot of small averages and a few outliers
to a lot of bigger averages and no real outlier")
#plot_offreb_over_years

plots <- team_stats_plot_over_years(
  team_stats_season,
  average_def_reb,
  "The average of defensive rebounds made per game",
  "Average def reb made by the team, per season",
  "plot_defreb_over_years"
)


plot_defreb_over_years <- plot_defreb_over_years +
  labs(subtitle = "is odd enough for us to doubt about it")
#plot_defreb_over_years

#Statistics per team about rebounds made
plots <- team_stats_plot_over_years(team_stats_season,average_FT_attempts,
                                    "What's the point of making a foul?",
                                    "Average number of Free-Throws made by 
the team, per season",
                                    "plot_FT_attempts_over_years"
)
plot_FT_attempts_over_years <- plot_FT_attempts_over_years +
  labs(subtitle = "The average number of Free-Throws shot by game decreases over the years")
#plot_FT_attempts_over_years

plots <- team_stats_plot_over_years(team_stats_season,avg_FT_accuracy,
                                    "Is there any interest to make a foul?",
                                    "Average accruracy of FT pointers",
                                    "plot_FT_accuracy_over_years"
)
plot_FT_accuracy_over_years <- plot_FT_accuracy_over_years +
  labs(subtitle = "The average accuracy at the free-throw doesn't encourage the defender
to foul the opponent")
#plot_FT_accuracy_over_years

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
  labs(title = "Top 3 teams with the highest win percentage",
       x = "Year",
       y = "Win Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")

#top_winners



#The Following measure effects of variable "x" on the probability
# to win, or on the variation of the difference between two teams



Effect_3Attempts_on_Win <- plot_separated_effect_2016(
  team_stats_df,
  three_attempts,
  "Three-point attempts",
  "A good strategy turned into a bad one?",
  "Shooting a lot of three-pointer was a good strategy, until it wasn't")+
  coord_cartesian( ylim = c(0.2, 0.8))

#Effect_3Attempts_on_Win

Effect_3Attempts_on_Win_no_distinction <- plot_effect_game(
  team_stats_df,
  three_attempts, 
  "Three-point attempts",
  "What's the point?",
  "Shooting a lot of three-pointer did not mean anything")+
  coord_cartesian( ylim = c(0.2, 0.8))

#Effect_3Attempts_on_Win_no_distinction

Effect_3_made_on_Win_distinction <- plot_separated_effect_2016(
  team_stats_df,
  ThreeS,
  "Three-points made",
  "A three-pointer does not always count the same",
  "Each team shoots more often behind the three-point line")+
  coord_cartesian( ylim = c(0.1, 0.9))

#Effect_3_made_on_Win_distinction

Effect_3Attempts_on_Win_first37 <-
  plot_separated_effect_2016(
    team_stats_df_37,
    three_attempts,
    "Three-point attempts in the first 37 minutes of the game",
    "...Or perhaps distressed three-pointers have hidden the reality",
    "Shooting more three-pointers is still a good strategy,the distressed teams shoot so many 
three-pointers  at the end of the game that it mislead us")

Effect_3Attempts_on_Win_first37 <- Effect_3Attempts_on_Win_first37 +
  coord_cartesian(xlim = c(10,30), ylim = c(0.2, 0.8))
#Effect_3Attempts_on_Win_first37

Effect_3Attempts_on_Win_last_4 <-
  plot_separated_effect_2016(
    team_stats_df_4,
    three_attempts,
    "Three-point attempts in the last 4 minutes of the game",
    "Here is what falsed the results",
    "The more you attempt three-pointers in the last minutes, the smaller your chances to win were")+
  coord_cartesian(xlim = c(0,9), ylim = c(0.2, 0.8))

#Effect_3Attempts_on_Win_last_4

Effect_3Attempts_on_Win_last_4_not_desperate <-
  plot_separated_effect_2016(
    team_stats_df_4_not_desperate,
    three_attempts,
    "Three-point attempts in the last 4 minutes of a tight game",
    "A good strategy does not mean that it works all the time",
    "The more you attempt three-pointers, the smaller your chances to win are
But there is always a pretty big chance")+
  coord_cartesian(xlim = c(0,9), ylim = c(0,1))

#Effect_3Attempts_on_Win_last_4_not_desperate

Effect_3accuracy_on_Win <- plot_effect_game(
  team_stats_df, 
  three_accuracy, 
  "Three-point accuracy",
  "What is the point of shooting it, if you don't make it",
  "At the end, the accuracy seems to be the best factor to estimate chances to win") +
  coord_cartesian( xlim= c(0.1,0.6), ylim = c(0, 1))

#Effect_3accuracy_on_Win

Effect_2Attempts_on_Win <- plot_separated_effect_2016(
  team_stats_df,
  two_attempts, 
  "Two-point attempts",
  "Shooting two-pointers is a better idea",
  "Perhaps teams who are sure to win shoot more two-pointers")+
  coord_cartesian(ylim = c(0.2, 0.8))

#Effect_2Attempts_on_Win


Effect_2accuracy_on_Win <- plot_effect_game(
  team_stats_df, 
  two_accuracy, 
  "Two-point accuracy",
  "Shooting two-pointers is a good idea too",
  "At the end, the accuracy seems to be the best factor to estimate chances to win")+
  coord_cartesian( xlim = c(0.3, 0.75), ylim = c(0,1))

#Effect_2accuracy_on_Win

Effect_2_made_on_Win <- plot_effect_game(
  team_stats_df, 
  TwoS, 
  "Two-point accuracy",
  "Shooting two-pointers is a good idea too",
  "At the end, the accuracy seems to be the best factor to estimate chances to win")+
  coord_cartesian(ylim = c(0,1))

#Effect_2_made_on_Win


#effect of the total of points made or points taken were not that interesting

Effect_Foul_on_Win <- plot_effect_game(
  team_stats_df, 
  Fouls_commited, 
  "Fouls commited",
  "Fouls are usually a bad idea",
  "after five fouls you would be out and you might give them free-throws")+
  coord_cartesian(ylim = c(0.2, 0.8))



Effect_Foul_on_Win_last_4 <-   #can't be used because it's biased
  plot_effect_game(team_stats_df_4_not_desperate,
                   Fouls_commited,
                   "Fouls commited in the last four minutes",
                   "If the game is tight, you better foul a bit",
                   "6 fouls or less improve the chances to win")+
  coord_cartesian(xlim = c(0,8), ylim = c(0.1, 1))



Effect_Foul_on_Gap_last_4_not_Desperate <- 
  plot_effect_game(team_stats_df_4_not_desperate,
                   Fouls_commited,
                   "Fouls commited in the last four minutes",
                   "Fouls do not do miracles",
                   "It can help gain a few points, but not that much",
                   Variation_Of_Gap,
                   "Change in difference of points")+
  coord_cartesian() +
  geom_hex(aes(Fouls_commited,Variation_Of_Gap))+
  scale_fill_gradientn(colors = plasma(100))+
  coord_fixed()+
  coord_cartesian(xlim = c(0, 13))






Effect_Three_accuracy_on_Gap_last_4_not_Desperate <- 
  plot_effect_game(team_stats_df_4_not_desperate,
                   three_accuracy,
                   "Accuracy at the three-point line in the last four minutes",
                   "Either you shoot more precisely, or you don't",
                   "If a team has 10 more points than the other, it might not need
to shoot from so far",
                   Variation_Of_Gap,
                   "change in difference of points") +
  geom_hex(aes(three_accuracy,Variation_Of_Gap))+
  scale_fill_gradientn(colors = plasma(10))+
  coord_fixed()+
  coord_cartesian(xlim = c(0, 1))+
  coord_cartesian()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")




Effect_Three_attempts_on_Gap_last_4_not_Desperate <- 
  plot_effect_game(team_stats_df_4_not_desperate,
                   three_attempts,
                   "Three attempts commited in the last four minutes",
                   "We need quality, not quantity",
                   "Shooting three-pointers does not do miracles",
                   Variation_Of_Gap,
                   "change in difference of points")+
  coord_cartesian()



Effect_Three_made_on_Gap_last_4_not_Desperate <-
  plot_effect_game(team_stats_df_4_not_desperate,
                   Fouls_commited,
                   "Fouls comitted in the last four minutes of a tight game by a team",
                   "Fouls and Three-pointers are correlated",
                   "The strategy consisting of fouling the weakest opponent is style viable",
                   ThreeS,
                   "Number of three-pointers made")+
  coord_cartesian()





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

#grid.arrange(points07, points08, points09, points10, ncol = 2)
#grid.arrange(points11, points12, points13, points14, ncol = 2)
#grid.arrange(points15, points16, points17, points18, ncol = 2)
#grid.arrange(points19, points20, ncol = 2)




p1 <- ggplot(data = subset(stat_per_games_last_4, absolute_diff_points < 30),
             aes(x = absolute_diff_points, y = tot_foul)) +
  geom_hex() +
  coord_fixed()+
  labs(title = "Fouls in the last 4 minutes",
       subtitle = "The number of foul decrease if the game is less tight",
       x = " Difference of points when we enter in the last 4 minutes",
       y = "Number of fouls")+
  scale_fill_viridis_c(option="magma", begin=0.1, end=0.9,
                       direction = -1)+
  geom_smooth(data = subset(stat_per_games_last_4, absolute_diff_points < 30),
              aes(x = absolute_diff_points, y = tot_foul)) 

#p1



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

#plot_foul



mean_foul_relative_minutes_first37 <- 
  mean(stat_per_games_first37$foul_relative_minutes, na.rm = TRUE)
mean_foul_relative_minutes_last4_serré <- 
  mean(stat_per_games_last_4$foul_relative_minutes
       [stat_per_games_last_4$absolute_diff_points <= 10],
       na.rm = TRUE)
mean_foul_relative_minutes_last4_pas_serré <-
  mean(stat_per_games_last_4$foul_relative_minutes
       [stat_per_games_last_4$absolute_diff_points > 10], 
       na.rm = TRUE)
df <- data.frame(Dataset = c("first part game", "tight game", "non tight game"),
                 MeanFoulRelativeMinutes = 
                   c(mean_foul_relative_minutes_first37,
                     mean_foul_relative_minutes_last4_serré,
                     mean_foul_relative_minutes_last4_pas_serré))
p2 <- ggplot(data = df, aes(x = Dataset,
                            y = MeanFoulRelativeMinutes, fill = Dataset)) +
  geom_bar(stat = "identity") +
  labs(title = "Foul per minute",
       subtitle = 
         "Compare first part of the game(blue) with the last 4 minutes(red)")+
  scale_fill_manual(values = c("first part game" = "blue",
                               "tight game" = "red",
                               "non tight game" = "lightcoral"),
                    labels = c("first_part_game" = "first part game",
                               "tight_game" = "tight game",
                               "non_tight_game" = "non tight game"))+
  theme(legend.position = "bottom") +
  ylab("Mean Foul Relative Minutes") +
  xlab(NULL)

#p2

subset_data <- subset(stat_per_games_last_4, absolute_diff_points <= 10)

# Calculer la moyenne de looser_foul et winner_foul dans les données filtrées
mean_looser_foul <- mean(subset_data$looser_foul, na.rm = TRUE)
mean_winner_foul <- mean(subset_data$winner_foul, na.rm = TRUE)
labels <- c("Looser", "Winner")

plot_mean_foul <- ggplot(data = NULL, aes(x = labels)) +
  geom_bar(stat = "identity", aes(y = c(mean_looser_foul, mean_winner_foul)),
           fill = c("red", "blue")) +
  labs(y = "Mean of fouls",
       title = "Mean of fouls in the last 4 minutes",
       subtitle = "The loosing team tends to do more fouls")
