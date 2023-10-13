library(ggplot2)
library (patchwork)
library(ggrepel)
library(gridExtra)
source("Function.R")
#Statistics per team about three pointers
plots <- team_stats_plot_over_years(team_stats_season,average_threeS,
                                    "Average of three pointer made per game",
                                    "Average Three pointer made by the team",
                                    "plot_threeS_over_years"
                                    )
print(plot_threeS_over_years)

plots <- team_stats_plot_over_years(team_stats_season,average_three_attempts,
                                    "Average of three pointer attempted per game",
                                    "Average Three pointer made by the team",
                                    "plot_three_attempts_over_years"
)
print(plot_three_attempts_over_years)

plots <- team_stats_plot_over_years(team_stats_season,avg_3pt_accuracy,
                                    "Average accuracy of three per game",
                                    "Average accruracy of three pointers",
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
                                    "Average two pointer made by the team",
                                    "plot_two_attempts_over_years"
)
print(plot_two_attempts_over_years)

plots <- team_stats_plot_over_years(team_stats_season,avg_2pt_accuracy,
                                    "Average accuracy of two per game",
                                    "Average accruracy of two pointers",
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
top_winners<-ggplot(top_teams, aes(x = as.factor(year), y = win_percentage, fill = Team)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Team), position = position_dodge(width = 0.9),
            angle = 90, hjust = 1, vjust = 0.5) + # Rotation et ajustements
  labs(title = "Les 3 équipes avec le meilleur win percentage chaque année",
       x = "Année",
       y = "Win Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")
  
top_winners
grid.arrange(top_winners,p1,nrow=2)


Effect_3Attempts_on_Win <- plot_effect_on_win(team_stats_df, 
                                              three_attempts, 
                                              "Three-point attempts")

print(Effect_3Attempts_on_Win)

Effect_3accuracy_on_Win <- plot_effect_on_win(team_stats_df, 
                                              three_accuracy, 
                                              "Three-point accuracy")

print(Effect_3accuracy_on_Win)


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
