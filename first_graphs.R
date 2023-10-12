library(ggplot2)
library (patchwork)
library(ggrepel)
library(gridExtra)



pp<-ggplot(data=team_stats_season) +
  geom_point(mapping = aes(x=year, y=average_points, color = Team))+
  theme(legend.position = "none")



pp

best_in_class <- team_stats_season|>
  group_by(year)|>  filter(average_threeS == max(average_threeS,
                                                 na.rm = TRUE)) %>%
  ungroup()


p1 <- ggplot(data = team_stats_season) + 
  geom_point(mapping = aes(x = year, y = average_threeS, color = Team)) + 
  geom_smooth(mapping = aes(x = year, y = average_threeS)) +
  labs(title = "Average of three pointer made per game",
       subtitle = str_wrap("The points represent different teams"),
       x= "Season year",
       y= "Average Three pointer Made ")+
  ggrepel::geom_label_repel(aes(x = year, y = average_threeS, label = Team), 
                            data = best_in_class,
                            box.padding = 0.6,
                            size = 2.3)+
  theme(legend.position = "none")


p1



best_in_class_2 <- team_stats_season|>
  group_by(year)|>  filter(average_three_attempts == max(average_three_attempts,
                                                 na.rm = TRUE)) %>%
  ungroup()

p2 <- ggplot(data = team_stats_season) +
  geom_point(mapping = aes(x = year, y = average_three_attempts, color = Team)) + 
  geom_smooth(mapping = aes(x = year, y = average_three_attempts)) +
  labs(title = "Average of three pointer attempt per game (Failed and Made)",
       subtitle = str_wrap("The points represent different teams"),
       x= "Season year",
       y= "Average Three pointer attempt")+
  ggrepel::geom_label_repel(aes(x = year, y = average_three_attempts, 
                                label = Team), 
                            data = best_in_class_2,
                            box.padding = 0.6,
                            size = 2.3)+
  theme(legend.position = "none")

p2






p3 <- ggplot(data = team_stats_season) +
  geom_point(mapping = aes(x = year, 
                           y = (average_threeS /(average_threeS+
                                                   average_three_attempts)),
                           color = Team)) +
  geom_smooth(mapping = aes(x = year, 
                            y = (average_threeS /(average_threeS+
                                                    average_three_attempts)))) +
  labs(title = "Percentage of succes for three pointer per game ",
       subtitle = str_wrap("The points represent different teams"),
       x= "Season year",
       y= "Percentage of succes")+
  theme(legend.position = "none")

p3


team_stats_season <- team_stats_season %>%
  arrange(year, desc(win_percentage))

top_teams <- team_stats_season %>%
  group_by(year) %>%
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





p4 <- ggplot(data = team_stats_season) + 
  geom_point(data = team_stats_season %>% filter(year >= 2007 & year <= 2015),
             aes(x = average_three_attempts, y = win_percentage), color = "black") + 
  geom_point(data = team_stats_season %>% filter(year >= 2016 & year <= 2020),
             aes(x = average_three_attempts, y = win_percentage), color = "blue") +
  geom_smooth(data = team_stats_season %>% filter(year >= 2007 & year <= 2015),
              aes(x = average_three_attempts, y = win_percentage), color = "black") +
  geom_smooth(data = team_stats_season %>% filter(year >= 2016 & year <= 2020),
              aes(x = average_three_attempts, y = win_percentage), color = "blue") +
  labs(title = "Average of three-pointer attempts per game",
       subtitle = str_wrap("The points represent different teams"),
       x = "Three-point attempts",
       y = "Win percentage") +
  theme(legend.position = "none")


p4

source("Function.R")
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
