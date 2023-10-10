library(ggplot2)
library (patchwork)
library(ggrepel)




pp<-ggplot(data=team_stats_season) +
  geom_point(mapping = aes(x=year, y=average_points, color = Team))+
  theme(legend.position = "none")





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




team_stats_season <- team_stats_season %>%
  arrange(year, desc(win_percentage))

top_teams <- team_stats_season %>%
  group_by(year) %>%
  slice_head(n = 3)

# Créez le graphique à barres et ajoutez le texte des noms d'équipe en position verticale
ggplot(top_teams, aes(x = as.factor(year), y = win_percentage, fill = Team)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Team), position = position_dodge(width = 0.9), angle = 90, hjust = 1.6, vjust = 0.5) + # Rotation et ajustements
  labs(title = "Les 3 équipes avec le meilleur win percentage chaque année",
       x = "Année",
       y = "Win Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")
  
  

# Créer un dataframe avec les trois meilleures équipes en termes de win_percentage par année

# Créer un histogramme





# Supposons que votre dataframe s'appelle "df"
# Assurez-vous que "year" et "average_points" sont les noms corrects des colonnes.


df_2007 <- team_stats_season %>% filter(year == 2007)


df_2007_sorted <- df_2007 %>% arrange(desc(average_points))


top_5_teams <- head(df_2007_sorted, 5)


bottom_5_teams <- tail(df_2007_sorted, 5)


combined_teams <- rbind(top_5_teams, bottom_5_teams)

points07<-ggplot(combined_teams, aes(x = reorder(Team, -average_points), y = average_points, fill = ifelse(rank(average_points) <= 5, "Bottom 5", "Top 5"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Top 5" = "green", "Bottom 5" = "red")) +  
  labs(title = "Top 5 and Bottom 5 Teams in 2007 by Average Points",
       x = "Team",
       y = "Average Points",
       fill = "") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))


df_2008 <- team_stats_season %>% filter(year == 2008)


df_2008_sorted <- df_2008 %>% arrange(desc(average_points))


top_5_teams <- head(df_2008_sorted, 5)


bottom_5_teams <- tail(df_2008_sorted, 5)


combined_teams <- rbind(top_5_teams, bottom_5_teams)

points08<-ggplot(combined_teams, aes(x = reorder(Team, -average_points), y = average_points, fill = ifelse(rank(average_points) <= 5, "Bottom 5", "Top 5"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Top 5" = "green", "Bottom 5" = "red")) +  
  labs(title = "Top 5 and Bottom 5 Teams in 2008 by Average Points",
       x = "Team",
       y = "Average Points",
       fill="") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

library(gridExtra)


grid.arrange(points07, points08, points08, points08, points08, points08, ncol = 3)

p1
  
p2
p3

