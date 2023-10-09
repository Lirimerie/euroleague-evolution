library(ggplot2)
library (patchwork)

pp<-ggplot(data=team_stats_season) + 
  geom_point(mapping = aes(x=year, y=average_points, color = Team))+
  theme(legend.position = "none")

p1 <- ggplot(data = team_stats_season) + 
  geom_point(mapping = aes(x = year, y = average_threeS, color = Team)) + 
  geom_smooth(mapping = aes(x = year, y = average_threeS)) +
  theme(legend.position = "none")

p2 <- ggplot(data = team_stats_season) +
  geom_point(mapping = aes(x = year, y = average_three_attempts, color = Team)) + 
  geom_smooth(mapping = aes(x = year, y = average_three_attempts)) +
  theme(legend.position = "none")


p3 <- ggplot(data = team_stats_season) +
  geom_point(mapping = aes(x = year, y = (average_threeS /(average_threeS+average_three_attempts)),
                           color = Team)) +
  geom_smooth(mapping = aes(x = year, y = (average_threeS /(average_threeS+average_three_attempts)))) +
  theme(legend.position = "none")



library(tidyverse)

# Supposons que votre dataframe s'appelle "df"
# Assurez-vous que "year" et "average_points" sont les noms corrects des colonnes.

# 1. Filtrez pour l'année 2007
df_2007 <- team_stats_season %>% filter(year == 2007)

# 2. Triez par "average_points" en ordre décroissant
df_2007_sorted <- df_2007 %>% arrange(desc(average_points))

# 3. Sélectionnez les 5 premières lignes avec les plus grands "average_points"
top_5_teams <- head(df_2007_sorted, 5)

# 4. Sélectionnez les 5 premières lignes avec les plus petits "average_points"
bottom_5_teams <- tail(df_2007_sorted, 5)

# 5. Fusionnez les données des 5 équipes les plus grandes et les 5 équipes les plus petites
combined_teams <- rbind(top_5_teams, bottom_5_teams)

ggplot(combined_teams, aes(x = reorder(Team, -average_points), y = average_points, fill = ifelse(rank(average_points) <= 5, "Bottom 5", "Top 5"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Top 5" = "green", "Bottom 5" = "red")) +  # Définir les couleurs
  labs(title = "Top 5 and Bottom 5 Teams in 2007 by Average Points",
       x = "Team",
       y = "Average Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p3
p2                                            
p1
