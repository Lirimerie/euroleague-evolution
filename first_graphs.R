library(ggplot2)

ggplot(data=team_stats_season) + 
  geom_point(mapping = aes(x=year, y=average_points, color = Team))+
  theme(legend.position = "none")

                                            