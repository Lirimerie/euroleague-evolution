generate_points_plot <- function(year) {
  df <- team_stats_season %>% filter(year == year)
  df_sorted <- df %>% arrange(desc(average_points))
  top_teams <- head(df_sorted, 5)
  bottom_teams <- tail(df_sorted, 5)
  combined_teams <- rbind(top_teams, bottom_teams)
  
  points_plot <- ggplot(combined_teams, aes(x = reorder(Team, -average_points), y = average_points, fill = ifelse(rank(average_points) <= 5, "Bottom 5", "Top 5"))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Top 5" = "green", "Bottom 5" = "red")) +  
    labs(title = paste("Top 5 and Bottom 5 Teams in", year, "by Average Points"),
         x = "Team",
         y = "Average Points",
         fill = "") +
    theme(axis.text.x = element_text(angle = 40, hjust = 1)
    )
  
  return(points_plot)
}
