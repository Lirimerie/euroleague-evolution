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
team_stats_plot_over_years <- function(team_stats_season, variable, title, y_label, p1_name) {
  
  best_in_class <- team_stats_season |>
    group_by(year) |>
    filter({{ variable }} == max({{ variable }}, na.rm = TRUE)) |>
    ungroup()
  
  p1 <- ggplot(data = team_stats_season) +
    geom_point(mapping = aes(x = year, y = {{ variable }}, color = Team)) +
    geom_smooth(mapping = aes(x = year, y = {{ variable }})) +
    labs(title = title,
         subtitle = str_wrap("The points represent different teams"),
         x = "Season year",
         y = y_label) +
    ggrepel::geom_label_repel(aes(x = year, y = {{ variable }}, label = Team),
                              data = best_in_class,
                              box.padding = 0.6,
                              size = 2.3) +
    theme(legend.position = "none")
  
  # Assign the plots to custom names
  assign(p1_name, p1, envir = .GlobalEnv)
  
  # Return the custom names for reference
  return(p1 = p1_name)
}

# Example usage:
#plots <- team_stats_plot_over_years(team_stats_season,average_threeS,
#                                    "Average of three pointer made per game",
#                                    "Average Three pointer Made",
#                                    "plot_threeS_over_years"
#                                    )

# Access the custom names for pp and p1 outside the function
#print(plot_threeS_over_years)
