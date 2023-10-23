create_team_points_plot <- function(Year) {
  df <- team_stats_season |> filter(year == Year)
  df_sorted <- df |> arrange(desc(average_points))
  top_teams <- head(df_sorted, 5)
  bottom_teams <- tail(df_sorted, 5)
  combined_teams <- rbind(top_teams, bottom_teams)
  
  # Tri des combined_teams
  combined_teams <- combined_teams |> arrange(desc(average_points))
  
  plot <- ggplot(combined_teams, 
                 aes(x = reorder(Team, -average_points),
                     y = average_points, 
                     fill = ifelse(rank(average_points) <= 5,
                                   "Bottom 5", "Top 5"))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Top 5" = "green", "Bottom 5" = "red")) +
    labs(title = paste("Top 5 and Bottom 5 Teams in",
                       Year, "by Average Points"),
         x = "Team",
         y = "Average Points",
         fill = "") +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    theme_minimal()
  
  return(plot)
}
team_stats_plot_over_years <- function(team_stats_season,
                                       variable,
                                       title,
                                       y_label,
                                       plot_name) {
  
  best_in_class <- team_stats_season |>
    group_by(year) |>
    filter({{ variable }} == max({{ variable }}, na.rm = TRUE)) |>
    ungroup()
  
  p1 <- ggplot(data = team_stats_season) +
    geom_point(mapping = aes(x = year, y = {{ variable }}, color = Team)) +
    geom_smooth(mapping = aes(x = year, y = {{ variable }}),color = "darkgray",
                se = FALSE,) +
    labs(title = title,
         x = "Year at which the season begins",
         y = y_label) +
    geom_label_repel(aes(x = year, y = {{ variable }}, label = Team),
                     data = best_in_class,
                     box.padding = 0.6,
                     size = 2.3) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "gray80", size = 0.2)
          )
  #For some reasons, didn't work calling theme_minimal()
                              
  # Assign the plots to custom names
  assign(plot_name, p1, envir = .GlobalEnv)
  
  # Return the custom names for reference
  return(p1 = plot_name)
}

# Example usage:
#plots <- team_stats_plot_over_years(team_stats_season,average_threeS,
#                                    "Average of three pointer made per game",
#                                    "Average Three pointer Made",
#                                    "plot_threeS_over_years"
#                                    )

# Access the custom names for pp and p1 outside the function
#print(plot_threeS_over_years)

#The following function allows to plot any graph comparing 2007-2016 to 2016-2020
plot_separated_effect_2016 <- function(data,
                                       variable,
                                       x_label,titre = "",sous_titre = "",
                                       y_variable= winner,
                                       y_label = "Win percentage"
                                        ) {
  p <- ggplot(data = data) +
    geom_smooth(data = data,
                aes(x = {{ variable }},
                    y = {{ y_variable }},
                    color = When,
                    ),
                     alpha = 0.15 ) +
    labs(title = titre,
         subtitle = sous_titre,
         x = x_label,
         y = y_label) +
    theme_minimal()+
    theme(legend.position = "bottom") 
  
  return(p)
}

# Example usage:
#p4 <- plot_separated_effect_2016(team_stats_df, three_attempts, "Three-point attempts")
#p4

# The following function just does a regression in general
plot_effect_game <- function(data, variable, x_label,
                             titre = "", sous_titre = "",
                             y_variable = winner, y_label = "Win percentage") {
  label_text = "General trend over the years"
  p <- ggplot(data = data) +
    geom_smooth(data = data,
                aes(x = {{ variable }}, y = {{ y_variable }}),
                color = "black", alpha = 0.8) +
    labs(title = titre,
         subtitle = sous_titre,
         x = x_label,
         y = y_label) +
    theme(legend.position = "bottom") +
 # coord_cartesian( ylim = c(0, 1)) +
    theme_minimal()
  
  return(p)
}
# Example usage:
#p5 <- plot_effect_game(team_stats_df, three_accuracy, "Three-point attempts")
#p5
#p6 <- plot_effect_game(team_stats_df, three_attempts, "Three-point attempts",
#                       Fouls_commited, "Fouls commited")
#p6
