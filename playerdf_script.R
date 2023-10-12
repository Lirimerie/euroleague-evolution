PlayerFunctiondf <- function(euroleague) {
  playerdf <- euroleague |>
    filter(!is.na(PLAYER)) |>
    group_by(year, PLAYER) |>
    summarise(
      tot_point3 = sum(str_count(PLAYTYPE, "3FGM")),
      tot_point3_missed = sum(str_count(PLAYTYPE, "3FGA")),
      tot_point2 = sum(str_count(PLAYTYPE, "2FGM")),
      tot_point2_missed = sum(str_count(PLAYTYPE, "2FGA")),
      tot_FT = sum(str_count(PLAYTYPE, "FTM")),
      tot_FT_missed = sum(str_count(PLAYTYPE, "FTA")),
      tot_Layup = sum(str_count(PLAYTYPE, "LAYUPMD")),
      tot_Layup_missed = sum(str_count(PLAYTYPE, "LAYUPATT")),
      tot_Dunk = sum(str_count(PLAYTYPE, "DUNK")),
      tot_OffReb = sum(str_count(PLAYTYPE, "O")),
      tot_DefReb = sum(str_count(PLAYTYPE, "D")),
      Team_player = first(TEAM),
      TeamA = first(TeamA),  
      TeamB = first(TeamB)
    )
  
}

stat_per_game <- PlayerFunctiondf(euroleague) |>
  mutate(three_perc = tot_point3 / (tot_point3_missed+tot_point3))
view(stat_per_game)

find_top_player <- function(stat_per_game, column_name) {
  result <- stat_per_game |>
    group_by(year) |>
    filter({{column_name}} == max({{column_name}})) |>
    ungroup()
  return(result)
}

top3pt_scorer <- find_top_player(stat_per_game, tot_point3)
top_def_rebound <- find_top_player(stat_per_game, tot_DefReb)
top_off_rebound <- find_top_player(stat_per_game, tot_OffReb)
top2pt_scorer <- find_top_player(stat_per_game, tot_point2)


create_ggplot <- function(data, x_var, y_var, color, y_limits, title) {
  gg <- ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_line(color = color) +
    geom_point(color = color, size = 3) +
    labs(title = title,
         x = "Year") + 
    theme_minimal() +
    scale_y_continuous(limits = y_limits)
  return(gg)
}

downtown <- create_ggplot(top3pt_scorer, year, 
                          tot_point3, "blue", c(0, 200), 
                          "Maximum 3-Pointers Scored Each Year")
def_reb <- create_ggplot(top_def_rebound, year, 
                         tot_DefReb, "red", c(0, 400), 
                         "Record Defensive Rebound by Year")
def_reb
off_reb <- create_ggplot(top_off_rebound, year, 
                         tot_OffReb, "green", c(0, 400), 
                         "Record Offensive Rebound by Year")
off_reb
racket <- create_ggplot(top2pt_scorer, year, 
                        tot_point2, "black", c(0, 200), 
                        "Record 2-point by Year")
racket


install.packages("gridExtra")
library(gridExtra)

merged_shoot <- grid.arrange(downtown, racket, ncol = 1)
merged_reb <- grid.arrange(off_reb, def_reb)

print(merged_shoot)
print(merged_reb)

ggplot(top3pt_scorer, aes(x = year, y = three_perc)) +
  geom_line() +         
  geom_point() +  
  labs(x = "Year", y = "3-Point Shooting Percentage") +  
  ggtitle("3-Point Shooting Percentage Over Years") + 
  theme_minimal() +
  scale_y_continuous(limits = c(0.10,0.6))

df_threebad <- stat_per_game|>
  filter(!(is.na(three_perc) | tot_point3 == 0))|>
  group_by(year) |>
  slice_min(order_by = three_perc, n = 75)|>
  arrange(year, three_perc)

df_mean_threebad <- df_threebad |>
  group_by(year) |>
  summarise(mean_three_perc = mean(three_perc))

ggplot(df_mean_threebad, aes(x = year, y = mean_three_perc)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean of the 75 players with the lowest 3-point percentage",
       x = "Year",
       y = "Percentage of 3-point success") +
  scale_y_continuous(limits = c(0, 0.4)) +
  theme_minimal() +  # Style minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotation des étiquettes de l'axe x
  scale_x_continuous(breaks = seq(min(df_mean_threebad$year), 
                                  max(df_mean_threebad$year), by = 1))  # Définir les étiquettes de l'axe x

