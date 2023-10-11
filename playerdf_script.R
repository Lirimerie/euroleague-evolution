euroleague<-read.csv("data/euroleague.csv")
library(tidyverse)
library(dplyr)
library(ggplot2)

euroleague <- euroleague |>
  mutate(PLAYINFO = str_remove(PLAYINFO, "\\s*\\([^)]+\\)"))

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
      Team = first(TEAM)
    )
  
}

stat_per_game <- PlayerFunctiondf(euroleague) |>
  mutate(three_perc = tot_point3 / (tot_point3_missed+tot_point3))
view(stat_per_game)

top3pt_scorer <- stat_per_game |>
  group_by(year) |>
  mutate(three_perc = tot_point3 / (tot_point3_missed+tot_point3))|>
  filter(tot_point3 == max(tot_point3))|>
  ungroup()

top_def_rebound <- vecteur |>
  group_by(year) |>
  filter(tot_DefReb == max(tot_DefReb)) |>
  ungroup()

top_off_rebound <- vecteur |>
  group_by(year) |>
  filter(tot_OffReb == max(tot_OffReb)) |>
  ungroup()

top2pt_scorer <- vecteur |>
  group_by(year) |>
  filter(tot_point2 == max(tot_point2))|>
  ungroup()

downtown <- ggplot(top3pt_scorer, aes(x = year, y = tot_point3)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 3) +
  labs(title = "Maximum 3-Pointers Scored Each Year",
       x = "Year",
       y = "Maximum 3-Pointers Scored") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,200))

def_reb <- ggplot(top_def_rebound, aes(x = year, y = tot_DefReb)) +
  geom_line(color = "red") +
  geom_point(color = "red", size = 3) +
  labs(title = "Record Defensive Rebound by Year",
       x = "Year",
       y = "Number of defensive rebound") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,400))

off_reb <- ggplot(top_off_rebound, aes(x = year, y = tot_OffReb)) +
  geom_line(color = "green") +
  geom_point(color = "green", size = 3) +
  labs(title = "Record Offensive Rebound by Year",
       x = "Year",
       y = "Number of offensive rebound") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,400))

racket <- ggplot(top2pt_scorer, aes(x = year, y = tot_point2)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Record 2-point by Year",
       x = "Year",
       y = "Number of 2-point scored") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,200))

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
