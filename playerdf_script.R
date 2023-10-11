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

stat_per_game <- PlayerFunctiondf(euroleague)
view(stat_per_game)

top3pt_scorer <- stat_per_game |>
  group_by(year) |>
  mutate(three_perc = tot_point3 / (tot_point3_missed+tot_point3))|>
  filter(tot_point3 == max(tot_point3))|>
  ungroup()

top_def_rebound <- stat_per_game |>
  group_by(year) |>
  filter(tot_DefReb == max(tot_DefReb)) |>
  ungroup()

top_off_rebound <- stat_per_game |>
  group_by(year) |>
  filter(tot_OffReb == max(tot_OffReb)) |>
  ungroup()

top2pt_scorer <- stat_per_game |>
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


