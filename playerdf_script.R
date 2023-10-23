# Load necessary libraries
euroleague<-read.csv("data/euroleague.csv")
library(tidyverse)
library(dplyr)
library(ggplot2)


# Define a function to process player statistics
PlayerFunctiondf <- function(euroleague) {
  playerdf <- euroleague |>
    filter(!is.na(PLAYER)) |>
    group_by(year, PLAYER) |>
    summarise(
      # Calculate various statistics based on the PLAYTYPE column
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
      Team = first(TEAM),
      TeamA = first(TeamA),  
      TeamB = first(TeamB)
    )
  
}


# Process player statistics
stat_per_game_player <- PlayerFunctiondf(euroleague) |>
  mutate(three_perc = tot_point3 / (tot_point3_missed+tot_point3))|>
  # Additional data wrangling steps
  mutate(Team = toupper(Team))|>
  mutate(Team = recode(Team,
                       "ANADOLU EFES"="ANADOLU EFES ISTANBUL",
                       "ARIS TT BANK" = "ARIS THESSALONIKI",
                       "CIBONA" = "KK CIBONA",
                       "MACCABI TEL AVIV" = "MACCABI ELITE TEL AVIV",
                       "MACCABI FOX TEL AVIV" = "MACCABI ELITE TEL AVIV",
                       "MACCABI ELECTRA TEL AVIV" = "MACCABI ELITE TEL AVIV",
                       "MONTEPASCHI" = "MONTEPASCHI SIENA",
                       "OLYMPIACOS PIRAEUS" = "OLYMPIACOS PIRAEUS B.C.",
                       "ZALGIRIS" = "ZALGIRIS KAUNAS",
                       "ASSECO PROKOM" = "ASSECO PROKOM GDYNIA",
                       "BASKONIA" = "CAJA LABORAL BASKONIA",
                       "TD SYSTEMS BASKONIA VITORIA-GASTEIZ" = "CAJA LABORAL BASKONIA",
                       "BC KHIMKI" = "BC KHIMKI MOSCOW REGION",
                       "UNION OLIMPIJA" = "UNION OLIMPIJA LJUBLJANA",
                       "UNICS" = 	"UNICS KAZAN",
                       "UNICAJA" = "UNICAJA MALAGA",
                       "AXA FC BARCELONA" = "FC BARCELONA",
                       "BELGACOM SPIROU BASKET" = "BELGACOM SPIROU",
                       "BILBAO BASKET" = "CAJA LABORAL BASKONIA",
                       "BROSE BASKETS" = "BROSE BASKETS BAMBERG",
                       "BROSE BAMBERG" = "BROSE BASKETS BAMBERG",
                       "SLUC NANCY BASKET" = "SLUC NANCY",
                       "BASKONIA VITORIA GASTEIZ" = "CAJA LABORAL BASKONIA",
                       "CAJA LABORAL VITORIA" = "CAJA LABORAL BASKONIA",
                       "CRVENA ZVEZDA TELEKOM BELGRADE" = "CRVENA ZVEZDA MTS BELGRADE",
                       "DARUSSAFAKA TEKFEN ISTANBUL" = "DARUSSAFAKA DOGUS ISTANBUL",
                       "EA7 EMPORIO ARMANI MILANO" = "AX ARMANI EXCHANGE MILAN",
                       "EA7 EMPORIO ARMANI MILAN" = "AX ARMANI EXCHANGE MILAN",
                       "MILANO" = "AX ARMANI EXCHANGE MILAN",
                       "AX ARMANI EXCHANGE OLIMPIA MILAN" = "AX ARMANI EXCHANGE OLIMPIA MILAN",
                       "FC BARCELONA REGAL" = "FC BARCELONA",
                       "FC BARCELONA LASSA REGAL" = "FC BARCELONA",
                       "FENERBAHCE DOGUS ISTANBUL" = "FENERBAHCE ISTANBUL",
                       "FENERBAHCE BEKO ISTANBUL" = "FENERBAHCE ISTANBUL",
                       "FENERBAHCE ULKER ISTANBUL" = "FENERBAHCE ISTANBUL",
                       "FB DOGUS" = "FENERBAHCE ISTANBUL",
                       "LABORAL KUTXA VITORIA GASTEIZ" = "LABORAL KUTXA VITORIA", 
                       "MACCABI ELECTRA" = "MACCABI ELITE TEL AVIV",
                       "MACCABI PLAYTIKA TEL AVIV" = "MACCABI ELITE TEL AVIV",
                       "OLYMPIACOS" = "OLYMPIACOS PIRAEUS B.C.",
                       "OLYMPIACOS PIRAEUS BC" = "OLYMPIACOS PIRAEUS B.C.",
                       "PANATHINAIKOS OPAP ATHENS" = "PANATHINAIKOS ATHENS",
                       "PANATHINAIKOS BSA ATHENS" = "PANATHINAIKOS ATHENS",
                       "PANATHINAIKOS OPAP ATHENS" = "PANATHINAIKOS ATHENS",
                       "PANATHINAIKOS SUPERFOODS ATHENS"  = "PANATHINAIKOS ATHENS",
                       "KK ZAGREB CROATIA OSIGURANJE" = "KK ZAGREB" ,
                       "SPIROU BASKET" = "SPIROU CHARLEROI",
                       "KIROLBET BASKONIA VITORIA GASTEIZ" = "CAJA LABORAL BASKONIA",
                       "BIZKAIA BILBAO BASKET" = "CAJA LABORAL BASKONIA",
                       "CAJA LABORAL" = "CAJA LABORAL BASKONIA",
                       "ROANNE" = "CHORALE ROANNE",
                       "ARMANI JEANS MILANO" = "AX ARMANI EXCHANGE MILAN",
                       "BASKETS BAMBERG" = "BROSE BASKETS BAMBERG",
                       "OLIMPIJA" = "UNION OLIMPIJA LJUBLJANA",
                       "BALONCESTO MALAGA" = "UNICAJA MALAGA",
                       "VIRTUS ROMA" = "LOTTOMATICA ROMA",
                       "PARTIZAN BC" = "PARTIZAN IGOKEA",
                       "REGAL FC BARCELONA" = "FC BARCELONA",
                       "FENERBAHCE ULKER" = "FENERBAHCE ISTANBUL",
                       "PARTIZAN MT:S" = "PARTIZAN",
                       "TAU CERAMICA" = "CAJA LABORAL BASKONIA",
                       "GESCRAP BB" = "CAJA LABORAL BASKONIA",
                       "PANATHINAIKOS ATHENS" = "PANATHINAIKOS",
                       "FC BARCELONA" = "FC BARCELONA"
  )
  )|>
  select(-TeamA, -TeamB) |>
  left_join(team_stats_season, by = c("year", "Team")) |>
  mutate(points_made = (tot_point2*2 + tot_point3*3 + tot_FT + tot_Layup*2 + tot_Dunk*2),
         avg_defreb = tot_DefReb / total_games,
         avg_offreb = tot_OffReb / total_games,
         avg_points = points_made / total_games
  ) |>
  filter(total_games > 10)

#Here, no need to merge 2points, layups and dunks as we did not use them to plot
#anything related to the number of 2pts


# Function to find the top player in a given column
find_top_player <- function(stat_per_game_player, column_name) {
  result <- stat_per_game_player |>
    group_by(year) |>
    filter({{column_name}} == max({{column_name}})) |>
    slice(1)|> # Keep only the first row if multiple players have the same max value
    ungroup()
  return(result)
}

top3pt_scorer <- find_top_player(stat_per_game_player, tot_point3)
top_def_rebound <- find_top_player(stat_per_game_player, tot_DefReb)
top_off_rebound <- find_top_player(stat_per_game_player, tot_OffReb)
top2pt_scorer <- find_top_player(stat_per_game_player, tot_point2)


# Define a function to create a ggplot visualization
create_ggplot <- function(data, x_var, y_var, color, y_limits, title) {
  gg <- ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_line(color = color) +
    geom_point(color = color, size = 1) +
    labs(title = title, x = "Year") +
    # Additional plot styling
    theme_minimal() +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1))
  
  return(gg)
}

# Create various ggplot visualizations

downtown <- create_ggplot(top3pt_scorer, year, 
                          tot_point3, "blue", c(0, 200), 
                          "Maximum 3-Pointers Scored Each Year")

def_reb <- create_ggplot(top_def_rebound, year, 
                         tot_DefReb, "red", c(0, 400), 
                         "Record Defensive Rebound by Year")

off_reb <- create_ggplot(top_off_rebound, year, 
                         tot_OffReb, "green", c(0, 400), 
                         "Record Offensive Rebound by Year")

racket <- create_ggplot(top2pt_scorer, year, 
                        tot_point2, "black", c(0, 200), 
                        "Record 2-point by Year")


# Additional data processing
df_threebad <- stat_per_game_player|>
  filter(!(is.na(three_perc) | tot_point3 == 0) & 
           (tot_point3 + tot_point3_missed) >= 70)|>
  group_by(year) |>
  slice_min(order_by = three_perc, n = 75)|>
  arrange(year, three_perc)

df_mean_threebad <- df_threebad |>
  group_by(year) |>
  summarise(mean_three_perc = mean(three_perc))


# Function to create win percentage plots
create_win_percentage_plot <- function(data, x_var, y_var, title) {
  p <- ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_line() +
    geom_point(size = 1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
    theme_minimal() +
    labs(title = title)
  return(p)
}

library(gridExtra)


# Additional ggplot visualizations
ggplot(top3pt_scorer, aes(x = year, y = three_perc)) +
  geom_bar(stat = "identity") + 
  labs(title = "3-Point Shooting Success Among Leading Scorers in the 3-Point Category Over Time",
       x = "Year", 
       y = "3-Point Shooting Percentage") + 
  theme_minimal() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = seq(min(top3pt_scorer$year), 
                                  max(top3pt_scorer$year), by = 1)) 


ggplot(df_mean_threebad, aes(x = year, y = mean_three_perc)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean of the 75 players with the lowest 3-point percentage",
       x = "Year",
       y = "Percentage of 3-point success") +
  scale_y_continuous(limits = c(0, 0.6)) +
  theme_minimal() +  # Style minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotation des étiquettes de l'axe x
  scale_x_continuous(breaks = seq(min(df_mean_threebad$year), 
                                  max(df_mean_threebad$year), by = 1))  # Définir les étiquettes de l'axe x


player_stats_plot_over_years <- function(stat_per_game_player, variable, title, y_label) {
  
  best_in_class_player <- stat_per_game_player |>
    group_by(year) |>
    filter({{ variable }} == max({{ variable }}, na.rm = TRUE)) |>
    ungroup()
  
  p1 <- ggplot(data = stat_per_game_player) +
    geom_point(mapping = aes(x = year, y = {{ variable }}, color = PLAYER)) +
    geom_smooth(mapping = aes(x = year, y = {{ variable }})) +
    labs(title = title,
         subtitle = str_wrap("The points represent different players"),
         x = "Season year",
         y = y_label) +
    theme(legend.position = "none")
  
  p2 <- ggplot(best_in_class_player, aes(x = year, y = win_percentage)) +
    geom_line() +
    geom_point(mapping = aes(x= year, y = win_percentage)) +
    labs(title = "Win percentage of the best statistic player's team",
         x = "Season year",
         y = "Win percentage") +
    ggrepel::geom_label_repel(aes(x = year, y = win_percentage, label = Team),
                              data = best_in_class_player,
                              box.padding = 0.4,
                              size = 1.5) +
    theme(legend.position = "none")
  
  merged <- grid.arrange(p1, p2, ncol = 1)
}

AvgDefReb_plot <- player_stats_plot_over_years(stat_per_game_player, avg_defreb,
                                    "Average Defensive Rebound by Player",
                                    "Number of Rebound")

AvgOffReb_plot <- player_stats_plot_over_years(stat_per_game_player, avg_offreb,
                                               "Average Offensive Rebound by Player",
                                               "Number of Rebound")

AvgNumPts_plot <- player_stats_plot_over_years(stat_per_game_player, avg_points,
                                               "Average Number of Points Scored by Player",
                                               "Number of points")
  
