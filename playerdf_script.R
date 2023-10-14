# Load necessary libraries
euroleague<-read.csv("data/euroleague.csv")
library(tidyverse)
library(dplyr)
library(ggplot2)


# Remove information in parentheses from the PLAYINFO column
euroleague <- euroleague |>
  mutate(PLAYINFO = str_remove(PLAYINFO, "\\s*\\([^)]+\\)"))


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

# Source "Main_Wrangle.R" script
source("Main_Wrangle.R")


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
  filter(total_games > 10)

view(stat_per_game_player)


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

# Create win percentage plots
racket_win <- create_win_percentage_plot(top2pt_scorer, year, 
                                         win_percentage, "Racket Win Percentage")

three_win <- create_win_percentage_plot(top3pt_scorer, year, win_percentage, 
                                        "3-pointers win percentage")

defreb_win <- create_win_percentage_plot(top_def_rebound, year, win_percentage,
                                         "Defensive rebound impact on winning")

offreb_win <- create_win_percentage_plot(top_off_rebound, year, win_percentage,
                                         "Offensive rebound impact on winning")


# Load gridExtra library and arrange plots for printing
install.packages("gridExtra")
library(gridExtra)


merged_racketwin <- grid.arrange(racket, racket_win, ncol = 1)
merged_threewin <- grid.arrange(downtown, three_win, ncol = 1)
merged_defwin <- grid.arrange(def_reb, defreb_win, ncol = 1)
merged_offwin <- grid.arrange(off_reb, offreb_win, ncol = 1)

# Print arranged plots
print(merged_threewin)
print(merged_racketwin)
print(merged_defwin)
print(merged_offwin)


# Additional ggplot visualizations
ggplot(top3pt_scorer, aes(x = year, y = three_perc)) +
  geom_line() +         
  geom_point() +  
  labs(x = "Year", y = "3-Point Shooting Percentage") +  
  ggtitle("3-Point Shooting Success Among Leading Scorers in the 3-Point Category Over Time") + 
  theme_minimal() +
  scale_y_continuous(limits = c(0.10,0.6)) +
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

