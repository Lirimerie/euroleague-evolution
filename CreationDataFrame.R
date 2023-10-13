library(tidyverse)

library(dplyr)




process_playtype <- function(df, stat_p_g, playtype, stat_a, stat_b) {
  result <- df %>%
    filter(grepl(playtype, PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  col_name_a <- paste(stat_a, "A", sep = "_")
  col_name_b <- paste(stat_b, "B", sep = "_")
  
  stat_p_g <- stat_p_g %>%
    left_join(result, by = c("gamenumber", "year", TeamA = "TEAM")) %>%
    mutate(!!col_name_a := jjj) %>%
    select(-jjj)
  
  stat_p_g <- stat_p_g %>%
    left_join(result, by = c("gamenumber", "year", TeamB = "TEAM")) %>%
    mutate(!!col_name_b := jjj) %>%
    select(-jjj)
  
  rm(result)
  
  return(stat_p_g)
}





CreationDataFrameTEST <- function(euroleague) {
  stat_per_games <- euroleague %>%
    group_by(gamenumber, year) %>%
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
      tot_foul = sum(str_count(PLAYTYPE, "CM")),
      tot_foul_drawn = sum(str_count(PLAYTYPE, "RV")),
      TeamA = first(TeamA),  
      TeamB = first(TeamB)
    )
  
  stat_per_games <- process_playtype(euroleague, stat_per_games, "3FGM", "ThreeS", "ThreeS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "3FGA", "ThreeF", "ThreeF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "2FGM", "TwoS", "TwoS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "2FGA", "TwoF", "TwoF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "FTM", "FTS", "FTS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "FTA", "FTF", "FTF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "LAYUPMD", "LUS", "LUS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "LAYUPATT", "LUF", "LUF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "DUNK", "DUNK", "DUNK")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "O", "Off_Reb", "Off_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "D", "Deff_Reb", "Deff_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "CM", "Foul_commited", "Foul_commited")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "RV", "Foul_Drawn", "Foul_Drawn")

  
  return(stat_per_games)
}
  
  
  
  









CreationDataFrame_last4 <- function(euroleague) {
  # Filtrer les données où les valeurs de la colonne "minute" sont supérieures à 35
  euroleague <- euroleague %>%
    filter(MINUTE > 35)
  
  # Ensuite, vous pouvez continuer avec le reste du traitement
  stat_per_games <- euroleague %>%
    group_by(gamenumber, year) %>%
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
      tot_foul = sum(str_count(PLAYTYPE, "CM")),
      tot_foul_drawn = sum(str_count(PLAYTYPE, "RV")),
      TeamA = first(TeamA),  
      TeamB = first(TeamB),
    )
  
  stat_per_games <- process_playtype(euroleague, stat_per_games, "3FGM", "ThreeS", "ThreeS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "3FGA", "ThreeF", "ThreeF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "2FGM", "TwoS", "TwoS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "2FGA", "TwoF", "TwoF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "FTM", "FTS", "FTS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "FTA", "FTF", "FTF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "LAYUPMD", "LUS", "LUS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "LAYUPATT", "LUF", "LUF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "DUNK", "DUNK", "DUNK")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "O", "Off_Reb", "Off_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "D", "Deff_Reb", "Deff_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "CM", "Foul_commited", "Foul_commited")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "RV", "Foul_Drawn", "Foul_Drawn")
  
  
  return(stat_per_games)
  
}






CreationDataFrame_37 <- function(euroleague) {
  # Filtrer les données où les valeurs de la colonne "minute" sont supérieures à 35
  euroleague <- euroleague %>%
    filter(MINUTE <= 35)
  
  # Ensuite, vous pouvez continuer avec le reste du traitement
  stat_per_games <- euroleague %>%
    group_by(gamenumber, year) %>%
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
      tot_foul = sum(str_count(PLAYTYPE, "CM")),
      tot_foul_drawn = sum(str_count(PLAYTYPE, "RV")),
      TeamA = first(TeamA),  
      TeamB = first(TeamB),
    )
  
  stat_per_games <- process_playtype(euroleague, stat_per_games, "3FGM", "ThreeS", "ThreeS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "3FGA", "ThreeF", "ThreeF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "2FGM", "TwoS", "TwoS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "2FGA", "TwoF", "TwoF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "FTM", "FTS", "FTS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "FTA", "FTF", "FTF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "LAYUPMD", "LUS", "LUS")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "LAYUPATT", "LUF", "LUF")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "DUNK", "DUNK", "DUNK")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "O", "Off_Reb", "Off_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "D", "Deff_Reb", "Deff_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "CM", "Foul_commited", "Foul_commited")
  stat_per_games <- process_playtype(euroleague, stat_per_games, "RV", "Foul_Drawn", "Foul_Drawn")
  
  
  return(stat_per_games)
  
  return(stat_per_games)
}
