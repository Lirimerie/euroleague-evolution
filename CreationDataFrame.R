library(tidyverse)

library(dplyr)


# Define a function called process_playtype that takes a dataframe (df),
#a per game statistics dataframe (stat_p_g),
# a specified playtype, and two statistics labels (stat_a and stat_b) as input.

process_playtype <- function(df, stat_p_g, playtype, stat_a, stat_b) {
  
  result <- df |>
    #choose the rows where the playtype that we are interested in appears
    filter(grepl(playtype, PLAYTYPE)) |>
    #group by yeear gamenumber and team 
    group_by(year, gamenumber, TEAM)|>
    #count the number of rows where this playtype appears 
    summarize(jjj = n())
  
  #name of the news columns 
  col_name_a <- paste(stat_a, "A", sep = "_")
  col_name_b <- paste(stat_b, "B", sep = "_")
  
  #add the column to to the dataframe and name it for team A and B
  stat_p_g <- stat_p_g |>
    left_join(result, by = c("gamenumber", "year", TeamA = "TEAM")) |>
    mutate(!!col_name_a := jjj) |>
    select(-jjj)
  
  stat_p_g <- stat_p_g |>
    left_join(result, by = c("gamenumber", "year", TeamB = "TEAM")) |>
    mutate(!!col_name_b := jjj) |>
    select(-jjj)
  
  #Remove the "result" dataframe from memory to free up memory.
  rm(result)
  
  return(stat_p_g)
}




# creates the dataframe from euroleague and count all the statistics 
# for all the game not for a specific team 

CreationDataFrameTEST <- function(euroleague) {
  stat_per_games <- euroleague |>
    group_by(gamenumber, year)|>
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
  
  #use the function that we create before to compute the statistics for each team 
  
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "3FGM", "ThreeS", "ThreeS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "3FGA", "ThreeF", "ThreeF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "2FGM", "TwoS", "TwoS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "2FGA", "TwoF", "TwoF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "FTM", "FTS", "FTS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "FTA", "FTF", "FTF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "LAYUPMD", "LUS", "LUS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "LAYUPATT", "LUF", "LUF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "DUNK", "DUNK", "DUNK")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "O", "Off_Reb", "Off_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "D", "Deff_Reb", "Deff_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "CM", "Foul_commited", "Foul_commited")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "RV", "Foul_Drawn", "Foul_Drawn")

  
  return(stat_per_games)
}
  

#we create the same dataframe but taking into account only the last 4 minutes 

CreationDataFrame_last4 <- function(euroleague) {
  euroleague <- euroleague |>
    filter(MINUTE > 36)
  
  stat_per_games <- euroleague |>
    group_by(gamenumber, year) |>
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
  
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "3FGM", "ThreeS", "ThreeS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "3FGA", "ThreeF", "ThreeF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "2FGM", "TwoS", "TwoS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "2FGA", "TwoF", "TwoF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "FTM", "FTS", "FTS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "FTA", "FTF", "FTF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "LAYUPMD", "LUS", "LUS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "LAYUPATT", "LUF", "LUF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "DUNK", "DUNK", "DUNK")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "O", "Off_Reb", "Off_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "D", "Deff_Reb", "Deff_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "CM", "Foul_commited", "Foul_commited")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "RV", "Foul_Drawn", "Foul_Drawn")
  
  
  return(stat_per_games)
  
}




#same thing but here for the first 37 minutes of the game 

CreationDataFrame_37 <- function(euroleague) {

  euroleague <- euroleague |>
    filter(MINUTE <= 36)
  

  stat_per_games <- euroleague |>
    group_by(gamenumber, year) |>
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
  
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "3FGM", "ThreeS", "ThreeS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "3FGA", "ThreeF", "ThreeF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "2FGM", "TwoS", "TwoS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "2FGA", "TwoF", "TwoF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "FTM", "FTS", "FTS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "FTA", "FTF", "FTF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "LAYUPMD", "LUS", "LUS")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "LAYUPATT", "LUF", "LUF")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "DUNK", "DUNK", "DUNK")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "O", "Off_Reb", "Off_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "D", "Deff_Reb", "Deff_Reb")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "CM", "Foul_commited", "Foul_commited")
  stat_per_games <- process_playtype(euroleague, stat_per_games,
                                     "RV", "Foul_Drawn", "Foul_Drawn")
  
  
  return(stat_per_games)
  
  return(stat_per_games)
}
#We think that NA means that the value is of 0
#We also needed to merge all different types of 2pts, as dunks and layups are
#not always counted
uniformisation <- function(data){
  data <- data|>
    mutate(TwoS_A = coalesce(TwoS_A,0) + coalesce(DUNK_A,0) + coalesce(LUS_A,0),
           TwoS_B = coalesce(TwoS_B,0) + coalesce(DUNK_B,0) + coalesce(LUS_B,0),
           TwoF_A = coalesce(TwoF_A,0) + coalesce(LUF_A,0),
           TwoF_B = coalesce(TwoF_B,0) + coalesce(LUF_B,0),
           ThreeS_A = coalesce(ThreeS_A,0),
           ThreeS_B = coalesce(ThreeS_B,0),
           ThreeF_A = coalesce(ThreeF_A,0),
           ThreeF_B = coalesce(ThreeF_B,0),
           FTS_A = coalesce(FTS_A,0),
           FTS_B = coalesce(FTS_B,0),
           FTF_A = coalesce(FTF_A,0),
           FTF_B = coalesce(FTF_B,0),
           Off_Reb_A = coalesce(Off_Reb_A,0) ,
           Off_Reb_B = coalesce(Off_Reb_B,0) ,
           Deff_Reb_A = coalesce(Deff_Reb_A,0),
           Deff_Reb_B = coalesce(Deff_Reb_B,0),
           Foul_commited_A = coalesce(Foul_commited_A,0),
           Foul_commited_B = coalesce(Foul_commited_B,0)
           )|>
    select(-LUF_A,-LUF_B,-LUS_A,-LUS_B,-DUNK_A,-DUNK_B)
}
