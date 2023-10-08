library(tidyverse)

library(dplyr)


CreationDataFrame <- function(euroleague) {
  stat_per_game <- euroleague %>%
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
      TeamA = first(TeamA),  
      TeamB = first(TeamB)
    )
  
  result <- euroleague %>%
    filter(grepl("3FGM", PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamA" = "TEAM")) %>%
    mutate(ThreeSA = jjj) %>%
    select(-jjj)
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamB" = "TEAM")) %>%
    mutate(ThreeSB = jjj) %>%
    select(-jjj)
  
  rm(result)
  
  
  result <- euroleague %>%
    filter(grepl("3FGA", PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamA" = "TEAM")) %>%
    mutate(ThreeFA = jjj) %>%
    select(-jjj)
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamB" = "TEAM")) %>%
    mutate(ThreeFB = jjj) %>%
    select(-jjj)
  
  rm(result)
  
  result <- euroleague %>%
    filter(grepl("2FGM", PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamA" = "TEAM")) %>%
    mutate(TwoSA = jjj) %>%
    select(-jjj)
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamB" = "TEAM")) %>%
    mutate(TwoSB = jjj) %>%
    select(-jjj)
  
  rm(result)
  
  result <- euroleague %>%
    filter(grepl("2FGA", PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamA" = "TEAM")) %>%
    mutate(TwoFA = jjj) %>%
    select(-jjj)
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamB" = "TEAM")) %>%
    mutate(TwoFB = jjj) %>%
    select(-jjj)
  
  rm(result)
  
  result <- euroleague %>%
    filter(grepl("FTM", PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamA" = "TEAM")) %>%
    mutate(FTSA = jjj) %>%
    select(-jjj)
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamB" = "TEAM")) %>%
    mutate(FTSB = jjj) %>%
    select(-jjj)
  
  rm(result)
  
  result <- euroleague %>%
    filter(grepl("FTA", PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamA" = "TEAM")) %>%
    mutate(FTFA = jjj) %>%
    select(-jjj)
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamB" = "TEAM")) %>%
    mutate(FTFB = jjj) %>%
    select(-jjj)
  
  rm(result)
  
  result <- euroleague %>%
    filter(grepl("LAYUPMD", PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamA" = "TEAM")) %>%
    mutate(LUSA = jjj) %>%
    select(-jjj)
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamB" = "TEAM")) %>%
    mutate(LUSB = jjj) %>%
    select(-jjj)
  
  rm(result)
  
  result <- euroleague %>%
    filter(grepl("LAYUPATT", PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamA" = "TEAM")) %>%
    mutate(LUFA = jjj) %>%
    select(-jjj)
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamB" = "TEAM")) %>%
    mutate(LUFB = jjj) %>%
    select(-jjj)
  
  
  rm(result)
  
  
  result <- euroleague %>%
    filter(grepl("DUNK", PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamA" = "TEAM")) %>%
    mutate(DunkA = jjj) %>%
    select(-jjj)
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamB" = "TEAM")) %>%
    mutate(DunkB = jjj) %>%
    select(-jjj)
  
  rm(result)
  
  result <- euroleague %>%
    filter(grepl("O", PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamA" = "TEAM")) %>%
    mutate(Off_RebA = jjj) %>%
    select(-jjj)
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamB" = "TEAM")) %>%
    mutate(Off_RebB = jjj) %>%
    select(-jjj)
  
  rm(result)
  
  result <- euroleague %>%
    filter(grepl("D", PLAYTYPE)) %>%
    group_by(year, gamenumber, TEAM) %>%
    summarize(jjj = n())
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamA" = "TEAM")) %>%
    mutate(Def_RebA = jjj) %>%
    select(-jjj)
  
  stat_per_game <- stat_per_game %>%
    left_join(result, by = c("gamenumber", "year", "TeamB" = "TEAM")) %>%
    mutate(Def_RebB = jjj) %>%
    select(-jjj)
  
  rm(result)

  return(stat_per_game)
}

#Punaise
