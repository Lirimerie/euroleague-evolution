library(tidyverse)

library(dplyr)


CreationDataFrame <- function(euroleague) {
  stat_per_game <- euroleague %>%
    group_by(gamenumber, year) %>%
    summarise(count_three_pointer = sum(str_count(PLAYINFO, "Three Pointer")),
              count_three_pointer_missed = sum(str_count(PLAYINFO, "Missed Three Pointer")),
              count_two_pointer = sum(str_count(PLAYINFO, "Two Pointer")),
              count_two_pointer_missed = sum(str_count(PLAYINFO, "Missed Two Pointer")),
              count_Free_Throw = sum(str_count(PLAYINFO, "Free Throw In")),
              count_Free_Throw_missed = sum(str_count(PLAYINFO, "Missed Free Throw")),
              count_Layup = sum(str_count(PLAYINFO, "Layup Made")),
              TeamA = first(TeamA),  
              TeamB = first(TeamB))
  
  result <- euroleague %>%
    filter(grepl("Three Pointer", PLAYINFO)) %>%
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
    filter(grepl("Missed Three Pointer", PLAYINFO)) %>%
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
    filter(grepl("Two Pointer", PLAYINFO)) %>%
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
    filter(grepl("Missed Two Pointer", PLAYINFO)) %>%
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
    filter(grepl("Free Throw In", PLAYINFO)) %>%
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
    filter(grepl("Missed Free Throw", PLAYINFO)) %>%
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
    filter(grepl("Layup Made", PLAYINFO)) %>%
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
  
 
  
  return(stat_per_game)
}

#HEY
