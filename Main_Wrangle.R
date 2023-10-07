euroleague<-read.csv("data/euroleague.csv")
library(tidyverse)

unique_playtypes <- unique(euroleague$PLAYINFO)
unique_playtypes


euroleague <- euroleague |>
  mutate(PLAYINFO = str_remove(PLAYINFO, "\\s*\\([^)]+\\)"))

#Ugly line to reduce the details of the Play information to 33 different ones
#by removing a space and what is inside the parenthesis



unique_playtypes <- unique(euroleague$PLAYINFO)
unique_playtypes

euroleague <- euroleague |>
  filter(!str_detect(PLAYINFO, "Fighting|Bench Foul|Coach Foul|Begin Period|
                     End Period"))|>
  select(-TYPE,-PLAYTYPE,-CODETEAM, -DORSAL,-MARKERTIME,-PLAYER_ID,
         -X,-NUMBEROFPLAY) 

# all variables in select() are unnecessary variables. 
# Out|In could be taken out but we need to let Free throw in in the data set
#(Will work on it)
# I don't know which story could be told with Fighting
# Begin Period|End Period are not that necessary or so do I think as of now
# Coach Foul and Bench Foul are not as relevant

euroleague <- euroleague |>
  mutate(points_made = case_when(
    PLAYINFO %in% c("Layup made", "Two Pointer","Dunk") ~ 2,
    PLAYINFO == "Three Pointer" ~ 3,
    PLAYINFO == "Free Throw In" ~ 1,
    TRUE ~ 0
  ))



stat_per_game <- euroleague |>
  group_by(gamenumber,year) |>  # Regrouper par gamenumber
  summarise(count_three_pointer = sum(str_count(PLAYINFO, "Three Pointer")),
            count_three_pointer_missed = sum(str_count(PLAYINFO, "Missed Three Pointer")),
            count_two_pointer = sum(str_count(PLAYINFO, "Two Pointer")),
            count_two_pointer_missed = sum(str_count(PLAYINFO, "Missed Two Pointer")),
            count_Free_Throw = sum(str_count(PLAYINFO, "Free Throw In")),
            count_Free_Throw_missed = sum(str_count(PLAYINFO, "Missed Free Throw")),
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

