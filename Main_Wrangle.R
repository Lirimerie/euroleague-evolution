
library(tidyverse)

euroleague<-read.csv("data/euroleague.csv")

unique_playtypes <- unique(euroleague$PLAYINFO)
unique_playtypes


euroleague <- euroleague |>
  mutate(PLAYINFO = str_remove(PLAYINFO, "\\s*\\([^)]+\\)"))

#Ugly line to reduce the details of the Play information to 33 different ones
#by removing a space and what is inside the parenthesis



unique_playtypes <- unique(euroleague$PLAYINFO)
unique_playtypes

#euroleague <- euroleague |>
  #filter(!str_detect(PLAYINFO, "Fighting|Bench Foul|Coach Foul|Begin Period|
                     #End Period"))|>
  #select(-TYPE,-PLAYTYPE,-CODETEAM, -DORSAL,-MARKERTIME,-PLAYER_ID,
         #-X,-NUMBEROFPLAY) 

# all variables in select() are unnecessary variables. 
# Out|In could be taken out but we need to let Free throw in in the data set
#(Will work on it)
# I don't know which story could be told with Fighting
# Begin Period|End Period are not that necessary or so do I think as of now
# Coach Foul and Bench Foul are not as relevant

euroleague <- euroleague |>
  mutate(points_made = case_when(
    PLAYINFO %in% c("Layup Made", "Two Pointer","Dunk") ~ 2,
    PLAYINFO == "Three Pointer" ~ 3,
    PLAYINFO == "Free Throw In" ~ 1,
    TRUE ~ 0
  ))

source("CreationDataFrame.R")

stat_per_game <- CreationDataFrame(euroleague)

stat_per_game <- stat_per_game |>
  rowwise() |>
  mutate(
    Tot_Point_A = 3 * sum(ThreeSA, na.rm = TRUE) + 
      2 * sum(TwoSA, na.rm = TRUE) + 
      sum(FTSA, na.rm = TRUE) + 
      2 * sum(LUSA, na.rm = TRUE) + 
      2 * sum(DunkA, na.rm = TRUE),
    Tot_Point_B = 3 * sum(ThreeSB, na.rm = TRUE) + 
      2 * sum(TwoSB, na.rm = TRUE) + 
      sum(FTSB, na.rm = TRUE) + 
      2 * sum(LUSB, na.rm = TRUE) + 
      2 * sum(DunkB, na.rm = TRUE)
  ) |>
  ungroup()

stat_per_game <- stat_per_game |>
  mutate(TeamA = toupper(TeamA))|>
  mutate(TeamB = toupper(TeamB))

stat_per_game <- stat_per_game |>
  mutate(winner = ifelse(Tot_Point_A > Tot_Point_B, TeamA, TeamB))

source("Ranking.R")




