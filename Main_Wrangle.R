euroleague<-read.zip("data/euroleague.zip")
library(tidyverse)

#unique_playtypes <- unique(euroleague$PLAYINFO)
#unique_playtypes


euroleague <- euroleague |>
  mutate(PLAYINFO = str_remove(PLAYINFO, "\\s*\\([^)]+\\)"))

# Ugly line to reduce the details of the Play information to 33 different ones
# by removing a space and what is inside the parenthesis



unique_playtypes <- unique(euroleague$PLAYINFO)
unique_playtypes

euroleague <- euroleague |>
  filter(!str_detect(PLAYINFO, "Out|In|Fighting|
                     Bench Foul|Coach Foul|Begin Period|End Period"))|>
  select(-TYPE,-COMMENT,-PLAYTYPE,-CODETEAM, -DORSAL,-MARKERTIME,-PLAYER_ID,
         -X,-NUMBEROFPLAY) 

# all variables in select() are unnecessary variables. 
# Out|In could be interesting to measure time on the court
# I dont know which story could be told with Fighting
# Begin Period|End Period are not that necessary or so do I think as of now
# Coach Foul and Bench Foul are not as relevant

#euroleague <- euroleague |>
#  mutate(points_made = case_when(
#    PLAYINFO %in% c("Layup made", "Two Pointer","Dunk") ~ 2,
#    PLAYINFO == "Three Pointer" ~ 3,
#    TRUE ~ 0
#  ))


