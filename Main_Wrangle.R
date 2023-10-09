
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

stat_per_game <- stat_per_game %>%
  rowwise() %>%
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
  ) %>%
  ungroup()

stat_per_game <- stat_per_game |>
  mutate(TeamA = toupper(TeamA))|>
  mutate(TeamB = toupper(TeamB))

stat_per_game <- stat_per_game |>
  mutate(winner = ifelse(Tot_Point_A > Tot_Point_B, TeamA, TeamB))

team_stats_df <- stat_per_game |>
  pivot_longer(cols = c(TeamA, TeamB), names_to = "Team_Type", values_to = "Team")|>
  mutate(winner = ifelse(winner == Team, 1, 0))

team_stats_df <- team_stats_df %>%
  mutate(
    Tot_Points = case_when(
      Team_Type == "TeamA" ~ Tot_Point_A,
      Team_Type == "TeamB" ~ Tot_Point_B,
      TRUE ~ NA_real_  # Handle other cases if needed
    ),
    ThreeS = case_when(
      Team_Type == "TeamA" ~ ThreeSA,
      Team_Type == "TeamB" ~ ThreeSB,
      TRUE ~ NA_real_ 
    ),
    ThreeF = case_when(
      Team_Type == "TeamA" ~ ThreeFA,
      Team_Type == "TeamB" ~ ThreeFB,
      TRUE ~ NA_real_ 
    ),
    TwoS = case_when(
      Team_Type == "TeamA" ~ TwoSA,
      Team_Type == "TeamB" ~ TwoSB,
      TRUE ~ NA_real_ 
    ),
    TwoF = case_when(
      Team_Type == "TeamA" ~ TwoFA,
      Team_Type == "TeamB" ~ TwoFB,
      TRUE ~ NA_real_ 
    ),
    FTS = case_when(
      Team_Type == "TeamA" ~ FTSA,
      Team_Type == "TeamB" ~ FTSB,
      TRUE ~ NA_real_ 
    ),
    FTF = case_when(
      Team_Type == "TeamA" ~ FTFA,
      Team_Type == "TeamB" ~ FTFB,
      TRUE ~ NA_real_ 
    ),
    LUS = case_when(
      Team_Type == "TeamA" ~ LUSA,
      Team_Type == "TeamB" ~ LUSB,
      TRUE ~ NA_real_ 
    ),
    LUF = case_when(
      Team_Type == "TeamA" ~ LUFA,
      Team_Type == "TeamB" ~ LUFB,
      TRUE ~ NA_real_ 
    ),
    Dunk = case_when(
      Team_Type == "TeamA" ~ DunkA,
      Team_Type == "TeamB" ~ DunkB,
      TRUE ~ NA_real_ 
    ),
    Off_Reb = case_when(
      Team_Type == "TeamA" ~ Off_RebA,
      Team_Type == "TeamB" ~ Off_RebB,
      TRUE ~ NA_real_ 
    ),
    Def_Reb = case_when(
      Team_Type == "TeamA" ~ Def_RebA,
      Team_Type == "TeamB" ~ Def_RebB,
      TRUE ~ NA_real_ 
    )
  )|>
  select(-Tot_Point_A,-Tot_Point_B,- ThreeSA, -ThreeSB, -ThreeFA,-ThreeFB,
         -TwoSA,-TwoSB,-TwoFA,-TwoFB,-FTSA,-FTSB,-FTFA,-FTFB,-LUSA,-LUSB,-LUFA,
         -LUFB,-DunkA,-DunkB,-Off_RebA,-Off_RebB,-Def_RebA,-Def_RebB,-Team_Type)

team_stats_df <- team_stats_df|>
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
                       "BASKONIA" = "BASKONIA VITORIA GASTEIZ",
                       "TD SYSTEMS BASKONIA VITORIA-GASTEIZ" = "BASKONIA VITORIA GASTEIZ",
                       "BC KHIMKI" = "BC KHIMKI MOSCOW REGION",
                       "UNION OLIMPIJA" = "UNION OLIMPIJA LJUBLJANA",
                       "UNICS" = 	"UNICS KAZAN",
                       "UNICAJA" = "UNICAJA MALAGA",
                       "AXA FC BARCELONA" = "FC BARCELONA",
                       "BELGACOM SPIROU BASKET" = "BELGACOM SPIROU",
                       "BILBAO BASKET" = "BIZKAIA BILBAO BASKET",
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
                       "AX ARMANI EXCHANGE OLIMPIA MILAN" = "AX ARMANI EXCHANGE OLIMPIA M",
                       "FC BARCELONA REGAL" = "FC BARCELONA",
                       "FC BARCELONA LASSA REGAL" = "FC BARCELONA",
                       "FENERBAHCE DOGUS ISTANBUL" = "FENERBAHCE ISTANBUL",
                       "FENERBAHCE BEKO ISTANBUL" = "FENERBAHCE ISTANBUL",
                       "FENERBAHCE ULKER ISTANBUL" = "FENERBAHCE ISTANBUL",
                       "FB DOGUS" = "FENERBAHCE ISTANBUL",
                       "LABORAL KUTXA VITORIA GASTEIZ" = "LABORAL KUTXA VITORIA", 
                       "MACCABI ELECTRA" = "MACCABI ELITE TEL AVIV",
                       "MACCABI PLAYTIKA TEL AVIV" = "MACCABI ELITE TEL AVIV",
                       "OLYMPIACOS PIRAEUS B.C." = "OLYMPIACOS",
                       "PANATHINAIKOS OPAP ATHENS" = "PANATHINAIKOS ATHENS",
                       "PANATHINAIKOS BSA ATHENS" = "PANATHINAIKOS ATHENS",
                       "PANATHINAIKOS OPAP ATHENS" = "PANATHINAIKOS ATHENS",
                       "PANATHINAIKOS SUPERFOODS ATHENS"  = "PANATHINAIKOS ATHENS",
                       "KK ZAGREB CROATIA OSIGURANJE" = "KK ZAGREB" ,
                       "SPIROU BASKET" = "SPIROU CHARLEROI",
                       "KIROLBET BASKONIA VITORIA GASTEIZ" = "BIZKAIA BILBAO BASKET",
                       "CAJA LABORAL BASKONIA" = "BIZKAIA BILBAO BASKET",
                       "ROANNE" = "CHORALE ROANNE",
                       "AX ARMANI EXCHANGE MILAN" = "ARMANI JEANS MILANO",
                       "BASKETS BAMBERG" = "BROSE BASKETS BAMBERG",
                       "OLIMPIJA" = "UNION OLIMPIJA LJUBLJANA",
                       "BALONCESTO MALAGA" = "UNICAJA MALAGA",
                       "VIRTUS ROMA" ="LOTTOMATICA ROMA"
  )
  )

team_stats_season <- team_stats_df|>
  group_by(year,Team)|>
  summarise(average_points = mean(Tot_Points),
            average_def_reb = mean(Def_Reb),
            average_off_reb = mean(Off_Reb),
            average_threeS = mean(ThreeS),
            average_threeF = mean(ThreeF),
            average_twoS = mean(TwoS),
            average_twoF = mean(TwoF),
            average_FTS = mean(FTS),
            average_FTF = mean(FTF),
            average_LUS = mean(LUS),
            average_LUF = mean(LUF),
            average_dunk = mean(Dunk),
            win_percentage = mean(winner),
            total_games = n()
            )|>
  mutate(average_three_attempts = average_threeS + average_threeF,
         average_two_attempts = average_twoS + average_twoF)
# quicker? $
# certaines équipes sont en double
# peut rajouter des colonnes avec des pourcentages





