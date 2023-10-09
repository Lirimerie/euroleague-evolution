team_stats_df <- stat_per_game |>
  pivot_longer(cols = c(TeamA, TeamB), names_to = "Team_Type", values_to = "Team")|>
  mutate(winner = ifelse(winner == Team, 1, 0))

team_stats_df <- team_stats_df |>
  mutate(
    Tot_Points = case_when(
      Team_Type == "TeamA" ~ Tot_Point_A,
      Team_Type == "TeamB" ~ Tot_Point_B,
      TRUE ~ NA_real_  # Handle other cases if needed
    ),
    Tot_Points_Opponent = case_when(
      Team_Type == "TeamA" ~ Tot_Point_B,
      Team_Type == "TeamB" ~ Tot_Point_A,
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
                       "PANATHINAIKOS" = "PANATHINAIKOS ATHENS",
                       "FC BARCELONA " = "FC BARCELONA"
  )
  )

team_stats_season <- team_stats_df|>
  group_by(year,Team)|>
  summarise(average_points = mean(Tot_Points),
            average_points_opp = mean(Tot_Points_Opponent),
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
# tout est à nettoyer 
