
process_team_stats_data <- function(data) {
  processed_data <- data |>
    pivot_longer(cols = c(TeamA, TeamB), names_to = "Team_Type",
                 values_to = "Team") |>
    # Create a "winner" variable (1 if the team is the winner, 0 otherwise)
    mutate(winner = ifelse(winner == Team, 1, 0)) |>
    mutate(
      # Create new columns by combining statistics from TeamA and TeamB based on "Team_Type."
      Tot_Points = case_when(
        Team_Type == "TeamA" ~ Tot_Point_A,
        Team_Type == "TeamB" ~ Tot_Point_B,
        TRUE ~ NA_real_
      ),
      Tot_Points_Opponent = case_when(
        Team_Type == "TeamA" ~ Tot_Point_B,
        Team_Type == "TeamB" ~ Tot_Point_A,
        TRUE ~ NA_real_
      ),
      ThreeS = case_when(
        Team_Type == "TeamA" ~ ThreeS_A,
        Team_Type == "TeamB" ~ ThreeS_B,
        TRUE ~ NA_real_
      ),
      ThreeF = case_when(
        Team_Type == "TeamA" ~ ThreeF_A,
        Team_Type == "TeamB" ~ ThreeF_B,
        TRUE ~ NA_real_
      ),
      TwoS = case_when(
        Team_Type == "TeamA" ~ TwoS_A,
        Team_Type == "TeamB" ~ TwoS_B,
        TRUE ~ NA_real_
      ),
      TwoF = case_when(
        Team_Type == "TeamA" ~ TwoF_A,
        Team_Type == "TeamB" ~ TwoF_B,
        TRUE ~ NA_real_
      ),
      FTS = case_when(
        Team_Type == "TeamA" ~ FTS_A,
        Team_Type == "TeamB" ~ FTS_B,
        TRUE ~ NA_real_
      ),
      FTF = case_when(
        Team_Type == "TeamA" ~ FTF_A,
        Team_Type == "TeamB" ~ FTF_B,
        TRUE ~ NA_real_
      ),
      Off_Reb = case_when(
        Team_Type == "TeamA" ~ Off_Reb_A,
        Team_Type == "TeamB" ~ Off_Reb_B,
        TRUE ~ NA_real_
      ),
      Def_Reb = case_when(
        Team_Type == "TeamA" ~ Deff_Reb_A,
        Team_Type == "TeamB" ~ Deff_Reb_B,
        TRUE ~ NA_real_
      ),
      Fouls_commited = case_when(
        Team_Type == "TeamA" ~ Foul_commited_A,
        Team_Type == "TeamB" ~ Foul_commited_B,
        TRUE ~ NA_real_
      ),
      Diff_Points_End = case_when(
        Team_Type == "TeamA" ~ ifelse(Tot_Point_A > Tot_Point_B,
                                      Total_Difference, -Total_Difference),
        Team_Type == "TeamB" ~ ifelse(Tot_Point_B > Tot_Point_A,
                                      Total_Difference, -Total_Difference),
        TRUE ~ NA_real_
      )
    ) |>
    select(-ThreeS_A, -ThreeS_B,
           -ThreeF_A, -ThreeF_B, -TwoS_A, -TwoS_B, -TwoF_A,
           -TwoF_B, -FTS_A, -FTS_B, -FTF_A, -FTF_B, -Off_Reb_A,
           -Off_Reb_B, -Deff_Reb_A, -Deff_Reb_B,
           -Foul_commited_A, -Foul_commited_B)
  if ("pts_A" %in% colnames(data)) {
    processed_data <- processed_data |>
      # Create additional columns related to points and point differentials 
      #if "pts_A" is in the original data.
      mutate(
        Points_35_first_min = case_when(
          Team_Type == "TeamA" ~ pts_A,
          Team_Type == "TeamB" ~ pts_B,
          TRUE ~ NA_real_
        ),
        Diff_Points_Min_35 = case_when(
          Team_Type == "TeamA" ~ pts_A - pts_B,
          Team_Type == "TeamB" ~ pts_B - pts_A,
          TRUE ~ NA_real_
        ),
        Variation_Of_Gap = Diff_Points_End - Diff_Points_Min_35
      )  |>
      select(-pts_A, -pts_B)
  } 
  
  #rename the teams because there is differents names for the same team 
  processed_data <- processed_data |>
    select(-Team_Type,-Tot_Point_A, -Tot_Point_B)|>  
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
    )) |>
    mutate(
      three_attempts = coalesce(ThreeS, 0) + coalesce(ThreeF, 0),
      two_attempts = coalesce(TwoS, 0) + coalesce(TwoF, 0),
      FT_attempts = coalesce(FTS, 0) + coalesce(FTF, 0),
      When = ifelse(year <=2015,"From 2007 to 2016","From 2016 to 2020") #useful for plots
    ) |>
    mutate(
      three_accuracy = ThreeS / three_attempts,
      two_accuracy = TwoS / two_attempts,
      FT_accuracy = FTS / FT_attempts
    )|>
    filter(gamenumber != 300) # removes the two outliers
  
  return(processed_data)
}

# Example of usage
#team_stats_df <- process_team_stats_data(stat_per_games)

calculate_team_season_stats <- function(data) {
  summary_data <- data |>
    group_by(year, Team) |>
    summarise(
      average_points = mean(Tot_Points),
      average_points_opp = mean(Tot_Points_Opponent),
      average_def_reb = mean(Def_Reb),
      average_off_reb = mean(Off_Reb),
      average_threeS = mean(ThreeS),
      average_threeF = mean(ThreeF),
      average_twoS = mean(TwoS),
      average_twoF = mean(TwoF),
      average_FTS = mean(FTS),
      average_FTF = mean(FTF),
      win_percentage = mean(winner),
      avg_3pt_accuracy = mean(three_accuracy),
      avg_2pt_accuracy = mean(two_accuracy),
      avg_FT_accuracy = mean(FT_accuracy),
      avg_Fouls_commited = mean(Fouls_commited),
      total_games = n()
    ) |>
    mutate(
      average_three_attempts = average_threeS + average_threeF,
      average_two_attempts = average_twoS + average_twoF,
      average_FT_attempts = average_FTS + average_FTF
    )
  
  return(summary_data)
}

# Example of usage
#team_stats_season <- calculate_team_season_stats(team_stats_df)
