# Clean out invalid rows and generate updated GC (Goals Condeeded)
# and S (Score) columns. Sub-set the input data to a given conditions
consolidateTopN = function(data, top = 200, past = 10){
  cleanedSeason =
    data %>%
    # Filter to valid rows (rows with real appearances)
    filter(`_V_` == TRUE) %>%
    # Add H/A information
    mutate(`_H_` = ifelse(`_CID_` == `_HTID_`, TRUE, FALSE)) %>%
    # Tmp: Drop `_GC_` column, this should not be un the MySQL table anymore
    select(-`_GC_`) %>% 
    # Create GCT column with the goals conceeded by the player
    mutate(`_GCT_` = ifelse(`_CID_` == `_HTID_`, `_GSA_`, `_GSH_`)) %>%
    # Create GST column with the total goals scored by the player's team
    mutate(`_GST_` = ifelse(`_CID_` == `_HTID_`, `_GSH_`, `_GSA_`)) %>%
    # Creates ANAME column with the name of the time playing away
    mutate(`_ANAME_` = ifelse(`_H_`, `_AT_`, `_HT_`)) %>%
    # Creates ANAME column with the name of the time playing away
    mutate(`_HNAME_` = ifelse(`_H_`, `_HT_`, `_AT_`)) %>%
    # YC and RC columns are more useful when they represent 'number of occurances'
    # rather than 'minute of the match the player saw the card'. We transform them
    # in this way
    mutate(`_YC_` = ifelse(is.na(`_YC_`), 0, 1), 
           `_Y2_` = ifelse(is.na(`_Y2_`), 0, 1),
           `_RC_` = ifelse(is.na(`_RC_`), 0, 1))
  # Comment
  withScores =
    cleanedSeason %>%
    select_at(.vars = all_columns) %>%
    # Group by player name and consolidate for each player in a nested table
    group_by(`_PNAME_`) %>%
    arrange(`_DATE_`) %>%
    tidyr::nest(.key = 'stats.real.tbl') %>%
    # Prepare nested tables as pure numeric matrix splitting out the dates column
    mutate(
      stats.meta.tbl = map(stats.real.tbl, ~ select_at(., non_statistical_columns)),
      stats.real.tbl = map(stats.real.tbl, ~ select_at(., statistical_columns))
    ) %>%
    # Calculate scores
    updateRealScores() %>%
    # Generate average score and N (number of samples)
    mutate(
      scores.real.mean = map_dbl(scores.real.ary, mean, na.rm = TRUE),
      Nsamples = map_int(stats.real.tbl, nrow)
    )
    
  # Take a sample of the top N scorers
  # Firstly, find out the top N scorers
  top_n_players = 
    withScores %>%
    select(`_PNAME_`, scores.real.mean, Nsamples) %>% 
    #unnest() %>%
    filter(Nsamples >= past) %>%
    top_n(n = top, wt = scores.real.mean)
  # Secondly, filter the original sample to contain only rows from this players
  all_n_players = 
    withScores %>% 
    filter(`_PNAME_` %in% top_n_players$`_PNAME_`)
  return(all_n_players %>% select(-Nsamples))
}
