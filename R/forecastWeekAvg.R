# Calculate rolling means by home and away teams
forecastWeekAvg = function(weekn, games_df) {
  dates = (games_df %>% distinct(date))$date
  tidy_boundaries = weekBoundaries(dates)
  boundaries = tidy_boundaries[tidy_boundaries$weekn == weekn,]
  init_date = boundaries$init_date
  last_date = boundaries$last_date
  day_before = as.character(as.Date(init_date) - 1)
  past_games = games_df %>% filter(date < day_before)
  home =  past_games %>% group_by(home.team) %>% summarise(
    cummean.home.score = mean(home.score), 
    cumsum.home.score = sum(home.score), 
    cummean.home.assists = mean(home.assists),
    cumsum.home.assists = sum(home.assists)
  )
  away = past_games %>% group_by(away.team) %>% summarise(
    cummean.away.score = mean(away.score), 
    cumsum.away.score = sum(away.score), 
    cummean.away.assists = mean(away.assists),
    cumsum.away.assists = sum(away.assists)
  )
  ret = full_join(
    home, 
    away, 
    by = c("home.team" = "away.team")) %>% 
    mutate(up_to = day_before, team = home.team) %>% select(-home.team)
  return(ret)
}
