# gives the correlation value for home and away goals and assits in a set of games
# it must be in the format of a "spread" games dataframe
gameCorrelations <- function(games_df) {
  cors = cor(games_df %>% drop_na() %>% select_if(is.numeric) %>% as.matrix())
  return(cors)
}
