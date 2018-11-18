# Forecasts for both fbRanks and rolling averages and joins results in one dataframe
#' Title
#'
#' @param games_df
#' @param method
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
forecastGames <- function(games_df, method = "old", ...){
  arguments <- list(...)
  if(method == "old") {
    fbranks = forecastSeasonFbRanks(0, games_df)
    #rolling_means = forecastSeasonAvg(games_df)
    # We may join now the rolling average predictions together with the fbRanks forecasts
    # 1) Add home team cumulative averages
    forecast_w_home_avg = left_join(
      fbranks,
      rolling_means %>% select(-cummean.away.score,-cumsum.away.score,-cummean.away.assists,-cumsum.away.assists),
      by = c("home.team" = "team", "up_to" = "up_to")
    )
    # 2) Add away team cumulative averages
    forecast_complete = left_join(
      forecast_w_home_avg,
      rolling_means %>% select(-cummean.home.score,-cumsum.home.score,-cummean.home.assists,-cumsum.home.assists),
      by = c("away.team" = "team", "up_to" = "up_to")
    )
    return(forecast_complete)
  }
  else if(method == "fbRanks") {
    games_df <- replaceGoalsColumns(games_df)
    fbranks_goals = forecastSeasonFbRanks(arguments[["eta"]], games_df)
    # mock the package for assists forecasting
    games_df_cpy = games_df
    games_df_cpy$home.score <- games_df$home.assists
    games_df_cpy$away.score <- games_df$away.assists
    fbranks_assists = forecastSeasonFbRanks(arguments[["eta"]], games_df_cpy)
    # place all predictions in the same dataframe
    fbranks = fbranks_goals %>%
      select(season, game.id, date, home.team, away.team, home.score, home.score.pred, away.score, away.score.pred)
    fbranks$home.assists = fbranks_assists$home.score
    fbranks$home.assists.pred = fbranks_assists$home.score.pred
    fbranks$away.assists = fbranks_assists$away.score
    fbranks$away.assists.pred = fbranks_assists$away.score.pred
    fbranks <- replaceScoreColumns(fbranks)
    return(fbranks)
  }
  else if(method == "dummy") {
    # the dummy forecast is better dealt with when the games are in the gathered format
    games_df_gathered = gatherGames(games_df)
    # the calculation logic is now self-explanatory
    dummy = games_df_gathered %>%
      group_by(team, venue) %>% arrange(date) %>%
      mutate(
        lagged_goals = lag(goals),
        lagged_assists = lag(assists)
      ) %>%
      ungroup(team, venue) %>%
      mutate(goals.pred = lagged_goals, assists.pred = lagged_assists) %>%
      select(-lagged_goals, -lagged_assists)
    # return the dataframe to the original spread format
    dummy_spread = spreadGames(dummy)
    return(dummy_spread)
  }
  else if(method == "cummeans") {
    #print(paste("forecast cummeans method, number of arguments", length(arguments)))
    if(length(arguments) == 0) {
      pinned_cols <-  c("season", "game.id", "date", "team", "venue")
      grouping_cols <- c("team", "venue")
      return(
        cummWeekLaggedMeans(games_df %>% gatherGames(), pinned_cols = pinned_cols, grouping_cols) %>%
          spreadGames() %>%
          rename_at(vars( contains("wmeans") ), funs( gsub("wmeans", "pred", .) ) )
      )
    }
  }
}
