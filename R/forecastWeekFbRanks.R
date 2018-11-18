# Given a games dataframe (with a proper format as it is produced by 'games' function)
# it generates predictions for the games of one week
# weekn: is the number of the week to forecast (2nd week in the season, 5th week in the season...)
# eta: time weight coefficient. see rank.teams docs.
#' Title
#'
#' @param weekn
#' @param games_df
#' @param eta
#'
#' @return
#' @export
#'
#' @examples
forecastWeekFbRanks <- function(weekn, games_df, eta) {
  dates = (games_df %>% dplyr::distinct(date))$date
  tidy_boundaries = findWeekBoundaries(dates)
  boundaries = tidy_boundaries[tidy_boundaries$week.id == weekn,]
  init_date = boundaries$init.date
  last_date = boundaries$last.date
  # browser()
  day_before = as.character(as.Date(init_date) - 1)
  # we cannot forecast for the first week
  if(weekn == 1){
    return(games_df %>% dplyr::filter(date >= init_date & date <= last_date) %>% dplyr::transmute(
        season = season,
        game.id = game.id,
        date = date,
        home.team = home.team,
        away.team = away.team,
        home.score = home.score,
        away.score = away.score,
        home.score.pred = NA,
        away.score.pred = NA
      )
    )
  }
  if(is.na(last_date)) {
    warning("Last date is NA, assuming single date week")
    last_date <- init_date
  }
  ngames = games_df %>% dplyr::filter(date >= init_date & date <= last_date) %>% dplyr::count() %>% as.integer()
  #print(paste("Forecasting week ", weekn, ": from ",  init_date, " to ",  last_date, ": ", ngames, " games will be forecasted"))
  fit <- fbRanks::rank.teams(games_df, max.date = day_before, time.weight.eta = eta, silent = TRUE)
  pred <- fbRanks::predict.fbRanks(fit, min.date = init_date, max.date = last_date, silent = TRUE, rnd = TRUE)
  scores <- pred$scores
  ngameso = scores %>% dplyr::count() %>% as.integer()
  #print(paste(ngameso, " games were forecasted"))
  scores_output = scores %>%
    dplyr::transmute(
      season = season,
      game.id = game.id,
      date = date,
      home.team = home.team,
      away.team = away.team,
      home.score = home.score,
      away.score = away.score,
      home.score.pred = pred.home.score,
      away.score.pred = pred.away.score
    )
  return(scores_output)
}
