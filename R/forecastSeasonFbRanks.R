# Given a games dataframe (with a proper format as it is produced by 'games' function)
# it generates predictions for the games of one full season based on the fbRanks package
# eta: time weight coefficient. see rank.teams docs.
#' Title
#'
#' @param eta
#' @param games_df
#'
#' @return
#' @export
#'
#' @examples
forecastSeasonFbRanks <- function(eta, games_df) {
  dates = (games_df %>% dplyr::distinct(date))$date
  tidy_boundaries = pRanks::findWeekBoundaries(dates)
  forecasts = do.call(rbind, lapply((X = tidy_boundaries$week.id), FUN = forecastWeekFbRanks, games_df, eta))
  return(forecasts)
}

#' We want to use 'goals' instead of 'score' all along the package to avoid confusion with the score points
#' of a player
#'
#' @param games_df
#'
#' @return
#' @export
#'
#' @examples
replaceScoreColumns <- function(games_df) {
  cols <- names(games_df)
  cols_replaced <- gsub("scores?", "goals", cols)
  names(games_df) <- cols_replaced
  return(games_df)
}

#' The oposite function to replaceScoreColumns
#'
#' @param games_df
#'
#' @return
#' @export
#'
#' @examples
replaceGoalsColumns <- function(games_df) {
  cols <- names(games_df)
  cols_replaced <- gsub("goals", "score", cols)
  # browser()
  names(games_df) <- cols_replaced
  return(games_df)
}
