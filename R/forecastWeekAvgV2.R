# Calculate rolling means by home and away teams
#' Title
#'
#' @param weekn
#' @param games_df
#'
#' @return
#' @export
#'
#' @examples
forecastWeekAvgV2 <- function(weekn, games_df) {
  dates = (games_df %>% dplyr::distinct(date))$date
  tidy_boundaries = findWeekBoundaries(dates)
  boundaries = tidy_boundaries[tidy_boundaries$week.id == weekn,]
  init_date = boundaries$init.date
  last_date = boundaries$last.date
  day_before = as.character(as.Date(init_date) - 1)
  past_games = games_df %>% dplyr::filter(date < day_before)
  past_games_gathered <- gatherGames(past_games)
  preds <- past_games_gathered %>% dplyr::group_by(team, venue) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::mutate_at(dplyr::vars(-season, -game.id, -date, -team, -venue), dplyr::funs(dplyr::lag(dplyr::cummean(.)))) %>%
    dplyr::rename_at(dplyr::vars(-season, -game.id, -date, -team, -venue), dplyr::funs(paste0(.,".pred")))
  perds_ungathered <- spreadGames(preds)
  return(perds_ungathered)
}
