#' Shape appearances data as a game scores dataframe
#' The ouput games are formatted as a \link[gatherGames]{spread} games dataframe
#'
#' @param season_data Player appearances. Required columns are: season, game.id, date, venue, team and some game variable like goals, assists...
#'
#' @return
#' @export
#'
#' @examples
extractGames <- function(appearances_w_venues) {
  games_df <-
    appearances_w_venues %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      date = as.Date(date, format="%Y-%m-%d")
    ) %>%
    dplyr::group_by(game.id, date, venue, team, season) %>%
    dplyr::summarise_at(
      dplyr::vars(-season, -game.id, -date, -player.id, -player.name,  -position, -competition, -team, -venue),
      dplyr::funs({as.numeric(sum(.))})
    ) %>%
    dplyr::ungroup() %>%
    tidyr::gather(key, value, -game.id, -venue, -date, -season) %>%
    tidyr::unite(var, venue, key, sep = ".") %>%
    tidyr::spread(var, value) %>%
    dplyr::mutate_at(dplyr::vars(-game.id, -date, -dplyr::contains("team")), dplyr::funs(as.numeric))
  return(games_df)
}
