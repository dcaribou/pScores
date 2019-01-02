#' Reverse to gatherGames. spread games goals and assists columns.
#' Inspired on: https://stackoverflow.com/questions/43695424/tidyr-spread-multiple-columns
#' @param games_df
#'
#' @return
#' @export
#'
#' @examples
spreadGames <- function(games_df) {
  HOME_AWAY = c("home", "away")
  games_spread <-
    games_df %>%
    dplyr::arrange(date, game.id, desc(venue)) %>%
    tidyr::gather(key, value, -season, -game.id, -date) %>%
    dplyr::group_by(game.id) %>%
    dplyr::mutate(concat_key = ifelse(dplyr::row_number() %% 2 == 0, HOME_AWAY, HOME_AWAY)) %>%
    tidyr::unite(subkey, concat_key, key, sep = ".") %>%
    tidyr::spread(subkey, value, fill = NA) %>% dplyr::select(-home.venue, -away.venue) %>%
    dplyr::mutate_at(dplyr::vars(-season, -game.id, -date, -dplyr::contains("team")), dplyr::funs(as.numeric))
  return(games_spread)
}
