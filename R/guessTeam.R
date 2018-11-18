#' Guesses the team of a player based on the most frequent team on the games for each season
#'
#' @param appearances_wo_teams
#'
#' @return
#' @export
#'
#' @examples
guessTeam <- function(appearances_wo_teams) {
  guessed_teams <- appearances_wo_teams %>%
    tidyr::gather(key, team, home.team, away.team) %>%
    dplyr::group_by(player.id, player.name, season, team) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(player.id, season) %>%
    dplyr::filter(n == max(n)) %>% dplyr::select(-n) %>%
    dplyr::group_by(player.name, season) %>% dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::select(season, player.id, player.name, team)
}
