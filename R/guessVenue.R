#' Guesses an appearance's venue: If the appearance is for a player playing home, then the venue
#' is home. If the appearance is for a player playing away, then the venue is away.
#' Guess by 1) assign a player to the most frequent team seen on the player appearances for each year
#' 2) calculate venue based on 1)
#'
#' @param appearances_wo_venue An appearances dataframe without venue
#'
#' @return
#' @export
#'
#' @examples
guessVenue <- function(appearances_wo_venue){
  guessed_teams <- guessTeam(appearances_wo_venue)
  appearances_w_teams <- dplyr::left_join(
    appearances_wo_venue, guessed_teams,
    c("player.id" = "player.id", "player.name" = "player.name", "season" = "season")
  )
  appearances_w_venues <- appearances_w_teams %>% dplyr::mutate(
      venue = ifelse(team == home.team, "home", ifelse(team == away.team, "away", NA))
    ) %>%
    dplyr::select(-home.team, -away.team)
  appearances_w_venues
}
