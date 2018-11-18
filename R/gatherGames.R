# gather home and away scored goals and assists into a singe column
# input columns:  date - home.team - home.score - away.team - away.score - home.assists - away.assists
# output columns: date - team - is_home - goals - goals.pred - assists - assists.pred
#' Title
#'
#' @param games_df
#'
#' @return
#' @export
#'
#' @examples
gatherGames <- function(games_df) {
  games_gathered =
    games_df %>%
    # reshaping explanation here https://github.com/tidyverse/tidyr/issues/150
    tidyr::gather(key, value, -season, -game.id, -date, -home.team, -away.team) %>%
    tidyr::separate(key, c("venue", "var"), sep = "\\.", extra = "merge") %>%
    tidyr::spread(var, value) %>%
    dplyr::mutate(home.team = ifelse(venue == "home", home.team, away.team)) %>%
    dplyr::rename(team = home.team) %>% dplyr::select(-away.team) %>%
    # replace the "score" word from "goals" in the column names
    # explanation here: https://stackoverflow.com/questions/30382908/r-dplyr-rename-variables-using-string-functions
    dplyr::rename_(.dots=setNames(names(.), gsub("score", "goals", names(.))))
  return(games_gathered)
}
