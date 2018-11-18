# Connect to database and get 'raw' data. The championsip identifier is the `_C_`. Some possible
# values are
#  GB1 : Premier League
#  ES1 : La Liga
#  IT1 : Calccio
#  L1 : Bundesliga
#  FR1 : French league
#  PO1 : Portuguese League
#  UCL : Champions Leage
#  EL : Europa League
#' Title
#'
#' @param dbname
#' @param user
#' @param password
#' @param host
#'
#' @return
#' @export
#'
#' @examples
loadData <- function(dbname = 'tfmkt', user = 'tfmkt', password = 'tfmkt', host = 'localhost') {
  conn <- RMySQL::dbConnect(RMySQL::MySQL(), dbname, user, password, host)
  season_data = dplyr::as_tibble(DBI::dbGetQuery(conn,
        "SELECT
          season,
          _GID_ as `game.id`,
          _DATE_ as `date`,
          _PID_ as `player.id`,
          _PNAME_ as `player.name`,
          _POS_ as position,
          _V_ as `is.valid`,
          _C_ as competition,
          current_team as `current.team`,
          _HT_ as `home.team`,
          _AT_ as `away.team`,
          _GS_ as `goals`,
          _AS_ as assists
        FROM tfmkt.appearances")
  ) %>% dplyr::mutate(
    date = as.Date(date, format = "%Y-%m-%d")
  )

  RMySQL::dbDisconnect(conn)
  return(season_data)
}
