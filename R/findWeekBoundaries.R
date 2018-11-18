#' Given a set of dates, it finds the week numbers they belong to and the initial and
#' last date for each week
#'
#' @param dates
#'
#' @return
#' @export
#'
#' @examples
findWeekBoundaries <- function(dates) {
  week_boundaries <-
    tibble::data_frame(date = dates) %>%
    dplyr::mutate(week.number = strftime(date, format = "%V"), year = strftime(date, format = "%Y")) %>%
    dplyr::group_by(week.number, year) %>% dplyr::arrange(date) %>% dplyr::mutate(
      boundary = ifelse(dplyr::row_number() == 1, "init.date", ifelse(dplyr::row_number() == n(), "last.date", "other")),
      total.games = dplyr::n()
    ) %>%
    dplyr::filter(boundary == "init.date" | boundary == "last.date") %>% dplyr::ungroup() %>%
    dplyr::select(date, week.number, boundary, total.games) %>%
    dplyr::mutate_at(dplyr::vars(-boundary, -date), dplyr::funs(as.numeric))
  tidy_boundaries <-
    week_boundaries %>% tidyr::spread(boundary, date) %>%
    # if there is only one date in the week, the last.date column is now NA
    # assign the last.date to be equal to the init.date in this case
    dplyr::mutate(last.date = dplyr::if_else(is.na(last.date), init.date, last.date)) %>%
    dplyr::arrange(init.date) %>%
    dplyr::mutate(
      week.id = as.numeric(dplyr::row_number())
      #last.date = lubridate::ceiling_date(init.date, "week", week_start = 1)
    )
  return(tidy_boundaries)
}
