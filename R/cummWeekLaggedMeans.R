#' Given a player dataframe it returns a dataframe with one new column per <col>.pred  column:
#' <col>.w. This would be the cumulative relative contribution of the player the his team's corresponding
#' aggregate.
#'
#' @param games_df
#' @param grouping_cols Which columns to use to split the computation of the cumuluative mean.
#' For example, if we select "player.name" average is calculated over the past games of a player, ignoring
#' the venue (whether he played home or away). If we choose "team" and "venue" (the default)
#' average is calculated for the past home or away games of a player.
#'
#' @return
#' @export
#'
#' @examples
cummWeekLaggedMeans <- function(data, pinned_cols, grouping_cols){
  #browser()
  unpinned_cols <- colnames(data %>% select(-one_of(pinned_cols)))
  lagged <- data %>%
    dplyr::mutate(
      week.number = strftime(date, format = "%V"),
      init.date = lubridate::floor_date(date, "week")
    ) %>%
    dplyr::mutate(week.id = dplyr::dense_rank(init.date)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(.dots = grouping_cols) %>% dplyr::arrange(week.id) %>%
    dplyr::mutate_at(
      unpinned_cols,
      dplyr::funs(dplyr::lag(.))
    )
  # At this point I have a dataframe lagged by one week on the grouping cols.
  # This means the values for the first week will be NA (because of the lagging). This is fine, but it
  # intereferes with the cummeans function we are about to call (because unfortunally it does not allow
  # to na.rm), so we need to filter them out. At the same time I want to keep them in the output data-
  # frame to account for the rows we were unable to calculate, so I save these filter records and append
  # them later to the output dataframa.
  cold_records <- lagged %>%
    dplyr::filter_at(unpinned_cols, dplyr::any_vars(is.na(.))) %>%
    dplyr::select(-week.number, -week.id, -init.date)
  hot_records <- lagged %>%
    dplyr::filter_at(unpinned_cols, dplyr::any_vars(!is.na(.))) %>%
    dplyr::mutate_at(
      unpinned_cols,
      dplyr::funs(dplyr::cummean(.))
    ) %>%
    dplyr::ungroup() %>% dplyr::select(-week.number, -week.id, -init.date)
  # Append the cold games (the system is still too cold to predict these games)
  hot_and_cold <- dplyr::bind_rows(cold_records, hot_records) %>%
    dplyr::rename_at(
      unpinned_cols,
      dplyr::funs(paste0(.,".wmeans"))
    )
  # At this point, all non aggregate columns are transformed into "<colname>.wmeans" with the generated
  # cummean for that column In order to return the real value as well in the output dataframe,
  # we need to resort to the input dataframe to obtain those
  hot_and_cold_and_reals <- hot_and_cold %>%
    dplyr::left_join(data)
  return(hot_and_cold_and_reals %>% ungroup())
}

#' Create a "mirrored" mate for each aggregate, named with a ".pred" preffix. For example, for goals and assits creates
#' the mate columns goals.pred and assists.pred with the same value as their mates
#'
#' @param games_df
#'
#' @return
#' @export
#'
#' @examples
mirrorAggregates <- function(games_df){
  aggregate_columns <- colnames(games_df %>% dplyr::select(not_aggregates))
  mate_columns <- paste0(aggregate_columns, ".pred")
  for(i in 1:length(mate_columns)) {
    games_df <- gamed_df %>% dplyr::mutate(!!mate_columns[1] := aggregate_columns[1])
  }
}
