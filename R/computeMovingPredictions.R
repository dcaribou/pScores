# Compute predictions by the pFUN method and return a joined tibble real vs predicted values
# As the first parameter accepts a tibble with the raw data, a parameter 'memory' for the width of the
# moving average and 
computeMovingPredictions = function(data, memory, pFUN, ...){
  debugval = data
  # Calculate moving averages on numeric columns
  predictions = 
    data %>%
    mutate(
      stats.pred.tbl = 
        map(
          stats.real.tbl,
          function(x){
            rolled_aplied = x %>%
            # Mutate columns in 'vars' by using the functions in 'funs'
            # In this case we use the windowing function 'rollapply' to constaint the 'mean'
            # function to a few occurances in the past
            mutate_at(
              .vars = statistical_columns,
              # The 'rollapply' syntax requires that we pass the vector we want to apply the
              # function over in the 'data' parameter. Since we are inside a 'mutate_at' verb
              # we have to use the '.', placeholder character, to represent the changing column
              .funs = funs(
                 rollapply(data = .,
                 # The width parameter represents the memory of the moving average
                 width = memory,
                 # This is the function itself. Here we can insert an arbitrary function
                 FUN = pFUN,
                 # Parameters to pFUN are passed on
                 ...,
                 align = "right",
                 fill = NA
               )
              )
            ) %>%
            # Lag numeric variables by 1 sample. This make sure windowing affects only to rows in the past
            mutate_at(.vars = statistical_columns, .funs = funs(dplyr::lag(x = ., n = 1)))
            return(rolled_aplied)
          }
        )
      ) %>%
    updatePredScores() %>%
    # Calculate some extra columns
    # The 'moving error' for the our predictions
    mutate(
      stats.err.tbl = map2(stats.real.tbl, stats.pred.tbl, ~ (as.matrix(.x) - as.matrix(.y)) %>% dplyr::as_tibble()),
      scores.pred.mean = map_dbl(scores.pred.ary, mean, na.rm = TRUE),
      scores.pred.sum = map_dbl(scores.pred.ary, sum, na.rm = TRUE),
      scores.real.sum = map_dbl(scores.real.ary, sum, na.rm = TRUE),
      scores.err.ary = map2(scores.pred.ary, scores.real.ary, ~ (as.matrix(.x) - as.matrix(.y))  %>% dplyr::as_tibble())
    )
  # Return predictions together with a new column holding the 'mean' predicted score
  return(
    predictions %>% 
      mutate(
        scores.err.mean = scores.pred.mean - scores.real.mean#,
       # scores.err.std = map_dbl(scores.err.ary, sd, na.rm = TRUE)
      )
    )
}
