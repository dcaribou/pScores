# Update real scores
# The score is calculated by passing to a custom function 'computeScore' the
# relevant columns
updateRealScores = function(data) {
  return(
    data %>% 
      mutate(scores.real.ary = map(stats.real.tbl, ~ apply(., 1, computeScore)))
  )
}
updatePredScores = function(data) {
  return(
    data %>% 
      mutate(scores.pred.ary = map(stats.pred.tbl, ~ apply(., 1, computeScore)))
  )
}
