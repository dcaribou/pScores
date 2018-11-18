# Auxiliar function to calculate an appearance score based on the
# stats. It takes one vector as the only parameter, where each
# position in the vector contains a different type of statistic
computeScore = function(x) {
  # All values in the input vector are required. If any is NA
  # then the computed score is also NA
  #print(x)
  if(any(is.na(x[1:6]))) return(NA)
  # Variable unpack
  pos = x[1]
  gs = x[2]
  gss = x[3]
  as = x[4]
  min = x[5]
  gc = x[6]
  yc = x[7]
  y2 = x[8]
  rc = x[9]
  # Calculate position based coeficients
  cgc_below = if(pos == 1 || pos == 2) 4 else if(pos == 3) 1 else 0
  cgc_above = if(pos == 1 || pos == 2) 0.5 else 0
  # Calculate score
  score = (
    # MINUTES
    # More than 60 minutes gives 2 points. More than 0 gives 1 point
    (if(min>60) 2
    else 1) +
    # GOALS SCORED
    # The reward can be calculated detracting the position
    # number to 8:
    # DEF: 8 - 2(DEF) = 6 points
    # MID: 8 - 3(MID) = 5 points
    # FOR: 8 - 4(FOR) = 4 points
    gs * (8 - pos) +
    # ASSISTS
    # Every assist rewards 3 points for every player
    as * 3 +
    # GOALS CONCEEDED
    (if ( gc <= 1) {
      # If goals conceeded are under 1, use cgc_below coeficient
      (1-gc)*cgc_below
    } else {
      # If goals are above 1, we use cgc_above coeficient
      (1-gc)*cgc_above
    }) +
    # CARDS
    # We only use information about yellow cards (one and two)
    # It is difficult to combine with the information about red
    # cards in continuous mode
    (yc + y2)*(-1)
    )
  #print(paste("Score: ", score))
  return(score)
}
