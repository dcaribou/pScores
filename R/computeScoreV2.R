# Attempt for a easier to use version of the previous function
computeScoreV2 = Vectorize(
#' Title
#'
#' @param position
#' @param goals_scored
#' @param own_goals
#' @param assists
#' @param minutes_played
#' @param goals_conceded
#' @param yellow_cards
#' @param red_cards
#'
#' @return
#' @export
#'
#' @examples
  function(position, goals_scored = 0, own_goals = 0, assists = 0, minutes_played = 0,
                          goals_conceded = 0, yellow_cards = 0, red_cards = 0
                          ) {
  # All values in the input vector are required. If any is NA
  # then the computed score should be also NA

  # Calculate position based coeficients
  cgc_below = if(position == 1 || position == 2) 4 else if(position == 3) 1 else 0
  cgc_above = if(position == 1 || position == 2) 0.5 else 0
  # Calculate score
  score = (
    # MINUTES
    # More than 60 minutes gives 2 points. More than 0 gives 1 point
    (if(minutes_played > 60) 2 else if(minutes_played == 0) 0 else 1) +
      # GOALS SCORED
      # The reward can be calculated detracting the position
      # number to 8:
      # DEF: 8 - 2(DEF) = 6 points
      # MID: 8 - 3(MID) = 5 points
      # FOR: 8 - 4(FOR) = 4 points
      goals_scored * (8 - position) +
      # ASSISTS
      # Every assist rewards 3 points for every player
      assists * 3 +
      # GOALS CONCEEDED
      (if ( goals_conceded <= 1) {
        # If goals conceeded are under 1, use cgc_below coeficient
        (1-goals_conceded)*cgc_below
      } else {
        # If goals are above 1, we use cgc_above coeficient
        (1-goals_conceded)*cgc_above
      }) +
      # CARDS
      # We only use information about yellow cards (one and two)
      # It is difficult to combine with the information about red
      # cards in continuous mode
      (- yellow_cards - 3*red_cards)
  )
  #print(paste("Score: ", score))
  return(score)
  }
)

