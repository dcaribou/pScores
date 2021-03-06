% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guessVenue.R
\name{guessVenue}
\alias{guessVenue}
\title{Guesses an appearance's venue: If the appearance is for a player playing home, then the venue
is home. If the appearance is for a player playing away, then the venue is away.
Guess by 1) assign a player to the most frequent team seen on the player appearances for each year
2) calculate venue based on 1)}
\usage{
guessVenue(appearances_wo_venue)
}
\arguments{
\item{appearances_wo_venue}{An appearances dataframe without venue}
}
\description{
Guesses an appearance's venue: If the appearance is for a player playing home, then the venue
is home. If the appearance is for a player playing away, then the venue is away.
Guess by 1) assign a player to the most frequent team seen on the player appearances for each year
2) calculate venue based on 1)
}
