context("data_driven_games")
skip("Takes too long")
season_data <- loadData(host = "192.168.1.165")
appearances_w_venues <- guessVenue(season_data)
appearances_w_venues_valid <- appearances_w_venues %>% dplyr::filter(is.valid == 1)
appearances_w_venues_wo_na <- appearances_w_venues_valid %>% dplyr::filter(!is.na(venue)) %>% dplyr::select(-is.valid, -current.team)
games <- extractGames(appearances_w_venues_wo_na) %>% tidyr::drop_na()

test_that("checkpoint game aggregates", {
  # ES1
  # https://www.transfermarkt.com/spielbericht/index/spielbericht/2899594
  expect_equal(
    tibble::tribble(
      ~home.goals, ~away.goals, ~home.assists, ~away.assists,
      6,           1,           5,             1
    ),
    games[games$date==as.Date("2017-09-19") & games$home.team == "fc-barcelona", c("home.goals", "away.goals", "home.assists", "away.assists")]
  )
  # https://www.transfermarkt.com/spielbericht/index/spielbericht/2899727
  expect_equal(
    tibble::tribble(
      ~home.goals, ~away.goals, ~home.assists, ~away.assists,
      1,           0,           1,             0
    ),
    games[games$date==as.Date("2018-01-12") & games$home.team == "fc-getafe", c("home.goals", "away.goals", "home.assists", "away.assists")]
  )
  # IT1
  # https://www.transfermarkt.com/spielbericht/index/spielbericht/3080934
  expect_equal(
    tibble::tribble(
      ~home.goals, ~away.goals, ~home.assists, ~away.assists,
      0,           5,           0,             4
    ),
    games[games$date==as.Date("2018-09-15") & games$away.team == "uc-sampdoria", c("home.goals", "away.goals", "home.assists", "away.assists")]
  )
})
