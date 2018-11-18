context("test-fbranks_forecast")
# It is not really possible to test this function without a deep knowledge of the fbRanks prediction
# algorith internals, but at least we can check that the output has the expected number of rows and
# columns

games <- tibble::frame_data(
  ~season, ~game.id, ~date,                  ~home.team,     ~away.team,    ~home.score, ~away.score,
  2017,    1,        as.Date("2017-08-18") , "fc-barcelona", "real-madrid",  4,          4,
  2017,    2,        as.Date("2017-08-19") , "girona-fc",    "celta-vigo",   2,          5,
  2017,    3,        as.Date("2017-09-18") , "fc-barcelona", "girona-fc",    6,          2,
  2017,    4,        as.Date("2017-09-18") , "real-madrid",  "celta-vigo",   0,          2,
  2017,    5,        as.Date("2017-10-18") , "celta-vigo",   "fc-barcelona", 2,          4,
  2017,    6,        as.Date("2017-10-18") , "girona-fc",    "real-madrid",  3,          0
)

test_that("basic forecast output structure", {
  pred_games <- forecastSeasonFbRanks(0, games)
  expect(!is.null(pred_games))
  expect_equal(names(pred_games), c(names(games), c("home.score.pred", "away.score.pred")))
})
