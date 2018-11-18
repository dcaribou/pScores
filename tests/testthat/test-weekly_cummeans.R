context("weekly cummeans")

games <- tibble::frame_data(
  ~season, ~game.id, ~date,                  ~home.team,     ~away.team,    ~home.goals, ~away.goals, ~home.assists, ~away.assists,
  2017,    1,        as.Date("2017-08-18") , "fc-barcelona", "real-madrid",  4,          4,            3,             4,
  2017,    2,        as.Date("2017-08-19") , "girona-fc",    "celta-vigo",   2,          5,            2,             3,
  2017,    3,        as.Date("2017-09-18") , "fc-barcelona", "girona-fc",    6,          2,            3,             2,
  2017,    4,        as.Date("2017-09-18") , "real-madrid",  "celta-vigo",   0,          2,            0,             2,
  2017,    5,        as.Date("2017-10-18") , "celta-vigo",   "fc-barcelona", 2,          4,            2,             0,
  2017,    6,        as.Date("2017-10-18") , "girona-fc",    "real-madrid",  3,          0,            1,             0
)

game_forecasts_teams <- tibble::frame_data(
  ~season, ~game.id, ~date,                  ~home.team,     ~away.team,     ~home.goals,     ~home.goals.wmeans, ~away.goals, ~away.goals.wmeans, ~home.assists, ~home.assists.wmeans, ~away.assists, ~away.assists.wmeans,
  2017,    1,        as.Date("2017-08-18") , "fc-barcelona", "real-madrid",  4,               NA,               4,            NA,              3,             NA,                 4,             NA,
  2017,    2,        as.Date("2017-08-19") , "girona-fc",    "celta-vigo",   2,               NA,               5,            NA,              2,             NA,                 3,             NA,
  2017,    3,        as.Date("2017-09-18") , "fc-barcelona", "girona-fc",    6,               4,                2,            2,               3,             3,                  2,             2,
  2017,    4,        as.Date("2017-09-18") , "real-madrid",  "celta-vigo",   0,               4,                2,            5,               0,             4,                  2,             3,
  2017,    5,        as.Date("2017-10-18") , "celta-vigo",   "fc-barcelona", 2,               3.5,              4,            5,               2,             2.5,                0,             3,
  2017,    6,        as.Date("2017-10-18") , "girona-fc",    "real-madrid",  3,               2,                0,            2,               1,             2,                  0,             2
)
game_forecasts_teams_and_venues <- tibble::frame_data(
  ~season, ~game.id, ~date,                  ~home.team,     ~away.team,    ~home.goals,     ~home.goals.wmeans, ~away.goals, ~away.goals.wmeans, ~home.assists, ~home.assists.wmeans, ~away.assists, ~away.assists.wmeans,
  2017,    1,        as.Date("2017-08-18") , "fc-barcelona", "real-madrid",  4,               NA,              4,           NA,               3,             NA,                 4,             NA,
  2017,    2,        as.Date("2017-08-19") , "girona-fc",    "celta-vigo",   2,               NA,              5,           NA,               2,             NA,                 3,             NA,
  2017,    3,        as.Date("2017-09-18") , "fc-barcelona", "girona-fc",    6,               4,               2,           NA,               3,             3,                  2,             NA,
  2017,    4,        as.Date("2017-09-18") , "real-madrid",  "celta-vigo",   0,               NA,              2,           5,                0,             NA,                 2,             3,
  2017,    5,        as.Date("2017-10-18") , "celta-vigo",   "fc-barcelona", 2,               NA,              4,           NA,               2,             NA,                 0,             NA,
  2017,    6,        as.Date("2017-10-18") , "girona-fc",    "real-madrid",  3,               2,               0,           4,                1,             2,                  0,             4
)

test_that("week cummeans grouping teams", {
  pinned_cols <-  c("season", "game.id", "date", "team", "venue")
  grouping_cols <- c("team")
  expect_equal(game_forecasts_teams, cummWeekLaggedMeans(games %>% gatherGames(), pinned_cols,  grouping_cols) %>% spreadGames())
})
test_that("week cummeans grouping teams and venue", {
  pinned_cols <-  c("season", "game.id", "date", "team", "venue")
  grouping_cols <- c("team", "venue")
  expect_equal(game_forecasts_teams_and_venues, cummWeekLaggedMeans(games %>% gatherGames(), pinned_cols,  grouping_cols) %>% spreadGames())
})
