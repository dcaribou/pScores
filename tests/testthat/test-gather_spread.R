context("gather_spread")
# test data
games_spread <- tibble::frame_data(
  ~season, ~game.id, ~date,                 ~home.team,     ~away.team,             ~home.goals, ~away.goals, ~home.assists, ~away.assists, ~home.shots, ~away.shots,
  2017,    1,        as.Date("2017-08-18") , "fc-valencia", "ud-las-palmas",        1,            0,            1,             0,           11,          4,
  2017,    2,        as.Date("2017-08-19") , "fc-girona",   "atletico-madrid",      2,            2,            0,             2,           5,           8,
  2017,    3,        as.Date("2017-08-19") , "fc-sevilla",  "espanyol-barcelona",   1,            1,            0,             0,           7,           7,
  2017,    3,        as.Date("2017-09-19") , "santander",   "levante",              NA,           NA,           NA,            NA,          NA,          NA
)

games_gathered <- tibble::frame_data(
  ~season, ~game.id, ~date,                 ~team,                 ~venue, ~goals, ~assists, ~shots,
  2017,    1,        as.Date("2017-08-18") , "fc-valencia",        "home",    1,      1,        11,
  2017,    1,        as.Date("2017-08-18") , "ud-las-palmas",      "away",    0,      0,        4,
  2017,    2,        as.Date("2017-08-19") , "fc-girona",          "home",    2,      0,        5,
  2017,    2,        as.Date("2017-08-19") , "atletico-madrid",    "away",    2,      2,        8,
  2017,    3,        as.Date("2017-08-19") , "fc-sevilla",         "home",    1,      0,        7,
  2017,    3,        as.Date("2017-08-19") , "espanyol-barcelona", "away",    1,      0,        7,
  2017,    3,        as.Date("2017-09-19") , "santander",          "home",    NA,     NA,       NA,
  2017,    3,        as.Date("2017-09-19") , "levante",            "away",    NA,     NA,       NA
)

test_that("gather games works", {
  expect_equal(games_gathered, gatherGames(games_spread))
})
test_that("spread games works", {
  expect_equal(games_spread, spreadGames(games_gathered))
})
test_that("spread is the inverse of gather", {
  expect_equal(games_spread, spreadGames(gatherGames(games_spread)))
})

