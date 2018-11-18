context("guess_team")
player_appearances <- tibble::frame_data(
  ~season, ~game.id, ~date,                  ~player.id, ~player.name,     ~position, ~competition, ~home.team,     ~away.team,     ~goals, ~assists,
  2017,    1,        as.Date("2017-08-18"),  4,          "lionel-messi",   4,         "ES1",        "fc-barcelona", "other",        1,      2,
  2017,    1,        as.Date("2017-08-18"),  4,          "lionel-messi",   4,         "ES1",        "fc-barcelona", "other",        2,      0,
  2017,    1,        as.Date("2017-08-18"),  4,          "lionel-messi",   3,         "ES1",        "away",         "fc-barcelona", 1,      1,
  2017,    2,        as.Date("2017-08-19"),  4,          "lionel-messi",   4,         "ES1",        "fc-barcelona", "team",         1,      1,
  2017,    2,        as.Date("2017-08-19"),  4,          "lionel-messi",   4,         "ES1",        "home",         "know",         1,      0
)
player_appearances_no_majority <- tibble::frame_data(
  ~season, ~game.id, ~date,                  ~player.id, ~player.name,     ~position, ~competition, ~home.team,     ~away.team,     ~goals, ~assists,
  2017,    1,        as.Date("2017-08-18"),  4,          "lionel-messi",   4,         "ES1",        "fc-barcelona", "other",        1,      2,
  2017,    1,        as.Date("2017-08-18"),  4,          "lionel-messi",   4,         "ES1",        "fc-barcelona", "other",        2,      0,
  2017,    1,        as.Date("2017-08-18"),  4,          "lionel-messi",   3,         "ES1",        "other",        "fc-barcelona", 1,      1,
  2017,    2,        as.Date("2017-08-19"),  4,          "lionel-messi",   4,         "ES1",        "fc-barcelona", "other",        1,      1,
  2017,    2,        as.Date("2017-08-19"),  4,          "lionel-messi",   4,         "ES1",        "home",         "know",         1,      0
)
guessed_teams <- tibble::frame_data(
  ~player.id, ~player.name,   ~season, ~team,
  4,          "lionel-messi", 2017,    "fc-barcelona"
)
guessed_teams_no_majority <- tibble::frame_data(
  ~player.id, ~player.name,   ~season, ~team,
  4,          "lionel-messi", 2017,    "fc-barcelona"
)
test_that("there is majority in the observed frequencies", {
  expect_equal(guessed_teams, guessTeam(player_appearances))
})
test_that("there is no majority in the observed frequencies", {
  expect_equal(guessed_teams_no_majority, guessTeam(player_appearances_no_majority))
})
