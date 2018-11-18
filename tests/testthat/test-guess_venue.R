context("guess_venue")
player_appearances_wo_venue <- tibble::frame_data(
  ~season, ~game.id, ~date,                 ~player.id, ~player.name,     ~position, ~competition, ~home.team,     ~away.team,     ~goals, ~assists,
  2017,    1,        as.Date("2017-08-18"), 5,          "lionel-messi",   4,         "ES1",        "fc-barcelona", "other",        1,      2,
  2017,    1,        as.Date("2017-08-18"), 5,          "lionel-messi",   4,         "ES1",        "fc-barcelona", "other",        2,      0,
  2017,    1,        as.Date("2017-08-18"), 5,          "lionel-messi",   3,         "ES1",        "away",         "fc-barcelona", 1,      1,
  2017,    2,        as.Date("2017-08-19"), 5,          "lionel-messi",   4,         "ES1",        "fc-barcelona", "team",         1,      1,
  2017,    2,        as.Date("2017-08-19"), 5,          "lionel-messi",   4,         "ES1",        "home",         "know",         1,      0,
  2018,    2,        as.Date("2018-08-19"), 5,          "lionel-messi",   4,         "ES1",        "fc-barcelona", "know",         1,      0,
  2018,    2,        as.Date("2018-08-19"), 5,          "lionel-messi",   4,         "ES1",        "fc-barcelona", "other",        1,      0
)
player_appearances_w_venue <- tibble::frame_data(
  ~season, ~game.id, ~date,                  ~player.id, ~player.name,     ~position, ~competition, ~venue,     ~team,           ~goals, ~assists,
  2017,    1,        as.Date("2017-08-18"),  5,          "lionel-messi",   4,         "ES1",        "home",     "fc-barcelona",  1,      2,
  2017,    1,        as.Date("2017-08-18"),  5,          "lionel-messi",   4,         "ES1",        "home",     "fc-barcelona",  2,      0,
  2017,    1,        as.Date("2017-08-18"),  5,          "lionel-messi",   3,         "ES1",        "away",     "fc-barcelona",  1,      1,
  2017,    2,        as.Date("2017-08-19"),  5,          "lionel-messi",   4,         "ES1",        "home",     "fc-barcelona",  1,      1,
  2017,    2,        as.Date("2017-08-19"),  5,          "lionel-messi",   4,         "ES1",        NA,         "fc-barcelona",  1,      0,
  2018,    2,        as.Date("2018-08-19"),  5,          "lionel-messi",   4,         "ES1",        "home",     "fc-barcelona",  1,      0,
  2018,    2,        as.Date("2018-08-19"),  5,          "lionel-messi",   4,         "ES1",        "home",     "fc-barcelona",  1,      0
)
test_that("venue is guessed when there is clear majority", {
  expect_equal(player_appearances_w_venue, guessVenue(player_appearances_wo_venue))
})
