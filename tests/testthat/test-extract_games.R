context("extract_games")

# define test data --------------------------------------------------------

player_appearances_essential <- tibble::frame_data(
  ~season, ~game.id, ~date,                  ~player.name,    ~player.id,     ~position, ~competition, ~team,                 ~venue, ~goals, ~assists,
  2017,    1,        as.Date("2017-08-18"),  "lionel-messi",  1,              4,         "ES1",        "fc-barcelona",        "home",  1,      2,
  2017,    1,        as.Date("2017-08-18"),  "luis-suarez",   2,              4,         "ES1",        "fc-barcelona",        "home",  2,      0,
  2017,    1,        as.Date("2017-08-18"),  "toni-kroos",    3,              3,         "ES1",        "real-madrid",         "away",  1,      1,
  2017,    2,        as.Date("2017-08-19"),  "iago-aspas",    4,              4,         "ES1",        "celta-vigo",          "away",  1,      1,
  2017,    2,        as.Date("2017-08-19"),  "portu",         5,              4,         "ES1",        "girona-fc",           "home",  1,      0
)
# the games that should be extracted from the first set of games
games <- tibble::frame_data(
  ~season, ~game.id, ~date,                  ~home.team,     ~away.team,    ~home.goals, ~away.goals, ~home.assists, ~away.assists,
  2017,    1,        as.Date("2017-08-18") , "fc-barcelona", "real-madrid",  3,          1,            2,             1,
  2017,    2,        as.Date("2017-08-19") , "girona-fc",    "celta-vigo",   1,          1,            0,             1
)
# if we add an arbitrary column to the appearances dataframe, then the aggregated column should be output too
player_appearances_plus_arbitrary_column <-
  cbind(player_appearances_essential, shots = 5)
games_plus_shots <- tibble::frame_data(
  ~season, ~game.id, ~date,                  ~home.team,     ~away.team,    ~home.goals, ~away.goals, ~home.assists, ~away.assists, ~home.shots, ~away.shots,
  2017,    1,        as.Date("2017-08-18") , "fc-barcelona", "real-madrid",  3,          1,            2,             1,            10,          5,
  2017,    2,        as.Date("2017-08-19") , "girona-fc",    "celta-vigo",   1,          1,            0,             1,            5,           5
)
# if we have appearance set containig appearances only for a home player in a game
one_side_appearances <- tibble::frame_data(
  ~season, ~game.id, ~date,                  ~player.name,    ~player.id,     ~position,  ~competition, ~team,        ~venue,          ~goals, ~assists,
  2017,    3,        as.Date("2017-08-20"),  "andre-gomes",   6,              3,         "GB1",         "everton-fc",         "home",        1,      2,
  2017,    4,        as.Date("2017-08-21"),  "sadio-mane",    7,              4,         "GB1",         "liverpool",          "away",         5,      1
)
# away team aggregate for this game can't be figured, so it should generate a game rows such as
# TODO: if the appearances dataframe does not contain, for example, a "home" appearance, the home aggregates do not appear
#       remove the line for sadio mane appearance in the test set above
one_side_game <- tibble::frame_data(
  ~season, ~game.id, ~date,                   ~home.team,     ~away.team,        ~home.goals, ~away.goals, ~home.assists, ~away.assists,
  2017,    3,        as.Date("2017-08-20") ,  "everton-fc",   NA,                1,           NA,            2,             NA,
  2017,    4,        as.Date("2017-08-21") ,  NA,            "liverpool",        NA,          5,             NA,            1
)
player_appearances_with_open_game <-
  rbind(player_appearances_essential, one_side_appearances)

# define test set ---------------------------------------------------------

test_that("games are extracted properly in a well formed appearances set", {
  expect_equal(games, extractGames(player_appearances_essential))
})
test_that("aggregates an arbitray column", {
  expect_equal(games_plus_shots, extractGames(player_appearances_plus_arbitrary_column))
})
test_that("one sided appearances set aggregates only one side", {
  expect_equal(one_side_game, extractGames(one_side_appearances))
})
