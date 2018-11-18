context("week_boundaries")

# test data ---------------------------------------------------------------

unbounded_dates <- as.Date(
    c("2018-09-17", "2018-09-21", "2018-09-24"),
    format="%Y-%m-%d"
  )
expected_unbounded <- tibble::frame_data(
  ~week.number, ~week.id, ~total.games, ~init.date,           ~last.date,
  38,           1,        2,            as.Date("2018-09-17"), as.Date("2018-09-21"),
  39,           2,        1,            as.Date("2018-09-24"), as.Date("2018-09-24")
)
bounded_dates <- as.Date(
  c("2018-09-17", "2018-09-21", "2018-10-15", "2018-10-21"),
  format="%Y-%m-%d"
)
expected_bounded <- tibble::frame_data(
  ~week.number, ~week.id, ~total.games, ~init.date,           ~last.date,
  38,           1,        2,           as.Date("2018-09-17"), as.Date("2018-09-21"),
  42,           2,        2,           as.Date("2018-10-15"), as.Date("2018-10-21")
)

# test expectations -------------------------------------------------------

test_that("unbounded date arrays", {
  expect_equal(expected_unbounded, findWeekBoundaries(unbounded_dates))
})
test_that("bounded date arrays", {
  expect_equal(expected_bounded, findWeekBoundaries(bounded_dates))
})
