context("test-replace-score-columns")

singular <- data.frame(
  col1 = c("1", "other", "string"),
  score = c(2, 5 , 6),
  col3 = c(4, 5, 7)
)

plural <- data.frame(
  col1 = c("1", "other", "string"),
  scores = c(2, 5 , 6),
  col3 = c(4, 5, 7)
)

goals_cols <- data.frame(
  col1 = c("1", "other", "string"),
  goals = c(2, 5 , 6),
  col3 = c(4, 5, 7)
)

test_that("replace 'score' (singular)", {
  expect_equal(names(replaceScoreColumns(singular)), c("col1", "goals", "col3"))
})
test_that("replace 'scores' (plural)", {
  expect_equal(names(replaceScoreColumns(plural)), c("col1", "goals", "col3"))
})
test_that("replace 'goals' to 'score'", {
  expect_equal(names(replaceGoalsColumns(goals_cols)), c("col1", "score", "col3"))
})
