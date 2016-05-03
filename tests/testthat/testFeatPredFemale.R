context("gender logical")

test_that("female is true", {
  expect_true(as.logical(featPredFemale("Grace")[2]))
})

