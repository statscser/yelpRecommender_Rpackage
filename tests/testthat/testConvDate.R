context("date class")

test_that("class is date", {
  expect_that(class(convertDate("2012-11")) == "Date", is_true())
  #expect_true(class(convertDate("2012-11")) == "Date")
  #expect_false(class(convertDate("2012-11")) == "Date") #forced error
})
