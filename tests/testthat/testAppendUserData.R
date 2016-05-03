context("userFeatures added")

f <- function() {
  data("join100Sample")
  data("user100Sample")
  u <- appendUserData(fulldata = join100Sample, userdata = user100Sample)
  return(ncol(u) - ncol(user100Sample))
}

test_that("userFeatures increase", {
  expect_true(1 < f())
})

#Ken Calhoon