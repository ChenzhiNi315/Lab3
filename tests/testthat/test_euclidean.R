test_that("euclidean function tests normal functionality", {
  expect_equal(euclidean(100, 10), 10)
  expect_equal(euclidean(48, 18), 6)
  expect_equal(euclidean(7, 0), 7)
  expect_equal(euclidean(1071, 462), 21)
})

test_that("euclidean function tests invalid input", {
  expect_error(euclidean("a", 10)) #The parameter is a string
  expect_error(euclidean(10, Inf)) #The parameter is infinitely large
  expect_error(euclidean(c(2, 3), 10)) #The parameter is a vector
  expect_error(euclidean(10.5, 5.2)) #The parameter is a floating-point number
})
