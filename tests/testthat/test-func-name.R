context("Test the func_name() function")

test_that("func_name() - Tests for argument errors", {
  expect_error(func_name(NULL))
  expect_error(func_name(c(1, 2)))
  expect_error(func_name("a"))
})

foo <- function(frame){
  bar(frame)
}
bar <- function(frame){
  freddy(frame)
}
freddy <- function(frame){
  func_name(frame)
}

test_that("func_name() - Tests for calling inside bar()", {
  expect_equal(foo(1), "freddy")
  expect_equal(foo(2), "bar")
  expect_equal(foo(3), "foo")
  # There are many calls due to testing so the 17th is back to Global
  expect_error(foo(17))
})
