context("Test the verify_argument() function")

test_that("verify_argument() - Tests for argument errors", {
  expect_error(verify_argument(NULL, chk_class = "numeric", chk_len = 1, chk_is_in = 1:20))
  expect_error(verify_argument(23, chk_class = c("numeric", "character"),
                                chk_len = 1, chk_is_in = 1:20))
  expect_error(verify_argument(23, chk_class = "numeric", chk_len = c(1, 3), chk_is_in = 1:20))

  expect_error(verify_argument(23, chk_class = "numeric", chk_len = 1, chk_is_in = 1:20))
})

test_that("verify_argument() - Test correct input", {
  expect_true(verify_argument(23, chk_class = "numeric", chk_len = 1))
  expect_true(verify_argument(23, chk_class = "numeric", chk_len = 1, chk_is_in = 1:23))
  expect_true(verify_argument(c(1, 2, 3), chk_class = "numeric", chk_len = 3, chk_is_in = 1:23))
  expect_true(verify_argument(c("a", "b"), chk_class = "character", chk_len = 2, chk_is_in = c("a", "b", "c")))
})

test_that("verify_argument() - Test incorrect input", {
  expect_error(verify_argument(23, chk_class = "character", chk_len = 1))
  expect_error(verify_argument(23, chk_class = "numeric", chk_len = 2))
  expect_error(verify_argument(23, chk_class = "numeric", chk_len = 1, chk_is_in = 1:22))
  expect_error(verify_argument(c(1, 2, 3), chk_class = "numeric", chk_len = 3, chk_is_in = 1:2))
  expect_error(verify_argument(c("a", "b"), chk_class = "character", chk_len = 2, chk_is_in = c("a", "c")))
})