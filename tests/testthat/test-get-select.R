context("Test the get_select() function")

test_that("get_select() - Tests for argument errors", {
  expect_error(get_select(ages = NULL, p_sel = c(0, 0.58, -0.23, 0.28, 0.38),
                          s_min = 2, s_max = 6))
  expect_error(get_select(ages = 1:20, p_sel = NULL,
                          s_min = 2, s_max = 6))
  expect_error(get_select(ages = 1:20, p_sel = c(0, 0.58, -0.23, 0.28, 0.38),
                          s_min = NULL, s_max = 6))
  expect_error(get_select(ages = 1:20, p_sel = c(0, 0.58, -0.23, 0.28, 0.38),
                          s_min = 2, s_max = NULL))
})

test_that("get_select() - Tests for correct output", {
  sel <- get_select(ages = 1:20, p_sel = c(0.58, -0.23, 0.28, 0.38),
                    s_min = 2, s_max = 6)
  out <- c(0, 0.36, 0.64, 0.29, 0.48, 0.53, rep(1, 14))
  expect_equivalent(sel, out, tolerance = 0.01)
})
