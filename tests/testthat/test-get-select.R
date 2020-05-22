context("Test the get_select() function")

p_sel <- tibble::tibble(value = c(0, 0.58, -0.23, 0.28, 0.38),
                        source = rep("fish", 5),
                        space = rep(1, 5),
                        age = 2:6)
test_that("get_select() - Tests for argument errors", {
  expect_error(get_select(ages = NULL, p_sel = p_sel,
                          s_min = 2, s_max = 6))
  expect_error(get_select(ages = 0:20, p_sel = NULL,
                          s_min = 2, s_max = 6))
  expect_error(get_select(ages = 0:20, p_sel = p_sel,
                          s_min = NULL, s_max = 6))
  expect_error(get_select(ages = 0:20, p_sel = p_sel,
                          s_min = 2, s_max = NULL))
})

test_that("get_select() - Tests for correct output", {
  sel <- get_select(ages = 0:20, p_sel =  p_sel,
                    s_min = 2, s_max = 6)
  out <- c(0, 0, 0.36, 0.65, 0.52, 0.68, rep(1, 15))
  expect_equivalent(sel, out, tolerance = 0.01)

  sel <- get_select(ages = 1:20, p_sel =  p_sel,
                    s_min = 2, s_max = 6)
  out <- c(0, 0.36, 0.65, 0.52, 0.68, rep(1, 15))
  expect_equivalent(sel, out, tolerance = 0.01)

})
