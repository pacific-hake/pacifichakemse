context("Test the wage_add_yr() function")

df <- tibble::tribble(
  ~Yr,  ~Fleet,  ~`0`,  ~`1`,
  2000,      1,    0.356, 0.402,
  2001,      1,    0.561, 0.305,
  2002,      1,    0.041, 0.612)

df_no_yr <- df
names(df_no_yr) <- c("Wrong", "Fleet", "0", "1")

df_empty <- tibble::tribble(  ~Yr,  ~Fleet,  ~`0`,  ~`1`)

test_that("wage_add_yr() - Tests for argument errors", {
  expect_error(wage_add_yr(wage = NULL))
  expect_error(wage_add_yr(wage = c(0, 1, 2)))
  expect_error(wage_add_yr(wage = df_no_yr))
  expect_error(wage_add_yr(wage = df_empty))
})

test_that("wage_add_yr() - Tests for correct output", {
  out <- wage_add_yr(wage = df)
  expect_equal(nrow(out), 4)
  expect_equivalent(out[3, -1], out[4, -1])
})


