context("Test the modify_wage_df() function")

df <- tibble::tribble(
  ~Yr,  ~Fleet,  ~`0`,  ~`1`,
  2000,      1,    0.356, 0.402,
  2001,      1,    0.561, 0.305,
  2002,      1,    0.041, 0.612)

df_no_yr <- df
names(df_no_yr) <- c("Wrong", "Fleet", "0", "1")

df_empty <- tibble::tribble(  ~Yr,  ~Fleet,  ~`0`,  ~`1`)

test_that("modify_wage_df() - Tests for argument errors", {
  expect_error(modify_wage_df(wage = NULL, yr = 2000))
  expect_error(modify_wage_df(wage = c(0, 1, 2), yr = 2000))
  expect_error(modify_wage_df(wage = df_no_yr, yr = 2000))
})

test_that("modify_wage_df - Tests for correct output", {
  out <- modify_wage_df(wage = df, yr = 2001)
  expect_equal(out[1, -1], out[2, -1])
  out <- modify_wage_df(wage = df, yr = 2002)
  expect_equal(out[1, -1], out[3, -1])
  out <- modify_wage_df(wage = df, yr = 2002, yr_copy = 2000)
  expect_equal(out[1, -1], out[3, -1])
})

test_that("modify_wage_df - Test for warnings", {
  expect_warning(modify_wage_df(wage = df_empty, yr = 2000))
  expect_warning(modify_wage_df(wage = df, yr = 1999))
  expect_warning(modify_wage_df(wage = df, yr = 2000))
  expect_warning(modify_wage_df(wage = df, yr = 2001, yr_copy = 2001))
  expect_warning(modify_wage_df(wage = df, yr = 2000, yr_copy = 2005))
  df[3, 1] <- 2000
  expect_warning(modify_wage_df(wage = df, yr = 2000))
})

