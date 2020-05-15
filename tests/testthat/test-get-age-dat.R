context("Test the get_age_dat() function")

d <- tibble::tribble(
  ~Yr,   ~Fleet,    ~`0`,   ~`1`,   ~`2`,
  1966,        1,   0.157, 0.0905,  0.252,
  1967,        1,   0.167, 0.0915,  0.352,
  1968,        1,   0.177, 0.0925,  0.452)

test_that("get_age_dat() - Tests for argument errors", {
  expect_error(get_age_dat(NULL, 1966))
  expect_error(get_age_dat(d, NULL))
  expect_error(get_age_dat(d, "a"))
  expect_error(get_age_dat(c(1, 2), 1966))
  expect_error(get_age_dat(d %>% rename(x = Yr), 1966))
  expect_error(get_age_dat(d %>% rename(x = Fleet), 1966))
})

test_that("get_age_dat() - Output is correct", {
  x <- get_age_dat(d, 1966) %>% as.data.frame
  expect_equivalent(x, c(0.157, 0.0905, 0.252))
  x <- get_age_dat(d, c(1966, 1968)) %>% as.data.frame
  exp_out <- tibble::tribble(
    ~`0`,   ~`1`,   ~`2`,
    0.157, 0.0905,  0.252,
    0.177, 0.0925,  0.452) %>% as.data.frame
  expect_equivalent(x, exp_out)
})
