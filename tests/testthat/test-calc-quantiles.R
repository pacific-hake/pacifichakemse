context("Test the conv_vec_to_mse_df() function")


pq <- tribble(
  ~year, ~grp, ~val,
   2000,    1,  2.1,
   2001,    1,  3.4,
   2002,    1,  4.5,
   2003,    1,  5.6,
   2004,    1,  6.7,
   2000,    2,  3.1,
   2001,    2,  4.4,
   2002,    2,  5.5,
   2003,    2,  6.6,
   2004,    2,  8.7,
   2000,    3, 13.1,
   2001,    3, 14.4,
   2002,    3, 15.5,
   2003,    3, 16.6,
   2004,    3, 18.7)

probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

test_that("calc_quantiles() - Tests for argument errors", {
  expect_error(calc_quantiles(df = NULL, col = "val", probs = probs))
  expect_error(calc_quantiles(df = pq, col = NULL, probs = probs))
  expect_error(calc_quantiles(df = pq, col = "val", probs = NULL))
  expect_error(calc_quantiles(df = pq, col = "valx", probs = probs))
  pq_wrongvaltype <- pq %>%
    mutate(val = as.character(val))
  expect_error(calc_quantiles(df = pq_wrongvaltype, col = "val", probs = probs))
})

test_that("calc_quantiles() - Tests for outputs, simple case", {
  df <- calc_quantiles(df = pq, col = "val", probs = probs, include_mean = FALSE)
  expect_true(nrow(df) == 1)
  expect_true(ncol(df) == 5)
  df_names <- names(df)
  expect_true(df_names[1] == "0.05")
  expect_true(df_names[2] == "0.25")
  expect_true(df_names[3] == "0.5")
  expect_true(df_names[4] == "0.75")
  expect_true(df_names[5] == "0.95")
})

test_that("calc_quantiles() - Tests for outputs, including mean", {
  df <- calc_quantiles(df = pq, col = "val", probs = probs)
  expect_true(nrow(df) == 1)
  expect_true(ncol(df) == 6)
  df_names <- names(df)
  expect_true(df_names[1] == "0.05")
  expect_true(df_names[2] == "0.25")
  expect_true(df_names[3] == "0.5")
  expect_true(df_names[4] == "0.75")
  expect_true(df_names[5] == "0.95")
  expect_true(df_names[6] == "avg")
})

test_that("calc_quantiles() - Tests for outputs, complex grouped case", {
  yrs <- sort(unique(pq$year))
  df <- pq %>%
    group_by(year) %>%
    group_map(~ calc_quantiles(.x, col = "val", probs = probs, include_mean = FALSE)) %>%
    map_df(~{.x}) %>%
    mutate(year = yrs) %>%
    select(year, everything())
  expect_true(nrow(df) == 5)
  expect_true(ncol(df) == 6)
  df_names <- names(df)
  expect_true(df_names[1] == "year")
  expect_true(df_names[2] == "0.05")
  expect_true(df_names[3] == "0.25")
  expect_true(df_names[4] == "0.5")
  expect_true(df_names[5] == "0.75")
  expect_true(df_names[6] == "0.95")
  df <- df %>%
    as.data.frame()
  correct_df <- tribble(
    ~year, ~`0.05`, ~`0.25`, ~`0.5`, ~`0.75`, ~`0.95`,
    2000, 2.2, 2.6, 3.1, 8.1, 12.1,
    2001, 3.5, 3.9, 4.4, 9.4, 13.4,
    2002, 4.6, 5, 5.5, 10.5, 14.5,
    2003, 5.70, 6.1, 6.6, 11.6, 15.6,
    2004, 6.9, 7.7, 8.7, 13.7, 17.7) %>%
    as.data.frame
  # Note you can't compare tibbles because of this: https://github.com/tidyverse/tibble/issues/287 and
  # this: https://github.com/tidyverse/dplyr/issues/2751
  # which is why df and correct_df are cast to data.frames before this call
  expect_equivalent(df, correct_df, tolerance = 1)
})
