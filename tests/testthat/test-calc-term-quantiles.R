context("Test the calc_term_quantiles() function")

pq <- tribble(
  ~year, ~run, ~val,
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

test_that("calc_term_quantiles() - Tests for argument errors", {
  expect_error(calc_term_quantiles(df = NULL, grp_col = "run", col = "val", probs = probs))
  expect_error(calc_term_quantiles(df = pq, grp_col = NULL, col = "val", probs = probs))
  expect_error(calc_term_quantiles(df = pq, grp_col = "run", col = NULL, probs = probs))
  expect_error(calc_term_quantiles(df = pq, grp_col = "run", col = "val", probs = NULL))
  expect_error(calc_term_quantiles(df = pq, grp_col = "runx", col = "val", probs = probs))
  expect_error(calc_term_quantiles(df = pq, grp_col = "run", col = "valx", probs = probs))
  pq_wrongvaltype <- pq %>%
    mutate(val = as.character(val))
  expect_error(calc_term_quantiles(df = pq_wrongvaltype, grp_col = "run", col = "val", probs = probs))
  expect_error(calc_term_quantiles(df = pq, grp_col = "run", col = "val",
                                   min_yr = NULL, probs = probs))
  expect_error(calc_term_quantiles(df = pq, grp_col = "run", col = "val",
                                   max_yr = NULL, probs = probs))
  expect_warning(calc_term_quantiles(df = pq, grp_col = "run", col = "val",
                                     min_yr = 2010, max_yr = 2009, probs = probs))

})

test_that("calc_term_quantiles() - Tests for outputs, simple case", {
  df <- calc_term_quantiles(df = pq,
                            grp_col = "run",
                            col = "val",
                            min_yr = 2000,
                            max_yr = 2020,
                            probs = probs) %>%
    as.data.frame()
  mean_run1 <- mean((pq %>% filter(run == 1))$val)
  mean_run2 <- mean((pq %>% filter(run == 2))$val)
  mean_run3 <- mean((pq %>% filter(run == 3))$val)
  quant_out <- quantile(c(mean_run1, mean_run2, mean_run3), probs = probs)
  expect_true(nrow(df) == 1)
  expect_true(ncol(df) == 5)
  df_names <- names(df)
  expect_true(df_names[1] == "0.05")
  expect_true(df_names[2] == "0.25")
  expect_true(df_names[3] == "0.5")
  expect_true(df_names[4] == "0.75")
  expect_true(df_names[5] == "0.95")
  # Note you can't compare tibbles because of this: https://github.com/tidyverse/tibble/issues/287 and
  # this: https://github.com/tidyverse/dplyr/issues/2751
  # which is why df and correct_df are cast to data.frames before this call
  expect_equivalent(df, quant_out, tolerance = 1)
})

test_that("calc_term_quantiles() - Tests for outputs, no years given", {
  df <- calc_term_quantiles(df = pq,
                            grp_col = "run",
                            col = "val",
                            probs = probs) %>%
    as.data.frame()
  mean_run1 <- mean((pq %>% filter(run == 1))$val)
  mean_run2 <- mean((pq %>% filter(run == 2))$val)
  mean_run3 <- mean((pq %>% filter(run == 3))$val)
  quant_out <- quantile(c(mean_run1, mean_run2, mean_run3), probs = probs)
  expect_equivalent(df, quant_out, tolerance = 1)
})

test_that("calc_term_quantiles() - Tests for outputs, years not in data frame given", {
  df <- calc_term_quantiles(df = pq,
                            grp_col = "run",
                            min_yr = 2021,
                            max_yr = 2047,
                            col = "val",
                            probs = probs)
  expect_true(is.null(df))
  df <- calc_term_quantiles(df = pq,
                            grp_col = "run",
                            min_yr = 1900,
                            max_yr = 1930,
                            col = "val",
                            probs = probs)
  expect_true(is.null(df))
  df <- calc_term_quantiles(df = pq,
                            grp_col = "run",
                            min_yr = 1999,
                            max_yr = 1999,
                            col = "val",
                            probs = probs)
  expect_true(is.null(df))
  df <- calc_term_quantiles(df = pq,
                            grp_col = "run",
                            min_yr = 2005,
                            max_yr = 2005,
                            col = "val",
                            probs = probs)
  expect_true(is.null(df))
})