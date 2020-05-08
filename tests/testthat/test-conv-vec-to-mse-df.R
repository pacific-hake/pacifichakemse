context("Test the conv_vec_to_mse_df() function")

vec <- c(208466.66, 93044.93, 79919.21, 81372.42, 83091.57,
         78692.74, 103359.35, 105915.67, 105188.23, 149307.26)

yr_vec <- c(2000, 2001, 2002, 2003, 2004,
            2000, 2001, 2002, 2003, 2004)

probs <- c(0.05, 0.5, 0.95)

test_that("conv_vec_to_mse_df() - Tests for argument errors", {
  expect_error(conv_vec_to_mse_df(vec = NULL, col = "ssb", run_vec = run_vec, yr_vec = yr_vec, probs = probs))
  expect_error(conv_vec_to_mse_df(vec = vec, col = NULL, run_vec = run_vec, yr_vec = yr_vec, probs = probs))
  expect_error(conv_vec_to_mse_df(vec = vec, col = "ssb", run_vec = NULL, yr_vec = yr_vec, probs = probs))
  expect_error(conv_vec_to_mse_df(vec = vec, col = "ssb", run_vec = run_vec, yr_vec = NULL, probs = probs))
  expect_error(conv_vec_to_mse_df(vec = vec, col = "ssb", run_vec = run_vec, yr_vec = yr_vec, probs = NULL))
  expect_error(conv_vec_to_mse_df(vec = vec[-1], col = "ssb", run_vec = run_vec, yr_vec = yr_vec, probs = probs))
  expect_error(conv_vec_to_mse_df(vec = vec, col = "ssb", run_vec = run_vec[-1], yr_vec = yr_vec, probs = probs))
  expect_error(conv_vec_to_mse_df(vec = vec, col = "ssb", run_vec = run_vec, yr_vec = yr_vec[-1], probs = probs))
})

test_that("conv_vec_to_mse_df() - Tests for outputs, simple case", {
  df <- conv_vec_to_mse_df(vec = vec, col = "ssb", yr_vec = yr_vec, probs = probs)
  expect_true(nrow(df) == 5)
  expect_true(ncol(df) == 5)
  expect_equivalent(df %>% dplyr::slice(1) %>% unlist(., use.names=FALSE),
                    c(2000, quantile(vec[c(1, 6)], probs = probs), mean(vec[c(1, 6)])))
  expect_equivalent(df %>% dplyr::slice(2) %>% unlist(., use.names=FALSE),
                    c(2001, quantile(vec[c(2, 7)], probs = probs), mean(vec[c(2, 7)])))
  expect_equivalent(df %>% dplyr::slice(3) %>% unlist(., use.names=FALSE),
                    c(2002, quantile(vec[c(3, 8)], probs = probs), mean(vec[c(3, 8)])))
  expect_equivalent(df %>% dplyr::slice(4) %>% unlist(., use.names=FALSE),
                    c(2003, quantile(vec[c(4, 9)], probs = probs), mean(vec[c(4, 9)])))
  expect_equivalent(df %>% dplyr::slice(5) %>% unlist(., use.names=FALSE),
                    c(2004, quantile(vec[c(5, 10)], probs = probs), mean(vec[c(5, 10)])))
})

test_that("conv_vec_to_mse_df() - Tests for outputs, add country", {
  df <- conv_vec_to_mse_df(vec = vec, col = "ssb", yr_vec = yr_vec, probs = probs, country = "Canada")
  expect_true(nrow(df) == 5)
  expect_true(ncol(df) == 6)
  expect_true(length(unique(df$country)) == 1)
})

test_that("conv_vec_to_mse_df() - Tests for outputs, no avg calculated", {
  df <- conv_vec_to_mse_df(vec = vec,
                           col = "ssb",
                           yr_vec = yr_vec,
                           probs = probs,
                           inc_mean = FALSE)
  expect_true(nrow(df) == 5)
  expect_true(ncol(df) == 4)
})