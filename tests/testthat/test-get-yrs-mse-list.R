
year <- 2001:2010
lst <- list()
lst[[1]] <- list()
lst[[1]]$Catch <- array(1:80,
                        dim = c(2, 2, 2, 10),
                        dimnames = list(c("a", "b"),
                                        c("d", "e"),
                                        c("f", "g"),
                                        year = year))
lst[[2]] <- list()
lst[[2]]$Catch <- array(1:80,
                        dim = c(2, 2, 2, 10),
                        dimnames = list(c("a", "b"),
                                        c("d", "e"),
                                        c("f", "g"),
                                        year = year))
lst[[3]] <- list()
lst[[3]]$Catch <- array(1:80,
                        dim = c(2, 2, 2, 10),
                        dimnames = list(c("a", "b"),
                                        c("d", "e"),
                                        c("f", "g"),
                                        year = year))

test_that("get_yrs_mse_list() - Tests for argument errors", {
  expect_error(get_yrs_mse_list(NULL))
})

test_that("get_yrs_mse_list() - Tests for correct output", {
  expect_equal(get_yrs_mse_list(lst), 2001:2010)
})

test_that("get_yrs_mse_list() - Tests for different numbers of years between runs", {
  year <- 2001:2009
  lst[[3]]$Catch <- array(1:72,
                          dim = c(2, 2, 2, 9),
                          dimnames = list(c("a", "b"),
                                          c("d", "e"),
                                          c("f", "g"),
                                          year = year))

  expect_error(get_yrs_mse_list(lst))
})
