context("Test the plot_standard_error() function")

ps <- readRDS("ps_c.rds")

test_that("plot_standard_error() - Tests for argument errors", {
  expect_error(plot_standard_error(ps = NULL, ci = c(0.05, 0.95)))
  expect_error(plot_standard_error(ps = ps, ci = NULL))
  expect_error(plot_standard_error(ps = ps, ci = c(0.05, 0.94)))
  expect_error(plot_standard_error(ps = ps, ci = c("a", "b")))
  expect_error(plot_standard_error(ps = ps, ci = c(0.05, 0.5, 0.95)))
})

test_that("plot_standard_error() - Tests for plots matching previous ones", {
  p <- plot_standard_error(ps = ps,
                           ci = c(0.05, 0.95),
                           facet_back_alpha = 50)
  vdiffr::expect_doppelganger("plot-standard-error", p)
  # Use following command to add new plots
  # vdiffr::manage_cases()
})

