context("Test the plot_bars_country_season() function")

ps <- readRDS("ps_c.rds")

test_that("plot_bars_country_season() - Tests for argument errors", {
  expect_error(plot_bars_country_season(ps = NULL))
})

test_that("plot_bars_country_season() - Tests for plots matching previous ones", {
  p <- plot_bars_country_season(ps = ps)
  vdiffr::expect_doppelganger("plot-bars-country-season", p)
  # Use following command to add new plots
  # vdiffr::manage_cases()
})

