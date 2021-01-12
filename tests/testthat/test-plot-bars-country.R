
ps <- readRDS("ps_c.rds")

test_that("plot_bars_country() - Tests for argument errors", {
  expect_error(plot_bars_country(ps = NULL))
})

# test_that("plot_bars_country() - Tests for plots matching previous ones", {
#   p <- plot_bars_country(ps = ps)
#   vdiffr::expect_doppelganger("plot-bars-country", p)
#   # Use following command to add new plots
#   # vdiffr::manage_cases()
# })

