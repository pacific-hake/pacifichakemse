context("Test the plot_ssb() function")

ps <- NULL
ps$cols <- PNWColors::pnw_palette("Starfish", n = 4, type = "discrete")
ps$mse_values_agg$catch_quant <- tibble::tibble(
  year = rep(c(2011, 2012, 2013), 4),
  `0.05` = rep(c(280000, 200000, 285000), 4),
  `0.25` = rep(c(290000, 210000, 295000), 4),
  `0.5` = rep(c(300000, 220000, 305000), 4),
  `0.75` = rep(c(310000, 240000, 325000), 4),
  `0.95` = rep(c(350000, 270000, 365000), 4),
  avg = rep(c(300000, 220000, 305000), 4),
  run = c(rep("Base scenario", 3),
              rep("Historical TAC", 3),
              rep("Realized", 3),
              rep("Floor 50", 3)))
ps$mse_values_agg$catch_quant <- ps$mse_values_agg$catch_quant

test_that("plot_catch() - Tests for argument errors", {
  expect_error(plot_catch(ps = NULL, ci = c(0.05, 0.95)))
  expect_error(plot_catch(ps = ps, ci = NULL))
  expect_error(plot_catch(ps = ps, ci = c(0.05, 0.94)))
  expect_error(plot_catch(ps = ps, ci = c("a", "b")))
  expect_error(plot_catch(ps = ps, ci = c(0.05, 0.5, 0.95)))
  ps_no_median <- ps
  tmpnames <- names(ps$mse_values_agg$catch_quant)
  tmpnames[4] <- "notright"
  names(ps_no_median$mse_values_agg$catch_quant) <- tmpnames
  expect_error(plot_catch(ps = ps_no_median,
                          ci = c(0.05, 0.95)))

})

test_that("plot_catch() - Tests for plots matching previous ones", {
  p <- plot_catch(ps = ps, ci = c(0.05, 0.95))
  vdiffr::expect_doppelganger("plot-catch", p)
  # Use following command to add new plots
  # vdiffr::manage_cases()
})

