context("Test the plot_ssb_ssb0() function")

ps <- NULL
ps$cols <- PNWColors::pnw_palette("Starfish", n = 4, type = "discrete")
ps$sim_data$SSB0 <- c(472000, 1417000)
ps$mse_quants$ssb_ssb0_quant <- tibble::tibble(
  year = rep(c(2011, 2012, 2013), 4),
  `0.05` = rep(c(880000, 820000, 885000), 4),
  `0.25` = rep(c(890000, 810000, 895000), 4),
  `0.5` = rep(c(900000, 820000, 805000), 4),
  `0.75` = rep(c(910000, 840000, 925000), 4),
  `0.95` = rep(c(950000, 870000, 965000), 4),
  avg = rep(c(900000, 920000, 855000), 4),
  scenario = c(rep("Base scenario", 3),
               rep("Historical TAC", 3),
               rep("Realized", 3),
               rep("Floor 50", 3)))

test_that("plot_ssb_ssb0() - Tests for argument errors", {
  expect_error(plot_ssb_ssb0(ps = NULL, ci = c(0.05, 0.95)))
  expect_error(plot_ssb_ssb0(ps = ps, ci = NULL))
  expect_error(plot_ssb_ssb0(ps = ps, ci = c(0.05, 0.94)))
  expect_error(plot_ssb_ssb0(ps = ps, ci = c("a", "b")))
  expect_error(plot_ssb_ssb0(ps = ps, ci = c(0.05, 0.5, 0.95)))
  ps_no_median <- ps
  tmpnames <- names(ps$mse_quants$ssb_ssb0_quant)
  tmpnames[4] <- "notright"
  names(ps_no_median$mse_quants$ssb_ssb0_quant) <- tmpnames
  expect_error(plot_catch(ps = ps_no_median,
                          ci = c(0.05, 0.95)))

})

test_that("plot_ssb_ssb0() - Tests for plots matching previous ones", {
  p <- plot_ssb_ssb0(ps = ps, ci = c(0.05, 0.95))
  vdiffr::expect_doppelganger("plot-ssb-ssb0", p)
  # Use following command to add new plots
  # vdiffr::manage_cases()
})

