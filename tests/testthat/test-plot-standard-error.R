context("Test the plot_standard_error() function")

ps <- NULL
ps$cols <- PNWColors::pnw_palette("Starfish", n = 4, type = "discrete")
ps$standard_error_ssb <- tibble::tibble(
  year = rep(c(2011, 2012, 2013), 4),
  `0.05` = rep(c(-0.88, 0.82, 0.885), 4),
  `0.25` = rep(c(-0.89, -0.810000, 0.895), 4),
  `0.5` = rep(c(-0.9, 0.820000, 0.805), 4),
  `0.75` = rep(c(-0.91, -0.840000, 0.925), 4),
  `0.95` = rep(c(-0.95, 0.87, 0.965), 4),
  avg = rep(c(-0.9, 0.92, 0.855), 4),
  run = c(rep("Base scenario", 3),
          rep("Historical TAC", 3),
          rep("Realized", 3),
          rep("Floor 50", 3)))

test_that("plot_standard_error() - Tests for argument errors", {
  expect_error(plot_standard_error(ps = NULL, ci = c(0.05, 0.95)))
  expect_error(plot_standard_error(ps = ps, ci = NULL))
  expect_error(plot_standard_error(ps = ps, ci = c(0.05, 0.94)))
  expect_error(plot_standard_error(ps = ps, ci = c("a", "b")))
  expect_error(plot_standard_error(ps = ps, ci = c(0.05, 0.5, 0.95)))
  ps_no_median <- ps
  tmpnames <- names(ps$standard_error)
  tmpnames[4] <- "notright"
  names(ps_no_median$standard_error) <- tmpnames
  expect_error(plot_catch(ps = ps_no_median,
                          ci = c(0.05, 0.95)))
})

test_that("plot_standard_error() - Tests for plots matching previous ones", {
  p <- plot_standard_error(ps = ps, ci = c(0.05, 0.95))
  vdiffr::expect_doppelganger("plot-standard-error", p)
  # Use following command to add new plots
  # vdiffr::manage_cases()
})

