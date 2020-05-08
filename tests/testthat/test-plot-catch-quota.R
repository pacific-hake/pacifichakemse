context("Test the plot_catch_quota() function")

ps <- NULL
ps$cols <- PNWColors::pnw_palette("Starfish", n = 4, type = "discrete")
j <- ps$mse_values_agg$catch_q_quant <- tibble::tibble(
  country = c(rep("Canada", 12), rep("US", 12)),
  year = rep(c(2011, 2012, 2013), 8),
  `0.05` = c(rep(c(0.3, 0.4, 0.5), 4), rep(c(0.2, 0.5, 0.6), 4)),
  `0.25` = c(rep(c(0.3, 0.4, 0.5), 4), rep(c(0.2, 0.5, 0.6), 4)),
  `0.5` = c(rep(c(0.3, 0.4, 0.5), 4), rep(c(0.2, 0.5, 0.6), 4)),
  `0.75` = c(rep(c(0.3, 0.4, 0.5), 4), rep(c(0.2, 0.5, 0.6), 4)),
  `0.95` = c(rep(c(0.3, 0.4, 0.5), 4), rep(c(0.2, 0.5, 0.6), 4)),
  avg = c(rep(c(0.3, 0.4, 0.5), 4), rep(c(0.2, 0.5, 0.6), 4)),
  run = rep(c(rep("Base scenario", 3),
              rep("Historical TAC", 3),
              rep("Realized", 3),
              rep("Floor 50", 3)), 2))

test_that("plot_catch_quota() - Tests for argument errors", {
  expect_error(plot_catch_quota(ps = NULL, ci = c(0.05, 0.95)))
  expect_error(plot_catch_quota(ps = ps, ci = NULL))
  expect_error(plot_catch_quota(ps = ps, ci = c(0.05, 0.94)))
  expect_error(plot_catch_quota(ps = ps, ci = c("a", "b")))
  expect_error(plot_catch_quota(ps = ps, ci = c(0.05, 0.5, 0.95)))
  ps_no_median <- ps
  tmpnames <- names(ps$mse_values_agg$catch_q_quant)
  tmpnames[5] <- "notright"
  names(ps_no_median$mse_values_agg$catch_q_quant) <- tmpnames
  expect_error(plot_catch_quota(ps = ps_no_median,
                                ci = c(0.05, 0.95)))

})

test_that("plot_catch_quota() - Tests for plots matching previous ones", {
  p <- plot_catch_quota(ps = ps, ci = c(0.05, 0.95))
  vdiffr::expect_doppelganger("plot-catch-quota", p)
  # Use following command to add new plots
  # vdiffr::manage_cases()
})

