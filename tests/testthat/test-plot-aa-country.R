context("Test the plot_aa_country() function")

ps <- NULL
ps$cols <- PNWColors::pnw_palette("Starfish", n = 4, type = "discrete")
ps$mse_quants$ams_quant <- tibble::tibble(
  country = c(rep("Canada", 12), rep("US", 12)),
  year = rep(c(2011, 2012, 2013), 8),
  `0.05` = c(rep(c(5.9, 5.8, 6.6), 4), rep(c(4.6, 6.9, 7.9), 4)),
  `0.25` = c(rep(c(4.9, 6.8, 7.6), 4), rep(c(5.6, 7.9, 9.9), 4)),
  `0.5` = c(rep(c(8.9, 9.8, 8.6), 4), rep(c(8.6, 8.9, 10.9), 4)),
  `0.75` = c(rep(c(9.9, 12.8, 13.6), 4), rep(c(11.6, 10.9, 10.9), 4)),
  `0.95` = c(rep(c(15.9, 15.8, 16.6), 4), rep(c(14.6, 16.9, 17.9), 4)),
  avg = c(rep(c(14, 15, 16), 4), rep(c(14, 16, 17), 4)),
  scenario = rep(c(rep("Base scenario", 3),
                   rep("Historical TAC", 3),
                   rep("Realized", 3),
                   rep("Floor 50", 3)), 2))
ps$mse_quants$amc_quant <- ps$mse_quants$ams_quant

test_that("plot_aa() - Tests for argument errors", {
  expect_error(plot_aa_country(ps = NULL,
                               type = "survey",
                               ci = c(0.05, 0.95),
                               country_colors = c("darkred", "blue4")))
  expect_error(plot_aa_country(ps = ps,
                               type = NULL,
                               ci = c(0.05, 0.95),
                               country_colors = c("darkred", "blue4")))
  expect_error(plot_aa_country(ps = ps,
                               type = "survey",
                               ci = NULL,
                               country_colors = c("darkred", "blue4")))
  expect_error(plot_aa_country(ps = ps,
                               type = "survey", ci = c(0.05, 0.95),
                               country_colors = NULL))
  expect_error(plot_aa_country(ps = ps,
                               type = "survey", ci = c(0.05, 0.94),
                               country_colors = c("darkred", "blue4")))
  expect_error(plot_aa_country(ps = ps,
                               type = "survey",
                               ci = c("a", "b"),
                               country_colors = c("darkred", "blue4")))
  expect_error(plot_aa_country(ps = ps,
                               type = "survey",
                               ci = c(0.05, 0.5, 0.95),
                               country_colors = c("darkred", "blue4")))
  expect_error(plot_aa_country(ps = ps,
                               type = "nonimplemented_type",
                               ci = c(0.05, 0.95)))
  ps_no_median <- ps
  tmpnames <- names(ps$mse_quants$ams_quant)
  tmpnames[5] <- "notright"
  names(ps_no_median$mse_quants$ams_quant) <- tmpnames
  expect_error(plot_aa_country(ps = ps_no_median,
                               type = "survey", ci = c(0.05, 0.95),
                               country_colors = c("darkred", "blue4")))
})

test_that("plot_aa_country() - Tests for plots matching previous ones", {
  p <- plot_aa_country(ps = ps,
                       type = "survey",
                       ci = c(0.05, 0.95))
  vdiffr::expect_doppelganger("plot-aa-country-survey", p)
  p <- plot_aa_country(ps = ps,
                       type = "catch",
                       ci = c(0.05, 0.95))
  vdiffr::expect_doppelganger("plot-aa-country-catch", p)
  p <- plot_aa_country(ps = ps,
                       type = "catch",
                       ci = c(0.05, 0.95),
                       country_colors = c("red", "green"))
  vdiffr::expect_doppelganger("plot-aa-country-catch-colordiff", p)
  # Use following command to add new plots
  # vdiffr::manage_cases()
})

