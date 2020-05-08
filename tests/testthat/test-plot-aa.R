context("Test the plot_aa() function")

ps <- NULL
ps$cols <- PNWColors::pnw_palette("Starfish", n = 4, type = "discrete")
ps$mse_values_agg$ams_quant <- tibble::tribble(
  ~year,   ~`0.05`,   ~`0.25`,   ~`0.5`,    ~`0.75`,   ~`0.95`,     ~avg,            ~run,
  2011,  3.970486,  3.970486, 3.970486,   3.970486,  3.970486, 3.970486, "Base scenario",
  2012,  3.028399,  3.028399, 3.028399,   3.028399,  3.028399, 3.028399, "Base scenario",
  2013,  3.596368,  3.596368, 3.596368,   3.596368,  3.596368, 3.596368, "Base scenario",
  2011,  3.970486,  3.970486, 3.970486,   3.970486,  3.970486, 3.970486, "Historical TAC",
  2012,  3.028399,  3.028399, 3.028399,   3.028399,  3.028399, 3.028399, "Historical TAC",
  2013,  3.596368,  3.596368, 3.596368,   3.596368,  3.596368, 3.596368, "Historical TAC",
  2011,  3.970486,  3.970486, 3.970486,   3.970486,  3.970486, 3.970486, "Realized",
  2012,  3.028399,  3.028399, 3.028399,   3.028399,  3.028399, 3.028399, "Realized",
  2013,  3.596368,  3.596368, 3.596368,   3.596368,  3.596368, 3.596368, "Realized",
  2011,  3.970486,  3.970486, 3.970486,   3.970486,  3.970486, 3.970486, "Floor 50",
  2012,  3.028399,  3.028399, 3.028399,   3.028399,  3.028399, 3.028399, "Floor 50",
  2013,  3.596368,  3.596368, 3.596368,   3.596368,  3.596368, 3.596368, "Floor 50")
ps$mse_values_agg$amc_quant <- ps$mse_values_agg$ams_quant

test_that("plot_aa() - Tests for argument errors", {
  expect_error(plot_aa(ps = NULL, type = "survey", ci = c(0.05, 0.95)))
  expect_error(plot_aa(ps = ps, type = NULL, ci = c(0.05, 0.95)))
  expect_error(plot_aa(ps = NULL, type = "survey", ci = NULL))
  expect_error(plot_aa(ps = ps, type = "survey", ci = c(0.05, 0.94)))
  expect_error(plot_aa(ps = ps, type = "survey", ci = c("a", "b")))
  expect_error(plot_aa(ps = ps, type = "survey", ci = c(0.05, 0.5, 0.95)))
})

test_that("plot_aa() - Tests for plots matching previous ones", {
  p <- plot_aa(ps = ps, type = "survey", ci = c(0.05, 0.95))
  vdiffr::expect_doppelganger("plot-aa-survey", p)
  p <- plot_aa(ps = ps, type = "catch", ci = c(0.05, 0.95))
  vdiffr::expect_doppelganger("plot-aa-catch", p)
  # Use following command to add new plots
  # vdiffr::manage_cases()
})

