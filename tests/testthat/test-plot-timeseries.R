context("Test plot_timeseries() function")

ps <- readRDS("ps_c.rds")

test_that("plot_timeseries() - Tests for argument errors", {
  # ps
  expect_error(plot_timeseries(NULL, type = "survey", ci = c(0.05, 0.95)))

  # type
  expect_error(plot_timeseries(ps, type = NULL, ci = c(0.05, 0.95)))
  expect_error(plot_timeseries(ps, type = "nonimplemented_type"))

  # ci
  expect_error(plot_timeseries(ps, ci = NULL))
  expect_error(plot_timeseries(ps, ci = c("a", "b")))
  expect_error(plot_timeseries(ps, ci = c(0.05, 0.5, 0.95)))

  # yr_lim
  expect_error(plot_timeseries(ps, yr_lim = c("a", 1)))
  expect_error(plot_timeseries(ps, yr_lim = c(1, 2, 3)))

  ps_no_median <- ps
  ps_no_median$mse_quants$amc_all_quant <- ps_no_median$mse_quants$amc_all_quant %>%
    rename(notright = `0.5`)
  expect_error(plot_timeseries(ps_no_median, type = "aac"))

  ps_no_median <- ps
  ps_no_median$mse_quants$amc_all_quant <- ps_no_median$mse_quants$amc_all_quant %>%
    rename(notright = scenario)
  expect_error(plot_timeseries(ps_no_median, type = "aac"))

  ps_no_median <- ps
  expect_error(plot_timeseries(ps_no_median, type = "aac", ci = c(0.001, 0.95)))

  ps_no_median <- ps
  ps_no_median$mse_quants$amc_quant <- ps_no_median$mse_quants$amc_quant %>%
    rename(notright = country)
  expect_error(plot_timeseries(ps_no_median, type = "aac", by_country = TRUE))

})

# Use following command to add new plots
# vdiffr::manage_cases()

test_that("AAC and AAS - Tests for plots matching previous ones", {
  p <- plot_timeseries(ps, type = "aas")
  vdiffr::expect_doppelganger("plot-aa-survey", p)
  p <- plot_timeseries(ps, type = "aas", by_country = TRUE)
  vdiffr::expect_doppelganger("plot-aa-survey-country", p)
  p <- plot_timeseries(ps, type = "aac")
  vdiffr::expect_doppelganger("plot-aa-catch", p)
  p <- plot_timeseries(ps, type = "aac", by_country = TRUE)
  vdiffr::expect_doppelganger("plot-aa-catch-country", p)
})

test_that("Catch - Tests for plots matching previous ones", {
  p <- plot_timeseries(ps, type = "catch")
  vdiffr::expect_doppelganger("plot-catch", p)
  p <- plot_timeseries(ps, type = "catch", ci_lines = FALSE)
  vdiffr::expect_doppelganger("plot-catch-ribbon", p)
})

test_that("SSB - Tests for plots matching previous ones", {
  p <- plot_timeseries(ps, type = "ssb")
  vdiffr::expect_doppelganger("plot-ssb", p)
  p <- plot_timeseries(ps, type = "ssb", show_ssb0 = FALSE)
  vdiffr::expect_doppelganger("plot-ssb-no-ssb0", p)
  p <- plot_timeseries(ps, type = "ssb", show_40_10 = FALSE)
  vdiffr::expect_doppelganger("plot-ssb-no-4010", p)
  p <- plot_timeseries(ps, type = "ssb", ci_lines = FALSE, by_country = TRUE)
  vdiffr::expect_doppelganger("plot-ssb-ribbon-country", p)
})

test_that("SSB/SSB0 - Tests for plots matching previous ones", {
  p <- plot_timeseries(ps, type = "ssb_ssb0")
  vdiffr::expect_doppelganger("plot-ssb-ssb0", p)
  p <- plot_timeseries(ps, type = "ssb_ssb0", show_ssb0 = FALSE)
  vdiffr::expect_doppelganger("plot-ssb_ssb0-no-ssb0", p)
  p <- plot_timeseries(ps, type = "ssb_ssb0", show_40_10 = FALSE)
  vdiffr::expect_doppelganger("plot-ssb_ssb0-no-4010", p)
})
