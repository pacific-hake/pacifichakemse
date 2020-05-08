context("Test the plot_ssb() function")

ps <- NULL
ps$mse_values_agg$ssb_quant <- tibble::tribble(
  ~country, ~year,   ~`0.05`,   ~`0.25`,   ~`0.5`,    ~`0.75`,   ~`0.95`,     ~avg,            ~run,
  "Canada",  2000,  65521.54,  65521.54,  65521.54,  65521.54,  65521.54,  65521.54, "Base scenario",
  "Canada",  2001, 122592.46, 122592.46, 122592.46, 122592.46, 122592.46, 122592.46, "Base scenario",
  "Canada",  2002, 132635.81, 132635.81, 132635.81, 132635.81, 132635.81, 132635.81, "Base scenario",
      "US",  2000, 622941.10, 622941.10, 622941.10, 622941.10, 622941.10, 622941.10, "Base scenario",
      "US",  2001, 839786.99, 839786.99, 839786.99, 839786.99, 839786.99, 839786.99, "Base scenario",
      "US",  2002,1582862.25,1582862.25,1582862.25,1582862.25,1582862.25,1582862.25, "Base scenario",
  "Canada",  2000,  65521.54,  65521.54,  65521.54,  65521.54,  65521.54,  65521.54, "Historical TAC",
  "Canada",  2001, 122592.46, 122592.46, 122592.46, 122592.46, 122592.46, 122592.46, "Historical TAC",
  "Canada",  2002, 132635.81, 132635.81, 132635.81, 132635.81, 132635.81, 132635.81, "Historical TAC",
      "US",  2000, 622941.10, 622941.10, 622941.10, 622941.10, 622941.10, 622941.10, "Historical TAC",
      "US",  2001, 839786.99, 839786.99, 839786.99, 839786.99, 839786.99, 839786.99, "Historical TAC",
      "US",  2002,1582862.25,1582862.25,1582862.25,1582862.25,1582862.25,1582862.25, "Historical TAC",
  "Canada",  2000,  65521.54,  65521.54,  65521.54,  65521.54,  65521.54,  65521.54, "Realized",
  "Canada",  2001, 122592.46, 122592.46, 122592.46, 122592.46, 122592.46, 122592.46, "Realized",
  "Canada",  2002, 132635.81, 132635.81, 132635.81, 132635.81, 132635.81, 132635.81, "Realized",
      "US",  2000, 622941.10, 622941.10, 622941.10, 622941.10, 622941.10, 622941.10, "Realized",
      "US",  2001, 839786.99, 839786.99, 839786.99, 839786.99, 839786.99, 839786.99, "Realized",
      "US",  2002,1582862.25,1582862.25,1582862.25,1582862.25,1582862.25,1582862.25, "Realized",
  "Canada",  2000,  65521.54,  65521.54,  65521.54,  65521.54,  65521.54,  65521.54, "Floor 50",
  "Canada",  2001, 122592.46, 122592.46, 122592.46, 122592.46, 122592.46, 122592.46, "Floor 50",
  "Canada",  2002, 132635.81, 132635.81, 132635.81, 132635.81, 132635.81, 132635.81, "Floor 50",
      "US",  2000, 622941.10, 622941.10, 622941.10, 622941.10, 622941.10, 622941.10, "Floor 50",
      "US",  2001, 839786.99, 839786.99, 839786.99, 839786.99, 839786.99, 839786.99, "Floor 50",
      "US",  2002,1582862.25,1582862.25,1582862.25,1582862.25,1582862.25,1582862.25, "Floor 50")

probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

test_that("plot_ssb() - Tests for argument errors", {
  expect_error(plot_ssb(ps = NULL, ci = c(0.05, 0.95)))
  expect_error(plot_ssb(ps = ps, ci = NULL))
  ps_wrongcol <- ps
  ps_wrongcol$mse_values_agg$ssb_quant <- ps$mse_values_agg$ssb_quant %>%
    dplyr::rename(cntry = country)
  expect_error(plot_ssb(ps = ps_wrongcol, ci = c(0.05, 0.95)))
  expect_error(plot_ssb(ps = ps_wrongcol, ci = c(0.05, 0.94)))
  expect_error(plot_ssb(ps = ps, ci = c("a", "b")))
  expect_error(plot_ssb(ps = ps, ci = c(0.05, 0.5, 0.95)))
})

test_that("plot_ssb() - Tests for plot layers matching expectations", {
  p <- plot_ssb(ps = ps, ci = c(0.05, 0.95))
  vdiffr::expect_doppelganger("plot-ssb", p)
  # Use following command to add new plots
  # vdiffr::manage_cases()
})

