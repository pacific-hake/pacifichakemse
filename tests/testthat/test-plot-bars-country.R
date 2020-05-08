context("Test the plot_bars_country() function")

ps <- NULL
ps$cols <- PNWColors::pnw_palette("Starfish", n = 4, type = "discrete")
ps$country_season_indicators <- tibble::tribble(
           ~indicator,       ~value,             ~HCR,  ~country,     ~season,
   "Canada TAC/V spr",  2547.186233,  "Base scenario",  "Canada",   "Apr-Jun",
   "Canada TAC/V sum",  1094.430019,  "Base scenario",  "Canada", "July-Sept",
  "Canada TAC/V fall",  3629.076840,  "Base scenario",  "Canada",   "Oct-Dec",
       "US TAC/V spr",  1239.766206,  "Base scenario",      "US",   "Apr-Jun",
       "US TAC/V sum",   778.157355,  "Base scenario",      "US", "July-Sept",
      "US TAC/V fall",   774.239265,  "Base scenario",      "US",   "Oct-Dec",
   "Canada TAC/V spr",    15.354829, "Historical TAC",  "Canada",   "Apr-Jun",
   "Canada TAC/V sum",     6.217072, "Historical TAC",  "Canada", "July-Sept",
  "Canada TAC/V fall",    17.819801, "Historical TAC",  "Canada",   "Oct-Dec",
       "US TAC/V spr",    12.652929, "Historical TAC",      "US",   "Apr-Jun",
       "US TAC/V sum",     8.194516, "Historical TAC",      "US", "July-Sept",
      "US TAC/V fall",     8.088099, "Historical TAC",      "US",   "Oct-Dec",
   "Canada TAC/V spr",    20.860059,       "Realized",  "Canada",   "Apr-Jun",
   "Canada TAC/V sum",     8.574779,       "Realized",  "Canada", "July-Sept",
  "Canada TAC/V fall",    25.556176,       "Realized",  "Canada",   "Oct-Dec",
       "US TAC/V spr",    15.373293,       "Realized",      "US",   "Apr-Jun",
       "US TAC/V sum",     9.897464,       "Realized",      "US", "July-Sept",
      "US TAC/V fall",     9.777941,       "Realized",      "US",   "Oct-Dec",
   "Canada TAC/V spr",    19.409247,       "Floor 50",  "Canada",   "Apr-Jun",
   "Canada TAC/V sum",     7.946516,       "Floor 50",  "Canada", "July-Sept",
  "Canada TAC/V fall",    23.452772,       "Floor 50",  "Canada",   "Oct-Dec",
       "US TAC/V spr",    14.558354,       "Floor 50",      "US",   "Apr-Jun",
       "US TAC/V sum",     9.372939,       "Floor 50",      "US", "July-Sept",
      "US TAC/V fall",     9.243503,       "Floor 50",      "US",   "Oct-Dec")

test_that("plot_bars_country() - Tests for argument errors", {
  expect_error(plot_bars_country(ps = NULL))
})

test_that("plot_ssb() - Tests for plots matching previous ones", {
  p <- plot_bars_country(ps = ps)
  vdiffr::expect_doppelganger("plot-bars-country", p)
  # Use following command to add new plots
  # vdiffr::manage_cases()
})

