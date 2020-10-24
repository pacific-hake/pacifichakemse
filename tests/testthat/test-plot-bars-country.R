context("Test the plot_bars_country() function")

ps <- NULL
ps$cols <- PNWColors::pnw_palette("Starfish", n = 4, type = "discrete")
ps$country_season_indicators <- tibble::tribble(
           ~indicator,       ~value,  ~run,            ~hcr,  ~country,     ~season,
   "Canada TAC/V spr",  2547.186233,     1,  "Base scenario",  "Canada",   "Apr-Jun",
   "Canada TAC/V sum",  1094.430019,     1,  "Base scenario",  "Canada", "July-Sept",
  "Canada TAC/V fall",  3629.076840,     1,  "Base scenario",  "Canada",   "Oct-Dec",
       "US TAC/V spr",  1239.766206,     1,  "Base scenario",      "US",   "Apr-Jun",
       "US TAC/V sum",   778.157355,     1,  "Base scenario",      "US", "July-Sept",
      "US TAC/V fall",   774.239265,     1,  "Base scenario",      "US",   "Oct-Dec",
   "Canada TAC/V spr",    15.354829,     1, "Historical TAC",  "Canada",   "Apr-Jun",
   "Canada TAC/V sum",     6.217072,     1, "Historical TAC",  "Canada", "July-Sept",
  "Canada TAC/V fall",    17.819801,     1, "Historical TAC",  "Canada",   "Oct-Dec",
       "US TAC/V spr",    12.652929,     1, "Historical TAC",      "US",   "Apr-Jun",
       "US TAC/V sum",     8.194516,     1, "Historical TAC",      "US", "July-Sept",
      "US TAC/V fall",     8.088099,     1, "Historical TAC",      "US",   "Oct-Dec",
   "Canada TAC/V spr",    20.860059,     1,      "Realized",  "Canada",   "Apr-Jun",
   "Canada TAC/V sum",     8.574779,     1,      "Realized",  "Canada", "July-Sept",
  "Canada TAC/V fall",    25.556176,     1,      "Realized",  "Canada",   "Oct-Dec",
       "US TAC/V spr",    15.373293,     1,      "Realized",      "US",   "Apr-Jun",
       "US TAC/V sum",     9.897464,     1,      "Realized",      "US", "July-Sept",
      "US TAC/V fall",     9.777941,     1,      "Realized",      "US",   "Oct-Dec",
   "Canada TAC/V spr",    19.409247,     1,      "Floor 50",  "Canada",   "Apr-Jun",
   "Canada TAC/V sum",     7.946516,     1,      "Floor 50",  "Canada", "July-Sept",
  "Canada TAC/V fall",    23.452772,     1,      "Floor 50",  "Canada",   "Oct-Dec",
       "US TAC/V spr",    14.558354,     1,      "Floor 50",      "US",   "Apr-Jun",
       "US TAC/V sum",     9.372939,     1,      "Floor 50",      "US", "July-Sept",
      "US TAC/V fall",     9.243503,     1,      "Floor 50",      "US",   "Oct-Dec")

test_that("plot_bars_country() - Tests for argument errors", {
  expect_error(plot_bars_country(ps = NULL))
})

test_that("plot_bars_country() - Tests for plots matching previous ones", {
  p <- plot_bars_country(ps = ps)
  vdiffr::expect_doppelganger("plot-bars-country", p)
  # Use following command to add new plots
  # vdiffr::manage_cases()
})

