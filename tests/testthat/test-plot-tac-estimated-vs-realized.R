
# ps <- NULL
# ps$cols <- PNWColors::pnw_palette("Starfish", n = 4, type = "discrete")
# ps$plotnames <- c("Base scenario", "Historical TAC", "Realized", "Floor 50")
#
# test_that("plot_tac_vs_hcr() - Tests for argument errors", {
#   expect_error(plot_tac_vs_hcr(ps = NULL))
# })
#
# test_that("plot_tac_vs_hcr() - Tests for plots matching previous ones", {
#   p <- plot_tac_vs_hcr(ps = ps)
#   suppressWarnings(
#     vdiffr::expect_doppelganger("plot-tac-estimated-vs-realized", p))
#   # Use following command to add new plots
#   # vdiffr::manage_cases()
# })

