#' Calculated the estimated TAC vs the Realized TAC (from data)
#'
#' @param catch_floor The least possible catch that is allowed in the fishery per year
#'
#' @details SSB is a vector of equally spaced values between zero and double the
#' Operating model SSB0 value (for a single-space, single-season model). TAC is
#' zero for SSB / SSB0 less than 0.1, 0.4\ * SSB for values greater than 0.4, and
#' for values between 0.1 and 0.4, TAC is 0.4\* SSB \* (SSB - 0.1SSB0) \*
#' the ratio (0.4 \* SBB0 / SSB) / (0.4 \* SSB0 - 0.1SSB0)
#' @return A list of 3 items. (1) A [data.frame] as read in from the TAC.csv data file
#' found in this package. (2) A [data.frame] with `ssb` and `tac` as columns, and
#' (3) A [data.frame] as calculated by this function
#' @importFrom readr read_csv cols col_double
#' @export
calc_tac_est_vs_real <- function(catch_floor = 180000){

  tac_fn <- system.file(file.path("extdata", "TAC.csv"),
                        package = "pacifichakemse",
                        mustWork = TRUE)
  df_tac <- read_csv(tac_fn, col_types = cols())
  df <- load_data_om(n_season = 1, n_space = 1)
  sim_data <- run_om(df)

  # Calculate the theoretical TAC
  ssb <- seq(0, sum(sim_data$SSB0) * 2, length.out = nrow(df_tac))
  ssb0 <- sum(sim_data$SSB0)
  tac <- matrix(NA, nrow = nrow(df_tac))
  # add a very low catch (fix later)
  tac[(ssb / ssb0) < 0.1] <- 0
  # For simplicity assume SSB is the same as V (it's close)
  tac[(ssb / ssb0) > 0.4] <- 0.4 * ssb[(ssb / ssb0) > 0.4]
  ix <- (ssb / ssb0) >= 0.1 & (ssb / ssb0) <= 0.4
  # TODO: Check the second-to-last term below. Should it be (0.4 * ssb[ix] / ssb0) ?
  tac[ix] <- 0.4 * ssb[ix] * ((ssb[ix] - 0.1 * ssb0) *
                                ((0.4 * ssb0 / ssb[ix]) /
                                   (0.4 * ssb0 - 0.1 * ssb0)))
  df_tac_ssb <- data.frame(ssb = ssb, tac = tac)

  # Do a regression on the difference between data (df_tac) and estimated
  # (df_tac_ssb)
  lm_historical <- lm(TAC ~ AssessTac, data = df_tac)
  lm_realized <- lm(Realized ~ AssessTac, data = df_tac)
  df_adjTAC <- data.frame(incpt = c(lm_historical$coefficients[1],
                                    lm_realized$coefficients[1]),
                          slp = c(lm_historical$coefficients[2],
                                  lm_realized$coefficients[2]),
                          adj = c("historical", "realized"))

  df_plot <- data.frame(tac = df_tac_ssb$tac,
                        tac_historical = predict(lm_historical,
                                                 newdata = data.frame(AssessTac = df_tac_ssb$tac)),
                        tac_realized = predict(lm_realized,
                                               newdata = data.frame(AssessTac = df_tac_ssb$tac)),
                        ssb = df_tac_ssb$ssb) %>%
    mutate(tac_historical = ifelse(tac_historical > tac, tac_historical, tac)) %>%
    mutate(tac_realized = ifelse(tac_realized > tac, tac_realized, tac)) %>%
    mutate(floor = tac * 0.5) %>%
    mutate(floor = ifelse(floor <= catch_floor, catch_floor, floor)) %>%
    mutate(tac_hcr = tac) %>%
    select(-ssb)

  df_plot <- melt(df_plot, id.vars = "tac", value.name = "Quota", variable.name = "HCR")
  cols <- pnw_palette("Starfish", n = length(unique(df_plot$HCR)), type = "discrete")

  df_plot$HCR <- factor(df_plot$HCR,
                        levels = c("tac_hcr",
                                   "tac_historical",
                                   "tac_realized",
                                   "floor"))
  attr(df_plot, "cols") <- cols
  list(tac = df_tac,
       tac_ssb = df_tac_ssb,
       plot = df_plot)
}