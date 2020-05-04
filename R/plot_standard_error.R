#' Plot Standard Error of the Spawning Stock Biomass between the Operating model and
#' the Estimation model
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param quants A vector of quantiles to pass to [stats::quantile()]. Median is also
#' calculated
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_standard_error <- function(ps = NULL,
                                quants = c(0.05, 0.95)){
  stopifnot(!is.null(ps))
  stopifnot(!is.null(quants[1]))
  stopifnot(length(quants) == 2)

  se <- ps$standard_error_ssb %>%
    group_by(scenario, year) %>%
    summarise(lower = quantile(SE.SSB, quants[1], na.rm = TRUE),
              mid = median(SE.SSB, na.rm = TRUE),
              upper = quantile(SE.SSB, quants[2], na.rm = TRUE))

  g <- ggplot(se, aes(x = year, y = lower)) +
    theme_classic() +
    geom_line(size = 1.5) +
    facet_wrap(~scenario) +
    geom_hline(yintercept = 0.0, linetype = 2) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = alpha("gray", alpha = 0.5)) +
    scale_y_continuous(name = "Standard error")
  g
}