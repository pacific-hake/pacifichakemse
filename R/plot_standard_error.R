#' Plot Standard Error of the Spawning Stock Biomass between the Operating model and
#' the Estimation model
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' #' @param ci A vector of length two of the lower and upper credible interval values.
#' These values will be passed to [stats::quantile()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_standard_error <- function(ps = NULL,
                                ci = c(0.05, 0.95)){
  stopifnot(!is.null(ps))
  stopifnot(!is.null(ci))

  se <- ps$standard_error_ssb
  stopifnot("0.5" %in% names(se))
  stopifnot(is.numeric(ci))
  stopifnot(all(ci %in% names(se)))
  stopifnot(length(ci) == 2)

  ci <- as.character(ci) %>% map(~{sym(.x)})

  g <- ggplot(se, aes(x = year, y = `0.5`)) +
    theme_classic() +
    geom_line(size = 1.5) +
    facet_wrap(~run) +
    geom_hline(yintercept = 0.0, linetype = 2) +
    geom_ribbon(aes(ymin = !!ci[[1]],
                    ymax = !!ci[[2]]),
                fill = alpha("gray",
                             alpha = 0.5)) +
    scale_y_continuous(name = "Standard error")
  g
}