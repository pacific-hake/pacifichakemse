#' Plot Standard Error of the Spawning Stock Biomass between the Operating model and
#' the Estimation model
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param ci A vector of length two of the lower and upper credible interval values.
#' These values will be passed to [stats::quantile()]
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used
#' @param ... Extra arguments to be passed to [color_facet_backgrounds()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_standard_error <- function(ps = NULL,
                                ci = c(0.05, 0.95),
                                yr_lim = c(NA_real_, NA_real_),
                                ...){

  verify_argument(ps, "list")
  verify_argument(ci, "numeric", 2)
  verify_argument(yr_lim, "numeric", 2)

  se <- ps$standard_error_ssb
  if(is.null(se) || !length(se)){
    stop("The ps$standard_error_ssb data frame is empty.\n",
         "Supply a `ps` object that holds the results of a full MSE.\n",
         "This `ps` object you supplied is likely an OM-only model.",
         call. = FALSE)
  }
  stopifnot("0.5" %in% names(se))
  stopifnot(all(ci %in% names(se)))
  stopifnot(length(ci) == 2)

  ci <- as.character(ci) %>% map(~{sym(.x)})
  facet_back_cols <- ps$cols

  g <- ggplot(se, aes(x = year, y = `0.5`)) +
    geom_line(size = 1.5) +
    facet_wrap(~scenario) +
    geom_hline(yintercept = 0.0, linetype = 2) +
    geom_ribbon(aes(ymin = !!ci[[1]],
                    ymax = !!ci[[2]]),
                fill = alpha("gray",
                             alpha = 0.5)) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5),
          legend.position = c(0.1, 0.9)) +
    scale_y_continuous(name = "Standard error") +
    coord_cartesian(xlim = yr_lim)

  color_facet_backgrounds(g, facet_back_cols, ...)
}