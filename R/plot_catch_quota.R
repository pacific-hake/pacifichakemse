#' Plot Catch/Quota by scenario
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param ci A vector of length two of the lower and upper credible interval values.
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used
#' @param y_lim A vector of 2 for minimum and maximum y axis values
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch_quota <- function(ps = NULL,
                             ci = c(0.05, 0.95),
                             yr_lim = c(NA_real_, NA_real_),
                             y_lim = c(NA_real_, NA_real_)){

  verify_argument(ps, "list")
  verify_argument(ci, "numeric", 2)
  verify_argument(yr_lim, "numeric", 2)
  verify_argument(y_lim, "numeric", 2)

  cqct <- ps$mse_quants$catch_quota_quant
  stopifnot("0.5" %in% names(cqct))
  stopifnot(all(ci %in% names(cqct)))

  ci <- as.character(ci) %>% map(~{sym(.x)})

  g <- ggplot(cqct,
              aes(x = year, y = `0.5`, color = scenario)) +
    geom_line(size = 1.5) +
    scale_color_manual(values = ps$cols) +
    scale_y_continuous(name = "Catch/quota") +
    geom_line(aes(y = !!ci[[1]] * 1e-6, color = scenario), linetype = 2) +
    geom_line(aes(y = !!ci[[2]] * 1e-6, color = scenario), linetype = 2) +
    geom_hline(aes(yintercept = 1),
               color = "black",
               linetype = 2) +
    theme(legend.position = c(0.15, 0.2),
          legend.title = element_blank(),
          legend.box.background = element_blank()) +
    coord_cartesian(xlim = yr_lim, ylim = y_lim)

  g
}

#' Plot Catch/Quota by scenario all in one panel
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch_quota_1panel <- function(ps = NULL){

  stopifnot(!is.null(ps))

  cq <- ps$mse_values_agg$catchq
  g <- ggplot(cq, aes(x = year, y = med.tot, color = run)) +
    geom_line(size = 1.4) +
    scale_color_manual(values = ps$cols) +
    geom_line(aes(y = p5.tot), linetype = 2, size = 0.9) +
    geom_line(aes(y = p95.tot), linetype = 2, size = 0.9) +
    theme_classic() +
    scale_y_continuous(name ="Catch/quota") +
    geom_hline(aes(yintercept = 1),
               color = "black",
               linetype = 2) +
    theme(legend.position = c(0.2, 0.4),
          legend.title = element_blank())
  g
}