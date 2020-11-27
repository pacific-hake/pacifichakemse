#' Plot the mean of the year medians SSB for all Estimation models
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()].
#' @param scen A number representing which scenario to plot
#' @param ci A vector of length two of the lower and upper credible interval values.
#' @param ci_lines If TRUE, use dashed lines for the CI envelope. If FALSE, use a shaded ribbon.
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used.
#' @param legend_position A vector of two - X and Y position for the legend. See [ggplot2::theme()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_em_timeseries <- function(ps,
                               scen = 1,
                               ci = c(0.05, 0.95),
                               ci_lines = TRUE,
                               yr_lim = c(NA_real_, NA_real_),
                               legend_position = "none"){

  verify_argument(ps, "list")
  verify_argument(ci, "numeric", 2)

  d <- ps$em_outputs$ssb_quants_by_year
  y_label = "Spawning biomass \n(million tonnes)"
  y_factor <- 1e-6 / 2

  scen_names <- unique(d$scenario)
  d <- d %>%
    filter(scenario == scen_names[scen])
  message("Plotting scenario ", scen_names[scen], " for EM SSB plot.")

  df <- d %>%
    group_by(year) %>%
    group_map(~{
      summarize_at(.x, .vars = vars(-scenario, -run), .funs = mean)
    }) %>%
    map_dfr(~{.x}) %>%
    mutate(year = as.numeric(unique(d$year))) %>%
    select(year, everything())

  stopifnot("scenario" %in% names(d))
  stopifnot("year" %in% names(d))
  stopifnot(all(ci %in% names(d)))

  ci <- as.character(ci) %>% map(~{sym(.x)})
  cols <- ps$cols

  g <- ggplot(df, aes(x = year, y = `0.5` * y_factor, fill = "black"))

  if(ci_lines){
    g <- g +
      geom_line(aes(y = !!ci[[1]] * y_factor), linetype = 2) +
      geom_line(aes(y = !!ci[[2]] * y_factor), linetype = 2)
  }else{
    g <- g +
      geom_ribbon(aes(ymin = !!ci[[1]] * y_factor, ymax = !!ci[[2]] * y_factor), linetype = 0) +
      scale_fill_manual(values = alpha("grey", alpha = 0.7))
  }

  g <- g +
    geom_line(size = 1.5) +
    scale_y_continuous(name = y_label) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5),
          legend.position = legend_position) +
    scale_color_manual(values = cols) +
    coord_cartesian(xlim = yr_lim) +
    theme(legend.title = element_blank())


  g
}