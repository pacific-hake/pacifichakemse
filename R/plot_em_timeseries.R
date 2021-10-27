#' Plot the mean of the year medians SSB for all Estimation models
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()].
#' @param type One of 'ssb', 'ssb_ssb0', catch', 'aas', 'aac','aap'
#' @param ci A vector of length two of the lower and upper credible interval values.
#' @param ci_lines If TRUE, use dashed lines for the CI envelope. If FALSE, use a shaded ribbon.
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used.
#' @param rev_scenarios If TRUE, reverse the order of the scenarios in the legend.
#' @param legend_position A vector of two - X and Y position for the legend. See [ggplot2::theme()]
#' @param free_y_scale See the `scales` argument of [ggplot2::facet_wrap()]
#' @param ... Extra arguments to pass to [color_facet_backgrounds()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_em_timeseries <- function(ps,
                               type = "ssb",
                               ci = c(0.05, 0.95),
                               ci_lines = TRUE,
                               yr_lim = c(NA_real_, NA_real_),
                               rev_scenarios = TRUE,
                               legend_position = c(0.15, 0.92),
                               free_y_scale = FALSE,
                               ...){

  y_factor <- 1
  if(type == "catch"){
    d <- ps$em_outputs$catch_quants_by_year_runmeans
    o <- ps$mse_quants$catch_quant
    y_label = "Catch\n(million tonnes)"
    y_factor <- 1e-6
  # }else if(type == "f"){
  #   d <- ps$em_outputs$f_quants_by_year_runmeans
  #   y_label = "Fishing mortality"
  # }else if(type == "r"){
  #   d <- ps$em_outputs$r_quants_by_year_runmeans
  #   y_label = "Recruitment"
  }else if(type == "ssb"){
    d <- ps$em_outputs$ssb_quants_by_year_runmeans
    o <- ps$mse_quants$ssb_all_quant
    y_label = "Spawning biomass \n(million tonnes)"
    y_factor <- 1e-6 / 2
  }#else if(type == "survey"){
  #   d <- ps$em_outputs$survey_quants_by_year_runmeans
  #   y_label = "Survey biomass"
  # }
  facet_back_cols <- ps$cols

  d <- d %>%
    mutate(scenario = as.factor(scenario)) %>%
    mutate(year = as.double(year)) %>%
    mutate(grp = "EM - Mean of run quantiles")
  o <- o %>%
    select(-avg) %>%
    select(scenario, everything()) %>%
    mutate(grp = "OM - Quantiles")

  d <- d %>%
    bind_rows(o)

  stopifnot("scenario" %in% names(d))
  stopifnot("year" %in% names(d))
  stopifnot(all(ci %in% names(d)))

  ci <- as.character(ci) %>% map(~{sym(.x)})

  # Reorder the legend and colors
  if(rev_scenarios){
    d <- d %>%
      mutate(scenario = fct_relevel(scenario, rev(levels(d$scenario))))
  }

  cols <- c("black", "blue")

  g <- ggplot(d, aes(x = year, y = `0.5` * y_factor, fill = grp, color = grp, group = grp))

  if(ci_lines){
    g <- g +
      geom_line(aes(y = !!ci[[1]] * y_factor), linetype = 2) +
      geom_line(aes(y = !!ci[[2]] * y_factor), linetype = 2)

  }else{
    g <- g +
      geom_ribbon(aes(ymin = !!ci[[1]] * y_factor, ymax = !!ci[[2]] * y_factor), linetype = 0, alpha = 0.3)
  }
  g <- g +
    geom_line(size = 1.5) +
    scale_y_continuous(name = y_label) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5),
          legend.position = legend_position) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    coord_cartesian(xlim = yr_lim) +
    theme(legend.title = element_blank())

  if(free_y_scale){
    g <- g +
      facet_wrap(~scenario, scales = "free_y")
  }else{
    g <- g +
      facet_wrap(~scenario)
  }

  color_facet_backgrounds(g, facet_back_cols, ...)
}