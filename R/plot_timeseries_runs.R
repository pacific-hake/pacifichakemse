#' Plot individual run output against each other as a time series
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()].
#' @param type One of 'ssb', 'ssb_ssb0', catch', 'aas', 'aac','aap'
#' @param scen The scenario number to use. This is an integer representing the names given by `names(ps$sim_data)`
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used.
#' @param ylim A vector of 2 for limits of the y-axis. If either are NA,
#' the limits of the data are used.
#' @param legend_position A vector of two - X and Y position for the legend. See [ggplot2::theme()]
#' @param line_width Width of the lines. See [ggplot2::geom_line()]
#' @param line_type Type of the lines. See [ggplot2::geom_line()]
#'
#' @return
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 scale_color_gradientn
#' @export
plot_timeseries_runs <- function(ps,
                                 type = "ssb",
                                 scen = 1,
                                 yr_lim = c(NA_real_, NA_real_),
                                 ylim = c(NA_real_, NA_real_),
                                 legend_position = c(0.26, 0.85),
                                 line_width = 1,
                                 line_type = "solid"){

  if(type == "ssb"){
    y_label <- "Mid-year Spawning biomass \n(million tonnes)"
    d <- ps$merged_run_data[[scen]]$ssb_plot %>%
      rename(value = ssb)
  }else if(type == "catch"){
    y_label <- "Catch (million t)"
    d <- ps$merged_run_data[[scen]]$catch_obs_plot %>%
      rename(value = catch) %>%
      mutate(value = value / 1e6)
  }else if(type == "recr"){
    y_label <- "Recruitment (millions)"
    d <- ps$merged_run_data[[scen]]$r_all %>%
      mutate(value = value / 1e6)
  }

  n_col <- length(unique(d$run))
  max_year <- max(unique(d$year))
  tmp <- d %>% filter(year == max_year)
  end_order <- tmp[rev(order(tmp$value)), ]
  pal <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  d <- d %>% rename(Year = year)

  g <- ggplot(d, aes(x = Year, y = value, group = run, color = run)) +
    geom_line(size = line_width, linetype = line_type) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5),
          legend.position = legend_position) +
    scale_color_gradientn(colors = pal(n_col)) +
        coord_cartesian(xlim = yr_lim,
                        ylim = ylim) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(name = y_label,
                       expand = c(0, 0),
                       sec.axis = sec_axis(~ ., breaks = end_order$value, labels = end_order$run))
    theme(legend.title = element_blank())

    message("Order of runs from highest to lowest value (", length(end_order$run), " runs) for the terminal year is:")
    print(end_order$run)
  g
}