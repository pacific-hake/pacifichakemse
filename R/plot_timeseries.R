#' Plot time series data from an MSE or OM simulation run
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()].
#' @param type One of 'ssb', 'ssb_ssb0', catch', 'recr', aas', 'aac','aap'
#' @param time Either 'beg' for beginning of the year SSB or 'mid' for mid-year SSB.
#' Only used if `type` is 'ssb' or 'ssb_ssb0'.
#' @param ci A vector of length two of the lower and upper credible interval values.
#' @param by_country If `TRUE`, the plot will be N panels of plots comparing the scenarios by country.
#' N is the number of scenarios. If `FALSE` the plot will be one panel comparing the scenarios.
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used.
#' @param ylim A vector of 2 for limits of the y-axis. If either are NA,
#' the limits of the data are used.
#' @param ci_lines If `TRUE`, use dashed lines for the CI envelope. If `FALSE`, use a shaded ribbon.
#' @param show_ci If `TRUE`, show the confidence interval, if `FALSE`, only show the median
#' @param show_ssb0 If `TRUE`, show the initial biomass, SSB0 line.
#' Only used if `type` is 'ssb' or 'ssb_ssb0'.
#' @param show_hcr If `TRUE`, show the 0.4 initial biomass and 0.1 initial biomass lines.
#' Only used if `type` is 'ssb' or 'ssb_ssb0'.
#' @param show_hcr_values If `TRUE`, show the SSB values for the HCR lines in the margin
#' Only used if `type` is 'ssb' or 'ssb_ssb0'.
#' @param show_y_values If `TRUE`, show the Y-axis values
#' @param rev_scenarios If `TRUE`, reverse the order of the scenarios in the legend.
#' @param legend_position A vector of two - X and Y position for the legend. See [ggplot2::theme()]
#' @param ssb_line_txt_cex Size multiplier for SSB reference point line labels.
#' @param ssb_line_col Color for SSB reference point line labels.
#' @param ssb_line_type Line type for SSB reference point line labels.
#' @param ... Extra arguments to pass to [color_facet_backgrounds()]
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom ggplot2 expand_limits sec_axis
#' @export
plot_timeseries <- function(ps = NULL,
                            type = "ssb",
                            time = "mid",
                            ci = c(0.05, 0.95),
                            by_country = FALSE,
                            yr_lim = c(NA_real_, NA_real_),
                            ylim = c(NA_real_, NA_real_),
                            ci_lines = TRUE,
                            show_ci = TRUE,
                            show_ssb0 = TRUE,
                            show_hcr = TRUE,
                            show_hcr_values = TRUE,
                            show_y_values = TRUE,
                            rev_scenarios = FALSE,
                            legend_position = c(0.26, 0.85),
                            ssb_line_txt_cex = 0.8,
                            ssb_line_col = "black",
                            ssb_line_type = 2,
                            ...){

  if(type == "ssb"){
    if(by_country){
      if(time == "beg"){
        y_label <- "Beginning-of-year Spawning biomass \n(million tonnes)"
        d <- ps$mse_quants$ssb_quant_country
      }else{
        y_label <- "Mid-year Spawning biomass \n(million tonnes)"
        d <- ps$mse_quants$ssb_mid_quant_country
      }
    }else{
      if(time == "beg"){
        y_label <- "Beginning-of-year Spawning biomass \n(million tonnes)"
        d <- ps$mse_quants$ssb_all_quant
      }else{
        y_label <- "Mid-year Spawning biomass \n(million tonnes)"
        d <- ps$mse_quants$ssb_mid_quant
      }
    }
    # SSB0 - All scenarios and runs are the same so just use the first scenario, first run
    ssb0 <- sum(ps$sim_data[[1]][[1]]$ssb_0) * 1e-6
    y_factor <- 1e-6
  }else if(type == "ssb_ssb0"){
    ssb0 <- 1
    subsc <- 0
    y_factor <- 1
    if(time == "beg"){
      y_label <- bquote("Beginning-of-year total SSB / SSB"[.(subsc)])
      d <- ps$mse_quants$ssb_ssb0_quant
    }else{
      y_label <- bquote("Mid-year total SSB / SSB"[.(subsc)])
      d <- ps$mse_quants$ssb_ssb0_mid_quant
    }
  }else if(type == "vb"){
    d <- ps$mse_quants$v_all_quant
    y_label <- "Vulnerable Biomass (million tonnes)"
    y_factor <- 1
  }else if(type == "catch"){
    if(by_country){
      d <- ps$mse_quants$catch_quant_country
    }else{
      d <- ps$mse_quants$catch_quant
    }
    y_label <- "Catch (million tonnes)"
    y_factor <- 1e-6
  }else if(type == "catch_obs"){
    d <- ps$mse_quants$catch_obs_quant
    y_label <- "Catch (million tonnes)"
    y_factor <- 1e-6
  }else if(type == "aas"){
    if(by_country){
      d <- ps$mse_quants$ams_quant
    }else{
      d <- ps$mse_quants$ams_all_quant
    }
    y_label <- "Average age in survey"
    y_factor <- 1
  }else if(type == "recr"){
    if(by_country){
      d <- ps$mse_quants$r_quant_country
    }else{
      d <- ps$mse_quants$r_quant
    }
    y_label <- "Recruitment (millions)"
    y_factor <- 1e-6
  }else if(type == "aac"){
    if(by_country){
      d <- ps$mse_quants$amc_quant
    }else{
      d <- ps$mse_quants$amc_all_quant
    }
    y_label <- "Average age in catch"
    y_factor <- 1
  }else if(type == "aap"){
    if(by_country){
      d <- ps$mse_quants$aap_quant
    }else{
      d <- ps$mse_quants$aap_all_quant
    }
    y_label <- "Average age in the population"
    y_factor <- 1
  }else if(type == "catch_quota"){
    d <- ps$mse_quants$catch_quota_quant
    y_label <- "Catch / Quota"
    ssb0 <- 1
    y_factor <- 1
  }

  stopifnot("0.5" %in% names(d))
  stopifnot("scenario" %in% names(d))
  stopifnot(all(ci %in% names(d)))
  if(by_country){
    stopifnot("country" %in% names(d))
  }

  ci <- as.character(ci) %>% map(~{sym(.x)})
  #cols <- pnw_palette("Starfish", n = length(ps$plotnames), type = "discrete")
  if(by_country){
    cols <- c("red", "blue")
    facet_back_cols <- ps$cols
  }else{
    cols <- ps$cols
  }

  # Reorder the legend and colors
  if(rev_scenarios){
    d <- d %>%
      mutate(scenario = fct_relevel(scenario, rev(levels(d$scenario))))
  }

  d <- d %>%
    filter(!is.na(`0.5`))

  if(by_country){
    g <- ggplot(d, aes(x = year, y = `0.5` * y_factor, color = country, fill = country))
  }else{
    g <- ggplot(d, aes(x = year, y = `0.5` * y_factor, color = scenario, fill = scenario))
  }
  g <- g +
    geom_line(size = 1.5) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5),
          legend.position = legend_position) +
    scale_color_manual(values = cols) +
    coord_cartesian(xlim = yr_lim,
                    ylim = ylim) +
    theme(legend.title = element_blank())

  if(!show_y_values){
    g <- g +
      theme(axis.text.y.left = element_blank(),
            axis.ticks.y = element_blank())
  }

  if(show_ci){
    if(ci_lines){
      if(by_country){
        g <- g +
          geom_line(aes(y = !!ci[[1]] * y_factor, color = country), linetype = 2) +
          geom_line(aes(y = !!ci[[2]] * y_factor, color = country), linetype = 2)
      }else{
        g <- g +
          geom_line(aes(y = !!ci[[1]] * y_factor, color = scenario), linetype = 2) +
          geom_line(aes(y = !!ci[[2]] * y_factor, color = scenario), linetype = 2)
      }
    }else{
      g <- g +
        geom_ribbon(aes(ymin = !!ci[[1]] * y_factor, ymax = !!ci[[2]] * y_factor), linetype = 0) +
        scale_fill_manual(values = alpha(cols, alpha = 0.2))
    }
  }
  # Breaks and labels for the SSB lines if they are to be shown. This is for the third axis on the right
  b_brks <- NULL
  b_lbls <- NULL
  if(show_ssb0 && type %in% c("ssb", "ssb_ssb0", "catch_quota")){

    if(type == "ssb"){
      ssb0_lab <- as.expression(bquote(SSB[.(0)] == .(round(ssb0, 2))))
    }else{
      ssb0_lab <- as.expression(bquote(SSB[.(0)]))
    }
    b_brks <- round(ssb0, 2)
    b_lbls <- ssb0_lab

    g <- g +
      geom_hline(aes(yintercept = ssb0),
                 color = ssb_line_col,
                 linetype = ssb_line_type)
  }

  if(show_hcr && type %in% c("ssb", "ssb_ssb0")){
    if(type == "ssb"){
      hcr_upper_vals <- map(ps$sim_data, ~{
        .x[[1]]$hcr_upper
      })
      hcr_upper_vals <- hcr_upper_vals[!map_lgl(hcr_upper_vals, is.null)]
      if(length(unique(hcr_upper_vals)) != 1){
        stop("There is more than one hcr_upper value in the list given, cannot plot more than one upper HCR line.\n",
             "When you ran run_oms() did you set some scenario's hcr_apply argument to TRUE and others to FALSE?",
             call. = FALSE)
      }
      hcr_upper_val <- unique(hcr_upper_vals)[[1]]
      hcr_lower_vals <- map(ps$sim_data, ~{
        .x[[1]]$hcr_lower
      })
      hcr_lower_vals <- hcr_lower_vals[!map_lgl(hcr_lower_vals, is.null)]
      if(length(unique(hcr_lower_vals)) != 1){
        stop("There is more than one hcr_lower value in the list given, cannot plot more than one lower HCR line\n",
             "When you ran run_oms() did you set some scenario's hcr_apply argument to TRUE and others to FALSE?",
             call. = FALSE)
      }

      hcr_lower_val <- unique(hcr_lower_vals)[[1]]
      if(!show_hcr_values){
        ssb_upper_lab <- as.expression(bquote(SSB[upper]))
        ssb_lower_lab <- as.expression(bquote(SSB[lower]))
      }else{
        ssb_upper_lab <- as.expression(bquote(SSB[.(hcr_upper_val)] == .(round(ssb0 * hcr_upper_val, 2))))
        ssb_lower_lab <- as.expression(bquote(SSB[.(hcr_lower_val)] == .(round(ssb0 * hcr_lower_val, 2))))
      }
    }else{
      ssb_upper_lab <- as.expression(bquote(SSB[.(hcr_upper_val)]))
      ssb_lower_lab <- as.expression(bquote(SSB[.(hcr_lower_val)]))
    }

    b_brks <- c(b_brks,
                round(ssb0 * hcr_upper_val, 2),
                round(ssb0 * hcr_lower_val, 2))
    b_lbls <- c(b_lbls,
                ssb_upper_lab,
                ssb_lower_lab)

    g <- g +
      geom_hline(aes(yintercept = ssb0 * hcr_upper_val),
                 color = ssb_line_col,
                 linetype = ssb_line_type) +
      geom_hline(aes(yintercept = ssb0 * hcr_lower_val),
                 color = ssb_line_col,
                 linetype = ssb_line_type)
  }

  if(is.null(b_brks)){
    g <- g +
      scale_y_continuous(name = y_label,
                         expand = c(0, 0))
  }else{
    g <- g +
      scale_y_continuous(name = y_label,
                         expand = c(0, 0),
                         sec.axis = sec_axis(~ ., breaks = b_brks, labels = b_lbls))
  }

  if(type == "catch_quota"){
    g <- g + coord_cartesian(ylim = c(.4, 1.1)) +
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       vjust = 0.5),
            legend.position = legend_position)
  }
  if(by_country){
    g <- g + facet_wrap(~scenario)
    return(color_facet_backgrounds(g, facet_back_cols, ...))
  }

  # Code to override clipping (allow plot lines to extend outside plot area)
  # gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(g))
  # gt$layout$clip[gt$layout$name == "panel"] <- "off"
  # grid::grid.draw(gt)

  g
}

