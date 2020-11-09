#' Plot time series data from an MSE or OM simulation run
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()].
#' @param type One of 'ssb', 'ssb_ssb0', catch', 'aas', 'aac'
#' @param time Either 'beg' for beginning of the year SSB or 'mid' for mid-year SSB.
#' Only used if `type` is 'ssb' or 'ssb_ssb0'.
#' @param ci A vector of length two of the lower and upper credible interval values.
#' @param by_country If TRUE, the plot will be N panels of plots comparing the scenarios by country.
#' N is the number of scenarios. If FALSE the plot will be one panel comparing the scenarios.
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used.
#' @param ci_lines If TRUE, use dashed lines for the CI envelope. If FALSE, use a shaded ribbon.
#' @param show_ssb0 If TRUE, show the initial biomass, SSB0 line.
#' Only used if `type` is 'ssb' or 'ssb_ssb0'.
#' @param show_40_10 If TRUE, show the 0.4 initial biomass and 0.1 initial biomass lines.
#' Only used if `type` is 'ssb' or 'ssb_ssb0'.
#' @param ... Extra arguments to pass to [color_facet_backgrounds()]
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom RColorBrewer brewer.pal
#' @export
plot_timeseries <- function(ps = NULL,
                            type = "ssb",
                            time = "mid",
                            ci = c(0.05, 0.95),
                            by_country = FALSE,
                            yr_lim = c(NA_real_, NA_real_),
                            ci_lines = TRUE,
                            show_ssb0 = TRUE,
                            show_40_10 = TRUE,
                            ...){

  verify_argument(ps, "list")
  verify_argument(type, "character", 1, c("ssb", "ssb_ssb0", "catch", "aas", "aac"))
  verify_argument(time, "character", 1, c("beg", "mid"))
  verify_argument(ci, "numeric", 2)
  verify_argument(by_country, "logical", 1)
  verify_argument(yr_lim, "numeric", 2)
  verify_argument(ci_lines, "logical", 1)
  verify_argument(show_ssb0, "logical", 1)
  verify_argument(show_40_10, "logical", 1)

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
    y_factor <- 1e-6 / 2
  }else if(type == "ssb_ssb0"){
    d <- ps$mse_quants$ssb_ssb0_quant
    ssb0 <- 1
    subsc <- 0
    y_label <- bquote("Total SSB / SSB"[.(subsc)])
    y_factor <- 1
  }else if(type == "catch"){
    d <- ps$mse_quants$catch_quant
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
  }else if(type == "aac"){
    if(by_country){
      d <- ps$mse_quants$amc_quant
    }else{
      d <- ps$mse_quants$amc_all_quant
    }
    y_label <- "Average age in catch"
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
    facet_back_cols <- brewer.pal(length(ps$plotnames), "Dark2")
  }else{
    cols <- brewer.pal(length(ps$plotnames), "Dark2")
  }

  # Reorder the legend and colors
  d <- d %>%
    mutate(scenario = fct_relevel(scenario, rev(levels(d$scenario))))

  d <- d %>%
    filter(!is.na(`0.5`))

  if(by_country){
    g <- ggplot(d, aes(x = year, y = `0.5` * y_factor, color = country, fill = country))
  }else{
    g <- ggplot(d, aes(x = year, y = `0.5` * y_factor, color = scenario, fill = scenario))
  }
  g <- g +
    geom_line(size = 1.5) +
    scale_y_continuous(name = y_label) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5),
          legend.position = c(0.1, 0.9)) +
    scale_color_manual(values = cols) +
    coord_cartesian(xlim = yr_lim)

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
  if(show_ssb0 && type %in% c("ssb", "ssb_ssb0")){
    g <- g +
      geom_hline(aes(yintercept = ssb0), color = "black", linetype = 2)
  }
  if(show_40_10 && type %in% c("ssb", "ssb_ssb0")){
    g <- g +
      geom_hline(aes(yintercept = ssb0 * 0.4), color = "black", linetype = 2) +
      geom_hline(aes(yintercept = ssb0 * 0.1), color = "black", linetype = 2)
  }
  if(by_country){
    g <- g + facet_wrap(~scenario)
    return(color_facet_backgrounds(g, facet_back_cols, ...))
  }

  g
}
