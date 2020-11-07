#' Plot Average Age in Survey (AAS) or Catch (AAC) by scenario
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param type Which data type to plot, "survey" or "catch"
#' @param ci A vector of length two of the lower and upper credible interval values.
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_aa <- function(ps = NULL,
                    type = NULL,
                    ci = c(0.05, 0.95),
                    yr_lim = c(NA_real_, NA_real_)){

  verify_argument(ps, "list")
  verify_argument(type, "character", 1, c("survey", "catch"))
  verify_argument(ci, "numeric", 2)
  verify_argument(yr_lim, "numeric", 2)

  if(type == "survey"){
    aa <- ps$mse_quants$ams_quant
    stopifnot("0.5" %in% names(aa))
    ylab <- "Average age in survey"
  }else if(type == "catch"){
    aa <- ps$mse_quants$amc_quant
    stopifnot("0.5" %in% names(aa))
    ylab <- "Average age in catch"
  }else{
    stop("`type` must be one of 'survey' or 'catch'",
         call. = FALSE)
  }
  stopifnot(all(ci %in% names(aa)))

  ci <- as.character(ci) %>% map(~{sym(.x)})
  aa$year <- as.numeric(aa$year)
  #cols <- pnw_palette("Starfish", n = length(ps$plotnames), type = "discrete")
  cols <- brewer.pal(length(ps$plotnames), "Dark2")

  # Reorder the legend and colors
  aa <- aa %>%
    mutate(scenario = fct_relevel(scenario, rev(levels(aa$scenario))))

  g <- ggplot(aa, aes(x = year, y = `0.5`, color = scenario, fill = scenario)) +
    geom_line(size = 2) +
    scale_y_continuous(name = ylab) +
    theme(legend.title = element_blank(),
          legend.position = c(0.1, 0.9)) +
    scale_color_manual(values = cols) +
    geom_ribbon(aes(ymin = !!ci[[1]], ymax = !!ci[[2]]), linetype = 0) +
    scale_fill_manual(values = alpha(cols, alpha = 0.2)) +
    coord_cartesian(xlim = yr_lim)

  g
}

#' Plot Average Age in Survey (AAS) of Catch (AAC) by scenario and country
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param type Which data type to plot, "survey" or "catch"
#' @param ci A vector of length two of the lower and upper credible interval values.
#' @param country_colors A vector of two colors, the first for Canada, the second for
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used
#' the US
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_aa_country <- function(ps = NULL,
                            type = NULL,
                            ci = c(0.05, 0.95),
                            country_colors = c("darkred", "blue4"),
                            yr_lim = c(NA_real_, NA_real_)){

  verify_argument(ps, "list")
  verify_argument(type, "character", 1, c("survey", "catch"))
  verify_argument(ci, "numeric", 2)
  verify_argument(country_colors, "character", 2)
  verify_argument(yr_lim, "numeric", 2)

  if(type == "survey"){
    aa <- ps$mse_quants$ams_quant
    ylab <- "Average age in survey"
  }else if(type == "catch"){
    aa <- ps$mse_quants$amc_quant
    ylab <- "Average age in catch"
  }else{
    stop("`type` must be one of 'survey' or 'catch'",
         call. = FALSE)
  }

  stopifnot(all(ci %in% names(aa)))
  stopifnot("0.5" %in% names(aa))
  aa$year <- as.numeric(aa$year)
  aa_can <- aa %>% filter(country == "Canada")
  aa_us <- aa %>% filter(country == "US")

  stopifnot(all(ci %in% names(aa)))
  ci <- as.character(ci) %>% map(~{sym(.x)})

  g <- ggplot(aa_can, aes(x = year, y = `0.5`)) +
    geom_line(size = 3, color = country_colors[1]) +
    geom_ribbon(aes(ymin = !!ci[[1]], ymax = !!ci[[2]]),
                linetype = 0,
                fill = alpha(country_colors[1], alpha = 0.2),
                color = country_colors[1])+
    geom_line(aes(y = aa_us$`0.5`), size = 2, color = country_colors[2]) +
    geom_ribbon(aes(ymin = aa_us[[ci[[1]]]], ymax = aa_us[[ci[[2]]]]),
                linetype = 0,
                fill = alpha(country_colors[2], alpha = 0.2),
                color = country_colors[2]) +
    scale_y_continuous(name = ylab)+
    facet_wrap(~scenario) +
    theme(legend.position = "n") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    coord_cartesian(xlim = yr_lim)

  g
}