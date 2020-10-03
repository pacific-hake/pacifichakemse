#' Plot Average Age in Survey (AAS) or Catch (AAC) by scenario
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param type Which data type to plot, "survey" or "catch"
#' @param ci A vector of length two of the lower and upper credible interval values.
#' These values must have been calculated in [df_lists()] and exist in the data in `ps`
#' @return A [ggplot2::ggplot()] object
#' @export
plot_aa <- function(ps = NULL,
                    type = NULL,
                    ci = c(0.05, 0.95)){
  stopifnot(!is.null(ps))
  stopifnot(!is.null(type))
  stopifnot(is.numeric(ci))
  stopifnot(length(ci) == 2)

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

  g <- ggplot(aa, aes(x = year, y = `0.5`, color = scenario)) +
    geom_line(size = 2) +
    #geom_ribbon(aes(ymin = p5, ymax = p95, color = scenario), linetype = 2, fill = NA) +
    scale_color_manual(values = ps$cols) +
    scale_y_continuous(name = ylab) +
    geom_line(aes(y = !!ci[[1]], color = scenario), linetype = 2) +
    geom_line(aes(y = !!ci[[2]], color = scenario), linetype = 2) +
    theme(legend.title = element_blank(),
          legend.position = c(0.1, 0.9))
  g
}

#' Plot Average Age in Survey (AAS) of Catch (AAC) by scenario and country
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param type Which data type to plot, "survey" or "catch"
#' @param ci A vector of length two of the lower and upper credible interval values.
#' These values must have been calculated in [df_lists()] and exist in the data in `ps`
#' @param country_colors A vector of two colors, the first for Canada, the second for
#' the US
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_aa_country <- function(ps = NULL,
                            type = NULL,
                            ci = c(0.05, 0.95),
                            country_colors = c("darkred", "blue4")){
  stopifnot(!is.null(ps))
  stopifnot(!is.null(type))
  stopifnot(!is.null(ci))
  stopifnot(!is.null(country_colors))
  stopifnot(is.numeric(ci))
  stopifnot(length(ci) == 2)

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
    geom_line(size = 2, color = country_colors[1]) +
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
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  g
}