#' Plot Average Age in Survey (AAS) or Catch (AAC) by scenario
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param type Which data type to plot, "survey" or "catch"
#' @param ci A vector of length two of the lower and upper credible interval values.
#' These values must have been calculated in [df_lists()] and exist in the data in
#' `ps`

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
    aa <- ps$mse_values_agg$ams_quant
    stopifnot(all(ci %in% names(aa)))
    are_na <- aa[-which(is.na(aa$`0.5`)), ]
    if(length(are_na)){
      aa <- aa[-which(is.na(aa$`0.5`)), ]
    }
    ylab <- "Average age in survey"
  }else if(type == "catch"){
    aa <- ps$mse_values_agg$amc_quant
    stopifnot(all(ci %in% names(aa)))
    ylab <- "Average age in catch"
  }else{
    stop("`type` must be one of 'survey' or 'catch'",
         call. = FALSE)
  }

  ci <- as.character(ci) %>% map(~{sym(.x)})
  g <- ggplot(aa, aes(x = year, y = `0.5`, color = run)) +
    geom_line(size = 2) +
    #geom_ribbon(aes(ymin = p5, ymax = p95, color = run), linetype = 2, fill = NA) +
    scale_color_manual(values = ps$cols) +
    scale_y_continuous(name = ylab) +
    geom_line(aes(y = !!ci[[1]], color = run), linetype = 2) +
    geom_line(aes(y = !!ci[[2]], color = run), linetype = 2) +
    theme(legend.title = element_blank(),
          legend.position = c(0.1, 0.9))
  g
}

#' Plot Average Age in Survey (AAS) of Catch (AAC) by scenario and country
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param type Which data type to plot, "survey" or "catch"
#' @param country_colors A vector of two colors, the first for Canada, the second for
#' the US
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_aa_country <- function(ps = NULL,
                            type = NULL,
                            country_colors = c("darkred", "blue4")){
  stopifnot(!is.null(ps))
  stopifnot(!is.null(type))

  if(type == "survey"){
    aa <- ps$mse_values_agg$amsspace
    ylab <- "Average age in survey"
  }else if(type == "catch"){
    aa <- ps$mse_values_agg$amcspace
    ylab <- "Average age in catch"
  }else{
    stop("`type` must be one of 'survey' or 'catch'",
         call. = FALSE)
  }
  g <- ggplot(aa, aes(x = year, y = med.can)) +
    geom_line(size = 2, color = country_colors[1]) +
    geom_ribbon(aes(ymin = p5.can, ymax = p95.can),
                linetype = 0,
                fill = alpha(country_colors[1], alpha = 0.2),
                color = country_colors[1])+
    geom_line(aes(y = med.us), size = 2, color = country_colors[2]) +
    geom_ribbon(aes(ymin = p5.us, ymax = p95.us),
                linetype = 0,
                fill = alpha(country_colors[2], alpha = 0.2),
                color = country_colors[2]) +
    scale_y_continuous(name = ylab)+
    facet_wrap(~run) +
    theme(legend.position = "n") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  g
}