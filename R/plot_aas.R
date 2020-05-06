#' Plot Average Age in Survey (AAS) or Catch (AAC) by scenario
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param type Which data type to plot, "survey" or "catch"
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_aa <- function(ps = NULL,
                    type = NULL){
  stopifnot(!is.null(ps))
  stopifnot(!is.null(type))

  if(type == "survey"){
    aa <- ps$mse_values_agg$amsplot
    aa <- aa[-which(is.na(aa$med)), ]
    ylab <- "Average age in survey"
  }else if(type == "catch"){
    aa <- ps$mse_values_agg$amcplot
    ylab <- "Average age in catch"
  }else{
    stop("`type` must be one of 'survey' or 'catch'",
         call. = FALSE)
  }
  g <- ggplot(aa, aes(x = year, y = med, color = run)) +
    geom_line(size = 2) +
    #geom_ribbon(aes(ymin = p5, ymax = p95, color = run), linetype = 2, fill = NA) +
    scale_color_manual(values = ps$cols) +
    scale_y_continuous(name = ylab) +
    geom_line(aes(y = p5, color = run), linetype = 2) +
    geom_line(aes(y = p95, color = run), linetype = 2) +
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