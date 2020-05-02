#' Plot Average Age in Survey (AAS) by scenario and country
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_aas <- function(ps){
  ams <- ps$mse_values_agg$amsplot
  ams <- ams[-which(is.na(ams$med)), ]
  g <- ggplot(ams, aes(x = year, y = med, color = run)) +
    geom_line(size = 2) +
    #geom_ribbon(aes(ymin = p5, ymax = p95, color = run), linetype = 2, fill = NA) +
    scale_color_manual(values = ps$cols) +
    scale_y_continuous(name = "Average age in survey") +
    geom_line(aes(y = p5, color = run), linetype = 2) +
    geom_line(aes(y = p95, color = run), linetype = 2) +
    theme(legend.title = element_blank(),
          legend.position = c(0.1, 0.9))
  g
}