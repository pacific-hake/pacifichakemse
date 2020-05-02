#' Plot Catch by scenario and country
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch <- function(ps){
  catch <- ps$mse_values_agg$catchplot
  g <- ggplot(catch, aes(x = year, y = med * 1e-6, color = run)) +
    geom_line(size = 1.5) +
    #geom_ribbon(aes(ymin = p5 * 1e-6, ymax = p95 * 1e-6), linetype = 2, fill = alpha(alpha =0.2, colour = ps$cols)) +
    scale_color_manual(values = ps$cols) +
    scale_y_continuous(name = "Catch (million tonnes)") +
    geom_line(aes(y = p5 * 1e-6, color = run), linetype = 2) +
    geom_line(aes(y = p95 * 1e-6, color = run), linetype = 2)+
    coord_cartesian(ylim = c(0, 1.5)) +
    theme(legend.position = c(0.1, 0.8), legend.title = element_blank())
  g
}