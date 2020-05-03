#' Plot Spawning Stock Biomass (SSB) by scenario and country
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_ssb <- function(ps){
  ssb <- ps$mse_values_agg$ssbplot
  g <- ggplot(ssb, aes(x = year, y = med.can * 1e-6)) +
    geom_line(color = "darkred", size = 1.5) +
    geom_line(aes(y = med.US * 1e-6), color = "darkblue", size = 1.5) +
    theme_classic() +
    scale_y_continuous(name ="SSB (million tonnes)") +
    facet_wrap(~run) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
    geom_ribbon(aes(ymin = p5.can * 1e-6, ymax = p95.can * 1e-6),
                fill = alpha("red", alpha = 0.2), linetype = 0)+
    geom_ribbon(aes(ymin = p5.US * 1e-6, ymax = p95.US * 1e-6),
                fill = alpha("blue", alpha = 0.2), linetype = 0)
  g
}