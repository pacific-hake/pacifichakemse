#' Plot total SSB/SSB0 by scenario where SSB is the Spawning Stock Biomass from the
#' Estimation model and SSB0 is the initial SSB from teh Operating model
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom dplyr ungroup
#' @export
plot_ssb_ssb0 <- function(ps = NULL){

  stopifnot(!is.null(ps))

  sim_data <- ps$sim_data

  ssbtot <- ps$mse_values_agg$ssbtot %>%
    mutate(med = med / (sum(sim_data$SSB0))) %>%
    mutate(p5 = p5 / (sum(sim_data$SSB0))) %>%
    mutate(p95 = p95 / (sum(sim_data$SSB0)))

  g <- ggplot(ssbtot, aes(x = year, y = med, color = run, fill = run))+
    geom_line(size = 1.5)+
    geom_line(aes(y = p95), linetype =2, size = 1.2)+
    geom_line(aes(y = p5), linetype =2, size = 1.2)+
    scale_color_manual(values = ps$cols)+
    theme_classic() +
    scale_y_continuous(name ="Total SSB/SSB0")+
    coord_cartesian(ylim  = c(0,4))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = c(0.2,.8),
          legend.title = element_blank())+
    scale_fill_manual(values = alpha(ps$cols, alpha = 0.2))+
    geom_hline(aes(yintercept = 1), color = "black", linetype = 2)
  g
}
