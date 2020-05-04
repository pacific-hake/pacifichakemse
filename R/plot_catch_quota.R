#' Plot Catch/Quota by scenario
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch_quota <- function(ps = NULL){

  stopifnot(!is.null(ps))

  cq <- ps$mse_values_agg$catchq
  g <- ggplot(cq, aes(x = year, y = med.can)) +
    geom_line(color = "red") +
    geom_line(aes(y = med.us), color = "blue") +
    theme_classic() +
    scale_y_continuous(name = "Catch/quota") +
    facet_wrap(~run) +
    coord_cartesian(ylim = c(0.6, 1.1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    geom_ribbon(aes(ymin = p5.can, ymax = p95.can),
                fill = alpha("red", alpha = 0.2),
                linetype = 0) +
    geom_ribbon(aes(ymin = p5.us, ymax = p95.us),
                fill = alpha("blue", alpha = 0.2),
                linetype = 0) +
    geom_hline(aes(yintercept = 1),
               color = "black",
               linetype = 2)
  g
}