#' Plot faceted barplots by Country for scenarios
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_bars_country <- function(ps){
  g <- ggplot(ps$country_season_indicators, aes(x = HCR,y = value, factor=season)) +
    geom_bar(stat = "identity", aes(fill = season), position="dodge2") +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "") +
    scale_fill_manual(values = ps$cols) +
    facet_wrap(~country, scales = "fixed", ncol = 2, dir="v") +
    #geom_hline(yintercept=c(.045, .07, 0.12))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  g
}