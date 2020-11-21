#' Plot faceted barplots by country, season, and scenarios
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_bars_country_season <- function(ps = NULL){

  stopifnot(!is.null(ps))

  g <- ggplot(ps$df_country_season_indicators,
              aes(x = scenario, y = value)) +
    geom_bar(stat = "identity", aes(fill = scenario)) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "") +
    scale_fill_manual(values = ps$cols) +
    facet_wrap(~indicator, scales = "free_y", ncol = 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none")
  g
}
