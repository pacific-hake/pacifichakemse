#' Plot season indicators barplots by country and scenario
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param facet_back_cols A vector of the facet text background colors
#' @param ... Extra arguments to be passed to [color_facet_backgrounds()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_bars_country <- function(ps = NULL,
                              facet_back_cols = c("red", "blue"),
                              ...){

  cols <- brewer.pal(3, "Dark2")

  g <- ggplot(ps$mse_quants$country_season_indicators_quant,
              aes(x = scenario, y = `0.5`, factor = season)) +
    geom_bar(stat = "identity", aes(fill = season), position = "dodge2") +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "TAC / Vulnerable biomass", expand = c(0, 0)) +
    scale_fill_manual(values = cols) +
    facet_wrap(~country, scales = "fixed", ncol = 2, dir="v") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

    color_facet_backgrounds(g, facet_back_cols, ...)
}