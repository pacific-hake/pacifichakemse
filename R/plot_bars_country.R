#' Plot season indicators barplots by country and scenario
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param run Which run number to use for the plot
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_bars_country <- function(ps = NULL, run = 1){

  verify_argument(ps, "list")
  verify_argument(run, c("integer", "numeric"), 1)

  runs <- unique(ps$country_season_indicators$run)
  stopifnot(run %in% runs)

  ps$country_season_indicators <- ps$country_season_indicators %>%
    filter(run == !!run)

  g <- ggplot(ps$country_season_indicators,
              aes(x = hcr, y = value, factor = season)) +
    geom_bar(stat = "identity", aes(fill = season), position = "dodge2") +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "") +
    scale_fill_manual(values = ps$cols) +
    facet_wrap(~country, scales = "fixed", ncol = 2, dir="v") +
    #geom_hline(yintercept=c(.045, .07, 0.12))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  g
}