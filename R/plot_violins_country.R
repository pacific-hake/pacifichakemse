#' Plot faceted violin plots by Country for scenarios
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param pidx The order in which to plot the scenarios. If `NULL`, they will be in the
#' order of the [data.frame]
#' @param yrs The years to include in the plot. If `NULL`, all available data will be
#' used
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_violins_country <- function(ps = NULL,
                                 pidx = NULL,
                                 yrs = NULL){
  stopifnot(!is.null(ps))

  d <- ps$violin_indicators
  if(!is.null(yrs)){
    d <- d %>%
      filter(year %in% yrs)
  }
  if(is.null(pidx)){
    pidx <- seq_along(unique(d$hcr))
  }
  stopifnot(length(pidx) == length(unique(d$hcr)))

  melted_d <- melt(d,
                   id.vars = c("run","hcr","country","year"),
                   measure.vars = 1:3,
                   variable.name = "season",
                   value.name = "exploitation") %>%
    mutate(exploitation = ifelse(exploitation > 1.0, NA, exploitation)) %>%
    mutate(hcr = factor(hcr, levels = unique(hcr)[pidx]))

  dodge <- position_dodge(width = 0.5)
  g <- ggplot(melted_d, aes(x = hcr, y = exploitation, factor = season, fill = hcr))+
    geom_violin(position = dodge) +
    geom_boxplot(width = 0.15,
                 col = "black",
                 outlier.shape = NA,
                 position = dodge) +
    scale_fill_manual(values = ps$cols) +
    facet_wrap(~country, dir = "v", ncol = 2) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "", limits = c(0, 0.5))
  g
}