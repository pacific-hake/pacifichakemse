#' Plot faceted violin plots for scenarios
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_violins <- function(ps){
  df <- ps$violin_data
  # Remove the 5th and 5th percentiles from df
  vars <- unique(df$variable)
  for(i in 1:length(vars)){
    idxtmp <- which(df$variable == vars[i])
    valstmp <- quantile(df$value[idxtmp], probs = c(0.05, 0.95))
    rmtmp <- which(df$value[idxtmp] < valstmp[1] | df$value[idxtmp] > valstmp[2])
    df$value[idxtmp][rmtmp] <- NA
  }
  g <- ggplot(df, aes(x = HCR, y = value, fill = HCR)) +
    geom_violin() +
    geom_boxplot(width = 0.15, col = "black", outlier.shape = NA) +
    scale_fill_manual(values = ps$cols) +
    facet_wrap(~variable, scales = "free_y", ncol = 3, dir = "v") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "")
  g
}