#' Plot TAC vs SSB as a simple line
#'
#' @param df A [data.frame] as output by [calc_tac_est_vs_real()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_tac_est_vs_real <- function(df){
  ggplot(data = df$tac_ssb, aes(x = ssb * 1e-6, y = tac * 1e-6)) +
    geom_line() +
    scale_y_continuous("TAC (million tonnes)") +
    scale_x_continuous("SSB (million tonnes)") +
    theme_classic()
}

#' Plot estimated vs realized TAC (called Catch in plot) by Harvest Control Rule
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom dplyr rename
#' @importFrom ggplot2 margin
#' @importFrom PNWColors pnw_palette
#' @export
plot_tac_vs_hcr <- function(ps = NULL){

  stopifnot(!is.null(ps))

  labels <- ps$plotnames

  df <- calc_tac_est_vs_real()
  d <- df$plot
  cols <- attributes(d)$cols
  g <- ggplot(d, aes(x = tac * 1e-3, y = Quota * 1e-3, color = HCR)) +
    geom_line(linetype = 2, size = 0.8) +
    scale_y_continuous("Catch \n(thousand tonnes)") +
    scale_color_manual(values = cols,
                       labels = labels) +
    scale_x_continuous("Harvest control rule") +
    geom_point(data = df$tac, aes(x = AssessTac * 1e-3, y = Realized * 1e-3), color = cols[3]) +
    geom_point(data = df$tac, aes(x = AssessTac * 1e-3, y = TAC * 1e-3), color = cols[2]) +
    theme_classic() +
    #geom_point(data = df.tac[df.tac$Year >= 2012,],
    # aes(x = AssessTac * 1e-3, y = TAC * 1e-3), color = alpha(cols[4], 0.5)) +
    theme(legend.title = element_blank(),
          legend.text = element_text(),
          legend.justification = "left",
          legend.margin = margin(t = 20, r = 0, b = 0, l = 0, unit = "mm"),
          legend.position = c(0.05, 1))
  g
}
