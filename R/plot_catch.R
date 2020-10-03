#' Plot Catch by scenario and country
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param ci A vector of length two of the lower and upper credible interval values.
#' These values must have been calculated in [df_lists()] and exist in the data in `ps`
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch <- function(ps = NULL,
                       ci = c(0.05, 0.95)){

  stopifnot(!is.null(ps))
  stopifnot(!is.null(ci))

  catch <- ps$mse_quants$catch_quant
  stopifnot("0.5" %in% names(catch))
  stopifnot(is.numeric(ci))
  stopifnot(all(ci %in% names(catch)))
  stopifnot(length(ci) == 2)

  ci <- as.character(ci) %>% map(~{sym(.x)})

  g <- ggplot(catch,
              aes(x = year, y = `0.5` * 1e-6, color = scenario)) +
    geom_line(size = 1.5) +
    #geom_ribbon(aes(ymin = p5 * 1e-6, ymax = p95 * 1e-6), linetype = 2, fill = alpha(alpha =0.2, colour = ps$cols)) +
    scale_color_manual(values = ps$cols) +
    scale_y_continuous(name = "Catch (million tonnes)") +
    geom_line(aes(y = !!ci[[1]] * 1e-6, color = scenario), linetype = 2) +
    geom_line(aes(y = !!ci[[2]] * 1e-6, color = scenario), linetype = 2) +
    #coord_cartesian(ylim = c(0, 1.5)) +
    theme(legend.position = c(0.1, 0.8), legend.title = element_blank())
  g
}