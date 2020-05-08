#' Plot Spawning Stock Biomass (SSB) by scenario and country
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()].
#' The variable `ssb_quant` in `ps$mse_values_agg$ssb_quant` is constructed
#' in [df_lists()]
#' @param ci A vector of length two of the lower and upper credible interval values.
#' These values must have been calculated in [df_lists()] and exist in the data in
#' `ps`
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_ssb <- function(ps = NULL,
                     ci = c(0.05, 0.95)){

  stopifnot(!is.null(ps))
  stopifnot(!is.null(ci))

  ssb <- ps$mse_values_agg$ssb_quant
  stopifnot("country" %in% names(ssb))
  stopifnot(is.numeric(ci))
  stopifnot(all(ci %in% names(ssb)))
  stopifnot(length(ci) == 2)
  ssb_can <- ssb %>% filter(country == "Canada")
  ssb_us <- ssb %>% filter(country == "US")
  ci <- as.character(ci) %>% map(~{sym(.x)})

  g <- ggplot(ssb_can, aes(x = year, y = `0.5` * 1e-6)) +
    geom_line(color = "darkred", size = 1.5) +
    geom_ribbon(aes(ymin = !!ci[[1]] * 1e-6, ymax = !!ci[[2]] * 1e-6),
                fill = alpha("red", alpha = 0.2), linetype = 0) +
    geom_line(aes(y = ssb_us$`0.5` * 1e-6), color = "darkblue", size = 1.5) +
    theme_classic() +
    scale_y_continuous(name ="SSB (million tonnes)") +
    facet_wrap(~run) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
    geom_ribbon(aes(ymin = ssb_us[[ci[[1]]]] * 1e-6, ymax = ssb_us[[ci[[2]]]] * 1e-6),
                fill = alpha("blue", alpha = 0.2), linetype = 0)
  g
}