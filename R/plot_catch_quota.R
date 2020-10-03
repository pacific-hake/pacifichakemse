#' Plot Catch/Quota by scenario
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param ci A vector of length two of the lower and upper credible interval values.
#' These values must have been calculated in [df_lists()] and exist in the data in `ps`
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch_quota <- function(ps = NULL,
                             ci = c(0.05, 0.95)){

  stopifnot(!is.null(ps))
  stopifnot(!is.null(ci))

  cq <- ps$mse_quants$quota_quant
  stopifnot("0.5" %in% names(cq))
  stopifnot(is.numeric(ci))
  stopifnot(all(ci %in% names(cq)))
  stopifnot(length(ci) == 2)

  cq_can <- cq %>%
    filter(country == "Canada")
  cq_us <- cq %>%
    filter(country == "US")

  ci <- as.character(ci) %>% map(~{sym(.x)})
  browser()
  g <- ggplot(cq_can, aes(x = year, y = `0.5`)) +
    geom_line(color = "red") +
    geom_line(aes(y = cq_us$`0.5`), color = "blue") +
    theme_classic() +
    scale_y_continuous(name = "Catch/quota") +
    facet_wrap(~scenario) +
    coord_cartesian(ylim = c(0.6, 1.1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    geom_ribbon(aes(ymin = !!ci[[1]], ymax = !!ci[[2]]),
                fill = alpha("red", alpha = 0.2),
                linetype = 0) +
    geom_ribbon(aes(ymin = cq_us[[ci[[1]]]], ymax = cq_us[[ci[[2]]]]),
                fill = alpha("blue", alpha = 0.2),
                linetype = 0) +
    geom_hline(aes(yintercept = 1),
               color = "black",
               linetype = 2)
  g
}

#' Plot Catch/Quota by scenario all in one panel
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch_quota_1panel <- function(ps = NULL){

  stopifnot(!is.null(ps))

  cq <- ps$mse_values_agg$catchq
  g <- ggplot(cq, aes(x = year, y = med.tot, color = run)) +
    geom_line(size = 1.4) +
    scale_color_manual(values = ps$cols) +
    geom_line(aes(y = p5.tot), linetype = 2, size = 0.9) +
    geom_line(aes(y = p95.tot), linetype = 2, size = 0.9) +
    theme_classic() +
    scale_y_continuous(name ="Catch/quota") +
    geom_hline(aes(yintercept = 1),
               color = "black",
               linetype = 2) +
    theme(legend.position = c(0.2, 0.4),
          legend.title = element_blank())
  g
}