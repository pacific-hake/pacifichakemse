#' Plot total SSB/SSB0 by scenario where SSB is the Spawning Stock Biomass from the
#' Estimation model and SSB0 is the initial SSB from teh Operating model
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param ci A vector of length two of the lower and upper credible interval values.
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom dplyr ungroup
#' @export
plot_ssb_ssb0 <- function(ps = NULL,
                          ci = c(0.05, 0.95)){

  stopifnot(!is.null(ps))
  stopifnot(!is.null(ci))

  sim_data <- ps$sim_data
  ssb0 <- sum(sim_data$SSB0)
  ssb <- ps$mse_values_agg$ssb_tot_quant
  stopifnot("0.5" %in% names(ssb))
  stopifnot(is.numeric(ci))
  stopifnot(all(ci %in% names(ssb)))
  stopifnot(length(ci) == 2)

  ssb_names <- names(ssb)
  suppressWarnings(quant_names <- ssb_names[!is.na(as.numeric(ssb_names))])
  if("avg" %in% names(ssb)){
    quant_names <- c(quant_names, "avg")
  }
  ssb_ssb0 <- ssb %>%
    mutate_at(vars(quant_names), function(x){x / ssb0})

  ci <- as.character(ci) %>% map(~{sym(.x)})

  g <- ggplot(ssb_ssb0, aes(x = year, y = `0.5`, color = run, fill = run)) +
    geom_line(size = 1.5) +
    geom_line(aes(y = !!ci[[1]]), linetype = 2, size = 1.2) +
    geom_line(aes(y = !!ci[[2]]), linetype = 2, size = 1.2) +
    scale_color_manual(values = ps$cols) +
    theme_classic() +
    scale_y_continuous(name ="Total SSB/SSB0") +
    coord_cartesian(ylim  = c(0, 4)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = c(0.2,.8),
          legend.title = element_blank()) +
    scale_fill_manual(values = alpha(ps$cols, alpha = 0.2)) +
    geom_hline(aes(yintercept = 1), color = "black", linetype = 2)
  g
}
