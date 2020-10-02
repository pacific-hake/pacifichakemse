#' Plot faceted violin plots for scenarios
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_violins <- function(ps, quants = c(0.05, 0.95), min_yr = 2020){

  # Extract Catch, SSB, and AAV and quantiles for them
  # catch <- map2(ps$violin_data, seq_along(ps$violin_data), ~{
  #   .x$catch_plot %>%
  #     mutate(hcr = ps$plotnames[.y]) %>%
  #     rename(value = catch)
  # }) %>%
  #   map_df(~{.x}) %>%
  #   filter(year >= min_yr)
  #
  # catch_quants <- map2(ps$violin_data, seq_along(ps$violin_data), ~{
  #   .x$catch_plot %>%
  #     calc_quantiles_by_group(grp_col = "year",
  #                             col = "catch",
  #                             probs = quants,
  #                             include_mean = FALSE) %>%
  #     mutate(hcr = ps$plotnames[.y])
  #
  # }) %>%
  #   map_df(~{.x}) %>%
  #   filter(year >= min_yr) %>%
  #   mutate(col = "catch")
  #
  # ssb <- map2(ps$violin_data, seq_along(ps$violin_data), ~{
  #   .x$ssb_plot  %>%mutate(hcr = ps$plotnames[.y]) %>%
  #     rename(value = ssb)
  # }) %>%
  #   map_df(~{.x}) %>%
  #   filter(year >= min_yr)
  #
  # ssb_quants <- map2(ps$violin_data, seq_along(ps$violin_data), ~{
  #   .x$ssb_plot  %>%
  #     calc_quantiles_by_group(grp_col = "year",
  #                             col = "ssb",
  #                             probs = quants,
  #                             include_mean = FALSE) %>%
  #     mutate(hcr = ps$plotnames[.y])
  # }) %>%
  #   map_df(~{.x}) %>%
  #   filter(year >= min_yr) %>%
  #   mutate(col = "ssb")
  #
  # aav <- map2(ps$violin_data, seq_along(ps$violin_data), ~{
  #   .x$aav_plot  %>%
  #     mutate(hcr = ps$plotnames[.y]) %>%
  #     rename(value = aav)
  # }) %>%
  #   map_df(~{.x}) %>%
  #   filter(year >= min_yr)
  #
  # aav_quants <- map2(ps$violin_data, seq_along(ps$violin_data), ~{
  #   .x$aav_plot  %>%
  #     calc_quantiles_by_group(grp_col = "year",
  #                             col = "aav",
  #                             probs = quants,
  #                             include_mean = FALSE) %>%
  #     mutate(hcr = ps$plotnames[.y])
  # }) %>%
  #   map_df(~{.x}) %>%
  #   filter(year >= min_yr) %>%
  #   mutate(col = "aav")
  #
  # df <- catch %>%
  #   bind_rows(ssb) %>%
  #   bind_rows(aav)
  # TODO: Need to remove all values which lie outside the quantile range. Set them to NA

  inds <- c("SSB < 0.10 SSB0",
            "0.10 < SSB < 0.4 SSB0",
            "SSB > 0.4 SSB0",
            "AAV",
            "Short term catch",
            "Long term catch")
  df <- ps$ssb_catch_indicators %>%
    filter(indicator %in% inds) %>%
    mutate(indicator = forcats::fct_relevel(indicator, inds))

  cols <- PNWColors::pnw_palette("Starfish",n = length(unique(df$hcr)), type = "discrete")
  g <- ggplot(df, aes(x = hcr, y = value, fill = hcr)) +
    geom_violin() +
    geom_boxplot(width = 0.15, col = "black", outlier.shape = NA) +
    scale_fill_manual(values = cols) +
    facet_wrap(~indicator, scales = "free_y", ncol = 3, dir = "h") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "")
  g
}