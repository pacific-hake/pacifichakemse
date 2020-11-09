#' Plot faceted violin plots for scenarios, showing performance metrics
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param quants Quantile values as limits to remove tail data from plot
#'
#' @return A [ggplot2::ggplot()] object
#' @export
#' @importFrom forcats fct_relevel
#' @importFrom PNWColors pnw_palette
plot_violins <- function(ps = NULL,
                         quants = c(0.05, 0.95)){

  verify_argument(ps, "list")
  verify_argument(quants, "numeric")

  inds <- c("SSB < 0.10 SSB0",
            "0.10 < SSB < 0.4 SSB0",
            "SSB > 0.4 SSB0",
            "AAV",
            "Short term catch",
            "Long term catch")

  d <- ps$df_ssb_catch_indicators
  stopifnot("value" %in% names(d))
  stopifnot("scenario" %in% names(d))

  df <-  d %>%
    filter(indicator %in% inds) %>%
    mutate(indicator = fct_relevel(indicator, inds))

  # Remove tails of data
  qs <- df %>%
    group_by(scenario, indicator) %>%
    summarize(qlow = quantile(value, quants[1]), qhigh = quantile(value, quants[2])) %>%
    ungroup()

  df <- df %>% left_join(qs, by = c("scenario", "indicator")) %>%
    group_by(scenario, indicator) %>%
    filter(value >= qlow & value <= qhigh) %>%
    ungroup() %>%
    select(-c(qlow, qhigh))

  # Standardize short and long-term catch values
  df_st <- df %>%
    filter(indicator ==  "Short term catch") %>%
    mutate(value = value / max(value))
  df_lt <- df %>%
    filter(indicator ==  "Long term catch") %>%
    mutate(value = value / max(value))
  df <-  df %>%
    filter(!indicator %in%  c("Short term catch", "Long term catch")) %>%
    bind_rows(df_st, df_lt)

  cols <- brewer.pal(length(ps$plotnames), "Dark2")
  g <- ggplot(df, aes(x = scenario, y = value, fill = scenario)) +
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