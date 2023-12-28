#' Create a [knitr::kable()] table object
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()].
#' @param scen The scenario number to use. This is an integer representing the names given by `names(ps$sim_data)`
#' @param ci A vector of credible interval values. 0.5 must be included if the median is desired.
#' @param decimals The number of decimals to round values to
#' @param inc_mean If TRUE, include the mean in the table
#' @param scen_names Names to show in the columns for scenarios. You can experiment by shortening names
#' so that the table fits on the page. If `NULL`, scenario names from the `ps` object will be used.
#' @param format See [knitr::kable()]
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return a [knitr::kable()] table object
#' @export
table_perf_metrics <- function(ps,
                               ci = c(0.05, 0.5, 0.95),
                               decimals = 0,
                               scen = NULL,
                               inc_mean = TRUE,
                               scen_names = NULL,
                               format = "latex",
                               ...){

  scenario_names <- names(ps$sim_data)

  if(is.null(scen)){
    scen <- seq_along(scenario_names)
  }
  stopifnot(all(scen %in% seq_along(scenario_names)))

  d <- ps$mse_quants$ssb_catch_indicators_quant

  stopifnot("0.5" %in% names(d))
  stopifnot("scenario" %in% names(d))
  stopifnot(all(ci %in% names(d)))

  d <- d %>%
    filter(scenario %in% scenario_names[scen])
  indicator <- unique(d$indicator)

  d <- d %>%
    group_by(scenario) %>%
    dplyr::group_split()

  if(inc_mean){
    d <- map_dfc(d, ~{
      .x <- .x %>%
        select(-indicator) %>%
        select_at(.vars = vars(as.character(ci), "avg"))
    })
  }else{
    d <- map_dfc(d, ~{
      .x <- .x %>%
        select(-indicator) %>%
        select_at(.vars = vars(as.character(ci)))
    })
  }
  d <- d %>%
    mutate(Indicator = as.character(indicator)) %>%
    select(Indicator, everything())

  # TODO: It is really hard to round and format rows differently in the same table
  # Short and long-term catch are rounded to zero decimals
  # d_notcatch <- d %>% filter(!Indicator %in% c("Short term catch", "Long term catch"))
  # d_catch <- d %>% filter(Indicator %in% c("Short term catch", "Long term catch")) %>%
  #   mutate_at(.vars = vars(-Indicator), .funs = function(x){
  #     formatC(as.numeric(x), format = "f", digits = 0)
  #   })
  # d <- rbind(d_notcatch, d_catch)

  if(inc_mean){
    names(d) <- c("Indicator", rep(c(ci, "Mean"), length(scen)))
  }else{
    names(d) <- c("Indicator", rep(ci, length(scen)))
  }
  len_dat <- length(ci) + ifelse(inc_mean, 1, 0)

  k <- kable(d,
             format = format,
             digits = decimals,
             format.args = list(decimal.mark = '.', big.mark = ","),
             ...) %>%
    collapse_rows(columns = 1, latex_hline = "none") %>%
    row_spec(0, bold = TRUE)

  if(length(scen) > 1){
    if(is.null(scen_names)){
      scen_header <- rep(len_dat, length(scen))
      names(scen_header) <- scenario_names[scen]
    }else{
      scen_header <- rep(len_dat, length(scen_names))
      names(scen_header) <- scen_names[scen]
    }

    k <- k %>%
      add_header_above(c(" ", scen_header), bold = TRUE)
  }

  k <- k %>% kable_styling(latex_options = "HOLD_position")

  k
}