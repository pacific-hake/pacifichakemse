#' Create a [knitr::kable()] table object
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()].
#' @param type One of 'ssb', 'ssb_ssb0', catch', 'aas', 'aac'
#' @param time Either 'beg' for beginning of the year SSB or 'mid' for mid-year SSB.
#' Only used if `type` is 'ssb' or 'ssb_ssb0'.
#' @param ci A vector of credible interval values. 0.5 must be included if the median is desired.
#' @param yr_lim A vector of 2 for minimum and maximum yrs to include in the table. If either are NA,
#' the limits of the data are used.
#' @param decimals The number of decimals to round values to
#' @param by_country If TRUE, a table with country data side-by-side will be produced. If FALSE,
#' a table with aggregated output will be produced
#' @param scen The scenario number to use. This is an integer representing the names given by `names(ps$sim_data)`
#' @param inc_mean If TRUE, include the mean in the table
#' @param scen_names Names to show in the columns for scenarios. You can experiment by shortening names
#' so that the table fits on the page. If `NULL`, scenario names from the `ps` object will be used.
#' @param format See [knitr::kable()]
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return a [knitr::kable()] table object
#' @importFrom knitr kable
#' @importFrom dplyr select_at mutate_at
#' @importFrom kableExtra collapse_rows row_spec add_header_above kable_styling
#' @export
table_timeseries <- function(ps = NULL,
                             type = "ssb",
                             time = "mid",
                             ci = c(0.05, 0.5, 0.95),
                             decimals = 0,
                             by_country = FALSE,
                             scen = NULL,
                             yr_lim = c(NA_real_, NA_real_),
                             inc_mean = TRUE,
                             scen_names = NULL,
                             format = "latex",
                             ...){

  verify_argument(ps, "list")
  verify_argument(type, "character", 1, c("ssb", "ssb_ssb0", "catch", "aas", "aac", "aap", "catch_quota"))
  verify_argument(time, "character", 1, c("beg", "mid"))
  verify_argument(ci, "numeric")
  verify_argument(decimals, "numeric", 1)
  verify_argument(by_country, "logical", 1)
  verify_argument(scen, c("integer", "numeric"))
  verify_argument(yr_lim, "numeric", 2)
  verify_argument(inc_mean, "logical", 1)
  verify_argument(format, "character", 1)

  stopifnot(!(by_country & length(scen) > 1))

  scenario_names <- names(ps$sim_data)
  stopifnot(all(scen %in% seq_along(scenario_names)))

  scale <- 1
  if(type == "ssb"){
    if(by_country){
      if(time == "beg"){
        d <- ps$mse_quants$ssb_quant_country
      }else{
        d <- ps$mse_quants$ssb_mid_quant_country
      }
    }else{
      if(time == "beg"){
        d <- ps$mse_quants$ssb_all_quant
      }else{
        d <- ps$mse_quants$ssb_mid_quant
      }
    }
    # SSB0 - All scenarios and runs are the same so just use the first scenario, first run
    ssb0 <- sum(ps$sim_data[[1]][[1]]$ssb_0)
    scale <- 1e-6 / 2
  }else if(type == "ssb_ssb0"){
    d <- ps$mse_quants$ssb_ssb0_quant
  }else if(type == "catch"){
    d <- ps$mse_quants$catch_quant
  }else if(type == "aas"){
    if(by_country){
      d <- ps$mse_quants$ams_quant
    }else{
      d <- ps$mse_quants$ams_all_quant
    }
  }else if(type == "aac"){
    if(by_country){
      d <- ps$mse_quants$amc_quant
    }else{
      d <- ps$mse_quants$amc_all_quant
    }
  }else if(type == "aap"){
    if(by_country){
      d <- ps$mse_quants$aap_quant
    }else{
      d <- ps$mse_quants$aap_all_quant
    }
  }else if(type == "catch_quota"){
    d <- ps$mse_quants$catch_quota_quant
  }

  stopifnot("0.5" %in% names(d))
  stopifnot("scenario" %in% names(d))
  stopifnot(all(ci %in% names(d)))
  if(by_country){
    stopifnot("country" %in% names(d))
  }

  yr_lim[1] <- ifelse(is.na(yr_lim[1]), d$year[1], yr_lim[1])
  yr_lim[2] <- ifelse(is.na(yr_lim[2]), tail(d$year, 1), yr_lim[2])

  if(!all(yr_lim %in% d$year)){
    stop("yr_lim years (", yr_lim[1], " to ", yr_lim[2], ") are out of range of data (",
         d$year[1], " to ", tail(d$year, 1), ")",
         call. = FALSE)
  }

  d <- d %>%
    filter(scenario %in% scenario_names[scen]) %>%
    filter(year %in% yr_lim[1]:yr_lim[2])
  yrs <- unique(d$year)

  if(by_country){
    d <- d %>%
      group_by(country) %>%
      dplyr::group_split()
    if(inc_mean){
      d <- map_dfc(d, ~{
        .x <- .x %>%
          select(-c(year, country)) %>%
          select_at(.vars = vars(as.character(ci), "avg"))
      })
    }else{
      d <- map_dfc(d, ~{
        .x <- .x %>%
          select(-c(year, country)) %>%
          select_at(.vars = vars(as.character(ci)))
      })
    }
  }else{
    d <- d %>%
      group_by(scenario) %>%
      dplyr::group_split()
    if(inc_mean){
      d <- map_dfc(d, ~{
        .x <- .x %>%
          select(-year) %>%
          select_at(.vars = vars(as.character(ci), "avg"))
      })
    }else{
      d <- map_dfc(d, ~{
        .x <- .x %>%
          select(-year) %>%
          select_at(.vars = vars(as.character(ci)))
      })
    }
  }

  d <- d %>%
    mutate(Year = as.character(yrs)) %>%
    select(Year, everything()) %>%
    mutate_at(.vars = vars(-Year), .funs = function(x){x * scale})

  if(by_country){
    if(inc_mean){
      names(d) <- c("Year", ci, "Mean", ci, "Mean")
    }else{
      names(d) <- c("Year", ci, ci)
    }
  }else{
    if(inc_mean){
      names(d) <- c("Year", rep(c(ci, "Mean"), length(scen)))
    }else{
      names(d) <- c("Year", rep(ci, length(scen)))
    }
  }
  len_dat <- length(ci) + ifelse(inc_mean, 1, 0)

  k <- kable(d,
             format = format,
             digits = decimals,
             format.args = list(decimal.mark = '.', big.mark = ","),
             ...) %>%
    collapse_rows(columns = 1, latex_hline = "none") %>%
    row_spec(0, bold = TRUE)

  if(by_country){
    k <- k %>%
      add_header_above(c(" ", "Canada" = len_dat, "US" = len_dat), bold = TRUE)
  }

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