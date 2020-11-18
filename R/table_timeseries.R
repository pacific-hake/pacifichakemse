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
#' @param format See [knitr::kable()]
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return a [knitr::kable()] table object
#' @importFrom knitr kable
#' @importFrom dplyr select_at mutate_at
#' @importFrom kableExtra collapse_rows row_spec add_header_above
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
                             format = "latex",
                             ...){

  verify_argument(ps, "list")
  verify_argument(type, "character", 1, c("ssb", "ssb_ssb0", "catch", "aas", "aac", "catch_quota"))
  verify_argument(time, "character", 1, c("beg", "mid"))
  verify_argument(ci, "numeric")
  verify_argument(by_country, "logical", 1)
  verify_argument(scen, "numeric", 1)
  verify_argument(yr_lim, "numeric", 2)
  verify_argument(inc_mean, "logical", 1)
  verify_argument(format, "character", 1)

  scenario_names <- names(ps$sim_data)
  stopifnot(scen %in% seq_along(scenario_names))

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
          select_at(.vars = vars(as.character(ci), "avg")) %>%
          mutate_at(.vars = vars(as.character(ci), "avg"), .funs = function(x){
            round(x, decimals)
          })
      })
    }else{
      d <- map_dfc(d, ~{
        .x <- .x %>%
          select(-c(year, country)) %>%
          select_at(.vars = vars(as.character(ci))) %>%
          mutate_at(.vars = vars(as.character(ci)), .funs = function(x){
            round(x, decimals)
          })
      })
    }
  }else{
    if(inc_mean){
      d <- d %>%
        select(-year) %>%
        select_at(.vars = vars(as.character(ci), "avg")) %>%
        mutate_at(.vars = vars(as.character(ci), "avg"), .funs = function(x){
          round(x, decimals)
        })
    }else{
      d <- d %>%
        select(-year) %>%
        select_at(.vars = vars(as.character(ci))) %>%
        mutate_at(.vars = vars(as.character(ci)), .funs = function(x){
          round(x, decimals)
        })
    }
  }

  d <- d %>%
    mutate(Year = as.character(yrs)) %>%
    select(Year, everything())

  if(by_country){
    if(inc_mean){
      names(d) <- c("Year", ci, "Mean", ci, "Mean")
    }else{
      names(d) <- c("Year", ci, ci)
    }
  }else{
    if(inc_mean){
      names(d) <- c("Year", ci, "Mean")
    }else{
      names(d) <- c("Year", ci)
    }
  }
  len_dat <- length(ci) + ifelse(inc_mean, 1, 0)

  k <- kable(d,
             format = format,
             format.args = list(decimal.mark = '.', big.mark = ","),
             ...) %>%
    collapse_rows(columns = 1, latex_hline = "none") %>%
    row_spec(0, bold = TRUE)

  if(by_country){
    k <- k %>%
      add_header_above(c(" ", "Canada" = len_dat, "US" = len_dat), bold = TRUE)
  }

  k
}