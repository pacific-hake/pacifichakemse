#' Calculate the catch proportions by country, year, and season
#'
#' @param ss_model_data_csv_dir The directory in which the assessment csv
#' files containing catch by month are located. Can be a URL
#' @param n_yrs If `NULL`, a data frame containing all years of data with
#' proportions of catch by season will be returned. If this is a number, it is
#' the number of years at the end of the data to average the proportions by
#' season for. In this case, the data frame will be only 8 rows long, without
#' any year column
#' @param weight A vector of 4 for the weights given to each season
#' @param ... Absorbs arguments meant for other functions
#'
#' @return If `n_yrs` is `NULL`, a data frame containing all years of data with
#' proportions of catch by country and season. If `n_yrs` is a number, the
#' data frame returned will be only 8 rows long, 4 rows per country, without
#' any year column
#'
#' @export
calc_catch_seas_country <- function(ss_model_data_csv_dir = NULL,
                                    n_yrs = 10,
                                    weight = c(1, 1, 1, 1),
                                    ...){

  make_catch_prop_by_season <- \(fn_lst){

    df <- map_dfr(fn_lst, \(fn){
      fn <- file.path(ss_model_data_csv_dir, fn)
      if(!files_exist(fn)){
        cat(red(symbol$cross),
            red(paste0("The data file ", fn, " does not exist.\n")))
        return(NULL)
      }

      read_csv(fn, col_types = cols())
    }) |>
      group_by(year, month) |>
      summarize(across(catch, ~sum(.x))) |>
      ungroup() |>
      # Add season
      mutate(season = ifelse(month %in% 1:3,
                             1,
                             ifelse(month %in% 4:6,
                                    2,
                                    ifelse(month %in% 7:9,
                                           3,
                                           4)))) |>
      select(-month) |>
      select(year, season, catch) |>
      # Get totals by year and season
      group_by(year, season) |>
      summarize(across(catch, ~sum(.x))) |>
      # Multiply the weight factor by season
      mutate(catch = catch * weight[season]) |>
      ungroup()

    df_tot_ct_by_yr <- df |>
      group_by(year) |>
      summarize(catch = sum(catch))

    df |>
      left_join(df_tot_ct_by_yr, by = "year") |>
      mutate(catch_prop = catch.x / catch.y) |>
      select(-catch.x, -catch.y)
  }

  fns_can <- list(hake::can_ft_catch_by_month_fn,
                  hake::can_ss_catch_by_month_fn)

  fns_us <- list(hake::us_cp_catch_by_month_fn,
                 hake::us_ms_catch_by_month_fn,
                 hake::us_ss_catch_by_month_fn,
                 hake::us_research_catch_by_month_fn)

  df_can <- make_catch_prop_by_season(fns_can) |>
    mutate(country = 1)
  df_us <- make_catch_prop_by_season(fns_us) |>
    mutate(country = 2)

  if(!is.null(n_yrs)){
    # Do average calculation for each country
    last_n_yrs <- df_can$year |>
      unique() |>
      sort() |>
      tail(n_yrs)
    df_can <- df_can |>
      filter(year %in% last_n_yrs) |>
      select(-year) |>
      group_by(country, season) |>
      summarize(catch_prop = mean(catch_prop))
    last_n_yrs <- df_us$year |>
      unique() |>
      sort() |>
      tail(n_yrs)
    df_us <- df_us |>
      filter(year %in% last_n_yrs) |>
      select(-year) |>
      group_by(country, season) |>
      summarize(catch_prop = mean(catch_prop))
  }

  df_can |>
    bind_rows(df_us) |>
    select(country, everything()) |>
    pivot_wider(names_from = "season", values_from = "catch_prop") |>
    ungroup() |>
    select(-country) |>
    as.matrix()
}
