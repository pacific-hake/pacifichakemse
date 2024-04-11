#' Load the SS model input and output data needed by this package in correct format
#'
#' @param s_min Minimum age in fishery selectivity
#' @param s_max Maximum age in fishery selectivity
#' @param s_min_survey Minimum age in survey selectivity
#' @param s_max_survey Maximum age in survey selectivity
#' @param weight_factor A factor to multiply and divide SSB values by
#' @param n_space The number of spaces (areas) in which fishing takes place
#' and to which a selectivity-at-age is to be set. See the value of
#' `p_sel_fish` in the returned list
#' @param rds_fn File name for the RDS file containing the4 model input/output
#' as created using [hake::create_rds_file()]
#' @param ... Arguments to be passed to [load_ss_sel_parameters()]
#'
#' @return A [list] with objects to be used throughout the package code
#'
#' @export
load_ss_model_data <- function(s_min = 1,
                               s_max = 6,
                               s_min_survey = 2,
                               s_max_survey = 6,
                               weight_factor = 1000,
                               n_space = 2,
                               rds_fn,
                               ...){

  # The RDS file will have the same name as the directory it is in
  if(!file.exists(rds_fn)){
    stop("The RDS file does not exist.\n",
         rds_fn)
  }

  ss_model <- readRDS(rds_fn)
  ss_model$catch_country <- extract_catch_country(...)
  ss_model$catch_seas_country <- calc_catch_seas_country(...)

  lst <- NULL

  # Catch observations --------------------------------------------------------
  lst$catch_obs <- ss_model$dat$catch |>
    transmute(yr = year,
              value = catch) |>
    filter(yr > 1900) |>
    as.matrix()

  lst$s_yr <- min(lst$catch_obs[, "yr"])
  lst$m_yr <- max(lst$catch_obs[, "yr"])
  yrs <- lst$s_yr:lst$m_yr


  lst$catch_props_space_season <- ss_model$catch_seas_country
  lst$catch_country <- ss_model$catch_country

  # Weight-at-age data --------------------------------------------------------
  waa <- ss_model$wtatage |>
    as_tibble() |>
    select(-c("Seas", "Sex", "Bio_Pattern", "BirthSeas"))

  fill_means <- \(waa_df){
    mn <- calc_mean_waa(
      waa = waa_df,
      yrs = -ss_model$startyr)
    fill_waa_years(
      waa = waa_df,
      values = mn,
      yrs = (ss_model$startyr + 1):(unique(ss_model$agedbase$Yr)[1] - 1))
  }
  lst$wage_catch_df <- waa |>
    filter(Fleet == 1) |>
    select(-Fleet) |>
    mutate(Yr = ifelse(Yr < 0, lst$s_yr, Yr)) |>
    filter(Yr <= lst$m_yr) |>
    fill_means()

  lst$wage_survey_df <- waa |>
    filter(Fleet == 2) |>
    select(-Fleet) |>
    mutate(Yr = ifelse(Yr < 0, lst$s_yr, Yr)) |>
    filter(Yr <= lst$m_yr) |>
    fill_means()

  lst$wage_mid_df <- waa |>
    filter(Fleet == -1) |>
    select(-Fleet) |>
    mutate(Yr = ifelse(Yr < 0, lst$s_yr, Yr)) |>
    filter(Yr <= lst$m_yr) |>
    fill_means()

  lst$wage_ssb_df <- waa |>
    as_tibble() |>
    filter(Yr < 0, Fleet == -2) |>
    mutate(Yr = lst$s_yr) |>
    select(-Fleet)

  # Maturity from first year only ---------------------------------------------
  lst$mat_sel <- lst$wage_ssb_df

  #lst$parms_scalar <- load_ss_parameters(ss_model)
  lst$age_survey <- load_ss_extract_age_comps(ss_model,
                                              age_comps_fleet = 2,
                                              s_yr = lst$s_yr,
                                              m_yr = lst$m_yr,
                                              ...)
  lst$age_catch <- load_ss_extract_age_comps(ss_model,
                                             age_comps_fleet = 1,
                                             s_yr = lst$s_yr,
                                             m_yr = lst$m_yr,
                                             ...)

  # Selectivity estimates -----------------------------------------------------
  sel_fish <- ss_model$extra_mcmc$sel_fishery_med |>
    select(yr, as.character(s_min:s_max))
  # The initial year is present, and that is duplicated in the year prior to
  # the TV selectivity starting (1991) So `dups` has two rows, for 1966 and
  # 1990. These are removed, and all the years in-between are filled in with
  # their values, then appended into the time series, completing it
  dups <- sel_fish[duplicated(sel_fish[, -1]) |
                     duplicated(sel_fish[, -1],
                                fromLast = TRUE), ]

  val_lst <- dups[1, -1] |>
    as.vector()
  static_sel_df <- dups |>
    complete(yr = min(dups$yr):max(dups$yr),
             fill = val_lst)

  sel_fish <- sel_fish[!duplicated(sel_fish[, -1]), ] |>
    filter(yr != ss_model$startyr) |>
    filter(yr <= ss_model$endyr) |>
    bind_rows(static_sel_df) |>
    arrange(yr) |>
    t() |>
    as_tibble() |>
    # Suppress .name_repair warning which doesn't matter
    suppressWarnings()
  names(sel_fish) <- sel_fish |>
    slice(1)
  sel_fish <- sel_fish |>
    slice(-1)

  # `lst$sel_by_yrs` has `s_max` - `s_min` + 1 rows, for ages `s_min` to
  # `s_max` and 33 columns, one for each year 1991-2023 (2023-1991 + 1)
  lst$sel_by_yrs <- sel_fish

  # Selectivity parameter estimates -------------------------------------------
  sel_param_ests <- load_ss_sel_parameters(ss_model, ...)

  p_sel_fish <- sel_param_ests |>
    filter(source == 1)
  p_sel_surv <- sel_param_ests |>
    filter(source == 2)

  # Add more selectivities by space (area).
  if(n_space == 1){
    p_sel_fish <- p_sel_fish |>
      mutate(space = 1)
  }else{
    p_sel_fish <- p_sel_fish |>
      mutate(space = 2)
    p_sel_fish2 <- p_sel_fish |>
      mutate(space = 1, value = 1)
    p_sel_fish <- p_sel_fish |>
      bind_rows(p_sel_fish2) |>
      select(value, source, space, age) |>
      arrange(order(space))
  }

  lst$p_sel_fish <- p_sel_fish |> as.matrix()
  lst$p_sel_surv <- p_sel_surv |> as.matrix()

  # Survey index and error (log SD) -------------------------------------------
  browser()
  surv <- ss_model$dat$CPUE |>
    as_tibble() |>
    filter(index == 2) |>
    transmute(yr = year,
              value = obs,
              err = se_log) |>
    complete(yr = seq(lst$s_yr, lst$m_yr)) %>%
    replace(is.na(.), 1)

  lst$survey <- surv |> pull(value)
  lst$survey_err <- surv |> pull(err)

  # Sample sizes for fishery and survey ---------------------------------------
  ss <- ss_model$agedbase  |>
    as_tibble() |>
    transmute(yr = Yr,
              fleet = Fleet,
              ss = Nsamp_adj) |>
    distinct()
  ss_catch <- ss |>
    filter(fleet == 1) |>
    select(-fleet) |>
    mutate(flag = 1) |>
    complete(yr = seq(lst$s_yr, lst$m_yr)) %>%
    replace(is.na(.), 0) %>%
    mutate(flag = ifelse(flag == 0, -1, flag))
  lst$ss_catch <- ss_catch |> pull(ss)
  lst$flag_catch <- ss_catch |> pull(flag)
  ss_survey <- ss |>
    filter(fleet == 2) |>
    select(-fleet) |>
    mutate(flag = 1) |>
    complete(yr = seq(lst$s_yr, lst$m_yr)) %>%
    replace(is.na(.), 0) |>
    mutate(flag = ifelse(flag == 0, -1, flag))
  lst$ss_survey <- ss_survey |> pull(ss)
  lst$flag_survey <- ss_survey |> pull(flag)

  # Median population estimates -----------------------------------------------
  # The following is shown in a table in the assessment doc and made by the
  # make.median.posterior.table() function in the hake-assessment repository
  mc <- ss_model$mcmccalcs
  if(is.null(mc)){
    stop("You must have a valid MCMC folder in the SS model directory to ",
         "load from",
         call. = FALSE)
  }
  vals_mc <- mc[c("smed", "dmed", "rmed", "pmed", "fmed")]
  vals_mc <- map(vals_mc, ~{
    .x[names(.x) %in% yrs]
  })

  if(is.null(ss_model$extra_mcmc)){
    lst$med_popests <-  tibble(f_ssb = vals_mc$smed * weight_factor,
                               rel_ssb = vals_mc$dmed,
                               r = vals_mc$rmed * weight_factor,
                               spr_f = vals_mc$pmed,
                               e = vals_mc$fmed)
  }else{
    b <- ss_model$timeseries |>
      as_tibble() |>
      filter(Yr %in% yrs) |>
      pull(Bio_all)
    b <- b / weight_factor

    lst$med_popests <-  tibble(f_ssb = vals_mc$smed * weight_factor,
                               rel_ssb = vals_mc$dmed,
                               b = b,
                               r = vals_mc$rmed * weight_factor,
                               spr_f = vals_mc$pmed,
                               e = vals_mc$fmed)
  }

  lst$med_popests <- lst$med_popests |>
    mutate(yr = yrs) |>
    select(yr, everything())
  # Make Relative fishing intensity and Exploitation fraction `NA` for the
  #  last year because the catch for that year has not occurred yet and the
  #  values output by the model are meaningless
  nrow_p <- nrow(lst$med_popests)
  ncol_p <- ncol(lst$med_popests)
  lst$med_popests[nrow_p, ncol_p - 1] <- NA
  lst$med_popests[nrow_p, ncol_p] <- NA

  # Recruitment deviations ----------------------------------------------------
  lst$r_dev <- ss_model$recruitpars |>
    select(-type) |>
    rownames_to_column("type") %>%
    filter(grepl("Early_RecrDev|Main_RecrDev", .[, "type"])) |>
    transmute(yr = Yr, value = Value)

  # Bias ramp adjustment ------------------------------------------------------
  # The hake model no longer used bias ramping after 2021 due to strictly
  # MCMC models being used. The MLE models were abandoned
  # TODO: Test to see if this needs to be present or if it can be deleted
  ctl <- ss_model$ctl
  b_breakpoints <- ss_model$breakpoints_for_bias_adjustment_ramp[1, ] |>
    as.numeric()
  yb_1 <- b_breakpoints[1]
  yb_2 <- b_breakpoints[2]
  yb_3 <- b_breakpoints[3]
  yb_4 <- b_breakpoints[4]
  b_max <- b_breakpoints[5]
  b_yrs <- lst$s_yr:lst$m_yr
  lst$b <- NULL
  for(j in 1:length(b_yrs)){
    if(b_yrs[j] <= yb_1){
      lst$b[j] <- 0
    }
    if(b_yrs[j] > yb_1 && b_yrs[j]< yb_2){
      lst$b[j] <- b_max * ((b_yrs[j] - yb_1) / (yb_2 - yb_1))
    }
    if(b_yrs[j] >= yb_2 && b_yrs[j] <= yb_3){
      lst$b[j] <- b_max
    }
    if(b_yrs[j] > yb_3 && b_yrs[j] < yb_4){
      lst$b[j] <- b_max * (1 - (yb_3 - b_yrs[j]) / (yb_4 - yb_3))
    }
    if(b_yrs[j] >= yb_4){
      lst$b[j] <- 0
    }
  }

  # Initial numbers-at-age ---------------------------------------------------
  lst$init_n <- ss_model$extra_mcmc$init_natage |>
    pull(`50%`)
  names(lst$init_n) <- ss_model$extra_mcmc$init_natage$age
  lst$init_n <- c(`0` = 0, lst$init_n)

  lst$ctl_file <- ss_model$ctl_file
  lst$ctl <- ss_model$ctl
  lst$dat_file <- ss_model$dat_file
  lst$dat <- ss_model$dat
  lst$mcmccalcs <- ss_model$mcmccalcs
  lst$mcmc <- ss_model$mcmc

  cat(green(symbol$tick),
      green("Loaded SS data successfully\n"))

  lst
}

