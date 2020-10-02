#' Hake objectives (TODO: Improve docs on this function)
#'
#' @param lst list of MSE results
#' @param sim_data simulated data from the OM
#' @param short_term_yrs Years for short term plots
#' @param long_term_yrs Years greater than this will be in long term plots
#' @param can.prop Proportion of the coastwide TAC that Canada receives
#' @param us.prop Proportion of the coastwide TAC that the US receives
#' @param quants Quantiles to calculate for plotting output [data.frame]s
#' @param catch_multiplier Value to multiply all catch calculations by
#'
#' @return List of three: p.export, t.export, ls.season (TODO: explain better)
#' @importFrom ggplot2 alpha
#' @importFrom dplyr left_join lead lag n summarize_at vars everything bind_rows
#' @importFrom purrr partial map_int
#' @importFrom stats quantile
#' @export
hake_objectives <- function(lst = NULL,
                            sim_data = NULL,
                            run_num = 1,
                            short_term_yrs = 2018:2022,
                            long_term_yrs = 2022,
                            can.prop = 0.2488,
                            us.prop = 0.7612,
                            quants = c(0.05, 0.25, 0.5, 0.75, 0.95),
                            catch_multiplier = 1e-6,
                            ...){
  verify_argument(lst, "list")
  verify_argument(sim_data, "list")
  verify_argument(run_num, "numeric")
  verify_argument(short_term_yrs, c("integer", "numeric"))
  verify_argument(long_term_yrs, c("integer", "numeric"))
  verify_argument(can.prop,  "numeric")
  verify_argument(us.prop,  "numeric")
  verify_argument(quants,  "numeric")
  verify_argument(catch_multiplier,  "numeric")

  if(run_num > length(lst)){
    stop("run_num (", run_num, ") is greater than the number of runs completed (", length(lst), ")",
         call. = FALSE)
  }
  yrs <- as.numeric(names(sim_data[[1]]$ssb[,1]))
  min_yr <- min(yrs)
  nyrs <- length(yrs)
  simyears <- nyrs - (length(min_yr:short_term_yrs[1])) + 1
  nruns <- length(lst)
  lst_run <- lst[[run_num]]

  out <- list()

  # ---------------------------------------------------------------------------
  out$ssb_plot <- map2(sim_data, seq_along(sim_data), ~{
    data.frame(year = yrs,
               ssb = rowSums(.x$ssb) / sum(.x$ssb_0),
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  out$ssb_mid_plot <- map2(sim_data, seq_along(sim_data), ~{
    data.frame(year = yrs,
               ssb = rowSums(.x$ssb_all[, , 3]) / sum(.x$ssb_0),
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  # Calculate SSB quantiles
  ssb_yrs <- sort(unique(out$ssb_plot$year))
  out$ssb_plotquant <- out$ssb_plot %>%
    group_by(year) %>%
    group_map(~ calc_quantiles(.x, col = "ssb", probs = quants)) %>%
    map_df(~{.x}) %>%
    mutate(year = ssb_yrs) %>%
    select(year, everything())

  ssb_yrs <- sort(unique(out$ssb_mid_plot$year))
  out$ssb_mid_plotquant <- out$ssb_mid_plot %>%
    group_by(year) %>%
    group_map(~ calc_quantiles(.x, col = "ssb", probs = quants)) %>%
    map_df(~{.x}) %>%
    mutate(year = ssb_yrs) %>%
    select(year, everything())

  # ---------------------------------------------------------------------------

  # Vulnerable biomass at mid-year start of season 3 for each country
  out$v_ca_plot <- map2(sim_data, seq_along(sim_data), ~{
    data.frame(year = yrs,
               v = .x$v_save[,1,3],
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  out$v_us_plot <- map2(sim_data, seq_along(sim_data), ~{
    data.frame(year = yrs,
               v = .x$v_save[,2,3],
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  out$f0_plot <- map2(sim_data, seq_along(sim_data), ~{
    yrs <- rownames(.x$f_out_save)
    apply(.x$f_out_save, c(1, 3), sum) %>%
      as_tibble() %>%
      mutate(run = .y) %>%
      mutate(year = yrs) %>%
      select(year, everything())
  }) %>%
    map_df(~{.x})

  out$f0_ca_plot <- out$f0_plot %>%
    select(-`2`) %>%
    rename(f = `1`)

  out$f0_us_plot <- out$f0_plot %>%
    select(-`1`) %>%
    rename(f = `2`)

  out$f0_ca_quant <- calc_quantiles_by_group(out$f0_ca_plot,
                                             "year",
                                             "f",
                                             probs = quants)
  out$f0_us_quant <- calc_quantiles_by_group(out$f0_us_plot,
                                             "year",
                                             "f",
                                             probs = quants)

  out$catch_plot <- map2(sim_data, seq_along(sim_data), ~{
    ct <- apply(.x$catch_save_age, MARGIN = 2, FUN = sum)
    if(length(ct) == 1){
      data.frame(year = yrs,
                 catch = .x$catch_save_age,
                 run = .y)
    }else{
      data.frame(year = yrs,
                 catch = ct,
                 run = .y)
    }
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  #----------- Calculate quantiles by year -------------------
  out$catch_quant <- map2(sim_data, 1:nruns,~{
    tmp <- .x$catch %>%
      as.data.frame()
    names(tmp) <- "value"
    tmp <- tmp %>%
      as_tibble() %>%
      mutate(year = yrs) %>%
      mutate(run = .y)
  })
  names(out$catch_quant) <- 1:nruns
  out$catch_quant <- out$catch_quant %>%
    map_dfr(~{.x}) %>%
    select(year, run, value)

  out$catch_quant <- calc_quantiles_by_group(out$catch_quant,
                                             "year",
                                             "value",
                                             probs = quants)
  #-----------------------------------------------------------

  # Need to check generation of age_comps_catch_space because there are no age 15's they are NA!!
  # sim_data[[1]]$age_comps_catch_space[,,1]
  yrs <- rownames(sim_data[[1]]$ssb)
  conv_am <- function(space, sim_age_comp_type = "catch", mse_dat = lst){
    if(sim_age_comp_type != "catch" && sim_age_comp_type != "surv"){
      stop("sim_age_comp_type must be catch or surv", call. = FALSE)
    }
    if(space != 0 && space != 1 && space != 2){
      stop("space must be 0, 1, or 2", call. = FALSE)
    }
    x <- map_df(1:nruns, ~{
      if(sim_age_comp_type == "catch"){
        if(space == 1){
          calc_mean_age(sim_data[[.x]]$age_comps_catch_space[,,1], mse_dat[[.x]][[1]]$age_max_age)
        }else if(space == 2){
          calc_mean_age(sim_data[[.x]]$age_comps_catch_space[,,2], mse_dat[[.x]][[1]]$age_max_age)
        }else{
          calc_mean_age(sim_data[[.x]]$age_comps_catch, mse_dat[[.x]][[1]]$age_max_age)
        }
      }else{
        if(space == 1){
          calc_mean_age(sim_data[[.x]]$age_comps_surv_space[,,1], mse_dat[[.x]][[1]]$age_max_age)
        }else if(space == 2){
          calc_mean_age(sim_data[[.x]]$age_comps_surv_space[,,2], mse_dat[[.x]][[1]]$age_max_age)
        }else{
          calc_mean_age(sim_data[[.x]]$age_comps_surv, mse_dat[[.x]][[1]]$age_max_age)
        }
      }
    }) %>%
      t() %>%
      as_tibble() %>%
      mutate(yr = yrs)
    names(x) <- c(1:nruns, "yr")
    x <- x %>%
      select(yr, everything())
    x
  }

  #----------------------------------------------------------------------------
  out$amc_tot <- conv_am(0)
  out$amc_tot_quant <- melt(out$amc_tot, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$amc_tot_quant <- calc_quantiles_by_group(out$amc_tot_quant,
                                               "year",
                                               "val",
                                               probs = quants)
  #----------------------------------------------------------------------------
  out$amc_ca <- conv_am(1)
  out$amc_ca_quant <- melt(out$amc_ca, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$amc_ca_quant <- calc_quantiles_by_group(out$amc_ca_quant,
                                              "year",
                                              "val",
                                              probs = quants)
  #----------------------------------------------------------------------------
  out$amc_us <- conv_am(2)
  out$amc_us_quant <- melt(out$amc_us, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$amc_us_quant <- calc_quantiles_by_group(out$amc_us_quant,
                                              "year",
                                              "val",
                                              probs = quants)
  #----------------------------------------------------------------------------
  out$ams_tot <- conv_am(0, "surv")
  out$ams_tot_quant <- melt(out$ams_tot, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$ams_tot_quant <- calc_quantiles_by_group(out$ams_tot_quant,
                                               "year",
                                               "val",
                                               probs = quants)
  #----------------------------------------------------------------------------
  out$ams_ca <- conv_am(1, "surv")
  out$ams_ca_quant <- melt(out$ams_ca, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$ams_ca_quant <- calc_quantiles_by_group(out$ams_ca_quant,
                                              "year",
                                              "val",
                                              probs = quants)
  #----------------------------------------------------------------------------
  out$ams_us <- conv_am(2, "surv")
  out$ams_us_quant <- melt(out$ams_us, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$ams_us_quant <- calc_quantiles_by_group(out$ams_us_quant,
                                              "year",
                                              "val",
                                              probs = quants)

  out$quota_tot <- map2(sim_data, seq_along(sim_data), ~{
    quot <- apply(.x$catch_quota, MARGIN = 1, FUN = sum)
    if(length(quot) == 1){
      data.frame(year = yrs,
                 catch = .x$catch_quota,
                 run = .y)
    }else{
      data.frame(year = yrs,
                 catch = quot,
                 run = .y)
    }
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  out$quota_quant <- calc_quantiles_by_group(out$quota_tot,
                                             "year",
                                             "catch",
                                             probs = quants)

  out$quota_plot <- map2(list(out$quota_tot), list(out$catch_plot), ~{
    class(.x$year) <- class(.y$year)
    class(.x$run) <- class(.y$run)
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(quota_frac = catch.x / catch.y) %>%
      select(year, quota_frac, run)
  }) %>% map_df(~{.x})

  out$quota_us_tot <- map2(sim_data, seq_along(sim_data), ~{
    data.frame(year = yrs,
               quota = rowSums(.x$catch_quota[,2,]),
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  out$quota_ca_tot <- map2(sim_data, seq_along(sim_data), ~{
    data.frame(year = yrs,
               quota = rowSums(.x$catch_quota[,1,]),
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  out$yrs_quota_met <- out$ssb_plot %>%
    group_by(run) %>%
    summarize(value = length(which(ssb > 0.1 && ssb <= 0.4)) / simyears)

  out$catch_area <- map2(sim_data, seq_along(sim_data), ~{
    data.frame(year = yrs,
               area = apply(.x$catch_save_age, MARGIN = c(2, 3), FUN = sum),
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    transmute(year,
              ca = area.1,
              us = area.2,
              run) %>%
    as_tibble()

  out$catch_us_tot <- out$catch_area %>%
    transmute(year, catch = us, run)
  out$catch_ca_tot <- out$catch_area %>%
    transmute(year, catch = ca, run)

  out$vtac_us <- map2(list(out$v_us_plot), list(out$catch_us_tot), ~{
    class(.x$year) <- class(.y$year)
    class(.x$run) <- class(.y$run)
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(v_tac = v / catch) %>%
      select(year, v_tac, run)
  }) %>%
    map_df(~{.x})

  out$vtac_ca <- map2(list(out$v_us_plot), list(out$catch_ca_tot), ~{
    class(.x$year) <- class(.y$year)
    class(.x$run) <- class(.y$run)
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(v_tac = v / catch) %>%
      select(year, v_tac, run)
  }) %>%
    map_df(~{.x})

  out$vtac_us_seas <- map2(sim_data, seq_along(sim_data), ~{
    ctmp <- colSums(.x$catch_save_age)
    data.frame(year = yrs,
               v_tac_sp = ctmp[, 2, 2] / .x$v_save[, 2, 2],
               v_tac_su = ctmp[, 2, 3] / .x$v_save[, 2, 3],
               v_tac_fa = ctmp[, 2, 4] / .x$v_save[, 2, 4],
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    mutate(country = "US") %>%
    as_tibble()

  out$vtac_ca_seas <- map2(sim_data, seq_along(sim_data), ~{
    ctmp <- colSums(.x$catch_save_age)
    data.frame(year = yrs,
               v_tac_sp = ctmp[, 1, 2] / .x$v_save[, 1, 2],
               v_tac_su = ctmp[, 1, 3] / .x$v_save[, 1, 3],
               v_tac_fa = ctmp[, 1, 4] / .x$v_save[, 1, 4],
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    mutate(country = "Canada") %>%
    as_tibble()

  out$vtac_seas <- bind_rows(out$vtac_ca_seas, out$vtac_us_seas)

  out$aav_plot <- map2(list(out$catch_plot), seq_along(list(out$catch_plot)), ~{
    .x %>%
      group_by(run) %>%
      mutate(catch_lag = lag(catch, 1)) %>%
      mutate(aav = abs(catch_lag - catch) / catch_lag) %>%
      ungroup() %>%
      filter(!is.na(aav)) %>%
      select(year, aav, run)
  }) %>%
    map_df(~{.x})

  out$v_ca_plotquant <- calc_quantiles_by_group(out$v_ca_plot, grp_col = "year", col = "v", probs = quants)
  out$v_us_plotquant <- calc_quantiles_by_group(out$v_us_plot, grp_col = "year", col = "v", probs = quants)
  out$catch_plotquant <- calc_quantiles_by_group(out$catch_plot, grp_col = "year", col = "catch", probs = quants)
  out$aav_plotquant <- calc_quantiles_by_group(out$aav_plot, grp_col = "year", col = "aav", probs = quants)
  out$quota_plotquant <- calc_quantiles_by_group(out$quota_plot, grp_col = "year", col = "quota_frac", probs = quants)

  ssb_future <- out$ssb_plot %>%
    filter(year > min(short_term_yrs))
  # Probability of SSB < SSB_10%
  out$ssb_10 <- ssb_future %>%
    group_by(run) %>%
    summarize(pcnt = length(which(ssb < 0.1)) /
                length(unique(year))) %>%
    group_map(~ calc_quantiles(.x, col = "pcnt", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  # Probability of SSB >= SSB_10% and <= SSB_40%
  out$ssb_4010 <- ssb_future %>%
    group_by(run) %>%
    summarize(pcnt = length(which(ssb >= 0.1 & ssb <= 0.4)) /
                length(unique(year))) %>%
    group_map(~ calc_quantiles(.x, col = "pcnt", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  out$catch_short_term <- calc_term_quantiles(out$catch_plot,
                                              grp_col = "run",
                                              col = "catch",
                                              min_yr = min(short_term_yrs),
                                              max_yr = long_term_yrs,
                                              probs = quants,
                                              mean_multiplier = catch_multiplier)

  out$catch_long_term <- calc_term_quantiles(out$catch_plot,
                                             grp_col = "run",
                                             col = "catch",
                                             min_yr = long_term_yrs + 1,
                                             probs = quants,
                                             mean_multiplier = catch_multiplier)


  out$aav_short_term <- calc_term_quantiles(out$aav_plot,
                                            grp_col = "run",
                                            col = "aav",
                                            min_yr = min(short_term_yrs),
                                            max_yr = long_term_yrs,
                                            probs = quants)

  out$v_ca_stat <- calc_term_quantiles(out$v_ca_plot,
                                       grp_col = "run",
                                       col = "v",
                                       probs = quants)

  out$v_us_stat <- calc_term_quantiles(out$v_us_plot,
                                       grp_col = "run",
                                       col = "v",
                                       probs = quants)

  out$vtac_ca_stat <- out$vtac_ca %>%
    group_by(run) %>%
    summarise(prop = length(which(v_tac > (1 / 0.3))) / length(v_tac)) %>%
    group_map(~ calc_quantiles(.x, col = "prop", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  out$vtac_us_stat <- out$vtac_us %>%
    group_by(run) %>%
    summarise(prop = length(which(v_tac > 1)) / length(v_tac)) %>%
    group_map(~ calc_quantiles(.x, col = "prop", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  out$vtac_ca_seas_stat <- out$vtac_ca_seas %>%
    filter(year > long_term_yrs) %>%
    group_by(run) %>%
    summarise(avg_sp = mean(1 / v_tac_sp),
              avg_su = mean(1 / v_tac_su),
              avg_fa = mean(1 / v_tac_fa)) %>%
    summarise(med_sp = median(avg_sp),
              med_su = median(avg_su),
              med_fa = median(avg_fa))

  out$vtac_us_seas_stat <- out$vtac_us_seas %>%
    filter(year > long_term_yrs) %>%
    group_by(run) %>%
    summarise(avg_sp = mean(1 / v_tac_sp),
              avg_su = mean(1 / v_tac_su),
              avg_fa = mean(1 / v_tac_fa)) %>%
    summarise(med_sp = median(avg_sp),
              med_su = median(avg_su),
              med_fa = median(avg_fa))

  # Calculate the median number of closed years
  out$nclosed <- map_int(unique(ssb_future$run), ~{
    tmp <- ssb_future %>% filter(run == .x)
    length(which(tmp$ssb < 0.1))
  })

  # Create a table with all the indicator data
  indicator <- c("SSB < 0.10 SSB0",
                 "0.10 < SSB < 0.4 SSB0",
                 "SSB > 0.4 SSB0",
                 "AAV",
                 "Mean SSB / SSB0",
                 "Short term catch",
                 "Long term catch",
                 "Canada TAC/V spr",
                 "Canada TAC/V sum",
                 "Canada TAC/V fall",
                 "US TAC/V spr",
                 "US TAC/V sum",
                 "US TAC/V fall")

  out$aav_quant_by_run <- calc_quantiles_by_group(out$aav_plot, grp_col = "run",
                                                  col = "aav", probs = quants)
  tmp_ssb_by_run <- out$ssb_plot %>% filter(year > min(short_term_yrs))
  out$ssb_quant_by_run <- calc_quantiles_by_group(tmp_ssb_by_run, grp_col = "run",
                                                    col = "ssb", probs = quants)

  tmp_catch_by_run <- out$catch_plot %>% filter(year > min(short_term_yrs), year <= long_term_yrs)
  out$catch_quant_by_run <- calc_quantiles_by_group(tmp_catch_by_run, grp_col = "run",
                                                    col = "catch", probs = quants)
  tmp_catch_by_run <- out$catch_plot %>% filter(year > long_term_yrs - 2)
  tmp_catch_by_run <- calc_quantiles_by_group(tmp_catch_by_run, grp_col = "run",
                                              col = "catch", probs = quants)
  out$info <- map(1:nruns, ~{
    data.frame(
      indicator = as.factor(indicator),
      value = c(
  round(length(which(ssb_future[ssb_future$run == .x,]$ssb <= 0.1)) /
         length(ssb_future[ssb_future$run == .x,]$ssb), digits = 2),
  round(length(which(ssb_future[ssb_future$run == .x,]$ssb > 0.1 & ssb_future[ssb_future$run == .x,]$ssb < 0.4)) /
          length(ssb_future[ssb_future$run == .x,]$ssb), digits = 2),
  round(length(which(ssb_future[ssb_future$run == .x,]$ssb > 0.4)) /
          length(ssb_future[ssb_future$run == .x,]$ssb), digits = 2),
  round(out$aav_quant_by_run[out$aav_quant_by_run$run == .x,]$`0.5`, digits = 2),
  round(out$ssb_quant_by_run[out$ssb_quant_by_run$run == .x,]$`0.5`, digits = 2),
  round(out$catch_quant_by_run[out$catch_quant_by_run$run == .x,]$`0.5`, digits = 2),
  round(tmp_catch_by_run[tmp_catch_by_run$run == .x,]$`0.5`, digits = 2),
  out$vtac_ca_seas_stat$med_sp,
  out$vtac_ca_seas_stat$med_su,
  out$vtac_ca_seas_stat$med_fa,
  out$vtac_us_seas_stat$med_sp,
  out$vtac_us_seas_stat$med_su,
  out$vtac_us_seas_stat$med_fa)) %>%
      as_tibble() %>%
      mutate(run = .x)
    }) %>%
    map_df(~{.x})

  out
}
