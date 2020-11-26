#' Hake objectives (TODO: Improve docs on this function)
#'
#' @param sim_data simulated data from the OM
#' @param short_term_yrs Years included in short term plots
#' @param long_term_yrs Years greater than this will be in long term plots
#' @param quants Quantiles to calculate for plotting output [data.frame]s
#' @param catch_multiplier Value to multiply all catch calculations by
#' @param ... Absorbs extra parameters
#'
#' @return List of many outputs from the MSE
#' @importFrom ggplot2 alpha
#' @importFrom dplyr left_join lead lag n summarize_at vars everything bind_rows
#' @importFrom purrr partial map_int
#' @importFrom stats quantile
#' @export
hake_objectives <- function(sim_data = NULL,
                            short_term_yrs = NULL,
                            long_term_yrs = NULL,
                            quants = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975),
                            catch_multiplier = 1e-6,
                            ...){

  verify_argument(sim_data, "list")
  verify_argument(short_term_yrs, c("integer", "numeric"))
  verify_argument(long_term_yrs, c("integer", "numeric"), 1)
  verify_argument(quants,  "numeric")
  verify_argument(catch_multiplier,  "numeric")

  yrs <- sim_data[[1]]$yrs
  min_yr <- min(yrs)
  nyrs <- length(yrs)
  simyears <- nyrs - (length(min_yr:short_term_yrs[1])) + 1
  nruns <- length(sim_data)

  out <- list()

  # ssb_plot ------------------------------------------------------------------
  out$ssb_plot <- map2(sim_data, seq_along(sim_data), ~{
    data.frame(year = yrs,
               ssb = rowSums(.x$ssb) / sum(.x$ssb_0),
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  # ssb_ssb0_quant ------------------------------------------------------------
  out$ssb_ssb0_quant <- calc_quantiles_by_group(out$ssb_plot,
                                                "year",
                                                "ssb",
                                                probs = quants)

  # ssb_mid_plot --------------------------------------------------------------
  out$ssb_mid_plot <- map2(sim_data, seq_along(sim_data), ~{
    data.frame(year = yrs,
               ssb = rowSums(.x$ssb_all[ , , 3]) / sum(.x$ssb_0),
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  # ssb_mid_space--------------------------------------------------------------
  out$ssb_mid_space <- map2(sim_data, seq_along(sim_data), ~{
    yrs <- as.numeric(rownames(.x$ssb_all[ , , 3]))
    .x$ssb_all[ , , 3] %>%
      as_tibble() %>%
      mutate(run = .y, year = yrs) %>%
      select(year, everything())
  }) %>%
    map_df(~{.x})

  # ssb_mid_ca ----------------------------------------------------------------
  out$ssb_mid_ca <- out$ssb_mid_space %>%
    select(year, `1`, run) %>%
    rename(ssb = `1`)

  # ssb_mid_ca_quant ----------------------------------------------------------
  out$ssb_mid_ca_quant <- out$ssb_mid_ca %>%
    calc_quantiles_by_group("year", "ssb", probs = quants)

  # ssb_mid_us ----------------------------------------------------------------
  out$ssb_mid_us <- out$ssb_mid_space %>%
    select(year, `2`, run) %>%
    rename(ssb = `2`)

  # ssb_mid_us_quant ----------------------------------------------------------
  out$ssb_mid_us_quant <- out$ssb_mid_us %>%
    calc_quantiles_by_group("year", "ssb", probs = quants)

  # ssb_space -----------------------------------------------------------------
  out$ssb_space <- map2(sim_data, seq_along(sim_data), ~{
    yrs <- as.numeric(rownames(.x$ssb_all[ , , 1]))
    .x$ssb_all[ , , 1] %>%
      as_tibble() %>%
      mutate(run = .y, year = yrs) %>%
      select(year, everything())
  }) %>%
    map_df(~{.x})

  # ssb_all -------------------------------------------------------------------
  out$ssb_all <- out$ssb_space %>%
    mutate(ssb = `1` + `2`) %>%
    select(year, ssb, run)

  # ssb_all_mid ---------------------------------------------------------------
  out$ssb_mid_all <- out$ssb_mid_space %>%
    mutate(ssb = `1` + `2`) %>%
    select(year, ssb, run)

  # ssb_all_quant -------------------------------------------------------------
  out$ssb_all_quant <- out$ssb_all %>%
    calc_quantiles_by_group("year", "ssb", probs = quants)

  # ssb_mid_quant -------------------------------------------------------------
  out$ssb_mid_quant <- out$ssb_mid_all %>%
    calc_quantiles_by_group("year", "ssb", probs = quants)

  # ssb_ca --------------------------------------------------------------------
  out$ssb_ca <- out$ssb_space %>%
    select(year, `1`, run) %>%
    rename(ssb = `1`)

  # ssb_ca_quant --------------------------------------------------------------
  out$ssb_ca_quant <- out$ssb_ca %>%
    calc_quantiles_by_group("year", "ssb", probs = quants)

  # ssb_us --------------------------------------------------------------------
  out$ssb_us <- out$ssb_space %>%
    select(year, `2`, run) %>%
    rename(ssb = `2`)

  # ssb_us_quant --------------------------------------------------------------
  out$ssb_us_quant <- out$ssb_us %>%
    calc_quantiles_by_group("year", "ssb", probs = quants)

  # v_ca_plot -----------------------------------------------------------------
  # Vulnerable biomass at mid-year start of season 3 for each country
  out$v_ca_plot <- map2(sim_data, seq_along(sim_data), ~{
    data.frame(year = yrs,
               v = .x$v_save[,1,3],
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  # v_us_plot -----------------------------------------------------------------
  out$v_us_plot <- map2(sim_data, seq_along(sim_data), ~{
    data.frame(year = yrs,
               v = .x$v_save[,2,3],
               run = .y)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  # f0_plot -------------------------------------------------------------------
  out$f0_plot <- map2(sim_data, seq_along(sim_data), ~{
    yrs <- rownames(.x$f_out_save)
    apply(.x$f_out_save, c(1, 3), sum) %>%
      as_tibble() %>%
      mutate(run = .y) %>%
      mutate(year = yrs) %>%
      select(year, everything())
  }) %>%
    map_df(~{.x})

  # f0_ca_plot-----------------------------------------------------------------
  out$f0_ca_plot <- out$f0_plot %>%
    select(-`2`) %>%
    rename(f = `1`)

  # f0_us_plot ----------------------------------------------------------------
  out$f0_us_plot <- out$f0_plot %>%
    select(-`1`) %>%
    rename(f = `2`)

  # f0_ca_quant ---------------------------------------------------------------
  out$f0_ca_quant <- calc_quantiles_by_group(out$f0_ca_plot,
                                             "year",
                                             "f",
                                             probs = quants)

  # f0_us_quant ---------------------------------------------------------------
  out$f0_us_quant <- calc_quantiles_by_group(out$f0_us_plot,
                                             "year",
                                             "f",
                                             probs = quants)

  # catch_plot ----------------------------------------------------------------
  out$catch_for_aav <- map2(sim_data, seq_along(sim_data), ~{
    apply(.x$catch_save_age, MARGIN = 2, FUN = sum) %>%
      as_tibble() %>%
      mutate(year = .x$yrs, run = .y) %>%
      rename(catch = value) %>%
      select(year, everything())
      #mutate(run = .y) %>%
  }) %>%
    bind_rows()

  # catch_plot ----------------------------------------------------------------
  out$catch_plot <- map2(sim_data, seq_along(sim_data), ~{
    .x$catch %>%
      as_tibble() %>%
      rename(catch = 1) %>%
      mutate(year = .x$yrs) %>%
      mutate(run = .y) %>%
      select(year, catch, run)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  # catch_quant ---------------------------------------------------------------
  out$catch_quant <- calc_quantiles_by_group(out$catch_plot,
                                             "year",
                                             "catch",
                                             probs = quants)

  # catch_country ----------------------------------------------------------------
  out$catch_country <- map2(sim_data, seq_along(sim_data), ~{
    .x$catch_country %>%
      as_tibble() %>%
      rename(catch_ca = space1, catch_us = space2) %>%
      mutate(run = .y) %>%
      select(year, catch_ca, catch_us, run)
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  # conv_am function ----------------------------------------------------------
  # TODO: Need to check generation of age_comps_catch_space because there are no age 15's they are NA!!
  # sim_data[[1]]$age_comps_catch_space[,,1]
  conv_am <- function(space, sim_age_comp_type = "catch"){
    if(!sim_age_comp_type %in% c("catch", "surv", "om")){
      stop("sim_age_comp_type must be catch, surv, or om", call. = FALSE)
    }
    if(space != 0 && space != 1 && space != 2){
      stop("space must be 0, 1, or 2", call. = FALSE)
    }

    x <- map_df(1:nruns, ~{
      if(sim_age_comp_type == "catch"){
        if(space == 1){
          # TODO: This code is all correct, but the space age_comps_catch_space[,,1], age_comps_catch_space[,,2],
          # age_comps_surv_space[,,1], age_comps_surv_space[,,2] are fully populated when I think they should
          # have NAs for non-data years. Populated correctly is age_comps_catch and age_comps_surv
          calc_mean_age(sim_data[[.x]]$age_comps_catch_space[,,1], sim_data[[.x]]$age_max_age)
        }else if(space == 2){
          calc_mean_age(sim_data[[.x]]$age_comps_catch_space[,,2], sim_data[[.x]]$age_max_age)
        }else{
          calc_mean_age(sim_data[[.x]]$age_comps_catch, sim_data[[.x]]$age_max_age)
        }
      }else if(sim_age_comp_type == "surv"){
        if(space == 1){
          calc_mean_age(sim_data[[.x]]$age_comps_surv_space[,,1], sim_data[[.x]]$age_max_age)
        }else if(space == 2){
          calc_mean_age(sim_data[[.x]]$age_comps_surv_space[,,2], sim_data[[.x]]$age_max_age)
        }else{
          calc_mean_age(sim_data[[.x]]$age_comps_surv, sim_data[[.x]]$age_max_age)
        }
      }else{
        if(space == 1){
          calc_mean_age(sim_data[[.x]]$age_comps_om[, , 1, 3], sim_data[[.x]]$age_max_age)
        }else if(space == 2){
          calc_mean_age(sim_data[[.x]]$age_comps_om[, , 2, 3], sim_data[[.x]]$age_max_age)
        }else{
          calc_mean_age(apply(sim_data[[.x]]$age_comps_om[, , , 3], c(1, 2), sum), sim_data[[.x]]$age_max_age)
        }
      }
    }) %>%
      t() %>%
      as_tibble(.name_repair = ~ as.character(1:nruns)) %>%
      mutate(yr = yrs)
    names(x) <- c(1:nruns, "yr")
    x <- x %>%
      select(yr, everything())
    x
  }

  # Average age in population -------------------------------------------------
  out$aap_tot <- conv_am(0, "om")
  out$aap_tot_quant <- melt(out$aap_tot, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$aap_tot_quant <- calc_quantiles_by_group(out$aap_tot_quant,
                                               "year",
                                               "val",
                                               probs = quants)

  # Average age in population for Canada --------------------------------------
  out$aap_ca <- conv_am(1, "om")
  out$aap_ca_quant <- melt(out$aap_ca, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$aap_ca_quant <- calc_quantiles_by_group(out$aap_ca_quant,
                                               "year",
                                               "val",
                                               probs = quants)

  # Average age in population for the US --------------------------------------
  out$aap_us <- conv_am(2, "om")
  out$aap_us_quant <- melt(out$aap_us, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$aap_us_quant <- calc_quantiles_by_group(out$aap_us_quant,
                                              "year",
                                              "val",
                                              probs = quants)

  # amc_tot_quant -------------------------------------------------------------
  out$amc_tot <- conv_am(0)
  out$amc_tot_quant <- melt(out$amc_tot, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$amc_tot_quant <- calc_quantiles_by_group(out$amc_tot_quant,
                                               "year",
                                               "val",
                                               probs = quants)
  # amc_ca_quant --------------------------------------------------------------
  out$amc_ca <- conv_am(1)
  out$amc_ca_quant <- melt(out$amc_ca, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$amc_ca_quant <- calc_quantiles_by_group(out$amc_ca_quant,
                                              "year",
                                              "val",
                                              probs = quants)
  # amc_us_quant --------------------------------------------------------------
  out$amc_us <- conv_am(2)
  out$amc_us_quant <- melt(out$amc_us, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$amc_us_quant <- calc_quantiles_by_group(out$amc_us_quant,
                                              "year",
                                              "val",
                                              probs = quants)
  # ams_tot_quant -------------------------------------------------------------
  out$ams_tot <- conv_am(0, "surv")
  out$ams_tot_quant <- melt(out$ams_tot, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$ams_tot_quant <- calc_quantiles_by_group(out$ams_tot_quant,
                                               "year",
                                               "val",
                                               probs = quants)
  # ams_ca_quant --------------------------------------------------------------
  out$ams_ca <- conv_am(1, "surv")
  out$ams_ca_quant <- melt(out$ams_ca, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$ams_ca_quant <- calc_quantiles_by_group(out$ams_ca_quant,
                                              "year",
                                              "val",
                                              probs = quants)
  # ams_us_quant --------------------------------------------------------------
  out$ams_us <- conv_am(2, "surv")
  out$ams_us_quant <- melt(out$ams_us, id.vars = "yr") %>%
    as_tibble() %>%
    set_names(c("year", "run", "val"))
  out$ams_us_quant <- calc_quantiles_by_group(out$ams_us_quant,
                                              "year",
                                              "val",
                                              probs = quants)

  # quota_space ---------------------------------------------------------------
  out$quota_space <- map2(sim_data, seq_along(sim_data), ~{
    yrs <- as.numeric(rownames(.x$catch_quota[,,1]))
    apply(.x$catch_quota, c(1,2), sum) %>%
      as_tibble() %>%
      mutate(run = .y) %>%
      mutate(year = yrs) %>%
      select(year, everything())
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  # quota_tot -----------------------------------------------------------------
  out$quota_tot <- out$quota_space %>%
    mutate(quota = `1` + `2`) %>%
    select(year, quota, run)

  # quota ---------------------------------------------------------------------
  out$quota <- map2(sim_data, seq_along(sim_data), ~{
    yrs <- as.numeric(rownames(.x$catch_quota[,,1]))
    rowSums(.x$catch_quota) %>%
      as_tibble() %>%
      mutate(run = .y) %>%
      mutate(year = yrs) %>%
      rename(quota = value) %>%
      select(year, everything())
  }) %>%
    map_df(~{.x}) %>%
    as_tibble()

  # catch_quota_tot -----------------------------------------------------------
  out$catch_quota_tot <- out$catch_plot %>%
    left_join(out$quota, by = c("run", "year")) %>%
    mutate(catch_quota = catch / quota) %>%
    select(year, catch, quota, catch_quota, run)

  #out$catch_quota_tot[is.na(out$catch_quota_tot)] <- 0

  # catch_quota_quant ---------------------------------------------------------
  out$catch_quota_quant <- out$catch_quota_tot %>%
    calc_quantiles_by_group("year", "catch_quota", probs = quants)

  # quota_ca ------------------------------------------------------------------
  out$quota_ca <- out$quota_space %>%
    select(year, `1`, run) %>%
    rename(quota = `1`)

  # quota_ca_quant ------------------------------------------------------------
  out$quota_ca_quant <- out$quota_ca %>%
    calc_quantiles_by_group("year", "quota", probs = quants)

  # quota_us ------------------------------------------------------------------
  out$quota_us <- out$quota_space %>%
    select(year, `2`, run) %>%
    rename(quota = `2`)

  # quota_us_quant ------------------------------------------------------------
  out$quota_us_quant <- out$quota_us %>%
    calc_quantiles_by_group("year", "quota", probs = quants)

  # quota_quant ---------------------------------------------------------------
  out$quota_quant <- calc_quantiles_by_group(out$quota_tot,
                                             "year",
                                             "quota",
                                             probs = quants)

  # quota_frac ----------------------------------------------------------------
  out$quota_frac <- map2(list(out$quota_tot), list(out$catch_plot), ~{
    class(.x$year) <- class(.y$year)
    class(.x$run) <- class(.y$run)
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(quota_frac = quota / catch) %>%
      select(year, quota_frac, run)
  }) %>% map_df(~{.x})

  # yrs_quota_met -------------------------------------------------------------
  out$yrs_quota_met <- out$ssb_plot %>%
    group_by(run) %>%
    summarize(value = length(which(ssb > 0.1 && ssb <= 0.4)) / simyears)

  # catch_area ----------------------------------------------------------------
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

  # catch_us_tot --------------------------------------------------------------
  out$catch_us_tot <- out$catch_area %>%
    transmute(year, catch = us, run)

  # catch_ca_tot --------------------------------------------------------------
  out$catch_ca_tot <- out$catch_area %>%
    transmute(year, catch = ca, run)

  # vtac_us -------------------------------------------------------------------
  out$vtac_us <- map2(list(out$v_us_plot), list(out$catch_us_tot), ~{
    class(.x$year) <- class(.y$year)
    class(.x$run) <- class(.y$run)
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(v_tac = v / catch) %>%
      select(year, v_tac, run)
  }) %>%
    map_df(~{.x})

  # vtac_ca -------------------------------------------------------------------
  out$vtac_ca <- map2(list(out$v_us_plot), list(out$catch_ca_tot), ~{
    class(.x$year) <- class(.y$year)
    class(.x$run) <- class(.y$run)
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(v_tac = v / catch) %>%
      select(year, v_tac, run)
  }) %>%
    map_df(~{.x})

  # vtac_us_seas --------------------------------------------------------------
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

  # vtac_ca_seas --------------------------------------------------------------
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

  # vtac_seas -----------------------------------------------------------------
  out$vtac_seas <- bind_rows(out$vtac_ca_seas, out$vtac_us_seas)

  # aav_plot ------------------------------------------------------------------
  out$aav_plot <- map2(list(out$catch_for_aav), seq_along(list(out$catch_for_aav)), ~{
    .x %>%
      group_by(run) %>%
      mutate(catch_lag = lag(catch, 1)) %>%
      mutate(aav = abs(catch_lag - catch) / catch_lag) %>%
      ungroup() %>%
      filter(!is.na(aav)) %>%
      select(year, aav, run)
  }) %>%
    map_df(~{.x}) %>%
    filter(year > sim_data[[1]]$m_yr) %>%
    group_by(run) %>%
    summarize(aav = median(aav))

  # v_ca_quant ----------------------------------------------------------------
  out$v_ca_quant <- calc_quantiles_by_group(out$v_ca_plot, grp_col = "year", col = "v", probs = quants)

  # v_us_quant ----------------------------------------------------------------
  out$v_us_quant <- calc_quantiles_by_group(out$v_us_plot, grp_col = "year", col = "v", probs = quants)

  # catch_quant ----------------------------------------------------------------
  out$catch_quant <- calc_quantiles_by_group(out$catch_plot, grp_col = "year", col = "catch", probs = quants)

  # quota_frac_quant ----------------------------------------------------------------
  out$quota_frac_quant <- calc_quantiles_by_group(out$quota_frac, grp_col = "year", col = "quota_frac", probs = quants)

  # ssb_10 --------------------------------------------------------------------
  # Probability of SSB < SSB_10%
  ssb_future <- out$ssb_plot %>%
    filter(year > min(short_term_yrs))
  out$ssb_10 <- ssb_future %>%
    group_by(run) %>%
    summarize(pcnt = length(which(ssb < 0.1)) /
                length(unique(year))) %>%
    group_map(~ calc_quantiles(.x, col = "pcnt", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  # ssb_4010 ------------------------------------------------------------------
  # Probability of SSB >= SSB_10% and <= SSB_40%
  out$ssb_4010 <- ssb_future %>%
    group_by(run) %>%
    summarize(pcnt = length(which(ssb >= 0.1 & ssb <= 0.4)) /
                length(unique(year))) %>%
    group_map(~ calc_quantiles(.x, col = "pcnt", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  # catch_short_term ----------------------------------------------------------
  out$catch_short_term <- calc_term_quantiles(out$catch_plot,
                                              grp_col = "run",
                                              col = "catch",
                                              min_yr = min(short_term_yrs),
                                              max_yr = long_term_yrs,
                                              probs = quants,
                                              mean_multiplier = catch_multiplier)

  # catch_long_term -----------------------------------------------------------
  out$catch_long_term <- calc_term_quantiles(out$catch_plot,
                                             grp_col = "run",
                                             col = "catch",
                                             min_yr = long_term_yrs + 1,
                                             probs = quants,
                                             mean_multiplier = catch_multiplier)

  # aav_short_term ------------------------------------------------------------
  # out$aav_short_term <- calc_term_quantiles(out$aav_plot,
  #                                           grp_col = "run",
  #                                           col = "aav",
  #                                           min_yr = min(short_term_yrs),
  #                                           max_yr = max(short_term_yrs),
  #                                           probs = quants)

  # v_ca_stat -----------------------------------------------------------------
  out$v_ca_stat <- calc_term_quantiles(out$v_ca_plot,
                                       grp_col = "run",
                                       col = "v",
                                       probs = quants)

  # v_us_stat -----------------------------------------------------------------
  out$v_us_stat <- calc_term_quantiles(out$v_us_plot,
                                       grp_col = "run",
                                       col = "v",
                                       probs = quants)

  # vtac_ca_stat --------------------------------------------------------------
  out$vtac_ca_stat <- out$vtac_ca %>%
    group_by(run) %>%
    summarise(prop = length(which(v_tac > (1 / 0.3))) / length(v_tac)) %>%
    group_map(~ calc_quantiles(.x, col = "prop", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  # vtac_us_stat --------------------------------------------------------------
  out$vtac_us_stat <- out$vtac_us %>%
    group_by(run) %>%
    summarise(prop = length(which(v_tac > 1)) / length(v_tac)) %>%
    group_map(~ calc_quantiles(.x, col = "prop", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  # vtac_ca_seas_stat ---------------------------------------------------------
  out$vtac_ca_seas_stat <- out$vtac_ca_seas %>%
    filter(year > long_term_yrs) %>%
    group_by(run) %>%
    summarise(avg_sp = mean(1 / v_tac_sp),
              avg_su = mean(1 / v_tac_su),
              avg_fa = mean(1 / v_tac_fa)) %>%
    summarise(med_sp = median(avg_sp),
              med_su = median(avg_su),
              med_fa = median(avg_fa))

  # vtac_us_seas_stat ---------------------------------------------------------
  out$vtac_us_seas_stat <- out$vtac_us_seas %>%
    filter(year > long_term_yrs) %>%
    group_by(run) %>%
    summarise(avg_sp = mean(1 / v_tac_sp),
              avg_su = mean(1 / v_tac_su),
              avg_fa = mean(1 / v_tac_fa)) %>%
    summarise(med_sp = median(avg_sp),
              med_su = median(avg_su),
              med_fa = median(avg_fa))

  # nclosed -------------------------------------------------------------------
  # Calculate the median number of closed years
  out$nclosed <- map_int(unique(ssb_future$run), ~{
    tmp <- ssb_future %>% filter(run == .x)
    length(which(tmp$ssb < 0.1))
  })

  # indicators ----------------------------------------------------------------
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
  indicator_key <- seq_along(indicator)

  # ssb_quant_by_run ----------------------------------------------------------
  tmp_ssb_by_run <- out$ssb_plot %>% filter(year > min(short_term_yrs))
  out$ssb_quant_by_run <- calc_quantiles_by_group(tmp_ssb_by_run, grp_col = "run",
                                                  col = "ssb", probs = quants)

  # catch_quant_by_run (short term catch) -------------------------------------
  tmp_catch_by_run <- out$catch_plot %>% filter(year > min(short_term_yrs), year <= long_term_yrs)
  out$catch_quant_by_run <- calc_quantiles_by_group(tmp_catch_by_run, grp_col = "run",
                                                    col = "catch", probs = quants)

  # catch_quant_by_run (long term catch) --------------------------------------
  tmp_catch_by_run <- out$catch_plot %>% filter(year > long_term_yrs - 2)
  tmp_catch_by_run <- calc_quantiles_by_group(tmp_catch_by_run, grp_col = "run",
                                              col = "catch", probs = quants)
  # info (indicator table) ----------------------------------------------------
  out$info <- map(1:nruns, ~{
    data.frame(
      indicator_key = indicator_key,
      indicator = indicator,
      value = c(
        round(length(which(ssb_future[ssb_future$run == .x,]$ssb <= 0.1)) /
                length(ssb_future[ssb_future$run == .x,]$ssb), digits = 2),
        round(length(which(ssb_future[ssb_future$run == .x,]$ssb > 0.1 & ssb_future[ssb_future$run == .x,]$ssb < 0.4)) /
                length(ssb_future[ssb_future$run == .x,]$ssb), digits = 2),
        round(length(which(ssb_future[ssb_future$run == .x,]$ssb > 0.4)) /
                length(ssb_future[ssb_future$run == .x,]$ssb), digits = 2),
        round(out$aav_plot[out$aav_plot$run == .x,]$aav, digits = 2),
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

  # info_quant (indicator table) ----------------------------------------------
  out$info_quant <- out$info %>%
    calc_quantiles_by_group(grp_col = "indicator_key",
                            col = "value",
                            grp_names = "indicator",
                            probs = quants)

  out$info <- out$info %>%
    select(-indicator_key) %>%
    mutate(indicator = as.factor(indicator))

  out
}
