#' Hake objectives (TODO: Improve docs on this function)
#'
#' @param lst list of MSE results
#' @param ssb0 unfished biomass from OM
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
                            ssb0 = NULL,
                            short_term_yrs = 2018:2027,
                            long_term_yrs = 2027,
                            can.prop = 0.2488,
                            us.prop = 0.7612,
                            quants = c(0.05, 0.25, 0.5, 0.75, 0.95),
                            catch_multiplier = 1e-6){
  stopifnot(!is.null(lst))
  stopifnot(!is.null(ssb0))

  yrs <- as.numeric(attributes(lst[[1]]$Catch)$dimnames$year)
  min_yr <- min(yrs)
  nyrs <- length(yrs)
  if(nyrs == 1){
    nyrs <- nrow(lst[[1]]$Catch)
  }
  simyears <- nyrs - (length(min_yr:short_term_yrs[1])) + 1
  nruns <- length(lst)

  ssb_plot <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               ssb = rowSums(.x$SSB) / sum(ssb0),
               run = .y)
  }) %>% map_df(~{.x})

  # Vulnerable biomass at mid-year start of season 3 for each country
  v_ca_plot <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               v = .x$V[,1,3],
               run = .y)
  }) %>% map_df(~{.x})

  v_us_plot <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               v = .x$V[,2,3],
               run = .y)
  }) %>% map_df(~{.x})

  catch_plot <- map2(lst, seq_along(lst), ~{
    ct <- apply(.x$Catch, MARGIN = 2, FUN = sum)
    if(length(ct) == 1){
      data.frame(year = yrs,
                 catch = .x$Catch,
                 run = .y)
    }else{
      data.frame(year = yrs,
                 catch = ct,
                 run = .y)
    }
  }) %>% map_df(~{.x})

  quota_tot <- map2(lst, seq_along(lst), ~{
    quot <- apply(.x$Catch.quota, MARGIN = 1, FUN = sum)
    if(length(quot) == 1){
      data.frame(year = yrs,
                 catch = .x$Catch.quota,
                 run = .y)
    }else{
      data.frame(year = yrs,
                 catch = quot,
                 run = .y)
    }
  }) %>% map_df(~{.x})

  quota_plot <- map2(list(quota_tot), list(catch_plot), ~{
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(quota_frac = catch.x / catch.y) %>%
      select(year, quota_frac, run)
  }) %>% map_df(~{.x})

  quota_us_tot <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               quota = rowSums(.x$Catch.quota[,2,]),
               run = .y)
  }) %>% map_df(~{.x})

  quota_ca_tot <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               quota = rowSums(.x$Catch.quota[,1,]),
               run = .y)
  }) %>% map_df(~{.x})

  catch_area <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               area = apply(.x$Catch, MARGIN = c(2, 3), FUN = sum),
               run = .y)
  }) %>% map_df(~{.x}) %>%
    transmute(year,
              ca = area.1,
              us = area.2,
              run)
  catch_us_tot <- catch_area %>%
    transmute(year, catch = us, run)
  catch_ca_tot <- catch_area %>%
    transmute(year, catch = ca, run)

  vtac_us <- map2(list(v_us_plot), list(catch_us_tot), ~{
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(v_tac = v / catch) %>%
      select(year, v_tac, run)
  }) %>% map_df(~{.x})

  vtac_ca <- map2(list(v_us_plot), list(catch_ca_tot), ~{
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(v_tac = v / catch) %>%
      select(year, v_tac, run)
  }) %>% map_df(~{.x})

  vtac_us_seas <- map2(lst, seq_along(lst), ~{
    ctmp <- colSums(.x$Catch)
    data.frame(year = yrs,
               v_tac_sp = ctmp[, 2, 2] / .x$V[, 2, 2],
               v_tac_su = ctmp[, 2, 3] / .x$V[, 2, 3],
               v_tac_fa = ctmp[, 2, 4] / .x$V[, 2, 4],
               run = .y)
  }) %>% map_df(~{.x}) %>%
    mutate(country = "US")

  vtac_ca_seas <- map2(lst, seq_along(lst), ~{
    ctmp <- colSums(.x$Catch)
    data.frame(year = yrs,
               v_tac_sp = ctmp[, 1, 2] / .x$V[, 1, 2],
               v_tac_su = ctmp[, 1, 3] / .x$V[, 1, 3],
               v_tac_fa = ctmp[, 1, 4] / .x$V[, 1, 4],
               run = .y)
  }) %>% map_df(~{.x}) %>%
    mutate(country = "Canada")

  vtac_seas <- bind_rows(vtac_ca_seas, vtac_us_seas)

  aav_plot <- map2(list(catch_plot), seq_along(list(catch_plot)), ~{
    .x %>%
      group_by(run) %>%
      mutate(catch_lag = lag(catch, 1)) %>%
      mutate(aav = abs(catch - catch_lag) / n()) %>%
      ungroup() %>%
      filter(!is.na(aav)) %>%
      select(year, aav, run)
  }) %>% map_df(~{.x})

  # Calculate quantiles
  ssb_yrs <- sort(unique(ssb_plot$year))
  ssb_plotquant <- ssb_plot %>%
    group_by(year) %>%
    group_map(~ calc_quantiles(.x, col = "ssb", probs = quants)) %>%
    map_df(~{.x}) %>%
    mutate(year = ssb_yrs) %>%
    select(year, everything())

  v_ca_yrs <- sort(unique(v_ca_plot$year))
  v_ca_plotquant <- v_ca_plot %>%
    group_by(year) %>%
    group_map(~ calc_quantiles(.x, col = "v", probs = quants)) %>%
    map_df(~{.x}) %>%
    mutate(year = v_ca_yrs) %>%
    select(year, everything())

  v_us_yrs <- sort(unique(v_us_plot$year))
  v_us_plotquant <- v_us_plot %>%
    group_by(year) %>%
    group_map(~ calc_quantiles(.x, col = "v", probs = quants)) %>%
    map_df(~{.x}) %>%
    mutate(year = v_us_yrs) %>%
    select(year, everything())

  catch_yrs <- sort(unique(catch_plot$year))
  catch_plotquant <- catch_plot %>%
    group_by(year) %>%
    group_map(~ calc_quantiles(.x, col = "catch", probs = quants)) %>%
    map_df(~{.x}) %>%
    mutate(year = catch_yrs) %>%
    select(year, everything()) %>%
    # Multiply all columns except `year` by catch_multiplier
    mutate_at(vars(-year), ~{.x * catch_multiplier})

  aav_yrs <- sort(unique(aav_plot$year))
  aav_plotquant <- aav_plot %>%
    group_by(year) %>%
    group_map(~ calc_quantiles(.x, col = "aav", probs = quants)) %>%
    map_df(~{.x}) %>%
    mutate(year = aav_yrs) %>%
    select(year, everything())

  quota_yrs <- sort(unique(quota_plot$year))
  quota_plotquant <- quota_plot %>%
    group_by(year) %>%
    group_map(~ calc_quantiles(.x, col = "quota_frac", probs = quants)) %>%
    map_df(~{.x}) %>%
    mutate(year = quota_yrs) %>%
    select(year, everything())

  ssb_future <- ssb_plot %>%
    filter(year > min(short_term_yrs))
  # Probability of SSB < SSB_10%
  ssb_10 <- ssb_future %>%
    group_by(run) %>%
    summarize(pcnt = length(which(ssb < 0.1)) /
                length(unique(year))) %>%
    group_map(~ calc_quantiles(.x, col = "pcnt", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  # Probability of SSB >= SSB_10% and <= SSB_40%
  ssb_4010 <- ssb_future %>%
    group_by(run) %>%
    summarize(pcnt = length(which(ssb >= 0.1 & ssb <= 0.4)) /
                length(unique(year))) %>%
    group_map(~ calc_quantiles(.x, col = "pcnt", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  catch_short_term <- calc_term_quantiles(catch_plot,
                                          grp_col = "run",
                                          col = "catch",
                                          min_yr = min(short_term_yrs),
                                          max_yr = long_term_yrs,
                                          probs = quants,
                                          mean_multiplier = catch_multiplier)

  catch_long_term <- calc_term_quantiles(catch_plot,
                                         grp_col = "run",
                                         col = "catch",
                                         min_yr = long_term_yrs + 1,
                                         probs = quants,
                                         mean_multiplier = catch_multiplier)


  aav_short_term <- calc_term_quantiles(aav_plot,
                                        grp_col = "run",
                                        col = "aav",
                                        min_yr = min(short_term_yrs),
                                        max_yr = long_term_yrs,
                                        probs = quants)

  v_ca_stat <- calc_term_quantiles(v_ca_plot,
                                   grp_col = "run",
                                   col = "v",
                                   probs = quants)

  v_us_stat <- calc_term_quantiles(v_us_plot,
                                   grp_col = "run",
                                   col = "v",
                                   probs = quants)

  vtac_ca_stat <- vtac_ca %>%
    group_by(run) %>%
    summarise(prop = length(which(v_tac > (1 / 0.3))) / length(v_tac)) %>%
    group_map(~ calc_quantiles(.x, col = "prop", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  vtac_us_stat <- vtac_us %>%
    group_by(run) %>%
    summarise(prop = length(which(v_tac > 1)) / length(v_tac)) %>%
    group_map(~ calc_quantiles(.x, col = "prop", probs = quants)) %>%
    map_df(~{.x}) %>%
    ungroup()

  vtac_ca_seas_stat <- vtac_ca_seas %>%
    filter(year > long_term_yrs) %>%
    group_by(run) %>%
    summarise(avg_sp = mean(1 / v_tac_sp),
              avg_su = mean(1 / v_tac_su),
              avg_fa = mean(1 / v_tac_fa)) %>%
    summarise(med_sp = median(avg_sp),
              med_su = median(avg_su),
              med_fa = median(avg_fa))

  vtac_us_seas_stat <- vtac_us_seas %>%
    filter(year > long_term_yrs) %>%
    group_by(run) %>%
    summarise(avg_sp = mean(1 / v_tac_sp),
              avg_su = mean(1 / v_tac_su),
              avg_fa = mean(1 / v_tac_fa)) %>%
    summarise(med_sp = median(avg_sp),
              med_su = median(avg_su),
              med_fa = median(avg_fa))

  # Calculate the median number of closed years
  nclosed <- map_int(unique(ssb_future$run), ~{
    tmp <- ssb_future %>% filter(run == .x)
    length(which(tmp$SSB < 0.1))
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
  info <- data.frame(
    indicator = as.factor(indicator),
    value = c(
      round(length(which(ssb_future$ssb <= 0.1)) / length(ssb_future$ssb), digits = 2),
      round(length(which(ssb_future$ssb>0.1 & ssb_future$ssb<0.4)) / length(ssb_future$ssb), digits = 2),
      round(length(which(ssb_future$ssb>0.4)) / length(ssb_future$ssb), digits = 2),
      round(median(aav_plotquant$`0.5`), digits = 2),
      median(ssb_plotquant$`0.5`[ssb_plotquant$year > min(short_term_yrs)]),
      median(1e6 * catch_plotquant$`0.5`[catch_plotquant$year > min(short_term_yrs) &
                                         catch_plotquant$year <= long_term_yrs]) * catch_multiplier,
      median(1e6 * catch_plotquant$`0.5`[catch_plotquant$year > long_term_yrs - 2]) * catch_multiplier,
      vtac_ca_seas_stat$med_sp,
      vtac_ca_seas_stat$med_su,
      vtac_ca_seas_stat$med_fa,
      vtac_us_seas_stat$med_sp,
      vtac_us_seas_stat$med_su,
      vtac_us_seas_stat$med_fa))

  list(ssb_plot = ssb_plot,
       ssb_plotquant = ssb_plotquant,
       v_ca_plot = v_ca_plot,
       v_ca_plotquant = v_ca_plotquant,
       v_ca_stat = v_ca_stat,
       v_us_plot = v_us_plot,
       v_us_plotquant = v_us_plotquant,
       v_us_stat = v_us_stat,
       catch_plot = catch_plot,
       catch_plotquant = catch_plotquant,
       catch_short_term = catch_short_term,
       catch_long_term = catch_long_term,
       catch_area = catch_area,
       quota_tot = quota_tot,
       quota_frac = quota_plot,
       quota_fracquant = quota_plotquant,
       quota_ca_tot = quota_ca_tot,
       quota_us_tot = quota_us_tot,
       vtac_ca = vtac_ca,
       vtac_us = vtac_us,
       vtac_ca_stat = vtac_ca_stat,
       vtac_us_stat = vtac_us_stat,
       vtac_ca_seas = vtac_ca_seas,
       vtac_us_seas = vtac_us_seas,
       vtac_seas = vtac_seas,
       vtac_ca_seas_stat = vtac_ca_seas_stat,
       vtac_us_seas_stat = vtac_us_seas_stat,
       aav_plot = aav_plot,
       aav_plotquant = aav_plotquant,
       aav_short_term = aav_short_term,
       ssb_10 = ssb_10,
       ssb_4010 = ssb_4010,
       nclosed = nclosed,
       info = info)
}
