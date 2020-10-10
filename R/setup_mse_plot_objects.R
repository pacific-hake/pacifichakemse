#' Setup MSE plot data frames, colors, and plot names
#'
#' @details Reads in the `.rds` files found in the `results_dir` directory and
#' creates two indicator [data.frame]s and colors for plotting
#'
#' @param results_dir A directory name where results from [run_mses()] resides. Only files
#' in this directory that end in '.rds' and contain 'MSE' will be loaded
#' @param plotnames Names for the plots
#' @param porder Order of the scenarios in the figures. Default is the order they appear in the
#' `results_dir` directory. All outputs will be ordered in this way
#' @param ... Arguments to be passed to [hake_objectives()] and [calc_standard_error_ssb()]
#'
#' @return A list of length 10: The items are (1) A [data.frame] containing the SSB/AAV/Catch
#' indicators, (2) A [data.frame] containing the country and season indicators, (3) A [data.frame]
#' containing the violin indicators (data in format for violin plots), (4) A [data.frame] of data
#' to be used to create violin plots, (5) A vector of colors, one for each file loaded (scenario),
#' (6) A vector of plot names, one for each scenario, (7) lst_indicators which is a list, one for
#' each scenario, and each containing a list of length 3, which is the output of [hake_objectives()],
#' (8) A list of data frames, which are the scenario-aggregated data frames from `mse_out_data[[N]][[3]]`,
#' (9) A list of length = number of scenarios, containing three-column [data.frame]s with `run`, `SE.SSB`,
#' and `year` as columns/ SE.SSB is the standard error between the OM and EM, (10) `sim_data` - output
#' from the function [run_om()] (the operating model output)
#' @importFrom dplyr filter summarise summarize group_by select %>% mutate
#' @importFrom PNWColors pnw_palette
#' @importFrom ggplot2 geom_bar scale_x_discrete scale_y_continuous scale_fill_manual
#' @importFrom ggplot2 facet_wrap element_text position_dodge geom_violin geom_boxplot
#' @importFrom ggplot2 theme_classic coord_cartesian element_blank element_rect geom_hline
#' @importFrom ggplot2 scale_color_manual
#' @importFrom reshape2 melt
#' @importFrom cowplot plot_grid
#' @importFrom purrr map_df
#' @importFrom stringr str_extract
#' @export
setup_mse_plot_objects <- function(results_dir = NULL,
                                   plotnames = NULL,
                                   porder = NULL,
                                   ...){
  verify_argument(results_dir, "character", 1)

  fls <- dir(results_dir)
  fls <- fls[grep("\\.rds", fls)]
  if(!length(fls)){
    stop("There are no .rds files in the '", results_dir, "' directory",
         call. = FALSE)
  }
  fls <- map_chr(fls, ~{
    file.path(results_dir, .x)
  })
  mse_om_output <- map(fls, ~{
    readRDS(.x)
  })
  # The output of this (mse_output) is a list of lists of MSE output, each list element is a list
  # of the number of runs elements, and each of those sub-lists contains an entry for each simulated year
  # check this using str(mse_output, 3)
  mse_output <- map(mse_om_output, ~{
    map(.x, ~{
      .x[[1]]
    })
  })

  # Operating model output - check this using str(om_output, 2)
  om_output <- map(mse_om_output, ~{
    map(.x, ~{
      .x[[2]]
    })
  })

  #  Estimation model output -  check this using str(em_output, 3)
  em_output <- map(mse_om_output, ~{
    map(.x, ~{
      .x[[3]]
    })
  })

  # Use the plot names provided to the function. If those are NULL,
  # use the plot names set up when running the MSE scenarios. If any of those
  # are NULL, use the file name associated with the run
  if(is.null(plotnames[1])){
    plotnames <- map_chr(mse_om_output, ~{
      if(is.null(attributes(.x)$plotname)){
        plotnames <- map_chr(fls, ~{
          gsub(".rds$", "", basename(.x))
        })
      }else{
        attributes(.x)$plotname
      }
    })
  }

  stopifnot(length(mse_om_output) == length(plotnames))
  seasons_in_output <- map(mse_output, ~{
    map_dbl(.x, ~{
      .x[[1]]$n_season
    })
  })
  spaces_in_output <- map(mse_output, ~{
    map_dbl(.x, ~{
      .x[[1]]$n_space
    })
  })

  names(mse_output) <- plotnames

  if(is.null(porder[1])){
    porder <- 1:length(plotnames)
  }
  stopifnot(length(porder) == length(mse_output))

  #----------------------------------------------------------------------------
  #cols <- brewer.pal(6, "Dark2")
  #cols <- LaCroixColoR::lacroix_palette("PassionFruit", n = 4, type = "discrete")
  cols <- pnw_palette("Starfish", n = length(plotnames), type = "discrete")
  # To view structure and names of lst_indicators: str(lst_indicators, 1) and str(lst_indicators[[1]], 1)
  # To see objectives probability table for the first scenario: lst_indicators[[1]]$info
  lst_indicators <- map2(mse_output, om_output, function(.x, .y, ...){
    tmp <- hake_objectives(.x, .y, ...)
    tmp$info <- tmp$info %>%
      mutate(HCR = names(.x))
    tmp$vtac_seas <- tmp$vtac_seas %>%
      mutate(HCR = names(.x))
    tmp
  }, ...)

  #----------------------------------------------------------------------------
  df_all_indicators <- map2(lst_indicators, 1:length(lst_indicators), ~{
    .x$info %>%
      mutate(hcr = names(lst_indicators)[.y])
  }) %>%
    map_df(~{.x}) %>%
    mutate(hcr = factor(hcr, levels = names(lst_indicators)[porder]))

  #----------------------------------------------------------------------------
  df_violin_indicators <-  map2(lst_indicators, 1:length(lst_indicators), ~{
    .x$vtac_seas %>%
      mutate(hcr = names(lst_indicators)[.y])
  }) %>%
    map_df(~{.x}) %>%
    mutate(hcr = factor(hcr, levels = names(lst_indicators)[porder]))
  violin_names <- names(df_violin_indicators)
  violin_last_word <- gsub("v_tac_", "", violin_names)
  season_inds <- grep("sp|su|fa", violin_last_word)
  month_strings <- ifelse(violin_last_word == "sp",
                          "Apr-Jun",
                          ifelse(violin_last_word == "su",
                                 "July-Sept",
                                 "Oct-Dec"))
  violin_names[season_inds] <- month_strings[season_inds]
  names(df_violin_indicators) <- violin_names

  indicators <- unique(df_all_indicators$indicator) %>% as.character
  cutoff_ind <- grep("ong term", indicators)

  #----------------------------------------------------------------------------
  df_ssb_catch_indicators <- df_all_indicators %>%
    filter(indicator %in% indicators[1:cutoff_ind]) %>%
    mutate(hcr = factor(hcr, levels = names(lst_indicators)[porder]))

  #----------------------------------------------------------------------------
  df_country_season_indicators <- df_all_indicators %>%
    filter(indicator %in% indicators[(cutoff_ind + 1):length(indicators)]) %>%
    mutate(country = str_extract(indicator, "^\\w+")) %>%
    mutate(season = str_extract(indicator, "\\w+$")) %>%
    mutate(season = ifelse(season == "spr",
                           "Apr-Jun",
                           ifelse(season == "sum",
                                  "July-Sept",
                                  "Oct-Dec"))) %>%
    mutate(hcr = factor(hcr, levels = names(lst_indicators)[porder]))

  #----------------------------------------------------------------------------
  df_violin <- map2(lst_indicators, seq_along(lst_indicators), ~{
    tmp <- list(yrs_quota_met = .x$yrs_quota_met, # years between 0.1 and 0.4
                ssb_10 = .x$ssb_10,
                ssb_40 = .x$ssb_40,
                catch_short_term = .x$catch_short_term,
                catch_long_term = .x$catch_long_term,
                aav_plot = .x$aav_plot,
                catch_plot = .x$catch_plot,
                ssb_plot = .x$ssb_plot)
    tmp[[1]]$hcr <- plotnames[.y]
    tmp
  })

  #----------------------------------------------------------------------------
  # Bind rows of all list[[3]] data frames into single data frame, and order it by scenario
  # `lst` is a list of the outputs from [df_lists()], `val` is the name of the data frame
  merge_dfs_from_scenarios <- function(lst, val){
    map2(lst, seq_along(lst), ~{
      .x[[val]] %>%
        mutate(scenario = plotnames[.y])
    }) %>%
      map_df(~{.x}) %>%
      mutate(scenario = factor(scenario, levels = plotnames[porder]))
  }

  #----------------------------------------------------------------------------
  # List for holing all the quantiles
  mse_quants <- NULL

  #----------------------------------------------------------------------------
  # AMC countries combined
  mse_quants$amc_ca_quant <- merge_dfs_from_scenarios(lst_indicators, "amc_ca_quant")
  mse_quants$amc_us_quant <- merge_dfs_from_scenarios(lst_indicators, "amc_us_quant")
  amc_ca_tmp <- mse_quants$amc_ca_quant %>%
    mutate(country = "Canada")
  amc_us_tmp <- mse_quants$amc_us_quant %>%
    mutate(country = "US")
  mse_quants$amc_quant <- amc_ca_tmp %>%
    bind_rows(amc_us_tmp)

  #----------------------------------------------------------------------------
  # AMS countries combined
  mse_quants$ams_ca_quant <- merge_dfs_from_scenarios(lst_indicators, "ams_ca_quant")
  mse_quants$ams_us_quant <- merge_dfs_from_scenarios(lst_indicators, "ams_us_quant")
  ams_ca_tmp <- mse_quants$ams_ca_quant %>%
    mutate(country = "Canada")
  ams_us_tmp <- mse_quants$ams_us_quant %>%
    mutate(country = "US")
  mse_quants$ams_quant <- ams_ca_tmp %>%
    bind_rows(ams_us_tmp)

  #----------------------------------------------------------------------------
  # F0 countries combined
  mse_quants$f0_ca_quant <- merge_dfs_from_scenarios(lst_indicators, "f0_ca_quant")
  mse_quants$f0_us_quant <- merge_dfs_from_scenarios(lst_indicators, "f0_us_quant")
  f0_ca_tmp <- mse_quants$f0_ca_quant %>%
    mutate(country = "Canada")
  f0_us_tmp <- mse_quants$f0_us_quant %>%
    mutate(country = "US")
  mse_quants$f0_quant <- f0_ca_tmp %>%
    bind_rows(f0_us_tmp)

  #----------------------------------------------------------------------------
  # Catch quota countries combined
  mse_quants$quota_ca_quant <- merge_dfs_from_scenarios(lst_indicators, "quota_ca_quant")
  mse_quants$quota_us_quant <- merge_dfs_from_scenarios(lst_indicators, "quota_us_quant")
  quota_ca_tmp <- mse_quants$quota_ca_quant %>%
    mutate(country = "Canada")
  quota_us_tmp <- mse_quants$quota_us_quant %>%
    mutate(country = "US")
  mse_quants$quota_quant <- quota_ca_tmp %>%
    bind_rows(quota_us_tmp)

  #----------------------------------------------------------------------------
  # SSB countries combined
  mse_quants$ssb_ca_quant <- merge_dfs_from_scenarios(lst_indicators, "ssb_ca_quant")
  mse_quants$ssb_us_quant <- merge_dfs_from_scenarios(lst_indicators, "ssb_us_quant")
  ssb_ca_tmp <- mse_quants$ssb_ca_quant %>%
    mutate(country = "Canada")
  ssb_us_tmp <- mse_quants$ssb_us_quant %>%
    mutate(country = "US")
  mse_quants$ssb_quant <- ssb_ca_tmp %>%
    bind_rows(ssb_us_tmp)

  #----------------------------------------------------------------------------
  # SSB mid-year countries combined
  mse_quants$ssb_mid_ca_quant <- merge_dfs_from_scenarios(lst_indicators, "ssb_mid_ca_quant")
  mse_quants$ssb_mid_us_quant <- merge_dfs_from_scenarios(lst_indicators, "ssb_mid_us_quant")
  ssb_mid_ca_tmp <- mse_quants$ssb_mid_ca_quant %>%
    mutate(country = "Canada")
  ssb_mid_us_tmp <- mse_quants$ssb_mid_us_quant %>%
    mutate(country = "US")
  mse_quants$ssb_mid_quant <- ssb_mid_ca_tmp %>%
    bind_rows(ssb_mid_us_tmp)

  #----------------------------------------------------------------------------
  mse_quants$catch_quant <- merge_dfs_from_scenarios(lst_indicators, "catch_quant")

  # Standard error between the OM and EM (final year)
  standard_error_ssb <- map(seq_along(em_output), ~{
    calc_standard_error_ssb(em_output[[.x]], om_output[[.x]]) %>%
      calc_quantiles_by_group(grp_col = "year", col = "ssb_se", include_mean = FALSE) %>%
      mutate(scenario = plotnames[.x])
  }) %>%
    map_df(~{.x})

  list(ssb_catch_indicators = df_ssb_catch_indicators,
       country_season_indicators = df_country_season_indicators,
       violin_indicators = df_violin_indicators,
       violin_data = df_violin,
       cols = cols,
       plotnames = plotnames,
       lst_indicators = lst_indicators,
       mse_quants = mse_quants,
       standard_error_ssb = standard_error_ssb,
       sim_data = om_output)
}