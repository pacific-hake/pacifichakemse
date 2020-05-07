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
#' @param ... Arguments to be passed to [hake_objectives()]
#'
#' @return A list of length 10: The items are (1) A [data.frame] containing the SSB/AAV/Catch
#' indicators, (2) A [data.frame] containing the country and season indicators, (3) A [data.frame]
#' containing the violin indicators (data in format for violin plots), (4) A [data.frame] of data
#' to be used to create violin plots, (5) A vector of colors, one for each file loaded (scenario),
#' (6) A vector of plot names, one for each scenario, (7) mse_out_data which is a list, one for
#' each scenario, and each containing a list of length 3, which is the output of [df_lists()],
#' (8) A list of data frames, which are the scenario-aggregated data frames from `mse_out_data[[N]][[3]]`,
#' (9) A list of length = number of scenarios, containing three-column [data.frame]s with `run`, `SE.SSB`,
#' and `year` as columns/ SE.SSB is the standard error between the OM and EM, (10) `sim_data` - output
#' from the function [run.agebased.true.catch()] (the operating model output)
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
                                   porder = NA,
                                   ...){
  stopifnot(!is.null(results_dir))

  fls <- dir(results_dir)
  fls <- fls[grep("\\.rds", fls)]
  if(!length(fls)){
    stop("There are no .rds files in the '", results_dir, "' directory",
         call. = FALSE)
  }
  fls <- fls[grep("MSE", fls)]
  fls <- map_chr(fls, ~{
    file.path(results_dir, .x)
  })
  ls_plots <- map(fls, ~{
    readRDS(.x)
  })

  # Use the plot names provided to the function. If those are `NULL`,
  # use the plot names set up when running the MSE scenarios. If any of those
  # are `NULL`, use the file name associated with the run
  if(is.null(plotnames[1])){
    plotnames <- map_chr(ls_plots, ~{
      if(is.null(attributes(.x)$plotname)){
        plotnames <- map_chr(fls, ~{
          gsub(".rds$", "", basename(.x))
        })
      }else{
        attributes(.x)$plotname
      }
    })
  }

  stopifnot(length(ls_plots) == length(plotnames))
  seasons_in_output <- as.numeric(attr(ls_plots[[1]][[1]]$Catch, "dimnames")$season)
  spaces_in_output <- as.numeric(attr(ls_plots[[1]][[1]]$Catch, "dimnames")$space)

  # Save these in future runs - calculates SSB0
  df <- load_data_seasons(nseason = length(seasons_in_output),
                          nspace = length(spaces_in_output))
  sim_data <- run.agebased.true.catch(df)
  names(ls_plots) <- plotnames

  if(all(is.na(porder))){
    porder <- 1:length(plotnames)
  }
  stopifnot(length(porder) == length(ls_plots))

  #cols <- brewer.pal(6, "Dark2")
  #cols <- LaCroixColoR::lacroix_palette("PassionFruit", n = 4, type = "discrete")
  cols <- pnw_palette("Starfish", n = length(plotnames), type = "discrete")
  lst_indicators <- map2(ls_plots, plotnames, function(.x, .y, ...){
    tmp <- hake_objectives(.x, sim_data$SSB0, ...)
    tmp$info <- tmp$info %>%
      mutate(HCR = .y)
    tmp$vtac_seas <- tmp$vtac_seas %>%
      mutate(HCR = .y)
    tmp
  }, ...)

  df_all_indicators <- map_df(lst_indicators, ~{
    .x$info
  })
  df_violin_indicators <- map_df(lst_indicators, ~{
    .x$vtac_seas
  }) %>%
    mutate(HCR = factor(HCR, levels = names(lst_indicators)[porder]))
  violin_names <- names(df_violin_indicators)
  violin_last_word <- str_extract(violin_names, "\\w+$")
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

  df_ssb_catch_indicators <- df_all_indicators %>%
    filter(indicator %in% indicators[1:cutoff_ind]) %>%
    mutate(HCR = factor(HCR, levels = names(lst_indicators)[porder]))

  df_country_season_indicators <- df_all_indicators %>%
    filter(indicator %in% indicators[(cutoff_ind + 1):length(indicators)]) %>%
    mutate(country = str_extract(indicator, "^\\w+")) %>%
    mutate(season = str_extract(indicator, "\\w+$")) %>%
    mutate(season = ifelse(season == "spr",
                           "Apr-Jun",
                           ifelse(season == "sum",
                                  "July-Sept",
                                  "Oct-Dec"))) %>%
    mutate(HCR = factor(HCR, levels = names(lst_indicators)[porder]))

  df_violin <- map_df(seq_along(ls_plots), ~{
    tmp <- hake_violin(ls_plots[[.x]],
                       sim_data$SSB0,
                       move = 1)
    tmp$HCR <- plotnames[.x]
    tmp
  }) %>%
    mutate(HCR = factor(HCR, levels = plotnames[porder]))

  mse_out_data <- map(ls_plots, ~{
    tmp <- df_lists(.x, max_yr = max(df$years))
  })

  # Bind rows of all list[[3]] data frames into single data frame, and order it by scenario
  # `lst` is a list of the outputs from [df_lists()], `val` is the name of the data frame
  merge_dfs_from_scenarios <- function(lst, val){
    map_df(lst, ~{
      .x[[3]][[val]]
    }) %>%
      mutate(run = factor(run, levels = plotnames[porder]))
  }
  mse_values_agg <- list(ssbplot = merge_dfs_from_scenarios(mse_out_data, "SSBplot"),
                         ssbmid = merge_dfs_from_scenarios(mse_out_data, "SSBmid"),
                         ssbtot = merge_dfs_from_scenarios(mse_out_data, "SSBtot"),
                         catchplot = merge_dfs_from_scenarios(mse_out_data, "Catchplot"),
                         amcplot = merge_dfs_from_scenarios(mse_out_data, "amcplot"),
                         amsplot = merge_dfs_from_scenarios(mse_out_data, "amsplot"),
                         amcspace = merge_dfs_from_scenarios(mse_out_data, "amc.space"),
                         amsspace = merge_dfs_from_scenarios(mse_out_data, "ams.space"),
                         f0 = merge_dfs_from_scenarios(mse_out_data, "F0"),
                         catchq = merge_dfs_from_scenarios(mse_out_data, "Catch.q"))

  # Standard error on SSB. First make a list of scenario data frames and reorder,
  # then bind those together into a data frame using [purrr::map_df()]
  standard_error_ssb <- map2(ls_plots, names(ls_plots), ~{
    calc_standard_error_ssb(.x) %>%
      mutate(scenario = .y) %>%
      select(scenario, run, year, SE.SSB)
  })
  standard_error_ssb <- map_df(standard_error_ssb, ~{
    .x
  }) %>%
    mutate(scenario = factor(scenario, levels = plotnames[porder]))

  list(ssb_catch_indicators = df_ssb_catch_indicators,
       country_season_indicators = df_country_season_indicators,
       violin_indicators = df_violin_indicators,
       violin_data = df_violin,
       cols = cols,
       plotnames = plotnames,
       mse_out_data = mse_out_data,
       mse_values_agg = mse_values_agg,
       standard_error_ssb = standard_error_ssb,
       sim_data = sim_data)
}