#' Setup MSE plot data frames and colors
#'
#' @details Reads in the `.rds` files found in the `results_dir` directory and
#' creates two indicator [data.frame]s and colors for plotting
#'
#' @param results_dir A directory name where results from [run_mses()] resides. Only files
#' in this directory that end in '.rds' and contain 'MSE' will be loaded
#' @param plotnames Names for the plots
#' @param plotexp See [fn_plot_MSE()]
#' @param pidx See [fn_plot_MSE()]
#'
#' @return A list of length 4: The items are (1) A [data.frame] containing the SSB/AAV/Catch
#' indicators, (2) A [data.frame] containing the country and season indicators, (3) A [data.frame]
#' containing the violin indicators (data in format for violin plots), (4) A [data.frame] of data
#' to be used to create violin plots, (5) A vector of colors, one for each file loaded (scenario),
#' (6) A vector of plot names, one for each scenario, and (7) sim_data, which is the output of
#' running [run.agebased.true.catch()]
#' @importFrom dplyr filter summarise summarize group_by select %>%
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
                                   plotexp = FALSE,
                                   pidx = NA){
  stopifnot(!is.null(results_dir))

  fls <- dir(results_dir)
  fls <- fls[grep("\\.rds", fls)]
  fls <- fls[grep("MSE", fls)]
  fls <- map_chr(fls, ~{
    file.path(results_dir, .x)
  })
  ls_plots <- map(fls, ~{
    readRDS(.x)
  })
  if(is.null(plotnames[1])){
    plotnames <- map_chr(fls, ~{
      gsub(".rds$", "", basename(.x))
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

  if(all(is.na(pidx))){
    pidx <- 1:length(plotnames)
  }

  #cols <- brewer.pal(6, "Dark2")
  cols <- pnw_palette("Starfish", n = 4, type = "discrete")
  #cols <- LaCroixColoR::lacroix_palette("PassionFruit", n = 4, type = "discrete")

  cols <- cols[1:(length(plotnames))]
  lst_indicators <- map2(ls_plots, plotnames, ~{
    tmp <- hake_objectives(.x, sim_data$SSB0, move = 1)
    tmp[[2]]$HCR <- .y
    tmp[[3]]$HCR <- .y
    tmp
  })

  df_all_indicators <- map_df(lst_indicators, ~{
    .x[[2]]
  })
  df_violin_indicators <- map_df(lst_indicators, ~{
    .x[[3]]
  })
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
  cutoff_ind <- grep("long term", indicators)
  df_ssb_catch_indicators <- df_all_indicators %>%
    filter(indicator %in% indicators[1:cutoff_ind]) %>%
    mutate(HCR = factor(HCR, levels = names(lst_indicators)[pidx]))

  df_country_season_indicators <- df_all_indicators %>%
    filter(indicator %in% indicators[(cutoff_ind + 1):length(indicators)]) %>%
    mutate(country = str_extract(indicator, "^\\w+")) %>%
    mutate(season = str_extract(indicator, "\\w+$")) %>%
    mutate(season = ifelse(season == "spr",
                           "Apr-Jun",
                           ifelse(season == "sum",
                                  "July-Sept",
                                  "Oct-Dec")))

  df_violin <- map_df(seq_along(ls_plots), ~{
    tmp <- hake_violin(ls_plots[[.x]],
                       sim_data$SSB0,
                       move = 1)
    tmp$HCR <- plotnames[.x]
    tmp
  }) %>%
    mutate(HCR = factor(HCR, levels = plotnames[pidx]))
browser()
  list(ssb_catch_indicators = df_ssb_catch_indicators,
       country_season_indicators = df_country_season_indicators,
       violin_indicators = df_violin_indicators,
       violin_data = df_violin,
       cols = cols,
       plotnames = plotnames,
       sim_data = sim_data)
}