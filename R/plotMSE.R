#' Plot MSE results (TODO: Improve docs on this function)
#'
#' @param results_dir A directory name where results from [run_mses()] resides. Only files
#' in this directory that end in '.rds' and contain 'MSE' will be loaded
#' @param figures_root_dir The root directory for figures. Scenario figure types will be
#' subdirectories called `figures_dir`.
#' @param figures_dir The directory to place the figures in.
#' @param plotnames Names for the plots
#' @param plotexp See [fn_plot_MSE()]
#' @param pidx See [fn_plot_MSE()]
#'
#' @return Nothing
#' @export
plotMSE <- function(results_dir = NULL,
                    figures_root_dir = here("figures"),
                    figures_dir = NULL,
                    plotnames = NULL,
                    plotexp = FALSE,
                    pidx = NA){
  stopifnot(!is.null(results_dir))
  stopifnot(!is.null(figures_root_dir))
  stopifnot(!is.null(figures_dir))

  if(!dir.exists(figures_root_dir)){
    dir.create(figures_root_dir)
  }
  if(!dir.exists(figures_dir)){
    dir.create(figures_dir)
  }

  fls <- dir(results_dir)
  ls.plots <- list()
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
  fn_plot_MSE(ls_plots,
              sim_data,
              plotfolder = figures_dir,
              plotexp = plotexp,
              pidx = pidx)
}