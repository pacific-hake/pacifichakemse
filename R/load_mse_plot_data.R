#' Load an mse plot object RDS file which holds the output of [setup_mse_plot_objects()]
#'
#' @details If the file exists and `overwrite_rds` is `FALSE`, the file is read in and the
#' content object is returned. If the file does not exist, it is created prior to the results
#' being returned.
#'
#' @param scenario The name of the scenario. This is the name of a subdirectory found in
#' the  `main_results_dir` directory of the project.
#' @param overwrite_rds Logical. If `TRUE` and the file exists, it will be overwritten. If `FALSE`,
#' the file will be created if it does not exist and left as-is if it exists.
#' @param main_results_dir The name of the results directory which holds the mse output subdirectories
#' in the project.
#' @param quants Quantile values to use
#' @param ... Arguments to send to [merge_run_data()]
#'
#' @return The object which was returned by [setup_mse_plot_objects()]
#'
#' @export
load_mse_plot_data <- function(scenario = NULL,
                               overwrite_rds = FALSE,
                               main_results_dir = "results",
                               quants = c(0.05, 0.5, 0.95),
                               #quants = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975),
                               ...){

  if(!0.5 %in% quants){
    stop("You must include the median (0.5) in the quants vector", call. = FALSE)
  }
  tic()
  results_dir <- here(main_results_dir, scenario)
  if(!dir.exists(results_dir)){
    stop("Directory ", scenario, " does not exist.",
         call. = FALSE)
  }
  plot_rds_dir <- file.path(results_dir, "plot_rds")
  if(!dir.exists(plot_rds_dir)){
    dir.create(plot_rds_dir)
  }
  po_filename <- file.path(plot_rds_dir, paste0(scenario, ".rds"))
  if(file.exists(po_filename) && overwrite_rds){
    unlink(po_filename, force = TRUE)
  }
  if(!file.exists(po_filename)){
    ps <- setup_mse_plot_objects(results_dir = results_dir,
                                 main_results_dir = main_results_dir,
                                 quants = quants,
                                 ...)
    saveRDS(ps, file = po_filename, compress = FALSE)
  }

  toc()
  readRDS(po_filename)
}
