#' Load an mse plot object RDS file which holds the output of [setup_mse_plot_objects()]
#'
#' @details If the file exists and `overwrite_rds` is `FALSE`, the file is read in and the
#' content object is returned. If the file does not exist, it is created prior to the results
#' being returned.
#'
#' @param scenario The name of the scenario. This is the name of a subdirectory found in
#' the 'results' directory of the project.
#' @param overwrite_rds Logical. If `TRUE` and the file exists, it will be overwritten. If `FALSE`,
#' the file will be created if it does not exist and left as-is if it exists.
#' @param main_results_dir The name of the results directory which holds the mse output subdirectories
#' in the project.
#' @param ... Arguments to send to [hake_objectives()]
#'
#' @return The object which was returned by [setup_mse_plot_objects()]
#' @export
load_mse_plot_data <- function(scenario = NULL,
                               overwrite_rds = FALSE,
                               main_results_dir = "results",
                               ...){

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
    ps <- setup_mse_plot_objects(results_dir = results_dir, ...)
    saveRDS(ps, file = po_filename)
  }

  readRDS(po_filename)
}