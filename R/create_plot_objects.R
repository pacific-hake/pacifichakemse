#' Create and/or load RDS file contents from multiple MSE runs
#'
#' @param scenarios A vector of directory names inside the results directory with MSE results
#' @param overwrite_rds If TRUE, all RDS files will be re-created (takes a long time). If FALSE,
#' the files that exists will be loaded and returned
#' @param ...
#'
#' @return A list of length of `scenarios` containing MSE output data
#' @export
#' @importFrom tictoc tic toc
create_plot_objects <- function(scenarios = c("biasadjust",
                                              "climate",
                                              "hcr",
                                              "selectivity"),
                                overwrite_rds = TRUE,
                                ...){

  ps <- map(scenarios, function(scen = .x, ...){
    tic()
    lst <- load_mse_plot_data(scenario = scen, overwrite_rds = overwrite_rds, ...)
    if(overwrite_rds){
      cat(crayon::green(symbol$tick), green(" Created and loaded plot objects for scenario:", scen, "\n"))
    }else{
      cat(crayon::green(symbol$tick), green(" Loaded plot objects for scenario:", scen, "\n"))
    }
    toc()
    lst
  }, ...)
  names(ps) <- scenarios

  ps
}

