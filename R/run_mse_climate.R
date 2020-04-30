#' Run the MSE for the Climate scenarios
#'
#' @details Saves the output of the MSE runs to the files specified in `fns`
#'
#' @param ss_extdata_dir A directory in the PacifichakeMSE package extdata directory
#' which contains SS3 model run output
#' @param nruns Then number of runs to do for each simulation
#' @param simyears The number of years to simulate into the future
#' @param fns A vector of file names for the scenarios (.rds files)
#' @param tacs A vector of TAC values to be passed to the [run_multiple_MSEs()] function, in the same
#' order as the `fns` file names
#' @param cincreases A vector of values to be passed to the [run_multiple_MSEs()] function, in the same
#' order as the `fns` file names
#' @param mincreases A vector of values to be passed to the [run_multiple_MSEs()] function, in the same
#' order as the `fns` file names
#' @param om_params_seed A seed value to use when calling the [run.agebased.true.catch()] function
#' @param results_root_dir The name of the results root directory (relative to [here::here()])
#' @param results_dir The name of the results directory (relative to [here::here(results_root_dir)])
#' @param ... Arguments passed to [load_data_seasons()]
#'
#' @return
#' @importFrom purrr map_chr
#' @importFrom stringr str_ends
#' @export
run_mse_climate <- function(ss_extdata_dir = NULL,
                            nruns = 10,
                            simyears = 30,
                            fns = NULL,
                            tacs = NULL,
                            cincreases = NULL,
                            mincreases = NULL,
                            om_params_seed = 12345,
                            results_root_dir = "results",
                            results_dir = "climate",
                            ...){
  stopifnot(!is.null(ss_extdata_dir))
  stopifnot(!is.null(fns))
  stopifnot(!is.null(tacs))
  stopifnot(!is.null(cincreases))
  stopifnot(!is.null(mincreases))
  stopifnot(length(fns) == length(tacs))
  stopifnot(length(tacs) == length(cincreases))
  stopifnot(length(mincreases) == length(cincreases))

  # Check file names and append .rds if necessary
  fns <- map_chr(fns, ~{
    ifelse(str_ends(.x, pattern = "\\.rds"), .x, paste0(.x, ".rds"))
  })

  if(!dir.exists(here(results_root_dir))){
    dir.create(here(results_root_dir))
  }
  if(!dir.exists(here(results_root_dir, results_dir))){
    dir.create(here(results_root_dir, results_dir))
  }

  mod <- SS_output(system.file(file.path("extdata", ss_extdata_dir),
                               package = "PacifichakeMSE",
                               mustWork = TRUE),
                   printstats = FALSE,
                   verbose = FALSE)

  # Prepare data for operating model
  df <- load_data_seasons(...)

  # Load parameters from assessment
  parms.true <- getParameters_OM(TRUE, mod, df)

  # Run the operating model until last_yr
  sim.data <- run.agebased.true.catch(df, om_params_seed)

  seeds <- floor(runif(n = nruns, min = 1, max = 1e6))
  tic()
  map2(fns, 1:length(fns), ~{
    ls_save <- map(1:nruns, function(run = .x, ...){
      tmp <- run_multiple_MSEs(simyears = simyears,
                               seeds = seeds[run],
                               TAC = tacs[.y],
                               df = df,
                               cincrease = cincreases[.y],
                               mincrease = mincreases[.y])
      if(is.list(tmp)) tmp else NA
    }, ...)
    saveRDS(ls_save, file = here(results_root_dir, results_dir, .x))
  })
  toc()
}