#' Run the MSE for the Harvest Control Rule (HCR) scenarios
#'
#' @details Saves the output of the MSE runs to the files specified in `fns`
#'
#' @param ss_extdata_dir A directory in the PacifichakeMSE package extdata directory
#' which contains SS3 model run output
#' @param nruns Then number of runs to do for each simulation
#' @param simyears The number of years to simulate into the future
#' @param fns A vector of file names for the scenarios (.rds files)
#' @param om_params_seed A seed value to use when calling the [run.agebased.true.catch()] function
#' @param results_root_dir The name of the results root directory (relative to [here::here()])
#' @param results_dir The name of the results directory (relative to [here::here(results_root_dir)])
#' @param ... Arguments passed to [load_data_seasons()]
#'
#' @return Nothing
#' @importFrom here here
#' @importFrom purrr map2 map
#' @importFrom r4ss SS_output
#' @importFrom tictoc tic toc
#' @export
run_mse_hcr <- function(ss_extdata_dir = NULL,
                        nruns = 10,
                        simyears = 30,
                        fns = NULL,
                        om_params_seed = 12345,
                        results_root_dir = "results",
                        results_dir = "hcr",
                        ...){
  stopifnot(!is.null(ss_extdata_dir))
  stopifnot(!is.null(fns))

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

  tacs <- 1:length(fns)
  seeds <- floor(runif(n = nruns, min = 1, max = 1e6))
  tic()
  map2(fns, tacs, ~{
    ls_save <- map(1:nruns, function(run = .x, ...){
      tmp <- run_multiple_MSEs(simyears = simyears,
                               seeds = seeds[run],
                               TAC = .y,
                               df = df)
      if(is.list(tmp)) tmp else NA
    }, ...)
    saveRDS(ls_save, file = here(results_root_dir, results_dir, .x))
  })
  toc()
}