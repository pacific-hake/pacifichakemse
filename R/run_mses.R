#' Run the MSE for a number of runs for any scenarios
#'
#' @details Saves the output of the MSE runs to the files specified in `fns`, in the directories
#' specified in `results_root_dir` and `results_dir`
#'
#' @param ss_extdata_dir A directory in the PacifichakeMSE package extdata directory
#' which contains SS3 model run output
#' @param nruns Then number of runs to do for each simulation
#' @param simyears The number of years to simulate into the future
#' @param fns A vector of file names for the scenarios (.rds files). .rds extension is optional
#' @param plotnames A vector of strings to use for the scenarios later when plotting. Must either be
#' `NULL` or the same length as `fns`
#' @param tacs A vector of TAC values to be passed to the [run_multiple_MSEs()] function, in the same
#' order as the `fns` file names, or a single value
#' @param cincreases A vector of values to be passed to the [run_multiple_MSEs()] function, in the same
#' order as the `fns` file names, or a single value
#' @param mincreases A vector of values to be passed to the [run_multiple_MSEs()] function, in the same
#' order as the `fns` file names, or a single value
#' @param sel_changes A vector of values to be passed to the [run_multiple_MSEs()] function, in the same
#' order as the `fns` file names, or a single value
#' @param nsurveys The number of surveys for each run. This must be a vector of the same length as `fns` or `NULL`.
#' If `NULL`, nothing will be changed
#' @param multiple_season_data A list of the same length as `fns`, with each element being a vector of
#' three items, `nseason`, `nspace`, and `bfuture`. If NULL, biasadjustment will not be incorporated
#' @param om_params_seed A seed value to use when calling the [run.agebased.true.catch()] function
#' @param results_root_dir The results root directory
#' @param results_dir The results directory
#' @param ... Arguments passed to [load_data_seasons()]
#'
#' @return Nothing
#' @importFrom here here
#' @importFrom purrr map2 map map_chr
#' @importFrom r4ss SS_output
#' @importFrom stringr str_ends
#' @importFrom tictoc tic toc
#' @export
run_mses <- function(ss_extdata_dir = NULL,
                     nruns = 10,
                     simyears = NULL,
                     fns = NULL,
                     plotnames = NULL,
                     tacs = 1,
                     cincreases = 0,
                     mincreases = 0,
                     sel_changes = 0,
                     nsurveys = NULL,
                     multiple_season_data = NULL,
                     om_params_seed = 12345,
                     results_root_dir = here("results"),
                     results_dir = here("results", "default"),
                     ...){

  stopifnot(!is.null(ss_extdata_dir))
  stopifnot(length(ss_extdata_dir) == 1)
  stopifnot(class(ss_extdata_dir) == "character")
  stopifnot(!is.null(fns))
  stopifnot(is.null(plotnames) | length(fns) == length(plotnames))
  stopifnot(!is.null(tacs))
  stopifnot(!is.null(cincreases))
  stopifnot(!is.null(mincreases))
  stopifnot(!is.null(sel_changes))
  stopifnot(is.null(nsurveys) | length(nsurveys) == length(fns))
  stopifnot(is.null(multiple_season_data) | length(multiple_season_data) == length(fns))
  stopifnot(length(tacs) == 1 | length(tacs) == length(fns))
  stopifnot(length(cincreases) == 1 | length(cincreases) == length(fns))
  stopifnot(length(mincreases) == 1 | length(mincreases) == length(fns))
  stopifnot(length(sel_changes) == 1 | length(sel_changes) == length(fns))

  # Check file names and append .rds if necessary
  fns <- map_chr(fns, ~{
    ifelse(str_ends(.x, pattern = "\\.rds"), .x, paste0(.x, ".rds"))
  })

  if(!dir.exists(results_root_dir)){
    dir.create(results_root_dir)
  }
  if(!dir.exists(results_dir)){
    dir.create(results_dir)
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
  map2(fns, 1:length(fns), function(.x, .y, ...){
    ls_save <- map(1:nruns, function(run = .x, ...){
      if(length(sel_changes) != 1 || sel_changes != 0){
        df <- load_data_seasons(...,
                                selectivity_change = ifelse(length(sel_changes) == 1,
                                                            sel_changes,
                                                            sel_changes[.y]))
      }
      if(!is.null(nsurveys)){
        df$nsurvey <- nsurveys[.y]
      }
      if(is.null(multiple_season_data)){
        tmp <- run_multiple_MSEs(
          simyears = simyears,
          seeds = seeds[run],
          df = df,
          TAC = if(length(tacs) == 1) tacs else tacs[.y],
          cincrease = ifelse(length(cincreases) == 1, cincreases, cincreases[.y]),
          mincrease = ifelse(length(mincreases) == 1, mincreases, mincreases[.y]))
      }else{
        dfs <- map(multiple_season_data, ~{
          do.call(load_data_seasons, as.list(.x))
        })
        tmp <- run_multiple_OMs(simyears = simyears,
                                seed = seeds[run],
                                df = dfs[[.y]],
                                Catchin = 0)
      }
      if(is.list(tmp)) tmp else NA
    }, ...)
    ls_save$plotname <- plotnames[.y]
    saveRDS(ls_save, file = file.path(results_dir, .x))
  }, ...)
  toc()
}
