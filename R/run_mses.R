#' Run the MSE for a number of runs for any scenarios
#'
#' @details Saves the output of the MSE runs to the files specified in `fns`, in the directories
#' specified in `results_root_dir` and `results_dir`
#'
#' @param ss_extdata_dir A directory in the PacifichakeMSE package extdata directory
#' which contains SS3 model run output
#' @param n_runs Then number of runs to do for each simulation
#' @param n_sim_yrs The number of years to simulate into the future
#' @param fns A vector of file names for the scenarios (.rds files). .rds extension is optional
#' @param plot_names A vector of strings to use for the scenarios later when plotting. Must either be
#' `NULL` or the same length as `fns`
#' @param tacs A vector of TAC values to be passed to the [run_multiple_MSEs()] function, in the same
#' order as the `fns` file names, or a single value
#' @param c_increases A vector of values to be passed to the [run_multiple_MSEs()] function, in the same
#' order as the `fns` file names, or a single value
#' @param m_increases A vector of values to be passed to the [run_multiple_MSEs()] function, in the same
#' order as the `fns` file names, or a single value
#' @param sel_changes A vector of values to be passed to the [run_multiple_MSEs()] function, in the same
#' order as the `fns` file names, or a single value
#' @param n_surveys The number of surveys for each run. This must be a vector of the same length as `fns` or `NULL`.
#' If `NULL`, 2 will be used for every scenario
#' @param multiple_season_data A list of the same length as `fns`, with each element being a vector of
#' three items, `nseason`, `nspace`, and `bfuture`. If NULL, biasadjustment will not be incorporated
#' @param om_params_seed A seed value to use when calling the [run_agebased_true_catch()] function
#' @param results_root_dir The results root directory
#' @param results_dir The results directory
#' @param ... Arguments passed to [load_data_seasons()]
#'
#' @return Nothing
#' @importFrom dplyr transmute group_map mutate_at quo
#' @importFrom here here
#' @importFrom purrr map2 map map_chr
#' @importFrom r4ss SS_output
#' @importFrom stringr str_ends
#' @importFrom tictoc tic toc
#' @export
run_mses <- function(ss_extdata_dir = NULL,
                     n_runs = 10,
                     n_sim_yrs = NULL,
                     fns = NULL,
                     plot_names = NULL,
                     tacs = 1,
                     c_increases = 0,
                     m_increases = 0,
                     sel_changes = 0,
                     n_surveys = NULL,
                     multiple_season_data = NULL,
                     om_params_seed = 12345,
                     results_root_dir = here("results"),
                     results_dir = here("results", "default"),
                     ...){

  verify_argument(ss_extdata_dir, "character", 1)
  verify_argument(fns, chk_len = length(plot_names))
  verify_argument(tacs, "numeric")
  verify_argument(c_increases, "numeric")
  verify_argument(m_increases, "numeric")
  verify_argument(sel_changes, "numeric")
  verify_argument(n_surveys, "numeric")

  stopifnot(length(tacs) == 1 | length(tacs) == length(fns))
  stopifnot(length(c_increases) == 1 | length(c_increases) == length(fns))
  stopifnot(length(m_increases) == 1 | length(m_increases) == length(fns))
  stopifnot(length(sel_changes) == 1 | length(sel_changes) == length(fns))
  stopifnot(is.null(n_surveys) | length(n_surveys) == length(fns))
  stopifnot(is.null(multiple_season_data) | length(multiple_season_data) == length(fns))

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

  # Run the operating model
  sim.data <- run_agebased_true_catch(df, om_params_seed)

  seeds <- floor(runif(n = n_runs, min = 1, max = 1e6))
  tic()
  map2(fns, 1:length(fns), function(.x, .y, ...){
    ls_save <- map(1:n_runs, function(run = .x, ...){
      if(length(sel_changes) != 1 || sel_changes != 0){
        df <- load_data_seasons(...,
                                selectivity_change = ifelse(length(sel_changes) == 1,
                                                            sel_changes,
                                                            sel_changes[.y]))
      }
      if(is.null(n_surveys)){
        df$n_survey <- 2
      }else{
        df$n_survey <- n_surveys[.y]
      }

      if(is.null(multiple_season_data)){
        tmp <- run_multiple_MSEs(
          df = df,
          n_sim_yrs = n_sim_yrs,
          seed = seeds[run],
          TAC = if(length(tacs) == 1) tacs else tacs[.y],
          c_increase = ifelse(length(c_increases) == 1, c_increases, c_increases[.y]),
          m_increase = ifelse(length(m_increases) == 1, m_increases, m_increases[.y]))
      }else{
        dfs <- map(multiple_season_data, ~{
          do.call(load_data_seasons, as.list(.x))
        })
        tmp <- run_multiple_OMs(n_sim_yrs = n_sim_yrs,
                                seed = seeds[run],
                                df = dfs[[.y]],
                                catch_in = 0,
                                ...)
      }
      if(is.list(tmp)) tmp else NA
    }, ...)
    attr(ls_save, "plotname") <- plot_names[.y]
    saveRDS(ls_save, file = file.path(results_dir, .x))
  }, ...)
  toc()
}
