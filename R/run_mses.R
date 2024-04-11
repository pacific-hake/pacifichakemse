#' Run the MSE for a number of runs for any scenarios
#'
#' @details Saves the output of the MSE runs to the files specified in `fns`,
#' in the directories specified in `results_root_dir` and `results_dir`
#'
#' @param n_runs Then number of runs to do for each simulation
#' @param n_sim_yrs The number of years to simulate into the future
#' @param fns A vector of file names for the scenarios (.rds files). .rds extension is optional
#' @param plot_names A vector of strings to use for the scenarios later when plotting. Must either be
#' `NULL` or the same length as `fns`
#' @param tacs A vector of TAC values to be passed to the [run_mse_scenario()] function, in the same
#' order as the `fns` file names, or a single value
#' @param attains A vector of 2, in the order Canada, US for proportion of catch to include
#' @param c_increases Increase in max movement. A vector of values to be passed to the [run_mse_scenario()]
#' function, in the same order as the `fns` file names, or a single value which will be used for all scenarios
#' @param m_increases Decrease of spawners returning south. A vector of values to be passed to the
#' [run_mse_scenario()] function, in the same order as the `fns` file names, or a single value which
#' will be used for all scenarios
#' @param sel_changes Selectivity scenario change type. A vector of values to be passed to the
#' [run_mse_scenario()] function, in the same order as the `fns` file names, or a single value which will
#' be used for all scenarios
#' @param n_surveys The number of surveys for each run. This must be a vector of the same length as `fns` or `NULL`.
#' If `NULL`, 2 will be used for every scenario
#' @param b_futures A vector of values to be passed to the [run_mse_scenario()] function for bias adjustment into
#' the future, in the same order as the `fns` file names, or a single value which will be used for all scenarios
#' @param random_seed A seed value to use when calling for all random functions
#' @param results_root_dir The results root directory
#' @param results_dir The results directory
#' @param catch_floor The lowest value to allow catch to drop to when applying the tac rule for the catch floor
#' @param single_seed If NULL, ignore. If a number, use that as a seed to run a single run of the MSE. User for testing.
#' @param ... Arguments passed to [load_data_om()]
#'
#' @return Nothing
#'
#' @export
run_mses <- function(n_runs = 10,
                     n_sim_yrs = NULL,
                     fns = NULL,
                     plot_names = NULL,
                     tacs = c(0, 1),
                     attains = c(1, 1),
                     c_increases = 0,
                     m_increases = 0,
                     sel_changes = 0,
                     n_surveys = 2,
                     b_futures = 0.5,
                     random_seed = 12345,
                     single_seed = NULL,
                     results_root_dir = here("results"),
                     results_dir = here("results", "default"),
                     catch_floor = NULL,
                     hcr_apply = FALSE,
                     ...){

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

  # This function expands a single value to a vector of the length of `fns`. If it is already
  # the same length, nothing happens.
  fill_vec <- function(d){
    stopifnot(length(d) == 1 | length(d) == length(fns))
    if(length(d) == 1 && length(fns) > 1){
      d <- rep(d, length(fns))
    }
    d
  }
  c_increases <- fill_vec(c_increases)
  m_increases <- fill_vec(m_increases)
  sel_changes <- fill_vec(sel_changes)
  n_surveys <- fill_vec(n_surveys)
  b_futures <- fill_vec(b_futures)
  tacs <- fill_vec(tacs)
  attains <- fill_vec(attains)

  if(any(map_lgl(tacs, ~{length(.x) == 1 && .x != 0})) &&
     is.null(catch_floor)){
    stop("`catch_floor` argument is NULL with at least one of the `tac` ",
         "argument list values having length 1, and being not equal to ",
         "zero (which signifies no tac application). Provide a ",
         "`catch_floor` value to use when applying tac value of 1.")
  }
  if(!all(map_lgl(tacs, ~{if(length(.x) %in% 1:2) TRUE else FALSE}))){
    stop("List elements of `tacs` must be either length 1 or length 2.")
  }

  tic()
  # Seed for the random recruitment deviations (search rnorm in update_om_data.R) and
  # survey error (search rnorm in run_year_loop.R).
  # This is also the seed used to set up the random seeds for each run:
  set.seed(random_seed)
  random_seeds <- floor(runif(n = n_runs, min = 1, max = 1e6))

  # Load the raw SS model inputs and outputs using the r4ss package and the same
  # methods used in the `hake-assessment` package
  # Create objects from the raw SS model inputs and outputs and
  # only include those in this list. To add new SS model outputs,
  # modify the `load_ss_model_data()` function
  ss_model <- load_ss_model_data(...)

  cat(green(symbol$tick), green(" SS model output successfully loaded\n"))

  if(!is.null(single_seed)){
    cat(white("Scenario:", fns[1], "\n"))
    om <- load_data_om(ss_model,
                       n_sim_yrs = n_sim_yrs,
                       n_survey = n_surveys[1],
                       b_future = b_futures[1],
                       selectivity_change = sel_changes[1],
                       ...)

    cat(green("Single run, seed = ", single_seed, "\n"))

    tmp <- run_mse_scenario(om = om,
                            ss_model = ss_model,
                            n_sim_yrs = n_sim_yrs,
                            random_seed = single_seed,
                            sel_change = sel_changes[1],
                            c_increase = c_increases[1],
                            m_increase = m_increases[1],
                            tac = tacs[[1]],
                            attain = attains[[1]],
                            catch_floor = catch_floor,
                            hcr_apply = FALSE, # Tell OM not to apply HCR. If TRUE MSE will break
                            ...)

    cat(green("End single run\n"))
    toc()
    return(invisible())
  }

  # Begin MSEs loop -----------------------------------------------------------
  map2(fns, 1:length(fns), function(fn = .x, fn_ind = .y, ...){
    cat(white("Scenario:", fn, "\n"))
    #lst <- furrr::future_map(1:n_runs, function(run = .x, ...){
    lst <- map(1:n_runs, function(run = .x, ...){
      om <- load_data_om(ss_model,
                         n_sim_yrs = n_sim_yrs,
                         n_survey = n_surveys[fn_ind],
                         b_future = b_futures[fn_ind],
                         selectivity_change = sel_changes[fn_ind],
                         ...)
browser()
      cat(green("Run #", run, "\n"))

      tmp <- run_mse_scenario(om = om,
                              ss_model = ss_model,
                              n_sim_yrs = n_sim_yrs,
                              random_seed = random_seeds[run],
                              sel_change = sel_changes[fn_ind],
                              c_increase = c_increases[fn_ind],
                              m_increase = m_increases[fn_ind],
                              tac = tacs[[fn_ind]],
                              attain = attains[[fn_ind]],
                              catch_floor = catch_floor,
                              hcr_apply = FALSE, # Tell OM not to apply HCR. If TRUE MSE will break
                              ...)
      if(is.list(tmp)) tmp else NA
    }, ...)
    #}, ..., .options = furrr::furrr_options(seed = T))
  attr(lst, "plotname") <- plot_names[fn_ind]
    saveRDS(lst, file = file.path(results_dir, fn))
  }, ...)
  # End MSEs loop -----------------------------------------------------------
  toc()
}
