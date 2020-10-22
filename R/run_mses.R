#' Run the MSE for a number of runs for any scenarios
#'
#' @details Saves the output of the MSE runs to the files specified in `fns`, in the directories
#' specified in `results_root_dir` and `results_dir`
#'
#' @param ss_model_output_dir A directory which contains SS3 model run output. Used to import
#' estimates from the SS3 model
#' @param n_runs Then number of runs to do for each simulation
#' @param n_sim_yrs The number of years to simulate into the future
#' @param fns A vector of file names for the scenarios (.rds files). .rds extension is optional
#' @param plot_names A vector of strings to use for the scenarios later when plotting. Must either be
#' `NULL` or the same length as `fns`
#' @param tacs A vector of TAC values to be passed to the [run_mse_scenario()] function, in the same
#' order as the `fns` file names, or a single value
#' @param c_increases Increase in max movement. A vector of values to be passed to the [run_mse_scenario()] function, in the same
#' order as the `fns` file names, or a single value
#' @param m_increases Decrease of spawners returning south. A vector of values to be passed to the [run_mse_scenario()] function, in the same
#' order as the `fns` file names, or a single value
#' @param sel_changes A vector of values to be passed to the [run_mse_scenario()] function, in the same
#' order as the `fns` file names, or a single value
#' @param n_surveys The number of surveys for each run. This must be a vector of the same length as `fns` or `NULL`.
#' If `NULL`, 2 will be used for every scenario
#' @param multiple_season_data A list of the same length as `fns`, with each element being a vector of
#' three items, `nseason`, `nspace`, and `bfuture`. If NULL, biasadjustment will not be incorporated
#' @param random_seed A seed value to use when calling for all random functions
#' @param results_root_dir The results root directory
#' @param results_dir The results directory
#' @param ... Arguments passed to [load_data_om()]
#'
#' @return Nothing
#' @importFrom dplyr transmute group_map mutate_at quo
#' @importFrom gfutilities verify_argument func_name
#' @importFrom here here
#' @importFrom purrr map2 map map_chr map_lgl
#' @importFrom r4ss SS_output
#' @importFrom stringr str_ends
#' @importFrom clisymbols symbol
#' @importFrom tictoc tic toc
#' @importFrom crayon white
#' @export
run_mses <- function(n_runs = 10,
                     n_sim_yrs = NULL,
                     fns = NULL,
                     plot_names = NULL,
                     tacs = 1,
                     c_increases = 0,
                     m_increases = 0,
                     sel_changes = 0,
                     n_surveys = 2,
                     multiple_season_data = NULL,
                     random_seed = 12345,
                     results_root_dir = here("results"),
                     results_dir = here("results", "default"),
                     catch_floor = NULL,
                     ...){

  verify_argument(fns, chk_len = length(plot_names))
  verify_argument(tacs, c("numeric", "list"))
  verify_argument(c_increases, c("integer", "numeric"))
  verify_argument(m_increases, c("integer", "numeric"))
  verify_argument(sel_changes, c("integer", "numeric"))
  verify_argument(n_surveys, c("integer", "numeric"))
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
  tacs <- fill_vec(tacs)

  if(any(map_lgl(tacs, ~{length(.x) == 1 && .x != 0})) && is.null(catch_floor)){
    stop("`catch_floor` argument is NULL with at least one of the `tac` argument list ",
         "values having length 1, and being not equal to zero (which signifies no tac application). ",
         "Provide a catch_floor value to use when applying tac value of 1.",
         call. = FALSE)
  }
  if(!all(map_lgl(tacs, ~{if(length(.x) %in% 1:2) TRUE else FALSE}))){
    stop("List elements of `tacs` must be either length 1 or length 2.",
         call. = FALSE)
  }

  tic()
  # Seed for the random recruitment deviations (search rnorm in update_om_data.R) and
  # survey error (search rnorm in run_year_loop.R).
  # This is also the seed used to set up the random seeds for each run:
  set.seed(random_seed)
  seeds <- floor(runif(n = n_runs, min = 1, max = 1e6))

  # Load the raw SS model inputs and outputs using the r4ss package and the same
  # methods used in the `hake-assessment` package
  # Create objects from the raw SS model inputs and outputs and
  # only include those in this list. To add new SS model outputs,
  # modify the `load_ss_model_data()` function
  ss_model <- load_ss_model_data(...)

  cat(green(symbol$tick), green(" SS model output successfully loaded\n"))

  map2(fns, 1:length(fns), function(fn = .x, fn_ind = .y, ...){
    cat(white("Scenario:", fn, "\n"))
    ls_save <- map(1:n_runs, function(run = .x, ...){
      # Prepare data for the OM. This includes initializing the movement model and selectivity
      om <- load_data_om(ss_model,
                         n_sim_yrs = n_sim_yrs,
                         n_survey = n_surveys[fn_ind],
                         selectivity_change = sel_changes[fn_ind],
                         ...)

      if(is.null(multiple_season_data)){
        cat(green("Run #", run, "\n"))
        tmp <- run_mse_scenario(om = om,
                                random_seed = seeds[run],
                                n_sim_yrs = n_sim_yrs,
                                tac = if(length(tacs) == 1) tacs else tacs[[fn_ind]],
                                #n_survey = n_surveys[fn_ind],
                                sel_change = sel_changes[fn_ind],
                                c_increase = c_increases[fn_ind],
                                m_increase = m_increases[fn_ind],
                                ss_model = ss_model,
                                catch_floor = catch_floor,
                                ...)
      }else{
        # TODO: Make sure this works. It hasn't been tested at all
        dfs <- map(multiple_season_data, ~{
          do.call(load_data_om, as.list(fn))
        })
        tmp <- run_multiple_OMs(n_sim_yrs = n_sim_yrs,
                                df = dfs[[fn_ind]],
                                catch_in = 0,
                                ...)
      }
      if(is.list(tmp)) tmp else NA
    }, ...)
    attr(ls_save, "plotname") <- plot_names[fn_ind]
    saveRDS(ls_save, file = file.path(results_dir, fn))
  }, ...)
  toc()
}
