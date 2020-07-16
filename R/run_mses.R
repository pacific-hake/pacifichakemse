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
#' @param random_seed A seed value to use when calling for all random functions
#' @param results_root_dir The results root directory
#' @param results_dir The results directory
#' @param ... Arguments passed to [load_data_om()]
#'
#' @return Nothing
#' @importFrom dplyr transmute group_map mutate_at quo
#' @importFrom gfutilities verify_argument func_name
#' @importFrom here here
#' @importFrom purrr map2 map map_chr
#' @importFrom r4ss SS_output
#' @importFrom stringr str_ends
#' @importFrom clisymbols symbol
#' @export
run_mses <- function(ss_model_output_dir = NULL,
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
                     random_seed = 12345,
                     results_root_dir = here("results"),
                     results_dir = here("results", "default"),
                     ...){

  verify_argument(ss_model_output_dir, "character", 1)
  if(!dir.exists(ss_model_output_dir)){
    stop("The directory name you set for the SS3 model output ",
         "(ss_model_output_dir) does not exist:\n",
    ss_model_output_dir,
    call. = FALSE)
  }
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

  # Seed for the random recruitment deviations: rnorm(n = 1, mean = 0, sd = exp(df$rdev_sd))
  # found in update_om_data.R. This is also the seed used to set up the random seeds for each
  # run (search below for "seeds")
  set.seed(random_seed)

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

  # Load the raw SS model inputs and outputs using the r4ss package and the same
  # methods used in the `hake-assessment` package
  ss_model_raw <- load_ss_model_from_rds(ss_model_output_dir, ...)
  # create objects from the raw SS model inputs and outputs and
  # only include those in this list. To add new SS model outputs,
  # modify the `load_ss_model_data()` function
  ss_model <- load_ss_model_data(ss_model_raw, ...)

  cat(green(symbol$tick), green(" SS model output successfully loaded\n"))

  # Prepare data for the OM. This includes initializing the movement model
  df <- load_data_om(ss_model, n_sim_yrs, ...)

  age_max_age <- nrow(ss_model$age_survey)
  # Parameters to initialize the OM with
  parameters <- list(log_r_init = ss_model$parms_scalar$log_r_init + log(df$r_mul),
                     log_h = ss_model$parms_scalar$log_h,
                     log_m_init = ss_model$parms_scalar$log_m_init,
                     log_sd_surv = ss_model$parms_scalar$log_sd_surv,
                     log_phi_survey = ss_model$parms_scalar$log_phi_survey,
                     log_phi_catch = ss_model$parms_scalar$log_phi_catch,
                     p_sel_fish = ss_model$p_sel_fish,
                     p_sel_surv = ss_model$p_sel_surv,
                     init_n = df$init_n,
                     r_in = df$r_dev)

  # Merge the SS \model output list with the OM outputs, and the parameters to
  # initialize the OM with
  df <- append_objs_to_list(df,
                            parameters,
                            ss_model$wage_catch_df,
                            ss_model$wage_survey_df,
                            ss_model$wage_ssb_df,
                            ss_model$wage_mid_df,
                            ss_model$mat_sel,
                            # Make sure the survey has the same length as the catch time series
                            ss_model$survey,
                            # Make sure the survey has the same length as the catch time series
                            ss_model$survey_err,
                            ss_model$ss_survey,
                            # Is there a survey in that year?
                            ss_model$flag_survey,
                            ss_model$age_survey,
                            ss_model$age_catch,
                            age_max_age,
                            ss_model$ss_catch,
                            ss_model$flag_catch,
                            ss_model$sel_by_yrs,
                            ss_model$b)

  # Add the sim yrs in so that arrays don't have to be redimensioned during the
  # simulations later. This makes the code faster and simpler overall
  yrs_all <- c(df$yrs, (df$yrs[length(df$yrs)] + 1):(df$yrs[length(df$yrs)] + n_sim_yrs))
  om_objs <- setup_blank_om_objects(yrs = yrs_all,
                                    ages = df$ages,
                                    max_surv_age = df$age_max_age,
                                    n_space = df$n_space,
                                    n_season = df$n_season)

  # Each run has its own random seed, with those seeds being chosen from
  # the base seed which is set at the beginning of this function
  seeds <- floor(runif(n = n_runs, min = 1, max = 1e6))

  map2(fns, 1:length(fns), function(.x, .y, ...){
    ls_save <- map(1:n_runs, function(run = .x, ...){
      if(length(sel_changes) != 1 || sel_changes != 0){
        df <- load_data_om(...,
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
          ss_model = ss_model,
          om_objs = om_objs,
          random_seed = seeds[run],
          n_sim_yrs = n_sim_yrs,
          tac = if(length(tacs) == 1) tacs else tacs[.y],
          c_increase = ifelse(length(c_increases) == 1, c_increases, c_increases[.y]),
          m_increase = ifelse(length(m_increases) == 1, m_increases, m_increases[.y]),
          ...)
      }else{
        dfs <- map(multiple_season_data, ~{
          do.call(load_data_om, as.list(.x))
        })
        tmp <- run_multiple_OMs(n_sim_yrs = n_sim_yrs,
                                df = dfs[[.y]],
                                catch_in = 0,
                                ...)
      }
      if(is.list(tmp)) tmp else NA
    }, ...)
    attr(ls_save, "plotname") <- plot_names[.y]
    saveRDS(ls_save, file = file.path(results_dir, .x))
  }, ...)
}
