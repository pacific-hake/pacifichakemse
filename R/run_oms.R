#' Run OMs for a number of runs
#'
#' @param ss_model SS model data as returned from [load_ss_model_data()]
#' @param n_runs The number of runs to run each scenario
#' @param yr_future Number of years to run the simulations out to
#' @param fns A vector of scenario names. See [run_mses()]
#' @param n_surveys The number of surveys. See [run_mses()]
#' @param b_futures Bias adjustment factors. See [run_mses()]
#' @param sel_changes Selectivity changes. See [run_mses()]
#' @param catch_in Catch into the future in kg
#' @param plot_names A vector of names to show on plots. See [run_mses()]
#' @param random_seed The random seed to base run random seeds from. See
#' [run_mses()]
#' @param results_root_dir The results root directory
#' @param results_dir The results directory
#' @param ... Arguments passed to [load_data_om()]
#'
#' @return Nothing, save OM output to a file
#'
#' @export
run_oms <- function(ss_model = NULL,
                    n_runs = NULL,
                    yr_future = 0,
                    fns = NULL,
                    n_surveys = 2,
                    b_futures = 0.5,
                    sel_changes = 0,
                    catch_in = NA_real_,
                    hcr_apply = TRUE,
                    random_recruitment = TRUE,
                    attain = c(1, 1),
                    plot_names = NULL,
                    random_seed = NULL,
                    results_root_dir = here("results"),
                    results_dir = here("results", "default"),
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

  # This function expands a single value to a vector of the length of
  # `fns`. If it is already the same length, nothing happens.
  fill_vec <- \(d){
    stopifnot(length(d) == 1 | length(d) == length(fns))
    if(length(d) == 1 && length(fns) > 1){
      d <- rep(d, length(fns))
    }
    d
  }
  sel_changes <- fill_vec(sel_changes)
  n_surveys <- fill_vec(n_surveys)
  b_futures <- fill_vec(b_futures)

  set.seed(random_seed)
  random_seeds <- floor(runif(n = n_runs, min = 1, max = 1e6))

  if(!is.na(catch_in) && catch_in != 0 && hcr_apply){
    warning("Both hcr_apply and catch_in are set.\n",
            "catch_in value was ignored, default catch was used ",
            "with the HCR rule applied.",
            call. = FALSE)
  }
  # Begin MSEs loop -----------------------------------------------------------
  iter <<- 1
  map2(fns, 1:length(fns), function(fn = .x, fn_ind = .y, ...){
    cat(white("Scenario:", fn, "\n"))
    # Begin run loop ----------------------------------------------------------
    lst <- map(1:ifelse(random_recruitment, n_runs, 1), \(run = .x, ...){
      cat(green("OM run", run, ": Seed =", random_seeds[run], "\n"))
      om <- load_data_om(ss_model,
                         yr_future = yr_future,
                         n_survey = n_surveys[fn_ind],
                         b_future = b_futures[fn_ind],
                         selectivity_change = sel_changes[fn_ind],
                         random_recruitment = random_recruitment,
                         ...)

      iter <<- iter + 1
      const_catch <- FALSE
      if(!is.na(catch_in)){
        om$catch_obs[(which(om$yrs == om$m_yr) + 1):nrow(om$catch_obs), 2] <- catch_in
        const_catch <- TRUE
      }
      run_om(om,
             random_seed = random_seeds[run],
             verbose = FALSE,
             const_catch = const_catch,
             zero_catch_val = 0,
             attain = attain,
             hcr_apply = hcr_apply,
             ...)
    }, ...)
    # End run loop ------------------------------------------------------------
    attr(lst, "plotname") <- plot_names[fn_ind]
    saveRDS(lst, file = file.path(results_dir, fn))
  }, ...)
  # End MSEs loop -------------------------------------------------------------
}