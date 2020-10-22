#' Run a single MSE scenario
#'
#' @param om List as output by [load_data_om()]
#' @param random_seed Seed for running the OM if it needs to be run (if `om_output` is `NULL`)
#' @param n_sim_yrs Number of years to simulate
#' @param tac Which harvest control rule should the model use
#' @param c_increase Increase in max movement
#' @param m_increase Decrease of spawners returning south
#' @param sel_change Time varying selectivity
#' @param f_sim Value of F to use for simulation years
#' @param ss_model Output from [load_ss_model_data()]
#' @param ... Absorb arguments intended for other functions
#'
#' @return A list of length 3: The MSE output, the OM output, and the EM output

#' @importFrom TMB sdreport MakeADFun
#' @importFrom stats rnorm nlminb runif predict lm median optim setNames
#' @importFrom utils read.csv read.table
#' @export
run_mse_scenario <- function(om = NULL,
                             random_seed = NULL,
                             n_sim_yrs = NULL,
                             tac = NULL,
                             c_increase = 0,
                             m_increase = 0,
                             sel_change = 0,
                             f_sim = NULL,
                             ...){

  verify_argument(om, "list")
  verify_argument(random_seed, c("integer", "numeric"), 1)
  verify_argument(n_sim_yrs, c("integer", "numeric"), 1)
  verify_argument(tac, c("integer", "numeric"))
  verify_argument(c_increase, c("integer", "numeric"), 1)
  verify_argument(m_increase, c("integer", "numeric"), 1)
  verify_argument(sel_change, c("integer", "numeric"), 1)
  verify_argument(f_sim, "numeric", 1)

  # TODO: Investigate setting the seed here instead of in the run_om() function
  # I think when it is set in run_om() all the r_devs end up being the same for a given scenario
  # set.seed(random_seed)

  # Survey years setup ---------------------------------------------------------
  # Calculate survey years, where odd years are survey years
  first_sim_surv_yr <- ifelse((om$m_yr + 1) %% 2 == 1, om$m_yr + 1, om$m_yr + 2)
  yr_survey_sims <- seq(first_sim_surv_yr, om$m_yr + 1 + n_sim_yrs, by = om$n_survey)
  # Remove any survey years not included in the simulated years
  yr_survey_sims <- yr_survey_sims[yr_survey_sims %in% om$yrs]

  # Leading parameter setup ---------------------------------------------------
  # Store the leading parameters from each year simulation year
  leading_params <- c("log_r_init", "log_h", "log_m_init", "log_sd_surv")
  params_save <- array(NA, dim = c(n_sim_yrs, length(leading_params)))

  # Modify survey objects in the simulated survey years and add catch for new year
  # Start with the last year in the time series `yr_last_non_sim` so that reference points can
  # be calculated for application in the first simulation year
  #
  # d_tmp and p_tmp are lists to hold data from the original code. They should be removed once the code is verified to be working
  d_tmp <- list()
  p_tmp <- list()
  tmp_iter <- 1

  # Create EM objects for saving -----------------------------------------------------
  # Save Estimation Model outputs in lists. iter is used to keep track of the positions for these
  em_output <- list(ssb_save = vector(mode = "list", length = om$n_future_yrs),
                    r_save = vector(mode = "list", length = om$n_future_yrs),
                    f40_save = vector(),
                    catch_save = vector(mode = "list", length = om$n_future_yrs),
                    ssb_se = vector(mode = "list", length = om$n_future_yrs),
                    ssb_min = vector(mode = "list", length = om$n_future_yrs),
                    ssb_max = vector(mode = "list", length = om$n_future_yrs))
  em_iter <- 1

  # Begin MSE loop ------------------------------------------------------------
  mse_run <- map(c(om$m_yr, om$future_yrs), function(yr = .x){
    yr_ind <- which(yr == om$yrs)

    # Run the Operating Model (OM)
    cat(green("OM: Year =", yr, "- Seed =", random_seed, "\n"))
    if(yr >= om$m_yr + 1){
      r_dev <- rnorm(n = 1, mean = 0, sd = exp(om$rdev_sd))
      om$parameters$r_in[om$parameters$r_in$yr == yr, ]$value <<- r_dev
    }
    #if(yr >= 2019) browser()

    om_output <<- run_om(om,
                         yrs = om$yrs[1:yr_ind],
                         random_seed = random_seed,
                         ...)
    om$catch_obs <- om_output$catch_obs <- om_output$catch

    # Create the data for the Estimation Model (EM)
    # if(yr >= yr_start){
    #   om$wage_catch_df <- modify_wage_df(om$wage_catch_df, yr)
    #   om$wage_survey_df <- modify_wage_df(om$wage_survey_df, yr)
    #   om$wage_mid_df <- modify_wage_df(om$wage_mid_df, yr)
    #   om$wage_ssb_df <- modify_wage_df(om$wage_ssb_df, yr)
    # }
    # Create TMB data for EM --------------------------------------------------
    #browser()
    lst_tmb <- create_tmb_data(om = om_output, yr = yr, ...)
    #browser()
    if(yr >= om$m_yr + 1){
      lst_tmb$params$f_0[length(lst_tmb$params$f_0)] <- 0.2
    }
    # Evaluate the Objective function
    d <- lst_tmb$om
    p <- lst_tmb$params
    #  Make OM equal old OM ---------------------------------------------------
    # All of this stuff is done to make sure the inputs are exactly the same as the inputs for the old code
    # It can be deleted once everything is proved to be working right.
    # Also go into the load_ss_parameters() function and delete the hardwired parameter values there as well
    # --
    d1 <- conv_d(yr)
    p1 <- conv_p(yr)
    d1$survey_x <- NULL
    # class(d$yr_sel) <- "integer"
    # class(d$ss_survey) <- "integer"
    # d$flag_survey <- as.numeric(d$flag_survey)
    # d$flag_catch <- as.numeric(d$flag_catch)
    # d1$flag_survey <- as.numeric(d1$flag_survey)
    # d1$flag_catch <- as.numeric(d1$flag_catch)
    # dimnames(d$catch_obs) <- dimnames(d1$catch_obs)
    # if("matrix" %in% class(d$b)){
    #   d$b <- d$b[,1]
    # }
    # if("matrix" %in% class(d1$b)){
    #   d1$b <- d1$b[,1]
    # }
    # dimnames(d$wage_catch) <- dimnames(d1$wage_catch)
    # dimnames(d$wage_survey) <- dimnames(d1$wage_survey)
    # dimnames(d$wage_ssb) <- dimnames(d1$wage_ssb)
    # dimnames(d$wage_mid) <- dimnames(d1$wage_mid)
    # dimnames(d$age_survey) <- dimnames(d1$age_survey)
    # dimnames(d$age_catch) <- dimnames(d1$age_catch)
    # dimnames(p$init_n) <- dimnames(p1$init_n)
    # dimnames(p$p_sel) <- dimnames(p1$p_sel)
    # d$age_max_age <- as.numeric(d$age_max_age)
    # cmp <- compare_tmb_data(d, d1, p, p1)
    # --

#browser()
    # Run TMB model -----------------------------------------------------------
    obj <- MakeADFun(d, p, DLL = "pacifichakemse", silent = FALSE)
    #obj <- MakeADFun(d1, p1, DLL = "pacifichakemse", silent = FALSE)
    report <- obj$report()
    rsmall <- report %>% map(~{format(.x, nsmall = 20)})
    plike <- report %>% get_likelihoods %>% format(nsmall = 20)
    objfn <- obj$fn() %>% format(nsmall = 20)
    psmall <- obj %>% extract_params_tmb %>% map(~{format(.x, nsmall = 20)})
#browser()

    # You can check likelihood components by placing a browser after the MakeADFun() call above and the
    # nlminb() call below and calling print_likelihoods()
    # Set up limits of optimization for the objective function minimization
    lower <- obj$par - Inf
    upper <- obj$par + Inf
    upper[names(upper) == "log_h"] <- log(0.999)
    upper[names(upper) == "f_0"] <- 2
    lower[names(lower) == "log_sd_surv"] <- log(0.01)
    lower[names(lower) == "f_0"] <- 0.01
    if(lst_tmb$om$catch_obs[length(lst_tmb$om$catch_obs)] == 1){
      lower[names(lower) == "f_0"] <- 1e-10
    }

    # Minimize model (nlminb) -------------------------------------------------
    # Stop "outer mgc: XXX" printing to screen
    obj$env$tracemgc <- FALSE
    # Minimize the Objective function
    # If error one of the random effects is unused
    #if(yr == 2022) browser()
    opt <- nlminb(obj$par,
                  obj$fn,
                  obj$gr,
                  lower = lower,
                  upper = upper,
                  control = list(iter.max = 1e6,
                                 eval.max = 1e6))

    report <- obj$report()
    pars <- extract_params_tmb(opt)
    plike <- get_likelihoods(report) %>% format(nsmall = 20)

    # Calc ref points for next year vals --------------------------------------
    # Calculate the reference points to be applied to the next year
    wage_catch <- get_age_dat(om$wage_catch_df, om$m_yr - 1) %>% unlist(use.names = FALSE)
    v_real <- sum(om_output$n_save_age[, which(om$yrs == om$m_yr), , om$n_season] *
                    matrix(rep(wage_catch, om$n_space),
                           ncol = om$n_space) * (om_output$f_sel[, which(om$yrs == om$m_yr),]))

#browser()

    f_new <- get_ref_point(pars,
                           om,
                           ssb_y = report$SSB %>% tail(1),
                           f_in = report$Fyear %>% tail(1),
                           n_end = report$N_beg[, ncol(report$N_beg)],
                           tac = tac,
                           v_real = v_real,
                           ...)
#browser()
    # Need to use map() here to keep names
    param_vals <- pars[leading_params] %>% map_dbl(~exp(as.numeric(.x)))

    # Save EM outputs ---------------------------------------------------------
    if(yr >= om$m_yr + 1){
      em_output$ssb_save[[em_iter]] <<- report$SSB
      em_output$r_save[[em_iter]] <<- report$N_beg[1,]
      em_output$f40_save[em_iter] <<- f_new[[2]]
      em_output$catch_save[[em_iter]] <<- report$Catch
      if(yr == tail(om$yrs, 1)){
        sdrep <- sdreport(obj)
        j <- sdrep_summary <- summary(sdrep)
        rep_names <- rownames(sdrep_summary)
        tmp <- sdrep_summary[rep_names == "SSB", 2]
        names(tmp) <- om$yrs
        em_output$ssb_se[[em_iter]] <<- tmp
        tmp <- em_output$ssb_save[[em_iter]] - 2 * em_output$ssb_se[[em_iter]]
        names(tmp) <- om$yrs
        em_output$ssb_min[[em_iter]] <<- tmp
        tmp <- em_output$ssb_save[[em_iter]] + 2 * em_output$ssb_se[[em_iter]]
        names(tmp) <- om$yrs
        em_output$ssb_max[[em_iter]] <<- tmp
      }
      em_iter <<- em_iter + 1
    }

    # Update the OM data for next year ----------------------------------------
    # Update the OM data for the next simulation year in the loop. Note reference points
    # are being passed into this function. Double <<- is used here so that `om` is
    # in scope in the next iteration of the loop. Without that, `om` would be `NULL`
    # in the next simulation year.
    if(yr < tail(om$yrs, 1)){
      # No need to call this in the final year as the loop is over
      om <<- update_om_data(yr + 1,
                            om,
                            yr_survey_sims,
                            f_new,
                            c_increase,
                            m_increase,
                            sel_change,
                            zero_rdevs = FALSE)

      #browser()
    }else{
      NA
    }
  })
  # End MSE loop --------------------------------------------------------------
  # Removes an NA entry at the end which is caused by the loop having one more year than
  # actual simulated years (see NA a few lines above). Making it NULL automatically removes it from the list.
  mse_run[is.na(mse_run)] <- NULL
  list(mse_run, om_output, em_output)
}
