#' Run a single MSE scenario
#'
#' @param om List as output by [load_data_om()]
#' @param random_seed Seed for running the OM
#' @param n_sim_yrs Number of years to simulate
#' @param c_increase Increase in max movement
#' @param m_increase Decrease of spawners returning south
#' @param sel_change Time varying selectivity
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
                             ...){

  verify_argument(om, "list")
  verify_argument(random_seed, c("integer", "numeric"), 1)
  verify_argument(n_sim_yrs, c("integer", "numeric"), 1)
  verify_argument(tac, c("integer", "numeric"))
  verify_argument(c_increase, c("integer", "numeric"), 1)
  verify_argument(m_increase, c("integer", "numeric"), 1)
  verify_argument(sel_change, c("integer", "numeric"), 1)

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

  # Create EM objects for saving ----------------------------------------------
  # Save Estimation Model outputs in lists. iter is used to keep track of the
  # positions for these
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

    # Recruitment deviations --------------------------------------------------
    if(yr >= om$m_yr + 1){
      r_dev <- rnorm(n = 1, mean = 0, sd = exp(om$rdev_sd))
      #r_dev <<- 0
      om$parameters$r_in[om$parameters$r_in$yr == yr, ]$value <<- r_dev
    }

    # Run the Operating Model -------------------------------------------------
    cat(green("OM: Year =", yr, "- Seed =", random_seed, "\n"))

    om_output <<- run_om(om,
                         yrs = om$yrs[1:yr_ind],
                         random_seed = random_seed,
                         ...)
    #if(yr == 2020) browser()

    #om$catch_obs <- om_output$catch_obs <- om_output$catch

    # Create TMB data for EM --------------------------------------------------

    lst_tmb <- create_tmb_data(om = om_output, yr = yr, ...)
    #browser()
    # if(yr > om$m_yr){
    #   #browser()
    #
    #   yrs <- as.numeric(names(lst_tmb$params$f_0))
    #   lst_tmb$params$f_0[yrs == yr] <- 0.2
    # }
    # Evaluate the Objective function
    d <- lst_tmb$om
    p <- lst_tmb$params
    #  Make OM equal old OM ---------------------------------------------------
    # All of this stuff is done to make sure the inputs are exactly the same as the inputs for the old code
    # It can be deleted once everything is proved to be working right.
    # Also go into the load_ss_parameters() function and delete the hardwired parameter values there as well
    # --
    # d1 <- conv_d(yr)
    # p1 <- conv_p(yr)
    # d1$survey_x <- NULL
    # class(d$yr_sel) <- "integer"
    # class(d$ss_survey) <- "integer"
    # d$flag_survey <- as.numeric(d$flag_survey)
    # d$flag_catch <- as.numeric(d$flag_catch)
    # d1$flag_survey <- as.numeric(d1$flag_survey)
    # d1$flag_catch <- as.numeric(d1$flag_catch)
    # d1$catch_obs <- d1$catch_obs[,1]
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
    # # cmp <- compare_tmb_data(d, d1, p, p1)
    # --

    # Run TMB model -----------------------------------------------------------
#browser()
    obj <- MakeADFun(d, p, DLL = "pacifichakemse", silent = FALSE)
    report <- obj$report()
    rsmall <- report %>% map(~{format(.x, nsmall = 20)})
    plike <- report %>% get_likelihoods %>% format(nsmall = 20)
    objfn <- obj$fn() %>% format(nsmall = 20)
    psmall <- obj %>% extract_params_tmb %>% map(~{format(.x, nsmall = 20)})
#browser()

    # You can check likelihood components and parameter estimates by placing
    # a browser after the MakeADFun() call above and the nlminb() call below and
    # looking at the `plike` vector and the `psmall` list.

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
    # Stop "outer mgc: XXX" from printing to screen
    obj$env$tracemgc <- FALSE
    # Minimize the Objective function
    opt <- nlminb(obj$par,
                  obj$fn,
                  obj$gr,
                  lower = lower,
                  upper = upper,
                  control = list(iter.max = 1e6,
                                 eval.max = 1e6))

    report <- obj$report()
    plike <- get_likelihoods(report) %>% format(nsmall = 20)
    pars <- extract_params_tmb(opt)
    psmall <- opt %>% extract_params_tmb %>% map(~{format(.x, nsmall = 20)})

#browser()

    # Calc ref points for next year vals --------------------------------------
    wage_catch <- get_age_dat(om$wage_catch_df, yr - 1) %>% unlist(use.names = FALSE)
    v_real <- sum(om_output$n_save_age[, which(om$yrs == yr), , om$n_season] *
                    matrix(rep(wage_catch, om$n_space),
                           ncol = om$n_space) * (om_output$f_sel[, which(om$yrs == yr),]))
#if(yr == 2020) browser()
    f_new <- get_ref_point(pars,
                           om,
                           yr = yr,
                           ssb_y = report$SSB %>% tail(1),
                           f_in = report$Fyear %>% tail(1),
                           n_end = report$N_beg[, ncol(report$N_beg)],
                           tac = tac,
                           v_real = v_real,
                           ...)
#if(yr == 2019) browser()

    param_vals <- pars[leading_params] %>% map_dbl(~exp(as.numeric(.x)))

    # Save EM outputs ---------------------------------------------------------
    if(yr >= om$m_yr + 1){
      em_output$ssb_save[[em_iter]] <<- report$SSB
      em_output$r_save[[em_iter]] <<- report$N_beg[1,]
      em_output$f40_save[em_iter] <<- f_new[[2]]
      em_output$catch_save[[em_iter]] <<- report$Catch
      if(yr == tail(om$yrs, 1)){
        # Suppress these warnings:
        # In sqrt(diag(object$cov.fixed)) : NaNs produced
        # In sqrt(diag(cov)) : NaNs produced
        # which are caused by using optimHess() for inverting the hessian.
        # See: https://groups.google.com/g/tmb-users/c/5dFwWqcuQQA
        suppressWarnings(sdrep <- sdreport(obj))
        suppressWarnings(sdrep_summary <- summary(sdrep))
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
      # No need to call this in the final year as the loop is finished
      om <<- update_om_data(yr + 1,
                            om,
                            yr_survey_sims,
                            f_new,
                            c_increase,
                            m_increase,
                            sel_change)
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
