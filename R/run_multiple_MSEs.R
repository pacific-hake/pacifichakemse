#' Run/Iterate the Pacific hake MSE
#'
#' @param n_sim_yrs Number of years to simulate
#' @param ss_model SS3 model output as created by [create_rds_file()]
#' and loaded by [load_ss_model_from_rds()]
#' @param sim_data Operating model as created by [run_om()]
#' @param om_params_seed Seed for running the OM if it needs to be run (if `sim_data`
#' is `NULL`)
#' @param seed The random number seed to use for the MSE run
#' @param tac Which harvest control rule should the model use
#' @param df Data frame of parameters as output by [load_data_om()]
#' @param c_increase Increase in max movement
#' @param m_increase Decrease of spawners returning south
#' @param sel_change Time varying selectivity
#' @param ... Absorb arguments intended for other functions
#'
#' @return A list of Catch, Catch.quota, SSB, SSB.mid, SSB.hes, Survey.om
#' F0, parms, N, converge, ams, amc, V
#' @importFrom TMB sdreport MakeADFun
#' @importFrom stats rnorm nlminb runif predict lm median optim setNames
#' @importFrom utils read.csv read.table
#' @export
run_multiple_MSEs <- function(df = NULL,
                              ss_model = NULL,
                              sim_data = NULL,
                              om_params_seed = 12345,
                              n_sim_yrs = NULL,
                              seed = 12345,
                              tac = 1,
                              c_increase = 0,
                              m_increase = 0,
                              sel_change = 0,
                              ...){
  verify_argument(df, "list")
  verify_argument(ss_model, "list")
  verify_argument(sim_data, "list")
  verify_argument(om_params_seed, "numeric", 1)
  verify_argument(n_sim_yrs, "numeric", 1)
  verify_argument(seed, "numeric", 1)
  verify_argument(tac, "numeric", 1)
  verify_argument(c_increase, "numeric", 1)
  verify_argument(m_increase, "numeric", 1)
  verify_argument(sel_change, "numeric", 1)

  if(is.null(sim_data)){
    if(is.null(om_params_seed)){
      stop("To run the agebased OM, you must supply `om_params_seed`",
           call. = FALSE)
    }
    sim_data <- run_om(df, om_params_seed, ...)
  }

  yr_last_non_sim <- df$yrs[df$n_yr]
  yr_start <- yr_last_non_sim + 1
  yr_end <- yr_last_non_sim + n_sim_yrs
  yr_sims <- yr_start:yr_end
  yr_all <- c(df$yrs, yr_sims)

  # Save the estimated parameters from the EM (exclude time varying)
  # em_parms_save <- array(NA, dim = c(n_sim_yrs, 4))
  # f40_save <- array(NA, n_sim_yrs)
  # ssb_save <- list()
  # r_save <- list()
  # catch_save <- list()

  # Calculate survey years, where odd years are survey years
  first_sim_surv_yr <- ifelse(yr_start %% 2 == 1, yr_start, yr_start + 1)
  yr_survey_sims <- seq(first_sim_surv_yr, yr_start + n_sim_yrs, by = df$n_survey)
  # Remove any survey years not included in the simulated years
  yr_survey_sims <- yr_survey_sims[yr_survey_sims %in% yr_sims]

  # Modify survey objects in the simulated survey years and add catch for new year
  # Start with the last year in the time series yr_last_non_sim so that reference points can
  # be calculated for application in the first simulation year
  map(c(yr_last_non_sim, yr_sims), function(yr = .x){
    yr_ind <- which(yr == yr_all)

    if(yr >= yr_start){
      df <- update_om_data(df,
                           sim_data,
                           yr,
                           yr_ind,
                           yr_survey_sims,
                           f_new,
                           c_increase,
                           m_increase,
                           sel_change)
      sim_data <- run_om(df, om_params_seed, ...)
    }
    lst_tmb <- create_TMB_data(sim_data, df, ss_model, sim_age_comps = FALSE)
    if(yr == yr_start){
      # TODO: Remove this whole if chunk once correct output has been verified with
      # the original output
      d1 <- readRDS("original_mse_data/d.rds")
      p1 <- readRDS("original_mse_data/p.rds")
      # Compare this package input data with the data from the original
      # If this line passes without causing as error, then the data and parameters are
      # almost identical. They are within tiny tolerances as found in the
      # compare_tmb_data() function.
      compare_tmb_data(lst_tmb$df, d1, lst_tmb$params, p1)
      # ---------------------
      # Debugging - set data and parameters to what they are in original
      lst_tmb$df$catch_obs <- d1$Catchobs
      lst_tmb$df$wage_catch <- d1$wage_catch
      lst_tmb$df$wage_survey <- d1$wage_survey
      lst_tmb$df$wage_mid <- d1$wage_mid
      lst_tmb$df$wage_ssb <- d1$wage_ssb
      lst_tmb$df$survey <- d1$survey
      lst_tmb$df$survey_err <- d1$survey_err
      lst_tmb$df$age_survey <- d1$age_survey
      lst_tmb$df$age_catch <- d1$age_catch
      lst_tmb$params$log_m_init <- p1$logMinit
      lst_tmb$params$log_h <- p1$logh
      lst_tmb$params$log_sd_surv <- p1$logSDsurv
      lst_tmb$params$init_n <- p1$initN
      lst_tmb$params$p_sel <- p1$PSEL
      lst_tmb$params$f_0 <- p1$F0
      # ---------------------
    }
    # Evaluate the Objective function
    browser()
    obj <- MakeADFun(lst_tmb$df, lst_tmb$params, DLL = "runHakeassessment", silent = FALSE)
    report <- obj$report()
    pars <- extract_params_tmb(obj)
    # You can check likelihood components here using this function
    print_likelihoods <- function(){
      map2(names(report), report, ~{if(length(grep("ans", .x))){ret <- .y;names(ret) <- .x;ret}}) %>%
        unlist() %>%
        `[`(!is.na(names(.)))
    }
    browser()

    lower <- obj$par - Inf
    upper <- obj$par + Inf
    upper[names(upper) == "log_h"] <- log(0.999)
    upper[names(upper) == "f_0"] <- 2
    lower[names(lower) == "log_sd_surv"] <- log(0.01)
    lower[names(lower) == "f_0"] <- 0.01
    if(lst_tmb$df$catch_obs[length(lst_tmb$df$catch_obs)] == 1){
      lower[names(lower) == "f_0"] <- 1e-10
    }
    # Minimize the Objective function
    opt <- nlminb(obj$par,
                  obj$fn,
                  obj$gr,
                  lower = lower,
                  upper = upper,
                  control = list(iter.max = 1e6,
                                 # If error one of the random effects is unused
                                 eval.max = 1e6))

    wage_catch <- df$wage_catch[nrow(df$wage_catch) - 1,] %>% select(-Yr) %>% unlist(use.names = FALSE)
    v_real <- sum(sim_data$n_save_age[, df$n_yr,,df$n_season] *
                    matrix(rep(wage_catch, df$n_space),
                           ncol = df$n_space) * (sim_data$f_sel[, df$n_yr,]))

    report <- obj$report()
    pars <- extract_params_tmb(opt)

    if(yr == yr_end){
      rep <- sdreport(obj)
      sdrep <- summary(report)
      rep_values <- rownames(sdrep)
      # Check convergence in last year
      conv <- Check_Identifiable_vs2(obj)
      browser()
    }

    f_new <- get_ref_point(pars,
                           df,
                           ssb_y = report$SSB %>% tail(1),
                           f_in = report$Fyear %>% tail(1),
                           n_end = report$N_beg[, ncol(report$N_beg)],
                           tac = tac,
                           v_real = v_real,
                           ...)

  })

}
