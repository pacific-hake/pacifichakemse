#' Run/Iterate the Pacific hake MSE
#'
#' @param results_dir Directory in which the OM output will be stored
#' @param file_name The name of the file in which the MSE output was stored. This will be prepended with 'om_'
#' for the OM used in this MSE. The actual MSE results are stored by the function calling this one, [run_mses()]
#' @param df Data frame of parameters as output by [load_data_om()]
#' @param ss_model SS3 model output as created by [create_rds_file()]
#' and loaded by [load_ss_model_from_rds()]
#' @param om_objs Operating model objects as created by [setup_blank_om_objects()]
#' @param random_seed Seed for running the OM if it needs to be run (if `sim_data`
#' is `NULL`)
#' @param n_sim_yrs Number of years to simulate
#' @param tac Which harvest control rule should the model use
#' @param c_increase Increase in max movement
#' @param m_increase Decrease of spawners returning south
#' @param sel_change Time varying selectivity
#' @param f_sim Value of F to use for simulation years
#' @param ... Absorb arguments intended for other functions
#'
#' @return A list of Catch, Catch.quota, SSB, SSB.mid, SSB.hes, Survey.om
#' F0, parms, N, converge, ams, amc, V
#' @importFrom TMB sdreport MakeADFun
#' @importFrom stats rnorm nlminb runif predict lm median optim setNames
#' @importFrom utils read.csv read.table
#' @export
run_multiple_MSEs <- function(results_dir = NULL,
                              file_name = NULL,
                              df = NULL,
                              ss_model = NULL,
                              om_objs = NULL,
                              random_seed = 12345,
                              n_sim_yrs = NULL,
                              tac = 1,
                              c_increase = 0,
                              m_increase = 0,
                              sel_change = 0,
                              f_sim = NULL,
                              ...){
  verify_argument(results_dir, "character", 1)
  verify_argument(file_name, "character", 1)
  verify_argument(df, "list")
  verify_argument(ss_model, "list")
  verify_argument(om_objs, "list")
  verify_argument(random_seed, "numeric", 1)
  verify_argument(n_sim_yrs, "numeric", 1)
  verify_argument(tac, "numeric", 1)
  verify_argument(c_increase, "numeric", 1)
  verify_argument(m_increase, "numeric", 1)
  verify_argument(sel_change, "numeric", 1)
  verify_argument(f_sim, "numeric", 1)

  set.seed(random_seed)

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

  # Store the leading parameters from each year simulation year
  leading_params <- c("log_r_init", "log_h", "log_m_init", "log_sd_surv")
  params_save <- array(NA, dim = c(n_sim_yrs, 4))

  # Modify survey objects in the simulated survey years and add catch for new year
  # Start with the last year in the time series `yr_last_non_sim` so that reference points can
  # be calculated for application in the first simulation year
  #
  # d_tmp and p_tmp are lists to hold data from the original code. They should be removed once the code is verified to be working
  d_tmp <- list()
  p_tmp <- list()
  tmp_iter <- 1

  # Save Estimation Model outputs in lists. iter is used to keep track of the positions for these
  em_output <- list(ssb_save = vector(mode = "list", length = length(yr_sims)),
                    r_save = vector(mode = "list", length = length(yr_sims)),
                    f40_save = vector(),
                    catch_save = vector(mode = "list", length = length(yr_sims)))
  em_iter <- 1

  mse_run <- map(c(yr_last_non_sim, yr_sims), function(yr = .x){
    yr_ind <- which(yr == yr_all)

    # Run the Operating Model (OM)
    sim_data <<- run_om(df, om_objs, ...)
    # Save the OM data the first time through for extraction later
    #if(yr == yr_end){
      #om_file_name <- file.path(results_dir, paste0("om_", file_name))
      #saveRDS(sim_data, om_file_name)
    #}

    # Create the data for the Estimation Model (EM)
    if(yr >= yr_start){
      df$wage_catch_df <- wage_add_yr(df$wage_catch_df)
      df$wage_survey_df <- wage_add_yr(df$wage_survey_df)
      df$wage_mid_df <- wage_add_yr(df$wage_mid_df)
      df$wage_ssb_df <- wage_add_yr(df$wage_ssb_df)
    }
    lst_tmb <- create_tmb_data(sim_data, df, ss_model)
    if(yr >= yr_start){
      lst_tmb$params$f_0[length(lst_tmb$params$f_0)] <- 0.2
    }
    # TODO: Remove this whole `if` chunk once correct output has been verified with
    # the original output
    if(yr >= yr_last_non_sim && yr <= 2022){
      d_tmp[[tmp_iter]] <-readRDS(paste0("original_mse_data/d_", yr,".rds"))
      p_tmp[[tmp_iter]] <- readRDS(paste0("original_mse_data/p_", yr,".rds"))
      names(d_tmp)[1] <- names(p_tmp)[1] <- yr
      # Compare this package input data with the data from the original
      # If this line passes without causing as error, then the data and parameters are
      # almost identical. They are within tiny tolerances as found in the
      # compare_tmb_data() function. Still, this is not enough to compare output to the
      # original and to get the same likelihoods and numbers- and biomasses-at-age.
      # That is why the list elements below are temporarily being used in this version of the code.
      # ---------------------
      # Debugging - set data and parameters to what they are in original
      class(lst_tmb$df$yrs) <- "integer"
      #$lst_tmb$df$b <- lst_tmb$df$b %>% as.numeric()
      #lst_tmb$df$flag_survey <- as.numeric(lst_tmb$df$flag_survey)
      #lst_tmb$df$flag_catch <- as.numeric(lst_tmb$df$flag_catch)
      if(class(lst_tmb$df$b)[1] == "matrix"){
        lst_tmb$df$b <- lst_tmb$df$b %>% as.numeric()
      }
      if(class(d_tmp[[tmp_iter]]$b)[1] == "matrix"){
        d_tmp[[tmp_iter]]$b <- d_tmp[[tmp_iter]]$b %>% as.numeric()
      }
      class(d_tmp[[tmp_iter]]$flag_survey) <- class(lst_tmb$df$flag_survey)
      class(d_tmp[[tmp_iter]]$flag_catch) <- class(lst_tmb$df$flag_catch)
      d_tmp[[tmp_iter]]$ss_survey[which(d_tmp[[tmp_iter]]$ss_survey == -1)] <- 0
      class(d_tmp[[tmp_iter]]$ss_survey) <- class(lst_tmb$df$ss_survey)
      # ---------------------
      # lst_tmb$df$catch_obs <- d_tmp[[tmp_iter]]$Catchobs
      # lst_tmb$df$wage_catch <- d_tmp[[tmp_iter]]$wage_catch
      # lst_tmb$df$wage_survey <- d_tmp[[tmp_iter]]$wage_survey
      # lst_tmb$df$wage_mid <- d_tmp[[tmp_iter]]$wage_mid
      # lst_tmb$df$wage_ssb <- d_tmp[[tmp_iter]]$wage_ssb
      # lst_tmb$df$survey <- d_tmp[[tmp_iter]]$survey
      # lst_tmb$df$survey_err <- d_tmp[[tmp_iter]]$survey_err
      # lst_tmb$df$age_survey <- d_tmp[[tmp_iter]]$age_survey
      # lst_tmb$df$age_catch <- d_tmp[[tmp_iter]]$age_catch
      # lst_tmb$params$log_m_init <- p_tmp[[tmp_iter]]$logMinit
      # lst_tmb$params$log_h <- p_tmp[[tmp_iter]]$logh
      # lst_tmb$params$log_sd_surv <- p_tmp[[tmp_iter]]$logSDsurv
      # lst_tmb$params$init_n <- p_tmp[[tmp_iter]]$initN
      # lst_tmb$params$p_sel <- p_tmp[[tmp_iter]]$PSEL
      # lst_tmb$params$f_0 <- p_tmp[[tmp_iter]]$F0
      # ---------------------
      tmp_iter <- tmp_iter + 1
    }
    # Evaluate the Objective function
    d <- lst_tmb$df
    p <- lst_tmb$params
    d_o <- d_tmp[as.character(yr)][[1]]
    p_o <- p_tmp[as.character(yr)][[1]]

    #compare_tmb_data_tol(d, d_o, p, p_o)
    obj <- MakeADFun(lst_tmb$df, lst_tmb$params, DLL = "runHakeassessment", silent = FALSE)
    report <- obj$report()
    pars <- extract_params_tmb(obj)
    # You can check likelihood components by placing a browser after the MakeADFun() call above and the
    # nlminb() call below and calling print_likelihoods()
    print_likelihoods <- function(){
      map2(names(report), report, ~{if(length(grep("ans", .x))){ret <- .y;names(ret) <- .x;ret}}) %>%
        unlist() %>%
        `[`(!is.na(names(.)))
    }
#browser()

    # Set up limits of optimization for the objective function minimization
    lower <- obj$par - Inf
    upper <- obj$par + Inf
    upper[names(upper) == "log_h"] <- log(0.999)
    upper[names(upper) == "f_0"] <- 2
    lower[names(lower) == "log_sd_surv"] <- log(0.01)
    lower[names(lower) == "f_0"] <- 0.01
    if(lst_tmb$df$catch_obs[length(lst_tmb$df$catch_obs)] == 1){
      lower[names(lower) == "f_0"] <- 1e-10
    }

    # Stop "outer mgc: XXX" printing to screen
    obj$env$tracemgc <- FALSE
    # Minimize the Objective function
    opt <- nlminb(obj$par,
                  obj$fn,
                  obj$gr,
                  lower = lower,
                  upper = upper,
                  control = list(iter.max = 1e6,
                                 # If error one of the random effects is unused
                                 eval.max = 1e6))

    report <- obj$report()
    pars <- extract_params_tmb(opt)

    # if(yr == yr_end){
    #   rep <- sdreport(obj)
    #   sdrep <- summary(report)
    #   rep_values <- rownames(sdrep)
    #   # Check convergence in last year
    #   conv <- Check_Identifiable_vs2(obj)
    #   #browser()
    # }
#browser()

    # Calculate the reference points to be applied to the next year
    wage_catch <- df$wage_catch[nrow(df$wage_catch) - 1,] %>% select(-Yr) %>% unlist(use.names = FALSE)
    v_real <- sum(sim_data$n_save_age[, df$n_yr,,df$n_season] *
                    matrix(rep(wage_catch, df$n_space),
                           ncol = df$n_space) * (sim_data$f_sel[, df$n_yr,]))
    f_new <- get_ref_point(pars,
                           df,
                           ssb_y = report$SSB %>% tail(1),
                           f_in = report$Fyear %>% tail(1),
                           n_end = report$N_beg[, ncol(report$N_beg)],
                           tac = tac,
                           v_real = v_real,
                           ...)
#browser()

    # Need to use map() here to keep names
    param_vals <- pars[leading_params] %>% map_dbl(~exp(as.numeric(.x)))

    # Save EM outputs
    if(yr >= yr_start){
      em_output$ssb_save[[em_iter]] <<- report$SSB
      em_output$r_save[[em_iter]] <<- report$N_beg[1,]
      em_output$f40_save[em_iter] <<- f_new[[2]]
      em_output$catch_save[[em_iter]] <<- report$Catch
      em_iter <<- em_iter + 1
    }

    # Update the OM data for the next simulation year in the loop. Note reference points
    # are being passed into this function. Double <<- is used here so that `df` is
    # in scope in the next iteration of the loop. Without that, `df` would be `NULL`
    # in the next simulation year.
    message("Year before update_om_data() call is ", yr)
    if(yr < yr_end){
      # No need to call this in the final year as the loop is over
      df <<- update_om_data(df,
                            sim_data,
                            yr + 1,
                            yr_ind + 1,
                            yr_survey_sims,
                            f_new,
                            c_increase,
                            m_increase,
                            sel_change,
                            zero_rdevs = TRUE)
    }else{
      NA
    }
  })
  # Removes an NA entry at the end which is caused by the loop having one more year than
  # actual simulated years (see NA a few lines above). Making it NULL automatically removes it from the list.
  mse_run[is.na(mse_run)] <- NULL
  list(mse_run, sim_data, em_output)
}
