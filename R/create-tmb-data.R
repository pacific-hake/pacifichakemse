#' Create the data for [TMB::MakeADFun()]
#'
#' @details This function mostly modified objects used in the OM which are
#'  [data.frame] and [array] types to be [matrix] and [vector] types that
#'  are required by TMB. It should be called on each iteration of the MSE
#'  loop before the call to [TMB::MakeADFun()].
#'
#' @param om Operating model output returned by the [run_om()] function
#' @param ss_model A model input/output list representing the SS model as
#'  found in the RDS file created by  [create_rds_file()]
#' @param yr The year to trim the data to on the end of the time series of
#'  various objects
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A list of 2 elements: the data and parameter values needed
#'  by [TMB::MakeADFun()]
#'
#' @export
create_tmb_data <- function(om = NULL,
                            ss_model = NULL,
                            yr = NULL,
                            ...){

  inc_yrs <- om$yrs[om$yrs <= yr]
  inc_yr_ind <- which(om$yrs == yr)
  om$catch_obs <- om$catch[rownames(om$catch) %in% inc_yrs, ]
  if(yr > om$m_yr){
    ct <- om$catch_country |>
      as_tibble() |>
      select_at(.vars = vars(contains("space"))) |>
      rowSums()
    om$catch_obs[inc_yr_ind] <- ct[inc_yr_ind]
  }

  # Maturity ----
  # Use last assessment year maturity for future years (`ss_model$m_yr`)
  om$mat_sel <- om$mat_sel |>
    filter(Yr == ss_model$m_yr) |>
    select(-Yr) |>
    unlist()
  names(om$mat_sel) <- NULL

  # Create matrix versions of the WA data frames ----
  om$wage_catch <- t(om$wage_catch_df[1:inc_yr_ind, -1])
  om$wage_survey <- t(om$wage_survey_df[1:inc_yr_ind, -1])
  om$wage_mid <- t(om$wage_mid_df[1:inc_yr_ind, -1])
  om$wage_ssb <- t(om$wage_ssb_df[1:inc_yr_ind, -1])

  # Make tibbles into matrices or vectors for TMB input
  om$flag_sel <- om$flag_sel[1:inc_yr_ind] |> as.integer()
  om$flag_survey <- om$flag_survey[1:inc_yr_ind] |> as.integer()
  om$flag_catch <- om$flag_catch[1:inc_yr_ind] |> as.integer()
  # This needs to be an index, not the year
  om$sel_change_yr <- which(om$sel_change_yr == inc_yrs)
  # Remove age column
  om$parameters$init_n <- om$parameters$init_n[, "value"]

  om$parameters$r_in <-
    om$parameters$r_in[om$parameters$r_in[, "yr"] <= yr, "50%"] |> pull()

  om$parameters$f_0 <- rowSums(om$f_out_save[1:inc_yr_ind, , ])

  # Set f_0 and r_in values to 0.2 and 0 respectively in the future years
  if(yr > om$m_yr + 1){
    start_yr_const_f <- which(inc_yrs == om$m_yr + 1)
    end_yr_const_f <- which(inc_yrs == yr)
    om$parameters$f_0[start_yr_const_f:end_yr_const_f] <- 0.2
  }
  # if(yr > om$m_yr){
  #   start_yr_const_r <- which(inc_yrs == om$m_yr + 1)
  #   end_yr_const_r <- which(inc_yrs == yr - 1)
  #   om$parameters$r_in[start_yr_const_r:end_yr_const_r] <- 0
  # }

  om$parameters$p_sel <- om$sel_by_yrs |>
    as.matrix()

  # Load parameters from the assessment
  # Steepness prior distribution
  ctl <- ss_model$ctl
  param_df <- ctl$SR_parms |>
    as_tibble(rownames = "param")
  h_row <- param_df |>
    filter(param == "SR_BH_steep")

  if(nrow(h_row) != 1){
    stop("There were ", nrow(h_row), " occurences of `SR_BH_STEEP`` ",
         "in the control file when there should be only one. Control ",
         "file location:\n",
         ss_model$ctl_file,
         call. = FALSE)
  }

  h_min <- h_row |> pull(LO)
  h_max <- h_row |> pull(HI)
  h_init <- h_row |> pull(INIT)
  h_prior <- h_row |> pull(PRIOR)
  h_sd <- h_row |> pull(PR_SD)
  # For testing. This value was used in the original but not what was in
  #  the assessment output
  #h_sd <- 0.117

  om$mu <- (h_prior - h_min) / (h_max - h_min)
  om$tau <- ((h_prior - h_min) * (h_max - h_prior)) / h_sd ^ 2 - 1
  om$b_prior <- om$tau * om$mu
  om$a_prior <- om$tau * (1 - om$mu)
  om$b <- om$b |>
    as.matrix()
  om$t_end <- inc_yr_ind

  # Copy simulated data into output data
  # Remove simulation years as they go beyond the dimensions required for
  #  the estimation model
  om$survey <- om$survey[1:inc_yr_ind]
  surv_ind_yrs <- which(om$survey > 1)
  om$survey[surv_ind_yrs] <- om$survey[surv_ind_yrs]

  om$survey_err <- om$survey_err[1:inc_yr_ind]
  om$ss_survey <- om$ss_survey[1:inc_yr_ind]

  om$age_survey <- om$age_comps_surv[,1:inc_yr_ind]
  om$age_catch <- om$age_comps_catch[,1:inc_yr_ind]

  sel_change_yr <- om$yrs[om$sel_change_yr]

  # Convert some parameter objects to base types
  params <- om$parameters
  params$p_sel_fish <-
    params$p_sel_fish[params$p_sel_fish[, "space"] == 2, "value"]
  params$p_sel_surv <- params$p_sel_surv[, "value"]

  last_catch <- tail(om$catch_obs, 1)
  if(last_catch == 0){
    params$f_0[length(params$f_0)] <- 0
  }

  om$yrs <- inc_yrs
  #om$yr_sel <- inc_yr_ind - om$sel_change_yr + 1
  om$b <- om$b[1:inc_yr_ind]
  om$rdev_sd <- log(om$rdev_sd)

  # Include only what appears in the estimation model (pacifichakemse.cpp)
  keep_vec <- c("wage_catch", "wage_survey", "wage_survey", "wage_ssb",
                "wage_mid", "yr_sel", "m_sel", "mat_sel", "n_age",
                "ages", "sel_change_yr", "yrs", "t_end", "log_q",
                "flag_sel", "s_min", "s_min_survey", "s_max",
                "s_max_survey", "b", "survey", "ss_survey", "flag_survey",
                "age_survey", "age_max_age", "catch_obs", "ss_catch",
                "flag_catch", "age_catch", "log_sd_catch", "rdev_sd",
                "sigma_p_sel", "sum_zero", "s_mul", "b_prior", "a_prior",
                "survey_err", "log_phi_survey")
  keep <- names(om) %in% keep_vec
  om <- om[keep]

  # Make parameter order correct
  ord <- c("log_r_init", "log_h", "log_m_init", "log_sd_surv", "log_phi_catch",
           "p_sel_fish", "p_sel_surv", "init_n", "r_in", "p_sel", "f_0")
  params <- params[ord]

  list(om = om, params = params)
}
