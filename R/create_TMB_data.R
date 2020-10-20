#' Create the data for [TMB::MakeADFun()]
#'
#' @details This function mostly modified objects used in the OM which are
#' [data.frame] and [array] types to be [matrix] and [vector] types that are required
#' by TMB. It should be called on each iteration of the MSE loop before the call to
#' [TMB::MakeADFun()].
#'
#' @param om Operating model output returned by the [run_om()] function
#' @param ss_model A model input/output list representing the SS model as found in
#' the RDS file created by  [create_rds_file()]
#' @param yr The year to trim the data to on the end of the time series. Various objects
#' are populated into the future and must be trimmed
#'
#' @return A list of 2 elements: the data and parameter values needed by [TMB::MakeADFun()]
#' @importFrom stringr str_split
#' @export
create_tmb_data <- function(om = NULL,
                            ss_model = NULL,
                            yr = NULL){

  verify_argument(om, "list")
  verify_argument(ss_model, "list")
  verify_argument(yr, c("integer", "numeric"), 1)

  inc_yrs <- om$yrs[om$yrs <= yr]
  inc_yr_ind <- which(om$yrs == yr)

  # Catch Observations
  catch_obs_yrs <- om$catch_obs %>% filter(yr <= !!yr) %>% pull(yr)
  if(!identical(inc_yrs, catch_obs_yrs)){
    stop("The years in the catch observations does not match the number of yrs in the OM")
  }

  om$catch_obs <- om$catch_obs %>%
    filter(yr <= !!yr) %>%
    select(value) %>%
    as.matrix() %>%
    `rownames<-`(inc_yrs)

  # Maturity
  om$mat_sel <- om$mat_sel %>%
    select(-Yr) %>%
    unlist(use.names = FALSE)

  # Create matrix versions of the WA data frames
  om$wage_catch <- format_wage_matrix(om$wage_catch_df[1:inc_yr_ind,])
  om$wage_survey <- format_wage_matrix(om$wage_survey_df[1:inc_yr_ind,])
  om$wage_mid <- format_wage_matrix(om$wage_mid_df[1:inc_yr_ind,])
  om$wage_ssb <- format_wage_matrix(om$wage_ssb_df[1:inc_yr_ind,])

  # Make tibbles into matrices or vectors for TMB input
  # Logical must be changed to integer
  om$flag_sel <- om$flag_sel[1:inc_yr_ind] %>% as.numeric()
  om$flag_survey <- om$flag_survey[1:inc_yr_ind] %>% as.integer()
  om$flag_catch <- om$flag_catch[1:inc_yr_ind] %>% as.integer()
  # This needs to be an index, not the year
  om$sel_change_yr <- which(om$sel_change_yr == inc_yrs)
  # Remove age column
  om$parameters$init_n <- om$parameters$init_n %>%
    select(value) %>%
    as.matrix()
  om$parameters$r_in <- om$parameters$r_in %>%
    filter(yr <= om$m_yr) %>%
    # Remove the final year
    slice(-n()) %>%
    pull(value)

  om$parameters$f_0 <- rowSums(om$f_out_save[1:inc_yr_ind, , ])
  om$parameters$p_sel <- om$sel_by_yrs %>%
    as.matrix()

  # Load parameters from the assessment
  # Steepness prior distribution
  ctl <- ss_model$ctl
  h_grep <- grep("SR_BH_steep", ctl)
  if(length(h_grep) != 1){
    stop("There were ", length(h_grep), " occurences of SR_BH_STEEP ",
         "in the control file when there should be only one. Control file location:\n",
         ss_model$ctl_file,
         call. = FALSE)
  }
  h_prior_vec <- str_split(ctl[h_grep], " +")[[1]]
  h_prior_vec <- h_prior_vec[h_prior_vec != ""]
  h_min <- as.numeric(h_prior_vec[1])
  h_max <- as.numeric(h_prior_vec[2])
  h_init <- as.numeric(h_prior_vec[3])
  h_prior <- as.numeric(h_prior_vec[4])
  h_sd <- as.numeric(h_prior_vec[5])
  # For testing. This value was used in the original but not what was in the assessment output
  h_sd <- 0.117

  om$mu <- (h_prior - h_min) / (h_max - h_min)
  om$tau <- ((h_prior - h_min) * (h_max - h_prior)) / h_sd ^ 2 - 1
  om$b_prior <- om$tau * om$mu
  om$a_prior <- om$tau * (1 - om$mu)

  om$b <- om$b %>% as.matrix()
  # colname required to be identical to original version
  colnames(om$b) <- "V1"

  om$t_end <- inc_yr_ind

  # Copy simulated data into output data
  # Remove simulation years as they go beyond the dimensions required for the estimation model
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
  params$p_sel_fish <- om$parameters$p_sel_fish %>%
    filter(space == 2) %>%
    pull(value)
  params$p_sel_surv <- om$parameters$p_sel_surv %>%
    pull(value)
  params$f_0 <- rowSums(om$f_out_save[1:inc_yr_ind, , ])

  last_catch <- om$catch_obs %>% tail(1)
  if(last_catch == 0){
    params$f_0[length(params$f_0)] <- 0
  }

  om$yrs <- inc_yrs
  #om$yr_sel <- inc_yr_ind - om$sel_change_yr + 1
  om$b <- om$b[1:inc_yr_ind]
  om$rdev_sd <- log(om$rdev_sd)

  # Include only what appears in the estimation model (pacifichakemse.cpp) - TODO age_catch
  keep <- names(om) %in% c("wage_catch", "wage_survey", "wage_survey", "wage_ssb", "wage_mid", "yr_sel",
                           "m_sel", "mat_sel", "n_age", "ages", "sel_change_yr", "yrs", "t_end",
                           "log_q", "flag_sel", "s_min", "s_min_survey", "s_max", "s_max_survey",
                           "b", "survey", "ss_survey", "flag_survey", "age_survey", "age_max_age",
                           "catch_obs", "ss_catch", "flag_catch", "age_catch", "log_sd_catch",
                           "rdev_sd", "sigma_p_sel", "sum_zero", "s_mul", "b_prior",
                           "a_prior", "survey_err", "log_phi_survey")
  om <- om[keep]

  # Make parameter order correct
  ord <- c("log_r_init", "log_h", "log_m_init", "log_sd_surv", "log_phi_catch",
           "p_sel_fish", "p_sel_surv", "init_n", "r_in", "p_sel", "f_0")
  params <- params[ord]

  list(om = om, params = params)
}
