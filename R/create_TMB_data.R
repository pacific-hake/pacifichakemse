#' Create the data for [TMB::MakeADFun()]
#'
#' @details This function mostly modified objects used in the OM which are
#' [data.frame] and [array] types to be [matrix] and [vector] types that are required
#' by [TMB]. It should be called on each iteration of the MSE loop before the call to
#' [TMB::MakeADFun()].
#'
#' @param sim_data Operating model
#' @param df input parameters
#' @param ss_model SS3 model output as created by [create_rds_file()]
#' and loaded by [load_ss_model_from_rds()]
#'
#' @return A list of 2 elements: the data and parameter values needed by [TMB::MakeADFun()]
#' @importFrom stringr str_split
#' @export
create_tmb_data <- function(sim_data = NULL,
                            df = NULL,
                            ss_model = NULL){
  verify_argument(sim_data, "list")
  verify_argument(df, "list")
  verify_argument(ss_model, "list")

  # Catch Observations
  catch_obs_yrs <- df$catch_obs %>% pull(yr)
  if(!identical(df$yrs, catch_obs_yrs)){
    stop("The years in the catch observations does not match the number of yrs in the OM")
  }
  df$catch_obs <- df$catch_obs %>%
    select(value) %>%
    as.matrix() %>%
    `rownames<-`(df$yrs)

  # Maturity
  df$mat_sel <- df$mat_sel %>%
    select(-Yr) %>%
    unlist(use.names = FALSE)

  # Create matrix versions of the WA data frames
  df$wage_catch <- format_wage_matrix(df$wage_catch_df)
  df$wage_survey <- format_wage_matrix(df$wage_survey_df)
  df$wage_mid <- format_wage_matrix(df$wage_mid_df)
  df$wage_ssb <- format_wage_matrix(df$wage_ssb_df)

  # Make tibbles into matrices or vectors for TMB input
  # Logical must be changed to integer
  df$flag_sel <- df$flag_sel %>% as.numeric()
  df$flag_survey <- df$flag_survey %>% as.integer()
  df$flag_catch <- df$flag_catch %>% as.integer()
  # This needs to be an index, not the year
  df$sel_change_yr <- which(df$sel_change_yr == df$yrs)
  # Remove age column
  df$parameters$init_n <- df$parameters$init_n %>%
    select(value) %>%
    as.matrix()
  df$parameters$r_in <- df$parameters$r_in %>%
    # Remove the final year
    slice(-n()) %>%
    pull(value)
  df$parameters$f_0 <- rowSums(sim_data$f_out_save)
  df$parameters$p_sel <- df$sel_by_yrs %>%
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

  df$mu <- (h_prior - h_min) / (h_max - h_min)
  df$tau <- ((h_prior - h_min) * (h_max - h_prior)) / h_sd ^ 2 - 1
  df$b_prior <- df$tau * df$mu
  df$a_prior <- df$tau * (1 - df$mu)

  df$b <- df$b %>% as.matrix()
  # colname required to be identical to original version
  colnames(df$b) <- "V1"

  df$t_end <- length(df$yrs)

  # Copy simulated data into output data
  df$survey <- sim_data$survey
  # Remove simulation years as they go beyond the dimensions required for the estimation model
  df$survey <- df$survey[as.numeric(names(df$survey)) %in% df$yrs] %>% as.numeric()
  df$age_survey <- sim_data$age_comps_surv
  # Remove simulation years as they go beyond the dimensions required for the estimation model
  df$age_survey <- df$age_survey[,as.numeric(colnames(df$age_survey)) %in% df$yrs]
  df$age_catch <- sim_data$age_comps_catch
  # Remove simulation years as they go beyond the dimensions required for the estimation model
  df$age_catch <- df$age_catch[,as.numeric(colnames(df$age_catch)) %in% df$yrs]

  # Convert some parameter objects to base types
  params <- df$parameters
  params$p_sel_fish <- df$parameters$p_sel_fish %>%
    filter(space == 2) %>%
    filter(age != 1) %>%
    pull(value)
  params$p_sel_surv <- df$parameters$p_sel_surv %>%
    filter(age != 2) %>%
    pull(value)
  params$f_0 <- rowSums(sim_data$f_out_save)
  params$f_0 <- params$f_0[as.numeric(names(params$f_0)) %in% df$yrs]

  last_catch <- df$catch_obs %>% tail(1)
  if(last_catch == 0){
    params$f_0[length(params$f_0)] <- 0
  }

  # Include only what appears in the estimation model (runHakeassessment.cpp) - TODO age_catch
  keep <- names(df) %in% c("wage_catch", "wage_survey", "wage_survey", "wage_ssb", "wage_mid", "yr_sel",
                           "m_sel", "mat_sel", "n_age", "ages", "sel_change_yr", "yrs", "t_end",
                           "log_q", "flag_sel", "s_min", "s_min_survey", "s_max", "s_max_survey",
                           "b", "survey", "ss_survey", "flag_survey", "age_survey", "age_max_age",
                           "catch_obs", "ss_catch", "flag_catch", "age_catch", "log_sd_catch",
                           "rdev_sd", "sigma_p_sel", "sum_zero", "s_mul", "b_prior",
                           "a_prior", "survey_err", "log_phi_survey")
  df <- df[keep]

  # Make parameter order correct
  ord <- c("log_r_init", "log_h", "log_m_init", "log_sd_surv", "log_phi_catch",
           "p_sel_fish", "p_sel_surv", "init_n", "r_in", "p_sel", "f_0")
  params <- params[ord]

  list(df = df, params = params)
}
