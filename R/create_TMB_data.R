#' Create the data for [TMB::MakeADFun()]
#'
#' @param sim_data Operating model
#' @param df input parameters
#' @param ss_model SS3 model output as created by [create_rds_file()]
#' and loaded by [load_ss_model_from_rds()]
#' @param history Logical. If TRUE use historical data. If FALSE, use simulated OM data
#'
#' @return A list of the data needed by [TMB::MakeADFun()]
#' @importFrom stringr str_split
#' @export
create_TMB_data <- function(sim_data = NULL,
                            df = NULL,
                            ss_model = NULL,
                            history = FALSE){
  verify_argument(sim_data, "list")
  verify_argument(df, "list")
  verify_argument(ss_model, "list")
  verify_argument(history, "logical", 1)

  if(max(df$yrs) > df$m_yr){
    # Copy last year of weight-at-age data and use that as the simulated year
    df$wage_catch <- wage_add_yr(df$wage_catch)
    df$wage_survey <- wage_add_yr(df$wage_survey)
    df$wage_mid <- wage_add_yr(df$wage_mid)
    df$wage_ssb <- wage_add_yr(df$wage_ssb)
  }
  df$wage_catch <- df$wage_catch %>%
    select(-c(Yr, Fleet)) %>%
    as.matrix() %>%
    t()
  df$wage_survey <- df$wage_survey %>%
    select(-c(Yr, Fleet)) %>%
    as.matrix() %>%
    t()
  df$wage_mid <- df$wage_mid %>%
    select(-c(Yr, Fleet)) %>%
    as.matrix() %>%
    t()
  df$wage_ssb <- df$wage_ssb %>%
    select(-c(Yr, Fleet)) %>%
    as.matrix() %>%
    t()

  # Make tibbles into matrices or vectors for TMB input
  # Logical must be changed to integer
  df$flag_sel <- df$flag_sel %>% as.integer()
  df$s_min <- 1
  df$survey <- df$survey %>% pull(x) %>% as.vector()
  # This needs to be an index, not the year
  df$sel_change_yr <- which(df$sel_change_yr == df$yrs)

  # Load parameters from the assessment
  # Steepness prior distribution
  ctl <- ss_model$ctl
  h_grep <- grep("SR_BH_steep", ctl)
  if(length(h_grep) != 1){
    stop("There were ", length(h_grep), " occurances of SR_BH_STEEP ",
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

  df$mu <- (h_prior - h_min) / (h_max - h_min)
  df$tau <- ((h_prior - h_min) * (h_max - h_prior)) / h_sd ^ 2 - 1
  df$b_prior <- df$tau * df$mu
  df$a_prior <- df$tau * (1 - df$mu)

  # Move things from sim_data into output list
  df$survey <- sim_data$survey
  df$age_survey <- sim_data$age_comps_surv
  df$catch_obs <- sim_data$catch
  df$age_catch <- sim_data$catch_age

  # Remove elements that will cause failure in the TMB code
  keep <- !names(df) %in% c("space_names",
                            "season_names",
                            "age_names")
  df <- df[keep]

  # if(history){
  #   df$survey <- df$survey[,1]
  # }
  df
}
