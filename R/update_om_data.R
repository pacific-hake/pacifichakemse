#' Update the OM data for a new year
#'
#' @param df Input data as created by [create_tmb_data()]
#' @param yr The year to add
#' @param yr_ind Index of the year to add
#' @param yr_survey_sims Years in which a survey takes place
#' @param f_new The output of the [get_ref_point()] function from the previous year
#' @param c_increase Increase in max movement
#' @param m_increase Decrease of spawners returning south
#' @param sel_change Time varying selectivity
#' @param wage_only Only update the wage_ [data.frame]s, nothing else
#' @param zero_rdevs Logical. If TRUE, return recruitment devs (r_in) of 0. If FALSE,
#' return a random normal using mean = 0 and sd = exp(df$rdev_sd)
#'
#' @return A list of the data needed by [TMB::MakeADFun()]
#' @importFrom stringr str_split
#' @export
update_om_data <- function(df = NULL,
                           yr = NULL,
                           yr_ind = NULL,
                           yr_survey_sims = NULL,
                           f_new = NULL,
                           c_increase = NULL,
                           m_increase = NULL,
                           sel_change = NULL,
                           wage_only = FALSE,
                           zero_rdevs = TRUE){

  verify_argument(df, "list")
  verify_argument(wage_only, "logical", 1)
  if(wage_only){
    df$wage_catch_df <- wage_add_yr(df$wage_catch_df)
    df$wage_survey_df <- wage_add_yr(df$wage_survey_df)
    df$wage_mid_df <- wage_add_yr(df$wage_mid_df)
    df$wage_ssb_df <- wage_add_yr(df$wage_ssb_df)
    return(df)
  }

  verify_argument(yr, c("integer", "numeric"), 1)
  verify_argument(yr_ind, c("integer", "numeric"), 1)
  verify_argument(yr_survey_sims, c("integer", "numeric"))
  verify_argument(f_new, "list", 2)
  verify_argument(c_increase, "numeric", 1)
  verify_argument(m_increase, "numeric", 1)
  verify_argument(sel_change, c("integer", "numeric"), 1)

  df$catch_obs <- df$catch_obs %>% add_row(yr = yr, value = f_new$c_new)

  df$flag_survey <- c(df$flag_survey, ifelse(yr %in% yr_survey_sims, 1, -1))
  browser()
  df$ss_survey <- c(df$ss_survey, ifelse(yr %in% yr_survey_sims,
                                         ceiling(mean(df$ss_survey[df$ss_survey > 0])),
                                         0))
  df$survey_err <- c(df$survey_err, ifelse(yr %in% yr_survey_sims,
                                           mean(df$survey_err[df$survey_err < 1]),
                                           1))
  df$ss_catch <- c(df$ss_catch, ceiling(mean(df$ss_catch[df$ss_catch > 0])))
  df$flag_catch <- c(df$flag_catch, 1)

  # Copy first year of weight-at-age data and use that as the simulated year
  df$wage_catch_df <- wage_add_yr(df$wage_catch_df)
  df$wage_survey_df <- wage_add_yr(df$wage_survey_df)
  df$wage_mid_df <- wage_add_yr(df$wage_mid_df)
  df$wage_ssb_df <- wage_add_yr(df$wage_ssb_df)

  if(df$catch_obs[yr_ind,]$value == 0 && df$flag_survey[yr_ind] == -1){
    red(message("Stock in peril! Conducting emergency survey"))
    df$flag_survey[yr_ind] <- 1
    # Emergency survey adds 200 more age samples onto the mean value for all surveys,
    # because it is an emergency survey the hypothesis is that they will sample more
    df$ss_survey[yr_ind] <- ceiling(mean(df$ss_survey[df$ss_survey > 0])) + 200
    df$survey_err[yr_ind] <- mean(df$survey_err[df$survey_err < 1])
  }

  df$b[length(df$b)] <- df$b_future
  df$b <- c(df$b, df$b_future)
  if(zero_rdevs){
    r_devs <- 0
  }else{
    r_devs <- rnorm(n = 1,
                    mean = 0,
                    sd = exp(df$rdev_sd))
  }
  df$parameters$r_in <- df$parameters$r_in %>% add_row(yr = yr, value = r_devs)

  # Add movement to the new years
  # If the first simulation year..
  if(yr_ind == df$n_yr + 1){
    move_max_tmp <- df$move_max[1] + c_increase
    df$move_out <- df$move_out - m_increase
  }else{
    move_max_tmp <- move_max_tmp + c_increase
    df$move_out <- df$move_out - m_increase
  }
  # Do not allow more than 90% to move out
  move_max_tmp <- ifelse(move_max_tmp > 0.9, 0.9, move_max_tmp)
  # If less than 50% move out, move 50% out
  df$move_out <- ifelse(df$move_out <= 0.5, 0.5, df$move_out)
  for(i in 1:df$n_space){
    for(j in 1:df$n_season){
      df$move_mat[i, , j, yr_ind] <- move_max_tmp /
        (1 + exp(-df$move_slope * (df$ages - df$move_fifty)))
    }
  }
  # For the standard model
  if(df$n_season == 4){
    # Recruits and 1 year olds don't move
    df$move_mat[, 1:2, , yr_ind] <- 0
    # Don't move south during the year
    df$move_mat[1, 4:df$n_age, 1:3, yr_ind] <- df$move_south
    # Proportion to move for age 3 and up out for the final season (4)
    df$move_mat[1, 3:df$n_age, 4, yr_ind] <- df$move_out
    # Proportion to move South for age 3 and up out for the final season (4)
    df$move_mat[2, 3:df$n_age, 4, yr_ind] <- df$move_south
  }

  # Selectivity modifications
  df$flag_sel <- c(df$flag_sel, FALSE)
  if(sel_change == 0){
  }else if(sel_change == 1){
  }else if(sel_change == 2){
    # df$sel_by_yrs <- cbind(df$sel_by_yrs, rep(0, nrow(df$sel_by_yrs)))
    # names(df$sel_by_yrs)[ncol(df$sel_by_yrs)] <- yr
  }

  df$yrs <- c(df$yrs, yr)
  df$n_yr <- df$n_yr + 1

  df
}