#' Update the OM data for a new year
#'
#' @param om Operating model output as returned by [run_om()]
#' @param yr The year to add
#' @param yr_survey_sims Years in which a survey takes place
#' @param f_new The output of the [get_ref_point()] function from the previous year
#' @param c_increase Increase in max movement
#' @param m_increase Decrease of spawners returning south
#' @param sel_change Time varying selectivity
#' @param wage_only Only update the wage_ [data.frame]s, nothing else
#' @param ... Absorb unused parameters
#' return a random normal using mean = 0 and sd = exp(om$rdev_sd)
#'
#' @return A list of the data needed by [TMB::MakeADFun()]
#' @importFrom stringr str_split
#' @export
update_om_data <- function(yr = NULL,
                           om = NULL,
                           yr_survey_sims = NULL,
                           f_new = NULL,
                           c_increase = NULL,
                           m_increase = NULL,
                           sel_change = NULL,
                           wage_only = FALSE,
                           ...){

  if(wage_only){
    om$wage_catch_df <- modify_wage_df(om$wage_catch_df, yr)
    om$wage_survey_df <- modify_wage_df(om$wage_survey_df, yr)
    om$wage_mid_df <- modify_wage_df(om$wage_mid_df, yr)
    om$wage_ssb_df <- modify_wage_df(om$wage_ssb_df, yr)
    return(om)
  }

  yr_ind <- which(om$yrs == yr)

  # Catch updates ------------------------------------------------------------
  om$catch_obs[om$catch_obs[, "yr"] == yr, "value"] <- f_new$c_new
  #om$catch_obs[as.numeric(rownames(om$catch_obs)) == yr,] <- f_new$c_new

  curr_ss_catch <- om$ss_catch[1:(yr_ind - 1)]
  om$ss_catch[yr_ind] <- ceiling(mean(curr_ss_catch[curr_ss_catch > 0]))

  om$flag_catch[yr_ind] <- 1

  # Survey updates ------------------------------------------------------------
  curr_ss_survey <- om$ss_survey[1:(yr_ind - 1)]
  om$ss_survey[yr_ind] <- ifelse(om$flag_survey[yr_ind] == 1,
                                 ceiling(mean(curr_ss_survey[curr_ss_survey > 0])),
                                 0)


  curr_survey_err <- om$survey_err[1:(yr_ind - 1)]
  om$survey_err[yr_ind] <- ifelse(om$flag_survey[yr_ind] == 1,
                                  mean(curr_survey_err[curr_survey_err < 1]),
                                  1)

  if(om$catch_obs[yr_ind, "value"] == 0 && om$flag_survey[yr_ind] == -1){
    red(message("Stock in peril! Conducting emergency survey"))
    om$flag_survey[yr_ind] <- 1
    # Emergency survey adds 200 more age samples onto the mean value for all surveys,
    # because it is an emergency survey the hypothesis is that they will sample more
    om$ss_survey[yr_ind] <- ceiling(mean(curr_ss_survey[curr_ss_survey > 0])) + 200
    om$survey_err[yr_ind] <- mean(curr_survey_err[curr_survey_err < 1])
  }

  # Weight-at-age updates -----------------------------------------------------
  # Copy first year of weight-at-age data and use that as the simulated year
  om$wage_catch_df <- modify_wage_df(om$wage_catch_df, yr)
  om$wage_survey_df <- modify_wage_df(om$wage_survey_df, yr)
  om$wage_mid_df <- modify_wage_df(om$wage_mid_df, yr)
  om$wage_ssb_df <- modify_wage_df(om$wage_ssb_df, yr)

  # Recruitment updates -------------------------------------------------------
  # Recruitment updates are done one year at a time in the 'Random recruitment deviations'
  # section in run_mse_scenario.R

  # Selectivity updates -------------------------------------------------------
  # Selectivity updates are handled in the season loop of the MSE, see the
  # 'Calculate selectivity' section in run_season_loop.R
  # This includes the updates for the different selectivity scenarios

  # Movement model updates ----------------------------------------------------
  if(is.null(om$move_max_tmp)){
    om$move_max_tmp <- om$move_max[1] + c_increase
    om$move_out <- om$move_out - m_increase
  }else{
    om$move_max_tmp <- om$move_max_tmp + c_increase
    om$move_out <- om$move_out - m_increase
  }
  # Do not allow more than 90% to move out
  om$move_max_tmp <- ifelse(om$move_max_tmp > 0.9, 0.9, om$move_max_tmp)
  # If less than 50% move out, move 50% out
  om$move_out <- ifelse(om$move_out <= 0.5, 0.5, om$move_out)
  for(i in 1:om$n_space){
    for(j in 1:om$n_season){
      om$move_mat[i, , j, yr_ind] <- om$move_max_tmp /
        (1 + exp(-om$move_slope * (om$ages - om$move_fifty)))
    }
  }
  if(om$n_season == 4){
    # Recruits and 1 year olds don't move
    om$move_mat[, 1:2, , yr_ind] <- 0
    # Don't move south during the year
    om$move_mat[1, 4:om$n_age, 1:3, yr_ind] <- om$move_south
    # Proportion to move for age 3 and up out for the final season (4)
    om$move_mat[1, 3:om$n_age, 4, yr_ind] <- om$move_out
    # Proportion to move South for age 3 and up out for the final season (4)
    om$move_mat[2, 3:om$n_age, 4, yr_ind] <- om$move_south
  }

  # Bias adjustment updates ---------------------------------------------------
  om$b[yr_ind - 1] <- om$b_future
  om$b[yr_ind] <- om$b_future

  om
}