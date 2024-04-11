#' Construct blank objects for Operating model outputs
#'
#' @param yrs A vector of years
#' @param ages A vector of ages
#' @param age_plus_grp Age for age plus group
#' @param max_surv_age The maximum age in the survey age comp data
#' @param n_space The number of spaces in the movement model
#' @param n_season The number of seasons in the movement model
#'
#' @return A list of the blank OM outputs
#'
#' @export
setup_blank_om_objects <- function(yrs = NULL,
                                   ages = NULL,
                                   age_plus_grp = NULL,
                                   max_surv_age = NULL,
                                   n_space = NULL,
                                   n_season = NULL){

  n_yr <- length(yrs)
  n_age <- length(ages)

  lst <- NULL

  lst$ssb_initial <- matrix(NA,
                            n_yr,
                            n_space,
                            dimnames = list(yrs = yrs,
                                            space = seq_len(n_space)))
  lst$ssb_all <- array(NA,
                       dim = c(n_yr, n_space, n_season),
                       dimnames = list(yrs = yrs,
                                       space = seq_len(n_space),
                                       season = seq_len(n_season)))
  lst$ssb_weight <- matrix(NA,
                           n_yr,
                           n_space,
                           dimnames = list(yrs = yrs,
                                           space = seq_len(n_space)))
  lst$biomass_save <- matrix(NA,
                             n_yr,
                             n_space,
                             dimnames = list(yrs = yrs,
                                             space = seq_len(n_space)))
  lst$catch <- matrix(NA,
                      n_yr,
                      dimnames = list(yrs = yrs))

  lst$catch_age <- matrix(NA,
                          n_age,
                          n_yr,
                          dimnames = list(ages = ages,
                                          yrs = yrs))
  lst$catch_n <- matrix(NA,
                        n_yr,
                        dimnames = list(yrs = yrs))
  lst$catch_n_age <- matrix(NA,
                            n_age,
                            n_yr,
                            dimnames = list(ages = ages,
                                            yrs = yrs))
  lst$f_sel_save <- array(NA,
                          dim = c(n_age, n_yr, n_space),
                          dimnames = list(ages = ages,
                                          yrs = yrs,
                                          space = seq_len(n_space)))
  lst$f_season_save <- array(NA,
                             dim = c(n_age, n_yr, n_space, n_season),
                             dimnames = list(ages = ages,
                                             yrs = yrs,
                                             space = seq_len(n_space),
                                             season = seq_len(n_season)))
  lst$f_out_save <- array(NA,
                          dim = c(n_yr, n_season, n_space),
                          dimnames = list(yrs = yrs,
                                          season = seq_len(n_season),
                                          space = seq_len(n_space)))
  lst$n_save_age <- array(NA,
                          dim = c(n_age, n_yr + 1, n_space, n_season),
                          dimnames = list(ages = ages,
                                          yrs = c(yrs, max(yrs) + 1),
                                          space = seq_len(n_space),
                                          season = seq_len(n_season)))
  lst$n_save_age_mid <- array(NA,
                              dim = c(n_age, n_yr + 1, n_space, n_season),
                              dimnames = list(ages = ages,
                                              yrs = c(yrs, max(yrs) + 1),
                                              space = seq_len(n_space),
                                              season = seq_len(n_season)))
  lst$r_save <- matrix(NA,
                       n_yr,
                       n_space)
  lst$v_save <- array(NA,
                      dim = c(n_yr, n_space, n_season),
                      dimnames = list(yrs = yrs,
                                      space = seq_len(n_space),
                                      season = seq_len(n_season)))
  lst$catch_save_age <- array(NA,
                              dim = c(n_age, n_yr, n_space, n_season),
                              dimnames = list(ages = ages,
                                              yrs = yrs,
                                              space = seq_len(n_space),
                                              season = seq_len(n_season)))

  lst$catch_n_save_age <- array(NA,
                                dim = c(n_age, n_yr, n_space, n_season),
                                dimnames = list(ages = ages,
                                                yrs = yrs,
                                                space = seq_len(n_space),
                                                season = seq_len(n_season)))
  lst$catch_quota <- array(NA,
                           dim = c(n_yr, n_space, n_season),
                           dimnames = list(yrs = yrs,
                                           space = seq_len(n_space),
                                           season = seq_len(n_season)))
  lst$catch_quota_n <- array(0,
                             dim = c(n_yr, n_space, n_season),
                             dimnames = list(yrs = yrs,
                                             space = seq_len(n_space),
                                             season = seq_len(n_season)))
  # lst$survey <- array(NA,
  #                 dim = c(n_yr),
  #                 dimnames = list(yrs = yrs))
  lst$survey_true <- array(NA,
                           dim = c(n_space, n_yr),
                           dimnames = list(space = seq_len(n_space),
                                           yrs = yrs))
  lst$surv_tot <- matrix(NA,
                         n_yr,
                         n_space,
                         dimnames = list(yrs = yrs,
                                         space = seq_len(n_space)))

  lst$age_comps_surv <- array(NA,
                              dim = c(max_surv_age, n_yr),
                              dimnames = list(ages = seq_len(max_surv_age),
                                              yrs = yrs))
  lst$age_comps_surv_space <- array(NA,
                                    dim = c(max(ages), n_yr, n_space),
                                    dimnames = list(ages = seq_len(max(ages)),
                                                    yrs = yrs))
  lst$age_comps_catch <- array(NA,
                               dim = c(max_surv_age, n_yr),
                               dimnames = list(ages = seq_len(max_surv_age),
                                               yrs = yrs))

  lst$age_comps_catch_space <- array(NA,
                                     dim = c(age_plus_grp, n_yr, n_space),
                                     dimnames = list(ages = seq_len(age_plus_grp),
                                                     yrs = yrs,
                                                     space = seq_len(n_space)))
  lst$age_comps_om <- array(NA,
                            dim = c(n_age, n_yr, n_space,n_season),
                            dimnames = list(ages = ages,
                                            yrs= yrs,
                                            space = seq_len(n_space),
                                            season = seq_len(n_season)))
  lst$z_save <- array(NA,
                      dim = c(n_age, n_yr, n_space, n_season),
                      dimnames = list(ages = ages,
                                      yrs = yrs,
                                      space = seq_len(n_space),
                                      season = seq_len(n_season)))

  lst
}

