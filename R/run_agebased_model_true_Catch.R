#' Run an agebased model
#'
#' @param df data frame of parameters and life history values
#' @param seed seed for survey error and recruitment deviations
#' @param n_sim_yrs The number of years to simulate into the future.

#' @param ... Arguments passed to [run_year_loop_om()]
#'
#' @return A list of model outputs (TODO)
#' @importFrom purrr map_dbl
#' @export
#'
#' @examples
#' \dontrun{
#' run_agebased_true_catch(df)
#' }
run_agebased_true_catch <- function(df = NULL,
                                    seed = 100,
                                    n_sim_yrs = NULL,
                                    ...){
  verify_argument(df, "list")
  verify_argument(seed, "numeric", 1)
  verify_argument(n_sim_yrs, c("integer", "numeric"), 1)

  set.seed(seed)

  # Add the sim yrs in so that we don't have to redimension the arrays later.
  # This makes it much faster and the code cleaner in the MSE loop later
  yrs_all <- c(df$yrs, (df$yrs[length(df$yrs)] + 1):(df$yrs[length(df$yrs)] + n_sim_yrs))
  lst <- setup_blank_om_objects(yrs = yrs_all,
                                ages = df$ages,
                                max_surv_age = df$age_max_age,
                                n_space = df$n_space,
                                n_season = df$n_season)

  lst <- init_agebased_model(df, lst)

  # Calculate initial year-1 spawning biomass by numbers-at-age * weight-at-age (ssb) and
  # numbers-at-age * selectivity (ssb_all)
  mat_sel <- df$mat_sel %>% select(-Yr)
  yr_ind <- 1
  wage <- get_wa_dfs(df, df$yrs[yr_ind])
  n_save_age <- lst$n_save_age[, yr_ind, , 1] %>% as.data.frame() %>% map(~{.x})
  # Calculate initial SSB for each space
  lst$init_ssb <- map_dbl(n_save_age, function(space_num_at_age = .x){
    j <- sum(space_num_at_age * wage$ssb, na.rm = TRUE) * 0.5
  })
  # Calculate SSB with selectivity applied for the initial year in season 1
  lst$init_ssb_all <- map_dbl(n_save_age, function(space_num_at_age = .x){
    sum(space_num_at_age * mat_sel, na.rm = TRUE) * 0.5
  })

  lst <- run_year_loop_om(df, lst, ...)
  browser()

    #Catch.age[,idx]  <- (Fyrs/(Fyrs+m_yrs))*(1-exp(-(Fyrs+m_yrs)))*rowSums(N.save.age[,idx,,1])*wage$catch # Calculate the catch in kg

}
