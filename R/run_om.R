#' Run the Operating Model, which is an age based model
#' conditioned on true catch
#'
#' @param df data frame of parameters and life history values
#' @param om_objs A [list] of the OM objects (arrays and matrices) for holding the
#' OM data. This list is what is returned by [setup_blank_om_objects()]
#' @param ... Arguments passed to [run_year_loop_om()]
#'
#' @return A list of model outputs
#' @importFrom purrr map_dbl
#' @export
run_om <- function(df = NULL,
                   om_objs = NULL,
                   random_seed,
                   ...){
  verify_argument(df, "list")
  verify_argument(om_objs, "list")

  set.seed(random_seed)

  lst <- init_agebased_model(df, om_objs)

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
  run_year_loop_om(df, lst, ...)
  # Catch.age[,idx]  <- (Fyrs/(Fyrs+m_yrs))*(1-exp(-(Fyrs+m_yrs)))*rowSums(N.save.age[,idx,,1])*wage$catch # Calculate the catch in kg
}
