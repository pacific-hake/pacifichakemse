#' Run the Operating Model, which is an age based model
#' conditioned on true catch
#'
#' @param om list of parameters and life history values OM data. This list is
#' what is returned by [load_data_om()]
#' @param ... Arguments passed to [run_year_loop_om()]
#'
#' @return A list of model outputs
#' @importFrom purrr map_dbl
#' @export
run_om <- function(om = NULL,
                   random_seed = 42,
                   ...){

  verify_argument(om, "list")

  set.seed(random_seed)

  om <- init_agebased_model(om)

  # Calculate initial year-1 spawning biomass by numbers-at-age * weight-at-age (ssb) and
  # numbers-at-age * selectivity (ssb_all)
  mat_sel <- om$mat_sel %>% select(-Yr)
  yr_ind <- 1
  wage <- get_wa_dfs(om, om$yrs[yr_ind])
  n_save_age <- om$n_save_age[, yr_ind, , 1] %>% as.data.frame() %>% map(~{.x})
  # Calculate initial SSB for each space
  om$init_ssb <- map_dbl(n_save_age, function(space_num_at_age = .x){
    j <- sum(space_num_at_age * wage$ssb, na.rm = TRUE) * 0.5
  })
  # Calculate SSB with selectivity applied for the initial year in season 1
  om$init_ssb_all <- map_dbl(n_save_age, function(space_num_at_age = .x){
    sum(space_num_at_age * mat_sel, na.rm = TRUE) * 0.5
  })
  j <- run_year_loop_om(om, ...)
  j
  # Catch.age[,idx]  <- (Fyrs/(Fyrs+m_yrs))*(1-exp(-(Fyrs+m_yrs)))*rowSums(N.save.age[,idx,,1])*wage$catch # Calculate the catch in kg
}
