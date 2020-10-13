#' Run the Operating Model, which is an age based model
#' conditioned on true catch
#'
#' @param om list of parameters and life history values OM data. This list is
#' what is returned by [load_data_om()]
#' @param random_seed The random seed to use for this Operating Model
#' @param ... Arguments passed to [run_year_loop_om()]
#'
#' @return A list of model outputs
#' @importFrom purrr map_dbl
#' @export
run_om <- function(om = NULL,
                   random_seed = 123,
                   ...){

  verify_argument(om, "list")
  verify_argument(random_seed, c( "integer", "numeric"), 1)

  set.seed(random_seed)

  om <- init_agebased_model(om)

  run_year_loop_om(om, ...)
  # Catch.age[,idx]  <- (Fyrs/(Fyrs+m_yrs))*(1-exp(-(Fyrs+m_yrs)))*rowSums(N.save.age[,idx,,1])*wage$catch # Calculate the catch in kg
}
