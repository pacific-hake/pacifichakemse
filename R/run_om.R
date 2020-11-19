#' Run the Operating Model, which is an age based model
#' conditioned on true catch
#'
#' @param om list of parameters and life history values OM data. This list is
#' what is returned by [load_data_om()]
#' @param yrs A vector of the years to run
#' @param random_seed The random seed to use for this Operating Model
#' @param ... Arguments passed to [run_year_loop_om()]
#'
#' @return A list of model outputs
#' @importFrom purrr map_dbl
#' @export
run_om <- function(om = NULL,
                   yrs = om$yrs,
                   random_seed = NULL,
                   ...){

  verify_argument(om, "list")
  verify_argument(random_seed, c( "integer", "numeric"), 1)

  set.seed(random_seed)

  om <- init_agebased_model(om)

  run_year_loop_om(om, yrs = yrs, ...)
}