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
                   hcr_apply = FALSE,
                   hcr_lower,
                   hcr_upper,
                   hcr_fspr,
                   ...){

  set.seed(random_seed)

  om <- init_agebased_model(om)

  om <- run_year_loop_om(om, yrs = yrs,
                         hcr_apply = hcr_apply,
                         hcr_lower = hcr_lower,
                         hcr_upper = hcr_upper,
                         hcr_fspr = hcr_fspr,
                         ...)

  # Save the HCR reference points used for reporting/plotting later
  if(hcr_apply){
    om$hcr_lower <- hcr_lower
    om$hcr_upper <- hcr_upper
    om$hcr_fspr <- hcr_fspr
  }else{
    om$hcr_lower <- NA
    om$hcr_upper <- NA
    om$hcr_fspr <- NA
  }

  om
}