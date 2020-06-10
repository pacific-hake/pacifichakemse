#' Update the OM data for a new year
#'
#' @param sim_data Operating model
#' @param df Input data and parameters
#' @param ss_model SS3 model output as created by [create_rds_file()]
#' and loaded by [load_ss_model_from_rds()]
#' @param sim_age_comps Logical. If TRUE, include simulated age comps (`age_survey` and `age_catch`),
#' If FALSE, leave them as-is
#'
#' @return A list of the data needed by [TMB::MakeADFun()]
#' @importFrom stringr str_split
#' @export
update_om_data <- function(sim_data = NULL,
                           df = NULL,
                           ss_model = NULL,
                           sim_age_comps = TRUE){

  verify_argument(sim_data, "list")
  verify_argument(df, "list")
  verify_argument(ss_model, "list")
  verify_argument(sim_age_comps, "logical", 1)

  if(max(df$yrs) > df$m_yr){
    # Copy last year of weight-at-age data and use that as the simulated year
    df$wage_catch_df <- wage_add_yr(df$wage_catch_df)
    df$wage_survey_df <- wage_add_yr(df$wage_survey_df)
    df$wage_mid_df <- wage_add_yr(df$wage_mid_df)
    df$wage_ssb_df <- wage_add_yr(df$wage_ssb_df)
  }

  df
}