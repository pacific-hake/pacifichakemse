#' Get the weight-at-age data for a particular year
#'
#' @param lst A list returned from [load_data_om()]
#' @param yr The year to fetch
#'
#' @return A list of 4 [data.frame]s: `catch`, `surv`, `mid`, and `ssb`
#' @export
get_wa_dfs <- function(lst = NULL,
                       yr = NULL){

  verify_argument(lst, "list")
  verify_argument(yr, c("integer", "numeric"))

  # If in a year past the end of the conditioned time period, return the start year weight-at-age data
  yr <- ifelse(yr <= lst$m_yr, yr, lst$s_yr)
  out <- NULL
  out$catch <- get_age_dat(lst$wage_catch_df, yr)
  out$survey <- get_age_dat(lst$wage_survey_df, yr)
  out$mid <- get_age_dat(lst$wage_mid_df, yr)
  out$ssb <- get_age_dat(lst$wage_ssb_df, yr)

  out
}