#' Get the weight-at-X data for a particular year
#'
#' @param lst A list returned from [load_data_seasons()]
#' @param yr The year to fetch
#'
#' @return A list of 4 [data.frame]: `catch`, `surv`, `mid`, and `ssb`
#' @export
get_wa_dfs <- function(lst, yr){
  if(yr <= lst$m_yr){
    catch <- get_age_dat(lst$wage_catch, yr)
    surv <- get_age_dat(lst$wage_survey, yr)
    mid <- get_age_dat(lst$wage_mid, yr)
    ssb <- get_age_dat(lst$wage_ssb, yr)
  }else{
    catch <- get_age_dat(lst$wage_catch, lst$s_yr)
    surv <- get_age_dat(lst$wage_survey, lst$s_yr)
    mid <- get_age_dat(lst$wage_mid, lst$s_yr)
    ssb <- get_age_dat(lst$wage_ssb, lst$s_yr)
  }
  list(catch = catch,
       surv = surv,
       mid = mid,
       ssb = ssb)
}