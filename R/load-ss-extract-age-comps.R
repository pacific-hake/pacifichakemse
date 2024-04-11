#' Extract the age comp estimates from the SS assessment model output into
#'  a format required for input into the TMB assessment model
#'
#' @details Proportions at age for each year are calculated and returned
#'
#' @param ss_model SS model input/output as read in by [load_ss_model_data()]
#' @param age_comps_fleet 1 for fishery, 2 for survey
#' @param start_yr If not `NULL`, remove all years prior to this
#' @param end_yr If not `NULL`, remove all years after this
#' @param age_comps_fill Value to replace NAs in the table, it can also be
#'  `NA`, but not `NULL`
#' @param yr_col The name of the column in `ss_model$extra_mcmc$age_comps`
#' that contains the year
#' @param age_comps_fleet_col The name of the column in
#' `ss_model$extra_mcmc$age_comps` that contains the fleet code
#' @param ... Arguments absorbed which are meant for other functions
#'
#' @return The matrix representing all years from `start_yr` to `end_yr` with
#' data included for years which are found in the `ss_model` data. Years
#' not in the `ss_model` data will be filled with the value of
#' `age_comps_fill`
#'
#' @export
load_ss_extract_age_comps <- function(ss_model = NULL,
                                      age_comps_fleet = 1,
                                      start_yr = NULL,
                                      end_yr = NULL,
                                      age_comps_fill = -1,
                                      yr_col = "yr",
                                      age_comps_fleet_col = "fleet",
                                      age_col = "age",
                                      value_col = "50%",
                                      ...){

  stopifnot(age_comps_fleet %in% c(1, 2))
  if(is.na(age_comps_fill)){
    age_comps_fill <- NA_real_
  }

  age_comp_data <- ss_model$extra_mcmc$age_comps
  if(!yr_col %in% names(age_comp_data)){
    stop("The column `", yr_col, "` does not exist in the SS age ",
         "comp data table.")
  }
  if(!age_comps_fleet_col %in% names(age_comp_data)){
    stop("The column `", age_comps_fleet_col, "` does not exist in the SS ",
         "age comp data table.")
  }
  if(!age_col %in% names(age_comp_data)){
    stop("The column `", age_col, "` does not exist in the SS ",
         "age comp data table.")
  }
  if(!age_comps_fleet %in% age_comp_data$fleet){
    stop("The fleet number `", age_comps_fleet, "` was not found in the SS ",
         "age comp data table. ")
  }

  age_comps_fleet_col_sym <- sym(age_comps_fleet_col)
  yr_col_sym <- sym(yr_col)
  age_col_sym <- sym(age_col)
  value_col_sym <- sym(value_col)
  ages <- ss_model$extra_mcmc$age_comps$age |> unique() |> sort()

  age_comps <- ss_model$extra_mcmc$age_comps |>
    filter(!!age_comps_fleet_col_sym == age_comps_fleet) |>
    select(-!!age_comps_fleet_col_sym) |>
    complete(!!yr_col_sym := ss_model$startyr:ss_model$endyr,
             !!age_col_sym := ages) |>
    transmute(!!yr_col_sym,
              !!age_col_sym,
              value = !!value_col_sym)

  if(!is.null(start_yr)){
    age_comps <- age_comps |>
      filter(!!yr_col_sym >= start_yr)
  }
  if(!is.null(end_yr)){
    age_comps <- age_comps |>
      filter(!!yr_col_sym <= end_yr)
  }

  age_comps[is.na(age_comps)] <- age_comps_fill
  age_comps <- age_comps |>
    pivot_wider(names_from = "yr", values_from = "value") |>
    select(-age)

  age_comps |> as.matrix()
}
