#' Fill in missing year rows in a weight-at-age data frame with supplied values
#'
#' @param waa A weight-at-age data frame
#' @param values A single-row data frame as output by [calc_mean_waa()]
#' @param yr_col Name of the column to use for the year. It must be present
#' in the `waa` data frame or an error will occur
#' @param yrs The years to include in the calculation. If `NULL`, all years
#' in `waa` will be used. Negatives will be removed from the calculation. For
#' example if yrs = -c(1966, 1967) then all years except 1966 and 1967 will
#' be used in the calculation
#'
#' @return A data frame, the same as the input `waa` data frame, but with
#' the missing years defined by `yrs` filled in with the mean data found in
#' `means`
#'
#' @export
fill_waa_years <- function(waa,
                           values,
                           yr_col = "Yr",
                           yrs = NULL){

  if(is.null(yrs)){
    warning("`yrs` is `NULL` so the function is not filling in any ",
            "weight-at-age years")
    return(waa)
  }

  if(ncol(values) != ncol(waa) - 1){
    stop("The numner of columns in the `waa` data frame (", ncol(waa),
         ") must be one greater than the number of columns in `values` (",
         ncol(values), ")")
  }

  if(!yr_col %in% names(waa)){
    stop("The column name `", yr_col, "` does not exist in the `waa` ",
         "data frame")
  }

  yr_col_sym <- sym(yr_col)

  missing_yrs <- setdiff(yrs, unique(waa$Yr))

  if(!length(missing_yrs)){
    return(waa)
  }

  for(i in missing_yrs){
    new_vals <- values |>
      mutate(!!yr_col_sym := i) |>
      select(!!yr_col_sym, everything())
    waa <- waa |>
      bind_rows(new_vals)
  }

  out <- waa |>
    arrange(!!yr_col_sym)

  out
}
