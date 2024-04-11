#' Calculate the mean weights-at-age for all rows in a data frame
#'
#' @param waa A weigth-at-age data frame
#' @param yr_col Name of the column to use for the year. It must be present
#' in the `waa` data frame or an error will occur
#' @param yrs The years to include in the calculation. If `NULL`, all years
#' in `waa` will be used. Negatives will be removed from the calculation. For
#' example if yrs = -c(1966, 1967) then all years except 1966 and 1967 will
#' be used in the calculation
#'
#' @return A single-row data frame containing the averages
#'
#' @export
calc_mean_waa <- function(waa,
                          yr_col = "Yr",
                          yrs = NULL){

  if(!yr_col %in% names(waa)){
    stop("The column name `", yr_col, "` does not exist in the `waa` ",
         "data frame")
  }

  yr_col_sym <- sym(yr_col)

  if(!is.null(yrs[1])){
    yrs_pos <- yrs[yrs > 0]
    yrs_neg <- yrs[yrs < 0]
    if(length(yrs_neg)){
      waa <- waa |>
        filter(!(!!yr_col_sym %in% abs(yrs_neg)))
    }
    if(length(yrs_pos)){
      waa <- waa |>
        filter(!!yr_col_sym %in% yrs_pos)
    }
  }

  map_dfr(waa |> select(-!!yr_col_sym), ~{
    mean(.x, na.rm = TRUE)
  })
}