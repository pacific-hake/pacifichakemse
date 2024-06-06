#' Calculate the mean weights-at-age for all rows in a data frame
#'
#' @param waa A weigth-at-age data frame
#' @param yr_col Name of the column to use for the year. It must be present
#' in the `waa` data frame or an error will occur
#'
#' @return A single-row data frame containing the averages
#'
#' @export
calc_mean_waa <- function(d,
                          yr_col = "Yr"){

  if(!yr_col %in% names(d)){
    stop("The column name `", yr_col, "` does not exist in the `d`",
         "data frame")
  }

  yr_col_sym <- sym(yr_col)

  map_dfr(d |> select(-!!yr_col_sym), ~{
    mean(.x, na.rm = TRUE)
  })
}