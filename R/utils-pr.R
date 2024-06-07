#' Wrapper for the `print()` function
#'
#' @param d The data frame
#' @param num_rows The number of rows to show on screen
#' @param ... Arguments passed to `print()`
#'
#' @return Nothing, prints a data frame to screen
#' @export
pr <- function(d, num_rows = 100, ...){
  d |>
    print(n = num_rows, ...)
}