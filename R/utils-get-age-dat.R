#' Extract row(s) of age data from a [data.frame]
#'
#' @param d A [data.frame] in the format of the `wage_*` data frames as
#' output by [load_data_om()]
#' @param yr A vector of years to extract row of data for
#'
#' @return A data frame with the rows requested
#'
#' @export
get_age_dat <- function(d = NULL,
                        yr = NULL){

  d[d[ ,"Yr"] == yr, -1]
}

