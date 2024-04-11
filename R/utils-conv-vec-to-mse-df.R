#' Convert a vector of aggregated values into a [data.frame] containing
#' the year, run, value, and possibly country as columns
#'
#' @param vec The aggregated vector of data. This must be the same length
#'  as `run_vec`and `yr_vec`
#' @param col The name of the column to create in the new data frame
#' @param yr_vec The vector of years to place in the [data.frame].
#'  This must be the same length as `vec`
#' @param country The name for the country. If `NULL`, no country column
#'  will be added
#' @param probs A vector of quantiles to pass to [stats::quantile()]
#' @param inc_mean Logical. Include the mean (as column `avg`)
#'
#' @return A [data.frame] containing the year, run, value, and possibly
#'  country as columns
#'
#' @export
conv_vec_to_mse_df <- function(vec = NULL,
                               col = NULL,
                               yr_vec = NULL,
                               country = NULL,
                               probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                               inc_mean = TRUE){

  stopifnot(!is.null(vec))
  stopifnot(!is.null(col))
  stopifnot(!is.null(yr_vec))
  stopifnot(!is.null(probs))
  stopifnot(length(vec) == length(yr_vec))

  col_sym <- sym(col)
  df <- vec |>
    as.data.frame() |>
    rename(!!col_sym := 1) |>
    mutate(year = yr_vec) |>
    group_by(year) |>
    group_map(~ calc_quantiles(.x,
                               col = col,
                               probs = probs,
                               include_mean = inc_mean)) |>
    map_df(~{.x}) |>
    mutate(year = unique(yr_vec))
  if(!is.null(country)){
    df <- df |>
      mutate(country = country) |>
      select(country, year, everything())
  }else{
    df <- df |>
      select(year, everything())
  }
  df
}

