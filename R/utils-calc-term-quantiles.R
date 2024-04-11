#' Calculate quantiles for the mean values (across years) of each value in
#' the group given by `grp_col` for a term of years
#'
#' @description Calculate quantiles for the mean values (across years) of each value in
#' the group given by `grp_col` for a term of years
#'
#' @rdname calc_quantiles
#'
#' @param df A [data.frame] with columns `year`, the string assigned to `grp_col`, and
#' the string assigned to `col`
#' @param grp_col The column name to use for grouping the data
#' @param min_yr The minimum year in the term. If `NA`, the minimum value in the [data.frame]
#' will be used
#' @param max_yr The maximum year in the term. If `NA`, the maximum value in the [data.frame]
#' will be used
#' @param probs A vector of quantiles to pass to [stats::quantile()]
#' @param mean_multiplier This value will be multiplied by the mean of the values
#' before the quantiles are calculated
#'
#' @return A single-row [data.frame] containing the quantile values of the run means
#'
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(purrr)
#' pq <- tribble(
#'   ~year, ~grp, ~val,
#'   2000,    1,  2.1,
#'   2001,    1,  3.4,
#'   2002,    1,  4.5,
#'   2003,    1,  5.6,
#'   2004,    1,  6.7,
#'   2000,    2,  3.1,
#'   2001,    2,  4.4,
#'   2002,    2,  5.5,
#'   2003,    2,  6.6,
#'   2004,    2,  8.7,
#'   2000,    3, 13.1,
#'   2001,    3, 14.4,
#'   2002,    3, 15.5,
#'   2003,    3, 16.6,
#'   2004,    3, 18.7)
#'
#' probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
#'
#' j <- calc_term_quantiles(pq,
#'                          grp_col = "grp",
#'                          col = "val",
#'                          probs = probs)
calc_term_quantiles <- function(df = NULL,
                                grp_col = NULL,
                                col = NULL,
                                min_yr = NA_real_,
                                max_yr = NA_real_,
                                probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                mean_multiplier = 1){

  stopifnot(grp_col %in% names(df))
  stopifnot(col %in% names(df))
  stopifnot(class(df[[col]]) == "numeric")

  grp_col_sym <- sym(grp_col)
  col_sym <- sym(col)
  if(is.na(min_yr)){
    min_yr <- min(df$year)
  }
  if(is.na(max_yr)){
    max_yr <- max(df$year)
  }
  if(max_yr < min_yr){
    warning("max_yr (", max_yr, ") is less than min_yr (", min_yr, "). ",
            "Returning NULL")
    return(NULL)
  }
  if(!df |>
     filter(df$year %in% min_yr:max_yr) |>
     nrow()){
    warning("There are no rows in the data frame within the years ",
            "specified. Returning NULL")
    return(NULL)
  }

  df |>
    filter(year >= min_yr & year <= max_yr) |>
    group_by(!!grp_col_sym) |>
    summarize(avg = mean(!!col_sym) * mean_multiplier) |>
    group_map(~ calc_quantiles(.x,
                               col = "avg",
                               probs = probs,
                               include_mean = FALSE)) |>
    map_df(~{.x}) |>
    ungroup()
}

