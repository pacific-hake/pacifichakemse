#' Apply arbitrary quantiles to a column of data
#'
#' @param df A [data.frame]
#' @param col A column name found in `df`
#' @param probs A vector of probabilities to be passed to the [stats::quantile()] function
#'
#' @return A [data.frame] with a new column for each value in the `probs` vector
#' @importFrom purrr set_names
#' @export
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
#' yrs <- sort(unique(pq$year))
#' df <- pq %>%
#'   group_by(year) %>%
#'   group_map(~ apply_quantiles(.x, col = "val", probs = probs)) %>%
#'   map_df(~{.x}) %>%
#'   mutate(year = yrs) %>%
#'   select(year, everything())
apply_quantiles <- function(df = NULL,
                            col = NULL,
                            probs = c(0.05, 0.5, 0.95)){

  stopifnot(!is.null(df))
  stopifnot(!is.null(col))
  stopifnot(!is.null(probs))
  stopifnot(col %in% names(df))
  stopifnot(class(df[[col]]) == "numeric")

  col_quo <- quo(col)
  summarize_at(df,
               vars(!!col_quo),
               map(probs,
                   ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
                 set_names(probs))
}

#' TMB helper function. Extract parameter from `obj` to use as initial values in optimization
#'
#' @details The code was copied from a non-exported function in the `TMBhelper` package called
#' `extract_fixed()`
#'
#' @param obj Object to be used as input to [TMB::MakeADFun()]
#'
#' @return A vector of initial values of parameters
#' @export
extract_initial_values <- function(obj){
  if(!length(obj$env$random)) {
    obj$env$last.par.best
  }
  else {
    obj$env$last.par.best[-c(obj$env$random)]
  }
}