#' Apply arbitrary quantiles to a column of data
#'
#' @param df A [data.frame]
#' @param col A column name found in `df`
#' @param probs A vector of probabilities to be passed to the [stats::quantile()] function
#'
#' @return A [data.frame] with a new column for wach value in the `probs` vector
#' @export
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
