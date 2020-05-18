#' Get a vector of selectivities of the same length as the `ages` vector
#' based on a range asked for
#'
#' @param ages A vector of ages to calculate selectivity for
#' @param p_sel Log selectivity values from age `s_min` to `s_max`
#' @param s_min Minimum age
#' @param s_max Maximum age
#'
#' @return A vector of selectivity values of the same length as the `ages` vector
#' @export
get_select <- function(ages = NULL,
                       p_sel = NULL,
                       s_min = NULL,
                       s_max = NULL){
  verify_argument(ages, "integer")
  verify_argument(p_sel, "numeric")
  verify_argument(s_min, "numeric", 1)
  verify_argument(s_max, "numeric", 1)
  stopifnot(s_min < s_max)
  stopifnot(sum(!is.na(match(s_min:s_max, ages))) == length(s_min:s_max))

  p_sel <- c(0, p_sel)
  nage <- length(ages)
  kk <- sel <- rep(NA, nage)
  p_max <- max(cumsum(p_sel))

  sel[1:(s_min - 1)] <- 0
  sel[(s_max + 1):nage] <- 1
  sel[is.na(sel)] <- exp(p_sel - p_max)

  sel
}
