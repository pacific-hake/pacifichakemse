#' Get a vector of selectivities of the same length as the `ages` vector
#' based on a range asked for
#'
#' @param ages A vector of ages to calculate selectivity for
#' @param p_sel Log selectivity values from age `s_min` to `s_max`
#' @param s_min Minimum age
#' @param s_max Maximum age
#'
#' @return A vector of selectivity values of the same length as the `ages` vector
#' @importFrom dplyr arrange
#' @export
get_select <- function(ages = NULL,
                       p_sel = NULL,
                       s_min = NULL,
                       s_max = NULL){

  n_age <- length(ages)
  sel <- rep(NA_real_, n_age)
  p_sel_val <- c(0, p_sel[, "value"])
  #p_max <- max(cumsum(p_sel_val))
  p_max <- sum(p_sel_val)

  # These logicals are set up for speed purposes. It costs a lot to do element comparisons inside a loop
  ages_less_s_min <- ages < s_min
  ages_eq_s_min <- ages == s_min
  ages_less_eq_s_min <- ages_less_s_min | ages_eq_s_min
  ages_greater_s_max <- ages > s_max
  ages_greater_eq_s_max <- ages >= s_max
  ages_between_s_min_s_max <- !ages_less_eq_s_min & !ages_greater_s_max
  for(i in seq_along(ages)){
    if(ages_less_s_min[i]){
      sel[i] <- 0
      p_tmp <- 0
    }else if(ages_eq_s_min[i]){
      p_tmp <- p_sel_val[i - s_min]
      sel[i] = exp(p_tmp - p_max)
    }else if(ages_between_s_min_s_max[i]){
      p_tmp <- p_sel_val[i - s_min] + p_tmp
      sel[i] <- exp(p_tmp - p_max)
    }else{
      sel[i] <- sel[s_max + 1]
    }
  }
  sel
}
