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

  stopifnot(s_min < s_max)
  stopifnot(sum(!is.na(match(s_min:s_max, ages))) == length(s_min:s_max))

  n_age <- length(ages)
  sel <- rep(NA, n_age)
  p_sel_val <- c(0, unlist(p_sel[, names(p_sel) == "value"]))
  #p_sel_val <- c(0, p_sel %>% pull(value))
  p_max <- max(cumsum(p_sel_val))

  for(i in seq_along(ages)){
    if(ages[i] < s_min){
      sel[i] <- 0
      p_tmp <- 0
    }else if(ages[i] == s_min){
      p_tmp <- p_sel_val[i - s_min]
      sel[i] = exp(p_tmp - p_max)
    }else if(ages[i] > s_min & ages[i] <= s_max){
      p_tmp <- p_sel_val[i - s_min] + p_tmp
      sel[i] <- exp(p_tmp - p_max)
    }else{
      sel[i] <- sel[s_max + 1]
    }
  }
  sel
}
