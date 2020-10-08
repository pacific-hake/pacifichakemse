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
  # TODO: Check this code again with the original. ptmp is possibly assigned on a previous iteration in a for loop
  # in the original

  verify_argument(ages, "integer")
  verify_argument(p_sel, "tbl_df")
  verify_argument(s_min, "numeric", 1)
  verify_argument(s_max, "numeric", 1)
  stopifnot(s_min < s_max)
  stopifnot(sum(!is.na(match(s_min:s_max, ages))) == length(s_min:s_max))

  n_age <- length(ages)
  sel <- rep(NA, n_age)
  p_max <- max(cumsum(p_sel$value))
  if(!1 %in% p_sel$age){
    p_sel <- rbind(p_sel, p_sel[nrow(p_sel), ])
    p_sel[nrow(p_sel), ]$age <- 1
    p_sel[nrow(p_sel), ]$value <- 0
  }

  for(i in seq_along(ages)){
    if(ages[i] < s_min){
      sel[i] <- 0
      p_tmp <- 0
    }else if(ages[i] >= s_min && ages[i] <= s_max){
      # Using pipes here significantly slows down the code, so use base code
      #p_tmp <- p_sel %>% filter(age == ages[i]) %>% pull(value) + p_tmp
      p_tmp <- p_tmp + p_sel[p_sel$age == ages[i],]$value
      sel[i] <- exp(p_tmp - p_max)
    }else{
      if(is.na(sel[s_max + 1])){
        sel[i] <- sel[s_max]
      }else{
        sel[i] <- sel[s_max + 1]
      }
    }
  }
  sel
}
