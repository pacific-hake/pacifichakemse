#' Calculate the fishing mortality based on catch
#'
#' @param e_tmp Catch
#' @param b_tmp Biomass
#' @param m_season Natural mortality
#' @param f_sel Fisheries selectivity
#' @param n_tmp Numbers at age
#' @param wage_catch Weight at age
#' @param method One of 'Pope' or 'Hybrid'
#'
#' @return The fishing mortality value
#' @export
get_f <- function(e_tmp = NULL,
                  b_tmp = NULL,
                  m_season = NULL,
                  f_sel = NULL,
                  n_tmp = NULL,
                  wage_catch = NULL,
                  method = "Pope"){

  verify_argument(e_tmp, "numeric", 1)
  verify_argument(b_tmp, "numeric", 1)
  verify_argument(m_season, "numeric")
  verify_argument(f_sel, "numeric")
  verify_argument(n_tmp, "numeric")
  verify_argument(wage_catch, "numeric")
  verify_argument(method, "character", 1)
  stopifnot(length(m_season) == length(f_sel))
  stopifnot(length(f_sel) == length(n_tmp))
  stopifnot(length(n_tmp) == length(wage_catch))
  stopifnot(method == "Pope" || method == "Hybrid")

  # TODO: This function can be simplified significantly
  if(e_tmp > 0){
    tmp <- e_tmp / (b_tmp + 0.1 * e_tmp)
    join <- (1 + exp(30 * (tmp - 0.95))) ^ -1
    tmp2 <- join * tmp + 0.95 * (1 - join)
    f_new <- -log(1 - tmp2)
    if(method == "Hybrid"){
      for(i in 1:4){
        z <- m_season + f_new * f_sel
        alpha <- (1 - exp(-z))
        c_tmp <- sum((f_new / z) * (n_tmp * wage_catch * f_sel) * alpha)
        z_adj <- e_tmp/(c_tmp + 0.0001)
        z_prime <- m_season + z_adj * (z - m_season)
        alpha <- (1 - exp(-z_prime)) / (z_prime)
        tmp <- sum(n_tmp * wage_catch * f_sel * alpha)
        f_tmp <-  e_tmp / (tmp + 0.0001)
        j2 <- 1 / (1 + exp(30 * (f_tmp - 0.95 * 1)))
        f_new <- j2 * f_tmp + (1 - j2)
      }
    }
  }else{
    f_new <- 0
  }
  f_new
}
