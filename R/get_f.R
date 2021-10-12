#' Calculate the fishing mortality based on catch.
#' See Methot & Wetzel 2013, Appendix A.
#'
#' @param e_tmp Catch
#' @param b_tmp Biomass
#' @param m_season Natural mortality
#' @param f_sel Fisheries selectivity
#' @param n_tmp Numbers at age
#' @param wage_catch Weight at age
#' @param method Only 'Hybrid' implemented
#'
#' @return The fishing mortality value
#' @export
get_f <- function(e_tmp = NULL,
                  b_tmp = NULL,
                  yr = yr,
                  season = season,
                  space = space,
                  m_season = NULL,
                  f_sel = NULL,
                  n_tmp = NULL,
                  wage_catch = NULL,
                  method = "Hybrid"){

  stopifnot(length(m_season) == length(f_sel))
  stopifnot(length(f_sel) == length(n_tmp))
  stopifnot(length(n_tmp) == length(wage_catch))
  stopifnot(method == "Hybrid")

  # TODO: This function can be simplified significantly

  if(e_tmp <= 0){
    return(0)
  }
  tmp <- e_tmp / (b_tmp + 0.1 * e_tmp)
  join <- (1 + exp(30 * (tmp - 0.95))) ^ -1
  tmp2 <- join * tmp + 0.95 * (1 - join)
  f_new <- -log(1 - tmp2)
  for(i in 1:4){
    z <- m_season + f_new * f_sel
    lambda <- (1 - exp(-z)) / z
    c_tmp <- sum((f_new / z) * (n_tmp * wage_catch * f_sel) * lambda)
    z_adj <- e_tmp/(c_tmp + 0.0001)
    z_prime <- m_season + z_adj * (z - m_season)
    lambda <- (1 - exp(-z_prime)) / z_prime
    tmp <- sum(n_tmp * wage_catch * f_sel * lambda)
    f_tmp <- e_tmp / (tmp + 0.0001)
    j2 <- (1 + exp(30 * (f_tmp - 0.95))) ^ -1
    f_new <- j2 * f_tmp + (1 - j2)
  }

  f_new
}
